------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R _ S C O                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2009-2012, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Debug;    use Debug;
with Errout;   use Errout;
with Lib;      use Lib;
with Lib.Util; use Lib.Util;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Put_SCOs;
with SCOs;     use SCOs;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Table;

with GNAT.HTable;      use GNAT.HTable;
with GNAT.Heap_Sort_G;

package body Par_SCO is

   -----------------------
   -- Unit Number Table --
   -----------------------

   --  This table parallels the SCO_Unit_Table, keeping track of the unit
   --  numbers corresponding to the entries made in this table, so that before
   --  writing out the SCO information to the ALI file, we can fill in the
   --  proper dependency numbers and file names.

   --  Note that the zero'th entry is here for convenience in sorting the
   --  table, the real lower bound is 1.

   package SCO_Unit_Number_Table is new Table.Table (
     Table_Component_Type => Unit_Number_Type,
     Table_Index_Type     => SCO_Unit_Index,
     Table_Low_Bound      => 0, -- see note above on sort
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "SCO_Unit_Number_Entry");

   ---------------------------------
   -- Condition/Pragma Hash Table --
   ---------------------------------

   --  We need to be able to get to conditions quickly for handling the calls
   --  to Set_SCO_Condition efficiently, and similarly to get to pragmas to
   --  handle calls to Set_SCO_Pragma_Enabled. For this purpose we identify the
   --  conditions and pragmas in the table by their starting sloc, and use this
   --  hash table to map from these sloc values to SCO_Table indexes.

   type Header_Num is new Integer range 0 .. 996;
   --  Type for hash table headers

   function Hash (F : Source_Ptr) return Header_Num;
   --  Function to Hash source pointer value

   function Equal (F1, F2 : Source_Ptr) return Boolean;
   --  Function to test two keys for equality

   package Condition_Pragma_Hash_Table is new Simple_HTable
     (Header_Num, Int, 0, Source_Ptr, Hash, Equal);
   --  The actual hash table

   --------------------------
   -- Internal Subprograms --
   --------------------------

   function Has_Decision (N : Node_Id) return Boolean;
   --  N is the node for a subexpression. Returns True if the subexpression
   --  contains a nested decision (i.e. either is a logical operator, or
   --  contains a logical operator in its subtree).

   function Is_Logical_Operator (N : Node_Id) return Boolean;
   --  N is the node for a subexpression. This procedure just tests N to see
   --  if it is a logical operator (including short circuit conditions, but
   --  excluding OR and AND) and returns True if so, False otherwise, it does
   --  no other processing.

   procedure Process_Decisions
     (N           : Node_Id;
      T           : Character;
      Pragma_Sloc : Source_Ptr);
   --  If N is Empty, has no effect. Otherwise scans the tree for the node N,
   --  to output any decisions it contains. T is one of IEGPWX (for context of
   --  expression: if/exit when/entry guard/pragma/while/expression). If T is
   --  other than X, the node N is the conditional expression involved, and a
   --  decision is always present (at the very least a simple decision is
   --  present at the top level).

   procedure Process_Decisions
     (L           : List_Id;
      T           : Character;
      Pragma_Sloc : Source_Ptr);
   --  Calls above procedure for each element of the list L

   procedure Set_Table_Entry
     (C1          : Character;
      C2          : Character;
      From        : Source_Ptr;
      To          : Source_Ptr;
      Last        : Boolean;
      Pragma_Sloc : Source_Ptr := No_Location;
      Pragma_Name : Pragma_Id  := Unknown_Pragma);
   --  Append an entry to SCO_Table with fields set as per arguments

   type Dominant_Info is record
      K : Character;
      --  F/T/S/E for a valid dominance marker, or ' ' for no dominant

      N : Node_Id;
      --  Node providing the Sloc(s) for the dominance marker
   end record;
   No_Dominant : constant Dominant_Info := (' ', Empty);

   procedure Traverse_Declarations_Or_Statements
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty);
   --  Process L, a list of statements or declarations dominated by D.
   --  If P is present, it is processed as though it had been prepended to L.

   procedure Traverse_Generic_Instantiation       (N : Node_Id);
   procedure Traverse_Generic_Package_Declaration (N : Node_Id);
   procedure Traverse_Handled_Statement_Sequence
     (N : Node_Id;
      D : Dominant_Info := No_Dominant);
   procedure Traverse_Package_Body                (N : Node_Id);
   procedure Traverse_Package_Declaration         (N : Node_Id);
   procedure Traverse_Protected_Body              (N : Node_Id);
   procedure Traverse_Subprogram_Or_Task_Body
     (N : Node_Id;
      D : Dominant_Info := No_Dominant);
   procedure Traverse_Subprogram_Declaration      (N : Node_Id);
   --  Traverse the corresponding construct, generating SCO table entries

   procedure Write_SCOs_To_ALI_File is new Put_SCOs;
   --  Write SCO information to the ALI file using routines in Lib.Util

   ----------
   -- dsco --
   ----------

   procedure dsco is
   begin
      --  Dump SCO unit table

      Write_Line ("SCO Unit Table");
      Write_Line ("--------------");

      for Index in 1 .. SCO_Unit_Table.Last loop
         declare
            UTE : SCO_Unit_Table_Entry renames SCO_Unit_Table.Table (Index);

         begin
            Write_Str ("  ");
            Write_Int (Int (Index));
            Write_Str (".  Dep_Num = ");
            Write_Int (Int (UTE.Dep_Num));
            Write_Str ("  From = ");
            Write_Int (Int (UTE.From));
            Write_Str ("  To = ");
            Write_Int (Int (UTE.To));

            Write_Str ("  File_Name = """);

            if UTE.File_Name /= null then
               Write_Str (UTE.File_Name.all);
            end if;

            Write_Char ('"');
            Write_Eol;
         end;
      end loop;

      --  Dump SCO Unit number table if it contains any entries

      if SCO_Unit_Number_Table.Last >= 1 then
         Write_Eol;
         Write_Line ("SCO Unit Number Table");
         Write_Line ("---------------------");

         for Index in 1 .. SCO_Unit_Number_Table.Last loop
            Write_Str ("  ");
            Write_Int (Int (Index));
            Write_Str (". Unit_Number = ");
            Write_Int (Int (SCO_Unit_Number_Table.Table (Index)));
            Write_Eol;
         end loop;
      end if;

      --  Dump SCO table itself

      Write_Eol;
      Write_Line ("SCO Table");
      Write_Line ("---------");

      for Index in 1 .. SCO_Table.Last loop
         declare
            T : SCO_Table_Entry renames SCO_Table.Table (Index);

         begin
            Write_Str  ("  ");
            Write_Int  (Index);
            Write_Char ('.');

            if T.C1 /= ' ' then
               Write_Str  ("  C1 = '");
               Write_Char (T.C1);
               Write_Char (''');
            end if;

            if T.C2 /= ' ' then
               Write_Str  ("  C2 = '");
               Write_Char (T.C2);
               Write_Char (''');
            end if;

            if T.From /= No_Source_Location then
               Write_Str ("  From = ");
               Write_Int (Int (T.From.Line));
               Write_Char (':');
               Write_Int (Int (T.From.Col));
            end if;

            if T.To /= No_Source_Location then
               Write_Str ("  To = ");
               Write_Int (Int (T.To.Line));
               Write_Char (':');
               Write_Int (Int (T.To.Col));
            end if;

            if T.Last then
               Write_Str ("  True");
            else
               Write_Str ("  False");
            end if;

            Write_Eol;
         end;
      end loop;
   end dsco;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : Source_Ptr) return Boolean is
   begin
      return F1 = F2;
   end Equal;

   ------------------
   -- Has_Decision --
   ------------------

   function Has_Decision (N : Node_Id) return Boolean is

      function Check_Node (N : Node_Id) return Traverse_Result;

      ----------------
      -- Check_Node --
      ----------------

      function Check_Node (N : Node_Id) return Traverse_Result is
      begin
         if Is_Logical_Operator (N) then
            return Abandon;
         else
            return OK;
         end if;
      end Check_Node;

      function Traverse is new Traverse_Func (Check_Node);

   --  Start of processing for Has_Decision

   begin
      return Traverse (N) = Abandon;
   end Has_Decision;

   ----------
   -- Hash --
   ----------

   function Hash (F : Source_Ptr) return Header_Num is
   begin
      return Header_Num (Nat (F) mod 997);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      SCO_Unit_Number_Table.Init;

      --  Set dummy 0'th entry in place for sort

      SCO_Unit_Number_Table.Increment_Last;
   end Initialize;

   -------------------------
   -- Is_Logical_Operator --
   -------------------------

   function Is_Logical_Operator (N : Node_Id) return Boolean is
   begin
      return Nkind_In (N, N_Op_Not, N_And_Then, N_Or_Else);
   end Is_Logical_Operator;

   -----------------------
   -- Process_Decisions --
   -----------------------

   --  Version taking a list

   procedure Process_Decisions
     (L           : List_Id;
      T           : Character;
      Pragma_Sloc : Source_Ptr)
   is
      N : Node_Id;
   begin
      if L /= No_List then
         N := First (L);
         while Present (N) loop
            Process_Decisions (N, T, Pragma_Sloc);
            Next (N);
         end loop;
      end if;
   end Process_Decisions;

   --  Version taking a node

   Current_Pragma_Sloc : Source_Ptr := No_Location;
   --  While processing a pragma, this is set to the sloc of the N_Pragma node

   procedure Process_Decisions
     (N           : Node_Id;
      T           : Character;
      Pragma_Sloc : Source_Ptr)
   is
      Mark : Nat;
      --  This is used to mark the location of a decision sequence in the SCO
      --  table. We use it for backing out a simple decision in an expression
      --  context that contains only NOT operators.

      X_Not_Decision : Boolean;
      --  This flag keeps track of whether a decision sequence in the SCO table
      --  contains only NOT operators, and is for an expression context (T=X).
      --  The flag will be set False if T is other than X, or if an operator
      --  other than NOT is in the sequence.

      function Process_Node (N : Node_Id) return Traverse_Result;
      --  Processes one node in the traversal, looking for logical operators,
      --  and if one is found, outputs the appropriate table entries.

      procedure Output_Decision_Operand (N : Node_Id);
      --  The node N is the top level logical operator of a decision, or it is
      --  one of the operands of a logical operator belonging to a single
      --  complex decision. This routine outputs the sequence of table entries
      --  corresponding to the node. Note that we do not process the sub-
      --  operands to look for further decisions, that processing is done in
      --  Process_Decision_Operand, because we can't get decisions mixed up in
      --  the global table. Call has no effect if N is Empty.

      procedure Output_Element (N : Node_Id);
      --  Node N is an operand of a logical operator that is not itself a
      --  logical operator, or it is a simple decision. This routine outputs
      --  the table entry for the element, with C1 set to ' '. Last is set
      --  False, and an entry is made in the condition hash table.

      procedure Output_Header (T : Character);
      --  Outputs a decision header node. T is I/W/E/P for IF/WHILE/EXIT WHEN/
      --  PRAGMA, and 'X' for the expression case.

      procedure Process_Decision_Operand (N : Node_Id);
      --  This is called on node N, the top level node of a decision, or on one
      --  of its operands or suboperands after generating the full output for
      --  the complex decision. It process the suboperands of the decision
      --  looking for nested decisions.

      -----------------------------
      -- Output_Decision_Operand --
      -----------------------------

      procedure Output_Decision_Operand (N : Node_Id) is
         C : Character;
         L : Node_Id;

      begin
         if No (N) then
            return;

         --  Logical operator

         elsif Is_Logical_Operator (N) then
            if Nkind (N) = N_Op_Not then
               C := '!';
               L := Empty;

            else
               L := Left_Opnd (N);

               if Nkind_In (N, N_Op_Or, N_Or_Else) then
                  C := '|';
               else
                  C := '&';
               end if;
            end if;

            Set_Table_Entry
              (C1   => C,
               C2   => ' ',
               From => Sloc (N),
               To   => No_Location,
               Last => False);

            Output_Decision_Operand (L);
            Output_Decision_Operand (Right_Opnd (N));

         --  Not a logical operator

         else
            Output_Element (N);
         end if;
      end Output_Decision_Operand;

      --------------------
      -- Output_Element --
      --------------------

      procedure Output_Element (N : Node_Id) is
         FSloc : Source_Ptr;
         LSloc : Source_Ptr;
      begin
         Sloc_Range (N, FSloc, LSloc);
         Set_Table_Entry
           (C1   => ' ',
            C2   => 'c',
            From => FSloc,
            To   => LSloc,
            Last => False);
         Condition_Pragma_Hash_Table.Set (FSloc, SCO_Table.Last);
      end Output_Element;

      -------------------
      -- Output_Header --
      -------------------

      procedure Output_Header (T : Character) is
         Loc : Source_Ptr := No_Location;
         --  Node whose Sloc is used for the decision

      begin
         case T is
            when 'I' | 'E' | 'W' =>

               --  For IF, EXIT, WHILE, the token SLOC can be found from
               --  the SLOC of the parent of the expression.

               Loc := Sloc (Parent (N));

            when 'G' | 'P' =>

               --  For entry guard, the token sloc is from the N_Entry_Body.
               --  For PRAGMA, we must get the location from the pragma node.
               --  Argument N is the pragma argument, and we have to go up
               --  two levels (through the pragma argument association) to
               --  get to the pragma node itself. For the guard on a select
               --  alternative, we do not have access to the token location for
               --  the WHEN, so we use the first sloc of the condition itself
               --  (note: we use First_Sloc, not Sloc, because this is what is
               --  referenced by dominance markers).

               --  Doesn't this requirement of using First_Sloc need to be
               --  documented in the spec ???

               if Nkind_In (Parent (N), N_Accept_Alternative,
                                        N_Delay_Alternative,
                                        N_Terminate_Alternative)
               then
                  Loc := First_Sloc (N);
               else
                  Loc := Sloc (Parent (Parent (N)));
               end if;

            when 'X' =>

               --  For an expression, no Sloc

               null;

            --  No other possibilities

            when others =>
               raise Program_Error;
         end case;

         Set_Table_Entry
           (C1          => T,
            C2          => ' ',
            From        => Loc,
            To          => No_Location,
            Last        => False,
            Pragma_Sloc => Pragma_Sloc);
      end Output_Header;

      ------------------------------
      -- Process_Decision_Operand --
      ------------------------------

      procedure Process_Decision_Operand (N : Node_Id) is
      begin
         if Is_Logical_Operator (N) then
            if Nkind (N) /= N_Op_Not then
               Process_Decision_Operand (Left_Opnd (N));
               X_Not_Decision := False;
            end if;

            Process_Decision_Operand (Right_Opnd (N));

         else
            Process_Decisions (N, 'X', Pragma_Sloc);
         end if;
      end Process_Decision_Operand;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (N : Node_Id) return Traverse_Result is
      begin
         case Nkind (N) is

            --  Logical operators, output table entries and then process
            --  operands recursively to deal with nested conditions.

            when N_And_Then | N_Or_Else  | N_Op_Not =>
               declare
                  T : Character;

               begin
                  --  If outer level, then type comes from call, otherwise it
                  --  is more deeply nested and counts as X for expression.

                  if N = Process_Decisions.N then
                     T := Process_Decisions.T;
                  else
                     T := 'X';
                  end if;

                  --  Output header for sequence

                  X_Not_Decision := T = 'X' and then Nkind (N) = N_Op_Not;
                  Mark := SCO_Table.Last;
                  Output_Header (T);

                  --  Output the decision

                  Output_Decision_Operand (N);

                  --  If the decision was in an expression context (T = 'X')
                  --  and contained only NOT operators, then we don't output
                  --  it, so delete it.

                  if X_Not_Decision then
                     SCO_Table.Set_Last (Mark);

                  --  Otherwise, set Last in last table entry to mark end

                  else
                     SCO_Table.Table (SCO_Table.Last).Last := True;
                  end if;

                  --  Process any embedded decisions

                  Process_Decision_Operand (N);
                  return Skip;
               end;

            --  Case expression

            when N_Case_Expression =>
               return OK; -- ???

            --  Conditional expression, processed like an if statement

            when N_Conditional_Expression =>
               declare
                  Cond : constant Node_Id := First (Expressions (N));
                  Thnx : constant Node_Id := Next (Cond);
                  Elsx : constant Node_Id := Next (Thnx);
               begin
                  Process_Decisions (Cond, 'I', Pragma_Sloc);
                  Process_Decisions (Thnx, 'X', Pragma_Sloc);
                  Process_Decisions (Elsx, 'X', Pragma_Sloc);
                  return Skip;
               end;

            --  All other cases, continue scan

            when others =>
               return OK;

         end case;
      end Process_Node;

      procedure Traverse is new Traverse_Proc (Process_Node);

   --  Start of processing for Process_Decisions

   begin
      if No (N) then
         return;
      end if;

      --  See if we have simple decision at outer level and if so then
      --  generate the decision entry for this simple decision. A simple
      --  decision is a boolean expression (which is not a logical operator
      --  or short circuit form) appearing as the operand of an IF, WHILE,
      --  EXIT WHEN, or special PRAGMA construct.

      if T /= 'X' and then not Is_Logical_Operator (N) then
         Output_Header (T);
         Output_Element (N);

         --  Change Last in last table entry to True to mark end of
         --  sequence, which is this case is only one element long.

         SCO_Table.Table (SCO_Table.Last).Last := True;
      end if;

      Traverse (N);
   end Process_Decisions;

   -----------
   -- pscos --
   -----------

   procedure pscos is

      procedure Write_Info_Char (C : Character) renames Write_Char;
      --  Write one character;

      procedure Write_Info_Initiate (Key : Character) renames Write_Char;
      --  Start new one and write one character;

      procedure Write_Info_Nat (N : Nat);
      --  Write value of N

      procedure Write_Info_Terminate renames Write_Eol;
      --  Terminate current line

      --------------------
      -- Write_Info_Nat --
      --------------------

      procedure Write_Info_Nat (N : Nat) is
      begin
         Write_Int (N);
      end Write_Info_Nat;

      procedure Debug_Put_SCOs is new Put_SCOs;

   --  Start of processing for pscos

   begin
      Debug_Put_SCOs;
   end pscos;

   ----------------
   -- SCO_Output --
   ----------------

   procedure SCO_Output is
   begin
      if Debug_Flag_Dot_OO then
         dsco;
      end if;

      --  Sort the unit tables based on dependency numbers

      Unit_Table_Sort : declare

         function Lt (Op1, Op2 : Natural) return Boolean;
         --  Comparison routine for sort call

         procedure Move (From : Natural; To : Natural);
         --  Move routine for sort call

         --------
         -- Lt --
         --------

         function Lt (Op1, Op2 : Natural) return Boolean is
         begin
            return
              Dependency_Num
                (SCO_Unit_Number_Table.Table (SCO_Unit_Index (Op1)))
                     <
              Dependency_Num
                (SCO_Unit_Number_Table.Table (SCO_Unit_Index (Op2)));
         end Lt;

         ----------
         -- Move --
         ----------

         procedure Move (From : Natural; To : Natural) is
         begin
            SCO_Unit_Table.Table (SCO_Unit_Index (To)) :=
              SCO_Unit_Table.Table (SCO_Unit_Index (From));
            SCO_Unit_Number_Table.Table (SCO_Unit_Index (To)) :=
              SCO_Unit_Number_Table.Table (SCO_Unit_Index (From));
         end Move;

         package Sorting is new GNAT.Heap_Sort_G (Move, Lt);

      --  Start of processing for Unit_Table_Sort

      begin
         Sorting.Sort (Integer (SCO_Unit_Table.Last));
      end Unit_Table_Sort;

      --  Loop through entries in the unit table to set file name and
      --  dependency number entries.

      for J in 1 .. SCO_Unit_Table.Last loop
         declare
            U   : constant Unit_Number_Type := SCO_Unit_Number_Table.Table (J);
            UTE : SCO_Unit_Table_Entry renames SCO_Unit_Table.Table (J);
         begin
            Get_Name_String (Reference_Name (Source_Index (U)));
            UTE.File_Name := new String'(Name_Buffer (1 .. Name_Len));
            UTE.Dep_Num := Dependency_Num (U);
         end;
      end loop;

      --  Now the tables are all setup for output to the ALI file

      Write_SCOs_To_ALI_File;
   end SCO_Output;

   -------------------------
   -- SCO_Pragma_Disabled --
   -------------------------

   function SCO_Pragma_Disabled (Loc : Source_Ptr) return Boolean is
      Index : Nat;

   begin
      if Loc = No_Location then
         return False;
      end if;

      Index := Condition_Pragma_Hash_Table.Get (Loc);

      --  The test here for zero is to deal with possible previous errors, and
      --  for the case of pragma statement SCOs, for which we always set the
      --  Pragma_Sloc even if the particular pragma cannot be specifically
      --  disabled.

      if Index /= 0 then
         declare
            T : SCO_Table_Entry renames SCO_Table.Table (Index);
         begin
            pragma Assert (T.C1 = 'S');
            return T.C2 = 'p';
         end;

      else
         return False;
      end if;
   end SCO_Pragma_Disabled;

   ----------------
   -- SCO_Record --
   ----------------

   procedure SCO_Record (U : Unit_Number_Type) is
      Lu   : Node_Id;
      From : Nat;

   begin
      --  Ignore call if not generating code and generating SCO's

      if not (Generate_SCO and then Operating_Mode = Generate_Code) then
         return;
      end if;

      --  Ignore call if this unit already recorded

      for J in 1 .. SCO_Unit_Number_Table.Last loop
         if U = SCO_Unit_Number_Table.Table (J) then
            return;
         end if;
      end loop;

      --  Otherwise record starting entry

      From := SCO_Table.Last + 1;

      --  Get Unit (checking case of subunit)

      Lu := Unit (Cunit (U));

      if Nkind (Lu) = N_Subunit then
         Lu := Proper_Body (Lu);
      end if;

      --  Traverse the unit

      case Nkind (Lu) is
         when N_Protected_Body =>
            Traverse_Protected_Body (Lu);

         when N_Subprogram_Body | N_Task_Body =>
            Traverse_Subprogram_Or_Task_Body (Lu);

         when N_Subprogram_Declaration =>
            Traverse_Subprogram_Declaration (Lu);

         when N_Package_Declaration =>
            Traverse_Package_Declaration (Lu);

         when N_Package_Body =>
            Traverse_Package_Body (Lu);

         when N_Generic_Package_Declaration =>
            Traverse_Generic_Package_Declaration (Lu);

         when N_Generic_Instantiation =>
            Traverse_Generic_Instantiation (Lu);

         when others =>

            --  All other cases of compilation units (e.g. renamings), generate
            --  no SCO information.

            null;
      end case;

      --  Make entry for new unit in unit tables, we will fill in the file
      --  name and dependency numbers later.

      SCO_Unit_Table.Append (
        (Dep_Num   => 0,
         File_Name => null,
         From      => From,
         To        => SCO_Table.Last));

      SCO_Unit_Number_Table.Append (U);
   end SCO_Record;

   -----------------------
   -- Set_SCO_Condition --
   -----------------------

   procedure Set_SCO_Condition (Cond : Node_Id; Val : Boolean) is
      Orig  : constant Node_Id := Original_Node (Cond);
      Index : Nat;
      Start : Source_Ptr;
      Dummy : Source_Ptr;

      Constant_Condition_Code : constant array (Boolean) of Character :=
                                  (False => 'f', True => 't');
   begin
      Sloc_Range (Orig, Start, Dummy);
      Index := Condition_Pragma_Hash_Table.Get (Start);

      --  The test here for zero is to deal with possible previous errors

      if Index /= 0 then
         pragma Assert (SCO_Table.Table (Index).C1 = ' ');
         SCO_Table.Table (Index).C2 := Constant_Condition_Code (Val);
      end if;
   end Set_SCO_Condition;

   ----------------------------
   -- Set_SCO_Pragma_Enabled --
   ----------------------------

   procedure Set_SCO_Pragma_Enabled (Loc : Source_Ptr) is
      Index : Nat;

   begin
      --  Note: the reason we use the Sloc value as the key is that in the
      --  generic case, the call to this procedure is made on a copy of the
      --  original node, so we can't use the Node_Id value.

      Index := Condition_Pragma_Hash_Table.Get (Loc);

      --  The test here for zero is to deal with possible previous errors

      if Index /= 0 then
         declare
            T : SCO_Table_Entry renames SCO_Table.Table (Index);

         begin
            --  Called multiple times for the same sloc (need to allow for
            --  C2 = 'P') ???

            pragma Assert (T.C1 = 'S'
                             and then
                           (T.C2 = 'p' or else T.C2 = 'P'));
            T.C2 := 'P';
         end;
      end if;
   end Set_SCO_Pragma_Enabled;

   ---------------------
   -- Set_Table_Entry --
   ---------------------

   procedure Set_Table_Entry
     (C1          : Character;
      C2          : Character;
      From        : Source_Ptr;
      To          : Source_Ptr;
      Last        : Boolean;
      Pragma_Sloc : Source_Ptr := No_Location;
      Pragma_Name : Pragma_Id  := Unknown_Pragma)
   is
      function To_Source_Location (S : Source_Ptr) return Source_Location;
      --  Converts Source_Ptr value to Source_Location (line/col) format

      ------------------------
      -- To_Source_Location --
      ------------------------

      function To_Source_Location (S : Source_Ptr) return Source_Location is
      begin
         if S = No_Location then
            return No_Source_Location;
         else
            return
              (Line => Get_Logical_Line_Number (S),
               Col  => Get_Column_Number (S));
         end if;
      end To_Source_Location;

   --  Start of processing for Set_Table_Entry

   begin
      SCO_Table.Append
        ((C1          => C1,
          C2          => C2,
          From        => To_Source_Location (From),
          To          => To_Source_Location (To),
          Last        => Last,
          Pragma_Sloc => Pragma_Sloc,
          Pragma_Name => Pragma_Name));
   end Set_Table_Entry;

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   --  Tables used by Traverse_Declarations_Or_Statements for temporarily
   --  holding statement and decision entries. These are declared globally
   --  since they are shared by recursive calls to this procedure.

   type SC_Entry is record
      N    : Node_Id;
      From : Source_Ptr;
      To   : Source_Ptr;
      Typ  : Character;
   end record;
   --  Used to store a single entry in the following table, From:To represents
   --  the range of entries in the CS line entry, and typ is the type, with
   --  space meaning that no type letter will accompany the entry.

   package SC is new Table.Table (
     Table_Component_Type => SC_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 1000,
     Table_Increment      => 200,
     Table_Name           => "SCO_SC");
      --  Used to store statement components for a CS entry to be output
      --  as a result of the call to this procedure. SC.Last is the last
      --  entry stored, so the current statement sequence is represented
      --  by SC_Array (SC_First .. SC.Last), where SC_First is saved on
      --  entry to each recursive call to the routine.
      --
      --  Extend_Statement_Sequence adds an entry to this array, and then
      --  Set_Statement_Entry clears the entries starting with SC_First,
      --  copying these entries to the main SCO output table. The reason that
      --  we do the temporary caching of results in this array is that we want
      --  the SCO table entries for a given CS line to be contiguous, and the
      --  processing may output intermediate entries such as decision entries.

   type SD_Entry is record
      Nod : Node_Id;
      Lst : List_Id;
      Typ : Character;
      Plo : Source_Ptr;
   end record;
   --  Used to store a single entry in the following table. Nod is the node to
   --  be searched for decisions for the case of Process_Decisions_Defer with a
   --  node argument (with Lst set to No_List. Lst is the list to be searched
   --  for decisions for the case of Process_Decisions_Defer with a List
   --  argument (in which case Nod is set to Empty). Plo is the sloc of the
   --  enclosing pragma, if any.

   package SD is new Table.Table (
     Table_Component_Type => SD_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 1000,
     Table_Increment      => 200,
     Table_Name           => "SCO_SD");
   --  Used to store possible decision information. Instead of calling the
   --  Process_Decisions procedures directly, we call Process_Decisions_Defer,
   --  which simply stores the arguments in this table. Then when we clear
   --  out a statement sequence using Set_Statement_Entry, after generating
   --  the CS lines for the statements, the entries in this table result in
   --  calls to Process_Decision. The reason for doing things this way is to
   --  ensure that decisions are output after the CS line for the statements
   --  in which the decisions occur.

   procedure Traverse_Declarations_Or_Statements
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty)
   is
      Current_Dominant : Dominant_Info := D;
      --  Dominance information for the current basic block

      Current_Test : Node_Id;
      --  Conditional node (N_If_Statement or N_Elsiif being processed

      N : Node_Id;

      SC_First : constant Nat := SC.Last + 1;
      SD_First : constant Nat := SD.Last + 1;
      --  Record first entries used in SC/SD at this recursive level

      procedure Extend_Statement_Sequence (N : Node_Id; Typ : Character);
      --  Extend the current statement sequence to encompass the node N. Typ
      --  is the letter that identifies the type of statement/declaration that
      --  is being added to the sequence.

      procedure Set_Statement_Entry;
      --  Output CS entries for all statements saved in table SC, and end the
      --  current CS sequence.

      procedure Process_Decisions_Defer (N : Node_Id; T : Character);
      pragma Inline (Process_Decisions_Defer);
      --  This routine is logically the same as Process_Decisions, except that
      --  the arguments are saved in the SD table, for later processing when
      --  Set_Statement_Entry is called, which goes through the saved entries
      --  making the corresponding calls to Process_Decision.

      procedure Process_Decisions_Defer (L : List_Id; T : Character);
      pragma Inline (Process_Decisions_Defer);
      --  Same case for list arguments, deferred call to Process_Decisions

      procedure Traverse_One (N : Node_Id);
      --  Traverse one declaration or statement

      -------------------------
      -- Set_Statement_Entry --
      -------------------------

      procedure Set_Statement_Entry is
         SC_Last : constant Int := SC.Last;
         SD_Last : constant Int := SD.Last;

      begin
         --  Output statement entries from saved entries in SC table

         for J in SC_First .. SC_Last loop
            if J = SC_First then

               if Current_Dominant /= No_Dominant then
                  declare
                     From, To : Source_Ptr;
                  begin
                     Sloc_Range (Current_Dominant.N, From, To);
                     if Current_Dominant.K /= 'E' then
                        To := No_Location;
                     end if;
                     Set_Table_Entry
                       (C1          => '>',
                        C2          => Current_Dominant.K,
                        From        => From,
                        To          => To,
                        Last        => False,
                        Pragma_Sloc => No_Location,
                        Pragma_Name => Unknown_Pragma);
                  end;
               end if;
            end if;

            declare
               SCE         : SC_Entry renames SC.Table (J);
               Pragma_Sloc : Source_Ptr := No_Location;
               Pragma_Name : Pragma_Id  := Unknown_Pragma;
            begin
               --  For the case of a statement SCO for a pragma controlled by
               --  Set_SCO_Pragma_Enabled, set Pragma_Sloc so that the SCO (and
               --  those of any nested decision) is emitted only if the pragma
               --  is enabled.

               if SCE.Typ = 'p' then
                  Pragma_Sloc := SCE.From;
                  Condition_Pragma_Hash_Table.Set
                    (Pragma_Sloc, SCO_Table.Last + 1);
                  Pragma_Name := Get_Pragma_Id (Sinfo.Pragma_Name (SCE.N));

               elsif SCE.Typ = 'P' then
                  Pragma_Name := Get_Pragma_Id (Sinfo.Pragma_Name (SCE.N));
               end if;

               Set_Table_Entry
                 (C1          => 'S',
                  C2          => SCE.Typ,
                  From        => SCE.From,
                  To          => SCE.To,
                  Last        => (J = SC_Last),
                  Pragma_Sloc => Pragma_Sloc,
                  Pragma_Name => Pragma_Name);
            end;
         end loop;

         --  Last statement of basic block, if present, becomes new current
         --  dominant.

         if SC_Last >= SC_First then
            Current_Dominant := ('S', SC.Table (SC_Last).N);
         end if;

         --  Clear out used section of SC table

         SC.Set_Last (SC_First - 1);

         --  Output any embedded decisions

         for J in SD_First .. SD_Last loop
            declare
               SDE : SD_Entry renames SD.Table (J);
            begin
               if Present (SDE.Nod) then
                  Process_Decisions (SDE.Nod, SDE.Typ, SDE.Plo);
               else
                  Process_Decisions (SDE.Lst, SDE.Typ, SDE.Plo);
               end if;
            end;
         end loop;

         --  Clear out used section of SD table

         SD.Set_Last (SD_First - 1);
      end Set_Statement_Entry;

      -------------------------------
      -- Extend_Statement_Sequence --
      -------------------------------

      procedure Extend_Statement_Sequence (N : Node_Id; Typ : Character) is
         F       : Source_Ptr;
         T       : Source_Ptr;
         Dummy   : Source_Ptr;
         To_Node : Node_Id := Empty;

      begin
         Sloc_Range (N, F, T);

         case Nkind (N) is
            when N_Accept_Statement =>
               if Present (Parameter_Specifications (N)) then
                  To_Node := Last (Parameter_Specifications (N));
               elsif Present (Entry_Index (N)) then
                  To_Node := Entry_Index (N);
               end if;

            when N_Case_Statement =>
               To_Node := Expression (N);

            when N_If_Statement | N_Elsif_Part =>
               To_Node := Condition (N);

            when N_Extended_Return_Statement =>
               To_Node := Last (Return_Object_Declarations (N));

            when N_Loop_Statement =>
               To_Node := Iteration_Scheme (N);

            when N_Selective_Accept       |
                 N_Timed_Entry_Call       |
                 N_Conditional_Entry_Call |
                 N_Asynchronous_Select    =>
               T := F;

            when others =>
               null;

         end case;

         if Present (To_Node) then
            Sloc_Range (To_Node, Dummy, T);
         end if;

         SC.Append ((N, F, T, Typ));
      end Extend_Statement_Sequence;

      -----------------------------
      -- Process_Decisions_Defer --
      -----------------------------

      procedure Process_Decisions_Defer (N : Node_Id; T : Character) is
      begin
         SD.Append ((N, No_List, T, Current_Pragma_Sloc));
      end Process_Decisions_Defer;

      procedure Process_Decisions_Defer (L : List_Id; T : Character) is
      begin
         SD.Append ((Empty, L, T, Current_Pragma_Sloc));
      end Process_Decisions_Defer;

      ------------------
      -- Traverse_One --
      ------------------

      procedure Traverse_One (N : Node_Id) is
      begin
         --  Initialize or extend current statement sequence. Note that for
         --  special cases such as IF and Case statements we will modify
         --  the range to exclude internal statements that should not be
         --  counted as part of the current statement sequence.

         case Nkind (N) is

            --  Package declaration

            when N_Package_Declaration =>
               Set_Statement_Entry;
               Traverse_Package_Declaration (N);

            --  Generic package declaration

            when N_Generic_Package_Declaration =>
               Set_Statement_Entry;
               Traverse_Generic_Package_Declaration (N);

            --  Package body

            when N_Package_Body =>
               Set_Statement_Entry;
               Traverse_Package_Body (N);

            --  Subprogram declaration

            when N_Subprogram_Declaration =>
               Process_Decisions_Defer
                 (Parameter_Specifications (Specification (N)), 'X');

            --  Generic subprogram declaration

            when N_Generic_Subprogram_Declaration =>
               Process_Decisions_Defer
                 (Generic_Formal_Declarations (N), 'X');
               Process_Decisions_Defer
                 (Parameter_Specifications (Specification (N)), 'X');

            --  Task or subprogram body

            when N_Task_Body | N_Subprogram_Body =>
               Set_Statement_Entry;
               Traverse_Subprogram_Or_Task_Body (N);

            --  Entry body

            when N_Entry_Body =>
               declare
                  Cond : constant Node_Id :=
                           Condition (Entry_Body_Formal_Part (N));

                  Inner_Dominant : Dominant_Info := No_Dominant;

               begin
                  Set_Statement_Entry;

                  if Present (Cond) then
                     Process_Decisions_Defer (Cond, 'G');

                     --  For an entry body with a barrier, the entry body
                     --  is dominanted by a True evaluation of the barrier.

                     Inner_Dominant := ('T', N);
                  end if;

                  Traverse_Subprogram_Or_Task_Body (N, Inner_Dominant);
               end;

            --  Protected body

            when N_Protected_Body =>
               Set_Statement_Entry;
               Traverse_Protected_Body (N);

            --  Exit statement, which is an exit statement in the SCO sense,
            --  so it is included in the current statement sequence, but
            --  then it terminates this sequence. We also have to process
            --  any decisions in the exit statement expression.

            when N_Exit_Statement =>
               Extend_Statement_Sequence (N, ' ');
               Process_Decisions_Defer (Condition (N), 'E');
               Set_Statement_Entry;

               --  If condition is present, then following statement is
               --  only executed if the condition evaluates to False.

               if Present (Condition (N)) then
                  Current_Dominant := ('F', N);
               else
                  Current_Dominant := No_Dominant;
               end if;

            --  Label, which breaks the current statement sequence, but the
            --  label itself is not included in the next statement sequence,
            --  since it generates no code.

            when N_Label =>
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Block statement, which breaks the current statement sequence

            when N_Block_Statement =>
               Set_Statement_Entry;
               Traverse_Declarations_Or_Statements
                 (L => Declarations (N),
                  D => Current_Dominant);
               Traverse_Handled_Statement_Sequence
                 (N => Handled_Statement_Sequence (N),
                  D => Current_Dominant);

            --  If statement, which breaks the current statement sequence,
            --  but we include the condition in the current sequence.

            when N_If_Statement =>
               Current_Test := N;
               Extend_Statement_Sequence (N, 'I');
               Process_Decisions_Defer (Condition (N), 'I');
               Set_Statement_Entry;

               --  Now we traverse the statements in the THEN part

               Traverse_Declarations_Or_Statements
                 (L => Then_Statements (N),
                  D => ('T', N));

               --  Loop through ELSIF parts if present

               if Present (Elsif_Parts (N)) then
                  declare
                     Saved_Dominant : constant Dominant_Info :=
                                        Current_Dominant;

                     Elif : Node_Id := First (Elsif_Parts (N));

                  begin
                     while Present (Elif) loop

                        --  An Elsif is executed only if the previous test
                        --  got a FALSE outcome.

                        Current_Dominant := ('F', Current_Test);

                        --  Now update current test information

                        Current_Test := Elif;

                        --  We generate a statement sequence for the
                        --  construct "ELSIF condition", so that we have
                        --  a statement for the resulting decisions.

                        Extend_Statement_Sequence (Elif, 'I');
                        Process_Decisions_Defer (Condition (Elif), 'I');
                        Set_Statement_Entry;

                        --  An ELSIF part is never guaranteed to have
                        --  been executed, following statements are only
                        --  dominated by the initial IF statement.

                        Current_Dominant := Saved_Dominant;

                        --  Traverse the statements in the ELSIF

                        Traverse_Declarations_Or_Statements
                          (L => Then_Statements (Elif),
                           D => ('T', Elif));
                        Next (Elif);
                     end loop;
                  end;
               end if;

               --  Finally traverse the ELSE statements if present

               Traverse_Declarations_Or_Statements
                 (L => Else_Statements (N),
                  D => ('F', Current_Test));

            --  CASE statement, which breaks the current statement sequence,
            --  but we include the expression in the current sequence.

            when N_Case_Statement =>
               Extend_Statement_Sequence (N, 'C');
               Process_Decisions_Defer (Expression (N), 'X');
               Set_Statement_Entry;

               --  Process case branches, all of which are dominated by the
               --  CASE statement.

               declare
                  Alt : Node_Id;
               begin
                  Alt := First (Alternatives (N));
                  while Present (Alt) loop
                     Traverse_Declarations_Or_Statements
                       (L => Statements (Alt),
                        D => Current_Dominant);
                     Next (Alt);
                  end loop;
               end;

            --  ACCEPT statement

            when N_Accept_Statement =>
               Extend_Statement_Sequence (N, 'A');
               Set_Statement_Entry;

               --  Process sequence of statements, dominant is the ACCEPT
               --  statement.

               Traverse_Handled_Statement_Sequence
                 (N => Handled_Statement_Sequence (N),
                  D => Current_Dominant);

            --  SELECT

            when N_Selective_Accept =>
               Extend_Statement_Sequence (N, 'S');
               Set_Statement_Entry;

               --  Process alternatives

               declare
                  Alt   : Node_Id;
                  Guard : Node_Id;
                  S_Dom : Dominant_Info;

               begin
                  Alt := First (Select_Alternatives (N));
                  while Present (Alt) loop
                     S_Dom := Current_Dominant;
                     Guard := Condition (Alt);

                     if Present (Guard) then
                        Process_Decisions
                          (Guard,
                           'G',
                           Pragma_Sloc => No_Location);
                        Current_Dominant := ('T', Guard);
                     end if;

                     Traverse_One (Alt);

                     Current_Dominant := S_Dom;
                     Next (Alt);
                  end loop;
               end;

               Traverse_Declarations_Or_Statements
                 (L => Else_Statements (N),
                  D => Current_Dominant);

            when N_Timed_Entry_Call | N_Conditional_Entry_Call =>
               Extend_Statement_Sequence (N, 'S');
               Set_Statement_Entry;

               --  Process alternatives

               Traverse_One (Entry_Call_Alternative (N));

               if Nkind (N) = N_Timed_Entry_Call then
                  Traverse_One (Delay_Alternative (N));
               else
                  Traverse_Declarations_Or_Statements
                    (L => Else_Statements (N),
                     D => Current_Dominant);
               end if;

            when N_Asynchronous_Select =>
               Extend_Statement_Sequence (N, 'S');
               Set_Statement_Entry;

               Traverse_One (Triggering_Alternative (N));
               Traverse_Declarations_Or_Statements
                 (L => Statements (Abortable_Part (N)),
                  D => Current_Dominant);

            when N_Accept_Alternative =>
               Traverse_Declarations_Or_Statements
                 (L => Statements (N),
                  D => Current_Dominant,
                  P => Accept_Statement (N));

            when N_Entry_Call_Alternative =>
               Traverse_Declarations_Or_Statements
                 (L => Statements (N),
                  D => Current_Dominant,
                  P => Entry_Call_Statement (N));

            when N_Delay_Alternative =>
               Traverse_Declarations_Or_Statements
                 (L => Statements (N),
                  D => Current_Dominant,
                  P => Delay_Statement (N));

            when N_Triggering_Alternative =>
               Traverse_Declarations_Or_Statements
                 (L => Statements (N),
                  D => Current_Dominant,
                  P => Triggering_Statement (N));

            when N_Terminate_Alternative =>
               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;

            --  Unconditional exit points, which are included in the current
            --  statement sequence, but then terminate it

            when N_Requeue_Statement |
                 N_Goto_Statement    |
                 N_Raise_Statement   =>
               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Simple return statement. which is an exit point, but we
            --  have to process the return expression for decisions.

            when N_Simple_Return_Statement =>
               Extend_Statement_Sequence (N, ' ');
               Process_Decisions_Defer (Expression (N), 'X');
               Set_Statement_Entry;
               Current_Dominant := No_Dominant;

            --  Extended return statement

            when N_Extended_Return_Statement =>
               Extend_Statement_Sequence (N, 'R');
               Process_Decisions_Defer
                 (Return_Object_Declarations (N), 'X');
               Set_Statement_Entry;

               Traverse_Handled_Statement_Sequence
                 (N => Handled_Statement_Sequence (N),
                  D => Current_Dominant);

               Current_Dominant := No_Dominant;

            --  Loop ends the current statement sequence, but we include
            --  the iteration scheme if present in the current sequence.
            --  But the body of the loop starts a new sequence, since it
            --  may not be executed as part of the current sequence.

            when N_Loop_Statement =>
               declare
                  ISC            : constant Node_Id := Iteration_Scheme (N);
                  Inner_Dominant : Dominant_Info    := No_Dominant;

               begin
                  if Present (ISC) then

                     --  If iteration scheme present, extend the current
                     --  statement sequence to include the iteration scheme
                     --  and process any decisions it contains.

                     --  While loop

                     if Present (Condition (ISC)) then
                        Extend_Statement_Sequence (N, 'W');
                        Process_Decisions_Defer (Condition (ISC), 'W');

                        --  Set more specific dominant for inner statements
                        --  (the control sloc for the decision is that of
                        --  the WHILE token).

                        Inner_Dominant := ('T', ISC);

                     --  For loop

                     else
                        Extend_Statement_Sequence (N, 'F');
                        Process_Decisions_Defer
                          (Loop_Parameter_Specification (ISC), 'X');
                     end if;
                  end if;

                  Set_Statement_Entry;

                  if Inner_Dominant = No_Dominant then
                     Inner_Dominant := Current_Dominant;
                  end if;

                  Traverse_Declarations_Or_Statements
                    (L => Statements (N),
                     D => Inner_Dominant);
               end;

            --  Pragma

            when N_Pragma =>

               --  Record sloc of pragma (pragmas don't nest)

               pragma Assert (Current_Pragma_Sloc = No_Location);
               Current_Pragma_Sloc := Sloc (N);

               --  Processing depends on the kind of pragma

               declare
                  Nam : constant Name_Id := Pragma_Name (N);
                  Arg : Node_Id          :=
                          First (Pragma_Argument_Associations (N));
                  Typ : Character;

               begin
                  case Nam is
                     when Name_Assert        |
                          Name_Check         |
                          Name_Precondition  |
                          Name_Postcondition =>

                        --  For Assert/Check/Precondition/Postcondition, we
                        --  must generate a P entry for the decision. Note
                        --  that this is done unconditionally at this stage.
                        --  Output for disabled pragmas is suppressed later
                        --  on when we output the decision line in Put_SCOs,
                        --  depending on setting by Set_SCO_Pragma_Enabled.

                        if Nam = Name_Check then
                           Next (Arg);
                        end if;

                        Process_Decisions_Defer (Expression (Arg), 'P');
                        Typ := 'p';

                     when Name_Debug =>
                        if Present (Arg) and then Present (Next (Arg)) then

                           --  Case of a dyadic pragma Debug: first argument
                           --  is a P decision, any nested decision in the
                           --  second argument is an X decision.

                           Process_Decisions_Defer (Expression (Arg), 'P');
                           Next (Arg);
                        end if;

                        Process_Decisions_Defer (Expression (Arg), 'X');
                        Typ := 'p';

                     --  For all other pragmas, we generate decision entries
                     --  for any embedded expressions, and the pragma is
                     --  never disabled.

                     when others =>
                        Process_Decisions_Defer (N, 'X');
                        Typ := 'P';
                  end case;

                  --  Add statement SCO

                  Extend_Statement_Sequence (N, Typ);

                  Current_Pragma_Sloc := No_Location;
               end;

            --  Object declaration. Ignored if Prev_Ids is set, since the
            --  parser generates multiple instances of the whole declaration
            --  if there is more than one identifier declared, and we only
            --  want one entry in the SCO's, so we take the first, for which
            --  Prev_Ids is False.

            when N_Object_Declaration =>
               if not Prev_Ids (N) then
                  Extend_Statement_Sequence (N, 'o');

                  if Has_Decision (N) then
                     Process_Decisions_Defer (N, 'X');
                  end if;
               end if;

            --  All other cases, which extend the current statement sequence
            --  but do not terminate it, even if they have nested decisions.

            when others =>

               --  Determine required type character code, or ASCII.NUL if
               --  no SCO should be generated for this node.

               declare
                  Typ : Character;

               begin
                  case Nkind (N) is
                     when N_Full_Type_Declaration         |
                          N_Incomplete_Type_Declaration   |
                          N_Private_Type_Declaration      |
                          N_Private_Extension_Declaration =>
                        Typ := 't';

                     when N_Subtype_Declaration           =>
                        Typ := 's';

                     when N_Renaming_Declaration          =>
                        Typ := 'r';

                     when N_Generic_Instantiation         =>
                        Typ := 'i';

                     when N_Representation_Clause         |
                          N_Use_Package_Clause            |
                          N_Use_Type_Clause               =>
                        Typ := ASCII.NUL;

                     when others                          =>
                        Typ := ' ';
                  end case;

                  if Typ /= ASCII.NUL then
                     Extend_Statement_Sequence (N, Typ);
                  end if;
               end;

               --  Process any embedded decisions

               if Has_Decision (N) then
                  Process_Decisions_Defer (N, 'X');
               end if;
         end case;

      end Traverse_One;

   --  Start of processing for Traverse_Declarations_Or_Statements

   begin
      if Present (P) then
         Traverse_One (P);
      end if;

      if Is_Non_Empty_List (L) then

         --  Loop through statements or declarations

         N := First (L);
         while Present (N) loop
            Traverse_One (N);
            Next (N);
         end loop;

         Set_Statement_Entry;
      end if;
   end Traverse_Declarations_Or_Statements;

   ------------------------------------
   -- Traverse_Generic_Instantiation --
   ------------------------------------

   procedure Traverse_Generic_Instantiation (N : Node_Id) is
      First : Source_Ptr;
      Last  : Source_Ptr;

   begin
      --  First we need a statement entry to cover the instantiation

      Sloc_Range (N, First, Last);
      Set_Table_Entry
        (C1   => 'S',
         C2   => ' ',
         From => First,
         To   => Last,
         Last => True);

      --  Now output any embedded decisions

      Process_Decisions (N, 'X', No_Location);
   end Traverse_Generic_Instantiation;

   ------------------------------------------
   -- Traverse_Generic_Package_Declaration --
   ------------------------------------------

   procedure Traverse_Generic_Package_Declaration (N : Node_Id) is
   begin
      Process_Decisions (Generic_Formal_Declarations (N), 'X', No_Location);
      Traverse_Package_Declaration (N);
   end Traverse_Generic_Package_Declaration;

   -----------------------------------------
   -- Traverse_Handled_Statement_Sequence --
   -----------------------------------------

   procedure Traverse_Handled_Statement_Sequence
     (N : Node_Id;
      D : Dominant_Info := No_Dominant)
   is
      Handler : Node_Id;

   begin
      --  For package bodies without a statement part, the parser adds an empty
      --  one, to normalize the representation. The null statement therein,
      --  which does not come from source, does not get a SCO.

      if Present (N) and then Comes_From_Source (N) then
         Traverse_Declarations_Or_Statements (Statements (N), D);

         if Present (Exception_Handlers (N)) then
            Handler := First (Exception_Handlers (N));
            while Present (Handler) loop
               Traverse_Declarations_Or_Statements
                 (L => Statements (Handler),
                  D => ('E', Handler));
               Next (Handler);
            end loop;
         end if;
      end if;
   end Traverse_Handled_Statement_Sequence;

   ---------------------------
   -- Traverse_Package_Body --
   ---------------------------

   procedure Traverse_Package_Body (N : Node_Id) is
   begin
      Traverse_Declarations_Or_Statements (Declarations (N));
      Traverse_Handled_Statement_Sequence (Handled_Statement_Sequence (N));
   end Traverse_Package_Body;

   ----------------------------------
   -- Traverse_Package_Declaration --
   ----------------------------------

   procedure Traverse_Package_Declaration (N : Node_Id) is
      Spec : constant Node_Id := Specification (N);
   begin
      Traverse_Declarations_Or_Statements (Visible_Declarations (Spec));
      Traverse_Declarations_Or_Statements (Private_Declarations (Spec));
   end Traverse_Package_Declaration;

   -----------------------------
   -- Traverse_Protected_Body --
   -----------------------------

   procedure Traverse_Protected_Body (N : Node_Id) is
   begin
      Traverse_Declarations_Or_Statements (Declarations (N));
   end Traverse_Protected_Body;

   --------------------------------------
   -- Traverse_Subprogram_Or_Task_Body --
   --------------------------------------

   procedure Traverse_Subprogram_Or_Task_Body
     (N : Node_Id;
      D : Dominant_Info := No_Dominant)
   is
   begin
      Traverse_Declarations_Or_Statements (Declarations (N), D);
      Traverse_Handled_Statement_Sequence (Handled_Statement_Sequence (N), D);
   end Traverse_Subprogram_Or_Task_Body;

   -------------------------------------
   -- Traverse_Subprogram_Declaration --
   -------------------------------------

   procedure Traverse_Subprogram_Declaration (N : Node_Id) is
      ADN : constant Node_Id := Aux_Decls_Node (Parent (N));
   begin
      Traverse_Declarations_Or_Statements (Config_Pragmas (ADN));
      Traverse_Declarations_Or_Statements (Declarations   (ADN));
      Traverse_Declarations_Or_Statements (Pragmas_After  (ADN));
   end Traverse_Subprogram_Declaration;

end Par_SCO;
