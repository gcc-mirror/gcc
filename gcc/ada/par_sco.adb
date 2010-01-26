------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R _ S C O                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
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

   --------------------------
   -- Condition Hash Table --
   --------------------------

   --  We need to be able to get to conditions quickly for handling the calls
   --  to Set_SCO_Condition efficiently. For this purpose we identify the
   --  conditions in the table by their starting sloc, and use the following
   --  hash table to map from these starting sloc values to SCO_Table indexes.

   type Header_Num is new Integer range 0 .. 996;
   --  Type for hash table headers

   function Hash (F : Source_Ptr) return Header_Num;
   --  Function to Hash source pointer value

   function Equal (F1, F2 : Source_Ptr) return Boolean;
   --  Function to test two keys for equality

   package Condition_Hash_Table is new Simple_HTable
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

   procedure Process_Decisions (N : Node_Id; T : Character);
   --  If N is Empty, has no effect. Otherwise scans the tree for the node N,
   --  to output any decisions it contains. T is one of IEWX (for context of
   --  expresion: if/while/when-exit/expression). If T is other than X, then
   --  the node is always a decision a decision is always present (at the very
   --  least a simple decision is present at the top level).

   procedure Process_Decisions (L : List_Id; T : Character);
   --  Calls above procedure for each element of the list L

   procedure Set_Table_Entry
     (C1   : Character;
      C2   : Character;
      From : Source_Ptr;
      To   : Source_Ptr;
      Last : Boolean);
   --  Append an entry to SCO_Table with fields set as per arguments

   procedure Traverse_Declarations_Or_Statements  (L : List_Id);
   procedure Traverse_Generic_Package_Declaration (N : Node_Id);
   procedure Traverse_Handled_Statement_Sequence  (N : Node_Id);
   procedure Traverse_Package_Body                (N : Node_Id);
   procedure Traverse_Package_Declaration         (N : Node_Id);
   procedure Traverse_Subprogram_Body             (N : Node_Id);
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
      return Nkind_In (N, N_Op_Xor,
                          N_Op_Not,
                          N_And_Then,
                          N_Or_Else);
   end Is_Logical_Operator;

   -----------------------
   -- Process_Decisions --
   -----------------------

   --  Version taking a list

   procedure Process_Decisions (L : List_Id; T : Character) is
      N : Node_Id;
   begin
      if L /= No_List then
         N := First (L);
         while Present (N) loop
            Process_Decisions (N, T);
            Next (N);
         end loop;
      end if;
   end Process_Decisions;

   --  Version taking a node

   procedure Process_Decisions (N : Node_Id; T : Character) is

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

      procedure Output_Element (N : Node_Id; T : Character);
      --  Node N is an operand of a logical operator that is not itself a
      --  logical operator, or it is a simple decision. This routine outputs
      --  the table entry for the element, with C1 set to T (' ' for one of
      --  the elements of a complex decision, or 'I'/'W'/'E' for a simple
      --  decision (from an IF, WHILE, or EXIT WHEN). Last is set to False,
      --  and an entry is made in the condition hash table.

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

               if Nkind (N) = N_Op_Xor then
                  C := '^';
               elsif Nkind_In (N, N_Op_Or, N_Or_Else) then
                  C := '|';
               else
                  C := '&';
               end if;
            end if;

            Set_Table_Entry (C, ' ', No_Location, No_Location, False);

            Output_Decision_Operand (L);
            Output_Decision_Operand (Right_Opnd (N));

         --  Not a logical operator

         else
            Output_Element (N, ' ');
         end if;
      end Output_Decision_Operand;

      --------------------
      -- Output_Element --
      --------------------

      procedure Output_Element (N : Node_Id; T : Character) is
         FSloc : Source_Ptr;
         LSloc : Source_Ptr;
      begin
         Sloc_Range (N, FSloc, LSloc);
         Set_Table_Entry (T, 'c', FSloc, LSloc, False);
         Condition_Hash_Table.Set (FSloc, SCO_Table.Last);
      end Output_Element;

      ------------------------------
      -- Process_Decision_Operand --
      ------------------------------

      procedure Process_Decision_Operand (N : Node_Id) is
      begin
         if Is_Logical_Operator (N) then
            if Nkind (N) /= N_Op_Not then
               Process_Decision_Operand (Left_Opnd (N));
            end if;

            Process_Decision_Operand (Right_Opnd (N));

         else
            Process_Decisions (N, 'X');
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

            when N_And_Then                    |
                 N_Or_Else                     |
                 N_Op_Not                      =>

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

                  Set_Table_Entry (T, ' ', No_Location, No_Location, False);

                  --  Output the decision

                  Output_Decision_Operand (N);

                  --  Change Last in last table entry to True to mark end

                  SCO_Table.Table (SCO_Table.Last).Last := True;

                  --  Process any embedded decisions

                  Process_Decision_Operand (N);
                  return Skip;
               end;

            --  Conditional expression, processed like an if statement

            when N_Conditional_Expression      =>
               declare
                  Cond : constant Node_Id := First (Expressions (N));
                  Thnx : constant Node_Id := Next (Cond);
                  Elsx : constant Node_Id := Next (Thnx);
               begin
                  Process_Decisions (Cond, 'I');
                  Process_Decisions (Thnx, 'X');
                  Process_Decisions (Elsx, 'X');
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
      --  or short circuit form) appearing as the operand of an IF, WHILE
      --  or EXIT WHEN construct.

      if T /= 'X' and then not Is_Logical_Operator (N) then
         Output_Element (N, T);

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

      if Nkind (Lu) = N_Subprogram_Body then
         Traverse_Subprogram_Body (Lu);

      elsif Nkind (Lu) = N_Package_Declaration then
         Traverse_Package_Declaration (Lu);

      elsif Nkind (Lu) = N_Package_Body then
         Traverse_Package_Body (Lu);

      elsif Nkind (Lu) = N_Generic_Package_Declaration then
         Traverse_Generic_Package_Declaration (Lu);

      --  For anything else, the only issue is default expressions for
      --  parameters, where we have to worry about possible embedded decisions
      --  but nothing else.

      else
         Process_Decisions (Lu, 'X');
      end if;

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

   procedure Set_SCO_Condition (First_Loc : Source_Ptr; Typ : Character) is
      Index : constant Nat := Condition_Hash_Table.Get (First_Loc);
   begin
      if Index /= 0 then
         SCO_Table.Table (Index).C2 := Typ;
      end if;
   end Set_SCO_Condition;

   ---------------------
   -- Set_Table_Entry --
   ---------------------

   procedure Set_Table_Entry
     (C1   : Character;
      C2   : Character;
      From : Source_Ptr;
      To   : Source_Ptr;
      Last : Boolean)
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
      Add_SCO
        (C1   => C1,
         C2   => C2,
         From => To_Source_Location (From),
         To   => To_Source_Location (To),
         Last => Last);
   end Set_Table_Entry;

   -----------------------------------------
   -- Traverse_Declarations_Or_Statements --
   -----------------------------------------

   procedure Traverse_Declarations_Or_Statements (L : List_Id) is
      N     : Node_Id;
      Start : Source_Ptr;
      Dummy : Source_Ptr;
      Stop  : Source_Ptr;
      From  : Source_Ptr;
      To    : Source_Ptr;

      Term  : Boolean;
      --  Set False if current entity terminates statement list

      procedure Set_Statement_Entry;
      --  If Start is No_Location, does nothing, otherwise outputs a SCO_Table
      --  statement entry for the range Start-Stop and then sets both Start
      --  and Stop to No_Location. Unconditionally sets Term to True. This is
      --  called when we find a statement or declaration that generates its
      --  own table entry, so that we must end the current statement sequence.

      -------------------------
      -- Set_Statement_Entry --
      -------------------------

      procedure Set_Statement_Entry is
      begin
         Term := True;

         if Start /= No_Location then
            Set_Table_Entry ('S', ' ', Start, Stop, False);
            Start := No_Location;
            Stop  := No_Location;
         end if;
      end Set_Statement_Entry;

   --  Start of processing for Traverse_Declarations_Or_Statements

   begin
      if Is_Non_Empty_List (L) then
         N := First (L);
         Start := No_Location;

         --  Loop through statements or declarations

         while Present (N) loop
            Term := False;

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
                  Set_Statement_Entry;
                  Process_Decisions
                    (Parameter_Specifications (Specification (N)), 'X');

               --  Generic subprogram declaration

               when N_Generic_Subprogram_Declaration =>
                  Set_Statement_Entry;
                  Process_Decisions (Generic_Formal_Declarations (N), 'X');
                  Process_Decisions
                    (Parameter_Specifications (Specification (N)), 'X');

               --  Subprogram_Body

               when N_Subprogram_Body =>
                  Set_Statement_Entry;
                  Traverse_Subprogram_Body (N);

               --  Exit statement

               when N_Exit_Statement =>
                  Set_Statement_Entry;
                  Process_Decisions (Condition (N), 'E');

                  --  This is an exit point

                  Sloc_Range (N, From, To);
                  Set_Table_Entry ('T', ' ', From, To, False);

               --  Label (breaks statement sequence)

               when N_Label =>
                  Set_Statement_Entry;

               --  Block statement

               when N_Block_Statement =>
                  Set_Statement_Entry;
                  Traverse_Declarations_Or_Statements (Declarations (N));
                  Traverse_Handled_Statement_Sequence
                    (Handled_Statement_Sequence (N));

               --  If statement

               when N_If_Statement =>
                  Set_Statement_Entry;
                  Process_Decisions (Condition (N), 'I');
                  Traverse_Declarations_Or_Statements (Then_Statements (N));

                  if Present (Elsif_Parts (N)) then
                     declare
                        Elif : Node_Id := First (Elsif_Parts (N));
                     begin
                        while Present (Elif) loop
                           Process_Decisions (Condition (Elif), 'I');
                           Traverse_Declarations_Or_Statements
                             (Then_Statements (Elif));
                           Next (Elif);
                        end loop;
                     end;
                  end if;

                  Traverse_Declarations_Or_Statements (Else_Statements (N));

               --  Case statement

               when N_Case_Statement =>

                  --  We include the expression, but not any of the case
                  --  branches in the generated statement sequence that
                  --  includes this case statement.

                  Sloc_Range (Expression (N), Dummy, Stop);
                  Set_Statement_Entry;
                  Process_Decisions (Expression (N), 'X');

                  --  Process case branches

                  declare
                     Alt : Node_Id;

                  begin
                     Alt := First (Alternatives (N));
                     while Present (Alt) loop
                        Traverse_Declarations_Or_Statements (Statements (Alt));
                        Next (Alt);
                     end loop;
                  end;

               --  Unconditional exit points

               when N_Requeue_Statement |
                    N_Goto_Statement    |
                    N_Raise_Statement   =>
                  Set_Statement_Entry;
                  Sloc_Range (N, From, To);
                  Set_Table_Entry ('T', ' ', From, To, False);

               --  Simple return statement

               when N_Simple_Return_Statement =>
                  Set_Statement_Entry;

                  --  Process possible return expression

                  Process_Decisions (Expression (N), 'X');

                  --  Return is an exit point

                  Sloc_Range (N, From, To);
                  Set_Table_Entry ('T', ' ', From, To, False);

               --  Extended return statement

               when N_Extended_Return_Statement =>
                  Set_Statement_Entry;
                  Traverse_Declarations_Or_Statements
                    (Return_Object_Declarations (N));
                  Traverse_Handled_Statement_Sequence
                    (Handled_Statement_Sequence (N));

                  --  Return is an exit point

                  Sloc_Range (N, From, To);
                  Set_Table_Entry ('T', ' ', From, To, False);

               --  Loop

               when N_Loop_Statement =>

                  --  Even if not a while loop, we want a new statement seq

                  Set_Statement_Entry;

                  if Present (Iteration_Scheme (N)) then
                     Process_Decisions
                       (Condition (Iteration_Scheme (N)), 'W');
                  end if;

                  Traverse_Declarations_Or_Statements (Statements (N));

               --  All other cases

               when others =>
                  if Has_Decision (N) then
                     Set_Statement_Entry;
                     Process_Decisions (N, 'X');
                  end if;
            end case;

            --  If that element did not terminate the current sequence of
            --  statements, then establish or extend this sequence.

            if not Term then
               if Start = No_Location then
                  Sloc_Range (N, Start, Stop);
               else
                  Sloc_Range (N, Dummy, Stop);
               end if;
            end if;

            Next (N);
         end loop;

         Set_Statement_Entry;
      end if;
   end Traverse_Declarations_Or_Statements;

   ------------------------------------------
   -- Traverse_Generic_Package_Declaration --
   ------------------------------------------

   procedure Traverse_Generic_Package_Declaration (N : Node_Id) is
   begin
      Process_Decisions (Generic_Formal_Declarations (N), 'X');
      Traverse_Package_Declaration (N);
   end Traverse_Generic_Package_Declaration;

   -----------------------------------------
   -- Traverse_Handled_Statement_Sequence --
   -----------------------------------------

   procedure Traverse_Handled_Statement_Sequence (N : Node_Id) is
      Handler : Node_Id;

   begin

      --  For package bodies without a statement part, the parser adds an empty
      --  one, to normalize the representation. The null statement therein,
      --  which does not come from source, does not get a SCO.

      if Present (N) and then Comes_From_Source (N) then
         Traverse_Declarations_Or_Statements (Statements (N));

         if Present (Exception_Handlers (N)) then
            Handler := First (Exception_Handlers (N));
            while Present (Handler) loop
               Traverse_Declarations_Or_Statements (Statements (Handler));
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

   ------------------------------
   -- Traverse_Subprogram_Body --
   ------------------------------

   procedure Traverse_Subprogram_Body (N : Node_Id) is
   begin
      Traverse_Declarations_Or_Statements (Declarations (N));
      Traverse_Handled_Statement_Sequence (Handled_Statement_Sequence (N));
   end Traverse_Subprogram_Body;

end Par_SCO;
