------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R _ S C O                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2009-2020, Free Software Foundation, Inc.         --
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

with Aspects;  use Aspects;
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
with Sem;      use Sem;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Table;

with GNAT.HTable;      use GNAT.HTable;
with GNAT.Heap_Sort_G;

package body Par_SCO is

   --------------------------
   -- First-pass SCO table --
   --------------------------

   --  The Short_Circuit_And_Or pragma enables one to use AND and OR operators
   --  in source code while the ones used with booleans will be interpreted as
   --  their short circuit alternatives (AND THEN and OR ELSE). Thus, the true
   --  meaning of these operators is known only after the semantic analysis.

   --  However, decision SCOs include short circuit operators only. The SCO
   --  information generation pass must be done before expansion, hence before
   --  the semantic analysis. Because of this, the SCO information generation
   --  is done in two passes.

   --  The first one (SCO_Record_Raw, before semantic analysis) completes the
   --  SCO_Raw_Table assuming all AND/OR operators are short circuit ones.
   --  Then, the semantic analysis determines which operators are promoted to
   --  short circuit ones. Finally, the second pass (SCO_Record_Filtered)
   --  translates the SCO_Raw_Table to SCO_Table, taking care of removing the
   --  remaining AND/OR operators and of adjusting decisions accordingly
   --  (splitting decisions, removing empty ones, etc.).

   type SCO_Generation_State_Type is (None, Raw, Filtered);
   SCO_Generation_State : SCO_Generation_State_Type := None;
   --  Keep track of the SCO generation state: this will prevent us from
   --  running some steps multiple times (the second pass has to be started
   --  from multiple places).

   package SCO_Raw_Table is new Table.Table
     (Table_Component_Type => SCO_Table_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 300,
      Table_Name           => "Raw_Table");

   -----------------------
   -- Unit Number Table --
   -----------------------

   --  This table parallels the SCO_Unit_Table, keeping track of the unit
   --  numbers corresponding to the entries made in this table, so that before
   --  writing out the SCO information to the ALI file, we can fill in the
   --  proper dependency numbers and file names.

   --  Note that the zeroth entry is here for convenience in sorting the table;
   --  the real lower bound is 1.

   package SCO_Unit_Number_Table is new Table.Table
     (Table_Component_Type => Unit_Number_Type,
      Table_Index_Type     => SCO_Unit_Index,
      Table_Low_Bound      => 0, -- see note above on sort
      Table_Initial        => 20,
      Table_Increment      => 200,
      Table_Name           => "SCO_Unit_Number_Entry");

   ------------------------------------------
   -- Condition/Operator/Pragma Hash Table --
   ------------------------------------------

   --  We need to be able to get to conditions quickly for handling the calls
   --  to Set_SCO_Condition efficiently, and similarly to get to pragmas to
   --  handle calls to Set_SCO_Pragma_Enabled (the same holds for operators and
   --  Set_SCO_Logical_Operator). For this purpose we identify the conditions,
   --  operators and pragmas in the table by their starting sloc, and use this
   --  hash table to map from these sloc values to SCO_Table indexes.

   type Header_Num is new Integer range 0 .. 996;
   --  Type for hash table headers

   function Hash (F : Source_Ptr) return Header_Num;
   --  Function to Hash source pointer value

   function Equal (F1 : Source_Ptr; F2 : Source_Ptr) return Boolean;
   --  Function to test two keys for equality

   function "<" (S1 : Source_Location; S2 : Source_Location) return Boolean;
   --  Function to test for source locations order

   package SCO_Raw_Hash_Table is new Simple_HTable
     (Header_Num, Int, 0, Source_Ptr, Hash, Equal);
   --  The actual hash table

   --------------------------
   -- Internal Subprograms --
   --------------------------

   function Has_Decision (N : Node_Id) return Boolean;
   --  N is the node for a subexpression. Returns True if the subexpression
   --  contains a nested decision (i.e. either is a logical operator, or
   --  contains a logical operator in its subtree).
   --
   --  This must be used in the first pass (SCO_Record_Raw) only: here AND/OR
   --  operators are considered as short circuit, just in case the
   --  Short_Circuit_And_Or pragma is used: only real short circuit operations
   --  will be kept in the secord pass.

   type Tristate is (False, True, Unknown);

   function Is_Logical_Operator (N : Node_Id) return Tristate;
   --  N is the node for a subexpression. This procedure determines whether N
   --  is a logical operator: True for short circuit conditions, Unknown for OR
   --  and AND (the Short_Circuit_And_Or pragma may be used) and False
   --  otherwise. Note that in cases where True is returned, callers assume
   --  Nkind (N) in N_Op.

   function To_Source_Location (S : Source_Ptr) return Source_Location;
   --  Converts Source_Ptr value to Source_Location (line/col) format

   procedure Process_Decisions
     (N           : Node_Id;
      T           : Character;
      Pragma_Sloc : Source_Ptr);
   --  If N is Empty, has no effect. Otherwise scans the tree for the node N,
   --  to output any decisions it contains. T is one of IEGPWX (for context of
   --  expression: if/exit when/entry guard/pragma/while/expression). If T is
   --  other than X, the node N is the if expression involved, and a decision
   --  is always present (at the very least a simple decision is present at the
   --  top level).

   procedure Process_Decisions
     (L           : List_Id;
      T           : Character;
      Pragma_Sloc : Source_Ptr);
   --  Calls above procedure for each element of the list L

   procedure Set_Raw_Table_Entry
     (C1                 : Character;
      C2                 : Character;
      From               : Source_Ptr;
      To                 : Source_Ptr;
      Last               : Boolean;
      Pragma_Sloc        : Source_Ptr := No_Location;
      Pragma_Aspect_Name : Name_Id    := No_Name);
   --  Append an entry to SCO_Raw_Table with fields set as per arguments

   type Dominant_Info is record
      K : Character;
      --  F/T/S/E for a valid dominance marker, or ' ' for no dominant

      N : Node_Id;
      --  Node providing the Sloc(s) for the dominance marker
   end record;
   No_Dominant : constant Dominant_Info := (' ', Empty);

   procedure Record_Instance (Id : Instance_Id; Inst_Sloc : Source_Ptr);
   --  Add one entry from the instance table to the corresponding SCO table

   procedure Traverse_Declarations_Or_Statements
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty);
   --  Process L, a list of statements or declarations dominated by D. If P is
   --  present, it is processed as though it had been prepended to L.

   function Traverse_Declarations_Or_Statements
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty) return Dominant_Info;
   --  Same as above, and returns dominant information corresponding to the
   --  last node with SCO in L.

   --  The following Traverse_* routines perform appropriate calls to
   --  Traverse_Declarations_Or_Statements to traverse specific node kinds.
   --  Parameter D, when present, indicates the dominant of the first
   --  declaration or statement within N.

   --  Why is Traverse_Sync_Definition commented specifically, whereas
   --  the others are not???

   procedure Traverse_Generic_Package_Declaration (N : Node_Id);

   procedure Traverse_Handled_Statement_Sequence
     (N : Node_Id;
      D : Dominant_Info := No_Dominant);

   procedure Traverse_Package_Body (N : Node_Id);

   procedure Traverse_Package_Declaration
     (N : Node_Id;
      D : Dominant_Info := No_Dominant);

   procedure Traverse_Subprogram_Or_Task_Body
     (N : Node_Id;
      D : Dominant_Info := No_Dominant);

   procedure Traverse_Sync_Definition (N : Node_Id);
   --  Traverse a protected definition or task definition

   --  Note regarding traversals: In a few cases where an Alternatives list is
   --  involved, pragmas such as "pragma Page" may show up before the first
   --  alternative. We skip them because we're out of statement or declaration
   --  context, so these can't be pragmas of interest for SCO purposes, and
   --  the regular alternative processing typically involves attribute queries
   --  which aren't valid for a pragma.

   procedure Write_SCOs_To_ALI_File is new Put_SCOs;
   --  Write SCO information to the ALI file using routines in Lib.Util

   ----------
   -- dsco --
   ----------

   procedure dsco is
      procedure Dump_Entry (Index : Nat; T : SCO_Table_Entry);
      --  Dump a SCO table entry

      ----------------
      -- Dump_Entry --
      ----------------

      procedure Dump_Entry (Index : Nat; T : SCO_Table_Entry) is
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
      end Dump_Entry;

   --  Start of processing for dsco

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
            Write_Str ("  Dep_Num = ");
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

      --  Dump SCO raw-table

      Write_Eol;
      Write_Line ("SCO Raw Table");
      Write_Line ("---------");

      if SCO_Generation_State = Filtered then
         Write_Line ("Empty (free'd after second pass)");
      else
         for Index in 1 .. SCO_Raw_Table.Last loop
            Dump_Entry (Index, SCO_Raw_Table.Table (Index));
         end loop;
      end if;

      --  Dump SCO table itself

      Write_Eol;
      Write_Line ("SCO Filtered Table");
      Write_Line ("---------");

      for Index in 1 .. SCO_Table.Last loop
         Dump_Entry (Index, SCO_Table.Table (Index));
      end loop;
   end dsco;

   -----------
   -- Equal --
   -----------

   function Equal (F1 : Source_Ptr; F2 : Source_Ptr) return Boolean is
   begin
      return F1 = F2;
   end Equal;

   -------
   -- < --
   -------

   function "<" (S1 : Source_Location; S2 : Source_Location) return Boolean is
   begin
      return S1.Line < S2.Line
        or else (S1.Line = S2.Line and then S1.Col < S2.Col);
   end "<";

   ------------------
   -- Has_Decision --
   ------------------

   function Has_Decision (N : Node_Id) return Boolean is
      function Check_Node (N : Node_Id) return Traverse_Result;
      --  Determine if Nkind (N) indicates the presence of a decision (i.e. N
      --  is a logical operator, which is a decision in itself, or an
      --  IF-expression whose Condition attribute is a decision).

      ----------------
      -- Check_Node --
      ----------------

      function Check_Node (N : Node_Id) return Traverse_Result is
      begin
         --  If we are not sure this is a logical operator (AND and OR may be
         --  turned into logical operators with the Short_Circuit_And_Or
         --  pragma), assume it is. Putative decisions will be discarded if
         --  needed in the secord pass.

         if Is_Logical_Operator (N) /= False
           or else Nkind (N) = N_If_Expression
         then
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

      --  The SCO_Unit_Number_Table entry with index 0 is intentionally set
      --  aside to be used as temporary for sorting.

      SCO_Unit_Number_Table.Increment_Last;
   end Initialize;

   -------------------------
   -- Is_Logical_Operator --
   -------------------------

   function Is_Logical_Operator (N : Node_Id) return Tristate is
   begin
      if Nkind_In (N, N_And_Then, N_Op_Not, N_Or_Else) then
         return True;
      elsif Nkind_In (N, N_Op_And, N_Op_Or) then
         return Unknown;
      else
         return False;
      end if;
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

      Mark_Hash : Nat;
      --  Likewise for the putative SCO_Raw_Hash_Table entries: see below

      type Hash_Entry is record
         Sloc      : Source_Ptr;
         SCO_Index : Nat;
      end record;
      --  We must register all conditions/pragmas in SCO_Raw_Hash_Table.
      --  However we cannot register them in the same time we are adding the
      --  corresponding SCO entries to the raw table since we may discard them
      --  later on. So instead we put all putative conditions into Hash_Entries
      --  (see below) and register them once we are sure we keep them.
      --
      --  This data structure holds the conditions/pragmas to register in
      --  SCO_Raw_Hash_Table.

      package Hash_Entries is new Table.Table
        (Table_Component_Type => Hash_Entry,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 10,
         Table_Name           => "Hash_Entries");
      --  Hold temporarily (i.e. free'd before returning) the Hash_Entry before
      --  they are registered in SCO_Raw_Hash_Table.

      X_Not_Decision : Boolean;
      --  This flag keeps track of whether a decision sequence in the SCO table
      --  contains only NOT operators, and is for an expression context (T=X).
      --  The flag will be set False if T is other than X, or if an operator
      --  other than NOT is in the sequence.

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

      function Process_Node (N : Node_Id) return Traverse_Result;
      --  Processes one node in the traversal, looking for logical operators,
      --  and if one is found, outputs the appropriate table entries.

      -----------------------------
      -- Output_Decision_Operand --
      -----------------------------

      procedure Output_Decision_Operand (N : Node_Id) is
         C1 : Character;
         C2 : Character;
         --  C1 holds a character that identifies the operation while C2
         --  indicates whether we are sure (' ') or not ('?') this operation
         --  belongs to the decision. '?' entries will be filtered out in the
         --  second (SCO_Record_Filtered) pass.

         L : Node_Id;
         T : Tristate;

      begin
         if No (N) then
            return;
         end if;

         T := Is_Logical_Operator (N);

         --  Logical operator

         if T /= False then
            if Nkind (N) = N_Op_Not then
               C1 := '!';
               L := Empty;

            else
               L := Left_Opnd (N);

               if Nkind_In (N, N_Op_Or, N_Or_Else) then
                  C1 := '|';
               else pragma Assert (Nkind_In (N, N_Op_And, N_And_Then));
                  C1 := '&';
               end if;
            end if;

            if T = True then
               C2 := ' ';
            else
               C2 := '?';
            end if;

            Set_Raw_Table_Entry
              (C1   => C1,
               C2   => C2,
               From => Sloc (N),
               To   => No_Location,
               Last => False);

            Hash_Entries.Append ((Sloc (N), SCO_Raw_Table.Last));

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
         Set_Raw_Table_Entry
           (C1   => ' ',
            C2   => 'c',
            From => FSloc,
            To   => LSloc,
            Last => False);
         Hash_Entries.Append ((FSloc, SCO_Raw_Table.Last));
      end Output_Element;

      -------------------
      -- Output_Header --
      -------------------

      procedure Output_Header (T : Character) is
         Loc : Source_Ptr := No_Location;
         --  Node whose Sloc is used for the decision

         Nam : Name_Id := No_Name;
         --  For the case of an aspect, aspect name

      begin
         case T is
            when 'I' | 'E' | 'W' | 'a' | 'A' =>

               --  For IF, EXIT, WHILE, or aspects, the token SLOC is that of
               --  the parent of the expression.

               Loc := Sloc (Parent (N));

               if T = 'a' or else T = 'A' then
                  Nam := Chars (Identifier (Parent (N)));
               end if;

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

         Set_Raw_Table_Entry
           (C1                 => T,
            C2                 => ' ',
            From               => Loc,
            To                 => No_Location,
            Last               => False,
            Pragma_Sloc        => Pragma_Sloc,
            Pragma_Aspect_Name => Nam);

         --  For an aspect specification, which will be rewritten into a
         --  pragma, enter a hash table entry now.

         if T = 'a' then
            Hash_Entries.Append ((Loc, SCO_Raw_Table.Last));
         end if;
      end Output_Header;

      ------------------------------
      -- Process_Decision_Operand --
      ------------------------------

      procedure Process_Decision_Operand (N : Node_Id) is
      begin
         if Is_Logical_Operator (N) /= False then
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

            when N_And_Then
               | N_Op_And
               | N_Op_Not
               | N_Op_Or
               | N_Or_Else
            =>
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
                  Mark      := SCO_Raw_Table.Last;
                  Mark_Hash := Hash_Entries.Last;
                  Output_Header (T);

                  --  Output the decision

                  Output_Decision_Operand (N);

                  --  If the decision was in an expression context (T = 'X')
                  --  and contained only NOT operators, then we don't output
                  --  it, so delete it.

                  if X_Not_Decision then
                     SCO_Raw_Table.Set_Last (Mark);
                     Hash_Entries.Set_Last (Mark_Hash);

                  --  Otherwise, set Last in last table entry to mark end

                  else
                     SCO_Raw_Table.Table (SCO_Raw_Table.Last).Last := True;
                  end if;

                  --  Process any embedded decisions

                  Process_Decision_Operand (N);
                  return Skip;
               end;

            --  Case expression

            --  Really hard to believe this is correct given the special
            --  handling for if expressions below ???

            when N_Case_Expression =>
               return OK; -- ???

            --  If expression, processed like an if statement

            when N_If_Expression =>
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

      Hash_Entries.Init;

      --  See if we have simple decision at outer level and if so then
      --  generate the decision entry for this simple decision. A simple
      --  decision is a boolean expression (which is not a logical operator
      --  or short circuit form) appearing as the operand of an IF, WHILE,
      --  EXIT WHEN, or special PRAGMA construct.

      if T /= 'X' and then Is_Logical_Operator (N) = False then
         Output_Header (T);
         Output_Element (N);

         --  Change Last in last table entry to True to mark end of
         --  sequence, which is this case is only one element long.

         SCO_Raw_Table.Table (SCO_Raw_Table.Last).Last := True;
      end if;

      Traverse (N);

      --  Now we have the definitive set of SCO entries, register them in the
      --  corresponding hash table.

      for J in 1 .. Hash_Entries.Last loop
         SCO_Raw_Hash_Table.Set
           (Hash_Entries.Table (J).Sloc,
            Hash_Entries.Table (J).SCO_Index);
      end loop;

      Hash_Entries.Free;
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

   ---------------------
   -- Record_Instance --
   ---------------------

   procedure Record_Instance (Id : Instance_Id; Inst_Sloc : Source_Ptr) is
      Inst_Src  : constant Source_File_Index :=
                    Get_Source_File_Index (Inst_Sloc);
   begin
      SCO_Instance_Table.Append
        ((Inst_Dep_Num       => Dependency_Num (Unit (Inst_Src)),
          Inst_Loc           => To_Source_Location (Inst_Sloc),
          Enclosing_Instance => SCO_Instance_Index (Instance (Inst_Src))));

      pragma Assert
        (SCO_Instance_Table.Last = SCO_Instance_Index (Id));
   end Record_Instance;

   ----------------
   -- SCO_Output --
   ----------------

   procedure SCO_Output is
      procedure Populate_SCO_Instance_Table is
        new Sinput.Iterate_On_Instances (Record_Instance);

   begin
      pragma Assert (SCO_Generation_State = Filtered);

      if Debug_Flag_Dot_OO then
         dsco;
      end if;

      Populate_SCO_Instance_Table;

      --  Sort the unit tables based on dependency numbers

      Unit_Table_Sort : declare
         function Lt (Op1 : Natural; Op2 : Natural) return Boolean;
         --  Comparison routine for sort call

         procedure Move (From : Natural; To : Natural);
         --  Move routine for sort call

         --------
         -- Lt --
         --------

         function Lt (Op1 : Natural; Op2 : Natural) return Boolean is
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

      Index := SCO_Raw_Hash_Table.Get (Loc);

      --  The test here for zero is to deal with possible previous errors, and
      --  for the case of pragma statement SCOs, for which we always set the
      --  Pragma_Sloc even if the particular pragma cannot be specifically
      --  disabled.

      if Index /= 0 then
         declare
            T : SCO_Table_Entry renames SCO_Raw_Table.Table (Index);

         begin
            case T.C1 is
               when 'S' =>
                  --  Pragma statement

                  return T.C2 = 'p';

               when 'A' =>
                  --  Aspect decision (enabled)

                  return False;

               when 'a' =>
                  --  Aspect decision (not enabled)

                  return True;

               when ASCII.NUL =>
                  --  Nullified disabled SCO

                  return True;

               when others =>
                  raise Program_Error;
            end case;
         end;

      else
         return False;
      end if;
   end SCO_Pragma_Disabled;

   --------------------
   -- SCO_Record_Raw --
   --------------------

   procedure SCO_Record_Raw (U : Unit_Number_Type) is
      procedure Traverse_Aux_Decls (N : Node_Id);
      --  Traverse the Aux_Decls_Node of compilation unit N

      ------------------------
      -- Traverse_Aux_Decls --
      ------------------------

      procedure Traverse_Aux_Decls (N : Node_Id) is
         ADN : constant Node_Id := Aux_Decls_Node (N);

      begin
         Traverse_Declarations_Or_Statements (Config_Pragmas (ADN));
         Traverse_Declarations_Or_Statements (Pragmas_After  (ADN));

         --  Declarations and Actions do not correspond to source constructs,
         --  they contain only nodes from expansion, so at this point they
         --  should still be empty:

         pragma Assert (No (Declarations (ADN)));
         pragma Assert (No (Actions (ADN)));
      end Traverse_Aux_Decls;

      --  Local variables

      From : Nat;
      Lu   : Node_Id;

   --  Start of processing for SCO_Record_Raw

   begin
      --  It is legitimate to run this pass multiple times (once per unit) so
      --  run it even if it was already run before.

      pragma Assert (SCO_Generation_State in None .. Raw);
      SCO_Generation_State := Raw;

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

      From := SCO_Raw_Table.Last + 1;

      --  Get Unit (checking case of subunit)

      Lu := Unit (Cunit (U));

      if Nkind (Lu) = N_Subunit then
         Lu := Proper_Body (Lu);
      end if;

      --  Traverse the unit

      Traverse_Aux_Decls (Cunit (U));

      case Nkind (Lu) is
         when N_Generic_Instantiation
            | N_Generic_Package_Declaration
            | N_Package_Body
            | N_Package_Declaration
            | N_Protected_Body
            | N_Subprogram_Body
            | N_Subprogram_Declaration
            | N_Task_Body
         =>
            Traverse_Declarations_Or_Statements (L => No_List, P => Lu);

         --  All other cases of compilation units (e.g. renamings), generate no
         --  SCO information.

         when others =>
            null;
      end case;

      --  Make entry for new unit in unit tables, we will fill in the file
      --  name and dependency numbers later.

      SCO_Unit_Table.Append (
        (Dep_Num    => 0,
         File_Name  => null,
         File_Index => Get_Source_File_Index (Sloc (Lu)),
         From       => From,
         To         => SCO_Raw_Table.Last));

      SCO_Unit_Number_Table.Append (U);
   end SCO_Record_Raw;

   -----------------------
   -- Set_SCO_Condition --
   -----------------------

   procedure Set_SCO_Condition (Cond : Node_Id; Val : Boolean) is

      --  SCO annotations are not processed after the filtering pass

      pragma Assert (not Generate_SCO or else SCO_Generation_State = Raw);

      Constant_Condition_Code : constant array (Boolean) of Character :=
                                  (False => 'f', True => 't');

      Orig  : constant Node_Id := Original_Node (Cond);
      Dummy : Source_Ptr;
      Index : Nat;
      Start : Source_Ptr;

   begin
      Sloc_Range (Orig, Start, Dummy);
      Index := SCO_Raw_Hash_Table.Get (Start);

      --  Index can be zero for boolean expressions that do not have SCOs
      --  (simple decisions outside of a control flow structure), or in case
      --  of a previous error.

      if Index = 0 then
         return;

      else
         pragma Assert (SCO_Raw_Table.Table (Index).C1 = ' ');
         SCO_Raw_Table.Table (Index).C2 := Constant_Condition_Code (Val);
      end if;
   end Set_SCO_Condition;

   ------------------------------
   -- Set_SCO_Logical_Operator --
   ------------------------------

   procedure Set_SCO_Logical_Operator (Op : Node_Id) is

      --  SCO annotations are not processed after the filtering pass

      pragma Assert (not Generate_SCO or else SCO_Generation_State = Raw);

      Orig      : constant Node_Id    := Original_Node (Op);
      Orig_Sloc : constant Source_Ptr := Sloc (Orig);
      Index     : constant Nat        := SCO_Raw_Hash_Table.Get (Orig_Sloc);

   begin
      --  All (putative) logical operators are supposed to have their own entry
      --  in the SCOs table. However, the semantic analysis may invoke this
      --  subprogram with nodes that are out of the SCO generation scope.

      if Index /= 0 then
         SCO_Raw_Table.Table (Index).C2 := ' ';
      end if;
   end Set_SCO_Logical_Operator;

   ----------------------------
   -- Set_SCO_Pragma_Enabled --
   ----------------------------

   procedure Set_SCO_Pragma_Enabled (Loc : Source_Ptr) is

      --  SCO annotations are not processed after the filtering pass

      pragma Assert (not Generate_SCO or else SCO_Generation_State = Raw);

      Index : Nat;

   begin
      --  Nothing to do if not generating SCO, or if we're not processing the
      --  original source occurrence of the pragma.

      if not (Generate_SCO
               and then In_Extended_Main_Source_Unit (Loc)
               and then not (In_Instance or In_Inlined_Body))
      then
         return;
      end if;

      --  Note: the reason we use the Sloc value as the key is that in the
      --  generic case, the call to this procedure is made on a copy of the
      --  original node, so we can't use the Node_Id value.

      Index := SCO_Raw_Hash_Table.Get (Loc);

      --  A zero index here indicates that semantic analysis found an
      --  activated pragma at Loc which does not have a corresponding pragma
      --  or aspect at the syntax level. This may occur in legitimate cases
      --  because of expanded code (such are Pre/Post conditions generated for
      --  formal parameter validity checks), or as a consequence of a previous
      --  error.

      if Index = 0 then
         return;

      else
         declare
            T : SCO_Table_Entry renames SCO_Raw_Table.Table (Index);

         begin
            --  Note: may be called multiple times for the same sloc, so
            --  account for the fact that the entry may already have been
            --  marked enabled.

            case T.C1 is
               --  Aspect (decision SCO)

               when 'a' =>
                  T.C1 := 'A';

               when 'A' =>
                  null;

               --  Pragma (statement SCO)

               when 'S' =>
                  pragma Assert (T.C2 = 'p' or else T.C2 = 'P');
                  T.C2 := 'P';

               when others =>
                  raise Program_Error;
            end case;
         end;
      end if;
   end Set_SCO_Pragma_Enabled;

   -------------------------
   -- Set_Raw_Table_Entry --
   -------------------------

   procedure Set_Raw_Table_Entry
     (C1                 : Character;
      C2                 : Character;
      From               : Source_Ptr;
      To                 : Source_Ptr;
      Last               : Boolean;
      Pragma_Sloc        : Source_Ptr := No_Location;
      Pragma_Aspect_Name : Name_Id    := No_Name)
   is
      pragma Assert (SCO_Generation_State = Raw);
   begin
      SCO_Raw_Table.Append
        ((C1                 => C1,
          C2                 => C2,
          From               => To_Source_Location (From),
          To                 => To_Source_Location (To),
          Last               => Last,
          Pragma_Sloc        => Pragma_Sloc,
          Pragma_Aspect_Name => Pragma_Aspect_Name));
   end Set_Raw_Table_Entry;

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

   package SC is new Table.Table
     (Table_Component_Type => SC_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 200,
      Table_Name           => "SCO_SC");
   --  Used to store statement components for a CS entry to be output as a
   --  result of the call to this procedure. SC.Last is the last entry stored,
   --  so the current statement sequence is represented by SC_Array (SC_First
   --  .. SC.Last), where SC_First is saved on entry to each recursive call to
   --  the routine.
   --
   --  Extend_Statement_Sequence adds an entry to this array, and then
   --  Set_Statement_Entry clears the entries starting with SC_First, copying
   --  these entries to the main SCO output table. The reason that we do the
   --  temporary caching of results in this array is that we want the SCO table
   --  entries for a given CS line to be contiguous, and the processing may
   --  output intermediate entries such as decision entries.

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

   package SD is new Table.Table
     (Table_Component_Type => SD_Entry,
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
      Discard_Dom : Dominant_Info;
      pragma Warnings (Off, Discard_Dom);
   begin
      Discard_Dom := Traverse_Declarations_Or_Statements (L, D, P);
   end Traverse_Declarations_Or_Statements;

   function Traverse_Declarations_Or_Statements
     (L : List_Id;
      D : Dominant_Info := No_Dominant;
      P : Node_Id       := Empty) return Dominant_Info
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
      --  Extend the current statement sequence to encompass the node N. Typ is
      --  the letter that identifies the type of statement/declaration that is
      --  being added to the sequence.

      procedure Process_Decisions_Defer (N : Node_Id; T : Character);
      pragma Inline (Process_Decisions_Defer);
      --  This routine is logically the same as Process_Decisions, except that
      --  the arguments are saved in the SD table for later processing when
      --  Set_Statement_Entry is called, which goes through the saved entries
      --  making the corresponding calls to Process_Decision. Note: the
      --  enclosing statement must have already been added to the current
      --  statement sequence, so that nested decisions are properly
      --  identified as such.

      procedure Process_Decisions_Defer (L : List_Id; T : Character);
      pragma Inline (Process_Decisions_Defer);
      --  Same case for list arguments, deferred call to Process_Decisions

      procedure Set_Statement_Entry;
      --  Output CS entries for all statements saved in table SC, and end the
      --  current CS sequence. Then output entries for all decisions nested in
      --  these statements, which have been deferred so far.

      procedure Traverse_One (N : Node_Id);
      --  Traverse one declaration or statement

      procedure Traverse_Aspects (N : Node_Id);
      --  Helper for Traverse_One: traverse N's aspect specifications

      procedure Traverse_Degenerate_Subprogram (N : Node_Id);
      --  Common code to handle null procedures and expression functions. Emit
      --  a SCO of the given Kind and N outside of the dominance flow.

      -------------------------------
      -- Extend_Statement_Sequence --
      -------------------------------

      procedure Extend_Statement_Sequence (N : Node_Id; Typ : Character) is
         Dummy   : Source_Ptr;
         F       : Source_Ptr;
         T       : Source_Ptr;
         To_Node : Node_Id := Empty;

      begin
         Sloc_Range (N, F, T);

         case Nkind (N) is
            when N_Accept_Statement =>
               if Present (Parameter_Specifications (N)) then
                  To_Node := Last (Parameter_Specifications (N));
               elsif Present (Entry_Index (N)) then
                  To_Node := Entry_Index (N);
               else
                  To_Node := Entry_Direct_Name (N);
               end if;

            when N_Case_Statement =>
               To_Node := Expression (N);

            when N_Elsif_Part
               | N_If_Statement
            =>
               To_Node := Condition (N);

            when N_Extended_Return_Statement =>
               To_Node := Last (Return_Object_Declarations (N));

            when N_Loop_Statement =>
               To_Node := Iteration_Scheme (N);

            when N_Asynchronous_Select
               | N_Conditional_Entry_Call
               | N_Selective_Accept
               | N_Single_Protected_Declaration
               | N_Single_Task_Declaration
               | N_Timed_Entry_Call
            =>
               T := F;

            when N_Protected_Type_Declaration
               | N_Task_Type_Declaration
            =>
               if Has_Aspects (N) then
                  To_Node := Last (Aspect_Specifications (N));

               elsif Present (Discriminant_Specifications (N)) then
                  To_Node := Last (Discriminant_Specifications (N));

               else
                  To_Node := Defining_Identifier (N);
               end if;

            when N_Subexpr =>
               To_Node := N;

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
                     From : Source_Ptr;
                     To   : Source_Ptr;

                  begin
                     Sloc_Range (Current_Dominant.N, From, To);

                     if Current_Dominant.K /= 'E' then
                        To := No_Location;
                     end if;

                     Set_Raw_Table_Entry
                       (C1                 => '>',
                        C2                 => Current_Dominant.K,
                        From               => From,
                        To                 => To,
                        Last               => False,
                        Pragma_Sloc        => No_Location,
                        Pragma_Aspect_Name => No_Name);
                  end;
               end if;
            end if;

            declare
               SCE                : SC_Entry renames SC.Table (J);
               Pragma_Sloc        : Source_Ptr := No_Location;
               Pragma_Aspect_Name : Name_Id    := No_Name;

            begin
               --  For the case of a statement SCO for a pragma controlled by
               --  Set_SCO_Pragma_Enabled, set Pragma_Sloc so that the SCO (and
               --  those of any nested decision) is emitted only if the pragma
               --  is enabled.

               if SCE.Typ = 'p' then
                  Pragma_Sloc := SCE.From;
                  SCO_Raw_Hash_Table.Set
                    (Pragma_Sloc, SCO_Raw_Table.Last + 1);
                  Pragma_Aspect_Name := Pragma_Name_Unmapped (SCE.N);
                  pragma Assert (Pragma_Aspect_Name /= No_Name);

               elsif SCE.Typ = 'P' then
                  Pragma_Aspect_Name := Pragma_Name_Unmapped (SCE.N);
                  pragma Assert (Pragma_Aspect_Name /= No_Name);
               end if;

               Set_Raw_Table_Entry
                 (C1                 => 'S',
                  C2                 => SCE.Typ,
                  From               => SCE.From,
                  To                 => SCE.To,
                  Last               => (J = SC_Last),
                  Pragma_Sloc        => Pragma_Sloc,
                  Pragma_Aspect_Name => Pragma_Aspect_Name);
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

      ----------------------
      -- Traverse_Aspects --
      ----------------------

      procedure Traverse_Aspects (N : Node_Id) is
         AE : Node_Id;
         AN : Node_Id;
         C1 : Character;

      begin
         AN := First (Aspect_Specifications (N));
         while Present (AN) loop
            AE := Expression (AN);

            --  SCOs are generated before semantic analysis/expansion:
            --  PPCs are not split yet.

            pragma Assert (not Split_PPC (AN));

            C1 := ASCII.NUL;

            case Get_Aspect_Id (AN) is

               --  Aspects rewritten into pragmas controlled by a Check_Policy:
               --  Current_Pragma_Sloc must be set to the sloc of the aspect
               --  specification. The corresponding pragma will have the same
               --  sloc. Note that Invariant, Pre, and Post will be enabled if
               --  the policy is Check; on the other hand, predicate aspects
               --  will be enabled for Check and Ignore (when Add_Predicate
               --  is called) because the actual checks occur in client units.
               --  When the assertion policy for Predicate is Disable, the
               --  SCO remains disabled, because Add_Predicate is never called.

               --  Pre/post can have checks in client units too because of
               --  inheritance, so should they receive the same treatment???

               when Aspect_Dynamic_Predicate
                  | Aspect_Invariant
                  | Aspect_Post
                  | Aspect_Postcondition
                  | Aspect_Pre
                  | Aspect_Precondition
                  | Aspect_Predicate
                  | Aspect_Static_Predicate
                  | Aspect_Type_Invariant
               =>
                  C1 := 'a';

               --  Other aspects: just process any decision nested in the
               --  aspect expression.

               when others =>
                  if Has_Decision (AE) then
                     C1 := 'X';
                  end if;
            end case;

            if C1 /= ASCII.NUL then
               pragma Assert (Current_Pragma_Sloc = No_Location);

               if C1 = 'a' or else C1 = 'A' then
                  Current_Pragma_Sloc := Sloc (AN);
               end if;

               Process_Decisions_Defer (AE, C1);

               Current_Pragma_Sloc := No_Location;
            end if;

            Next (AN);
         end loop;
      end Traverse_Aspects;

      ------------------------------------
      -- Traverse_Degenerate_Subprogram --
      ------------------------------------

      procedure Traverse_Degenerate_Subprogram (N : Node_Id) is
      begin
         --  Complete current sequence of statements

         Set_Statement_Entry;

         declare
            Saved_Dominant : constant Dominant_Info := Current_Dominant;
            --  Save last statement in current sequence as dominant

         begin
            --  Output statement SCO for degenerate subprogram body (null
            --  statement or freestanding expression) outside of the dominance
            --  chain.

            Current_Dominant := No_Dominant;
            Extend_Statement_Sequence (N, Typ => 'X');

            --  For the case of an expression-function, collect decisions
            --  embedded in the expression now.

            if Nkind (N) in N_Subexpr then
               Process_Decisions_Defer (N, 'X');
            end if;

            Set_Statement_Entry;

            --  Restore current dominant information designating last statement
            --  in previous sequence (i.e. make the dominance chain skip over
            --  the degenerate body).

            Current_Dominant := Saved_Dominant;
         end;
      end Traverse_Degenerate_Subprogram;

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
               Traverse_Package_Declaration (N, Current_Dominant);

            --  Generic package declaration

            when N_Generic_Package_Declaration =>
               Set_Statement_Entry;
               Traverse_Generic_Package_Declaration (N);

            --  Package body

            when N_Package_Body =>
               Set_Statement_Entry;
               Traverse_Package_Body (N);

            --  Subprogram declaration or subprogram body stub

            when N_Expression_Function
               | N_Subprogram_Body_Stub
               | N_Subprogram_Declaration
            =>
               declare
                  Spec : constant Node_Id := Specification (N);
               begin
                  Process_Decisions_Defer
                    (Parameter_Specifications (Spec), 'X');

                  --  Case of a null procedure: generate SCO for fictitious
                  --  NULL statement located at the NULL keyword in the
                  --  procedure specification.

                  if Nkind (N) = N_Subprogram_Declaration
                    and then Nkind (Spec) = N_Procedure_Specification
                    and then Null_Present (Spec)
                  then
                     Traverse_Degenerate_Subprogram (Null_Statement (Spec));

                  --  Case of an expression function: generate a statement SCO
                  --  for the expression (and then decision SCOs for any nested
                  --  decisions).

                  elsif Nkind (N) = N_Expression_Function then
                     Traverse_Degenerate_Subprogram (Expression (N));
                  end if;
               end;

            --  Entry declaration

            when N_Entry_Declaration =>
               Process_Decisions_Defer (Parameter_Specifications (N), 'X');

            --  Generic subprogram declaration

            when N_Generic_Subprogram_Declaration =>
               Process_Decisions_Defer
                 (Generic_Formal_Declarations (N), 'X');
               Process_Decisions_Defer
                 (Parameter_Specifications (Specification (N)), 'X');

            --  Task or subprogram body

            when N_Subprogram_Body
               | N_Task_Body
            =>
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
               Traverse_Declarations_Or_Statements (Declarations (N));

            --  Exit statement, which is an exit statement in the SCO sense,
            --  so it is included in the current statement sequence, but
            --  then it terminates this sequence. We also have to process
            --  any decisions in the exit statement expression.

            when N_Exit_Statement =>
               Extend_Statement_Sequence (N, 'E');
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

               --  The first statement in the handled sequence of statements
               --  is dominated by the elaboration of the last declaration.

               Current_Dominant := Traverse_Declarations_Or_Statements
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
                  Alt := First_Non_Pragma (Alternatives (N));
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

            when N_Conditional_Entry_Call
               | N_Timed_Entry_Call
            =>
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

               --  It is dubious to emit a statement SCO for a TERMINATE
               --  alternative, since no code is actually executed if the
               --  alternative is selected -- the tasking runtime call just
               --  never returns???

               Extend_Statement_Sequence (N, ' ');
               Set_Statement_Entry;

            --  Unconditional exit points, which are included in the current
            --  statement sequence, but then terminate it

            when N_Goto_Statement
               | N_Raise_Statement
               | N_Requeue_Statement
            =>
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
               Process_Decisions_Defer (Return_Object_Declarations (N), 'X');
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
                  Nam : constant Name_Id := Pragma_Name_Unmapped (N);
                  Arg : Node_Id          :=
                          First (Pragma_Argument_Associations (N));
                  Typ : Character;

               begin
                  case Nam is
                     when Name_Assert
                        | Name_Assert_And_Cut
                        | Name_Assume
                        | Name_Check
                        | Name_Loop_Invariant
                        | Name_Postcondition
                        | Name_Precondition
                     =>
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

                        --  Pre/postconditions can be inherited so SCO should
                        --  never be deactivated???

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

                     --  Should generate P decisions (not X) for assertion
                     --  related pragmas: [Type_]Invariant,
                     --  [{Static,Dynamic}_]Predicate???

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
            --  want one entry in the SCOs, so we take the first, for which
            --  Prev_Ids is False.

            when N_Number_Declaration
               | N_Object_Declaration
            =>
               if not Prev_Ids (N) then
                  Extend_Statement_Sequence (N, 'o');

                  if Has_Decision (N) then
                     Process_Decisions_Defer (N, 'X');
                  end if;
               end if;

            --  All other cases, which extend the current statement sequence
            --  but do not terminate it, even if they have nested decisions.

            when N_Protected_Type_Declaration
               | N_Task_Type_Declaration
            =>
               Extend_Statement_Sequence (N, 't');
               Process_Decisions_Defer (Discriminant_Specifications (N), 'X');
               Set_Statement_Entry;

               Traverse_Sync_Definition (N);

            when N_Single_Protected_Declaration
               | N_Single_Task_Declaration
            =>
               Extend_Statement_Sequence (N, 'o');
               Set_Statement_Entry;

               Traverse_Sync_Definition (N);

            when others =>

               --  Determine required type character code, or ASCII.NUL if
               --  no SCO should be generated for this node.

               declare
                  NK  : constant Node_Kind := Nkind (N);
                  Typ : Character;

               begin
                  case NK is
                     when N_Full_Type_Declaration
                        | N_Incomplete_Type_Declaration
                        | N_Private_Extension_Declaration
                        | N_Private_Type_Declaration
                     =>
                        Typ := 't';

                     when N_Subtype_Declaration =>
                        Typ := 's';

                     when N_Renaming_Declaration =>
                        Typ := 'r';

                     when N_Generic_Instantiation =>
                        Typ := 'i';

                     when N_Package_Body_Stub
                        | N_Protected_Body_Stub
                        | N_Representation_Clause
                        | N_Task_Body_Stub
                        | N_Use_Package_Clause
                        | N_Use_Type_Clause
                     =>
                        Typ := ASCII.NUL;

                     when N_Procedure_Call_Statement =>
                        Typ := ' ';

                     when others =>
                        if NK in N_Statement_Other_Than_Procedure_Call then
                           Typ := ' ';
                        else
                           Typ := 'd';
                        end if;
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

         --  Process aspects if present

         Traverse_Aspects (N);
      end Traverse_One;

   --  Start of processing for Traverse_Declarations_Or_Statements

   begin
      --  Process single prefixed node

      if Present (P) then
         Traverse_One (P);
      end if;

      --  Loop through statements or declarations

      if Is_Non_Empty_List (L) then
         N := First (L);
         while Present (N) loop

            --  Note: For separate bodies, we see the tree after Par.Labl has
            --  introduced implicit labels, so we need to ignore those nodes.

            if Nkind (N) /= N_Implicit_Label_Declaration then
               Traverse_One (N);
            end if;

            Next (N);
         end loop;

      end if;

      --  End sequence of statements and flush deferred decisions

      if Present (P) or else Is_Non_Empty_List (L) then
         Set_Statement_Entry;
      end if;

      return Current_Dominant;
   end Traverse_Declarations_Or_Statements;

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
            Handler := First_Non_Pragma (Exception_Handlers (N));
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
      Dom : Dominant_Info;
   begin
      --  The first statement in the handled sequence of statements is
      --  dominated by the elaboration of the last declaration.

      Dom := Traverse_Declarations_Or_Statements (Declarations (N));

      Traverse_Handled_Statement_Sequence
        (Handled_Statement_Sequence (N), Dom);
   end Traverse_Package_Body;

   ----------------------------------
   -- Traverse_Package_Declaration --
   ----------------------------------

   procedure Traverse_Package_Declaration
     (N : Node_Id;
      D : Dominant_Info := No_Dominant)
   is
      Spec : constant Node_Id := Specification (N);
      Dom  : Dominant_Info;

   begin
      Dom :=
        Traverse_Declarations_Or_Statements (Visible_Declarations (Spec), D);

      --  First private declaration is dominated by last visible declaration

      Traverse_Declarations_Or_Statements (Private_Declarations (Spec), Dom);
   end Traverse_Package_Declaration;

   ------------------------------
   -- Traverse_Sync_Definition --
   ------------------------------

   procedure Traverse_Sync_Definition (N : Node_Id) is
      Dom_Info : Dominant_Info := ('S', N);
      --  The first declaration is dominated by the protected or task [type]
      --  declaration.

      Sync_Def : Node_Id;
      --  N's protected or task definition

      Priv_Decl : List_Id;
      Vis_Decl  : List_Id;
      --  Sync_Def's Visible_Declarations and Private_Declarations

   begin
      case Nkind (N) is
         when N_Protected_Type_Declaration
            | N_Single_Protected_Declaration
         =>
            Sync_Def := Protected_Definition (N);

         when N_Single_Task_Declaration
            | N_Task_Type_Declaration
         =>
            Sync_Def := Task_Definition (N);

         when others =>
            raise Program_Error;
      end case;

      --  Sync_Def may be Empty at least for empty Task_Type_Declarations.
      --  Querying Visible or Private_Declarations is invalid in this case.

      if Present (Sync_Def) then
         Vis_Decl  := Visible_Declarations (Sync_Def);
         Priv_Decl := Private_Declarations (Sync_Def);
      else
         Vis_Decl  := No_List;
         Priv_Decl := No_List;
      end if;

      Dom_Info := Traverse_Declarations_Or_Statements
                    (L => Vis_Decl,
                     D => Dom_Info);

      --  If visible declarations are present, the first private declaration
      --  is dominated by the last visible declaration.

      Traverse_Declarations_Or_Statements
        (L => Priv_Decl,
         D => Dom_Info);
   end Traverse_Sync_Definition;

   --------------------------------------
   -- Traverse_Subprogram_Or_Task_Body --
   --------------------------------------

   procedure Traverse_Subprogram_Or_Task_Body
     (N : Node_Id;
      D : Dominant_Info := No_Dominant)
   is
      Decls    : constant List_Id := Declarations (N);
      Dom_Info : Dominant_Info    := D;

   begin
      --  If declarations are present, the first statement is dominated by the
      --  last declaration.

      Dom_Info := Traverse_Declarations_Or_Statements
                    (L => Decls, D => Dom_Info);

      Traverse_Handled_Statement_Sequence
        (N => Handled_Statement_Sequence (N),
         D => Dom_Info);
   end Traverse_Subprogram_Or_Task_Body;

   -------------------------
   -- SCO_Record_Filtered --
   -------------------------

   procedure SCO_Record_Filtered is
      type Decision is record
         Kind : Character;
         --  Type of the SCO decision (see comments for SCO_Table_Entry.C1)

         Sloc : Source_Location;

         Top  : Nat;
         --  Index in the SCO_Raw_Table for the root operator/condition for the
         --  expression that controls the decision.
      end record;
      --  Decision descriptor: used to gather information about a candidate
      --  SCO decision.

      package Pending_Decisions is new Table.Table
        (Table_Component_Type => Decision,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 1000,
         Table_Increment      => 200,
         Table_Name           => "Filter_Pending_Decisions");
      --  Table used to hold decisions to process during the collection pass

      procedure Add_Expression_Tree (Idx : in out Nat);
      --  Add SCO raw table entries for the decision controlling expression
      --  tree starting at Idx to the filtered SCO table.

      procedure Collect_Decisions
        (D    : Decision;
         Next : out Nat);
      --  Collect decisions to add to the filtered SCO table starting at the
      --  D decision (including it and its nested operators/conditions). Set
      --  Next to the first node index passed the whole decision.

      procedure Compute_Range
        (Idx  : in out Nat;
         From : out Source_Location;
         To   : out Source_Location);
      --  Compute the source location range for the expression tree starting at
      --  Idx in the SCO raw table. Store its bounds in From and To.

      function Is_Decision (Idx : Nat) return Boolean;
      --  Return if the expression tree starting at Idx has adjacent nested
      --  nodes that make a decision.

      procedure Process_Pending_Decisions
        (Original_Decision : SCO_Table_Entry);
      --  Complete the filtered SCO table using collected decisions. Output
      --  decisions inherit the pragma information from the original decision.

      procedure Search_Nested_Decisions (Idx : in out Nat);
      --  Collect decisions to add to the filtered SCO table starting at the
      --  node at Idx in the SCO raw table. This node must not be part of an
      --  already-processed decision. Set Idx to the first node index passed
      --  the whole expression tree.

      procedure Skip_Decision
        (Idx                      : in out Nat;
         Process_Nested_Decisions : Boolean);
      --  Skip all the nodes that belong to the decision starting at Idx. If
      --  Process_Nested_Decision, call Search_Nested_Decisions on the first
      --  nested nodes that do not belong to the decision. Set Idx to the first
      --  node index passed the whole expression tree.

      -------------------------
      -- Add_Expression_Tree --
      -------------------------

      procedure Add_Expression_Tree (Idx : in out Nat) is
         Node_Idx : constant Nat := Idx;
         T        : SCO_Table_Entry renames SCO_Raw_Table.Table (Node_Idx);
         From     : Source_Location;
         To       : Source_Location;

      begin
         case T.C1 is
            when ' ' =>

               --  This is a single condition. Add an entry for it and move on

               SCO_Table.Append (T);
               Idx := Idx + 1;

            when '!' =>

               --  This is a NOT operator: add an entry for it and browse its
               --  only child.

               SCO_Table.Append (T);
               Idx := Idx + 1;
               Add_Expression_Tree (Idx);

            when others =>

               --  This must be an AND/OR/AND THEN/OR ELSE operator

               if T.C2 = '?' then

                  --  This is not a short circuit operator: consider this one
                  --  and all its children as a single condition.

                  Compute_Range (Idx, From, To);
                  SCO_Table.Append
                    ((From               => From,
                      To                 => To,
                      C1                 => ' ',
                      C2                 => 'c',
                      Last               => False,
                      Pragma_Sloc        => No_Location,
                      Pragma_Aspect_Name => No_Name));

               else
                  --  This is a real short circuit operator: add an entry for
                  --  it and browse its children.

                  SCO_Table.Append (T);
                  Idx := Idx + 1;
                  Add_Expression_Tree (Idx);
                  Add_Expression_Tree (Idx);
               end if;
         end case;
      end Add_Expression_Tree;

      -----------------------
      -- Collect_Decisions --
      -----------------------

      procedure Collect_Decisions
        (D    : Decision;
         Next : out Nat)
      is
         Idx : Nat := D.Top;

      begin
         if D.Kind /= 'X' or else Is_Decision (D.Top) then
            Pending_Decisions.Append (D);
         end if;

         Skip_Decision (Idx, True);
         Next := Idx;
      end Collect_Decisions;

      -------------------
      -- Compute_Range --
      -------------------

      procedure Compute_Range
        (Idx  : in out Nat;
         From : out Source_Location;
         To   : out Source_Location)
      is
         Sloc_F : Source_Location := No_Source_Location;
         Sloc_T : Source_Location := No_Source_Location;

         procedure Process_One;
         --  Process one node of the tree, and recurse over children. Update
         --  Idx during the traversal.

         -----------------
         -- Process_One --
         -----------------

         procedure Process_One is
         begin
            if Sloc_F = No_Source_Location
                 or else
               SCO_Raw_Table.Table (Idx).From < Sloc_F
            then
               Sloc_F := SCO_Raw_Table.Table (Idx).From;
            end if;

            if Sloc_T = No_Source_Location
                 or else
               Sloc_T < SCO_Raw_Table.Table (Idx).To
            then
               Sloc_T := SCO_Raw_Table.Table (Idx).To;
            end if;

            if SCO_Raw_Table.Table (Idx).C1 = ' ' then

               --  This is a condition: nothing special to do

               Idx := Idx + 1;

            elsif SCO_Raw_Table.Table (Idx).C1 = '!' then

               --  The "not" operator has only one operand

               Idx := Idx + 1;
               Process_One;

            else
               --  This is an AND THEN or OR ELSE logical operator: follow the
               --  left, then the right operands.

               Idx := Idx + 1;

               Process_One;
               Process_One;
            end if;
         end Process_One;

      --  Start of processing for Compute_Range

      begin
         Process_One;
         From := Sloc_F;
         To   := Sloc_T;
      end Compute_Range;

      -----------------
      -- Is_Decision --
      -----------------

      function Is_Decision (Idx : Nat) return Boolean is
         Index : Nat := Idx;

      begin
         loop
            declare
               T : SCO_Table_Entry renames SCO_Raw_Table.Table (Index);

            begin
               case T.C1 is
                  when ' ' =>
                     return False;

                  when '!' =>

                     --  This is a decision iff the only operand of the NOT
                     --  operator could be a standalone decision.

                     Index := Idx + 1;

                  when others =>

                     --  This node is a logical operator (and thus could be a
                     --  standalone decision) iff it is a short circuit
                     --  operator.

                     return T.C2 /= '?';
               end case;
            end;
         end loop;
      end Is_Decision;

      -------------------------------
      -- Process_Pending_Decisions --
      -------------------------------

      procedure Process_Pending_Decisions
        (Original_Decision : SCO_Table_Entry)
      is
      begin
         for Index in 1 .. Pending_Decisions.Last loop
            declare
               D   : Decision renames Pending_Decisions.Table (Index);
               Idx : Nat := D.Top;

            begin
               --  Add a SCO table entry for the decision itself

               pragma Assert (D.Kind /= ' ');

               SCO_Table.Append
                 ((To                 => No_Source_Location,
                   From               => D.Sloc,
                   C1                 => D.Kind,
                   C2                 => ' ',
                   Last               => False,
                   Pragma_Sloc        => Original_Decision.Pragma_Sloc,
                   Pragma_Aspect_Name =>
                      Original_Decision.Pragma_Aspect_Name));

               --  Then add ones for its nested operators/operands. Do not
               --  forget to tag its *last* entry as such.

               Add_Expression_Tree (Idx);
               SCO_Table.Table (SCO_Table.Last).Last := True;
            end;
         end loop;

         --  Clear the pending decisions list
         Pending_Decisions.Set_Last (0);
      end Process_Pending_Decisions;

      -----------------------------
      -- Search_Nested_Decisions --
      -----------------------------

      procedure Search_Nested_Decisions (Idx : in out Nat) is
      begin
         loop
            declare
               T : SCO_Table_Entry renames SCO_Raw_Table.Table (Idx);

            begin
               case T.C1 is
                  when ' ' =>
                     Idx := Idx + 1;
                     exit;

                  when '!' =>
                     Collect_Decisions
                       ((Kind => 'X',
                         Sloc => T.From,
                         Top  => Idx),
                        Idx);
                     exit;

                  when others =>
                     if T.C2 = '?' then

                        --  This is not a logical operator: start looking for
                        --  nested decisions from here. Recurse over the left
                        --  child and let the loop take care of the right one.

                        Idx := Idx + 1;
                        Search_Nested_Decisions (Idx);

                     else
                        --  We found a nested decision

                        Collect_Decisions
                          ((Kind => 'X',
                            Sloc => T.From,
                            Top  => Idx),
                            Idx);
                        exit;
                     end if;
               end case;
            end;
         end loop;
      end Search_Nested_Decisions;

      -------------------
      -- Skip_Decision --
      -------------------

      procedure Skip_Decision
        (Idx                      : in out Nat;
         Process_Nested_Decisions : Boolean)
      is
      begin
         loop
            declare
               T : SCO_Table_Entry renames SCO_Raw_Table.Table (Idx);

            begin
               Idx := Idx + 1;

               case T.C1 is
                  when ' ' =>
                     exit;

                  when '!' =>

                     --  This NOT operator belongs to the outside decision:
                     --  just skip it.

                     null;

                  when others =>
                     if T.C2 = '?' and then Process_Nested_Decisions then

                        --  This is not a logical operator: start looking for
                        --  nested decisions from here. Recurse over the left
                        --  child and let the loop take care of the right one.

                        Search_Nested_Decisions (Idx);

                     else
                        --  This is a logical operator, so it belongs to the
                        --  outside decision: skip its left child, then let the
                        --  loop take care of the right one.

                        Skip_Decision (Idx, Process_Nested_Decisions);
                     end if;
               end case;
            end;
         end loop;
      end Skip_Decision;

   --  Start of processing for SCO_Record_Filtered

   begin
      --  Filtering must happen only once: do nothing if it this pass was
      --  already run.

      if SCO_Generation_State = Filtered then
         return;
      else
         pragma Assert (SCO_Generation_State = Raw);
         SCO_Generation_State := Filtered;
      end if;

      --  Loop through all SCO entries under SCO units

      for Unit_Idx in 1 .. SCO_Unit_Table.Last loop
         declare
            Unit : SCO_Unit_Table_Entry
                     renames SCO_Unit_Table.Table (Unit_Idx);

            Idx : Nat := Unit.From;
            --  Index of the current SCO raw table entry

            New_From : constant Nat := SCO_Table.Last + 1;
            --  After copying SCO enties of interest to the final table, we
            --  will have to change the From/To indexes this unit targets.
            --  This constant keeps track of the new From index.

         begin
            while Idx <= Unit.To loop
               declare
                  T : SCO_Table_Entry renames SCO_Raw_Table.Table (Idx);

               begin
                  case T.C1 is

                     --  Decision (of any kind, including pragmas and aspects)

                     when 'E' | 'G' | 'I' | 'W' | 'X' | 'P' | 'a' | 'A' =>
                        if SCO_Pragma_Disabled (T.Pragma_Sloc) then

                           --  Skip SCO entries for decisions in disabled
                           --  constructs (pragmas or aspects).

                           Idx := Idx + 1;
                           Skip_Decision (Idx, False);

                        else
                           Collect_Decisions
                             ((Kind => T.C1,
                               Sloc => T.From,
                               Top  => Idx + 1),
                              Idx);
                           Process_Pending_Decisions (T);
                        end if;

                     --  There is no translation/filtering to do for other kind
                     --  of SCO items (statements, dominance markers, etc.).

                     when '|' | '&' | '!' | ' ' =>

                        --  SCO logical operators and conditions cannot exist
                        --  on their own: they must be inside a decision (such
                        --  entries must have been skipped by
                        --  Collect_Decisions).

                        raise Program_Error;

                     when others =>
                        SCO_Table.Append (T);
                        Idx := Idx + 1;
                  end case;
               end;
            end loop;

            --  Now, update the SCO entry indexes in the unit entry

            Unit.From := New_From;
            Unit.To   := SCO_Table.Last;
         end;
      end loop;

      --  Then clear the raw table to free bytes

      SCO_Raw_Table.Free;
   end SCO_Record_Filtered;

end Par_SCO;
