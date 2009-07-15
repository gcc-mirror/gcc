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
with Nlists;   use Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Table;

with GNAT.HTable;      use GNAT.HTable;
with GNAT.Heap_Sort_G;

package body Par_SCO is

   ---------------
   -- SCO_Table --
   ---------------

   --  Internal table used to store recorded SCO values. Table is populated by
   --  calls to SCO_Record, and entries may be modified by Set_SCO_Condition.

   type SCO_Table_Entry is record
      From : Source_Ptr;
      To   : Source_Ptr;
      C1   : Character;
      C2   : Character;
      Last : Boolean;
   end record;

   package SCO_Table is new Table.Table (
     Table_Component_Type => SCO_Table_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 500,
     Table_Increment      => 300,
     Table_Name           => "SCO_Table_Entry");

   --  The SCO_Table_Entry values appear as follows:

   --    Statements
   --      C1   = 'S'
   --      C2   = ' '
   --      From = starting sloc
   --      To   = ending sloc
   --      Last = unused

   --    Exit
   --      C1   = 'T'
   --      C2   = ' '
   --      From = starting sloc
   --      To   = ending sloc
   --      Last = unused

   --    Simple Decision
   --      C1   = 'I', 'E', 'W', 'X' (if/exit/while/expression)
   --      C2   = 'c', 't', or 'f'
   --      From = starting sloc
   --      To   = ending sloc
   --      Last = True

   --    Complex Decision
   --      C1   = 'I', 'E', 'W', 'X' (if/exit/while/expression)
   --      C2   = ' '
   --      From = No_Location
   --      To   = No_Location
   --      Last = False

   --    Operator
   --      C1   = '!', '^', '&', '|'
   --      C2   = ' '
   --      From = No_Location
   --      To   = No_Location
   --      Last = False

   --    Element
   --      C1   = ' '
   --      C2   = 'c', 't', or 'f' (condition/true/false)
   --      From = starting sloc
   --      To   = ending sloc
   --      Last = False for all but the last entry, True for last entry

   --    Note: the sequence starting with a decision, and continuing with
   --    operators and elements up to and including the first one labeled with
   --    Last=True, indicate the sequence to be output for a complex decision
   --    on a single CD decision line.

   ----------------
   -- Unit Table --
   ----------------

   --  This table keeps track of the units and the corresponding starting and
   --  ending indexes (From, To) in the SCO table. Note that entry zero is
   --  unused, it is for convenience in calling the sort routine.

   type SCO_Unit_Table_Entry is record
      Unit : Unit_Number_Type;
      From : Nat;
      To   : Nat;
   end record;

   package SCO_Unit_Table is new Table.Table (
     Table_Component_Type => SCO_Unit_Table_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "SCO_Unit_Table_Entry");

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
   --  if it is a logical operator (including short circuit conditions) and
   --  returns True if so, False otherwise, it does no other processing.

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

   procedure dsco;
   --  Debug routine to dump SCO table

   ----------
   -- dsco --
   ----------

   procedure dsco is
   begin
      Write_Line ("SCO Unit Table");
      Write_Line ("--------------");

      for Index in SCO_Unit_Table.First .. SCO_Unit_Table.Last loop
         Write_Str ("  ");
         Write_Int (Index);
         Write_Str (".  Unit = ");
         Write_Int (Int (SCO_Unit_Table.Table (Index).Unit));
         Write_Str ("  From = ");
         Write_Int (Int (SCO_Unit_Table.Table (Index).From));
         Write_Str ("  To = ");
         Write_Int (Int (SCO_Unit_Table.Table (Index).To));
         Write_Eol;
      end loop;

      Write_Eol;
      Write_Line ("SCO Table");
      Write_Line ("---------");

      for Index in SCO_Table.First .. SCO_Table.Last loop
         declare
            T : SCO_Table_Entry renames SCO_Table.Table (Index);

         begin
            Write_Str ("  ");
            Write_Int (Index);
            Write_Str (".  C1 = '");
            Write_Char (T.C1);
            Write_Str ("' C2 = '");
            Write_Char (T.C2);
            Write_Str ("' From = ");
            Write_Location (T.From);
            Write_Str ("  To = ");
            Write_Location (T.To);
            Write_Str (" Last = ");

            if T.Last then
               Write_Str (" True");
            else
               Write_Str (" False");
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
      SCO_Unit_Table.Init;
      SCO_Unit_Table.Increment_Last;
      SCO_Table.Init;
   end Initialize;

   -------------------------
   -- Is_Logical_Operator --
   -------------------------

   function Is_Logical_Operator (N : Node_Id) return Boolean is
   begin
      return Nkind_In (N, N_Op_And,
                          N_Op_Or,
                          N_Op_Xor,
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

         FSloc : Source_Ptr;
         LSloc : Source_Ptr;

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

            Sloc_Range (N, FSloc, LSloc);
            Set_Table_Entry (C, ' ', FSloc, LSloc, False);

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

               --  Logical operators and short circuit forms, output table
               --  entries and then process operands recursively to deal with
               --  nested conditions.

            when N_And_Then                    |
                 N_Or_Else                     |
                 N_Op_And                      |
                 N_Op_Or                       |
                 N_Op_Xor                      |
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

   ----------------
   -- SCO_Output --
   ----------------

   procedure SCO_Output is
      Start : Nat;
      Stop  : Nat;
      U     : Unit_Number_Type;

      procedure Output_Range (From : Source_Ptr; To : Source_Ptr);
      --  Outputs Sloc range in line:col-line:col format (for now we do not
      --  worry about generic instantiations???)

      ------------------
      -- Output_Range --
      ------------------

      procedure Output_Range (From : Source_Ptr; To : Source_Ptr) is
      begin
         Write_Info_Nat (Int (Get_Logical_Line_Number (From)));
         Write_Info_Char (':');
         Write_Info_Nat (Int (Get_Column_Number (From)));
         Write_Info_Char ('-');
         Write_Info_Nat (Int (Get_Logical_Line_Number (To)));
         Write_Info_Char (':');
         Write_Info_Nat (Int (Get_Column_Number (To)));
      end Output_Range;

   --  Start of processing for SCO_Output

   begin
      if Debug_Flag_Dot_OO then
         dsco;
      end if;

      --  Sort the unit table

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
            return Dependency_Num (SCO_Unit_Table.Table (Nat (Op1)).Unit) <
                   Dependency_Num (SCO_Unit_Table.Table (Nat (Op2)).Unit);
         end Lt;

         ----------
         -- Move --
         ----------

         procedure Move (From : Natural; To : Natural) is
         begin
            SCO_Unit_Table.Table (Nat (To)) :=
              SCO_Unit_Table.Table (Nat (From));
         end Move;

         package Sorting is new GNAT.Heap_Sort_G (Move, Lt);

      --  Start of processing for Unit_Table_Sort

      begin
         Sorting.Sort (Integer (SCO_Unit_Table.Last));
      end Unit_Table_Sort;

      --  Loop through entries in the unit table

      for J in 1 .. SCO_Unit_Table.Last loop
         U := SCO_Unit_Table.Table (J).Unit;

         --  Output header line preceded by blank line

         Write_Info_Terminate;
         Write_Info_Initiate ('C');
         Write_Info_Char (' ');
         Write_Info_Nat (Dependency_Num (U));
         Write_Info_Char (' ');
         Write_Info_Name (Reference_Name (Source_Index (U)));
         Write_Info_Terminate;

         Start := SCO_Unit_Table.Table (J).From;
         Stop  := SCO_Unit_Table.Table (J).To;

         --  Loop through relevant entries in SCO table, outputting C lines

         while Start <= Stop loop
            declare
               T : SCO_Table_Entry renames SCO_Table.Table (Start);

            begin
               Write_Info_Initiate ('C');
               Write_Info_Char (T.C1);

               case T.C1 is

                  --  Statements, exit

                  when 'S' | 'T' =>
                     Write_Info_Char (' ');
                     Output_Range (T.From, T.To);

                     --  Decision

                  when 'I' | 'E' | 'W' | 'X' =>
                     if T.C2 = ' ' then
                        Start := Start + 1;
                     end if;

                     --  Loop through table entries for this decision

                     loop
                        declare
                           T : SCO_Table_Entry renames SCO_Table.Table (Start);

                        begin
                           Write_Info_Char (' ');

                           if T.C1 = '!' or else
                             T.C1 = '^' or else
                             T.C1 = '&' or else
                             T.C1 = '|'
                           then
                              Write_Info_Char (T.C1);

                           else
                              Write_Info_Char (T.C2);
                              Output_Range (T.From, T.To);
                           end if;

                           exit when T.Last;
                           Start := Start + 1;
                        end;
                     end loop;

                  when others =>
                     raise Program_Error;
               end case;

               Write_Info_Terminate;
            end;

            exit when Start = Stop;
            Start := Start + 1;

            pragma Assert (Start <= Stop);
         end loop;
      end loop;
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

      for J in 1 .. SCO_Unit_Table.Last loop
         if SCO_Unit_Table.Table (J).Unit = U then
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

      --  Make entry for new unit in unit table

      SCO_Unit_Table.Append ((Unit => U, From => From, To => SCO_Table.Last));
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
   begin
      SCO_Table.Append ((C1   => C1,
                         C2   => C2,
                         From => From,
                         To   => To,
                         Last => Last));
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
      if Present (N) then
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
