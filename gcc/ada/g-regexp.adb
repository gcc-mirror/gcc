------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G N A T . R E G E X P                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--            Copyright (C) 1999-2001 Ada Core Technologies, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Unchecked_Deallocation;
with Ada.Exceptions;
with GNAT.Case_Util;

package body GNAT.Regexp is

   Open_Paren    : constant Character := '(';
   Close_Paren   : constant Character := ')';
   Open_Bracket  : constant Character := '[';
   Close_Bracket : constant Character := ']';

   type State_Index is new Natural;
   type Column_Index is new Natural;

   type Regexp_Array is array
     (State_Index range <>, Column_Index range <>) of State_Index;
   --  First index is for the state number
   --  Second index is for the character type
   --  Contents is the new State

   type Regexp_Array_Access is access Regexp_Array;
   --  Use this type through the functions Set below, so that it
   --  can grow dynamically depending on the needs.

   type Mapping is array (Character'Range) of Column_Index;
   --  Mapping between characters and column in the Regexp_Array

   type Boolean_Array is array (State_Index range <>) of Boolean;

   type Regexp_Value
     (Alphabet_Size : Column_Index;
      Num_States    : State_Index) is
   record
      Map            : Mapping;
      States         : Regexp_Array (1 .. Num_States, 0 .. Alphabet_Size);
      Is_Final       : Boolean_Array (1 .. Num_States);
      Case_Sensitive : Boolean;
   end record;
   --  Deterministic finite-state machine

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set
     (Table  : in out Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index;
      Value  : State_Index);
   --  Sets a value in the table. If the table is too small, reallocate it
   --  dynamically so that (State, Column) is a valid index in it.

   function Get
     (Table  : Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index)
      return   State_Index;
   --  Returns the value in the table at (State, Column).
   --  If this index does not exist in the table, returns 0

   procedure Free is new Unchecked_Deallocation
     (Regexp_Array, Regexp_Array_Access);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (R : in out Regexp) is
      Tmp : Regexp_Access;

   begin
      Tmp := new Regexp_Value (Alphabet_Size => R.R.Alphabet_Size,
                               Num_States    => R.R.Num_States);
      Tmp.all := R.R.all;
      R.R := Tmp;
   end Adjust;

   -------------
   -- Compile --
   -------------

   function Compile
     (Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True)
      return           Regexp
   is
      S : String := Pattern;
      --  The pattern which is really compiled (when the pattern is case
      --  insensitive, we convert this string to lower-cases

      Map : Mapping := (others => 0);
      --  Mapping between characters and columns in the tables

      Alphabet_Size : Column_Index := 0;
      --  Number of significant characters in the regular expression.
      --  This total does not include special operators, such as *, (, ...

      procedure Create_Mapping;
      --  Creates a mapping between characters in the regexp and columns
      --  in the tables representing the regexp. Test that the regexp is
      --  well-formed Modifies Alphabet_Size and Map

      procedure Create_Primary_Table
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index);
      --  Creates the first version of the regexp (this is a non determinist
      --  finite state machine, which is unadapted for a fast pattern
      --  matching algorithm). We use a recursive algorithm to process the
      --  parenthesis sub-expressions.
      --
      --  Table : at the end of the procedure : Column 0 is for any character
      --  ('.') and the last columns are for no character (closure)
      --  Num_States is set to the number of states in the table
      --  Start_State is the number of the starting state in the regexp
      --  End_State is the number of the final state when the regexp matches

      procedure Create_Primary_Table_Glob
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index);
      --  Same function as above, but it deals with the second possible
      --  grammar for 'globbing pattern', which is a kind of subset of the
      --  whole regular expression grammar.

      function Create_Secondary_Table
        (First_Table : Regexp_Array_Access;
         Num_States  : State_Index;
         Start_State : State_Index;
         End_State   : State_Index)
         return        Regexp;
      --  Creates the definitive table representing the regular expression
      --  This is actually a transformation of the primary table First_Table,
      --  where every state is grouped with the states in its 'no-character'
      --  columns. The transitions between the new states are then recalculated
      --  and if necessary some new states are created.
      --
      --  Note that the resulting finite-state machine is not optimized in
      --  terms of the number of states : it would be more time-consuming to
      --  add a third pass to reduce the number of states in the machine, with
      --  no speed improvement...

      procedure Raise_Exception
        (M     : String;
         Index : Integer);
      pragma No_Return (Raise_Exception);
      --  Raise an exception, indicating an error at character Index in S.

      --------------------
      -- Create_Mapping --
      --------------------

      procedure Create_Mapping is

         procedure Add_In_Map (C : Character);
         --  Add a character in the mapping, if it is not already defined

         -----------------
         --  Add_In_Map --
         -----------------

         procedure Add_In_Map (C : Character) is
         begin
            if Map (C) = 0 then
               Alphabet_Size := Alphabet_Size + 1;
               Map (C) := Alphabet_Size;
            end if;
         end Add_In_Map;

         J                 : Integer := S'First;
         Parenthesis_Level : Integer := 0;
         Curly_Level       : Integer := 0;

      --  Start of processing for Create_Mapping

      begin
         while J <= S'Last loop
            case S (J) is
               when Open_Bracket =>
                  J := J + 1;

                  if S (J) = '^' then
                     J := J + 1;
                  end if;

                  if S (J) = ']' or S (J) = '-' then
                     J := J + 1;
                  end if;

                  --  The first character never has a special meaning

                  loop
                     if J > S'Last then
                        Raise_Exception
                          ("Ran out of characters while parsing ", J);
                     end if;

                     exit when S (J) = Close_Bracket;

                     if S (J) = '-'
                       and then S (J + 1) /= Close_Bracket
                     then
                        declare
                           Start : constant Integer := J - 1;

                        begin
                           J := J + 1;

                           if S (J) = '\' then
                              J := J + 1;
                           end if;

                           for Char in S (Start) .. S (J) loop
                              Add_In_Map (Char);
                           end loop;
                        end;
                     else
                        if S (J) = '\' then
                           J := J + 1;
                        end if;

                        Add_In_Map (S (J));
                     end if;

                     J := J + 1;
                  end loop;

                  --  A close bracket must follow a open_bracket,
                  --  and cannot be found alone on the line

               when Close_Bracket =>
                  Raise_Exception
                    ("Incorrect character ']' in regular expression", J);

               when '\' =>
                  if J < S'Last  then
                     J := J + 1;
                     Add_In_Map (S (J));

                  else
                     --  \ not allowed at the end of the regexp

                     Raise_Exception
                       ("Incorrect character '\' in regular expression", J);
                  end if;

               when Open_Paren =>
                  if not Glob then
                     Parenthesis_Level := Parenthesis_Level + 1;
                  else
                     Add_In_Map (Open_Paren);
                  end if;

               when Close_Paren =>
                  if not Glob then
                     Parenthesis_Level := Parenthesis_Level - 1;

                     if Parenthesis_Level < 0 then
                        Raise_Exception
                          ("')' is not associated with '(' in regular "
                           & "expression", J);
                     end if;

                     if S (J - 1) = Open_Paren then
                        Raise_Exception
                          ("Empty parenthesis not allowed in regular "
                           & "expression", J);
                     end if;

                  else
                     Add_In_Map (Close_Paren);
                  end if;

               when '.' =>
                  if Glob then
                     Add_In_Map ('.');
                  end if;

               when '{' =>
                  if not Glob then
                     Add_In_Map (S (J));
                  else
                     Curly_Level := Curly_Level + 1;
                  end if;

               when '}' =>
                  if not Glob then
                     Add_In_Map (S (J));
                  else
                     Curly_Level := Curly_Level - 1;
                  end if;

               when '*' | '?' =>
                  if not Glob then
                     if J = S'First then
                        Raise_Exception
                          ("'*', '+', '?' and '|' operators can not be in "
                           & "first position in regular expression", J);
                     end if;
                  end if;

               when '|' | '+' =>
                  if not Glob then
                     if J = S'First then

                        --  These operators must apply to a sub-expression,
                        --  and cannot be found at the beginning of the line

                        Raise_Exception
                          ("'*', '+', '?' and '|' operators can not be in "
                           & "first position in regular expression", J);
                     end if;

                  else
                     Add_In_Map (S (J));
                  end if;

               when others =>
                  Add_In_Map (S (J));
            end case;

            J := J + 1;
         end loop;

         --  A closing parenthesis must follow an open parenthesis

         if Parenthesis_Level /= 0 then
            Raise_Exception
              ("'(' must always be associated with a ')'", J);
         end if;

         if Curly_Level /= 0 then
            Raise_Exception
              ("'{' must always be associated with a '}'", J);
         end if;
      end Create_Mapping;

      --------------------------
      -- Create_Primary_Table --
      --------------------------

      procedure Create_Primary_Table
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index)
      is
         Empty_Char : constant Column_Index := Alphabet_Size + 1;

         Current_State : State_Index := 0;
         --  Index of the last created state

         procedure Add_Empty_Char
           (State    : State_Index;
            To_State : State_Index);
         --  Add a empty-character transition from State to To_State.

         procedure Create_Repetition
           (Repetition : Character;
            Start_Prev : State_Index;
            End_Prev   : State_Index;
            New_Start  : out State_Index;
            New_End    : in out State_Index);
         --  Create the table in case we have a '*', '+' or '?'.
         --  Start_Prev .. End_Prev should indicate respectively the start and
         --  end index of the previous expression, to which '*', '+' or '?' is
         --  applied.

         procedure Create_Simple
           (Start_Index : Integer;
            End_Index   : Integer;
            Start_State : out State_Index;
            End_State   : out State_Index);
         --  Fill the table for the regexp Simple.
         --  This is the recursive procedure called to handle () expressions
         --  If End_State = 0, then the call to Create_Simple creates an
         --  independent regexp, not a concatenation
         --  Start_Index .. End_Index is the starting index in the string S.
         --
         --  Warning: it may look like we are creating too many empty-string
         --  transitions, but they are needed to get the correct regexp.
         --  The table is filled as follow ( s means start-state, e means
         --  end-state) :
         --
         --  regexp   state_num | a b * empty_string
         --  -------  ---------------------------------------
         --    a          1 (s) | 2 - - -
         --               2 (e) | - - - -
         --
         --    ab         1 (s) | 2 - - -
         --               2     | - - - 3
         --               3     | - 4 - -
         --               4 (e) | - - - -
         --
         --    a|b        1     | 2 - - -
         --               2     | - - - 6
         --               3     | - 4 - -
         --               4     | - - - 6
         --               5 (s) | - - - 1,3
         --               6 (e) | - - - -
         --
         --    a*         1     | 2 - - -
         --               2     | - - - 4
         --               3 (s) | - - - 1,4
         --               4 (e) | - - - 3
         --
         --    (a)        1 (s) | 2 - - -
         --               2 (e) | - - - -
         --
         --    a+         1     | 2 - - -
         --               2     | - - - 4
         --               3 (s) | - - - 1
         --               4 (e) | - - - 3
         --
         --    a?         1     | 2 - - -
         --               2     | - - - 4
         --               3 (s) | - - - 1,4
         --               4 (e) | - - - -
         --
         --    .          1 (s) | 2 2 2 -
         --               2 (e) | - - - -

         function Next_Sub_Expression
           (Start_Index : Integer;
            End_Index   : Integer)
            return        Integer;
         --  Returns the index of the last character of the next sub-expression
         --  in Simple. Index can not be greater than End_Index

         --------------------
         -- Add_Empty_Char --
         --------------------

         procedure Add_Empty_Char
           (State    : State_Index;
            To_State : State_Index)
         is
            J : Column_Index := Empty_Char;

         begin
            while Get (Table, State, J) /= 0 loop
               J := J + 1;
            end loop;

            Set (Table, State, J, To_State);
         end Add_Empty_Char;

         -----------------------
         -- Create_Repetition --
         -----------------------

         procedure Create_Repetition
           (Repetition : Character;
            Start_Prev : State_Index;
            End_Prev   : State_Index;
            New_Start  : out State_Index;
            New_End    : in out State_Index)
         is
         begin
            New_Start := Current_State + 1;

            if New_End /= 0 then
               Add_Empty_Char (New_End, New_Start);
            end if;

            Current_State := Current_State + 2;
            New_End   := Current_State;

            Add_Empty_Char (End_Prev, New_End);
            Add_Empty_Char (New_Start, Start_Prev);

            if Repetition /= '+' then
               Add_Empty_Char (New_Start, New_End);
            end if;

            if Repetition /= '?' then
               Add_Empty_Char (New_End, New_Start);
            end if;
         end Create_Repetition;

         -------------------
         -- Create_Simple --
         -------------------

         procedure Create_Simple
           (Start_Index : Integer;
            End_Index   : Integer;
            Start_State : out State_Index;
            End_State   : out State_Index)
         is
            J          : Integer := Start_Index;
            Last_Start : State_Index := 0;

         begin
            Start_State := 0;
            End_State   := 0;
            while J <= End_Index loop
               case S (J) is
                  when Open_Paren =>
                     declare
                        J_Start    : Integer := J + 1;
                        Next_Start : State_Index;
                        Next_End   : State_Index;

                     begin
                        J := Next_Sub_Expression (J, End_Index);
                        Create_Simple (J_Start, J - 1, Next_Start, Next_End);

                        if J < End_Index
                          and then (S (J + 1) = '*' or else
                                    S (J + 1) = '+' or else
                                    S (J + 1) = '?')
                        then
                           J := J + 1;
                           Create_Repetition
                             (S (J),
                              Next_Start,
                              Next_End,
                              Last_Start,
                              End_State);

                        else
                           Last_Start := Next_Start;

                           if End_State /= 0 then
                              Add_Empty_Char (End_State, Last_Start);
                           end if;

                           End_State := Next_End;
                        end if;
                     end;

                  when '|' =>
                     declare
                        Start_Prev : State_Index := Start_State;
                        End_Prev   : State_Index := End_State;
                        Start_Next : State_Index := 0;
                        End_Next   : State_Index := 0;
                        Start_J    : Integer := J + 1;

                     begin
                        J := Next_Sub_Expression (J, End_Index);

                        --  Create a new state for the start of the alternative

                        Current_State := Current_State + 1;
                        Last_Start := Current_State;
                        Start_State := Last_Start;

                        --  Create the tree for the second part of alternative

                        Create_Simple (Start_J, J, Start_Next, End_Next);

                        --  Create the end state

                        Add_Empty_Char (Last_Start, Start_Next);
                        Add_Empty_Char (Last_Start, Start_Prev);
                        Current_State := Current_State + 1;
                        End_State := Current_State;
                        Add_Empty_Char (End_Prev, End_State);
                        Add_Empty_Char (End_Next, End_State);
                     end;

                  when Open_Bracket =>
                     Current_State := Current_State + 1;

                     declare
                        Next_State : State_Index := Current_State + 1;

                     begin
                        J := J + 1;

                        if S (J) = '^' then
                           J := J + 1;

                           Next_State := 0;

                           for Column in 0 .. Alphabet_Size loop
                              Set (Table, Current_State, Column,
                                   Value => Current_State + 1);
                           end loop;
                        end if;

                        --  Automatically add the first character

                        if S (J) = '-' or S (J) = ']' then
                           Set (Table, Current_State, Map (S (J)),
                                Value => Next_State);
                           J := J + 1;
                        end if;

                        --  Loop till closing bracket found

                        loop
                           exit when S (J) = Close_Bracket;

                           if S (J) = '-'
                             and then S (J + 1) /= ']'
                           then
                              declare
                                 Start : constant Integer := J - 1;

                              begin
                                 J := J + 1;

                                 if S (J) = '\' then
                                    J := J + 1;
                                 end if;

                                 for Char in S (Start) .. S (J) loop
                                    Set (Table, Current_State, Map (Char),
                                         Value => Next_State);
                                 end loop;
                              end;

                           else
                              if S (J) = '\' then
                                 J := J + 1;
                              end if;

                              Set (Table, Current_State, Map (S (J)),
                                   Value => Next_State);
                           end if;
                           J := J + 1;
                        end loop;
                     end;

                     Current_State := Current_State + 1;

                     --  If the next symbol is a special symbol

                     if J < End_Index
                       and then (S (J + 1) = '*' or else
                                 S (J + 1) = '+' or else
                                 S (J + 1) = '?')
                     then
                        J := J + 1;
                        Create_Repetition
                          (S (J),
                           Current_State - 1,
                           Current_State,
                           Last_Start,
                           End_State);

                     else
                        Last_Start := Current_State - 1;

                        if End_State /= 0 then
                           Add_Empty_Char (End_State, Last_Start);
                        end if;

                        End_State := Current_State;
                     end if;

                  when '*' | '+' | '?' | Close_Paren | Close_Bracket =>
                     Raise_Exception
                       ("Incorrect character in regular expression :", J);

                  when others =>
                     Current_State := Current_State + 1;

                     --  Create the state for the symbol S (J)

                     if S (J) = '.' then
                        for K in 0 .. Alphabet_Size loop
                           Set (Table, Current_State, K,
                                Value => Current_State + 1);
                        end loop;

                     else
                        if S (J) = '\' then
                           J := J + 1;
                        end if;

                        Set (Table, Current_State, Map (S (J)),
                             Value => Current_State + 1);
                     end if;

                     Current_State := Current_State + 1;

                     --  If the next symbol is a special symbol

                     if J < End_Index
                       and then (S (J + 1) = '*' or else
                                 S (J + 1) = '+' or else
                                 S (J + 1) = '?')
                     then
                        J := J + 1;
                        Create_Repetition
                          (S (J),
                           Current_State - 1,
                           Current_State,
                           Last_Start,
                           End_State);

                     else
                        Last_Start := Current_State - 1;

                        if End_State /= 0 then
                           Add_Empty_Char (End_State, Last_Start);
                        end if;

                        End_State := Current_State;
                     end if;

               end case;

               if Start_State = 0 then
                  Start_State := Last_Start;
               end if;

               J := J + 1;
            end loop;
         end Create_Simple;

         -------------------------
         -- Next_Sub_Expression --
         -------------------------

         function Next_Sub_Expression
           (Start_Index : Integer;
            End_Index   : Integer)
            return        Integer
         is
            J              : Integer := Start_Index;
            Start_On_Alter : Boolean := False;

         begin
            if S (J) = '|' then
               Start_On_Alter := True;
            end if;

            loop
               exit when J = End_Index;
               J := J + 1;

               case S (J) is
                  when '\' =>
                     J := J + 1;

                  when Open_Bracket =>
                     loop
                        J := J + 1;
                        exit when S (J) = Close_Bracket;

                        if S (J) = '\' then
                           J := J + 1;
                        end if;
                     end loop;

                  when Open_Paren =>
                     J := Next_Sub_Expression (J, End_Index);

                  when Close_Paren =>
                     return J;

                  when '|' =>
                     if Start_On_Alter then
                        return J - 1;
                     end if;

                  when others =>
                     null;
               end case;
            end loop;

            return J;
         end Next_Sub_Expression;

      --  Start of Create_Primary_Table

      begin
         Table.all := (others => (others => 0));
         Create_Simple (S'First, S'Last, Start_State, End_State);
         Num_States := Current_State;
      end Create_Primary_Table;

      -------------------------------
      -- Create_Primary_Table_Glob --
      -------------------------------

      procedure Create_Primary_Table_Glob
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index)
      is
         Empty_Char : constant Column_Index := Alphabet_Size + 1;

         Current_State : State_Index := 0;
         --  Index of the last created state

         procedure Add_Empty_Char
           (State    : State_Index;
            To_State : State_Index);
         --  Add a empty-character transition from State to To_State.

         procedure Create_Simple
           (Start_Index : Integer;
            End_Index   : Integer;
            Start_State : out State_Index;
            End_State   : out State_Index);
         --  Fill the table for the S (Start_Index .. End_Index).
         --  This is the recursive procedure called to handle () expressions

         --------------------
         -- Add_Empty_Char --
         --------------------

         procedure Add_Empty_Char
           (State    : State_Index;
            To_State : State_Index)
         is
            J : Column_Index := Empty_Char;

         begin
            while Get (Table, State, J) /= 0 loop
               J := J + 1;
            end loop;

            Set (Table, State, J,
                 Value => To_State);
         end Add_Empty_Char;

         -------------------
         -- Create_Simple --
         -------------------

         procedure Create_Simple
           (Start_Index : Integer;
            End_Index   : Integer;
            Start_State : out State_Index;
            End_State   : out State_Index)
         is
            J          : Integer := Start_Index;
            Last_Start : State_Index := 0;

         begin
            Start_State := 0;
            End_State   := 0;

            while J <= End_Index loop
               case S (J) is

                  when Open_Bracket =>
                     Current_State := Current_State + 1;

                     declare
                        Next_State : State_Index := Current_State + 1;

                     begin
                        J := J + 1;

                        if S (J) = '^' then
                           J := J + 1;
                           Next_State := 0;

                           for Column in 0 .. Alphabet_Size loop
                              Set (Table, Current_State, Column,
                                   Value => Current_State + 1);
                           end loop;
                        end if;

                        --  Automatically add the first character

                        if S (J) = '-' or S (J) = ']' then
                           Set (Table, Current_State, Map (S (J)),
                                Value => Current_State);
                           J := J + 1;
                        end if;

                        --  Loop till closing bracket found

                        loop
                           exit when S (J) = Close_Bracket;

                           if S (J) = '-'
                             and then S (J + 1) /= ']'
                           then
                              declare
                                 Start : constant Integer := J - 1;
                              begin
                                 J := J + 1;

                                 if S (J) = '\' then
                                    J := J + 1;
                                 end if;

                                 for Char in S (Start) .. S (J) loop
                                    Set (Table, Current_State, Map (Char),
                                         Value => Next_State);
                                 end loop;
                              end;

                           else
                              if S (J) = '\' then
                                 J := J + 1;
                              end if;

                              Set (Table, Current_State, Map (S (J)),
                                   Value => Next_State);
                           end if;
                           J := J + 1;
                        end loop;
                     end;

                     Last_Start := Current_State;
                     Current_State := Current_State + 1;

                     if End_State /= 0 then
                        Add_Empty_Char (End_State, Last_Start);
                     end if;

                     End_State := Current_State;

                  when '{' =>
                     declare
                        End_Sub          : Integer;
                        Start_Regexp_Sub : State_Index;
                        End_Regexp_Sub   : State_Index;
                        Create_Start     : State_Index := 0;

                        Create_End : State_Index := 0;
                        --  Initialized to avoid junk warning

                     begin
                        while S (J) /= '}' loop

                           --  First step : find sub pattern

                           End_Sub := J + 1;
                           while S (End_Sub) /= ','
                             and then S (End_Sub) /= '}'
                           loop
                              End_Sub := End_Sub + 1;
                           end loop;

                           --  Second step : create a sub pattern

                           Create_Simple
                             (J + 1,
                              End_Sub - 1,
                              Start_Regexp_Sub,
                              End_Regexp_Sub);

                           J := End_Sub;

                           --  Third step : create an alternative

                           if Create_Start = 0 then
                              Current_State := Current_State + 1;
                              Create_Start := Current_State;
                              Add_Empty_Char (Create_Start, Start_Regexp_Sub);
                              Current_State := Current_State + 1;
                              Create_End := Current_State;
                              Add_Empty_Char (End_Regexp_Sub, Create_End);

                           else
                              Current_State := Current_State + 1;
                              Add_Empty_Char (Current_State, Create_Start);
                              Create_Start := Current_State;
                              Add_Empty_Char (Create_Start, Start_Regexp_Sub);
                              Add_Empty_Char (End_Regexp_Sub, Create_End);
                           end if;
                        end loop;

                        if End_State /= 0 then
                           Add_Empty_Char (End_State, Create_Start);
                        end if;

                        End_State := Create_End;
                        Last_Start := Create_Start;
                     end;

                  when '*' =>
                     Current_State := Current_State + 1;

                     if End_State /= 0 then
                        Add_Empty_Char (End_State, Current_State);
                     end if;

                     Add_Empty_Char (Current_State, Current_State + 1);
                     Add_Empty_Char (Current_State, Current_State + 3);
                     Last_Start := Current_State;

                     Current_State := Current_State + 1;

                     for K in 0 .. Alphabet_Size loop
                        Set (Table, Current_State, K,
                             Value => Current_State + 1);
                     end loop;

                     Current_State := Current_State + 1;
                     Add_Empty_Char (Current_State, Current_State + 1);

                     Current_State := Current_State + 1;
                     Add_Empty_Char (Current_State,  Last_Start);
                     End_State := Current_State;

                  when others =>
                     Current_State := Current_State + 1;

                     if S (J) = '?' then
                        for K in 0 .. Alphabet_Size loop
                           Set (Table, Current_State, K,
                                Value => Current_State + 1);
                        end loop;

                     else
                        if S (J) = '\' then
                           J := J + 1;
                        end if;

                        --  Create the state for the symbol S (J)

                        Set (Table, Current_State, Map (S (J)),
                             Value => Current_State + 1);
                     end if;

                     Last_Start := Current_State;
                     Current_State := Current_State + 1;

                     if End_State /= 0 then
                        Add_Empty_Char (End_State, Last_Start);
                     end if;

                     End_State := Current_State;

               end case;

               if Start_State = 0 then
                  Start_State := Last_Start;
               end if;

               J := J + 1;
            end loop;
         end Create_Simple;

      --  Start of processing for Create_Primary_Table_Glob

      begin
         Table.all := (others => (others => 0));
         Create_Simple (S'First, S'Last, Start_State, End_State);
         Num_States := Current_State;
      end Create_Primary_Table_Glob;

      ----------------------------
      -- Create_Secondary_Table --
      ----------------------------

      function Create_Secondary_Table
        (First_Table : Regexp_Array_Access;
         Num_States  : State_Index;
         Start_State : State_Index;
         End_State   : State_Index)
         return        Regexp
      is
         Last_Index : constant State_Index := First_Table'Last (1);
         type Meta_State is array (1 .. Last_Index) of Boolean;

         Table : Regexp_Array (1 .. Last_Index, 0 .. Alphabet_Size) :=
                   (others => (others => 0));

         Meta_States : array (1 .. Last_Index + 1) of Meta_State :=
                         (others => (others => False));

         Temp_State_Not_Null : Boolean;

         Is_Final : Boolean_Array (1 .. Last_Index) := (others => False);

         Current_State       : State_Index := 1;
         Nb_State            : State_Index := 1;

         procedure Closure
           (State : in out Meta_State;
            Item  :        State_Index);
         --  Compute the closure of the state (that is every other state which
         --  has a empty-character transition) and add it to the state

         -------------
         -- Closure --
         -------------

         procedure Closure
           (State : in out Meta_State;
            Item  : State_Index)
         is
         begin
            if State (Item) then
               return;
            end if;

            State (Item) := True;

            for Column in Alphabet_Size + 1 .. First_Table'Last (2) loop
               if First_Table (Item, Column) = 0 then
                  return;
               end if;

               Closure (State, First_Table (Item, Column));
            end loop;
         end Closure;

      --  Start of procesing for Create_Secondary_Table

      begin
         --  Create a new state

         Closure (Meta_States (Current_State), Start_State);

         while Current_State <= Nb_State loop

            --  If this new meta-state includes the primary table end state,
            --  then this meta-state will be a final state in the regexp

            if Meta_States (Current_State)(End_State) then
               Is_Final (Current_State) := True;
            end if;

            --  For every character in the regexp, calculate the possible
            --  transitions from Current_State

            for Column in 0 .. Alphabet_Size loop
               Meta_States (Nb_State + 1) := (others => False);
               Temp_State_Not_Null := False;

               for K in Meta_States (Current_State)'Range loop
                  if Meta_States (Current_State)(K)
                    and then First_Table (K, Column) /= 0
                  then
                     Closure
                       (Meta_States (Nb_State + 1), First_Table (K, Column));
                     Temp_State_Not_Null := True;
                  end if;
               end loop;

               --  If at least one transition existed

               if Temp_State_Not_Null then

                  --  Check if this new state corresponds to an old one

                  for K in 1 .. Nb_State loop
                     if Meta_States (K) = Meta_States (Nb_State + 1) then
                        Table (Current_State, Column) := K;
                        exit;
                     end if;
                  end loop;

                  --  If not, create a new state

                  if Table (Current_State, Column) = 0 then
                     Nb_State := Nb_State + 1;
                     Table (Current_State, Column) := Nb_State;
                  end if;
               end if;
            end loop;

            Current_State := Current_State + 1;
         end loop;

         --  Returns the regexp

         declare
            R : Regexp_Access;

         begin
            R := new Regexp_Value (Alphabet_Size => Alphabet_Size,
                                   Num_States    => Nb_State);
            R.Map            := Map;
            R.Is_Final       := Is_Final (1 .. Nb_State);
            R.Case_Sensitive := Case_Sensitive;

            for State in 1 .. Nb_State loop
               for K in 0 .. Alphabet_Size loop
                  R.States (State, K) := Table (State, K);
               end loop;
            end loop;

            return (Ada.Finalization.Controlled with R => R);
         end;
      end Create_Secondary_Table;

      ---------------------
      -- Raise_Exception --
      ---------------------

      procedure Raise_Exception
        (M     : String;
         Index : Integer)
      is
      begin
         Ada.Exceptions.Raise_Exception
           (Error_In_Regexp'Identity, M & " at offset " & Index'Img);
      end Raise_Exception;

   --  Start of processing for Compile

   begin
      if not Case_Sensitive then
         GNAT.Case_Util.To_Lower (S);
      end if;

      Create_Mapping;

      --  Creates the primary table

      declare
         Table : Regexp_Array_Access;
         Num_States  : State_Index;
         Start_State : State_Index;
         End_State   : State_Index;
         R           : Regexp;

      begin
         Table := new Regexp_Array (1 .. 100,
                                    0 .. Alphabet_Size + 10);
         if not Glob then
            Create_Primary_Table (Table, Num_States, Start_State, End_State);
         else
            Create_Primary_Table_Glob
              (Table, Num_States, Start_State, End_State);
         end if;

         --  Creates the secondary table

         R := Create_Secondary_Table
           (Table, Num_States, Start_State, End_State);
         Free (Table);
         return R;
      end;
   end Compile;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (R : in out Regexp) is
      procedure Free is new
        Unchecked_Deallocation (Regexp_Value, Regexp_Access);

   begin
      Free (R.R);
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get
     (Table  : Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index)
      return   State_Index
   is
   begin
      if State <= Table'Last (1)
        and then Column <= Table'Last (2)
      then
         return Table (State, Column);
      else
         return 0;
      end if;
   end Get;

   -----------
   -- Match --
   -----------

   function Match (S : String; R : Regexp) return Boolean is
      Current_State : State_Index := 1;

   begin
      if R.R = null then
         raise Constraint_Error;
      end if;

      for Char in S'Range loop

         if R.R.Case_Sensitive then
            Current_State := R.R.States (Current_State, R.R.Map (S (Char)));
         else
            Current_State :=
              R.R.States (Current_State,
                          R.R.Map (GNAT.Case_Util.To_Lower (S (Char))));
         end if;

         if Current_State = 0 then
            return False;
         end if;

      end loop;

      return R.R.Is_Final (Current_State);
   end Match;

   ---------
   -- Set --
   ---------

   procedure Set
     (Table  : in out Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index;
      Value  : State_Index)
   is
      New_Lines   : State_Index;
      New_Columns : Column_Index;
      New_Table   : Regexp_Array_Access;

   begin
      if State <= Table'Last (1)
        and then Column <= Table'Last (2)
      then
         Table (State, Column) := Value;
      else
         --  Doubles the size of the table until it is big enough that
         --  (State, Column) is a valid index

         New_Lines := Table'Last (1) * (State / Table'Last (1) + 1);
         New_Columns := Table'Last (2) * (Column / Table'Last (2) + 1);
         New_Table := new Regexp_Array (Table'First (1) .. New_Lines,
                                        Table'First (2) .. New_Columns);
         New_Table.all := (others => (others => 0));

         for J in Table'Range (1) loop
            for K in Table'Range (2) loop
               New_Table (J, K) := Table (J, K);
            end loop;
         end loop;

         Free (Table);
         Table := New_Table;
         Table (State, Column) := Value;
      end if;
   end Set;

end GNAT.Regexp;
