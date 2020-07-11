------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . R E G E X P                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2020, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with System.Case_Util;

package body System.Regexp is

   Initial_Max_States_In_Primary_Table : constant := 100;
   --  Initial size for the number of states in the indefinite state
   --  machine. The number of states will be increased as needed.
   --
   --  This is also used as the maximal number of meta states (groups of
   --  states) in the secondary table.

   Open_Paren    : constant Character := '(';
   Close_Paren   : constant Character := ')';
   Open_Bracket  : constant Character := '[';
   Close_Bracket : constant Character := ']';

   type State_Index is new Natural;
   type Column_Index is new Natural;

   type Regexp_Array is array
     (State_Index range <>, Column_Index range <>) of State_Index;
   --  First index is for the state number. Second index is for the character
   --  type. Contents is the new State.

   type Regexp_Array_Access is access Regexp_Array;
   --  Use this type through the functions Set below, so that it can grow
   --  dynamically depending on the needs.

   type Mapping is array (Character'Range) of Column_Index;
   --  Mapping between characters and column in the Regexp_Array

   type Boolean_Array is array (State_Index range <>) of Boolean;

   type Regexp_Value
     (Alphabet_Size : Column_Index;
      Num_States    : State_Index) is
   record
      Map            : Mapping;
      Case_Sensitive : Boolean;
      States         : Regexp_Array (1 .. Num_States, 0 .. Alphabet_Size);
      Is_Final       : Boolean_Array (1 .. Num_States);
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
      Column : Column_Index) return State_Index;
   --  Returns the value in the table at (State, Column). If this index does
   --  not exist in the table, returns zero.

   procedure Free is new Ada.Unchecked_Deallocation
     (Regexp_Array, Regexp_Array_Access);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (R : in out Regexp) is
      Tmp : Regexp_Access;
   begin
      if R.R /= null then
         Tmp := new Regexp_Value (Alphabet_Size => R.R.Alphabet_Size,
                                  Num_States    => R.R.Num_States);
         Tmp.all := R.R.all;
         R.R := Tmp;
      end if;
   end Adjust;

   -------------
   -- Compile --
   -------------

   function Compile
     (Pattern        : String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True) return Regexp
   is
      S : String := Pattern;
      --  The pattern which is really compiled (when the pattern is case
      --  insensitive, we convert this string to lower-cases

      Map : Mapping := (others => 0);
      --  Mapping between characters and columns in the tables

      Alphabet_Size : Column_Index := 0;
      --  Number of significant characters in the regular expression.
      --  This total does not include special operators, such as *, (, ...

      procedure Check_Well_Formed_Pattern;
      --  Check that the pattern to compile is well-formed, so that subsequent
      --  code can rely on this without performing each time the checks to
      --  avoid accessing the pattern outside its bounds. However, not all
      --  well-formedness rules are checked. In particular, rules about special
      --  characters not being treated as regular characters are not checked.

      procedure Create_Mapping;
      --  Creates a mapping between characters in the regexp and columns
      --  in the tables representing the regexp. Test that the regexp is
      --  well-formed Modifies Alphabet_Size and Map

      procedure Create_Primary_Table
        (Table       : out Regexp_Array_Access;
         Num_States  : out State_Index;
         Start_State : out State_Index;
         End_State   : out State_Index);
      --  Creates the first version of the regexp (this is a non deterministic
      --  finite state machine, which is unadapted for a fast pattern
      --  matching algorithm). We use a recursive algorithm to process the
      --  parenthesis sub-expressions.
      --
      --  Table : at the end of the procedure : Column 0 is for any character
      --  ('.') and the last columns are for no character (closure). Num_States
      --  is set to the number of states in the table Start_State is the number
      --  of the starting state in the regexp End_State is the number of the
      --  final state when the regexp matches.

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
         Start_State : State_Index;
         End_State   : State_Index) return Regexp;
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

      procedure Raise_Exception (M : String; Index : Integer);
      pragma No_Return (Raise_Exception);
      --  Raise an exception, indicating an error at character Index in S

      -------------------------------
      -- Check_Well_Formed_Pattern --
      -------------------------------

      procedure Check_Well_Formed_Pattern is
         J : Integer;

         Past_Elmt : Boolean := False;
         --  Set to True everywhere an elmt has been parsed, if Glob=False,
         --  meaning there can be now an occurrence of '*', '+' and '?'.

         Past_Term : Boolean := False;
         --  Set to True everywhere a term has been parsed, if Glob=False,
         --  meaning there can be now an occurrence of '|'.

         Parenthesis_Level : Integer := 0;
         Curly_Level       : Integer := 0;

         Last_Open : Integer := S'First - 1;
         --  The last occurrence of an opening parenthesis, if Glob=False,
         --  or the last occurrence of an opening curly brace, if Glob=True.

         procedure Raise_Exception_If_No_More_Chars (K : Integer := 0);
         --  If no more characters are raised, call Raise_Exception

         --------------------------------------
         -- Raise_Exception_If_No_More_Chars --
         --------------------------------------

         procedure Raise_Exception_If_No_More_Chars (K : Integer := 0) is
         begin
            if J + K > S'Last then
               Raise_Exception ("Ill-formed pattern while parsing", J);
            end if;
         end Raise_Exception_If_No_More_Chars;

      --  Start of processing for Check_Well_Formed_Pattern

      begin
         J := S'First;
         while J <= S'Last loop
            case S (J) is
               when Open_Bracket =>
                  J := J + 1;
                  Raise_Exception_If_No_More_Chars;

                  if not Glob then
                     if S (J) = '^' then
                        J := J + 1;
                        Raise_Exception_If_No_More_Chars;
                     end if;
                  end if;

                  --  The first character never has a special meaning

                  if S (J) = ']' or else S (J) = '-' then
                     J := J + 1;
                     Raise_Exception_If_No_More_Chars;
                  end if;

                  --  The set of characters cannot be empty

                  if S (J) = ']' then
                     Raise_Exception
                       ("Set of characters cannot be empty in regular "
                          & "expression", J);
                  end if;

                  declare
                     Possible_Range_Start : Boolean := True;
                     --  Set True everywhere a range character '-' can occur

                  begin
                     loop
                        exit when S (J) = Close_Bracket;

                        --  The current character should be followed by a
                        --  closing bracket.

                        Raise_Exception_If_No_More_Chars (1);

                        if S (J) = '-'
                          and then S (J + 1) /= Close_Bracket
                        then
                           if not Possible_Range_Start then
                              Raise_Exception
                                ("No mix of ranges is allowed in "
                                   & "regular expression", J);
                           end if;

                           J := J + 1;
                           Raise_Exception_If_No_More_Chars;

                           --  Range cannot be followed by '-' character,
                           --  except as last character in the set.

                           Possible_Range_Start := False;

                        else
                           Possible_Range_Start := True;
                        end if;

                        if S (J) = '\' then
                           J := J + 1;
                           Raise_Exception_If_No_More_Chars;
                        end if;

                        J := J + 1;
                     end loop;
                  end;

                  --  A closing bracket can end an elmt or term

                  Past_Elmt := True;
                  Past_Term := True;

               when Close_Bracket =>

                  --  A close bracket must follow a open_bracket, and cannot be
                  --  found alone on the line.

                  Raise_Exception
                    ("Incorrect character ']' in regular expression", J);

               when '\' =>
                  if J < S'Last then
                     J := J + 1;

                     --  Any character can be an elmt or a term

                     Past_Elmt := True;
                     Past_Term := True;

                  else
                     --  \ not allowed at the end of the regexp

                     Raise_Exception
                       ("Incorrect character '\' in regular expression", J);
                  end if;

               when Open_Paren =>
                  if not Glob then
                     Parenthesis_Level := Parenthesis_Level + 1;
                     Last_Open := J;

                     --  An open parenthesis does not end an elmt or term

                     Past_Elmt := False;
                     Past_Term := False;
                  end if;

               when Close_Paren =>
                  if not Glob then
                     Parenthesis_Level := Parenthesis_Level - 1;

                     if Parenthesis_Level < 0 then
                        Raise_Exception
                          ("')' is not associated with '(' in regular "
                           & "expression", J);
                     end if;

                     if J = Last_Open + 1 then
                        Raise_Exception
                          ("Empty parentheses not allowed in regular "
                           & "expression", J);
                     end if;

                     if not Past_Term then
                        Raise_Exception
                          ("Closing parenthesis not allowed here in regular "
                             & "expression", J);
                     end if;

                     --  A closing parenthesis can end an elmt or term

                     Past_Elmt := True;
                     Past_Term := True;
                  end if;

               when '{' =>
                  if Glob then
                     Curly_Level := Curly_Level + 1;
                     Last_Open := J;

                  else
                     --  Any character can be an elmt or a term

                     Past_Elmt := True;
                     Past_Term := True;
                  end if;

                  --  No need to check for ',' as the code always accepts them

               when '}' =>
                  if Glob then
                     Curly_Level := Curly_Level - 1;

                     if Curly_Level < 0 then
                        Raise_Exception
                          ("'}' is not associated with '{' in regular "
                           & "expression", J);
                     end if;

                     if J = Last_Open + 1 then
                        Raise_Exception
                          ("Empty curly braces not allowed in regular "
                           & "expression", J);
                     end if;

                  else
                     --  Any character can be an elmt or a term

                     Past_Elmt := True;
                     Past_Term := True;
                  end if;

               when '*' | '?' | '+' =>
                  if not Glob then

                     --  These operators must apply to an elmt sub-expression,
                     --  and cannot be found if one has not just been parsed.

                     if not Past_Elmt then
                        Raise_Exception
                          ("'*', '+' and '?' operators must be "
                           & "applied to an element in regular expression", J);
                     end if;

                     Past_Elmt := False;
                     Past_Term := True;
                  end if;

               when '|' =>
                  if not Glob then

                     --  This operator must apply to a term sub-expression,
                     --  and cannot be found if one has not just been parsed.

                     if not Past_Term then
                        Raise_Exception
                          ("'|' operator must be "
                           & "applied to a term in regular expression", J);
                     end if;

                     Past_Elmt := False;
                     Past_Term := False;
                  end if;

               when others =>
                  if not Glob then

                     --  Any character can be an elmt or a term

                     Past_Elmt := True;
                     Past_Term := True;
                  end if;
            end case;

            J := J + 1;
         end loop;

         --  A closing parenthesis must follow an open parenthesis

         if Parenthesis_Level /= 0 then
            Raise_Exception
              ("'(' must always be associated with a ')'", J);
         end if;

         --  A closing curly brace must follow an open curly brace

         if Curly_Level /= 0 then
            Raise_Exception
              ("'{' must always be associated with a '}'", J);
         end if;
      end Check_Well_Formed_Pattern;

      --------------------
      -- Create_Mapping --
      --------------------

      procedure Create_Mapping is

         procedure Add_In_Map (C : Character);
         --  Add a character in the mapping, if it is not already defined

         ----------------
         -- Add_In_Map --
         ----------------

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
         Last_Open         : Integer := S'First - 1;

      --  Start of processing for Create_Mapping

      begin
         while J <= S'Last loop
            case S (J) is
               when Open_Bracket =>
                  J := J + 1;

                  if S (J) = '^' then
                     J := J + 1;
                  end if;

                  if S (J) = ']' or else S (J) = '-' then
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

                  --  A close bracket must follow a open_bracket and cannot be
                  --  found alone on the line

               when Close_Bracket =>
                  Raise_Exception
                    ("Incorrect character ']' in regular expression", J);

               when '\' =>
                  if J < S'Last then
                     J := J + 1;
                     Add_In_Map (S (J));

                  else
                     --  Back slash \ not allowed at the end of the regexp

                     Raise_Exception
                       ("Incorrect character '\' in regular expression", J);
                  end if;

               when Open_Paren =>
                  if not Glob then
                     Parenthesis_Level := Parenthesis_Level + 1;
                     Last_Open := J;
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

                     if J = Last_Open + 1 then
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
                          ("'*', '+', '?' and '|' operators cannot be in "
                           & "first position in regular expression", J);
                     end if;
                  end if;

               when '|' | '+' =>
                  if not Glob then
                     if J = S'First then

                        --  These operators must apply to a sub-expression,
                        --  and cannot be found at the beginning of the line

                        Raise_Exception
                          ("'*', '+', '?' and '|' operators cannot be in "
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
         --  Add a empty-character transition from State to To_State

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
         --  Fill the table for the regexp Simple. This is the recursive
         --  procedure called to handle () expressions If End_State = 0, then
         --  the call to Create_Simple creates an independent regexp, not a
         --  concatenation Start_Index .. End_Index is the starting index in
         --  the string S.
         --
         --  Warning: it may look like we are creating too many empty-string
         --  transitions, but they are needed to get the correct regexp.
         --  The table is filled as follow ( s means start-state, e means
         --  end-state) :
         --
         --  regexp   state_num | a b * empty_string
         --  -------  ------------------------------
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
            End_Index   : Integer) return Integer;
         --  Returns the index of the last character of the next sub-expression
         --  in Simple. Index cannot be greater than End_Index.

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
                        J_Start    : constant Integer := J + 1;
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
                        Start_Prev : constant State_Index := Start_State;
                        End_Prev   : constant State_Index := End_State;
                        Start_J    : constant Integer     := J + 1;
                        Start_Next : State_Index := 0;
                        End_Next   : State_Index := 0;

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

                        if S (J) = '-' or else S (J) = ']' then
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

                  when Close_Bracket
                     | Close_Paren
                     | '*' | '+' | '?'
                  =>
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
            End_Index   : Integer) return Integer
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

      --  Start of processing for Create_Primary_Table

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
         --  Add a empty-character transition from State to To_State

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
            J : Column_Index;

         begin
            J := Empty_Char;
            while Get (Table, State, J) /= 0 loop
               J := J + 1;
            end loop;

            Set (Table, State, J, Value => To_State);
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
            J          : Integer;
            Last_Start : State_Index := 0;

         begin
            Start_State := 0;
            End_State   := 0;

            J := Start_Index;
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

                        if S (J) = '-' or else S (J) = ']' then
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
         Start_State : State_Index;
         End_State   : State_Index) return Regexp
      is
         Last_Index : constant State_Index := First_Table'Last (1);

         type Meta_State is array (0 .. Last_Index) of Boolean;
         pragma Pack (Meta_State);
         --  Whether a state from first_table belongs to a metastate.

         No_States : constant Meta_State := (others => False);

         type Meta_States_Array is array (State_Index range <>) of Meta_State;
         type Meta_States_List is access all Meta_States_Array;
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
            (Meta_States_Array, Meta_States_List);
         Meta_States : Meta_States_List;
         --  Components of meta-states. A given state might belong to
         --  several meta-states.
         --  This array grows dynamically.

         type Char_To_State is array (0 .. Alphabet_Size) of State_Index;
         type Meta_States_Transition_Arr is
            array (State_Index range <>) of Char_To_State;
         type Meta_States_Transition is access all Meta_States_Transition_Arr;
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (Meta_States_Transition_Arr, Meta_States_Transition);
         Table : Meta_States_Transition;
         --  Documents the transitions between each meta-state. The
         --  first index is the meta-state, the second column is the
         --  character seen in the input, the value is the new meta-state.

         Temp_State_Not_Null : Boolean;

         Current_State       : State_Index := 1;
         --  The current meta-state we are creating

         Nb_State            : State_Index := 1;
         --  The total number of meta-states created so far.

         procedure Closure
           (Meta_State : State_Index;
            State      : State_Index);
         --  Compute the closure of the state (that is every other state which
         --  has a empty-character transition) and add it to the state

         procedure Ensure_Meta_State (Meta : State_Index);
         --  grows the Meta_States array as needed to make sure that there
         --  is enough space to store the new meta state.

         -----------------------
         -- Ensure_Meta_State --
         -----------------------

         procedure Ensure_Meta_State (Meta : State_Index) is
            Tmp  : Meta_States_List       := Meta_States;
            Tmp2 : Meta_States_Transition := Table;

         begin
            if Meta_States = null then
               Meta_States := new Meta_States_Array
                  (1 .. State_Index'Max (Last_Index, Meta) + 1);
               Meta_States (Meta_States'Range) := (others => No_States);

               Table := new Meta_States_Transition_Arr
                  (1 .. State_Index'Max (Last_Index, Meta) + 1);
               Table.all := (others => (others => 0));

            elsif Meta > Meta_States'Last then
               Meta_States := new Meta_States_Array
                  (1 .. State_Index'Max (2 * Tmp'Last, Meta));
               Meta_States (Tmp'Range) := Tmp.all;
               Meta_States (Tmp'Last + 1 .. Meta_States'Last) :=
                  (others => No_States);
               Unchecked_Free (Tmp);

               Table := new Meta_States_Transition_Arr
                  (1 .. State_Index'Max (2 * Tmp2'Last, Meta) + 1);
               Table (Tmp2'Range) := Tmp2.all;
               Table (Tmp2'Last + 1 .. Table'Last) :=
                  (others => (others => 0));
               Unchecked_Free (Tmp2);
            end if;
         end Ensure_Meta_State;

         -------------
         -- Closure --
         -------------

         procedure Closure
           (Meta_State : State_Index;
            State      : State_Index)
         is
         begin
            if not Meta_States (Meta_State)(State) then
               Meta_States (Meta_State)(State) := True;

               --  For each transition on empty-character

               for Column in Alphabet_Size + 1 .. First_Table'Last (2) loop
                  exit when First_Table (State, Column) = 0;
                  Closure (Meta_State, First_Table (State, Column));
               end loop;
            end if;
         end Closure;

      --  Start of processing for Create_Secondary_Table

      begin
         --  Create a new state

         Ensure_Meta_State (Current_State);
         Closure (Current_State, Start_State);

         while Current_State <= Nb_State loop

            --  We will be trying, below, to create the next meta-state

            Ensure_Meta_State (Nb_State + 1);

            --  For every character in the regexp, calculate the possible
            --  transitions from Current_State.

            for Column in 0 .. Alphabet_Size loop
               Temp_State_Not_Null := False;

               for K in Meta_States (Current_State)'Range loop
                  if Meta_States (Current_State)(K)
                    and then First_Table (K, Column) /= 0
                  then
                     Closure (Nb_State + 1, First_Table (K, Column));
                     Temp_State_Not_Null := True;
                  end if;
               end loop;

               --  If at least one transition existed

               if Temp_State_Not_Null then

                  --  Check if this new state corresponds to an old one

                  for K in 1 .. Nb_State loop
                     if Meta_States (K) = Meta_States (Nb_State + 1) then
                        Table (Current_State)(Column) := K;

                        --  Reset data, for the next time we try that state

                        Meta_States (Nb_State + 1) := No_States;
                        exit;
                     end if;
                  end loop;

                  --  If not, create a new state

                  if Table (Current_State)(Column) = 0 then
                     Nb_State := Nb_State + 1;
                     Ensure_Meta_State (Nb_State + 1);
                     Table (Current_State)(Column) := Nb_State;
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
            R.Case_Sensitive := Case_Sensitive;

            for S in 1 .. Nb_State loop
               R.Is_Final (S) := Meta_States (S)(End_State);
            end loop;

            for State in 1 .. Nb_State loop
               for K in 0 .. Alphabet_Size loop
                  R.States (State, K) := Table (State)(K);
               end loop;
            end loop;

            Unchecked_Free (Meta_States);
            Unchecked_Free (Table);

            return (Ada.Finalization.Controlled with R => R);
         end;
      end Create_Secondary_Table;

      ---------------------
      -- Raise_Exception --
      ---------------------

      procedure Raise_Exception (M : String; Index : Integer) is
      begin
         raise Error_In_Regexp with M & " at offset" & Index'Img;
      end Raise_Exception;

   --  Start of processing for Compile

   begin
      --  Special case for the empty string: it always matches, and the
      --  following processing would fail on it.

      if S = "" then
         return (Ada.Finalization.Controlled with
                 R => new Regexp_Value'
                      (Alphabet_Size => 0,
                       Num_States    => 1,
                       Map           => (others => 0),
                       States        => (others => (others => 1)),
                       Is_Final      => (others => True),
                       Case_Sensitive => True));
      end if;

      if not Case_Sensitive then
         System.Case_Util.To_Lower (S);
      end if;

      --  Check the pattern is well-formed before any treatment

      Check_Well_Formed_Pattern;

      Create_Mapping;

      --  Creates the primary table

      declare
         Table       : Regexp_Array_Access;
         Num_States  : State_Index;
         Start_State : State_Index;
         End_State   : State_Index;
         R           : Regexp;

      begin
         Table := new Regexp_Array (1 .. Initial_Max_States_In_Primary_Table,
                                    0 .. Alphabet_Size + 10);
         if not Glob then
            Create_Primary_Table (Table, Num_States, Start_State, End_State);
         else
            Create_Primary_Table_Glob
              (Table, Num_States, Start_State, End_State);
         end if;

         --  Creates the secondary table

         R := Create_Secondary_Table (Table, Start_State, End_State);
         Free (Table);
         return R;
      end;
   end Compile;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (R : in out Regexp) is
      procedure Free is new
        Ada.Unchecked_Deallocation (Regexp_Value, Regexp_Access);
   begin
      Free (R.R);
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get
     (Table  : Regexp_Array_Access;
      State  : State_Index;
      Column : Column_Index) return State_Index
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
                          R.R.Map (System.Case_Util.To_Lower (S (Char))));
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
         --  (State, Column) is a valid index.

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

end System.Regexp;
