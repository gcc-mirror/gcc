------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 P R E P                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2021, Free Software Foundation, Inc.         --
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

with Csets;    use Csets;
with Err_Vars; use Err_Vars;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Scans;    use Scans;
with Snames;   use Snames;
with Sinput;
with Stringt;  use Stringt;
with Table;
with Uintp;    use Uintp;

with GNAT.Heap_Sort_G;

package body Prep is

   use Symbol_Table;

   type Token_Name_Array is array (Token_Type) of Name_Id;
   Token_Names : constant Token_Name_Array :=
     (Tok_Abort     => Name_Abort,
      Tok_Abs       => Name_Abs,
      Tok_Abstract  => Name_Abstract,
      Tok_Accept    => Name_Accept,
      Tok_Aliased   => Name_Aliased,
      Tok_All       => Name_All,
      Tok_Array     => Name_Array,
      Tok_And       => Name_And,
      Tok_At        => Name_At,
      Tok_Begin     => Name_Begin,
      Tok_Body      => Name_Body,
      Tok_Case      => Name_Case,
      Tok_Constant  => Name_Constant,
      Tok_Declare   => Name_Declare,
      Tok_Delay     => Name_Delay,
      Tok_Delta     => Name_Delta,
      Tok_Digits    => Name_Digits,
      Tok_Else      => Name_Else,
      Tok_Elsif     => Name_Elsif,
      Tok_End       => Name_End,
      Tok_Entry     => Name_Entry,
      Tok_Exception => Name_Exception,
      Tok_Exit      => Name_Exit,
      Tok_For       => Name_For,
      Tok_Function  => Name_Function,
      Tok_Generic   => Name_Generic,
      Tok_Goto      => Name_Goto,
      Tok_If        => Name_If,
      Tok_Is        => Name_Is,
      Tok_Limited   => Name_Limited,
      Tok_Loop      => Name_Loop,
      Tok_Mod       => Name_Mod,
      Tok_New       => Name_New,
      Tok_Null      => Name_Null,
      Tok_Of        => Name_Of,
      Tok_Or        => Name_Or,
      Tok_Others    => Name_Others,
      Tok_Out       => Name_Out,
      Tok_Package   => Name_Package,
      Tok_Pragma    => Name_Pragma,
      Tok_Private   => Name_Private,
      Tok_Procedure => Name_Procedure,
      Tok_Protected => Name_Protected,
      Tok_Raise     => Name_Raise,
      Tok_Range     => Name_Range,
      Tok_Record    => Name_Record,
      Tok_Rem       => Name_Rem,
      Tok_Renames   => Name_Renames,
      Tok_Requeue   => Name_Requeue,
      Tok_Return    => Name_Return,
      Tok_Reverse   => Name_Reverse,
      Tok_Select    => Name_Select,
      Tok_Separate  => Name_Separate,
      Tok_Subtype   => Name_Subtype,
      Tok_Tagged    => Name_Tagged,
      Tok_Task      => Name_Task,
      Tok_Terminate => Name_Terminate,
      Tok_Then      => Name_Then,
      Tok_Type      => Name_Type,
      Tok_Until     => Name_Until,
      Tok_Use       => Name_Use,
      Tok_When      => Name_When,
      Tok_While     => Name_While,
      Tok_With      => Name_With,
      Tok_Xor       => Name_Xor,
      others        => No_Name);

   Already_Initialized : Boolean := False;
   --  Used to avoid repetition of the part of the initialisation that needs
   --  to be done only once.

   Empty_String : String_Id;
   --  "", as a string_id

   String_False : String_Id;
   --  "false", as a string_id

   --------------
   -- Behavior --
   --------------

   --  Accesses to procedure specified by procedure Initialize

   Error_Msg : Error_Msg_Proc;
   --  Report an error

   Scan : Scan_Proc;
   --  Scan one token

   Set_Ignore_Errors : Set_Ignore_Errors_Proc;
   --  Indicate if error should be taken into account

   Put_Char : Put_Char_Proc;
   --  Output one character

   New_EOL : New_EOL_Proc;
   --  Output an end of line indication

   -------------------------------
   -- State of the Preprocessor --
   -------------------------------

   type Pp_State is record
      If_Ptr : Source_Ptr;
      --  The location of the #if statement (used to flag #if with no
      --  corresponding #end if, at the end).

      Else_Ptr : Source_Ptr;
      --  The location of the #else statement (used to detect multiple #else's)

      Deleting : Boolean;
      --  Set to True when the code should be deleted or commented out

      Match_Seen : Boolean;
      --  Set to True when a condition in an #if or an #elsif is True. Also set
      --  to True if Deleting at the previous level is True. Used to decide if
      --  Deleting should be set to True in a following #elsif or #else.

   end record;

   type Pp_Depth is new Nat;

   Ground : constant Pp_Depth := 0;

   package Pp_States is new Table.Table
     (Table_Component_Type => Pp_State,
      Table_Index_Type     => Pp_Depth,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Prep.Pp_States");
   --  A stack of the states of the preprocessor, for nested #if

   type Operator is (None, Op_Or, Op_And);

   -----------------
   -- Subprograms --
   -----------------

   function Deleting return Boolean;
   --  Return True if code should be deleted or commented out

   function Expression
     (Evaluate_It  : Boolean;
      Complemented : Boolean := False) return Boolean;
   --  Evaluate a condition in an #if or an #elsif statement. If Evaluate_It
   --  is False, the condition is effectively evaluated, otherwise, only the
   --  syntax is checked.

   procedure Go_To_End_Of_Line;
   --  Advance the scan pointer until we reach an end of line or the end of the
   --  buffer.

   function Matching_Strings (S1, S2 : String_Id) return Boolean;
   --  Returns True if the two string parameters are equal (case insensitive)

   ---------------------------------------
   -- Change_Reserved_Keyword_To_Symbol --
   ---------------------------------------

   procedure Change_Reserved_Keyword_To_Symbol
     (All_Keywords : Boolean := False)
   is
      New_Name : constant Name_Id := Token_Names (Token);

   begin
      if New_Name /= No_Name then
         case Token is
            when Tok_And
               | Tok_Else
               | Tok_Elsif
               | Tok_End
               | Tok_If
               | Tok_Or
               | Tok_Then
            =>
               if All_Keywords then
                  Token := Tok_Identifier;
                  Token_Name := New_Name;
               end if;

            when others =>
               Token := Tok_Identifier;
               Token_Name := New_Name;
         end case;
      end if;
   end Change_Reserved_Keyword_To_Symbol;

   ------------------------------------------
   -- Check_Command_Line_Symbol_Definition --
   ------------------------------------------

   procedure Check_Command_Line_Symbol_Definition
     (Definition  : String;
      Data        : out Symbol_Data)
   is
      Index       : Natural := 0;
      Result      : Symbol_Data;

   begin
      --  Look for the character '='

      for J in Definition'Range loop
         if Definition (J) = '=' then
            Index := J;
            exit;
         end if;
      end loop;

      --  If no character '=', then the value is True

      if Index = 0 then

         --  Put the symbol in the name buffer

         Name_Len := Definition'Length;
         Name_Buffer (1 .. Name_Len) := Definition;
         Result := True_Value;

      elsif Index = Definition'First then
         Fail ("invalid symbol definition """ & Definition & """");

      else
         --  Put the symbol in the name buffer

         Name_Len := Index - Definition'First;
         Name_Buffer (1 .. Name_Len) :=
           String'(Definition (Definition'First .. Index - 1));

         --  Check the syntax of the value

         if Definition (Index + 1) /= '"'
           or else Definition (Definition'Last) /= '"'
         then
            for J in Index + 1 .. Definition'Last loop
               case Definition (J) is
                  when '_' | '.' | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' =>
                     null;

                  when others =>
                     Fail ("illegal value """
                           & Definition (Index + 1 .. Definition'Last)
                           & """");
               end case;
            end loop;
         end if;

         --  Even if the value is a string, we still set Is_A_String to False,
         --  to avoid adding additional quotes in the preprocessed sources when
         --  replacing $<symbol>.

         Result.Is_A_String := False;

         --  Put the value in the result

         Start_String;
         Store_String_Chars (Definition (Index + 1 .. Definition'Last));
         Result.Value := End_String;
      end if;

      --  Now, check the syntax of the symbol (we don't allow accented or
      --  wide characters).

      if Name_Buffer (1) not in 'a' .. 'z'
        and then Name_Buffer (1) not in 'A' .. 'Z'
      then
         Fail ("symbol """
               & Name_Buffer (1 .. Name_Len)
               & """ does not start with a letter");
      end if;

      for J in 2 .. Name_Len loop
         case Name_Buffer (J) is
            when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' =>
               null;

            when '_' =>
               if J = Name_Len then
                  Fail ("symbol """
                        & Name_Buffer (1 .. Name_Len)
                        & """ end with a '_'");

               elsif Name_Buffer (J + 1) = '_' then
                  Fail ("symbol """
                        & Name_Buffer (1 .. Name_Len)
                        & """ contains consecutive '_'");
               end if;

            when others =>
               Fail ("symbol """
                     & Name_Buffer (1 .. Name_Len)
                     & """ contains illegal character(s)");
         end case;
      end loop;

      Result.On_The_Command_Line := True;

      --  Put the symbol name in the result

      declare
         Sym : constant String := Name_Buffer (1 .. Name_Len);

      begin
         for Index in 1 .. Name_Len loop
            Name_Buffer (Index) := Fold_Lower (Name_Buffer (Index));
         end loop;

         Result.Symbol := Name_Find;
         Name_Len := Sym'Length;
         Name_Buffer (1 .. Name_Len) := Sym;
         Result.Original := Name_Find;
      end;

      Data := Result;
   end Check_Command_Line_Symbol_Definition;

   --------------
   -- Deleting --
   --------------

   function Deleting return Boolean is
   begin
      --  Always return False when not inside an #if statement

      if Opt.No_Deletion or else Pp_States.Last = Ground then
         return False;
      else
         return Pp_States.Table (Pp_States.Last).Deleting;
      end if;
   end Deleting;

   ----------------
   -- Expression --
   ----------------

   function Expression
     (Evaluate_It  : Boolean;
      Complemented : Boolean := False) return Boolean
   is
      Evaluation : Boolean := Evaluate_It;
      --  Is set to False after an "or else" when left term is True and after
      --  an "and then" when left term is False.

      Final_Result : Boolean := False;

      Current_Result : Boolean := False;
      --  Value of a term

      Current_Operator : Operator := None;
      Symbol1          : Symbol_Id;
      Symbol2          : Symbol_Id;
      Symbol_Name1     : Name_Id;
      Symbol_Name2     : Name_Id;
      Symbol_Pos1      : Source_Ptr;
      Symbol_Pos2      : Source_Ptr;
      Symbol_Value1    : String_Id;
      Symbol_Value2    : String_Id;

      Relop : Token_Type;

   begin
      --  Loop for each term

      loop
         Change_Reserved_Keyword_To_Symbol;

         Current_Result := False;

         --  Scan current term, starting with Token

         case Token is

            --  Handle parenthesized expression

            when Tok_Left_Paren =>
               Scan.all;
               Current_Result := Expression (Evaluation);

               if Token = Tok_Right_Paren then
                  Scan.all;

               else
                  Error_Msg -- CODEFIX
                    ("`)` expected", Token_Ptr);
               end if;

            --  Handle not expression

            when Tok_Not =>
               Scan.all;
               Current_Result :=
                 not Expression (Evaluation, Complemented => True);

            --  Handle sequence starting with identifier

            when Tok_Identifier =>
               Symbol_Name1 := Token_Name;
               Symbol_Pos1  := Token_Ptr;
               Scan.all;

               if Token = Tok_Apostrophe then

                  --  symbol'Defined

                  Scan.all;

                  if Token = Tok_Identifier
                    and then Token_Name = Name_Defined
                  then
                     Scan.all;

                  else
                     Error_Msg ("identifier `Defined` expected", Token_Ptr);
                  end if;

                  if Evaluation then
                     Current_Result := Index_Of (Symbol_Name1) /= No_Symbol;
                  end if;

               --  Handle relational operator

               elsif     Token = Tok_Equal
                 or else Token = Tok_Less
                 or else Token = Tok_Less_Equal
                 or else Token = Tok_Greater
                 or else Token = Tok_Greater_Equal
               then
                  Relop := Token;
                  Scan.all;
                  Change_Reserved_Keyword_To_Symbol;

                  if Token = Tok_Integer_Literal then

                     --  symbol =  integer
                     --  symbol <  integer
                     --  symbol <= integer
                     --  symbol >  integer
                     --  symbol >= integer

                     declare
                        Value : constant Int := UI_To_Int (Int_Literal_Value);
                        Data  : Symbol_Data;

                        Symbol_Value : Int;
                        --  Value of symbol as Int

                     begin
                        if Evaluation then
                           Symbol1 := Index_Of (Symbol_Name1);

                           if Symbol1 = No_Symbol then
                              Error_Msg_Name_1 := Symbol_Name1;
                              Error_Msg ("unknown symbol %", Symbol_Pos1);
                              Symbol_Value1 := No_String;

                           else
                              Data := Mapping.Table (Symbol1);

                              if Data.Is_A_String then
                                 Error_Msg_Name_1 := Symbol_Name1;
                                 Error_Msg
                                   ("symbol % value is not integer",
                                    Symbol_Pos1);

                              else
                                 begin
                                    String_To_Name_Buffer (Data.Value);
                                    Symbol_Value :=
                                      Int'Value (Name_Buffer (1 .. Name_Len));

                                    case Relop is
                                       when Tok_Equal =>
                                          Current_Result :=
                                            Symbol_Value = Value;

                                       when Tok_Less =>
                                          Current_Result :=
                                            Symbol_Value < Value;

                                       when Tok_Less_Equal =>
                                          Current_Result :=
                                            Symbol_Value <= Value;

                                       when Tok_Greater =>
                                          Current_Result :=
                                            Symbol_Value > Value;

                                       when Tok_Greater_Equal =>
                                          Current_Result :=
                                            Symbol_Value >= Value;

                                       when others =>
                                          null;
                                    end case;

                                 exception
                                    when Constraint_Error =>
                                       Error_Msg_Name_1 := Symbol_Name1;
                                       Error_Msg
                                         ("symbol % value is not an integer",
                                          Symbol_Pos1);
                                 end;
                              end if;
                           end if;
                        end if;

                        Scan.all;
                     end;

                  --  Error if relational operator other than = if not numbers

                  elsif Relop /= Tok_Equal then
                     Error_Msg ("number expected", Token_Ptr);

                  --  Equality comparison of two strings

                  elsif Token = Tok_Identifier then

                     --  symbol = symbol

                     Symbol_Name2 := Token_Name;
                     Symbol_Pos2  := Token_Ptr;
                     Scan.all;

                     if Evaluation then
                        Symbol1 := Index_Of (Symbol_Name1);

                        if Symbol1 = No_Symbol then
                           if Undefined_Symbols_Are_False then
                              Symbol_Value1 := String_False;

                           else
                              Error_Msg_Name_1 := Symbol_Name1;
                              Error_Msg ("unknown symbol %", Symbol_Pos1);
                              Symbol_Value1 := No_String;
                           end if;

                        else
                           Symbol_Value1 :=
                             Mapping.Table (Symbol1).Value;
                        end if;

                        Symbol2 := Index_Of (Symbol_Name2);

                        if Symbol2 = No_Symbol then
                           if Undefined_Symbols_Are_False then
                              Symbol_Value2 := String_False;

                           else
                              Error_Msg_Name_1 := Symbol_Name2;
                              Error_Msg ("unknown symbol %", Symbol_Pos2);
                              Symbol_Value2 := No_String;
                           end if;

                        else
                           Symbol_Value2 := Mapping.Table (Symbol2).Value;
                        end if;

                        if Symbol_Value1 /= No_String
                             and then
                           Symbol_Value2 /= No_String
                        then
                           Current_Result :=
                             Matching_Strings (Symbol_Value1, Symbol_Value2);
                        end if;
                     end if;

                  elsif Token = Tok_String_Literal then

                     --  symbol = "value"

                     if Evaluation then
                        Symbol1 := Index_Of (Symbol_Name1);

                        if Symbol1 = No_Symbol then
                           if Undefined_Symbols_Are_False then
                              Symbol_Value1 := String_False;

                           else
                              Error_Msg_Name_1 := Symbol_Name1;
                              Error_Msg ("unknown symbol %", Symbol_Pos1);
                              Symbol_Value1 := No_String;
                           end if;

                        else
                           Symbol_Value1 := Mapping.Table (Symbol1).Value;
                        end if;

                        if Symbol_Value1 /= No_String then
                           Current_Result :=
                             Matching_Strings
                               (Symbol_Value1,
                                String_Literal_Id);
                        end if;
                     end if;

                     Scan.all;

                  else
                     Error_Msg
                       ("literal integer, symbol or literal string expected",
                        Token_Ptr);
                  end if;

               --  Handle True or False

               else
                  if Evaluation then
                     Symbol1 := Index_Of (Symbol_Name1);

                     if Symbol1 = No_Symbol then
                        if Undefined_Symbols_Are_False then
                           Symbol_Value1 := String_False;

                        else
                           Error_Msg_Name_1 := Symbol_Name1;
                           Error_Msg ("unknown symbol %", Symbol_Pos1);
                           Symbol_Value1 := No_String;
                        end if;

                     else
                        Symbol_Value1 := Mapping.Table (Symbol1).Value;
                     end if;

                     if Symbol_Value1 /= No_String then
                        String_To_Name_Buffer (Symbol_Value1);

                        for Index in 1 .. Name_Len loop
                           Name_Buffer (Index) :=
                             Fold_Lower (Name_Buffer (Index));
                        end loop;

                        if Name_Buffer (1 .. Name_Len) = "true" then
                           Current_Result := True;

                        elsif Name_Buffer (1 .. Name_Len) = "false" then
                           Current_Result := False;

                        else
                           Error_Msg_Name_1 := Symbol_Name1;
                           Error_Msg
                             ("value of symbol % is not True or False",
                              Symbol_Pos1);
                        end if;
                     end if;
                  end if;
               end if;

            --  Unrecognized sequence

            when others =>
               Error_Msg ("`(`, NOT or symbol expected", Token_Ptr);
         end case;

         --  Update the cumulative final result

         case Current_Operator is
            when None =>
               Final_Result := Current_Result;

            when Op_Or =>
               Final_Result := Final_Result or Current_Result;

            when Op_And =>
               Final_Result := Final_Result and Current_Result;
         end case;

         --  Handle AND

         if Token = Tok_And then
            if Complemented then
               Error_Msg
                ("mixing NOT and AND is not allowed, parentheses are required",
                 Token_Ptr);

            elsif Current_Operator = Op_Or then
               Error_Msg ("mixing OR and AND is not allowed", Token_Ptr);
            end if;

            Current_Operator := Op_And;
            Scan.all;

            if Token = Tok_Then then
               Scan.all;

               if Final_Result = False then
                  Evaluation := False;
               end if;
            end if;

         --  Handle OR

         elsif Token = Tok_Or then
            if Complemented then
               Error_Msg
                 ("mixing NOT and OR is not allowed, parentheses are required",
                  Token_Ptr);

            elsif Current_Operator = Op_And then
               Error_Msg ("mixing AND and OR is not allowed", Token_Ptr);
            end if;

            Current_Operator := Op_Or;
            Scan.all;

            if Token = Tok_Else then
               Scan.all;

               if Final_Result then
                  Evaluation := False;
               end if;
            end if;

         --  No AND/OR operator, so exit from the loop through terms

         else
            exit;
         end if;
      end loop;

      return Final_Result;
   end Expression;

   -----------------------
   -- Go_To_End_Of_Line --
   -----------------------

   procedure Go_To_End_Of_Line is
   begin
      --  Scan until we get an end of line or we reach the end of the buffer

      while Token /= Tok_End_Of_Line
        and then Token /= Tok_EOF
      loop
         Scan.all;
      end loop;
   end Go_To_End_Of_Line;

   --------------
   -- Index_Of --
   --------------

   function Index_Of (Symbol : Name_Id) return Symbol_Id is
   begin
      if Mapping.Table /= null then
         for J in Symbol_Id range 1 .. Symbol_Table.Last (Mapping) loop
            if Mapping.Table (J).Symbol = Symbol then
               return J;
            end if;
         end loop;
      end if;

      return No_Symbol;
   end Index_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Already_Initialized then
         Start_String;
         Store_String_Chars ("True");
         True_Value.Value := End_String;

         Start_String;
         Empty_String := End_String;

         Start_String;
         Store_String_Chars ("False");
         String_False := End_String;

         Already_Initialized := True;
      end if;
   end Initialize;

   ------------------
   -- List_Symbols --
   ------------------

   procedure List_Symbols (Foreword : String) is
      Order : array (0 .. Integer (Symbol_Table.Last (Mapping)))
                 of Symbol_Id;
      --  After alphabetical sorting, this array stores the indexes of the
      --  symbols in the order they are displayed.

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Comparison routine for sort call

      procedure Move (From : Natural; To : Natural);
      --  Move routine for sort call

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean is
         S1 : constant String :=
                Get_Name_String (Mapping.Table (Order (Op1)).Symbol);
         S2 : constant String :=
                Get_Name_String (Mapping.Table (Order (Op2)).Symbol);
      begin
         return S1 < S2;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From : Natural; To : Natural) is
      begin
         Order (To) := Order (From);
      end Move;

      package Sort_Syms is new GNAT.Heap_Sort_G (Move, Lt);

      Max_L : Natural;
      --  Maximum length of any symbol

   --  Start of processing for List_Symbols_Case

   begin
      if Symbol_Table.Last (Mapping) = 0 then
         return;
      end if;

      if Foreword'Length > 0 then
         Write_Eol;
         Write_Line (Foreword);

         for J in Foreword'Range loop
            Write_Char ('=');
         end loop;
      end if;

      --  Initialize the order

      for J in Order'Range loop
         Order (J) := Symbol_Id (J);
      end loop;

      --  Sort alphabetically

      Sort_Syms.Sort (Order'Last);

      Max_L := 7;

      for J in 1 .. Symbol_Table.Last (Mapping) loop
         Get_Name_String (Mapping.Table (J).Original);
         Max_L := Integer'Max (Max_L, Name_Len);
      end loop;

      Write_Eol;
      Write_Str ("Symbol");

      for J in 1 .. Max_L - 5 loop
         Write_Char (' ');
      end loop;

      Write_Line ("Value");

      Write_Str ("------");

      for J in 1 .. Max_L - 5 loop
         Write_Char (' ');
      end loop;

      Write_Line ("------");

      for J in 1 .. Order'Last loop
         declare
            Data : constant Symbol_Data := Mapping.Table (Order (J));

         begin
            Get_Name_String (Data.Original);
            Write_Str (Name_Buffer (1 .. Name_Len));

            for K in Name_Len .. Max_L loop
               Write_Char (' ');
            end loop;

            String_To_Name_Buffer (Data.Value);

            if Data.Is_A_String then
               Write_Char ('"');

               for J in 1 .. Name_Len loop
                  Write_Char (Name_Buffer (J));

                  if Name_Buffer (J) = '"' then
                     Write_Char ('"');
                  end if;
               end loop;

               Write_Char ('"');

            else
               Write_Str (Name_Buffer (1 .. Name_Len));
            end if;
         end;

         Write_Eol;
      end loop;

      Write_Eol;
   end List_Symbols;

   ----------------------
   -- Matching_Strings --
   ----------------------

   function Matching_Strings (S1, S2 : String_Id) return Boolean is
   begin
      String_To_Name_Buffer (S1);

      for Index in 1 .. Name_Len loop
         Name_Buffer (Index) := Fold_Lower (Name_Buffer (Index));
      end loop;

      declare
         String1 : constant String := Name_Buffer (1 .. Name_Len);

      begin
         String_To_Name_Buffer (S2);

         for Index in 1 .. Name_Len loop
            Name_Buffer (Index) := Fold_Lower (Name_Buffer (Index));
         end loop;

         return String1 = Name_Buffer (1 .. Name_Len);
      end;
   end Matching_Strings;

   --------------------
   -- Parse_Def_File --
   --------------------

   --  This procedure REALLY needs some more comments ???

   procedure Parse_Def_File is
      Symbol        : Symbol_Id;
      Symbol_Name   : Name_Id;
      Original_Name : Name_Id;
      Data          : Symbol_Data;
      Value_Start   : Source_Ptr;
      Value_End     : Source_Ptr;
      Ch            : Character;

      use ASCII;

   begin
      Def_Line_Loop :
      loop
         Scan.all;

         exit Def_Line_Loop when Token = Tok_EOF;

         if Token /= Tok_End_Of_Line then
            Change_Reserved_Keyword_To_Symbol;

            if Token /= Tok_Identifier then
               Error_Msg ("identifier expected", Token_Ptr);
               goto Cleanup;
            end if;

            Symbol_Name := Token_Name;
            Name_Len := 0;

            for Ptr in Token_Ptr .. Scan_Ptr - 1 loop
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := Sinput.Source (Ptr);
            end loop;

            Original_Name := Name_Find;
            Scan.all;

            if Token /= Tok_Colon_Equal then
               Error_Msg -- CODEFIX
                 ("`:=` expected", Token_Ptr);
               goto Cleanup;
            end if;

            Scan.all;

            if Token = Tok_Integer_Literal then
               declare
                  Ptr : Source_Ptr := Token_Ptr;

               begin
                  Start_String;
                  while Ptr < Scan_Ptr loop
                     Store_String_Char (Sinput.Source (Ptr));
                     Ptr := Ptr + 1;
                  end loop;

                  Data := (Symbol              => Symbol_Name,
                           Original            => Original_Name,
                           On_The_Command_Line => False,
                           Is_A_String         => False,
                           Value               => End_String);
               end;

               Scan.all;

               if Token /= Tok_End_Of_Line and then Token /= Tok_EOF then
                  Error_Msg ("extraneous text in definition", Token_Ptr);
                  goto Cleanup;
               end if;

            elsif Token = Tok_String_Literal then
               Data := (Symbol              => Symbol_Name,
                        Original            => Original_Name,
                        On_The_Command_Line => False,
                        Is_A_String         => True,
                        Value               => String_Literal_Id);

               Scan.all;

               if Token /= Tok_End_Of_Line and then Token /= Tok_EOF then
                  Error_Msg ("extraneous text in definition", Token_Ptr);
                  goto Cleanup;
               end if;

            elsif Token = Tok_End_Of_Line or else Token = Tok_EOF then
               Data := (Symbol              => Symbol_Name,
                        Original            => Original_Name,
                        On_The_Command_Line => False,
                        Is_A_String         => False,
                        Value               => Empty_String);

            else
               Value_Start := Token_Ptr;
               Value_End   := Token_Ptr - 1;
               Scan_Ptr    := Token_Ptr;

               Value_Chars_Loop :
               loop
                  Ch := Sinput.Source (Scan_Ptr);

                  case Ch is
                     when '_' | '.' | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' =>
                        Value_End := Scan_Ptr;
                        Scan_Ptr := Scan_Ptr + 1;

                     when ' ' | HT | VT | CR | LF | FF =>
                        exit Value_Chars_Loop;

                     when others =>
                        Error_Msg ("illegal character", Scan_Ptr);
                        goto Cleanup;
                  end case;
               end loop Value_Chars_Loop;

               Scan.all;

               if Token /= Tok_End_Of_Line and then Token /= Tok_EOF then
                  Error_Msg ("extraneous text in definition", Token_Ptr);
                  goto Cleanup;
               end if;

               Start_String;

               while Value_Start <= Value_End loop
                  Store_String_Char (Sinput.Source (Value_Start));
                  Value_Start := Value_Start + 1;
               end loop;

               Data := (Symbol              => Symbol_Name,
                        Original            => Original_Name,
                        On_The_Command_Line => False,
                        Is_A_String         => False,
                        Value               => End_String);
            end if;

            --  Now that we have the value, get the symbol index

            Symbol := Index_Of (Symbol_Name);

            if Symbol /= No_Symbol then

               --  If we already have an entry for this symbol, replace it
               --  with the new value, except if the symbol was declared on
               --  the command line.

               if Mapping.Table (Symbol).On_The_Command_Line then
                  goto Continue;
               end if;

            else
               --  As it is the first time we see this symbol, create a new
               --  entry in the table.

               if Mapping.Table = null then
                  Symbol_Table.Init (Mapping);
               end if;

               Symbol_Table.Increment_Last (Mapping);
               Symbol := Symbol_Table.Last (Mapping);
            end if;

            Mapping.Table (Symbol) := Data;
            goto Continue;

            <<Cleanup>>
               Set_Ignore_Errors (To => True);

               while Token /= Tok_End_Of_Line and then Token /= Tok_EOF loop
                  Scan.all;
               end loop;

               Set_Ignore_Errors (To => False);

            <<Continue>>
               null;
         end if;
      end loop Def_Line_Loop;
   end Parse_Def_File;

   ----------------
   -- Preprocess --
   ----------------

   procedure Preprocess (Source_Modified : out Boolean) is
      Start_Of_Processing : Source_Ptr;
      Cond                : Boolean;
      Preprocessor_Line   : Boolean := False;
      No_Error_Found      : Boolean := True;
      Modified            : Boolean := False;

      procedure Output (From, To : Source_Ptr);
      --  Output the characters with indexes From .. To in the buffer to the
      --  output file.

      procedure Output_Line (From, To : Source_Ptr);
      --  Output a line or the end of a line from the buffer to the output
      --  file, followed by an end of line terminator. Depending on the value
      --  of Deleting and the switches, the line may be commented out, blank or
      --  not output at all.

      ------------
      -- Output --
      ------------

      procedure Output (From, To : Source_Ptr) is
      begin
         for J in From .. To loop
            Put_Char (Sinput.Source (J));
         end loop;
      end Output;

      -----------------
      -- Output_Line --
      -----------------

      procedure Output_Line (From, To : Source_Ptr) is
      begin
         if Deleting or else Preprocessor_Line then
            if Blank_Deleted_Lines then
               New_EOL.all;

            elsif Comment_Deleted_Lines then
               Put_Char ('-');
               Put_Char ('-');
               Put_Char ('!');

               if From < To then
                  Put_Char (' ');
                  Output (From, To);
               end if;

               New_EOL.all;
            end if;

         else
            Output (From, To);
            New_EOL.all;
         end if;
      end Output_Line;

   --  Start of processing for Preprocess

   begin
      Start_Of_Processing := Scan_Ptr;

      --  First a call to Scan, because Initialize_Scanner is not doing it

      Scan.all;

      Input_Line_Loop : loop
         exit Input_Line_Loop when Token = Tok_EOF;

         Preprocessor_Line := False;

         if Token /= Tok_End_Of_Line then

            --  Preprocessor line

            if Token = Tok_Special and then Special_Character = '#' then
               Modified := True;
               Preprocessor_Line := True;
               Scan.all;

               case Token is

                  --  #if

                  when Tok_If =>
                     declare
                        If_Ptr : constant Source_Ptr := Token_Ptr;

                     begin
                        Scan.all;
                        Cond := Expression (not Deleting);

                        --  Check for an eventual "then"

                        if Token = Tok_Then then
                           Scan.all;
                        end if;

                        --  It is an error to have trailing characters after
                        --  the condition or "then".

                        if Token /= Tok_End_Of_Line
                          and then Token /= Tok_EOF
                        then
                           Error_Msg
                             ("extraneous text on preprocessor line",
                              Token_Ptr);
                           No_Error_Found := False;
                           Go_To_End_Of_Line;
                        end if;

                        declare
                           --  Set the initial state of this new "#if". This
                           --  must be done before incrementing the Last of
                           --  the table, otherwise function Deleting does
                           --  not report the correct value.

                           New_State : constant Pp_State :=
                                         (If_Ptr     => If_Ptr,
                                          Else_Ptr   => 0,
                                          Deleting   => Deleting
                                                          or else not Cond,
                                          Match_Seen => Deleting or else Cond);

                        begin
                           Pp_States.Increment_Last;
                           Pp_States.Table (Pp_States.Last) := New_State;
                        end;
                     end;

                  --  #elsif

                  when Tok_Elsif =>
                     Cond := False;

                     if Pp_States.Last = 0
                       or else Pp_States.Table (Pp_States.Last).Else_Ptr /= 0
                     then
                        Error_Msg ("no IF for this ELSIF", Token_Ptr);
                        No_Error_Found := False;

                     else
                        Cond :=
                          not Pp_States.Table (Pp_States.Last).Match_Seen;
                     end if;

                     Scan.all;
                     Cond := Expression (Cond);

                     --  Check for an eventual "then"

                     if Token = Tok_Then then
                        Scan.all;
                     end if;

                     --  It is an error to have trailing characters after the
                     --  condition or "then".

                     if Token /= Tok_End_Of_Line
                       and then Token /= Tok_EOF
                     then
                        Error_Msg
                          ("extraneous text on preprocessor line",
                           Token_Ptr);
                        No_Error_Found := False;

                        Go_To_End_Of_Line;
                     end if;

                     --  Depending on the value of the condition, set the new
                     --  values of Deleting and Match_Seen.

                     if Pp_States.Last > 0 then
                        if Pp_States.Table (Pp_States.Last).Match_Seen then
                           Pp_States.Table (Pp_States.Last).Deleting := True;
                        else
                           if Cond then
                              Pp_States.Table (Pp_States.Last).Match_Seen :=
                                True;
                              Pp_States.Table (Pp_States.Last).Deleting :=
                                False;
                           end if;
                        end if;
                     end if;

                  --  #else

                  when Tok_Else =>
                     if Pp_States.Last = 0 then
                        Error_Msg ("no IF for this ELSE", Token_Ptr);
                        No_Error_Found := False;

                     elsif
                       Pp_States.Table (Pp_States.Last).Else_Ptr /= 0
                     then
                        Error_Msg -- CODEFIX
                          ("duplicate ELSE line", Token_Ptr);
                        No_Error_Found := False;
                     end if;

                     --  Set the possibly new values of Deleting and Match_Seen

                     if Pp_States.Last > 0 then
                        if Pp_States.Table (Pp_States.Last).Match_Seen then
                           Pp_States.Table (Pp_States.Last).Deleting :=
                             True;

                        else
                           Pp_States.Table (Pp_States.Last).Match_Seen :=
                             True;
                           Pp_States.Table (Pp_States.Last).Deleting :=
                             False;
                        end if;

                        --  Set the Else_Ptr to check for illegal #elsif later

                        Pp_States.Table (Pp_States.Last).Else_Ptr :=
                          Token_Ptr;
                     end if;

                     Scan.all;

                     --  Error of character present after "#else"

                     if Token /= Tok_End_Of_Line
                       and then Token /= Tok_EOF
                     then
                        Error_Msg
                          ("extraneous text on preprocessor line",
                           Token_Ptr);
                        No_Error_Found := False;
                        Go_To_End_Of_Line;
                     end if;

                  --  #end if;

                  when Tok_End =>
                     if Pp_States.Last = 0 then
                        Error_Msg ("no IF for this END", Token_Ptr);
                        No_Error_Found := False;
                     end if;

                     Scan.all;

                     if Token /= Tok_If then
                        Error_Msg -- CODEFIX
                          ("IF expected", Token_Ptr);
                        No_Error_Found := False;

                     else
                        Scan.all;

                        if Token /= Tok_Semicolon then
                           Error_Msg -- CODEFIX
                             ("`;` Expected", Token_Ptr);
                           No_Error_Found := False;

                        else
                           Scan.all;

                           --  Error of character present after "#end if;"

                           if Token /= Tok_End_Of_Line
                             and then Token /= Tok_EOF
                           then
                              Error_Msg
                                ("extraneous text on preprocessor line",
                                 Token_Ptr);
                              No_Error_Found := False;
                           end if;
                        end if;
                     end if;

                     --  In case of one of the errors above, skip the tokens
                     --  until the end of line is reached.

                     Go_To_End_Of_Line;

                     --  Decrement the depth of the #if stack

                     if Pp_States.Last > 0 then
                        Pp_States.Decrement_Last;
                     end if;

                  --  Illegal preprocessor line

                  when others =>
                     No_Error_Found := False;

                     if Pp_States.Last = 0 then
                        Error_Msg -- CODEFIX
                          ("IF expected", Token_Ptr);

                     elsif
                       Pp_States.Table (Pp_States.Last).Else_Ptr = 0
                     then
                        Error_Msg
                          ("IF, ELSIF, ELSE, or `END IF` expected",
                           Token_Ptr);

                     else
                        Error_Msg ("IF or `END IF` expected", Token_Ptr);
                     end if;

                     --  Skip to the end of this illegal line

                     Go_To_End_Of_Line;
               end case;

            --  Not a preprocessor line

            else
               --  Do not report errors for those lines, even if there are
               --  Ada parsing errors.

               Set_Ignore_Errors (To => True);

               if Deleting then
                  Go_To_End_Of_Line;

               else
                  while Token /= Tok_End_Of_Line
                    and then Token /= Tok_EOF
                  loop
                     if Token = Tok_Special
                       and then Special_Character = '$'
                     then
                        Modified := True;

                        declare
                           Dollar_Ptr : constant Source_Ptr := Token_Ptr;
                           Symbol     : Symbol_Id;

                        begin
                           Scan.all;
                           Change_Reserved_Keyword_To_Symbol;

                           if Token = Tok_Identifier
                             and then Token_Ptr = Dollar_Ptr + 1
                           then
                              --  $symbol

                              Symbol := Index_Of (Token_Name);

                              --  If symbol exists, replace by its value

                              if Symbol /= No_Symbol then
                                 Output (Start_Of_Processing, Dollar_Ptr - 1);
                                 Start_Of_Processing := Scan_Ptr;
                                 String_To_Name_Buffer
                                   (Mapping.Table (Symbol).Value);

                                 if Mapping.Table (Symbol).Is_A_String then

                                    --  Value is an Ada string

                                    Put_Char ('"');

                                    for J in 1 .. Name_Len loop
                                       Put_Char (Name_Buffer (J));

                                       if Name_Buffer (J) = '"' then
                                          Put_Char ('"');
                                       end if;
                                    end loop;

                                    Put_Char ('"');

                                 else
                                    --  Value is a sequence of characters, not
                                    --  an Ada string.

                                    for J in 1 .. Name_Len loop
                                       Put_Char (Name_Buffer (J));
                                    end loop;
                                 end if;
                              end if;
                           end if;
                        end;
                     end if;

                     Scan.all;
                  end loop;
               end if;

               Set_Ignore_Errors (To => False);
            end if;
         end if;

         pragma Assert (Token = Tok_End_Of_Line or else Token = Tok_EOF);

         --  At this point, the token is either end of line or EOF. The line to
         --  possibly output stops just before the token.

         Output_Line (Start_Of_Processing, Token_Ptr - 1);

         --  If we are at the end of a line, the scan pointer is at the first
         --  non-blank character (may not be the first character of the line),
         --  so we have to deduct Start_Of_Processing from the token pointer.

         if Token = Tok_End_Of_Line then
            if Sinput.Source (Token_Ptr) = ASCII.CR
              and then Sinput.Source (Token_Ptr + 1) = ASCII.LF
            then
               Start_Of_Processing := Token_Ptr + 2;
            else
               Start_Of_Processing := Token_Ptr + 1;
            end if;
         end if;

         --  Now, scan the first token of the next line. If the token is EOF,
         --  the scan pointer will not move, and the token will still be EOF.

         Set_Ignore_Errors (To => True);
         Scan.all;
         Set_Ignore_Errors (To => False);
      end loop Input_Line_Loop;

      --  Report an error for any missing some "#end if;"

      for Level in reverse 1 .. Pp_States.Last loop
         Error_Msg ("no `END IF` for this IF", Pp_States.Table (Level).If_Ptr);
         No_Error_Found := False;
      end loop;

      Source_Modified := No_Error_Found and Modified;
   end Preprocess;

   -----------------
   -- Setup_Hooks --
   -----------------

   procedure Setup_Hooks
     (Error_Msg         : Error_Msg_Proc;
      Scan              : Scan_Proc;
      Set_Ignore_Errors : Set_Ignore_Errors_Proc;
      Put_Char          : Put_Char_Proc;
      New_EOL           : New_EOL_Proc)
   is
   begin
      pragma Assert (Already_Initialized);

      Prep.Error_Msg         := Error_Msg;
      Prep.Scan              := Scan;
      Prep.Set_Ignore_Errors := Set_Ignore_Errors;
      Prep.Put_Char          := Put_Char;
      Prep.New_EOL           := New_EOL;
   end Setup_Hooks;

end Prep;
