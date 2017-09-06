------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ D I M                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2017, Free Software Foundation, Inc.         --
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
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

with GNAT.HTable;

package body Sem_Dim is

   -------------------------
   -- Rational Arithmetic --
   -------------------------

   type Whole is new Int;
   subtype Positive_Whole is Whole range 1 .. Whole'Last;

   type Rational is record
      Numerator   : Whole;
      Denominator : Positive_Whole;
   end record;

   Zero : constant Rational := Rational'(Numerator =>   0,
                                         Denominator => 1);

   No_Rational : constant Rational := Rational'(Numerator =>   0,
                                                Denominator => 2);
   --  Used to indicate an expression that cannot be interpreted as a rational
   --  Returned value of the Create_Rational_From routine when parameter Expr
   --  is not a static representation of a rational.

   --  Rational constructors

   function "+" (Right : Whole) return Rational;
   function GCD (Left, Right : Whole) return Int;
   function Reduce (X : Rational) return Rational;

   --  Unary operator for Rational

   function "-" (Right : Rational) return Rational;
   function "abs" (Right : Rational) return Rational;

   --  Rational operations for Rationals

   function "+" (Left, Right : Rational) return Rational;
   function "-" (Left, Right : Rational) return Rational;
   function "*" (Left, Right : Rational) return Rational;
   function "/" (Left, Right : Rational) return Rational;

   ------------------
   -- System Types --
   ------------------

   Max_Number_Of_Dimensions : constant := 7;
   --  Maximum number of dimensions in a dimension system

   High_Position_Bound : constant := Max_Number_Of_Dimensions;
   Invalid_Position    : constant := 0;
   Low_Position_Bound  : constant := 1;

   subtype Dimension_Position is
     Nat range Invalid_Position .. High_Position_Bound;

   type Name_Array is
     array (Dimension_Position range
              Low_Position_Bound .. High_Position_Bound) of Name_Id;
   --  Store the names of all units within a system

   No_Names : constant Name_Array := (others => No_Name);

   type Symbol_Array is
     array (Dimension_Position range
              Low_Position_Bound ..  High_Position_Bound) of String_Id;
   --  Store the symbols of all units within a system

   No_Symbols : constant Symbol_Array := (others => No_String);

   --  The following record should be documented field by field

   type System_Type is record
      Type_Decl    : Node_Id;
      Unit_Names   : Name_Array;
      Unit_Symbols : Symbol_Array;
      Dim_Symbols  : Symbol_Array;
      Count        : Dimension_Position;
   end record;

   Null_System : constant System_Type :=
                   (Empty, No_Names, No_Symbols, No_Symbols, Invalid_Position);

   subtype System_Id is Nat;

   --  The following table maps types to systems

   package System_Table is new Table.Table (
     Table_Component_Type => System_Type,
     Table_Index_Type     => System_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 5,
     Table_Increment      => 5,
     Table_Name           => "System_Table");

   --------------------
   -- Dimension Type --
   --------------------

   type Dimension_Type is
     array (Dimension_Position range
              Low_Position_Bound ..  High_Position_Bound) of Rational;

   Null_Dimension : constant Dimension_Type := (others => Zero);

   type Dimension_Table_Range is range 0 .. 510;
   function Dimension_Table_Hash (Key : Node_Id) return Dimension_Table_Range;

   --  The following table associates nodes with dimensions

   package Dimension_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => Dimension_Table_Range,
        Element    => Dimension_Type,
        No_Element => Null_Dimension,
        Key        => Node_Id,
        Hash       => Dimension_Table_Hash,
        Equal      => "=");

   ------------------
   -- Symbol Types --
   ------------------

   type Symbol_Table_Range is range 0 .. 510;
   function Symbol_Table_Hash (Key : Entity_Id) return Symbol_Table_Range;

   --  Each subtype with a dimension has a symbolic representation of the
   --  related unit. This table establishes a relation between the subtype
   --  and the symbol.

   package Symbol_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => Symbol_Table_Range,
        Element    => String_Id,
        No_Element => No_String,
        Key        => Entity_Id,
        Hash       => Symbol_Table_Hash,
        Equal      => "=");

   --  The following array enumerates all contexts which may contain or
   --  produce a dimension.

   OK_For_Dimension : constant array (Node_Kind) of Boolean :=
     (N_Attribute_Reference       => True,
      N_Expanded_Name             => True,
      N_Explicit_Dereference      => True,
      N_Defining_Identifier       => True,
      N_Function_Call             => True,
      N_Identifier                => True,
      N_Indexed_Component         => True,
      N_Integer_Literal           => True,
      N_Op_Abs                    => True,
      N_Op_Add                    => True,
      N_Op_Divide                 => True,
      N_Op_Expon                  => True,
      N_Op_Minus                  => True,
      N_Op_Mod                    => True,
      N_Op_Multiply               => True,
      N_Op_Plus                   => True,
      N_Op_Rem                    => True,
      N_Op_Subtract               => True,
      N_Qualified_Expression      => True,
      N_Real_Literal              => True,
      N_Selected_Component        => True,
      N_Slice                     => True,
      N_Type_Conversion           => True,
      N_Unchecked_Type_Conversion => True,

      others                      => False);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Dimension_Assignment_Statement (N : Node_Id);
   --  Subroutine of Analyze_Dimension for assignment statement. Check that the
   --  dimensions of the left-hand side and the right-hand side of N match.

   procedure Analyze_Dimension_Binary_Op (N : Node_Id);
   --  Subroutine of Analyze_Dimension for binary operators. Check the
   --  dimensions of the right and the left operand permit the operation.
   --  Then, evaluate the resulting dimensions for each binary operator.

   procedure Analyze_Dimension_Component_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for component declaration. Check that
   --  the dimensions of the type of N and of the expression match.

   procedure Analyze_Dimension_Extended_Return_Statement (N : Node_Id);
   --  Subroutine of Analyze_Dimension for extended return statement. Check
   --  that the dimensions of the returned type and of the returned object
   --  match.

   procedure Analyze_Dimension_Has_Etype (N : Node_Id);
   --  Subroutine of Analyze_Dimension for a subset of N_Has_Etype denoted by
   --  the list below:
   --    N_Attribute_Reference
   --    N_Identifier
   --    N_Indexed_Component
   --    N_Qualified_Expression
   --    N_Selected_Component
   --    N_Slice
   --    N_Type_Conversion
   --    N_Unchecked_Type_Conversion

   procedure Analyze_Dimension_Number_Declaration (N : Node_Id);
   --  Procedure to analyze dimension of expression in a number declaration.
   --  This allows a named number to have nontrivial dimensions, while by
   --  default a named number is dimensionless.

   procedure Analyze_Dimension_Object_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for object declaration. Check that
   --  the dimensions of the object type and the dimensions of the expression
   --  (if expression is present) match. Note that when the expression is
   --  a literal, no error is returned. This special case allows object
   --  declaration such as: m : constant Length := 1.0;

   procedure Analyze_Dimension_Object_Renaming_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for object renaming declaration. Check
   --  the dimensions of the type and of the renamed object name of N match.

   procedure Analyze_Dimension_Simple_Return_Statement (N : Node_Id);
   --  Subroutine of Analyze_Dimension for simple return statement
   --  Check that the dimensions of the returned type and of the returned
   --  expression match.

   procedure Analyze_Dimension_Subtype_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for subtype declaration. Propagate the
   --  dimensions from the parent type to the identifier of N. Note that if
   --  both the identifier and the parent type of N are not dimensionless,
   --  return an error.

   procedure Analyze_Dimension_Unary_Op (N : Node_Id);
   --  Subroutine of Analyze_Dimension for unary operators. For Plus, Minus and
   --  Abs operators, propagate the dimensions from the operand to N.

   function Create_Rational_From
     (Expr     : Node_Id;
      Complain : Boolean) return Rational;
   --  Given an arbitrary expression Expr, return a valid rational if Expr can
   --  be interpreted as a rational. Otherwise return No_Rational and also an
   --  error message if Complain is set to True.

   function Dimensions_Of (N : Node_Id) return Dimension_Type;
   --  Return the dimension vector of node N

   function Dimensions_Msg_Of
      (N                  : Node_Id;
       Description_Needed : Boolean := False) return String;
   --  Given a node N, return the dimension symbols of N, preceded by "has
   --  dimension" if Description_Needed. if N is dimensionless, return "'[']",
   --  or "is dimensionless" if Description_Needed.

   procedure Dim_Warning_For_Numeric_Literal (N : Node_Id; Typ : Entity_Id);
   --  Issue a warning on the given numeric literal N to indicate that the
   --  compiler made the assumption that the literal is not dimensionless
   --  but has the dimension of Typ.

   procedure Eval_Op_Expon_With_Rational_Exponent
     (N              : Node_Id;
      Exponent_Value : Rational);
   --  Evaluate the exponent it is a rational and the operand has a dimension

   function Exists (Dim : Dimension_Type) return Boolean;
   --  Returns True iff Dim does not denote the null dimension

   function Exists (Str : String_Id) return Boolean;
   --  Returns True iff Str does not denote No_String

   function Exists (Sys : System_Type) return Boolean;
   --  Returns True iff Sys does not denote the null system

   function From_Dim_To_Str_Of_Dim_Symbols
     (Dims         : Dimension_Type;
      System       : System_Type;
      In_Error_Msg : Boolean := False) return String_Id;
   --  Given a dimension vector and a dimension system, return the proper
   --  string of dimension symbols. If In_Error_Msg is True (i.e. the String_Id
   --  will be used to issue an error message) then this routine has a special
   --  handling for the insertion characters * or [ which must be preceded by
   --  a quote ' to be placed literally into the message.

   function From_Dim_To_Str_Of_Unit_Symbols
     (Dims   : Dimension_Type;
      System : System_Type) return String_Id;
   --  Given a dimension vector and a dimension system, return the proper
   --  string of unit symbols.

   function Is_Dim_IO_Package_Entity (E : Entity_Id) return Boolean;
   --  Return True if E is the package entity of System.Dim.Float_IO or
   --  System.Dim.Integer_IO.

   function Is_Invalid (Position : Dimension_Position) return Boolean;
   --  Return True if Pos denotes the invalid position

   procedure Move_Dimensions (From : Node_Id; To : Node_Id);
   --  Copy dimension vector of From to To and delete dimension vector of From

   procedure Remove_Dimensions (N : Node_Id);
   --  Remove the dimension vector of node N

   procedure Set_Dimensions (N : Node_Id; Val : Dimension_Type);
   --  Associate a dimension vector with a node

   procedure Set_Symbol (E : Entity_Id; Val : String_Id);
   --  Associate a symbol representation of a dimension vector with a subtype

   function String_From_Numeric_Literal (N : Node_Id) return String_Id;
   --  Return the string that corresponds to the numeric litteral N as it
   --  appears in the source.

   function Symbol_Of (E : Entity_Id) return String_Id;
   --  E denotes a subtype with a dimension. Return the symbol representation
   --  of the dimension vector.

   function System_Of (E : Entity_Id) return System_Type;
   --  E denotes a type, return associated system of the type if it has one

   ---------
   -- "+" --
   ---------

   function "+" (Right : Whole) return Rational is
   begin
      return Rational'(Numerator => Right, Denominator => 1);
   end "+";

   function "+" (Left, Right : Rational) return Rational is
      R : constant Rational :=
            Rational'(Numerator   =>  Left.Numerator   * Right.Denominator +
                                      Left.Denominator * Right.Numerator,
                      Denominator => Left.Denominator  * Right.Denominator);
   begin
      return Reduce (R);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Right : Rational) return Rational is
   begin
      return Rational'(Numerator   => -Right.Numerator,
                       Denominator => Right.Denominator);
   end "-";

   function "-" (Left, Right : Rational) return Rational is
      R : constant Rational :=
            Rational'(Numerator   => Left.Numerator   * Right.Denominator -
                                     Left.Denominator * Right.Numerator,
                      Denominator => Left.Denominator * Right.Denominator);

   begin
      return Reduce (R);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Rational) return Rational is
      R : constant Rational :=
            Rational'(Numerator   => Left.Numerator   * Right.Numerator,
                      Denominator => Left.Denominator * Right.Denominator);
   begin
      return Reduce (R);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Rational) return Rational is
      R : constant Rational := abs Right;
      L : Rational := Left;

   begin
      if Right.Numerator < 0 then
         L.Numerator := Whole (-Integer (L.Numerator));
      end if;

      return Reduce (Rational'(Numerator   => L.Numerator   * R.Denominator,
                               Denominator => L.Denominator * R.Numerator));
   end "/";

   -----------
   -- "abs" --
   -----------

   function "abs" (Right : Rational) return Rational is
   begin
      return Rational'(Numerator   => abs Right.Numerator,
                       Denominator => Right.Denominator);
   end "abs";

   ------------------------------
   -- Analyze_Aspect_Dimension --
   ------------------------------

   --  with Dimension =>
   --    ([Symbol =>] SYMBOL, DIMENSION_VALUE {, DIMENSION_Value})
   --
   --  SYMBOL ::= STRING_LITERAL | CHARACTER_LITERAL

   --  DIMENSION_VALUE ::=
   --    RATIONAL
   --  | others               => RATIONAL
   --  | DISCRETE_CHOICE_LIST => RATIONAL

   --  RATIONAL ::= [-] NUMERIC_LITERAL [/ NUMERIC_LITERAL]

   --  Note that when the dimensioned type is an integer type, then any
   --  dimension value must be an integer literal.

   procedure Analyze_Aspect_Dimension
     (N    : Node_Id;
      Id   : Entity_Id;
      Aggr : Node_Id)
   is
      Def_Id : constant Entity_Id := Defining_Identifier (N);

      Processed : array (Dimension_Type'Range) of Boolean := (others => False);
      --  This array is used when processing ranges or Others_Choice as part of
      --  the dimension aggregate.

      Dimensions : Dimension_Type := Null_Dimension;

      procedure Extract_Power
        (Expr     : Node_Id;
         Position : Dimension_Position);
      --  Given an expression with denotes a rational number, read the number
      --  and associate it with Position in Dimensions.

      function Position_In_System
        (Id     : Node_Id;
         System : System_Type) return Dimension_Position;
      --  Given an identifier which denotes a dimension, return the position of
      --  that dimension within System.

      -------------------
      -- Extract_Power --
      -------------------

      procedure Extract_Power
        (Expr     : Node_Id;
         Position : Dimension_Position)
      is
      begin
         --  Integer case

         if Is_Integer_Type (Def_Id) then

            --  Dimension value must be an integer literal

            if Nkind (Expr) = N_Integer_Literal then
               Dimensions (Position) := +Whole (UI_To_Int (Intval (Expr)));
            else
               Error_Msg_N ("integer literal expected", Expr);
            end if;

         --  Float case

         else
            Dimensions (Position) := Create_Rational_From (Expr, True);
         end if;

         Processed (Position) := True;
      end Extract_Power;

      ------------------------
      -- Position_In_System --
      ------------------------

      function Position_In_System
        (Id     : Node_Id;
         System : System_Type) return Dimension_Position
      is
         Dimension_Name : constant Name_Id := Chars (Id);

      begin
         for Position in System.Unit_Names'Range loop
            if Dimension_Name = System.Unit_Names (Position) then
               return Position;
            end if;
         end loop;

         return Invalid_Position;
      end Position_In_System;

      --  Local variables

      Assoc          : Node_Id;
      Choice         : Node_Id;
      Expr           : Node_Id;
      Num_Choices    : Nat := 0;
      Num_Dimensions : Nat := 0;
      Others_Seen    : Boolean := False;
      Position       : Nat := 0;
      Sub_Ind        : Node_Id;
      Symbol         : String_Id := No_String;
      Symbol_Expr    : Node_Id;
      System         : System_Type;
      Typ            : Entity_Id;

      Errors_Count : Nat;
      --  Errors_Count is a count of errors detected by the compiler so far
      --  just before the extraction of symbol, names and values in the
      --  aggregate (Step 2).
      --
      --  At the end of the analysis, there is a check to verify that this
      --  count equals to Serious_Errors_Detected i.e. no erros have been
      --  encountered during the process. Otherwise the Dimension_Table is
      --  not filled.

   --  Start of processing for Analyze_Aspect_Dimension

   begin
      --  STEP 1: Legality of aspect

      if Nkind (N) /= N_Subtype_Declaration then
         Error_Msg_NE ("aspect& must apply to subtype declaration", N, Id);
         return;
      end if;

      Sub_Ind := Subtype_Indication (N);
      Typ := Etype (Sub_Ind);
      System := System_Of (Typ);

      if Nkind (Sub_Ind) = N_Subtype_Indication then
         Error_Msg_NE
           ("constraint not allowed with aspect&", Constraint (Sub_Ind), Id);
         return;
      end if;

      --  The dimension declarations are useless if the parent type does not
      --  declare a valid system.

      if not Exists (System) then
         Error_Msg_NE
           ("parent type of& lacks dimension system", Sub_Ind, Def_Id);
         return;
      end if;

      if Nkind (Aggr) /= N_Aggregate then
         Error_Msg_N ("aggregate expected", Aggr);
         return;
      end if;

      --  STEP 2: Symbol, Names and values extraction

      --  Get the number of errors detected by the compiler so far

      Errors_Count := Serious_Errors_Detected;

      --  STEP 2a: Symbol extraction

      --  The first entry in the aggregate may be the symbolic representation
      --  of the quantity.

      --  Positional symbol argument

      Symbol_Expr := First (Expressions (Aggr));

      --  Named symbol argument

      if No (Symbol_Expr)
        or else not Nkind_In (Symbol_Expr, N_Character_Literal,
                                           N_String_Literal)
      then
         Symbol_Expr := Empty;

         --  Component associations present

         if Present (Component_Associations (Aggr)) then
            Assoc  := First (Component_Associations (Aggr));
            Choice := First (Choices (Assoc));

            if No (Next (Choice)) and then Nkind (Choice) = N_Identifier then

               --  Symbol component association is present

               if Chars (Choice) = Name_Symbol then
                  Num_Choices := Num_Choices + 1;
                  Symbol_Expr := Expression (Assoc);

                  --  Verify symbol expression is a string or a character

                  if not Nkind_In (Symbol_Expr, N_Character_Literal,
                                                N_String_Literal)
                  then
                     Symbol_Expr := Empty;
                     Error_Msg_N
                       ("symbol expression must be character or string",
                        Symbol_Expr);
                  end if;

               --  Special error if no Symbol choice but expression is string
               --  or character.

               elsif Nkind_In (Expression (Assoc), N_Character_Literal,
                                                   N_String_Literal)
               then
                  Num_Choices := Num_Choices + 1;
                  Error_Msg_N
                    ("optional component Symbol expected, found&", Choice);
               end if;
            end if;
         end if;
      end if;

      --  STEP 2b: Names and values extraction

      --  Positional elements

      Expr := First (Expressions (Aggr));

      --  Skip the symbol expression when present

      if Present (Symbol_Expr) and then Num_Choices = 0 then
         Expr := Next (Expr);
      end if;

      Position := Low_Position_Bound;
      while Present (Expr) loop
         if Position > High_Position_Bound then
            Error_Msg_N
              ("type& has more dimensions than system allows", Def_Id);
            exit;
         end if;

         Extract_Power (Expr, Position);

         Position := Position + 1;
         Num_Dimensions := Num_Dimensions + 1;

         Next (Expr);
      end loop;

      --  Named elements

      Assoc := First (Component_Associations (Aggr));

      --  Skip the symbol association when present

      if Num_Choices = 1 then
         Next (Assoc);
      end if;

      while Present (Assoc) loop
         Expr := Expression (Assoc);

         Choice := First (Choices (Assoc));
         while Present (Choice) loop

            --  Identifier case: NAME => EXPRESSION

            if Nkind (Choice) = N_Identifier then
               Position := Position_In_System (Choice, System);

               if Is_Invalid (Position) then
                  Error_Msg_N ("dimension name& not part of system", Choice);
               else
                  Extract_Power (Expr, Position);
               end if;

            --  Range case: NAME .. NAME => EXPRESSION

            elsif Nkind (Choice) = N_Range then
               declare
                  Low      : constant Node_Id := Low_Bound (Choice);
                  High     : constant Node_Id := High_Bound (Choice);
                  Low_Pos  : Dimension_Position;
                  High_Pos : Dimension_Position;

               begin
                  if Nkind (Low) /= N_Identifier then
                     Error_Msg_N ("bound must denote a dimension name", Low);

                  elsif Nkind (High) /= N_Identifier then
                     Error_Msg_N ("bound must denote a dimension name", High);

                  else
                     Low_Pos  := Position_In_System (Low, System);
                     High_Pos := Position_In_System (High, System);

                     if Is_Invalid (Low_Pos) then
                        Error_Msg_N ("dimension name& not part of system",
                                     Low);

                     elsif Is_Invalid (High_Pos) then
                        Error_Msg_N ("dimension name& not part of system",
                                     High);

                     elsif Low_Pos > High_Pos then
                        Error_Msg_N ("expected low to high range", Choice);

                     else
                        for Position in Low_Pos .. High_Pos loop
                           Extract_Power (Expr, Position);
                        end loop;
                     end if;
                  end if;
               end;

            --  Others case: OTHERS => EXPRESSION

            elsif Nkind (Choice) = N_Others_Choice then
               if Present (Next (Choice)) or else Present (Prev (Choice)) then
                  Error_Msg_N
                    ("OTHERS must appear alone in a choice list", Choice);

               elsif Present (Next (Assoc)) then
                  Error_Msg_N
                    ("OTHERS must appear last in an aggregate", Choice);

               elsif Others_Seen then
                  Error_Msg_N ("multiple OTHERS not allowed", Choice);

               else
                  --  Fill the non-processed dimensions with the default value
                  --  supplied by others.

                  for Position in Processed'Range loop
                     if not Processed (Position) then
                        Extract_Power (Expr, Position);
                     end if;
                  end loop;
               end if;

               Others_Seen := True;

            --  All other cases are illegal declarations of dimension names

            else
               Error_Msg_NE ("wrong syntax for aspect&", Choice, Id);
            end if;

            Num_Choices := Num_Choices + 1;
            Next (Choice);
         end loop;

         Num_Dimensions := Num_Dimensions + 1;
         Next (Assoc);
      end loop;

      --  STEP 3: Consistency of system and dimensions

      if Present (First (Expressions (Aggr)))
        and then (First (Expressions (Aggr)) /= Symbol_Expr
                   or else Present (Next (Symbol_Expr)))
        and then (Num_Choices > 1
                   or else (Num_Choices = 1 and then not Others_Seen))
      then
         Error_Msg_N
           ("named associations cannot follow positional associations", Aggr);
      end if;

      if Num_Dimensions > System.Count then
         Error_Msg_N ("type& has more dimensions than system allows", Def_Id);

      elsif Num_Dimensions < System.Count and then not Others_Seen then
         Error_Msg_N ("type& has less dimensions than system allows", Def_Id);
      end if;

      --  STEP 4: Dimension symbol extraction

      if Present (Symbol_Expr) then
         if Nkind (Symbol_Expr) = N_Character_Literal then
            Start_String;
            Store_String_Char (UI_To_CC (Char_Literal_Value (Symbol_Expr)));
            Symbol := End_String;

         else
            Symbol := Strval (Symbol_Expr);
         end if;

         if String_Length (Symbol) = 0 then
            Error_Msg_N ("empty string not allowed here", Symbol_Expr);
         end if;
      end if;

      --  STEP 5: Storage of extracted values

      --  Check that no errors have been detected during the analysis

      if Errors_Count = Serious_Errors_Detected then

         --  Check for useless declaration

         if Symbol = No_String and then not Exists (Dimensions) then
            Error_Msg_N ("useless dimension declaration", Aggr);
         end if;

         if Symbol /= No_String then
            Set_Symbol (Def_Id, Symbol);
         end if;

         if Exists (Dimensions) then
            Set_Dimensions (Def_Id, Dimensions);
         end if;
      end if;
   end Analyze_Aspect_Dimension;

   -------------------------------------
   -- Analyze_Aspect_Dimension_System --
   -------------------------------------

   --  with Dimension_System => (DIMENSION {, DIMENSION});

   --  DIMENSION ::= (
   --    [Unit_Name   =>] IDENTIFIER,
   --    [Unit_Symbol =>] SYMBOL,
   --    [Dim_Symbol  =>] SYMBOL)

   procedure Analyze_Aspect_Dimension_System
     (N    : Node_Id;
      Id   : Entity_Id;
      Aggr : Node_Id)
   is
      function Is_Derived_Numeric_Type (N : Node_Id) return Boolean;
      --  Determine whether type declaration N denotes a numeric derived type

      -------------------------------
      -- Is_Derived_Numeric_Type --
      -------------------------------

      function Is_Derived_Numeric_Type (N : Node_Id) return Boolean is
      begin
         return
           Nkind (N) = N_Full_Type_Declaration
             and then Nkind (Type_Definition (N)) = N_Derived_Type_Definition
             and then Is_Numeric_Type
                        (Entity (Subtype_Indication (Type_Definition (N))));
      end Is_Derived_Numeric_Type;

      --  Local variables

      Assoc        : Node_Id;
      Choice       : Node_Id;
      Dim_Aggr     : Node_Id;
      Dim_Symbol   : Node_Id;
      Dim_Symbols  : Symbol_Array := No_Symbols;
      Dim_System   : System_Type  := Null_System;
      Position     : Nat := 0;
      Unit_Name    : Node_Id;
      Unit_Names   : Name_Array   := No_Names;
      Unit_Symbol  : Node_Id;
      Unit_Symbols : Symbol_Array := No_Symbols;

      Errors_Count : Nat;
      --  Errors_Count is a count of errors detected by the compiler so far
      --  just before the extraction of names and symbols in the aggregate
      --  (Step 3).
      --
      --  At the end of the analysis, there is a check to verify that this
      --  count equals Serious_Errors_Detected i.e. no errors have been
      --  encountered during the process. Otherwise the System_Table is
      --  not filled.

   --  Start of processing for Analyze_Aspect_Dimension_System

   begin
      --  STEP 1: Legality of aspect

      if not Is_Derived_Numeric_Type (N) then
         Error_Msg_NE
           ("aspect& must apply to numeric derived type declaration", N, Id);
         return;
      end if;

      if Nkind (Aggr) /= N_Aggregate then
         Error_Msg_N ("aggregate expected", Aggr);
         return;
      end if;

      --  STEP 2: Structural verification of the dimension aggregate

      if Present (Component_Associations (Aggr)) then
         Error_Msg_N ("expected positional aggregate", Aggr);
         return;
      end if;

      --  STEP 3: Name and Symbol extraction

      Dim_Aggr     := First (Expressions (Aggr));
      Errors_Count := Serious_Errors_Detected;
      while Present (Dim_Aggr) loop
         Position := Position + 1;

         if Position > High_Position_Bound then
            Error_Msg_N ("too many dimensions in system", Aggr);
            exit;
         end if;

         if Nkind (Dim_Aggr) /= N_Aggregate then
            Error_Msg_N ("aggregate expected", Dim_Aggr);

         else
            if Present (Component_Associations (Dim_Aggr))
              and then Present (Expressions (Dim_Aggr))
            then
               Error_Msg_N
                 ("mixed positional/named aggregate not allowed here",
                  Dim_Aggr);

            --  Verify each dimension aggregate has three arguments

            elsif List_Length (Component_Associations (Dim_Aggr)) /= 3
              and then List_Length (Expressions (Dim_Aggr)) /= 3
            then
               Error_Msg_N
                 ("three components expected in aggregate", Dim_Aggr);

            else
               --  Named dimension aggregate

               if Present (Component_Associations (Dim_Aggr)) then

                  --  Check first argument denotes the unit name

                  Assoc     := First (Component_Associations (Dim_Aggr));
                  Choice    := First (Choices (Assoc));
                  Unit_Name := Expression (Assoc);

                  if Present (Next (Choice))
                    or else Nkind (Choice) /= N_Identifier
                  then
                     Error_Msg_NE ("wrong syntax for aspect&", Choice, Id);

                  elsif Chars (Choice) /= Name_Unit_Name then
                     Error_Msg_N ("expected Unit_Name, found&", Choice);
                  end if;

                  --  Check the second argument denotes the unit symbol

                  Next (Assoc);
                  Choice      := First (Choices (Assoc));
                  Unit_Symbol := Expression (Assoc);

                  if Present (Next (Choice))
                    or else Nkind (Choice) /= N_Identifier
                  then
                     Error_Msg_NE ("wrong syntax for aspect&", Choice, Id);

                  elsif Chars (Choice) /= Name_Unit_Symbol then
                     Error_Msg_N ("expected Unit_Symbol, found&", Choice);
                  end if;

                  --  Check the third argument denotes the dimension symbol

                  Next (Assoc);
                  Choice     := First (Choices (Assoc));
                  Dim_Symbol := Expression (Assoc);

                  if Present (Next (Choice))
                    or else Nkind (Choice) /= N_Identifier
                  then
                     Error_Msg_NE ("wrong syntax for aspect&", Choice, Id);
                  elsif Chars (Choice) /= Name_Dim_Symbol then
                     Error_Msg_N ("expected Dim_Symbol, found&", Choice);
                  end if;

               --  Positional dimension aggregate

               else
                  Unit_Name   := First (Expressions (Dim_Aggr));
                  Unit_Symbol := Next (Unit_Name);
                  Dim_Symbol  := Next (Unit_Symbol);
               end if;

               --  Check the first argument for each dimension aggregate is
               --  a name.

               if Nkind (Unit_Name) = N_Identifier then
                  Unit_Names (Position) := Chars (Unit_Name);
               else
                  Error_Msg_N ("expected unit name", Unit_Name);
               end if;

               --  Check the second argument for each dimension aggregate is
               --  a string or a character.

               if not Nkind_In (Unit_Symbol, N_String_Literal,
                                             N_Character_Literal)
               then
                  Error_Msg_N
                    ("expected unit symbol (string or character)",
                     Unit_Symbol);

               else
                  --  String case

                  if Nkind (Unit_Symbol) = N_String_Literal then
                     Unit_Symbols (Position) := Strval (Unit_Symbol);

                  --  Character case

                  else
                     Start_String;
                     Store_String_Char
                       (UI_To_CC (Char_Literal_Value (Unit_Symbol)));
                     Unit_Symbols (Position) := End_String;
                  end if;

                  --  Verify that the string is not empty

                  if String_Length (Unit_Symbols (Position)) = 0 then
                     Error_Msg_N
                       ("empty string not allowed here", Unit_Symbol);
                  end if;
               end if;

               --  Check the third argument for each dimension aggregate is
               --  a string or a character.

               if not Nkind_In (Dim_Symbol, N_String_Literal,
                                            N_Character_Literal)
               then
                  Error_Msg_N
                    ("expected dimension symbol (string or character)",
                     Dim_Symbol);

               else
                  --  String case

                  if Nkind (Dim_Symbol) = N_String_Literal then
                     Dim_Symbols (Position) := Strval (Dim_Symbol);

                  --  Character case

                  else
                     Start_String;
                     Store_String_Char
                       (UI_To_CC (Char_Literal_Value (Dim_Symbol)));
                     Dim_Symbols (Position) := End_String;
                  end if;

                  --  Verify that the string is not empty

                  if String_Length (Dim_Symbols (Position)) = 0 then
                     Error_Msg_N ("empty string not allowed here", Dim_Symbol);
                  end if;
               end if;
            end if;
         end if;

         Next (Dim_Aggr);
      end loop;

      --  STEP 4: Storage of extracted values

      --  Check that no errors have been detected during the analysis

      if Errors_Count = Serious_Errors_Detected then
         Dim_System.Type_Decl    := N;
         Dim_System.Unit_Names   := Unit_Names;
         Dim_System.Unit_Symbols := Unit_Symbols;
         Dim_System.Dim_Symbols  := Dim_Symbols;
         Dim_System.Count        := Position;
         System_Table.Append (Dim_System);
      end if;
   end Analyze_Aspect_Dimension_System;

   -----------------------
   -- Analyze_Dimension --
   -----------------------

   --  This dispatch routine propagates dimensions for each node

   procedure Analyze_Dimension (N : Node_Id) is
   begin
      --  Aspect is an Ada 2012 feature. Note that there is no need to check
      --  dimensions for nodes that don't come from source, except for subtype
      --  declarations where the dimensions are inherited from the base type,
      --  for explicit dereferences generated when expanding iterators, and
      --  for object declarations generated for inlining.

      if Ada_Version < Ada_2012 then
         return;

      elsif not Comes_From_Source (N) then
         if Nkind_In (N, N_Explicit_Dereference,
                         N_Identifier,
                         N_Object_Declaration,
                         N_Subtype_Declaration)
         then
            null;
         else
            return;
         end if;
      end if;

      case Nkind (N) is
         when N_Assignment_Statement =>
            Analyze_Dimension_Assignment_Statement (N);

         when N_Binary_Op =>
            Analyze_Dimension_Binary_Op (N);

         when N_Component_Declaration =>
            Analyze_Dimension_Component_Declaration (N);

         when N_Extended_Return_Statement =>
            Analyze_Dimension_Extended_Return_Statement (N);

         when N_Attribute_Reference
            | N_Expanded_Name
            | N_Explicit_Dereference
            | N_Function_Call
            | N_Indexed_Component
            | N_Qualified_Expression
            | N_Selected_Component
            | N_Slice
            | N_Unchecked_Type_Conversion
         =>
            Analyze_Dimension_Has_Etype (N);

         --  In the presence of a repaired syntax error, an identifier
         --  may be introduced without a usable type.

         when N_Identifier =>
            if Present (Etype (N)) then
               Analyze_Dimension_Has_Etype (N);
            end if;

         when N_Number_Declaration =>
            Analyze_Dimension_Number_Declaration (N);

         when N_Object_Declaration =>
            Analyze_Dimension_Object_Declaration (N);

         when N_Object_Renaming_Declaration =>
            Analyze_Dimension_Object_Renaming_Declaration (N);

         when N_Simple_Return_Statement =>
            if not Comes_From_Extended_Return_Statement (N) then
               Analyze_Dimension_Simple_Return_Statement (N);
            end if;

         when N_Subtype_Declaration =>
            Analyze_Dimension_Subtype_Declaration (N);

         when  N_Type_Conversion =>
            if In_Instance
              and then Exists (Dimensions_Of (Expression (N)))
            then
               Set_Dimensions (N, Dimensions_Of (Expression (N)));
            else
               Analyze_Dimension_Has_Etype (N);
            end if;

         when N_Unary_Op =>

            Analyze_Dimension_Unary_Op (N);

         when others =>
            null;
      end case;
   end Analyze_Dimension;

   ---------------------------------------
   -- Analyze_Dimension_Array_Aggregate --
   ---------------------------------------

   procedure Analyze_Dimension_Array_Aggregate
     (N        : Node_Id;
      Comp_Typ : Entity_Id)
   is
      Comp_Ass         : constant List_Id        := Component_Associations (N);
      Dims_Of_Comp_Typ : constant Dimension_Type := Dimensions_Of (Comp_Typ);
      Exps             : constant List_Id        := Expressions (N);

      Comp : Node_Id;
      Expr : Node_Id;

      Error_Detected : Boolean := False;
      --  This flag is used in order to indicate if an error has been detected
      --  so far by the compiler in this routine.

   begin
      --  Aspect is an Ada 2012 feature. Nothing to do here if the component
      --  base type is not a dimensioned type.

      --  Note that here the original node must come from source since the
      --  original array aggregate may not have been entirely decorated.

      if Ada_Version < Ada_2012
        or else not Comes_From_Source (Original_Node (N))
        or else not Has_Dimension_System (Base_Type (Comp_Typ))
      then
         return;
      end if;

      --  Check whether there is any positional component association

      if Is_Empty_List (Exps) then
         Comp := First (Comp_Ass);
      else
         Comp := First (Exps);
      end if;

      while Present (Comp) loop

         --  Get the expression from the component

         if Nkind (Comp) = N_Component_Association then
            Expr := Expression (Comp);
         else
            Expr := Comp;
         end if;

         --  Issue an error if the dimensions of the component type and the
         --  dimensions of the component mismatch.

         --  Note that we must ensure the expression has been fully analyzed
         --  since it may not be decorated at this point. We also don't want to
         --  issue the same error message multiple times on the same expression
         --  (may happen when an aggregate is converted into a positional
         --  aggregate). We also must verify that this is a scalar component,
         --  and not a subaggregate of a multidimensional aggregate.

         if Comes_From_Source (Original_Node (Expr))
           and then Present (Etype (Expr))
           and then Is_Numeric_Type (Etype (Expr))
           and then Dimensions_Of (Expr) /= Dims_Of_Comp_Typ
           and then Sloc (Comp) /= Sloc (Prev (Comp))
         then
            --  Check if an error has already been encountered so far

            if not Error_Detected then
               Error_Msg_N ("dimensions mismatch in array aggregate", N);
               Error_Detected := True;
            end if;

            Error_Msg_N
              ("\expected dimension " & Dimensions_Msg_Of (Comp_Typ)
               & ", found " & Dimensions_Msg_Of (Expr), Expr);
         end if;

         --  Look at the named components right after the positional components

         if not Present (Next (Comp))
           and then List_Containing (Comp) = Exps
         then
            Comp := First (Comp_Ass);
         else
            Next (Comp);
         end if;
      end loop;
   end Analyze_Dimension_Array_Aggregate;

   --------------------------------------------
   -- Analyze_Dimension_Assignment_Statement --
   --------------------------------------------

   procedure Analyze_Dimension_Assignment_Statement (N : Node_Id) is
      Lhs         : constant Node_Id := Name (N);
      Dims_Of_Lhs : constant Dimension_Type := Dimensions_Of (Lhs);
      Rhs         : constant Node_Id := Expression (N);
      Dims_Of_Rhs : constant Dimension_Type := Dimensions_Of (Rhs);

      procedure Error_Dim_Msg_For_Assignment_Statement
        (N   : Node_Id;
         Lhs : Node_Id;
         Rhs : Node_Id);
      --  Error using Error_Msg_N at node N. Output the dimensions of left
      --  and right hand sides.

      --------------------------------------------
      -- Error_Dim_Msg_For_Assignment_Statement --
      --------------------------------------------

      procedure Error_Dim_Msg_For_Assignment_Statement
        (N   : Node_Id;
         Lhs : Node_Id;
         Rhs : Node_Id)
      is
      begin
         Error_Msg_N ("dimensions mismatch in assignment", N);
         Error_Msg_N ("\left-hand side "  & Dimensions_Msg_Of (Lhs, True), N);
         Error_Msg_N ("\right-hand side " & Dimensions_Msg_Of (Rhs, True), N);
      end Error_Dim_Msg_For_Assignment_Statement;

   --  Start of processing for Analyze_Dimension_Assignment

   begin
      if Dims_Of_Lhs /= Dims_Of_Rhs then
         Error_Dim_Msg_For_Assignment_Statement (N, Lhs, Rhs);
      end if;
   end Analyze_Dimension_Assignment_Statement;

   ---------------------------------
   -- Analyze_Dimension_Binary_Op --
   ---------------------------------

   --  Check and propagate the dimensions for binary operators
   --  Note that when the dimensions mismatch, no dimension is propagated to N.

   procedure Analyze_Dimension_Binary_Op (N : Node_Id) is
      N_Kind : constant Node_Kind := Nkind (N);

      function Dimensions_Of_Operand (N : Node_Id) return Dimension_Type;
      --  If the operand is a numeric literal that comes from a declared
      --  constant, use the dimensions of the constant which were computed
      --  from the expression of the constant declaration. Otherwise the
      --  dimensions are those of the operand, or the type of the operand.
      --  This takes care of node rewritings from validity checks, where the
      --  dimensions of the operand itself may not be preserved, while the
      --  type comes from context and must have dimension information.

      procedure Error_Dim_Msg_For_Binary_Op (N, L, R : Node_Id);
      --  Error using Error_Msg_NE and Error_Msg_N at node N. Output the
      --  dimensions of both operands.

      ---------------------------
      -- Dimensions_Of_Operand --
      ---------------------------

      function Dimensions_Of_Operand (N : Node_Id) return Dimension_Type is
         Dims : constant Dimension_Type := Dimensions_Of (N);

      begin
         if Exists (Dims) then
            return Dims;

         elsif Is_Entity_Name (N) then
            return Dimensions_Of (Etype (Entity (N)));

         elsif Nkind (N) = N_Real_Literal then

            if Present (Original_Entity (N)) then
               return Dimensions_Of (Original_Entity (N));

            else
               return Dimensions_Of (Etype (N));
            end if;

         --  A type conversion may have been inserted to rewrite other
         --  expressions, e.g. function returns. Dimensions are those of
         --  the target type, unless this is a conversion in an instance,
         --  in which case the proper dimensions are those of the operand,

         elsif Nkind (N) = N_Type_Conversion then
            if In_Instance
              and then Is_Generic_Actual_Type (Etype (Expression (N)))
            then
               return Dimensions_Of (Etype (Expression (N)));

            elsif In_Instance
              and then Exists (Dimensions_Of (Expression (N)))
            then
               return Dimensions_Of (Expression (N));

            else
               return Dimensions_Of (Etype (N));
            end if;

         --  Otherwise return the default dimensions

         else
            return Dims;
         end if;
      end Dimensions_Of_Operand;

      ---------------------------------
      -- Error_Dim_Msg_For_Binary_Op --
      ---------------------------------

      procedure Error_Dim_Msg_For_Binary_Op (N, L, R : Node_Id) is
      begin
         Error_Msg_NE
           ("both operands for operation& must have same dimensions",
            N, Entity (N));
         Error_Msg_N ("\left operand "  & Dimensions_Msg_Of (L, True), N);
         Error_Msg_N ("\right operand " & Dimensions_Msg_Of (R, True), N);
      end Error_Dim_Msg_For_Binary_Op;

   --  Start of processing for Analyze_Dimension_Binary_Op

   begin
      --  If the node is already analyzed, do not examine the operands. At the
      --  end of the analysis their dimensions have been removed, and the node
      --  itself may have been rewritten.

      if Analyzed (N) then
         return;
      end if;

      if Nkind_In (N_Kind, N_Op_Add, N_Op_Expon, N_Op_Subtract)
        or else N_Kind in N_Multiplying_Operator
        or else N_Kind in N_Op_Compare
      then
         declare
            L                : constant Node_Id        := Left_Opnd (N);
            Dims_Of_L        : constant Dimension_Type :=
                                 Dimensions_Of_Operand (L);
            L_Has_Dimensions : constant Boolean        := Exists (Dims_Of_L);
            R                : constant Node_Id        := Right_Opnd (N);
            Dims_Of_R        : constant Dimension_Type :=
                                 Dimensions_Of_Operand (R);
            R_Has_Dimensions : constant Boolean        := Exists (Dims_Of_R);
            Dims_Of_N        : Dimension_Type          := Null_Dimension;

         begin
            --  N_Op_Add, N_Op_Mod, N_Op_Rem or N_Op_Subtract case

            if Nkind_In (N, N_Op_Add, N_Op_Mod, N_Op_Rem, N_Op_Subtract) then

               --  Check both operands have same dimension

               if Dims_Of_L /= Dims_Of_R then
                  Error_Dim_Msg_For_Binary_Op (N, L, R);
               else
                  --  Check both operands are not dimensionless

                  if Exists (Dims_Of_L) then
                     Set_Dimensions (N, Dims_Of_L);
                  end if;
               end if;

            --  N_Op_Multiply or N_Op_Divide case

            elsif Nkind_In (N_Kind, N_Op_Multiply, N_Op_Divide) then

               --  Check at least one operand is not dimensionless

               if L_Has_Dimensions or R_Has_Dimensions then

                  --  Multiplication case

                  --  Get both operands dimensions and add them

                  if N_Kind = N_Op_Multiply then
                     for Position in Dimension_Type'Range loop
                        Dims_Of_N (Position) :=
                          Dims_Of_L (Position) + Dims_Of_R (Position);
                     end loop;

                  --  Division case

                  --  Get both operands dimensions and subtract them

                  else
                     for Position in Dimension_Type'Range loop
                        Dims_Of_N (Position) :=
                          Dims_Of_L (Position) - Dims_Of_R (Position);
                     end loop;
                  end if;

                  if Exists (Dims_Of_N) then
                     Set_Dimensions (N, Dims_Of_N);
                  end if;
               end if;

            --  Exponentiation case

            --  Note: a rational exponent is allowed for dimensioned operand

            elsif N_Kind = N_Op_Expon then

               --  Check the left operand is not dimensionless. Note that the
               --  value of the exponent must be known compile time. Otherwise,
               --  the exponentiation evaluation will return an error message.

               if L_Has_Dimensions then
                  if not Compile_Time_Known_Value (R) then
                     Error_Msg_N
                       ("exponent of dimensioned operand must be "
                        & "known at compile time", N);
                  end if;

                  declare
                     Exponent_Value : Rational := Zero;

                  begin
                     --  Real operand case

                     if Is_Real_Type (Etype (L)) then

                        --  Define the exponent as a Rational number

                        Exponent_Value := Create_Rational_From (R, False);

                        --  Verify that the exponent cannot be interpreted
                        --  as a rational, otherwise interpret the exponent
                        --  as an integer.

                        if Exponent_Value = No_Rational then
                           Exponent_Value :=
                             +Whole (UI_To_Int (Expr_Value (R)));
                        end if;

                     --  Integer operand case.

                     --  For integer operand, the exponent cannot be
                     --  interpreted as a rational.

                     else
                        Exponent_Value := +Whole (UI_To_Int (Expr_Value (R)));
                     end if;

                     for Position in Dimension_Type'Range loop
                        Dims_Of_N (Position) :=
                          Dims_Of_L (Position) * Exponent_Value;
                     end loop;

                     if Exists (Dims_Of_N) then
                        Set_Dimensions (N, Dims_Of_N);
                     end if;
                  end;
               end if;

            --  Comparison cases

            --  For relational operations, only dimension checking is
            --  performed (no propagation). If one operand is the result
            --  of constant folding the dimensions may have been lost
            --  in a tree copy, so assume that pre-analysis has verified
            --  that dimensions are correct.

            elsif N_Kind in N_Op_Compare then
               if (L_Has_Dimensions or R_Has_Dimensions)
                 and then Dims_Of_L /= Dims_Of_R
               then
                  if Nkind (L) = N_Real_Literal
                    and then not (Comes_From_Source (L))
                    and then Expander_Active
                  then
                     null;

                  elsif Nkind (R) = N_Real_Literal
                    and then not (Comes_From_Source (R))
                    and then Expander_Active
                  then
                     null;

                  else
                     Error_Dim_Msg_For_Binary_Op (N, L, R);
                  end if;
               end if;
            end if;

            --  If expander is active, remove dimension information from each
            --  operand, as only dimensions of result are relevant.

            if Expander_Active then
               Remove_Dimensions (L);
               Remove_Dimensions (R);
            end if;
         end;
      end if;
   end Analyze_Dimension_Binary_Op;

   ----------------------------
   -- Analyze_Dimension_Call --
   ----------------------------

   procedure Analyze_Dimension_Call (N : Node_Id; Nam : Entity_Id) is
      Actuals        : constant List_Id := Parameter_Associations (N);
      Actual         : Node_Id;
      Dims_Of_Formal : Dimension_Type;
      Formal         : Node_Id;
      Formal_Typ     : Entity_Id;

      Error_Detected : Boolean := False;
      --  This flag is used in order to indicate if an error has been detected
      --  so far by the compiler in this routine.

   begin
      --  Aspect is an Ada 2012 feature. Note that there is no need to check
      --  dimensions for calls that don't come from source, or those that may
      --  have semantic errors.

      if Ada_Version < Ada_2012
        or else not Comes_From_Source (N)
        or else Error_Posted (N)
      then
         return;
      end if;

      --  Check the dimensions of the actuals, if any

      if not Is_Empty_List (Actuals) then

         --  Special processing for elementary functions

         --  For Sqrt call, the resulting dimensions equal to half the
         --  dimensions of the actual. For all other elementary calls, this
         --  routine check that every actual is dimensionless.

         if Nkind (N) = N_Function_Call then
            Elementary_Function_Calls : declare
               Dims_Of_Call : Dimension_Type;
               Ent          : Entity_Id := Nam;

               function Is_Elementary_Function_Entity
                 (Sub_Id : Entity_Id) return Boolean;
               --  Given Sub_Id, the original subprogram entity, return True
               --  if call is to an elementary function (see Ada.Numerics.
               --  Generic_Elementary_Functions).

               -----------------------------------
               -- Is_Elementary_Function_Entity --
               -----------------------------------

               function Is_Elementary_Function_Entity
                 (Sub_Id : Entity_Id) return Boolean
               is
                  Loc : constant Source_Ptr := Sloc (Sub_Id);

               begin
                  --  Is entity in Ada.Numerics.Generic_Elementary_Functions?

                  return
                    Loc > No_Location
                      and then
                        Is_RTU
                          (Cunit_Entity (Get_Source_Unit (Loc)),
                            Ada_Numerics_Generic_Elementary_Functions);
               end Is_Elementary_Function_Entity;

            --  Start of processing for Elementary_Function_Calls

            begin
               --  Get original subprogram entity following the renaming chain

               if Present (Alias (Ent)) then
                  Ent := Alias (Ent);
               end if;

               --  Check the call is an Elementary function call

               if Is_Elementary_Function_Entity (Ent) then

                  --  Sqrt function call case

                  if Chars (Ent) = Name_Sqrt then
                     Dims_Of_Call := Dimensions_Of (First_Actual (N));

                     --  Evaluates the resulting dimensions (i.e. half the
                     --  dimensions of the actual).

                     if Exists (Dims_Of_Call) then
                        for Position in Dims_Of_Call'Range loop
                           Dims_Of_Call (Position) :=
                             Dims_Of_Call (Position) *
                               Rational'(Numerator => 1, Denominator => 2);
                        end loop;

                        Set_Dimensions (N, Dims_Of_Call);
                     end if;

                  --  All other elementary functions case. Note that every
                  --  actual here should be dimensionless.

                  else
                     Actual := First_Actual (N);
                     while Present (Actual) loop
                        if Exists (Dimensions_Of (Actual)) then

                           --  Check if error has already been encountered

                           if not Error_Detected then
                              Error_Msg_NE
                                ("dimensions mismatch in call of&",
                                 N, Name (N));
                              Error_Detected := True;
                           end if;

                           Error_Msg_N
                             ("\expected dimension '['], found "
                              & Dimensions_Msg_Of (Actual), Actual);
                        end if;

                        Next_Actual (Actual);
                     end loop;
                  end if;

                  --  Nothing more to do for elementary functions

                  return;
               end if;
            end Elementary_Function_Calls;
         end if;

         --  General case. Check, for each parameter, the dimensions of the
         --  actual and its corresponding formal match. Otherwise, complain.

         Actual := First_Actual (N);
         Formal := First_Formal (Nam);
         while Present (Formal) loop

            --  A missing corresponding actual indicates that the analysis of
            --  the call was aborted due to a previous error.

            if No (Actual) then
               Check_Error_Detected;
               return;
            end if;

            Formal_Typ     := Etype (Formal);
            Dims_Of_Formal := Dimensions_Of (Formal_Typ);

            --  If the formal is not dimensionless, check dimensions of formal
            --  and actual match. Otherwise, complain.

            if Exists (Dims_Of_Formal)
              and then Dimensions_Of (Actual) /= Dims_Of_Formal
            then
               --  Check if an error has already been encountered so far

               if not Error_Detected then
                  Error_Msg_NE ("dimensions mismatch in& call", N, Name (N));
                  Error_Detected := True;
               end if;

               Error_Msg_N
                 ("\expected dimension " & Dimensions_Msg_Of (Formal_Typ)
                  & ", found " & Dimensions_Msg_Of (Actual), Actual);
            end if;

            Next_Actual (Actual);
            Next_Formal (Formal);
         end loop;
      end if;

      --  For function calls, propagate the dimensions from the returned type

      if Nkind (N) = N_Function_Call then
         Analyze_Dimension_Has_Etype (N);
      end if;
   end Analyze_Dimension_Call;

   ---------------------------------------------
   -- Analyze_Dimension_Component_Declaration --
   ---------------------------------------------

   procedure Analyze_Dimension_Component_Declaration (N : Node_Id) is
      Expr         : constant Node_Id        := Expression (N);
      Id           : constant Entity_Id      := Defining_Identifier (N);
      Etyp         : constant Entity_Id      := Etype (Id);
      Dims_Of_Etyp : constant Dimension_Type := Dimensions_Of (Etyp);
      Dims_Of_Expr : Dimension_Type;

      procedure Error_Dim_Msg_For_Component_Declaration
        (N    : Node_Id;
         Etyp : Entity_Id;
         Expr : Node_Id);
      --  Error using Error_Msg_N at node N. Output the dimensions of the
      --  type Etyp and the expression Expr of N.

      ---------------------------------------------
      -- Error_Dim_Msg_For_Component_Declaration --
      ---------------------------------------------

      procedure Error_Dim_Msg_For_Component_Declaration
        (N    : Node_Id;
         Etyp : Entity_Id;
         Expr : Node_Id) is
      begin
         Error_Msg_N ("dimensions mismatch in component declaration", N);
         Error_Msg_N
           ("\expected dimension " & Dimensions_Msg_Of (Etyp) & ", found "
            & Dimensions_Msg_Of (Expr), Expr);
      end Error_Dim_Msg_For_Component_Declaration;

   --  Start of processing for Analyze_Dimension_Component_Declaration

   begin
      --  Expression is present

      if Present (Expr) then
         Dims_Of_Expr := Dimensions_Of (Expr);

         --  Check dimensions match

         if Dims_Of_Etyp /= Dims_Of_Expr then

            --  Numeric literal case. Issue a warning if the object type is not
            --  dimensionless to indicate the literal is treated as if its
            --  dimension matches the type dimension.

            if Nkind_In (Original_Node (Expr), N_Real_Literal,
                                               N_Integer_Literal)
            then
               Dim_Warning_For_Numeric_Literal (Expr, Etyp);

            --  Issue a dimension mismatch error for all other cases

            else
               Error_Dim_Msg_For_Component_Declaration (N, Etyp, Expr);
            end if;
         end if;
      end if;
   end Analyze_Dimension_Component_Declaration;

   -------------------------------------------------
   -- Analyze_Dimension_Extended_Return_Statement --
   -------------------------------------------------

   procedure Analyze_Dimension_Extended_Return_Statement (N : Node_Id) is
      Return_Ent       : constant Entity_Id := Return_Statement_Entity (N);
      Return_Etyp      : constant Entity_Id :=
                           Etype (Return_Applies_To (Return_Ent));
      Return_Obj_Decls : constant List_Id := Return_Object_Declarations (N);
      Return_Obj_Decl  : Node_Id;
      Return_Obj_Id    : Entity_Id;
      Return_Obj_Typ   : Entity_Id;

      procedure Error_Dim_Msg_For_Extended_Return_Statement
        (N              : Node_Id;
         Return_Etyp    : Entity_Id;
         Return_Obj_Typ : Entity_Id);
      --  Error using Error_Msg_N at node N. Output dimensions of the returned
      --  type Return_Etyp and the returned object type Return_Obj_Typ of N.

      -------------------------------------------------
      -- Error_Dim_Msg_For_Extended_Return_Statement --
      -------------------------------------------------

      procedure Error_Dim_Msg_For_Extended_Return_Statement
        (N              : Node_Id;
         Return_Etyp    : Entity_Id;
         Return_Obj_Typ : Entity_Id)
      is
      begin
         Error_Msg_N ("dimensions mismatch in extended return statement", N);
         Error_Msg_N
           ("\expected dimension " & Dimensions_Msg_Of (Return_Etyp)
            & ", found " & Dimensions_Msg_Of (Return_Obj_Typ), N);
      end Error_Dim_Msg_For_Extended_Return_Statement;

   --  Start of processing for Analyze_Dimension_Extended_Return_Statement

   begin
      if Present (Return_Obj_Decls) then
         Return_Obj_Decl := First (Return_Obj_Decls);
         while Present (Return_Obj_Decl) loop
            if Nkind (Return_Obj_Decl) = N_Object_Declaration then
               Return_Obj_Id := Defining_Identifier (Return_Obj_Decl);

               if Is_Return_Object (Return_Obj_Id) then
                  Return_Obj_Typ := Etype (Return_Obj_Id);

                  --  Issue an error message if dimensions mismatch

                  if Dimensions_Of (Return_Etyp) /=
                       Dimensions_Of (Return_Obj_Typ)
                  then
                     Error_Dim_Msg_For_Extended_Return_Statement
                       (N, Return_Etyp, Return_Obj_Typ);
                     return;
                  end if;
               end if;
            end if;

            Next (Return_Obj_Decl);
         end loop;
      end if;
   end Analyze_Dimension_Extended_Return_Statement;

   -----------------------------------------------------
   -- Analyze_Dimension_Extension_Or_Record_Aggregate --
   -----------------------------------------------------

   procedure Analyze_Dimension_Extension_Or_Record_Aggregate (N : Node_Id) is
      Comp     : Node_Id;
      Comp_Id  : Entity_Id;
      Comp_Typ : Entity_Id;
      Expr     : Node_Id;

      Error_Detected : Boolean := False;
      --  This flag is used in order to indicate if an error has been detected
      --  so far by the compiler in this routine.

   begin
      --  Aspect is an Ada 2012 feature. Note that there is no need to check
      --  dimensions for aggregates that don't come from source, or if we are
      --  within an initialization procedure, whose expressions have been
      --  checked at the point of record declaration.

      if Ada_Version < Ada_2012
        or else not Comes_From_Source (N)
        or else Inside_Init_Proc
      then
         return;
      end if;

      Comp := First (Component_Associations (N));
      while Present (Comp) loop
         Comp_Id  := Entity (First (Choices (Comp)));
         Comp_Typ := Etype (Comp_Id);

         --  Check the component type is either a dimensioned type or a
         --  dimensioned subtype.

         if Has_Dimension_System (Base_Type (Comp_Typ)) then
            Expr := Expression (Comp);

            --  A box-initialized component needs no checking.

            if No (Expr) and then Box_Present (Comp) then
               null;

            --  Issue an error if the dimensions of the component type and the
            --  dimensions of the component mismatch.

            elsif Dimensions_Of (Expr) /= Dimensions_Of (Comp_Typ) then

               --  Check if an error has already been encountered so far

               if not Error_Detected then

                  --  Extension aggregate case

                  if Nkind (N) = N_Extension_Aggregate then
                     Error_Msg_N
                       ("dimensions mismatch in extension aggregate", N);

                  --  Record aggregate case

                  else
                     Error_Msg_N
                       ("dimensions mismatch in record aggregate", N);
                  end if;

                  Error_Detected := True;
               end if;

               Error_Msg_N
                 ("\expected dimension " & Dimensions_Msg_Of (Comp_Typ)
                  & ", found " & Dimensions_Msg_Of (Expr), Comp);
            end if;
         end if;

         Next (Comp);
      end loop;
   end Analyze_Dimension_Extension_Or_Record_Aggregate;

   -------------------------------
   -- Analyze_Dimension_Formals --
   -------------------------------

   procedure Analyze_Dimension_Formals (N : Node_Id; Formals : List_Id) is
      Dims_Of_Typ : Dimension_Type;
      Formal      : Node_Id;
      Typ         : Entity_Id;

   begin
      --  Aspect is an Ada 2012 feature. Note that there is no need to check
      --  dimensions for sub specs that don't come from source.

      if Ada_Version < Ada_2012 or else not Comes_From_Source (N) then
         return;
      end if;

      Formal := First (Formals);
      while Present (Formal) loop
         Typ         := Parameter_Type (Formal);
         Dims_Of_Typ := Dimensions_Of  (Typ);

         if Exists (Dims_Of_Typ) then
            declare
               Expr : constant Node_Id := Expression (Formal);

            begin
               --  Issue a warning if Expr is a numeric literal and if its
               --  dimensions differ with the dimensions of the formal type.

               if Present (Expr)
                 and then Dims_Of_Typ /= Dimensions_Of (Expr)
                 and then Nkind_In (Original_Node (Expr), N_Real_Literal,
                                                          N_Integer_Literal)
               then
                  Dim_Warning_For_Numeric_Literal (Expr, Etype (Typ));
               end if;
            end;
         end if;

         Next (Formal);
      end loop;
   end Analyze_Dimension_Formals;

   ---------------------------------
   -- Analyze_Dimension_Has_Etype --
   ---------------------------------

   procedure Analyze_Dimension_Has_Etype (N : Node_Id) is
      Etyp         : constant Entity_Id := Etype (N);
      Dims_Of_Etyp : Dimension_Type     := Dimensions_Of (Etyp);

   begin
      --  General case. Propagation of the dimensions from the type

      if Exists (Dims_Of_Etyp) then
         Set_Dimensions (N, Dims_Of_Etyp);

      --  Identifier case. Propagate the dimensions from the entity for
      --  identifier whose entity is a non-dimensionless constant.

      elsif Nkind (N) = N_Identifier then
         Analyze_Dimension_Identifier : declare
            Id : constant Entity_Id := Entity (N);

         begin
            --  If Id is missing, abnormal tree, assume previous error

            if No (Id) then
               Check_Error_Detected;
               return;

            elsif Ekind_In (Id,  E_Constant, E_Named_Real)
              and then Exists (Dimensions_Of (Id))
            then
               Set_Dimensions (N, Dimensions_Of (Id));
            end if;
         end Analyze_Dimension_Identifier;

      --  Attribute reference case. Propagate the dimensions from the prefix.

      elsif Nkind (N) = N_Attribute_Reference
        and then Has_Dimension_System (Base_Type (Etyp))
      then
         Dims_Of_Etyp := Dimensions_Of (Prefix (N));

         --  Check the prefix is not dimensionless

         if Exists (Dims_Of_Etyp) then
            Set_Dimensions (N, Dims_Of_Etyp);
         end if;
      end if;

      --  Remove dimensions from inner expressions, to prevent dimensions
      --  table from growing uselessly.

      case Nkind (N) is
         when N_Attribute_Reference
            | N_Indexed_Component
         =>
            declare
               Exprs : constant List_Id := Expressions (N);
               Expr  : Node_Id;

            begin
               if Present (Exprs) then
                  Expr := First (Exprs);
                  while Present (Expr) loop
                     Remove_Dimensions (Expr);
                     Next (Expr);
                  end loop;
               end if;
            end;

         when N_Qualified_Expression
            | N_Type_Conversion
            | N_Unchecked_Type_Conversion
         =>
            Remove_Dimensions (Expression (N));

         when N_Selected_Component =>
            Remove_Dimensions (Selector_Name (N));

         when others =>
            null;
      end case;
   end Analyze_Dimension_Has_Etype;

   ------------------------------------------
   -- Analyze_Dimension_Number_Declaration --
   ------------------------------------------

   procedure Analyze_Dimension_Number_Declaration (N : Node_Id) is
      Expr        : constant Node_Id        := Expression (N);
      Id          : constant Entity_Id      := Defining_Identifier (N);
      Dim_Of_Expr : constant Dimension_Type := Dimensions_Of (Expr);

   begin
      if Exists (Dim_Of_Expr) then
         Set_Dimensions (Id, Dim_Of_Expr);
         Set_Etype (Id, Etype (Expr));
      end if;
   end Analyze_Dimension_Number_Declaration;

   ------------------------------------------
   -- Analyze_Dimension_Object_Declaration --
   ------------------------------------------

   procedure Analyze_Dimension_Object_Declaration (N : Node_Id) is
      Expr        : constant Node_Id   := Expression (N);
      Id          : constant Entity_Id := Defining_Identifier (N);
      Etyp        : constant Entity_Id := Etype (Id);
      Dim_Of_Etyp : constant Dimension_Type := Dimensions_Of (Etyp);
      Dim_Of_Expr : Dimension_Type;

      procedure Error_Dim_Msg_For_Object_Declaration
        (N    : Node_Id;
         Etyp : Entity_Id;
         Expr : Node_Id);
      --  Error using Error_Msg_N at node N. Output the dimensions of the
      --  type Etyp and of the expression Expr.

      ------------------------------------------
      -- Error_Dim_Msg_For_Object_Declaration --
      ------------------------------------------

      procedure Error_Dim_Msg_For_Object_Declaration
        (N    : Node_Id;
         Etyp : Entity_Id;
         Expr : Node_Id) is
      begin
         Error_Msg_N ("dimensions mismatch in object declaration", N);
         Error_Msg_N
           ("\expected dimension " & Dimensions_Msg_Of (Etyp) & ", found "
            & Dimensions_Msg_Of (Expr), Expr);
      end Error_Dim_Msg_For_Object_Declaration;

   --  Start of processing for Analyze_Dimension_Object_Declaration

   begin
      --  Expression is present

      if Present (Expr) then
         Dim_Of_Expr := Dimensions_Of (Expr);

         --  Check dimensions match

         if Dim_Of_Expr /= Dim_Of_Etyp then

            --  Numeric literal case. Issue a warning if the object type is
            --  not dimensionless to indicate the literal is treated as if
            --  its dimension matches the type dimension.

            if Nkind_In (Original_Node (Expr), N_Real_Literal,
                                               N_Integer_Literal)
            then
               Dim_Warning_For_Numeric_Literal (Expr, Etyp);

            --  Case of object is a constant whose type is a dimensioned type

            elsif Constant_Present (N) and then not Exists (Dim_Of_Etyp) then

               --  Propagate dimension from expression to object entity

               Set_Dimensions (Id, Dim_Of_Expr);

            --  Expression may have been constant-folded. If nominal type has
            --  dimensions, verify that expression has same type.

            elsif Exists (Dim_Of_Etyp) and then Etype (Expr) = Etyp then
               null;

            --  For all other cases, issue an error message

            else
               Error_Dim_Msg_For_Object_Declaration (N, Etyp, Expr);
            end if;
         end if;

         --  Remove dimensions in expression after checking consistency with
         --  given type.

         Remove_Dimensions (Expr);
      end if;
   end Analyze_Dimension_Object_Declaration;

   ---------------------------------------------------
   -- Analyze_Dimension_Object_Renaming_Declaration --
   ---------------------------------------------------

   procedure Analyze_Dimension_Object_Renaming_Declaration (N : Node_Id) is
      Renamed_Name : constant Node_Id := Name (N);
      Sub_Mark     : constant Node_Id := Subtype_Mark (N);

      procedure Error_Dim_Msg_For_Object_Renaming_Declaration
        (N            : Node_Id;
         Sub_Mark     : Node_Id;
         Renamed_Name : Node_Id);
      --  Error using Error_Msg_N at node N. Output the dimensions of
      --  Sub_Mark and of Renamed_Name.

      ---------------------------------------------------
      -- Error_Dim_Msg_For_Object_Renaming_Declaration --
      ---------------------------------------------------

      procedure Error_Dim_Msg_For_Object_Renaming_Declaration
        (N            : Node_Id;
         Sub_Mark     : Node_Id;
         Renamed_Name : Node_Id) is
      begin
         Error_Msg_N ("dimensions mismatch in object renaming declaration", N);
         Error_Msg_N
           ("\expected dimension " & Dimensions_Msg_Of (Sub_Mark) & ", found "
            & Dimensions_Msg_Of (Renamed_Name), Renamed_Name);
      end Error_Dim_Msg_For_Object_Renaming_Declaration;

   --  Start of processing for Analyze_Dimension_Object_Renaming_Declaration

   begin
      if Dimensions_Of (Renamed_Name) /= Dimensions_Of (Sub_Mark) then
         Error_Dim_Msg_For_Object_Renaming_Declaration
           (N, Sub_Mark, Renamed_Name);
      end if;
   end Analyze_Dimension_Object_Renaming_Declaration;

   -----------------------------------------------
   -- Analyze_Dimension_Simple_Return_Statement --
   -----------------------------------------------

   procedure Analyze_Dimension_Simple_Return_Statement (N : Node_Id) is
      Expr                : constant Node_Id := Expression (N);
      Return_Ent          : constant Entity_Id := Return_Statement_Entity (N);
      Return_Etyp         : constant Entity_Id :=
                              Etype (Return_Applies_To (Return_Ent));
      Dims_Of_Return_Etyp : constant Dimension_Type :=
                              Dimensions_Of (Return_Etyp);

      procedure Error_Dim_Msg_For_Simple_Return_Statement
        (N           : Node_Id;
         Return_Etyp : Entity_Id;
         Expr        : Node_Id);
      --  Error using Error_Msg_N at node N. Output the dimensions of the
      --  returned type Return_Etyp and the returned expression Expr of N.

      -----------------------------------------------
      -- Error_Dim_Msg_For_Simple_Return_Statement --
      -----------------------------------------------

      procedure Error_Dim_Msg_For_Simple_Return_Statement
        (N           : Node_Id;
         Return_Etyp : Entity_Id;
         Expr        : Node_Id)
      is
      begin
         Error_Msg_N ("dimensions mismatch in return statement", N);
         Error_Msg_N
           ("\expected dimension " & Dimensions_Msg_Of (Return_Etyp)
            & ", found " & Dimensions_Msg_Of (Expr), Expr);
      end Error_Dim_Msg_For_Simple_Return_Statement;

   --  Start of processing for Analyze_Dimension_Simple_Return_Statement

   begin
      if Dims_Of_Return_Etyp /= Dimensions_Of (Expr) then
         Error_Dim_Msg_For_Simple_Return_Statement (N, Return_Etyp, Expr);
         Remove_Dimensions (Expr);
      end if;
   end Analyze_Dimension_Simple_Return_Statement;

   -------------------------------------------
   -- Analyze_Dimension_Subtype_Declaration --
   -------------------------------------------

   procedure Analyze_Dimension_Subtype_Declaration (N : Node_Id) is
      Id           : constant Entity_Id := Defining_Identifier (N);
      Dims_Of_Id   : constant Dimension_Type := Dimensions_Of (Id);
      Dims_Of_Etyp : Dimension_Type;
      Etyp         : Node_Id;

   begin
      --  No constraint case in subtype declaration

      if Nkind (Subtype_Indication (N)) /= N_Subtype_Indication then
         Etyp := Etype (Subtype_Indication (N));
         Dims_Of_Etyp := Dimensions_Of (Etyp);

         if Exists (Dims_Of_Etyp) then

            --  If subtype already has a dimension (from Aspect_Dimension), it
            --  cannot inherit different dimensions from its subtype.

            if Exists (Dims_Of_Id) and then Dims_Of_Etyp /= Dims_Of_Id then
               Error_Msg_NE
                 ("subtype& already " & Dimensions_Msg_Of (Id, True), N, Id);
            else
               Set_Dimensions (Id, Dims_Of_Etyp);
               Set_Symbol (Id, Symbol_Of (Etyp));
            end if;
         end if;

      --  Constraint present in subtype declaration

      else
         Etyp := Etype (Subtype_Mark (Subtype_Indication (N)));
         Dims_Of_Etyp := Dimensions_Of (Etyp);

         if Exists (Dims_Of_Etyp) then
            Set_Dimensions (Id, Dims_Of_Etyp);
            Set_Symbol (Id, Symbol_Of (Etyp));
         end if;
      end if;
   end Analyze_Dimension_Subtype_Declaration;

   --------------------------------
   -- Analyze_Dimension_Unary_Op --
   --------------------------------

   procedure Analyze_Dimension_Unary_Op (N : Node_Id) is
   begin
      case Nkind (N) is

         --  Propagate the dimension if the operand is not dimensionless

         when N_Op_Abs
            | N_Op_Minus
            | N_Op_Plus
         =>
            declare
               R : constant Node_Id := Right_Opnd (N);
            begin
               Move_Dimensions (R, N);
            end;

         when others =>
            null;
      end case;
   end Analyze_Dimension_Unary_Op;

   ---------------------------------
   -- Check_Expression_Dimensions --
   ---------------------------------

   procedure Check_Expression_Dimensions
     (Expr : Node_Id;
      Typ  : Entity_Id)
   is
   begin
      if Is_Floating_Point_Type (Etype (Expr)) then
         Analyze_Dimension (Expr);

         if Dimensions_Of (Expr) /= Dimensions_Of (Typ) then
            Error_Msg_N ("dimensions mismatch in array aggregate", Expr);
            Error_Msg_N
              ("\expected dimension " & Dimensions_Msg_Of (Typ)
               & ", found " & Dimensions_Msg_Of (Expr), Expr);
         end if;
      end if;
   end Check_Expression_Dimensions;

   ---------------------
   -- Copy_Dimensions --
   ---------------------

   procedure Copy_Dimensions (From : Node_Id; To : Node_Id) is
      Dims_Of_From : constant Dimension_Type := Dimensions_Of (From);

   begin
      --  Ignore if not Ada 2012 or beyond

      if Ada_Version < Ada_2012 then
         return;

      --  For Ada 2012, Copy the dimension of 'From to 'To'

      elsif Exists (Dims_Of_From) then
         Set_Dimensions (To, Dims_Of_From);
      end if;
   end Copy_Dimensions;

   --------------------------
   -- Create_Rational_From --
   --------------------------

   --  RATIONAL ::= [-] NUMERAL [/ NUMERAL]

   --  A rational number is a number that can be expressed as the quotient or
   --  fraction a/b of two integers, where b is non-zero positive.

   function Create_Rational_From
     (Expr     : Node_Id;
      Complain : Boolean) return Rational
   is
      Or_Node_Of_Expr : constant Node_Id := Original_Node (Expr);
      Result          : Rational := No_Rational;

      function Process_Minus (N : Node_Id) return Rational;
      --  Create a rational from a N_Op_Minus node

      function Process_Divide (N : Node_Id) return Rational;
      --  Create a rational from a N_Op_Divide node

      function Process_Literal (N : Node_Id) return Rational;
      --  Create a rational from a N_Integer_Literal node

      -------------------
      -- Process_Minus --
      -------------------

      function Process_Minus (N : Node_Id) return Rational is
         Right  : constant Node_Id := Original_Node (Right_Opnd (N));
         Result : Rational;

      begin
         --  Operand is an integer literal

         if Nkind (Right) = N_Integer_Literal then
            Result := -Process_Literal (Right);

         --  Operand is a divide operator

         elsif Nkind (Right) = N_Op_Divide then
            Result := -Process_Divide (Right);

         else
            Result := No_Rational;
         end if;

         --  Provide minimal semantic information on dimension expressions,
         --  even though they have no run-time existence. This is for use by
         --  ASIS tools, in particular pretty-printing. If generating code
         --  standard operator resolution will take place.

         if ASIS_Mode then
            Set_Entity (N, Standard_Op_Minus);
            Set_Etype  (N, Standard_Integer);
         end if;

         return Result;
      end Process_Minus;

      --------------------
      -- Process_Divide --
      --------------------

      function Process_Divide (N : Node_Id) return Rational is
         Left      : constant Node_Id := Original_Node (Left_Opnd (N));
         Right     : constant Node_Id := Original_Node (Right_Opnd (N));
         Left_Rat  : Rational;
         Result    : Rational := No_Rational;
         Right_Rat : Rational;

      begin
         --  Both left and right operands are integer literals

         if Nkind (Left) = N_Integer_Literal
              and then
            Nkind (Right) = N_Integer_Literal
         then
            Left_Rat := Process_Literal (Left);
            Right_Rat := Process_Literal (Right);
            Result := Left_Rat / Right_Rat;
         end if;

         --  Provide minimal semantic information on dimension expressions,
         --  even though they have no run-time existence. This is for use by
         --  ASIS tools, in particular pretty-printing. If generating code
         --  standard operator resolution will take place.

         if ASIS_Mode then
            Set_Entity (N, Standard_Op_Divide);
            Set_Etype  (N, Standard_Integer);
         end if;

         return Result;
      end Process_Divide;

      ---------------------
      -- Process_Literal --
      ---------------------

      function Process_Literal (N : Node_Id) return Rational is
      begin
         return +Whole (UI_To_Int (Intval (N)));
      end Process_Literal;

   --  Start of processing for Create_Rational_From

   begin
      --  Check the expression is either a division of two integers or an
      --  integer itself. Note that the check applies to the original node
      --  since the node could have already been rewritten.

      --  Integer literal case

      if Nkind (Or_Node_Of_Expr) = N_Integer_Literal then
         Result := Process_Literal (Or_Node_Of_Expr);

      --  Divide operator case

      elsif Nkind (Or_Node_Of_Expr) = N_Op_Divide then
         Result := Process_Divide (Or_Node_Of_Expr);

      --  Minus operator case

      elsif Nkind (Or_Node_Of_Expr) = N_Op_Minus then
         Result := Process_Minus (Or_Node_Of_Expr);
      end if;

      --  When Expr cannot be interpreted as a rational and Complain is true,
      --  generate an error message.

      if Complain and then Result = No_Rational then
         Error_Msg_N ("rational expected", Expr);
      end if;

      return Result;
   end Create_Rational_From;

   -------------------
   -- Dimensions_Of --
   -------------------

   function Dimensions_Of (N : Node_Id) return Dimension_Type is
   begin
      return Dimension_Table.Get (N);
   end Dimensions_Of;

   -----------------------
   -- Dimensions_Msg_Of --
   -----------------------

   function Dimensions_Msg_Of
      (N                  : Node_Id;
       Description_Needed : Boolean := False) return String
   is
      Dims_Of_N      : constant Dimension_Type := Dimensions_Of (N);
      Dimensions_Msg : Name_Id;
      System         : System_Type;

   begin
      --  Initialization of Name_Buffer

      Name_Len := 0;

      --  N is not dimensionless

      if Exists (Dims_Of_N) then
         System := System_Of (Base_Type (Etype (N)));

         --  When Description_Needed, add to string "has dimension " before the
         --  actual dimension.

         if Description_Needed then
            Add_Str_To_Name_Buffer ("has dimension ");
         end if;

         Append
           (Global_Name_Buffer,
            From_Dim_To_Str_Of_Dim_Symbols (Dims_Of_N, System, True));

      --  N is dimensionless

      --  When Description_Needed, return "is dimensionless"

      elsif Description_Needed then
         Add_Str_To_Name_Buffer ("is dimensionless");

      --  Otherwise, return "'[']"

      else
         Add_Str_To_Name_Buffer ("'[']");
      end if;

      Dimensions_Msg := Name_Find;
      return Get_Name_String (Dimensions_Msg);
   end Dimensions_Msg_Of;

   --------------------------
   -- Dimension_Table_Hash --
   --------------------------

   function Dimension_Table_Hash
     (Key : Node_Id) return Dimension_Table_Range
   is
   begin
      return Dimension_Table_Range (Key mod 511);
   end Dimension_Table_Hash;

   -------------------------------------
   -- Dim_Warning_For_Numeric_Literal --
   -------------------------------------

   procedure Dim_Warning_For_Numeric_Literal (N : Node_Id; Typ : Entity_Id) is
   begin
      --  Initialize name buffer

      Name_Len := 0;

      Append (Global_Name_Buffer, String_From_Numeric_Literal (N));

      --  Insert a blank between the literal and the symbol

      Add_Str_To_Name_Buffer (" ");
      Append (Global_Name_Buffer, Symbol_Of (Typ));

      Error_Msg_Name_1 := Name_Find;
      Error_Msg_N ("assumed to be%%??", N);
   end Dim_Warning_For_Numeric_Literal;

   ----------------------
   -- Dimensions_Match --
   ----------------------

   function Dimensions_Match (T1 : Entity_Id; T2 : Entity_Id) return Boolean is
   begin
      return
        not Has_Dimension_System (Base_Type (T1))
          or else Dimensions_Of (T1) = Dimensions_Of (T2);
   end Dimensions_Match;

   ----------------------------------------
   -- Eval_Op_Expon_For_Dimensioned_Type --
   ----------------------------------------

   --  Evaluate the expon operator for real dimensioned type.

   --  Note that if the exponent is an integer (denominator = 1) the node is
   --  evaluated by the regular Eval_Op_Expon routine (see Sem_Eval).

   procedure Eval_Op_Expon_For_Dimensioned_Type
     (N    : Node_Id;
      Btyp : Entity_Id)
   is
      R       : constant Node_Id := Right_Opnd (N);
      R_Value : Rational := No_Rational;

   begin
      if Is_Real_Type (Btyp) then
         R_Value := Create_Rational_From (R, False);
      end if;

      --  Check that the exponent is not an integer

      if R_Value /= No_Rational and then R_Value.Denominator /= 1 then
         Eval_Op_Expon_With_Rational_Exponent (N, R_Value);
      else
         Eval_Op_Expon (N);
      end if;
   end Eval_Op_Expon_For_Dimensioned_Type;

   ------------------------------------------
   -- Eval_Op_Expon_With_Rational_Exponent --
   ------------------------------------------

   --  For dimensioned operand in exponentiation, exponent is allowed to be a
   --  Rational and not only an Integer like for dimensionless operands. For
   --  that particular case, the left operand is rewritten as a function call
   --  using the function Expon_LLF from s-llflex.ads.

   procedure Eval_Op_Expon_With_Rational_Exponent
     (N              : Node_Id;
      Exponent_Value : Rational)
   is
      Loc                   : constant Source_Ptr     := Sloc (N);
      Dims_Of_N             : constant Dimension_Type := Dimensions_Of (N);
      L                     : constant Node_Id        := Left_Opnd (N);
      Etyp_Of_L             : constant Entity_Id      := Etype (L);
      Btyp_Of_L             : constant Entity_Id      := Base_Type (Etyp_Of_L);
      Actual_1              : Node_Id;
      Actual_2              : Node_Id;
      Dim_Power             : Rational;
      List_Of_Dims          : List_Id;
      New_Aspect            : Node_Id;
      New_Aspects           : List_Id;
      New_Id                : Entity_Id;
      New_N                 : Node_Id;
      New_Subtyp_Decl_For_L : Node_Id;
      System                : System_Type;

   begin
      --  Case when the operand is not dimensionless

      if Exists (Dims_Of_N) then

         --  Get the corresponding System_Type to know the exact number of
         --  dimensions in the system.

         System := System_Of (Btyp_Of_L);

         --  Generation of a new subtype with the proper dimensions

         --  In order to rewrite the operator as a type conversion, a new
         --  dimensioned subtype with the resulting dimensions of the
         --  exponentiation must be created.

         --  Generate:

         --  Btyp_Of_L   : constant Entity_Id := Base_Type (Etyp_Of_L);
         --  System      : constant System_Id :=
         --                  Get_Dimension_System_Id (Btyp_Of_L);
         --  Num_Of_Dims : constant Number_Of_Dimensions :=
         --                  Dimension_Systems.Table (System).Dimension_Count;

         --  subtype T is Btyp_Of_L
         --    with
         --      Dimension => (
         --        Dims_Of_N (1).Numerator / Dims_Of_N (1).Denominator,
         --        Dims_Of_N (2).Numerator / Dims_Of_N (2).Denominator,
         --        ...
         --        Dims_Of_N (Num_Of_Dims).Numerator /
         --          Dims_Of_N (Num_Of_Dims).Denominator);

         --  Step 1: Generate the new aggregate for the aspect Dimension

         New_Aspects  := Empty_List;

         List_Of_Dims := New_List;
         for Position in Dims_Of_N'First ..  System.Count loop
            Dim_Power := Dims_Of_N (Position);
            Append_To (List_Of_Dims,
               Make_Op_Divide (Loc,
                 Left_Opnd  =>
                   Make_Integer_Literal (Loc, Int (Dim_Power.Numerator)),
                 Right_Opnd =>
                   Make_Integer_Literal (Loc, Int (Dim_Power.Denominator))));
         end loop;

         --  Step 2: Create the new Aspect Specification for Aspect Dimension

         New_Aspect :=
           Make_Aspect_Specification (Loc,
             Identifier => Make_Identifier (Loc, Name_Dimension),
             Expression => Make_Aggregate (Loc, Expressions => List_Of_Dims));

         --  Step 3: Make a temporary identifier for the new subtype

         New_Id := Make_Temporary (Loc, 'T');
         Set_Is_Internal (New_Id);

         --  Step 4: Declaration of the new subtype

         New_Subtyp_Decl_For_L :=
            Make_Subtype_Declaration (Loc,
               Defining_Identifier => New_Id,
               Subtype_Indication  => New_Occurrence_Of (Btyp_Of_L, Loc));

         Append (New_Aspect, New_Aspects);
         Set_Parent (New_Aspects, New_Subtyp_Decl_For_L);
         Set_Aspect_Specifications (New_Subtyp_Decl_For_L, New_Aspects);

         Analyze (New_Subtyp_Decl_For_L);

      --  Case where the operand is dimensionless

      else
         New_Id := Btyp_Of_L;
      end if;

      --  Replacement of N by New_N

      --  Generate:

      --  Actual_1 := Long_Long_Float (L),

      --  Actual_2 := Long_Long_Float (Exponent_Value.Numerator) /
      --                Long_Long_Float (Exponent_Value.Denominator);

      --  (T (Expon_LLF (Actual_1, Actual_2)));

      --  where T is the subtype declared in step 1

      --  The node is rewritten as a type conversion

      --  Step 1: Creation of the two parameters of Expon_LLF function call

      Actual_1 :=
        Make_Type_Conversion (Loc,
          Subtype_Mark => New_Occurrence_Of (Standard_Long_Long_Float, Loc),
          Expression   => Relocate_Node (L));

      Actual_2 :=
        Make_Op_Divide (Loc,
          Left_Opnd  =>
            Make_Real_Literal (Loc,
              UR_From_Uint (UI_From_Int (Int (Exponent_Value.Numerator)))),
          Right_Opnd =>
            Make_Real_Literal (Loc,
              UR_From_Uint (UI_From_Int (Int (Exponent_Value.Denominator)))));

      --  Step 2: Creation of New_N

      New_N :=
         Make_Type_Conversion (Loc,
           Subtype_Mark => New_Occurrence_Of (New_Id, Loc),
           Expression   =>
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (RE_Expon_LLF), Loc),
               Parameter_Associations => New_List (
                 Actual_1, Actual_2)));

      --  Step 3: Rewrite N with the result

      Rewrite (N, New_N);
      Set_Etype (N, New_Id);
      Analyze_And_Resolve (N, New_Id);
   end Eval_Op_Expon_With_Rational_Exponent;

   ------------
   -- Exists --
   ------------

   function Exists (Dim : Dimension_Type) return Boolean is
   begin
      return Dim /= Null_Dimension;
   end Exists;

   function Exists (Str : String_Id) return Boolean is
   begin
      return Str /= No_String;
   end Exists;

   function Exists (Sys : System_Type) return Boolean is
   begin
      return Sys /= Null_System;
   end Exists;

   ---------------------------------
   -- Expand_Put_Call_With_Symbol --
   ---------------------------------

   --  For procedure Put (resp. Put_Dim_Of) and function Image, defined in
   --  System.Dim.Float_IO or System.Dim.Integer_IO, the default string
   --  parameter is rewritten to include the unit symbol (or the dimension
   --  symbols if not a defined quantity) in the output of a dimensioned
   --  object.  If a value is already supplied by the user for the parameter
   --  Symbol, it is used as is.

   --  Case 1. Item is dimensionless

   --   * Put        : Item appears without a suffix

   --   * Put_Dim_Of : the output is []

   --      Obj : Mks_Type := 2.6;
   --      Put (Obj, 1, 1, 0);
   --      Put_Dim_Of (Obj);

   --      The corresponding outputs are:
   --      $2.6
   --      $[]

   --  Case 2. Item has a dimension

   --   * Put        : If the type of Item is a dimensioned subtype whose
   --                  symbol is not empty, then the symbol appears as a
   --                  suffix. Otherwise, a new string is created and appears
   --                  as a suffix of Item. This string results in the
   --                  successive concatanations between each unit symbol
   --                  raised by its corresponding dimension power from the
   --                  dimensions of Item.

   --   * Put_Dim_Of : The output is a new string resulting in the successive
   --                  concatanations between each dimension symbol raised by
   --                  its corresponding dimension power from the dimensions of
   --                  Item.

   --      subtype Random is Mks_Type
   --        with
   --         Dimension => (
   --           Meter =>   3,
   --           Candela => -1,
   --           others =>  0);

   --      Obj : Random := 5.0;
   --      Put (Obj);
   --      Put_Dim_Of (Obj);

   --      The corresponding outputs are:
   --      $5.0 m**3.cd**(-1)
   --      $[l**3.J**(-1)]

   --      The function Image returns the string identical to that produced by
   --      a call to Put whose first parameter is a string.

   procedure Expand_Put_Call_With_Symbol (N : Node_Id) is
      Actuals        : constant List_Id := Parameter_Associations (N);
      Loc            : constant Source_Ptr := Sloc (N);
      Name_Call      : constant Node_Id := Name (N);
      New_Actuals    : constant List_Id := New_List;
      Actual         : Node_Id;
      Dims_Of_Actual : Dimension_Type;
      Etyp           : Entity_Id;
      New_Str_Lit    : Node_Id := Empty;
      Symbols        : String_Id;

      Is_Put_Dim_Of : Boolean := False;
      --  This flag is used in order to differentiate routines Put and
      --  Put_Dim_Of. Set to True if the procedure is one of the Put_Dim_Of
      --  defined in System.Dim.Float_IO or System.Dim.Integer_IO.

      function Has_Symbols return Boolean;
      --  Return True if the current Put call already has a parameter
      --  association for parameter "Symbols" with the correct string of
      --  symbols.

      function Is_Procedure_Put_Call return Boolean;
      --  Return True if the current call is a call of an instantiation of a
      --  procedure Put defined in the package System.Dim.Float_IO and
      --  System.Dim.Integer_IO.

      function Item_Actual return Node_Id;
      --  Return the item actual parameter node in the output call

      -----------------
      -- Has_Symbols --
      -----------------

      function Has_Symbols return Boolean is
         Actual     : Node_Id;
         Actual_Str : Node_Id;

      begin
         --  Look for a symbols parameter association in the list of actuals

         Actual := First (Actuals);
         while Present (Actual) loop

            --  Positional parameter association case when the actual is a
            --  string literal.

            if Nkind (Actual) = N_String_Literal then
               Actual_Str := Actual;

            --  Named parameter association case when selector name is Symbol

            elsif Nkind (Actual) = N_Parameter_Association
              and then Chars (Selector_Name (Actual)) = Name_Symbol
            then
               Actual_Str := Explicit_Actual_Parameter (Actual);

            --  Ignore all other cases

            else
               Actual_Str := Empty;
            end if;

            if Present (Actual_Str) then

               --  Return True if the actual comes from source or if the string
               --  of symbols doesn't have the default value (i.e. it is ""),
               --  in which case it is used as suffix of the generated string.

               if Comes_From_Source (Actual)
                 or else String_Length (Strval (Actual_Str)) /= 0
               then
                  return True;

               else
                  return False;
               end if;
            end if;

            Next (Actual);
         end loop;

         --  At this point, the call has no parameter association. Look to the
         --  last actual since the symbols parameter is the last one.

         return Nkind (Last (Actuals)) = N_String_Literal;
      end Has_Symbols;

      ---------------------------
      -- Is_Procedure_Put_Call --
      ---------------------------

      function Is_Procedure_Put_Call return Boolean is
         Ent : Entity_Id;
         Loc : Source_Ptr;

      begin
         --  There are three different Put (resp. Put_Dim_Of) routines in each
         --  generic dim IO package. Verify the current procedure call is one
         --  of them.

         if Is_Entity_Name (Name_Call) then
            Ent := Entity (Name_Call);

            --  Get the original subprogram entity following the renaming chain

            if Present (Alias (Ent)) then
               Ent := Alias (Ent);
            end if;

            Loc := Sloc (Ent);

            --  Check the name of the entity subprogram is Put (resp.
            --  Put_Dim_Of) and verify this entity is located in either
            --  System.Dim.Float_IO or System.Dim.Integer_IO.

            if Loc > No_Location
              and then Is_Dim_IO_Package_Entity
                         (Cunit_Entity (Get_Source_Unit (Loc)))
            then
               if Chars (Ent) = Name_Put_Dim_Of then
                  Is_Put_Dim_Of := True;
                  return True;

               elsif Chars (Ent) = Name_Put
                 or else Chars (Ent) = Name_Image
               then
                  return True;
               end if;
            end if;
         end if;

         return False;
      end Is_Procedure_Put_Call;

      -----------------
      -- Item_Actual --
      -----------------

      function Item_Actual return Node_Id is
         Actual : Node_Id;

      begin
         --  Look for the item actual as a parameter association

         Actual := First (Actuals);
         while Present (Actual) loop
            if Nkind (Actual) = N_Parameter_Association
              and then Chars (Selector_Name (Actual)) = Name_Item
            then
               return Explicit_Actual_Parameter (Actual);
            end if;

            Next (Actual);
         end loop;

         --  Case where the item has been defined without an association

         Actual := First (Actuals);

         --  Depending on the procedure Put, Item actual could be first or
         --  second in the list of actuals.

         if Has_Dimension_System (Base_Type (Etype (Actual))) then
            return Actual;
         else
            return Next (Actual);
         end if;
      end Item_Actual;

   --  Start of processing for Expand_Put_Call_With_Symbol

   begin
      if Is_Procedure_Put_Call and then not Has_Symbols then
         Actual := Item_Actual;
         Dims_Of_Actual := Dimensions_Of (Actual);
         Etyp := Etype (Actual);

         --  Put_Dim_Of case

         if Is_Put_Dim_Of then

            --  Check that the item is not dimensionless

            --  Create the new String_Literal with the new String_Id generated
            --  by the routine From_Dim_To_Str_Of_Dim_Symbols.

            if Exists (Dims_Of_Actual) then
               New_Str_Lit :=
                 Make_String_Literal (Loc,
                   From_Dim_To_Str_Of_Dim_Symbols
                     (Dims_Of_Actual, System_Of (Base_Type (Etyp))));

            --  If dimensionless, the output is []

            else
               New_Str_Lit :=
                 Make_String_Literal (Loc, "[]");
            end if;

         --  Put case

         else
            --  Add the symbol as a suffix of the value if the subtype has a
            --  unit symbol or if the parameter is not dimensionless.

            if Exists (Symbol_Of (Etyp)) then
               Symbols := Symbol_Of (Etyp);
            else
               Symbols := From_Dim_To_Str_Of_Unit_Symbols
                            (Dims_Of_Actual, System_Of (Base_Type (Etyp)));
            end if;

            --  Check Symbols exists

            if Exists (Symbols) then
               Start_String;

               --  Put a space between the value and the dimension

               Store_String_Char (' ');
               Store_String_Chars (Symbols);
               New_Str_Lit := Make_String_Literal (Loc, End_String);
            end if;
         end if;

         if Present (New_Str_Lit) then

            --  Insert all actuals in New_Actuals

            Actual := First (Actuals);
            while Present (Actual) loop

               --  Copy every actuals in New_Actuals except the Symbols
               --  parameter association.

               if Nkind (Actual) = N_Parameter_Association
                 and then Chars (Selector_Name (Actual)) /= Name_Symbol
               then
                  Append_To (New_Actuals,
                     Make_Parameter_Association (Loc,
                        Selector_Name => New_Copy (Selector_Name (Actual)),
                        Explicit_Actual_Parameter =>
                           New_Copy (Explicit_Actual_Parameter (Actual))));

               elsif Nkind (Actual) /= N_Parameter_Association then
                  Append_To (New_Actuals, New_Copy (Actual));
               end if;

               Next (Actual);
            end loop;

            --  Create new Symbols param association and append to New_Actuals

            Append_To (New_Actuals,
              Make_Parameter_Association (Loc,
                Selector_Name => Make_Identifier (Loc, Name_Symbol),
                Explicit_Actual_Parameter => New_Str_Lit));

            --  Rewrite and analyze the procedure call

            if Chars (Name_Call) = Name_Image then
               Rewrite (N,
                 Make_Function_Call (Loc,
                   Name =>                   New_Copy (Name_Call),
                   Parameter_Associations => New_Actuals));
               Analyze_And_Resolve (N);
            else
               Rewrite (N,
                 Make_Procedure_Call_Statement (Loc,
                   Name =>                   New_Copy (Name_Call),
                   Parameter_Associations => New_Actuals));
               Analyze (N);
            end if;

         end if;
      end if;
   end Expand_Put_Call_With_Symbol;

   ------------------------------------
   -- From_Dim_To_Str_Of_Dim_Symbols --
   ------------------------------------

   --  Given a dimension vector and the corresponding dimension system, create
   --  a String_Id to output dimension symbols corresponding to the dimensions
   --  Dims. If In_Error_Msg is True, there is a special handling for character
   --  asterisk * which is an insertion character in error messages.

   function From_Dim_To_Str_Of_Dim_Symbols
     (Dims         : Dimension_Type;
      System       : System_Type;
      In_Error_Msg : Boolean := False) return String_Id
   is
      Dim_Power : Rational;
      First_Dim : Boolean := True;

      procedure Store_String_Oexpon;
      --  Store the expon operator symbol "**" in the string. In error
      --  messages, asterisk * is a special character and must be quoted
      --  to be placed literally into the message.

      -------------------------
      -- Store_String_Oexpon --
      -------------------------

      procedure Store_String_Oexpon is
      begin
         if In_Error_Msg then
            Store_String_Chars ("'*'*");
         else
            Store_String_Chars ("**");
         end if;
      end Store_String_Oexpon;

   --  Start of processing for From_Dim_To_Str_Of_Dim_Symbols

   begin
      --  Initialization of the new String_Id

      Start_String;

      --  Store the dimension symbols inside boxes

      if In_Error_Msg then
         Store_String_Chars ("'[");
      else
         Store_String_Char ('[');
      end if;

      for Position in Dimension_Type'Range loop
         Dim_Power := Dims (Position);
         if Dim_Power /= Zero then

            if First_Dim then
               First_Dim := False;
            else
               Store_String_Char ('.');
            end if;

            Store_String_Chars (System.Dim_Symbols (Position));

            --  Positive dimension case

            if Dim_Power.Numerator > 0 then

               --  Integer case

               if Dim_Power.Denominator = 1 then
                  if Dim_Power.Numerator /= 1 then
                     Store_String_Oexpon;
                     Store_String_Int (Int (Dim_Power.Numerator));
                  end if;

               --  Rational case when denominator /= 1

               else
                  Store_String_Oexpon;
                  Store_String_Char ('(');
                  Store_String_Int (Int (Dim_Power.Numerator));
                  Store_String_Char ('/');
                  Store_String_Int (Int (Dim_Power.Denominator));
                  Store_String_Char (')');
               end if;

            --  Negative dimension case

            else
               Store_String_Oexpon;
               Store_String_Char ('(');
               Store_String_Char ('-');
               Store_String_Int (Int (-Dim_Power.Numerator));

               --  Integer case

               if Dim_Power.Denominator = 1 then
                  Store_String_Char (')');

               --  Rational case when denominator /= 1

               else
                  Store_String_Char ('/');
                  Store_String_Int (Int (Dim_Power.Denominator));
                  Store_String_Char (')');
               end if;
            end if;
         end if;
      end loop;

      if In_Error_Msg then
         Store_String_Chars ("']");
      else
         Store_String_Char (']');
      end if;

      return End_String;
   end From_Dim_To_Str_Of_Dim_Symbols;

   -------------------------------------
   -- From_Dim_To_Str_Of_Unit_Symbols --
   -------------------------------------

   --  Given a dimension vector and the corresponding dimension system,
   --  create a String_Id to output the unit symbols corresponding to the
   --  dimensions Dims.

   function From_Dim_To_Str_Of_Unit_Symbols
     (Dims   : Dimension_Type;
      System : System_Type) return String_Id
   is
      Dim_Power : Rational;
      First_Dim : Boolean := True;

   begin
      --  Return No_String if dimensionless

      if not Exists (Dims) then
         return No_String;
      end if;

      --  Initialization of the new String_Id

      Start_String;

      for Position in Dimension_Type'Range loop
         Dim_Power := Dims (Position);

         if Dim_Power /= Zero then
            if First_Dim then
               First_Dim := False;
            else
               Store_String_Char ('.');
            end if;

            Store_String_Chars (System.Unit_Symbols (Position));

            --  Positive dimension case

            if Dim_Power.Numerator > 0 then

               --  Integer case

               if Dim_Power.Denominator = 1 then
                  if Dim_Power.Numerator /= 1 then
                     Store_String_Chars ("**");
                     Store_String_Int (Int (Dim_Power.Numerator));
                  end if;

               --  Rational case when denominator /= 1

               else
                  Store_String_Chars ("**");
                  Store_String_Char ('(');
                  Store_String_Int (Int (Dim_Power.Numerator));
                  Store_String_Char ('/');
                  Store_String_Int (Int (Dim_Power.Denominator));
                  Store_String_Char (')');
               end if;

            --  Negative dimension case

            else
               Store_String_Chars ("**");
               Store_String_Char ('(');
               Store_String_Char ('-');
               Store_String_Int (Int (-Dim_Power.Numerator));

               --  Integer case

               if Dim_Power.Denominator = 1 then
                  Store_String_Char (')');

               --  Rational case when denominator /= 1

               else
                  Store_String_Char ('/');
                  Store_String_Int (Int (Dim_Power.Denominator));
                  Store_String_Char (')');
               end if;
            end if;
         end if;
      end loop;

      return End_String;
   end From_Dim_To_Str_Of_Unit_Symbols;

   ---------
   -- GCD --
   ---------

   function GCD (Left, Right : Whole) return Int is
      L : Whole;
      R : Whole;

   begin
      L := Left;
      R := Right;
      while R /= 0 loop
         L := L mod R;

         if L = 0 then
            return Int (R);
         end if;

         R := R mod L;
      end loop;

      return Int (L);
   end GCD;

   --------------------------
   -- Has_Dimension_System --
   --------------------------

   function Has_Dimension_System (Typ : Entity_Id) return Boolean is
   begin
      return Exists (System_Of (Typ));
   end Has_Dimension_System;

   ------------------------------
   -- Is_Dim_IO_Package_Entity --
   ------------------------------

   function Is_Dim_IO_Package_Entity (E : Entity_Id) return Boolean is
   begin
      --  Check the package entity corresponds to System.Dim.Float_IO or
      --  System.Dim.Integer_IO.

      return
        Is_RTU (E, System_Dim_Float_IO)
          or else
        Is_RTU (E, System_Dim_Integer_IO);
   end Is_Dim_IO_Package_Entity;

   -------------------------------------
   -- Is_Dim_IO_Package_Instantiation --
   -------------------------------------

   function Is_Dim_IO_Package_Instantiation (N : Node_Id) return Boolean is
      Gen_Id : constant Node_Id := Name (N);

   begin
      --  Check that the instantiated package is either System.Dim.Float_IO
      --  or System.Dim.Integer_IO.

      return
        Is_Entity_Name (Gen_Id)
          and then Is_Dim_IO_Package_Entity (Entity (Gen_Id));
   end Is_Dim_IO_Package_Instantiation;

   ----------------
   -- Is_Invalid --
   ----------------

   function Is_Invalid (Position : Dimension_Position) return Boolean is
   begin
      return Position = Invalid_Position;
   end Is_Invalid;

   ---------------------
   -- Move_Dimensions --
   ---------------------

   procedure Move_Dimensions (From, To : Node_Id) is
   begin
      if Ada_Version < Ada_2012 then
         return;
      end if;

      --  Copy the dimension of 'From to 'To' and remove dimension of 'From'

      Copy_Dimensions   (From, To);
      Remove_Dimensions (From);
   end Move_Dimensions;

   ------------
   -- Reduce --
   ------------

   function Reduce (X : Rational) return Rational is
   begin
      if X.Numerator = 0 then
         return Zero;
      end if;

      declare
         G : constant Int := GCD (X.Numerator, X.Denominator);
      begin
         return Rational'(Numerator =>   Whole (Int (X.Numerator)   / G),
                          Denominator => Whole (Int (X.Denominator) / G));
      end;
   end Reduce;

   -----------------------
   -- Remove_Dimensions --
   -----------------------

   procedure Remove_Dimensions (N : Node_Id) is
      Dims_Of_N : constant Dimension_Type := Dimensions_Of (N);
   begin
      if Exists (Dims_Of_N) then
         Dimension_Table.Remove (N);
      end if;
   end Remove_Dimensions;

   -----------------------------------
   -- Remove_Dimension_In_Statement --
   -----------------------------------

   --  Removal of dimension in statement as part of the Analyze_Statements
   --  routine (see package Sem_Ch5).

   procedure Remove_Dimension_In_Statement (Stmt : Node_Id) is
   begin
      if Ada_Version < Ada_2012 then
         return;
      end if;

      --  Remove dimension in parameter specifications for accept statement

      if Nkind (Stmt) = N_Accept_Statement then
         declare
            Param : Node_Id := First (Parameter_Specifications (Stmt));
         begin
            while Present (Param) loop
               Remove_Dimensions (Param);
               Next (Param);
            end loop;
         end;

      --  Remove dimension of name and expression in assignments

      elsif Nkind (Stmt) = N_Assignment_Statement then
         Remove_Dimensions (Expression (Stmt));
         Remove_Dimensions (Name (Stmt));
      end if;
   end Remove_Dimension_In_Statement;

   --------------------
   -- Set_Dimensions --
   --------------------

   procedure Set_Dimensions (N : Node_Id; Val : Dimension_Type) is
   begin
      pragma Assert (OK_For_Dimension (Nkind (N)));
      pragma Assert (Exists (Val));

      Dimension_Table.Set (N, Val);
   end Set_Dimensions;

   ----------------
   -- Set_Symbol --
   ----------------

   procedure Set_Symbol (E : Entity_Id; Val : String_Id) is
   begin
      Symbol_Table.Set (E, Val);
   end Set_Symbol;

   ---------------------------------
   -- String_From_Numeric_Literal --
   ---------------------------------

   function String_From_Numeric_Literal (N : Node_Id) return String_Id is
      Loc     : constant Source_Ptr        := Sloc (N);
      Sbuffer : constant Source_Buffer_Ptr :=
                  Source_Text (Get_Source_File_Index (Loc));
      Src_Ptr : Source_Ptr := Loc;

      C : Character  := Sbuffer (Src_Ptr);
      --  Current source program character

      function Belong_To_Numeric_Literal (C : Character) return Boolean;
      --  Return True if C belongs to a numeric literal

      -------------------------------
      -- Belong_To_Numeric_Literal --
      -------------------------------

      function Belong_To_Numeric_Literal (C : Character) return Boolean is
      begin
         case C is
            when '0' .. '9'
               | '_' | '.' | 'e' | '#' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
            =>
               return True;

            --  Make sure '+' or '-' is part of an exponent.

            when '+' | '-' =>
               declare
                  Prev_C : constant Character := Sbuffer (Src_Ptr - 1);
               begin
                  return Prev_C = 'e' or else Prev_C = 'E';
               end;

            --  All other character doesn't belong to a numeric literal

            when others =>
               return False;
         end case;
      end Belong_To_Numeric_Literal;

   --  Start of processing for String_From_Numeric_Literal

   begin
      Start_String;
      while Belong_To_Numeric_Literal (C) loop
         Store_String_Char (C);
         Src_Ptr := Src_Ptr + 1;
         C       := Sbuffer (Src_Ptr);
      end loop;

      return End_String;
   end String_From_Numeric_Literal;

   ---------------
   -- Symbol_Of --
   ---------------

   function Symbol_Of (E : Entity_Id) return String_Id is
      Subtype_Symbol : constant String_Id := Symbol_Table.Get (E);
   begin
      if Subtype_Symbol /= No_String then
         return Subtype_Symbol;
      else
         return From_Dim_To_Str_Of_Unit_Symbols
                  (Dimensions_Of (E), System_Of (Base_Type (E)));
      end if;
   end Symbol_Of;

   -----------------------
   -- Symbol_Table_Hash --
   -----------------------

   function Symbol_Table_Hash (Key : Entity_Id) return Symbol_Table_Range is
   begin
      return Symbol_Table_Range (Key mod 511);
   end Symbol_Table_Hash;

   ---------------
   -- System_Of --
   ---------------

   function System_Of (E : Entity_Id) return System_Type is
      Type_Decl : constant Node_Id := Parent (E);

   begin
      --  Look for Type_Decl in System_Table

      for Dim_Sys in 1 .. System_Table.Last loop
         if Type_Decl = System_Table.Table (Dim_Sys).Type_Decl then
            return System_Table.Table (Dim_Sys);
         end if;
      end loop;

      return Null_System;
   end System_Of;

end Sem_Dim;
