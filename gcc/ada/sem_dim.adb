------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ D I M                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
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
   -- Rational arithmetic --
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
   -- System types --
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
   --  A data structure used to store the names of all units within a system

   No_Names : constant Name_Array := (others => No_Name);

   type Symbol_Array is
     array (Dimension_Position range
              Low_Position_Bound ..  High_Position_Bound) of String_Id;
   --  A data structure used to store the symbols of all units within a system

   No_Symbols : constant Symbol_Array := (others => No_String);

   type System_Type is record
      Type_Decl : Node_Id;
      Names     : Name_Array;
      Symbols   : Symbol_Array;
      Count     : Dimension_Position;
   end record;

   Null_System : constant System_Type :=
                   (Empty, No_Names, No_Symbols, Invalid_Position);

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
   -- Dimension type --
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
   -- Symbol types --
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

   procedure Analyze_Dimension_Function_Call (N : Node_Id);
   --  Subroutine of Analyze_Dimension for function call. General case:
   --  propagate the dimensions from the returned type to N. Elementary
   --  function case (Ada.Numerics.Generic_Elementary_Functions): If N
   --  is a Sqrt call, then evaluate the resulting dimensions as half the
   --  dimensions of the parameter. Otherwise, verify that each parameters
   --  are dimensionless.

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

   procedure Analyze_Dimension_Object_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for object declaration. Check that
   --  the dimensions of the object type and the dimensions of the expression
   --  (if expression is present) match. Note that when the expression is
   --  a literal, no warning is returned. This special case allows object
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
   --  return an error message.

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

   function Dimensions_Msg_Of (N : Node_Id) return String;
   --  Given a node, return "has dimension" followed by the dimension vector of
   --  N or "is dimensionless" if N is dimensionless.

   procedure Eval_Op_Expon_With_Rational_Exponent
     (N              : Node_Id;
      Exponent_Value : Rational);
   --  Evaluate the exponent it is a rational and the operand has a dimension

   function Exists (Dim : Dimension_Type) return Boolean;
   --  Returns True iff Dim does not denote the null dimension

   function Exists (Sys : System_Type) return Boolean;
   --  Returns True iff Sys does not denote the null system

   function From_Dimension_To_String_Of_Symbols
     (Dims   : Dimension_Type;
      System : System_Type) return String_Id;
   --  Given a dimension vector and a dimension system, return the proper
   --  string of symbols.

   function Is_Invalid (Position : Dimension_Position) return Boolean;
   --  Return True if Pos denotes the invalid position

   procedure Move_Dimensions (From : Node_Id; To : Node_Id);
   --  Copy dimension vector of From to To, delete dimension vector of From

   procedure Remove_Dimensions (N : Node_Id);
   --  Remove the dimension vector of node N

   procedure Set_Dimensions (N : Node_Id; Val : Dimension_Type);
   --  Associate a dimension vector with a node

   procedure Set_Symbol (E : Entity_Id; Val : String_Id);
   --  Associate a symbol representation of a dimension vector with a subtype

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
      return Rational'(Numerator =>   Right,
                       Denominator => 1);
   end "+";

   function "+" (Left, Right : Rational) return Rational is
      R : constant Rational :=
            Rational'(Numerator =>   Left.Numerator * Right.Denominator +
                                       Left.Denominator * Right.Numerator,
                      Denominator => Left.Denominator * Right.Denominator);
   begin
      return Reduce (R);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Right : Rational) return Rational is
   begin
      return Rational'(Numerator =>   -Right.Numerator,
                       Denominator => Right.Denominator);
   end "-";

   function "-" (Left, Right : Rational) return Rational is
      R : constant Rational :=
            Rational'(Numerator =>   Left.Numerator * Right.Denominator -
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
            Rational'(Numerator =>   Left.Numerator * Right.Numerator,
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

      return Reduce (Rational'(Numerator =>   L.Numerator * R.Denominator,
                               Denominator => L.Denominator * R.Numerator));
   end "/";
   -----------
   -- "abs" --
   -----------

   function "abs" (Right : Rational) return Rational is
   begin
      return Rational'(Numerator =>   abs Right.Numerator,
                       Denominator => Right.Denominator);
   end "abs";

   ------------------------------
   -- Analyze_Aspect_Dimension --
   ------------------------------

   --  with Dimension => DIMENSION_FOR_SUBTYPE
   --  DIMENSION_FOR_SUBTYPE ::= (DIMENSION_STRING, DIMENSION_RATIONALS)
   --  DIMENSION_RATIONALS ::=
   --    RATIONAL,  {, RATIONAL}
   --  | RATIONAL {, RATIONAL}, others => RATIONAL
   --  | DISCRETE_CHOICE_LIST => RATIONAL
   --  RATIONAL ::= [-] NUMERAL [/ NUMERAL]

   --  (see Analyze_Aspect_Dimension_System for DIMENSION_STRING grammar)

   procedure Analyze_Aspect_Dimension
     (N    : Node_Id;
      Id   : Entity_Id;
      Aggr : Node_Id)
   is
      Def_Id    : constant Entity_Id := Defining_Identifier (N);

      Processed : array (Dimension_Type'Range) of Boolean := (others => False);
      --  This array is used when processing ranges or Others_Choice as part of
      --  the dimension aggregate.

      Dimensions : Dimension_Type := Null_Dimension;

      procedure Extract_Power
        (Expr     : Node_Id;
         Position : Dimension_Position);
      --  Given an expression with denotes a rational number, read the number
      --  and associate it with Position in Dimensions.

      function Has_Compile_Time_Known_Expressions
        (Aggr : Node_Id) return Boolean;
      --  Determine whether aggregate Aggr contains only expressions that are
      --  known at compile time.

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
         if Is_Integer_Type (Def_Id) then
            Dimensions (Position) := +Whole (UI_To_Int (Expr_Value (Expr)));
         else
            Dimensions (Position) := Create_Rational_From (Expr, True);
         end if;

         Processed (Position) := True;
      end Extract_Power;

      ----------------------------------------
      -- Has_Compile_Time_Known_Expressions --
      ----------------------------------------

      function Has_Compile_Time_Known_Expressions
        (Aggr : Node_Id) return Boolean
      is
         Comp : Node_Id;
         Expr : Node_Id;

      begin
         Expr := First (Expressions (Aggr));
         if Present (Expr) then

            --  The first expression within the aggregate describes the
            --  symbolic name of a dimension, skip it.

            Next (Expr);
            while Present (Expr) loop
               Analyze_And_Resolve (Expr);

               if not Compile_Time_Known_Value (Expr) then
                  return False;
               end if;

               Next (Expr);
            end loop;
         end if;

         Comp := First (Component_Associations (Aggr));
         while Present (Comp) loop
            Expr := Expression (Comp);

            Analyze_And_Resolve (Expr);

            if not Compile_Time_Known_Value (Expr) then
               return False;
            end if;

            Next (Comp);
         end loop;

         return True;
      end Has_Compile_Time_Known_Expressions;

      ------------------------
      -- Position_In_System --
      ------------------------

      function Position_In_System
        (Id     : Node_Id;
         System : System_Type) return Dimension_Position
      is
         Dimension_Name : constant Name_Id := Chars (Id);

      begin
         for Position in System.Names'Range loop
            if Dimension_Name = System.Names (Position) then
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
      Symbol         : String_Id;
      Symbol_Decl    : Node_Id;
      System         : System_Type;
      Typ            : Entity_Id;

      Errors_Count : Nat;
      --  Errors_Count is a count of errors detected by the compiler so far
      --  just before the extraction of names and values in the aggregate
      --  (Step 3).
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

      if Nkind (Aggr) /= N_Aggregate then
         Error_Msg_N ("aggregate expected", Aggr);
         return;
      end if;

      --  Each expression in dimension aggregate must be known at compile time

      if not Has_Compile_Time_Known_Expressions (Aggr) then
         Error_Msg_N ("values of aggregate must be static", Aggr);
         return;
      end if;

      --  The dimension declarations are useless if the parent type does not
      --  declare a valid system.

      if not Exists (System) then
         Error_Msg_NE
           ("parent type of& lacks dimension system", Sub_Ind, Def_Id);
         return;
      end if;

      --  STEP 2: Structural verification of the dimension aggregate

      --  The first entry in the aggregate is the symbolic representation of
      --  the dimension.

      Symbol_Decl := First (Expressions (Aggr));

      if No (Symbol_Decl)
        or else not Nkind_In (Symbol_Decl, N_Character_Literal,
                                           N_String_Literal)
      then
         Error_Msg_N ("first argument must be character or string", Aggr);
         return;
      end if;

      --  STEP 3: Name and value extraction

      --  Get the number of errors detected by the compiler so far

      Errors_Count := Serious_Errors_Detected;

      --  Positional elements

      Expr := Next (Symbol_Decl);
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
      while Present (Assoc) loop
         Expr   := Expression (Assoc);
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
               if Present (Next (Choice))
                 or else Present (Prev (Choice))
               then
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

            --  All other cases are erroneous declarations of dimension names

            else
               Error_Msg_NE ("wrong syntax for aspect&", Choice, Id);
            end if;

            Num_Choices := Num_Choices + 1;
            Next (Choice);
         end loop;

         Num_Dimensions := Num_Dimensions + 1;
         Next (Assoc);
      end loop;

      --  STEP 4: Consistency of system and dimensions

      if Present (Next (Symbol_Decl))
        and then (Num_Choices > 1
                   or else (Num_Choices = 1 and then not Others_Seen))
      then
         Error_Msg_N
           ("named associations cannot follow positional associations", Aggr);

      elsif Num_Dimensions > System.Count then
         Error_Msg_N ("type& has more dimensions than system allows", Def_Id);

      elsif Num_Dimensions < System.Count and then not Others_Seen then
         Error_Msg_N ("type& has less dimensions than system allows", Def_Id);
      end if;

      --  STEP 5: Dimension symbol extraction

      if Nkind (Symbol_Decl) = N_Character_Literal then
         Start_String;
         Store_String_Char (UI_To_CC (Char_Literal_Value (Symbol_Decl)));
         Symbol := End_String;

      else
         Symbol := Strval (Symbol_Decl);
      end if;

      if String_Length (Symbol) = 0 and then not Exists (Dimensions) then
         Error_Msg_N ("useless dimension declaration", Aggr);
      end if;

      --  STEP 6: Storage of extracted values

      --  Check that no errors have been detected during the analysis

      if Errors_Count = Serious_Errors_Detected then
         if String_Length (Symbol) /= 0 then
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

   --  with Dimension_System => DIMENSION_PAIRS

   --  DIMENSION_PAIRS ::=
   --    (DIMENSION_PAIR
   --      [, DIMENSION_PAIR]
   --      [, DIMENSION_PAIR]
   --      [, DIMENSION_PAIR]
   --      [, DIMENSION_PAIR]
   --      [, DIMENSION_PAIR]
   --      [, DIMENSION_PAIR])
   --  DIMENSION_PAIR ::= (DIMENSION_IDENTIFIER, DIMENSION_STRING)
   --  DIMENSION_IDENTIFIER ::= IDENTIFIER
   --  DIMENSION_STRING ::= STRING_LITERAL | CHARACTER_LITERAL

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

      Dim_Name     : Node_Id;
      Dim_Pair     : Node_Id;
      Dim_Symbol   : Node_Id;
      Dim_System   : System_Type  := Null_System;
      Names        : Name_Array   := No_Names;
      Position     : Nat := 0;
      Symbols      : Symbol_Array := No_Symbols;

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

      Dim_Pair     := First (Expressions (Aggr));
      Errors_Count := Serious_Errors_Detected;
      while Present (Dim_Pair) loop
         Position := Position + 1;

         if Position > High_Position_Bound then
            Error_Msg_N
              ("too many dimensions in system", Aggr);
            exit;
         end if;

         if Nkind (Dim_Pair) /= N_Aggregate then
            Error_Msg_N ("aggregate expected", Dim_Pair);

         else
            if Present (Component_Associations (Dim_Pair)) then
               Error_Msg_N ("expected positional aggregate", Dim_Pair);

            else
               if List_Length (Expressions (Dim_Pair)) = 2 then
                  Dim_Name := First (Expressions (Dim_Pair));
                  Dim_Symbol := Next (Dim_Name);

                  --  Check the first argument for each pair is a name

                  if Nkind (Dim_Name) = N_Identifier then
                     Names (Position) := Chars (Dim_Name);
                  else
                     Error_Msg_N ("expected dimension name", Dim_Name);
                  end if;

                  --  Check the second argument for each pair is a string or a
                  --  character.

                  if not Nkind_In
                           (Dim_Symbol,
                              N_String_Literal,
                              N_Character_Literal)
                  then
                     Error_Msg_N ("expected dimension string or character",
                                  Dim_Symbol);

                  else
                     --  String case

                     if Nkind (Dim_Symbol) = N_String_Literal then
                        Symbols (Position) := Strval (Dim_Symbol);

                     --  Character case

                     else
                        Start_String;
                        Store_String_Char
                          (UI_To_CC (Char_Literal_Value (Dim_Symbol)));
                        Symbols (Position) := End_String;
                     end if;

                     --  Verify that the string is not empty

                     if String_Length (Symbols (Position)) = 0 then
                        Error_Msg_N
                          ("empty string not allowed here", Dim_Symbol);
                     end if;
                  end if;

               else
                  Error_Msg_N
                    ("two expressions expected in aggregate", Dim_Pair);
               end if;
            end if;
         end if;

         Next (Dim_Pair);
      end loop;

      --  STEP 4: Storage of extracted values

      --  Check that no errors have been detected during the analysis

      if Errors_Count = Serious_Errors_Detected then
         Dim_System.Type_Decl := N;
         Dim_System.Names := Names;
         Dim_System.Count := Position;
         Dim_System.Symbols := Symbols;
         System_Table.Append (Dim_System);
      end if;
   end Analyze_Aspect_Dimension_System;

   -----------------------
   -- Analyze_Dimension --
   -----------------------

   --  This dispatch routine propagates dimensions for each node

   procedure Analyze_Dimension (N : Node_Id) is
   begin
      --  Aspect is an Ada 2012 feature

      if Ada_Version < Ada_2012 then
         return;
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

         when N_Function_Call =>
            Analyze_Dimension_Function_Call (N);

         when N_Attribute_Reference       |
              N_Identifier                |
              N_Indexed_Component         |
              N_Qualified_Expression      |
              N_Selected_Component        |
              N_Slice                     |
              N_Type_Conversion           |
              N_Unchecked_Type_Conversion =>
            Analyze_Dimension_Has_Etype (N);

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

         when N_Unary_Op =>
            Analyze_Dimension_Unary_Op (N);

         when others => null;

      end case;
   end Analyze_Dimension;

   --------------------------------------------
   -- Analyze_Dimension_Assignment_Statement --
   --------------------------------------------

   procedure Analyze_Dimension_Assignment_Statement (N : Node_Id) is
      Lhs         : constant Node_Id := Name (N);
      Dims_Of_Lhs : constant Dimension_Type := Dimensions_Of (Lhs);
      Rhs         : constant Node_Id := Expression (N);
      Dims_Of_Rhs : constant Dimension_Type := Dimensions_Of (Rhs);

      procedure Error_Dim_For_Assignment_Statement (N, Lhs, Rhs : Node_Id);
      --  Error using Error_Msg_N at node N. Output in the error message the
      --  dimensions of left and right hand sides.

      ----------------------------------------
      -- Error_Dim_For_Assignment_Statement --
      ----------------------------------------

      procedure Error_Dim_For_Assignment_Statement (N, Lhs, Rhs : Node_Id) is
      begin
         Error_Msg_N ("?dimensions mismatch in assignment", N);
         Error_Msg_N ("?left-hand side " & Dimensions_Msg_Of (Lhs), N);
         Error_Msg_N ("?right-hand side " & Dimensions_Msg_Of (Rhs), N);
      end Error_Dim_For_Assignment_Statement;

   --  Start of processing for Analyze_Dimension_Assignment

   begin
      if Dims_Of_Lhs /= Dims_Of_Rhs then
         Error_Dim_For_Assignment_Statement (N, Lhs, Rhs);
      end if;
   end Analyze_Dimension_Assignment_Statement;

   ---------------------------------
   -- Analyze_Dimension_Binary_Op --
   ---------------------------------

   --  Check and propagate the dimensions for binary operators
   --  Note that when the dimensions mismatch, no dimension is propagated to N.

   procedure Analyze_Dimension_Binary_Op (N : Node_Id) is
      N_Kind : constant Node_Kind := Nkind (N);

      procedure Error_Dim_For_Binary_Op (N, L, R : Node_Id);
      --  Error using Error_Msg_N at node N
      --  Output in the error message the dimensions of both operands.

      -----------------------------
      -- Error_Dim_For_Binary_Op --
      -----------------------------

      procedure Error_Dim_For_Binary_Op (N, L, R : Node_Id) is
      begin
         Error_Msg_NE ("?both operands for operation& must have same " &
                       "dimensions",
                       N,
                       Entity (N));
         Error_Msg_N ("?left operand " & Dimensions_Msg_Of (L), N);
         Error_Msg_N ("?right operand " & Dimensions_Msg_Of (R), N);
      end Error_Dim_For_Binary_Op;

   --  Start of processing for Analyze_Dimension_Binary_Op

   begin
      if Nkind_In (N_Kind, N_Op_Add, N_Op_Expon, N_Op_Subtract)
        or else N_Kind in N_Multiplying_Operator
        or else N_Kind in N_Op_Compare
      then
         declare
            L                : constant Node_Id := Left_Opnd (N);
            Dims_Of_L        : constant Dimension_Type := Dimensions_Of (L);
            L_Has_Dimensions : constant Boolean := Exists (Dims_Of_L);
            R                : constant Node_Id := Right_Opnd (N);
            Dims_Of_R        : constant Dimension_Type := Dimensions_Of (R);
            R_Has_Dimensions : constant Boolean := Exists (Dims_Of_R);
            Dims_Of_N        : Dimension_Type := Null_Dimension;

         begin
            --  N_Op_Add, N_Op_Mod, N_Op_Rem or N_Op_Subtract case

            if Nkind_In (N, N_Op_Add, N_Op_Mod, N_Op_Rem, N_Op_Subtract) then

               --  Check both operands have same dimension

               if Dims_Of_L /= Dims_Of_R then
                  Error_Dim_For_Binary_Op (N, L, R);
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

               if L_Has_Dimensions
                 and then Compile_Time_Known_Value (R)
               then
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
            --  performed (no propagation).

            elsif N_Kind in N_Op_Compare then
               if (L_Has_Dimensions or R_Has_Dimensions)
                 and then Dims_Of_L /= Dims_Of_R
               then
                  Error_Dim_For_Binary_Op (N, L, R);
               end if;
            end if;

            --  Removal of dimensions for each operands

            Remove_Dimensions (L);
            Remove_Dimensions (R);
         end;
      end if;
   end Analyze_Dimension_Binary_Op;

   ---------------------------------------------
   -- Analyze_Dimension_Component_Declaration --
   ---------------------------------------------

   procedure Analyze_Dimension_Component_Declaration (N : Node_Id) is
      Expr         : constant Node_Id        := Expression (N);
      Id           : constant Entity_Id      := Defining_Identifier (N);
      Etyp         : constant Entity_Id      := Etype (Id);
      Dims_Of_Etyp : constant Dimension_Type := Dimensions_Of (Etyp);
      Dims_Of_Expr : Dimension_Type;

      procedure Error_Dim_For_Component_Declaration
        (N    : Node_Id;
         Etyp : Entity_Id;
         Expr : Node_Id);
      --  Error using Error_Msg_N at node N. Output in the error message the
      --  dimensions of the type Etyp and the expression Expr of N.

      -----------------------------------------
      -- Error_Dim_For_Component_Declaration --
      -----------------------------------------

      procedure Error_Dim_For_Component_Declaration
        (N    : Node_Id;
         Etyp : Entity_Id;
         Expr : Node_Id) is
      begin
         Error_Msg_N ("?dimensions mismatch in component declaration", N);
         Error_Msg_N ("\?component type " & Dimensions_Msg_Of (Etyp), N);
         Error_Msg_N ("\?component expression " & Dimensions_Msg_Of (Expr), N);
      end Error_Dim_For_Component_Declaration;

   --  Start of processing for Analyze_Dimension_Component_Declaration

   begin
      if Present (Expr) then
         Dims_Of_Expr := Dimensions_Of (Expr);

         --  Return an error if the dimension of the expression and the
         --  dimension of the type mismatch.

         if Dims_Of_Etyp /= Dims_Of_Expr then
            Error_Dim_For_Component_Declaration (N, Etyp, Expr);
         end if;

         --  Removal of dimensions in expression

         Remove_Dimensions (Expr);
      end if;
   end Analyze_Dimension_Component_Declaration;

   -------------------------------------------------
   -- Analyze_Dimension_Extended_Return_Statement --
   -------------------------------------------------

   procedure Analyze_Dimension_Extended_Return_Statement (N : Node_Id) is
      Return_Ent            : constant Entity_Id :=
                                Return_Statement_Entity (N);
      Return_Etyp           : constant Entity_Id :=
                                Etype (Return_Applies_To (Return_Ent));
      Dims_Of_Return_Etyp   : constant Dimension_Type :=
                                Dimensions_Of (Return_Etyp);
      Return_Obj_Decls      : constant List_Id :=
                                Return_Object_Declarations (N);
      Dims_Of_Return_Obj_Id : Dimension_Type;
      Return_Obj_Decl       : Node_Id;
      Return_Obj_Id         : Entity_Id;

      procedure Error_Dim_For_Extended_Return_Statement
        (N             : Node_Id;
         Return_Etyp   : Entity_Id;
         Return_Obj_Id : Entity_Id);
      --  Warning using Error_Msg_N at node N. Output in the error message the
      --  dimensions of the returned type Return_Etyp and the returned object
      --  Return_Obj_Id of N.

      ---------------------------------------------
      -- Error_Dim_For_Extended_Return_Statement --
      ---------------------------------------------

      procedure Error_Dim_For_Extended_Return_Statement
        (N             : Node_Id;
         Return_Etyp   : Entity_Id;
         Return_Obj_Id : Entity_Id)
      is
      begin
         Error_Msg_N ("?dimensions mismatch in extended return statement", N);
         Error_Msg_N ("?returned type " & Dimensions_Msg_Of (Return_Etyp), N);
         Error_Msg_N ("?returned object " & Dimensions_Msg_Of (Return_Obj_Id),
                      N);
      end Error_Dim_For_Extended_Return_Statement;

   --  Start of processing for Analyze_Dimension_Extended_Return_Statement
   begin
      if Present (Return_Obj_Decls) then
         Return_Obj_Decl := First (Return_Obj_Decls);
         while Present (Return_Obj_Decl) loop
            if Nkind (Return_Obj_Decl) = N_Object_Declaration then
               Return_Obj_Id := Defining_Identifier (Return_Obj_Decl);

               if Is_Return_Object (Return_Obj_Id) then
                  Dims_Of_Return_Obj_Id := Dimensions_Of (Return_Obj_Id);

                  if Dims_Of_Return_Etyp /= Dims_Of_Return_Obj_Id then
                     Error_Dim_For_Extended_Return_Statement
                       (N, Return_Etyp, Return_Obj_Id);
                     return;
                  end if;
               end if;
            end if;

            Next (Return_Obj_Decl);
         end loop;
      end if;
   end Analyze_Dimension_Extended_Return_Statement;

   -------------------------------------
   -- Analyze_Dimension_Function_Call --
   -------------------------------------

   procedure Analyze_Dimension_Function_Call (N : Node_Id) is
      Name_Call      : constant Node_Id := Name (N);
      Actuals        : constant List_Id := Parameter_Associations (N);
      Actual         : Node_Id;
      Dims_Of_Actual : Dimension_Type;
      Dims_Of_Call   : Dimension_Type;

      function Is_Elementary_Function_Call (N : Node_Id) return Boolean;
      --  Return True if the call is a call of an elementary function (see
      --  Ada.Numerics.Generic_Elementary_Functions).

      ---------------------------------
      -- Is_Elementary_Function_Call --
      ---------------------------------

      function Is_Elementary_Function_Call (N : Node_Id) return Boolean is
         Ent : Entity_Id;

      begin
         --  Note that the node must come from source (why not???)

         if Comes_From_Source (N) and then Is_Entity_Name (Name_Call) then
            Ent := Entity (Name_Call);

            --  Check the procedure is defined in an instantiation of a generic
            --  package.

            if Is_Generic_Instance (Scope (Ent)) then
               Ent := Cunit_Entity (Get_Source_Unit (Ent));

               --  Check the name of the generic package is
               --  Generic_Elementary_Functions

               return
                 Is_Library_Level_Entity (Ent)
                   and then Chars (Ent) = Name_Generic_Elementary_Functions;
            end if;
         end if;

         return False;
      end Is_Elementary_Function_Call;

   --  Start of processing for Analyze_Dimension_Function_Call

   begin
      --  Elementary function case

      if Is_Elementary_Function_Call (N) then

         --  Sqrt function call case

         if Chars (Name_Call) = Name_Sqrt then
            Dims_Of_Call := Dimensions_Of (First (Actuals));

            if Exists (Dims_Of_Call) then
               for Position in Dims_Of_Call'Range loop
                  Dims_Of_Call (Position) :=
                    Dims_Of_Call (Position) * Rational'(Numerator =>   1,
                                                        Denominator => 2);
               end loop;

               Set_Dimensions (N, Dims_Of_Call);
            end if;

         --  All other functions in Ada.Numerics.Generic_Elementary_Functions
         --  case. Note that all parameters here should be dimensionless.

         else
            Actual := First (Actuals);
            while Present (Actual) loop
               Dims_Of_Actual := Dimensions_Of (Actual);

               if Exists (Dims_Of_Actual) then
                  Error_Msg_NE
                    ("?parameter should be dimensionless for elementary "
                     & "function&", Actual, Name_Call);
                  Error_Msg_N
                    ("?parameter " & Dimensions_Msg_Of (Actual), Actual);
               end if;

               Next (Actual);
            end loop;
         end if;

      --  Other case

      else
         Analyze_Dimension_Has_Etype (N);
      end if;
   end Analyze_Dimension_Function_Call;

   ---------------------------------
   -- Analyze_Dimension_Has_Etype --
   ---------------------------------

   procedure Analyze_Dimension_Has_Etype (N : Node_Id) is
      Etyp         : constant Entity_Id := Etype (N);
      Dims_Of_Etyp : constant Dimension_Type := Dimensions_Of (Etyp);
      N_Kind       : constant Node_Kind := Nkind (N);

   begin
      --  Propagation of the dimensions from the type

      if Exists (Dims_Of_Etyp) then
         Set_Dimensions (N, Dims_Of_Etyp);
      end if;

      --  Removal of dimensions in expression

      --  Wouldn't a case statement be clearer here???

      if Nkind_In (N_Kind, N_Attribute_Reference, N_Indexed_Component) then
         declare
            Expr  : Node_Id;
            Exprs : constant List_Id := Expressions (N);
         begin
            if Present (Exprs) then
               Expr := First (Exprs);
               while Present (Expr) loop
                  Remove_Dimensions (Expr);
                  Next (Expr);
               end loop;
            end if;
         end;

      elsif Nkind_In (N_Kind, N_Qualified_Expression,
                              N_Type_Conversion,
                              N_Unchecked_Type_Conversion)
      then
         Remove_Dimensions (Expression (N));

      elsif N_Kind = N_Selected_Component then
         Remove_Dimensions (Selector_Name (N));
      end if;
   end Analyze_Dimension_Has_Etype;

   ------------------------------------------
   -- Analyze_Dimension_Object_Declaration --
   ------------------------------------------

   procedure Analyze_Dimension_Object_Declaration (N : Node_Id) is
      Expr        : constant Node_Id   := Expression (N);
      Id          : constant Entity_Id := Defining_Identifier (N);
      Etyp        : constant Entity_Id := Etype (Id);
      Dim_Of_Etyp : constant Dimension_Type := Dimensions_Of (Etyp);
      Dim_Of_Expr : Dimension_Type;

      procedure Error_Dim_For_Object_Declaration
        (N    : Node_Id;
         Etyp : Entity_Id;
         Expr : Node_Id);
      --  Warnings using Error_Msg_N at node N. Output in the error message the
      --  dimensions of the type Etyp and the ???

      --------------------------------------
      -- Error_Dim_For_Object_Declaration --
      --------------------------------------

      procedure Error_Dim_For_Object_Declaration
        (N    : Node_Id;
         Etyp : Entity_Id;
         Expr : Node_Id) is
      begin
         Error_Msg_N ("?dimensions mismatch in object declaration", N);
         Error_Msg_N ("\?object type " & Dimensions_Msg_Of (Etyp), N);
         Error_Msg_N ("\?object expression " & Dimensions_Msg_Of (Expr), N);
      end Error_Dim_For_Object_Declaration;

   --  Start of processing for Analyze_Dimension_Object_Declaration

   begin
      --  Expression is present

      if Present (Expr) then
         Dim_Of_Expr := Dimensions_Of (Expr);

         --  case when expression is not a literal and when dimensions of the
         --  expression and of the type mismatch

         if not Nkind_In (Original_Node (Expr),
                             N_Real_Literal,
                             N_Integer_Literal)
           and then Dim_Of_Expr /= Dim_Of_Etyp
         then
            Error_Dim_For_Object_Declaration (N, Etyp, Expr);
         end if;

         --  Removal of dimensions in expression

         Remove_Dimensions (Expr);
      end if;
   end Analyze_Dimension_Object_Declaration;

   ---------------------------------------------------
   -- Analyze_Dimension_Object_Renaming_Declaration --
   ---------------------------------------------------

   procedure Analyze_Dimension_Object_Renaming_Declaration (N : Node_Id) is
      Renamed_Name : constant Node_Id := Name (N);
      Sub_Mark     : constant Node_Id := Subtype_Mark (N);

      procedure Error_Dim_For_Object_Renaming_Declaration
        (N            : Node_Id;
         Sub_Mark     : Node_Id;
         Renamed_Name : Node_Id);
      --  Error using Error_Msg_N at node N. Output in the error message the
      --  dimensions of Sub_Mark and of Renamed_Name.

      -----------------------------------------------
      -- Error_Dim_For_Object_Renaming_Declaration --
      -----------------------------------------------

      procedure Error_Dim_For_Object_Renaming_Declaration
        (N            : Node_Id;
         Sub_Mark     : Node_Id;
         Renamed_Name : Node_Id) is
      begin
         Error_Msg_N ("?dimensions mismatch in object renaming declaration",
                      N);
         Error_Msg_N ("?type " & Dimensions_Msg_Of (Sub_Mark), N);
         Error_Msg_N ("?renamed object " & Dimensions_Msg_Of (Renamed_Name),
                      N);
      end Error_Dim_For_Object_Renaming_Declaration;

   --  Start of processing for Analyze_Dimension_Object_Renaming_Declaration

   begin
      if Dimensions_Of (Renamed_Name) /= Dimensions_Of (Sub_Mark) then
         Error_Dim_For_Object_Renaming_Declaration
           (N, Sub_Mark, Renamed_Name);
      end if;
   end Analyze_Dimension_Object_Renaming_Declaration;

   -----------------------------------------------
   -- Analyze_Dimension_Simple_Return_Statement --
   -----------------------------------------------

   procedure Analyze_Dimension_Simple_Return_Statement (N : Node_Id) is
      Expr                : constant Node_Id := Expression (N);
      Dims_Of_Expr        : constant Dimension_Type := Dimensions_Of (Expr);
      Return_Ent          : constant Entity_Id := Return_Statement_Entity (N);
      Return_Etyp         : constant Entity_Id :=
                              Etype (Return_Applies_To (Return_Ent));
      Dims_Of_Return_Etyp : constant Dimension_Type :=
                              Dimensions_Of (Return_Etyp);

      procedure Error_Dim_For_Simple_Return_Statement
        (N           : Node_Id;
         Return_Etyp : Entity_Id;
         Expr        : Node_Id);
      --  Error using Error_Msg_N at node N. Output in the error message
      --  the dimensions of the returned type Return_Etyp and the returned
      --  expression Expr of N.

      -------------------------------------------
      -- Error_Dim_For_Simple_Return_Statement --
      -------------------------------------------

      procedure Error_Dim_For_Simple_Return_Statement
        (N           : Node_Id;
         Return_Etyp : Entity_Id;
         Expr        : Node_Id)
      is
      begin
         Error_Msg_N ("?dimensions mismatch in return statement", N);
         Error_Msg_N ("\?returned type " & Dimensions_Msg_Of (Return_Etyp), N);
         Error_Msg_N ("\?returned expression " & Dimensions_Msg_Of (Expr), N);
      end Error_Dim_For_Simple_Return_Statement;

   --  Start of processing for Analyze_Dimension_Simple_Return_Statement

   begin
      if Dims_Of_Return_Etyp /= Dims_Of_Expr then
         Error_Dim_For_Simple_Return_Statement (N, Return_Etyp, Expr);
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

            --  If subtype already has a dimension (from Aspect_Dimension),
            --  it cannot inherit a dimension from its subtype.

            if Exists (Dims_Of_Id) then
               Error_Msg_N ("?subtype& already" & Dimensions_Msg_Of (Id), N);
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
         when N_Op_Plus | N_Op_Minus | N_Op_Abs =>
            declare
               R : constant Node_Id := Right_Opnd (N);

            begin
               --  Propagate the dimension if the operand is not dimensionless

               Move_Dimensions (R, N);
            end;

         when others => null;

      end case;
   end Analyze_Dimension_Unary_Op;

   --------------------------
   -- Create_Rational_From --
   --------------------------

   --  RATIONAL ::= [-] NUMERAL [/ NUMERAL]

   --  A rational number is a number that can be expressed as the quotient or
   --  fraction a/b of two integers, where b is non-zero.

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
         --  Both left and right operands are an integer literal

         if Nkind (Left) = N_Integer_Literal
           and then Nkind (Right) = N_Integer_Literal
         then
            Left_Rat := Process_Literal (Left);
            Right_Rat := Process_Literal (Right);
            Result := Left_Rat / Right_Rat;
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
         Error_Msg_N ("must be a rational", Expr);
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

   function Dimensions_Msg_Of (N : Node_Id) return String is
      Dims_Of_N      : constant Dimension_Type := Dimensions_Of (N);
      Dimensions_Msg : Name_Id;
      System         : System_Type;

      procedure Add_Dimension_Vector_To_Buffer
        (Dims   : Dimension_Type;
         System : System_Type);
      --  Given a Dims and System, add to Name_Buffer the string representation
      --  of a dimension vector.

      procedure Add_Whole_To_Buffer (W : Whole);
      --  Add image of Whole to Name_Buffer

      ------------------------------------
      -- Add_Dimension_Vector_To_Buffer --
      ------------------------------------

      procedure Add_Dimension_Vector_To_Buffer
        (Dims   : Dimension_Type;
         System : System_Type)
      is
         Dim_Power : Rational;
         First_Dim : Boolean := True;

      begin
         Add_Char_To_Name_Buffer ('(');

         for Position in Dims_Of_N'First ..  System.Count loop
            Dim_Power := Dims (Position);

            if First_Dim then
               First_Dim := False;
            else
               Add_Str_To_Name_Buffer (", ");
            end if;

            Add_Whole_To_Buffer (Dim_Power.Numerator);

            if Dim_Power.Denominator /= 1 then
               Add_Char_To_Name_Buffer ('/');
               Add_Whole_To_Buffer (Dim_Power.Denominator);
            end if;
         end loop;

         Add_Char_To_Name_Buffer (')');
      end Add_Dimension_Vector_To_Buffer;

      -------------------------
      -- Add_Whole_To_Buffer --
      -------------------------

      procedure Add_Whole_To_Buffer (W : Whole) is
      begin
         UI_Image (UI_From_Int (Int (W)));
         Add_Str_To_Name_Buffer (UI_Image_Buffer (1 .. UI_Image_Length));
      end Add_Whole_To_Buffer;

   --  Start of processing for Dimensions_Msg_Of

   begin
      --  Initialization of Name_Buffer

      Name_Len := 0;

      if Exists (Dims_Of_N) then
         System := System_Of (Base_Type (Etype (N)));
         Add_Str_To_Name_Buffer ("has dimensions: ");
         Add_Dimension_Vector_To_Buffer (Dims_Of_N, System);
      else
         Add_Str_To_Name_Buffer ("is dimensionless");
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

   ----------------------------------------
   -- Eval_Op_Expon_For_Dimensioned_Type --
   ----------------------------------------

   --  Evaluate the expon operator for real dimensioned type. Note that the
   --  node must come from source. Why???

   --  Note that if the exponent is an integer (denominator = 1) the node is
   --  evaluated by the regular Eval_Op_Expon routine (see Sem_Eval).

   procedure Eval_Op_Expon_For_Dimensioned_Type
     (N    : Node_Id;
      Btyp : Entity_Id)
   is
      R       : constant Node_Id := Right_Opnd (N);
      R_Value : Rational := No_Rational;

   begin
      if Comes_From_Source (N)
        and then Is_Real_Type (Btyp)
      then
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
      Dims_Of_N             : constant Dimension_Type := Dimensions_Of (N);
      L                     : constant Node_Id := Left_Opnd (N);
      Etyp_Of_L             : constant Entity_Id := Etype (L);
      Btyp_Of_L             : constant Entity_Id := Base_Type (Etyp_Of_L);
      Loc                   : constant Source_Ptr := Sloc (N);
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
         --      Dimension => ("",
         --        Dims_Of_N (1).Numerator / Dims_Of_N (1).Denominator,
         --        Dims_Of_N (2).Numerator / Dims_Of_N (2).Denominator,
         --        ...
         --        Dims_Of_N (Num_Of_Dims).Numerator /
         --          Dims_Of_N (Num_Of_Dims).Denominator);

         --  Step 1: Generate the new aggregate for the aspect Dimension

         New_Aspects  := Empty_List;
         List_Of_Dims := New_List;
         Append (Make_String_Literal (Loc, ""), List_Of_Dims);

         for Position in Dims_Of_N'First ..  System.Count loop
            Dim_Power := Dims_Of_N (Position);
            Append_To (List_Of_Dims,
               Make_Op_Divide (Loc,
                 Left_Opnd  =>
                   Make_Integer_Literal (Loc,
                     Int (Dim_Power.Numerator)),
                 Right_Opnd =>
                   Make_Integer_Literal (Loc,
                     Int (Dim_Power.Denominator))));
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
          Subtype_Mark => New_Reference_To (Standard_Long_Long_Float, Loc),
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
           Subtype_Mark => New_Reference_To (New_Id, Loc),
           Expression =>
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (RE_Expon_LLF), Loc),
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

   function Exists (Sys : System_Type) return Boolean is
   begin
      return Sys /= Null_System;
   end Exists;

   -------------------------------------------
   -- Expand_Put_Call_With_Dimension_Symbol --
   -------------------------------------------

   --  For procedure Put defined in System.Dim_Float_IO/System.Dim_Integer_IO,
   --  the default string parameter must be rewritten to include the dimension
   --  symbols in the output of a dimensioned object.

   --  Case 1: the parameter is a variable

   --  The default string parameter is replaced by the symbol defined in the
   --  aspect Dimension of the subtype. For instance to output a speed:

   --  subtype Force is Mks_Type
   --    with
   --      Dimension => ("N",
   --        Meter =>    1,
   --        Kilogram => 1,
   --        Second =>   -2,
   --        others =>   0);
   --  F : Force := 2.1 * m * kg * s**(-2);
   --  Put (F);
   --  > 2.1 N

   --  Case 2: the parameter is an expression

   --  In this case we call the procedure Expand_Put_Call_With_Dimension_Symbol
   --  that creates the string of symbols (for instance "m.s**(-1)") and
   --  rewrites the default string parameter of Put with the corresponding
   --  the String_Id. For instance:

   --  Put (2.1 * m * kg * s**(-2));
   --  > 2.1 m.kg.s**(-2)

   procedure Expand_Put_Call_With_Dimension_Symbol (N : Node_Id) is
      Actuals        : constant List_Id := Parameter_Associations (N);
      Loc            : constant Source_Ptr := Sloc (N);
      Name_Call      : constant Node_Id := Name (N);
      Actual         : Node_Id;
      Base_Typ       : Node_Id;
      Dims_Of_Actual : Dimension_Type;
      Etyp           : Entity_Id;
      First_Actual   : Node_Id;
      New_Actuals    : List_Id;
      New_Str_Lit    : Node_Id;
      Package_Name   : Name_Id;
      System         : System_Type;

      function Is_Procedure_Put_Call return Boolean;
      --  Return True if the current call is a call of an instantiation of a
      --  procedure Put defined in the package System.Dim_Float_IO and
      --  System.Dim_Integer_IO.

      ---------------------------
      -- Is_Procedure_Put_Call --
      ---------------------------

      function Is_Procedure_Put_Call return Boolean is
         Ent : Entity_Id;

      begin
         --  There are three different Put routine in each generic package
         --  Check that the current procedure call is one of them

         if Is_Entity_Name (Name_Call) then
            Ent := Entity (Name_Call);

            --  Check that the name of the procedure is Put
            --  Check the procedure is defined in an instantiation of a
            --  generic package.

            if Chars (Name_Call) = Name_Put
              and then Is_Generic_Instance (Scope (Ent))
            then
               Ent := Cunit_Entity (Get_Source_Unit (Ent));

               --  Verify that the generic package is System.Dim_Float_IO or
               --  System.Dim_Integer_IO.

               if Is_Library_Level_Entity (Ent) then
                  Package_Name := Chars (Ent);

                  return
                    Package_Name = Name_Dim_Float_IO
                      or else Package_Name = Name_Dim_Integer_IO;
               end if;
            end if;
         end if;

         return False;
      end Is_Procedure_Put_Call;

   --  Start of processing for Expand_Put_Call_With_Dimension_Symbol

   begin
      if Is_Procedure_Put_Call then

         --  Get the first parameter

         First_Actual := First (Actuals);

         --  Case when the Put routine has four (System.Dim_Integer_IO) or five
         --  (System.Dim_Float_IO) parameters.

         if List_Length (Actuals) = 5
           or else List_Length (Actuals) = 4
         then
            Actual := Next (First_Actual);

            if Nkind (Actual) = N_Parameter_Association then

               --  Get the dimensions and the corresponding dimension system
               --  from the first actual.

               Actual := First_Actual;
            end if;

         --  Case when the Put routine has six parameters

         else
            Actual := Next (First_Actual);
         end if;

         Base_Typ := Base_Type (Etype (Actual));
         System := System_Of (Base_Typ);

         --  Check the base type of Actual is a dimensioned type

         if Exists (System) then
            Dims_Of_Actual := Dimensions_Of (Actual);
            Etyp := Etype (Actual);

            --  Add the symbol as a suffix of the value if the subtype has a
            --  dimension symbol or if the parameter is not dimensionless.

            if Exists (Dims_Of_Actual)
              or else Symbol_Of (Etyp) /= No_String
            then
               New_Actuals := New_List;

               --  Add to the list First_Actual and Actual if they differ

               if Actual /= First_Actual then
                  Append (New_Copy (First_Actual), New_Actuals);
               end if;

               Append (New_Copy (Actual), New_Actuals);

               --  Look to the next parameter

               Next (Actual);

               --  Check if the type of N is a subtype that has a symbol of
               --  dimensions in Aspect_Dimension_String_Id_Hash_Table.

               if Symbol_Of (Etyp) /= No_String then
                  Start_String;

                  --  Put a space between the value and the dimension

                  Store_String_Char (' ');
                  Store_String_Chars (Symbol_Of (Etyp));
                  New_Str_Lit := Make_String_Literal (Loc, End_String);

               --  Rewrite the String_Literal of the second actual with the
               --  new String_Id created by the routine
               --  From_Dimension_To_String.

               else
                  New_Str_Lit :=
                    Make_String_Literal (Loc,
                      From_Dimension_To_String_Of_Symbols (Dims_Of_Actual,
                        System));
               end if;

               Append (New_Str_Lit, New_Actuals);

               --  Rewrite the procedure call with the new list of parameters

               Rewrite (N,
                 Make_Procedure_Call_Statement (Loc,
                   Name =>                   New_Copy (Name_Call),
                   Parameter_Associations => New_Actuals));

               Analyze (N);
            end if;
         end if;
      end if;
   end Expand_Put_Call_With_Dimension_Symbol;

   -----------------------------------------
   -- From_Dimension_To_String_Of_Symbols --
   -----------------------------------------

   --  Given a dimension vector and the corresponding dimension system,
   --  create a String_Id to output the dimension symbols corresponding to
   --  the dimensions Dims.

   function From_Dimension_To_String_Of_Symbols
     (Dims   : Dimension_Type;
      System : System_Type) return String_Id
   is
      Dimension_Power     : Rational;
      First_Symbol_In_Str : Boolean := True;

   begin
      --  Initialization of the new String_Id

      Start_String;

      --  Put a space between the value and the symbols

      Store_String_Char (' ');

      for Position in Dimension_Type'Range loop
         Dimension_Power := Dims (Position);
         if Dimension_Power /= Zero then

            if First_Symbol_In_Str then
               First_Symbol_In_Str := False;
            else
               Store_String_Char ('.');
            end if;

            --  Positive dimension case

            if Dimension_Power.Numerator > 0 then
               if System.Symbols (Position) = No_String then
                  Store_String_Chars
                    (Get_Name_String (System.Names (Position)));
               else
                  Store_String_Chars (System.Symbols (Position));
               end if;

               --  Integer case

               if Dimension_Power.Denominator = 1 then
                  if Dimension_Power.Numerator /= 1 then
                     Store_String_Chars ("**");
                     Store_String_Int (Int (Dimension_Power.Numerator));
                  end if;

               --  Rational case when denominator /= 1

               else
                  Store_String_Chars ("**");
                  Store_String_Char ('(');
                  Store_String_Int (Int (Dimension_Power.Numerator));
                  Store_String_Char ('/');
                  Store_String_Int (Int (Dimension_Power.Denominator));
                  Store_String_Char (')');
               end if;

            --  Negative dimension case

            else
               if System.Symbols (Position) = No_String then
                  Store_String_Chars
                    (Get_Name_String (System.Names (Position)));
               else
                  Store_String_Chars (System.Symbols (Position));
               end if;

               Store_String_Chars ("**");
               Store_String_Char ('(');
               Store_String_Char ('-');
               Store_String_Int (Int (-Dimension_Power.Numerator));

               --  Integer case

               if Dimension_Power.Denominator = 1 then
                  Store_String_Char (')');

               --  Rational case when denominator /= 1

               else
                  Store_String_Char ('/');
                  Store_String_Int (Int (Dimension_Power.Denominator));
                  Store_String_Char (')');
               end if;
            end if;
         end if;
      end loop;

      return End_String;
   end From_Dimension_To_String_Of_Symbols;

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

   -------------------------------------
   -- Is_Dim_IO_Package_Instantiation --
   -------------------------------------

   function Is_Dim_IO_Package_Instantiation (N : Node_Id) return Boolean is
      Gen_Id : constant Node_Id := Name (N);
      Ent    : Entity_Id;

   begin
      if Is_Entity_Name (Gen_Id) then
         Ent := Entity (Gen_Id);

         return
           Is_Library_Level_Entity (Ent)
             and then
               (Chars (Ent) = Name_Dim_Float_IO
                 or else Chars (Ent) = Name_Dim_Integer_IO);
      end if;

      return False;
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
      Dims_Of_From : constant Dimension_Type := Dimensions_Of (From);

   begin
      --  Copy the dimension of 'From to 'To' and remove dimension of 'From'

      if Exists (Dims_Of_From) then
         Set_Dimensions (To, Dims_Of_From);
         Remove_Dimensions (From);
      end if;
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
         return Rational'(Numerator =>   Whole (Int (X.Numerator) / G),
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

   ------------------------------
   -- Remove_Dimension_In_Call --
   ------------------------------

   procedure Remove_Dimension_In_Call (Call : Node_Id) is
      Actual : Node_Id;

   begin
      if Ada_Version < Ada_2012 then
         return;
      end if;

      Actual := First (Parameter_Associations (Call));

      while Present (Actual) loop
         Remove_Dimensions (Actual);
         Next (Actual);
      end loop;
   end Remove_Dimension_In_Call;

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

   ---------------
   -- Symbol_Of --
   ---------------

   function Symbol_Of (E : Entity_Id) return String_Id is
   begin
      return Symbol_Table.Get (E);
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
