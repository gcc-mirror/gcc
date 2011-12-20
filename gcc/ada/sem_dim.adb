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

   Zero : constant Rational := (0, 1);

   --  Rational constructors

   function "+" (Right : Whole) return Rational;
   function "/" (Left, Right : Whole) return Rational;
   function GCD (Left, Right : Whole) return Int;
   function Reduce (X : Rational) return Rational;

   --  Unary operator for Rational

   function "-" (Right : Rational) return Rational;

   --  Rational operations for Rationals

   function "+" (Left, Right : Rational) return Rational;
   function "-" (Left, Right : Rational) return Rational;
   function "*" (Left, Right : Rational) return Rational;

   --  Operation between Rational and Int

   function "*" (Left : Rational; Right : Whole) return Rational;

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
   --  Subroutine of Analyze_Dimension for assignment statement
   --  ??? what does this routine do?

   procedure Analyze_Dimension_Binary_Op (N : Node_Id);
   --  Subroutine of Analyze_Dimension for binary operators
   --  ??? same here

   procedure Analyze_Dimension_Component_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for component declaration
   --  ??? same here

   procedure Analyze_Dimension_Extended_Return_Statement (N : Node_Id);
   --  Subroutine of Analyze_Dimension for extended return statement
   --  ??? same here

   procedure Analyze_Dimension_Function_Call (N : Node_Id);
   --  Subroutine of Analyze_Dimension for function call
   --  ??? same here

   procedure Analyze_Dimension_Has_Etype (N : Node_Id);
   --  Subroutine of Analyze_Dimension for N_Has_Etype nodes:
   --  N_Attribute_Reference
   --  N_Indexed_Component
   --  N_Qualified_Expression
   --  N_Selected_Component
   --  N_Slice
   --  N_Type_Conversion
   --  N_Unchecked_Type_Conversion
   --  ??? poor comment, N_Has_Etype contains Node_Ids not listed above, what
   --  about those?

   procedure Analyze_Dimension_Identifier (N : Node_Id);
   --  Subroutine of Analyze_Dimension for identifier
   --  ??? what does this routine do?

   procedure Analyze_Dimension_Object_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for object declaration
   --  ??? same here

   procedure Analyze_Dimension_Object_Renaming_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for object renaming declaration
   --  ??? same here

   procedure Analyze_Dimension_Simple_Return_Statement (N : Node_Id);
   --  Subroutine of Analyze_Dimension for simple return statement
   --  ??? same here

   procedure Analyze_Dimension_Subtype_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for subtype declaration
   --  ??? same here

   procedure Analyze_Dimension_Unary_Op (N : Node_Id);
   --  Subroutine of Analyze_Dimension for unary operators
   --  ??? same here

   procedure Copy_Dimensions (From : Node_Id; To : Node_Id);
   --  Copy the dimension vector from one node to another

   function Create_Rational_From_Expr (Expr : Node_Id) return Rational;
   --  Given an expression, creates a rational number
   --  ??? what does this expression represent?

   function Dimensions_Of (N : Node_Id) return Dimension_Type;
   --  Return the dimension vector of node N

   procedure Eval_Op_Expon_With_Rational_Exponent
     (N   : Node_Id;
      Rat : Rational);
   --  Evaluate the Expon if the exponent is a rational and the operand has a
   --  dimension.

   function Exists (Dim : Dimension_Type) return Boolean;
   --  Determine whether Dim does not denote the null dimension

   function Exists (Sys : System_Type) return Boolean;
   --  Determine whether Sys does not denote the null system

   function From_Dimension_To_String_Id
     (Dims   : Dimension_Type;
      System : System_Type) return String_Id;
   --  Given a dimension vector and a dimension system, return the proper
   --  string of symbols.

   function Is_Invalid (Position : Dimension_Position) return Boolean;
   --  Determine whether Pos denotes the invalid position

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
      return (Right, 1);
   end "+";

   function "+" (Left, Right : Rational) return Rational is
      R : constant Rational :=
            Rational'(Numerator   => Left.Numerator * Right.Denominator +
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
      return Rational'(Numerator   => -Right.Numerator,
                       Denominator => Right.Denominator);
   end "-";

   function "-" (Left, Right : Rational) return Rational is
      R : constant Rational :=
            Rational'(Numerator   => Left.Numerator * Right.Denominator -
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
            Rational'(Numerator   => Left.Numerator * Right.Numerator,
                      Denominator => Left.Denominator * Right.Denominator);

   begin
      return Reduce (R);
   end "*";

   function "*" (Left : Rational; Right : Whole) return Rational is
      R : constant Rational :=
            Rational'(Numerator   => Left.Numerator * Right,
                      Denominator => Left.Denominator);

   begin
      return Reduce (R);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (Left, Right : Whole) return  Rational is
      R : constant Int := abs Int (Right);
      L : Int          := Int (Left);

   begin
      if Right < 0 then
         L := -L;
      end if;

      return Reduce (Rational'(Numerator   => Whole (L),
                               Denominator => Whole (R)));
   end "/";

   ------------------------------
   -- Analyze_Aspect_Dimension --
   ------------------------------

   --  with Dimension => DIMENSION_FOR_SUBTYPE
   --  DIMENSION_FOR_SUBTYPE ::= (DIMENSION_STRING, DIMENSION_RATIONALS)
   --  DIMENSION_RATIONALS ::=
   --    RATIONAL,  {, RATIONAL}
   --  | RATIONAL {, RATIONAL}, others => RATIONAL
   --  | DISCRETE_CHOICE_LIST => RATIONAL

   --  (see Analyze_Aspect_Dimension_System for DIMENSION_STRING grammar)

   procedure Analyze_Aspect_Dimension
     (N    : Node_Id;
      Id   : Node_Id;
      Aggr : Node_Id)
   is
      Def_Id   : constant Entity_Id   := Defining_Identifier (N);
      Typ      : constant Entity_Id   := Etype (Def_Id);
      Base_Typ : constant Entity_Id   := Base_Type (Typ);
      System   : constant System_Type := System_Of (Base_Typ);

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
            Dimensions (Position) := Create_Rational_From_Expr (Expr);
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
      Symbol         : String_Id;
      Symbol_Decl    : Node_Id;

   --  Start of processing for Analyze_Aspect_Dimension

   begin
      --  STEP 1: Legality of aspect

      if Nkind (N) /= N_Subtype_Declaration then
         Error_Msg_NE ("aspect % must apply to subtype declaration", N, Id);
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
         Error_Msg_NE ("parent type of % lacks dimension system", N, Def_Id);
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

      --  Positional elements

      Expr := Next (Symbol_Decl);
      Position := Low_Position_Bound;
      while Present (Expr) loop
         if Position > High_Position_Bound then
            Error_Msg_N
              ("type has more dimensions than system allows", Def_Id);
            return;
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
                  Error_Msg_N ("dimension name not part of system", Choice);
                  return;
               end if;

               Extract_Power (Expr, Position);

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
                     return;
                  elsif Nkind (High) /= N_Identifier then
                     Error_Msg_N ("bound must denote a dimension name", High);
                     return;
                  end if;

                  Low_Pos  := Position_In_System (Low, System);
                  High_Pos := Position_In_System (High, System);

                  if Is_Invalid (Low_Pos) then
                     Error_Msg_N ("dimension name not part of system", Low);
                     return;

                  elsif Is_Invalid (High_Pos) then
                     Error_Msg_N ("dimension name not part of system", High);
                     return;

                  elsif Low_Pos > High_Pos then
                     Error_Msg_N ("expected low to high range", Choice);
                     return;
                  end if;

                  for Position in Low_Pos .. High_Pos loop
                     Extract_Power (Expr, Position);
                  end loop;
               end;

            --  Others case: OTHERS => EXPRESSION

            elsif Nkind (Choice) = N_Others_Choice then
               if Present (Next (Choice)) then
                  Error_Msg_N
                    ("OTHERS must appear alone in a choice list", Choice);
                  return;

               elsif Present (Next (Assoc)) then
                  Error_Msg_N
                    ("OTHERS must appear last in an aggregate", Choice);
                  return;

               elsif Others_Seen then
                  Error_Msg_N ("multiple OTHERS not allowed", Choice);
                  return;
               end if;

               Others_Seen := True;

               --  Fill the non-processed dimensions with the default value
               --  supplied by others.

               for Position in Processed'Range loop
                  if not Processed (Position) then
                     Extract_Power (Expr, Position);
                  end if;
               end loop;

            --  All other cases are erroneous declarations of dimension names

            else
               Error_Msg_N ("wrong syntax for aspect%", Choice);
               return;
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
         Error_Msg_N ("type has more dimensions than system allows", Def_Id);

      elsif Num_Dimensions < System.Count and then not Others_Seen then
         Error_Msg_N ("type has less dimensions than system allows", Def_Id);
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

      if String_Length (Symbol) /= 0 then
         Set_Symbol (Def_Id, Symbol);
      end if;

      if Exists (Dimensions) then
         Set_Dimensions (Def_Id, Dimensions);
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
      Id   : Node_Id;
      Expr : Node_Id)
   is
      Dim_Name   : Node_Id;
      Dim_Node   : Node_Id;
      Dim_Symbol : Node_Id;
      D_Sys      : System_Type  := Null_System;
      Names      : Name_Array   := No_Names;
      N_Of_Dims  : Dimension_Position;
      Symbols    : Symbol_Array := No_Symbols;

      function Derived_From_Numeric_Type (N : Node_Id) return Boolean;
      --  Return True if the node is a derived type declaration from any
      --  numeric type.

      function Check_Dimension_System_Syntax (N : Node_Id) return Boolean;
      --  Return True if the expression is an aggregate of names

      function Check_Number_Of_Dimensions (Expr : Node_Id) return Boolean;
      --  Return True if the number of dimensions in the corresponding
      --  dimension is positive and lower than Max_Number_Of_Dimensions.

      -------------------------------
      -- Derived_From_Numeric_Type --
      -------------------------------

      function Derived_From_Numeric_Type (N : Node_Id) return Boolean is
      begin
         case (Nkind (N)) is
            when N_Full_Type_Declaration =>
               declare
                  T_Def : constant Node_Id := Type_Definition (N);
                  Ent   : Entity_Id;

               begin
                  --  Check that the node is a derived type declaration from
                  --  a numeric type.

                  if Nkind (T_Def) /= N_Derived_Type_Definition then
                     return False;
                  else
                     Ent := Entity (Subtype_Indication (T_Def));

                     if Is_Numeric_Type (Ent) then
                        return True;
                     else
                        return False;
                     end if;
                  end if;
               end;

            when others => return False;
         end case;
      end Derived_From_Numeric_Type;

      -----------------------------------
      -- Check_Dimension_System_Syntax --
      -----------------------------------

      --  Check that the expression of aspect Dimension_System is an aggregate
      --  which contains pairs of identifier and string or character literal.

      function Check_Dimension_System_Syntax (N : Node_Id) return Boolean is
         Dim_Node : Node_Id;
         Expr_Dim : Node_Id;

      begin
         --  Chek that the aggregate is a positional array

         if Present (Component_Associations (N)) then
            return False;

         else
            --  Check that each component of the aggregate is an aggregate

            Dim_Node := First (Expressions (N));
            while Present (Dim_Node) loop

               --  Verify that the aggregate is a pair of identifier and string
               --  or character literal.

               if Nkind (Dim_Node) = N_Aggregate then
                  if not Present (Expressions (Dim_Node)) then
                     return False;
                  end if;

                  if Present (Component_Associations (Dim_Node)) then
                     return False;
                  end if;

                  --  First expression in the aggregate

                  Expr_Dim := First (Expressions (Dim_Node));

                  if Nkind (Expr_Dim) /= N_Identifier then
                     return False;
                  end if;

                  --  Second expression in the aggregate

                  Next (Expr_Dim);

                  if not Nkind_In (Expr_Dim,
                                   N_String_Literal,
                                   N_Character_Literal)
                  then
                     return False;
                  end if;

                  --  If the aggregate has a third expression, return False

                  Next (Expr_Dim);

                  if Present (Expr_Dim) then
                     return False;
                  end if;
               else
                  return False;
               end if;

               Next (Dim_Node);
            end loop;

            return True;
         end if;
      end Check_Dimension_System_Syntax;

      --------------------------------
      -- Check_Number_Of_Dimensions --
      --------------------------------

      function Check_Number_Of_Dimensions (Expr : Node_Id) return Boolean is
         List_Expr : constant List_Id := Expressions (Expr);
      begin
         if List_Length (List_Expr) < Dimension_Position'First
           or else List_Length (List_Expr) > Max_Number_Of_Dimensions
         then
            return False;
         else
            return True;
         end if;
      end Check_Number_Of_Dimensions;

   --  Start of processing for Analyze_Aspect_Dimension_System

   begin
      --  Error_Msg_Name_1 := Chars (Id);

      --  Syntax checking

      if Nkind (Expr) /= N_Aggregate then
         Error_Msg_N ("wrong syntax for aspect%", Expr);
         return;
      end if;

      if not Derived_From_Numeric_Type (N) then
         Error_Msg_N
           ("aspect% only apply for type derived from numeric type", Id);
         return;
      end if;

      if not Check_Dimension_System_Syntax (Expr) then
         Error_Msg_N ("wrong syntax for aspect%", Expr);
         return;
      end if;

      if not Check_Number_Of_Dimensions (Expr) then
         Error_Msg_N ("wrong number of dimensions for aspect%", Expr);
         return;
      end if;

      --  Number of dimensions in the system

      N_Of_Dims := List_Length (Expressions (Expr));

      --  Create the new dimension system

      D_Sys.Type_Decl := N;
      Dim_Node := First (Expressions (Expr));

      for Dim in Names'First .. N_Of_Dims loop
         Dim_Name := First (Expressions (Dim_Node));
         Names (Dim) := Chars (Dim_Name);
         Dim_Symbol := Next (Dim_Name);

         --  N_Character_Literal case

         if Nkind (Dim_Symbol) = N_Character_Literal then
            Start_String;
            Store_String_Char (UI_To_CC (Char_Literal_Value (Dim_Symbol)));
            Symbols (Dim) := End_String;

         --  N_String_Literal case

         else
            Symbols (Dim) := Strval (Dim_Symbol);
         end if;

         Next (Dim_Node);
      end loop;

      D_Sys.Names := Names;
      D_Sys.Count := N_Of_Dims;
      D_Sys.Symbols := Symbols;

      --  Store the dimension system in the Table

      System_Table.Append (D_Sys);
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

         when N_Subtype_Declaration =>
            Analyze_Dimension_Subtype_Declaration (N);

         when N_Object_Declaration =>
            Analyze_Dimension_Object_Declaration (N);

         when N_Object_Renaming_Declaration =>
            Analyze_Dimension_Object_Renaming_Declaration (N);

         when N_Component_Declaration =>
            Analyze_Dimension_Component_Declaration (N);

         when N_Binary_Op =>
            Analyze_Dimension_Binary_Op (N);

         when N_Unary_Op =>
            Analyze_Dimension_Unary_Op (N);

         when N_Identifier =>
            Analyze_Dimension_Identifier (N);

         when N_Attribute_Reference       |
              N_Indexed_Component         |
              N_Qualified_Expression      |
              N_Selected_Component        |
              N_Slice                     |
              N_Type_Conversion           |
              N_Unchecked_Type_Conversion =>
            Analyze_Dimension_Has_Etype (N);

         when N_Function_Call =>
            Analyze_Dimension_Function_Call (N);

         when N_Extended_Return_Statement =>
            Analyze_Dimension_Extended_Return_Statement (N);

         when N_Simple_Return_Statement =>
            Analyze_Dimension_Simple_Return_Statement (N);

         when others => null;

      end case;
   end Analyze_Dimension;

   --------------------------------------------
   -- Analyze_Dimension_Assignment_Statement --
   --------------------------------------------

   procedure Analyze_Dimension_Assignment_Statement (N : Node_Id) is
      Lhs     : constant Node_Id    := Name (N);
      Dim_Lhs : constant Dimension_Type := Dimensions_Of (Lhs);
      Rhs     : constant Node_Id    := Expression (N);
      Dim_Rhs : constant Dimension_Type := Dimensions_Of (Rhs);

      procedure Analyze_Dimensions_In_Assignment
        (Dim_Lhs : Dimension_Type;
         Dim_Rhs : Dimension_Type);
      --  Perform the dimensionality checking for assignment

      --------------------------------------
      -- Analyze_Dimensions_In_Assignment --
      --------------------------------------

      procedure Analyze_Dimensions_In_Assignment
        (Dim_Lhs : Dimension_Type;
         Dim_Rhs : Dimension_Type)
      is
      begin
         --  Check the lhs and the rhs have the same dimension

         if not Exists (Dim_Lhs) then
            if Exists (Dim_Rhs) then
               Error_Msg_N ("?dimensions missmatch in assignment", N);
            end if;

         else
            if Dim_Lhs /= Dim_Rhs then
               Error_Msg_N ("?dimensions missmatch in assignment", N);
            end if;
         end if;
      end Analyze_Dimensions_In_Assignment;

   --  Start of processing for Analyze_Dimension_Assignment

   begin
      Analyze_Dimensions_In_Assignment (Dim_Lhs, Dim_Rhs);
   end Analyze_Dimension_Assignment_Statement;

   ---------------------------------
   -- Analyze_Dimension_Binary_Op --
   ---------------------------------

   procedure Analyze_Dimension_Binary_Op (N : Node_Id) is
      N_Kind : constant Node_Kind := Nkind (N);

   begin
      if Nkind_In (N_Kind, N_Op_Add, N_Op_Expon, N_Op_Subtract)
        or else N_Kind in N_Multiplying_Operator
        or else N_Kind in N_Op_Compare
      then
         declare
            L                 : constant Node_Id := Left_Opnd (N);
            L_Dims            : constant Dimension_Type := Dimensions_Of (L);
            L_Has_Dimensions  : constant Boolean := Exists (L_Dims);
            R                 : constant Node_Id := Right_Opnd (N);
            R_Dims            : constant Dimension_Type := Dimensions_Of (R);
            R_Has_Dimensions  : constant Boolean := Exists (R_Dims);
            Dims              : Dimension_Type := Null_Dimension;

         begin
            if Nkind_In (N, N_Op_Add, N_Op_Mod, N_Op_Rem, N_Op_Subtract) then

               --  What is the following deleted code about
               --  Error_Msg_Name_1 := Chars (N);

               --  Check both operands dimension

               if L_Has_Dimensions and R_Has_Dimensions then

                  --  If dimensions missmatch

                  if L_Dims /= R_Dims then
                     Error_Msg_N
                       ("?both operands for operation% must have same " &
                        "dimension", N);
                  else
                     Set_Dimensions (N, L_Dims);
                  end if;

               elsif not L_Has_Dimensions and R_Has_Dimensions then
                  Error_Msg_N
                    ("?both operands for operation% must have same dimension",
                     N);

               elsif L_Has_Dimensions and not R_Has_Dimensions then
                  Error_Msg_N
                    ("?both operands for operation% must have same dimension",
                     N);

               end if;

            elsif Nkind_In (N_Kind, N_Op_Multiply, N_Op_Divide) then
               if L_Has_Dimensions and R_Has_Dimensions then

                  --  Get both operands dimension and add them

                  if N_Kind = N_Op_Multiply then
                     for Dim in Dimension_Type'Range loop
                        Dims (Dim) := L_Dims (Dim) + R_Dims (Dim);
                     end loop;

                  --  Get both operands dimension and subtract them

                  else
                     for Dim in Dimension_Type'Range loop
                        Dims (Dim) := L_Dims (Dim) - R_Dims (Dim);
                     end loop;
                  end if;

               elsif L_Has_Dimensions and not R_Has_Dimensions then
                  Dims := L_Dims;

               elsif not L_Has_Dimensions and R_Has_Dimensions then
                  if N_Kind = N_Op_Multiply then
                     Dims := R_Dims;
                  else
                     for Dim in R_Dims'Range loop
                        Dims (Dim) := -R_Dims (Dim);
                     end loop;
                  end if;
               end if;

               if Exists (Dims) then
                  Set_Dimensions (N, Dims);
               end if;

               --  N_Op_Expon

            --  Propagation of the dimension and evaluation of the result if
            --  the exponent is a rational and if the operand has a dimension.

            elsif N_Kind = N_Op_Expon then
               declare
                  Rat : Rational := Zero;

               begin
                  --  Check exponent is dimensionless

                  if R_Has_Dimensions then
                     Error_Msg_N
                      ("?right operand cannot have a dimension for&",
                       Identifier (N));

                  else
                     --  Check the left operand is not dimensionless

                     --  Note that the value of the exponent must be know at
                     --  compile time. Otherwise, the exponentiation evaluation
                     --  will return an error message.

                     if Exists (System_Of (Base_Type (Etype (L))))
                       and then Compile_Time_Known_Value (R)
                     then
                        --  Real exponent case

                        if Is_Real_Type (Etype (L)) then

                           --  Define the exponent as a Rational number

                           Rat := Create_Rational_From_Expr (R);

                           if L_Has_Dimensions then
                              for Dim in Dimension_Type'Range loop
                                 Dims (Dim) := L_Dims (Dim) * Rat;
                              end loop;

                              if Exists (Dims) then
                                 Set_Dimensions (N, Dims);
                              end if;
                           end if;

                           --  Evaluate the operator with rational exponent

                           --  Eval_Op_Expon_With_Rational_Exponent (N, Rat);

                        --  Integer exponent case

                        else
                           for Dim in Dimension_Type'Range loop
                              Dims (Dim) :=
                                L_Dims (Dim) *
                                 Whole (UI_To_Int (Expr_Value (R)));
                           end loop;

                           if Exists (Dims) then
                              Set_Dimensions (N, Dims);
                           end if;
                        end if;
                     end if;
                  end if;
               end;

            --  For relational operations, only a dimension checking is
            --  performed (no propagation).

            elsif N_Kind in N_Op_Compare then

               --  What is this deleted code about ???
               --  Error_Msg_Name_1 := Chars (N);

               if (L_Has_Dimensions or R_Has_Dimensions)
                  and then L_Dims /= R_Dims
               then
                  Error_Msg_N
                    ("?both operands for operation% must have same dimension",
                     N);
               end if;
            end if;

            Remove_Dimensions (L);
            Remove_Dimensions (R);
         end;
      end if;
   end Analyze_Dimension_Binary_Op;

   ---------------------------------------------
   -- Analyze_Dimension_Component_Declaration --
   ---------------------------------------------

   procedure Analyze_Dimension_Component_Declaration (N : Node_Id) is
      Expr   : constant Node_Id    := Expression (N);
      Id     : constant Entity_Id  := Defining_Identifier (N);
      E_Typ  : constant Entity_Id  := Etype (Id);
      Dim_T  : constant Dimension_Type := Dimensions_Of (E_Typ);
      Dim_E  : Dimension_Type;

   begin
      if Exists (Dim_T) then

         --  If the component type has a dimension and there is no expression,
         --  propagates the dimension.

         if Present (Expr) then
            Dim_E := Dimensions_Of (Expr);

            if Exists (Dim_E) then

               --  Return an error if the dimension of the expression and the
               --  dimension of the type missmatch.

               if Dim_E /= Dim_T then
                  Error_Msg_N ("?dimensions missmatch in object " &
                               "declaration", N);
               end if;

               --  Case of dimensionless expression

            else
               Error_Msg_N
                 ("?dimensions missmatch in component declaration", N);
            end if;

         --  For every other cases, propagate the dimensions

         else
            Copy_Dimensions (E_Typ, Id);
         end if;
      end if;
   end Analyze_Dimension_Component_Declaration;

   -------------------------------------------------
   -- Analyze_Dimension_Extended_Return_Statement --
   -------------------------------------------------

   procedure Analyze_Dimension_Extended_Return_Statement (N : Node_Id) is
      Obj_Decls : constant List_Id := Return_Object_Declarations (N);
      R_Ent     : constant Entity_Id := Return_Statement_Entity (N);
      R_Etyp    : constant Entity_Id := Etype (Return_Applies_To (R_Ent));
      Dims_R    : constant Dimension_Type := Dimensions_Of (R_Etyp);
      Dims_Obj  : Dimension_Type;
      Obj_Decl  : Node_Id;
      Obj_Id    : Entity_Id;

   begin
      if Present (Obj_Decls) then
         Obj_Decl := First (Obj_Decls);
         while Present (Obj_Decl) loop
            if Nkind (Obj_Decl) = N_Object_Declaration then
               Obj_Id := Defining_Identifier (Obj_Decl);

               if Is_Return_Object (Obj_Id) then
                  Dims_Obj := Dimensions_Of (Obj_Id);

                  if Dims_R /= Dims_Obj then
                     Error_Msg_N
                       ("?dimensions missmatch in return statement", N);
                     return;
                  end if;
               end if;
            end if;

            Next (Obj_Decl);
         end loop;
      end if;
   end Analyze_Dimension_Extended_Return_Statement;

   -------------------------------------
   -- Analyze_Dimension_Function_Call --
   -------------------------------------

   procedure Analyze_Dimension_Function_Call (N : Node_Id) is
      Name_Call  : constant Node_Id := Name (N);
      Par_Ass    : constant List_Id := Parameter_Associations (N);
      Dims       : Dimension_Type;
      Dims_Param : Dimension_Type;
      Param      : Node_Id;

      function Is_Elementary_Function_Call (N : Node_Id) return Boolean;
      --  Return True if the call is a call of an elementary function (see
      --  Ada.Numerics.Generic_Elementary_Functions).

      ---------------------------------
      -- Is_Elementary_Function_Call --
      ---------------------------------

      function Is_Elementary_Function_Call (N : Node_Id) return Boolean is
         Ent : Entity_Id;

      begin
         --  Note that the node must come from source

         if Comes_From_Source (N) and then Is_Entity_Name (Name_Call) then
            Ent := Entity (Name_Call);

            --  Check the procedure is defined in an instantiation of a generic
            --  package.

            if Is_Generic_Instance (Scope (Ent)) then
               Ent := Cunit_Entity (Get_Source_Unit (Ent));

               --  Check the name of the generic package is
               --  Generic_Elementary_Functions

               if Is_Library_Level_Entity (Ent)
                 and then Chars (Ent) = Name_Generic_Elementary_Functions
               then
                  return True;
               end if;
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
            Dims := Dimensions_Of (First (Par_Ass));

            if Exists (Dims) then
               for Dim in Dims'Range loop
                  Dims (Dim) := Dims (Dim) * (1, 2);
               end loop;

               Set_Dimensions (N, Dims);
            end if;

         --  All other functions in Ada.Numerics.Generic_Elementary_Functions
         --  Note that all parameters here should be dimensionless

         else
            Param := First (Par_Ass);
            while Present (Param) loop
               Dims_Param := Dimensions_Of (Param);

               if Exists (Dims_Param) then

                  --  What is this deleted code about ???
                  --  Error_Msg_Name_1 := Chars (Name_Call);

                  Error_Msg_N
                    ("?parameter should be dimensionless for elementary "
                     & "function%", Param);
                  return;
               end if;

               Next (Param);
            end loop;
         end if;

      --  General case

      else
         Analyze_Dimension_Has_Etype (N);
      end if;
   end Analyze_Dimension_Function_Call;

   ---------------------------------
   -- Analyze_Dimension_Has_Etype --
   ---------------------------------

   procedure Analyze_Dimension_Has_Etype (N : Node_Id) is
      E_Typ  : constant Entity_Id := Etype (N);
      Dims   : constant Dimension_Type := Dimensions_Of (E_Typ);
      N_Kind : constant Node_Kind := Nkind (N);

   begin
      --  Propagation of the dimensions from the type

      if Exists (Dims) then
         Set_Dimensions (N, Dims);
      end if;

      --  Removal of dimensions in expression

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

      elsif Nkind_In
              (N_Kind,
                 N_Qualified_Expression,
                 N_Type_Conversion,
                 N_Unchecked_Type_Conversion)
      then
         Remove_Dimensions (Expression (N));

      elsif N_Kind = N_Selected_Component then
         Remove_Dimensions (Selector_Name (N));
      end if;
   end Analyze_Dimension_Has_Etype;

   ----------------------------------
   -- Analyze_Dimension_Identifier --
   ----------------------------------

   procedure Analyze_Dimension_Identifier (N : Node_Id) is
      Ent  : constant Entity_Id := Entity (N);
      Dims : constant Dimension_Type := Dimensions_Of (Ent);
   begin
      if Exists (Dims) then
         Set_Dimensions (N, Dims);
      else
         Analyze_Dimension_Has_Etype (N);
      end if;
   end Analyze_Dimension_Identifier;

   ------------------------------------------
   -- Analyze_Dimension_Object_Declaration --
   ------------------------------------------

   procedure Analyze_Dimension_Object_Declaration (N : Node_Id) is
      Expr   : constant Node_Id   := Expression (N);
      Id     : constant Entity_Id := Defining_Identifier (N);
      E_Typ  : constant Entity_Id := Etype (Id);
      Dim_T  : constant Dimension_Type := Dimensions_Of (E_Typ);
      Dim_E  : Dimension_Type;

   begin
      if Exists (Dim_T) then

         --  Expression is present

         if Present (Expr) then
            Dim_E := Dimensions_Of (Expr);

            if Exists (Dim_E) then

               --  Return an error if the dimension of the expression and the
               --  dimension of the type missmatch.

               if Dim_E /= Dim_T then
                  Error_Msg_N ("?dimensions missmatch in object " &
                               "declaration", N);
               end if;

            --  If the expression is dimensionless

            else
               --  If node is not a real or integer constant (depending on the
               --  dimensioned numeric type), generate an error message.

               if not Nkind_In (Original_Node (Expr),
                                N_Real_Literal,
                                N_Integer_Literal)
               then
                  Error_Msg_N
                    ("?dimensions missmatch in object declaration", N);
               end if;
            end if;

         --  For every other cases, propagate the dimensions

         else
            Copy_Dimensions (E_Typ, Id);
         end if;
      end if;
   end Analyze_Dimension_Object_Declaration;

   ---------------------------------------------------
   -- Analyze_Dimension_Object_Renaming_Declaration --
   ---------------------------------------------------

   procedure Analyze_Dimension_Object_Renaming_Declaration (N : Node_Id) is
      Id       : constant Entity_Id := Defining_Identifier (N);
      Ren_Id   : constant Node_Id   := Name (N);
      E_Typ    : constant Entity_Id := Etype (Ren_Id);
      Dims_Typ : constant Dimension_Type := Dimensions_Of (E_Typ);
   begin
      if Exists (Dims_Typ) then
         Copy_Dimensions (E_Typ, Id);
      end if;
   end Analyze_Dimension_Object_Renaming_Declaration;

   -----------------------------------------------
   -- Analyze_Dimension_Simple_Return_Statement --
   -----------------------------------------------

   procedure Analyze_Dimension_Simple_Return_Statement (N : Node_Id) is
      Expr      : constant Node_Id := Expression (N);
      Dims_Expr : constant Dimension_Type := Dimensions_Of (Expr);
      R_Ent     : constant Entity_Id := Return_Statement_Entity (N);
      R_Etyp    : constant Entity_Id := Etype (Return_Applies_To (R_Ent));
      Dims_R    : constant Dimension_Type := Dimensions_Of (R_Etyp);
   begin
      if Dims_R /= Dims_Expr then
         Error_Msg_N ("?dimensions missmatch in return statement", N);
         Remove_Dimensions (Expr);
      end if;
   end Analyze_Dimension_Simple_Return_Statement;

   -------------------------------------------
   -- Analyze_Dimension_Subtype_Declaration --
   -------------------------------------------

   procedure Analyze_Dimension_Subtype_Declaration (N : Node_Id) is
      Ent      : constant Entity_Id := Defining_Identifier (N);
      Dims_Ent : constant Dimension_Type := Dimensions_Of (Ent);
      E_Typ    : Node_Id;

   begin
      if Nkind (Subtype_Indication (N)) /= N_Subtype_Indication then
         E_Typ := Etype (Subtype_Indication (N));
         declare
            Dims_Typ : constant Dimension_Type := Dimensions_Of (E_Typ);

         begin
            if Exists (Dims_Typ) then

               --  If subtype already has a dimension (from Aspect_Dimension),
               --  it cannot inherit a dimension from its subtype.

               if Exists (Dims_Ent) then
                  Error_Msg_N ("?subtype& already has a dimension", N);

               else
                  Set_Dimensions (Ent, Dims_Typ);
                  Set_Symbol (Ent, Symbol_Of (E_Typ));
               end if;
            end if;
         end;

      else
         E_Typ := Etype (Subtype_Mark (Subtype_Indication (N)));
         declare
            Dims_Typ : constant Dimension_Type := Dimensions_Of (E_Typ);

         begin
            if Exists (Dims_Typ) then

               --  If subtype already has a dimension (from Aspect_Dimension),
               --  it cannot inherit a dimension from its subtype.

               if Exists (Dims_Ent) then
                  Error_Msg_N ("?subtype& already has a dimension", N);

               else
                  Set_Dimensions (Ent, Dims_Typ);
                  Set_Symbol (Ent, Symbol_Of (E_Typ));
               end if;
            end if;
         end;
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

   ---------------------
   -- Copy_Dimensions --
   ---------------------

   procedure Copy_Dimensions (From : Node_Id; To : Node_Id) is
      Dims : constant Dimension_Type := Dimensions_Of (From);

   begin
      --  Propagate the dimension from one node to another

      pragma Assert (OK_For_Dimension (Nkind (To)));
      pragma Assert (Exists (Dims));
      Set_Dimensions (To, Dims);
   end Copy_Dimensions;

   -------------------------------
   -- Create_Rational_From_Expr --
   -------------------------------

   function Create_Rational_From_Expr (Expr : Node_Id) return Rational is
      Or_N         : constant Node_Id := Original_Node (Expr);
      Left         : Node_Id;
      Left_Int     : Int;
      Ltype        : Entity_Id;
      Right        : Node_Id;
      Right_Int    : Int;
      R_Opnd_Minus : Node_Id;
      Rtype        : Entity_Id;
      Result       : Rational;

   begin
      --  A rational number is a number that can be expressed as the quotient
      --  or fraction a/b of two integers, where b is non-zero.

      --  Check the expression is either a division of two integers or an
      --  integer itself. The check applies to the original node since the
      --  node could have already been rewritten.

      --  Numerator is positive

      if Nkind (Or_N) = N_Op_Divide then
         Left  := Left_Opnd (Or_N);
         Ltype := Etype (Left);
         Right := Right_Opnd (Or_N);
         Rtype := Etype (Right);

         if Is_Integer_Type (Ltype) and then Is_Integer_Type (Rtype) then
            Left_Int  := UI_To_Int (Expr_Value (Left));
            Right_Int := UI_To_Int (Expr_Value (Right));

            --  Verify that the denominator of the rational is positive

            if Right_Int > 0 then
               if Left_Int mod Right_Int = 0 then
                  Result := +Whole (UI_To_Int (Expr_Value (Expr)));
               else
                  Result := Whole (Left_Int) / Whole (Right_Int);
               end if;

            else
               Error_Msg_N
                 ("denominator in a rational number must be positive", Right);
            end if;

         else
            Error_Msg_N ("must be a rational", Expr);
         end if;

      --  Numerator is negative

      elsif Nkind (Or_N) = N_Op_Minus
        and then Nkind (Original_Node (Right_Opnd (Or_N))) = N_Op_Divide
      then
         R_Opnd_Minus := Original_Node (Right_Opnd (Or_N));
         Left  := Left_Opnd (R_Opnd_Minus);
         Ltype := Etype (Left);
         Right := Right_Opnd (R_Opnd_Minus);
         Rtype := Etype (Right);

         if Is_Integer_Type (Ltype)
           and then Is_Integer_Type (Rtype)
         then
            Left_Int  := UI_To_Int (Expr_Value (Left));
            Right_Int := UI_To_Int (Expr_Value (Right));

            --  Verify that the denominator of the rational is positive

            if Right_Int > 0 then
               if Left_Int mod Right_Int = 0 then
                  Result := +Whole (-UI_To_Int (Expr_Value (Expr)));
               else
                  Result := Whole (-Left_Int) / Whole (Right_Int);
               end if;

            else
               Error_Msg_N
                 ("denominator in a rational number must be positive", Right);
            end if;

         else
            Error_Msg_N ("must be a rational", Expr);
         end if;

      --  Integer case

      else
         if Is_Integer_Type (Etype (Expr)) then
            Right_Int := UI_To_Int (Expr_Value (Expr));
            Result    :=  +Whole (Right_Int);

         else
            Error_Msg_N ("must be a rational", Expr);
         end if;
      end if;

      return Result;
   end Create_Rational_From_Expr;

   -------------------
   -- Dimensions_Of --
   -------------------

   function Dimensions_Of (N : Node_Id) return Dimension_Type is
   begin
      return Dimension_Table.Get (N);
   end Dimensions_Of;

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

   --  Evaluate the expon operator for dimensioned type

   --  Note that if the exponent is an integer (denominator = 1) the node is
   --  not evaluated here and must be evaluated by the Eval_Op_Expon routine.

   procedure Eval_Op_Expon_For_Dimensioned_Type
     (N : Node_Id;
      B_Typ : Entity_Id)
   is
      R   : constant Node_Id := Right_Opnd (N);
      Rat : Rational := Zero;
   begin
      if Compile_Time_Known_Value (R) and then Is_Real_Type (B_Typ) then
         Rat := Create_Rational_From_Expr (R);
         Eval_Op_Expon_With_Rational_Exponent (N, Rat);
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
     (N   : Node_Id;
      Rat : Rational)
   is
      Dims         : constant Dimension_Type := Dimensions_Of (N);
      L            : constant Node_Id := Left_Opnd (N);
      Etyp         : constant Entity_Id := Etype (L);
      Loc          : constant Source_Ptr := Sloc (N);
      Actual_1     : Node_Id;
      Actual_2     : Node_Id;
      Base_Typ     : Entity_Id;
      Dim_Value    : Rational;
      List_Of_Dims : List_Id;
      New_Aspect   : Node_Id;
      New_Aspects  : List_Id;
      New_E        : Entity_Id;
      New_N        : Node_Id;
      New_Typ_L    : Node_Id;
      System       : System_Type;

   begin
      --  If Rat.Denominator = 1 that means the exponent is an Integer so
      --  nothing has to be changed. Note that the node must come from source.

      if Comes_From_Source (N) and then Rat.Denominator /= 1 then
         Base_Typ := Base_Type (Etyp);

         --  Case when the operand is not dimensionless

         if Exists (Dims) then

            --  Get the corresponding Dim_Sys_Id to know the exact number of
            --  dimensions in the system.

            System := System_Of (Base_Typ);

            --  Step 1: Generation of a new subtype with the proper dimensions

            --  In order to rewrite the operator as a function call, a new
            --  subtype with an aspect dimension using the dimensions of the
            --  node has to be created.

            --  Generate:

            --  Base_Typ  : constant Entity_Id := Base_Type (Etyp);
            --  Sys       : constant System_Id :=
            --               Get_Dimension_System_Id (Base_Typ);
            --  N_Dims    : constant Number_Of_Dimensions :=
            --               Dimension_Systems.Table (Sys).Dimension_Count;
            --  Dim_Value : Rational;

            --  Aspect_Dim_Expr : List;

            --  Append ("", Aspect_Dim_Expr);

            --  for Dim in Dims'First .. N_Dims loop
            --     Dim_Value := Dims (Dim);

            --     if Dim_Value.Denominator /= 1 then
            --        Append (Dim_Value.Numerator / Dim_Value.Denominator,
            --                Aspect_Dim_Expr);
            --     else
            --        Append (Dim_Value.Numerator, Aspect_Dim_Expr);
            --     end if;
            --  end loop;

            --  subtype T is Base_Typ with Dimension => Aspect_Dim_Expr;

            --  Step 1a: Generate the aggregate for the new Aspect_dimension

            New_Aspects  := Empty_List;
            List_Of_Dims := New_List;

            Append (Make_String_Literal (Loc, No_String), List_Of_Dims);

            for Dim in Dims'First ..  System.Count loop
               Dim_Value := Dims (Dim);

               if Dim_Value.Denominator /= 1 then
                  Append_To (List_Of_Dims,
                     Make_Op_Divide (Loc,
                       Left_Opnd  =>
                         Make_Integer_Literal (Loc,
                           Int (Dim_Value.Numerator)),
                       Right_Opnd =>
                         Make_Integer_Literal (Loc,
                           Int (Dim_Value.Denominator))));

               else
                  Append_To (List_Of_Dims,
                    Make_Integer_Literal (Loc, Int (Dim_Value.Numerator)));
               end if;
            end loop;

            --  Step 1b: Create the new Aspect_Dimension

            New_Aspect :=
              Make_Aspect_Specification (Loc,
                Identifier => Make_Identifier (Loc, Name_Dimension),
                Expression =>
                  Make_Aggregate (Loc, Expressions => List_Of_Dims));

            --  Step 1c: New identifier for the subtype

            New_E := Make_Temporary (Loc, 'T');
            Set_Is_Internal (New_E);

            --  Step 1d: Declaration of the new subtype

            New_Typ_L :=
               Make_Subtype_Declaration (Loc,
                  Defining_Identifier => New_E,
                  Subtype_Indication  => New_Occurrence_Of (Base_Typ, Loc));

            Append (New_Aspect, New_Aspects);
            Set_Parent (New_Aspects, New_Typ_L);
            Set_Aspect_Specifications (New_Typ_L, New_Aspects);

            Analyze (New_Typ_L);

         --  Case where the operand is dimensionless

         else
            New_E := Base_Typ;
         end if;

         --  Step 2: Generation of the function call

         --  Generate:

         --  Actual_1 := Long_Long_Float (L),

         --  Actual_2 := Long_Long_Float (Rat.Numerator) /
         --                Long_Long_Float (Rat.Denominator);

         --  (T (Expon_LLF (Actual_1, Actual_2)));

         --  --  where T is the subtype declared in step 1

         --  -- The node is rewritten as a type conversion

         --  Step 2a: Creation of the two parameters for function Expon_LLF

         Actual_1 :=
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Reference_To (Standard_Long_Long_Float, Loc),
             Expression   => Relocate_Node (L));

         Actual_2 :=
           Make_Op_Divide (Loc,
             Left_Opnd  =>
               Make_Real_Literal (Loc,
                 UR_From_Uint (UI_From_Int (Int (Rat.Numerator)))),
             Right_Opnd =>
               Make_Real_Literal (Loc,
                 UR_From_Uint (UI_From_Int (Int (Rat.Denominator)))));

         --  Step 2b: New Node N

         New_N :=
            Make_Type_Conversion (Loc,
              Subtype_Mark => New_Reference_To (New_E, Loc),
              Expression   =>
                Make_Function_Call (Loc,
                  Name => New_Reference_To (RTE (RE_Expon_LLF), Loc),
                  Parameter_Associations => New_List (
                    Actual_1, Actual_2)));

         --  Step 3: Rewitten of N

         Rewrite (N, New_N);
         Set_Etype (N, New_E);
         Analyze_And_Resolve (N, New_E);
      end if;
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
   -- Expand_Put_Call_With_Dimension_String --
   -------------------------------------------

   --  For procedure Put defined in System.Dim_Float_IO/System.Dim_Integer_IO,
   --  the default string parameter must be rewritten to include the dimension
   --  symbols in the output of a dimensioned object.

   --  There are two different cases:

   --  1) If the parameter is a variable, the default string parameter is
   --  replaced by the string defined in the aspect Dimension of the subtype.
   --  For instance if the user wants to output a speed:

   --  subtype Speed is Mks_Type with Dimension =>
   --    ("speed", Meter => 1, Second => -1, others => 0);
   --  v : Speed := 2.1 * m * s**(-1);

   --  Put (v) returns:
   --  > 2.1 speed

   --  2) If the parameter is an expression, then we call the procedure
   --  Expand_Put_Call_With_Dimension_String creates the string (for instance
   --  "m.s**(-1)") and rewrite the default string parameter of Put with the
   --  corresponding the String_Id.

   procedure Expand_Put_Call_With_Dimension_String (N : Node_Id) is
      Actuals      : constant List_Id := Parameter_Associations (N);
      Loc          : constant Source_Ptr := Sloc (N);
      Name_Call    : constant Node_Id := Name (N);
      Actual       : Node_Id;
      Base_Typ     : Node_Id;
      Char_Pack    : Name_Id;
      Dims         : Dimension_Type;
      Etyp         : Entity_Id;
      First_Actual : Node_Id;
      New_Par_Ass  : List_Id;
      New_Str_Lit  : Node_Id;
      System       : System_Type;

      function Is_Procedure_Put_Call (N : Node_Id) return Boolean;
      --  Return True if the current call is a call of an instantiation of a
      --  procedure Put defined in the package System.Dim_Float_IO and
      --  System.Dim_Integer_IO.

      function Is_Procedure_Put_Call (N : Node_Id) return Boolean is
         Name_Call : constant Node_Id := Name (N);
         Ent       : Entity_Id;

      begin
         --  There are three different Put routine in each generic package
         --  Check that the current procedure call is one of them

         if Is_Entity_Name (Name_Call) then
            Ent := Entity (Name_Call);

            --  Check that the name of the procedure is Put

            if Chars (Name_Call) /= Name_Put then
               return False;
            end if;

            --  Check the procedure is defined in an instantiation of a
            --  generic package.

            if Is_Generic_Instance (Scope (Ent)) then
               Ent := Cunit_Entity (Get_Source_Unit (Ent));

               --  Verify that the generic package is System.Dim_Float_IO or
               --  System.Dim_Integer_IO.

               if Is_Library_Level_Entity (Ent) then
                  Char_Pack := Chars (Ent);

                  if Char_Pack = Name_Dim_Float_IO
                    or else Char_Pack = Name_Dim_Integer_IO
                  then
                     return True;
                  end if;
               end if;
            end if;
         end if;

         return False;
      end Is_Procedure_Put_Call;

   --  Start of processing for Expand_Put_Call_With_Dimension_String

   begin
      if Is_Procedure_Put_Call (N) then

         --  Get the first parameter

         First_Actual := First (Actuals);

         --  Case when the Put routine has four (integer case) or five (float
         --  case) parameters.

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

         if Exists (System) then
            Dims := Dimensions_Of (Actual);
            Etyp := Etype (Actual);

            --  Add the string as a suffix of the value if the subtype has a
            --  string of dimensions or if the parameter is not dimensionless.

            if Exists (Dims)
              or else Symbol_Of (Etyp) /= No_String
            then
               New_Par_Ass := New_List;

               --  Add to the list First_Actual and Actual if they differ

               if Actual /= First_Actual then
                  Append (New_Copy (First_Actual), New_Par_Ass);
               end if;

               Append (New_Copy (Actual), New_Par_Ass);

               --  Look to the next parameter

               Next (Actual);

               --  Check if the type of N is a subtype that has a string of
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
                      From_Dimension_To_String_Id (Dims, System));
               end if;

               Append (New_Str_Lit, New_Par_Ass);

               --  Rewrite the procedure call with the new list of parameters

               Rewrite (N,
                 Make_Procedure_Call_Statement (Loc,
                   Name                   => New_Copy (Name_Call),
                   Parameter_Associations => New_Par_Ass));

               Analyze (N);
            end if;
         end if;
      end if;
   end Expand_Put_Call_With_Dimension_String;

   ---------------------------------
   -- From_Dimension_To_String_Id --
   ---------------------------------

   --  Given a dimension vector and the corresponding dimension system, create
   --  a String_Id to output the dimension symbols corresponding to the
   --  dimensions Dims.

   function From_Dimension_To_String_Id
     (Dims   : Dimension_Type;
      System : System_Type) return String_Id
   is
      Dim_Rat          : Rational;
      First_Dim_In_Str : Boolean := True;

   begin
      --  Initialization of the new String_Id

      Start_String;

      --  Put a space between the value and the dimensions

      Store_String_Char (' ');

      for Dim in Dimension_Type'Range loop
         Dim_Rat := Dims (Dim);
         if Dim_Rat /= Zero then

            if First_Dim_In_Str then
               First_Dim_In_Str := False;
            else
               Store_String_Char ('.');
            end if;

            --  Positive dimension case

            if Dim_Rat.Numerator > 0 then
               if System.Symbols (Dim) = No_String then
                  Store_String_Chars (Get_Name_String (System.Names (Dim)));
               else
                  Store_String_Chars (System.Symbols (Dim));
               end if;

               --  Integer case

               if Dim_Rat.Denominator = 1 then
                  if Dim_Rat.Numerator /= 1 then
                     Store_String_Chars ("**");
                     Store_String_Int (Int (Dim_Rat.Numerator));
                  end if;

               --  Rational case when denominator /= 1

               else
                  Store_String_Chars ("**");
                  Store_String_Char ('(');
                  Store_String_Int (Int (Dim_Rat.Numerator));
                  Store_String_Char ('/');
                  Store_String_Int (Int (Dim_Rat.Denominator));
                  Store_String_Char (')');
               end if;

            --  Negative dimension case

            else
               if System.Symbols (Dim) = No_String then
                  Store_String_Chars (Get_Name_String (System.Names (Dim)));
               else
                  Store_String_Chars (System.Symbols (Dim));
               end if;

               Store_String_Chars ("**");
               Store_String_Char ('(');
               Store_String_Char ('-');
               Store_String_Int (Int (-Dim_Rat.Numerator));

               --  Integer case

               if Dim_Rat.Denominator = 1 then
                  Store_String_Char (')');

               --  Rational case when denominator /= 1

               else
                  Store_String_Char ('/');
                  Store_String_Int (Int (Dim_Rat.Denominator));
                  Store_String_Char (')');
               end if;
            end if;
         end if;
      end loop;

      return End_String;
   end From_Dimension_To_String_Id;

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
      Dims : constant Dimension_Type := Dimensions_Of (From);

   begin
      --  Copy the dimension of 'From to 'To' and remove dimension of 'From'

      if Exists (Dims) then
         Set_Dimensions (To, Dims);
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
         return Rational'(Numerator   => Whole (Int (X.Numerator) / G),
                          Denominator => Whole (Int (X.Denominator) / G));
      end;
   end Reduce;

   -----------------------
   -- Remove_Dimensions --
   -----------------------

   procedure Remove_Dimensions (N : Node_Id) is
      Dims : constant Dimension_Type := Dimensions_Of (N);
   begin
      if Exists (Dims) then
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

   -------------------------------------
   -- Remove_Dimension_In_Declaration --
   -------------------------------------

   --  Removal of dimension in expressions of N_Object_Declaration and
   --  N_Component_Declaration as part of the Analyze_Declarations routine
   --  (see package Sem_Ch3).

   procedure Remove_Dimension_In_Declaration (Decl : Node_Id) is
   begin
      if Ada_Version >= Ada_2012
        and then Nkind_In (Decl, N_Object_Declaration, N_Component_Declaration)
        and then Present (Expression (Decl))
      then
         Remove_Dimensions (Expression (Decl));
      end if;
   end Remove_Dimension_In_Declaration;

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
      --  Scan the Table in order to find N
      --  What is N??? no sign of anything called N here ???

      for Dim_Sys in 1 .. System_Table.Last loop
         if Type_Decl = System_Table.Table (Dim_Sys).Type_Decl then
            return System_Table.Table (Dim_Sys);
         end if;
      end loop;

      return Null_System;
   end System_Of;

end Sem_Dim;
