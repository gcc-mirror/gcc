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
with Namet.Sp; use Namet.Sp;
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

   Max_Dimensions : constant Int := 7;
   --  Maximum number of dimensions in a dimension system

   subtype Dim_Id is Pos range 1 .. Max_Dimensions;
   --  Dim_Id values are used to identify dimensions in a dimension system
   --  Note that the highest value of Dim_Id is Max_Dimensions

   --  Record type for dimension system

   --  A dimension system is defined by the number and the names of its
   --  dimensions and its base type.

   subtype N_Of_Dimensions is Int range 0 .. Max_Dimensions;

   No_Dimensions : constant N_Of_Dimensions := N_Of_Dimensions'First;

   type Name_Array is array (Dim_Id) of Name_Id;

   No_Names : constant Name_Array := (others => No_Name);

   --  The symbols are used for IO purposes

   type Symbol_Array is array (Dim_Id) of String_Id;

   No_Symbols : constant Symbol_Array := (others => No_String);

   type Dimension_System is record
      Base_Type : Node_Id;
      Names     : Name_Array;
      N_Of_Dims : N_Of_Dimensions;
      Symbols   : Symbol_Array;
   end record;

   No_Dimension_System : constant Dimension_System :=
                           (Empty, No_Names, No_Dimensions, No_Symbols);

   --  Dim_Sys_Id values are used to identify dimension system in the Table
   --  Note that the special value No_Dim_Sys has no corresponding component in
   --  the Table since it represents no dimension system.

   subtype Dim_Sys_Id is Nat;

   No_Dim_Sys : constant Dim_Sys_Id := Dim_Sys_Id'First;

   --  The following table records every dimension system

   package Dim_Systems is new Table.Table (
     Table_Component_Type => Dimension_System,
     Table_Index_Type     => Dim_Sys_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 5,
     Table_Increment      => 5,
     Table_Name           => "Dim_Systems");

   --  Rational (definitions & operations)

   type Whole is new Int;
   subtype Positive_Whole is Whole range 1 .. Whole'Last;

   type Rational is record
      Numerator   : Whole;
      Denominator : Positive_Whole;
   end record;

   Zero_Rational : constant Rational := (0, 1);

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

   ------------
   -- Reduce --
   ------------

   function Reduce (X : Rational) return Rational is
   begin
      if X.Numerator = 0 then
         return Zero_Rational;
      end if;

      declare
         G : constant Int := GCD (X.Numerator, X.Denominator);

      begin
         return Rational'(Numerator   => Whole (Int (X.Numerator) / G),
                          Denominator => Whole (Int (X.Denominator) / G));
      end;
   end Reduce;

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

   --  Hash Table for aspect dimension.

   --  The following table provides a relation between nodes and its dimension
   --  (if not dimensionless). If a node is not stored in the Hash Table, the
   --  node is considered to be dimensionless.

   --  A dimension is represented by an array of Max_Dimensions Rationals.
   --  If the corresponding dimension system has less than Max_Dimensions
   --  dimensions, the array is filled by as many as Zero_Rationals needed to
   --  complete the array.

   --  Here is a list of nodes that can have entries in this Htable:

   --  N_Attribute_Reference
   --  N_Defining_Identifier
   --  N_Function_Call
   --  N_Identifier
   --  N_Indexed_Component
   --  N_Integer_Literal
   --  N_Op_Abs
   --  N_Op_Add
   --  N_Op_Divide
   --  N_Op_Expon
   --  N_Op_Minus
   --  N_Op_Mod
   --  N_Op_Multiply
   --  N_Op_Plus
   --  N_Op_Rem
   --  N_Op_Subtract
   --  N_Qualified_Expression
   --  N_Real_Literal
   --  N_Selected_Component
   --  N_Slice
   --  N_Type_Conversion
   --  N_Unchecked_Type_Conversion

   type Dimensions is array (Dim_Id) of Rational;

   Zero_Dimensions : constant Dimensions := (others => Zero_Rational);

   type AD_Hash_Range is range 0 .. 511;

   function AD_Hash (F : Node_Id) return AD_Hash_Range;

   -------------
   -- AD_Hash --
   -------------

   function AD_Hash (F : Node_Id) return AD_Hash_Range is
   begin
      return AD_Hash_Range (F mod 512);
   end AD_Hash;

   --  Node_Id --> Dimensions

   package Aspect_Dimension_Hash_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => AD_Hash_Range,
        Element    => Dimensions,
        No_Element => Zero_Dimensions,
        Key        => Node_Id,
        Hash       => AD_Hash,
        Equal      => "=");

   --  Table to record the string of each subtype declaration
   --  Note that this table is only used for IO purposes

   --  Entity_Id --> String_Id

   package Aspect_Dimension_String_Id_Hash_Table is new
     GNAT.HTable.Simple_HTable
       (Header_Num => AD_Hash_Range,
        Element    => String_Id,
        No_Element => No_String,
        Key        => Entity_Id,
        Hash       => AD_Hash,
        Equal      => "=");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Dimension_Assignment_Statement (N : Node_Id);
   --  Subroutine of Analyze_Dimension for assignment statement

   procedure Analyze_Dimension_Binary_Op (N : Node_Id);
   --  Subroutine of Analyze_Dimension for binary operators

   procedure Analyze_Dimension_Component_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for component declaration

   procedure Analyze_Dimension_Extended_Return_Statement (N : Node_Id);
   --  Subroutine of Analyze_Dimension for extended return statement

   procedure Analyze_Dimension_Function_Call (N : Node_Id);
   --  Subroutine of Analyze_Dimension for function call

   procedure Analyze_Dimension_Has_Etype (N : Node_Id);
   --  Subroutine of Analyze_Dimension for N_Has_Etype nodes:
   --  N_Attribute_Reference
   --  N_Indexed_Component
   --  N_Qualified_Expression
   --  N_Selected_Component
   --  N_Slice
   --  N_Type_Conversion
   --  N_Unchecked_Type_Conversion

   procedure Analyze_Dimension_Identifier (N : Node_Id);
   --  Subroutine of Analyze_Dimension for identifier

   procedure Analyze_Dimension_Object_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for object declaration

   procedure Analyze_Dimension_Object_Renaming_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for object renaming declaration

   procedure Analyze_Dimension_Simple_Return_Statement (N : Node_Id);
   --  Subroutine of Analyze_Dimension for simple return statement

   procedure Analyze_Dimension_Subtype_Declaration (N : Node_Id);
   --  Subroutine of Analyze_Dimension for subtype declaration

   procedure Analyze_Dimension_Unary_Op (N : Node_Id);
   --  Subroutine of Analyze_Dimension for unary operators

   procedure Copy_Dimensions (From, To : Node_Id);
   --  Propagate dimensions between two nodes

   procedure Create_Rational_From_Expr (Expr : Node_Id; R : in out Rational);
   --  Given an expression, creates a rational number

   procedure Eval_Op_Expon_With_Rational_Exponent
     (N   : Node_Id;
      Rat : Rational);
   --  Evaluate the Expon if the exponent is a rational and the operand has a
   --  dimension.

   function From_Dimension_To_String_Id
     (Dims : Dimensions;
      Sys  : Dim_Sys_Id) return String_Id;
   --  Given a dimension vector and a dimension system, return the proper
   --  string of symbols.

   function Get_Dimensions (N : Node_Id) return Dimensions;
   --  Return the dimensions for the corresponding node

   function Get_Dimensions_String_Id (E : Entity_Id) return String_Id;
   --  Return the String_Id of dimensions for the corresponding entity

   function Get_Dimension_System_Id (E : Entity_Id) return Dim_Sys_Id;
   --  Return the Dim_Id of the corresponding dimension system

   procedure Move_Dimensions (From, To : Node_Id);
   --  Move Dimensions from 'From' to 'To'. Only called when 'From' has a
   --  dimension.

   function Permits_Dimensions (N : Node_Id) return Boolean;
   --  Return True if a node can have a dimension

   function Present (Dim : Dimensions) return Boolean;
   --  Return True if Dim is not equal to Zero_Dimensions.

   procedure Remove_Dimensions (N : Node_Id);
   --  Remove the node from the HTable

   procedure Set_Dimensions (N : Node_Id; Dims : Dimensions);
   --  Store the dimensions of N in the Hash_Table for Dimensions

   procedure Set_Dimensions_String_Id (E : Entity_Id; Str : String_Id);
   --  Store the string of dimensions of E in the Hash_Table for String_Id

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
      Expr : Node_Id)
   is
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      N_Kind : constant Node_Kind := Nkind (N);

      Analyzed : array (Dimensions'Range) of Boolean := (others => False);
      --  This array has been defined in order to deals with Others_Choice
      --  It is a reminder of the dimensions in the aggregate that have already
      --  been analyzed.

      Choice      : Node_Id;
      Comp_Expr   : Node_Id;
      Comp_Assn   : Node_Id;
      Dim         : Dim_Id;
      Dims        : Dimensions := Zero_Dimensions;
      Dim_Str_Lit : Node_Id;
      D_Sys       : Dim_Sys_Id := No_Dim_Sys;
      N_Of_Dims   : N_Of_Dimensions;
      Str         : String_Id := No_String;

      function Check_Identifier_Is_Dimension
        (Id    : Node_Id;
         D_Sys : Dim_Sys_Id) return Boolean;
      --  Return True if the identifier name is the name of a dimension in the
      --  dimension system D_Sys.

      function Check_Compile_Time_Known_Expressions_In_Aggregate
        (Expr : Node_Id) return Boolean;
      --  Check that each expression in the aggregate is known at compile time

      function Check_Number_Dimensions_Aggregate
        (Expr      : Node_Id;
         D_Sys     : Dim_Sys_Id;
         N_Of_Dims : N_Of_Dimensions) return Boolean;
      --  This routine checks the number of dimensions in the aggregate.

      function Corresponding_Dimension_System (N : Node_Id) return Dim_Sys_Id;
      --  Return the Dim_Sys_Id of the corresponding dimension system

      function Corresponding_Etype_Has_Dimensions (N : Node_Id) return Boolean;
      --  Return True if the Etype of N has a dimension

      function Get_Dimension_Id
        (Id    : Node_Id;
         D_Sys : Dim_Sys_Id) return Dim_Id;
      --  Given an identifier and the Dim_Sys_Id of the dimension system in the
      --  Table, returns the Dim_Id that has the same name as the identifier.

      ------------------------------------
      -- Corresponding_Dimension_System --
      ------------------------------------

      function Corresponding_Dimension_System
        (N : Node_Id) return Dim_Sys_Id
      is
         B_Typ   : Node_Id;
         Sub_Ind : Node_Id;

      begin
         --  Aspect_Dimension can only apply for subtypes

         --  Look for the dimension system corresponding to this
         --  Aspect_Dimension.

         if Nkind (N) = N_Subtype_Declaration then
            Sub_Ind := Subtype_Indication (N);

            if Nkind (Sub_Ind) /= N_Subtype_Indication then
               B_Typ := Etype (Sub_Ind);
               return Get_Dimension_System_Id (B_Typ);
            else
               return No_Dim_Sys;
            end if;

         else
            return No_Dim_Sys;
         end if;
      end Corresponding_Dimension_System;

      ----------------------------------------
      -- Corresponding_Etype_Has_Dimensions --
      ----------------------------------------

      function Corresponding_Etype_Has_Dimensions
        (N : Node_Id) return Boolean
      is
         Dims_Typ : Dimensions;
         Typ      : Entity_Id;

      begin
         --  Check the type is dimensionless before assigning a dimension

         if Nkind (N) = N_Subtype_Declaration then
            declare
               Sub : constant Node_Id := Subtype_Indication (N);

            begin
               if Nkind (Sub) /= N_Subtype_Indication then
                  Typ := Etype (Sub);
               else
                  Typ := Etype (Subtype_Mark (Sub));
               end if;

               Dims_Typ := Get_Dimensions (Typ);
               return Present (Dims_Typ);
            end;

         else
            return False;
         end if;
      end Corresponding_Etype_Has_Dimensions;

      ---------------------------------------
      -- Check_Number_Dimensions_Aggregate --
      ---------------------------------------

      function Check_Number_Dimensions_Aggregate
        (Expr      : Node_Id;
         D_Sys     : Dim_Sys_Id;
         N_Of_Dims : N_Of_Dimensions) return Boolean
      is
         Assoc       : Node_Id;
         Choice      : Node_Id;
         Comp_Expr   : Node_Id;
         N_Dims_Aggr : Int := No_Dimensions;
         --  The number of dimensions in this aggregate

      begin
         --  Check the size of the aggregate match with the size of the
         --  corresponding dimension system.

         Comp_Expr := First (Expressions (Expr));

         --  Skip the first argument in the aggregate since it's a character or
         --  a string and not a dimension value.

         Next (Comp_Expr);

         if Present (Component_Associations (Expr)) then

            --  For a positional aggregate with an Others_Choice, the number
            --  of expressions must be less than or equal to N_Of_Dims - 1.

            if Present (Comp_Expr) then
               N_Dims_Aggr := List_Length (Expressions (Expr)) - 1;
               return N_Dims_Aggr <= N_Of_Dims - 1;

            --  If the aggregate is a named aggregate, N_Dims_Aggr is used to
            --  count all the dimensions referenced by the aggregate.

            else
               Assoc := First (Component_Associations (Expr));

               while Present (Assoc) loop
                  if Nkind (Assoc) = N_Range then
                     Choice := First (Choices (Assoc));

                     declare
                        HB     : constant Node_Id := High_Bound (Choice);
                        LB     : constant Node_Id := Low_Bound (Choice);
                        LB_Dim : Dim_Id;
                        HB_Dim : Dim_Id;

                     begin
                        if not Check_Identifier_Is_Dimension (HB, D_Sys)
                          or else not Check_Identifier_Is_Dimension (LB, D_Sys)
                        then
                           return False;
                        end if;

                        HB_Dim := Get_Dimension_Id (HB, D_Sys);
                        LB_Dim := Get_Dimension_Id (LB, D_Sys);

                        N_Dims_Aggr := N_Dims_Aggr + HB_Dim - LB_Dim +  1;
                     end;

                  else
                     N_Dims_Aggr :=
                       N_Dims_Aggr + List_Length (Choices (Assoc));
                  end if;

                  Next (Assoc);
               end loop;

               --  Check whether an Others_Choice is present or not

               if Nkind
                    (First (Choices (Last (Component_Associations (Expr))))) =
                     N_Others_Choice
               then
                  return N_Dims_Aggr <= N_Of_Dims;
               else
                  return N_Dims_Aggr = N_Of_Dims;
               end if;
            end if;

         --  If the aggregate is a positional aggregate without Others_Choice,
         --  the number of expressions must match the number of dimensions in
         --  the dimension system.

         else
            N_Dims_Aggr := List_Length (Expressions (Expr)) - 1;
            return N_Dims_Aggr = N_Of_Dims;
         end if;
      end Check_Number_Dimensions_Aggregate;

      -----------------------------------
      -- Check_Identifier_Is_Dimension --
      -----------------------------------

      function Check_Identifier_Is_Dimension
        (Id    : Node_Id;
         D_Sys : Dim_Sys_Id) return Boolean
      is
         Na_Id     : constant Name_Id := Chars (Id);
         Dim_Name1 : Name_Id;
         Dim_Name2 : Name_Id;

      begin

         for Dim1 in Dim_Id'Range loop
            Dim_Name1 := Dim_Systems.Table (D_Sys).Names (Dim1);

            if Dim_Name1 = Na_Id then
               return True;
            end if;

            if Dim1 = Max_Dimensions then

               --  Check for possible misspelling

               Error_Msg_N ("& is not a dimension argument for aspect%", Id);

               for Dim2 in Dim_Id'Range loop
                  Dim_Name2 := Dim_Systems.Table (D_Sys).Names (Dim2);

                  if Is_Bad_Spelling_Of (Na_Id, Dim_Name2) then
                     Error_Msg_Name_1 := Dim_Name2;
                     Error_Msg_N ("\possible misspelling of%", Id);
                     exit;
                  end if;
               end loop;
            end if;
         end loop;

         return False;
      end Check_Identifier_Is_Dimension;

      ----------------------
      -- Get_Dimension_Id --
      ----------------------

      --  Given an identifier, returns the correponding position of the
      --  dimension in the dimension system.

      function Get_Dimension_Id
        (Id    : Node_Id;
         D_Sys : Dim_Sys_Id) return Dim_Id
      is
         Na_Id    : constant Name_Id := Chars (Id);
         Dim      : Dim_Id;
         Dim_Name : Name_Id;

      begin
         for D in Dim_Id'Range loop
            Dim_Name := Dim_Systems.Table (D_Sys).Names (D);

            if Dim_Name = Na_Id then
               Dim := D;
            end if;
         end loop;

         return Dim;
      end Get_Dimension_Id;

      -------------------------------------------------------
      -- Check_Compile_Time_Known_Expressions_In_Aggregate --
      -------------------------------------------------------

      function Check_Compile_Time_Known_Expressions_In_Aggregate
        (Expr : Node_Id) return Boolean
      is
         Comp_Assn : Node_Id;
         Comp_Expr : Node_Id;

      begin

         Comp_Expr := Next (First (Expressions (Expr)));
         while Present (Comp_Expr) loop

            --  First, analyze the expression

            Analyze_And_Resolve (Comp_Expr);

            if not Compile_Time_Known_Value (Comp_Expr) then
               return False;
            end if;

            Next (Comp_Expr);
         end loop;

         Comp_Assn := First (Component_Associations (Expr));
         while Present (Comp_Assn) loop
            Comp_Expr := Expression (Comp_Assn);

            --  First, analyze the expression

            Analyze_And_Resolve (Comp_Expr);

            if not Compile_Time_Known_Value (Comp_Expr) then
               return False;
            end if;

            Next (Comp_Assn);
         end loop;

         return True;
      end Check_Compile_Time_Known_Expressions_In_Aggregate;

   --  Start of processing for Analyze_Aspect_Dimension

   begin
      --  Syntax checking

      Error_Msg_Name_1 := Chars (Id);

      if N_Kind /= N_Subtype_Declaration then
         Error_Msg_N ("aspect% doesn't apply here", N);
         return;
      end if;

      if Nkind (Expr) /= N_Aggregate then
         Error_Msg_N ("wrong syntax for aspect%", Expr);
         return;
      end if;

      D_Sys := Corresponding_Dimension_System (N);

      if D_Sys = No_Dim_Sys then
         Error_Msg_N ("dimension system not found for aspect%", N);
         return;
      end if;

      if Corresponding_Etype_Has_Dimensions (N) then
         Error_Msg_N ("corresponding type already has a dimension", N);
         return;
      end if;

      --  Check the first expression is a string or a character literal and
      --  skip it.

      Dim_Str_Lit := First (Expressions (Expr));

      if not Present (Dim_Str_Lit)
        or else not Nkind_In (Dim_Str_Lit,
                              N_String_Literal,
                              N_Character_Literal)
      then
         Error_Msg_N
           ("wrong syntax for aspect%: first argument in the aggregate must " &
            "be a character or a string",
            Expr);
         return;
      end if;

      Comp_Expr := Next (Dim_Str_Lit);

      --  Check the number of dimensions match with the dimension system

      N_Of_Dims := Dim_Systems.Table (D_Sys).N_Of_Dims;

      if not Check_Number_Dimensions_Aggregate (Expr, D_Sys, N_Of_Dims) then
         Error_Msg_N ("wrong number of dimensions for aspect%", Expr);
         return;
      end if;

      Dim := Dim_Id'First;
      Comp_Assn := First (Component_Associations (Expr));

      if Present (Comp_Expr) then
         if List_Length (Component_Associations (Expr)) > 1 then
            Error_Msg_N ("named association cannot follow " &
                         "positional association for aspect%", Expr);
            return;
         end if;

         if Present (Comp_Assn)
           and then Nkind (First (Choices (Comp_Assn))) /= N_Others_Choice
         then
            Error_Msg_N ("named association cannot follow " &
                         "positional association for aspect%", Expr);
            return;
         end if;
      end if;

      --  Check each expression in the aspect Dimension aggregate is known at
      --  compile time.

      if not Check_Compile_Time_Known_Expressions_In_Aggregate (Expr) then
         Error_Msg_N ("wrong syntax for aspect%", Expr);
         return;
      end if;

      --  Get the dimension values and store them in the Hash_Table

      --  Positional aggregate case

      while Present (Comp_Expr) loop
         if Is_Integer_Type (Def_Id) then
            Dims (Dim) := +Whole (UI_To_Int (Expr_Value (Comp_Expr)));
         else
            Create_Rational_From_Expr (Comp_Expr, Dims (Dim));
         end if;

         Analyzed (Dim) := True;

         exit when Dim = Max_Dimensions;

         Dim := Dim + 1;
         Next (Comp_Expr);
      end loop;

      --  Named aggregate case

      while Present (Comp_Assn) loop
         Comp_Expr := Expression (Comp_Assn);
         Choice := First (Choices (Comp_Assn));

         if List_Length (Choices (Comp_Assn)) = 1 then

            --  N_Identifier case

            if Nkind (Choice) = N_Identifier then

               if not Check_Identifier_Is_Dimension (Choice, D_Sys) then
                  return;
               end if;

               Dim := Get_Dimension_Id (Choice, D_Sys);

               if Is_Integer_Type (Def_Id) then
                  Dims (Dim) := +Whole (UI_To_Int (Expr_Value (Comp_Expr)));
               else
                  Create_Rational_From_Expr (Comp_Expr, Dims (Dim));
               end if;

               Analyzed (Dim) := True;

            --  N_Range case

            elsif Nkind (Choice) = N_Range then
               declare
                  HB     : constant Node_Id := High_Bound (Choice);
                  LB     : constant Node_Id := Low_Bound (Choice);
                  LB_Dim : constant Dim_Id  := Get_Dimension_Id (LB, D_Sys);
                  HB_Dim : constant Dim_Id  := Get_Dimension_Id (HB, D_Sys);

               begin
                  for Dim in LB_Dim .. HB_Dim loop
                     if Is_Integer_Type (Def_Id) then
                        Dims (Dim) :=
                          +Whole (UI_To_Int (Expr_Value (Comp_Expr)));
                     else
                        Create_Rational_From_Expr (Comp_Expr, Dims (Dim));
                     end if;

                     Analyzed (Dim) := True;
                  end loop;
               end;

            --  N_Others_Choice case

            elsif Nkind (Choice) = N_Others_Choice then

               --  Check the Others_Choice is alone and last in the aggregate

               if Present (Next (Comp_Assn)) then
                  Error_Msg_N
                    ("OTHERS must appear alone and last in expression " &
                     "for aspect%", Choice);
                  return;
               end if;

               --  End the filling of Dims by the Others_Choice value. If
               --  N_Of_Dims < Max_Dimensions then only the positions that
               --  haven't been already analyzed from Dim_Id'First to N_Of_Dims
               --  are filled.

               for Dim in Dim_Id'First .. N_Of_Dims loop
                  if not Analyzed (Dim) then
                     if Is_Integer_Type (Def_Id) then
                        Dims (Dim) :=
                          +Whole (UI_To_Int (Expr_Value (Comp_Expr)));
                     else
                        Create_Rational_From_Expr (Comp_Expr, Dims (Dim));
                     end if;
                  end if;
               end loop;

            else
               Error_Msg_N ("wrong syntax for aspect%", Id);
            end if;

         else
            while Present (Choice) loop
               if Nkind (Choice) = N_Identifier then

                  if not Check_Identifier_Is_Dimension (Choice, D_Sys) then
                     return;
                  end if;

                  Dim := Get_Dimension_Id (Choice, D_Sys);

                  if Is_Integer_Type (Def_Id) then
                     Dims (Dim) := +Whole (UI_To_Int (Expr_Value (Comp_Expr)));
                  else
                     Create_Rational_From_Expr (Comp_Expr, Dims (Dim));
                  end if;

                  Analyzed (Dim) := True;
                  Next (Choice);
               else
                  Error_Msg_N ("wrong syntax for aspect%", Id);
               end if;
            end loop;
         end if;

         Next (Comp_Assn);
      end loop;

      --  Create the string of dimensions

      if Nkind (Dim_Str_Lit) = N_Character_Literal then
         Start_String;
         Store_String_Char (UI_To_CC (Char_Literal_Value (Dim_Str_Lit)));
         Str := End_String;
      else
         Str := Strval (Dim_Str_Lit);
      end if;

      --  Store the dimensions in the Hash Table if not all equal to zero and
      --  string is empty.

      if not Present (Dims) then
         if String_Length (Str) = 0 then
            Error_Msg_N
              ("?dimension values all equal to zero for aspect%", Expr);
            return;
         end if;
      else
         Set_Dimensions (Def_Id, Dims);
      end if;

      --  Store the string in the Hash Table
      --  When the string is empty, don't store the string in the Hash Table

      if Str /= No_String
        and then String_Length (Str) /= 0
      then
         Set_Dimensions_String_Id (Def_Id, Str);
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
      D_Sys      : Dimension_System := No_Dimension_System;
      Names      : Name_Array       := No_Names;
      N_Of_Dims  : N_Of_Dimensions;
      Symbols    : Symbol_Array     := No_Symbols;

      function Derived_From_Numeric_Type (N : Node_Id) return Boolean;
      --  Return True if the node is a derived type declaration from any
      --  numeric type.

      function Check_Dimension_System_Syntax (N : Node_Id) return Boolean;
      --  Return True if the expression is an aggregate of names

      function Check_Number_Of_Dimensions (Expr : Node_Id) return Boolean;
      --  Return True if the number of dimensions in the corresponding
      --  dimension is positive and lower than Max_Dimensions.

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
         if List_Length (List_Expr) < Dim_Id'First
           or else List_Length (List_Expr) > Max_Dimensions
         then
            return False;
         else
            return True;
         end if;
      end Check_Number_Of_Dimensions;

   --  Start of processing for Analyze_Aspect_Dimension_System

   begin
      Error_Msg_Name_1 := Chars (Id);

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

      D_Sys.Base_Type := N;
      Dim_Node := First (Expressions (Expr));

      for Dim in Dim_Id'First .. N_Of_Dims loop
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

      D_Sys.Names     := Names;
      D_Sys.N_Of_Dims := N_Of_Dims;
      D_Sys.Symbols   := Symbols;

      --  Store the dimension system in the Table

      Dim_Systems.Append (D_Sys);
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
      Dim_Lhs : constant Dimensions := Get_Dimensions (Lhs);
      Rhs     : constant Node_Id    := Expression (N);
      Dim_Rhs : constant Dimensions := Get_Dimensions (Rhs);

      procedure Analyze_Dimensions_In_Assignment
        (Dim_Lhs : Dimensions;
         Dim_Rhs : Dimensions);
      --  Subroutine to perform the dimensionnality checking for assignment

      --------------------------------------
      -- Analyze_Dimensions_In_Assignment --
      --------------------------------------

      procedure Analyze_Dimensions_In_Assignment
        (Dim_Lhs : Dimensions;
         Dim_Rhs : Dimensions)
      is
      begin
         --  Check the lhs and the rhs have the same dimension

         if not Present (Dim_Lhs) then
            if Present (Dim_Rhs) then
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
            L_Dims            : constant Dimensions := Get_Dimensions (L);
            L_Has_Dimensions  : constant Boolean := Present (L_Dims);
            R                 : constant Node_Id := Right_Opnd (N);
            R_Dims            : constant Dimensions := Get_Dimensions (R);
            R_Has_Dimensions  : constant Boolean := Present (R_Dims);
            Dims              : Dimensions := Zero_Dimensions;

         begin
            if Nkind_In (N, N_Op_Add, N_Op_Mod, N_Op_Rem, N_Op_Subtract) then
               Error_Msg_Name_1 := Chars (N);

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
                     for Dim in Dimensions'Range loop
                        Dims (Dim) := L_Dims (Dim) + R_Dims (Dim);
                     end loop;

                  --  Get both operands dimension and subtract them

                  else
                     for Dim in Dimensions'Range loop
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

               if Present (Dims) then
                  Set_Dimensions (N, Dims);
               end if;

            --  N_Op_Expon
            --  Propagation of the dimension and evaluation of the result if
            --  the exponent is a rational and if the operand has a dimension.

            elsif N_Kind = N_Op_Expon then
               declare
                  Rat : Rational := Zero_Rational;

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

                     if Get_Dimension_System_Id
                          (Base_Type (Etype (L))) /= No_Dim_Sys
                       and then Compile_Time_Known_Value (R)
                     then
                        --  Real exponent case

                        if Is_Real_Type (Etype (L)) then
                           --  Define the exponent as a Rational number

                           Create_Rational_From_Expr (R, Rat);

                           if L_Has_Dimensions then
                              for Dim in Dimensions'Range loop
                                 Dims (Dim) := L_Dims (Dim) * Rat;
                              end loop;

                              if Present (Dims) then
                                 Set_Dimensions (N, Dims);
                              end if;
                           end if;

                           --  Evaluate the operator with rational exponent

                           --  Eval_Op_Expon_With_Rational_Exponent (N, Rat);

                        --  Integer exponent case

                        else
                           for Dim in Dimensions'Range loop
                              Dims (Dim) :=
                                L_Dims (Dim) *
                                 Whole (UI_To_Int (Expr_Value (R)));
                           end loop;

                           if Present (Dims) then
                              Set_Dimensions (N, Dims);
                           end if;
                        end if;
                     end if;
                  end if;
               end;

            --  For relational operations, only a dimension checking is
            --  performed (no propagation).

            elsif N_Kind in N_Op_Compare then
               Error_Msg_Name_1 := Chars (N);

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
      Dim_T  : constant Dimensions := Get_Dimensions (E_Typ);
      Dim_E  : Dimensions;

   begin
      if Present (Dim_T) then

         --  If the component type has a dimension and there is no expression,
         --  propagates the dimension.

         if Present (Expr) then
            Dim_E := Get_Dimensions (Expr);

            if Present (Dim_E) then

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
      Dims_R    : constant Dimensions := Get_Dimensions (R_Etyp);
      Dims_Obj  : Dimensions;
      Obj_Decl  : Node_Id;
      Obj_Id    : Entity_Id;

   begin
      if Present (Obj_Decls) then
         Obj_Decl := First (Obj_Decls);
         while Present (Obj_Decl) loop
            if Nkind (Obj_Decl) = N_Object_Declaration then
               Obj_Id := Defining_Identifier (Obj_Decl);

               if Is_Return_Object (Obj_Id) then
                  Dims_Obj := Get_Dimensions (Obj_Id);

                  if Dims_R /= Dims_Obj then
                     Error_Msg_N ("?dimensions missmatch in return statement",
                                  N);
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
      Dims       : Dimensions;
      Dims_Param : Dimensions;
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

         if Comes_From_Source (N)
           and then Is_Entity_Name (Name_Call)
         then
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
            Dims := Get_Dimensions (First (Par_Ass));

            if Present (Dims) then
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
               Dims_Param := Get_Dimensions (Param);

               if Present (Dims_Param) then
                  Error_Msg_Name_1 := Chars (Name_Call);
                  Error_Msg_N
                    ("?parameter should be dimensionless for elementary " &
                     "function%",
                      Param);
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
      Dims   : constant Dimensions := Get_Dimensions (E_Typ);
      N_Kind : constant Node_Kind := Nkind (N);

   begin
      --  Propagation of the dimensions from the type

      if Present (Dims) then
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
      Dims : constant Dimensions := Get_Dimensions (Ent);
   begin
      if Present (Dims) then
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
      Dim_T  : constant Dimensions := Get_Dimensions (E_Typ);
      Dim_E  : Dimensions;

   begin
      if Present (Dim_T) then

         --  Expression is present

         if Present (Expr) then
            Dim_E := Get_Dimensions (Expr);

            if Present (Dim_E) then

               --  Return an error if the dimension of the expression and the
               --  dimension of the type missmatch.

               if Dim_E /= Dim_T then
                  Error_Msg_N ("?dimensions missmatch in object " &
                               "declaration", N);
               end if;

            --  If the expression is dimensionless

            else
               --  If the node is not a real constant or an integer constant
               --  (depending on the dimensioned numeric type), return an error
               --  message.

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
      Dims_Typ : constant Dimensions := Get_Dimensions (E_Typ);
   begin
      if Present (Dims_Typ) then
         Copy_Dimensions (E_Typ, Id);
      end if;
   end Analyze_Dimension_Object_Renaming_Declaration;

   -----------------------------------------------
   -- Analyze_Dimension_Simple_Return_Statement --
   -----------------------------------------------

   procedure Analyze_Dimension_Simple_Return_Statement (N : Node_Id) is
      Expr      : constant Node_Id := Expression (N);
      Dims_Expr : constant Dimensions := Get_Dimensions (Expr);
      R_Ent     : constant Entity_Id := Return_Statement_Entity (N);
      R_Etyp    : constant Entity_Id := Etype (Return_Applies_To (R_Ent));
      Dims_R    : constant Dimensions := Get_Dimensions (R_Etyp);
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
      Dims_Ent : constant Dimensions := Get_Dimensions (Ent);
      E_Typ    : Node_Id;

   begin
      if Nkind (Subtype_Indication (N)) /= N_Subtype_Indication then
         E_Typ := Etype (Subtype_Indication (N));
         declare
            Dims_Typ : constant Dimensions := Get_Dimensions (E_Typ);

         begin
            if Present (Dims_Typ) then

               --  If subtype already has a dimension (from Aspect_Dimension),
               --  it cannot inherit a dimension from its subtype.

               if Present (Dims_Ent) then
                  Error_Msg_N ("?subtype& already has a dimension", N);

               else
                  Set_Dimensions (Ent, Dims_Typ);
                  Set_Dimensions_String_Id
                    (Ent, Get_Dimensions_String_Id (E_Typ));
               end if;
            end if;
         end;

      else
         E_Typ := Etype (Subtype_Mark (Subtype_Indication (N)));
         declare
            Dims_Typ : constant Dimensions := Get_Dimensions (E_Typ);

         begin
            if Present (Dims_Typ) then

               --  If subtype already has a dimension (from Aspect_Dimension),
               --  it cannot inherit a dimension from its subtype.

               if Present (Dims_Ent) then
                  Error_Msg_N ("?subtype& already has a dimension", N);

               else
                  Set_Dimensions (Ent, Dims_Typ);
                  Set_Dimensions_String_Id
                    (Ent, Get_Dimensions_String_Id (E_Typ));
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

   procedure Copy_Dimensions (From, To : Node_Id) is
      Dims : constant Dimensions := Aspect_Dimension_Hash_Table.Get (From);

   begin
      --  Propagate the dimension from one node to another

      pragma Assert (Permits_Dimensions (To));
      pragma Assert (Present (Dims));
      Aspect_Dimension_Hash_Table.Set (To, Dims);
   end Copy_Dimensions;

   -------------------------------
   -- Create_Rational_From_Expr --
   -------------------------------

   procedure Create_Rational_From_Expr (Expr : Node_Id; R : in out Rational) is
      Or_N         : constant Node_Id := Original_Node (Expr);
      Left         : Node_Id;
      Left_Int     : Int;
      Ltype        : Entity_Id;
      Right        : Node_Id;
      Right_Int    : Int;
      R_Opnd_Minus : Node_Id;
      Rtype        : Entity_Id;

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
                  R := +Whole (UI_To_Int (Expr_Value (Expr)));
               else
                  R := Whole (Left_Int) / Whole (Right_Int);
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
                  R := +Whole (-UI_To_Int (Expr_Value (Expr)));
               else
                  R := Whole (-Left_Int) / Whole (Right_Int);
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
            R         :=  +Whole (Right_Int);

         else
            Error_Msg_N ("must be a rational", Expr);
         end if;
      end if;
   end Create_Rational_From_Expr;

   ----------------------------------------
   -- Eval_Op_Expon_For_Dimensioned_Type --
   ----------------------------------------

   --  Eval the expon operator for dimensioned type

   --  Note that if the exponent is an integer (denominator = 1) the node is
   --  not evaluated here and must be evaluated by the Eval_Op_Expon routine.

   procedure Eval_Op_Expon_For_Dimensioned_Type
     (N : Node_Id;
      B_Typ : Entity_Id)
   is
      R   : constant Node_Id := Right_Opnd (N);
      Rat : Rational := Zero_Rational;
   begin
      if Compile_Time_Known_Value (R) and then Is_Real_Type (B_Typ) then
         Create_Rational_From_Expr (R, Rat);
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
      Dims         : constant Dimensions := Get_Dimensions (N);
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
      Sys          : Dim_Sys_Id;

   begin
      --  If Rat.Denominator = 1 that means the exponent is an Integer so
      --  nothing has to be changed. Note that the node must come from source.

      if Comes_From_Source (N)
        and then Rat.Denominator /= 1
      then
         Base_Typ := Base_Type (Etyp);

         --  Case when the operand is not dimensionless

         if Present (Dims) then

            --  Get the corresponding Dim_Sys_Id to know the exact number of
            --  dimensions in the system.

            Sys := Get_Dimension_System_Id (Base_Typ);

            --  Step 1: Generation of a new subtype with the proper dimensions

            --  In order to rewrite the operator as a function call, a new
            --  subtype with an aspect dimension using the dimensions of the
            --  node has to be created.

            --  Generate:

            --  Base_Typ  : constant Entity_Id := Base_Type (Etyp);
            --  Sys       : constant Dim_Sys_Id :=
            --               Get_Dimension_System_Id (Base_Typ);
            --  N_Dims    : constant N_Of_Dimensions :=
            --               Dim_Systems.Table (Sys).N_Of_Dims;
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

            for Dim in Dims'First .. Dim_Systems.Table (Sys).N_Of_Dims loop
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
      Dims         : Dimensions;
      Etyp         : Entity_Id;
      First_Actual : Node_Id;
      New_Par_Ass  : List_Id;
      New_Str_Lit  : Node_Id;
      Sys          : Dim_Sys_Id;

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
         Sys := Get_Dimension_System_Id (Base_Typ);

         if Sys /= No_Dim_Sys then
            Dims := Get_Dimensions (Actual);
            Etyp := Etype (Actual);

            --  Add the string as a suffix of the value if the subtype has a
            --  string of dimensions or if the parameter is not dimensionless.

            if Present (Dims)
              or else Get_Dimensions_String_Id (Etyp) /= No_String
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

               if Get_Dimensions_String_Id (Etyp) /= No_String then
                  Start_String;

                  --  Put a space between the value and the dimension

                  Store_String_Char (' ');
                  Store_String_Chars (Get_Dimensions_String_Id (Etyp));
                  New_Str_Lit :=
                    Make_String_Literal (Loc, End_String);

               --  Rewrite the String_Literal of the second actual with the
               --  new String_Id created by the routine
               --  From_Dimension_To_String.

               else
                  New_Str_Lit :=
                    Make_String_Literal (Loc,
                      From_Dimension_To_String_Id (Dims, Sys));
               end if;

               Append (New_Str_Lit, New_Par_Ass);

               --  Rewrite the procedure call with the new list of parameters

               Rewrite (N,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Copy (Name_Call),
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
     (Dims : Dimensions;
      Sys  : Dim_Sys_Id) return String_Id
   is
      Dim_Rat          : Rational;
      First_Dim_In_Str : Boolean := True;

   begin
      --  Initialization of the new String_Id

      Start_String;

      --  Put a space between the value and the dimensions

      Store_String_Char (' ');

      for Dim in Dimensions'Range loop
         Dim_Rat := Dims (Dim);
         if Dim_Rat /= Zero_Rational then

            if First_Dim_In_Str then
               First_Dim_In_Str := False;
            else
               Store_String_Char ('.');
            end if;

            --  Positive dimension case

            if Dim_Rat.Numerator > 0 then
               if Dim_Systems.Table (Sys).Symbols (Dim) = No_String then
                  Store_String_Chars
                    (Get_Name_String (Dim_Systems.Table (Sys).Names (Dim)));
               else
                  Store_String_Chars (Dim_Systems.Table (Sys).Symbols (Dim));
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
               if Dim_Systems.Table (Sys).Symbols (Dim) = No_String then
                  Store_String_Chars
                    (Get_Name_String (Dim_Systems.Table (Sys).Names (Dim)));
               else
                  Store_String_Chars (Dim_Systems.Table (Sys).Symbols (Dim));
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

   --------------------
   -- Get_Dimensions --
   --------------------

   function Get_Dimensions (N : Node_Id) return Dimensions is
   begin
      return Aspect_Dimension_Hash_Table.Get (N);
   end Get_Dimensions;

   ------------------------------
   -- Get_Dimensions_String_Id --
   ------------------------------

   function Get_Dimensions_String_Id (E : Entity_Id) return String_Id is
   begin
      return Aspect_Dimension_String_Id_Hash_Table.Get (E);
   end Get_Dimensions_String_Id;

   -----------------------------
   -- Get_Dimension_System_Id --
   -----------------------------

   function Get_Dimension_System_Id (E : Entity_Id) return Dim_Sys_Id is
      D_Sys : Dim_Sys_Id := No_Dim_Sys;

   begin
      --  Scan the Table in order to find N
      --  What is N??? no sign of anything called N here ???

      for Dim_Sys in 1 .. Dim_Systems.Last loop
         if Parent (E) = Dim_Systems.Table (Dim_Sys).Base_Type then
            D_Sys := Dim_Sys;
         end if;
      end loop;

      return D_Sys;
   end Get_Dimension_System_Id;

   --------------------------
   -- Is_Dimensioned_Type --
   --------------------------

   function Is_Dimensioned_Type (E : Entity_Id) return Boolean is
   begin
      if Get_Dimension_System_Id (E) /= No_Dim_Sys then
         return True;
      else
         return False;
      end if;
   end Is_Dimensioned_Type;

   ---------------------
   -- Move_Dimensions --
   ---------------------

   procedure Move_Dimensions (From, To : Node_Id) is
      Dims : constant Dimensions := Get_Dimensions (From);

   begin
      --  Copy the dimension of 'From to 'To' and remove dimension of 'From'

      if Present (Dims) then
         Set_Dimensions (To, Dims);
         Remove_Dimensions (From);
      end if;
   end Move_Dimensions;

   ------------------------
   -- Permits_Dimensions --
   ------------------------

   --  Here is the list of node that permits a dimension

   Dimensions_Permission : constant array (Node_Kind) of Boolean :=
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

   function Permits_Dimensions (N : Node_Id) return Boolean is
   begin
      return Dimensions_Permission (Nkind (N));
   end Permits_Dimensions;

   -------------
   -- Present --
   -------------

   function Present (Dim : Dimensions) return Boolean is
   begin
      return Dim /= Zero_Dimensions;
   end Present;

   -----------------------
   -- Remove_Dimensions --
   -----------------------

   procedure Remove_Dimensions (N : Node_Id) is
      Dims : constant Dimensions := Get_Dimensions (N);
   begin
      if Present (Dims) then
         Aspect_Dimension_Hash_Table.Remove (N);
      end if;
   end Remove_Dimensions;

   ------------------------------
   -- Remove_Dimension_In_Call --
   ------------------------------

   procedure Remove_Dimension_In_Call (N : Node_Id) is
      Actual  : Node_Id;
      Par_Ass : constant List_Id := Parameter_Associations (N);

   begin
      if Ada_Version < Ada_2012 then
         return;
      end if;

      if Present (Par_Ass) then
         Actual := First (Par_Ass);
         while Present (Actual) loop
            Remove_Dimensions (Actual);
            Next (Actual);
         end loop;
      end if;
   end Remove_Dimension_In_Call;

   -------------------------------------
   -- Remove_Dimension_In_Declaration --
   -------------------------------------

   --  Removal of dimension in expressions of N_Object_Declaration and
   --  N_Component_Declaration as part of the Analyze_Declarations routine
   --  (see package Sem_Ch3).

   procedure Remove_Dimension_In_Declaration (D : Node_Id) is
   begin
      if Ada_Version < Ada_2012 then
         return;
      end if;

      if Nkind_In (D, N_Object_Declaration, N_Component_Declaration) then
         if Present (Expression (D)) then
            Remove_Dimensions (Expression (D));
         end if;
      end if;
   end Remove_Dimension_In_Declaration;

   -----------------------------------
   -- Remove_Dimension_In_Statement --
   -----------------------------------

   --  Removal of dimension in statement as part of the Analyze_Statements
   --  routine (see package Sem_Ch5).

   procedure Remove_Dimension_In_Statement (S : Node_Id) is
      S_Kind : constant Node_Kind := Nkind (S);

   begin
      if Ada_Version < Ada_2012 then
         return;
      end if;

      --  Remove dimension in parameter specifications for accept statement

      if S_Kind = N_Accept_Statement then
         declare
            Param : Node_Id := First (Parameter_Specifications (S));
         begin
            while Present (Param) loop
               Remove_Dimensions (Param);
               Next (Param);
            end loop;
         end;

      --  Remove dimension of name and expression in assignments

      elsif S_Kind = N_Assignment_Statement then
         Remove_Dimensions (Expression (S));
         Remove_Dimensions (Name (S));
      end if;
   end Remove_Dimension_In_Statement;

   --------------------
   -- Set_Dimensions --
   --------------------

   procedure Set_Dimensions (N : Node_Id; Dims : Dimensions) is
   begin
      pragma Assert (Permits_Dimensions (N));
      pragma Assert (Present (Dims));
      Aspect_Dimension_Hash_Table.Set (N, Dims);
   end Set_Dimensions;

   ------------------------------
   -- Set_Dimensions_String_Id --
   ------------------------------

   procedure Set_Dimensions_String_Id (E : Entity_Id; Str : String_Id) is
   begin
      Aspect_Dimension_String_Id_Hash_Table.Set (E, Str);
   end Set_Dimensions_String_Id;

end Sem_Dim;
