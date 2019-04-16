------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ I N T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  Processing for intrinsic subprogram declarations

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Uintp;    use Uintp;

package body Sem_Intr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_Exception_Function (E : Entity_Id; N : Node_Id);
   --  Check use of intrinsic Exception_Message, Exception_Info or
   --  Exception_Name, as used in the DEC compatible Current_Exceptions
   --  package. In each case we must have a parameterless function that
   --  returns type String.

   procedure Check_Intrinsic_Operator (E : Entity_Id; N : Node_Id);
   --  Check that operator is one of the binary arithmetic operators, and that
   --  the types involved both have underlying integer types.

   procedure Check_Shift (E : Entity_Id; N : Node_Id);
   --  Check intrinsic shift subprogram, the two arguments are the same
   --  as for Check_Intrinsic_Subprogram (i.e. the entity of the subprogram
   --  declaration, and the node for the pragma argument, used for messages).

   procedure Errint
     (Msg : String; S : Node_Id; N : Node_Id; Relaxed : Boolean := False);
   --  Post error message for bad intrinsic, the message itself is posted
   --  on the appropriate spec node and another message is placed on the
   --  pragma itself, referring to the spec. S is the node in the spec on
   --  which the message is to be placed, and N is the pragma argument node.
   --  Relaxed is True if the message should not be emitted in
   --  Relaxed_RM_Semantics mode.

   ------------------------------
   -- Check_Exception_Function --
   ------------------------------

   procedure Check_Exception_Function (E : Entity_Id; N : Node_Id) is
   begin
      if not Ekind_In (E, E_Function, E_Generic_Function) then
         Errint
           ("intrinsic exception subprogram must be a function", E, N);

      elsif Present (First_Formal (E)) then
         Errint
           ("intrinsic exception subprogram may not have parameters",
            E, First_Formal (E));
         return;

      elsif Etype (E) /= Standard_String then
         Errint
           ("return type of exception subprogram must be String", E, N);
         return;
      end if;
   end Check_Exception_Function;

   --------------------------
   -- Check_Intrinsic_Call --
   --------------------------

   procedure Check_Intrinsic_Call (N : Node_Id) is
      Nam  : constant Entity_Id := Entity (Name (N));
      Arg1 : constant Node_Id   := First_Actual (N);
      Typ  : Entity_Id;
      Rtyp : Entity_Id := Empty;
      Cnam : Name_Id;
      Unam : Node_Id;

   begin
      --  Set argument type if argument present

      if Present (Arg1) then
         Typ := Etype (Arg1);
         Rtyp := Underlying_Type (Root_Type (Typ));
      end if;

      --  Set intrinsic name (getting original name in the generic case)

      Unam := Ultimate_Alias (Nam);

      if Present (Parent (Unam))
        and then Present (Generic_Parent (Parent (Unam)))
      then
         Cnam := Chars (Generic_Parent (Parent (Unam)));
      else
         Cnam := Chars (Nam);
      end if;

      --  For Import_xxx calls, argument must be static string. A string
      --  literal is legal even in Ada 83 mode, where such literals are
      --  not static.

      if Nam_In (Cnam, Name_Import_Address,
                       Name_Import_Largest_Value,
                       Name_Import_Value)
      then
         if Etype (Arg1) = Any_Type
           or else Raises_Constraint_Error (Arg1)
         then
            null;

         elsif Nkind (Arg1) /= N_String_Literal
           and then not Is_OK_Static_Expression (Arg1)
         then
            Error_Msg_FE
              ("call to & requires static string argument!", N, Nam);
            Why_Not_Static (Arg1);

         elsif String_Length (Strval (Expr_Value_S (Arg1))) = 0 then
            Error_Msg_NE
              ("call to & does not permit null string", N, Nam);
         end if;

      --  Check for the case of freeing a non-null object which will raise
      --  Constraint_Error. Issue warning here, do the expansion in Exp_Intr.

      elsif Cnam = Name_Unchecked_Deallocation
        and then Can_Never_Be_Null (Etype (Arg1))
      then
         Error_Msg_N
           ("freeing `NOT NULL` object will raise Constraint_Error??", N);

      --  For unchecked deallocation, error to deallocate from empty pool.
      --  Note: this test used to be in Exp_Intr as a warning, but AI 157
      --  issues a binding interpretation that this should be an error, and
      --  consequently it needs to be done in the semantic analysis so that
      --  the error is issued even in semantics only mode.

      elsif Cnam = Name_Unchecked_Deallocation
        and then No_Pool_Assigned (Rtyp)
      then
         Error_Msg_N ("deallocation from empty storage pool!", N);

      --  For now, no other special checks are required

      else
         return;
      end if;
   end Check_Intrinsic_Call;

   ------------------------------
   -- Check_Intrinsic_Operator --
   ------------------------------

   procedure Check_Intrinsic_Operator (E : Entity_Id; N : Node_Id) is
      Ret : constant Entity_Id := Etype (E);
      Nam : constant Name_Id   := Chars (E);
      T1  : Entity_Id;
      T2  : Entity_Id;

   begin
      --  Arithmetic operators

      if Nam_In (Nam, Name_Op_Add, Name_Op_Subtract, Name_Op_Multiply,
                      Name_Op_Divide, Name_Op_Rem, Name_Op_Mod, Name_Op_Abs)
      then
         T1 := Etype (First_Formal (E));

         if No (Next_Formal (First_Formal (E))) then
            if Nam_In (Nam, Name_Op_Add, Name_Op_Subtract, Name_Op_Abs) then
               T2 := T1;

            --  Previous error in declaration

            else
               return;
            end if;

         else
            T2 := Etype (Next_Formal (First_Formal (E)));
         end if;

         --  Same types, predefined operator will apply

         if Root_Type (T1) = Root_Type (T2)
           or else Root_Type (T1) = Root_Type (Ret)
         then
            null;

         --  Expansion will introduce conversions if sizes are not equal

         elsif Is_Integer_Type (Underlying_Type (T1))
           and then Is_Integer_Type (Underlying_Type (T2))
           and then Is_Integer_Type (Underlying_Type (Ret))
         then
            null;

         else
            Errint
              ("types of intrinsic operator operands do not match", E, N);
         end if;

      --  Comparison operators

      elsif Nam_In (Nam, Name_Op_Eq, Name_Op_Ge, Name_Op_Gt, Name_Op_Le,
                         Name_Op_Lt, Name_Op_Ne)
      then
         T1 := Etype (First_Formal (E));

         --  Return if previous error in declaration, otherwise get T2 type

         if No (Next_Formal (First_Formal (E))) then
            Check_Error_Detected;
            return;

         else
            T2 := Etype (Next_Formal (First_Formal (E)));
         end if;

         if Root_Type (T1) /= Root_Type (T2) then
            Errint
              ("types of intrinsic operator must have the same size", E, N);
         end if;

         if Root_Type (Ret) /= Standard_Boolean then
            Errint
              ("result type of intrinsic comparison must be boolean", E, N);
         end if;

      --  Exponentiation

      elsif Nam = Name_Op_Expon then
         T1 := Etype (First_Formal (E));

         if No (Next_Formal (First_Formal (E))) then

            --  Previous error in declaration

            return;

         else
            T2 := Etype (Next_Formal (First_Formal (E)));
         end if;

         if not (Is_Integer_Type (T1)
                   or else
                 Is_Floating_Point_Type (T1))
           or else Root_Type (T1) /= Root_Type (Ret)
           or else Root_Type (T2) /= Root_Type (Standard_Integer)
         then
            Errint ("incorrect operands for intrinsic operator", N, E);
         end if;

      --  All other operators (are there any?) are not handled

      else
         Errint ("incorrect context for ""Intrinsic"" convention", E, N);
         return;
      end if;

      --  The type must be fully defined and numeric.

      if No (Underlying_Type (T1))
        or else not Is_Numeric_Type (Underlying_Type (T1))
      then
         Errint ("intrinsic operator can only apply to numeric types", E, N);
      end if;
   end Check_Intrinsic_Operator;

   --------------------------------
   -- Check_Intrinsic_Subprogram --
   --------------------------------

   procedure Check_Intrinsic_Subprogram (E : Entity_Id; N : Node_Id) is
      Spec : constant Node_Id := Specification (Unit_Declaration_Node (E));
      Nam  : Name_Id;

   begin
      if Present (Spec)
        and then Present (Generic_Parent (Spec))
      then
         Nam := Chars (Generic_Parent (Spec));
      else
         Nam := Chars (E);
      end if;

      --  Check name is valid intrinsic name

      Get_Name_String (Nam);

      if Name_Buffer (1) /= 'O'
        and then Nam /= Name_Asm
        and then Nam /= Name_To_Address
        and then Nam not in First_Intrinsic_Name .. Last_Intrinsic_Name
      then
         Errint ("unrecognized intrinsic subprogram", E, N);

      --  Shift cases. We allow user specification of intrinsic shift operators
      --  for any numeric types.

      elsif Nam_In (Nam, Name_Rotate_Left, Name_Rotate_Right, Name_Shift_Left,
                         Name_Shift_Right, Name_Shift_Right_Arithmetic)
      then
         Check_Shift (E, N);

      --  We always allow intrinsic specifications in language defined units
      --  and in expanded code. We assume that the GNAT implementors know what
      --  they are doing, and do not write or generate junk use of intrinsic.

      elsif not Comes_From_Source (E)
        or else not Comes_From_Source (N)
        or else In_Predefined_Unit (N)
      then
         null;

      --  Exception functions

      elsif Nam_In (Nam, Name_Exception_Information,
                         Name_Exception_Message,
                         Name_Exception_Name)
      then
         Check_Exception_Function (E, N);

      --  Intrinsic operators

      elsif Nkind (E) = N_Defining_Operator_Symbol then
         Check_Intrinsic_Operator (E, N);

      --  Source_Location and navigation functions

      elsif Nam_In (Nam, Name_File,
                         Name_Line,
                         Name_Source_Location,
                         Name_Enclosing_Entity,
                         Name_Compilation_ISO_Date,
                         Name_Compilation_Date,
                         Name_Compilation_Time)
      then
         null;

      --  For now, no other intrinsic subprograms are recognized in user code

      else
         Errint ("incorrect context for ""Intrinsic"" convention", E, N);
      end if;
   end Check_Intrinsic_Subprogram;

   -----------------
   -- Check_Shift --
   -----------------

   procedure Check_Shift (E : Entity_Id; N : Node_Id) is
      Arg1  : Node_Id;
      Arg2  : Node_Id;
      Size  : Nat;
      Typ1  : Entity_Id;
      Typ2  : Entity_Id;
      Ptyp1 : Node_Id;
      Ptyp2 : Node_Id;

   begin
      if not Ekind_In (E, E_Function, E_Generic_Function) then
         Errint ("intrinsic shift subprogram must be a function", E, N);
         return;
      end if;

      Arg1 := First_Formal (E);

      if Present (Arg1) then
         Arg2 := Next_Formal (Arg1);
      else
         Arg2 := Empty;
      end if;

      if Arg1 = Empty or else Arg2 = Empty then
         Errint ("intrinsic shift function must have two arguments", E, N);
         return;
      end if;

      Typ1 := Etype (Arg1);
      Typ2 := Etype (Arg2);

      Ptyp1 := Parameter_Type (Parent (Arg1));
      Ptyp2 := Parameter_Type (Parent (Arg2));

      if not Is_Integer_Type (Typ1) then
         Errint ("first argument to shift must be integer type", Ptyp1, N);
         return;
      end if;

      if Typ2 /= Standard_Natural then
         Errint ("second argument to shift must be type Natural", Ptyp2, N);
         return;
      end if;

      --  type'Size (not 'Object_Size) must be one of the allowed values

      Size := UI_To_Int (RM_Size (Typ1));

      if Size /= 8  and then
         Size /= 16 and then
         Size /= 32 and then
         Size /= 64
      then
         Errint
           ("first argument for shift must have size 8, 16, 32 or 64",
            Ptyp1, N, Relaxed => True);
         return;

      elsif Non_Binary_Modulus (Typ1) then
         Errint ("shifts not allowed for nonbinary modular types", Ptyp1, N);

      --  For modular type, modulus must be 2**8, 2**16, 2**32, or 2**64.
      --  Don't apply to generic types, since we may not have a modulus value.

      elsif Is_Modular_Integer_Type (Typ1)
        and then not Is_Generic_Type (Typ1)
        and then Modulus (Typ1) /= Uint_2 ** 8
        and then Modulus (Typ1) /= Uint_2 ** 16
        and then Modulus (Typ1) /= Uint_2 ** 32
        and then Modulus (Typ1) /= Uint_2 ** 64
      then
         Errint
           ("modular type for shift must have modulus of 2'*'*8, "
            & "2'*'*16, 2'*'*32, or 2'*'*64", Ptyp1, N, Relaxed => True);

      elsif Etype (Arg1) /= Etype (E) then
         Errint
           ("first argument of shift must match return type", Ptyp1, N);
         return;
      end if;

      Set_Has_Shift_Operator (Base_Type (Typ1));
   end Check_Shift;

   ------------
   -- Errint --
   ------------

   procedure Errint
     (Msg : String; S : Node_Id; N : Node_Id; Relaxed : Boolean := False) is
   begin
      --  Ignore errors on Intrinsic in Relaxed_RM_Semantics mode where we can
      --  be more liberal.

      if not (Relaxed and Relaxed_RM_Semantics) then
         Error_Msg_N (Msg, S);
         Error_Msg_N ("incorrect intrinsic subprogram, see spec", N);
      end if;
   end Errint;

end Sem_Intr;
