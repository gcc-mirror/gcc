------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ I N T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Processing for intrinsic subprogram declarations

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Fname;    use Fname;
with Lib;      use Lib;
with Namet;    use Namet;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
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
   --  Check that operator is one of the binary arithmetic operators, and
   --  that the types involved have the same size.

   procedure Check_Shift (E : Entity_Id; N : Node_Id);
   --  Check intrinsic shift subprogram, the two arguments are the same
   --  as for Check_Intrinsic_Subprogram (i.e. the entity of the subprogram
   --  declaration, and the node for the pragma argument, used for messages)

   procedure Errint (Msg : String; S : Node_Id; N : Node_Id);
   --  Post error message for bad intrinsic, the message itself is posted
   --  on the appropriate spec node and another message is placed on the
   --  pragma itself, referring to the spec. S is the node in the spec on
   --  which the message is to be placed, and N is the pragma argument node.

   ------------------------------
   -- Check_Exception_Function --
   ------------------------------

   procedure Check_Exception_Function (E : Entity_Id; N : Node_Id) is
   begin
      if Ekind (E) /= E_Function
        and then Ekind (E) /= E_Generic_Function
      then
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
      Cnam : constant Name_Id   := Chars (Nam);
      Arg1 : constant Node_Id   := First_Actual (N);

   begin
      --  For Import_xxx calls, argument must be static string

      if Cnam = Name_Import_Address
           or else
         Cnam = Name_Import_Largest_Value
           or else
         Cnam = Name_Import_Value
      then
         if Etype (Arg1) = Any_Type
           or else Raises_Constraint_Error (Arg1)
         then
            null;

         elsif not Is_Static_Expression (Arg1) then
            Error_Msg_FE
              ("call to & requires static string argument!", N, Nam);
            Why_Not_Static (Arg1);

         elsif String_Length (Strval (Expr_Value_S (Arg1))) = 0 then
            Error_Msg_NE
              ("call to & does not permit null string", N, Nam);

         elsif OpenVMS_On_Target
           and then String_Length (Strval (Expr_Value_S (Arg1))) > 31
         then
            Error_Msg_NE
              ("argument in call to & must be 31 characters or less", N, Nam);
         end if;

      --  Check for the case of freeing a non-null object which will raise
      --  Constaint_Error. Issue warning here, do the expansion in Exp_Intr.

      elsif Cnam = Name_Free
        and then Can_Never_Be_Null (Etype (Arg1))
      then
         Error_Msg_N
           ("freeing `NOT NULL` object will raise Constraint_Error?", N);

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
      --  Aritnmetic operators

      if Nam = Name_Op_Add
           or else
         Nam = Name_Op_Subtract
           or else
         Nam = Name_Op_Multiply
           or else
         Nam = Name_Op_Divide
           or else
         Nam = Name_Op_Rem
           or else
         Nam = Name_Op_Mod
           or else
         Nam = Name_Op_Abs
      then
         T1 := Etype (First_Formal (E));

         if No (Next_Formal (First_Formal (E))) then

            if Nam = Name_Op_Add
                 or else
               Nam = Name_Op_Subtract
                 or else
               Nam = Name_Op_Abs
            then
               T2 := T1;

            else
               --  Previous error in declaration

               return;
            end if;

         else
            T2 := Etype (Next_Formal (First_Formal (E)));
         end if;

         if Root_Type (T1) /= Root_Type (T2)
           or else Root_Type (T1) /= Root_Type (Ret)
         then
            Errint
              ("types of intrinsic operator must have the same size", E, N);
         end if;

      --  Comparison operators

      elsif Nam = Name_Op_Eq
              or else
            Nam = Name_Op_Ge
              or else
            Nam = Name_Op_Gt
              or else
            Nam = Name_Op_Le
              or else
            Nam = Name_Op_Lt
              or else
            Nam = Name_Op_Ne
      then
         T1 := Etype (First_Formal (E));

         if No (Next_Formal (First_Formal (E))) then

            --  Previous error in declaration

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

      if not Is_Numeric_Type (T1) then
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

      --  We always allow intrinsic specifications in language defined units
      --  and in expanded code. We assume that the GNAT implemetors know what
      --  they are doing, and do not write or generate junk use of intrinsic!

      elsif not Comes_From_Source (E)
        or else not Comes_From_Source (N)
        or else Is_Predefined_File_Name
                  (Unit_File_Name (Get_Source_Unit (N)))
      then
         null;

      --  Shift cases. We allow user specification of intrinsic shift
      --  operators for any numeric types.

      elsif
        Nam = Name_Rotate_Left
          or else
        Nam = Name_Rotate_Right
          or else
        Nam = Name_Shift_Left
          or else
        Nam = Name_Shift_Right
          or else
        Nam = Name_Shift_Right_Arithmetic
      then
         Check_Shift (E, N);

      elsif
        Nam = Name_Exception_Information
          or else
        Nam = Name_Exception_Message
          or else
        Nam = Name_Exception_Name
      then
         Check_Exception_Function (E, N);

      elsif Nkind (E) = N_Defining_Operator_Symbol then
         Check_Intrinsic_Operator (E, N);

      elsif Nam = Name_File
        or else Nam = Name_Line
        or else Nam = Name_Source_Location
        or else Nam = Name_Enclosing_Entity
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
      if Ekind (E) /= E_Function
        and then Ekind (E) /= E_Generic_Function
      then
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

      Size := UI_To_Int (Esize (Typ1));

      if Size /= 8
        and then Size /= 16
        and then Size /= 32
        and then Size /= 64
      then
         Errint
           ("first argument for shift must have size 8, 16, 32 or 64",
             Ptyp1, N);
         return;

      elsif Is_Modular_Integer_Type (Typ1)
        and then Non_Binary_Modulus (Typ1)
      then
         Errint
           ("shifts not allowed for non-binary modular types",
            Ptyp1, N);

      elsif Etype (Arg1) /= Etype (E) then
         Errint
           ("first argument of shift must match return type", Ptyp1, N);
         return;
      end if;
   end Check_Shift;

   ------------
   -- Errint --
   ------------

   procedure Errint (Msg : String; S : Node_Id; N : Node_Id) is
   begin
      Error_Msg_N (Msg, S);
      Error_Msg_N ("incorrect intrinsic subprogram, see spec", N);
   end Errint;

end Sem_Intr;
