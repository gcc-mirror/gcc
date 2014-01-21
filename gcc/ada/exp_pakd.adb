------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P A K D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
with Checks;   use Checks;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Dbug; use Exp_Dbug;
with Exp_Util; use Exp_Util;
with Layout;   use Layout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;

package body Exp_Pakd is

   ---------------------------
   -- Endian Considerations --
   ---------------------------

   --  As described in the specification, bit numbering in a packed array
   --  is consistent with bit numbering in a record representation clause,
   --  and hence dependent on the endianness of the machine:

   --    For little-endian machines, element zero is at the right hand end
   --    (low order end) of a bit field.

   --    For big-endian machines, element zero is at the left hand end
   --    (high order end) of a bit field.

   --  The shifts that are used to right justify a field therefore differ in
   --  the two cases. For the little-endian case, we can simply use the bit
   --  number (i.e. the element number * element size) as the count for a right
   --  shift. For the big-endian case, we have to subtract the shift count from
   --  an appropriate constant to use in the right shift. We use rotates
   --  instead of shifts (which is necessary in the store case to preserve
   --  other fields), and we expect that the backend will be able to change the
   --  right rotate into a left rotate, avoiding the subtract, if the machine
   --  architecture provides such an instruction.

   ----------------------------------------------
   -- Entity Tables for Packed Access Routines --
   ----------------------------------------------

   --  For the cases of component size = 3,5-7,9-15,17-31,33-63 we call library
   --  routines. This table provides the entity for the proper routine.

   type E_Array is array (Int range 01 .. 63) of RE_Id;

   --  Array of Bits_nn entities. Note that we do not use library routines
   --  for the 8-bit and 16-bit cases, but we still fill in the table, using
   --  entries from System.Unsigned, because we also use this table for
   --  certain special unchecked conversions in the big-endian case.

   Bits_Id : constant E_Array :=
     (01 => RE_Bits_1,
      02 => RE_Bits_2,
      03 => RE_Bits_03,
      04 => RE_Bits_4,
      05 => RE_Bits_05,
      06 => RE_Bits_06,
      07 => RE_Bits_07,
      08 => RE_Unsigned_8,
      09 => RE_Bits_09,
      10 => RE_Bits_10,
      11 => RE_Bits_11,
      12 => RE_Bits_12,
      13 => RE_Bits_13,
      14 => RE_Bits_14,
      15 => RE_Bits_15,
      16 => RE_Unsigned_16,
      17 => RE_Bits_17,
      18 => RE_Bits_18,
      19 => RE_Bits_19,
      20 => RE_Bits_20,
      21 => RE_Bits_21,
      22 => RE_Bits_22,
      23 => RE_Bits_23,
      24 => RE_Bits_24,
      25 => RE_Bits_25,
      26 => RE_Bits_26,
      27 => RE_Bits_27,
      28 => RE_Bits_28,
      29 => RE_Bits_29,
      30 => RE_Bits_30,
      31 => RE_Bits_31,
      32 => RE_Unsigned_32,
      33 => RE_Bits_33,
      34 => RE_Bits_34,
      35 => RE_Bits_35,
      36 => RE_Bits_36,
      37 => RE_Bits_37,
      38 => RE_Bits_38,
      39 => RE_Bits_39,
      40 => RE_Bits_40,
      41 => RE_Bits_41,
      42 => RE_Bits_42,
      43 => RE_Bits_43,
      44 => RE_Bits_44,
      45 => RE_Bits_45,
      46 => RE_Bits_46,
      47 => RE_Bits_47,
      48 => RE_Bits_48,
      49 => RE_Bits_49,
      50 => RE_Bits_50,
      51 => RE_Bits_51,
      52 => RE_Bits_52,
      53 => RE_Bits_53,
      54 => RE_Bits_54,
      55 => RE_Bits_55,
      56 => RE_Bits_56,
      57 => RE_Bits_57,
      58 => RE_Bits_58,
      59 => RE_Bits_59,
      60 => RE_Bits_60,
      61 => RE_Bits_61,
      62 => RE_Bits_62,
      63 => RE_Bits_63);

   --  Array of Get routine entities. These are used to obtain an element from
   --  a packed array. The N'th entry is used to obtain elements from a packed
   --  array whose component size is N. RE_Null is used as a null entry, for
   --  the cases where a library routine is not used.

   Get_Id : constant E_Array :=
     (01 => RE_Null,
      02 => RE_Null,
      03 => RE_Get_03,
      04 => RE_Null,
      05 => RE_Get_05,
      06 => RE_Get_06,
      07 => RE_Get_07,
      08 => RE_Null,
      09 => RE_Get_09,
      10 => RE_Get_10,
      11 => RE_Get_11,
      12 => RE_Get_12,
      13 => RE_Get_13,
      14 => RE_Get_14,
      15 => RE_Get_15,
      16 => RE_Null,
      17 => RE_Get_17,
      18 => RE_Get_18,
      19 => RE_Get_19,
      20 => RE_Get_20,
      21 => RE_Get_21,
      22 => RE_Get_22,
      23 => RE_Get_23,
      24 => RE_Get_24,
      25 => RE_Get_25,
      26 => RE_Get_26,
      27 => RE_Get_27,
      28 => RE_Get_28,
      29 => RE_Get_29,
      30 => RE_Get_30,
      31 => RE_Get_31,
      32 => RE_Null,
      33 => RE_Get_33,
      34 => RE_Get_34,
      35 => RE_Get_35,
      36 => RE_Get_36,
      37 => RE_Get_37,
      38 => RE_Get_38,
      39 => RE_Get_39,
      40 => RE_Get_40,
      41 => RE_Get_41,
      42 => RE_Get_42,
      43 => RE_Get_43,
      44 => RE_Get_44,
      45 => RE_Get_45,
      46 => RE_Get_46,
      47 => RE_Get_47,
      48 => RE_Get_48,
      49 => RE_Get_49,
      50 => RE_Get_50,
      51 => RE_Get_51,
      52 => RE_Get_52,
      53 => RE_Get_53,
      54 => RE_Get_54,
      55 => RE_Get_55,
      56 => RE_Get_56,
      57 => RE_Get_57,
      58 => RE_Get_58,
      59 => RE_Get_59,
      60 => RE_Get_60,
      61 => RE_Get_61,
      62 => RE_Get_62,
      63 => RE_Get_63);

   --  Array of Get routine entities to be used in the case where the packed
   --  array is itself a component of a packed structure, and therefore may not
   --  be fully aligned. This only affects the even sizes, since for the odd
   --  sizes, we do not get any fixed alignment in any case.

   GetU_Id : constant E_Array :=
     (01 => RE_Null,
      02 => RE_Null,
      03 => RE_Get_03,
      04 => RE_Null,
      05 => RE_Get_05,
      06 => RE_GetU_06,
      07 => RE_Get_07,
      08 => RE_Null,
      09 => RE_Get_09,
      10 => RE_GetU_10,
      11 => RE_Get_11,
      12 => RE_GetU_12,
      13 => RE_Get_13,
      14 => RE_GetU_14,
      15 => RE_Get_15,
      16 => RE_Null,
      17 => RE_Get_17,
      18 => RE_GetU_18,
      19 => RE_Get_19,
      20 => RE_GetU_20,
      21 => RE_Get_21,
      22 => RE_GetU_22,
      23 => RE_Get_23,
      24 => RE_GetU_24,
      25 => RE_Get_25,
      26 => RE_GetU_26,
      27 => RE_Get_27,
      28 => RE_GetU_28,
      29 => RE_Get_29,
      30 => RE_GetU_30,
      31 => RE_Get_31,
      32 => RE_Null,
      33 => RE_Get_33,
      34 => RE_GetU_34,
      35 => RE_Get_35,
      36 => RE_GetU_36,
      37 => RE_Get_37,
      38 => RE_GetU_38,
      39 => RE_Get_39,
      40 => RE_GetU_40,
      41 => RE_Get_41,
      42 => RE_GetU_42,
      43 => RE_Get_43,
      44 => RE_GetU_44,
      45 => RE_Get_45,
      46 => RE_GetU_46,
      47 => RE_Get_47,
      48 => RE_GetU_48,
      49 => RE_Get_49,
      50 => RE_GetU_50,
      51 => RE_Get_51,
      52 => RE_GetU_52,
      53 => RE_Get_53,
      54 => RE_GetU_54,
      55 => RE_Get_55,
      56 => RE_GetU_56,
      57 => RE_Get_57,
      58 => RE_GetU_58,
      59 => RE_Get_59,
      60 => RE_GetU_60,
      61 => RE_Get_61,
      62 => RE_GetU_62,
      63 => RE_Get_63);

   --  Array of Set routine entities. These are used to assign an element of a
   --  packed array. The N'th entry is used to assign elements for a packed
   --  array whose component size is N. RE_Null is used as a null entry, for
   --  the cases where a library routine is not used.

   Set_Id : constant E_Array :=
     (01 => RE_Null,
      02 => RE_Null,
      03 => RE_Set_03,
      04 => RE_Null,
      05 => RE_Set_05,
      06 => RE_Set_06,
      07 => RE_Set_07,
      08 => RE_Null,
      09 => RE_Set_09,
      10 => RE_Set_10,
      11 => RE_Set_11,
      12 => RE_Set_12,
      13 => RE_Set_13,
      14 => RE_Set_14,
      15 => RE_Set_15,
      16 => RE_Null,
      17 => RE_Set_17,
      18 => RE_Set_18,
      19 => RE_Set_19,
      20 => RE_Set_20,
      21 => RE_Set_21,
      22 => RE_Set_22,
      23 => RE_Set_23,
      24 => RE_Set_24,
      25 => RE_Set_25,
      26 => RE_Set_26,
      27 => RE_Set_27,
      28 => RE_Set_28,
      29 => RE_Set_29,
      30 => RE_Set_30,
      31 => RE_Set_31,
      32 => RE_Null,
      33 => RE_Set_33,
      34 => RE_Set_34,
      35 => RE_Set_35,
      36 => RE_Set_36,
      37 => RE_Set_37,
      38 => RE_Set_38,
      39 => RE_Set_39,
      40 => RE_Set_40,
      41 => RE_Set_41,
      42 => RE_Set_42,
      43 => RE_Set_43,
      44 => RE_Set_44,
      45 => RE_Set_45,
      46 => RE_Set_46,
      47 => RE_Set_47,
      48 => RE_Set_48,
      49 => RE_Set_49,
      50 => RE_Set_50,
      51 => RE_Set_51,
      52 => RE_Set_52,
      53 => RE_Set_53,
      54 => RE_Set_54,
      55 => RE_Set_55,
      56 => RE_Set_56,
      57 => RE_Set_57,
      58 => RE_Set_58,
      59 => RE_Set_59,
      60 => RE_Set_60,
      61 => RE_Set_61,
      62 => RE_Set_62,
      63 => RE_Set_63);

   --  Array of Set routine entities to be used in the case where the packed
   --  array is itself a component of a packed structure, and therefore may not
   --  be fully aligned. This only affects the even sizes, since for the odd
   --  sizes, we do not get any fixed alignment in any case.

   SetU_Id : constant E_Array :=
     (01 => RE_Null,
      02 => RE_Null,
      03 => RE_Set_03,
      04 => RE_Null,
      05 => RE_Set_05,
      06 => RE_SetU_06,
      07 => RE_Set_07,
      08 => RE_Null,
      09 => RE_Set_09,
      10 => RE_SetU_10,
      11 => RE_Set_11,
      12 => RE_SetU_12,
      13 => RE_Set_13,
      14 => RE_SetU_14,
      15 => RE_Set_15,
      16 => RE_Null,
      17 => RE_Set_17,
      18 => RE_SetU_18,
      19 => RE_Set_19,
      20 => RE_SetU_20,
      21 => RE_Set_21,
      22 => RE_SetU_22,
      23 => RE_Set_23,
      24 => RE_SetU_24,
      25 => RE_Set_25,
      26 => RE_SetU_26,
      27 => RE_Set_27,
      28 => RE_SetU_28,
      29 => RE_Set_29,
      30 => RE_SetU_30,
      31 => RE_Set_31,
      32 => RE_Null,
      33 => RE_Set_33,
      34 => RE_SetU_34,
      35 => RE_Set_35,
      36 => RE_SetU_36,
      37 => RE_Set_37,
      38 => RE_SetU_38,
      39 => RE_Set_39,
      40 => RE_SetU_40,
      41 => RE_Set_41,
      42 => RE_SetU_42,
      43 => RE_Set_43,
      44 => RE_SetU_44,
      45 => RE_Set_45,
      46 => RE_SetU_46,
      47 => RE_Set_47,
      48 => RE_SetU_48,
      49 => RE_Set_49,
      50 => RE_SetU_50,
      51 => RE_Set_51,
      52 => RE_SetU_52,
      53 => RE_Set_53,
      54 => RE_SetU_54,
      55 => RE_Set_55,
      56 => RE_SetU_56,
      57 => RE_Set_57,
      58 => RE_SetU_58,
      59 => RE_Set_59,
      60 => RE_SetU_60,
      61 => RE_Set_61,
      62 => RE_SetU_62,
      63 => RE_Set_63);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Compute_Linear_Subscript
     (Atyp   : Entity_Id;
      N      : Node_Id;
      Subscr : out Node_Id);
   --  Given a constrained array type Atyp, and an indexed component node N
   --  referencing an array object of this type, build an expression of type
   --  Standard.Integer representing the zero-based linear subscript value.
   --  This expression includes any required range checks.

   procedure Convert_To_PAT_Type (Aexp : Node_Id);
   --  Given an expression of a packed array type, builds a corresponding
   --  expression whose type is the implementation type used to represent
   --  the packed array. Aexp is analyzed and resolved on entry and on exit.

   procedure Get_Base_And_Bit_Offset
     (N      : Node_Id;
      Base   : out Node_Id;
      Offset : out Node_Id);
   --  Given a node N for a name which involves a packed array reference,
   --  return the base object of the reference and build an expression of
   --  type Standard.Integer representing the zero-based offset in bits
   --  from Base'Address to the first bit of the reference.

   function Known_Aligned_Enough (Obj : Node_Id; Csiz : Nat) return Boolean;
   --  There are two versions of the Set routines, the ones used when the
   --  object is known to be sufficiently well aligned given the number of
   --  bits, and the ones used when the object is not known to be aligned.
   --  This routine is used to determine which set to use. Obj is a reference
   --  to the object, and Csiz is the component size of the packed array.
   --  True is returned if the alignment of object is known to be sufficient,
   --  defined as 1 for odd bit sizes, 4 for bit sizes divisible by 4, and
   --  2 otherwise.

   function Make_Shift_Left (N : Node_Id; S : Node_Id) return Node_Id;
   --  Build a left shift node, checking for the case of a shift count of zero

   function Make_Shift_Right (N : Node_Id; S : Node_Id) return Node_Id;
   --  Build a right shift node, checking for the case of a shift count of zero

   function RJ_Unchecked_Convert_To
     (Typ  : Entity_Id;
      Expr : Node_Id) return Node_Id;
   --  The packed array code does unchecked conversions which in some cases
   --  may involve non-discrete types with differing sizes. The semantics of
   --  such conversions is potentially endian dependent, and the effect we
   --  want here for such a conversion is to do the conversion in size as
   --  though numeric items are involved, and we extend or truncate on the
   --  left side. This happens naturally in the little-endian case, but in
   --  the big endian case we can get left justification, when what we want
   --  is right justification. This routine does the unchecked conversion in
   --  a stepwise manner to ensure that it gives the expected result. Hence
   --  the name (RJ = Right justified). The parameters Typ and Expr are as
   --  for the case of a normal Unchecked_Convert_To call.

   procedure Setup_Enumeration_Packed_Array_Reference (N : Node_Id);
   --  This routine is called in the Get and Set case for arrays that are
   --  packed but not bit-packed, meaning that they have at least one
   --  subscript that is of an enumeration type with a non-standard
   --  representation. This routine modifies the given node to properly
   --  reference the corresponding packed array type.

   procedure Setup_Inline_Packed_Array_Reference
     (N      : Node_Id;
      Atyp   : Entity_Id;
      Obj    : in out Node_Id;
      Cmask  : out Uint;
      Shift  : out Node_Id);
   --  This procedure performs common processing on the N_Indexed_Component
   --  parameter given as N, whose prefix is a reference to a packed array.
   --  This is used for the get and set when the component size is 1, 2, 4,
   --  or for other component sizes when the packed array type is a modular
   --  type (i.e. the cases that are handled with inline code).
   --
   --  On entry:
   --
   --    N is the N_Indexed_Component node for the packed array reference
   --
   --    Atyp is the constrained array type (the actual subtype has been
   --    computed if necessary to obtain the constraints, but this is still
   --    the original array type, not the Packed_Array_Type value).
   --
   --    Obj is the object which is to be indexed. It is always of type Atyp.
   --
   --  On return:
   --
   --    Obj is the object containing the desired bit field. It is of type
   --    Unsigned, Long_Unsigned, or Long_Long_Unsigned, and is either the
   --    entire value, for the small static case, or the proper selected byte
   --    from the array in the large or dynamic case. This node is analyzed
   --    and resolved on return.
   --
   --    Shift is a node representing the shift count to be used in the
   --    rotate right instruction that positions the field for access.
   --    This node is analyzed and resolved on return.
   --
   --    Cmask is a mask corresponding to the width of the component field.
   --    Its value is 2 ** Csize - 1 (e.g. 2#1111# for component size of 4).
   --
   --  Note: in some cases the call to this routine may generate actions
   --  (for handling multi-use references and the generation of the packed
   --  array type on the fly). Such actions are inserted into the tree
   --  directly using Insert_Action.

   function Byte_Swap
     (N             : Node_Id;
      Left_Justify  : Boolean := False;
      Right_Justify : Boolean := False) return Node_Id;
   --  Wrap N in a call to a byte swapping function, with appropriate type
   --  conversions. If Left_Justify is set True, the value is left justified
   --  before swapping. If Right_Justify is set True, the value is right
   --  justified after swapping. The Etype of the returned node is an
   --  integer type of an appropriate power-of-2 size.

   ---------------
   -- Byte_Swap --
   ---------------

   function Byte_Swap
     (N             : Node_Id;
      Left_Justify  : Boolean := False;
      Right_Justify : Boolean := False) return Node_Id
   is
      Loc     : constant Source_Ptr := Sloc (N);
      T       : constant Entity_Id := Etype (N);
      T_Size  : constant Uint := RM_Size (T);

      Swap_RE : RE_Id;
      Swap_F  : Entity_Id;
      Swap_T  : Entity_Id;
      --  Swapping function

      Arg     : Node_Id;
      Swapped : Node_Id;
      Shift   : Uint;

   begin
      pragma Assert (T_Size > 8);

      if T_Size <= 16 then
         Swap_RE := RE_Bswap_16;

      elsif T_Size <= 32 then
         Swap_RE := RE_Bswap_32;

      else pragma Assert (T_Size <= 64);
         Swap_RE := RE_Bswap_64;
      end if;

      Swap_F := RTE (Swap_RE);
      Swap_T := Etype (Swap_F);
      Shift := Esize (Swap_T) - T_Size;

      Arg := RJ_Unchecked_Convert_To (Swap_T, N);

      if Left_Justify and then Shift > Uint_0 then
         Arg :=
           Make_Op_Shift_Left (Loc,
             Left_Opnd  => Arg,
             Right_Opnd => Make_Integer_Literal (Loc, Shift));
      end if;

      Swapped :=
        Make_Function_Call (Loc,
          Name                   => New_Occurrence_Of (Swap_F, Loc),
          Parameter_Associations => New_List (Arg));

      if Right_Justify and then Shift > Uint_0 then
         Swapped :=
           Make_Op_Shift_Right (Loc,
             Left_Opnd  => Swapped,
             Right_Opnd => Make_Integer_Literal (Loc, Shift));
      end if;

      Set_Etype (Swapped, Swap_T);
      return Swapped;
   end Byte_Swap;

   ------------------------------
   -- Compute_Linear_Subscript --
   ------------------------------

   procedure Compute_Linear_Subscript
     (Atyp   : Entity_Id;
      N      : Node_Id;
      Subscr : out Node_Id)
   is
      Loc    : constant Source_Ptr := Sloc (N);
      Oldsub : Node_Id;
      Newsub : Node_Id;
      Indx   : Node_Id;
      Styp   : Entity_Id;

   begin
      Subscr := Empty;

      --  Loop through dimensions

      Indx   := First_Index (Atyp);
      Oldsub := First (Expressions (N));

      while Present (Indx) loop
         Styp := Etype (Indx);
         Newsub := Relocate_Node (Oldsub);

         --  Get expression for the subscript value. First, if Do_Range_Check
         --  is set on a subscript, then we must do a range check against the
         --  original bounds (not the bounds of the packed array type). We do
         --  this by introducing a subtype conversion.

         if Do_Range_Check (Newsub)
           and then Etype (Newsub) /= Styp
         then
            Newsub := Convert_To (Styp, Newsub);
         end if;

         --  Now evolve the expression for the subscript. First convert
         --  the subscript to be zero based and of an integer type.

         --  Case of integer type, where we just subtract to get lower bound

         if Is_Integer_Type (Styp) then

            --  If length of integer type is smaller than standard integer,
            --  then we convert to integer first, then do the subtract

            --  Integer (subscript) - Integer (Styp'First)

            if Esize (Styp) < Esize (Standard_Integer) then
               Newsub :=
                 Make_Op_Subtract (Loc,
                   Left_Opnd => Convert_To (Standard_Integer, Newsub),
                 Right_Opnd =>
                   Convert_To (Standard_Integer,
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (Styp, Loc),
                       Attribute_Name => Name_First)));

            --  For larger integer types, subtract first, then convert to
            --  integer, this deals with strange long long integer bounds.

            --    Integer (subscript - Styp'First)

            else
               Newsub :=
                 Convert_To (Standard_Integer,
                   Make_Op_Subtract (Loc,
                     Left_Opnd => Newsub,
                   Right_Opnd =>
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (Styp, Loc),
                       Attribute_Name => Name_First)));
            end if;

         --  For the enumeration case, we have to use 'Pos to get the value
         --  to work with before subtracting the lower bound.

         --    Integer (Styp'Pos (subscr)) - Integer (Styp'Pos (Styp'First));

         --  This is not quite right for bizarre cases where the size of the
         --  enumeration type is > Integer'Size bits due to rep clause ???

         else
            pragma Assert (Is_Enumeration_Type (Styp));

            Newsub :=
              Make_Op_Subtract (Loc,
                Left_Opnd => Convert_To (Standard_Integer,
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Occurrence_Of (Styp, Loc),
                    Attribute_Name => Name_Pos,
                    Expressions    => New_List (Newsub))),

                Right_Opnd =>
                  Convert_To (Standard_Integer,
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Styp, Loc),
                      Attribute_Name => Name_Pos,
                      Expressions    => New_List (
                        Make_Attribute_Reference (Loc,
                          Prefix         => New_Occurrence_Of (Styp, Loc),
                          Attribute_Name => Name_First)))));
         end if;

         Set_Paren_Count (Newsub, 1);

         --  For the first subscript, we just copy that subscript value

         if No (Subscr) then
            Subscr := Newsub;

         --  Otherwise, we must multiply what we already have by the current
         --  stride and then add in the new value to the evolving subscript.

         else
            Subscr :=
              Make_Op_Add (Loc,
                Left_Opnd =>
                  Make_Op_Multiply (Loc,
                    Left_Opnd  => Subscr,
                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Range_Length,
                        Prefix         => New_Occurrence_Of (Styp, Loc))),
                Right_Opnd => Newsub);
         end if;

         --  Move to next subscript

         Next_Index (Indx);
         Next (Oldsub);
      end loop;
   end Compute_Linear_Subscript;

   -------------------------
   -- Convert_To_PAT_Type --
   -------------------------

   --  The PAT is always obtained from the actual subtype

   procedure Convert_To_PAT_Type (Aexp : Node_Id) is
      Act_ST : Entity_Id;

   begin
      Convert_To_Actual_Subtype (Aexp);
      Act_ST := Underlying_Type (Etype (Aexp));
      Create_Packed_Array_Type (Act_ST);

      --  Just replace the etype with the packed array type. This works because
      --  the expression will not be further analyzed, and Gigi considers the
      --  two types equivalent in any case.

      --  This is not strictly the case ??? If the reference is an actual in
      --  call, the expansion of the prefix is delayed, and must be reanalyzed,
      --  see Reset_Packed_Prefix. On the other hand, if the prefix is a simple
      --  array reference, reanalysis can produce spurious type errors when the
      --  PAT type is replaced again with the original type of the array. Same
      --  for the case of a dereference. Ditto for function calls: expansion
      --  may introduce additional actuals which will trigger errors if call is
      --  reanalyzed. The following is correct and minimal, but the handling of
      --  more complex packed expressions in actuals is confused. Probably the
      --  problem only remains for actuals in calls.

      Set_Etype (Aexp, Packed_Array_Type (Act_ST));

      if Is_Entity_Name (Aexp)
        or else
           (Nkind (Aexp) = N_Indexed_Component
             and then Is_Entity_Name (Prefix (Aexp)))
        or else Nkind_In (Aexp, N_Explicit_Dereference, N_Function_Call)
      then
         Set_Analyzed (Aexp);
      end if;
   end Convert_To_PAT_Type;

   ------------------------------
   -- Create_Packed_Array_Type --
   ------------------------------

   procedure Create_Packed_Array_Type (Typ : Entity_Id) is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Ctyp     : constant Entity_Id  := Component_Type (Typ);
      Csize    : constant Uint       := Component_Size (Typ);

      Ancest   : Entity_Id;
      PB_Type  : Entity_Id;
      PASize   : Uint;
      Decl     : Node_Id;
      PAT      : Entity_Id;
      Len_Dim  : Node_Id;
      Len_Expr : Node_Id;
      Len_Bits : Uint;
      Bits_U1  : Node_Id;
      PAT_High : Node_Id;
      Btyp     : Entity_Id;
      Lit      : Node_Id;

      procedure Install_PAT;
      --  This procedure is called with Decl set to the declaration for the
      --  packed array type. It creates the type and installs it as required.

      procedure Set_PB_Type;
      --  Sets PB_Type to Packed_Bytes{1,2,4} as required by the alignment
      --  requirements (see documentation in the spec of this package).

      -----------------
      -- Install_PAT --
      -----------------

      procedure Install_PAT is
         Pushed_Scope : Boolean := False;

      begin
         --  We do not want to put the declaration we have created in the tree
         --  since it is often hard, and sometimes impossible to find a proper
         --  place for it (the impossible case arises for a packed array type
         --  with bounds depending on the discriminant, a declaration cannot
         --  be put inside the record, and the reference to the discriminant
         --  cannot be outside the record).

         --  The solution is to analyze the declaration while temporarily
         --  attached to the tree at an appropriate point, and then we install
         --  the resulting type as an Itype in the packed array type field of
         --  the original type, so that no explicit declaration is required.

         --  Note: the packed type is created in the scope of its parent
         --  type. There are at least some cases where the current scope
         --  is deeper, and so when this is the case, we temporarily reset
         --  the scope for the definition. This is clearly safe, since the
         --  first use of the packed array type will be the implicit
         --  reference from the corresponding unpacked type when it is
         --  elaborated.

         if Is_Itype (Typ) then
            Set_Parent (Decl, Associated_Node_For_Itype (Typ));
         else
            Set_Parent (Decl, Declaration_Node (Typ));
         end if;

         if Scope (Typ) /= Current_Scope then
            Push_Scope (Scope (Typ));
            Pushed_Scope := True;
         end if;

         Set_Is_Itype (PAT, True);
         Set_Packed_Array_Type (Typ, PAT);
         Analyze (Decl, Suppress => All_Checks);

         if Pushed_Scope then
            Pop_Scope;
         end if;

         --  Set Esize and RM_Size to the actual size of the packed object
         --  Do not reset RM_Size if already set, as happens in the case of
         --  a modular type.

         if Unknown_Esize (PAT) then
            Set_Esize (PAT, PASize);
         end if;

         if Unknown_RM_Size (PAT) then
            Set_RM_Size (PAT, PASize);
         end if;

         Adjust_Esize_Alignment (PAT);

         --  Set remaining fields of packed array type

         Init_Alignment                (PAT);
         Set_Parent                    (PAT, Empty);
         Set_Associated_Node_For_Itype (PAT, Typ);
         Set_Is_Packed_Array_Type      (PAT, True);
         Set_Original_Array_Type       (PAT, Typ);

         --  We definitely do not want to delay freezing for packed array
         --  types. This is of particular importance for the itypes that
         --  are generated for record components depending on discriminants
         --  where there is no place to put the freeze node.

         Set_Has_Delayed_Freeze (PAT, False);
         Set_Has_Delayed_Freeze (Etype (PAT), False);

         --  If we did allocate a freeze node, then clear out the reference
         --  since it is obsolete (should we delete the freeze node???)

         Set_Freeze_Node (PAT, Empty);
         Set_Freeze_Node (Etype (PAT), Empty);
      end Install_PAT;

      -----------------
      -- Set_PB_Type --
      -----------------

      procedure Set_PB_Type is
      begin
         --  If the user has specified an explicit alignment for the
         --  type or component, take it into account.

         if Csize <= 2 or else Csize = 4 or else Csize mod 2 /= 0
           or else Alignment (Typ) = 1
           or else Component_Alignment (Typ) = Calign_Storage_Unit
         then
            PB_Type := RTE (RE_Packed_Bytes1);

         elsif Csize mod 4 /= 0
           or else Alignment (Typ) = 2
         then
            PB_Type := RTE (RE_Packed_Bytes2);

         else
            PB_Type := RTE (RE_Packed_Bytes4);
         end if;
      end Set_PB_Type;

   --  Start of processing for Create_Packed_Array_Type

   begin
      --  If we already have a packed array type, nothing to do

      if Present (Packed_Array_Type (Typ)) then
         return;
      end if;

      --  If our immediate ancestor subtype is constrained, and it already
      --  has a packed array type, then just share the same type, since the
      --  bounds must be the same. If the ancestor is not an array type but
      --  a private type, as can happen with multiple instantiations, create
      --  a new packed type, to avoid privacy issues.

      if Ekind (Typ) = E_Array_Subtype then
         Ancest := Ancestor_Subtype (Typ);

         if Present (Ancest)
           and then Is_Array_Type (Ancest)
           and then Is_Constrained (Ancest)
           and then Present (Packed_Array_Type (Ancest))
         then
            Set_Packed_Array_Type (Typ, Packed_Array_Type (Ancest));
            return;
         end if;
      end if;

      --  We preset the result type size from the size of the original array
      --  type, since this size clearly belongs to the packed array type. The
      --  size of the conceptual unpacked type is always set to unknown.

      PASize := RM_Size (Typ);

      --  Case of an array where at least one index is of an enumeration
      --  type with a non-standard representation, but the component size
      --  is not appropriate for bit packing. This is the case where we
      --  have Is_Packed set (we would never be in this unit otherwise),
      --  but Is_Bit_Packed_Array is false.

      --  Note that if the component size is appropriate for bit packing,
      --  then the circuit for the computation of the subscript properly
      --  deals with the non-standard enumeration type case by taking the
      --  Pos anyway.

      if not Is_Bit_Packed_Array (Typ) then

         --  Here we build a declaration:

         --    type tttP is array (index1, index2, ...) of component_type

         --  where index1, index2, are the index types. These are the same
         --  as the index types of the original array, except for the non-
         --  standard representation enumeration type case, where we have
         --  two subcases.

         --  For the unconstrained array case, we use

         --    Natural range <>

         --  For the constrained case, we use

         --    Natural range Enum_Type'Pos (Enum_Type'First) ..
         --                  Enum_Type'Pos (Enum_Type'Last);

         PAT :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Typ), 'P'));

         Set_Packed_Array_Type (Typ, PAT);

         declare
            Indexes   : constant List_Id := New_List;
            Indx      : Node_Id;
            Indx_Typ  : Entity_Id;
            Enum_Case : Boolean;
            Typedef   : Node_Id;

         begin
            Indx := First_Index (Typ);

            while Present (Indx) loop
               Indx_Typ := Etype (Indx);

               Enum_Case := Is_Enumeration_Type (Indx_Typ)
                              and then Has_Non_Standard_Rep (Indx_Typ);

               --  Unconstrained case

               if not Is_Constrained (Typ) then
                  if Enum_Case then
                     Indx_Typ := Standard_Natural;
                  end if;

                  Append_To (Indexes, New_Occurrence_Of (Indx_Typ, Loc));

               --  Constrained case

               else
                  if not Enum_Case then
                     Append_To (Indexes, New_Occurrence_Of (Indx_Typ, Loc));

                  else
                     Append_To (Indexes,
                       Make_Subtype_Indication (Loc,
                         Subtype_Mark =>
                           New_Occurrence_Of (Standard_Natural, Loc),
                         Constraint =>
                           Make_Range_Constraint (Loc,
                             Range_Expression =>
                               Make_Range (Loc,
                                 Low_Bound =>
                                   Make_Attribute_Reference (Loc,
                                     Prefix         =>
                                       New_Occurrence_Of (Indx_Typ, Loc),
                                     Attribute_Name => Name_Pos,
                                     Expressions    => New_List (
                                       Make_Attribute_Reference (Loc,
                                         Prefix         =>
                                           New_Occurrence_Of (Indx_Typ, Loc),
                                         Attribute_Name => Name_First))),

                                 High_Bound =>
                                   Make_Attribute_Reference (Loc,
                                     Prefix         =>
                                       New_Occurrence_Of (Indx_Typ, Loc),
                                     Attribute_Name => Name_Pos,
                                     Expressions    => New_List (
                                       Make_Attribute_Reference (Loc,
                                         Prefix         =>
                                           New_Occurrence_Of (Indx_Typ, Loc),
                                         Attribute_Name => Name_Last)))))));

                  end if;
               end if;

               Next_Index (Indx);
            end loop;

            if not Is_Constrained (Typ) then
               Typedef :=
                 Make_Unconstrained_Array_Definition (Loc,
                   Subtype_Marks => Indexes,
                   Component_Definition =>
                     Make_Component_Definition (Loc,
                       Aliased_Present    => False,
                       Subtype_Indication =>
                          New_Occurrence_Of (Ctyp, Loc)));

            else
               Typedef :=
                  Make_Constrained_Array_Definition (Loc,
                    Discrete_Subtype_Definitions => Indexes,
                    Component_Definition =>
                      Make_Component_Definition (Loc,
                        Aliased_Present    => False,
                        Subtype_Indication =>
                          New_Occurrence_Of (Ctyp, Loc)));
            end if;

            Decl :=
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => PAT,
                Type_Definition => Typedef);
         end;

         --  Set type as packed array type and install it

         Set_Is_Packed_Array_Type (PAT);
         Install_PAT;
         return;

      --  Case of bit-packing required for unconstrained array. We create
      --  a subtype that is equivalent to use Packed_Bytes{1,2,4} as needed.

      elsif not Is_Constrained (Typ) then
         PAT :=
           Make_Defining_Identifier (Loc,
             Chars => Make_Packed_Array_Type_Name (Typ, Csize));

         Set_Packed_Array_Type (Typ, PAT);
         Set_PB_Type;

         Decl :=
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => PAT,
               Subtype_Indication => New_Occurrence_Of (PB_Type, Loc));
         Install_PAT;
         return;

      --  Remaining code is for the case of bit-packing for constrained array

      --  The name of the packed array subtype is

      --    ttt___XPsss

      --  where sss is the component size in bits and ttt is the name of
      --  the parent packed type.

      else
         PAT :=
           Make_Defining_Identifier (Loc,
             Chars => Make_Packed_Array_Type_Name (Typ, Csize));

         Set_Packed_Array_Type (Typ, PAT);

         --  Build an expression for the length of the array in bits.
         --  This is the product of the length of each of the dimensions

         declare
            J : Nat := 1;

         begin
            Len_Expr := Empty; -- suppress junk warning

            loop
               Len_Dim :=
                 Make_Attribute_Reference (Loc,
                   Attribute_Name => Name_Length,
                   Prefix         => New_Occurrence_Of (Typ, Loc),
                   Expressions    => New_List (
                     Make_Integer_Literal (Loc, J)));

               if J = 1 then
                  Len_Expr := Len_Dim;

               else
                  Len_Expr :=
                    Make_Op_Multiply (Loc,
                      Left_Opnd  => Len_Expr,
                      Right_Opnd => Len_Dim);
               end if;

               J := J + 1;
               exit when J > Number_Dimensions (Typ);
            end loop;
         end;

         --  Temporarily attach the length expression to the tree and analyze
         --  and resolve it, so that we can test its value. We assume that the
         --  total length fits in type Integer. This expression may involve
         --  discriminants, so we treat it as a default/per-object expression.

         Set_Parent (Len_Expr, Typ);
         Preanalyze_Spec_Expression (Len_Expr, Standard_Long_Long_Integer);

         --  Use a modular type if possible. We can do this if we have
         --  static bounds, and the length is small enough, and the length
         --  is not zero. We exclude the zero length case because the size
         --  of things is always at least one, and the zero length object
         --  would have an anomalous size.

         if Compile_Time_Known_Value (Len_Expr) then
            Len_Bits := Expr_Value (Len_Expr) * Csize;

            --  Check for size known to be too large

            if Len_Bits >
              Uint_2 ** (Standard_Integer_Size - 1) * System_Storage_Unit
            then
               if System_Storage_Unit = 8 then
                  Error_Msg_N
                    ("packed array size cannot exceed " &
                     "Integer''Last bytes", Typ);
               else
                  Error_Msg_N
                    ("packed array size cannot exceed " &
                     "Integer''Last storage units", Typ);
               end if;

               --  Reset length to arbitrary not too high value to continue

               Len_Expr := Make_Integer_Literal (Loc, 65535);
               Analyze_And_Resolve (Len_Expr, Standard_Long_Long_Integer);
            end if;

            --  We normally consider small enough to mean no larger than the
            --  value of System_Max_Binary_Modulus_Power, checking that in the
            --  case of values longer than word size, we have long shifts.

            if Len_Bits > 0
              and then
                (Len_Bits <= System_Word_Size
                   or else (Len_Bits <= System_Max_Binary_Modulus_Power
                              and then Support_Long_Shifts_On_Target))
            then
               --  We can use the modular type, it has the form:

               --    subtype tttPn is btyp
               --      range 0 .. 2 ** ((Typ'Length (1)
               --                * ... * Typ'Length (n)) * Csize) - 1;

               --  The bounds are statically known, and btyp is one of the
               --  unsigned types, depending on the length.

               if Len_Bits <= Standard_Short_Short_Integer_Size then
                  Btyp := RTE (RE_Short_Short_Unsigned);

               elsif Len_Bits <= Standard_Short_Integer_Size then
                  Btyp := RTE (RE_Short_Unsigned);

               elsif Len_Bits <= Standard_Integer_Size then
                  Btyp := RTE (RE_Unsigned);

               elsif Len_Bits <= Standard_Long_Integer_Size then
                  Btyp := RTE (RE_Long_Unsigned);

               else
                  Btyp := RTE (RE_Long_Long_Unsigned);
               end if;

               Lit := Make_Integer_Literal (Loc, 2 ** Len_Bits - 1);
               Set_Print_In_Hex (Lit);

               Decl :=
                 Make_Subtype_Declaration (Loc,
                   Defining_Identifier => PAT,
                     Subtype_Indication =>
                       Make_Subtype_Indication (Loc,
                         Subtype_Mark => New_Occurrence_Of (Btyp, Loc),

                         Constraint =>
                           Make_Range_Constraint (Loc,
                             Range_Expression =>
                               Make_Range (Loc,
                                 Low_Bound =>
                                   Make_Integer_Literal (Loc, 0),
                                 High_Bound => Lit))));

               if PASize = Uint_0 then
                  PASize := Len_Bits;
               end if;

               Install_PAT;

               --  Propagate a given alignment to the modular type. This can
               --  cause it to be under-aligned, but that's OK.

               if Present (Alignment_Clause (Typ)) then
                  Set_Alignment (PAT, Alignment (Typ));
               end if;

               return;
            end if;
         end if;

         --  Could not use a modular type, for all other cases, we build
         --  a packed array subtype:

         --    subtype tttPn is
         --      System.Packed_Bytes{1,2,4} (0 .. (Bits + 7) / 8 - 1);

         --  Bits is the length of the array in bits

         Set_PB_Type;

         Bits_U1 :=
           Make_Op_Add (Loc,
             Left_Opnd =>
               Make_Op_Multiply (Loc,
                 Left_Opnd  =>
                   Make_Integer_Literal (Loc, Csize),
                 Right_Opnd => Len_Expr),

             Right_Opnd =>
               Make_Integer_Literal (Loc, 7));

         Set_Paren_Count (Bits_U1, 1);

         PAT_High :=
           Make_Op_Subtract (Loc,
             Left_Opnd =>
               Make_Op_Divide (Loc,
                 Left_Opnd => Bits_U1,
                 Right_Opnd => Make_Integer_Literal (Loc, 8)),
             Right_Opnd => Make_Integer_Literal (Loc, 1));

         Decl :=
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => PAT,
               Subtype_Indication =>
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark => New_Occurrence_Of (PB_Type, Loc),
                   Constraint =>
                     Make_Index_Or_Discriminant_Constraint (Loc,
                       Constraints => New_List (
                         Make_Range (Loc,
                           Low_Bound =>
                             Make_Integer_Literal (Loc, 0),
                           High_Bound =>
                             Convert_To (Standard_Integer, PAT_High))))));

         Install_PAT;

         --  Currently the code in this unit requires that packed arrays
         --  represented by non-modular arrays of bytes be on a byte
         --  boundary for bit sizes handled by System.Pack_nn units.
         --  That's because these units assume the array being accessed
         --  starts on a byte boundary.

         if Get_Id (UI_To_Int (Csize)) /= RE_Null then
            Set_Must_Be_On_Byte_Boundary (Typ);
         end if;
      end if;
   end Create_Packed_Array_Type;

   -----------------------------------
   -- Expand_Bit_Packed_Element_Set --
   -----------------------------------

   procedure Expand_Bit_Packed_Element_Set (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Lhs : constant Node_Id    := Name (N);

      Ass_OK : constant Boolean := Assignment_OK (Lhs);
      --  Used to preserve assignment OK status when assignment is rewritten

      Rhs : Node_Id := Expression (N);
      --  Initially Rhs is the right hand side value, it will be replaced
      --  later by an appropriate unchecked conversion for the assignment.

      Obj   : Node_Id;
      Atyp  : Entity_Id;
      PAT   : Entity_Id;
      Ctyp  : Entity_Id;
      Csiz  : Int;
      Cmask : Uint;

      Shift : Node_Id;
      --  The expression for the shift value that is required

      Shift_Used : Boolean := False;
      --  Set True if Shift has been used in the generated code at least once,
      --  so that it must be duplicated if used again.

      New_Lhs : Node_Id;
      New_Rhs : Node_Id;

      Rhs_Val_Known : Boolean;
      Rhs_Val       : Uint;
      --  If the value of the right hand side as an integer constant is
      --  known at compile time, Rhs_Val_Known is set True, and Rhs_Val
      --  contains the value. Otherwise Rhs_Val_Known is set False, and
      --  the Rhs_Val is undefined.

      function Get_Shift return Node_Id;
      --  Function used to get the value of Shift, making sure that it
      --  gets duplicated if the function is called more than once.

      ---------------
      -- Get_Shift --
      ---------------

      function Get_Shift return Node_Id is
      begin
         --  If we used the shift value already, then duplicate it. We
         --  set a temporary parent in case actions have to be inserted.

         if Shift_Used then
            Set_Parent (Shift, N);
            return Duplicate_Subexpr_No_Checks (Shift);

         --  If first time, use Shift unchanged, and set flag for first use

         else
            Shift_Used := True;
            return Shift;
         end if;
      end Get_Shift;

   --  Start of processing for Expand_Bit_Packed_Element_Set

   begin
      pragma Assert (Is_Bit_Packed_Array (Etype (Prefix (Lhs))));

      Obj := Relocate_Node (Prefix (Lhs));
      Convert_To_Actual_Subtype (Obj);
      Atyp := Etype (Obj);
      PAT  := Packed_Array_Type (Atyp);
      Ctyp := Component_Type (Atyp);
      Csiz := UI_To_Int (Component_Size (Atyp));

      --  We remove side effects, in case the rhs modifies the lhs, because we
      --  are about to transform the rhs into an expression that first READS
      --  the lhs, so we can do the necessary shifting and masking. Example:
      --  "X(2) := F(...);" where F modifies X(3). Otherwise, the side effect
      --  will be lost.

      Remove_Side_Effects (Rhs);

      --  We convert the right hand side to the proper subtype to ensure
      --  that an appropriate range check is made (since the normal range
      --  check from assignment will be lost in the transformations). This
      --  conversion is analyzed immediately so that subsequent processing
      --  can work with an analyzed Rhs (and e.g. look at its Etype)

      --  If the right-hand side is a string literal, create a temporary for
      --  it, constant-folding is not ready to wrap the bit representation
      --  of a string literal.

      if Nkind (Rhs) = N_String_Literal then
         declare
            Decl : Node_Id;
         begin
            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Make_Temporary (Loc, 'T', Rhs),
                Object_Definition   => New_Occurrence_Of (Ctyp, Loc),
                Expression          => New_Copy_Tree (Rhs));

            Insert_Actions (N, New_List (Decl));
            Rhs := New_Occurrence_Of (Defining_Identifier (Decl), Loc);
         end;
      end if;

      Rhs := Convert_To (Ctyp, Rhs);
      Set_Parent (Rhs, N);

      --  If we are building the initialization procedure for a packed array,
      --  and Initialize_Scalars is enabled, each component assignment is an
      --  out-of-range value by design.  Compile this value without checks,
      --  because a call to the array init_proc must not raise an exception.

      if Within_Init_Proc
        and then Initialize_Scalars
      then
         Analyze_And_Resolve (Rhs, Ctyp, Suppress => All_Checks);
      else
         Analyze_And_Resolve (Rhs, Ctyp);
      end if;

      --  For the AAMP target, indexing of certain packed array is passed
      --  through to the back end without expansion, because the expansion
      --  results in very inefficient code on that target. This allows the
      --  GNAAMP back end to generate specialized macros that support more
      --  efficient indexing of packed arrays with components having sizes
      --  that are small powers of two.

      if AAMP_On_Target
        and then (Csiz = 1 or else Csiz = 2 or else Csiz = 4)
      then
         return;
      end if;

      --  Case of component size 1,2,4 or any component size for the modular
      --  case. These are the cases for which we can inline the code.

      if Csiz = 1 or else Csiz = 2 or else Csiz = 4
        or else (Present (PAT) and then Is_Modular_Integer_Type (PAT))
      then
         Setup_Inline_Packed_Array_Reference (Lhs, Atyp, Obj, Cmask, Shift);

         --  The statement to be generated is:

         --    Obj := atyp!((Obj and Mask1) or (shift_left (rhs, Shift)))

         --  or in the case of a freestanding Reverse_Storage_Order object,

         --    Obj := Swap (atyp!((Swap (Obj) and Mask1)
         --                         or (shift_left (rhs, Shift))))

         --      where Mask1 is obtained by shifting Cmask left Shift bits
         --      and then complementing the result.

         --      the "and Mask1" is omitted if rhs is constant and all 1 bits

         --      the "or ..." is omitted if rhs is constant and all 0 bits

         --      rhs is converted to the appropriate type

         --      The result is converted back to the array type, since
         --      otherwise we lose knowledge of the packed nature.

         --  Determine if right side is all 0 bits or all 1 bits

         if Compile_Time_Known_Value (Rhs) then
            Rhs_Val       := Expr_Rep_Value (Rhs);
            Rhs_Val_Known := True;

         --  The following test catches the case of an unchecked conversion of
         --  an integer literal. This results from optimizing aggregates of
         --  packed types.

         elsif Nkind (Rhs) = N_Unchecked_Type_Conversion
           and then Compile_Time_Known_Value (Expression (Rhs))
         then
            Rhs_Val       := Expr_Rep_Value (Expression (Rhs));
            Rhs_Val_Known := True;

         else
            Rhs_Val       := No_Uint;
            Rhs_Val_Known := False;
         end if;

         --  Some special checks for the case where the right hand value is
         --  known at compile time. Basically we have to take care of the
         --  implicit conversion to the subtype of the component object.

         if Rhs_Val_Known then

            --  If we have a biased component type then we must manually do the
            --  biasing, since we are taking responsibility in this case for
            --  constructing the exact bit pattern to be used.

            if Has_Biased_Representation (Ctyp) then
               Rhs_Val := Rhs_Val - Expr_Rep_Value (Type_Low_Bound (Ctyp));
            end if;

            --  For a negative value, we manually convert the two's complement
            --  value to a corresponding unsigned value, so that the proper
            --  field width is maintained. If we did not do this, we would
            --  get too many leading sign bits later on.

            if Rhs_Val < 0 then
               Rhs_Val := 2 ** UI_From_Int (Csiz) + Rhs_Val;
            end if;
         end if;

         --  Now create copies removing side effects. Note that in some complex
         --  cases, this may cause the fact that we have already set a packed
         --  array type on Obj to get lost. So we save the type of Obj, and
         --  make sure it is reset properly.

         New_Lhs := Duplicate_Subexpr (Obj, Name_Req => True);
         New_Rhs := Duplicate_Subexpr_No_Checks (Obj);

         --  First we deal with the "and"

         if not Rhs_Val_Known or else Rhs_Val /= Cmask then
            declare
               Mask1 : Node_Id;
               Lit   : Node_Id;

            begin
               if Compile_Time_Known_Value (Shift) then
                  Mask1 :=
                    Make_Integer_Literal (Loc,
                      Modulus (Etype (Obj)) - 1 -
                                 (Cmask * (2 ** Expr_Value (Get_Shift))));
                  Set_Print_In_Hex (Mask1);

               else
                  Lit := Make_Integer_Literal (Loc, Cmask);
                  Set_Print_In_Hex (Lit);
                  Mask1 :=
                    Make_Op_Not (Loc,
                      Right_Opnd => Make_Shift_Left (Lit, Get_Shift));
               end if;

               New_Rhs :=
                 Make_Op_And (Loc,
                   Left_Opnd  => New_Rhs,
                   Right_Opnd => Mask1);
            end;
         end if;

         --  Then deal with the "or"

         if not Rhs_Val_Known or else Rhs_Val /= 0 then
            declare
               Or_Rhs : Node_Id;

               procedure Fixup_Rhs;
               --  Adjust Rhs by bias if biased representation for components
               --  or remove extraneous high order sign bits if signed.

               procedure Fixup_Rhs is
                  Etyp : constant Entity_Id := Etype (Rhs);

               begin
                  --  For biased case, do the required biasing by simply
                  --  converting to the biased subtype (the conversion
                  --  will generate the required bias).

                  if Has_Biased_Representation (Ctyp) then
                     Rhs := Convert_To (Ctyp, Rhs);

                  --  For a signed integer type that is not biased, generate
                  --  a conversion to unsigned to strip high order sign bits.

                  elsif Is_Signed_Integer_Type (Ctyp) then
                     Rhs := Unchecked_Convert_To (RTE (Bits_Id (Csiz)), Rhs);
                  end if;

                  --  Set Etype, since it can be referenced before the node is
                  --  completely analyzed.

                  Set_Etype (Rhs, Etyp);

                  --  We now need to do an unchecked conversion of the
                  --  result to the target type, but it is important that
                  --  this conversion be a right justified conversion and
                  --  not a left justified conversion.

                  Rhs := RJ_Unchecked_Convert_To (Etype (Obj), Rhs);
               end Fixup_Rhs;

            begin
               if Rhs_Val_Known
                 and then Compile_Time_Known_Value (Get_Shift)
               then
                  Or_Rhs :=
                    Make_Integer_Literal (Loc,
                      Rhs_Val * (2 ** Expr_Value (Get_Shift)));
                  Set_Print_In_Hex (Or_Rhs);

               else
                  --  We have to convert the right hand side to Etype (Obj).
                  --  A special case arises if what we have now is a Val
                  --  attribute reference whose expression type is Etype (Obj).
                  --  This happens for assignments of fields from the same
                  --  array. In this case we get the required right hand side
                  --  by simply removing the inner attribute reference.

                  if Nkind (Rhs) = N_Attribute_Reference
                    and then Attribute_Name (Rhs) = Name_Val
                    and then Etype (First (Expressions (Rhs))) = Etype (Obj)
                  then
                     Rhs := Relocate_Node (First (Expressions (Rhs)));
                     Fixup_Rhs;

                  --  If the value of the right hand side is a known integer
                  --  value, then just replace it by an untyped constant,
                  --  which will be properly retyped when we analyze and
                  --  resolve the expression.

                  elsif Rhs_Val_Known then

                     --  Note that Rhs_Val has already been normalized to
                     --  be an unsigned value with the proper number of bits.

                     Rhs := Make_Integer_Literal (Loc, Rhs_Val);

                  --  Otherwise we need an unchecked conversion

                  else
                     Fixup_Rhs;
                  end if;

                  Or_Rhs := Make_Shift_Left (Rhs, Get_Shift);
               end if;

               if Nkind (New_Rhs) = N_Op_And then
                  Set_Paren_Count (New_Rhs, 1);
                  Set_Etype (New_Rhs, Etype (Left_Opnd (New_Rhs)));
               end if;

               New_Rhs :=
                 Make_Op_Or (Loc,
                   Left_Opnd  => New_Rhs,
                   Right_Opnd => Or_Rhs);
            end;
         end if;

         --  Now do the rewrite

         Rewrite (N,
           Make_Assignment_Statement (Loc,
             Name       => New_Lhs,
             Expression =>
               Unchecked_Convert_To (Etype (New_Lhs), New_Rhs)));
         Set_Assignment_OK (Name (N), Ass_OK);

      --  All other component sizes for non-modular case

      else
         --  We generate

         --    Set_nn (Arr'address, Subscr, Bits_nn!(Rhs))

         --  where Subscr is the computed linear subscript

         declare
            Bits_nn : constant Entity_Id := RTE (Bits_Id (Csiz));
            Set_nn  : Entity_Id;
            Subscr  : Node_Id;
            Atyp    : Entity_Id;

         begin
            if No (Bits_nn) then

               --  Error, most likely High_Integrity_Mode restriction

               return;
            end if;

            --  Acquire proper Set entity. We use the aligned or unaligned
            --  case as appropriate.

            if Known_Aligned_Enough (Obj, Csiz) then
               Set_nn := RTE (Set_Id (Csiz));
            else
               Set_nn := RTE (SetU_Id (Csiz));
            end if;

            --  Now generate the set reference

            Obj := Relocate_Node (Prefix (Lhs));
            Convert_To_Actual_Subtype (Obj);
            Atyp := Etype (Obj);
            Compute_Linear_Subscript (Atyp, Lhs, Subscr);

            --  Below we must make the assumption that Obj is
            --  at least byte aligned, since otherwise its address
            --  cannot be taken. The assumption holds since the
            --  only arrays that can be misaligned are small packed
            --  arrays which are implemented as a modular type, and
            --  that is not the case here.

            Rewrite (N,
              Make_Procedure_Call_Statement (Loc,
                  Name => New_Occurrence_Of (Set_nn, Loc),
                  Parameter_Associations => New_List (
                    Make_Attribute_Reference (Loc,
                      Prefix         => Obj,
                      Attribute_Name => Name_Address),
                    Subscr,
                    Unchecked_Convert_To (Bits_nn,
                      Convert_To (Ctyp, Rhs)))));

         end;
      end if;

      Analyze (N, Suppress => All_Checks);
   end Expand_Bit_Packed_Element_Set;

   -------------------------------------
   -- Expand_Packed_Address_Reference --
   -------------------------------------

   procedure Expand_Packed_Address_Reference (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Base   : Node_Id;
      Offset : Node_Id;

   begin
      --  We build an expression that has the form

      --    outer_object'Address
      --      + (linear-subscript * component_size  for each array reference
      --      +  field'Bit_Position                 for each record field
      --      +  ...
      --      +  ...) / Storage_Unit;

      Get_Base_And_Bit_Offset (Prefix (N), Base, Offset);

      Rewrite (N,
        Unchecked_Convert_To (RTE (RE_Address),
          Make_Op_Add (Loc,
            Left_Opnd =>
              Unchecked_Convert_To (RTE (RE_Integer_Address),
                Make_Attribute_Reference (Loc,
                  Prefix         => Base,
                  Attribute_Name => Name_Address)),

            Right_Opnd =>
              Unchecked_Convert_To (RTE (RE_Integer_Address),
                Make_Op_Divide (Loc,
                  Left_Opnd => Offset,
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, System_Storage_Unit))))));

      Analyze_And_Resolve (N, RTE (RE_Address));
   end Expand_Packed_Address_Reference;

   ---------------------------------
   -- Expand_Packed_Bit_Reference --
   ---------------------------------

   procedure Expand_Packed_Bit_Reference (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Base   : Node_Id;
      Offset : Node_Id;

   begin
      --  We build an expression that has the form

      --    (linear-subscript * component_size      for each array reference
      --      +  field'Bit_Position                 for each record field
      --      +  ...
      --      +  ...) mod Storage_Unit;

      Get_Base_And_Bit_Offset (Prefix (N), Base, Offset);

      Rewrite (N,
        Unchecked_Convert_To (Universal_Integer,
          Make_Op_Mod (Loc,
            Left_Opnd => Offset,
            Right_Opnd => Make_Integer_Literal (Loc, System_Storage_Unit))));

      Analyze_And_Resolve (N, Universal_Integer);
   end Expand_Packed_Bit_Reference;

   ------------------------------------
   -- Expand_Packed_Boolean_Operator --
   ------------------------------------

   --  This routine expands "a op b" for the packed cases

   procedure Expand_Packed_Boolean_Operator (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      L   : constant Node_Id    := Relocate_Node (Left_Opnd  (N));
      R   : constant Node_Id    := Relocate_Node (Right_Opnd (N));

      Ltyp : Entity_Id;
      Rtyp : Entity_Id;
      PAT  : Entity_Id;

   begin
      Convert_To_Actual_Subtype (L);
      Convert_To_Actual_Subtype (R);

      Ensure_Defined (Etype (L), N);
      Ensure_Defined (Etype (R), N);

      Apply_Length_Check (R, Etype (L));

      Ltyp := Etype (L);
      Rtyp := Etype (R);

      --  Deal with silly case of XOR where the subcomponent has a range
      --  True .. True where an exception must be raised.

      if Nkind (N) = N_Op_Xor then
         Silly_Boolean_Array_Xor_Test (N, Rtyp);
      end if;

      --  Now that that silliness is taken care of, get packed array type

      Convert_To_PAT_Type (L);
      Convert_To_PAT_Type (R);

      PAT := Etype (L);

      --  For the modular case, we expand a op b into

      --    rtyp!(pat!(a) op pat!(b))

      --  where rtyp is the Etype of the left operand. Note that we do not
      --  convert to the base type, since this would be unconstrained, and
      --  hence not have a corresponding packed array type set.

      --  Note that both operands must be modular for this code to be used

      if Is_Modular_Integer_Type (PAT)
           and then
         Is_Modular_Integer_Type (Etype (R))
      then
         declare
            P : Node_Id;

         begin
            if Nkind (N) = N_Op_And then
               P := Make_Op_And (Loc, L, R);

            elsif Nkind (N) = N_Op_Or then
               P := Make_Op_Or  (Loc, L, R);

            else -- Nkind (N) = N_Op_Xor
               P := Make_Op_Xor (Loc, L, R);
            end if;

            Rewrite (N, Unchecked_Convert_To (Ltyp, P));
         end;

      --  For the array case, we insert the actions

      --    Result : Ltype;

      --    System.Bit_Ops.Bit_And/Or/Xor
      --     (Left'Address,
      --      Ltype'Length * Ltype'Component_Size;
      --      Right'Address,
      --      Rtype'Length * Rtype'Component_Size
      --      Result'Address);

      --  where Left and Right are the Packed_Bytes{1,2,4} operands and
      --  the second argument and fourth arguments are the lengths of the
      --  operands in bits. Then we replace the expression by a reference
      --  to Result.

      --  Note that if we are mixing a modular and array operand, everything
      --  works fine, since we ensure that the modular representation has the
      --  same physical layout as the array representation (that's what the
      --  left justified modular stuff in the big-endian case is about).

      else
         declare
            Result_Ent : constant Entity_Id := Make_Temporary (Loc, 'T');
            E_Id       : RE_Id;

         begin
            if Nkind (N) = N_Op_And then
               E_Id := RE_Bit_And;

            elsif Nkind (N) = N_Op_Or then
               E_Id := RE_Bit_Or;

            else -- Nkind (N) = N_Op_Xor
               E_Id := RE_Bit_Xor;
            end if;

            Insert_Actions (N, New_List (

              Make_Object_Declaration (Loc,
                Defining_Identifier => Result_Ent,
                Object_Definition => New_Occurrence_Of (Ltyp, Loc)),

              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (RTE (E_Id), Loc),
                  Parameter_Associations => New_List (

                    Make_Byte_Aligned_Attribute_Reference (Loc,
                      Prefix         => L,
                      Attribute_Name => Name_Address),

                    Make_Op_Multiply (Loc,
                      Left_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Prefix         =>
                            New_Occurrence_Of
                              (Etype (First_Index (Ltyp)), Loc),
                          Attribute_Name => Name_Range_Length),

                      Right_Opnd =>
                        Make_Integer_Literal (Loc, Component_Size (Ltyp))),

                    Make_Byte_Aligned_Attribute_Reference (Loc,
                      Prefix         => R,
                      Attribute_Name => Name_Address),

                    Make_Op_Multiply (Loc,
                      Left_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Prefix         =>
                            New_Occurrence_Of
                              (Etype (First_Index (Rtyp)), Loc),
                          Attribute_Name => Name_Range_Length),

                      Right_Opnd =>
                        Make_Integer_Literal (Loc, Component_Size (Rtyp))),

                    Make_Byte_Aligned_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (Result_Ent, Loc),
                      Attribute_Name => Name_Address)))));

            Rewrite (N,
              New_Occurrence_Of (Result_Ent, Loc));
         end;
      end if;

      Analyze_And_Resolve (N, Typ, Suppress => All_Checks);
   end Expand_Packed_Boolean_Operator;

   -------------------------------------
   -- Expand_Packed_Element_Reference --
   -------------------------------------

   procedure Expand_Packed_Element_Reference (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Obj   : Node_Id;
      Atyp  : Entity_Id;
      PAT   : Entity_Id;
      Ctyp  : Entity_Id;
      Csiz  : Int;
      Shift : Node_Id;
      Cmask : Uint;
      Lit   : Node_Id;
      Arg   : Node_Id;

   begin
      --  If the node is an actual in a call, the prefix has not been fully
      --  expanded, to account for the additional expansion for in-out actuals
      --  (see expand_actuals for details). If the prefix itself is a packed
      --  reference as well, we have to recurse to complete the transformation
      --  of the prefix.

      if Nkind (Prefix (N)) = N_Indexed_Component
        and then not Analyzed (Prefix (N))
        and then Is_Bit_Packed_Array (Etype (Prefix (Prefix (N))))
      then
         Expand_Packed_Element_Reference (Prefix (N));
      end if;

      --  If not bit packed, we have the enumeration case, which is easily
      --  dealt with (just adjust the subscripts of the indexed component)

      --  Note: this leaves the result as an indexed component, which is
      --  still a variable, so can be used in the assignment case, as is
      --  required in the enumeration case.

      if not Is_Bit_Packed_Array (Etype (Prefix (N))) then
         Setup_Enumeration_Packed_Array_Reference (N);
         return;
      end if;

      --  Remaining processing is for the bit-packed case

      Obj := Relocate_Node (Prefix (N));
      Convert_To_Actual_Subtype (Obj);
      Atyp := Etype (Obj);
      PAT  := Packed_Array_Type (Atyp);
      Ctyp := Component_Type (Atyp);
      Csiz := UI_To_Int (Component_Size (Atyp));

      --  For the AAMP target, indexing of certain packed array is passed
      --  through to the back end without expansion, because the expansion
      --  results in very inefficient code on that target. This allows the
      --  GNAAMP back end to generate specialized macros that support more
      --  efficient indexing of packed arrays with components having sizes
      --  that are small powers of two.

      if AAMP_On_Target
        and then (Csiz = 1 or else Csiz = 2 or else Csiz = 4)
      then
         return;
      end if;

      --  Case of component size 1,2,4 or any component size for the modular
      --  case. These are the cases for which we can inline the code.

      if Csiz = 1 or else Csiz = 2 or else Csiz = 4
        or else (Present (PAT) and then Is_Modular_Integer_Type (PAT))
      then
         Setup_Inline_Packed_Array_Reference (N, Atyp, Obj, Cmask, Shift);
         Lit := Make_Integer_Literal (Loc, Cmask);
         Set_Print_In_Hex (Lit);

         --  We generate a shift right to position the field, followed by a
         --  masking operation to extract the bit field, and we finally do an
         --  unchecked conversion to convert the result to the required target.

         --  Note that the unchecked conversion automatically deals with the
         --  bias if we are dealing with a biased representation. What will
         --  happen is that we temporarily generate the biased representation,
         --  but almost immediately that will be converted to the original
         --  unbiased component type, and the bias will disappear.

         Arg :=
           Make_Op_And (Loc,
             Left_Opnd  => Make_Shift_Right (Obj, Shift),
             Right_Opnd => Lit);
         Set_Etype (Arg, Ctyp);

         --  Component extraction is performed on a native endianness scalar
         --  value: if Atyp has reverse storage order, then it has been byte
         --  swapped, and if the component being extracted is itself of a
         --  composite type with reverse storage order, then we need to swap
         --  it back to its expected endianness after extraction.

         if Reverse_Storage_Order (Atyp)
           and then Esize (Atyp) > 8
           and then (Is_Record_Type (Ctyp) or else Is_Array_Type (Ctyp))
           and then Reverse_Storage_Order (Ctyp)
         then
            Arg :=
              Byte_Swap
                (Arg,
                 Left_Justify  => not Bytes_Big_Endian,
                 Right_Justify => False);
         end if;

         --  We needed to analyze this before we do the unchecked convert
         --  below, but we need it temporarily attached to the tree for
         --  this analysis (hence the temporary Set_Parent call).

         Set_Parent (Arg, Parent (N));
         Analyze_And_Resolve (Arg);

         Rewrite (N, RJ_Unchecked_Convert_To (Ctyp, Arg));

      --  All other component sizes for non-modular case

      else
         --  We generate

         --    Component_Type!(Get_nn (Arr'address, Subscr))

         --  where Subscr is the computed linear subscript

         declare
            Get_nn : Entity_Id;
            Subscr : Node_Id;

         begin
            --  Acquire proper Get entity. We use the aligned or unaligned
            --  case as appropriate.

            if Known_Aligned_Enough (Obj, Csiz) then
               Get_nn := RTE (Get_Id (Csiz));
            else
               Get_nn := RTE (GetU_Id (Csiz));
            end if;

            --  Now generate the get reference

            Compute_Linear_Subscript (Atyp, N, Subscr);

            --  Below we make the assumption that Obj is at least byte
            --  aligned, since otherwise its address cannot be taken.
            --  The assumption holds since the only arrays that can be
            --  misaligned are small packed arrays which are implemented
            --  as a modular type, and that is not the case here.

            Rewrite (N,
              Unchecked_Convert_To (Ctyp,
                Make_Function_Call (Loc,
                  Name => New_Occurrence_Of (Get_nn, Loc),
                  Parameter_Associations => New_List (
                    Make_Attribute_Reference (Loc,
                      Prefix         => Obj,
                      Attribute_Name => Name_Address),
                    Subscr))));
         end;
      end if;

      Analyze_And_Resolve (N, Ctyp, Suppress => All_Checks);

   end Expand_Packed_Element_Reference;

   ----------------------
   -- Expand_Packed_Eq --
   ----------------------

   --  Handles expansion of "=" on packed array types

   procedure Expand_Packed_Eq (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      L   : constant Node_Id    := Relocate_Node (Left_Opnd  (N));
      R   : constant Node_Id    := Relocate_Node (Right_Opnd (N));

      LLexpr : Node_Id;
      RLexpr : Node_Id;

      Ltyp : Entity_Id;
      Rtyp : Entity_Id;
      PAT  : Entity_Id;

   begin
      Convert_To_Actual_Subtype (L);
      Convert_To_Actual_Subtype (R);
      Ltyp := Underlying_Type (Etype (L));
      Rtyp := Underlying_Type (Etype (R));

      Convert_To_PAT_Type (L);
      Convert_To_PAT_Type (R);
      PAT := Etype (L);

      LLexpr :=
        Make_Op_Multiply (Loc,
          Left_Opnd =>
            Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (Ltyp, Loc),
              Attribute_Name => Name_Length),
          Right_Opnd =>
            Make_Integer_Literal (Loc, Component_Size (Ltyp)));

      RLexpr :=
        Make_Op_Multiply (Loc,
          Left_Opnd =>
            Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (Rtyp, Loc),
              Attribute_Name => Name_Length),
          Right_Opnd =>
            Make_Integer_Literal (Loc, Component_Size (Rtyp)));

      --  For the modular case, we transform the comparison to:

      --    Ltyp'Length = Rtyp'Length and then PAT!(L) = PAT!(R)

      --  where PAT is the packed array type. This works fine, since in the
      --  modular case we guarantee that the unused bits are always zeroes.
      --  We do have to compare the lengths because we could be comparing
      --  two different subtypes of the same base type.

      if Is_Modular_Integer_Type (PAT) then
         Rewrite (N,
           Make_And_Then (Loc,
             Left_Opnd =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => LLexpr,
                 Right_Opnd => RLexpr),

             Right_Opnd =>
               Make_Op_Eq (Loc,
                 Left_Opnd => L,
                 Right_Opnd => R)));

      --  For the non-modular case, we call a runtime routine

      --    System.Bit_Ops.Bit_Eq
      --      (L'Address, L_Length, R'Address, R_Length)

      --  where PAT is the packed array type, and the lengths are the lengths
      --  in bits of the original packed arrays. This routine takes care of
      --  not comparing the unused bits in the last byte.

      else
         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (RTE (RE_Bit_Eq), Loc),
             Parameter_Associations => New_List (
               Make_Byte_Aligned_Attribute_Reference (Loc,
                 Prefix         => L,
                 Attribute_Name => Name_Address),

               LLexpr,

               Make_Byte_Aligned_Attribute_Reference (Loc,
                 Prefix         => R,
                 Attribute_Name => Name_Address),

               RLexpr)));
      end if;

      Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
   end Expand_Packed_Eq;

   -----------------------
   -- Expand_Packed_Not --
   -----------------------

   --  Handles expansion of "not" on packed array types

   procedure Expand_Packed_Not (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Opnd : constant Node_Id    := Relocate_Node (Right_Opnd (N));

      Rtyp : Entity_Id;
      PAT  : Entity_Id;
      Lit  : Node_Id;

   begin
      Convert_To_Actual_Subtype (Opnd);
      Rtyp := Etype (Opnd);

      --  Deal with silly False..False and True..True subtype case

      Silly_Boolean_Array_Not_Test (N, Rtyp);

      --  Now that the silliness is taken care of, get packed array type

      Convert_To_PAT_Type (Opnd);
      PAT := Etype (Opnd);

      --  For the case where the packed array type is a modular type, "not A"
      --  expands simply into:

      --     Rtyp!(PAT!(A) xor Mask)

      --  where PAT is the packed array type, Mask is a mask of all 1 bits of
      --  length equal to the size of this packed type, and Rtyp is the actual
      --  actual subtype of the operand.

      Lit := Make_Integer_Literal (Loc, 2 ** RM_Size (PAT) - 1);
      Set_Print_In_Hex (Lit);

      if not Is_Array_Type (PAT) then
         Rewrite (N,
           Unchecked_Convert_To (Rtyp,
             Make_Op_Xor (Loc,
               Left_Opnd  => Opnd,
               Right_Opnd => Lit)));

      --  For the array case, we insert the actions

      --    Result : Typ;

      --    System.Bit_Ops.Bit_Not
      --     (Opnd'Address,
      --      Typ'Length * Typ'Component_Size,
      --      Result'Address);

      --  where Opnd is the Packed_Bytes{1,2,4} operand and the second argument
      --  is the length of the operand in bits. We then replace the expression
      --  with a reference to Result.

      else
         declare
            Result_Ent : constant Entity_Id := Make_Temporary (Loc, 'T');

         begin
            Insert_Actions (N, New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => Result_Ent,
                Object_Definition   => New_Occurrence_Of (Rtyp, Loc)),

              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (RTE (RE_Bit_Not), Loc),
                  Parameter_Associations => New_List (
                    Make_Byte_Aligned_Attribute_Reference (Loc,
                      Prefix         => Opnd,
                      Attribute_Name => Name_Address),

                    Make_Op_Multiply (Loc,
                      Left_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Prefix         =>
                            New_Occurrence_Of
                              (Etype (First_Index (Rtyp)), Loc),
                          Attribute_Name => Name_Range_Length),

                      Right_Opnd =>
                        Make_Integer_Literal (Loc, Component_Size (Rtyp))),

                    Make_Byte_Aligned_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Result_Ent, Loc),
                      Attribute_Name => Name_Address)))));

            Rewrite (N, New_Occurrence_Of (Result_Ent, Loc));
         end;
      end if;

      Analyze_And_Resolve (N, Typ, Suppress => All_Checks);
   end Expand_Packed_Not;

   -----------------------------
   -- Get_Base_And_Bit_Offset --
   -----------------------------

   procedure Get_Base_And_Bit_Offset
     (N      : Node_Id;
      Base   : out Node_Id;
      Offset : out Node_Id)
   is
      Loc    : Source_Ptr;
      Term   : Node_Id;
      Atyp   : Entity_Id;
      Subscr : Node_Id;

   begin
      Base   := N;
      Offset := Empty;

      --  We build up an expression serially that has the form

      --    linear-subscript * component_size       for each array reference
      --      +  field'Bit_Position                 for each record field
      --      +  ...

      loop
         Loc := Sloc (Base);

         if Nkind (Base) = N_Indexed_Component then
            Convert_To_Actual_Subtype (Prefix (Base));
            Atyp := Etype (Prefix (Base));
            Compute_Linear_Subscript (Atyp, Base, Subscr);

            Term :=
              Make_Op_Multiply (Loc,
                Left_Opnd => Subscr,
                Right_Opnd =>
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Atyp, Loc),
                   Attribute_Name => Name_Component_Size));

         elsif Nkind (Base) = N_Selected_Component then
            Term :=
              Make_Attribute_Reference (Loc,
                Prefix         => Selector_Name (Base),
                Attribute_Name => Name_Bit_Position);

         else
            return;
         end if;

         if No (Offset) then
            Offset := Term;

         else
            Offset :=
              Make_Op_Add (Loc,
                Left_Opnd  => Offset,
                Right_Opnd => Term);
         end if;

         Base := Prefix (Base);
      end loop;
   end Get_Base_And_Bit_Offset;

   -------------------------------------
   -- Involves_Packed_Array_Reference --
   -------------------------------------

   function Involves_Packed_Array_Reference (N : Node_Id) return Boolean is
   begin
      if Nkind (N) = N_Indexed_Component
        and then Is_Bit_Packed_Array (Etype (Prefix (N)))
      then
         return True;

      elsif Nkind (N) = N_Selected_Component then
         return Involves_Packed_Array_Reference (Prefix (N));

      else
         return False;
      end if;
   end Involves_Packed_Array_Reference;

   --------------------------
   -- Known_Aligned_Enough --
   --------------------------

   function Known_Aligned_Enough (Obj : Node_Id; Csiz : Nat) return Boolean is
      Typ : constant Entity_Id := Etype (Obj);

      function In_Partially_Packed_Record (Comp : Entity_Id) return Boolean;
      --  If the component is in a record that contains previous packed
      --  components, consider it unaligned because the back-end might
      --  choose to pack the rest of the record. Lead to less efficient code,
      --  but safer vis-a-vis of back-end choices.

      --------------------------------
      -- In_Partially_Packed_Record --
      --------------------------------

      function In_Partially_Packed_Record (Comp : Entity_Id) return Boolean is
         Rec_Type  : constant Entity_Id := Scope (Comp);
         Prev_Comp : Entity_Id;

      begin
         Prev_Comp := First_Entity (Rec_Type);
         while Present (Prev_Comp) loop
            if Is_Packed (Etype (Prev_Comp)) then
               return True;

            elsif Prev_Comp = Comp then
               return False;
            end if;

            Next_Entity (Prev_Comp);
         end loop;

         return False;
      end  In_Partially_Packed_Record;

   --  Start of processing for Known_Aligned_Enough

   begin
      --  Odd bit sizes don't need alignment anyway

      if Csiz mod 2 = 1 then
         return True;

      --  If we have a specified alignment, see if it is sufficient, if not
      --  then we can't possibly be aligned enough in any case.

      elsif Known_Alignment (Etype (Obj)) then
         --  Alignment required is 4 if size is a multiple of 4, and
         --  2 otherwise (e.g. 12 bits requires 4, 10 bits requires 2)

         if Alignment (Etype (Obj)) < 4 - (Csiz mod 4) then
            return False;
         end if;
      end if;

      --  OK, alignment should be sufficient, if object is aligned

      --  If object is strictly aligned, then it is definitely aligned

      if Strict_Alignment (Typ) then
         return True;

      --  Case of subscripted array reference

      elsif Nkind (Obj) = N_Indexed_Component then

         --  If we have a pointer to an array, then this is definitely
         --  aligned, because pointers always point to aligned versions.

         if Is_Access_Type (Etype (Prefix (Obj))) then
            return True;

         --  Otherwise, go look at the prefix

         else
            return Known_Aligned_Enough (Prefix (Obj), Csiz);
         end if;

      --  Case of record field

      elsif Nkind (Obj) = N_Selected_Component then

         --  What is significant here is whether the record type is packed

         if Is_Record_Type (Etype (Prefix (Obj)))
           and then Is_Packed (Etype (Prefix (Obj)))
         then
            return False;

         --  Or the component has a component clause which might cause
         --  the component to become unaligned (we can't tell if the
         --  backend is doing alignment computations).

         elsif Present (Component_Clause (Entity (Selector_Name (Obj)))) then
            return False;

         elsif In_Partially_Packed_Record (Entity (Selector_Name (Obj))) then
            return False;

         --  In all other cases, go look at prefix

         else
            return Known_Aligned_Enough (Prefix (Obj), Csiz);
         end if;

      elsif Nkind (Obj) = N_Type_Conversion then
         return Known_Aligned_Enough (Expression (Obj), Csiz);

      --  For a formal parameter, it is safer to assume that it is not
      --  aligned, because the formal may be unconstrained while the actual
      --  is constrained. In this situation, a small constrained packed
      --  array, represented in modular form, may be unaligned.

      elsif Is_Entity_Name (Obj) then
         return not Is_Formal (Entity (Obj));
      else

      --  If none of the above, must be aligned
         return True;
      end if;
   end Known_Aligned_Enough;

   ---------------------
   -- Make_Shift_Left --
   ---------------------

   function Make_Shift_Left (N : Node_Id; S : Node_Id) return Node_Id is
      Nod : Node_Id;

   begin
      if Compile_Time_Known_Value (S) and then Expr_Value (S) = 0 then
         return N;
      else
         Nod :=
           Make_Op_Shift_Left (Sloc (N),
             Left_Opnd  => N,
             Right_Opnd => S);
         Set_Shift_Count_OK (Nod, True);
         return Nod;
      end if;
   end Make_Shift_Left;

   ----------------------
   -- Make_Shift_Right --
   ----------------------

   function Make_Shift_Right (N : Node_Id; S : Node_Id) return Node_Id is
      Nod : Node_Id;

   begin
      if Compile_Time_Known_Value (S) and then Expr_Value (S) = 0 then
         return N;
      else
         Nod :=
           Make_Op_Shift_Right (Sloc (N),
             Left_Opnd  => N,
             Right_Opnd => S);
         Set_Shift_Count_OK (Nod, True);
         return Nod;
      end if;
   end Make_Shift_Right;

   -----------------------------
   -- RJ_Unchecked_Convert_To --
   -----------------------------

   function RJ_Unchecked_Convert_To
     (Typ  : Entity_Id;
      Expr : Node_Id) return Node_Id
   is
      Source_Typ : constant Entity_Id := Etype (Expr);
      Target_Typ : constant Entity_Id := Typ;

      Src : Node_Id := Expr;

      Source_Siz : Nat;
      Target_Siz : Nat;

   begin
      Source_Siz := UI_To_Int (RM_Size (Source_Typ));
      Target_Siz := UI_To_Int (RM_Size (Target_Typ));

      --  For a little-endian target type stored byte-swapped on a
      --  big-endian machine, do not mask to Target_Siz bits.

      if Bytes_Big_Endian
           and then (Is_Record_Type (Target_Typ)
                       or else
                     Is_Array_Type (Target_Typ))
           and then Reverse_Storage_Order (Target_Typ)
      then
         Source_Siz := Target_Siz;
      end if;

      --  First step, if the source type is not a discrete type, then we first
      --  convert to a modular type of the source length, since otherwise, on
      --  a big-endian machine, we get left-justification. We do it for little-
      --  endian machines as well, because there might be junk bits that are
      --  not cleared if the type is not numeric.

      if Source_Siz /= Target_Siz
        and then not Is_Discrete_Type (Source_Typ)
      then
         Src := Unchecked_Convert_To (RTE (Bits_Id (Source_Siz)), Src);
      end if;

      --  In the big endian case, if the lengths of the two types differ, then
      --  we must worry about possible left justification in the conversion,
      --  and avoiding that is what this is all about.

      if Bytes_Big_Endian and then Source_Siz /= Target_Siz then

         --  Next step. If the target is not a discrete type, then we first
         --  convert to a modular type of the target length, since otherwise,
         --  on a big-endian machine, we get left-justification.

         if not Is_Discrete_Type (Target_Typ) then
            Src := Unchecked_Convert_To (RTE (Bits_Id (Target_Siz)), Src);
         end if;
      end if;

      --  And now we can do the final conversion to the target type

      return Unchecked_Convert_To (Target_Typ, Src);
   end RJ_Unchecked_Convert_To;

   ----------------------------------------------
   -- Setup_Enumeration_Packed_Array_Reference --
   ----------------------------------------------

   --  All we have to do here is to find the subscripts that correspond to the
   --  index positions that have non-standard enumeration types and insert a
   --  Pos attribute to get the proper subscript value.

   --  Finally the prefix must be uncheck-converted to the corresponding packed
   --  array type.

   --  Note that the component type is unchanged, so we do not need to fiddle
   --  with the types (Gigi always automatically takes the packed array type if
   --  it is set, as it will be in this case).

   procedure Setup_Enumeration_Packed_Array_Reference (N : Node_Id) is
      Pfx   : constant Node_Id   := Prefix (N);
      Typ   : constant Entity_Id := Etype (N);
      Exprs : constant List_Id   := Expressions (N);
      Expr  : Node_Id;

   begin
      --  If the array is unconstrained, then we replace the array reference
      --  with its actual subtype. This actual subtype will have a packed array
      --  type with appropriate bounds.

      if not Is_Constrained (Packed_Array_Type (Etype (Pfx))) then
         Convert_To_Actual_Subtype (Pfx);
      end if;

      Expr := First (Exprs);
      while Present (Expr) loop
         declare
            Loc      : constant Source_Ptr := Sloc (Expr);
            Expr_Typ : constant Entity_Id := Etype (Expr);

         begin
            if Is_Enumeration_Type (Expr_Typ)
              and then Has_Non_Standard_Rep (Expr_Typ)
            then
               Rewrite (Expr,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Expr_Typ, Loc),
                   Attribute_Name => Name_Pos,
                   Expressions    => New_List (Relocate_Node (Expr))));
               Analyze_And_Resolve (Expr, Standard_Natural);
            end if;
         end;

         Next (Expr);
      end loop;

      Rewrite (N,
        Make_Indexed_Component (Sloc (N),
          Prefix      =>
            Unchecked_Convert_To (Packed_Array_Type (Etype (Pfx)), Pfx),
          Expressions => Exprs));

      Analyze_And_Resolve (N, Typ);
   end Setup_Enumeration_Packed_Array_Reference;

   -----------------------------------------
   -- Setup_Inline_Packed_Array_Reference --
   -----------------------------------------

   procedure Setup_Inline_Packed_Array_Reference
     (N      : Node_Id;
      Atyp   : Entity_Id;
      Obj    : in out Node_Id;
      Cmask  : out Uint;
      Shift  : out Node_Id)
   is
      Loc  : constant Source_Ptr := Sloc (N);
      PAT  : Entity_Id;
      Otyp : Entity_Id;
      Csiz : Uint;
      Osiz : Uint;

   begin
      Csiz := Component_Size (Atyp);

      Convert_To_PAT_Type (Obj);
      PAT := Etype (Obj);

      Cmask := 2 ** Csiz - 1;

      if Is_Array_Type (PAT) then
         Otyp := Component_Type (PAT);
         Osiz := Component_Size (PAT);

      else
         Otyp := PAT;

         --  In the case where the PAT is a modular type, we want the actual
         --  size in bits of the modular value we use. This is neither the
         --  Object_Size nor the Value_Size, either of which may have been
         --  reset to strange values, but rather the minimum size. Note that
         --  since this is a modular type with full range, the issue of
         --  biased representation does not arise.

         Osiz := UI_From_Int (Minimum_Size (Otyp));
      end if;

      Compute_Linear_Subscript (Atyp, N, Shift);

      --  If the component size is not 1, then the subscript must be multiplied
      --  by the component size to get the shift count.

      if Csiz /= 1 then
         Shift :=
           Make_Op_Multiply (Loc,
             Left_Opnd  => Make_Integer_Literal (Loc, Csiz),
             Right_Opnd => Shift);
      end if;

      --  If we have the array case, then this shift count must be broken down
      --  into a byte subscript, and a shift within the byte.

      if Is_Array_Type (PAT) then

         declare
            New_Shift : Node_Id;

         begin
            --  We must analyze shift, since we will duplicate it

            Set_Parent (Shift, N);
            Analyze_And_Resolve
              (Shift, Standard_Integer, Suppress => All_Checks);

            --  The shift count within the word is
            --    shift mod Osiz

            New_Shift :=
              Make_Op_Mod (Loc,
                Left_Opnd  => Duplicate_Subexpr (Shift),
                Right_Opnd => Make_Integer_Literal (Loc, Osiz));

            --  The subscript to be used on the PAT array is
            --    shift / Osiz

            Obj :=
              Make_Indexed_Component (Loc,
                Prefix => Obj,
                Expressions => New_List (
                  Make_Op_Divide (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Shift),
                    Right_Opnd => Make_Integer_Literal (Loc, Osiz))));

            Shift := New_Shift;
         end;

      --  For the modular integer case, the object to be manipulated is the
      --  entire array, so Obj is unchanged. Note that we will reset its type
      --  to PAT before returning to the caller.

      else
         null;
      end if;

      --  The one remaining step is to modify the shift count for the
      --  big-endian case. Consider the following example in a byte:

      --     xxxxxxxx  bits of byte
      --     vvvvvvvv  bits of value
      --     33221100  little-endian numbering
      --     00112233  big-endian numbering

      --  Here we have the case of 2-bit fields

      --  For the little-endian case, we already have the proper shift count
      --  set, e.g. for element 2, the shift count is 2*2 = 4.

      --  For the big endian case, we have to adjust the shift count, computing
      --  it as (N - F) - Shift, where N is the number of bits in an element of
      --  the array used to implement the packed array, F is the number of bits
      --  in a source array element, and Shift is the count so far computed.

      --  We also have to adjust if the storage order is reversed

      if Bytes_Big_Endian xor Reverse_Storage_Order (Base_Type (Atyp)) then
         Shift :=
           Make_Op_Subtract (Loc,
             Left_Opnd  => Make_Integer_Literal (Loc, Osiz - Csiz),
             Right_Opnd => Shift);
      end if;

      Set_Parent (Shift, N);
      Set_Parent (Obj, N);
      Analyze_And_Resolve (Obj,   Otyp,             Suppress => All_Checks);
      Analyze_And_Resolve (Shift, Standard_Integer, Suppress => All_Checks);

      --  Make sure final type of object is the appropriate packed type

      Set_Etype (Obj, Otyp);

   end Setup_Inline_Packed_Array_Reference;

end Exp_Pakd;
