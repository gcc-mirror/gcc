------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ V F P T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2012, Free Software Foundation, Inc.         --
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
with Einfo;    use Einfo;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem_Res;  use Sem_Res;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Urealp;   use Urealp;
with Eval_Fat; use Eval_Fat;

package body Exp_VFpt is

   --  Vax floating point format (from Vax Architecture Reference Manual
   --  version 6):

   --  Float F:
   --  --------

   --   1 1
   --   5 4             7 6            0
   --  +-+---------------+--------------+
   --  |S|     exp       |   fraction   |  A
   --  +-+---------------+--------------+
   --  |             fraction           |  A + 2
   --  +--------------------------------+

   --  bit 15 is the sign bit,
   --  bits 14:7 is the excess 128 binary exponent,
   --  bits 6:0 and 31:16 the normalized 24-bit fraction with the redundant
   --    most significant fraction bit not represented.

   --  An exponent value of 0 together with a sign bit of 0, is taken to
   --  indicate that the datum has a value of 0. Exponent values of 1 through
   --  255 indicate true binary exponents of -127 to +127. An exponent value
   --  of 0, together with a sign bit of 1, is taken as reserved.

   --  Note that fraction bits are not continuous in memory, VAX is little
   --  endian (LSB first).

   --  Float D:
   --  --------

   --   1 1
   --   5 4             7 6            0
   --  +-+---------------+--------------+
   --  |S|     exp       |   fraction   |  A
   --  +-+---------------+--------------+
   --  |             fraction           |  A + 2
   --  +--------------------------------+
   --  |             fraction           |  A + 4
   --  +--------------------------------+
   --  |             fraction (low)     |  A + 6
   --  +--------------------------------+

   --  Note that the fraction bits are not continuous in memory. Bytes in a
   --  words are stored in little endian format, but words are stored using
   --  big endian format (PDP endian).

   --  Like Float F but with 55 bits for the fraction.

   --  Float G:
   --  --------

   --   1 1
   --   5 4                   4 3      0
   --  +-+---------------------+--------+
   --  |S|     exp             |  fract |  A
   --  +-+---------------------+--------+
   --  |             fraction           |  A + 2
   --  +--------------------------------+
   --  |             fraction           |  A + 4
   --  +--------------------------------+
   --  |             fraction (low)     |  A + 6
   --  +--------------------------------+

   --  Exponent values of 1 through 2047 indicate true binary exponents of
   --  -1023 to +1023.

   --  Main differences compared to IEEE 754:

   --  * No denormalized numbers
   --  * No infinity
   --  * No NaN
   --  * No -0.0
   --  * Reserved values (exp = 0, sign = 1)
   --  * Vax mantissa represent values [0.5, 1)
   --  * Bias is shifted by 1 (for single float: 128 on Vax, 127 on IEEE)

   VAXFF_Digits : constant := 6;
   VAXDF_Digits : constant := 9;
   VAXGF_Digits : constant := 15;

   ----------------------
   -- Expand_Vax_Arith --
   ----------------------

   procedure Expand_Vax_Arith (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Base_Type (Etype (N));
      Typc  : Character;
      Atyp  : Entity_Id;
      Func  : RE_Id;
      Args  : List_Id;

   begin
      --  Get arithmetic type, note that we do D stuff in G

      if Digits_Value (Typ) = VAXFF_Digits then
         Typc := 'F';
         Atyp := RTE (RE_F);
      else
         Typc := 'G';
         Atyp := RTE (RE_G);
      end if;

      case Nkind (N) is

         when N_Op_Abs =>
            if Typc = 'F' then
               Func := RE_Abs_F;
            else
               Func := RE_Abs_G;
            end if;

         when N_Op_Add =>
            if Typc = 'F' then
               Func := RE_Add_F;
            else
               Func := RE_Add_G;
            end if;

         when N_Op_Divide =>
            if Typc = 'F' then
               Func := RE_Div_F;
            else
               Func := RE_Div_G;
            end if;

         when N_Op_Multiply =>
            if Typc = 'F' then
               Func := RE_Mul_F;
            else
               Func := RE_Mul_G;
            end if;

         when N_Op_Minus =>
            if Typc = 'F' then
               Func := RE_Neg_F;
            else
               Func := RE_Neg_G;
            end if;

         when N_Op_Subtract =>
            if Typc = 'F' then
               Func := RE_Sub_F;
            else
               Func := RE_Sub_G;
            end if;

         when others =>
            Func := RE_Null;
            raise Program_Error;

      end case;

      Args := New_List;

      if Nkind (N) in N_Binary_Op then
         Append_To (Args,
           Convert_To (Atyp, Left_Opnd (N)));
      end if;

      Append_To (Args,
        Convert_To (Atyp, Right_Opnd (N)));

      Rewrite (N,
        Convert_To (Typ,
          Make_Function_Call (Loc,
            Name => New_Occurrence_Of (RTE (Func), Loc),
            Parameter_Associations => Args)));

      Analyze_And_Resolve (N, Typ, Suppress => All_Checks);
   end Expand_Vax_Arith;

   ---------------------------
   -- Expand_Vax_Comparison --
   ---------------------------

   procedure Expand_Vax_Comparison (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Base_Type (Etype (Left_Opnd (N)));
      Typc  : Character;
      Func  : RE_Id;
      Atyp  : Entity_Id;
      Revrs : Boolean := False;
      Args  : List_Id;

   begin
      --  Get arithmetic type, note that we do D stuff in G

      if Digits_Value (Typ) = VAXFF_Digits then
         Typc := 'F';
         Atyp := RTE (RE_F);
      else
         Typc := 'G';
         Atyp := RTE (RE_G);
      end if;

      case Nkind (N) is

         when N_Op_Eq =>
            if Typc = 'F' then
               Func := RE_Eq_F;
            else
               Func := RE_Eq_G;
            end if;

         when N_Op_Ge =>
            if Typc = 'F' then
               Func := RE_Le_F;
            else
               Func := RE_Le_G;
            end if;

            Revrs := True;

         when N_Op_Gt =>
            if Typc = 'F' then
               Func := RE_Lt_F;
            else
               Func := RE_Lt_G;
            end if;

            Revrs := True;

         when N_Op_Le =>
            if Typc = 'F' then
               Func := RE_Le_F;
            else
               Func := RE_Le_G;
            end if;

         when N_Op_Lt =>
            if Typc = 'F' then
               Func := RE_Lt_F;
            else
               Func := RE_Lt_G;
            end if;

         when N_Op_Ne =>
            if Typc = 'F' then
               Func := RE_Ne_F;
            else
               Func := RE_Ne_G;
            end if;

         when others =>
            Func := RE_Null;
            raise Program_Error;

      end case;

      if not Revrs then
         Args := New_List (
           Convert_To (Atyp, Left_Opnd  (N)),
           Convert_To (Atyp, Right_Opnd (N)));

      else
         Args := New_List (
           Convert_To (Atyp, Right_Opnd (N)),
           Convert_To (Atyp, Left_Opnd  (N)));
      end if;

      Rewrite (N,
        Make_Function_Call (Loc,
          Name => New_Occurrence_Of (RTE (Func), Loc),
          Parameter_Associations => Args));

      Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
   end Expand_Vax_Comparison;

   ---------------------------
   -- Expand_Vax_Conversion --
   ---------------------------

   procedure Expand_Vax_Conversion (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Expr  : constant Node_Id    := Expression (N);
      S_Typ : constant Entity_Id  := Base_Type (Etype (Expr));
      T_Typ : constant Entity_Id  := Base_Type (Etype (N));

      CallS : RE_Id;
      CallT : RE_Id;
      Func  : RE_Id;

      function Call_Type (T : Entity_Id; Otyp : Entity_Id) return RE_Id;
      --  Given one of the two types T, determines the corresponding call
      --  type, i.e. the type to be used for the call (or the result of
      --  the call). The actual operand is converted to (or from) this type.
      --  Otyp is the other type, which is useful in figuring out the result.
      --  The result returned is the RE_Id value for the type entity.

      function Equivalent_Integer_Type (T : Entity_Id) return Entity_Id;
      --  Find the predefined integer type that has the same size as the
      --  fixed-point type T, for use in fixed/float conversions.

      ---------------
      -- Call_Type --
      ---------------

      function Call_Type (T : Entity_Id; Otyp : Entity_Id) return RE_Id is
      begin
         --  Vax float formats

         if Vax_Float (T) then
            if Digits_Value (T) = VAXFF_Digits then
               return RE_F;

            elsif Digits_Value (T) = VAXGF_Digits then
               return RE_G;

            --  For D_Float, leave it as D float if the other operand is
            --  G_Float, since this is the one conversion that is properly
            --  supported for D_Float, but otherwise, use G_Float.

            else pragma Assert (Digits_Value (T) = VAXDF_Digits);

               if Vax_Float (Otyp)
                 and then Digits_Value (Otyp) = VAXGF_Digits
               then
                  return RE_D;
               else
                  return RE_G;
               end if;
            end if;

         --  For all discrete types, use 64-bit integer

         elsif Is_Discrete_Type (T) then
            return RE_Q;

         --  For all real types (other than Vax float format), we use the
         --  IEEE float-type which corresponds in length to the other type
         --  (which is Vax Float).

         else pragma Assert (Is_Real_Type (T));

            if Digits_Value (Otyp) = VAXFF_Digits then
               return RE_S;
            else
               return RE_T;
            end if;
         end if;
      end Call_Type;

      -------------------------------------------------
      -- Expand_Multiply_Fixed_By_Fixed_Giving_Fixed --
      -------------------------------------------------

      function Equivalent_Integer_Type (T : Entity_Id) return Entity_Id is
      begin
         if Esize (T) = Esize (Standard_Long_Long_Integer) then
            return Standard_Long_Long_Integer;
         elsif Esize (T) = Esize (Standard_Long_Integer) then
            return  Standard_Long_Integer;
         else
            return Standard_Integer;
         end if;
      end Equivalent_Integer_Type;

   --  Start of processing for Expand_Vax_Conversion;

   begin
      --  If input and output are the same Vax type, we change the
      --  conversion to be an unchecked conversion and that's it.

      if Vax_Float (S_Typ) and then Vax_Float (T_Typ)
        and then Digits_Value (S_Typ) = Digits_Value (T_Typ)
      then
         Rewrite (N,
           Unchecked_Convert_To (T_Typ, Expr));

      --  Case of conversion of fixed-point type to Vax_Float type

      elsif Is_Fixed_Point_Type (S_Typ) then

         --  If Conversion_OK set, then we introduce an intermediate IEEE
         --  target type since we are expecting the code generator to handle
         --  the case of integer to IEEE float.

         if Conversion_OK (N) then
            Rewrite (N,
              Convert_To (T_Typ, OK_Convert_To (Universal_Real, Expr)));

         --  Otherwise, convert the scaled integer value to the target type,
         --  and multiply by 'Small of type.

         else
            Rewrite (N,
               Make_Op_Multiply (Loc,
                 Left_Opnd =>
                   Make_Type_Conversion (Loc,
                     Subtype_Mark => New_Occurrence_Of (T_Typ, Loc),
                     Expression   =>
                       Unchecked_Convert_To (
                         Equivalent_Integer_Type (S_Typ), Expr)),
                 Right_Opnd =>
                   Make_Real_Literal (Loc, Realval => Small_Value (S_Typ))));
         end if;

      --  Case of conversion of Vax_Float type to fixed-point type

      elsif Is_Fixed_Point_Type (T_Typ) then

         --  If Conversion_OK set, then we introduce an intermediate IEEE
         --  target type, since we are expecting the code generator to handle
         --  the case of IEEE float to integer.

         if Conversion_OK (N) then
            Rewrite (N,
              OK_Convert_To (T_Typ, Convert_To (Universal_Real, Expr)));

         --  Otherwise, multiply value by 'small of type, and convert to the
         --  corresponding integer type.

         else
            Rewrite (N,
              Unchecked_Convert_To (T_Typ,
                Make_Type_Conversion (Loc,
                  Subtype_Mark =>
                    New_Occurrence_Of (Equivalent_Integer_Type (T_Typ), Loc),
                  Expression =>
                    Make_Op_Multiply (Loc,
                      Left_Opnd => Expr,
                      Right_Opnd =>
                        Make_Real_Literal (Loc,
                          Realval => Ureal_1 / Small_Value (T_Typ))))));
         end if;

      --  All other cases

      else
         --  Compute types for call

         CallS := Call_Type (S_Typ, T_Typ);
         CallT := Call_Type (T_Typ, S_Typ);

         --  Get function and its types

         if CallS = RE_D and then CallT = RE_G then
            Func := RE_D_To_G;

         elsif CallS = RE_G and then CallT = RE_D then
            Func := RE_G_To_D;

         elsif CallS = RE_G and then CallT = RE_F then
            Func := RE_G_To_F;

         elsif CallS = RE_F and then CallT = RE_G then
            Func := RE_F_To_G;

         elsif CallS = RE_F and then CallT = RE_S then
            Func := RE_F_To_S;

         elsif CallS = RE_S and then CallT = RE_F then
            Func := RE_S_To_F;

         elsif CallS = RE_G and then CallT = RE_T then
            Func := RE_G_To_T;

         elsif CallS = RE_T and then CallT = RE_G then
            Func := RE_T_To_G;

         elsif CallS = RE_F and then CallT = RE_Q then
            Func := RE_F_To_Q;

         elsif CallS = RE_Q and then CallT = RE_F then
            Func := RE_Q_To_F;

         elsif CallS = RE_G and then CallT = RE_Q then
            Func := RE_G_To_Q;

         else pragma Assert (CallS = RE_Q and then CallT = RE_G);
            Func := RE_Q_To_G;
         end if;

         Rewrite (N,
           Convert_To (T_Typ,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (Func), Loc),
               Parameter_Associations => New_List (
                 Convert_To (RTE (CallS), Expr)))));
      end if;

      Analyze_And_Resolve (N, T_Typ, Suppress => All_Checks);
   end Expand_Vax_Conversion;

   -------------------------------
   -- Expand_Vax_Foreign_Return --
   -------------------------------

   procedure Expand_Vax_Foreign_Return (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Base_Type (Etype (N));
      Func : RE_Id;
      Args : List_Id;
      Atyp : Entity_Id;
      Rtyp : constant Entity_Id  := Etype (N);

   begin
      if Digits_Value (Typ) = VAXFF_Digits then
         Func := RE_Return_F;
         Atyp := RTE (RE_F);
      elsif Digits_Value (Typ) = VAXDF_Digits then
         Func := RE_Return_D;
         Atyp := RTE (RE_D);
      else pragma Assert (Digits_Value (Typ) = VAXGF_Digits);
         Func := RE_Return_G;
         Atyp := RTE (RE_G);
      end if;

      Args := New_List (Convert_To (Atyp, N));

      Rewrite (N,
        Convert_To (Rtyp,
          Make_Function_Call (Loc,
            Name                   => New_Occurrence_Of (RTE (Func), Loc),
            Parameter_Associations => Args)));

      Analyze_And_Resolve (N, Typ, Suppress => All_Checks);
   end Expand_Vax_Foreign_Return;

   --------------------------------
   -- Vax_Real_Literal_As_Signed --
   --------------------------------

   function Get_Vax_Real_Literal_As_Signed (N : Node_Id) return Uint is
      Btyp     : constant Entity_Id :=
                   Base_Type (Underlying_Type (Etype (N)));

      Value    : constant Ureal := Realval (N);
      Negative : Boolean;
      Fraction : UI;
      Exponent : UI;
      Res      : UI;

      Exponent_Size : Uint;
      --  Number of bits for the exponent

      Fraction_Size : Uint;
      --  Number of bits for the fraction

      Uintp_Mark : constant Uintp.Save_Mark := Mark;
      --  Use the mark & release feature to delete temporaries
   begin
      --  Extract the sign now

      Negative := UR_Is_Negative (Value);

      --  Decompose the number

      Decompose_Int (Btyp, abs Value, Fraction, Exponent, Round_Even);

      --  Number of bits for the fraction, leading fraction bit is implicit

      Fraction_Size := Machine_Mantissa_Value (Btyp) - Int'(1);

      --  Number of bits for the exponent (one bit for the sign)

      Exponent_Size := RM_Size (Btyp) - Fraction_Size - Int'(1);

      if Fraction = Uint_0 then
         --  Handle zero

         Res := Uint_0;

      elsif Exponent <= -(Uint_2 ** (Exponent_Size - 1)) then
         --  Underflow

         Res := Uint_0;
      else
         --  Check for overflow

         pragma Assert (Exponent < Uint_2 ** (Exponent_Size - 1));

         --  MSB of the fraction must be 1

         pragma Assert (Fraction / Uint_2 ** Fraction_Size = Uint_1);

         --  Remove the redudant most significant fraction bit

         Fraction := Fraction - Uint_2 ** Fraction_Size;

         --  Build the fraction part. Note that this field is in mixed
         --  endianness: words are stored using little endianness, while bytes
         --  in words are stored using big endianness.

         Res := Uint_0;
         for I in 1 .. UI_To_Int (RM_Size (Btyp)) / 16 loop
            Res := (Res * (Uint_2 ** 16)) + (Fraction mod (Uint_2 ** 16));
            Fraction := Fraction / (Uint_2 ** 16);
         end loop;

         --  The sign bit

         if Negative then
            Res := Res + Int (2**15);
         end if;

         --  The exponent

         Res := Res + (Exponent + Uint_2 ** (Exponent_Size - 1))
           * Uint_2 ** (15 - Exponent_Size);

         --  Until now, we have created an unsigned number, but an underlying
         --  type is a signed type. Convert to a signed number to avoid
         --  overflow in gigi.

         if Res >= Uint_2 ** (Exponent_Size + Fraction_Size) then
            Res := Res - Uint_2 ** (Exponent_Size + Fraction_Size + 1);
         end if;
      end if;

      Release_And_Save (Uintp_Mark, Res);

      return Res;
   end Get_Vax_Real_Literal_As_Signed;

   ----------------------
   -- Expand_Vax_Valid --
   ----------------------

   procedure Expand_Vax_Valid (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Pref : constant Node_Id    := Prefix (N);
      Ptyp : constant Entity_Id  := Root_Type (Etype (Pref));
      Rtyp : constant Entity_Id  := Etype (N);
      Vtyp : RE_Id;
      Func : RE_Id;

   begin
      if Digits_Value (Ptyp) = VAXFF_Digits then
         Func := RE_Valid_F;
         Vtyp := RE_F;
      elsif Digits_Value (Ptyp) = VAXDF_Digits then
         Func := RE_Valid_D;
         Vtyp := RE_D;
      else pragma Assert (Digits_Value (Ptyp) = VAXGF_Digits);
         Func := RE_Valid_G;
         Vtyp := RE_G;
      end if;

      Rewrite (N,
        Convert_To (Rtyp,
          Make_Function_Call (Loc,
            Name                   => New_Occurrence_Of (RTE (Func), Loc),
            Parameter_Associations => New_List (
              Convert_To (RTE (Vtyp), Pref)))));

      Analyze_And_Resolve (N);
   end Expand_Vax_Valid;

end Exp_VFpt;
