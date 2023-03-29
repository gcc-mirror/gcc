------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--       G N A T . A L T I V E C . L O W _ L E V E L _ V E C T O R S        --
--                                                                          --
--                                 B o d y                                  --
--                         (Soft Binding Version)                           --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
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

--  ??? What is exactly needed for the soft case is still a bit unclear on
--  some accounts. The expected functional equivalence with the Hard binding
--  might require tricky things to be done on some targets.

--  Examples that come to mind are endianness variations or differences in the
--  base FP model while we need the operation results to be the same as what
--  the real AltiVec instructions would do on a PowerPC.

with Ada.Numerics.Generic_Elementary_Functions;
with Interfaces;                       use Interfaces;
with System.Storage_Elements;          use System.Storage_Elements;

with GNAT.Altivec.Conversions;         use  GNAT.Altivec.Conversions;
with GNAT.Altivec.Low_Level_Interface; use  GNAT.Altivec.Low_Level_Interface;

package body GNAT.Altivec.Low_Level_Vectors is

   --  Pixel types. As defined in [PIM-2.1 Data types]:
   --  A 16-bit pixel is 1/5/5/5;
   --  A 32-bit pixel is 8/8/8/8.
   --  We use the following records as an intermediate representation, to
   --  ease computation.

   type Unsigned_1 is mod 2 ** 1;
   type Unsigned_5 is mod 2 ** 5;

   type Pixel_16 is record
      T : Unsigned_1;
      R : Unsigned_5;
      G : Unsigned_5;
      B : Unsigned_5;
   end record;

   type Pixel_32 is record
      T : unsigned_char;
      R : unsigned_char;
      G : unsigned_char;
      B : unsigned_char;
   end record;

   --  Conversions to/from the pixel records to the integer types that are
   --  actually stored into the pixel vectors:

   function To_Pixel (Source : unsigned_short) return Pixel_16;
   function To_unsigned_short (Source : Pixel_16) return unsigned_short;
   function To_Pixel (Source : unsigned_int) return Pixel_32;
   function To_unsigned_int (Source : Pixel_32) return unsigned_int;

   package C_float_Operations is
     new Ada.Numerics.Generic_Elementary_Functions (C_float);

   --  Model of the Vector Status and Control Register (VSCR), as
   --  defined in [PIM-4.1 Vector Status and Control Register]:

   VSCR : unsigned_int;

   --  Positions of the flags in VSCR(0 .. 31):

   NJ_POS   : constant := 15;
   SAT_POS  : constant := 31;

   --  To control overflows, integer operations are done on 64-bit types:

   SINT64_MIN : constant := -2 ** 63;
   SINT64_MAX : constant := 2 ** 63 - 1;
   UINT64_MAX : constant := 2 ** 64 - 1;

   type SI64 is range SINT64_MIN .. SINT64_MAX;
   type UI64 is mod UINT64_MAX + 1;

   type F64 is digits 15
     range -16#0.FFFF_FFFF_FFFF_F8#E+256 .. 16#0.FFFF_FFFF_FFFF_F8#E+256;

   function Bits
     (X    : unsigned_int;
      Low  : Natural;
      High : Natural) return unsigned_int;

   function Bits
     (X    : unsigned_short;
      Low  : Natural;
      High : Natural) return unsigned_short;

   function Bits
     (X    : unsigned_char;
      Low  : Natural;
      High : Natural) return unsigned_char;

   function Write_Bit
     (X     : unsigned_int;
      Where : Natural;
      Value : Unsigned_1) return unsigned_int;

   function Write_Bit
     (X     : unsigned_short;
      Where : Natural;
      Value : Unsigned_1) return unsigned_short;

   function Write_Bit
     (X     : unsigned_char;
      Where : Natural;
      Value : Unsigned_1) return unsigned_char;

   function NJ_Truncate (X : C_float) return C_float;
   --  If NJ and A is a denormalized number, return zero

   function Bound_Align
     (X : Integer_Address;
      Y : Integer_Address) return Integer_Address;
   --  [PIM-4.3 Notations and Conventions]
   --  Align X in a y-byte boundary and return the result

   function Rnd_To_FP_Nearest (X : F64) return C_float;
   --  [PIM-4.3 Notations and Conventions]

   function Rnd_To_FPI_Near (X : F64) return F64;

   function Rnd_To_FPI_Trunc (X : F64) return F64;

   function FP_Recip_Est (X : C_float) return C_float;
   --  [PIM-4.3 Notations and Conventions]
   --  12-bit accurate floating-point estimate of 1/x

   function ROTL
     (Value  : unsigned_char;
      Amount : Natural) return unsigned_char;
   --  [PIM-4.3 Notations and Conventions]
   --  Rotate left

   function ROTL
     (Value  : unsigned_short;
      Amount : Natural) return unsigned_short;

   function ROTL
     (Value  : unsigned_int;
      Amount : Natural) return unsigned_int;

   function Recip_SQRT_Est (X : C_float) return C_float;

   function Shift_Left
     (Value  : unsigned_char;
      Amount : Natural) return unsigned_char;
   --  [PIM-4.3 Notations and Conventions]
   --  Shift left

   function Shift_Left
     (Value  : unsigned_short;
      Amount : Natural) return unsigned_short;

   function Shift_Left
     (Value  : unsigned_int;
      Amount : Natural) return unsigned_int;

   function Shift_Right
     (Value  : unsigned_char;
      Amount : Natural) return unsigned_char;
   --  [PIM-4.3 Notations and Conventions]
   --  Shift Right

   function Shift_Right
     (Value  : unsigned_short;
      Amount : Natural) return unsigned_short;

   function Shift_Right
     (Value  : unsigned_int;
      Amount : Natural) return unsigned_int;

   Signed_Bool_False : constant := 0;
   Signed_Bool_True  : constant := -1;

   ------------------------------
   -- Signed_Operations (spec) --
   ------------------------------

   generic
      type Component_Type is range <>;
      type Index_Type is range <>;
      type Varray_Type is array (Index_Type) of Component_Type;

   package Signed_Operations is

      function Modular_Result (X : SI64) return Component_Type;

      function Saturate (X : SI64) return Component_Type;

      function Saturate (X : F64) return Component_Type;

      function Sign_Extend (X : c_int) return Component_Type;
      --  [PIM-4.3 Notations and Conventions]
      --  Sign-extend X

      function abs_vxi (A : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, abs_vxi);

      function abss_vxi (A : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, abss_vxi);

      function vaddsxs (A : Varray_Type; B : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vaddsxs);

      function vavgsx (A : Varray_Type; B : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vavgsx);

      function vcmpgtsx (A : Varray_Type; B : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vcmpgtsx);

      function lvexx (A : c_long; B : c_ptr) return Varray_Type;
      pragma Convention (LL_Altivec, lvexx);

      function vmaxsx (A : Varray_Type;  B : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vmaxsx);

      function vmrghx (A : Varray_Type; B : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vmrghx);

      function vmrglx (A : Varray_Type; B : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vmrglx);

      function vminsx (A : Varray_Type; B : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vminsx);

      function vspltx (A : Varray_Type; B : c_int) return Varray_Type;
      pragma Convention (LL_Altivec, vspltx);

      function vspltisx (A : c_int) return Varray_Type;
      pragma Convention (LL_Altivec, vspltisx);

      type Bit_Operation is
        not null access function
        (Value  : Component_Type;
         Amount : Natural) return Component_Type;

      function vsrax
        (A          : Varray_Type;
         B          : Varray_Type;
         Shift_Func : Bit_Operation) return Varray_Type;

      procedure stvexx (A : Varray_Type; B : c_int; C : c_ptr);
      pragma Convention (LL_Altivec, stvexx);

      function vsubsxs (A : Varray_Type; B : Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vsubsxs);

      function Check_CR6 (A : c_int; D : Varray_Type) return c_int;
      --  If D is the result of a vcmp operation and A the flag for
      --  the kind of operation (e.g CR6_LT), check the predicate
      --  that corresponds to this flag.

   end Signed_Operations;

   ------------------------------
   -- Signed_Operations (body) --
   ------------------------------

   package body Signed_Operations is

      Bool_True  : constant Component_Type := Signed_Bool_True;
      Bool_False : constant Component_Type := Signed_Bool_False;

      Number_Of_Elements : constant Integer :=
                             VECTOR_BIT / Component_Type'Size;

      --------------------
      -- Modular_Result --
      --------------------

      function Modular_Result (X : SI64) return Component_Type is
         D : Component_Type;

      begin
         if X > 0 then
            D := Component_Type (UI64 (X)
                                 mod (UI64 (Component_Type'Last) + 1));
         else
            D := Component_Type ((-(UI64 (-X)
                                    mod (UI64 (Component_Type'Last) + 1))));
         end if;

         return D;
      end Modular_Result;

      --------------
      -- Saturate --
      --------------

      function Saturate (X : SI64) return Component_Type is
         D : Component_Type;

      begin
         --  Saturation, as defined in
         --  [PIM-4.1 Vector Status and Control Register]

         D := Component_Type (SI64'Max
                              (SI64 (Component_Type'First),
                               SI64'Min
                               (SI64 (Component_Type'Last),
                                X)));

         if SI64 (D) /= X then
            VSCR := Write_Bit (VSCR, SAT_POS, 1);
         end if;

         return D;
      end Saturate;

      function Saturate (X : F64) return Component_Type is
         D : Component_Type;

      begin
         --  Saturation, as defined in
         --  [PIM-4.1 Vector Status and Control Register]

         D := Component_Type (F64'Max
                              (F64 (Component_Type'First),
                               F64'Min
                               (F64 (Component_Type'Last),
                                X)));

         if F64 (D) /= X then
            VSCR := Write_Bit (VSCR, SAT_POS, 1);
         end if;

         return D;
      end Saturate;

      -----------------
      -- Sign_Extend --
      -----------------

      function Sign_Extend (X : c_int) return Component_Type is
      begin
         --  X is usually a 5-bits literal. In the case of the simulator,
         --  it is an integral parameter, so sign extension is straightforward.

         return Component_Type (X);
      end Sign_Extend;

      -------------
      -- abs_vxi --
      -------------

      function abs_vxi (A : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for K in Varray_Type'Range loop
            D (K) := (if A (K) /= Component_Type'First
                      then abs (A (K)) else Component_Type'First);
         end loop;

         return D;
      end abs_vxi;

      --------------
      -- abss_vxi --
      --------------

      function abss_vxi (A : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for K in Varray_Type'Range loop
            D (K) := Saturate (abs (SI64 (A (K))));
         end loop;

         return D;
      end abss_vxi;

      -------------
      -- vaddsxs --
      -------------

      function vaddsxs (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := Saturate (SI64 (A (J)) + SI64 (B (J)));
         end loop;

         return D;
      end vaddsxs;

      ------------
      -- vavgsx --
      ------------

      function vavgsx (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := Component_Type ((SI64 (A (J)) + SI64 (B (J)) + 1) / 2);
         end loop;

         return D;
      end vavgsx;

      --------------
      -- vcmpgtsx --
      --------------

      function vcmpgtsx
        (A : Varray_Type;
         B : Varray_Type) return Varray_Type
      is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := (if A (J) > B (J) then Bool_True else Bool_False);
         end loop;

         return D;
      end vcmpgtsx;

      -----------
      -- lvexx --
      -----------

      function lvexx (A : c_long; B : c_ptr) return Varray_Type is
         D  : Varray_Type;
         S  : Integer;
         EA : Integer_Address;
         J  : Index_Type;

      begin
         S := 16 / Number_Of_Elements;
         EA := Bound_Align (Integer_Address (A) + To_Integer (B),
                            Integer_Address (S));
         J := Index_Type (((EA mod 16) / Integer_Address (S))
                          + Integer_Address (Index_Type'First));

         declare
            Component : Component_Type;
            for Component'Address use To_Address (EA);
         begin
            D (J) := Component;
         end;

         return D;
      end lvexx;

      ------------
      -- vmaxsx --
      ------------

      function vmaxsx (A : Varray_Type;  B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := (if A (J) > B (J) then A (J) else B (J));
         end loop;

         return D;
      end vmaxsx;

      ------------
      -- vmrghx --
      ------------

      function vmrghx (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D      : Varray_Type;
         Offset : constant Integer := Integer (Index_Type'First);
         M      : constant Integer := Number_Of_Elements / 2;

      begin
         for J in 0 .. M - 1 loop
            D (Index_Type (2 * J + Offset)) := A (Index_Type (J + Offset));
            D (Index_Type (2 * J + Offset + 1)) := B (Index_Type (J + Offset));
         end loop;

         return D;
      end vmrghx;

      ------------
      -- vmrglx --
      ------------

      function vmrglx (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D      : Varray_Type;
         Offset : constant Integer := Integer (Index_Type'First);
         M      : constant Integer := Number_Of_Elements / 2;

      begin
         for J in 0 .. M - 1 loop
            D (Index_Type (2 * J + Offset)) := A (Index_Type (J + Offset + M));
            D (Index_Type (2 * J + Offset + 1)) :=
              B (Index_Type (J + Offset + M));
         end loop;

         return D;
      end vmrglx;

      ------------
      -- vminsx --
      ------------

      function vminsx (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := (if A (J) < B (J) then A (J) else B (J));
         end loop;

         return D;
      end vminsx;

      ------------
      -- vspltx --
      ------------

      function vspltx (A : Varray_Type; B : c_int) return Varray_Type is
         J : constant Integer :=
               Integer (B) mod Number_Of_Elements
           + Integer (Varray_Type'First);
         D : Varray_Type;

      begin
         for K in Varray_Type'Range loop
            D (K) := A (Index_Type (J));
         end loop;

         return D;
      end vspltx;

      --------------
      -- vspltisx --
      --------------

      function vspltisx (A : c_int) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := Sign_Extend (A);
         end loop;

         return D;
      end vspltisx;

      -----------
      -- vsrax --
      -----------

      function vsrax
        (A          : Varray_Type;
         B          : Varray_Type;
         Shift_Func : Bit_Operation) return Varray_Type
      is
         D : Varray_Type;
         S : constant Component_Type :=
               Component_Type (128 / Number_Of_Elements);

      begin
         for J in Varray_Type'Range loop
            D (J) := Shift_Func (A (J), Natural (B (J) mod S));
         end loop;

         return D;
      end vsrax;

      ------------
      -- stvexx --
      ------------

      procedure stvexx (A : Varray_Type; B : c_int; C : c_ptr) is
         S  : Integer;
         EA : Integer_Address;
         J  : Index_Type;

      begin
         S := 16 / Number_Of_Elements;
         EA := Bound_Align (Integer_Address (B) + To_Integer (C),
                            Integer_Address (S));
         J := Index_Type ((EA mod 16) / Integer_Address (S)
                          + Integer_Address (Index_Type'First));

         declare
            Component : Component_Type;
            for Component'Address use To_Address (EA);
         begin
            Component := A (J);
         end;
      end stvexx;

      -------------
      -- vsubsxs --
      -------------

      function vsubsxs (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := Saturate (SI64 (A (J)) - SI64 (B (J)));
         end loop;

         return D;
      end vsubsxs;

      ---------------
      -- Check_CR6 --
      ---------------

      function Check_CR6 (A : c_int; D : Varray_Type) return c_int is
         All_Element : Boolean := True;
         Any_Element : Boolean := False;

      begin
         for J in Varray_Type'Range loop
            All_Element := All_Element and then (D (J) = Bool_True);
            Any_Element := Any_Element or else  (D (J) = Bool_True);
         end loop;

         if A = CR6_LT then
            if All_Element then
               return 1;
            else
               return 0;
            end if;

         elsif A = CR6_EQ then
            if not Any_Element then
               return 1;
            else
               return 0;
            end if;

         elsif A = CR6_EQ_REV then
            if Any_Element then
               return 1;
            else
               return 0;
            end if;

         elsif A = CR6_LT_REV then
            if not All_Element then
               return 1;
            else
               return 0;
            end if;
         end if;

         return 0;
      end Check_CR6;

   end Signed_Operations;

   --------------------------------
   -- Unsigned_Operations (spec) --
   --------------------------------

   generic
      type Component_Type is mod <>;
      type Index_Type is range <>;
      type Varray_Type is array (Index_Type) of Component_Type;

   package Unsigned_Operations is

      function Bits
        (X    : Component_Type;
         Low  : Natural;
         High : Natural) return Component_Type;
      --  Return X [Low:High] as defined in [PIM-4.3 Notations and Conventions]
      --  using big endian bit ordering.

      function Write_Bit
        (X     : Component_Type;
         Where : Natural;
         Value : Unsigned_1) return Component_Type;
      --  Write Value into X[Where:Where] (if it fits in) and return the result
      --  (big endian bit ordering).

      function Modular_Result (X : UI64) return Component_Type;

      function Saturate (X : UI64) return Component_Type;

      function Saturate (X : F64) return Component_Type;

      function Saturate (X : SI64) return Component_Type;

      function vadduxm  (A : Varray_Type; B : Varray_Type) return Varray_Type;

      function vadduxs  (A : Varray_Type; B : Varray_Type) return Varray_Type;

      function vavgux   (A : Varray_Type; B : Varray_Type) return Varray_Type;

      function vcmpequx (A : Varray_Type; B : Varray_Type) return Varray_Type;

      function vcmpgtux (A : Varray_Type; B : Varray_Type) return Varray_Type;

      function vmaxux   (A : Varray_Type; B : Varray_Type) return Varray_Type;

      function vminux   (A : Varray_Type; B : Varray_Type) return Varray_Type;

      type Bit_Operation is
        access function
        (Value  : Component_Type;
         Amount : Natural) return Component_Type;

      function vrlx
        (A    : Varray_Type;
         B    : Varray_Type;
         ROTL : Bit_Operation) return Varray_Type;

      function vsxx
        (A          : Varray_Type;
         B          : Varray_Type;
         Shift_Func : Bit_Operation) return Varray_Type;
      --  Vector shift (left or right, depending on Shift_Func)

      function vsubuxm (A : Varray_Type; B : Varray_Type) return Varray_Type;

      function vsubuxs (A : Varray_Type; B : Varray_Type) return Varray_Type;

      function Check_CR6 (A : c_int; D : Varray_Type) return c_int;
      --  If D is the result of a vcmp operation and A the flag for
      --  the kind of operation (e.g CR6_LT), check the predicate
      --  that corresponds to this flag.

   end Unsigned_Operations;

   --------------------------------
   -- Unsigned_Operations (body) --
   --------------------------------

   package body Unsigned_Operations is

      Number_Of_Elements : constant Integer :=
                             VECTOR_BIT / Component_Type'Size;

      Bool_True  : constant Component_Type := Component_Type'Last;
      Bool_False : constant Component_Type := 0;

      --------------------
      -- Modular_Result --
      --------------------

      function Modular_Result (X : UI64) return Component_Type is
         D : Component_Type;
      begin
         D := Component_Type (X mod (UI64 (Component_Type'Last) + 1));
         return D;
      end Modular_Result;

      --------------
      -- Saturate --
      --------------

      function Saturate (X : UI64) return Component_Type is
         D : Component_Type;

      begin
         --  Saturation, as defined in
         --  [PIM-4.1 Vector Status and Control Register]

         D := Component_Type (UI64'Max
                              (UI64 (Component_Type'First),
                               UI64'Min
                               (UI64 (Component_Type'Last),
                                X)));

         if UI64 (D) /= X then
            VSCR := Write_Bit (VSCR, SAT_POS, 1);
         end if;

         return D;
      end Saturate;

      function Saturate (X : SI64) return Component_Type is
         D : Component_Type;

      begin
         --  Saturation, as defined in
         --  [PIM-4.1 Vector Status and Control Register]

         D := Component_Type (SI64'Max
                              (SI64 (Component_Type'First),
                               SI64'Min
                               (SI64 (Component_Type'Last),
                                X)));

         if SI64 (D) /= X then
            VSCR := Write_Bit (VSCR, SAT_POS, 1);
         end if;

         return D;
      end Saturate;

      function Saturate (X : F64) return Component_Type is
         D : Component_Type;

      begin
         --  Saturation, as defined in
         --  [PIM-4.1 Vector Status and Control Register]

         D := Component_Type (F64'Max
                              (F64 (Component_Type'First),
                               F64'Min
                               (F64 (Component_Type'Last),
                                X)));

         if F64 (D) /= X then
            VSCR := Write_Bit (VSCR, SAT_POS, 1);
         end if;

         return D;
      end Saturate;

      ----------
      -- Bits --
      ----------

      function Bits
        (X    : Component_Type;
         Low  : Natural;
         High : Natural) return Component_Type
      is
         Mask : Component_Type := 0;

         --  The Altivec ABI uses a big endian bit ordering, and we are
         --  using little endian bit ordering for extracting bits:

         Low_LE  : constant Natural := Component_Type'Size - 1 - High;
         High_LE : constant Natural := Component_Type'Size - 1 - Low;

      begin
         pragma Assert (Low <= Component_Type'Size);
         pragma Assert (High <= Component_Type'Size);

         for J in Low_LE .. High_LE loop
            Mask := Mask or 2 ** J;
         end loop;

         return (X and Mask) / 2 ** Low_LE;
      end Bits;

      ---------------
      -- Write_Bit --
      ---------------

      function Write_Bit
        (X     : Component_Type;
         Where : Natural;
         Value : Unsigned_1) return Component_Type
      is
         Result   : Component_Type := 0;

         --  The Altivec ABI uses a big endian bit ordering, and we are
         --  using little endian bit ordering for extracting bits:

         Where_LE : constant Natural := Component_Type'Size - 1 - Where;

      begin
         pragma Assert (Where < Component_Type'Size);

         case Value is
            when 1 =>
               Result := X or 2 ** Where_LE;
            when 0 =>
               Result := X and not (2 ** Where_LE);
         end case;

         return Result;
      end Write_Bit;

      -------------
      -- vadduxm --
      -------------

      function vadduxm (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := A (J) + B (J);
         end loop;

         return D;
      end vadduxm;

      -------------
      -- vadduxs --
      -------------

      function vadduxs (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := Saturate (UI64 (A (J)) + UI64 (B (J)));
         end loop;

         return D;
      end vadduxs;

      ------------
      -- vavgux --
      ------------

      function vavgux (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := Component_Type ((UI64 (A (J)) + UI64 (B (J)) + 1) / 2);
         end loop;

         return D;
      end vavgux;

      --------------
      -- vcmpequx --
      --------------

      function vcmpequx
        (A : Varray_Type;
         B : Varray_Type) return Varray_Type
      is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := (if A (J) = B (J) then Bool_True else Bool_False);
         end loop;

         return D;
      end vcmpequx;

      --------------
      -- vcmpgtux --
      --------------

      function vcmpgtux
        (A : Varray_Type;
         B : Varray_Type) return Varray_Type
      is
         D : Varray_Type;
      begin
         for J in Varray_Type'Range loop
            D (J) := (if A (J) > B (J) then Bool_True else Bool_False);
         end loop;

         return D;
      end vcmpgtux;

      ------------
      -- vmaxux --
      ------------

      function vmaxux (A : Varray_Type;  B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := (if A (J) > B (J) then A (J) else B (J));
         end loop;

         return D;
      end vmaxux;

      ------------
      -- vminux --
      ------------

      function vminux (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := (if A (J) < B (J) then A (J) else B (J));
         end loop;

         return D;
      end vminux;

      ----------
      -- vrlx --
      ----------

      function vrlx
        (A    : Varray_Type;
         B    : Varray_Type;
         ROTL : Bit_Operation) return Varray_Type
      is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := ROTL (A (J), Natural (B (J)));
         end loop;

         return D;
      end vrlx;

      ----------
      -- vsxx --
      ----------

      function vsxx
        (A          : Varray_Type;
         B          : Varray_Type;
         Shift_Func : Bit_Operation) return Varray_Type
      is
         D : Varray_Type;
         S : constant Component_Type :=
               Component_Type (128 / Number_Of_Elements);

      begin
         for J in Varray_Type'Range loop
            D (J) := Shift_Func (A (J), Natural (B (J) mod S));
         end loop;

         return D;
      end vsxx;

      -------------
      -- vsubuxm --
      -------------

      function vsubuxm (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := A (J) - B (J);
         end loop;

         return D;
      end vsubuxm;

      -------------
      -- vsubuxs --
      -------------

      function vsubuxs (A : Varray_Type; B : Varray_Type) return Varray_Type is
         D : Varray_Type;

      begin
         for J in Varray_Type'Range loop
            D (J) := Saturate (SI64 (A (J)) - SI64 (B (J)));
         end loop;

         return D;
      end vsubuxs;

      ---------------
      -- Check_CR6 --
      ---------------

      function Check_CR6 (A : c_int; D : Varray_Type) return c_int is
         All_Element : Boolean := True;
         Any_Element : Boolean := False;

      begin
         for J in Varray_Type'Range loop
            All_Element := All_Element and then (D (J) = Bool_True);
            Any_Element := Any_Element or else  (D (J) = Bool_True);
         end loop;

         if A = CR6_LT then
            if All_Element then
               return 1;
            else
               return 0;
            end if;

         elsif A = CR6_EQ then
            if not Any_Element then
               return 1;
            else
               return 0;
            end if;

         elsif A = CR6_EQ_REV then
            if Any_Element then
               return 1;
            else
               return 0;
            end if;

         elsif A = CR6_LT_REV then
            if not All_Element then
               return 1;
            else
               return 0;
            end if;
         end if;

         return 0;
      end Check_CR6;

   end Unsigned_Operations;

   --------------------------------------
   -- Signed_Merging_Operations (spec) --
   --------------------------------------

   generic
      type Component_Type is range <>;
      type Index_Type is range <>;
      type Varray_Type is array (Index_Type) of Component_Type;
      type Double_Component_Type is range <>;
      type Double_Index_Type is range <>;
      type Double_Varray_Type is array (Double_Index_Type)
        of Double_Component_Type;

   package Signed_Merging_Operations is

      pragma Assert (Integer (Varray_Type'First)
                     = Integer (Double_Varray_Type'First));
      pragma Assert (Varray_Type'Length = 2 * Double_Varray_Type'Length);
      pragma Assert (2 * Component_Type'Size = Double_Component_Type'Size);

      function Saturate
        (X : Double_Component_Type) return Component_Type;

      function vmulxsx
        (Use_Even_Components : Boolean;
         A                   : Varray_Type;
         B                   : Varray_Type) return Double_Varray_Type;

      function vpksxss
        (A : Double_Varray_Type;
         B : Double_Varray_Type) return Varray_Type;
      pragma Convention (LL_Altivec, vpksxss);

      function vupkxsx
        (A      : Varray_Type;
         Offset : Natural) return Double_Varray_Type;

   end Signed_Merging_Operations;

   --------------------------------------
   -- Signed_Merging_Operations (body) --
   --------------------------------------

   package body Signed_Merging_Operations is

      --------------
      -- Saturate --
      --------------

      function Saturate
        (X : Double_Component_Type) return Component_Type
      is
         D : Component_Type;

      begin
         --  Saturation, as defined in
         --  [PIM-4.1 Vector Status and Control Register]

         D := Component_Type (Double_Component_Type'Max
                              (Double_Component_Type (Component_Type'First),
                               Double_Component_Type'Min
                               (Double_Component_Type (Component_Type'Last),
                                X)));

         if Double_Component_Type (D) /= X then
            VSCR := Write_Bit (VSCR, SAT_POS, 1);
         end if;

         return D;
      end Saturate;

      -------------
      -- vmulxsx --
      -------------

      function vmulxsx
        (Use_Even_Components : Boolean;
         A                   : Varray_Type;
         B                   : Varray_Type) return Double_Varray_Type
      is
         Double_Offset : Double_Index_Type;
         Offset        : Index_Type;
         D             : Double_Varray_Type;
         N             : constant Integer :=
                           Integer (Double_Index_Type'Last)
                           - Integer (Double_Index_Type'First) + 1;

      begin

         for J in 0 .. N - 1 loop
            Offset :=
              Index_Type ((if Use_Even_Components then 2 * J else 2 * J + 1) +
                          Integer (Index_Type'First));

            Double_Offset :=
              Double_Index_Type (J + Integer (Double_Index_Type'First));
            D (Double_Offset) :=
              Double_Component_Type (A (Offset)) *
              Double_Component_Type (B (Offset));
         end loop;

         return D;
      end vmulxsx;

      -------------
      -- vpksxss --
      -------------

      function vpksxss
        (A : Double_Varray_Type;
         B : Double_Varray_Type) return Varray_Type
      is
         N             : constant Index_Type :=
                           Index_Type (Double_Index_Type'Last);
         D             : Varray_Type;
         Offset        : Index_Type;
         Double_Offset : Double_Index_Type;

      begin
         for J in 0 .. N - 1 loop
            Offset := Index_Type (Integer (J) + Integer (Index_Type'First));
            Double_Offset :=
              Double_Index_Type (Integer (J)
                                 + Integer (Double_Index_Type'First));
            D (Offset) := Saturate (A (Double_Offset));
            D (Offset + N) := Saturate (B (Double_Offset));
         end loop;

         return D;
      end vpksxss;

      -------------
      -- vupkxsx --
      -------------

      function vupkxsx
        (A      : Varray_Type;
         Offset : Natural) return Double_Varray_Type
      is
         K : Index_Type;
         D : Double_Varray_Type;

      begin
         for J in Double_Varray_Type'Range loop
            K := Index_Type (Integer (J)
                             - Integer (Double_Index_Type'First)
                             + Integer (Index_Type'First)
                             + Offset);
            D (J) := Double_Component_Type (A (K));
         end loop;

         return D;
      end vupkxsx;

   end Signed_Merging_Operations;

   ----------------------------------------
   -- Unsigned_Merging_Operations (spec) --
   ----------------------------------------

   generic
      type Component_Type is mod <>;
      type Index_Type is range <>;
      type Varray_Type is array (Index_Type) of Component_Type;
      type Double_Component_Type is mod <>;
      type Double_Index_Type is range <>;
      type Double_Varray_Type is array (Double_Index_Type)
        of Double_Component_Type;

   package Unsigned_Merging_Operations is

      pragma Assert (Integer (Varray_Type'First)
                     = Integer (Double_Varray_Type'First));
      pragma Assert (Varray_Type'Length = 2 * Double_Varray_Type'Length);
      pragma Assert (2 * Component_Type'Size = Double_Component_Type'Size);

      function UI_To_UI_Mod
        (X : Double_Component_Type;
         Y : Natural) return Component_Type;

      function Saturate (X : Double_Component_Type) return Component_Type;

      function vmulxux
        (Use_Even_Components : Boolean;
         A                   : Varray_Type;
         B                   : Varray_Type) return Double_Varray_Type;

      function vpkuxum
        (A : Double_Varray_Type;
         B : Double_Varray_Type) return Varray_Type;

      function vpkuxus
        (A : Double_Varray_Type;
         B : Double_Varray_Type) return Varray_Type;

   end Unsigned_Merging_Operations;

   ----------------------------------------
   -- Unsigned_Merging_Operations (body) --
   ----------------------------------------

   package body Unsigned_Merging_Operations is

      ------------------
      -- UI_To_UI_Mod --
      ------------------

      function UI_To_UI_Mod
        (X : Double_Component_Type;
         Y : Natural) return Component_Type is
         Z : Component_Type;
      begin
         Z := Component_Type (X mod 2 ** Y);
         return Z;
      end UI_To_UI_Mod;

      --------------
      -- Saturate --
      --------------

      function Saturate (X : Double_Component_Type) return Component_Type is
         D : Component_Type;

      begin
         --  Saturation, as defined in
         --  [PIM-4.1 Vector Status and Control Register]

         D := Component_Type (Double_Component_Type'Max
                              (Double_Component_Type (Component_Type'First),
                               Double_Component_Type'Min
                               (Double_Component_Type (Component_Type'Last),
                                X)));

         if Double_Component_Type (D) /= X then
            VSCR := Write_Bit (VSCR, SAT_POS, 1);
         end if;

         return D;
      end Saturate;

      -------------
      -- vmulxux --
      -------------

      function vmulxux
        (Use_Even_Components : Boolean;
         A                   : Varray_Type;
         B                   : Varray_Type) return Double_Varray_Type
      is
         Double_Offset : Double_Index_Type;
         Offset        : Index_Type;
         D             : Double_Varray_Type;
         N             : constant Integer :=
                           Integer (Double_Index_Type'Last)
                           - Integer (Double_Index_Type'First) + 1;

      begin
         for J in 0 .. N - 1 loop
            Offset :=
              Index_Type ((if Use_Even_Components then 2 * J else 2 * J + 1) +
                          Integer (Index_Type'First));

            Double_Offset :=
              Double_Index_Type (J + Integer (Double_Index_Type'First));
            D (Double_Offset) :=
              Double_Component_Type (A (Offset)) *
              Double_Component_Type (B (Offset));
         end loop;

         return D;
      end vmulxux;

      -------------
      -- vpkuxum --
      -------------

      function vpkuxum
        (A : Double_Varray_Type;
         B : Double_Varray_Type) return Varray_Type
      is
         S             : constant Natural :=
                           Double_Component_Type'Size / 2;
         N             : constant Index_Type :=
                           Index_Type (Double_Index_Type'Last);
         D             : Varray_Type;
         Offset        : Index_Type;
         Double_Offset : Double_Index_Type;

      begin
         for J in 0 .. N - 1 loop
            Offset := Index_Type (Integer (J) + Integer (Index_Type'First));
            Double_Offset :=
              Double_Index_Type (Integer (J)
                                 + Integer (Double_Index_Type'First));
            D (Offset) := UI_To_UI_Mod (A (Double_Offset), S);
            D (Offset + N) := UI_To_UI_Mod (B (Double_Offset), S);
         end loop;

         return D;
      end vpkuxum;

      -------------
      -- vpkuxus --
      -------------

      function vpkuxus
        (A : Double_Varray_Type;
         B : Double_Varray_Type) return Varray_Type
      is
         N             : constant Index_Type :=
                           Index_Type (Double_Index_Type'Last);
         D             : Varray_Type;
         Offset        : Index_Type;
         Double_Offset : Double_Index_Type;

      begin
         for J in 0 .. N - 1 loop
            Offset := Index_Type (Integer (J) + Integer (Index_Type'First));
            Double_Offset :=
              Double_Index_Type (Integer (J)
                                 + Integer (Double_Index_Type'First));
            D (Offset) := Saturate (A (Double_Offset));
            D (Offset + N) := Saturate (B (Double_Offset));
         end loop;

         return D;
      end vpkuxus;

   end Unsigned_Merging_Operations;

   package LL_VSC_Operations is
     new Signed_Operations (signed_char,
                            Vchar_Range,
                            Varray_signed_char);

   package LL_VSS_Operations is
     new Signed_Operations (signed_short,
                            Vshort_Range,
                            Varray_signed_short);

   package LL_VSI_Operations is
     new Signed_Operations (signed_int,
                            Vint_Range,
                            Varray_signed_int);

   package LL_VUC_Operations is
     new Unsigned_Operations (unsigned_char,
                              Vchar_Range,
                              Varray_unsigned_char);

   package LL_VUS_Operations is
     new Unsigned_Operations (unsigned_short,
                              Vshort_Range,
                              Varray_unsigned_short);

   package LL_VUI_Operations is
     new Unsigned_Operations (unsigned_int,
                              Vint_Range,
                              Varray_unsigned_int);

   package LL_VSC_LL_VSS_Operations is
     new Signed_Merging_Operations (signed_char,
                                    Vchar_Range,
                                    Varray_signed_char,
                                    signed_short,
                                    Vshort_Range,
                                    Varray_signed_short);

   package LL_VSS_LL_VSI_Operations is
     new Signed_Merging_Operations (signed_short,
                                    Vshort_Range,
                                    Varray_signed_short,
                                    signed_int,
                                    Vint_Range,
                                    Varray_signed_int);

   package LL_VUC_LL_VUS_Operations is
     new Unsigned_Merging_Operations (unsigned_char,
                                      Vchar_Range,
                                      Varray_unsigned_char,
                                      unsigned_short,
                                      Vshort_Range,
                                      Varray_unsigned_short);

   package LL_VUS_LL_VUI_Operations is
     new Unsigned_Merging_Operations (unsigned_short,
                                      Vshort_Range,
                                      Varray_unsigned_short,
                                      unsigned_int,
                                      Vint_Range,
                                      Varray_unsigned_int);

   ----------
   -- Bits --
   ----------

   function Bits
     (X    : unsigned_int;
      Low  : Natural;
      High : Natural) return unsigned_int renames LL_VUI_Operations.Bits;

   function Bits
     (X    : unsigned_short;
      Low  : Natural;
      High : Natural) return unsigned_short renames LL_VUS_Operations.Bits;

   function Bits
     (X    : unsigned_char;
      Low  : Natural;
      High : Natural) return unsigned_char renames LL_VUC_Operations.Bits;

   ---------------
   -- Write_Bit --
   ---------------

   function Write_Bit
     (X     : unsigned_int;
      Where : Natural;
      Value : Unsigned_1) return unsigned_int
     renames LL_VUI_Operations.Write_Bit;

   function Write_Bit
     (X     : unsigned_short;
      Where : Natural;
      Value : Unsigned_1) return unsigned_short
     renames LL_VUS_Operations.Write_Bit;

   function Write_Bit
     (X     : unsigned_char;
      Where : Natural;
      Value : Unsigned_1) return unsigned_char
     renames LL_VUC_Operations.Write_Bit;

   -----------------
   -- Bound_Align --
   -----------------

   function Bound_Align
     (X : Integer_Address;
      Y : Integer_Address) return Integer_Address
   is
      D : Integer_Address;
   begin
      D := X - X mod Y;
      return D;
   end Bound_Align;

   -----------------
   -- NJ_Truncate --
   -----------------

   function NJ_Truncate (X : C_float) return C_float is
      D : C_float;

   begin
      if (Bits (VSCR, NJ_POS, NJ_POS) = 1)
        and then abs (X) < 2.0 ** (-126)
      then
         D := (if X < 0.0 then -0.0 else +0.0);
      else
         D := X;
      end if;

      return D;
   end NJ_Truncate;

   -----------------------
   -- Rnd_To_FP_Nearest --
   -----------------------

   function Rnd_To_FP_Nearest (X : F64) return C_float is
   begin
      return C_float (X);
   end Rnd_To_FP_Nearest;

   ---------------------
   -- Rnd_To_FPI_Near --
   ---------------------

   function Rnd_To_FPI_Near (X : F64) return F64 is
      Result  : F64;
      Ceiling : F64;

   begin
      Result := F64 (SI64 (X));

      if (F64'Ceiling (X) - X) = (X + 1.0 - F64'Ceiling (X)) then

         --  Round to even

         Ceiling := F64'Ceiling (X);
         Result :=
           (if Rnd_To_FPI_Trunc (Ceiling / 2.0) * 2.0 = Ceiling
            then Ceiling else Ceiling - 1.0);
      end if;

      return Result;
   end Rnd_To_FPI_Near;

   ----------------------
   -- Rnd_To_FPI_Trunc --
   ----------------------

   function Rnd_To_FPI_Trunc (X : F64) return F64 is
      Result : F64;

   begin
      Result := F64'Ceiling (X);

      --  Rnd_To_FPI_Trunc rounds toward 0, 'Ceiling rounds toward
      --  +Infinity

      if X > 0.0
        and then Result /= X
      then
         Result := Result - 1.0;
      end if;

      return Result;
   end Rnd_To_FPI_Trunc;

   ------------------
   -- FP_Recip_Est --
   ------------------

   function FP_Recip_Est (X : C_float) return C_float is
   begin
      --  ???  [PIM-4.4 vec_re] "For result that are not +0, -0, +Inf,
      --  -Inf, or QNaN, the estimate has a relative error no greater
      --  than one part in 4096, that is:
      --  Abs ((estimate - 1 / x) / (1 / x)) < = 1/4096"

      return NJ_Truncate (1.0 / NJ_Truncate (X));
   end FP_Recip_Est;

   ----------
   -- ROTL --
   ----------

   function ROTL
     (Value  : unsigned_char;
      Amount : Natural) return unsigned_char
   is
      Result : Unsigned_8;
   begin
      Result := Rotate_Left (Unsigned_8 (Value), Amount);
      return unsigned_char (Result);
   end ROTL;

   function ROTL
     (Value  : unsigned_short;
      Amount : Natural) return unsigned_short
   is
      Result : Unsigned_16;
   begin
      Result := Rotate_Left (Unsigned_16 (Value), Amount);
      return unsigned_short (Result);
   end ROTL;

   function ROTL
     (Value  : unsigned_int;
      Amount : Natural) return unsigned_int
   is
      Result : Unsigned_32;
   begin
      Result := Rotate_Left (Unsigned_32 (Value), Amount);
      return unsigned_int (Result);
   end ROTL;

   --------------------
   -- Recip_SQRT_Est --
   --------------------

   function Recip_SQRT_Est (X : C_float) return C_float is
      Result : C_float;

   begin
      --  ???
      --  [PIM-4.4 vec_rsqrte] the estimate has a relative error in precision
      --  no greater than one part in 4096, that is:
      --  abs ((estimate - 1 / sqrt (x)) / (1 / sqrt (x)) <= 1 / 4096"

      Result := 1.0 / NJ_Truncate (C_float_Operations.Sqrt (NJ_Truncate (X)));
      return NJ_Truncate (Result);
   end Recip_SQRT_Est;

   ----------------
   -- Shift_Left --
   ----------------

   function Shift_Left
     (Value  : unsigned_char;
      Amount : Natural) return unsigned_char
   is
      Result : Unsigned_8;
   begin
      Result := Shift_Left (Unsigned_8 (Value), Amount);
      return unsigned_char (Result);
   end Shift_Left;

   function Shift_Left
     (Value  : unsigned_short;
      Amount : Natural) return unsigned_short
   is
      Result : Unsigned_16;
   begin
      Result := Shift_Left (Unsigned_16 (Value), Amount);
      return unsigned_short (Result);
   end Shift_Left;

   function Shift_Left
     (Value  : unsigned_int;
      Amount : Natural) return unsigned_int
   is
      Result : Unsigned_32;
   begin
      Result := Shift_Left (Unsigned_32 (Value), Amount);
      return unsigned_int (Result);
   end Shift_Left;

   -----------------
   -- Shift_Right --
   -----------------

   function Shift_Right
     (Value  : unsigned_char;
      Amount : Natural) return unsigned_char
   is
      Result : Unsigned_8;
   begin
      Result := Shift_Right (Unsigned_8 (Value), Amount);
      return unsigned_char (Result);
   end Shift_Right;

   function Shift_Right
     (Value  : unsigned_short;
      Amount : Natural) return unsigned_short
   is
      Result : Unsigned_16;
   begin
      Result := Shift_Right (Unsigned_16 (Value), Amount);
      return unsigned_short (Result);
   end Shift_Right;

   function Shift_Right
     (Value  : unsigned_int;
      Amount : Natural) return unsigned_int
   is
      Result : Unsigned_32;
   begin
      Result := Shift_Right (Unsigned_32 (Value), Amount);
      return unsigned_int (Result);
   end Shift_Right;

   -------------------
   -- Shift_Right_A --
   -------------------

   generic
      type Signed_Type is range <>;
      type Unsigned_Type is mod <>;
      with function Shift_Right (Value : Unsigned_Type; Amount : Natural)
                                return Unsigned_Type;
   function Shift_Right_Arithmetic
     (Value  : Signed_Type;
      Amount : Natural) return Signed_Type;

   function Shift_Right_Arithmetic
     (Value  : Signed_Type;
      Amount : Natural) return Signed_Type
   is
   begin
      if Value > 0 then
         return Signed_Type (Shift_Right (Unsigned_Type (Value), Amount));
      else
         return -Signed_Type (Shift_Right (Unsigned_Type (-Value - 1), Amount)
                              + 1);
      end if;
   end Shift_Right_Arithmetic;

   function Shift_Right_A is new Shift_Right_Arithmetic (signed_int,
                                                         Unsigned_32,
                                                         Shift_Right);

   function Shift_Right_A is new Shift_Right_Arithmetic (signed_short,
                                                         Unsigned_16,
                                                         Shift_Right);

   function Shift_Right_A is new Shift_Right_Arithmetic (signed_char,
                                                         Unsigned_8,
                                                         Shift_Right);
   --------------
   -- To_Pixel --
   --------------

   function To_Pixel (Source : unsigned_short) return Pixel_16 is

      --  This conversion should not depend on the host endianness;
      --  therefore, we cannot use an unchecked conversion.

      Target : Pixel_16;

   begin
      Target.T := Unsigned_1 (Bits (Source, 0, 0)   mod 2 ** 1);
      Target.R := Unsigned_5 (Bits (Source, 1, 5)   mod 2 ** 5);
      Target.G := Unsigned_5 (Bits (Source, 6, 10)  mod 2 ** 5);
      Target.B := Unsigned_5 (Bits (Source, 11, 15) mod 2 ** 5);
      return Target;
   end To_Pixel;

   function To_Pixel (Source : unsigned_int) return Pixel_32 is

      --  This conversion should not depend on the host endianness;
      --  therefore, we cannot use an unchecked conversion.

      Target : Pixel_32;

   begin
      Target.T := unsigned_char (Bits (Source, 0, 7));
      Target.R := unsigned_char (Bits (Source, 8, 15));
      Target.G := unsigned_char (Bits (Source, 16, 23));
      Target.B := unsigned_char (Bits (Source, 24, 31));
      return Target;
   end To_Pixel;

   ---------------------
   -- To_unsigned_int --
   ---------------------

   function To_unsigned_int (Source : Pixel_32) return unsigned_int is

      --  This conversion should not depend on the host endianness;
      --  therefore, we cannot use an unchecked conversion.
      --  It should also be the same result, value-wise, on two hosts
      --  with the same endianness.

      Target : unsigned_int := 0;

   begin
      --  In big endian bit ordering, Pixel_32 looks like:
      --  -------------------------------------
      --  |   T    |   R    |   G    |    B   |
      --  -------------------------------------
      --  0 (MSB)  7        15       23       32
      --
      --  Sizes of the components: (8/8/8/8)
      --
      Target := Target or unsigned_int (Source.T);
      Target := Shift_Left (Target, 8);
      Target := Target or unsigned_int (Source.R);
      Target := Shift_Left (Target, 8);
      Target := Target or unsigned_int (Source.G);
      Target := Shift_Left (Target, 8);
      Target := Target or unsigned_int (Source.B);
      return Target;
   end To_unsigned_int;

   -----------------------
   -- To_unsigned_short --
   -----------------------

   function To_unsigned_short (Source : Pixel_16) return unsigned_short is

      --  This conversion should not depend on the host endianness;
      --  therefore, we cannot use an unchecked conversion.
      --  It should also be the same result, value-wise, on two hosts
      --  with the same endianness.

      Target : unsigned_short := 0;

   begin
      --  In big endian bit ordering, Pixel_16 looks like:
      --  -------------------------------------
      --  |   T    |   R    |   G    |    B   |
      --  -------------------------------------
      --  0 (MSB)  1        5        11       15
      --
      --  Sizes of the components: (1/5/5/5)
      --
      Target := Target or unsigned_short (Source.T);
      Target := Shift_Left (Target, 5);
      Target := Target or unsigned_short (Source.R);
      Target := Shift_Left (Target, 5);
      Target := Target or unsigned_short (Source.G);
      Target := Shift_Left (Target, 5);
      Target := Target or unsigned_short (Source.B);
      return Target;
   end To_unsigned_short;

   ---------------
   -- abs_v16qi --
   ---------------

   function abs_v16qi (A : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
   begin
      return To_Vector ((Values =>
                           LL_VSC_Operations.abs_vxi (VA.Values)));
   end abs_v16qi;

   --------------
   -- abs_v8hi --
   --------------

   function abs_v8hi (A : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
   begin
      return To_Vector ((Values =>
                           LL_VSS_Operations.abs_vxi (VA.Values)));
   end abs_v8hi;

   --------------
   -- abs_v4si --
   --------------

   function abs_v4si (A : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
   begin
      return To_Vector ((Values =>
                           LL_VSI_Operations.abs_vxi (VA.Values)));
   end abs_v4si;

   --------------
   -- abs_v4sf --
   --------------

   function abs_v4sf (A : LL_VF) return LL_VF is
      D  : Varray_float;
      VA : constant VF_View := To_View (A);

   begin
      for J in Varray_float'Range loop
         D (J) := abs (VA.Values (J));
      end loop;

      return To_Vector ((Values => D));
   end abs_v4sf;

   ----------------
   -- abss_v16qi --
   ----------------

   function abss_v16qi (A : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
   begin
      return To_Vector ((Values =>
                           LL_VSC_Operations.abss_vxi (VA.Values)));
   end abss_v16qi;

   ---------------
   -- abss_v8hi --
   ---------------

   function abss_v8hi (A : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
   begin
      return To_Vector ((Values =>
                           LL_VSS_Operations.abss_vxi (VA.Values)));
   end abss_v8hi;

   ---------------
   -- abss_v4si --
   ---------------

   function abss_v4si (A : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
   begin
      return To_Vector ((Values =>
                           LL_VSI_Operations.abss_vxi (VA.Values)));
   end abss_v4si;

   -------------
   -- vaddubm --
   -------------

   function vaddubm (A : LL_VSC; B : LL_VSC) return LL_VSC is
      UC : constant GNAT.Altivec.Low_Level_Vectors.LL_VUC :=
             To_LL_VUC (A);
      VA : constant VUC_View :=
             To_View (UC);
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : Varray_unsigned_char;

   begin
      D := LL_VUC_Operations.vadduxm (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (VUC_View'(Values => D)));
   end vaddubm;

   -------------
   -- vadduhm --
   -------------

   function vadduhm (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : Varray_unsigned_short;

   begin
      D := LL_VUS_Operations.vadduxm (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (VUS_View'(Values => D)));
   end vadduhm;

   -------------
   -- vadduwm --
   -------------

   function vadduwm (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : Varray_unsigned_int;

   begin
      D := LL_VUI_Operations.vadduxm (VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (VUI_View'(Values => D)));
   end vadduwm;

   ------------
   -- vaddfp --
   ------------

   function vaddfp (A : LL_VF; B : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      D  : Varray_float;

   begin
      for J in Varray_float'Range loop
         D (J) := NJ_Truncate (NJ_Truncate (VA.Values (J))
                               + NJ_Truncate (VB.Values (J)));
      end loop;

      return To_Vector (VF_View'(Values => D));
   end vaddfp;

   -------------
   -- vaddcuw --
   -------------

   function vaddcuw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      Addition_Result : UI64;
      D               : VUI_View;
      VA              : constant VUI_View := To_View (To_LL_VUI (A));
      VB              : constant VUI_View := To_View (To_LL_VUI (B));

   begin
      for J in Varray_unsigned_int'Range loop
         Addition_Result := UI64 (VA.Values (J)) + UI64 (VB.Values (J));
         D.Values (J) :=
           (if Addition_Result > UI64 (unsigned_int'Last) then 1 else 0);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vaddcuw;

   -------------
   -- vaddubs --
   -------------

   function vaddubs (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));

   begin
      return To_LL_VSC (To_Vector
                        (VUC_View'(Values =>
                                     (LL_VUC_Operations.vadduxs
                                      (VA.Values,
                                       VB.Values)))));
   end vaddubs;

   -------------
   -- vaddsbs --
   -------------

   function vaddsbs (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;

   begin
      D.Values := LL_VSC_Operations.vaddsxs (VA.Values, VB.Values);
      return To_Vector (D);
   end vaddsbs;

   -------------
   -- vadduhs --
   -------------

   function vadduhs (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;

   begin
      D.Values := LL_VUS_Operations.vadduxs (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vadduhs;

   -------------
   -- vaddshs --
   -------------

   function vaddshs (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;

   begin
      D.Values := LL_VSS_Operations.vaddsxs (VA.Values, VB.Values);
      return To_Vector (D);
   end vaddshs;

   -------------
   -- vadduws --
   -------------

   function vadduws (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;

   begin
      D.Values := LL_VUI_Operations.vadduxs (VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vadduws;

   -------------
   -- vaddsws --
   -------------

   function vaddsws (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;

   begin
      D.Values := LL_VSI_Operations.vaddsxs (VA.Values, VB.Values);
      return To_Vector (D);
   end vaddsws;

   ----------
   -- vand --
   ----------

   function vand (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;

   begin
      for J in Varray_unsigned_int'Range loop
         D.Values (J) := VA.Values (J) and VB.Values (J);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vand;

   -----------
   -- vandc --
   -----------

   function vandc (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;

   begin
      for J in Varray_unsigned_int'Range loop
         D.Values (J) := VA.Values (J) and not VB.Values (J);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vandc;

   ------------
   -- vavgub --
   ------------

   function vavgub (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;

   begin
      D.Values := LL_VUC_Operations.vavgux (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vavgub;

   ------------
   -- vavgsb --
   ------------

   function vavgsb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;

   begin
      D.Values := LL_VSC_Operations.vavgsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vavgsb;

   ------------
   -- vavguh --
   ------------

   function vavguh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;

   begin
      D.Values := LL_VUS_Operations.vavgux (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vavguh;

   ------------
   -- vavgsh --
   ------------

   function vavgsh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;

   begin
      D.Values := LL_VSS_Operations.vavgsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vavgsh;

   ------------
   -- vavguw --
   ------------

   function vavguw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;

   begin
      D.Values := LL_VUI_Operations.vavgux (VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vavguw;

   ------------
   -- vavgsw --
   ------------

   function vavgsw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;

   begin
      D.Values := LL_VSI_Operations.vavgsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vavgsw;

   -----------
   -- vrfip --
   -----------

   function vrfip (A : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      D  : VF_View;

   begin
      for J in Varray_float'Range loop

         --  If A (J) is infinite, D (J) should be infinite; With
         --  IEEE floating points, we can use 'Ceiling for that purpose.

         D.Values (J) := C_float'Ceiling (NJ_Truncate (VA.Values (J)));

      end loop;

      return To_Vector (D);
   end vrfip;

   -------------
   -- vcmpbfp --
   -------------

   function vcmpbfp (A : LL_VF; B : LL_VF) return LL_VSI is
      VA   : constant VF_View := To_View (A);
      VB   : constant VF_View := To_View (B);
      D    : VUI_View;
      K    : Vint_Range;

   begin
      for J in Varray_float'Range loop
         K := Vint_Range (J);
         D.Values (K) := 0;

         if NJ_Truncate (VB.Values (J)) < 0.0 then

            --  [PIM-4.4 vec_cmpb] "If any single-precision floating-point
            --  word element in B is negative; the corresponding element in A
            --  is out of bounds.

            D.Values (K) := Write_Bit (D.Values (K), 0, 1);
            D.Values (K) := Write_Bit (D.Values (K), 1, 1);

         else
            D.Values (K) :=
              (if NJ_Truncate (VA.Values (J)) <= NJ_Truncate (VB.Values (J))
               then Write_Bit (D.Values (K), 0, 0)
               else Write_Bit (D.Values (K), 0, 1));

            D.Values (K) :=
              (if NJ_Truncate (VA.Values (J)) >= -NJ_Truncate (VB.Values (J))
               then Write_Bit (D.Values (K), 1, 0)
               else Write_Bit (D.Values (K), 1, 1));
         end if;
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vcmpbfp;

   --------------
   -- vcmpequb --
   --------------

   function vcmpequb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;

   begin
      D.Values := LL_VUC_Operations.vcmpequx (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vcmpequb;

   --------------
   -- vcmpequh --
   --------------

   function vcmpequh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_Operations.vcmpequx (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vcmpequh;

   --------------
   -- vcmpequw --
   --------------

   function vcmpequw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values := LL_VUI_Operations.vcmpequx (VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vcmpequw;

   --------------
   -- vcmpeqfp --
   --------------

   function vcmpeqfp (A : LL_VF; B : LL_VF) return LL_VSI is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      D  : VUI_View;

   begin
      for J in Varray_float'Range loop
         D.Values (Vint_Range (J)) :=
            (if VA.Values (J) = VB.Values (J) then unsigned_int'Last else 0);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vcmpeqfp;

   --------------
   -- vcmpgefp --
   --------------

   function vcmpgefp (A : LL_VF; B : LL_VF) return LL_VSI is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      D : VSI_View;

   begin
      for J in Varray_float'Range loop
         D.Values (Vint_Range (J)) :=
           (if VA.Values (J) >= VB.Values (J) then Signed_Bool_True
                                              else Signed_Bool_False);
      end loop;

      return To_Vector (D);
   end vcmpgefp;

   --------------
   -- vcmpgtub --
   --------------

   function vcmpgtub (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
   begin
      D.Values := LL_VUC_Operations.vcmpgtux (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vcmpgtub;

   --------------
   -- vcmpgtsb --
   --------------

   function vcmpgtsb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;
   begin
      D.Values := LL_VSC_Operations.vcmpgtsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vcmpgtsb;

   --------------
   -- vcmpgtuh --
   --------------

   function vcmpgtuh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_Operations.vcmpgtux (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vcmpgtuh;

   --------------
   -- vcmpgtsh --
   --------------

   function vcmpgtsh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSS_Operations.vcmpgtsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vcmpgtsh;

   --------------
   -- vcmpgtuw --
   --------------

   function vcmpgtuw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values := LL_VUI_Operations.vcmpgtux (VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vcmpgtuw;

   --------------
   -- vcmpgtsw --
   --------------

   function vcmpgtsw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values := LL_VSI_Operations.vcmpgtsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vcmpgtsw;

   --------------
   -- vcmpgtfp --
   --------------

   function vcmpgtfp (A : LL_VF; B : LL_VF) return LL_VSI is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      D  : VSI_View;

   begin
      for J in Varray_float'Range loop
         D.Values (Vint_Range (J)) :=
           (if NJ_Truncate (VA.Values (J)) > NJ_Truncate (VB.Values (J))
            then Signed_Bool_True else Signed_Bool_False);
      end loop;

      return To_Vector (D);
   end vcmpgtfp;

   -----------
   -- vcfux --
   -----------

   function vcfux (A : LL_VUI; B : c_int) return LL_VF is
      VA : constant VUI_View := To_View (A);
      D  : VF_View;
      K  : Vfloat_Range;

   begin
      for J in Varray_signed_int'Range loop
         K := Vfloat_Range (J);

         --  Note: The conversion to Integer is safe, as Integers are required
         --  to include the range -2 ** 15 + 1 .. 2 ** 15 + 1 and therefore
         --  include the range of B (should be 0 .. 255).

         D.Values (K) :=
           C_float (VA.Values (J)) / (2.0 ** Integer (B));
      end loop;

      return To_Vector (D);
   end vcfux;

   -----------
   -- vcfsx --
   -----------

   function vcfsx (A : LL_VSI; B : c_int) return LL_VF is
      VA : constant VSI_View := To_View (A);
      D  : VF_View;
      K  : Vfloat_Range;

   begin
      for J in Varray_signed_int'Range loop
         K := Vfloat_Range (J);
         D.Values (K) := C_float (VA.Values (J))
           / (2.0 ** Integer (B));
      end loop;

      return To_Vector (D);
   end vcfsx;

   ------------
   -- vctsxs --
   ------------

   function vctsxs (A : LL_VF; B : c_int) return LL_VSI is
      VA : constant VF_View := To_View (A);
      D  : VSI_View;
      K  : Vfloat_Range;

   begin
      for J in Varray_signed_int'Range loop
         K := Vfloat_Range (J);
         D.Values (J) :=
           LL_VSI_Operations.Saturate
           (F64 (NJ_Truncate (VA.Values (K)))
            * F64 (2.0 ** Integer (B)));
      end loop;

      return To_Vector (D);
   end vctsxs;

   ------------
   -- vctuxs --
   ------------

   function vctuxs (A : LL_VF; B : c_int) return LL_VUI is
      VA : constant VF_View := To_View (A);
      D  : VUI_View;
      K  : Vfloat_Range;

   begin
      for J in Varray_unsigned_int'Range loop
         K := Vfloat_Range (J);
         D.Values (J) :=
           LL_VUI_Operations.Saturate
           (F64 (NJ_Truncate (VA.Values (K)))
            * F64 (2.0 ** Integer (B)));
      end loop;

      return To_Vector (D);
   end vctuxs;

   ---------
   -- dss --
   ---------

   --  No-ops, as allowed by [PEM-5.2.1.1 Data Stream Touch (dst)]:

   procedure dss (A : c_int) is
      pragma Unreferenced (A);
   begin
      null;
   end dss;

   ------------
   -- dssall --
   ------------

   --  No-ops, as allowed by [PEM-5.2.1.1 Data Stream Touch (dst)]:

   procedure dssall is
   begin
      null;
   end dssall;

   ---------
   -- dst --
   ---------

   --  No-ops, as allowed by [PEM-5.2.1.1 Data Stream Touch (dst)]:

   procedure dst    (A : c_ptr; B : c_int; C : c_int) is
      pragma Unreferenced (A);
      pragma Unreferenced (B);
      pragma Unreferenced (C);
   begin
      null;
   end dst;

   -----------
   -- dstst --
   -----------

   --  No-ops, as allowed by [PEM-5.2.1.1 Data Stream Touch (dst)]:

   procedure dstst  (A : c_ptr; B : c_int; C : c_int) is
      pragma Unreferenced (A);
      pragma Unreferenced (B);
      pragma Unreferenced (C);
   begin
      null;
   end dstst;

   ------------
   -- dststt --
   ------------

   --  No-ops, as allowed by [PEM-5.2.1.1 Data Stream Touch (dst)]:

   procedure dststt (A : c_ptr; B : c_int; C : c_int) is
      pragma Unreferenced (A);
      pragma Unreferenced (B);
      pragma Unreferenced (C);
   begin
      null;
   end dststt;

   ----------
   -- dstt --
   ----------

   --  No-ops, as allowed by [PEM-5.2.1.1 Data Stream Touch (dst)]:

   procedure dstt   (A : c_ptr; B : c_int; C : c_int) is
      pragma Unreferenced (A);
      pragma Unreferenced (B);
      pragma Unreferenced (C);
   begin
      null;
   end dstt;

   --------------
   -- vexptefp --
   --------------

   function vexptefp (A : LL_VF) return LL_VF is
      use C_float_Operations;

      VA : constant VF_View := To_View (A);
      D  : VF_View;

   begin
      for J in Varray_float'Range loop

         --  ??? Check the precision of the operation.
         --  As described in [PEM-6 vexptefp]:
         --  If theoretical_result is equal to 2 at the power of A (J) with
         --  infinite precision, we should have:
         --  abs ((D (J) - theoretical_result) / theoretical_result) <= 1/16

         D.Values (J) := 2.0 ** NJ_Truncate (VA.Values (J));
      end loop;

      return To_Vector (D);
   end vexptefp;

   -----------
   -- vrfim --
   -----------

   function vrfim (A : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      D  : VF_View;

   begin
      for J in Varray_float'Range loop

         --  If A (J) is infinite, D (J) should be infinite; With
         --  IEEE floating point, we can use 'Ceiling for that purpose.

         D.Values (J) := C_float'Ceiling (NJ_Truncate (VA.Values (J)));

         --  Vrfim rounds toward -Infinity, whereas 'Ceiling rounds toward
         --  +Infinity:

         if D.Values (J) /= VA.Values (J) then
            D.Values (J) := D.Values (J) - 1.0;
         end if;
      end loop;

      return To_Vector (D);
   end vrfim;

   ---------
   -- lvx --
   ---------

   function lvx (A : c_long; B : c_ptr) return LL_VSI is

      --  Simulate the altivec unit behavior regarding what Effective Address
      --  is accessed, stripping off the input address least significant bits
      --  wrt to vector alignment.

      --  On targets where VECTOR_ALIGNMENT is less than the vector size (16),
      --  an address within a vector is not necessarily rounded back at the
      --  vector start address. Besides, rounding on 16 makes no sense on such
      --  targets because the address of a properly aligned vector (that is,
      --  a proper multiple of VECTOR_ALIGNMENT) could be affected, which we
      --  want never to happen.

      EA : constant System.Address :=
             To_Address
               (Bound_Align
                  (Integer_Address (A) + To_Integer (B), VECTOR_ALIGNMENT));

      D : LL_VSI;
      for D'Address use EA;

   begin
      return D;
   end lvx;

   -----------
   -- lvebx --
   -----------

   function lvebx (A : c_long; B : c_ptr) return LL_VSC is
      D : VSC_View;
   begin
      D.Values := LL_VSC_Operations.lvexx (A, B);
      return To_Vector (D);
   end lvebx;

   -----------
   -- lvehx --
   -----------

   function lvehx (A : c_long; B : c_ptr) return LL_VSS is
      D : VSS_View;
   begin
      D.Values := LL_VSS_Operations.lvexx (A, B);
      return To_Vector (D);
   end lvehx;

   -----------
   -- lvewx --
   -----------

   function lvewx (A : c_long; B : c_ptr) return LL_VSI is
      D : VSI_View;
   begin
      D.Values := LL_VSI_Operations.lvexx (A, B);
      return To_Vector (D);
   end lvewx;

   ----------
   -- lvxl --
   ----------

   function lvxl  (A : c_long; B : c_ptr) return LL_VSI renames
     lvx;

   -------------
   -- vlogefp --
   -------------

   function vlogefp (A : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      D  : VF_View;

   begin
      for J in Varray_float'Range loop

         --  ??? Check the precision of the operation.
         --  As described in [PEM-6 vlogefp]:
         --  If theorical_result is equal to the log2 of A (J) with
         --  infinite precision, we should have:
         --  abs (D (J) - theorical_result) <= 1/32,
         --  unless abs(D(J) - 1) <= 1/8.

         D.Values (J) :=
           C_float_Operations.Log (NJ_Truncate (VA.Values (J)), 2.0);
      end loop;

      return To_Vector (D);
   end vlogefp;

   ----------
   -- lvsl --
   ----------

   function lvsl (A : c_long; B : c_ptr) return LL_VSC is
      type bit4_type is mod 16#F# + 1;
      for bit4_type'Alignment use 1;
      EA : Integer_Address;
      D  : VUC_View;
      SH : bit4_type;

   begin
      EA := Integer_Address (A) + To_Integer (B);
      SH := bit4_type (EA mod 2 ** 4);

      for J in D.Values'Range loop
         D.Values (J) := unsigned_char (SH) + unsigned_char (J)
           - unsigned_char (D.Values'First);
      end loop;

      return To_LL_VSC (To_Vector (D));
   end lvsl;

   ----------
   -- lvsr --
   ----------

   function lvsr (A : c_long; B : c_ptr) return LL_VSC is
      type bit4_type is mod 16#F# + 1;
      for bit4_type'Alignment use 1;
      EA : Integer_Address;
      D  : VUC_View;
      SH : bit4_type;

   begin
      EA := Integer_Address (A) + To_Integer (B);
      SH := bit4_type (EA mod 2 ** 4);

      for J in D.Values'Range loop
         D.Values (J) := (16#F# - unsigned_char (SH)) + unsigned_char (J);
      end loop;

      return To_LL_VSC (To_Vector (D));
   end lvsr;

   -------------
   -- vmaddfp --
   -------------

   function vmaddfp (A : LL_VF; B : LL_VF; C : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      VC : constant VF_View := To_View (C);
      D  : VF_View;

   begin
      for J in Varray_float'Range loop
         D.Values (J) :=
           Rnd_To_FP_Nearest (F64 (VA.Values (J))
                              * F64 (VB.Values (J))
                              + F64 (VC.Values (J)));
      end loop;

      return To_Vector (D);
   end vmaddfp;

   ---------------
   -- vmhaddshs --
   ---------------

   function vmhaddshs  (A : LL_VSS; B : LL_VSS; C : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      VC : constant VSS_View := To_View (C);
      D  : VSS_View;

   begin
      for J in Varray_signed_short'Range loop
         D.Values (J) := LL_VSS_Operations.Saturate
           ((SI64 (VA.Values (J)) * SI64 (VB.Values (J)))
            / SI64 (2 ** 15) + SI64 (VC.Values (J)));
      end loop;

      return To_Vector (D);
   end vmhaddshs;

   ------------
   -- vmaxub --
   ------------

   function vmaxub (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
   begin
      D.Values := LL_VUC_Operations.vmaxux (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vmaxub;

   ------------
   -- vmaxsb --
   ------------

   function vmaxsb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;
   begin
      D.Values := LL_VSC_Operations.vmaxsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmaxsb;

   ------------
   -- vmaxuh --
   ------------

   function vmaxuh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_Operations.vmaxux (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vmaxuh;

   ------------
   -- vmaxsh --
   ------------

   function vmaxsh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSS_Operations.vmaxsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmaxsh;

   ------------
   -- vmaxuw --
   ------------

   function vmaxuw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values := LL_VUI_Operations.vmaxux (VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vmaxuw;

   ------------
   -- vmaxsw --
   ------------

   function vmaxsw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values := LL_VSI_Operations.vmaxsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmaxsw;

   ------------
   -- vmaxfp --
   ------------

   function vmaxfp (A : LL_VF; B : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      D  : VF_View;

   begin
      for J in Varray_float'Range loop
         D.Values (J) := (if VA.Values (J) > VB.Values (J) then VA.Values (J)
                                                           else VB.Values (J));
      end loop;

      return To_Vector (D);
   end vmaxfp;

   ------------
   -- vmrghb --
   ------------

   function vmrghb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;
   begin
      D.Values := LL_VSC_Operations.vmrghx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmrghb;

   ------------
   -- vmrghh --
   ------------

   function vmrghh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSS_Operations.vmrghx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmrghh;

   ------------
   -- vmrghw --
   ------------

   function vmrghw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values := LL_VSI_Operations.vmrghx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmrghw;

   ------------
   -- vmrglb --
   ------------

   function vmrglb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;
   begin
      D.Values := LL_VSC_Operations.vmrglx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmrglb;

   ------------
   -- vmrglh --
   ------------

   function vmrglh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSS_Operations.vmrglx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmrglh;

   ------------
   -- vmrglw --
   ------------

   function vmrglw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values := LL_VSI_Operations.vmrglx (VA.Values, VB.Values);
      return To_Vector (D);
   end vmrglw;

   ------------
   -- mfvscr --
   ------------

   function  mfvscr return LL_VSS is
      D : VUS_View;
   begin
      for J in Varray_unsigned_short'Range loop
         D.Values (J) := 0;
      end loop;

      D.Values (Varray_unsigned_short'Last) :=
        unsigned_short (VSCR mod 2 ** unsigned_short'Size);
      D.Values (Varray_unsigned_short'Last - 1) :=
        unsigned_short (VSCR / 2 ** unsigned_short'Size);
      return To_LL_VSS (To_Vector (D));
   end mfvscr;

   ------------
   -- vminfp --
   ------------

   function vminfp (A : LL_VF;  B : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      D  : VF_View;

   begin
      for J in Varray_float'Range loop
         D.Values (J) := (if VA.Values (J) < VB.Values (J) then VA.Values (J)
                                                           else VB.Values (J));
      end loop;

      return To_Vector (D);
   end vminfp;

   ------------
   -- vminsb --
   ------------

   function vminsb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;
   begin
      D.Values := LL_VSC_Operations.vminsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vminsb;

   ------------
   -- vminub --
   ------------

   function vminub (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
   begin
      D.Values := LL_VUC_Operations.vminux (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vminub;

   ------------
   -- vminsh --
   ------------

   function vminsh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSS_Operations.vminsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vminsh;

   ------------
   -- vminuh --
   ------------

   function vminuh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_Operations.vminux (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vminuh;

   ------------
   -- vminsw --
   ------------

   function vminsw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values := LL_VSI_Operations.vminsx (VA.Values, VB.Values);
      return To_Vector (D);
   end vminsw;

   ------------
   -- vminuw --
   ------------

   function vminuw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values := LL_VUI_Operations.vminux (VA.Values,
                                            VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vminuw;

   ---------------
   -- vmladduhm --
   ---------------

   function vmladduhm (A : LL_VSS; B : LL_VSS; C : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      VC : constant VUS_View := To_View (To_LL_VUS (C));
      D  : VUS_View;

   begin
      for J in Varray_unsigned_short'Range loop
         D.Values (J) := VA.Values (J) * VB.Values (J)
           + VC.Values (J);
      end loop;

      return To_LL_VSS (To_Vector (D));
   end vmladduhm;

   ----------------
   -- vmhraddshs --
   ----------------

   function vmhraddshs (A : LL_VSS; B : LL_VSS; C : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      VC : constant VSS_View := To_View (C);
      D  : VSS_View;

   begin
      for J in Varray_signed_short'Range loop
         D.Values (J) :=
           LL_VSS_Operations.Saturate (((SI64 (VA.Values (J))
                                         * SI64 (VB.Values (J))
                                         + 2 ** 14)
                                        / 2 ** 15
                                        + SI64 (VC.Values (J))));
      end loop;

      return To_Vector (D);
   end vmhraddshs;

   --------------
   -- vmsumubm --
   --------------

   function vmsumubm (A : LL_VSC; B : LL_VSC; C : LL_VSI) return LL_VSI is
      Offset : Vchar_Range;
      VA     : constant VUC_View := To_View (To_LL_VUC (A));
      VB     : constant VUC_View := To_View (To_LL_VUC (B));
      VC     : constant VUI_View := To_View (To_LL_VUI (C));
      D      : VUI_View;

   begin
      for J in 0 .. 3 loop
         Offset := Vchar_Range (4 * J + Integer (Vchar_Range'First));
         D.Values (Vint_Range
                   (J + Integer (Vint_Range'First))) :=
           (unsigned_int (VA.Values (Offset))
            * unsigned_int (VB.Values (Offset)))
           + (unsigned_int (VA.Values (Offset + 1))
              * unsigned_int (VB.Values (1 + Offset)))
           + (unsigned_int (VA.Values (2 + Offset))
              * unsigned_int (VB.Values (2 + Offset)))
           + (unsigned_int (VA.Values (3 + Offset))
              * unsigned_int (VB.Values (3 + Offset)))
           + VC.Values (Vint_Range
                        (J + Integer (Varray_unsigned_int'First)));
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vmsumubm;

   --------------
   -- vmsummbm --
   --------------

   function vmsummbm (A : LL_VSC; B : LL_VSC; C : LL_VSI) return LL_VSI is
      Offset : Vchar_Range;
      VA     : constant VSC_View := To_View (A);
      VB     : constant VUC_View := To_View (To_LL_VUC (B));
      VC     : constant VSI_View := To_View (C);
      D      : VSI_View;

   begin
      for J in 0 .. 3 loop
         Offset := Vchar_Range (4 * J + Integer (Vchar_Range'First));
         D.Values (Vint_Range
                   (J + Integer (Varray_unsigned_int'First))) := 0
           + LL_VSI_Operations.Modular_Result (SI64 (VA.Values (Offset))
                                               * SI64 (VB.Values (Offset)))
           + LL_VSI_Operations.Modular_Result (SI64 (VA.Values (Offset + 1))
                                               * SI64 (VB.Values
                                                       (1 + Offset)))
           + LL_VSI_Operations.Modular_Result (SI64 (VA.Values (2 + Offset))
                                               * SI64 (VB.Values
                                                       (2 + Offset)))
           + LL_VSI_Operations.Modular_Result (SI64 (VA.Values (3 + Offset))
                                               * SI64 (VB.Values
                                                       (3 + Offset)))
           + VC.Values (Vint_Range
                        (J + Integer (Varray_unsigned_int'First)));
      end loop;

      return To_Vector (D);
   end vmsummbm;

   --------------
   -- vmsumuhm --
   --------------

   function vmsumuhm (A : LL_VSS; B : LL_VSS; C : LL_VSI) return LL_VSI is
      Offset : Vshort_Range;
      VA     : constant VUS_View := To_View (To_LL_VUS (A));
      VB     : constant VUS_View := To_View (To_LL_VUS (B));
      VC     : constant VUI_View := To_View (To_LL_VUI (C));
      D      : VUI_View;

   begin
      for J in 0 .. 3 loop
         Offset :=
           Vshort_Range (2 * J + Integer (Vshort_Range'First));
         D.Values (Vint_Range
                   (J + Integer (Varray_unsigned_int'First))) :=
           (unsigned_int (VA.Values (Offset))
            * unsigned_int (VB.Values (Offset)))
           + (unsigned_int (VA.Values (Offset + 1))
              * unsigned_int (VB.Values (1 + Offset)))
           + VC.Values (Vint_Range
                        (J + Integer (Vint_Range'First)));
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vmsumuhm;

   --------------
   -- vmsumshm --
   --------------

   function vmsumshm (A : LL_VSS; B : LL_VSS; C : LL_VSI) return LL_VSI is
      VA     : constant VSS_View := To_View (A);
      VB     : constant VSS_View := To_View (B);
      VC     : constant VSI_View := To_View (C);
      Offset : Vshort_Range;
      D      : VSI_View;

   begin
      for J in 0 .. 3 loop
         Offset :=
           Vshort_Range (2 * J + Integer (Varray_signed_char'First));
         D.Values (Vint_Range
                   (J + Integer (Varray_unsigned_int'First))) := 0
           + LL_VSI_Operations.Modular_Result (SI64 (VA.Values (Offset))
                                               * SI64 (VB.Values (Offset)))
           + LL_VSI_Operations.Modular_Result (SI64 (VA.Values (Offset + 1))
                                               * SI64 (VB.Values
                                                       (1 + Offset)))
           + VC.Values (Vint_Range
                        (J + Integer (Varray_unsigned_int'First)));
      end loop;

      return To_Vector (D);
   end vmsumshm;

   --------------
   -- vmsumuhs --
   --------------

   function vmsumuhs (A : LL_VSS; B : LL_VSS; C : LL_VSI) return LL_VSI is
      Offset : Vshort_Range;
      VA     : constant VUS_View := To_View (To_LL_VUS (A));
      VB     : constant VUS_View := To_View (To_LL_VUS (B));
      VC     : constant VUI_View := To_View (To_LL_VUI (C));
      D      : VUI_View;

   begin
      for J in 0 .. 3 loop
         Offset :=
           Vshort_Range (2 * J + Integer (Varray_signed_short'First));
         D.Values (Vint_Range
                   (J + Integer (Varray_unsigned_int'First))) :=
           LL_VUI_Operations.Saturate
           (UI64 (VA.Values (Offset))
            * UI64 (VB.Values (Offset))
            + UI64 (VA.Values (Offset + 1))
            * UI64 (VB.Values (1 + Offset))
            + UI64 (VC.Values
                    (Vint_Range
                     (J + Integer (Varray_unsigned_int'First)))));
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vmsumuhs;

   --------------
   -- vmsumshs --
   --------------

   function vmsumshs (A : LL_VSS; B : LL_VSS; C : LL_VSI) return LL_VSI is
      VA     : constant VSS_View := To_View (A);
      VB     : constant VSS_View := To_View (B);
      VC     : constant VSI_View := To_View (C);
      Offset : Vshort_Range;
      D      : VSI_View;

   begin
      for J in 0 .. 3 loop
         Offset :=
           Vshort_Range (2 * J + Integer (Varray_signed_short'First));
         D.Values (Vint_Range
                   (J + Integer (Varray_signed_int'First))) :=
           LL_VSI_Operations.Saturate
           (SI64 (VA.Values (Offset))
            * SI64 (VB.Values (Offset))
            + SI64 (VA.Values (Offset + 1))
            * SI64 (VB.Values (1 + Offset))
            + SI64 (VC.Values
                    (Vint_Range
                     (J + Integer (Varray_signed_int'First)))));
      end loop;

      return To_Vector (D);
   end vmsumshs;

   ------------
   -- mtvscr --
   ------------

   procedure mtvscr (A : LL_VSI) is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
   begin
      VSCR := VA.Values (Varray_unsigned_int'Last);
   end mtvscr;

   -------------
   -- vmuleub --
   -------------

   function vmuleub (A : LL_VSC; B : LL_VSC) return LL_VSS is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUC_LL_VUS_Operations.vmulxux (True,
                                                    VA.Values,
                                                    VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vmuleub;

   -------------
   -- vmuleuh --
   -------------

   function vmuleuh (A : LL_VSS; B : LL_VSS) return LL_VSI is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUI_View;
   begin
      D.Values := LL_VUS_LL_VUI_Operations.vmulxux (True,
                                                    VA.Values,
                                                    VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vmuleuh;

   -------------
   -- vmulesb --
   -------------

   function vmulesb (A : LL_VSC; B : LL_VSC) return LL_VSS is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSC_LL_VSS_Operations.vmulxsx (True,
                                                    VA.Values,
                                                    VB.Values);
      return To_Vector (D);
   end vmulesb;

   -------------
   -- vmulesh --
   -------------

   function vmulesh (A : LL_VSS; B : LL_VSS) return LL_VSI is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values := LL_VSS_LL_VSI_Operations.vmulxsx (True,
                                                    VA.Values,
                                                    VB.Values);
      return To_Vector (D);
   end vmulesh;

   -------------
   -- vmuloub --
   -------------

   function vmuloub (A : LL_VSC; B : LL_VSC) return LL_VSS is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUC_LL_VUS_Operations.vmulxux (False,
                                                    VA.Values,
                                                    VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vmuloub;

   -------------
   -- vmulouh --
   -------------

   function vmulouh (A : LL_VSS; B : LL_VSS) return LL_VSI is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUI_View;
   begin
      D.Values :=
        LL_VUS_LL_VUI_Operations.vmulxux (False, VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vmulouh;

   -------------
   -- vmulosb --
   -------------

   function vmulosb (A : LL_VSC; B : LL_VSC) return LL_VSS is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSC_LL_VSS_Operations.vmulxsx (False,
                                                    VA.Values,
                                                    VB.Values);
      return To_Vector (D);
   end vmulosb;

   -------------
   -- vmulosh --
   -------------

   function vmulosh (A : LL_VSS; B : LL_VSS) return LL_VSI is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values := LL_VSS_LL_VSI_Operations.vmulxsx (False,
                                                    VA.Values,
                                                    VB.Values);
      return To_Vector (D);
   end vmulosh;

   --------------
   -- vnmsubfp --
   --------------

   function vnmsubfp (A : LL_VF; B : LL_VF; C : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      VC : constant VF_View := To_View (C);
      D  : VF_View;

   begin
      for J in Vfloat_Range'Range loop
         D.Values (J) :=
           -Rnd_To_FP_Nearest (F64 (VA.Values (J))
                               * F64 (VB.Values (J))
                               - F64 (VC.Values (J)));
      end loop;

      return To_Vector (D);
   end vnmsubfp;

   ----------
   -- vnor --
   ----------

   function vnor (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;

   begin
      for J in Vint_Range'Range loop
         D.Values (J) := not (VA.Values (J) or VB.Values (J));
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vnor;

   ----------
   -- vor --
   ----------

   function vor (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;

   begin
      for J in Vint_Range'Range loop
         D.Values (J) := VA.Values (J) or VB.Values (J);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vor;

   -------------
   -- vpkuhum --
   -------------

   function vpkuhum (A : LL_VSS; B : LL_VSS) return LL_VSC is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUC_View;
   begin
      D.Values := LL_VUC_LL_VUS_Operations.vpkuxum (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vpkuhum;

   -------------
   -- vpkuwum --
   -------------

   function vpkuwum (A : LL_VSI; B : LL_VSI) return LL_VSS is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_LL_VUI_Operations.vpkuxum (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vpkuwum;

   -----------
   -- vpkpx --
   -----------

   function vpkpx (A : LL_VSI; B : LL_VSI) return LL_VSS is
      VA     : constant VUI_View := To_View (To_LL_VUI (A));
      VB     : constant VUI_View := To_View (To_LL_VUI (B));
      D      : VUS_View;
      Offset : Vint_Range;
      P16    : Pixel_16;
      P32    : Pixel_32;

   begin
      for J in 0 .. 3 loop
         Offset := Vint_Range (J + Integer (Vshort_Range'First));
         P32 := To_Pixel (VA.Values (Offset));
         P16.T := Unsigned_1 (P32.T mod 2 ** 1);
         P16.R := Unsigned_5 (Shift_Right (P32.R, 3) mod 2 ** 5);
         P16.G := Unsigned_5 (Shift_Right (P32.G, 3) mod 2 ** 5);
         P16.B := Unsigned_5 (Shift_Right (P32.B, 3) mod 2 ** 5);
         D.Values (Vshort_Range (Offset)) := To_unsigned_short (P16);
         P32 := To_Pixel (VB.Values (Offset));
         P16.T := Unsigned_1 (P32.T mod 2 ** 1);
         P16.R := Unsigned_5 (Shift_Right (P32.R, 3) mod 2 ** 5);
         P16.G := Unsigned_5 (Shift_Right (P32.G, 3) mod 2 ** 5);
         P16.B := Unsigned_5 (Shift_Right (P32.B, 3) mod 2 ** 5);
         D.Values (Vshort_Range (Offset) + 4) := To_unsigned_short (P16);
      end loop;

      return To_LL_VSS (To_Vector (D));
   end vpkpx;

   -------------
   -- vpkuhus --
   -------------

   function vpkuhus (A : LL_VSS; B : LL_VSS) return LL_VSC is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUC_View;
   begin
      D.Values := LL_VUC_LL_VUS_Operations.vpkuxus (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vpkuhus;

   -------------
   -- vpkuwus --
   -------------

   function vpkuwus (A : LL_VSI; B : LL_VSI) return LL_VSS is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_LL_VUI_Operations.vpkuxus (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vpkuwus;

   -------------
   -- vpkshss --
   -------------

   function vpkshss (A : LL_VSS; B : LL_VSS) return LL_VSC is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSC_View;
   begin
      D.Values := LL_VSC_LL_VSS_Operations.vpksxss (VA.Values, VB.Values);
      return To_Vector (D);
   end vpkshss;

   -------------
   -- vpkswss --
   -------------

   function vpkswss (A : LL_VSI; B : LL_VSI) return LL_VSS is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSS_LL_VSI_Operations.vpksxss (VA.Values, VB.Values);
      return To_Vector (D);
   end vpkswss;

   -------------
   -- vpksxus --
   -------------

   generic
      type Signed_Component_Type is range <>;
      type Signed_Index_Type is range <>;
      type Signed_Varray_Type is
        array (Signed_Index_Type) of Signed_Component_Type;
      type Unsigned_Component_Type is mod <>;
      type Unsigned_Index_Type is range <>;
      type Unsigned_Varray_Type is
        array (Unsigned_Index_Type) of Unsigned_Component_Type;

   function vpksxus
     (A : Signed_Varray_Type;
      B : Signed_Varray_Type) return Unsigned_Varray_Type;

   function vpksxus
     (A : Signed_Varray_Type;
      B : Signed_Varray_Type) return Unsigned_Varray_Type
   is
      N             : constant Unsigned_Index_Type :=
                        Unsigned_Index_Type (Signed_Index_Type'Last);
      Offset        : Unsigned_Index_Type;
      Signed_Offset : Signed_Index_Type;
      D             : Unsigned_Varray_Type;

      function Saturate
        (X : Signed_Component_Type) return Unsigned_Component_Type;
      --  Saturation, as defined in
      --  [PIM-4.1 Vector Status and Control Register]

      --------------
      -- Saturate --
      --------------

      function Saturate
        (X : Signed_Component_Type) return Unsigned_Component_Type
      is
         D : Unsigned_Component_Type;

      begin
         D := Unsigned_Component_Type
           (Signed_Component_Type'Max
            (Signed_Component_Type (Unsigned_Component_Type'First),
             Signed_Component_Type'Min
             (Signed_Component_Type (Unsigned_Component_Type'Last),
              X)));
         if Signed_Component_Type (D) /= X then
            VSCR := Write_Bit (VSCR, SAT_POS, 1);
         end if;

         return D;
      end Saturate;

   --  Start of processing for vpksxus

   begin
      for J in 0 .. N - 1 loop
         Offset :=
           Unsigned_Index_Type (Integer (J)
                                + Integer (Unsigned_Index_Type'First));
         Signed_Offset :=
           Signed_Index_Type (Integer (J)
                              + Integer (Signed_Index_Type'First));
         D (Offset) := Saturate (A (Signed_Offset));
         D (Offset + N) := Saturate (B (Signed_Offset));
      end loop;

      return D;
   end vpksxus;

   -------------
   -- vpkshus --
   -------------

   function vpkshus (A : LL_VSS; B : LL_VSS) return LL_VSC is
      function vpkshus_Instance is
        new vpksxus (signed_short,
                     Vshort_Range,
                     Varray_signed_short,
                     unsigned_char,
                     Vchar_Range,
                     Varray_unsigned_char);

      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VUC_View;

   begin
      D.Values := vpkshus_Instance (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vpkshus;

   -------------
   -- vpkswus --
   -------------

   function vpkswus (A : LL_VSI; B : LL_VSI) return LL_VSS is
      function vpkswus_Instance is
        new vpksxus (signed_int,
                     Vint_Range,
                     Varray_signed_int,
                     unsigned_short,
                     Vshort_Range,
                     Varray_unsigned_short);

      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VUS_View;
   begin
      D.Values := vpkswus_Instance (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vpkswus;

   ---------------
   -- vperm_4si --
   ---------------

   function vperm_4si (A : LL_VSI; B : LL_VSI; C : LL_VSC) return LL_VSI is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      VC : constant VUC_View := To_View (To_LL_VUC (C));
      J  : Vchar_Range;
      D  : VUC_View;

   begin
      for N in Vchar_Range'Range loop
         J := Vchar_Range (Integer (Bits (VC.Values (N), 4, 7))
                           + Integer (Vchar_Range'First));
         D.Values (N) :=
           (if Bits (VC.Values (N), 3, 3) = 0 then VA.Values (J)
                                              else VB.Values (J));
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vperm_4si;

   -----------
   -- vrefp --
   -----------

   function vrefp (A : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      D  : VF_View;

   begin
      for J in Vfloat_Range'Range loop
         D.Values (J) := FP_Recip_Est (VA.Values (J));
      end loop;

      return To_Vector (D);
   end vrefp;

   ----------
   -- vrlb --
   ----------

   function vrlb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
   begin
      D.Values := LL_VUC_Operations.vrlx (VA.Values, VB.Values, ROTL'Access);
      return To_LL_VSC (To_Vector (D));
   end vrlb;

   ----------
   -- vrlh --
   ----------

   function vrlh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_Operations.vrlx (VA.Values, VB.Values, ROTL'Access);
      return To_LL_VSS (To_Vector (D));
   end vrlh;

   ----------
   -- vrlw --
   ----------

   function vrlw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values := LL_VUI_Operations.vrlx (VA.Values, VB.Values, ROTL'Access);
      return To_LL_VSI (To_Vector (D));
   end vrlw;

   -----------
   -- vrfin --
   -----------

   function vrfin (A : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      D  : VF_View;

   begin
      for J in Vfloat_Range'Range loop
         D.Values (J) := C_float (Rnd_To_FPI_Near (F64 (VA.Values (J))));
      end loop;

      return To_Vector (D);
   end vrfin;

   ---------------
   -- vrsqrtefp --
   ---------------

   function vrsqrtefp (A : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      D  : VF_View;

   begin
      for J in Vfloat_Range'Range loop
         D.Values (J) := Recip_SQRT_Est (VA.Values (J));
      end loop;

      return To_Vector (D);
   end vrsqrtefp;

   --------------
   -- vsel_4si --
   --------------

   function vsel_4si (A : LL_VSI; B : LL_VSI; C : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      VC : constant VUI_View := To_View (To_LL_VUI (C));
      D  : VUI_View;

   begin
      for J in Vint_Range'Range loop
         D.Values (J) := ((not VC.Values (J)) and VA.Values (J))
           or (VC.Values (J) and VB.Values (J));
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vsel_4si;

   ----------
   -- vslb --
   ----------

   function vslb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
   begin
      D.Values :=
        LL_VUC_Operations.vsxx (VA.Values, VB.Values, Shift_Left'Access);
      return To_LL_VSC (To_Vector (D));
   end vslb;

   ----------
   -- vslh --
   ----------

   function vslh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values :=
        LL_VUS_Operations.vsxx (VA.Values, VB.Values, Shift_Left'Access);
      return To_LL_VSS (To_Vector (D));
   end vslh;

   ----------
   -- vslw --
   ----------

   function vslw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values :=
        LL_VUI_Operations.vsxx (VA.Values, VB.Values, Shift_Left'Access);
      return To_LL_VSI (To_Vector (D));
   end vslw;

   ----------------
   -- vsldoi_4si --
   ----------------

   function vsldoi_4si (A : LL_VSI; B : LL_VSI; C : c_int) return LL_VSI is
      VA     : constant VUC_View := To_View (To_LL_VUC (A));
      VB     : constant VUC_View := To_View (To_LL_VUC (B));
      Offset : c_int;
      Bound  : c_int;
      D      : VUC_View;

   begin
      for J in Vchar_Range'Range loop
         Offset := c_int (J) + C;
         Bound := c_int (Vchar_Range'First)
           + c_int (Varray_unsigned_char'Length);

         if Offset < Bound then
            D.Values (J) := VA.Values (Vchar_Range (Offset));
         else
            D.Values (J) :=
              VB.Values (Vchar_Range (Offset - Bound
                                      + c_int (Vchar_Range'First)));
         end if;
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vsldoi_4si;

   ----------------
   -- vsldoi_8hi --
   ----------------

   function vsldoi_8hi (A : LL_VSS; B : LL_VSS; C : c_int) return LL_VSS is
   begin
      return To_LL_VSS (vsldoi_4si (To_LL_VSI (A), To_LL_VSI (B), C));
   end vsldoi_8hi;

   -----------------
   -- vsldoi_16qi --
   -----------------

   function vsldoi_16qi (A : LL_VSC; B : LL_VSC; C : c_int) return LL_VSC is
   begin
      return To_LL_VSC (vsldoi_4si (To_LL_VSI (A), To_LL_VSI (B), C));
   end vsldoi_16qi;

   ----------------
   -- vsldoi_4sf --
   ----------------

   function vsldoi_4sf (A : LL_VF; B : LL_VF; C : c_int) return LL_VF is
   begin
      return To_LL_VF (vsldoi_4si (To_LL_VSI (A), To_LL_VSI (B), C));
   end vsldoi_4sf;

   ---------
   -- vsl --
   ---------

   function vsl  (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
      M  : constant Natural :=
             Natural (Bits (VB.Values (Vint_Range'Last), 29, 31));

      --  [PIM-4.4 vec_sll] "Note that the three low-order byte elements in B
      --  must be the same. Otherwise the value placed into D is undefined."
      --  ??? Shall we add a optional check for B?

   begin
      for J in Vint_Range'Range loop
         D.Values (J) := 0;
         D.Values (J) := D.Values (J) + Shift_Left (VA.Values (J), M);

         if J /= Vint_Range'Last then
            D.Values (J) :=
              D.Values (J) + Shift_Right (VA.Values (J + 1),
                                          signed_int'Size - M);
         end if;
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vsl;

   ----------
   -- vslo --
   ----------

   function vslo (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
      M  : constant Natural :=
             Natural (Bits (VB.Values (Vchar_Range'Last), 1, 4));
      J  : Natural;

   begin
      for N in Vchar_Range'Range loop
         J := Natural (N) + M;
         D.Values (N) :=
           (if J <= Natural (Vchar_Range'Last) then VA.Values (Vchar_Range (J))
                                               else 0);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vslo;

   ------------
   -- vspltb --
   ------------

   function vspltb (A : LL_VSC; B : c_int) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      D  : VSC_View;
   begin
      D.Values := LL_VSC_Operations.vspltx (VA.Values, B);
      return To_Vector (D);
   end vspltb;

   ------------
   -- vsplth --
   ------------

   function vsplth (A : LL_VSS; B : c_int) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      D  : VSS_View;
   begin
      D.Values := LL_VSS_Operations.vspltx (VA.Values, B);
      return To_Vector (D);
   end vsplth;

   ------------
   -- vspltw --
   ------------

   function vspltw (A : LL_VSI; B : c_int) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      D  : VSI_View;
   begin
      D.Values := LL_VSI_Operations.vspltx (VA.Values, B);
      return To_Vector (D);
   end vspltw;

   --------------
   -- vspltisb --
   --------------

   function vspltisb (A : c_int) return LL_VSC is
      D : VSC_View;
   begin
      D.Values := LL_VSC_Operations.vspltisx (A);
      return To_Vector (D);
   end vspltisb;

   --------------
   -- vspltish --
   --------------

   function vspltish (A : c_int) return LL_VSS is
      D : VSS_View;
   begin
      D.Values := LL_VSS_Operations.vspltisx (A);
      return To_Vector (D);
   end vspltish;

   --------------
   -- vspltisw --
   --------------

   function vspltisw (A : c_int) return LL_VSI is
      D : VSI_View;
   begin
      D.Values := LL_VSI_Operations.vspltisx (A);
      return To_Vector (D);
   end vspltisw;

   ----------
   -- vsrb --
   ----------

   function vsrb (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
   begin
      D.Values :=
        LL_VUC_Operations.vsxx (VA.Values, VB.Values, Shift_Right'Access);
      return To_LL_VSC (To_Vector (D));
   end vsrb;

   ----------
   -- vsrh --
   ----------

   function vsrh (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values :=
        LL_VUS_Operations.vsxx (VA.Values, VB.Values, Shift_Right'Access);
      return To_LL_VSS (To_Vector (D));
   end vsrh;

   ----------
   -- vsrw --
   ----------

   function vsrw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values :=
        LL_VUI_Operations.vsxx (VA.Values, VB.Values, Shift_Right'Access);
      return To_LL_VSI (To_Vector (D));
   end vsrw;

   -----------
   -- vsrab --
   -----------

   function vsrab (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;
   begin
      D.Values :=
        LL_VSC_Operations.vsrax (VA.Values, VB.Values, Shift_Right_A'Access);
      return To_Vector (D);
   end vsrab;

   -----------
   -- vsrah --
   -----------

   function vsrah (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values :=
        LL_VSS_Operations.vsrax (VA.Values, VB.Values, Shift_Right_A'Access);
      return To_Vector (D);
   end vsrah;

   -----------
   -- vsraw --
   -----------

   function vsraw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values :=
        LL_VSI_Operations.vsrax (VA.Values, VB.Values, Shift_Right_A'Access);
      return To_Vector (D);
   end vsraw;

   ---------
   -- vsr --
   ---------

   function vsr  (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      M  : constant Natural :=
             Natural (Bits (VB.Values (Vint_Range'Last), 29, 31));
      D  : VUI_View;

   begin
      for J in Vint_Range'Range loop
         D.Values (J) := 0;
         D.Values (J) := D.Values (J) + Shift_Right (VA.Values (J), M);

         if J /= Vint_Range'First then
            D.Values (J) :=
              D.Values (J)
              + Shift_Left (VA.Values (J - 1), signed_int'Size - M);
         end if;
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vsr;

   ----------
   -- vsro --
   ----------

   function vsro (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      M  : constant Natural :=
             Natural (Bits (VB.Values (Vchar_Range'Last), 1, 4));
      J  : Natural;
      D  : VUC_View;

   begin
      for N in Vchar_Range'Range loop
         J := Natural (N) - M;

         if J >= Natural (Vchar_Range'First) then
            D.Values (N) := VA.Values (Vchar_Range (J));
         else
            D.Values (N) := 0;
         end if;
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vsro;

   ----------
   -- stvx --
   ----------

   procedure stvx   (A : LL_VSI; B : c_int; C : c_ptr) is

      --  Simulate the altivec unit behavior regarding what Effective Address
      --  is accessed, stripping off the input address least significant bits
      --  wrt to vector alignment (see comment in lvx for further details).

      EA : constant System.Address :=
             To_Address
               (Bound_Align
                  (Integer_Address (B) + To_Integer (C), VECTOR_ALIGNMENT));

      D  : LL_VSI;
      for D'Address use EA;

   begin
      D := A;
   end stvx;

   ------------
   -- stvebx --
   ------------

   procedure stvebx (A : LL_VSC; B : c_int; C : c_ptr) is
      VA : constant VSC_View := To_View (A);
   begin
      LL_VSC_Operations.stvexx (VA.Values, B, C);
   end stvebx;

   ------------
   -- stvehx --
   ------------

   procedure stvehx (A : LL_VSS; B : c_int; C : c_ptr) is
      VA : constant VSS_View := To_View (A);
   begin
      LL_VSS_Operations.stvexx (VA.Values, B, C);
   end stvehx;

   ------------
   -- stvewx --
   ------------

   procedure stvewx (A : LL_VSI; B : c_int; C : c_ptr) is
      VA : constant VSI_View := To_View (A);
   begin
      LL_VSI_Operations.stvexx (VA.Values, B, C);
   end stvewx;

   -----------
   -- stvxl --
   -----------

   procedure stvxl   (A : LL_VSI; B : c_int; C : c_ptr) renames stvx;

   -------------
   -- vsububm --
   -------------

   function vsububm (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
   begin
      D.Values := LL_VUC_Operations.vsubuxm (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vsububm;

   -------------
   -- vsubuhm --
   -------------

   function vsubuhm (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_Operations.vsubuxm (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vsubuhm;

   -------------
   -- vsubuwm --
   -------------

   function vsubuwm (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values := LL_VUI_Operations.vsubuxm (VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vsubuwm;

   ------------
   -- vsubfp --
   ------------

   function vsubfp (A : LL_VF; B : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      VB : constant VF_View := To_View (B);
      D  : VF_View;

   begin
      for J in Vfloat_Range'Range loop
         D.Values (J) :=
           NJ_Truncate (NJ_Truncate (VA.Values (J))
                        - NJ_Truncate (VB.Values (J)));
      end loop;

      return To_Vector (D);
   end vsubfp;

   -------------
   -- vsubcuw --
   -------------

   function vsubcuw (A : LL_VSI; B : LL_VSI) return LL_VSI is
      Subst_Result : SI64;

      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;

   begin
      for J in Vint_Range'Range loop
         Subst_Result := SI64 (VA.Values (J)) - SI64 (VB.Values (J));
         D.Values (J) :=
           (if Subst_Result < SI64 (unsigned_int'First) then 0 else 1);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vsubcuw;

   -------------
   -- vsububs --
   -------------

   function vsububs (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VUC_View := To_View (To_LL_VUC (A));
      VB : constant VUC_View := To_View (To_LL_VUC (B));
      D  : VUC_View;
   begin
      D.Values := LL_VUC_Operations.vsubuxs (VA.Values, VB.Values);
      return To_LL_VSC (To_Vector (D));
   end vsububs;

   -------------
   -- vsubsbs --
   -------------

   function vsubsbs (A : LL_VSC; B : LL_VSC) return LL_VSC is
      VA : constant VSC_View := To_View (A);
      VB : constant VSC_View := To_View (B);
      D  : VSC_View;
   begin
      D.Values := LL_VSC_Operations.vsubsxs (VA.Values, VB.Values);
      return To_Vector (D);
   end vsubsbs;

   -------------
   -- vsubuhs --
   -------------

   function vsubuhs (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VUS_View := To_View (To_LL_VUS (A));
      VB : constant VUS_View := To_View (To_LL_VUS (B));
      D  : VUS_View;
   begin
      D.Values := LL_VUS_Operations.vsubuxs (VA.Values, VB.Values);
      return To_LL_VSS (To_Vector (D));
   end vsubuhs;

   -------------
   -- vsubshs --
   -------------

   function vsubshs (A : LL_VSS; B : LL_VSS) return LL_VSS is
      VA : constant VSS_View := To_View (A);
      VB : constant VSS_View := To_View (B);
      D  : VSS_View;
   begin
      D.Values := LL_VSS_Operations.vsubsxs (VA.Values, VB.Values);
      return To_Vector (D);
   end vsubshs;

   -------------
   -- vsubuws --
   -------------

   function vsubuws (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;
   begin
      D.Values := LL_VUI_Operations.vsubuxs (VA.Values, VB.Values);
      return To_LL_VSI (To_Vector (D));
   end vsubuws;

   -------------
   -- vsubsws --
   -------------

   function vsubsws (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VSI_View := To_View (A);
      VB : constant VSI_View := To_View (B);
      D  : VSI_View;
   begin
      D.Values := LL_VSI_Operations.vsubsxs (VA.Values, VB.Values);
      return To_Vector (D);
   end vsubsws;

   --------------
   -- vsum4ubs --
   --------------

   function vsum4ubs (A : LL_VSC; B : LL_VSI) return LL_VSI is
      VA     : constant VUC_View := To_View (To_LL_VUC (A));
      VB     : constant VUI_View := To_View (To_LL_VUI (B));
      Offset : Vchar_Range;
      D      : VUI_View;

   begin
      for J in 0 .. 3 loop
         Offset := Vchar_Range (4 * J + Integer (Vchar_Range'First));
         D.Values (Vint_Range (J + Integer (Vint_Range'First))) :=
           LL_VUI_Operations.Saturate
           (UI64 (VA.Values (Offset))
            + UI64 (VA.Values (Offset + 1))
            + UI64 (VA.Values (Offset + 2))
            + UI64 (VA.Values (Offset + 3))
            + UI64 (VB.Values (Vint_Range (J + Integer (Vint_Range'First)))));
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vsum4ubs;

   --------------
   -- vsum4sbs --
   --------------

   function vsum4sbs (A : LL_VSC; B : LL_VSI) return LL_VSI is
      VA     : constant VSC_View := To_View (A);
      VB     : constant VSI_View := To_View (B);
      Offset : Vchar_Range;
      D      : VSI_View;

   begin
      for J in 0 .. 3 loop
         Offset := Vchar_Range (4 * J + Integer (Vchar_Range'First));
         D.Values (Vint_Range (J + Integer (Vint_Range'First))) :=
           LL_VSI_Operations.Saturate
           (SI64 (VA.Values (Offset))
            + SI64 (VA.Values (Offset + 1))
            + SI64 (VA.Values (Offset + 2))
            + SI64 (VA.Values (Offset + 3))
            + SI64 (VB.Values (Vint_Range (J + Integer (Vint_Range'First)))));
      end loop;

      return To_Vector (D);
   end vsum4sbs;

   --------------
   -- vsum4shs --
   --------------

   function vsum4shs (A : LL_VSS; B : LL_VSI) return LL_VSI is
      VA     : constant VSS_View := To_View (A);
      VB     : constant VSI_View := To_View (B);
      Offset : Vshort_Range;
      D      : VSI_View;

   begin
      for J in 0 .. 3 loop
         Offset := Vshort_Range (2 * J + Integer (Vchar_Range'First));
         D.Values (Vint_Range (J + Integer (Vint_Range'First))) :=
           LL_VSI_Operations.Saturate
           (SI64 (VA.Values (Offset))
            + SI64 (VA.Values (Offset + 1))
            + SI64 (VB.Values (Vint_Range (J + Integer (Vint_Range'First)))));
      end loop;

      return To_Vector (D);
   end vsum4shs;

   --------------
   -- vsum2sws --
   --------------

   function vsum2sws (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA     : constant VSI_View := To_View (A);
      VB     : constant VSI_View := To_View (B);
      Offset : Vint_Range;
      D      : VSI_View;

   begin
      for J in 0 .. 1 loop
         Offset := Vint_Range (2 * J + Integer (Vchar_Range'First));
         D.Values (Offset) := 0;
         D.Values (Offset + 1) :=
           LL_VSI_Operations.Saturate
           (SI64 (VA.Values (Offset))
            + SI64 (VA.Values (Offset + 1))
            + SI64 (VB.Values (Vint_Range (Offset + 1))));
      end loop;

      return To_Vector (D);
   end vsum2sws;

   -------------
   -- vsumsws --
   -------------

   function vsumsws (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA         : constant VSI_View := To_View (A);
      VB         : constant VSI_View := To_View (B);
      D          : VSI_View;
      Sum_Buffer : SI64 := 0;

   begin
      for J in Vint_Range'Range loop
         D.Values (J) := 0;
         Sum_Buffer := Sum_Buffer + SI64 (VA.Values (J));
      end loop;

      Sum_Buffer := Sum_Buffer + SI64 (VB.Values (Vint_Range'Last));
      D.Values (Vint_Range'Last) := LL_VSI_Operations.Saturate (Sum_Buffer);
      return To_Vector (D);
   end vsumsws;

   -----------
   -- vrfiz --
   -----------

   function vrfiz (A : LL_VF) return LL_VF is
      VA : constant VF_View := To_View (A);
      D  : VF_View;
   begin
      for J in Vfloat_Range'Range loop
         D.Values (J) := C_float (Rnd_To_FPI_Trunc (F64 (VA.Values (J))));
      end loop;

      return To_Vector (D);
   end vrfiz;

   -------------
   -- vupkhsb --
   -------------

   function vupkhsb (A : LL_VSC) return LL_VSS is
      VA : constant VSC_View := To_View (A);
      D  : VSS_View;
   begin
      D.Values := LL_VSC_LL_VSS_Operations.vupkxsx (VA.Values, 0);
      return To_Vector (D);
   end vupkhsb;

   -------------
   -- vupkhsh --
   -------------

   function vupkhsh (A : LL_VSS) return LL_VSI is
      VA : constant VSS_View := To_View (A);
      D  : VSI_View;
   begin
      D.Values := LL_VSS_LL_VSI_Operations.vupkxsx (VA.Values, 0);
      return To_Vector (D);
   end vupkhsh;

   -------------
   -- vupkxpx --
   -------------

   function vupkxpx (A : LL_VSS; Offset : Natural) return LL_VSI;
   --  For vupkhpx and vupklpx (depending on Offset)

   function vupkxpx (A : LL_VSS; Offset : Natural) return LL_VSI is
      VA  : constant VUS_View := To_View (To_LL_VUS (A));
      K   : Vshort_Range;
      D   : VUI_View;
      P16 : Pixel_16;
      P32 : Pixel_32;

      function Sign_Extend (X : Unsigned_1) return unsigned_char;

      function Sign_Extend (X : Unsigned_1) return unsigned_char is
      begin
         if X = 1 then
            return 16#FF#;
         else
            return 16#00#;
         end if;
      end Sign_Extend;

   begin
      for J in Vint_Range'Range loop
         K := Vshort_Range (Integer (J)
                            - Integer (Vint_Range'First)
                            + Integer (Vshort_Range'First)
                            + Offset);
         P16 := To_Pixel (VA.Values (K));
         P32.T := Sign_Extend (P16.T);
         P32.R := unsigned_char (P16.R);
         P32.G := unsigned_char (P16.G);
         P32.B := unsigned_char (P16.B);
         D.Values (J) := To_unsigned_int (P32);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vupkxpx;

   -------------
   -- vupkhpx --
   -------------

   function vupkhpx (A : LL_VSS) return LL_VSI is
   begin
      return vupkxpx (A, 0);
   end vupkhpx;

   -------------
   -- vupklsb --
   -------------

   function vupklsb (A : LL_VSC) return LL_VSS is
      VA : constant VSC_View := To_View (A);
      D  : VSS_View;
   begin
      D.Values :=
        LL_VSC_LL_VSS_Operations.vupkxsx (VA.Values,
                                          Varray_signed_short'Length);
      return To_Vector (D);
   end vupklsb;

   -------------
   -- vupklsh --
   -------------

   function vupklsh (A : LL_VSS) return LL_VSI is
      VA : constant VSS_View := To_View (A);
      D  : VSI_View;
   begin
      D.Values :=
        LL_VSS_LL_VSI_Operations.vupkxsx (VA.Values,
                                          Varray_signed_int'Length);
      return To_Vector (D);
   end vupklsh;

   -------------
   -- vupklpx --
   -------------

   function vupklpx (A : LL_VSS) return LL_VSI is
   begin
      return vupkxpx (A, Varray_signed_int'Length);
   end vupklpx;

   ----------
   -- vxor --
   ----------

   function vxor (A : LL_VSI; B : LL_VSI) return LL_VSI is
      VA : constant VUI_View := To_View (To_LL_VUI (A));
      VB : constant VUI_View := To_View (To_LL_VUI (B));
      D  : VUI_View;

   begin
      for J in Vint_Range'Range loop
         D.Values (J) := VA.Values (J) xor VB.Values (J);
      end loop;

      return To_LL_VSI (To_Vector (D));
   end vxor;

   ----------------
   -- vcmpequb_p --
   ----------------

   function vcmpequb_p (A : c_int; B : LL_VSC; C : LL_VSC) return c_int is
      D : LL_VSC;
   begin
      D := vcmpequb (B, C);
      return LL_VSC_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpequb_p;

   ----------------
   -- vcmpequh_p --
   ----------------

   function vcmpequh_p (A : c_int; B : LL_VSS; C : LL_VSS) return c_int is
      D : LL_VSS;
   begin
      D := vcmpequh (B, C);
      return LL_VSS_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpequh_p;

   ----------------
   -- vcmpequw_p --
   ----------------

   function vcmpequw_p (A : c_int; B : LL_VSI; C : LL_VSI) return c_int is
      D : LL_VSI;
   begin
      D := vcmpequw (B, C);
      return LL_VSI_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpequw_p;

   ----------------
   -- vcmpeqfp_p --
   ----------------

   function vcmpeqfp_p (A : c_int; B : LL_VF; C : LL_VF) return c_int is
      D : LL_VSI;
   begin
      D := vcmpeqfp (B, C);
      return LL_VSI_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpeqfp_p;

   ----------------
   -- vcmpgtub_p --
   ----------------

   function vcmpgtub_p (A : c_int; B : LL_VSC; C : LL_VSC) return c_int is
      D : LL_VSC;
   begin
      D := vcmpgtub (B, C);
      return LL_VSC_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpgtub_p;

   ----------------
   -- vcmpgtuh_p --
   ----------------

   function vcmpgtuh_p (A : c_int; B : LL_VSS; C : LL_VSS) return c_int is
      D : LL_VSS;
   begin
      D := vcmpgtuh (B, C);
      return LL_VSS_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpgtuh_p;

   ----------------
   -- vcmpgtuw_p --
   ----------------

   function vcmpgtuw_p (A : c_int; B : LL_VSI; C : LL_VSI) return c_int is
      D : LL_VSI;
   begin
      D := vcmpgtuw (B, C);
      return LL_VSI_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpgtuw_p;

   ----------------
   -- vcmpgtsb_p --
   ----------------

   function vcmpgtsb_p (A : c_int; B : LL_VSC; C : LL_VSC) return c_int is
      D : LL_VSC;
   begin
      D := vcmpgtsb (B, C);
      return LL_VSC_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpgtsb_p;

   ----------------
   -- vcmpgtsh_p --
   ----------------

   function vcmpgtsh_p (A : c_int; B : LL_VSS; C : LL_VSS) return c_int is
      D : LL_VSS;
   begin
      D := vcmpgtsh (B, C);
      return LL_VSS_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpgtsh_p;

   ----------------
   -- vcmpgtsw_p --
   ----------------

   function vcmpgtsw_p (A : c_int; B : LL_VSI; C : LL_VSI) return c_int is
      D : LL_VSI;
   begin
      D := vcmpgtsw (B, C);
      return LL_VSI_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpgtsw_p;

   ----------------
   -- vcmpgefp_p --
   ----------------

   function vcmpgefp_p (A : c_int; B : LL_VF; C : LL_VF) return c_int is
      D : LL_VSI;
   begin
      D := vcmpgefp (B, C);
      return LL_VSI_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpgefp_p;

   ----------------
   -- vcmpgtfp_p --
   ----------------

   function vcmpgtfp_p (A : c_int; B : LL_VF; C : LL_VF) return c_int is
      D : LL_VSI;
   begin
      D := vcmpgtfp (B, C);
      return LL_VSI_Operations.Check_CR6 (A, To_View (D).Values);
   end vcmpgtfp_p;

   ----------------
   -- vcmpbfp_p --
   ----------------

   function vcmpbfp_p (A : c_int; B : LL_VF; C : LL_VF) return c_int is
      D : VSI_View;
   begin
      D := To_View (vcmpbfp (B, C));

      for J in Vint_Range'Range loop

         --  vcmpbfp is not returning the usual bool vector; do the conversion

         D.Values (J) :=
           (if D.Values (J) = 0 then Signed_Bool_False else Signed_Bool_True);
      end loop;

      return LL_VSI_Operations.Check_CR6 (A, D.Values);
   end vcmpbfp_p;

end GNAT.Altivec.Low_Level_Vectors;
