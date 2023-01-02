------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U I N T P                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

with Output; use Output;

with GNAT.HTable; use GNAT.HTable;

package body Uintp is

   ------------------------
   -- Local Declarations --
   ------------------------

   Uint_Int_First : Uint := Uint_0;
   --  Uint value containing Int'First value, set by Initialize. The initial
   --  value of Uint_0 is used for an assertion check that ensures that this
   --  value is not used before it is initialized. This value is used in the
   --  UI_Is_In_Int_Range predicate, and it is right that this is a host value,
   --  since the issue is host representation of integer values.

   Uint_Int_Last : Uint;
   --  Uint value containing Int'Last value set by Initialize

   UI_Power_2 : array (Int range 0 .. 128) of Uint;
   --  This table is used to memoize exponentiations by powers of 2. The Nth
   --  entry, if set, contains the Uint value 2**N. Initially UI_Power_2_Set
   --  is zero and only the 0'th entry is set, the invariant being that all
   --  entries in the range 0 .. UI_Power_2_Set are initialized.

   UI_Power_2_Set : Nat;
   --  Number of entries set in UI_Power_2;

   UI_Power_10 : array (Int range 0 .. 128) of Uint;
   --  This table is used to memoize exponentiations by powers of 10 in the
   --  same manner as described above for UI_Power_2.

   UI_Power_10_Set : Nat;
   --  Number of entries set in UI_Power_10;

   Uints_Min   : Uint;
   Udigits_Min : Int;
   --  These values are used to make sure that the mark/release mechanism does
   --  not destroy values saved in the U_Power tables or in the hash table used
   --  by UI_From_Int. Whenever an entry is made in either of these tables,
   --  Uints_Min and Udigits_Min are updated to protect the entry, and Release
   --  never cuts back beyond these minimum values.

   Int_0 : constant Int := 0;
   Int_1 : constant Int := 1;
   Int_2 : constant Int := 2;
   --  These values are used in some cases where the use of numeric literals
   --  would cause ambiguities (integer vs Uint).

   type UI_Vector is array (Pos range <>) of Int;
   --  Vector containing the integer values of a Uint value

   --  Note: An earlier version of this package used pointers of arrays of Ints
   --  (dynamically allocated) for the Uint type. The change leads to a few
   --  less natural idioms used throughout this code, but eliminates all uses
   --  of the heap except for the table package itself. For example, Uint
   --  parameters are often converted to UI_Vectors for internal manipulation.
   --  This is done by creating the local UI_Vector using the function N_Digits
   --  on the Uint to find the size needed for the vector, and then calling
   --  Init_Operand to copy the values out of the table into the vector.

   ----------------------------
   -- UI_From_Int Hash Table --
   ----------------------------

   --  UI_From_Int uses a hash table to avoid duplicating entries and wasting
   --  storage. This is particularly important for complex cases of back
   --  annotation.

   subtype Hnum is Nat range 0 .. 1022;

   function Hash_Num (F : Int) return Hnum;
   --  Hashing function

   package UI_Ints is new Simple_HTable (
     Header_Num => Hnum,
     Element    => Uint,
     No_Element => No_Uint,
     Key        => Int,
     Hash       => Hash_Num,
     Equal      => "=");

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Direct (U : Valid_Uint) return Boolean;
   pragma Inline (Direct);
   --  Returns True if U is represented directly

   function Direct_Val (U : Valid_Uint) return Int;
   --  U is a Uint that is represented directly. The returned result is the
   --  value represented.

   function GCD (Jin, Kin : Int) return Int;
   --  Compute GCD of two integers. Assumes that Jin >= Kin >= 0

   procedure Image_Out
     (Input     : Uint;
      To_Buffer : Boolean;
      Format    : UI_Format);
   --  Common processing for UI_Image and UI_Write, To_Buffer is set True for
   --  UI_Image, and false for UI_Write, and Format is copied from the Format
   --  parameter to UI_Image or UI_Write.

   procedure Init_Operand (UI : Valid_Uint; Vec : out UI_Vector);
   pragma Inline (Init_Operand);
   --  This procedure puts the value of UI into the vector in canonical
   --  multiple precision format. The parameter should be of the correct size
   --  as determined by a previous call to N_Digits (UI). The first digit of
   --  Vec contains the sign, all other digits are always non-negative. Note
   --  that the input may be directly represented, and in this case Vec will
   --  contain the corresponding one or two digit value. The low bound of Vec
   --  is always 1.

   function Vector_To_Uint
     (In_Vec   : UI_Vector;
      Negative : Boolean) return Valid_Uint;
   --  Functions that calculate values in UI_Vectors, call this function to
   --  create and return the Uint value. In_Vec contains the multiple precision
   --  (Base) representation of a non-negative value. Leading zeroes are
   --  permitted. Negative is set if the desired result is the negative of the
   --  given value. The result will be either the appropriate directly
   --  represented value, or a table entry in the proper canonical format is
   --  created and returned.
   --
   --  Note that Init_Operand puts a signed value in the result vector, but
   --  Vector_To_Uint is always presented with a non-negative value. The
   --  processing of signs is something that is done by the caller before
   --  calling Vector_To_Uint.

   function Least_Sig_Digit (Arg : Valid_Uint) return Int;
   pragma Inline (Least_Sig_Digit);
   --  Returns the Least Significant Digit of Arg quickly. When the given Uint
   --  is less than 2**15, the value returned is the input value, in this case
   --  the result may be negative. It is expected that any use will mask off
   --  unnecessary bits. This is used for finding Arg mod B where B is a power
   --  of two. Hence the actual base is irrelevant as long as it is a power of
   --  two.

   procedure Most_Sig_2_Digits
     (Left      : Valid_Uint;
      Right     : Valid_Uint;
      Left_Hat  : out Int;
      Right_Hat : out Int);
   --  Returns leading two significant digits from the given pair of Uint's.
   --  Mathematically: returns Left / (Base**K) and Right / (Base**K) where
   --  K is as small as possible S.T. Right_Hat < Base * Base. It is required
   --  that Left >= Right for the algorithm to work.

   function N_Digits (Input : Valid_Uint) return Int;
   pragma Inline (N_Digits);
   --  Returns number of "digits" in a Uint

   procedure UI_Div_Rem
     (Left, Right       : Valid_Uint;
      Quotient          : out Uint;
      Remainder         : out Uint;
      Discard_Quotient  : Boolean := False;
      Discard_Remainder : Boolean := False);
   --  Compute Euclidean division of Left by Right. If Discard_Quotient is
   --  False then the quotient is returned in Quotient. If Discard_Remainder
   --  is False, then the remainder is returned in Remainder.
   --
   --  If Discard_Quotient is True, Quotient is set to No_Uint.
   --  If Discard_Remainder is True, Remainder is set to No_Uint.

   function UI_Modular_Exponentiation
     (B      : Valid_Uint;
      E      : Valid_Uint;
      Modulo : Valid_Uint) return Valid_Uint with Unreferenced;
   --  Efficiently compute (B**E) rem Modulo

   function UI_Modular_Inverse
     (N : Valid_Uint; Modulo : Valid_Uint) return Valid_Uint with Unreferenced;
   --  Compute the multiplicative inverse of N in modular arithmetics with the
   --  given Modulo (uses Euclid's algorithm). Note: the call is considered
   --  to be erroneous (and the behavior is undefined) if n is not invertible.

   ------------
   -- Direct --
   ------------

   function Direct (U : Valid_Uint) return Boolean is
   begin
      return Int (U) <= Int (Uint_Direct_Last);
   end Direct;

   ----------------
   -- Direct_Val --
   ----------------

   function Direct_Val (U : Valid_Uint) return Int is
   begin
      pragma Assert (Direct (U));
      return Int (U) - Int (Uint_Direct_Bias);
   end Direct_Val;

   ---------
   -- GCD --
   ---------

   function GCD (Jin, Kin : Int) return Int is
      J, K, Tmp : Int;

   begin
      pragma Assert (Jin >= Kin);
      pragma Assert (Kin >= Int_0);

      J := Jin;
      K := Kin;
      while K /= Uint_0 loop
         Tmp := J mod K;
         J := K;
         K := Tmp;
      end loop;

      return J;
   end GCD;

   --------------
   -- Hash_Num --
   --------------

   function Hash_Num (F : Int) return Hnum is
   begin
      return Types."mod" (F, Hnum'Range_Length);
   end Hash_Num;

   ---------------
   -- Image_Out --
   ---------------

   procedure Image_Out
     (Input     : Uint;
      To_Buffer : Boolean;
      Format    : UI_Format)
   is
      Marks  : constant Uintp.Save_Mark := Uintp.Mark;
      Base   : Valid_Uint;
      Ainput : Valid_Uint;

      Digs_Output : Natural := 0;
      --  Counts digits output. In hex mode, but not in decimal mode, we
      --  put an underline after every four hex digits that are output.

      Exponent : Natural := 0;
      --  If the number is too long to fit in the buffer, we switch to an
      --  approximate output format with an exponent. This variable records
      --  the exponent value.

      function Better_In_Hex return Boolean;
      --  Determines if it is better to generate digits in base 16 (result
      --  is true) or base 10 (result is false). The choice is purely a
      --  matter of convenience and aesthetics, so it does not matter which
      --  value is returned from a correctness point of view.

      procedure Image_Char (C : Character);
      --  Output one character

      procedure Image_String (S : String);
      --  Output characters

      procedure Image_Exponent (N : Natural);
      --  Output non-zero exponent. Note that we only use the exponent form in
      --  the buffer case, so we know that To_Buffer is true.

      procedure Image_Uint (U : Valid_Uint);
      --  Internal procedure to output characters of non-negative Uint

      -------------------
      -- Better_In_Hex --
      -------------------

      function Better_In_Hex return Boolean is
         T16 : constant Valid_Uint := Uint_2**Int'(16);
         A   : Valid_Uint := UI_Abs (Input);

      begin
         --  Small values up to 2**16 can always be in decimal

         if A < T16 then
            return False;
         end if;

         --  Otherwise, see if we are a power of 2 or one less than a power
         --  of 2. For the moment these are the only cases printed in hex.

         if A mod Uint_2 = Uint_1 then
            A := A + Uint_1;
         end if;

         loop
            if A mod T16 /= Uint_0 then
               return False;

            else
               A := A / T16;
            end if;

            exit when A < T16;
         end loop;

         while A > Uint_2 loop
            if A mod Uint_2 /= Uint_0 then
               return False;

            else
               A := A / Uint_2;
            end if;
         end loop;

         return True;
      end Better_In_Hex;

      ----------------
      -- Image_Char --
      ----------------

      procedure Image_Char (C : Character) is
      begin
         if To_Buffer then
            if UI_Image_Length + 6 > UI_Image_Max then
               Exponent := Exponent + 1;
            else
               UI_Image_Length := UI_Image_Length + 1;
               UI_Image_Buffer (UI_Image_Length) := C;
            end if;
         else
            Write_Char (C);
         end if;
      end Image_Char;

      --------------------
      -- Image_Exponent --
      --------------------

      procedure Image_Exponent (N : Natural) is
      begin
         if N >= 10 then
            Image_Exponent (N / 10);
         end if;

         UI_Image_Length := UI_Image_Length + 1;
         UI_Image_Buffer (UI_Image_Length) :=
           Character'Val (Character'Pos ('0') + N mod 10);
      end Image_Exponent;

      ------------------
      -- Image_String --
      ------------------

      procedure Image_String (S : String) is
      begin
         for X of S loop
            Image_Char (X);
         end loop;
      end Image_String;

      ----------------
      -- Image_Uint --
      ----------------

      procedure Image_Uint (U : Valid_Uint) is
         H : constant array (Int range 0 .. 15) of Character :=
               "0123456789ABCDEF";

         Q, R : Valid_Uint;
      begin
         UI_Div_Rem (U, Base, Q, R);

         if Q > Uint_0 then
            Image_Uint (Q);
         end if;

         if Digs_Output = 4 and then Base = Uint_16 then
            Image_Char ('_');
            Digs_Output := 0;
         end if;

         Image_Char (H (UI_To_Int (R)));

         Digs_Output := Digs_Output + 1;
      end Image_Uint;

   --  Start of processing for Image_Out

   begin
      if No (Input) then
         Image_String ("No_Uint");
         return;
      end if;

      UI_Image_Length := 0;

      if Input < Uint_0 then
         Image_Char ('-');
         Ainput := -Input;
      else
         Ainput := Input;
      end if;

      if Format = Hex
        or else (Format = Auto and then Better_In_Hex)
      then
         Base := Uint_16;
         Image_Char ('1');
         Image_Char ('6');
         Image_Char ('#');
         Image_Uint (Ainput);
         Image_Char ('#');

      else
         Base := Uint_10;
         Image_Uint (Ainput);
      end if;

      if Exponent /= 0 then
         UI_Image_Length := UI_Image_Length + 1;
         UI_Image_Buffer (UI_Image_Length) := 'E';
         Image_Exponent (Exponent);
      end if;

      Uintp.Release (Marks);
   end Image_Out;

   -------------------
   -- Init_Operand --
   -------------------

   procedure Init_Operand (UI : Valid_Uint; Vec : out UI_Vector) is
      Loc : Int;

      pragma Assert (Vec'First = Int'(1));

   begin
      if Direct (UI) then
         Vec (1) := Direct_Val (UI);

         if Vec (1) >= Base then
            Vec (2) := Vec (1) rem Base;
            Vec (1) := Vec (1) / Base;
         end if;

      else
         Loc := Uints.Table (UI).Loc;

         for J in 1 .. Uints.Table (UI).Length loop
            Vec (J) := Udigits.Table (Loc + J - 1);
         end loop;
      end if;
   end Init_Operand;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Uints.Init;
      Udigits.Init;

      Uint_Int_First := UI_From_Int (Int'First);
      Uint_Int_Last  := UI_From_Int (Int'Last);

      UI_Power_2 (0) := Uint_1;
      UI_Power_2_Set := 0;

      UI_Power_10 (0) := Uint_1;
      UI_Power_10_Set := 0;

      Uints_Min := Uints.Last;
      Udigits_Min := Udigits.Last;

      UI_Ints.Reset;
   end Initialize;

   ---------------------
   -- Least_Sig_Digit --
   ---------------------

   function Least_Sig_Digit (Arg : Valid_Uint) return Int is
      V : Int;

   begin
      if Direct (Arg) then
         V := Direct_Val (Arg);

         if V >= Base then
            V := V mod Base;
         end if;

         --  Note that this result may be negative

         return V;

      else
         return
           Udigits.Table
            (Uints.Table (Arg).Loc + Uints.Table (Arg).Length - 1);
      end if;
   end Least_Sig_Digit;

   ----------
   -- Mark --
   ----------

   function Mark return Save_Mark is
   begin
      return (Save_Uint => Uints.Last, Save_Udigit => Udigits.Last);
   end Mark;

   -----------------------
   -- Most_Sig_2_Digits --
   -----------------------

   procedure Most_Sig_2_Digits
     (Left      : Valid_Uint;
      Right     : Valid_Uint;
      Left_Hat  : out Int;
      Right_Hat : out Int)
   is
   begin
      pragma Assert (Left >= Right);

      if Direct (Left) then
         pragma Assert (Direct (Right));
         Left_Hat  := Direct_Val (Left);
         Right_Hat := Direct_Val (Right);
         return;

      else
         declare
            L1 : constant Int :=
                   Udigits.Table (Uints.Table (Left).Loc);
            L2 : constant Int :=
                   Udigits.Table (Uints.Table (Left).Loc + 1);

         begin
            --  It is not so clear what to return when Arg is negative???

            Left_Hat := abs (L1) * Base + L2;
         end;
      end if;

      declare
         Length_L : constant Int := Uints.Table (Left).Length;
         Length_R : Int;
         R1 : Int;
         R2 : Int;
         T  : Int;

      begin
         if Direct (Right) then
            T := Direct_Val (Right);
            R1 := abs (T / Base);
            R2 := T rem Base;
            Length_R := 2;

         else
            R1 := abs (Udigits.Table (Uints.Table (Right).Loc));
            R2 := Udigits.Table (Uints.Table (Right).Loc + 1);
            Length_R := Uints.Table (Right).Length;
         end if;

         if Length_L = Length_R then
            Right_Hat := R1 * Base + R2;
         elsif Length_L = Length_R + Int_1 then
            Right_Hat := R1;
         else
            Right_Hat := 0;
         end if;
      end;
   end Most_Sig_2_Digits;

   ---------------
   -- N_Digits --
   ---------------

   function N_Digits (Input : Valid_Uint) return Int is
   begin
      if Direct (Input) then
         if Direct_Val (Input) >= Base then
            return 2;
         else
            return 1;
         end if;

      else
         return Uints.Table (Input).Length;
      end if;
   end N_Digits;

   --------------
   -- Num_Bits --
   --------------

   function Num_Bits (Input : Valid_Uint) return Nat is
      Bits : Nat;
      Num  : Nat;

   begin
      --  Largest negative number has to be handled specially, since it is in
      --  Int_Range, but we cannot take the absolute value.

      if Input = Uint_Int_First then
         return Int'Size;

      --  For any other number in Int_Range, get absolute value of number

      elsif UI_Is_In_Int_Range (Input) then
         Num := abs (UI_To_Int (Input));
         Bits := 0;

      --  If not in Int_Range then initialize bit count for all low order
      --  words, and set number to high order digit.

      else
         Bits := Base_Bits * (Uints.Table (Input).Length - 1);
         Num  := abs (Udigits.Table (Uints.Table (Input).Loc));
      end if;

      --  Increase bit count for remaining value in Num

      while Types.">" (Num, 0) loop
         Num := Num / 2;
         Bits := Bits + 1;
      end loop;

      return Bits;
   end Num_Bits;

   ---------
   -- pid --
   ---------

   procedure pid (Input : Uint) is
   begin
      UI_Write (Input, Decimal);
      Write_Eol;
   end pid;

   ---------
   -- pih --
   ---------

   procedure pih (Input : Uint) is
   begin
      UI_Write (Input, Hex);
      Write_Eol;
   end pih;

   -------------
   -- Release --
   -------------

   procedure Release (M : Save_Mark) is
   begin
      Uints.Set_Last   (Valid_Uint'Max (M.Save_Uint, Uints_Min));
      Udigits.Set_Last (Int'Max  (M.Save_Udigit, Udigits_Min));
   end Release;

   ----------------------
   -- Release_And_Save --
   ----------------------

   procedure Release_And_Save (M : Save_Mark; UI : in out Valid_Uint) is
   begin
      if Direct (UI) then
         Release (M);

      else
         declare
            UE_Len : constant Pos := Uints.Table (UI).Length;
            UE_Loc : constant Int := Uints.Table (UI).Loc;

            UD : constant Udigits.Table_Type (1 .. UE_Len) :=
                   Udigits.Table (UE_Loc .. UE_Loc + UE_Len - 1);

         begin
            Release (M);

            Uints.Append ((Length => UE_Len, Loc => Udigits.Last + 1));
            UI := Uints.Last;

            for J in 1 .. UE_Len loop
               Udigits.Append (UD (J));
            end loop;
         end;
      end if;
   end Release_And_Save;

   procedure Release_And_Save (M : Save_Mark; UI1, UI2 : in out Valid_Uint) is
   begin
      if Direct (UI1) then
         Release_And_Save (M, UI2);

      elsif Direct (UI2) then
         Release_And_Save (M, UI1);

      else
         declare
            UE1_Len : constant Pos := Uints.Table (UI1).Length;
            UE1_Loc : constant Int := Uints.Table (UI1).Loc;

            UD1 : constant Udigits.Table_Type (1 .. UE1_Len) :=
                    Udigits.Table (UE1_Loc .. UE1_Loc + UE1_Len - 1);

            UE2_Len : constant Pos := Uints.Table (UI2).Length;
            UE2_Loc : constant Int := Uints.Table (UI2).Loc;

            UD2 : constant Udigits.Table_Type (1 .. UE2_Len) :=
                    Udigits.Table (UE2_Loc .. UE2_Loc + UE2_Len - 1);

         begin
            Release (M);

            Uints.Append ((Length => UE1_Len, Loc => Udigits.Last + 1));
            UI1 := Uints.Last;

            for J in 1 .. UE1_Len loop
               Udigits.Append (UD1 (J));
            end loop;

            Uints.Append ((Length => UE2_Len, Loc => Udigits.Last + 1));
            UI2 := Uints.Last;

            for J in 1 .. UE2_Len loop
               Udigits.Append (UD2 (J));
            end loop;
         end;
      end if;
   end Release_And_Save;

   -------------
   -- UI_Abs --
   -------------

   function UI_Abs (Right : Valid_Uint) return Unat is
   begin
      if Right < Uint_0 then
         return -Right;
      else
         return Right;
      end if;
   end UI_Abs;

   -------------
   -- UI_Add --
   -------------

   function UI_Add (Left : Int; Right : Valid_Uint) return Valid_Uint is
   begin
      return UI_Add (UI_From_Int (Left), Right);
   end UI_Add;

   function UI_Add (Left : Valid_Uint; Right : Int) return Valid_Uint is
   begin
      return UI_Add (Left, UI_From_Int (Right));
   end UI_Add;

   function UI_Add (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint is
   begin
      pragma Assert (Present (Left));
      pragma Assert (Present (Right));
      --  Assertions are here in case we're called from C++ code, which does
      --  not check the predicates.

      --  Simple cases of direct operands and addition of zero

      if Direct (Left) then
         if Direct (Right) then
            return UI_From_Int (Direct_Val (Left) + Direct_Val (Right));

         elsif Int (Left) = Int (Uint_0) then
            return Right;
         end if;

      elsif Direct (Right) and then Int (Right) = Int (Uint_0) then
         return Left;
      end if;

      --  Otherwise full circuit is needed

      declare
         L_Length   : constant Int := N_Digits (Left);
         R_Length   : constant Int := N_Digits (Right);
         L_Vec      : UI_Vector (1 .. L_Length);
         R_Vec      : UI_Vector (1 .. R_Length);
         Sum_Length : Int;
         Tmp_Int    : Int;
         Carry      : Int;
         Borrow     : Int;
         X_Bigger   : Boolean := False;
         Y_Bigger   : Boolean := False;
         Result_Neg : Boolean := False;

      begin
         Init_Operand (Left, L_Vec);
         Init_Operand (Right, R_Vec);

         --  At least one of the two operands is in multi-digit form.
         --  Calculate the number of digits sufficient to hold result.

         if L_Length > R_Length then
            Sum_Length := L_Length + 1;
            X_Bigger := True;
         else
            Sum_Length := R_Length + 1;

            if R_Length > L_Length then
               Y_Bigger := True;
            end if;
         end if;

         --  Make copies of the absolute values of L_Vec and R_Vec into X and Y
         --  both with lengths equal to the maximum possibly needed. This makes
         --  looping over the digits much simpler.

         declare
            X      : UI_Vector (1 .. Sum_Length);
            Y      : UI_Vector (1 .. Sum_Length);
            Tmp_UI : UI_Vector (1 .. Sum_Length);

         begin
            for J in 1 .. Sum_Length - L_Length loop
               X (J) := 0;
            end loop;

            X (Sum_Length - L_Length + 1) := abs L_Vec (1);

            for J in 2 .. L_Length loop
               X (J + (Sum_Length - L_Length)) := L_Vec (J);
            end loop;

            for J in 1 .. Sum_Length - R_Length loop
               Y (J) := 0;
            end loop;

            Y (Sum_Length - R_Length + 1) := abs R_Vec (1);

            for J in 2 .. R_Length loop
               Y (J + (Sum_Length - R_Length)) := R_Vec (J);
            end loop;

            if (L_Vec (1) < Int_0) = (R_Vec (1) < Int_0) then

               --  Same sign so just add

               Carry := 0;
               for J in reverse 1 .. Sum_Length loop
                  Tmp_Int := X (J) + Y (J) + Carry;

                  if Tmp_Int >= Base then
                     Tmp_Int := Tmp_Int - Base;
                     Carry := 1;
                  else
                     Carry := 0;
                  end if;

                  X (J) := Tmp_Int;
               end loop;

               return Vector_To_Uint (X, L_Vec (1) < Int_0);

            else
               --  Find which one has bigger magnitude

               if not (X_Bigger or Y_Bigger) then
                  for J in L_Vec'Range loop
                     if abs L_Vec (J) > abs R_Vec (J) then
                        X_Bigger := True;
                        exit;
                     elsif abs R_Vec (J) > abs L_Vec (J) then
                        Y_Bigger := True;
                        exit;
                     end if;
                  end loop;
               end if;

               --  If they have identical magnitude, just return 0, else swap
               --  if necessary so that X had the bigger magnitude. Determine
               --  if result is negative at this time.

               Result_Neg := False;

               if not (X_Bigger or Y_Bigger) then
                  return Uint_0;

               elsif Y_Bigger then
                  if R_Vec (1) < Int_0 then
                     Result_Neg := True;
                  end if;

                  Tmp_UI := X;
                  X := Y;
                  Y := Tmp_UI;

               else
                  if L_Vec (1) < Int_0 then
                     Result_Neg := True;
                  end if;
               end if;

               --  Subtract Y from the bigger X

               Borrow := 0;

               for J in reverse 1 .. Sum_Length loop
                  Tmp_Int := X (J) - Y (J) + Borrow;

                  if Tmp_Int < Int_0 then
                     Tmp_Int := Tmp_Int + Base;
                     Borrow := -1;
                  else
                     Borrow := 0;
                  end if;

                  X (J) := Tmp_Int;
               end loop;

               return Vector_To_Uint (X, Result_Neg);

            end if;
         end;
      end;
   end UI_Add;

   --------------------------
   -- UI_Decimal_Digits_Hi --
   --------------------------

   function UI_Decimal_Digits_Hi (U : Valid_Uint) return Nat is
   begin
      --  The maximum value of a "digit" is 32767, which is 5 decimal digits,
      --  so an N_Digit number could take up to 5 times this number of digits.
      --  This is certainly too high for large numbers but it is not worth
      --  worrying about.

      return 5 * N_Digits (U);
   end UI_Decimal_Digits_Hi;

   --------------------------
   -- UI_Decimal_Digits_Lo --
   --------------------------

   function UI_Decimal_Digits_Lo (U : Valid_Uint) return Nat is
   begin
      --  The maximum value of a "digit" is 32767, which is more than four
      --  decimal digits, but not a full five digits. The easily computed
      --  minimum number of decimal digits is thus 1 + 4 * the number of
      --  digits. This is certainly too low for large numbers but it is not
      --  worth worrying about.

      return 1 + 4 * (N_Digits (U) - 1);
   end UI_Decimal_Digits_Lo;

   ------------
   -- UI_Div --
   ------------

   function UI_Div (Left : Int; Right : Nonzero_Uint) return Valid_Uint is
   begin
      return UI_Div (UI_From_Int (Left), Right);
   end UI_Div;

   function UI_Div
     (Left : Valid_Uint; Right : Nonzero_Int) return Valid_Uint
   is
   begin
      return UI_Div (Left, UI_From_Int (Right));
   end UI_Div;

   function UI_Div
     (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint
   is
      Quotient  : Valid_Uint;
      Ignored_Remainder : Uint;
   begin
      UI_Div_Rem
        (Left, Right,
         Quotient, Ignored_Remainder,
         Discard_Remainder => True);
      return Quotient;
   end UI_Div;

   ----------------
   -- UI_Div_Rem --
   ----------------

   procedure UI_Div_Rem
     (Left, Right       : Valid_Uint;
      Quotient          : out Uint;
      Remainder         : out Uint;
      Discard_Quotient  : Boolean := False;
      Discard_Remainder : Boolean := False)
   is
   begin
      pragma Assert (Right /= Uint_0);

      Quotient  := No_Uint;
      Remainder := No_Uint;

      --  Cases where both operands are represented directly

      if Direct (Left) and then Direct (Right) then
         declare
            DV_Left  : constant Int := Direct_Val (Left);
            DV_Right : constant Int := Direct_Val (Right);

         begin
            if not Discard_Quotient then
               Quotient := UI_From_Int (DV_Left / DV_Right);
            end if;

            if not Discard_Remainder then
               Remainder := UI_From_Int (DV_Left rem DV_Right);
            end if;

            return;
         end;
      end if;

      declare
         L_Length    : constant Int := N_Digits (Left);
         R_Length    : constant Int := N_Digits (Right);
         Q_Length    : constant Int := L_Length - R_Length + 1;
         L_Vec       : UI_Vector (1 .. L_Length);
         R_Vec       : UI_Vector (1 .. R_Length);
         D           : Int;
         Remainder_I : Int;
         Tmp_Divisor : Int;
         Carry       : Int;
         Tmp_Int     : Int;
         Tmp_Dig     : Int;

         procedure UI_Div_Vector
           (L_Vec     : UI_Vector;
            R_Int     : Int;
            Quotient  : out UI_Vector;
            Remainder : out Int);
         pragma Inline (UI_Div_Vector);
         --  Specialised variant for case where the divisor is a single digit

         procedure UI_Div_Vector
           (L_Vec     : UI_Vector;
            R_Int     : Int;
            Quotient  : out UI_Vector;
            Remainder : out Int)
         is
            Tmp_Int : Int;

         begin
            Remainder := 0;
            for J in L_Vec'Range loop
               Tmp_Int := Remainder * Base + abs L_Vec (J);
               Quotient (Quotient'First + J - L_Vec'First) := Tmp_Int / R_Int;
               Remainder := Tmp_Int rem R_Int;
            end loop;

            if L_Vec (L_Vec'First) < Int_0 then
               Remainder := -Remainder;
            end if;
         end UI_Div_Vector;

      --  Start of processing for UI_Div_Rem

      begin
         --  Result is zero if left operand is shorter than right

         if L_Length < R_Length then
            if not Discard_Quotient then
               Quotient := Uint_0;
            end if;

            if not Discard_Remainder then
               Remainder := Left;
            end if;

            return;
         end if;

         Init_Operand (Left, L_Vec);
         Init_Operand (Right, R_Vec);

         --  Case of right operand is single digit. Here we can simply divide
         --  each digit of the left operand by the divisor, from most to least
         --  significant, carrying the remainder to the next digit (just like
         --  ordinary long division by hand).

         if R_Length = Int_1 then
            Tmp_Divisor := abs R_Vec (1);

            declare
               Quotient_V : UI_Vector (1 .. L_Length);

            begin
               UI_Div_Vector (L_Vec, Tmp_Divisor, Quotient_V, Remainder_I);

               if not Discard_Quotient then
                  Quotient :=
                    Vector_To_Uint
                      (Quotient_V, (L_Vec (1) < Int_0 xor R_Vec (1) < Int_0));
               end if;

               if not Discard_Remainder then
                  Remainder := UI_From_Int (Remainder_I);
               end if;

               return;
            end;
         end if;

         --  The possible simple cases have been exhausted. Now turn to the
         --  algorithm D from the section of Knuth mentioned at the top of
         --  this package.

         Algorithm_D : declare
            Dividend     : UI_Vector (1 .. L_Length + 1);
            Divisor      : UI_Vector (1 .. R_Length);
            Quotient_V   : UI_Vector (1 .. Q_Length);
            Divisor_Dig1 : Int;
            Divisor_Dig2 : Int;
            Q_Guess      : Int;
            R_Guess      : Int;

         begin
            --  [ NORMALIZE ] (step D1 in the algorithm). First calculate the
            --  scale d, and then multiply Left and Right (u and v in the book)
            --  by d to get the dividend and divisor to work with.

            D := Base / (abs R_Vec (1) + 1);

            Dividend (1) := 0;
            Dividend (2) := abs L_Vec (1);

            for J in 3 .. L_Length + Int_1 loop
               Dividend (J) := L_Vec (J - 1);
            end loop;

            Divisor (1) := abs R_Vec (1);

            for J in Int_2 .. R_Length loop
               Divisor (J) := R_Vec (J);
            end loop;

            if D > Int_1 then

               --  Multiply Dividend by d

               Carry := 0;
               for J in reverse Dividend'Range loop
                  Tmp_Int      := Dividend (J) * D + Carry;
                  Dividend (J) := Tmp_Int rem Base;
                  Carry        := Tmp_Int / Base;
               end loop;

               --  Multiply Divisor by d

               Carry := 0;
               for J in reverse Divisor'Range loop
                  Tmp_Int      := Divisor (J) * D + Carry;
                  Divisor (J)  := Tmp_Int rem Base;
                  Carry        := Tmp_Int / Base;
               end loop;
            end if;

            --  Main loop of long division algorithm

            Divisor_Dig1 := Divisor (1);
            Divisor_Dig2 := Divisor (2);

            for J in Quotient_V'Range loop

               --  [ CALCULATE Q (hat) ] (step D3 in the algorithm)

               --  Note: this version of step D3 is from the original published
               --  algorithm, which is known to have a bug causing overflows.
               --  See: http://www-cs-faculty.stanford.edu/~uno/err2-2e.ps.gz
               --  and http://www-cs-faculty.stanford.edu/~uno/all2-pre.ps.gz.
               --  The code below is the fixed version of this step.

               Tmp_Int := Dividend (J) * Base + Dividend (J + 1);

               --  Initial guess

               Q_Guess := Tmp_Int / Divisor_Dig1;
               R_Guess := Tmp_Int rem Divisor_Dig1;

               --  Refine the guess

               while Q_Guess >= Base
                 or else Divisor_Dig2 * Q_Guess >
                           R_Guess * Base + Dividend (J + 2)
               loop
                  Q_Guess := Q_Guess - 1;
                  R_Guess := R_Guess + Divisor_Dig1;
                  exit when R_Guess >= Base;
               end loop;

               --  [ MULTIPLY & SUBTRACT ] (step D4). Q_Guess * Divisor is
               --  subtracted from the remaining dividend.

               Carry := 0;
               for K in reverse Divisor'Range loop
                  Tmp_Int := Dividend (J + K) - Q_Guess * Divisor (K) + Carry;
                  Tmp_Dig := Tmp_Int rem Base;
                  Carry   := Tmp_Int / Base;

                  if Tmp_Dig < Int_0 then
                     Tmp_Dig := Tmp_Dig + Base;
                     Carry   := Carry - 1;
                  end if;

                  Dividend (J + K) := Tmp_Dig;
               end loop;

               Dividend (J) := Dividend (J) + Carry;

               --  [ TEST REMAINDER ] & [ ADD BACK ] (steps D5 and D6)

               --  Here there is a slight difference from the book: the last
               --  carry is always added in above and below (cancelling each
               --  other). In fact the dividend going negative is used as
               --  the test.

               --  If the Dividend went negative, then Q_Guess was off by
               --  one, so it is decremented, and the divisor is added back
               --  into the relevant portion of the dividend.

               if Dividend (J) < Int_0 then
                  Q_Guess := Q_Guess - 1;

                  Carry := 0;
                  for K in reverse Divisor'Range loop
                     Tmp_Int := Dividend (J + K) + Divisor (K) + Carry;

                     if Tmp_Int >= Base then
                        Tmp_Int := Tmp_Int - Base;
                        Carry := 1;
                     else
                        Carry := 0;
                     end if;

                     Dividend (J + K) := Tmp_Int;
                  end loop;

                  Dividend (J) := Dividend (J) + Carry;
               end if;

               --  Finally we can get the next quotient digit

               Quotient_V (J) := Q_Guess;
            end loop;

            --  [ UNNORMALIZE ] (step D8)

            if not Discard_Quotient then
               Quotient := Vector_To_Uint
                 (Quotient_V, (L_Vec (1) < Int_0 xor R_Vec (1) < Int_0));
            end if;

            if not Discard_Remainder then
               declare
                  Remainder_V : UI_Vector (1 .. R_Length);
                  Ignore : Int;
               begin
                  pragma Assert (D /= Int'(0));
                  UI_Div_Vector
                    (Dividend (Dividend'Last - R_Length + 1 .. Dividend'Last),
                     D,
                     Remainder_V, Ignore);
                  Remainder := Vector_To_Uint (Remainder_V, L_Vec (1) < Int_0);
               end;
            end if;
         end Algorithm_D;
      end;
   end UI_Div_Rem;

   ------------
   -- UI_Eq --
   ------------

   function UI_Eq (Left : Int; Right : Valid_Uint) return Boolean is
   begin
      return not UI_Ne (UI_From_Int (Left), Right);
   end UI_Eq;

   function UI_Eq (Left : Valid_Uint; Right : Int) return Boolean is
   begin
      return not UI_Ne (Left, UI_From_Int (Right));
   end UI_Eq;

   function UI_Eq (Left : Valid_Uint; Right : Valid_Uint) return Boolean is
   begin
      return not UI_Ne (Left, Right);
   end UI_Eq;

   --------------
   -- UI_Expon --
   --------------

   function UI_Expon (Left : Int; Right : Unat) return Valid_Uint is
   begin
      return UI_Expon (UI_From_Int (Left), Right);
   end UI_Expon;

   function UI_Expon (Left : Valid_Uint; Right : Nat) return Valid_Uint is
   begin
      return UI_Expon (Left, UI_From_Int (Right));
   end UI_Expon;

   function UI_Expon (Left : Int; Right : Nat) return Valid_Uint is
   begin
      return UI_Expon (UI_From_Int (Left), UI_From_Int (Right));
   end UI_Expon;

   function UI_Expon
     (Left : Valid_Uint; Right : Unat) return Valid_Uint
   is
   begin
      pragma Assert (Right >= Uint_0);

      --  Any value raised to power of 0 is 1

      if Right = Uint_0 then
         return Uint_1;

      --  0 to any positive power is 0

      elsif Left = Uint_0 then
         return Uint_0;

      --  1 to any power is 1

      elsif Left = Uint_1 then
         return Uint_1;

      --  Any value raised to power of 1 is that value

      elsif Right = Uint_1 then
         return Left;

      --  Cases which can be done by table lookup

      elsif Right <= Uint_128 then

         --  2**N for N in 2 .. 128

         if Left = Uint_2 then
            declare
               Right_Int : constant Int := Direct_Val (Right);

            begin
               if Right_Int > UI_Power_2_Set then
                  for J in UI_Power_2_Set + Int_1 .. Right_Int loop
                     UI_Power_2 (J) := UI_Power_2 (J - Int_1) * Int_2;
                     Uints_Min := Uints.Last;
                     Udigits_Min := Udigits.Last;
                  end loop;

                  UI_Power_2_Set := Right_Int;
               end if;

               return UI_Power_2 (Right_Int);
            end;

         --  10**N for N in 2 .. 128

         elsif Left = Uint_10 then
            declare
               Right_Int : constant Int := Direct_Val (Right);

            begin
               if Right_Int > UI_Power_10_Set then
                  for J in UI_Power_10_Set + Int_1 .. Right_Int loop
                     UI_Power_10 (J) := UI_Power_10 (J - Int_1) * Int (10);
                     Uints_Min := Uints.Last;
                     Udigits_Min := Udigits.Last;
                  end loop;

                  UI_Power_10_Set := Right_Int;
               end if;

               return UI_Power_10 (Right_Int);
            end;
         end if;
      end if;

      --  If we fall through, then we have the general case (see Knuth 4.6.3)

      declare
         N       : Valid_Uint := Right;
         Squares : Valid_Uint := Left;
         Result  : Valid_Uint := Uint_1;
         M       : constant Uintp.Save_Mark := Uintp.Mark;

      begin
         loop
            if (Least_Sig_Digit (N) mod Int_2) = Int_1 then
               Result := Result * Squares;
            end if;

            N := N / Uint_2;
            exit when N = Uint_0;
            Squares := Squares * Squares;
         end loop;

         Uintp.Release_And_Save (M, Result);
         return Result;
      end;
   end UI_Expon;

   ----------------
   -- UI_From_CC --
   ----------------

   function UI_From_CC (Input : Char_Code) return Valid_Uint is
   begin
      return UI_From_Int (Int (Input));
   end UI_From_CC;

   -----------------
   -- UI_From_Int --
   -----------------

   function UI_From_Int (Input : Int) return Valid_Uint is
      U : Uint;

   begin
      if Min_Direct <= Input and then Input <= Max_Direct then
         return Valid_Uint (Int (Uint_Direct_Bias) + Input);
      end if;

      --  If already in the hash table, return entry

      U := UI_Ints.Get (Input);

      if Present (U) then
         return U;
      end if;

      --  For values of larger magnitude, compute digits into a vector and call
      --  Vector_To_Uint.

      declare
         Max_For_Int : constant := 3;
         --  Base is defined so that 3 Uint digits is sufficient to hold the
         --  largest possible Int value.

         V : UI_Vector (1 .. Max_For_Int);

         Temp_Integer : Int := Input;

      begin
         for J in reverse V'Range loop
            V (J) := abs (Temp_Integer rem Base);
            Temp_Integer := Temp_Integer / Base;
         end loop;

         U := Vector_To_Uint (V, Input < Int_0);
         UI_Ints.Set (Input, U);
         Uints_Min := Uints.Last;
         Udigits_Min := Udigits.Last;
         return U;
      end;
   end UI_From_Int;

   ----------------------
   -- UI_From_Integral --
   ----------------------

   function UI_From_Integral (Input : In_T) return Valid_Uint is
   begin
      --  If in range of our normal conversion function, use it so we can use
      --  direct access and our cache.

      if In_T'Size <= Int'Size
        or else Input in In_T (Int'First) .. In_T (Int'Last)
      then
         return UI_From_Int (Int (Input));

      else
         --  For values of larger magnitude, compute digits into a vector and
         --  call Vector_To_Uint.

         declare
            Max_For_In_T : constant Int  := 3 * In_T'Size / Int'Size;
            Our_Base     : constant In_T := In_T (Base);
            Temp_Integer : In_T := Input;
            --  Base is defined so that 3 Uint digits is sufficient to hold the
            --  largest possible Int value.

            U : Valid_Uint;
            V : UI_Vector (1 .. Max_For_In_T);

         begin
            for J in reverse V'Range loop
               V (J) := Int (abs (Temp_Integer rem Our_Base));
               Temp_Integer := Temp_Integer / Our_Base;
            end loop;

            U := Vector_To_Uint (V, Input < 0);
            Uints_Min := Uints.Last;
            Udigits_Min := Udigits.Last;

            return U;
         end;
      end if;
   end UI_From_Integral;

   ------------
   -- UI_GCD --
   ------------

   --  Lehmer's algorithm for GCD

   --  The idea is to avoid using multiple precision arithmetic wherever
   --  possible, substituting Int arithmetic instead. See Knuth volume II,
   --  Algorithm L (page 329).

   --  We use the same notation as Knuth (U_Hat standing for the obvious)

   function UI_GCD (Uin, Vin : Valid_Uint) return Valid_Uint is
      U, V : Valid_Uint;
      --  Copies of Uin and Vin

      U_Hat, V_Hat : Int;
      --  The most Significant digits of U,V

      A, B, C, D, T, Q, Den1, Den2 : Int;

      Tmp_UI : Valid_Uint;
      Marks  : constant Uintp.Save_Mark := Uintp.Mark;
      Iterations : Integer := 0;

   begin
      pragma Assert (Uin >= Vin);
      pragma Assert (Vin >= Uint_0);

      U := Uin;
      V := Vin;

      loop
         Iterations := Iterations + 1;

         if Direct (V) then
            if V = Uint_0 then
               return U;
            else
               return
                 UI_From_Int (GCD (Direct_Val (V), UI_To_Int (U rem V)));
            end if;
         end if;

         Most_Sig_2_Digits (U, V, U_Hat, V_Hat);
         A := 1;
         B := 0;
         C := 0;
         D := 1;

         loop
            --  We might overflow and get division by zero here. This just
            --  means we cannot take the single precision step

            Den1 := V_Hat + C;
            Den2 := V_Hat + D;
            exit when Den1 = Int_0 or else Den2 = Int_0;

            --  Compute Q, the trial quotient

            Q := (U_Hat + A) / Den1;

            exit when Q /= ((U_Hat + B) / Den2);

            --  A single precision step Euclid step will give same answer as a
            --  multiprecision one.

            T := A - (Q * C);
            A := C;
            C := T;

            T := B - (Q * D);
            B := D;
            D := T;

            T := U_Hat - (Q * V_Hat);
            U_Hat := V_Hat;
            V_Hat := T;

         end loop;

         --  Take a multiprecision Euclid step

         if B = Int_0 then

            --  No single precision steps take a regular Euclid step

            Tmp_UI := U rem V;
            U := V;
            V := Tmp_UI;

         else
            --  Use prior single precision steps to compute this Euclid step

            Tmp_UI := (UI_From_Int (A) * U) + (UI_From_Int (B) * V);
            V := (UI_From_Int (C) * U) + (UI_From_Int (D) * V);
            U := Tmp_UI;
         end if;

         --  If the operands are very different in magnitude, the loop will
         --  generate large amounts of short-lived data, which it is worth
         --  removing periodically.

         if Iterations > 100 then
            Release_And_Save (Marks, U, V);
            Iterations := 0;
         end if;
      end loop;
   end UI_GCD;

   ------------
   -- UI_Ge --
   ------------

   function UI_Ge (Left : Int; Right : Valid_Uint) return Boolean is
   begin
      return not UI_Lt (UI_From_Int (Left), Right);
   end UI_Ge;

   function UI_Ge (Left : Valid_Uint; Right : Int) return Boolean is
   begin
      return not UI_Lt (Left, UI_From_Int (Right));
   end UI_Ge;

   function UI_Ge (Left : Valid_Uint; Right : Valid_Uint) return Boolean is
   begin
      return not UI_Lt (Left, Right);
   end UI_Ge;

   ------------
   -- UI_Gt --
   ------------

   function UI_Gt (Left : Int; Right : Valid_Uint) return Boolean is
   begin
      return UI_Lt (Right, UI_From_Int (Left));
   end UI_Gt;

   function UI_Gt (Left : Valid_Uint; Right : Int) return Boolean is
   begin
      return UI_Lt (UI_From_Int (Right), Left);
   end UI_Gt;

   function UI_Gt (Left : Valid_Uint; Right : Valid_Uint) return Boolean is
   begin
      return UI_Lt (Left => Right, Right => Left);
   end UI_Gt;

   ---------------
   -- UI_Image --
   ---------------

   procedure UI_Image (Input : Uint; Format : UI_Format := Auto) is
   begin
      Image_Out (Input, True, Format);
   end UI_Image;

   function UI_Image
     (Input  : Uint;
      Format : UI_Format := Auto) return String
   is
   begin
      Image_Out (Input, True, Format);
      return UI_Image_Buffer (1 .. UI_Image_Length);
   end UI_Image;

   -------------------------
   -- UI_Is_In_Int_Range --
   -------------------------

   function UI_Is_In_Int_Range (Input : Valid_Uint) return Boolean is
      pragma Assert (Present (Input));
      --  Assertion is here in case we're called from C++ code, which does
      --  not check the predicates.
   begin
      --  Make sure we don't get called before Initialize

      pragma Assert (Uint_Int_First /= Uint_0);

      if Direct (Input) then
         return True;
      else
         return Input >= Uint_Int_First and then Input <= Uint_Int_Last;
      end if;
   end UI_Is_In_Int_Range;

   ------------
   -- UI_Le --
   ------------

   function UI_Le (Left : Int; Right : Valid_Uint) return Boolean is
   begin
      return not UI_Lt (Right, UI_From_Int (Left));
   end UI_Le;

   function UI_Le (Left : Valid_Uint; Right : Int) return Boolean is
   begin
      return not UI_Lt (UI_From_Int (Right), Left);
   end UI_Le;

   function UI_Le (Left : Valid_Uint; Right : Valid_Uint) return Boolean is
   begin
      return not UI_Lt (Left => Right, Right => Left);
   end UI_Le;

   ------------
   -- UI_Lt --
   ------------

   function UI_Lt (Left : Int; Right : Valid_Uint) return Boolean is
   begin
      return UI_Lt (UI_From_Int (Left), Right);
   end UI_Lt;

   function UI_Lt (Left : Valid_Uint; Right : Int) return Boolean is
   begin
      return UI_Lt (Left, UI_From_Int (Right));
   end UI_Lt;

   function UI_Lt (Left : Valid_Uint; Right : Valid_Uint) return Boolean is
   begin
      pragma Assert (Present (Left));
      pragma Assert (Present (Right));
      --  Assertions are here in case we're called from C++ code, which does
      --  not check the predicates.

      --  Quick processing for identical arguments

      if Int (Left) = Int (Right) then
         return False;

      --  Quick processing for both arguments directly represented

      elsif Direct (Left) and then Direct (Right) then
         return Int (Left) < Int (Right);

      --  At least one argument is more than one digit long

      else
         declare
            L_Length : constant Int := N_Digits (Left);
            R_Length : constant Int := N_Digits (Right);

            L_Vec : UI_Vector (1 .. L_Length);
            R_Vec : UI_Vector (1 .. R_Length);

         begin
            Init_Operand (Left, L_Vec);
            Init_Operand (Right, R_Vec);

            if L_Vec (1) < Int_0 then

               --  First argument negative, second argument non-negative

               if R_Vec (1) >= Int_0 then
                  return True;

               --  Both arguments negative

               else
                  if L_Length /= R_Length then
                     return L_Length > R_Length;

                  elsif L_Vec (1) /= R_Vec (1) then
                     return L_Vec (1) < R_Vec (1);

                  else
                     for J in 2 .. L_Vec'Last loop
                        if L_Vec (J) /= R_Vec (J) then
                           return L_Vec (J) > R_Vec (J);
                        end if;
                     end loop;

                     return False;
                  end if;
               end if;

            else
               --  First argument non-negative, second argument negative

               if R_Vec (1) < Int_0 then
                  return False;

               --  Both arguments non-negative

               else
                  if L_Length /= R_Length then
                     return L_Length < R_Length;
                  else
                     for J in L_Vec'Range loop
                        if L_Vec (J) /= R_Vec (J) then
                           return L_Vec (J) < R_Vec (J);
                        end if;
                     end loop;

                     return False;
                  end if;
               end if;
            end if;
         end;
      end if;
   end UI_Lt;

   ------------
   -- UI_Max --
   ------------

   function UI_Max (Left : Int; Right : Valid_Uint) return Valid_Uint is
   begin
      return UI_Max (UI_From_Int (Left), Right);
   end UI_Max;

   function UI_Max (Left : Valid_Uint; Right : Int) return Valid_Uint is
   begin
      return UI_Max (Left, UI_From_Int (Right));
   end UI_Max;

   function UI_Max (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint is
   begin
      if Left >= Right then
         return Left;
      else
         return Right;
      end if;
   end UI_Max;

   ------------
   -- UI_Min --
   ------------

   function UI_Min (Left : Int; Right : Valid_Uint) return Valid_Uint is
   begin
      return UI_Min (UI_From_Int (Left), Right);
   end UI_Min;

   function UI_Min (Left : Valid_Uint; Right : Int) return Valid_Uint is
   begin
      return UI_Min (Left, UI_From_Int (Right));
   end UI_Min;

   function UI_Min (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint is
   begin
      if Left <= Right then
         return Left;
      else
         return Right;
      end if;
   end UI_Min;

   -------------
   -- UI_Mod --
   -------------

   function UI_Mod (Left : Int; Right : Nonzero_Uint) return Valid_Uint is
   begin
      return UI_Mod (UI_From_Int (Left), Right);
   end UI_Mod;

   function UI_Mod
     (Left : Valid_Uint; Right : Nonzero_Int) return Valid_Uint
   is
   begin
      return UI_Mod (Left, UI_From_Int (Right));
   end UI_Mod;

   function UI_Mod
     (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint
   is
      Urem : constant Valid_Uint := Left rem Right;

   begin
      if (Left < Uint_0) = (Right < Uint_0)
        or else Urem = Uint_0
      then
         return Urem;
      else
         return Right + Urem;
      end if;
   end UI_Mod;

   -------------------------------
   -- UI_Modular_Exponentiation --
   -------------------------------

   function UI_Modular_Exponentiation
     (B      : Valid_Uint;
      E      : Valid_Uint;
      Modulo : Valid_Uint) return Valid_Uint
   is
      M : constant Save_Mark := Mark;

      Result   : Valid_Uint := Uint_1;
      Base     : Valid_Uint := B;
      Exponent : Valid_Uint := E;

   begin
      while Exponent /= Uint_0 loop
         if Least_Sig_Digit (Exponent) rem Int'(2) = Int'(1) then
            Result := (Result * Base) rem Modulo;
         end if;

         Exponent := Exponent / Uint_2;
         Base := (Base * Base) rem Modulo;
      end loop;

      Release_And_Save (M, Result);
      return Result;
   end UI_Modular_Exponentiation;

   ------------------------
   -- UI_Modular_Inverse --
   ------------------------

   function UI_Modular_Inverse
     (N : Valid_Uint; Modulo : Valid_Uint) return Valid_Uint
   is
      M : constant Save_Mark := Mark;
      U : Valid_Uint;
      V : Valid_Uint;
      Q : Valid_Uint;
      R : Valid_Uint;
      X : Valid_Uint;
      Y : Valid_Uint;
      T : Valid_Uint;
      S : Int := 1;

   begin
      U := Modulo;
      V := N;

      X := Uint_1;
      Y := Uint_0;

      loop
         UI_Div_Rem (U, V, Quotient => Q, Remainder => R);

         U := V;
         V := R;

         T := X;
         X := Y + Q * X;
         Y := T;
         S := -S;

         exit when R = Uint_1;
      end loop;

      if S = Int'(-1) then
         X := Modulo - X;
      end if;

      Release_And_Save (M, X);
      return X;
   end UI_Modular_Inverse;

   ------------
   -- UI_Mul --
   ------------

   function UI_Mul (Left : Int; Right : Valid_Uint) return Valid_Uint is
   begin
      return UI_Mul (UI_From_Int (Left), Right);
   end UI_Mul;

   function UI_Mul (Left : Valid_Uint; Right : Int) return Valid_Uint is
   begin
      return UI_Mul (Left, UI_From_Int (Right));
   end UI_Mul;

   function UI_Mul (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint is
   begin
      --  Case where product fits in the range of a 32-bit integer

      if Int (Left)  <= Int (Uint_Max_Simple_Mul)
           and then
         Int (Right) <= Int (Uint_Max_Simple_Mul)
      then
         return UI_From_Int (Direct_Val (Left) * Direct_Val (Right));
      end if;

      --  Otherwise we have the general case (Algorithm M in Knuth)

      declare
         L_Length : constant Int := N_Digits (Left);
         R_Length : constant Int := N_Digits (Right);
         L_Vec    : UI_Vector (1 .. L_Length);
         R_Vec    : UI_Vector (1 .. R_Length);
         Neg      : Boolean;

      begin
         Init_Operand (Left, L_Vec);
         Init_Operand (Right, R_Vec);
         Neg := (L_Vec (1) < Int_0) xor (R_Vec (1) < Int_0);
         L_Vec (1) := abs (L_Vec (1));
         R_Vec (1) := abs (R_Vec (1));

         Algorithm_M : declare
            Product : UI_Vector (1 .. L_Length + R_Length);
            Tmp_Sum : Int;
            Carry   : Int;

         begin
            for J in Product'Range loop
               Product (J) := 0;
            end loop;

            for J in reverse R_Vec'Range loop
               Carry := 0;
               for K in reverse L_Vec'Range loop
                  Tmp_Sum :=
                    L_Vec (K) * R_Vec (J) + Product (J + K) + Carry;
                  Product (J + K) := Tmp_Sum rem Base;
                  Carry := Tmp_Sum / Base;
               end loop;

               Product (J) := Carry;
            end loop;

            return Vector_To_Uint (Product, Neg);
         end Algorithm_M;
      end;
   end UI_Mul;

   ------------
   -- UI_Ne --
   ------------

   function UI_Ne (Left : Int; Right : Valid_Uint) return Boolean is
   begin
      return UI_Ne (UI_From_Int (Left), Right);
   end UI_Ne;

   function UI_Ne (Left : Valid_Uint; Right : Int) return Boolean is
   begin
      return UI_Ne (Left, UI_From_Int (Right));
   end UI_Ne;

   function UI_Ne (Left : Valid_Uint; Right : Valid_Uint) return Boolean is
   begin
      pragma Assert (Present (Left));
      pragma Assert (Present (Right));
      --  Assertions are here in case we're called from C++ code, which does
      --  not check the predicates.

      --  Quick processing for identical arguments

      if Int (Left) = Int (Right) then
         return False;
      end if;

      --  See if left operand directly represented

      if Direct (Left) then

         --  If right operand directly represented then compare

         if Direct (Right) then
            return Int (Left) /= Int (Right);

         --  Left operand directly represented, right not, must be unequal

         else
            return True;
         end if;

      --  Right operand directly represented, left not, must be unequal

      elsif Direct (Right) then
         return True;
      end if;

      --  Otherwise both multi-word, do comparison

      declare
         Size      : constant Int := N_Digits (Left);
         Left_Loc  : Int;
         Right_Loc : Int;

      begin
         if Size /= N_Digits (Right) then
            return True;
         end if;

         Left_Loc  := Uints.Table (Left).Loc;
         Right_Loc := Uints.Table (Right).Loc;

         for J in Int_0 .. Size - Int_1 loop
            if Udigits.Table (Left_Loc + J) /=
               Udigits.Table (Right_Loc + J)
            then
               return True;
            end if;
         end loop;

         return False;
      end;
   end UI_Ne;

   ----------------
   -- UI_Negate --
   ----------------

   function UI_Negate (Right : Valid_Uint) return Valid_Uint is
   begin
      --  Case where input is directly represented. Note that since the range
      --  of Direct values is non-symmetrical, the result may not be directly
      --  represented, this is taken care of in UI_From_Int.

      if Direct (Right) then
         return UI_From_Int (-Direct_Val (Right));

      --  Full processing for multi-digit case. Note that we cannot just copy
      --  the value to the end of the table negating the first digit, since the
      --  range of Direct values is non-symmetrical, so we can have a negative
      --  value that is not Direct whose negation can be represented directly.

      else
         declare
            R_Length : constant Int := N_Digits (Right);
            R_Vec    : UI_Vector (1 .. R_Length);
            Neg      : Boolean;

         begin
            Init_Operand (Right, R_Vec);
            Neg := R_Vec (1) > Int_0;
            R_Vec (1) := abs R_Vec (1);
            return Vector_To_Uint (R_Vec, Neg);
         end;
      end if;
   end UI_Negate;

   -------------
   -- UI_Rem --
   -------------

   function UI_Rem (Left : Int; Right : Nonzero_Uint) return Valid_Uint is
   begin
      return UI_Rem (UI_From_Int (Left), Right);
   end UI_Rem;

   function UI_Rem
     (Left : Valid_Uint; Right : Nonzero_Int) return Valid_Uint
   is
   begin
      return UI_Rem (Left, UI_From_Int (Right));
   end UI_Rem;

   function UI_Rem
     (Left : Valid_Uint; Right : Nonzero_Uint) return Valid_Uint
   is
      Remainder : Valid_Uint;
      Ignored_Quotient  : Uint;

   begin
      pragma Assert (Right /= Uint_0);

      if Direct (Right) and then Direct (Left) then
         return UI_From_Int (Direct_Val (Left) rem Direct_Val (Right));

      else
         UI_Div_Rem
           (Left, Right, Ignored_Quotient, Remainder,
            Discard_Quotient => True);
         return Remainder;
      end if;
   end UI_Rem;

   ------------
   -- UI_Sub --
   ------------

   function UI_Sub (Left : Int; Right : Valid_Uint) return Valid_Uint is
   begin
      return UI_Add (Left, -Right);
   end UI_Sub;

   function UI_Sub (Left : Valid_Uint; Right : Int) return Valid_Uint is
   begin
      return UI_Add (Left, -Right);
   end UI_Sub;

   function UI_Sub (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint is
   begin
      if Direct (Left) and then Direct (Right) then
         return UI_From_Int (Direct_Val (Left) - Direct_Val (Right));
      else
         return UI_Add (Left, -Right);
      end if;
   end UI_Sub;

   --------------
   -- UI_To_CC --
   --------------

   function UI_To_CC (Input : Valid_Uint) return Char_Code is
   begin
      --  Char_Code and Int have equal upper bounds, so simply guard against
      --  negative Input and reuse conversion to Int. We trust that conversion
      --  to Int will raise Constraint_Error when Input is too large.

      pragma Assert
        (Char_Code'First = 0 and then Int (Char_Code'Last) = Int'Last);

      if Input >= Uint_0 then
         return Char_Code (UI_To_Int (Input));
      else
         raise Constraint_Error;
      end if;
   end UI_To_CC;

   ---------------
   -- UI_To_Int --
   ---------------

   function UI_To_Int (Input : Valid_Uint) return Int is
   begin
      if Direct (Input) then
         return Direct_Val (Input);

      --  Case of input is more than one digit

      else
         declare
            In_Length : constant Int := N_Digits (Input);
            In_Vec    : UI_Vector (1 .. In_Length);
            Ret_Int   : Int;

         begin
            --  Uints of more than one digit could be outside the range for
            --  Ints. Caller should have checked for this if not certain.
            --  Constraint_Error to attempt to convert from value outside
            --  Int'Range.

            if not UI_Is_In_Int_Range (Input) then
               raise Constraint_Error;
            end if;

            --  Otherwise, proceed ahead, we are OK

            Init_Operand (Input, In_Vec);
            Ret_Int := 0;

            --  Calculate -|Input| and then negates if value is positive. This
            --  handles our current definition of Int (based on 2s complement).
            --  Is it secure enough???

            for Idx in In_Vec'Range loop
               Ret_Int := Ret_Int * Base - abs In_Vec (Idx);
            end loop;

            if In_Vec (1) < Int_0 then
               return Ret_Int;
            else
               return -Ret_Int;
            end if;
         end;
      end if;
   end UI_To_Int;

   -----------------
   -- UI_To_Uns64 --
   -----------------

   function UI_To_Unsigned_64 (Input : Valid_Uint) return Unsigned_64 is
   begin
      if Input < Uint_0 then
         raise Constraint_Error;
      end if;

      if Direct (Input) then
         return Unsigned_64 (Direct_Val (Input));

      --  Case of input is more than one digit

      else
         if Input >= Uint_2**Int'(64) then
            raise Constraint_Error;
         end if;

         declare
            In_Length : constant Int := N_Digits (Input);
            In_Vec    : UI_Vector (1 .. In_Length);
            Ret_Int   : Unsigned_64 := 0;

         begin
            Init_Operand (Input, In_Vec);

            for Idx in In_Vec'Range loop
               Ret_Int :=
                 Ret_Int * Unsigned_64 (Base) + Unsigned_64 (In_Vec (Idx));
            end loop;

            return Ret_Int;
         end;
      end if;
   end UI_To_Unsigned_64;

   --------------
   -- UI_Write --
   --------------

   procedure UI_Write (Input : Uint; Format : UI_Format := Auto) is
   begin
      Image_Out (Input, False, Format);
   end UI_Write;

   ---------------------
   -- Vector_To_Uint --
   ---------------------

   function Vector_To_Uint
     (In_Vec   : UI_Vector;
      Negative : Boolean) return Valid_Uint
   is
      Size : Int;
      Val  : Int;

   begin
      --  The vector can contain leading zeros. These are not stored in the
      --  table, so loop through the vector looking for first non-zero digit

      for J in In_Vec'Range loop
         if In_Vec (J) /= Int_0 then

            --  The length of the value is the length of the rest of the vector

            Size := In_Vec'Last - J + 1;

            --  One digit value can always be represented directly

            if Size = Int_1 then
               if Negative then
                  return Valid_Uint (Int (Uint_Direct_Bias) - In_Vec (J));
               else
                  return Valid_Uint (Int (Uint_Direct_Bias) + In_Vec (J));
               end if;

            --  Positive two digit values may be in direct representation range

            elsif Size = Int_2 and then not Negative then
               Val := In_Vec (J) * Base + In_Vec (J + 1);

               if Val <= Max_Direct then
                  return Valid_Uint (Int (Uint_Direct_Bias) + Val);
               end if;
            end if;

            --  The value is outside the direct representation range and must
            --  therefore be stored in the table. Expand the table to contain
            --  the count and digits. The index of the new table entry will be
            --  returned as the result.

            Uints.Append ((Length => Size, Loc => Udigits.Last + 1));

            if Negative then
               Val := -In_Vec (J);
            else
               Val := +In_Vec (J);
            end if;

            Udigits.Append (Val);

            for K in 2 .. Size loop
               Udigits.Append (In_Vec (J + K - 1));
            end loop;

            return Uints.Last;
         end if;
      end loop;

      --  Dropped through loop only if vector contained all zeros

      return Uint_0;
   end Vector_To_Uint;

end Uintp;
