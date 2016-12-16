with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;

package body Opt61_Pkg is

   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);

   subtype Uns64 is Unsigned_64;

   function To_Int is new Ada.Unchecked_Conversion (Uns64, Int64);

   subtype Uns32 is Unsigned_32;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function "+" (A : Uns64; B : Uns32) return Uns64 is (A + Uns64 (B));
   --  Length doubling additions

   function "*" (A, B : Uns32) return Uns64 is (Uns64 (A) * Uns64 (B));
   --  Length doubling multiplication

   function "&" (Hi, Lo : Uns32) return Uns64 is
     (Shift_Left (Uns64 (Hi), 32) or Uns64 (Lo));
   --  Concatenate hi, lo values to form 64-bit result

   function "abs" (X : Int64) return Uns64 is
     (if X = Int64'First then 2**63 else Uns64 (Int64'(abs X)));
   --  Convert absolute value of X to unsigned. Note that we can't just use
   --  the expression of the Else, because it overflows for X = Int64'First.

   function Lo (A : Uns64) return Uns32 is (Uns32 (A and 16#FFFF_FFFF#));
   --  Low order half of 64-bit value

   function Hi (A : Uns64) return Uns32 is (Uns32 (Shift_Right (A, 32)));
   --  High order half of 64-bit value

   -------------------
   -- Double_Divide --
   -------------------

   procedure Double_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean)
   is
      Xu  : constant Uns64 := abs X;
      Yu  : constant Uns64 := abs Y;

      Yhi : constant Uns32 := Hi (Yu);
      Ylo : constant Uns32 := Lo (Yu);

      Zu  : constant Uns64 := abs Z;
      Zhi : constant Uns32 := Hi (Zu);
      Zlo : constant Uns32 := Lo (Zu);

      T1, T2     : Uns64;
      Du, Qu, Ru : Uns64;
      Den_Pos    : Boolean;

   begin
      if Yu = 0 or else Zu = 0 then
         raise Constraint_Error;
      end if;

      --  Compute Y * Z. Note that if the result overflows 64 bits unsigned,
      --  then the rounded result is clearly zero (since the dividend is at
      --  most 2**63 - 1, the extra bit of precision is nice here).

      if Yhi /= 0 then
         if Zhi /= 0 then
            Q := 0;
            R := X;
            return;
         else
            T2 := Yhi * Zlo;
         end if;

      else
         T2 := (if Zhi /= 0 then Ylo * Zhi else 0);
      end if;

      T1 := Ylo * Zlo;
      T2 := T2 + Hi (T1);

      if Hi (T2) /= 0 then
         Q := 0;
         R := X;
         return;
      end if;

      Du := Lo (T2) & Lo (T1);

      --  Set final signs (RM 4.5.5(27-30))

      Den_Pos := (Y < 0) = (Z < 0);

      --  Check overflow case of largest negative number divided by 1

      if X = Int64'First and then Du = 1 and then not Den_Pos then
         raise Constraint_Error;
      end if;

      --  Perform the actual division

      Qu := Xu / Du;
      Ru := Xu rem Du;

      --  Deal with rounding case

      if Round and then Ru > (Du - Uns64'(1)) / Uns64'(2) then
         Qu := Qu + Uns64'(1);
      end if;

      --  Case of dividend (X) sign positive

      if X >= 0 then
         R := To_Int (Ru);
         Q := (if Den_Pos then To_Int (Qu) else -To_Int (Qu));

      --  Case of dividend (X) sign negative

      else
         R := -To_Int (Ru);
         Q := (if Den_Pos then -To_Int (Qu) else To_Int (Qu));
      end if;
   end Double_Divide;

end Opt61_Pkg;
