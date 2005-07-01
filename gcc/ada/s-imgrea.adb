------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                      S Y S T E M . I M G _ R E A L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Img_LLU;        use System.Img_LLU;
with System.Img_Uns;        use System.Img_Uns;
with System.Powten_Table;   use System.Powten_Table;
with System.Unsigned_Types; use System.Unsigned_Types;

package body System.Img_Real is

   --  The following defines the maximum number of digits that we can convert
   --  accurately. This is limited by the precision of Long_Long_Float, and
   --  also by the number of digits we can hold in Long_Long_Unsigned, which
   --  is the integer type we use as an intermediate for the result.

   --  We assume that in practice, the limitation will come from the digits
   --  value, rather than the integer value. This is true for typical IEEE
   --  implementations, and at worst, the only loss is for some precision
   --  in very high precision floating-point output.

   --  Note that in the following, the "-2" accounts for the sign and one
   --  extra digits, since we need the maximum number of 9's that can be
   --  supported, e.g. for the normal 64 bit case, Long_Long_Integer'Width
   --  is 21, since the maximum value (approx 1.6 * 10**19) has 20 digits,
   --  but the maximum number of 9's that can be supported is 19.

   Maxdigs : constant :=
               Natural'Min
                 (Long_Long_Unsigned'Width - 2, Long_Long_Float'Digits);

   Unsdigs : constant := Unsigned'Width - 2;
   --  Number of digits that can be converted using type Unsigned
   --  See above for the explanation of the -2.

   Maxscaling : constant := 5000;
   --  Max decimal scaling required during conversion of floating-point
   --  numbers to decimal. This is used to defend against infinite
   --  looping in the conversion, as can be caused by erroneous executions.
   --  The largest exponent used on any current system is 2**16383, which
   --  is approximately 10**4932, and the highest number of decimal digits
   --  is about 35 for 128-bit floating-point formats, so 5000 leaves
   --  enough room for scaling such values

   function Is_Negative (V : Long_Long_Float) return Boolean;
   pragma Import (Intrinsic, Is_Negative);

   --------------------------
   -- Image_Floating_Point --
   --------------------------

   function Image_Floating_Point
     (V    : Long_Long_Float;
      Digs : Natural)
      return String
   is
      P : Natural := 0;
      S : String (1 .. Long_Long_Float'Width);

   begin
      --  Decide wether a blank should be prepended before the call to
      --  Set_Image_Real. We generate a blank for positive values, and
      --  also for positive zeroes. For negative zeroes, we generate a
      --  space only if Signed_Zeroes is True (the RM only permits the
      --  output of -0.0 on targets where this is the case). We can of
      --  course still see a -0.0 on a target where Signed_Zeroes is
      --  False (since this attribute refers to the proper handling of
      --  negative zeroes, not to their existence).

      if not Is_Negative (V)
        or else (not Long_Long_Float'Signed_Zeros and then V = -0.0)
      then
         S (1) := ' ';
         P := 1;
      end if;

      Set_Image_Real (V, S, P, 1, Digs - 1, 3);
      return S (1 .. P);
   end Image_Floating_Point;

   --------------------------------
   -- Image_Ordinary_Fixed_Point --
   --------------------------------

   function Image_Ordinary_Fixed_Point
     (V    : Long_Long_Float;
      Aft  : Natural)
      return String
   is
      P : Natural := 0;
      S : String (1 .. Long_Long_Float'Width);

   begin
      if V >= 0.0 then
         S (1) := ' ';
         P := 1;
      end if;

      Set_Image_Real (V, S, P, 1, Aft, 0);
      return S (1 .. P);
   end Image_Ordinary_Fixed_Point;

   --------------------
   -- Set_Image_Real --
   --------------------

   procedure Set_Image_Real
     (V    : Long_Long_Float;
      S    : out String;
      P    : in out Natural;
      Fore : Natural;
      Aft  : Natural;
      Exp  : Natural)
   is
      procedure Reset;
      pragma Import (C, Reset, "__gnat_init_float");
      --  We import the floating-point processor reset routine so that we can
      --  be sure the floating-point processor is properly set for conversion
      --  calls (see description of Reset in GNAT.Float_Control (g-flocon.ads).
      --  This is notably need on Windows, where calls to the operating system
      --  randomly reset the processor into 64-bit mode.

      NFrac : constant Natural := Natural'Max (Aft, 1);
      Sign  : Character;
      X     : aliased Long_Long_Float;
      --  This is declared aliased because the expansion of X'Valid passes
      --  X by access and JGNAT requires all access parameters to be aliased.
      --  The Valid attribute probably needs to be handled via a different
      --  expansion for JGNAT, and this use of aliased should be removed
      --  once Valid is handled properly. ???
      Scale : Integer;
      Expon : Integer;

      Field_Max : constant := 255;
      --  This should be the same value as Ada.[Wide_]Text_IO.Field'Last.
      --  It is not worth dragging in Ada.Text_IO to pick up this value,
      --  since it really should never be necessary to change it!

      Digs : String (1 .. 2 * Field_Max + 16);
      --  Array used to hold digits of converted integer value. This is a
      --  large enough buffer to accommodate ludicrous values of Fore and Aft.

      Ndigs : Natural;
      --  Number of digits stored in Digs (and also subscript of last digit)

      procedure Adjust_Scale (S : Natural);
      --  Adjusts the value in X by multiplying or dividing by a power of
      --  ten so that it is in the range 10**(S-1) <= X < 10**S. Includes
      --  adding 0.5 to round the result, readjusting if the rounding causes
      --  the result to wander out of the range. Scale is adjusted to reflect
      --  the power of ten used to divide the result (i.e. one is added to
      --  the scale value for each division by 10.0, or one is subtracted
      --  for each multiplication by 10.0).

      procedure Convert_Integer;
      --  Takes the value in X, outputs integer digits into Digs. On return,
      --  Ndigs is set to the number of digits stored. The digits are stored
      --  in Digs (1 .. Ndigs),

      procedure Set (C : Character);
      --  Sets character C in output buffer

      procedure Set_Blanks_And_Sign (N : Integer);
      --  Sets leading blanks and minus sign if needed. N is the number of
      --  positions to be filled (a minus sign is output even if N is zero
      --  or negative, but for a positive value, if N is non-positive, then
      --  the call has no effect).

      procedure Set_Digs (S, E : Natural);
      --  Set digits S through E from Digs buffer. No effect if S > E

      procedure Set_Special_Fill (N : Natural);
      --  After outputting +Inf, -Inf or NaN, this routine fills out the
      --  rest of the field with * characters. The argument is the number
      --  of characters output so far (either 3 or 4)

      procedure Set_Zeros (N : Integer);
      --  Set N zeros, no effect if N is negative

      pragma Inline (Set);
      pragma Inline (Set_Digs);
      pragma Inline (Set_Zeros);

      ------------------
      -- Adjust_Scale --
      ------------------

      procedure Adjust_Scale (S : Natural) is
         Lo  : Natural;
         Hi  : Natural;
         Mid : Natural;
         XP  : Long_Long_Float;

      begin
         --  Cases where scaling up is required

         if X < Powten (S - 1) then

            --  What we are looking for is a power of ten to multiply X by
            --  so that the result lies within the required range.

            loop
               XP := X * Powten (Maxpow);
               exit when XP >= Powten (S - 1) or Scale < -Maxscaling;
               X := XP;
               Scale := Scale - Maxpow;
            end loop;

            --  The following exception is only raised in case of erroneous
            --  execution, where a number was considered valid but still
            --  fails to scale up. One situation where this can happen is
            --  when a system which is supposed to be IEEE-compliant, but
            --  has been reconfigured to flush denormals to zero.

            if Scale < -Maxscaling then
               raise Constraint_Error;
            end if;

            --  Here we know that we must multiply by at least 10**1 and that
            --  10**Maxpow takes us too far: binary search to find right one.

            --  Because of roundoff errors, it is possible for the value
            --  of XP to be just outside of the interval when Lo >= Hi. In
            --  that case we adjust explicitly by a factor of 10. This
            --  can only happen with a value that is very close to an
            --  exact power of 10.

            Lo := 1;
            Hi := Maxpow;

            loop
               Mid := (Lo + Hi) / 2;
               XP := X * Powten (Mid);

               if XP < Powten (S - 1) then

                  if Lo >= Hi then
                     Mid := Mid + 1;
                     XP := XP * 10.0;
                     exit;

                  else
                     Lo := Mid + 1;
                  end if;

               elsif XP >= Powten (S) then

                  if Lo >= Hi then
                     Mid := Mid - 1;
                     XP := XP / 10.0;
                     exit;

                  else
                     Hi := Mid - 1;
                  end if;

               else
                  exit;
               end if;
            end loop;

            X := XP;
            Scale := Scale - Mid;

         --  Cases where scaling down is required

         elsif X >= Powten (S) then

            --  What we are looking for is a power of ten to divide X by
            --  so that the result lies within the required range.

            loop
               XP := X / Powten (Maxpow);
               exit when XP < Powten (S) or Scale > Maxscaling;
               X := XP;
               Scale := Scale + Maxpow;
            end loop;

            --  The following exception is only raised in case of erroneous
            --  execution, where a number was considered valid but still
            --  fails to scale up. One situation where this can happen is
            --  when a system which is supposed to be IEEE-compliant, but
            --  has been reconfigured to flush denormals to zero.

            if Scale > Maxscaling then
               raise Constraint_Error;
            end if;

            --  Here we know that we must divide by at least 10**1 and that
            --  10**Maxpow takes us too far, binary search to find right one.

            Lo := 1;
            Hi := Maxpow;

            loop
               Mid := (Lo + Hi) / 2;
               XP := X / Powten (Mid);

               if XP < Powten (S - 1) then

                  if Lo >= Hi then
                     XP := XP * 10.0;
                     Mid := Mid - 1;
                     exit;

                  else
                     Hi := Mid - 1;
                  end if;

               elsif XP >= Powten (S) then

                  if Lo >= Hi then
                     XP := XP / 10.0;
                     Mid := Mid + 1;
                     exit;

                  else
                     Lo := Mid + 1;
                  end if;

               else
                  exit;
               end if;
            end loop;

            X := XP;
            Scale := Scale + Mid;

         --  Here we are already scaled right

         else
            null;
         end if;

         --  Round, readjusting scale if needed. Note that if a readjustment
         --  occurs, then it is never necessary to round again, because there
         --  is no possibility of such a second rounding causing a change.

         X := X + 0.5;

         if X >= Powten (S) then
            X := X / 10.0;
            Scale := Scale + 1;
         end if;

      end Adjust_Scale;

      ---------------------
      -- Convert_Integer --
      ---------------------

      procedure Convert_Integer is
      begin
         --  Use Unsigned routine if possible, since on many machines it will
         --  be significantly more efficient than the Long_Long_Unsigned one.

         if X < Powten (Unsdigs) then
            Ndigs := 0;
            Set_Image_Unsigned
              (Unsigned (Long_Long_Float'Truncation (X)),
               Digs, Ndigs);

         --  But if we want more digits than fit in Unsigned, we have to use
         --  the Long_Long_Unsigned routine after all.

         else
            Ndigs := 0;
            Set_Image_Long_Long_Unsigned
              (Long_Long_Unsigned (Long_Long_Float'Truncation (X)),
               Digs, Ndigs);
         end if;
      end Convert_Integer;

      ---------
      -- Set --
      ---------

      procedure Set (C : Character) is
      begin
         P := P + 1;
         S (P) := C;
      end Set;

      -------------------------
      -- Set_Blanks_And_Sign --
      -------------------------

      procedure Set_Blanks_And_Sign (N : Integer) is
      begin
         if Sign = '-' then
            for J in 1 .. N - 1 loop
               Set (' ');
            end loop;

            Set ('-');

         else
            for J in 1 .. N loop
               Set (' ');
            end loop;
         end if;
      end Set_Blanks_And_Sign;

      --------------
      -- Set_Digs --
      --------------

      procedure Set_Digs (S, E : Natural) is
      begin
         for J in S .. E loop
            Set (Digs (J));
         end loop;
      end Set_Digs;

      ----------------------
      -- Set_Special_Fill --
      ----------------------

      procedure Set_Special_Fill (N : Natural) is
         F : Natural;

      begin
         F := Fore + 1 + Aft - N;

         if Exp /= 0 then
            F := F + Exp + 1;
         end if;

         for J in 1 .. F loop
            Set ('*');
         end loop;
      end Set_Special_Fill;

      ---------------
      -- Set_Zeros --
      ---------------

      procedure Set_Zeros (N : Integer) is
      begin
         for J in 1 .. N loop
            Set ('0');
         end loop;
      end Set_Zeros;

   --  Start of processing for Set_Image_Real

   begin
      Reset;
      Scale := 0;

      --  Deal with invalid values first,

      if not V'Valid then

         --  Note that we're taking our chances here, as V might be
         --  an invalid bit pattern resulting from erroneous execution
         --  (caused by using uninitialized variables for example).

         --  No matter what, we'll at least get reasonable behaviour,
         --  converting to infinity or some other value, or causing an
         --  exception to be raised is fine.

         --  If the following test succeeds, then we definitely have
         --  an infinite value, so we print Inf.

         if V > Long_Long_Float'Last then
            Set ('+');
            Set ('I');
            Set ('n');
            Set ('f');
            Set_Special_Fill (4);

         --  In all other cases we print NaN

         elsif V < Long_Long_Float'First then
            Set ('-');
            Set ('I');
            Set ('n');
            Set ('f');
            Set_Special_Fill (4);

         else
            Set ('N');
            Set ('a');
            Set ('N');
            Set_Special_Fill (3);
         end if;

         return;
      end if;

      --  Positive values

      if V > 0.0 then
         X := V;
         Sign := '+';

      --  Negative values

      elsif V < 0.0 then
         X := -V;
         Sign := '-';

      --  Zero values

      elsif V = 0.0 then
         if Long_Long_Float'Signed_Zeros and then Is_Negative (V) then
            Sign := '-';
         else
            Sign := '+';
         end if;

         Set_Blanks_And_Sign (Fore - 1);
         Set ('0');
         Set ('.');
         Set_Zeros (NFrac);

         if Exp /= 0 then
            Set ('E');
            Set ('+');
            Set_Zeros (Natural'Max (1, Exp - 1));
         end if;

         return;

      else
         --  It should not be possible for a NaN to end up here.
         --  Either the 'Valid test has failed, or we have some form
         --  of erroneous execution. Raise Constraint_Error instead of
         --  attempting to go ahead printing the value.

         raise Constraint_Error;
      end if;

      --  X and Sign are set here, and X is known to be a valid,
      --  non-zero floating-point number.

      --  Case of non-zero value with Exp = 0

      if Exp = 0 then

         --  First step is to multiply by 10 ** Nfrac to get an integer
         --  value to be output, an then add 0.5 to round the result.

         declare
            NF : Natural := NFrac;

         begin
            loop
               --  If we are larger than Powten (Maxdigs) now, then
               --  we have too many significant digits, and we have
               --  not even finished multiplying by NFrac (NF shows
               --  the number of unaccounted-for digits).

               if X >= Powten (Maxdigs) then

                  --  In this situation, we only to generate a reasonable
                  --  number of significant digits, and then zeroes after.
                  --  So first we rescale to get:

                  --    10 ** (Maxdigs - 1) <= X < 10 ** Maxdigs

                  --  and then convert the resulting integer

                  Adjust_Scale (Maxdigs);
                  Convert_Integer;

                  --  If that caused rescaling, then add zeros to the end
                  --  of the number to account for this scaling. Also add
                  --  zeroes to account for the undone multiplications

                  for J in 1 .. Scale + NF loop
                     Ndigs := Ndigs + 1;
                     Digs (Ndigs) := '0';
                  end loop;

                  exit;

               --  If multiplication is complete, then convert the resulting
               --  integer after rounding (note that X is non-negative)

               elsif NF = 0 then
                  X := X + 0.5;
                  Convert_Integer;
                  exit;

               --  Otherwise we can go ahead with the multiplication. If it
               --  can be done in one step, then do it in one step.

               elsif NF < Maxpow then
                  X := X * Powten (NF);
                  NF := 0;

               --  If it cannot be done in one step, then do partial scaling

               else
                  X := X * Powten (Maxpow);
                  NF := NF - Maxpow;
               end if;
            end loop;
         end;

         --  If number of available digits is less or equal to NFrac,
         --  then we need an extra zero before the decimal point.

         if Ndigs <= NFrac then
            Set_Blanks_And_Sign (Fore - 1);
            Set ('0');
            Set ('.');
            Set_Zeros (NFrac - Ndigs);
            Set_Digs (1, Ndigs);

         --  Normal case with some digits before the decimal point

         else
            Set_Blanks_And_Sign (Fore - (Ndigs - NFrac));
            Set_Digs (1, Ndigs - NFrac);
            Set ('.');
            Set_Digs (Ndigs - NFrac + 1, Ndigs);
         end if;

      --  Case of non-zero value with non-zero Exp value

      else
         --  If NFrac is less than Maxdigs, then all the fraction digits are
         --  significant, so we can scale the resulting integer accordingly.

         if NFrac < Maxdigs then
            Adjust_Scale (NFrac + 1);
            Convert_Integer;

         --  Otherwise, we get the maximum number of digits available

         else
            Adjust_Scale (Maxdigs);
            Convert_Integer;

            for J in 1 .. NFrac - Maxdigs + 1 loop
               Ndigs := Ndigs + 1;
               Digs (Ndigs) := '0';
               Scale := Scale - 1;
            end loop;
         end if;

         Set_Blanks_And_Sign (Fore - 1);
         Set (Digs (1));
         Set ('.');
         Set_Digs (2, Ndigs);

         --  The exponent is the scaling factor adjusted for the digits
         --  that we output after the decimal point, since these were
         --  included in the scaled digits that we output.

         Expon := Scale + NFrac;

         Set ('E');
         Ndigs := 0;

         if Expon >= 0 then
            Set ('+');
            Set_Image_Unsigned (Unsigned (Expon), Digs, Ndigs);
         else
            Set ('-');
            Set_Image_Unsigned (Unsigned (-Expon), Digs, Ndigs);
         end if;

         Set_Zeros (Exp - Ndigs - 1);
         Set_Digs (1, Ndigs);
      end if;

   end Set_Image_Real;

end System.Img_Real;
