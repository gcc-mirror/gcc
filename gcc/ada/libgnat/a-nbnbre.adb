------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   ADA.NUMERICS.BIG_NUMBERS.BIG_REALS                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2019, Free Software Foundation, Inc.           --
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

--  This is the default version of this package, based on Big_Integers only.

with Ada.Characters.Conversions; use Ada.Characters.Conversions;

package body Ada.Numerics.Big_Numbers.Big_Reals is

   use Big_Integers;

   procedure Normalize (Arg : in out Big_Real);
   --  Normalize Arg by ensuring that Arg.Den is always positive and that
   --  Arg.Num and Arg.Den always have a GCD of 1.

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Arg : Big_Real) return Boolean is
     (Is_Valid (Arg.Num) and then Is_Valid (Arg.Den));

   ---------
   -- "/" --
   ---------

   function "/" (Num, Den : Big_Integer) return Big_Real is
      Result : Big_Real;
   begin
      if Den = To_Big_Integer (0) then
         raise Constraint_Error with "divide by zero";
      end if;

      Result.Num := Num;
      Result.Den := Den;
      Normalize (Result);
      return Result;
   end "/";

   ---------------
   -- Numerator --
   ---------------

   function Numerator (Arg : Big_Real) return Big_Integer is (Arg.Num);

   -----------------
   -- Denominator --
   -----------------

   function Denominator (Arg : Big_Real) return Big_Positive is (Arg.Den);

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Big_Real) return Boolean is
     (abs L.Num = abs R.Num and then L.Den = R.Den);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Big_Real) return Boolean is
     (abs L.Num * R.Den < abs R.Num * L.Den);

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Big_Real) return Boolean is (not (R < L));

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Big_Real) return Boolean is (R < L);

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Big_Real) return Boolean is (not (L < R));

   -----------------------
   -- Float_Conversions --
   -----------------------

   package body Float_Conversions is

      -----------------
      -- To_Big_Real --
      -----------------

      function To_Big_Real (Arg : Num) return Big_Real is
      begin
         return From_String (Arg'Image);
      end To_Big_Real;

      -------------------
      -- From_Big_Real --
      -------------------

      function From_Big_Real (Arg : Big_Real) return Num is
      begin
         return Num'Value (To_String (Arg));
      end From_Big_Real;

   end Float_Conversions;

   -----------------------
   -- Fixed_Conversions --
   -----------------------

   package body Fixed_Conversions is

      -----------------
      -- To_Big_Real --
      -----------------

      function To_Big_Real (Arg : Num) return Big_Real is
      begin
         return From_String (Arg'Image);
      end To_Big_Real;

      -------------------
      -- From_Big_Real --
      -------------------

      function From_Big_Real (Arg : Big_Real) return Num is
      begin
         return Num'Value (To_String (Arg));
      end From_Big_Real;

   end Fixed_Conversions;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Arg : Big_Real; Fore : Field := 2; Aft : Field := 3; Exp : Field := 0)
      return String
   is
      Zero : constant Big_Integer := To_Big_Integer (0);
      Ten  : constant Big_Integer := To_Big_Integer (10);

      function Leading_Padding
        (Str        : String;
         Min_Length : Field;
         Char       : Character := ' ') return String;
      --  Return padding of Char concatenated with Str so that the resulting
      --  string is at least Min_Length long.

      function Trailing_Padding
        (Str    : String;
         Length : Field;
         Char   : Character := '0') return String;
      --  Return Str with trailing Char removed, and if needed either
      --  truncated or concatenated with padding of Char so that the resulting
      --  string is Length long.

      function Image (N : Natural) return String;
      --  Return image of N, with no leading space.

      function Numerator_Image
        (Num   : Big_Integer;
         After : Natural) return String;
      --  Return image of Num as a float value with After digits after the "."
      --  and taking Fore, Aft, Exp into account.

      -----------
      -- Image --
      -----------

      function Image (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         return S (2 .. S'Last);
      end Image;

      ---------------------
      -- Leading_Padding --
      ---------------------

      function Leading_Padding
        (Str        : String;
         Min_Length : Field;
         Char       : Character := ' ') return String is
      begin
         if Str = "" then
            return Leading_Padding ("0", Min_Length, Char);
         else
            return (1 .. Integer'Max (Integer (Min_Length) - Str'Length, 0)
                           => Char) & Str;
         end if;
      end Leading_Padding;

      ----------------------
      -- Trailing_Padding --
      ----------------------

      function Trailing_Padding
        (Str    : String;
         Length : Field;
         Char   : Character := '0') return String is
      begin
         if Str'Length > 0 and then Str (Str'Last) = Char then
            for J in reverse Str'Range loop
               if Str (J) /= '0' then
                  return Trailing_Padding
                    (Str (Str'First .. J), Length, Char);
               end if;
            end loop;
         end if;

         if Str'Length >= Length then
            return Str (Str'First .. Str'First + Length - 1);
         else
            return Str &
              (1 .. Integer'Max (Integer (Length) - Str'Length, 0)
                      => Char);
         end if;
      end Trailing_Padding;

      ---------------------
      -- Numerator_Image --
      ---------------------

      function Numerator_Image
        (Num   : Big_Integer;
         After : Natural) return String
      is
         Tmp   : constant String := To_String (Num);
         Str   : constant String (1 .. Tmp'Last - 1) := Tmp (2 .. Tmp'Last);
         Index : Integer;

      begin
         if After = 0 then
            return Leading_Padding (Str, Fore) & "."
                   & Trailing_Padding ("0", Aft);
         else
            Index := Str'Last - After;

            if Index < 0 then
               return Leading_Padding ("0", Fore)
                 & "."
                 & Trailing_Padding ((1 .. -Index => '0') & Str, Aft)
                 & (if Exp = 0 then "" else "E+" & Image (Natural (Exp)));
            else
               return Leading_Padding (Str (Str'First .. Index), Fore)
                 & "."
                 & Trailing_Padding (Str (Index + 1 .. Str'Last), Aft)
                 & (if Exp = 0 then "" else "E+" & Image (Natural (Exp)));
            end if;
         end if;
      end Numerator_Image;

   begin
      if Arg.Num < Zero then
         declare
            Str : String := To_String (-Arg, Fore, Aft, Exp);
         begin
            if Str (1) = ' ' then
               for J in 1 .. Str'Last - 1 loop
                  if Str (J + 1) /= ' ' then
                     Str (J) := '-';
                     exit;
                  end if;
               end loop;

               return Str;
            else
               return '-' & Str;
            end if;
         end;
      else
         --  Compute Num * 10^Aft so that we get Aft significant digits
         --  in the integer part (rounded) to display.

         return Numerator_Image
           ((Arg.Num * Ten ** Aft) / Arg.Den, After => Exp + Aft);
      end if;
   end To_String;

   -----------------
   -- From_String --
   -----------------

   function From_String (Arg : String) return Big_Real is
      Ten   : constant Big_Integer := To_Big_Integer (10);
      Frac  : Big_Integer;
      Exp   : Integer := 0;
      Pow   : Natural := 0;
      Index : Natural := 0;
      Last  : Natural := Arg'Last;

   begin
      for J in reverse Arg'Range loop
         if Arg (J) in 'e' | 'E' then
            if Last /= Arg'Last then
               raise Constraint_Error with "multiple exponents specified";
            end if;

            Last := J - 1;
            Exp := Integer'Value (Arg (J + 1 .. Arg'Last));
            Pow := 0;

         elsif Arg (J) = '.' then
            Index := J - 1;
            exit;
         else
            Pow := Pow + 1;
         end if;
      end loop;

      if Index = 0 then
         raise Constraint_Error with "invalid real value";
      end if;

      declare
         Result : Big_Real;
      begin
         Result.Den := Ten ** Pow;
         Result.Num := From_String (Arg (Arg'First .. Index)) * Result.Den;
         Frac := From_String (Arg (Index + 2 .. Last));

         if Result.Num < To_Big_Integer (0) then
            Result.Num := Result.Num - Frac;
         else
            Result.Num := Result.Num + Frac;
         end if;

         if Exp > 0 then
            Result.Num := Result.Num * Ten ** Exp;
         elsif Exp < 0 then
            Result.Den := Result.Den * Ten ** (-Exp);
         end if;

         Normalize (Result);
         return Result;
      end;
   end From_String;

   --------------------------
   -- From_Quotient_String --
   --------------------------

   function From_Quotient_String (Arg : String) return Big_Real is
      Index : Natural := 0;
   begin
      for J in Arg'First + 1 .. Arg'Last - 1 loop
         if Arg (J) = '/' then
            Index := J;
            exit;
         end if;
      end loop;

      if Index = 0 then
         raise Constraint_Error with "no quotient found";
      end if;

      return Big_Integers.From_String (Arg (Arg'First .. Index - 1)) /
        Big_Integers.From_String (Arg (Index + 1 .. Arg'Last));
   end From_Quotient_String;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Arg    : Big_Real) is
   begin
      Wide_Wide_String'Write (Stream, To_Wide_Wide_String (To_String (Arg)));
   end Put_Image;

   ---------
   -- "+" --
   ---------

   function "+" (L : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num;
      Result.Den := L.Den;
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L : Big_Real) return Big_Real is
     (Num => -L.Num, Den => L.Den);

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Big_Real) return Big_Real is
     (Num => abs L.Num, Den => L.Den);

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Den + R.Num * L.Den;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Den - R.Num * L.Den;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Num;
      Result.Den := L.Den * R.Den;
      Normalize (Result);
      return Result;
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Big_Real) return Big_Real is
      Result : Big_Real;
   begin
      Result.Num := L.Num * R.Den;
      Result.Den := L.Den * R.Num;
      Normalize (Result);
      return Result;
   end "/";

   ----------
   -- "**" --
   ----------

   function "**" (L : Big_Real; R : Integer) return Big_Real is
      Result : Big_Real;
   begin
      if R = 0 then
         Result.Num := To_Big_Integer (1);
         Result.Den := To_Big_Integer (1);
      else
         if R < 0 then
            Result.Num := L.Den ** (-R);
            Result.Den := L.Num ** (-R);
         else
            Result.Num := L.Num ** R;
            Result.Den := L.Den ** R;
         end if;

         Normalize (Result);
      end if;

      return Result;
   end "**";

   ---------
   -- Min --
   ---------

   function Min (L, R : Big_Real) return Big_Real is (if L < R then L else R);

   ---------
   -- Max --
   ---------

   function Max (L, R : Big_Real) return Big_Real is (if L > R then L else R);

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (Arg : in out Big_Real) is
   begin
      if Arg.Den < To_Big_Integer (0) then
         Arg.Num := -Arg.Num;
         Arg.Den := -Arg.Den;
      end if;

      declare
         GCD : constant Big_Integer :=
           Greatest_Common_Divisor (Arg.Num, Arg.Den);
      begin
         Arg.Num := Arg.Num / GCD;
         Arg.Den := Arg.Den / GCD;
      end;
   end Normalize;

end Ada.Numerics.Big_Numbers.Big_Reals;
