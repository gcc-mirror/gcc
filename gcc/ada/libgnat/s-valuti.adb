------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . V A L _ U T I L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  Ghost code, loop invariants and assertions in this unit are meant for
--  analysis only, not for run-time checking, as it would be too costly
--  otherwise. This is enforced by setting the assertion policy to Ignore.

pragma Assertion_Policy (Ghost          => Ignore,
                         Loop_Invariant => Ignore,
                         Assert         => Ignore);

with System.Case_Util; use System.Case_Util;

package body System.Val_Util
  with SPARK_Mode
is

   ---------------
   -- Bad_Value --
   ---------------

   procedure Bad_Value (S : String) is
      pragma Annotate (GNATprove, Intentional, "exception might be raised",
                       "Intentional exception from Bad_Value");
   begin
      --  Bad_Value might be called with very long strings allocated on the
      --  heap. Limit the size of the message so that we avoid creating a
      --  Storage_Error during error handling.
      if S'Length > 127 then
         raise Constraint_Error with "bad input for 'Value: """
         & S (S'First .. S'First + 127) & "...""";
      else
         raise Constraint_Error with "bad input for 'Value: """ & S & '"';
      end if;
   end Bad_Value;

   ----------------------
   -- Normalize_String --
   ----------------------

   procedure Normalize_String
     (S             : in out String;
      F, L          : out Integer;
      To_Upper_Case : Boolean)
   is
   begin
      F := S'First;
      L := S'Last;

      --  Case of empty string

      if F > L then
         return;
      end if;

      --  Scan for leading spaces

      while F < L and then S (F) = ' ' loop
         pragma Loop_Invariant (F in S'First .. L - 1);
         pragma Loop_Invariant (for all J in S'First .. F => S (J) = ' ');
         pragma Loop_Variant (Increases => F);
         F := F + 1;
      end loop;

      --  Case of no nonspace characters found. Decrease L to ensure L < F
      --  without risking an overflow if F is Integer'Last.

      if S (F) = ' ' then
         L := L - 1;
         return;
      end if;

      --  Scan for trailing spaces

      while S (L) = ' ' loop
         pragma Loop_Invariant (L in F + 1 .. S'Last);
         pragma Loop_Invariant (for all J in L .. S'Last => S (J) = ' ');
         pragma Loop_Variant (Decreases => L);
         L := L - 1;
      end loop;

      --  Convert to upper case if requested and not a character literal

      if To_Upper_Case and then S (F) /= ''' then
         for J in F .. L loop
            S (J) := To_Upper (S (J));
            pragma Loop_Invariant
              (for all K in F .. J => S (K) = To_Upper (S'Loop_Entry (K)));
         end loop;
      end if;
   end Normalize_String;

   -------------------
   -- Scan_Exponent --
   -------------------

   procedure Scan_Exponent
     (Str  : String;
      Ptr  : not null access Integer;
      Max  : Integer;
      Exp  : out Integer;
      Real : Boolean := False)
   is
      P : Integer := Ptr.all;
      M : Boolean;
      X : Integer;

   begin
      if P >= Max
        or else (Str (P) /= 'E' and then Str (P) /= 'e')
      then
         Exp := 0;
         return;
      end if;
      pragma Annotate
        (CodePeer, False_Positive, "test always false",
         "the slice might be empty or not start with an 'e'");

      --  We have an E/e, see if sign follows

      P := P + 1;

      if Str (P) = '+' then
         P := P + 1;

         if P > Max then
            Exp := 0;
            return;
         else
            M := False;
         end if;

      elsif Str (P) = '-' then
         P := P + 1;

         if P > Max or else not Real then
            Exp := 0;
            return;
         else
            M := True;
         end if;

      else
         M := False;
      end if;

      if Str (P) not in '0' .. '9' then
         Exp := 0;
         return;
      end if;

      --  Scan out the exponent value as an unsigned integer. Values larger
      --  than (Integer'Last / 10) are simply considered large enough here.
      --  This assumption is correct for all machines we know of (e.g. in the
      --  case of 16 bit integers it allows exponents up to 3276, which is
      --  large enough for the largest floating types in base 2.)

      X := 0;

      declare
         Rest : constant String := Str (P .. Max) with Ghost;
         Last : constant Natural := Sp.Last_Number_Ghost (Rest) with Ghost;

      begin
         pragma Assert (Sp.Is_Natural_Format_Ghost (Rest));

         loop
            pragma Assert (Str (P) in '0' .. '9');

            if X < (Integer'Last / 10) then
               X := X * 10 + (Character'Pos (Str (P)) - Character'Pos ('0'));
            end if;

            pragma Loop_Invariant (X >= 0);
            pragma Loop_Invariant (P in Rest'First .. Last);
            pragma Loop_Invariant (Str (P) in '0' .. '9');
            pragma Loop_Invariant
              (Sp.Scan_Natural_Ghost (Rest, Rest'First, 0)
               = Sp.Scan_Natural_Ghost (Rest, P + 1, X));

            P := P + 1;

            exit when P > Max;

            if Str (P) = '_' then
               Scan_Underscore (Str, P, Ptr, Max, False);
            else
               exit when Str (P) not in '0' .. '9';
            end if;
         end loop;

         pragma Assert (P = Last + 1);
      end;

      if M then
         X := -X;
      end if;

      Ptr.all := P;
      Exp := X;
   end Scan_Exponent;

   --------------------
   -- Scan_Plus_Sign --
   --------------------

   procedure Scan_Plus_Sign
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Start : out Positive)
   is
      P : Integer := Ptr.all;

   begin
      if P > Max then
         Bad_Value (Str);
      end if;

      --  Scan past initial blanks

      while Str (P) = ' ' loop
         P := P + 1;

         pragma Loop_Invariant (Ptr.all = Ptr.all'Loop_Entry);
         pragma Loop_Invariant (P in Ptr.all .. Max);
         pragma Loop_Invariant (for some J in P .. Max => Str (J) /= ' ');
         pragma Loop_Invariant
           (for all J in Ptr.all .. P - 1 => Str (J) = ' ');

         if P > Max then
            Ptr.all := P;
            Bad_Value (Str);
         end if;
      end loop;

      Start := P;

      pragma Assert (Start = Sp.First_Non_Space_Ghost (Str, Ptr.all, Max));

      --  Skip past an initial plus sign

      if Str (P) = '+' then
         P := P + 1;

         if P > Max then
            Ptr.all := Start;
            Bad_Value (Str);
         end if;
      end if;

      Ptr.all := P;
   end Scan_Plus_Sign;

   ---------------
   -- Scan_Sign --
   ---------------

   procedure Scan_Sign
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Minus : out Boolean;
      Start : out Positive)
   is
      P : Integer := Ptr.all;

   begin
      --  Deal with case of null string (all blanks). As per spec, we raise
      --  constraint error, with Ptr unchanged, and thus > Max.

      if P > Max then
         Bad_Value (Str);
      end if;

      --  Scan past initial blanks

      while Str (P) = ' ' loop
         P := P + 1;

         pragma Loop_Invariant (Ptr.all = Ptr.all'Loop_Entry);
         pragma Loop_Invariant (P in Ptr.all .. Max);
         pragma Loop_Invariant (for some J in P .. Max => Str (J) /= ' ');
         pragma Loop_Invariant
           (for all J in Ptr.all .. P - 1 => Str (J) = ' ');

         if P > Max then
            Ptr.all := P;
            Bad_Value (Str);
         end if;
      end loop;

      Start := P;

      pragma Assert (Start = Sp.First_Non_Space_Ghost (Str, Ptr.all, Max));

      --  Remember an initial minus sign

      if Str (P) = '-' then
         Minus := True;
         P := P + 1;

         if P > Max then
            Ptr.all := Start;
            Bad_Value (Str);
         end if;

      --  Skip past an initial plus sign

      elsif Str (P) = '+' then
         Minus := False;
         P := P + 1;

         if P > Max then
            Ptr.all := Start;
            Bad_Value (Str);
         end if;

      else
         Minus := False;
      end if;

      Ptr.all := P;
   end Scan_Sign;

   --------------------------
   -- Scan_Trailing_Blanks --
   --------------------------

   procedure Scan_Trailing_Blanks (Str : String; P : Positive) is
   begin
      for J in P .. Str'Last loop
         if Str (J) /= ' ' then
            Bad_Value (Str);
         end if;

         pragma Loop_Invariant (for all K in P .. J => Str (K) = ' ');
      end loop;
   end Scan_Trailing_Blanks;

   ---------------------
   -- Scan_Underscore --
   ---------------------

   procedure Scan_Underscore
     (Str : String;
      P   : in out Natural;
      Ptr : not null access Integer;
      Max : Integer;
      Ext : Boolean)
   is
      C : Character;

   begin
      P := P + 1;

      --  If underscore is at the end of string, then this is an error and we
      --  raise Constraint_Error, leaving the pointer past the underscore. This
      --  seems a bit strange. It means e.g. that if the field is:

      --    345_

      --  that Constraint_Error is raised. You might think that the RM in this
      --  case would scan out the 345 as a valid integer, leaving the pointer
      --  at the underscore, but the ACVC suite clearly requires an error in
      --  this situation (see for example CE3704M).

      if P > Max then
         Ptr.all := P;
         Bad_Value (Str);
      end if;

      --  Similarly, if no digit follows the underscore raise an error. This
      --  also catches the case of double underscore which is also an error.

      C := Str (P);

      if C in '0' .. '9'
        or else (Ext and then (C in 'A' .. 'F' or else C in 'a' .. 'f'))
      then
         return;
      else
         Ptr.all := P;
         Bad_Value (Str);
      end if;
   end Scan_Underscore;

end System.Val_Util;
