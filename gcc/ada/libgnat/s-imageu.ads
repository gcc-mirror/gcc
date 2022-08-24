------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . I M A G E _ U                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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

--  This package contains the routines for supporting the Image attribute for
--  modular integer types, and also for conversion operations required in
--  Text_IO.Modular_IO for such types.

--  Preconditions in this unit are meant for analysis only, not for run-time
--  checking, so that the expected exceptions are raised. This is enforced by
--  setting the corresponding assertion policy to Ignore. Postconditions and
--  contract cases should not be executed at runtime as well, in order not to
--  slow down the execution of these functions.

pragma Assertion_Policy (Pre                => Ignore,
                         Post               => Ignore,
                         Contract_Cases     => Ignore,
                         Ghost              => Ignore,
                         Subprogram_Variant => Ignore);

generic

   type Uns is mod <>;
   type Uns_Option is private;

   --  Additional parameters for ghost subprograms used inside contracts

   Unsigned_Width_Ghost : Natural;

   with function Wrap_Option (Value : Uns) return Uns_Option
      with Ghost;
   with function Only_Decimal_Ghost
     (Str      : String;
      From, To : Integer)
      return Boolean
      with Ghost;
   with function Hexa_To_Unsigned_Ghost (X : Character) return Uns
      with Ghost;
   with function Scan_Based_Number_Ghost
     (Str      : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0) return Uns_Option
      with Ghost;
   with function Is_Unsigned_Ghost (Str : String) return Boolean
      with Ghost;
   with function Value_Unsigned (Str : String) return Uns;
   with procedure Prove_Iter_Scan_Based_Number_Ghost
     (Str1, Str2 : String;
      From, To : Integer;
      Base     : Uns := 10;
      Acc      : Uns := 0)
      with Ghost;
   with procedure Prove_Scan_Only_Decimal_Ghost
     (Str : String;
      Val : Uns)
      with Ghost;

package System.Image_U is

   procedure Image_Unsigned
     (V : Uns;
      S : in out String;
      P : out Natural)
   with
     Pre  => S'First = 1
       and then S'Last < Integer'Last
       and then S'Last >= Unsigned_Width_Ghost,
     Post => P in S'Range
       and then Value_Unsigned (S (1 .. P)) = V;
   pragma Inline (Image_Unsigned);
   --  Computes Uns'Image (V) and stores the result in S (1 .. P) setting
   --  the resulting value of P. The caller guarantees that S is long enough to
   --  hold the result, and that S'First is 1.

   procedure Set_Image_Unsigned
     (V : Uns;
      S : in out String;
      P : in out Natural)
   with
     Pre  => P < Integer'Last
       and then S'Last < Integer'Last
       and then S'First <= P + 1
       and then S'First <= S'Last
       and then P <= S'Last - Unsigned_Width_Ghost + 1,
     Post => S (S'First .. P'Old) = S'Old (S'First .. P'Old)
       and then P in P'Old + 1 .. S'Last
       and then Only_Decimal_Ghost (S, From => P'Old + 1, To => P)
       and then Scan_Based_Number_Ghost (S, From => P'Old + 1, To => P)
         = Wrap_Option (V);
   --  Stores the image of V in S starting at S (P + 1), P is updated to point
   --  to the last character stored. The value stored is identical to the value
   --  of Uns'Image (V) except that no leading space is stored. The caller
   --  guarantees that S is long enough to hold the result. S need not have a
   --  lower bound of 1.

end System.Image_U;
