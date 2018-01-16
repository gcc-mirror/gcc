------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  G N A T . R A N D O M _ N U M B E R S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2007-2018, Free Software Foundation, Inc.         --
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

with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Conversion;

with System.Random_Numbers; use System.Random_Numbers;

package body GNAT.Random_Numbers with
  SPARK_Mode => Off
is
   Sys_Max_Image_Width : constant := System.Random_Numbers.Max_Image_Width;

   subtype Image_String is String (1 .. Max_Image_Width);

   --  Utility function declarations

   procedure Insert_Image
     (S     : in out Image_String;
      Index : Integer;
      V     : Integer_64);
   --  Insert string representation of V in S starting at position Index

   ---------------
   -- To_Signed --
   ---------------

   function To_Signed is
     new Ada.Unchecked_Conversion (Unsigned_32, Integer_32);
   function To_Signed is
     new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);

   ------------------
   -- Insert_Image --
   ------------------

   procedure Insert_Image
     (S     : in out Image_String;
      Index : Integer;
      V     : Integer_64)
   is
      Image : constant String := Integer_64'Image (V);
   begin
      S (Index .. Index + Image'Length - 1) := Image;
   end Insert_Image;

   ---------------------
   -- Random_Discrete --
   ---------------------

   function Random_Discrete
     (Gen   : Generator;
      Min   : Result_Subtype := Default_Min;
      Max   : Result_Subtype := Result_Subtype'Last) return Result_Subtype
   is
      function F is
        new System.Random_Numbers.Random_Discrete
              (Result_Subtype, Default_Min);
   begin
      return F (Gen.Rep, Min, Max);
   end Random_Discrete;

   --------------------------
   -- Random_Decimal_Fixed --
   --------------------------

   function Random_Decimal_Fixed
     (Gen : Generator;
      Min : Result_Subtype := Default_Min;
      Max : Result_Subtype := Result_Subtype'Last) return Result_Subtype
   is
      subtype IntV is Integer_64 range
        Integer_64'Integer_Value (Min) ..
        Integer_64'Integer_Value (Max);
      function R is new Random_Discrete (Integer_64, IntV'First);
   begin
      return Result_Subtype'Fixed_Value (R (Gen, IntV'First, IntV'Last));
   end Random_Decimal_Fixed;

   ---------------------------
   -- Random_Ordinary_Fixed --
   ---------------------------

   function Random_Ordinary_Fixed
     (Gen : Generator;
      Min : Result_Subtype := Default_Min;
      Max : Result_Subtype := Result_Subtype'Last) return Result_Subtype
   is
      subtype IntV is Integer_64 range
        Integer_64'Integer_Value (Min) ..
        Integer_64'Integer_Value (Max);
      function R is new Random_Discrete (Integer_64, IntV'First);
   begin
      return Result_Subtype'Fixed_Value (R (Gen, IntV'First, IntV'Last));
   end Random_Ordinary_Fixed;

   ------------
   -- Random --
   ------------

   function Random (Gen : Generator) return Float is
   begin
      return Random (Gen.Rep);
   end Random;

   function Random (Gen : Generator) return Long_Float is
   begin
      return Random (Gen.Rep);
   end Random;

   function Random (Gen : Generator) return Interfaces.Unsigned_32 is
   begin
      return Random (Gen.Rep);
   end Random;

   function Random (Gen : Generator) return Interfaces.Unsigned_64 is
   begin
      return Random (Gen.Rep);
   end Random;

   function Random (Gen : Generator) return Integer_64 is
   begin
      return To_Signed (Unsigned_64'(Random (Gen)));
   end Random;

   function Random (Gen : Generator) return Integer_32 is
   begin
      return To_Signed (Unsigned_32'(Random (Gen)));
   end Random;

   function Random (Gen : Generator) return Long_Integer is
      function Random_Long_Integer is new Random_Discrete (Long_Integer);
   begin
      return Random_Long_Integer (Gen);
   end Random;

   function Random (Gen : Generator) return Integer is
      function Random_Integer is new Random_Discrete (Integer);
   begin
      return Random_Integer (Gen);
   end Random;

   ------------------
   -- Random_Float --
   ------------------

   function Random_Float (Gen : Generator) return Result_Subtype is
      function F is new System.Random_Numbers.Random_Float (Result_Subtype);
   begin
      return F (Gen.Rep);
   end Random_Float;

   ---------------------
   -- Random_Gaussian --
   ---------------------

   --  Generates pairs of normally distributed values using the polar method of
   --  G. E. P. Box, M. E. Muller, and G. Marsaglia. See Donald E. Knuth, The
   --  Art of Computer Programming, Vol 2: Seminumerical Algorithms, section
   --  3.4.1, subsection C, algorithm P. Returns half of the pair on each call,
   --  using the Next_Gaussian field of Gen to hold the second member on
   --  even-numbered calls.

   function Random_Gaussian (Gen : Generator) return Long_Float is
      G : Generator renames Gen'Unrestricted_Access.all;

      V1, V2, Rad2, Mult : Long_Float;

   begin
      if G.Have_Gaussian then
         G.Have_Gaussian := False;
         return G.Next_Gaussian;

      else
         loop
            V1 := 2.0 * Random (G) - 1.0;
            V2 := 2.0 * Random (G) - 1.0;
            Rad2 := V1 ** 2 + V2 ** 2;
            exit when Rad2 < 1.0 and then Rad2 /= 0.0;
         end loop;

         --  Now V1 and V2 are coordinates in the unit circle

         Mult := Sqrt (-2.0 * Log (Rad2) / Rad2);
         G.Next_Gaussian := V2 * Mult;
         G.Have_Gaussian := True;
         return Long_Float'Machine (V1 * Mult);
      end if;
   end Random_Gaussian;

   function Random_Gaussian (Gen : Generator) return Float is
      V : constant Long_Float := Random_Gaussian (Gen);
   begin
      return Float'Machine (Float (V));
   end Random_Gaussian;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : out Generator) is
   begin
      Reset (Gen.Rep);
      Gen.Have_Gaussian := False;
   end Reset;

   procedure Reset
     (Gen       : out Generator;
      Initiator : Initialization_Vector)
   is
   begin
      Reset (Gen.Rep, Initiator);
      Gen.Have_Gaussian := False;
   end Reset;

   procedure Reset
     (Gen       : out Generator;
      Initiator : Interfaces.Integer_32)
   is
   begin
      Reset (Gen.Rep, Initiator);
      Gen.Have_Gaussian := False;
   end Reset;

   procedure Reset
     (Gen       : out Generator;
      Initiator : Interfaces.Unsigned_32)
   is
   begin
      Reset (Gen.Rep, Initiator);
      Gen.Have_Gaussian := False;
   end Reset;

   procedure Reset
     (Gen       : out Generator;
      Initiator : Integer)
   is
   begin
      Reset (Gen.Rep, Initiator);
      Gen.Have_Gaussian := False;
   end Reset;

   procedure Reset
     (Gen        : out Generator;
      From_State : Generator)
   is
   begin
      Reset (Gen.Rep, From_State.Rep);
      Gen.Have_Gaussian := From_State.Have_Gaussian;
      Gen.Next_Gaussian := From_State.Next_Gaussian;
   end Reset;

   Frac_Scale : constant Long_Float :=
                  Long_Float
                    (Long_Float'Machine_Radix) ** Long_Float'Machine_Mantissa;

   function Val64 (Image : String) return Integer_64;
   --  Renames Integer64'Value
   --  We cannot use a 'renames Integer64'Value' since for some strange
   --  reason, this requires a dependency on s-auxdec.ads which not all
   --  run-times support ???

   function Val64 (Image : String) return Integer_64 is
   begin
      return Integer_64'Value (Image);
   end Val64;

   procedure Reset
     (Gen        : out Generator;
      From_Image : String)
   is
      F0 : constant Integer := From_Image'First;
      T0 : constant Integer := From_Image'First + Sys_Max_Image_Width;

   begin
      Reset (Gen.Rep, From_Image (F0 .. F0 + Sys_Max_Image_Width));

      if From_Image (T0 + 1) = '1' then
         Gen.Have_Gaussian := True;
         Gen.Next_Gaussian :=
           Long_Float (Val64 (From_Image (T0 + 3 .. T0 + 23))) / Frac_Scale
           * Long_Float (Long_Float'Machine_Radix)
           ** Integer (Val64 (From_Image (T0 + 25 .. From_Image'Last)));
      else
         Gen.Have_Gaussian := False;
      end if;
   end Reset;

   -----------
   -- Image --
   -----------

   function Image (Gen : Generator) return String is
      Result : Image_String;

   begin
      Result := (others => ' ');
      Result (1 .. Sys_Max_Image_Width) := Image (Gen.Rep);

      if Gen.Have_Gaussian then
         Result (Sys_Max_Image_Width + 2) := '1';
         Insert_Image (Result, Sys_Max_Image_Width + 4,
                       Integer_64 (Long_Float'Fraction (Gen.Next_Gaussian)
                                   * Frac_Scale));
         Insert_Image (Result, Sys_Max_Image_Width + 24,
                       Integer_64 (Long_Float'Exponent (Gen.Next_Gaussian)));

      else
         Result (Sys_Max_Image_Width + 2) := '0';
      end if;

      return Result;
   end Image;

end GNAT.Random_Numbers;
