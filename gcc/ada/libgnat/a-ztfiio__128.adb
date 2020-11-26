------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--        A D A . W I D E _ W I D E _ T E X T _ I O . F I X E D _ I O       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

with Interfaces;
with Ada.Wide_Wide_Text_IO.Fixed_Aux;
with Ada.Wide_Wide_Text_IO.Float_Aux;
with System.Img_Fixed_32;  use System.Img_Fixed_32;
with System.Img_Fixed_64;  use System.Img_Fixed_64;
with System.Img_Fixed_128; use System.Img_Fixed_128;
with System.Val_Fixed_32;  use System.Val_Fixed_32;
with System.Val_Fixed_64;  use System.Val_Fixed_64;
with System.Val_Fixed_128; use System.Val_Fixed_128;
with System.WCh_Con;       use System.WCh_Con;
with System.WCh_WtS;       use System.WCh_WtS;

package body Ada.Wide_Wide_Text_IO.Fixed_IO is

   --  Note: we still use the floating-point I/O routines for types whose small
   --  is not a sufficiently small integer or the reciprocal thereof. This will
   --  result in inaccuracies for fixed point types that require more precision
   --  than is available in Long_Long_Float.

   subtype Int32  is Interfaces.Integer_32;
   subtype Int64  is Interfaces.Integer_64;
   subtype Int128 is Interfaces.Integer_128;

   package Aux32 is new
     Ada.Wide_Wide_Text_IO.Fixed_Aux (Int32, Scan_Fixed32, Set_Image_Fixed32);

   package Aux64 is new
     Ada.Wide_Wide_Text_IO.Fixed_Aux (Int64, Scan_Fixed64, Set_Image_Fixed64);

   package Aux128 is new
     Ada.Wide_Wide_Text_IO.Fixed_Aux
      (Int128, Scan_Fixed128, Set_Image_Fixed128);

   Exact : constant Boolean :=
     (Float'Floor (Num'Small) = Float'Ceiling (Num'Small)
       or else Float'Floor (1.0 / Num'Small) = Float'Ceiling (1.0 / Num'Small))
     and then Num'Small >= 2.0**(-127)
     and then Num'Small <= 2.0**127;
   --  True if the exact algorithm implemented in Fixed_Aux can be used. The
   --  condition is a Small which is either an integer or the reciprocal of an
   --  integer with the appropriate magnitude.

   Need_64 : constant Boolean :=
     Num'Object_Size > 32
       or else Num'Small > 2.0**31
       or else Num'Small < 2.0**(-31);
   Need_128 : constant Boolean :=
     Num'Object_Size > 64
       or else Num'Small > 2.0**63
       or else Num'Small < 2.0**(-63);
   --  Throughout this generic body, we distinguish between the cases where
   --  type Int32 is acceptable, where type Int64 is acceptable, and where
   --  type Int128 is needed. These boolean constants are used to test for
   --  these cases and since they are constant, only code for the relevant
   --  case will be really included in the instance.

   E : constant Natural :=
         31 + 32 * Boolean'Pos (Need_64) + 64 * Boolean'Pos (Need_128);
   --  T'Size - 1 for the selected Int{32,64,128}

   F0 : constant Natural := 0;
   F1 : constant Natural :=
          F0 + 38 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F0) >= 1.0E+38);
   F2 : constant Natural :=
          F1 + 19 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F1) >= 1.0E+19);
   F3 : constant Natural :=
          F2 +  9 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F2) >= 1.0E+9);
   F4 : constant Natural :=
          F3 +  5 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F3) >= 1.0E+5);
   F5 : constant Natural :=
          F4 +  3 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F4) >= 1.0E+3);
   F6 : constant Natural :=
          F5 +  2 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F5) >= 1.0E+2);
   F7 : constant Natural :=
          F6 +  1 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F6) >= 1.0E+1);
   --  Binary search for the number of digits - 1 before the decimal point of
   --  the product 2.0**E * Num'Small.

   For0 : constant Natural := 2 + F7;
   --  Fore value for the fixed point type whose mantissa is Int{32,64,128} and
   --  whose small is Num'Small.

   ---------
   -- Get --
   ---------

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field := 0)
   is
      pragma Unsuppress (Range_Check);

   begin
      if not Exact then
         Float_Aux.Get (File, Long_Long_Float (Item), Width);
      elsif Need_128 then
         Item := Num'Fixed_Value
                   (Aux128.Get (File, Width,
                                Int128 (-Float'Ceiling (Num'Small)),
                                Int128 (-Float'Ceiling (1.0 / Num'Small))));
      elsif Need_64 then
         Item := Num'Fixed_Value
                   (Aux64.Get (File, Width,
                               Int64 (-Float'Ceiling (Num'Small)),
                               Int64 (-Float'Ceiling (1.0 / Num'Small))));
      else
         Item := Num'Fixed_Value
                   (Aux32.Get (File, Width,
                               Int32 (-Float'Ceiling (Num'Small)),
                               Int32 (-Float'Ceiling (1.0 / Num'Small))));
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : Field := 0)
   is
   begin
      Get (Current_Input, Item, Width);
   end Get;

   procedure Get
     (From : Wide_Wide_String;
      Item : out Num;
      Last : out Positive)
   is
      pragma Unsuppress (Range_Check);

      S : constant String := Wide_Wide_String_To_String (From, WCEM_Upper);
      --  String on which we do the actual conversion. Note that the method
      --  used for wide character encoding is irrelevant, since if there is
      --  a character outside the Standard.Character range then the call to
      --  Aux.Gets will raise Data_Error in any case.

   begin
      if not Exact then
         Float_Aux.Gets (S, Long_Long_Float (Item), Last);
      elsif Need_128 then
         Item := Num'Fixed_Value
                   (Aux128.Gets (S, Last,
                                 Int128 (-Float'Ceiling (Num'Small)),
                                 Int128 (-Float'Ceiling (1.0 / Num'Small))));
      elsif Need_64 then
         Item := Num'Fixed_Value
                   (Aux64.Gets (S, Last,
                                Int64 (-Float'Ceiling (Num'Small)),
                                Int64 (-Float'Ceiling (1.0 / Num'Small))));
      else
         Item := Num'Fixed_Value
                   (Aux32.Gets (S, Last,
                                Int32 (-Float'Ceiling (Num'Small)),
                                Int32 (-Float'Ceiling (1.0 / Num'Small))));
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put
     (File : File_Type;
      Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      if not Exact then
         Float_Aux.Put (File, Long_Long_Float (Item), Fore, Aft, Exp);
      elsif Need_128 then
         Aux128.Put (File, Int128'Integer_Value (Item), Fore, Aft, Exp,
                     Int128 (-Float'Ceiling (Num'Small)),
                     Int128 (-Float'Ceiling (1.0 / Num'Small)),
                     For0, Num'Aft);
      elsif Need_64 then
         Aux64.Put (File, Int64'Integer_Value (Item), Fore, Aft, Exp,
                    Int64 (-Float'Ceiling (Num'Small)),
                    Int64 (-Float'Ceiling (1.0 / Num'Small)),
                    For0, Num'Aft);
      else
         Aux32.Put (File, Int32'Integer_Value (Item), Fore, Aft, Exp,
                    Int32 (-Float'Ceiling (Num'Small)),
                    Int32 (-Float'Ceiling (1.0 / Num'Small)),
                    For0, Num'Aft);
      end if;
   end Put;

   procedure Put
     (Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Put (Current_Output, Item, Fore, Aft, Exp);
   end Put;

   procedure Put
     (To   : out Wide_Wide_String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      S : String (To'First .. To'Last);

   begin
      if not Exact then
         Float_Aux.Puts (S, Long_Long_Float (Item), Aft, Exp);
      elsif Need_128 then
         Aux128.Puts (S, Int128'Integer_Value (Item), Aft, Exp,
                      Int128 (-Float'Ceiling (Num'Small)),
                      Int128 (-Float'Ceiling (1.0 / Num'Small)),
                      For0, Num'Aft);
      elsif Need_64 then
         Aux64.Puts (S, Int64'Integer_Value (Item), Aft, Exp,
                     Int64 (-Float'Ceiling (Num'Small)),
                     Int64 (-Float'Ceiling (1.0 / Num'Small)),
                     For0, Num'Aft);
      else
         Aux32.Puts (S, Int32'Integer_Value (Item), Aft, Exp,
                     Int32 (-Float'Ceiling (Num'Small)),
                     Int32 (-Float'Ceiling (1.0 / Num'Small)),
                     For0, Num'Aft);
      end if;

      for J in S'Range loop
         To (J) := Wide_Wide_Character'Val (Character'Pos (S (J)));
      end loop;
   end Put;

end Ada.Wide_Wide_Text_IO.Fixed_IO;
