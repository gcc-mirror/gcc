------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             A D A . W I D E _ T E X T _ I O . F I X E D _ I O            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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
with Ada.Wide_Text_IO.Fixed_Aux;
with Ada.Wide_Text_IO.Float_Aux;
with System.Img_Fixed_32; use System.Img_Fixed_32;
with System.Img_Fixed_64; use System.Img_Fixed_64;
with System.Img_LFlt;     use System.Img_LFlt;
with System.Val_Fixed_32; use System.Val_Fixed_32;
with System.Val_Fixed_64; use System.Val_Fixed_64;
with System.Val_LFlt;     use System.Val_LFlt;
with System.WCh_Con;      use System.WCh_Con;
with System.WCh_WtS;      use System.WCh_WtS;

package body Ada.Wide_Text_IO.Fixed_IO is

   --  Note: we still use the floating-point I/O routines for types whose small
   --  is not the ratio of two sufficiently small integers. This will result in
   --  inaccuracies for fixed point types that require more precision than is
   --  available in Long_Float.

   subtype Int32 is Interfaces.Integer_32; use type Int32;
   subtype Int64 is Interfaces.Integer_64; use type Int64;

   package Aux32 is new
     Ada.Wide_Text_IO.Fixed_Aux (Int32, Scan_Fixed32, Set_Image_Fixed32);

   package Aux64 is new
     Ada.Wide_Text_IO.Fixed_Aux (Int64, Scan_Fixed64, Set_Image_Fixed64);

   package Aux_Long_Float is new
     Ada.Wide_Text_IO.Float_Aux
       (Long_Float, Scan_Long_Float, Set_Image_Long_Float);

   --  Throughout this generic body, we distinguish between the case where type
   --  Int32 is OK and where type Int64 is OK. These boolean constants are used
   --  to test for this, such that only code for the relevant case is included
   --  in the instance; that's why the computation of their value must be fully
   --  static (although it is not a static expressions in the RM sense).

   OK_Get_32 : constant Boolean :=
     Num'Base'Object_Size <= 32
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**31)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**31)
           or else
          (Num'Small_Numerator <= 2**27
            and then Num'Small_Denominator <= 2**27));
   --  These conditions are derived from the prerequisites of System.Value_F

   OK_Put_32 : constant Boolean :=
     Num'Base'Object_Size <= 32
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**31)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**31)
           or else
          (Num'Small_Numerator < Num'Small_Denominator
            and then Num'Small_Denominator <= 2**27)
           or else
          (Num'Small_Denominator < Num'Small_Numerator
            and then Num'Small_Numerator <= 2**25));
   --  These conditions are derived from the prerequisites of System.Image_F

   OK_Get_64 : constant Boolean :=
     Num'Base'Object_Size <= 64
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**63)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**63)
           or else
          (Num'Small_Numerator <= 2**59
            and then Num'Small_Denominator <= 2**59));
   --  These conditions are derived from the prerequisites of System.Value_F

   OK_Put_64 : constant Boolean :=
     Num'Base'Object_Size <= 64
       and then
         ((Num'Small_Numerator = 1 and then Num'Small_Denominator <= 2**63)
           or else
          (Num'Small_Denominator = 1 and then Num'Small_Numerator <= 2**63)
           or else
          (Num'Small_Numerator < Num'Small_Denominator
            and then Num'Small_Denominator <= 2**59)
           or else
          (Num'Small_Denominator < Num'Small_Numerator
            and then Num'Small_Numerator <= 2**53));
   --  These conditions are derived from the prerequisites of System.Image_F

   E : constant Natural := 63 - 32 * Boolean'Pos (OK_Put_32);
   --  T'Size - 1 for the selected Int{32,64}

   F0 : constant Natural := 0;
   F1 : constant Natural :=
          F0 + 18 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F0) >= 1.0E+18);
   F2 : constant Natural :=
          F1 +  9 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F1) >= 1.0E+9);
   F3 : constant Natural :=
          F2 +  5 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F2) >= 1.0E+5);
   F4 : constant Natural :=
          F3 +  3 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F3) >= 1.0E+3);
   F5 : constant Natural :=
          F4 +  2 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F4) >= 1.0E+2);
   F6 : constant Natural :=
          F5 +  1 * Boolean'Pos (2.0**E * Num'Small * 10.0**(-F5) >= 1.0E+1);
   --  Binary search for the number of digits - 1 before the decimal point of
   --  the product 2.0**E * Num'Small.

   For0 : constant Natural := 2 + F6;
   --  Fore value for the fixed point type whose mantissa is Int{32,64} and
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
      if OK_Get_32 then
         Item := Num'Fixed_Value
                   (Aux32.Get (File, Width,
                               -Num'Small_Numerator,
                               -Num'Small_Denominator));
      elsif OK_Get_64 then
         Item := Num'Fixed_Value
                   (Aux64.Get (File, Width,
                               -Num'Small_Numerator,
                               -Num'Small_Denominator));
      else
         Aux_Long_Float.Get (File, Long_Float (Item), Width);
      end if;

   exception
      when Constraint_Error => raise Data_Error;
   end Get;

   procedure Get
     (Item  : out Num;
      Width : Field := 0)
   is
   begin
      Get (Current_In, Item, Width);
   end Get;

   procedure Get
     (From : Wide_String;
      Item : out Num;
      Last : out Positive)
   is
      pragma Unsuppress (Range_Check);

      S : constant String := Wide_String_To_String (From, WCEM_Upper);
      --  String on which we do the actual conversion. Note that the method
      --  used for wide character encoding is irrelevant, since if there is
      --  a character outside the Standard.Character range then the call to
      --  Aux.Gets will raise Data_Error in any case.

   begin
      if OK_Get_32 then
         Item := Num'Fixed_Value
                   (Aux32.Gets (S, Last,
                                -Num'Small_Numerator,
                                -Num'Small_Denominator));
      elsif OK_Get_64 then
         Item := Num'Fixed_Value
                   (Aux64.Gets (S, Last,
                                -Num'Small_Numerator,
                                -Num'Small_Denominator));
      else
         Aux_Long_Float.Gets (S, Long_Float (Item), Last);
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
      if OK_Put_32 then
         Aux32.Put (File, Int32'Integer_Value (Item), Fore, Aft, Exp,
                    -Num'Small_Numerator, -Num'Small_Denominator,
                    For0, Num'Aft);
      elsif OK_Put_64 then
         Aux64.Put (File, Int64'Integer_Value (Item), Fore, Aft, Exp,
                    -Num'Small_Numerator, -Num'Small_Denominator,
                    For0, Num'Aft);
      else
         Aux_Long_Float.Put (File, Long_Float (Item), Fore, Aft, Exp);
      end if;
   end Put;

   procedure Put
     (Item : Num;
      Fore : Field := Default_Fore;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
   begin
      Put (Current_Out, Item, Fore, Aft, Exp);
   end Put;

   procedure Put
     (To   : out Wide_String;
      Item : Num;
      Aft  : Field := Default_Aft;
      Exp  : Field := Default_Exp)
   is
      S : String (To'First .. To'Last);

   begin
      if OK_Put_32 then
         Aux32.Puts (S, Int32'Integer_Value (Item), Aft, Exp,
                     -Num'Small_Numerator, -Num'Small_Denominator,
                     For0, Num'Aft);
      elsif OK_Put_64 then
         Aux64.Puts (S, Int64'Integer_Value (Item), Aft, Exp,
                     -Num'Small_Numerator, -Num'Small_Denominator,
                     For0, Num'Aft);
      else
         Aux_Long_Float.Puts (S, Long_Float (Item), Aft, Exp);
      end if;

      for J in S'Range loop
         To (J) := Wide_Character'Val (Character'Pos (S (J)));
      end loop;
   end Put;

end Ada.Wide_Text_IO.Fixed_IO;
