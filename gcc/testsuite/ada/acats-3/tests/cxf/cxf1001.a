-- CXF1001.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that values of 2 and 10 are allowable values for Machine_Radix 
--      of a decimal first subtype.
--      Check that the value of Decimal.Max_Decimal_Digits is at least 18; 
--      the value of Decimal.Max_Scale is at least 18; the value of 
--      Decimal.Min_Scale is at most 0.
--
-- TEST DESCRIPTION:
--      This test examines the Machine_Radix attribute definition clause
--      and its effect on Decimal fixed point types, as well as several
--      constants from the package Ada.Decimal.
--      The first subtest checks that the Machine_Radix attribute will 
--      return the value set for Machine_Radix by an attribute definition
--      clause.  The second and third subtests examine differences between
--      the binary and decimal scaling of a type, based on the radix 
--      representation.  The final subtest examines the values
--      assigned to constants Min_Scale, Max_Scale, and Max_Decimal_Digits,
--      found in the package Ada.Decimal.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      29 Dec 94   SAIC    Restructured Radix 10 and Radix 2 test blocks.
--
--!

with Report;
with Ada.Decimal;

procedure CXF1001 is
begin

   Report.Test ("CXF1001", "Check that values of 2 and 10 are allowable " &
                           "values for Machine_Radix of a decimal first " &
                           "subtype.  Check that the value of "           &
                           "Decimal.Max_Decimal_Digits is at least 18; "  &
                           "the value of Decimal.Max_Scale is at least "  &
                           "18; the value of Decimal.Min_Scale is at "    &
                           "most 0");

   Attribute_Check_Block:
   declare

      Del          : constant := 1.0/10**2;
      Const_Digits : constant :=  3;
      Two          : constant :=  2;
      Ten          : constant := 10;

      type Radix_2_Type_1  is delta 0.01  digits 7;
      type Radix_2_Type_2  is delta Ada.Decimal.Min_Delta digits 10;        
      type Radix_2_Type_3  is
        delta 0.000_1 digits Ada.Decimal.Max_Decimal_Digits;

      type Radix_10_Type_1 is delta 10.0**(-Ada.Decimal.Max_Scale) digits 8;
      type Radix_10_Type_2 is delta 10.0**(-Ada.Decimal.Min_Scale) digits 6;
      type Radix_10_Type_3 is delta Ada.Decimal.Max_Delta digits 15;        
        

      -- Use an attribute definition clause to set the Machine_Radix for a
      -- decimal first subtype to either 2 or 10.
      for Radix_2_Type_1'Machine_Radix  use 2;                      
      for Radix_2_Type_2'Machine_Radix  use Two;                    
      for Radix_2_Type_3'Machine_Radix  use 10-8;                   

      for Radix_10_Type_1'Machine_Radix use 2*15/Const_Digits;     
      for Radix_10_Type_2'Machine_Radix use Ten;                             
      for Radix_10_Type_3'Machine_Radix use Radix_10_Type_2'Machine_Radix;


   begin

      -- Check that the attribute 'Machine_Radix returns the value assigned
      -- by the attribute definition clause.

      if Radix_2_Type_1'Machine_Radix /= 2  or else
         Radix_2_Type_2'Machine_Radix /= 2  or else
         Radix_2_Type_3'Machine_Radix /= 2  
      then
         Report.Failed("Incorrect radix value returned, 2 expected");
      end if;

      if Radix_10_Type_1'Machine_Radix /= 10  or else
         Radix_10_Type_2'Machine_Radix /= 10  or else
         Radix_10_Type_3'Machine_Radix /= 10  
      then
         Report.Failed("Incorrect radix value returned, 10 expected");
      end if;

   exception
      when others => Report.Failed ("Exception raised in Attr_Check_Block");
   end Attribute_Check_Block;



   Radix_Block:
   -- Premises:
   --   1) Choose several numbers, from types using either decimal scaling or
   --      binary scaling.
   --   1) Repetitively add these numbers to themselves.
   --   3) Validate that the result is the expected result, regardless of the
   --      scaling used in the definition of the type.
   declare

      Number_Of_Values : constant :=    3;
      Loop_Count       : constant := 1000;

      type Radix_2_Type  is delta 0.0001 digits 10;
      type Radix_10_Type is delta 0.0001 digits 10;
                                      
      for Radix_2_Type'Machine_Radix  use  2;
      for Radix_10_Type'Machine_Radix use 10;

      type Result_Record_Type is record
         Rad_2  : Radix_2_Type;
         Rad_10 : Radix_10_Type;
      end record;

      type Result_Array_Type is array (1..Number_Of_Values) 
        of Result_Record_Type;

      Result_Array : Result_Array_Type := ((50.00,  50.00), 
                                           (613.00, 613.00), 
                                           (72.70,  72.70));

      function Repetitive_Radix_2_Add (Value : in Radix_2_Type) 
        return Radix_2_Type is
         Result : Radix_2_Type := 0.0;
      begin
        for i in 1..Loop_Count loop
           Result := Result + Value;
        end loop;
        return Result;
      end Repetitive_Radix_2_Add;

      function Repetitive_Radix_10_Add (Value : in Radix_10_Type) 
        return Radix_10_Type is
         Result : Radix_10_Type := 0.0;
      begin
        for i in 1..Loop_Count loop
           Result := Result + Value;
        end loop;
        return Result;
      end Repetitive_Radix_10_Add;

   begin

      -- Radix 2 Cases, three different values.
      -- Compare the result of the repetitive addition with the expected
      -- Radix 2 result, as well as with the Radix 10 value after type 
      -- conversion.

      if Repetitive_Radix_2_Add(0.05) /= Result_Array(1).Rad_2  or
         Repetitive_Radix_2_Add(0.05) /= Radix_2_Type(Result_Array(1).Rad_10)
      then
         Report.Failed("Incorrect Radix 2 Result, Case 1");
      end if;

      if Repetitive_Radix_2_Add(0.613) /= 
         Result_Array(2).Rad_2            or
         Repetitive_Radix_2_Add(0.613) /= 
         Radix_2_Type(Result_Array(2).Rad_10)
      then
         Report.Failed("Incorrect Radix 2 Result, Case 2");
      end if;

      if Repetitive_Radix_2_Add(0.0727)  /= 
         Result_Array(3).Rad_2              or
         Repetitive_Radix_2_Add(0.0727)  /= 
         Radix_2_Type(Result_Array(3).Rad_10)
      then
         Report.Failed("Incorrect Radix 2 Result, Case 3");
      end if;

      -- Radix 10 Cases, three different values.
      -- Compare the result of the repetitive addition with the expected
      -- Radix 10 result, as well as with the Radix 2 value after type 
      -- conversion.

      if Repetitive_Radix_10_Add(0.05) /= Result_Array(1).Rad_10  or
         Repetitive_Radix_10_Add(0.05) /= Radix_10_Type(Result_Array(1).Rad_2)
      then
         Report.Failed("Incorrect Radix 10 Result, Case 1");
      end if;

      if Repetitive_Radix_10_Add(0.613) /= 
         Result_Array(2).Rad_10            or
         Repetitive_Radix_10_Add(0.613) /= 
         Radix_10_Type(Result_Array(2).Rad_2)
      then
         Report.Failed("Incorrect Radix 10 Result, Case 2");
      end if;

      if Repetitive_Radix_10_Add(0.0727) /= 
         Result_Array(3).Rad_10             or
         Repetitive_Radix_10_Add(0.0727) /= 
         Radix_10_Type(Result_Array(3).Rad_2)
      then
         Report.Failed("Incorrect Radix 10 Result, Case 3");
      end if;

   exception
      when others => Report.Failed ("Exception raised in Radix_Block");
   end Radix_Block;



   Size_Block:
   -- Check the implementation max/min values of constants declared in
   -- package Ada.Decimal.
   declare
      Minimum_Required_Size : constant := 18;
      Maximum_Allowed_Size  : constant :=  0;
   begin

      -- Check that the Max_Decimal_Digits value is at least 18.
      if not (Ada.Decimal.Max_Decimal_Digits >= Minimum_Required_Size) then
         Report.Failed("Insufficient size provided for Max_Decimal_Digits");
      end if;

      -- Check that the Max_Scale value is at least 18.
      if not (Ada.Decimal.Max_Scale >= Minimum_Required_Size) then
         Report.Failed("Insufficient size provided for Max_Scale");
      end if;

      -- Check that the Min_Scale value is at most 0.
      if not (Ada.Decimal.Min_Scale <= Maximum_Allowed_Size) then
         Report.Failed("Too large a value provided for Min_Scale");
      end if;

   exception
      when others => Report.Failed ("Exception raised in Size_Block");
   end Size_Block;

   Report.Result;

end CXF1001;
