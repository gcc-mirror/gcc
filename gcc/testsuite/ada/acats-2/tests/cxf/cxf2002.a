-- CXF2002.A
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
--      Check that the multiplying operators for a decimal fixed point type
--      return values that are integral multiples of the small of the type.
--      Check the case where the operand and result types are the same.
--
--      Check that if the mathematical result is between multiples of the
--      small of the result type, the result is truncated toward zero.
--      Check that if the attribute 'Round is applied to the mathematical
--      result, however, the result is rounded to the nearest multiple of
--      the small (away from zero if the result is midway between two
--      multiples of the small).
--    
-- TEST DESCRIPTION:
--      Two decimal fixed point types are declared, one with a Machine_Radix
--      value of 2, and one with a value of 10. For each type, checks are
--      performed on the following operations, where the operand and result
--      types are the same:
--
--         - Multiplication.
--         - Multiplication, where the attribute 'Round is applied to the
--           result.
--         - Division.
--         - Division, where the attribute 'Round is applied to the result.
--
--      Each operation is performed within a loop, where one operand is
--      always the same variable. After the loop completes, the cumulative
--      total contained in this variable is compared with the expected
--      result.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Information Systems Annex.
--
--
-- CHANGE HISTORY:
--      27 Mar 96   SAIC    Prerelease version for ACVC 2.1.
--
--!

generic
   type Decimal_Fixed is delta <> digits <>;
package CXF2002_0 is

   procedure Multiply_And_Truncate (Balance : in out Decimal_Fixed;
                                    Factor  : in     Decimal_Fixed);

   procedure Divide_And_Truncate   (Balance : in out Decimal_Fixed;
                                    Divisor : in     Decimal_Fixed);

   procedure Multiply_And_Round    (Balance : in out Decimal_Fixed;
                                    Factor  : in     Decimal_Fixed);

   procedure Divide_And_Round      (Balance : in out Decimal_Fixed;
                                    Divisor : in     Decimal_Fixed);

end CXF2002_0;


     --==================================================================--


package body CXF2002_0 is

   procedure Multiply_And_Truncate (Balance : in out Decimal_Fixed;
                                    Factor  : in     Decimal_Fixed) is
      Interest : Decimal_Fixed;
   begin
      Interest := Factor * Balance;      -- Fixed-fixed multiplication.
      Balance  := Balance + Interest;
   end Multiply_And_Truncate;


   procedure Divide_And_Truncate (Balance : in out Decimal_Fixed;
                                  Divisor : in     Decimal_Fixed) is
      Interest : Decimal_Fixed;
   begin
      Interest := Balance / Divisor;     -- Fixed-fixed division.
      Balance  := Balance + Interest;
   end Divide_And_Truncate;


   procedure Multiply_And_Round (Balance : in out Decimal_Fixed;
                                 Factor  : in     Decimal_Fixed) is
      Interest : Decimal_Fixed;
   begin
                                         -- Fixed-fixed multiplication.
      Interest := Decimal_Fixed'Round ( Factor * Balance );
      Balance  := Balance + Interest;
   end Multiply_And_Round;


   procedure Divide_And_Round (Balance : in out Decimal_Fixed;
                               Divisor : in     Decimal_Fixed) is
      Interest : Decimal_Fixed;
   begin
                                         -- Fixed-fixed division.
      Interest := Decimal_Fixed'Round ( Balance / Divisor );
      Balance  := Balance + Interest;
   end Divide_And_Round;

end CXF2002_0;


     --==================================================================--


package CXF2002_1 is

   type Money_Radix2 is delta 0.01 digits 11;  -- range -999,999,999.99 ..
   for Money_Radix2'Machine_Radix use 2;       --       +999,999,999.99


   type Money_Radix10 is delta 0.01 digits 11; -- range -999,999,999.99 ..
   for Money_Radix10'Machine_Radix use 10;     --       +999,999,999.99

end CXF2002_1;


     --==================================================================--


with CXF2002_0;
with CXF2002_1;

with Report;
procedure CXF2002 is

   Loop_Count : constant := 300;
   type Loop_Range is range 1 .. Loop_Count;

begin

   Report.Test ("CXF2002", "Check decimal multiplication and division, and " &
                           "'Round, where the operand and result types are " &
                           "the same");


               ---=---=---=---=---=---=---=---=---=---=---


   RADIX_2_SUBTESTS:
   declare
      package Radix_2 is new CXF2002_0 (CXF2002_1.Money_Radix2);
      use type CXF2002_1.Money_Radix2;
   begin

      RADIX_2_MULTIPLICATION:
      declare
         Rate           : constant CXF2002_1.Money_Radix2 := 0.12;
         Period         : constant Integer                := 12;
         Factor         :          CXF2002_1.Money_Radix2 := Rate / Period;

         Initial        : constant CXF2002_1.Money_Radix2 :=   100_000.00;
         Trunc_Expected : constant CXF2002_1.Money_Radix2 := 1_978_837.50;
         Round_Expected : constant CXF2002_1.Money_Radix2 := 1_978_846.75;

         Balance        : CXF2002_1.Money_Radix2;
      begin
                       ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_2.Multiply_And_Truncate (Balance, Factor);
         end loop;

         if Balance /= Trunc_Expected then
            Report.Failed ("Wrong result: Radix 2 multiply and truncate");
         end if;

                       ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_2.Multiply_And_Round (Balance, Factor);
         end loop;

         if Balance /= Round_Expected then
            Report.Failed ("Wrong result: Radix 2 multiply and round");
         end if;

                       ---=---=---=---=---=---=---
      end RADIX_2_MULTIPLICATION;


      RADIX_2_DIVISION:
      declare
         Rate           : constant CXF2002_1.Money_Radix2 := 0.25;
         Period         : constant Integer                := 12;
         Factor         :          CXF2002_1.Money_Radix2 := Rate / Period;
         Divisor        : constant CXF2002_1.Money_Radix2 := 1.0 / Factor;

         Initial        : constant CXF2002_1.Money_Radix2 :=     5_500.36;
         Trunc_Expected : constant CXF2002_1.Money_Radix2 := 2_091_332.87;
         Round_Expected : constant CXF2002_1.Money_Radix2 := 2_091_436.88;

         Balance        : CXF2002_1.Money_Radix2;
      begin
                       ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_2.Divide_And_Truncate (Balance, Divisor);
         end loop;

         if Balance /= Trunc_Expected then
            Report.Failed ("Wrong result: Radix 2 divide and truncate");
         end if;

                       ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_2.Divide_And_Round (Balance, Divisor);
         end loop;

         if Balance /= Round_Expected then
            Report.Failed ("Wrong result: Radix 2 divide and round");
         end if;

                       ---=---=---=---=---=---=---
      end RADIX_2_DIVISION;

   end RADIX_2_SUBTESTS;


               ---=---=---=---=---=---=---=---=---=---=---


   RADIX_10_SUBTESTS:
   declare
      package Radix_10 is new CXF2002_0 (CXF2002_1.Money_Radix10);
      use type CXF2002_1.Money_Radix10;
   begin

      RADIX_10_MULTIPLICATION:
      declare
         Rate           : constant CXF2002_1.Money_Radix10 := 0.37;
         Period         : constant Integer                 := 12;
         Factor         :          CXF2002_1.Money_Radix10 := Rate / Period;

         Initial        : constant CXF2002_1.Money_Radix10 :=       459.33;
         Trunc_Expected : constant CXF2002_1.Money_Radix10 := 3_259_305.54;
         Round_Expected : constant CXF2002_1.Money_Radix10 := 3_260_544.11;

         Balance        : CXF2002_1.Money_Radix10;
      begin
                       ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_10.Multiply_And_Truncate (Balance, Factor);
         end loop;

         if Balance /= Trunc_Expected then
            Report.Failed ("Wrong result: Radix 10 multiply and truncate");
         end if;

                       ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_10.Multiply_And_Round (Balance, Factor);
         end loop;

         if Balance /= Round_Expected then
            Report.Failed ("Wrong result: Radix 10 multiply and round");
         end if;

                       ---=---=---=---=---=---=---
      end RADIX_10_MULTIPLICATION;


      RADIX_10_DIVISION:
      declare
         Rate           : constant CXF2002_1.Money_Radix10 := 0.15;
         Period         : constant Integer                 := 12;
         Factor         :          CXF2002_1.Money_Radix10 := Rate / Period;
         Divisor        : constant CXF2002_1.Money_Radix10 := 1.0 / Factor;

         Initial        : constant CXF2002_1.Money_Radix10 :=    29_842.08;
         Trunc_Expected : constant CXF2002_1.Money_Radix10 :=   590_519.47;
         Round_Expected : constant CXF2002_1.Money_Radix10 :=   590_528.98;

         Balance        : CXF2002_1.Money_Radix10;
      begin
                       ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_10.Divide_And_Truncate (Balance, Divisor);
         end loop;

         if Balance /= Trunc_Expected then
            Report.Failed ("Wrong result: Radix 10 divide and truncate");
         end if;

                       ---=---=---=---=---=---=---

         Balance := Initial;

         for I in Loop_Range loop
            Radix_10.Divide_And_Round (Balance, Divisor);
         end loop;

         if Balance /= Round_Expected then
            Report.Failed ("Wrong result: Radix 10 divide and round");
         end if;

                       ---=---=---=---=---=---=---
      end RADIX_10_DIVISION;

   end RADIX_10_SUBTESTS;


               ---=---=---=---=---=---=---=---=---=---=---


   Report.Result;

end CXF2002;
