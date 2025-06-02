-- FXACB00.A
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
-- FOUNDATION DESCRIPTION:
--      This foundation consists of type definitions and object declarations 
--      used by tests of Stream_IO functionality.
--      These types include an unconstrained array type, and a discriminated 
--      record without a default discriminant, specifically chosen for use in 
--      demonstrating the capabilities of 'Output and 'Input. 
--      
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package FXACB00 is

   type    Customer_Type          is (Residence, Apartment, Commercial);
   type    Electric_Usage_Type    is range 0..100000;
   type    Months_In_Service_Type is range 1..12;
   type    Quarterly_Period_Type  is (Spring, Summer, Autumn, Winter);
   subtype Month_In_Quarter_Type  is Positive range 1..3;
   type    Service_History_Type   is 
     array (Quarterly_Period_Type range <>, Month_In_Quarter_Type range <>) 
     of Electric_Usage_Type;


   type Service_Type (Customer : Customer_Type) is
      record
         Name       : String (1..21);               
         Account_ID : Natural range 0..100;
         case Customer is
            when Residence | Apartment =>
               Low_Income_Credit : Boolean := False;
            when Commercial            =>
               Baseline_Allowance : Natural range 0..1000;
               Quantity_Discount  : Boolean := False;
         end case;
      end record;


   -- Object Declarations


   Customer1 : Service_Type (Residence) := 
     (Residence, "1221 Morningstar Lane", 44, False); 
   Customer2 : Service_Type (Apartment) := (Customer => Apartment,
                                            Account_ID => 67,
                                            Name => "15 South Front St. #8",
                                            Low_Income_Credit => True);
   Customer3 : Service_Type (Commercial) := (Commercial,
                                             "12442 Central Avenue ", 
                                             100, 
                                             Baseline_Allowance => 938, 
                                             Quantity_Discount  => True);

   --

   C1_Months : Months_In_Service_Type := 10;
   C2_Months : Months_In_Service_Type :=  2;
   C3_Months : Months_In_Service_Type := 12;
   
   --

   C1_Service_History : 
     Service_History_Type (Quarterly_Period_Type, Month_In_Quarter_Type) := 
       (Spring => (1 => 35, 2 => 39, 3 => 32),
        Summer => (1 => 34, 2 => 33, 3 => 39),
        Autumn => (1 => 45, 2 => 40, 3 => 38),
        Winter => (1 => 53, 2 =>  0, 3 =>  0));
  
   C2_Service_History : 
     Service_History_Type (Quarterly_Period_Type range Spring..Summer, 
                           Month_In_Quarter_Type) := 
       (Spring => (23, 22, 0), Summer => (0, 0, 0));

   C3_Service_History :
     Service_History_Type (Quarterly_Period_Type, Month_In_Quarter_Type) := 
       (others => (others => 200));

   --

   Total_Customers_In_Service : constant Natural := 3;

end FXACB00;
