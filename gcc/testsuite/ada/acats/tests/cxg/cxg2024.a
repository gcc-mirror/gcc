-- CXG2024.A
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
--      Check that multiplication and division of decimal 
--      and binary fixed point numbers that result in a
--      decimal fixed point type produce acceptable results.
--
-- TEST DESCRIPTION:
--      Multiplication and division of mixed binary and decimal
--      values are performed.  Identity functions are used so 
--      that the operands of the expressions will not be seen
--      as static by the compiler.
--
-- SPECIAL REQUIREMENTS
--      The Strict Mode for the numerical accuracy must be
--      selected.  The method by which this mode is selected
--      is implementation dependent.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Numerics Annex.
--      This test only applies to the Strict Mode for numerical
--      accuracy.
--      This test applies only to implementations supporting
--      decimal fixed point types of at least 9 digits.
--
--
-- CHANGE HISTORY:
--       4 Apr 96   SAIC    Initial release for 2.1
--      17 Aug 96   SAIC    Removed checks for close results
--
--!

with System;
with Report;
procedure CXG2024 is

procedure Do_Check is
   Num_Digits : constant := 9;
   type Pennies is delta 0.01 digits Num_Digits;
   type Dollars is delta 1.0 digits Num_Digits;

   type Signed_Sixteenths is delta 0.0625 
          range -2.0 ** (System.Max_Mantissa-5) ..
                 2.0 ** (System.Max_Mantissa-5) - 1.0;
   type Unsigned_Sixteenths is delta 0.0625 
          range 0.0 .. 2.0 ** (System.Max_Mantissa-4) - 1.0;

   P1 : Pennies;
   D1 : Dollars;

   -- optimization thwarting functions

   function P (X : Pennies) return Pennies is
   begin
      if Report.Ident_Bool (True) then
          return X;
      else
          return 3.21;    -- never executed
      end if;
   end P;


   function D (X : Dollars) return Dollars is
   begin
      if Report.Ident_Bool (True) then
          return X;
      else
          return 321.0;    -- never executed
      end if;
   end D;


   function US (X : Unsigned_Sixteenths) return Unsigned_Sixteenths is
   begin
      if Report.Ident_Bool (True) then
          return X;
      else
          return 321.0;    -- never executed
      end if;
   end US;


   function SS (X : Signed_Sixteenths) return Signed_Sixteenths is
   begin
      if Report.Ident_Bool (True) then
          return X;
      else
          return 321.0;    -- never executed
      end if;
   end SS;


begin

   P1 := P(0.05) * SS(-200.0);
   if P1 /= -10.00 then
         Report.Failed ("1 - expected -10.00  got " & Pennies'Image (P1));
   end if;

   D1 := P(0.05) * SS(-100.0);
   if D1 /= -5.00 then
         Report.Failed ("2 - expected -5.00  got " & Dollars'Image (D1));
   end if;

   P1 := P(0.05) * US(200.0);
   if P1 /= 10.00 then
         Report.Failed ("3 - expected 10.00  got " & Pennies'Image (P1));
   end if;

   D1 := P(-0.05) * US(100.0);
   if D1 /= -5.00 then
         Report.Failed ("4 - expected -5.00  got " & Dollars'Image (D1));
   end if;



   P1 := P(0.05) / US(1.0);
   if P1 /= 0.05 then
         Report.Failed ("6 - expected 0.05  got " & Pennies'Image (P1));
   end if;

  
   -- check rounding

   D1 := Dollars'Round (Pennies (P(-101.00) / US(2.0)));
   if D1 /= -51.00 then
         Report.Failed ("11 - expected -51.00  got " & Dollars'Image (D1));
   end if;

   D1 := Dollars'Round (Pennies (P(101.00) / US(2.0)));
   if D1 /= 51.00 then
         Report.Failed ("12 - expected 51.00  got " & Dollars'Image (D1));
   end if;

   D1 := Dollars'Round (Pennies (SS(-101.00) / P(2.0)));
   if D1 /= -51.00 then
         Report.Failed ("13 - expected -51.00  got " & Dollars'Image (D1));
   end if;

   D1 := Dollars'Round (Pennies (US(101.00) / P(2.0)));
   if D1 /= 51.00 then
         Report.Failed ("14 - expected 51.00  got " & Dollars'Image (D1));
   end if;



   P1 := P(-102.03) / SS(-0.5);
   if P1 /= 204.06 then
         Report.Failed ("15 - expected 204.06  got " & Pennies'Image (P1));
   end if;


exception
   when others =>
      Report.Failed ("unexpected exception in Do_Check");
end Do_Check;


begin  -- main
   Report.Test ("CXG2024",
                "Check the accuracy of multiplication and division" &
                " of mixed decimal and binary fixed point numbers"); 

   Do_Check;

   Report.Result;
end CXG2024;
