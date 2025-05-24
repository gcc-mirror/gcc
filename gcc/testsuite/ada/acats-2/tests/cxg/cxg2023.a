-- CXG2023.A
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
--      Check that multiplication and division of decimal fixed point 
--      numbers produce exact results.
--
-- TEST DESCRIPTION:
--      Check that multiplication and division of decimal fixed point 
--      numbers produce exact results.
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
--       3 Apr 96   SAIC    Initial release for 2.1
--
--!

with System;
with Report;
procedure CXG2023 is
   Verbose : constant Boolean := False;

procedure Check_1 is
   Num_Digits : constant := 6;
   type Pennies is delta 0.01 digits Num_Digits;
   type Franklins is delta 100.0 digits Num_Digits;
   type Dollars is delta 1.0 digits Num_Digits;

   P1 : Pennies;
   F1 : Franklins;
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


   function F (X : Franklins) return Franklins is
   begin
      if Report.Ident_Bool (True) then
          return X;
      else
          return 32100.0;    -- never executed
      end if;
   end F;


   function D (X : Dollars) return Dollars is
   begin
      if Report.Ident_Bool (True) then
          return X;
      else
          return 321.0;    -- never executed
      end if;
   end D;


begin
   -- multiplication where one operand is universal real

   P1 := P(0.05) * 200.0;
   if P1 /= 10.00 then
      Report.Failed ("1 - expected 10.00  got " & Pennies'Image (P1));
   end if;

   D1 := P(0.05) * 100.0;
   if D1 /= 5.00 then
      Report.Failed ("2 - expected 5.00  got " & Dollars'Image (D1));
   end if;

   F1 := P(0.05) * 50_000.0;
   if F1 /= 2500.00 then
      Report.Failed ("3 - expected 2500.0  got " & Franklins'Image (F1));
   end if;

   -- multiplication where both operands are decimal fixed

   P1 := P(0.05) * D(-200.0);
   if P1 /= -10.00 then
      Report.Failed ("4 - expected -10.00  got " & Pennies'Image (P1));
   end if;

   D1 := P(0.05) * P(-100.0);
   if D1 /= -5.00 then
      Report.Failed ("5 - expected -5.00  got " & Dollars'Image (D1));
   end if;

   F1 := P(-0.05) * F(50_000.0);
   if F1 /= -2500.00 then
      Report.Failed ("6 - expected -2500.0  got " & Franklins'Image (F1));
   end if;

   -- division where one operand is universal real

   P1 := P(0.05) / 0.001;
   if P1 /= 50.00 then
      Report.Failed ("7 - expected 50.00  got " & Pennies'Image (P1));
   end if;

   D1 := D(1000.0) / 3.0;
   if D1 /= 333.00 then
      Report.Failed ("8 - expected 333.00  got " & Dollars'Image (D1));
   end if;

   F1 := P(1234.56) / 0.0001;
   if F1 /= 12345600.00 then
      Report.Failed ("9 - expected 12345600.0  got " & Franklins'Image (F1));
   end if;


   -- division where both operands are decimal fixed

   P1 := P(0.05) / D(1.0);
   if P1 /= 0.05 then
      Report.Failed ("10 - expected 0.05  got " & Pennies'Image (P1));
   end if;

       -- check for truncation toward 0
   D1 := P(-101.00) / P(2.0);
   if D1 /= -50.00 then
      Report.Failed ("11 - expected -50.00  got " & Dollars'Image (D1));
   end if;

   P1 := P(-102.03) / P(-0.5);
   if P1 /= 204.06 then
      Report.Failed ("12 - expected 204.06  got " & Pennies'Image (P1));
   end if;

   F1 := P(876.54) / P(0.03);
   if F1 /= 29200.00 then
      Report.Failed ("13 - expected 29200.0  got " & Franklins'Image (F1));
   end if;

exception
   when others =>
      Report.Failed ("unexpected exception in Check_1");
end Check_1;

generic
   type Pennies   is delta<> digits<>;
   type Dollars   is delta<> digits<>;
   type Franklins is delta<> digits<>;
procedure Generic_Check;
procedure Generic_Check is

   -- the following code is copied directly from the
   -- above procedure Check_1

   P1 : Pennies;
   F1 : Franklins;
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


   function F (X : Franklins) return Franklins is
   begin
      if Report.Ident_Bool (True) then
          return X;
      else
          return 32100.0;    -- never executed
      end if;
   end F;


   function D (X : Dollars) return Dollars is
   begin
      if Report.Ident_Bool (True) then
          return X;
      else
          return 321.0;    -- never executed
      end if;
   end D;


begin
   -- multiplication where one operand is universal real

   P1 := P(0.05) * 200.0;
   if P1 /= 10.00 then
      Report.Failed ("1 - expected 10.00  got " & Pennies'Image (P1));
   end if;

   D1 := P(0.05) * 100.0;
   if D1 /= 5.00 then
      Report.Failed ("2 - expected 5.00  got " & Dollars'Image (D1));
   end if;

   F1 := P(0.05) * 50_000.0;
   if F1 /= 2500.00 then
      Report.Failed ("3 - expected 2500.0  got " & Franklins'Image (F1));
   end if;

   -- multiplication where both operands are decimal fixed

   P1 := P(0.05) * D(-200.0);
   if P1 /= -10.00 then
      Report.Failed ("4 - expected -10.00  got " & Pennies'Image (P1));
   end if;

   D1 := P(0.05) * P(-100.0);
   if D1 /= -5.00 then
      Report.Failed ("5 - expected -5.00  got " & Dollars'Image (D1));
   end if;

   F1 := P(-0.05) * F(50_000.0);
   if F1 /= -2500.00 then
      Report.Failed ("6 - expected -2500.0  got " & Franklins'Image (F1));
   end if;

   -- division where one operand is universal real

   P1 := P(0.05) / 0.001;
   if P1 /= 50.00 then
      Report.Failed ("7 - expected 50.00  got " & Pennies'Image (P1));
   end if;

   D1 := D(1000.0) / 3.0;
   if D1 /= 333.00 then
      Report.Failed ("8 - expected 333.00  got " & Dollars'Image (D1));
   end if;

   F1 := P(1234.56) / 0.0001;
   if F1 /= 12345600.00 then
      Report.Failed ("9 - expected 12345600.0  got " & Franklins'Image (F1));
   end if;


   -- division where both operands are decimal fixed

   P1 := P(0.05) / D(1.0);
   if P1 /= 0.05 then
      Report.Failed ("10 - expected 0.05  got " & Pennies'Image (P1));
   end if;

       -- check for truncation toward 0
   D1 := P(-101.00) / P(2.0);
   if D1 /= -50.00 then
      Report.Failed ("11 - expected -50.00  got " & Dollars'Image (D1));
   end if;

   P1 := P(-102.03) / P(-0.5);
   if P1 /= 204.06 then
      Report.Failed ("12 - expected 204.06  got " & Pennies'Image (P1));
   end if;

   F1 := P(876.54) / P(0.03);
   if F1 /= 29200.00 then
      Report.Failed ("13 - expected 29200.0  got " & Franklins'Image (F1));
   end if;

end Generic_Check;


procedure Check_G6 is
   Num_Digits : constant := 6;
   type Pennies is delta 0.01 digits Num_Digits;
   type Franklins is delta 100.0 digits Num_Digits;
   type Dollars is delta 1.0 digits Num_Digits;

   procedure G is new Generic_Check (Pennies, Dollars, Franklins);
begin
   G;
end Check_G6;


procedure Check_G9 is
   Num_Digits : constant := 9;
   type Pennies is delta 0.01 digits Num_Digits;
   type Franklins is delta 100.0 digits Num_Digits;
   type Dollars is delta 1.0 digits Num_Digits;

   procedure G is new Generic_Check (Pennies, Dollars, Franklins);
begin
   G;
end Check_G9;


begin  -- main
   Report.Test ("CXG2023",
                "Check the accuracy of multiplication and division" &
                " of decimal fixed point numbers"); 

   if Verbose then
      Report.Comment ("starting Check_1");
   end if;
   Check_1;

   if Verbose then
      Report.Comment ("starting Check_G6");
   end if;
   Check_G6;

   if Verbose then
      Report.Comment ("starting Check_G9");
   end if;
   Check_G9;

   Report.Result;
end CXG2023;
