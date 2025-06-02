-- CXG2022.A
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
--      Check that multiplication and division of binary fixed point 
--      numbers with compatible 'small values produce exact results.
--
-- TEST DESCRIPTION:
--      Signed, unsigned, and a mixture of signed and unsigned
--      binary fixed point values are multiplied and divided.
--      The result is checked against the expected "perfect result set"
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
--
--
-- CHANGE HISTORY:
--       1 Apr 96   SAIC    Initial release for 2.1
--      29 Jan 1998 EDS     Repaired fixed point errors ("**" and 
--                          assumptions about 'Small)
--!

with System;
with Report;
procedure CXG2022 is
   Verbose : constant Boolean := False;

procedure Check_Signed is
   type Pairs is delta 2.0 range -2.0 ** (System.Max_Mantissa) ..
                                  2.0 ** (System.Max_Mantissa) - 1.0;
   type Halves is delta 0.5 range -2.0 ** (System.Max_Mantissa-2) ..
                                   2.0 ** (System.Max_Mantissa-2) - 1.0;
   P1, P2, P3, P4 : Pairs;
   H1, H2, H3, H4 : Halves;

   procedure Dont_Opt is
   -- keep optimizer from knowing the constant value of expressions
   begin
      if Report.Ident_Bool (False) then
         P1 :=  2.0;  P2 := 4.0;  P3 := 6.0;
         H1 := -2.0;  H2 := 9.0;  H3 := 3.0;
      end if;
   end Dont_Opt;

begin
   H1 := -0.5;
   H2 := Halves'First;
   H3 := 1.0;
   P1 := 12.0;
   P2 := Pairs'First;
   P3 := Pairs'Last;
   Dont_Opt;

   P4 := Pairs (P1 * H1);     -- 12.0 * -0.5
   if P4 /= -6.0 then
      Report.Failed ("12.0 * -0.5 = " & Pairs'Image (P4));
   end if;

   H4 := Halves (P1 / H1);     -- 12.0 / -0.5
   if H4 /= -24.0 then
      Report.Failed ("12.0 / -0.5 = " & Halves'Image (H4));
   end if;

   P4 := P3 * H3;     -- Pairs'Last * 1.0
   if P4 /= Pairs'Last then
      Report.Failed ("Pairs'Last * 1.0 = " & Pairs'Image (P4));
   end if;

   P4 := P3 / H3;     -- Pairs'Last / 1.0
   if P4 /= Pairs'Last then
      Report.Failed ("Pairs'Last / 1.0 = " & Pairs'Image (P4));
   end if;

   P4 := P2 * 0.25;     -- Pairs'First * 0.25
   if P4 /= Pairs (-2.0 ** (System.Max_Mantissa - 2)) then
      Report.Failed ("Pairs'First * 0.25 = " & Pairs'Image (P4));
   end if;

   P4 := 100.5 / H1;   -- 100.5 / -0.5
   if P4 = -201.0 then
      null;   -- Perfect result
   elsif Pairs'Small = 2.0 and ( P4 = -200.0 or P4 = -202.0 ) then
      null;   -- Allowed variation
   else
      Report.Failed ("Pairs'Small =" & Pairs'Image (Pairs'Small) &
                     " and 100.5/-0.5 = " & Pairs'Image (P4) );
   end if;

   H4 := H1 * H2;   --  -0.5 * Halves'First
   if H4 /= Halves (2.0 ** (System.Max_Mantissa-3)) then
      Report.Failed ("-0.5 * Halves'First =" & Halves'Image (H4) &
                     " instead of " &
                     Halves'Image( Halves(2.0 ** (System.Max_Mantissa-3)))); 
   end if;

exception
   when others =>
      Report.Failed ("unexpected exception in Check_Signed");
end Check_Signed;



procedure Check_Unsigned is
   type Pairs is delta 2.0 range 0.0 .. 2.0 ** (System.Max_Mantissa+1) - 1.0;
   type Halves is delta 0.5 range 0.0 .. 2.0 ** (System.Max_Mantissa-1) - 1.0;
   P1, P2, P3, P4 : Pairs;
   H1, H2, H3, H4 : Halves;

   procedure Dont_Opt is
   -- keep optimizer from knowing the constant value of expressions
   begin
      if Report.Ident_Bool (False) then
         P1 :=  2.0;  P2 := 4.0;  P3 := 6.0;
         H1 :=  2.0;  H2 := 9.0;  H3 := 3.0;
      end if;
   end Dont_Opt;

begin
   H1 := 10.5;
   H2 := Halves(2.0 ** (System.Max_Mantissa - 6));
   H3 := 1.0;
   P1 := 12.0;
   P2 := Pairs'Last / 2;
   P3 := Pairs'Last;
   Dont_Opt;

   P4 := Pairs (P1 * H1);     -- 12.0 * 10.5
   if P4 /= 126.0 then
      Report.Failed ("12.0 * 10.5 = " & Pairs'Image (P4));
   end if;

   H4 := Halves (P1 / H1);     -- 12.0 / 10.5
   if H4 /= 1.0 and H4 /= 1.5 then
      Report.Failed ("12.0 / 10.5 = " & Halves'Image (H4));
   end if;

   P4 := P3 * H3;     -- Pairs'Last * 1.0
   if P4 /= Pairs'Last then
      Report.Failed ("Pairs'Last * 1.0 = " & Pairs'Image (P4));
   end if;

   P4 := P3 / H3;     -- Pairs'Last / 1.0
   if P4 /= Pairs'Last then
      Report.Failed ("Pairs'Last / 1.0 = " & Pairs'Image (P4));
   end if;

   P4 := P1 * 0.25;     -- 12.0 * 0.25
   if P4 /= 2.0 and P4 /= 4.0 then
      Report.Failed ("12.0 * 0.25 = " & Pairs'Image (P4));
   end if;

   P4 := 100.5 / H1;   -- 100.5 / 10.5    = 9.571...
   if P4 /= 8.0 and P4 /= 10.0 then
      Report.Failed ("100.5/10.5 = " & Pairs'Image (P4));
   end if;

   H4 := H2 * 2;   --  2**(max_mantissa-6) * 2
   if H4 /= Halves(2.0 ** (System.Max_Mantissa-5)) then
      Report.Failed ("2**(System.Max_Mantissa-6) * 2=" & Halves'Image (H4) &
                     " instead of " &
                     Halves'Image( Halves(2.0 ** (System.Max_Mantissa-5)))); 
   end if;

exception
   when others =>
      Report.Failed ("unexpected exception in Check_Unsigned");
end Check_Unsigned;



procedure Check_Mixed is
   type Pairs is delta 2.0 range -2.0 ** (System.Max_Mantissa) ..
                                  2.0 ** (System.Max_Mantissa) - 1.0;
   type Halves is delta 0.5 range 0.0 .. 2.0 ** (System.Max_Mantissa-1) - 1.0;
   P1, P2, P3, P4 : Pairs;
   H1, H2, H3, H4 : Halves;

   procedure Dont_Opt is
   -- keep optimizer from knowing the constant value of expressions
   begin
      if Report.Ident_Bool (False) then
         P1 :=  2.0;  P2 := 4.0;  P3 := 6.0;
         H1 :=  2.0;  H2 := 9.0;  H3 := 3.0;
      end if;
   end Dont_Opt;

begin
   H1 := 10.5;
   H2 := Halves(2.0 ** (System.Max_Mantissa - 6));
   H3 := 1.0;
   P1 := 12.0;
   P2 := -4.0;
   P3 := Pairs'Last;
   Dont_Opt;

   P4 := Pairs (P1 * H1);     -- 12.0 * 10.5
   if P4 /= 126.0 then
      Report.Failed ("12.0 * 10.5 = " & Pairs'Image (P4));
   end if;

   H4 := Halves (P1 / H1);     -- 12.0 / 10.5
   if H4 /= 1.0 and H4 /= 1.5 then
      Report.Failed ("12.0 / 10.5 = " & Halves'Image (H4));
   end if;

   P4 := P3 * H3;     -- Pairs'Last * 1.0
   if P4 /= Pairs'Last then
      Report.Failed ("Pairs'Last * 1.0 = " & Pairs'Image (P4));
   end if;

   P4 := P3 / H3;     -- Pairs'Last / 1.0
   if P4 /= Pairs'Last then
      Report.Failed ("Pairs'Last / 1.0 = " & Pairs'Image (P4));
   end if;

   P4 := P1 * 0.25;   -- 12.0 * 0.25
   if P4 = 3.0 then
      null;    -- Perfect result
   elsif Pairs'Small = 2.0 and then ( P4 = 2.0 or P4 = 4.0 ) then
      null;    -- Allowed deviation
   else
      Report.Failed ("Pairs'Small =" & Pairs'Image (Pairs'Small) &
                     "and 12.0 * 0.25 = " & Pairs'Image (P4) );
   end if;

   P4 := 100.5 / H1;   -- 100.5 / 10.5    = 9.571...
   if P4 = 9.0 then
      null;    -- Perfect result
   elsif Pairs'Small = 2.0 and then ( P4 = 8.0 or P4 = 10.0 ) then
      null;    -- Allowed values
   else
     Report.Failed ("Pairs'Small =" & Pairs'Image (Pairs'Small) &
                    "and 100.5/10.5 = " & Pairs'Image (P4) );
   end if;

   H4 := H2 * 2;   --  2**(max_mantissa-6) * 2
   if H4 /= Halves(2.0 ** (System.Max_Mantissa-5)) then
      Report.Failed ("2**(System.Max_Mantissa-6) * 2=" & Halves'Image (H4) &
                     " instead of " &
                     Halves'Image( Halves(2.0 ** (System.Max_Mantissa-5)))); 
   end if;

   P4 := Pairs(P1 * 6) / P2;    -- 12 * 6 / -4
   if (P4 /= -18.0)  then
      Report.Failed ("12*6/-4 = " & Pairs'Image(P4));
   end if;

   P4 := Halves(P1 * 6.0) / P2;    -- 12 * 6 / -4
   if (P4 /= -18.0)  then
      Report.Failed ("Halves(12*6)/-4 = " & Pairs'Image(P4));
   end if;

exception
   when others =>
      Report.Failed ("unexpected exception in Check_Mixed");
end Check_Mixed;


begin  -- main
   Report.Test ("CXG2022",
                "Check the accuracy of multiplication and division" &
                " of binary fixed point numbers"); 
   if Verbose then
      Report.Comment ("starting signed test");
   end if;
   Check_Signed;

   if Verbose then
      Report.Comment ("starting unsigned test");
   end if;
   Check_Unsigned;

   if Verbose then
      Report.Comment ("starting mixed sign test");
   end if;
   Check_Mixed;

   Report.Result;
end CXG2022;
