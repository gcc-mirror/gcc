-- CXF2001.A
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
--      Check that the Divide procedure provides the following results:
--        Quotient = Dividend divided by Divisor  and
--        Remainder = Dividend - (Divisor * Quotient)
--      Check that the Remainder is calculated exactly.
--
-- TEST DESCRIPTION:
--      This test is designed to test the generic procedure Divide found in
--      package Ada.Decimal.
--
--      The table below attempts to portray the design approach used in this
--      test.  There are three "dimensions" of concern:
--        1) the delta value of the Quotient and Remainder types, shown as
--           column headers,
--        2) specific choices for the Dividend and Divisor numerical values
--           (i.e., whether they yielded a repeating/non-terminating result,
--            or a terminating result ["exact"]), displayed on the left side
--            of the tables, and
--        3) the delta for the Dividend and Divisor.
--
--      Each row in the tables indicates a specific test case, showing the
--      specific quotient and remainder (under the appropriate Delta column)
--      for each combination of dividend and divisor values.  Test cases
--      follow the top-to-bottom sequence shown in the tables.
--
--      Most of the test case sets (same dividend/divisor combinations -
--      indicated by dashed horizontal lines in the tables) vary the
--      delta of the quotient and remainder types between test cases. This
--      allows for an examination of how different deltas for a quotient
--      and/or remainder type can influence the results of a division with
--      identical dividend and divisor.
--
--      Note: Test cases are performed for both Radix 10 and Radix 2 types.
--
--
--  Divid  Divis    Delta     Delta       Delta       Delta       Delta
-- (Delta)(Delta)|  .1   |    .01    |   .001    |   .0001   |  .00001   |Test
--               |---|---|-----|-----|-----|-----|-----|-----|-----|-----|Case
--    quotient   | Q | R |  Q  |  R  |  Q  |  R  |  Q  |  R  |  Q  |  R  | No.
-- ---------------------------------------------------------------------------
--  .05     .3   |.1             .02                                       1,21
-- (.01)   (.1)  |.1   0                                                   2,22
--               |         .16               .002                          3,23
--   0.166666..  |         .16   .00                                       4,24
--               |                     .166             .0002              5,25
-- ---------------------------------------------------------------------------
--  .15     20   |         .00                          .1500              6,26
-- (.01)    (1)  |         .00               .150                          7,27
--               |         .00   .15                                       8,28
--   0.0075      |               .01   .007                                9,29
--               |                     .007  .010                         10,30
--               |                                .0075 .0000             11,31
-- ---------------------------------------------------------------------------
--  .03125   .5  |                                .0625 .0000             12,32
-- (.00001) (.1) |                     .062                        .00025 13,33
--               |                     .062             .0002             14,34
--   0.0625      |                     .062  .000                         15,35
--               |               .00   .062                               16,36
--               |         .06                                     .00125 17,37
--               |         .06                          .0012             18,38
--               |         .06               .001                         19,39
--               |         .06   .00                                      20,40
-- ---------------------------------------------------------------------------
-- Divide by Zero| Raise Constraint_Error                                 41
-- ---------------------------------------------------------------------------
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      29 Dec 94   SAIC    Modified Radix 2 cases to match Radix 10 cases.
--      03 Oct 95   RBKD    Modified to fix incorrect remainder results.
--      15 Nov 95   SAIC    Incorporated reviewer fixes for ACVC 2.0.1.
--      18 Dec 06   RLB     Fixed failure message to have correct block name.
--!

with Report;
with Ada.Decimal;

procedure CXF2001 is

   TC_Verbose : Boolean := False;

begin

   Report.Test ("CXF2001", "Check that the Divide procedure provides "  &
                           "correct results. Check that the Remainder " &
                           "is calculated exactly");
   Radix_10_Block:
   declare


      -- Declare all types and variables used in the various blocks below
      -- for all Radix 10 evaluations.

      type DT_1       is delta 1.0     digits 5;
      type DT_0_1     is delta 0.1     digits 10;
      type DT_0_01    is delta 0.01    digits 10;
      type DT_0_001   is delta 0.001   digits 10;
      type DT_0_0001  is delta 0.0001  digits 10;
      type DT_0_00001 is delta 0.00001 digits 10;

      for DT_1'Machine_Radix       use 10;
      for DT_0_1'Machine_Radix     use 10;
      for DT_0_01'Machine_Radix    use 10;
      for DT_0_001'Machine_Radix   use 10;
      for DT_0_0001'Machine_Radix  use 10;
      for DT_0_00001'Machine_Radix use 10;

      Dd_1,       Dv_1,       Quot_1,       Rem_1       : DT_1       := 0.0;
      Dd_0_1,     Dv_0_1,     Quot_0_1,     Rem_0_1     : DT_0_1     := 0.0;
      Dd_0_01,    Dv_0_01,    Quot_0_01,    Rem_0_01    : DT_0_01    := 0.0;
      Dd_0_001,   Dv_0_001,   Quot_0_001,   Rem_0_001   : DT_0_001   := 0.0;
      Dd_0_0001,  Dv_0_0001,  Quot_0_0001,  Rem_0_0001  : DT_0_0001  := 0.0;
      Dd_0_00001, Dv_0_00001, Quot_0_00001, Rem_0_00001 : DT_0_00001 := 0.0;

   begin


      declare
         procedure Div is
            new Ada.Decimal.Divide(Dividend_Type  => DT_0_01,
                                   Divisor_Type   => DT_0_1,
                                   Quotient_Type  => DT_0_1,
                                   Remainder_Type => DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 1"); end if;
         Dd_0_01 := DT_0_01(0.05);  Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_1, Rem_0_01);
         if Quot_0_1 /= DT_0_1(0.1)  or  Rem_0_01 /= DT_0_01(0.02) then
            Report.Failed("Incorrect values returned, Case 1");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_0_1, DT_0_1, DT_0_1);
      begin
         if TC_Verbose then Report.Comment("Case 2"); end if;
         Dd_0_01 := DT_0_01(0.05);   Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_1, Rem_0_1);
         if Quot_0_1 /= DT_0_1(0.1) or  Rem_0_1 /= DT_0_1(0.0) then
            Report.Failed("Incorrect values returned, Case 2");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_0_1, DT_0_01, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 3"); end if;
         Dd_0_01 := DT_0_01(0.05);  Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_01, Rem_0_001);
         if Quot_0_01 /= DT_0_01(0.16) or  Rem_0_001 /= DT_0_001(0.002) then
            Report.Failed("Incorrect values returned, Case 3");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_0_1, DT_0_01, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 4"); end if;
         Dd_0_01 := DT_0_01(0.05);  Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_01, Rem_0_01);
         if Quot_0_01 /= DT_0_01(0.16) or  Rem_0_01 /= DT_0_01(0.0) then
            Report.Failed("Incorrect values returned, Case 4");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_0_1, DT_0_001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 5"); end if;
         Dd_0_01 := DT_0_01(0.05);  Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_001, Rem_0_0001);
         if Quot_0_001 /= DT_0_001(0.166)   or
            Rem_0_0001 /= DT_0_0001(0.0002)
         then
            Report.Failed("Incorrect values returned, Case 5");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_01, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 6"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_01, Rem_0_0001);
         if Quot_0_01 /= DT_0_01(0.0) or Rem_0_0001 /= DT_0_0001(0.1500) then
            Report.Failed("Incorrect values returned, Case 6");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_01, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 7"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_01, Rem_0_001);
         if Quot_0_01 /= DT_0_01(0.0) or Rem_0_001 /= DT_0_001(0.150) then
            Report.Failed("Incorrect values returned, Case 7");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_01, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 8"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_01, Rem_0_01);
         if Quot_0_01 /= DT_0_01(0.0) or Rem_0_01 /= DT_0_01(0.15) then
            Report.Failed("Incorrect values returned, Case 8");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_001, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 9"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_001, Rem_0_001);
         if Quot_0_001 /= DT_0_001(0.007) or  Rem_0_001 /= DT_0_001(0.01) then
            Report.Failed("Incorrect values returned, Case 9");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_001, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 10"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_001, Rem_0_01);
         if Quot_0_001 /= DT_0_001(0.007) or  Rem_0_01 /= DT_0_01(0.01) then
            Report.Failed("Incorrect values returned, Case 10");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_0001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 11"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_0001, Rem_0_0001);
         if Quot_0_0001 /= DT_0_0001(0.0075) or
            Rem_0_0001  /= DT_0_0001(0.0)
         then
            Report.Failed("Incorrect values returned, Case 11");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_0001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 12"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_0001, Rem_0_0001);
         if Quot_0_0001 /= DT_0_0001(0.0625) or
            Rem_0_0001  /= DT_0_0001(0.0)
         then
            Report.Failed("Incorrect values returned, Case 12");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_001, DT_0_00001);
      begin
         if TC_Verbose then Report.Comment("Case 13"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_001, Rem_0_00001);
         if Quot_0_001 /= DT_0_001(0.062) or
            Rem_0_00001  /= DT_0_00001(0.00025)
         then
            Report.Failed("Incorrect values returned, Case 13");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 14"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_001, Rem_0_0001);
         if Quot_0_001 /= DT_0_001(0.062) or
            Rem_0_0001  /= DT_0_0001(0.0002)
         then
            Report.Failed("Incorrect values returned, Case 14");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_001, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 15"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_001, Rem_0_001);
         if Quot_0_001 /= DT_0_001(0.062) or Rem_0_001 /= DT_0_001(0.000)
         then
            Report.Failed("Incorrect values returned, Case 15");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_001, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 16"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_001, Rem_0_01);
         if Quot_0_001 /= DT_0_001(0.062) or Rem_0_01 /= DT_0_01(0.00) then
            Report.Failed("Incorrect values returned, Case 16");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_01, DT_0_00001);
      begin
         if TC_Verbose then Report.Comment("Case 17"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_01, Rem_0_00001);
         if Quot_0_01 /= DT_0_01(0.06) or Rem_0_00001 /= DT_0_00001(0.00125)
         then
            Report.Failed("Incorrect values returned, Case 17");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_01, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 18"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_01, Rem_0_0001);
         if Quot_0_01 /= DT_0_01(0.06) or Rem_0_0001 /= DT_0_0001(0.0012)
         then
            Report.Failed("Incorrect values returned, Case 18");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_01, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 19"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_01, Rem_0_001);
         if Quot_0_01 /= DT_0_01(0.06) or Rem_0_001 /= DT_0_001(0.001) then
            Report.Failed("Incorrect values returned, Case 19");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_01, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 20"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_01, Rem_0_01);
         if Quot_0_01 /= DT_0_01(0.06) or Rem_0_01 /= DT_0_01(0.0) then
            Report.Failed("Incorrect values returned, Case 20");
         end if;
      end;


   exception
      when others => Report.Failed("Exception raised in Radix_10_Block");
   end Radix_10_Block;



   Radix_2_Block:
   declare

      -- Declare all types and variables used in the various blocks below
      -- for all Radix 2 evaluations.

      type DT_1       is delta 1.0     digits 5;
      type DT_0_1     is delta 0.1     digits 10;
      type DT_0_01    is delta 0.01    digits 10;
      type DT_0_001   is delta 0.001   digits 10;
      type DT_0_0001  is delta 0.0001  digits 10;
      type DT_0_00001 is delta 0.00001 digits 10;

      for DT_1'Machine_Radix       use 2;
      for DT_0_1'Machine_Radix     use 2;
      for DT_0_01'Machine_Radix    use 2;
      for DT_0_001'Machine_Radix   use 2;
      for DT_0_0001'Machine_Radix  use 2;
      for DT_0_00001'Machine_Radix use 2;

      Dd_1,       Dv_1,       Quot_1,       Rem_1       : DT_1       := 0.0;
      Dd_0_1,     Dv_0_1,     Quot_0_1,     Rem_0_1     : DT_0_1     := 0.0;
      Dd_0_01,    Dv_0_01,    Quot_0_01,    Rem_0_01    : DT_0_01    := 0.0;
      Dd_0_001,   Dv_0_001,   Quot_0_001,   Rem_0_001   : DT_0_001   := 0.0;
      Dd_0_0001,  Dv_0_0001,  Quot_0_0001,  Rem_0_0001  : DT_0_0001  := 0.0;
      Dd_0_00001, Dv_0_00001, Quot_0_00001, Rem_0_00001 : DT_0_00001 := 0.0;

   begin


      declare
         procedure Div is
            new Ada.Decimal.Divide(Dividend_Type  => DT_0_01,
                                   Divisor_Type   => DT_0_1,
                                   Quotient_Type  => DT_0_1,
                                   Remainder_Type => DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 21"); end if;
         Dd_0_01 := DT_0_01(0.05);  Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_1, Rem_0_01);
         if Quot_0_1 /= DT_0_1(0.1)  or  Rem_0_01 /= DT_0_01(0.02) then
            Report.Failed("Incorrect values returned, Case 21");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_0_1, DT_0_1, DT_0_1);
      begin
         if TC_Verbose then Report.Comment("Case 22"); end if;
         Dd_0_01 := DT_0_01(0.05);   Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_1, Rem_0_1);
         if Quot_0_1 /= DT_0_1(0.1) or  Rem_0_1 /= DT_0_1(0.0) then
            Report.Failed("Incorrect values returned, Case 22");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_0_1, DT_0_01, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 23"); end if;
         Dd_0_01 := DT_0_01(0.05);  Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_01, Rem_0_001);
         if Quot_0_01 /= DT_0_01(0.16) or  Rem_0_001 /= DT_0_001(0.002) then
            Report.Failed("Incorrect values returned, Case 23");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_0_1, DT_0_01, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 24"); end if;
         Dd_0_01 := DT_0_01(0.05);  Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_01, Rem_0_01);
         if Quot_0_01 /= DT_0_01(0.16) or  Rem_0_01 /= DT_0_01(0.0) then
            Report.Failed("Incorrect values returned, Case 24");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_0_1, DT_0_001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 25"); end if;
         Dd_0_01 := DT_0_01(0.05);  Dv_0_1 := DT_0_1(0.3);
         Div(Dd_0_01, Dv_0_1, Quot_0_001, Rem_0_0001);
         if Quot_0_001 /= DT_0_001(0.166)   or
            Rem_0_0001 /= DT_0_0001(0.0002)
         then
            Report.Failed("Incorrect values returned, Case 25");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_01, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 26"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_01, Rem_0_0001);
         if Quot_0_01 /= DT_0_01(0.0) or Rem_0_0001 /= DT_0_0001(0.1500) then
            Report.Failed("Incorrect values returned, Case 26");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_01, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 27"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_01, Rem_0_001);
         if Quot_0_01 /= DT_0_01(0.0) or Rem_0_001 /= DT_0_001(0.150) then
            Report.Failed("Incorrect values returned, Case 27");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_01, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 28"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_01, Rem_0_01);
         if Quot_0_01 /= DT_0_01(0.0) or Rem_0_01 /= DT_0_01(0.15) then
            Report.Failed("Incorrect values returned, Case 28");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_001, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 29"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_001, Rem_0_001);
         if Quot_0_001 /= DT_0_001(0.007) or  Rem_0_001 /= DT_0_001(0.01) then
            Report.Failed("Incorrect values returned, Case 29");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_001, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 30"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_001, Rem_0_01);
         if Quot_0_001 /= DT_0_001(0.007) or  Rem_0_01 /= DT_0_01(0.01) then
            Report.Failed("Incorrect values returned, Case 30");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_01, DT_1, DT_0_0001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 31"); end if;
         Dd_0_01 := DT_0_01(0.15);  Dv_1 := DT_1(20);
         Div(Dd_0_01, Dv_1, Quot_0_0001, Rem_0_0001);
         if Quot_0_0001 /= DT_0_0001(0.0075) or
            Rem_0_0001  /= DT_0_0001(0.0)
         then
            Report.Failed("Incorrect values returned, Case 31");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_0001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 32"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_0001, Rem_0_0001);
         if Quot_0_0001 /= DT_0_0001(0.0625) or
            Rem_0_0001  /= DT_0_0001(0.0)
         then
            Report.Failed("Incorrect values returned, Case 32");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_001, DT_0_00001);
      begin
         if TC_Verbose then Report.Comment("Case 33"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_001, Rem_0_00001);
         if Quot_0_001 /= DT_0_001(0.062) or
            Rem_0_00001  /= DT_0_00001(0.00025)
         then
            Report.Failed("Incorrect values returned, Case 33");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 34"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_001, Rem_0_0001);
         if Quot_0_001 /= DT_0_001(0.062) or
            Rem_0_0001  /= DT_0_0001(0.0002)
         then
            Report.Failed("Incorrect values returned, Case 34");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_001, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 35"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_001, Rem_0_001);
         if Quot_0_001 /= DT_0_001(0.062) or Rem_0_001 /= DT_0_001(0.000)
         then
            Report.Failed("Incorrect values returned, Case 35");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_001, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 36"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_001, Rem_0_01);
         if Quot_0_001 /= DT_0_001(0.062) or Rem_0_01 /= DT_0_01(0.00) then
            Report.Failed("Incorrect values returned, Case 36");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_01, DT_0_00001);
      begin
         if TC_Verbose then Report.Comment("Case 37"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_01, Rem_0_00001);
         if Quot_0_01 /= DT_0_01(0.06) or Rem_0_00001 /= DT_0_00001(0.00125)
         then
            Report.Failed("Incorrect values returned, Case 37");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_01, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 38"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_01, Rem_0_0001);
         if Quot_0_01 /= DT_0_01(0.06) or Rem_0_0001 /= DT_0_0001(0.0012)
         then
            Report.Failed("Incorrect values returned, Case 38");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_01, DT_0_001);
      begin
         if TC_Verbose then Report.Comment("Case 39"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_01, Rem_0_001);
         if Quot_0_01 /= DT_0_01(0.06) or Rem_0_001 /= DT_0_001(0.001) then
            Report.Failed("Incorrect values returned, Case 39");
         end if;
      end;


      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_00001, DT_0_1, DT_0_01, DT_0_01);
      begin
         if TC_Verbose then Report.Comment("Case 40"); end if;
         Dd_0_00001 := DT_0_00001(0.03125);  Dv_0_1 := DT_0_1(0.5);
         Div(Dd_0_00001, Dv_0_1, Quot_0_01, Rem_0_01);
         if Quot_0_01 /= DT_0_01(0.06) or Rem_0_01 /= DT_0_01(0.0) then
            Report.Failed("Incorrect values returned, Case 40");
         end if;
      end;

      declare
         procedure Div is
            new Ada.Decimal.Divide(DT_0_0001, DT_1, DT_0_0001, DT_0_0001);
      begin
         if TC_Verbose then Report.Comment("Case 41"); end if;
         Dd_0_0001 := (DT_0_0001(6062.0) / DT_0_0001(16384.0));
         Dv_1 := DT_1(0.0);
         Div(Dd_0_0001, Dv_1, Quot_0_0001, Rem_0_0001);
         Report.Failed("Divide by Zero didn't raise Constraint_Error, " &
                       "Case 41");
      exception
         when Constraint_Error =>  null;  -- OK, expected exception.
         when others =>
            Report.Failed("Unexpected exception raised by Divide by Zero," &
                          "Case 41");
      end;

   exception
      when others => Report.Failed("Exception raised in Radix_2_Block");
   end Radix_2_Block;


   Report.Result;

end CXF2001;
