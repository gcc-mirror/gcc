-- C457006.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--     Check that overloaded functions can be resolved when they appear as
--     dependent expressions in a conditional expression. Part 1: Enumeration
--     literals.
--
-- TEST DESCRIPTION:
--     We declare three enumeration types with a few literals that appear
--     in more than one type. While these types aren't that realistic
--     themselves (although the list of employees is a subset of the
--     real employees of one Ada vendor, circa 1988), this sort of overloading
--     is common in practice, especially when multiple packages or types
--     appear in use clauses. Similarly, the use of conditional expressions
--     is common enough that we expect pretty much anything that can be written
--     to be written eventually. As such, we think that pretty much anything
--     meets the intent of usage-oriented testing for these features. Thus,
--     we created a rather simple test that just tries to test the objective
--     rather than creating a fancy usage scenario.
--
-- CHANGE HISTORY:
--     04 Dec 15   RLB     Created test.
--!

with Report;
procedure C457006 is

   type Base is (Bin, Oct, Dec, Hex);

   type Month is (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

   type Employee is (Jan, Jen, Jim, Tim, Tom, Mary, Randy, Ike);

   type TC_Limb is (First, Second, Third);

   procedure Check (Value, First_Expected,
                    Second_Expected, Third_Expected : Base;
                    TC_This_Limb : TC_Limb; TC_Subtest : String) is
   begin
      case TC_This_Limb is
         when First =>
            if Value /= First_Expected then
               Report.Failed ("Value of " & Base'Image(Value) &
                              " not expected value of " &
                              Base'Image(First_Expected) &
                              " (" & TC_Subtest & "1)");
            end if;
         when Second =>
            if Value /= Second_Expected then
               Report.Failed ("Value of " & Base'Image(Value) &
                              " not expected value of " &
                              Base'Image(Second_Expected) &
                              " (" & TC_Subtest & "2)");
            end if;
         when Third =>
            if Value /= Third_Expected then
               Report.Failed ("Value of " & Base'Image(Value) &
                              " not expected value of " &
                              Base'Image(Third_Expected) &
                              " (" & TC_Subtest & "3)");
            end if;
      end case;
   end Check;

   procedure Check (Value, First_Expected,
                    Second_Expected, Third_Expected : Month;
                    TC_This_Limb : TC_Limb; TC_Subtest : String) is
   begin
      case TC_This_Limb is
         when First =>
            if Value /= First_Expected then
               Report.Failed ("Value of " & Month'Image(Value) &
                              " not expected value of " &
                              Month'Image(First_Expected) &
                              " (" & TC_Subtest & "1)");
            end if;
         when Second =>
            if Value /= Second_Expected then
               Report.Failed ("Value of " & Month'Image(Value) &
                              " not expected value of " &
                              Month'Image(Second_Expected) &
                              " (" & TC_Subtest & "2)");
            end if;
         when Third =>
            if Value /= Third_Expected then
               Report.Failed ("Value of " & Month'Image(Value) &
                              " not expected value of " &
                              Month'Image(Third_Expected) &
                              " (" & TC_Subtest & "3)");
            end if;
      end case;
   end Check;

   procedure Check (Value, First_Expected,
                    Second_Expected, Third_Expected : Employee;
                    TC_This_Limb : TC_Limb; TC_Subtest : String) is
   begin
      case TC_This_Limb is
         when First =>
            if Value /= First_Expected then
               Report.Failed ("Value of " & Employee'Image(Value) &
                              " not expected value of " &
                              Employee'Image(First_Expected) &
                              " (" & TC_Subtest & "1)");
            end if;
         when Second =>
            if Value /= Second_Expected then
               Report.Failed ("Value of " & Employee'Image(Value) &
                              " not expected value of " &
                              Employee'Image(Second_Expected) &
                              " (" & TC_Subtest & "2)");
            end if;
         when Third =>
            if Value /= Third_Expected then
               Report.Failed ("Value of " & Employee'Image(Value) &
                              " not expected value of " &
                              Employee'Image(Third_Expected) &
                              " (" & TC_Subtest & "3)");
            end if;
      end case;
   end Check;

   procedure SubtestA (Selector : Natural; Limb : TC_Limb) is
      -- As a qualified expression (easy: the context provides a single type).
      A_Month : Month := Dec;
   begin
      case Limb is
         when First =>
            if not (Selector < 1) then
               Report.Failed ("Selector chooses wrong limb (A1)");
            end if;
         when Second =>
            if not (Selector in 1..8) then
               Report.Failed ("Selector chooses wrong limb (A2)");
            end if;
         when Third =>
            if not (Selector > 8) then
               Report.Failed ("Selector chooses wrong limb (A3)");
            end if;
      end case;
      -- No overloaded dependent expressions:
      Check (Month'(if Selector < 1 then A_Month
                    elsif Selector in 1..8 then Jul
                    else Aug),
             A_Month, Jul, Aug, Limb, "A1");
      Check (Month'(case Selector is
                       when 0      => A_Month,
                       when 1..8   => Aug,
                       when others => Nov),
             A_Month, Aug, Nov, Limb, "A2");
      -- One overloaded dependent expression:
      Check (Month'(if Selector < 1 then Jan
                    elsif Selector in 1..8 then A_Month
                    else Apr),
             Jan, A_Month, Apr, Limb, "A3");
      Check (Month'(case Selector is
                       when 0      => Oct,
                       when 1..8   => Mar,
                       when others => A_Month),
             Oct, Mar, A_Month, Limb, "A4");
      -- All overloaded dependent expressions:
      Check (Month'(if Selector < 1 then Jan
                    elsif Selector in 1..8 then Oct
                    else Dec),
             Jan, Oct, Dec, Limb, "A5");
      Check (Month'(case Selector is
                       when 0      => Jan,
                       when 1..8   => Oct,
                       when others => Dec),
             Jan, Oct, Dec, Limb, "A6");
   end SubTestA;

   procedure SubtestB (Selector : Natural; Limb : TC_Limb) is
      -- As the parameter of an overloaded routine (harder: the context
      -- provides several separate types, only one of which works)
      A_Month : Month := Aug;

      -- Overloaded routines using global objects so that they don't
      -- affect the resolution themselves:
      procedure Hide (Value : Base; Which : Base) is
      begin
          Report.Failed ("Should not call this routine (BB)");
      end Hide;

      procedure Hide (Value : Employee; Which : Base) is
      begin
          Report.Failed ("Should not call this routine (BE)");
      end Hide;

      procedure Hide (Value : Month; Which : Base) is
      begin
          case Which is
             when Bin =>
                Check (Value, Jan, A_Month, Apr, Limb, "B7");
             when Oct =>
                Check (Value, Oct, Mar, A_Month, Limb, "B8");
             when Dec =>
                Check (Value, Jan, Oct, Dec, Limb, "B9");
             when Hex =>
                Check (Value, Jan, Oct, Dec, Limb, "BA");
          end case;
      end Hide;

   begin
      case Limb is
         when First =>
            if not (Selector < 1) then
               Report.Failed ("Selector chooses wrong limb (B1)");
            end if;
         when Second =>
            if not (Selector in 1..8) then
               Report.Failed ("Selector chooses wrong limb (B2)");
            end if;
         when Third =>
            if not (Selector > 8) then
               Report.Failed ("Selector chooses wrong limb (B3)");
            end if;
      end case;
      -- No overloaded dependent expressions:
      Check ((if Selector < 1 then A_Month
              elsif Selector in 1..8 then Jul
              else Aug),
             A_Month, Jul, Aug, Limb, "B1");
      Check ((case Selector is
                 when 0      => A_Month,
                 when 1..8   => Aug,
                 when others => Nov),
             A_Month, Aug, Nov, Limb, "B2");
      -- One overloaded dependent expression:
      Check ((if Selector < 1 then Jan
              elsif Selector in 1..8 then A_Month
              else Apr),
             Jan, A_Month, Apr, Limb, "B3");
      Check ((case Selector is
                 when 0      => Oct,
                 when 1..8   => Mar,
                 when others => A_Month),
             Oct, Mar, A_Month, Limb, "B4");
      -- All overloaded dependent expressions:
      Check ((if Selector < 1 then Jan
              elsif Selector in 1..8 then Oct
              else Dec),
             Jan, Oct, Dec, Limb, "B5");
      Check ((case Selector is
                 when 0      => Jan,
                 when 1..8   => Oct,
                 when others => Dec),
             Jan, Oct, Dec, Limb, "B6");
      -- One overloaded dependent expression, context provides no help:
      Hide ((if Selector < 1 then Jan
             elsif Selector in 1..8 then A_Month
             else Apr),
             Which => Bin);
      Hide ((case Selector is
                 when 0      => Oct,
                 when 1..8   => Mar,
                 when others => A_Month),
             Which => Oct);

      -- All overloaded dependent expressions, context provides no help:
      Hide ((if Selector < 1 then Jan
            elsif Selector in 1..8 then Oct
            else Dec), Which => Dec);
      Hide ((case Selector is
                 when 0      => Jan,
                 when 1..8   => Oct,
                 when others => Dec),
             Which => Hex);
   end SubTestB;

   procedure SubtestC (Selector : Natural; Limb : TC_Limb) is
      -- As the selector expression of a case statement (hardest: the context
      -- provides nothing, only the relationship between the dependent
      -- expressions gives an answer)

      function Convert (Value : Base) return Base is
      begin
         return Value;
      end Convert;

      function Convert (Value : Month) return Base is
      begin
         case Value is
            when Jan => return Bin;
            when Oct => return Oct;
            when Dec => return Dec;
            when others => return Hex;
         end case;
      end Convert;

      function Convert (Value : Employee) return Base is
      begin
         case Value is
            when Jan => return Bin;
            when Tim => return Oct;
            when Tom => return Dec;
            when others => return Hex;
         end case;
      end Convert;

   begin
      -- One overloaded dependent expression:
      case Convert (if Selector < 1 then Jan
                    elsif Selector in 1..8 then Tim
                    else Tom) is
         when Bin => Check (Jan, Jan, Tim, Tom, Limb, "C1");
         when Oct => Check (Tim, Jan, Tim, Tom, Limb, "C1");
         when Dec => Check (Tom, Jan, Tim, Tom, Limb, "C1");
         when Hex => Report.Failed ("Unusual result (C1)");
      end case;

      case Convert (case Selector is
                      when 0      => Jan,
                      when 1..8   => Tim,
                      when others => Tom) is
         when Bin => Check (Jan, Jan, Tim, Tom, Limb, "C2");
         when Oct => Check (Tim, Jan, Tim, Tom, Limb, "C2");
         when Dec => Check (Tom, Jan, Tim, Tom, Limb, "C2");
         when Hex => Report.Failed ("Unusual result (C2)");
      end case;

      -- All overloaded dependent expressions:
      case Convert (if Selector < 1 then Jan
                    elsif Selector in 1..8 then Oct
                    else Dec) is
         when Bin => Check (Jan, Jan, Oct, Dec, Limb, "C3");
         when Oct => Check (Oct, Jan, Oct, Dec, Limb, "C3");
         when Dec => Check (Dec, Jan, Oct, Dec, Limb, "C3");
         when Hex => Report.Failed ("Unusual result (C3)");
      end case;

      case Convert (case Selector is
                      when 0      => Jan,
                      when 1..8   => Oct,
                      when others => Dec) is
         when Bin => Check (Jan, Jan, Oct, Dec, Limb, "C4");
         when Oct => Check (Oct, Jan, Oct, Dec, Limb, "C4");
         when Dec => Check (Dec, Jan, Oct, Dec, Limb, "C4");
         when Hex => Report.Failed ("Unusual result (C4)");
      end case;

      -- Direct use of the conditional expression, all overloaded.
      case (if Selector < 1 then Jan
            elsif Selector in 1..8 then Oct
            else Dec) is
         when Jan => Check (Jan, Jan, Oct, Dec, Limb, "C5");
         when Oct => Check (Oct, Jan, Oct, Dec, Limb, "C5");
         when Dec => Check (Dec, Jan, Oct, Dec, Limb, "C5");
         when others => Report.Failed ("Unusual result (C5)");
      end case;

      case (case Selector is
               when 0      => Jan,
               when 1..8   => Oct,
               when others => Dec) is
         when Jan => Check (Jan, Jan, Oct, Dec, Limb, "C6");
         when Oct => Check (Oct, Jan, Oct, Dec, Limb, "C6");
         when Dec => Check (Dec, Jan, Oct, Dec, Limb, "C6");
         when others => Report.Failed ("Unusual result (C6)");
      end case;
   end SubTestC;

begin

   Report.Test ("C457006",
                "Check that overloaded functions can be resolved when they " &
                "appear as dependent expressions in a conditional " &
                "expression. Part 1: Enumeration literals");

   SubTestA (Report.Ident_Int(0), Limb => First);

   SubTestA (Report.Ident_Int(5), Limb => Second);

   SubTestA (Report.Ident_Int(9), Limb => Third);

   SubTestB (Report.Ident_Int(0), Limb => First);

   SubTestB (Report.Ident_Int(4), Limb => Second);

   SubTestB (Report.Ident_Int(9), Limb => Third);

   SubTestC (Report.Ident_Int(0), Limb => First);

   SubTestC (Report.Ident_Int(3), Limb => Second);

   SubTestC (Report.Ident_Int(9), Limb => Third);

   Report.Result;

end C457006;

