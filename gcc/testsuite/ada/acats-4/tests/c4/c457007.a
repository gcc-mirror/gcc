-- C457007.A
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
--     Check that literals can be resolved when they appear as
--     dependent expressions in a conditional expression.
--
-- TEST DESCRIPTION:
--     We test each kind of literal (integer, real, character, string, null)
--     in each kind of conditional expression.
--
--     Obviously, the use of literals is very common in Ada programs.
--     Similarly, the use of conditional expressions is common enough that we
--     expect pretty much anything that can be written to be written
--     eventually. As such, we think that pretty much anything meets the intent
--     of usage-oriented testing for the combination of these features. Thus,
--     we created a rather simple test that just tries to test the objective
--     rather than creating a fancy usage scenario.
--
-- CHANGE HISTORY:
--     04 Dec 15   RLB     Created test.
--!

with Report;
procedure C457007 is

   type Acc_Int is access all Integer;
   A_I1,  A_I2 : aliased Integer;

   type TC_Limb is (First, Second, Third);

   procedure Check (Value, First_Expected,
                    Second_Expected, Third_Expected : Natural;
                    TC_This_Limb : TC_Limb; TC_Subtest : String) is
   begin
      case TC_This_Limb is
         when First =>
            if Value /= First_Expected then
               Report.Failed ("Value of " & Natural'Image(Value) &
                              " not expected value of " &
                              Natural'Image(First_Expected) &
                              " (" & TC_Subtest & "1)");
            end if;
         when Second =>
            if Value /= Second_Expected then
               Report.Failed ("Value of " & Natural'Image(Value) &
                              " not expected value of " &
                              Natural'Image(Second_Expected) &
                              " (" & TC_Subtest & "2)");
            end if;
         when Third =>
            if Value /= Third_Expected then
               Report.Failed ("Value of " & Natural'Image(Value) &
                              " not expected value of " &
                              Natural'Image(Third_Expected) &
                              " (" & TC_Subtest & "3)");
            end if;
      end case;
   end Check;

   procedure Check (Value, First_Expected,
                    Second_Expected, Third_Expected : Float;
                    TC_This_Limb : TC_Limb; TC_Subtest : String) is
   begin
      case TC_This_Limb is
         when First =>
            if Value /= First_Expected then
               Report.Failed ("Value of " & Float'Image(Value) &
                              " not expected value of " &
                              Float'Image(First_Expected) &
                              " (" & TC_Subtest & "1)");
            end if;
         when Second =>
            if Value /= Second_Expected then
               Report.Failed ("Value of " & Float'Image(Value) &
                              " not expected value of " &
                              Float'Image(Second_Expected) &
                              " (" & TC_Subtest & "2)");
            end if;
         when Third =>
            if Value /= Third_Expected then
               Report.Failed ("Value of " & Float'Image(Value) &
                              " not expected value of " &
                              Float'Image(Third_Expected) &
                              " (" & TC_Subtest & "3)");
            end if;
      end case;
   end Check;

   procedure Check (Value, First_Expected,
                    Second_Expected, Third_Expected : Character;
                    TC_This_Limb : TC_Limb; TC_Subtest : String) is
   begin
      case TC_This_Limb is
         when First =>
            if Value /= First_Expected then
               Report.Failed ("Value of " & Character'Image(Value) &
                              " not expected value of " &
                              Character'Image(First_Expected) &
                              " (" & TC_Subtest & "1)");
            end if;
         when Second =>
            if Value /= Second_Expected then
               Report.Failed ("Value of " & Character'Image(Value) &
                              " not expected value of " &
                              Character'Image(Second_Expected) &
                              " (" & TC_Subtest & "2)");
            end if;
         when Third =>
            if Value /= Third_Expected then
               Report.Failed ("Value of " & Character'Image(Value) &
                              " not expected value of " &
                              Character'Image(Third_Expected) &
                              " (" & TC_Subtest & "3)");
            end if;
      end case;
   end Check;

   procedure Check (Value, First_Expected,
                    Second_Expected, Third_Expected : String;
                    TC_This_Limb : TC_Limb; TC_Subtest : String) is
   begin
      case TC_This_Limb is
         when First =>
            if Value /= First_Expected then
               Report.Failed ("Value of " & Value &
                              " not expected value of " &
                              First_Expected &
                              " (" & TC_Subtest & "1)");
            end if;
         when Second =>
            if Value /= Second_Expected then
               Report.Failed ("Value of " & Value &
                              " not expected value of " &
                              Second_Expected &
                              " (" & TC_Subtest & "2)");
            end if;
         when Third =>
            if Value /= Third_Expected then
               Report.Failed ("Value of " & Value &
                              " not expected value of " &
                              Third_Expected &
                              " (" & TC_Subtest & "3)");
            end if;
      end case;
   end Check;

   procedure Check (Value, First_Expected,
                    Second_Expected, Third_Expected : Acc_Int;
                    TC_This_Limb : TC_Limb; TC_Subtest : String) is
   begin
      case TC_This_Limb is
         when First =>
            if Value /= First_Expected then
               Report.Failed ("Value not expected value (" &
                              TC_Subtest & "1)");
            end if;
         when Second =>
            if Value /= Second_Expected then
               Report.Failed ("Value not expected value (" &
                              TC_Subtest & "2)");
            end if;
         when Third =>
            if Value /= Third_Expected then
               Report.Failed ("Value not expected value (" &
                              TC_Subtest & "3)");
            end if;
      end case;
   end Check;

   procedure SubtestA (Selector : Natural; Limb : TC_Limb) is
      -- As a qualified expression (easy: the context provides a single type).
      A_Natural : Natural := Report.Ident_Int(4);
      A_Float : Float := Float(A_Natural);
      A_Character : Character := Character'Val(Character'Pos('B') + A_Natural);
      A_String : constant String := "Dec";
      A_Acc_Int : Acc_Int := A_I1'Access;
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

      -- Some non-literals:
      Check (Natural'(if Selector < 1 then A_Natural
                      elsif Selector in 1..8 then 12
                      else 52),
             A_Natural, 12, 52, Limb, "A1");
      Check (Natural'(case Selector is
                       when 0      => 66,
                       when 1..8   => A_Natural,
                       when others => 92),
             66, A_Natural, 92, Limb, "A2");

      Check (Float'(if Selector < 1 then A_Float
                    elsif Selector in 1..8 then 3.25
                    else 1.5),
             A_Float, 3.25, 1.5, Limb, "A3");
      Check (Float'(case Selector is
                       when 0      => 0.25,
                       when 1..8   => A_Float,
                       when others => 9.75),
             0.25, A_Float, 9.75, Limb, "A4");

      Check (Character'(if Selector < 1 then A_Character
                        elsif Selector in 1..8 then 'R'
                        else 'B'),
             A_Character, 'R', 'B', Limb, "A5");
      Check (Character'(case Selector is
                       when 0      => 'J',
                       when 1..8   => 'P',
                       when others => A_Character),
             'J', 'P', A_Character, Limb, "A6");

      Check (String'(if Selector < 1 then A_String
                        elsif Selector in 1..8 then "Oct"
                        else "Jan"),
             A_String, "Oct", "Jan", Limb, "A7");
      Check (String'(case Selector is
                       when 0      => "Jenny",
                       when 1..8   => "Becky",
                       when others => A_String),
             "Jenny", "Becky", A_String, Limb, "A8");

      Check (Acc_Int'(if Selector < 1 then A_Acc_Int
                        elsif Selector in 1..8 then null
                        else A_I2'Access),
             A_Acc_Int, null, A_I2'Access, Limb, "A9");
      Check (Acc_Int'(case Selector is
                       when 0      => null,
                       when 1..8   => A_I2'Access,
                       when others => A_Acc_Int),
             null, A_I2'Access, A_Acc_Int, Limb, "AA");

      -- All literals:
      Check (Natural'(if Selector < 1 then 82
                      elsif Selector in 1..8 then 12
                      else 52),
             82, 12, 52, Limb, "AB");
      Check (Natural'(case Selector is
                       when 0      => 15,
                       when 1..8   => 66,
                       when others => 92),
             15, 66, 92, Limb, "AC");

      Check (Float'(if Selector < 1 then 2.0
                    elsif Selector in 1..8 then 4.25
                    else 6.5),
             2.0, 4.25, 6.5, Limb, "AD");
      Check (Float'(case Selector is
                       when 0      => 7.25,
                       when 1..8   => 8.50,
                       when others => 9.75),
             7.25, 8.50, 9.75, Limb, "AE");

      Check (Character'(if Selector < 1 then 'R'
                        elsif Selector in 1..8 then 'L'
                        else 'B'),
             'R', 'L', 'B', Limb, "AF");
      Check (Character'(case Selector is
                       when 0      => 'K',
                       when 1..8   => 'B',
                       when others => 'D'),
             'K', 'B', 'D', Limb, "AG");

      Check (String'(if Selector < 1 then "Oct"
                        elsif Selector in 1..8 then "Nov"
                        else "Dec"),
             "Oct", "Nov", "Dec", Limb, "AH");
      Check (String'(case Selector is
                       when 0      => "Aaron",
                       when 1..8   => "Eddie",
                       when others => "Don"),
             "Aaron", "Eddie", "Don", Limb, "AJ");

      -- We don't try the access test here as it would require all of the
      -- dependent_expressions to be the same (null), which is definitely not
      -- going to happen in real code.

   end SubTestA;

   procedure SubtestB (Selector : Natural; Limb : TC_Limb) is
      -- As the parameter of an overloaded routine (harder: the context
      -- provides several separate types, only one of which works)
      A_Natural : Natural := Report.Ident_Int(2);
      A_Float : Float := Float(A_Natural);
      A_Character : Character := Character'Val(Character'Pos('A') + A_Natural);
      A_String : constant String := "Aug";
      A_Acc_Int : Acc_Int := A_I1'Access;
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

      Check ((if Selector < 1 then A_Natural
              elsif Selector in 1..8 then 12
              else 52),
             A_Natural, 12, 52, Limb, "A1");
      Check ((case Selector is
                when 0      => 66,
                when 1..8   => A_Natural,
                when others => 92),
             66, A_Natural, 92, Limb, "A2");

      Check ((if Selector < 1 then A_Float
              elsif Selector in 1..8 then 3.25
              else 1.5),
             A_Float, 3.25, 1.5, Limb, "A3");
      Check ((case Selector is
                 when 0      => 0.25,
                 when 1..8   => A_Float,
                 when others => 9.75),
             0.25, A_Float, 9.75, Limb, "A4");

      Check ((if Selector < 1 then A_Character
              elsif Selector in 1..8 then 'R'
              else 'B'),
             A_Character, 'R', 'B', Limb, "A5");
      Check ((case Selector is
                 when 0      => 'J',
                 when 1..8   => 'P',
                 when others => A_Character),
             'J', 'P', A_Character, Limb, "A6");

      Check ((if Selector < 1 then A_String
              elsif Selector in 1..8 then "Oct"
              else "Jan"),
             A_String, "Oct", "Jan", Limb, "A7");
      Check ((case Selector is
                 when 0      => "Jenny",
                 when 1..8   => "Becky",
                 when others => A_String),
             "Jenny", "Becky", A_String, Limb, "A8");

      Check ((if Selector < 1 then A_Acc_Int
              elsif Selector in 1..8 then null
              else A_I2'Access),
             A_Acc_Int, null, A_I2'Access, Limb, "A9");
      Check ((case Selector is
                 when 0      => null,
                 when 1..8   => A_I2'Access,
                 when others => A_Acc_Int),
             null, A_I2'Access, A_Acc_Int, Limb, "AA");

   end SubTestB;

begin

   Report.Test ("C457007",
                "Check that literals can be resolved when they " &
                "appear as dependent expressions in a conditional " &
                "expression");

   SubTestA (Report.Ident_Int(0), Limb => First);

   SubTestA (Report.Ident_Int(5), Limb => Second);

   SubTestA (Report.Ident_Int(9), Limb => Third);

   SubTestB (Report.Ident_Int(0), Limb => First);

   SubTestB (Report.Ident_Int(4), Limb => Second);

   SubTestB (Report.Ident_Int(9), Limb => Third);

   Report.Result;

end C457007;

