-- CXA5011.A
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
--      Check that, for both Float_Random and Discrete_Random packages,
--      the following are true:
--      1) two objects of type Generator are initialized to the same state.
--      2) when the Function Reset is used to reset two generators
--         to different time-dependent states, the resulting random values
--         from each generator are different.
--      3) when the Function Reset uses the same integer initiator
--         to reset two generators to the same state, the resulting random
--         values from each generator are identical.
--      4) when the Function Reset uses different integer initiator
--         values to reset two generators, the resulting random numbers are
--         different.
--
-- TEST DESCRIPTION:
--      This test evaluates components of the Ada.Numerics.Float_Random and
--      Ada.Numerics.Discrete_Random packages.
--      This test checks to see that objects of type Generator are initialized
--      to the same state. In addition, the functionality of Function Reset is
--      validated.
--      For each of the objectives above, evaluation of the various generators
--      is performed using each of the following techniques. When the states of
--      two generators are to be compared, each state is saved, then
--      transformed to a bounded-string variable.  The bounded-strings can
--      then be compared for equality.  In this case, matching bounded-strings
--      are evidence that the states of two generators are the same.
--      In addition, two generators are compared by evaluating a series of
--      random numbers they produce.  A matching series of random numbers
--      implies that the generators were in the same state prior to producing
--      the numbers.
--
--
-- CHANGE HISTORY:
--      20 Apr 95   SAIC    Initial prerelease version.
--      07 Jul 95   SAIC    Incorporated reviewer comments/suggestions.
--      22 Apr 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      17 Aug 96   SAIC    Deleted Subtest #2.
--      09 Feb 01   RLB     Repaired to work on implementations with a 16-bit
--                          Integer.

--!

with Ada.Exceptions;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Bounded;
with ImpDef;
with Report;

procedure CXA5011 is
begin

   Report.Test ("CXA5011", "Check the effect of Function Reset on the " &
                           "state of random number generators");

   Test_Block:
   declare

      use Ada.Exceptions;
      use Ada.Numerics;
      use Ada.Strings.Bounded;

      -- Declare an modular subtype, and use it to instantiate the discrete
      -- random number generator generic package.

      type    Discrete_Range   is mod 2**(Integer'Size-1);
      package Discrete_Package is new Discrete_Random(Discrete_Range);

      -- Declaration of random number generator objects.

      Discrete_Generator_1,
      Discrete_Generator_2  : Discrete_Package.Generator;
      Float_Generator_1,
      Float_Generator_2     : Float_Random.Generator;

      -- Declaration of bounded string packages instantiated with the
      -- value of Max_Image_Width constant from each random number generator
      -- package, and bounded string variables used to hold the image of
      -- random number generator states.

      package Discrete_String_Pack is
        new Generic_Bounded_Length(Discrete_Package.Max_Image_Width);

      package Float_String_Pack is
        new Generic_Bounded_Length(Float_Random.Max_Image_Width);

      use Discrete_String_Pack, Float_String_Pack;

      TC_Seed                  : Integer;
      TC_Max_Loop_Count        : constant Natural := 1000;
      Allowed_Matches          : constant Natural := 2;
      --
      -- In a sequence of TC_Max_Loop_Count random numbers that should
      -- not match, some may match by chance.  Up to Allowed_Matches
      -- numbers may match before the test is considered to fail.
      --


      procedure Check_Float_State (Gen_1, Gen_2 : Float_Random.Generator;
                                   Sub_Test     : Integer;
                                   States_Should_Match : Boolean) is

         use type Float_Random.State;

         State_1,
         State_2         : Float_Random.State;

         State_String_1,
         State_String_2  : Float_String_Pack.Bounded_String :=
                             Float_String_Pack.Null_Bounded_String;
      begin

         Float_Random.Save(Gen => Gen_1, To_State => State_1);
         Float_Random.Save(Gen_2, State_2);

         State_String_1 :=
           Float_String_Pack.To_Bounded_String(Source =>
             Float_Random.Image(Of_State => State_1));

         State_String_2 :=
           Float_String_Pack.To_Bounded_String(Float_Random.Image(State_2));

         case States_Should_Match is
            when True  =>
               if State_1 /= State_2 then
                  Report.Failed("Subtest #" & Integer'Image(Sub_Test)    &
                                "   State values from Float generators " &
                                "are not the same");
               end if;
               if State_String_1 /= State_String_2 then
                  Report.Failed("Subtest #" & Integer'Image(Sub_Test)     &
                                "   State strings from Float generators " &
                                "are not the same");
               end if;
            when False =>
               if State_1 = State_2 then
                  Report.Failed("Subtest #" & Integer'Image(Sub_Test)    &
                                "   State values from Float generators " &
                                "are the same");
               end if;
               if State_String_1 = State_String_2 then
                  Report.Failed("Subtest #" & Integer'Image(Sub_Test)     &
                                "   State strings from Float generators " &
                                "are the same");
               end if;
         end case;
      end Check_Float_State;



      procedure Check_Discrete_State (Gen_1,
                                      Gen_2    : Discrete_Package.Generator;
                                      Sub_Test : Integer;
                                      States_Should_Match : Boolean) is

         use type Discrete_Package.State;

         State_1, State_2  : Discrete_Package.State;

         State_String_1,
         State_String_2    : Discrete_String_Pack.Bounded_String :=
                               Discrete_String_Pack.Null_Bounded_String;
      begin

         Discrete_Package.Save(Gen      => Gen_1,
                               To_State => State_1);
         Discrete_Package.Save(Gen_2, To_State => State_2);

         State_String_1 :=
           Discrete_String_Pack.To_Bounded_String(Source =>
             Discrete_Package.Image(Of_State => State_1));

         State_String_2 :=
           Discrete_String_Pack.To_Bounded_String(Source =>
             Discrete_Package.Image(Of_State => State_2));

         case States_Should_Match is
            when True  =>
               if State_1 /= State_2 then
                  Report.Failed("Subtest #" & Integer'Image(Sub_Test) &
                                "   State values from Discrete "      &
                                "generators are not the same");
               end if;
               if State_String_1 /= State_String_2 then
                  Report.Failed("Subtest #" & Integer'Image(Sub_Test) &
                                "   State strings from Discrete "     &
                                "generators are not the same");
               end if;
            when False =>
               if State_1 = State_2 then
                  Report.Failed("Subtest #" & Integer'Image(Sub_Test) &
                                "   State values from Discrete "      &
                                "generators are the same");
               end if;
               if State_String_1 = State_String_2 then
                  Report.Failed("Subtest #" & Integer'Image(Sub_Test) &
                                "   State strings from Discrete "     &
                                "generators are the same");
               end if;
         end case;
      end Check_Discrete_State;



      procedure Check_Float_Values (Gen_1, Gen_2 : Float_Random.Generator;
                                    Sub_Test     : Integer;
                                    Values_Should_Match : Boolean) is
         Matches         : Natural := 0;
         Check_Failed    : Boolean := False;
      begin
         case Values_Should_Match is
            when True  =>
               for i in 1..TC_Max_Loop_Count loop
                  if Float_Random.Random(Gen_1) /= Float_Random.Random(Gen_2)
                  then
                     Check_Failed := True;
                     exit;
                  end if;
               end loop;
               if Check_Failed then
                  Report.Failed("Sub_Test # " & Integer'Image(Sub_Test)    &
                                "   Random numbers from Float generators " &
                                "Failed check");
               end if;
            when False =>
               for i in 1..TC_Max_Loop_Count loop
                  if Float_Random.Random(Gen_1) = Float_Random.Random(Gen_2)
                  then
                     Matches := Matches + 1;
                  end if;
               end loop;
         end case;

         if (Values_Should_Match and Check_Failed) or
            (not Values_Should_Match and Matches > Allowed_Matches)
         then
            Report.Failed("Sub_Test # " & Integer'Image(Sub_Test)    &
                          "   Random numbers from Float generators " &
                          "Failed check");
         end if;

      end Check_Float_Values;



      procedure Check_Discrete_Values (Gen_1,
                                       Gen_2    : Discrete_Package.Generator;
                                       Sub_Test : Integer;
                                       Values_Should_Match : Boolean) is
         Matches         : Natural := 0;
         Check_Failed    : Boolean := False;
      begin
         case Values_Should_Match is
            when True  =>
               for i in 1..TC_Max_Loop_Count loop
                  if Discrete_Package.Random(Gen_1) /=
                     Discrete_Package.Random(Gen_2)
                  then
                     Check_Failed := True;
                     exit;
                  end if;
               end loop;
            when False =>
               for i in 1..TC_Max_Loop_Count loop
                  if Discrete_Package.Random(Gen_1) =
                     Discrete_Package.Random(Gen_2)
                  then
                     Matches := Matches + 1;
                  end if;
               end loop;
         end case;

         if (Values_Should_Match and Check_Failed) or
            (not Values_Should_Match and Matches > Allowed_Matches)
         then
            Report.Failed("Sub_Test # " & Integer'Image(Sub_Test)    &
                          "   Random numbers from Discrete generators " &
                          "Failed check");
         end if;

      end Check_Discrete_Values;



   begin

      Sub_Test_1:
         -- Check that two objects of type Generator are initialized to the
         -- same state.
      begin

         -- Since the discrete and float random generators are in the initial
         -- state, using Procedure Save to save the states of the generator
         -- objects, and transforming these states into strings using
         -- Function Image, should yield identical strings.

         Check_Discrete_State (Discrete_Generator_1,
                               Discrete_Generator_2,
                               Sub_Test => 1,
                               States_Should_Match => True);

         Check_Float_State (Float_Generator_1,
                            Float_Generator_2,
                            Sub_Test => 1,
                            States_Should_Match => True);

         -- Since the two random generator objects are in their initial
         -- state, the values produced from each (upon calls to Random)
         -- should be identical.

         Check_Discrete_Values (Discrete_Generator_1,
                                Discrete_Generator_2,
                                Sub_Test => 1,
                                Values_Should_Match => True);

         Check_Float_Values (Float_Generator_1,
                             Float_Generator_2,
                             Sub_Test => 1,
                             Values_Should_Match => True);

      end Sub_Test_1;



      Sub_Test_3:
         -- Check that when the Function Reset uses the same integer
         -- initiator to reset two generators to the same state, the
         -- resulting random values and the state from each generator
         -- are identical.
      declare
         use Discrete_Package, Float_Random;
      begin

         -- Reset the generators to the same states, using the version of
         -- Function Reset with both generator parameter and initiator
         -- specified.

         TC_Seed := Integer(Random(Discrete_Generator_1));
         Reset(Gen => Discrete_Generator_1, Initiator => TC_Seed);
         Reset(Discrete_Generator_2, Initiator => TC_Seed);
         Reset(Float_Generator_1, TC_Seed);
         Reset(Float_Generator_2, TC_Seed);

         -- Since the random generators have been reset to identical states,
         -- bounded string images of these states should yield identical
         -- strings.

         Check_Discrete_State (Discrete_Generator_1,
                               Discrete_Generator_2,
                               Sub_Test => 3,
                               States_Should_Match => True);

         Check_Float_State (Float_Generator_1,
                            Float_Generator_2,
                            Sub_Test => 3,
                            States_Should_Match => True);

         -- Since the random generators have been reset to identical states,
         -- the values produced from each (upon calls to Random) should
         -- be identical.

         Check_Discrete_Values (Discrete_Generator_1,
                                Discrete_Generator_2,
                                Sub_Test => 3,
                                Values_Should_Match => True);

         Check_Float_Values (Float_Generator_1,
                             Float_Generator_2,
                             Sub_Test => 3,
                             Values_Should_Match => True);

      end Sub_Test_3;



      Sub_Test_4:
         -- Check that when the Function Reset uses different integer
         -- initiator values to reset two generators, the resulting random
         -- numbers and states are different.
      begin

         -- Reset the generators to different states.

         TC_Seed :=
           Integer(Discrete_Package.Random(Discrete_Generator_1));

         Discrete_Package.Reset(Gen       => Discrete_Generator_1,
                                Initiator => TC_Seed);

         -- Set the seed value to a different value for the second call
         -- to Reset.
         -- Note: A second call to Random could be made, as above, but that
         --       would not ensure that the resulting seed value was
         --       different from the first.

         if TC_Seed /= Integer'Last then
            TC_Seed := TC_Seed + 1;
         else
            TC_Seed := TC_Seed - 1;
         end if;

         Discrete_Package.Reset(Gen       => Discrete_Generator_2,
                                Initiator => TC_Seed);

         Float_Random.Reset(Float_Generator_1, 16#FF#);             -- 255
         Float_Random.Reset(Float_Generator_2, 2#1110_0000#);       -- 224

         -- Since the two float random generators are in different
         -- states, the bounded string images depicting their states should
         -- differ.

         Check_Discrete_State (Discrete_Generator_1,
                               Discrete_Generator_2,
                               Sub_Test => 4,
                               States_Should_Match => False);

         Check_Float_State (Float_Generator_1,
                            Float_Generator_2,
                            Sub_Test => 4,
                            States_Should_Match => False);

         -- Since the two discrete random generator objects were reset
         -- to different states, the values produced from each (upon calls
         -- to Random) should differ.

         Check_Discrete_Values (Discrete_Generator_1,
                                Discrete_Generator_2,
                                Sub_Test => 4,
                                Values_Should_Match => False);

         Check_Float_Values (Float_Generator_1,
                             Float_Generator_2,
                             Sub_Test => 4,
                             Values_Should_Match => False);

      end Sub_Test_4;

   exception
      when The_Error : others =>
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXA5011;
