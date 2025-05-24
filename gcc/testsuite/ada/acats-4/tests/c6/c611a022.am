-- C611A022.AM
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
--     See C611A020.A.
--
-- TEST DESCRIPTION:
--     See C611A020.A.
--
-- TEST FILES:
--     This test consists of the following files:
--        C611A020.A
--        C611A021.A
--     -> C611A022.AM
--
-- CHANGE HISTORY:
--     22 Mar 16   JAC    Initial pre-release version.
--     24 Mar 16   RLB    Renamed so all test parts have same first 7
--                        characters.
--     30 Mar 16   RLB    Split all of the specific type objectives into
--                        a single test. Added a number of test cases,
--                        to ensure that statically bound calls also
--                        evaluate their checks in the correct order.
--     31 Mar 16   RLB    Removed duplicative suffixes.
--     25 Jan 17   RLB    Corrected the name of the main subprogram.
--
--!
with Ada.Assertions;
with Ada.Tags;
with F611A00;
with C611A020;
with C611A020.Child;
with Report;

procedure C611A022 is

   pragma Assertion_Policy (Check);

   Assertion_Error_Raised : Boolean;
   Order_Error_Raised : Boolean;

   Outer_Distance : Float;

   My_Object : C611A020.Object;

   My_Triangle : C611A020.Child.Triangle :=
                  C611A020.Child.Make_Triangle
                   (A => 3.0, B => 4.0, C => 5.0);

   procedure Test_Get_Distance (O    : in C611A020.Object'Class;
                                Code : in Character) is

      use type Ada.Tags.Tag;

      function TC_Eval_Object (O    : in C611A020.Object'Class)
         return C611A020.Object'Class is
         -- Called when the parameter of Distance is evaluated. Better
         -- be before the precondition is evaluated.
      begin
         if F611A00.TC_Object_Distance_Specific_Pre_Called or
            F611A00.TC_Triangle_Distance_Specific_Pre_Called then
            Report.Failed
               ("Specific precondition evaluated before the parameter was " &
                "evaluated - " & Code);
            F611A00.TC_Output;
         end if;
         return O;
      end TC_Eval_Object;


      function Call_Get_Distance (O : in C611A020.Object'Class)
         return Float is
         Inner_Distance : Float;
      begin
         -- Dispatching call
         Inner_Distance := C611A020.Distance (TC_Eval_Object (O));

         -- Note that either specific postcondition could be called depending
         -- on which is dispatched to
         if not (F611A00.TC_Object_Distance_Specific_Post_Called or
                 F611A00.TC_Triangle_Distance_Specific_Post_Called) then
            Report.Failed
             ("Specific postcondition should have been called " &
              "before resuming after calling the subprogram body - " & Code);
            F611A00.TC_Output;
         end if;

         return Inner_Distance;
      end Call_Get_Distance;
   begin
      begin
         Assertion_Error_Raised := False;
         Order_Error_Raised := False;
         Outer_Distance := Call_Get_Distance (O);

         -- Check the following objective:
         --    For a dispatching call, check that the specific precondition and
         --    specific postcondition evaluated is that of the actual body
         --    invoked.
         if O'Tag = C611A020.Child.Triangle'Tag then
            if      F611A00.TC_Object_Distance_Specific_Pre_Called or
               (not F611A00.TC_Triangle_Distance_Specific_Pre_Called) then
               Report.Failed
                  ("Wrong specific precondition called - " & Code);
               F611A00.TC_Output;
            end if;

            if      F611A00.TC_Object_Distance_Specific_Post_Called or
               (not F611A00.TC_Triangle_Distance_Specific_Post_Called) then
               Report.Failed
                  ("Wrong specific postcondition called - " & Code);
               F611A00.TC_Output;
            end if;
         else
            if (not F611A00.TC_Object_Distance_Specific_Pre_Called) or
                    F611A00.TC_Triangle_Distance_Specific_Pre_Called then
               Report.Failed
                  ("Wrong specific precondition called - " & Code);
               F611A00.TC_Output;
            end if;

            if (not F611A00.TC_Object_Distance_Specific_Post_Called) or
                    F611A00.TC_Triangle_Distance_Specific_Post_Called then
               Report.Failed
                  ("Wrong specific postcondition called - " & Code);
               F611A00.TC_Output;
            end if;
         end if;
         if Outer_Distance < 0.0 then -- Provide a sink so the whole
                                      -- routine can't get optimized away:
            Report.Failed
               ("Result doesn't meet the postcondition - " & Code);
         end if;
      exception
         when Ada.Assertions.Assertion_Error =>
            Assertion_Error_Raised := True;
         when F611A00.TC_Order_Error =>
            Order_Error_Raised := True;
      end;
   end Test_Get_Distance;


   procedure Test_Get_Distance2 (O    : in C611A020.Child.Triangle;
                                 Code : in Character) is

      function TC_Eval_Object (O    : in C611A020.Child.Triangle)
         return C611A020.Child.Triangle is
         -- Called when the parameter of Distance is evaluated. Better
         -- be before the precondition is evaluated.
      begin
         if F611A00.TC_Object_Distance_Specific_Pre_Called or
            F611A00.TC_Triangle_Distance_Specific_Pre_Called then
            Report.Failed
               ("Specific precondition evaluated before the parameter was " &
                "evaluated for a statically bound call - " & Code);
            F611A00.TC_Output;
         end if;
         return O;
      end TC_Eval_Object;


      function Call_Get_Distance (O : in C611A020.Child.Triangle)
         return Float is
         Inner_Distance : Float;
      begin
         -- Statically bound call
         Inner_Distance := TC_Eval_Object (O).Distance;

         if      F611A00.TC_Object_Distance_Specific_Post_Called or
            (not F611A00.TC_Triangle_Distance_Specific_Post_Called) then
            Report.Failed
             ("Specific postcondition should have been called " &
              "before resuming after calling the subprogram body - " & Code);
            F611A00.TC_Output;
         end if;

         return Inner_Distance;
      end Call_Get_Distance;
   begin
      begin
         Assertion_Error_Raised := False;
         Order_Error_Raised := False;
         Outer_Distance := Call_Get_Distance (O);

         if      F611A00.TC_Object_Distance_Specific_Pre_Called or
            (not F611A00.TC_Triangle_Distance_Specific_Pre_Called) then
            Report.Failed
               ("Wrong specific precondition called - " & Code);
            F611A00.TC_Output;
         end if;

         if      F611A00.TC_Object_Distance_Specific_Post_Called or
            (not F611A00.TC_Triangle_Distance_Specific_Post_Called) then
            Report.Failed
               ("Wrong specific postcondition called - " & Code);
            F611A00.TC_Output;
         end if;

         if Outer_Distance < 0.0 then -- Provide a sink so the whole
                                      -- routine can't get optimized away:
            Report.Failed
               ("Result doesn't meet the postcondition - " & Code);
         end if;
      exception
         when Ada.Assertions.Assertion_Error =>
            Assertion_Error_Raised := True;
         when F611A00.TC_Order_Error =>
            Order_Error_Raised := True;
      end;
   end Test_Get_Distance2;

begin

   Report.Test
     ("C611A02",
      "Check that the correct expressions are evaluated for specific " &
      "precondition and postconditions, and that they are evaluated " &
      "at the correct point");

   -- We start by testing dispatching calls:
   --    For a dispatching call, check that the specific precondition and
   --    specific postcondition evaluated is that of the actual body
   --    invoked.
   -- and we also check the ordering tests for these.

   -- Should pass Triangle's specific precondition and postcondition.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 3.0, Y => 4.0);

   Test_Get_Distance (My_Triangle, Code => '1');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - 1");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 1");
   end if;

   -- Should pass Object's specific precondition and postcondition.

   F611A00.TC_Clear;

   My_Object.Move (X => 1.0, Y => 1.0);

   Test_Get_Distance (My_Object, Code => '2');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - 2");
   end if;

   -- Should fail Triangle's specific precondition.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 3.0, Y => 12.0);

   Test_Get_Distance (My_Triangle, Code => '3');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 3");
   end if;

   -- Should fail Object's specific precondition.

   F611A00.TC_Clear;

   My_Object.Move (X => 12.0, Y => 0.5);

   Test_Get_Distance (My_Object, Code => '4');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 4");
   end if;

   -- Should fail Triangle's specific postcondition.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 0.3, Y => 0.4);

   Test_Get_Distance (My_Triangle, Code => '5');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 5");
   end if;
   if Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 5");
   end if;

   -- Should fail Object's specific postcondition.

   F611A00.TC_Clear;

   My_Object.Move (X => 1.0, Y => -(0.25));

   Test_Get_Distance (My_Object, Code => '6');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 6");
   end if;
   if Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 6");
   end if;

   -- Check the ordering objectives for a statically bound call:

   -- Should pass Triangle's specific precondition and postcondition.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 5.0, Y => 4.0);

   Test_Get_Distance2 (My_Triangle, Code => '7');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - 7");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 7");
   end if;

   -- Should fail Triangle's specific precondition.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 3.0, Y => 16.0);

   Test_Get_Distance2 (My_Triangle, Code => '8');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 8");
   end if;

   -- Should fail Triangle's specific postcondition.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 0.3, Y => 0.4);

   Test_Get_Distance2 (My_Triangle, Code => '9');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 9");
   end if;
   if Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 9");
   end if;

   -- Lastly, check the following objectives:
   --    For a call on a subprogram S whose implementation is inherited
   --    from the primitive subprogram A of an ancestor, check that the
   --    specific precondition and specific postcondition that applies
   --    to A is checked for a call on S.

   declare
      My_X_Coord : Float;
      My_Y_Coord : Float;
   begin

      F611A00.TC_Clear;

      -- Statically bound call
      My_X_Coord := My_Triangle.X_Coord;

      if not F611A00.TC_Object_X_Coord_Specific_Pre_Called then
         Report.Failed
          ("Did not call the specific precondition of the" &
           " inherited subprogram - A");
         F611A00.TC_Output;
      end if;

      if not F611A00.TC_Object_X_Coord_Specific_Post_Called then
         Report.Failed
          ("Did not call the specific postcondition of the" &
           " inherited subprogram - A");
         F611A00.TC_Output;
      end if;

      F611A00.TC_Clear;

      -- Dispatching call
      My_Y_Coord := C611A020.Y_Coord (
         C611A020.Object'Class(My_Triangle));

      if not F611A00.TC_Object_Y_Coord_Specific_Pre_Called then
         Report.Failed
            ("Did not call the specific precondition of the" &
             " inherited subprogram - B");
         F611A00.TC_Output;
      end if;

      if not F611A00.TC_Object_Y_Coord_Specific_Post_Called then
         Report.Failed
           ("Did not call the specific postcondition of the" &
            " inherited subprogram - B");
         F611A00.TC_Output;
      end if;
   end;

   Report.Result;

end C611A022;
