-- C611A032.AM
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
--     See C611A030.A.
--
-- TEST DESCRIPTION:
--     See C611A030.A.
--
-- TEST FILES:
--     This test consists of the following files:
--        C611A030.A
--        C611A031.A
--     -> C611A032.AM
--
-- CHANGE HISTORY:
--      22 Mar 16   JAC     Initial pre-release version.
--      24 Mar 16   RLB     Renamed so all test parts have same first 7
--                          characters.
--      31 Mar 16   RLB     Split all of the class-wide type objectives into
--                          a single test. Added test cases for My_Object,
--                          put the static binding tests into a subprogram.
--      25 Jan 17   RLB     Corrected the name of the main subprogram.
--
--!
with Ada.Assertions;
with Ada.Tags;
with F611A00;
with C611A030;
with C611A030.Child;
with Report;

procedure C611A032 is

   pragma Assertion_Policy (Check);

   Assertion_Error_Raised : Boolean;
   Order_Error_Raised : Boolean;

   Outer_Distance : Float;

   My_Object : C611A030.Object;

   My_Triangle : C611A030.Child.Triangle :=
                  C611A030.Child.Make_Triangle
                   (A => 3.0, B => 4.0, C => 5.0);

   procedure Test_Get_Distance (O    : in C611A030.Object'Class;
                                Code : in Character) is
      -- Test a dispatching call of Distance.

      use type Ada.Tags.Tag;

      function TC_Eval_Object (O    : in C611A030.Object'Class)
         return C611A030.Object'Class is
         -- Called when the parameter of Distance is evaluated. Better
         -- be before the precondition is evaluated.
      begin
         -- Check the objective:
         --    Check that an enabled class-wide precondition of a subprogram S
         --    is evaluated after evaluating the parameters of a call on S.

         if F611A00.TC_Object_Distance_Pre_Class_Called or
            F611A00.TC_Triangle_Distance_Pre_Class_Called then
            Report.Failed
               ("Class-wide precondition evaluated before the parameter was " &
                "evaluated - " & Code);
            F611A00.TC_Output;
         end if;
         return O;
      end TC_Eval_Object;


      function Call_Get_Distance (O : in C611A030.Object'Class)
         return Float is
         Inner_Distance : Float;
      begin
         -- Dispatching call
         Inner_Distance := C611A030.Distance (TC_Eval_Object (O));

         -- Check the objective:
         --    Check that an enabled class-wide postcondition of a subprogram S
         --    is evaluated after completing the subprogram body but before
         --    continuing execution after the call of S.
         if not F611A00.TC_Object_Distance_Post_Class_Called then
            Report.Failed
             ("Post'Class should have been called before " &
              "resuming after calling the subprogram body - " & Code);
            F611A00.TC_Output;
         end if;

         return Inner_Distance;
      end Call_Get_Distance;
   begin
      declare
      begin
         Assertion_Error_Raised := False;
         Order_Error_Raised := False;
         Outer_Distance := Call_Get_Distance (O);

         -- The following checks assume that the precondition and postcondition
         -- of Distance both pass, but of course if either doesn't pass we
         -- don't get here as an exception is raised.

         if O'Tag = C611A030.Child.Triangle'Tag then
            -- Check the following objective:
            --    For a nonabstract tagged type T and a primitive subprogram S
            --    of T and that has a class-wide postcondition expression E,
            --    check that for a dynamically tagged dispatching call of S,
            --    calls to primitive operations of T within E invoke the
            --    bodies appropriate for the controlling tag, even if it is not
            --    T.
            if F611A00.TC_Object_Distance_Is_Positive_Called or
               F611A00.TC_Object_Not_Too_Far_Called or
               (not F611A00.TC_Triangle_Distance_Is_Positive_Called) or
               (not F611A00.TC_Triangle_Not_Too_Far_Called) then
               Report.Failed
                 ("Postcondition expressions should only have called " &
                  "primitives appropriate for the tag, and should have " &
                  "called all of them - " & Code);
               F611A00.TC_Output;
            end if;

            -- Check the following objective:
            --   For a dispatching call, check that the class-wide
            --   postcondition evaluated is that of the actual body invoked.
            if not (F611A00.TC_Triangle_Distance_Called and
                    F611A00.TC_Triangle_Distance_Post_Class_Called) then
               Report.Failed
                  ("Hasn't called the classwide postcondition of the " &
                   "body dispatched to - " & Code);
               F611A00.TC_Output;
            end if;

            -- Check the following objective:
            --    Check that if multiple enabled class-wide postconditions
            --    apply to a subprogram S, check that they are all evaluated
            --    if they all evaluate to True.
            if not (F611A00.TC_Object_Distance_Post_Class_Called and
                    F611A00.TC_Triangle_Distance_Post_Class_Called) then
               Report.Failed
                 ("Hasn't called all classwide postconditions - " & Code);
               F611A00.TC_Output;
            end if;

            -- Check the following objective:
            --    For a nonabstract tagged type T and a primitive subprogram S
            --    of T and that has a class-wide precondition expression E,
            --    check that for a dynamically tagged dispatching call of S,
            --    calls to primitive operations of T within E invoke the
            --    bodies appropriate for the controlling tag, even if it is not
            --    T.
            -- Note that we don't assume that both preconditions are evaluated
            -- (if either are True, the other doesn't have to be evaluated),
            -- but if a precondition is evaluated, make sure that the correct
            -- routines are called.
            if (F611A00.TC_Object_Distance_Pre_Class_Called and then
                  (F611A00.TC_Object_X_Coord_Called or
                   (not F611A00.TC_Triangle_X_Coord_Called))) or else
               (F611A00.TC_Triangle_Distance_Pre_Class_Called and then
                  (F611A00.TC_Object_Y_Coord_Called or
                   (not F611A00.TC_Triangle_Y_Coord_Called))) then
               Report.Failed
                 ("Precondition expressions should only have called " &
                  "primitives appropriate for the tag, and should have " &
                  "called all of them that were evaluated - " & Code);
               F611A00.TC_Output;
            end if;

         else -- O'Tag = C611A030.Object'Tag
            -- Check the following objective:
            --    For a nonabstract tagged type T and a primitive subprogram S
            --    of T and that has a class-wide postcondition expression E,
            --    check that for a dynamically tagged dispatching call of S,
            --    calls to primitive operations of T within E invoke the
            --    bodies appropriate for the controlling tag, even if it is not
            --    T. [In this case, it is T, but it's still worth testing.]
            if (not F611A00.TC_Object_Distance_Is_Positive_Called) or
                F611A00.TC_Object_Not_Too_Far_Called or
                F611A00.TC_Triangle_Distance_Is_Positive_Called or
                F611A00.TC_Triangle_Not_Too_Far_Called then
               Report.Failed
                 ("Postcondition expressions should only have called " &
                  "primitives appropriate for the tag, and should have " &
                  "called all of them - " & Code);
               F611A00.TC_Output;
            end if;

            -- Check the following objective:
            --   For a dispatching call, check that the class-wide
            --   postcondition evaluated is that of the actual body invoked.
            if not (F611A00.TC_Object_Distance_Called and
                    F611A00.TC_Object_Distance_Post_Class_Called) then
               Report.Failed
                  ("Hasn't called the classwide postcondition of the " &
                   "body dispatched to - " & Code);
               F611A00.TC_Output;
            end if;

            -- Check the following objective:
            --    For a nonabstract tagged type T and a primitive subprogram S
            --    of T and that has a class-wide precondition expression E,
            --    check that for a dynamically tagged dispatching call of S,
            --    calls to primitive operations of T within E invoke the
            --    bodies appropriate for the controlling tag, even if it is not
            --    T. [In this case, it is T, but it's still worth testing.]
            if ((not F611A00.TC_Object_X_Coord_Called) or
                 F611A00.TC_Triangle_X_Coord_Called) then
               Report.Failed
                 ("Precondition expressions should only have called " &
                  "primitives appropriate for the tag - " & Code);
               F611A00.TC_Output;
            elsif F611A00.TC_Triangle_Distance_Pre_Class_Called or
                  F611A00.TC_Object_Y_Coord_Called or
                  F611A00.TC_Triangle_Y_Coord_Called then
               Report.Failed
                 ("Wrong class-wide precondition expression evaluated - " &
                                                                       Code);
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


   procedure Test_Get_Distance2 (O    : in C611A030.Object;
                                 Code : in Character) is
      -- Test a statically bound call of Distance (where the real tag
      -- might differ from the statically determined tag).

      function TC_Eval_Object (O    : in C611A030.Object'Class)
         return C611A030.Object'Class is
         -- Called when the parameter of Distance is evaluated. Better
         -- be before the precondition is evaluated.

         -- Note: If we had this return the specific type (which is what
         -- we really want), this would change the tag to that specific
         -- type (which is definitely NOT what we really want). Thus the
         -- silly call below.
      begin
         -- Check the objective:
         --    Check that an enabled class-wide precondition of a subprogram S
         --    is evaluated after evaluating the parameters of a call on S.

         if F611A00.TC_Object_Distance_Pre_Class_Called or
            F611A00.TC_Triangle_Distance_Pre_Class_Called then
            Report.Failed
               ("Class-wide precondition evaluated before the parameter was " &
                "evaluated (statically bound call) - " & Code);
            F611A00.TC_Output;
         end if;
         return O;
      end TC_Eval_Object;


      function Call_Get_Distance (O : in C611A030.Object)
         return Float is
         Inner_Distance : Float;
      begin
         -- Statically bound call:
         --Inner_Distance := O.Distance;
         Inner_Distance :=
            C611A030.Distance (C611A030.Object(TC_Eval_Object (O)));

         -- Check the objective:
         --    Check that an enabled class-wide postcondition of a subprogram S
         --    is evaluated after completing the subprogram body but before
         --    continuing execution after the call of S.
         if not F611A00.TC_Object_Distance_Post_Class_Called then
            Report.Failed
             ("Post'Class should have been called before " &
              "resuming after calling the subprogram body (statically " &
              "bound call)- " & Code);
            F611A00.TC_Output;
         end if;

         return Inner_Distance;
      end Call_Get_Distance;
   begin
      declare
      begin
         Assertion_Error_Raised := False;
         Order_Error_Raised := False;
         Outer_Distance := Call_Get_Distance (O);

         -- The following checks assume that the precondition and postcondition
         -- of Distance both pass, but of course if either doesn't pass we
         -- don't get here as an exception is raised.

         -- In the following, the tag of O doesn't matter.

         -- Check the following objective:
         --    For a nonabstract tagged type T and a primitive subprogram S
         --    of T and that has a class-wide postcondition expression E,
         --    check that for a call of S that is statically bound to
         --    type T, calls to primitive operations of T within E invoke
         --    the bodies appropriate for T, even if the tag of the
         --    controlling parameter object is not T.
         if (not F611A00.TC_Object_Distance_Is_Positive_Called) or
             F611A00.TC_Object_Not_Too_Far_Called or
             F611A00.TC_Triangle_Distance_Is_Positive_Called or
             F611A00.TC_Triangle_Not_Too_Far_Called then
            Report.Failed
              ("Postcondition expressions should only have called " &
              "primitives appropriate for the statically determined tag, " &
              "and should have called all of them - " & Code);
             F611A00.TC_Output;
         end if;

         -- Check the following objective:
         --    For a nonabstract tagged type T and a primitive subprogram S of
         --    T and that has a class-wide precondition expression E, check
         --    that for a call of S that is statically bound to type T, calls
         --    to primitive operations of T within E invoke the bodies
         --    appropriate for T, even if the tag of the controlling parameter
         --    object is not T.
         if ((not F611A00.TC_Object_X_Coord_Called) or
              F611A00.TC_Triangle_X_Coord_Called) then
            Report.Failed
              ("Precondition expressions should only have called primitives " &
               "appropriate for the statically determined tag - " & Code);
            F611A00.TC_Output;
         elsif F611A00.TC_Triangle_Distance_Pre_Class_Called or
               F611A00.TC_Object_Y_Coord_Called or
               F611A00.TC_Triangle_Y_Coord_Called then
            Report.Failed
              ("Wrong class-wide precondition expression evaluated for " &
               "a statically bound call - " & Code);
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


   procedure Test_Get_Distance3 (O    : in C611A030.Child.Triangle;
                                 Code : in Character) is
      -- Test a statically bound call of Distance (where the real tag
      -- might differ from the statically determined tag).

      function TC_Eval_Object (O    : in C611A030.Object'Class)
         return C611A030.Object'Class is
         -- Called when the parameter of Distance is evaluated. Better
         -- be before the precondition is evaluated.

         -- Note: If we had this return the specific type (which is what
         -- we really want), this would change the tag to that specific
         -- type (which is definitely NOT what we really want). Thus the
         -- silly call below.
      begin
         -- Check the objective:
         --    Check that an enabled class-wide precondition of a subprogram S
         --    is evaluated after evaluating the parameters of a call on S.

         if F611A00.TC_Object_Distance_Pre_Class_Called or
            F611A00.TC_Triangle_Distance_Pre_Class_Called then
            Report.Failed
               ("Class-wide precondition evaluated before the parameter was " &
                "evaluated (statically bound call) - " & Code);
            F611A00.TC_Output;
         end if;
         return O;
      end TC_Eval_Object;


      function Call_Get_Distance (O : in C611A030.Child.Triangle)
         return Float is
         Inner_Distance : Float;
      begin
         -- Statically bound call:
         --Inner_Distance := O.Distance;
         Inner_Distance :=
            C611A030.Child.Triangle(TC_Eval_Object (O)).Distance;

         -- Check the objective:
         --    Check that an enabled class-wide postcondition of a subprogram S
         --    is evaluated after completing the subprogram body but before
         --    continuing execution after the call of S.
         if not F611A00.TC_Object_Distance_Post_Class_Called then
            Report.Failed
             ("Post'Class should have been called before " &
              "resuming after calling the subprogram body (statically " &
              "bound call)- " & Code);
            F611A00.TC_Output;
         end if;

         return Inner_Distance;
      end Call_Get_Distance;
   begin
      declare
      begin
         Assertion_Error_Raised := False;
         Order_Error_Raised := False;
         Outer_Distance := Call_Get_Distance (O);

         -- The following checks assume that the precondition and postcondition
         -- of Distance both pass, but of course if either doesn't pass we
         -- don't get here as an exception is raised.

         -- In the following, the tag of O doesn't matter.

         -- Check the following objective:
         --    For a nonabstract tagged type T and a primitive subprogram S
         --    of T and that has a class-wide postcondition expression E,
         --    check that for a dynamically tagged dispatching call of S,
         --    calls to primitive operations of T within E invoke the
         --    bodies appropriate for the controlling tag, even if it is not
         --    T.
         if F611A00.TC_Object_Distance_Is_Positive_Called or
            F611A00.TC_Object_Not_Too_Far_Called or
            (not F611A00.TC_Triangle_Distance_Is_Positive_Called) or
            (not F611A00.TC_Triangle_Not_Too_Far_Called) then
            Report.Failed
              ("Postcondition expressions should only have called " &
               "primitives appropriate for the tag, and should have " &
               "called all of them (statically bound call) - " & Code);
            F611A00.TC_Output;
         end if;

         -- Check the following objective:
         --    Check that if multiple enabled class-wide postconditions
         --    apply to a subprogram S, check that they are all evaluated
         --    if they all evaluate to True.
         if not (F611A00.TC_Object_Distance_Post_Class_Called and
                 F611A00.TC_Triangle_Distance_Post_Class_Called) then
            Report.Failed
              ("Hasn't called all classwide postconditions for a " &
               "statically bound call - " & Code);
            F611A00.TC_Output;
         end if;

         -- Check the following objective:
         --    For a nonabstract tagged type T and a primitive subprogram S
         --    of T and that has a class-wide precondition expression E,
         --    check that for a dynamically tagged dispatching call of S,
         --    calls to primitive operations of T within E invoke the
         --    bodies appropriate for the controlling tag, even if it is not
         --    T.
         -- Note that we don't assume that both preconditions are evaluated
         -- (if either are True, the other doesn't have to be evaluated),
         -- but if a precondition is evaluated, make sure that the correct
         -- routines are called.
         if (F611A00.TC_Object_Distance_Pre_Class_Called and then
               (F611A00.TC_Object_X_Coord_Called or
                (not F611A00.TC_Triangle_X_Coord_Called))) or else
            (F611A00.TC_Triangle_Distance_Pre_Class_Called and then
               (F611A00.TC_Object_Y_Coord_Called or
                (not F611A00.TC_Triangle_Y_Coord_Called))) then
            Report.Failed
              ("Precondition expressions should only have called " &
               "primitives appropriate for the tag, and should have " &
               "called all of them that were evaluated for a statically " &
               "bound call - " & Code);
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
   end Test_Get_Distance3;

begin

   Report.Test
     ("C611A03",
      "Check that the correct expressions are evaluated for class-wide " &
      "precondition and postconditions, and that they are evaluated " &
      "at the correct point");

   -- First test the objectives that relate to dispatching calls.

   -- Should pass all of the preconditions and postconditions:

   F611A00.TC_Clear;

   My_Triangle.Move (X => 3.0, Y => 4.0);

   Test_Get_Distance (My_Triangle, Code => '1');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - 1");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 1");
   end if;

   -- Should pass Object's Pre'Class but fail Triangle's Pre'Class.
   -- This means that the precondition passes.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 3.0, Y => -4.0);

   Test_Get_Distance (My_Triangle, Code => '2');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - 2");
      -- Recall that a class-wide precondition passes if ANY of the expressions
      -- evaluate to True (meaning the other can fail).
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 2");
   end if;

   -- Should fail Object's Pre'Class but pass Triangle's Pre'Class.

   -- As the denoted program C611A030.Distance is a primitive of Object, only
   -- its Pre'Class should be checked, not the class-wide precondition of the
   -- body dispatched to (C611A030.Child.Distance, whose class-wide
   -- precondition is an 'or' of C611A030.Distance's and
   -- C611A030.Child.Distance's Pre'Classes).
   -- Thus this precondition should fail. This checks the objective:
   --    Check that the class-wide precondition of a dispatching call is that
   --    associated with the denoted subprogram, even if the body of a
   --    descendant operation is invoked.

   F611A00.TC_Clear;

   My_Triangle.Move (X => -3.0, Y => 4.0);

   Test_Get_Distance (My_Triangle, Code => '3');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 3");
      Report.Comment ("Probably the class-wide precondition of the body " &
                      "actually invoked was used rather than that of the " &
                      "denoted subprogram");
   end if;
   -- Don't care about the order of the postcondition.

   -- Should fail Object's and Triangle's Pre'Class

   F611A00.TC_Clear;

   My_Triangle.Move (X => -3.0, Y => -4.0);

   Test_Get_Distance (My_Triangle, Code => '4');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 4");
   end if;
   -- Don't care about the order of the postcondition.

   -- Should fail Triangle's Post'Class

   F611A00.TC_Clear;

   My_Triangle.Move (X => 9.0, Y => 12.0);

   Test_Get_Distance (My_Triangle, Code => '5');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 5");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 5");
   end if;

   -- Should pass Object's preconditions and postconditions:

   F611A00.TC_Clear;

   My_Object.Move (X => 1.0, Y => 2.0);

   Test_Get_Distance (My_Object, Code => '6');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - 6");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 6");
   end if;

   -- Should fail Object's class-wide precondition.

   F611A00.TC_Clear;

   My_Object.Move (X => -4.0, Y => 0.5);

   Test_Get_Distance (My_Object, Code => '7');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 7");
   end if;
   -- Don't care about the order of the postcondition.

   -- Should fail Object's class-wide postcondition.

   F611A00.TC_Clear;

   My_Object.Move (X => 8.0, Y => -0.25);

   Test_Get_Distance (My_Object, Code => '8');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - 8");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - 8");
   end if;

   -- Now test the objectives that relate to statically bound calls.

   -- Should pass all of the preconditions and postconditions:

   F611A00.TC_Clear;

   My_Triangle.Move (X => 3.0, Y => 4.0);

   Test_Get_Distance2 (C611A030.Object(My_Triangle), Code => 'A');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - A");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - A");
   end if;

   -- Should fail Object's Pre'Class. (It would pass Triangle's Pre'Class,
   -- but that's irrelevant for a statically bound call, since Object's
   -- body will be called.)

   F611A00.TC_Clear;

   My_Triangle.Move (X => -3.0, Y => 4.0);

   Test_Get_Distance2 (C611A030.Object(My_Triangle), Code => 'B');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - B");
   end if;
   -- Don't care about the order of the postcondition.

   -- Should pass Object's preconditions and postconditions:

   F611A00.TC_Clear;

   My_Object.Move (X => 1.0, Y => 2.0);

   Test_Get_Distance2 (My_Object, Code => 'C');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - C");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - C");
   end if;

   -- Should fail Object's class-wide precondition.

   F611A00.TC_Clear;

   My_Object.Move (X => -4.0, Y => 0.5);

   Test_Get_Distance2 (My_Object, Code => 'D');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - D");
   end if;
   -- Don't care about the order of the postcondition.

   -- Should fail Object's class-wide postcondition.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 8.0, Y => -0.25);

   Test_Get_Distance2 (My_Object, Code => 'E');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - E");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - E");
   end if;

   -- Finally, check the objective:
   --   If a subprogram S has multiple applicable class-wide preconditions,
   --   that all such expressions evaluate to False before Assertion_Error
   --   is raised.
   -- To check this properly, we need a statically bound call for type
   -- Triangle.

   -- Should pass all of the preconditions and postconditions:

   F611A00.TC_Clear;

   My_Triangle.Move (X => 3.0, Y => 4.0);

   Test_Get_Distance3 (My_Triangle, Code => 'K');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - K");
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - K");
   end if;

   -- Should pass Object's Pre'Class but fail Triangle's Pre'Class.
   -- This means that the precondition passes.

   F611A00.TC_Clear;

   My_Triangle.Move (X => 3.0, Y => -4.0);

   Test_Get_Distance3 (My_Triangle, Code => 'L');

   if Assertion_Error_Raised then
      Report.Failed ("Assertion_Error raised - L");
      -- Recall that a class-wide precondition passes if ANY of the expressions
      -- evaluate to True (meaning the other can fail).
   elsif Order_Error_Raised then
      Report.Failed ("Postcondition should only have been called after the" &
        " subprogram body - L");
   end if;

   -- Should fail Object's and Triangle's Pre'Class

   F611A00.TC_Clear;

   My_Triangle.Move (X => -3.0, Y => -4.0);

   Test_Get_Distance3 (My_Triangle, Code => 'M');

   if not Assertion_Error_Raised then
      Report.Failed ("Assertion_Error not raised - M");
   end if;
   -- Don't care about the order of the postcondition.

   Report.Result;

end C611A032;
