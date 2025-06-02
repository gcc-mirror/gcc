-- C7320012.AM
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
-- OBJECTIVE:
--      See C7320010.A.
--
-- TEST DESCRIPTION:
--      See C7320010.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         C7320010.A
--         C7320011.A
--      -> C7320012.AM
--
-- CHANGE HISTORY:
--      28 Dec 14   JAC     Initial pre-release version.
--      16 Jan 15   RLB     Renamed test parts for release, added test cases
--                          for parameter passing.
--      13 Mar 15   RLB     Eliminated overlong lines.
--
--!
with Ada.Assertions;
with Ada.Exceptions;
with C7320010;
with C7320011;
with Report;

procedure C7320012 is

   pragma Assertion_Policy (Check);

   My_T : C7320011.T;
      -- We've made sure that default-initialized objects are OK.

   -- For parameters we have a variety of possibilities (letters in brackets
   -- are the codes for the combinations):
   --    (1) Either in out [M] or out [O] parameter;
   --    (2) Explicit [E] view conversion (direct call to ancestor subprogram)
   --     or implicit [I] view conversion (call to inherited subprogram);
   --    (3) Call via prefix view [P] or "regular" [R] postfix parameters;
   --    (4) Invariant check fails [F] or succeeds [S].
   -- Each possibility is indicated by its letter code, for instance
   -- "MEPF" is an in out parameter with an explicit view conversion used
   -- in a prefix call with an invariant check that fails.
   --
   -- Codes "AssF" and "AssS" are used for the two assignment tests.

   type Subtest_Type is access procedure;

   procedure TC_Run_Subtest (Subtest : in Subtest_Type;
                             Code    : in String;
                             Fails   : in Boolean) is
      -- A framework within which we run each test. This handles all of
      -- the mess of checking results. While the actual operations are
      -- realistic (calling inherited routines is very common), the way we
      -- run them here is purely for testing purposes.
   begin
      begin

         Subtest.all; -- Propagates Assertion_Error if the invariant
                      -- check fails.

         if Fails then
            Report.Failed
               ("Invalid conversion should have raised error - " & Code);
         else
            null;
            --Report.Comment
            --   ("Correct conversion did not raise error - " & Code);
         end if;

      exception

         when Ada.Assertions.Assertion_Error =>

            if Fails then
               null;
               --Report.Comment
               --   ("Invalid conversion raised Assertion_Error as expected - "
               --      & Code);
            else
               Report.Failed
                  ("Correct conversion unexpectedly raised Assertion_Error - "
                     & Code);
            end if;

         when Err:others =>
            Report.Failed
               ("Unexpected " & Ada.Exceptions.Exception_Name (Err) &
                "raised by subtest " & Code);
            Report.Comment
               ("  Info: " & Ada.Exceptions.Exception_Information (Err));

      end;

      if My_T.Val = Report.Ident_Int(0) then -- Optimization blocker, use My_T.
          Report.Failed ("Unusual My_T value");
      end if;

   end TC_Run_Subtest;

   -- All of the subtests.

   procedure Do_Invalid_Assignment is
      Parent_Val : C7320010.T_Ancestor;
   begin
      Parent_Val.Set (3);
      My_T := C7320011.Init;
      -- Should make invariant false; after the assignment, there should be
      -- an invariant check which will fail.
      C7320010.T_Ancestor(My_T) := Parent_Val;
   end Do_Invalid_Assignment;

   procedure Do_Valid_Assignment is
      Parent_Val : C7320010.T_Ancestor;
   begin
      Parent_Val.Set (1);
      My_T := C7320011.Init;
      -- Should leave invariant true; after the assignment, there should be
      -- an invariant check which will pass.
      C7320010.T_Ancestor(My_T) := Parent_Val;
   end Do_Valid_Assignment;

   procedure Do_MEPF is
   begin
      My_T := C7320011.Init;
      -- Should make invariant false; after the call, there should be
      -- an invariant check which will fail.
      C7320010.T_Ancestor(My_T).Inc;
   end Do_MEPF;

   procedure Do_MEPS is
   begin
      My_T := C7320011.Init;
      My_T.Set_B (4);
      -- Should leave invariant true; after the call, there should be
      -- an invariant check which will pass.
      C7320010.T_Ancestor(My_T).Inc;
   end Do_MEPS;

   procedure Do_MERF is
   begin
      My_T := C7320011.Init;
      -- Should make invariant false; after the call, there should be
      -- an invariant check which will fail.
      C7320010.Inc (C7320010.T_Ancestor(My_T));
   end Do_MERF;

   procedure Do_MERS is
   begin
      My_T := C7320011.Init;
      My_T.Set_B (5);
      -- Should leave invariant true; after the call, there should be
      -- an invariant check which will pass.
      C7320010.Inc (C7320010.T_Ancestor(My_T));
   end Do_MERS;

   procedure Do_MIPF is
   begin
      My_T := C7320011.Init;
      -- Should make invariant false; after the call, there should be
      -- an invariant check which will fail.
      My_T.Inc;
   end Do_MIPF;

   procedure Do_MIPS is
   begin
      My_T := C7320011.Init;
      My_T.Set_B (4);
      -- Should leave invariant true; after the call, there should be
      -- an invariant check which will pass.
      My_T.Inc;
   end Do_MIPS;

   procedure Do_MIRF is
   begin
      My_T := C7320011.Init;
      -- Should make invariant false; after the call, there should be
      -- an invariant check which will fail.
      C7320011.Inc (My_T);
   end Do_MIRF;

   procedure Do_MIRS is
   begin
      My_T := C7320011.Init;
      My_T.Set_B (5);
      -- Should leave invariant true; after the call, there should be
      -- an invariant check which will pass.
      C7320011.Inc (My_T);
   end Do_MIRS;

   procedure Do_OEPF is
   begin
      My_T := C7320011.Init;
      -- Should make invariant false; after the call, there should be
      -- an invariant check which will fail.
      C7320010.T_Ancestor(My_T).Set (4);
   end Do_OEPF;

   procedure Do_OEPS is
   begin
      My_T := C7320011.Init;
      My_T.Set_B (4);
      -- Should leave invariant true; after the call, there should be
      -- an invariant check which will pass.
      C7320010.T_Ancestor(My_T).Set (1);
   end Do_OEPS;

   procedure Do_OERF is
   begin
      My_T := C7320011.Init;
      -- Should make invariant false; after the call, there should be
      -- an invariant check which will fail.
      C7320010.Set (C7320010.T_Ancestor(My_T), 6);
   end Do_OERF;

   procedure Do_OERS is
   begin
      My_T := C7320011.Init;
      My_T.Set_B (5);
      -- Should leave invariant true; after the call, there should be
      -- an invariant check which will pass.
      C7320010.Set (C7320010.T_Ancestor(My_T), 3);
   end Do_OERS;

   procedure Do_OIPF is
   begin
      My_T := C7320011.Init;
      -- Should make invariant false; after the call, there should be
      -- an invariant check which will fail.
      My_T.Set (4);
   end Do_OIPF;

   procedure Do_OIPS is
   begin
      My_T := C7320011.Init;
      My_T.Set_B (4);
      -- Should leave invariant true; after the call, there should be
      -- an invariant check which will pass.
      My_T.Set (2);
   end Do_OIPS;

   procedure Do_OIRF is
   begin
      My_T := C7320011.Init;
      -- Should make invariant false; after the call, there should be
      -- an invariant check which will fail.
      C7320011.Set (My_T, 7);
   end Do_OIRF;

   procedure Do_OIRS is
   begin
      My_T := C7320011.Init;
      My_T.Set_B (5);
      -- Should leave invariant true; after the call, there should be
      -- an invariant check which will pass.
      C7320011.Set (My_T, 4);
   end Do_OIRS;

   procedure Do_Init_1 is
   begin
      -- Check that the (re)initialization of My_T doesn't fail for some
      -- inexplicable reason. (If so, the rest of the test is unusable.)
      My_T := C7320011.Init;
   end Do_Init_1;

   procedure Do_Init_2 is
   begin
      -- Check that the (re)initialization of My_T doesn't fail for some
      -- inexplicable reason. (If so, the rest of the test is unusable.)
      My_T := C7320011.Init;
      My_T.Set_B (5);
   end Do_Init_2;

begin

   Report.Test
     ("C732001",
      "Check that when the assertion policy requires checks and there " &
      "is a view conversion from a type to an ancestor of the type, an " &
      "invariant check is performed after assigning to the view conversion " &
      "and after a call where the view conversion is an actual in out " &
      "or out parameter");

   TC_Run_Subtest (Do_Init_1'Access, Code => "In1S", Fails => False);

   TC_Run_Subtest (Do_Init_2'Access, Code => "In2S", Fails => False);

   TC_Run_Subtest (Do_Invalid_Assignment'Access,
                   Code => "AssF", Fails => True);

   TC_Run_Subtest (Do_Valid_Assignment'Access,
                   Code => "AssS", Fails => False);

   TC_Run_Subtest (Do_MEPF'Access, Code => "MEPF", Fails => True);

   TC_Run_Subtest (Do_MEPS'Access, Code => "MEPS", Fails => False);

   TC_Run_Subtest (Do_MERF'Access, Code => "MERF", Fails => True);

   TC_Run_Subtest (Do_MERS'Access, Code => "MERS", Fails => False);

   TC_Run_Subtest (Do_MIPF'Access, Code => "MIPF", Fails => True);

   TC_Run_Subtest (Do_MIPS'Access, Code => "MIPS", Fails => False);

   TC_Run_Subtest (Do_MIRF'Access, Code => "MIRF", Fails => True);

   TC_Run_Subtest (Do_MIRS'Access, Code => "MIRS", Fails => False);

   TC_Run_Subtest (Do_OEPF'Access, Code => "OEPF", Fails => True);

   TC_Run_Subtest (Do_OEPS'Access, Code => "OEPS", Fails => False);

   TC_Run_Subtest (Do_OERF'Access, Code => "OERF", Fails => True);

   TC_Run_Subtest (Do_OERS'Access, Code => "OERS", Fails => False);

   TC_Run_Subtest (Do_OIPF'Access, Code => "OIPF", Fails => True);

   TC_Run_Subtest (Do_OIPS'Access, Code => "OIPS", Fails => False);

   TC_Run_Subtest (Do_OIRF'Access, Code => "OIRF", Fails => True);

   TC_Run_Subtest (Do_OIRS'Access, Code => "OIRS", Fails => False);

   Report.Result;

end C7320012;
