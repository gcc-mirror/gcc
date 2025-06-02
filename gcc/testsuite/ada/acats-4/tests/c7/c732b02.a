-- C732B02.A
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
--      Check that a successful return from a call on a function declared in
--      the immediate scope of T and visible outside of that scope and that
--      returns an access-to-object designating T, includes an invariant check
--      for T on that result.
--
--      Check that a successful return from a call on a subprogram declared
--      in the immediate scope of T and visible outside of that scope and that
--      has an access-to-object parameter designating T, includes an invariant
--      check for T on those parameters.
--
-- TEST DESCRIPTION:
--      This test is based on a program for generating "Diagrams" for
--      bellringing.  The type invariant is that each bell occurs once and only
--      once.
--
--      The type with parts here is a "Concert", which is a list of
--      instructions for bellringing.
--
--      An invariant check fails if any enabled invariant expression for the
--      type evaluates to False. For this test, there is only one invariant
--      expression (given in the foundation) and it is enabled. (It does not
--      matter if invariants are enabled in this client subprogram; it is
--      the state for the place where the invariant is defined that matters.)
--
--      In this test, we check invariant checks that fail for:
--         (1) A return object of access-to-Change.
--         (2) A parameter of access-to-Change after a procedure or
--             function call.
--
-- CHANGE HISTORY:
--      10 Apr 15   RLB     Created test from parts of C732B01 and C732A02.
--
--!
with Ada.Assertions;
with F732B00;
with Report;
with Ada.Exceptions;

procedure C732B02 is

   pragma Assertion_Policy (Check);

   package Bells renames F732B00;

   procedure Test_Function (Kind    : Bells.Get_Change_Kind;
                            Code    : in String;
                            Fails   : in Boolean) is

      A2C : Bells.Access_to_Change := Bells.Create_Change (Bells.Valid);

   begin
      declare

         Result : Bells.Access_to_Change :=
            Bells.Get_Change (A2C, Kind);
             -- Propagates Assertion_Error if the invariant
             -- check fails.
         use type Bells.Access_to_Change; -- For "=".

      begin

         if Fails then
            Report.Failed
               ("Invalid designated object should have raised " &
                "Assertion_Error - " & Code);
         else
            null;
            --Report.Comment
            --   ("Valid designated object did not raise error - " & Code);
         end if;

         -- Optimization blocker, use Result.
         if Result = null then
            null; -- Can't check this.
         elsif not Bells.Each_Bell_Occurs_Once (Result.all) then
            Report.Failed
               ("Result contains an invalid Change - " & Code);
         end if;
      end;

   exception

      when Ada.Assertions.Assertion_Error =>

         if Fails then
            null;
            --Report.Comment
            --   ("Invalid part raised Assertion_Error as expected - "
            --      & Code);
         else
            Report.Failed
               ("Valid designated object unexpectedly raised " &
                "Assertion_Error - " & Code);
         end if;

      when Err:others =>
         Report.Failed
            ("Unexpected " & Ada.Exceptions.Exception_Name (Err) &
             "raised by subtest " & Code);
         Report.Comment
            ("  Info: " & Ada.Exceptions.Exception_Information (Err));

   end Test_Function;


   procedure Test_Invalid_Procedure_Parameter is

      procedure Do_Invalid_Procedure_Parameter is

         My_Change : aliased Bells.Change_Discriminated_Type := Bells.Rounds;

      begin

         Bells.Reset_Change (My_Change'Access, Kind => Bells.Invalid);

         if not Bells.Each_Bell_Occurs_Once (My_Change) then
            Report.Failed
                ("Incorrect broken alteration");
          end if;

      end Do_Invalid_Procedure_Parameter;

   begin

      begin

         Do_Invalid_Procedure_Parameter;

         Report.Failed
           ("Invalid access-to-Change parameter should have raised " &
            "Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

   end Test_Invalid_Procedure_Parameter;


   procedure Test_Valid_Procedure_Parameter is

      My_Change : aliased Bells.Change_Discriminated_Type := Bells.Rounds;

   begin

      Bells.Reset_Change (My_Change'Access, Kind => Bells.Valid);

      if not Bells.Each_Bell_Occurs_Once (My_Change) then
         -- The invariant should have failed if this is True.
         Report.Failed
             ("Incorrect OK alteration");
      end if;

   end Test_Valid_Procedure_Parameter;


   procedure Test_Invalid_Anonymous_Result is

      procedure Do_Invalid_Anonymous_Result is

         A2C : Bells.Access_to_Change := Bells.Create_Change (Bells.Invalid);

      begin

         if not Bells.Each_Bell_Occurs_Once (A2C.all) then
            -- Use result to block dead object optimizations.
            Report.Failed
                ("Incorrect broken result");
          end if;

      end Do_Invalid_Anonymous_Result;

   begin

      begin

         Do_Invalid_Anonymous_Result;

         Report.Failed
           ("Invalid access-to-Change parameter should have raised " &
            "Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

   end Test_Invalid_Anonymous_Result;


   procedure Test_Valid_Anonymous_Result is

      A2C : Bells.Access_to_Change := Bells.Create_Change (Bells.Valid);

   begin

      if not Bells.Each_Bell_Occurs_Once (A2C.all) then
         -- The invariant should have failed if this is True.
         Report.Failed
             ("Incorrect OK result");
      end if;

   end Test_Valid_Anonymous_Result;

begin

   Report.Test
     ("C732B02",
      "Check that return from a call on a subprogram declared with a type T " &
      "checks the invariants of T when the result or an in out parameter " &
      "has is an access-to-object designating T");

   Test_Function (Kind  => Bells.Result_is_Param,
                  Code  => "Result is Param",
                  Fails => False);

   Test_Function (Kind  => Bells.Param_Invalid,
                  Code  => "Param Invalid",
                  Fails => True);

   Test_Function (Kind  => Bells.Result_Invalid,
                  Code  => "Result Invalid",
                  Fails => True);

   Test_Function (Kind  => Bells.Result_Other_Valid,
                  Code  => "Result Valid",
                  Fails => False);

   Test_Invalid_Procedure_Parameter;

   Test_Valid_Procedure_Parameter;

   Test_Invalid_Anonymous_Result;

   Test_Valid_Anonymous_Result;

   Report.Result;

end C732B02;
