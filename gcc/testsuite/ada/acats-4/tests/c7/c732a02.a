-- C732A02.A
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
--      returns an object with a part of T in the return object, includes
--      an invariant check for T.
--
--      Check that a successful return from a call on a subprogram declared
--      in the immediate scope of T and visible outside of that scope and that
--      has in out or out parameters with a part of T, includes an invariant
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
--         (1) A default initialized object of Concert (both from an object
--             declaration and from <> in an aggregate).
--         (2) A return object of the type Concert.
--         (3) An in out parameter of the type Concert after a procedure call.
--
-- CHANGE HISTORY:
--      09 Apr 15   RLB     Created test from parts of C732A01 and C732001.
--
--!
with Ada.Assertions;
with F732A00;
with Report;
with Ada.Exceptions;

procedure C732A02 is

   pragma Assertion_Policy (Check);

   package Bells renames F732A00;

   procedure Test_Default_Initialization is

      procedure Do_Default_Initialization_in_an_Aggregate is

         Default_Initialized_Concert : Bells.Concert_Type(1..2) :=
            (1 => <>, 2 => Bells.Rounds);

      begin

         if not Bells.Each_Bell_Occurs_Once (
                                      Default_Initialized_Concert(2)) then
            Report.Failed
              ("Incorrect default initialization - Aggregate");
            -- Mainly to use the object so it isn't subject to dead object
            -- elimination.
         end if;

      end Do_Default_Initialization_in_an_Aggregate;

      procedure Do_Default_Initialization_Here is

         Default_Initialized_Concert : Bells.Concert_Type(1..3);

      begin

         if not Bells.Each_Bell_Occurs_Once (
                                       Default_Initialized_Concert(1)) then
            Report.Failed
              ("Incorrect default initialization - Outside");
         end if;

      end Do_Default_Initialization_Here;

   begin

      declare
      begin

         Do_Default_Initialization_in_an_Aggregate;

         Report.Failed
           ("Default initialization of an aggregate should have raised " &
            "Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

      declare
      begin

         Do_Default_Initialization_Here;

         Report.Failed
           ("Outside default initialization should have raised " &
            "Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

   end Test_Default_Initialization;


   procedure Test_Function_Return (Kind    : Bells.Concert_Kind;
                                   Code    : in String;
                                   Fails   : in Boolean) is
   begin
      declare

         My_Concert : Bells.Concert_Type := Bells.Get_Concert (Kind);
             -- Propagates Assertion_Error if the invariant
             -- check fails.

      begin

         if Fails then
            Report.Failed
               ("Invalid part should have raised Assertion_Error - " & Code);
         else
            null;
            --Report.Comment
            --   ("Valid parts did not raise error - " & Code);
         end if;

         -- Optimization blocker, use My_Concert.
         if My_Concert'Length = 0 then
            if Bells."/=" (Kind, Bells.Null_Concert) then
               Report.Failed
                  ("No concert returned - " & Code);
            -- else OK.
            end if;
         elsif not Bells.Each_Bell_Occurs_Once (
                            My_Concert(My_Concert'First)) then
            Report.Failed
               ("Concert contains an invalid Change - " & Code);
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
               ("Valid parts unexpectedly raised Assertion_Error - "
                  & Code);
         end if;

      when Err:others =>
         Report.Failed
            ("Unexpected " & Ada.Exceptions.Exception_Name (Err) &
             "raised by subtest " & Code);
         Report.Comment
            ("  Info: " & Ada.Exceptions.Exception_Information (Err));

   end Test_Function_Return;


   procedure Test_Invalid_In_Out_Parameter is

      procedure Do_Invalid_In_Out_Parameter is

         My_Concert : Bells.Concert_Type :=
             Bells.Get_Concert (Kind => Bells.OK_Len_3);

      begin

         Bells.Alter_Concert_Part (My_Concert, 2, Kind => Bells.Set_Invalid);

         if not Bells.Each_Bell_Occurs_Once (My_Concert(3)) then
            Report.Failed
                ("Incorrect broken alteration");
          end if;

      end Do_Invalid_In_Out_Parameter;

   begin

      declare
      begin

         Do_Invalid_In_Out_Parameter;

         Report.Failed
           ("Invalid Concert In Out parameter should have raised " &
            "Assertion_Error");

      exception

         when Ada.Assertions.Assertion_Error =>

            null; -- Invariant check failed, as expected.

      end;

   end Test_Invalid_In_Out_Parameter;


   procedure Test_Valid_In_Out_Parameter is

      My_Concert : Bells.Concert_Type :=
         Bells.Get_Concert (Kind => Bells.OK_Len_3);

   begin

      Bells.Alter_Concert_Part (My_Concert, 2, Kind => Bells.Set_Valid);

      if not Bells.Each_Bell_Occurs_Once (My_Concert(2)) then
         -- The invariant should have failed if this is True.
         Report.Failed
             ("Incorrect OK alteration");
      end if;

   end Test_Valid_In_Out_Parameter;

begin

   Report.Test
     ("C732A02",
      "Check that return from a call on a subprogram declared with a type T " &
      "checks the invariants of T when the result or an in out parameter " &
      "has a part of T");

   Test_Default_Initialization;

   Test_Function_Return (Kind  => Bells.OK_Len_3,
                         Code  => "A",
                         Fails => False);

   Test_Function_Return (Kind  => Bells.Four_Invalid_Changes,
                         Code  => "B",
                         Fails => True);

   Test_Function_Return (Kind  => Bells.OK_Len_6,
                         Code  => "C",
                         Fails => False);

   Test_Function_Return (Kind  => Bells.One_Invalid_Change_First_of_2,
                         Code  => "D",
                         Fails => True);

   Test_Function_Return (Kind  => Bells.Null_Concert,
                         Code  => "E",
                         Fails => False);

   Test_Function_Return (Kind  => Bells.One_Invalid_Change_Last_of_3,
                         Code  => "F",
                         Fails => True);

   Test_Invalid_In_Out_Parameter;

   Test_Valid_In_Out_Parameter;

   Report.Result;

end C732A02;
