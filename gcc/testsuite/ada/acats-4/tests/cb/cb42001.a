-- CB42001.A
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
--      Check that the boolean expression of a pragma Assert is evaluated
--      if the Assertion_Policy is Check, and Ada.Assertions.Assertion_Error
--      is raised if the expression is False.
--
--      Check that the optional message string in a pragma Assert whose
--      boolean expression evaluates to False is associated with the raised
--      exception occurrence, and that the message string can be obtained
--      using the Exception_Message function with the associated
--      Exception_Occurrence object.
--
-- TEST DESCRIPTION:
--      This test checks the handling of failed Assertions. We use expressions
--      for pragma Assert which fail if one of the arguments is outside of the
--      expected range.
--
--      We check that the optional message associated with a pragma Assert
--      can be retrieved from Ada.Exceptions.Exception_Message.
--
-- CHANGE HISTORY:
--      25 Jan 16   RLB     Created test.
--
--!

with Report;
with Ada.Exceptions;
with Ada.Assertions;
procedure CB42001 is
   pragma Assertion_Policy (Check);
      -- We always have to give the policy, as the default
      -- is implementation-defined.

   Assertion_Message_Prefix : constant String :=
     "Wrong number of iterations=";

   procedure Loop_Counter (Bound : in Integer) is
      -- Try a pragma Assert in place of a statement.
      Iters : Integer := 0;
   begin
      -- We simulate a algorithm where the author has forgotten about
      -- the possibility of negative input values.
      for Index in 1 .. Bound loop
         Iters := Iters + 1;
      end loop;
      pragma Assert (Iters = Bound, Message =>
        Assertion_Message_Prefix & Integer'Image(Iters));
      if Report.Equal (Iters, 100) then
         Report.Comment ("Century!"); -- Prevent optimization.
      end if;
   end Loop_Counter;

   First_Call : Boolean := True;

   function Random return Float is
      -- Not really random! Simulating a random function.
   begin
      First_Call := not First_Call;
      if not First_Call then
          return 0.65;
      else
          return 0.05;
      end if;
   end Random;

   function Get_Die_Roll return Positive is
      Roll : Float := Random;
      Result : Integer := Integer(Roll*6.0);

      -- Simulating an off-by-one error; pragma Assert in place of
      -- a declarative item.
      pragma Assert (Result in 1 .. 6);
   begin
      return Result;
   end Get_Die_Roll;


begin

   Report.Test ("CB42001", "Check that the boolean expression of a pragma " &
                           "Assert is evaluated if the Assertion_Policy is " &
                           "Check, and Ada.Assertions.Assertion_Error is " &
                           "raised if the expression is False. Check that " &
                           "the optional message string in a pragma Assert " &
                           "whose boolean expression evaluates to False is " &
                           "associated with the raised exception occurrence");

   Test_Block:
   declare
      My_Roll : Positive;
   begin

      -- Assertion succeeds:
      begin
         Loop_Counter (4);
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 1: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      -- Assertion fails:
      begin
         Loop_Counter (-10);
         Report.Failed ("Assertion_Error not raised by failing assertion, " &
                        "case 2");
      exception
         when Excptn : Ada.Assertions.Assertion_Error =>

            if Ada.Exceptions.Exception_Message(Excptn) /=
               Assertion_Message_Prefix & " 0" then
               Report.Failed
                  ("Case 2: Message captured from exception is not the "  &
                   "message provided to the pragma Assert, saw: " &
                   Ada.Exceptions.Exception_Message(Excptn) &
                   "; expected: " & Assertion_Message_Prefix & " 0");
            end if;
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 2: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
       end;

      -- Assertion succeeds:
      begin
         My_Roll := Get_Die_Roll;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 3: " &
                Ada.Exceptions.Exception_Name(Ugh));
      end;

      -- Assertion fails:
      begin
         My_Roll := Get_Die_Roll;
         Report.Failed ("Assertion_Error not raised by failing assertion, " &
                        "case 4");
      exception
         when Excptn : Ada.Assertions.Assertion_Error =>
            Report.Comment
              ("Case 4: Exception message was: " &
               Ada.Exceptions.Exception_Message(Excptn));
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 4: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
       end;


      -- Verify that the assertion message is carried even when the
      -- exception is reraised.

      begin

         begin
            Loop_Counter (-4);
            Report.Failed ("Assertion_Error not raised by failing " &
                           "assertion, case 5");
         exception
            when Exc : Ada.Assertions.Assertion_Error =>

               -- The exception is reraised here; message should propagate
               -- with exception occurrence.

               Ada.Exceptions.Reraise_Occurrence(Exc);
            when others => Report.Failed ("Unusual exception raised - " &
                                          "case 5 inner");
         end;
         Report.Failed ("Assertion_Error not propagated");
      exception
         when Excptn : Ada.Assertions.Assertion_Error =>

            if Ada.Exceptions.Exception_Message(Excptn) /=
               Assertion_Message_Prefix & " 0" then
               Report.Failed
                  ("Case 5: Message propagated with exception is not the "  &
                   "message provided to the pragma Assert, saw: " &
                   Ada.Exceptions.Exception_Message(Excptn) &
                   "; expected: " & Assertion_Message_Prefix & " 0");
            end if;

         when others => Report.Failed ("Unusual exception raised - " &
                                       "case 5 outer");
      end;

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CB42001;
