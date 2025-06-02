-- CB42002.A
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
--      Check that if the Assertion_Policy is Ignore, neither the boolean
--      expression nor the string expression of a pragma Assert is evaluated,
--      even if the assertion would have failed.
--
-- TEST DESCRIPTION:
--      This test is similar to CB42001, other than here we test the assertions
--      with the policy as Ignore and that we have functions in the various
--      expressions to check whether they are evaluated.
--
-- CHANGE HISTORY:
--      25 Jan 16   RLB     Created test.

--!

with Report;
with Ada.Exceptions;
with Ada.Assertions;
procedure CB42002 is
   pragma Assertion_Policy (Ignore);
      -- We always have to give the policy, as the default
      -- is implementation-defined.


   -- Evaluation checkers:

   TC_Was_Evaled_Bool : Boolean := False;
   TC_Was_Evaled_Str  : Boolean := False;

   function TC_Eval (Val : in Boolean) return Boolean is
   begin
      TC_Was_Evaled_Bool := Report.Ident_Bool (True);
      return Val;
   end TC_Eval;

   function TC_Eval (Val : in String) return String is
   begin
      TC_Was_Evaled_Str := Report.Ident_Bool (True);
      return Val;
   end TC_Eval;


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
      pragma Assert (TC_Eval(Iters = Bound), Message =>
         Assertion_Message_Prefix & TC_Eval(Integer'Image(Iters)));
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
      pragma Assert (TC_Eval(Result in 1 .. 6));
   begin
      return Result;
   end Get_Die_Roll;


begin

   Report.Test ("CB42002", "Check that if the Assertion_Policy is Ignore, " &
                           "neither the boolean expression nor the string " &
                           "expression of a pragma Assert is evaluated, " &
                           "even if the assertion would have failed");

   -- Verify that checkers work:
   if TC_Eval (Boolean'Image(TC_Was_Evaled_Str) = TC_Eval ("FALSE")) then
      if not TC_Was_Evaled_Bool then
         Report.Failed
            ("Boolean evaluation checker not called");
      end if;
      if not TC_Was_Evaled_Str then
         Report.Failed
            ("String evaluation checker not called");
      end if;
   else
      Report.Failed
         ("wrong result from evaluation checker");
   end if;

   Test_Block:
   declare
      My_Roll : Positive;
   begin

      -- Reset checkers:
      TC_Was_Evaled_Bool := False;
      TC_Was_Evaled_Str  := False;
      -- Assertion succeeds:
      begin
         Loop_Counter (4);
         if TC_Was_Evaled_Bool or else TC_Was_Evaled_Str then
            Report.Failed
               ("pragma Assert expression evaluated, case 1");
         end if;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 1: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
      end;

      -- Reset checkers:
      TC_Was_Evaled_Bool := False;
      TC_Was_Evaled_Str  := False;
      -- Assertion fails:
      begin
         Loop_Counter (-10);
         if TC_Was_Evaled_Bool or else TC_Was_Evaled_Str then
            Report.Failed
               ("pragma Assert expression evaluated, case 2");
         end if;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 2: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
       end;

      TC_Was_Evaled_Bool := False;
      TC_Was_Evaled_Str  := False;
      -- Assertion succeeds:
      begin
         My_Roll := Get_Die_Roll;
         if TC_Was_Evaled_Bool or else TC_Was_Evaled_Str then
            Report.Failed
               ("pragma Assert expression evaluated, case 3");
         end if;
      exception
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 3: " &
                Ada.Exceptions.Exception_Name(Ugh));
      end;

      TC_Was_Evaled_Bool := False;
      TC_Was_Evaled_Str  := False;
      -- Assertion fails:
      begin
         My_Roll := Get_Die_Roll;
         Report.Failed
            ("No exception raised, expected Constraint_Error, case 4");
         -- With assertions off, the return value is out of the range of
         -- the result subtype, and the check still has to be made (
         -- assertions are ignored, not suppressed).
      exception
         when Constraint_Error =>
            -- Expected this.
            if TC_Was_Evaled_Bool or else TC_Was_Evaled_Str then
               Report.Failed
                  ("pragma Assert expression evaluated, case 4");
            end if;
         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 4: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
       end;

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   -- Try a local scope where the Assertion_Policy is Check:

   declare
      pragma Assertion_Policy (Check);

      procedure Loopy (Bound : in Integer) is
         Iters : Integer := 0;
      begin
         -- We simulate a algorithm where the author has an off-by-one error:
         for Index in 0 .. Bound loop
            Iters := Iters + 1;
         end loop;
         pragma Assert (TC_Eval(Iters = Bound), Message =>
           Assertion_Message_Prefix & TC_Eval(Integer'Image(Iters)));
         if Report.Equal (Iters, 100) then
            Report.Comment ("Century!"); -- Prevent optimization.
         end if;
      end Loopy;

   begin

      -- Assertion fails:
      begin
         Loopy (5);
         Report.Failed ("Assertion_Error not raised by failing assertion, " &
                        "policy = Check");
      exception
         when Excptn : Ada.Assertions.Assertion_Error =>

            if Ada.Exceptions.Exception_Message(Excptn) /=
               Assertion_Message_Prefix & " 6" then
               Report.Failed
                  ("Message captured from exception is not the "  &
                   "message provided to the pragma Assert, saw: " &
                   Ada.Exceptions.Exception_Message(Excptn) &
                   "; expected: " & Assertion_Message_Prefix & " 6");
            end if;
            if (not TC_Was_Evaled_Bool) or else (not TC_Was_Evaled_Str) then
               Report.Failed
                  ("pragma Assert expression not evaluated, case 5");
            end if;

         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 5: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
       end;

   end;

   Report.Result;

end CB42002;
