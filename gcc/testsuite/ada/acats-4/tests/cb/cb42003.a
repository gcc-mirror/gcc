-- CB42003.A
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
--      Check that procedure Assert evaluates its arguments and raises
--      Assertion_Error if the boolean expression is True, regardless of
--      the Assertion_Policy.
--
-- TEST DESCRIPTION:
--      This test is similar to CB42002.
--
-- CHANGE HISTORY:
--      25 Jan 16   RLB     Created test.
--
--!

with Report;
with Ada.Exceptions;
with Ada.Assertions;
procedure CB42003 is
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
      Iters : Integer := 0;
   begin
      -- We simulate a algorithm where the author has forgotten about
      -- the possibility of negative input values.
      for Index in 1 .. Bound loop
         Iters := Iters + 1;
      end loop;
      Ada.Assertions.Assert (TC_Eval(Iters = Bound), Message =>
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
   begin
      Ada.Assertions.Assert (TC_Eval(Result in 1 .. 6));
      return Result;
   end Get_Die_Roll;


begin

   Report.Test ("CB42003", "Check that procedure Assert evaluates its " &
                           "arguments and raises Assertion_Error if the " &
                           "boolean expression is True, regardless of " &
                           "the Assertion_Policy");

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
         if (not TC_Was_Evaled_Bool) or else
            (not TC_Was_Evaled_Str) then
            Report.Failed
               ("procedure Assert arguments not evaluated, case 1");
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
         Report.Failed ("Assertion_Error not raised by failing assertion " &
                        "in procedure Assert");
      exception
         when Excptn : Ada.Assertions.Assertion_Error =>

            if Ada.Exceptions.Exception_Message(Excptn) /=
               Assertion_Message_Prefix & " 0" then
               Report.Failed
                  ("Message captured from exception is not the "  &
                   "message provided to the pragma Assert, case 2, saw: " &
                   Ada.Exceptions.Exception_Message(Excptn) &
                   "; expected: " & Assertion_Message_Prefix & " 0");
            end if;
            if (not TC_Was_Evaled_Bool) or else
               (not TC_Was_Evaled_Str) then
               Report.Failed
                  ("procedure Assert arguments not evaluated, case 2");
            end if;

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
         if (not TC_Was_Evaled_Bool) then
            Report.Failed
               ("procedure Assert argument not evaluated, case 3");
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
         Report.Failed ("Assertion_Error not raised by failing assertion " &
                        "in procedure Assert");
      exception
         when Excptn : Ada.Assertions.Assertion_Error =>
            if (not TC_Was_Evaled_Bool) then
               Report.Failed
                  ("procedure Assert argument not evaluated, case 4");
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
         Ada.Assertions.Assert (TC_Eval(Iters = Bound), Message =>
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
                  ("procedure Assert expressions not evaluated, case 5");
            end if;

         when Ugh : others =>
            Report.Failed
               ("Unexpected exception, case 5: " &
                Ada.Exceptions.Exception_Name(Ugh) &
                " with message " & Ada.Exceptions.Exception_Message(Ugh));
       end;

   end;

   Report.Result;

end CB42003;
