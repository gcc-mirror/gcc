-- CXAH002.A
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
--
--*
-- OBJECTIVE:
--     Check that package Ada.Environment_Variables can be used to modify
--     environment variables.
--
-- TEST DESCRIPTION:
--     We try to create, modify, and clear a specified environment variable
--     (named by an Impdef constant). This test tests Exists, Set, Clear
--     (one parameter version), Value (one parameter version), and Iterate.
--
--     This test assumes that no other process will change the environment
--     while the test is running. It also assumes that the specified
--     environment variable does not exist when the test starts.
--
--     If the target execution environment does not support modifying
--     environment variables, the Impdef constant
--     Unused_Environment_Variable_To_Modify MUST be set to the null string.
--     If it is not, the test will fail with an unexpected exception.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations capable of modifying,
--      creating, and deleting environment variables.
--
-- CHANGE HISTORY:
--     14 Jan 2015  RLB  Created test.
--      9 Feb 2015  RLB  Added missing Applicability Criteria.
--     12 Mar 2015  RLB  Fixed overlength lines.
--
--!
with Report;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Impdef;
procedure CXAH002 is
   Env_Name : constant String := Impdef.Unused_Environment_Variable_To_Modify;

   Found_It : Boolean := False;

   procedure Check_It (Name, Value : in String) is
      -- Called for each variable in the environment.
   begin
      if Name = Env_Name then
         if Value = "Madison" then
            null; -- Expected value.
         elsif Value = "San Diego" then
            Report.Failed ("Value of variable unchanged");
         else
            Report.Failed ("Value of variable incorrect - " & Value);
         end if;
         Found_It := True;
      end if;
   end Check_It;

begin
   Report.Test ("CXAH002", "Check that package Ada.Environment_Variables " &
                           "can be used to modify an environment variable");

   if Env_Name = "" then
      Report.Not_Applicable ("No writable environment variable defined");
      goto Done;
   elsif Exists (Env_Name) then
      Report.Failed ("Writable environment variable already exists");
   end if;

   -- Create the environment variable:
   Set (Env_Name, "San Diego");

   if not Exists (Env_Name) then
      Report.Failed ("Environment variable does not exist after creating");
   elsif Value (Env_Name) /= "San Diego" then
      Report.Failed ("Environment variable has the wrong value");
   end if;

   -- Modify the environment variable:
   Set (Env_Name, "Madison");

   if not Exists (Env_Name) then
      Report.Failed ("Environment variable does not exist after modification");
   elsif Value (Env_Name) /= "Madison" then
      if Value (Env_Name) = "San Diego" then
         Report.Failed ("Environment variable value not changed");
      else
         Report.Failed ("Environment variable has the wrong value " &
                        "after modification");
      end if;
   end if;

   Iterate (Check_It'Access);

   if not Found_It then
      Report.Failed ("Environment variable not found by iterator");
   end if;

   -- Delete the environment variable:
   Clear (Env_Name);

   if Exists (Env_Name) then
      Report.Failed ("Environment variable still exists");
   end if;

   Found_It := False;

   Iterate (Check_It'Access);

   if Found_It then
      Report.Failed ("Environment variable found by iterator");
   end if;

<<Done>>
   Report.Result;
end CXAH002;
