-- CXAH003.A
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
--     Check that the two parameter Value in package Ada.Environment_Variables
--     works as expected.
--
-- TEST DESCRIPTION:
--     We try to use a non-existent specified environment variable, then
--     create it and try again. This test tests Exists, Set, Clear
--     (one parameter version), Value (two parameter version), and Iterate.
--     This is a separate test so that this Ada 2012 feature is tested
--     separately from the rest of the package.
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
procedure CXAH003 is
   Env_Name : constant String := Impdef.Unused_Environment_Variable_To_Modify;

begin
   Report.Test ("CXAH003", "Check that the two parameter Value in package " &
                           "Ada.Environment_Variables works as expected");

   if Env_Name = "" then
      Report.Not_Applicable ("No writable environment variable defined");
      goto Done;
   elsif Exists (Env_Name) then
      Report.Failed ("Writable environment variable already exists");
   elsif Value (Env_Name, "Maui") /= "Maui" then
      Report.Failed ("Wrong result from two parameter Value when the " &
                     "variable does not exist");
   end if;

   -- Create the environment variable:
   Set (Env_Name, "Madison");

   if not Exists (Env_Name) then
      Report.Failed ("Environment variable does not exist after creation");
   elsif Value (Env_Name, "Maui") /= "Madison" then
      Report.Failed ("Wrong result from two parameter Value when the " &
                     "variable does exist");
   end if;

   -- Delete the environment variable:
   Clear (Env_Name);

   if Exists (Env_Name) then
      Report.Failed ("Environment variable still exists");
   elsif Value (Env_Name, "Nassau") /= "Nassau" then
      Report.Failed ("Wrong result from two parameter Value when the " &
                     "variable does not exist after deletion");
   end if;

<<Done>>
   Report.Result;
end CXAH003;
