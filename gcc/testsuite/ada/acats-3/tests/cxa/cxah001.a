-- CXAH001.A
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
--     Check that package Ada.Environment_Variables exists and can be used to
--     read environment variables.
--
-- TEST DESCRIPTION:
--     We try to read the environment variables of the test execution, all
--     of them using the Iterate procedure, and the specific one named
--     by an Impdef constant. This test tests Exists, Value (the original
--     version only), and Iterate.
--
--     This test does NOT modify the environment of this test. The test
--     assumes that no other process will change the environment while the
--     test is running.
--
--     If the target execution environment does not support reading environment
--     variables, the Impdef constant Existing_Environment_Variable_To_Read
--     MUST be set to the null string. If it is not, the test will fail with
--     an unexpected exception.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations capable of reading
--      existing environment variables.
--
-- CHANGE HISTORY:
--     14 Jan 2015  RLB  Created test.
--      9 Feb 2015  RLB  Added missing Applicability Criteria.
--
--!
with Report;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Impdef;
procedure CXAH001 is
   Env_Read : constant String := Impdef.Existing_Environment_Variable_To_Read;
   Env_Read_Value : access String := null;

   First_Name : access String := null;
   First_Name_Value : access String := null;

   Count : Natural := 0;

   procedure Do_It (Name, Value : in String) is
      -- Called for each variable in the environment.
   begin
      if First_Name = null then
         First_Name := new String'(Name);
         First_Name_Value := new String'(Value);
      end if;
      if Name = Env_Read then
         if Env_Read_Value /= null and then
            Value /= Env_Read_Value.all then
            Report.Failed ("Value of specified variable " & Env_Read &
               " changed during test");
            Report.Comment ("New value is " & Value & "; Old value is " &
               Env_Read_Value.all);
         end if;
      end if;
      Count := Count + 1;
      --Report.Comment ("Value of variable " & Name & " is " & Value); -- Debug
   end Do_It;

begin
   Report.Test ("CXAH001", "Check that package Ada.Environment_Variables " &
                           "exists and can be used to read environment " &
                           "variables");

   if Env_Read = "" then
      Report.Not_Applicable ("No readable environment variable defined");
   elsif not Exists (Env_Read) then
      Report.Failed ("Missing readable environment variable");
   else
      Report.Comment ("Test reading of specified variable: " & Env_Read);
      declare
         Env_Value : constant String := Value (Env_Read);
      begin
         Env_Read_Value := new String'(Env_Value);
         --Report.Comment ("Value of specified variable " & Env_Read &
         --   " is " & Env_Value); -- Debug
      end;
   end if;

   Report.Comment ("Iterate over environment variables"); -- Debug

   begin
      Iterate (Do_It'Access);
   exception
      when Program_Error =>
         Report.Comment ("Environment variables not supported for Iterate");
   end;

   Report.Comment (Natural'Image(Count) & " environment variables found");

   if First_Name /= null then
      if not Exists (First_Name.all) then
         Report.Failed ("Environment variable " & First_Name.all &
                        "found by Iterate has disappeared");
      elsif First_Name_Value /= null then
         if Value (First_Name.all) /= First_Name_Value.all then
            Report.Failed ("Value of variable " & First_Name.all &
               " has changed");
            Report.Comment ("New value is " & Value (First_Name.all) &
                            "; Old value is " & First_Name_Value.all);
         end if;
      end if;
   elsif Count /= 0 then
      Report.Failed ("No environment variable name saved, yet some seen");
   -- else no environment values found.
   end if;

   Report.Result;
end CXAH001;
