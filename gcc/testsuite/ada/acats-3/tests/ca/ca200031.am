-- CA200031.A
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
--      See CA200030.A.
--
-- TEST DESCRIPTION:
--      See CA200030.A.
--
-- SPECIAL REQUIREMENTS:
--      See CA200030.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         CA200030.A
--      -> CA200031.AM
--
-- PASS/FAIL CRITERIA:
--      See CA200030.A.
--
-- CHANGE HISTORY:
--     09 Apr 07   RLB     Initial test; created from LA20002.
--
--!

limited with CA20003_L;
package CA20003_0 is
   type Data is record
      B : Boolean := False;
   end record;

   procedure Do_It (D : in out Data;
                    Dbg : access CA20003_L.Debug_Info := null);

end CA20003_0;

with CA20003_TC;
package body CA20003_0 is

   procedure Do_It (D : in out Data;
                    Dbg : access CA20003_L.Debug_Info := null) is
   begin
      D.B := True;
      CA20003_TC.TC_Test_Subsystem := CA20003_TC.FIRST_VERSION;
   end Do_It;

end CA20003_0;

with Report;
use Report;
with CA20003_0;
with CA20003_TC;
procedure CA200031 is
    My_Data : CA20003_0.Data;
begin
    Test ("CA20003","Check that a package that is needed only because it is " &
                    "referenced in a limited with clause in included in " &
                    "a partition");

    CA20003_0.Do_It (My_Data);

    if CA20003_TC.TC_Debug_Subsystem = CA20003_TC.INITIAL then
        Failed ("Subsystem referenced by limited with not included");
    elsif CA20003_TC.TC_Debug_Subsystem = CA20003_TC.FIRST_VERSION then
        null; --Comment ("Subsystem referenced by limited with included");
    else
        Failed ("Unusual result from subsystem referenced by limited with");
    end if;

    if CA20003_TC.TC_Test_Subsystem = CA20003_TC.INITIAL then
        Failed ("Test subsystem not executed");
    elsif CA20003_TC.TC_Test_Subsystem = CA20003_TC.FIRST_VERSION then
        null; --Comment ("Test subsystem executed");
    else
        Failed ("Unusual result from test subsystem");
    end if;

    Result;
end CA200031;
