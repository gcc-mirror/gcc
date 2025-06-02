-- CXA4037.A
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
--
-- OBJECTIVE:
--    Check that Constraint_Error is raised by Ada.Strings.Fixed.Find_Token if
--    Source'First is not in Positive.  (Defect Report 8652/0049).
--
-- CHANGE HISTORY:
--    12 Feb 2001   PHL   Initial version.
--    16 Apr 2015   RLB   Readied for issuing; fixed case of messages.
--    17 Apr 2015   RLB   Added Wide_String subtest.
--
--!
with Ada.Exceptions;
use Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps.Wide_Constants;
with Report;
use Report;
procedure CXA4037 is

    Null_String : String (-12 .. -345);

    Null_WString : Wide_String (-52 .. -92);

begin
    Test ("CXA4037", "Check that Constraint_Error is raised by Ada.Strings." &
                      "Fixed.Find_Token if Source'First is not in Positive");

    declare
        First, Last : Integer;
    begin
        Ada.Strings.Fixed.Find_Token (
                      Source => Null_String,
                      Set    => Ada.Strings.Maps.Constants.Control_Set,
                      Test   => Ada.Strings.Outside,
                      First  => First,
                      Last   => Last);
        Failed ("No exception raised by Fixed.Find_Token");
    exception
        when Constraint_Error =>
            null;
        when E: others =>
            Failed ("Wrong exception raised - " & Exception_Name (E) & " - " &
                    Exception_Information (E) & " - " & Exception_Message (E));
    end;

    declare
        First, Last : Integer;
    begin
        Ada.Strings.Wide_Fixed.Find_Token (
                   Source => Null_WString,
                   Set    => Ada.Strings.Wide_Maps.Wide_Constants.Control_Set,
                   Test   => Ada.Strings.Outside,
                   First  => First,
                   Last   => Last);
        Failed ("No exception raised by Wide_Fixed.Find_Token");
    exception
        when Constraint_Error =>
            null;
        when E: others =>
            Failed ("Wrong exception raised - " & Exception_Name (E) & " - " &
                    Exception_Information (E) & " - " & Exception_Message (E));
    end;

    Result;
end CXA4037;

