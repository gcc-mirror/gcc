-- CXAA020
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
-- DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
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
--     Check, for enumeration output, that output that
--     does not fit on the current line is handled correctly.
--
-- TEST DESCRIPTION:
--     RM A.10.6(8), A.10.10(10,11); AI05-0036-1
--
--     An enumeration type with one literal is defined and Ada.Text_IO.Enumer-
--     ation_IO is instantiated with it.
--     An Out_File is created with a bounded line length.
--     The literal is output to the file with a Width parameter so that it does
--     not fit on the current line so that a new line must be inserted first.
--     Then it is output to the file with a Width parameter that it does not fit
--     on the bounded line so that Layout_Error must be propagated.
--
--     The file is reset to In_File and the contents compared to the expected one
--     (b stand for the blank character):
--        12345678901234567890  <- Col number
--        bbbbbbbbbb
--        LITERALbbbb
--
--     The test passes if it raises Layout_Error at the second Put and if the
--     resulting file has the contents shown above.
--
--     Author: Christoph Karl Walter Grein
--
-- CHANGE HISTORY
--     16 Jan 07  CKWG Created test.
--     30 May 13  RLB  Minor changes for issuing (added AI number to description,
--                     improved wording of objective, simplified wording of
--                     messages).
--     23 Jan 14  RLB  Added missing Not_Applicable checks.
--!

with Report;

with Ada.Text_IO;
use  Ada.Text_IO;

procedure CXAA020 is

  type Enum is (Literal);
  package Enum_IO is new Enumeration_IO (Enum);
  use Enum_IO;

  File: File_Type;
  Line: String (1 ..21);
  Last: Natural;

begin

  Report.Test ("CXAA020",
               "Check, for enumeration output, that output that does not fit" &
               "on the current line is handled correctly");

  -- Set up test environment

  begin
    Create (File, Mode => Out_File);
  exception
    when others =>
      Report.Not_Applicable ("Unable to create Out mode Text_IO file");
      goto Done;
  end;

  Set_Line_Length (File, 20);
  Set_Col (File, 11);

  -- Write test output

  Put (File, Literal, Width => 11);
  begin
    Put (File, Literal, Width => 21);
    Report.Failed ("Expected Layout_Error is not propagated.");
  exception
    when Layout_Error => null;
  end;

  -- Compare test output with expectation

  begin
    Reset (File, Mode => In_File);
  exception
    when others =>
      Report.Not_Applicable ("Unable to Reset open Text_IO file");
      Close (File);
      goto Done;
  end;

  Get_Line (File, Line, Last);
  if Line (1 .. Last) /= (1 .. 10 => ' ') then
    Report.Failed ("First line is not as expected.");
  end if;

  Get_Line (File, Line, Last);
  if Line (1 .. Last) /= "LITERAL" & (1 .. 4 => ' ') then
    Report.Failed ("Second line is not as expected.");
  end if;

  if not End_of_File (File) then
    Report.Failed ("EoF is not yet encountered");
  end if;

  Close (File);

  -- Summary
<<Done>>

  Report.Result;

end CXAA020;
