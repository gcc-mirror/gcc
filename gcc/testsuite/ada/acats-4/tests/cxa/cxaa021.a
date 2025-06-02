-- CXAA021.A
--
--                             Grant of Unlimited Rights
--
--     AdaCore holds unlimited rights in the software and documentation
--     contained herein. Unlimited rights are the same as those granted
--     by the U.S. Government for older parts of the Ada Conformity
--     Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--     By making this public release, AdaCore intends to confer upon all
--     recipients unlimited rights equal to those held by the Ada Conformity
--     Assessment Authority. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever,
--     and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--     TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--     DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--     DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--     This test is based on one submitted by AdaCore; AdaCore retains
--     the copyright on the test.
--
--*
--  OBJECTIVE:
--      Check file operations defined in Text_IO.Bounded_IO.
--
--  CHANGE HISTORY:
--      06 Dec 2005 H K  Initial Version.
--      22 Dec 2014 RLB  Renamed, split into multiple tests as different
--                       clauses are involved, added missing applicability
--                       check, corrected to use file name generator, added
--                       short and long string examples.
--!

with Ada.Strings.Bounded;           use Ada.Strings.Bounded;
with Ada.Text_IO;
with Ada.Text_IO.Bounded_IO;

with Report; use Report;

procedure CXAA021 is
begin
   Test ("CXAA021", "Check file operations defined in Text_IO.Bounded_IO");

   declare
      package B_Str is new
        Ada.Strings.Bounded.Generic_Bounded_Length (4);
      package B_Str_IO is new
        Ada.Text_IO.Bounded_IO (B_Str);

      use Ada.Text_IO;
      use B_Str;
      use B_Str_IO;

      B : Bounded_String := To_Bounded_String ("junk");
      F : File_Type;
      S1 : Bounded_String := To_Bounded_String ("ABCD");
      S2 : Bounded_String := To_Bounded_String ("Sh");
      Long : String := "Too Long";
   begin
      --  Put (File_Type; Bounded_String

      begin
         Create (F, Out_File, Report.Legal_File_Name(X => 1));
      exception
         when others =>
             Report.Not_Applicable ("Unable to create Out mode Text_IO file");
             goto Done;
      end;

      Put    (F, S1);
      Close  (F);

      --  Put_Line (File_Type; Bounded_String)

      Create   (F, Out_File, Report.Legal_File_Name(X => 2));
      Put_Line (F, S1);
      Put_Line (F, S2);
      Put_Line (F, Long);
      Close    (F);

      --  Get_Line (File_Type) return Bounded_String);

      Open  (F, In_File, Report.Legal_File_Name(X => 1));
      B := Get_Line (F);
      Delete (F);

      if B /= S1 then
         Failed ("Get_Line (File_Type) return Bounded_String");
      end if;

      --  Get_Line (File_Type; out Bounded_String)

      Open     (F, In_File, Report.Legal_File_Name(X => 2));
      Get_Line (F, B);

      if B /= S1 then
         Failed ("Get_Line (File_Type; out Bounded_String) - S1");
      end if;

      Get_Line (F, B);

      if B /= S2 then
         Failed ("Get_Line (File_Type; out Bounded_String) - S2");
      end if;

      begin
         Get_Line (F, B);

         Failed ("Exception not raised by Get_Line of too long string");
         if B /= S2 then -- Stop dead code elimination.
            Failed ("Get_Line (File_Type; out Bounded_String) - Long");
         end if;
      exception
         when Ada.Strings.Length_Error => null; -- Expected exception.
      end;

      Delete    (F);

   end;

<<Done>>
   Result;
end CXAA021;
