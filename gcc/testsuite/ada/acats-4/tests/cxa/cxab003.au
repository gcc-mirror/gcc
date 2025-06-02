-- CXAB003.AU
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
--      Check file operations defined in Wide_Wide_Text_IO.Wide_Wide_Bounded_IO.
--
--  CHANGE HISTORY:
--      06 Dec 2005 H K  Initial Version.
--      22 Dec 2014 RLB  Renamed, split into multiple tests as different
--                       clauses are involved, added missing applicability
--                       check, corrected to use file name generator, added
--                       short and long string examples, included Unicode
--                       characters in string.
--!

with Ada.Strings.Wide_Wide_Bounded;           use Ada.Strings.Wide_Wide_Bounded;
with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO;

with Report; use Report;

procedure CXAB003 is
begin
   Test ("CXAB003", "Check file operations defined in Wide_Wide_Text_IO.Wide_Wide_Bounded_IO");

   declare
      package BWW_Str is new
        Ada.Strings.Wide_Wide_Bounded.Generic_Bounded_Length (6);
      package BWW_Str_IO is new
        Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO (BWW_Str);

      use Ada.Wide_Wide_Text_IO;
      use BWW_Str;
      use BWW_Str_IO;

      B : Bounded_Wide_Wide_String := To_Bounded_Wide_Wide_String ("junk");
      F : File_Type;
      S1 : Bounded_Wide_Wide_String := To_Bounded_Wide_Wide_String ("Εύρηκα");
                                           -- It's all Greek to me! :-)
      S2 : Bounded_Wide_Wide_String := To_Bounded_Wide_Wide_String ("KLM");
      Long : Wide_Wide_String := "Double-wide Long";
   begin
      --  Put (File_Type; Bounded_Wide_Wide_String

      begin
         Create (F, Out_File, Report.Legal_File_Name(X => 1));
      exception
         when others =>
             Report.Not_Applicable ("Unable to create Out mode Wide_Wide_Text_IO file");
             goto Done;
      end;

      Put    (F, S1);
      Close  (F);

      --  Put_Line (File_Type; Bounded_Wide_Wide_String)

      Create   (F, Out_File, Report.Legal_File_Name(X => 2));
      Put_Line (F, S1);
      Put_Line (F, S2);
      Put_Line (F, Long);
      Close    (F);

      --  Get_Line (File_Type) return Bounded_Wide_Wide_String);

      Open  (F, In_File, Report.Legal_File_Name(X => 1));
      B := Get_Line (F);
      Delete (F);

      if B /= S1 then
         Failed ("Get_Line (File_Type) return Bounded_Wide_Wide_String");
      end if;

      --  Get_Line (File_Type; out Bounded_Wide_Wide_String)

      Open     (F, In_File, Report.Legal_File_Name(X => 2));
      Get_Line (F, B);

      if B /= S1 then
         Failed ("Get_Line (File_Type; out Bounded_Wide_Wide_String) - S1");
      end if;

      Get_Line (F, B);

      if B /= S2 then
         Failed ("Get_Line (File_Type; out Bounded_Wide_Wide_String) - S2");
      end if;

      begin
         Get_Line (F, B);

         Failed ("Exception not raised by Get_Line of too long string");
         if B /= S2 then -- Stop dead code elimination.
            Failed ("Get_Line (File_Type; out Bounded_Wide_Wide_String) - Long");
         end if;
      exception
         when Ada.Strings.Length_Error => null; -- Expected exception.
      end;

      Delete    (F);

   end;

<<Done>>
   Result;
end CXAB003;
