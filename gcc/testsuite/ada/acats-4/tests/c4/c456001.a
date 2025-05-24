-- C456001.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
-- OBJECTIVE:
--     For exponentiation of floating point types, check that
--       Constraint_Error is raised (or, if no exception is raised and
--       Machine_Overflows is False, that a result is produced) if the
--       result is outside of the range of the base type.
--     This tests digits 5.

-- HISTORY:
--     04/30/03  RLB  Created test from old C45622A and C45624A.

with Report;

procedure C456001 is

     type Flt is digits 5;

     F : Flt;

     function Equal_Flt (One, Two : Flt) return Boolean is
         -- Break optimization.
     begin
          return One = Two * Flt (Report.Ident_Int(1));
     end Equal_Flt;

begin
     Report.Test ("C456001", "For exponentiation of floating point types, " &
                      "check that Constraint_Error is raised (or, if " &
                      "if no exception is raised and Machine_Overflows is " &
                      "False, that a result is produced) if the result is " &
                      "outside of the range of the base type.");

     begin
         F := (Flt'Base'Last)**Report.Ident_Int (2);
         if Flt'Machine_Overflows Then
             Report.Failed ("Constraint_Error was not raised for " &
                       "exponentiation");
         else
             -- RM95 3.5.6(7) allows disobeying RM95 4.5(10) if
             -- Machine_Overflows is False.
             Report.Comment ("Constraint_Error was not raised for " &
                       "exponentiation and Machine_Overflows is False");
         end if;
         if not Equal_Flt (F, F) then
             -- Optimization breaker, F must be evaluated.
             Report.Comment ("Don't optimize F");
         end if;
     exception
         when Constraint_Error =>
             Report.Comment ("Constraint_Error was raised for " &
                             "exponentiation");
         when others =>
             Report.Failed ("An exception other than Constraint_Error " &
                            "was raised for exponentiation");
     end;

     Report.Result;
end C456001;
