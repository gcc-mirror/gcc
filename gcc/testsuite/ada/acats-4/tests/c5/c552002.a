--  C552002.A
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
--  OBJECTIVE:
--    Check correct operation of array component iterators when the array
--    has convention Fortran:
--       (A) Check that a forward array component iterator for a
--           two-dimensional array in Fortran convention visits each component
--           in canonical order, with the first dimension varying fastest.
--       (B) Check that a reverse array component iterator for a
--           two-dimensional array in Fortran convention visits each component
--           in canonical order, with the first dimension varying fastest.
--
--  APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support convention
--     Fortran on Fortran-eligible types.
--
--  CHANGE HISTORY:
--    10 Feb 2015 BJM Created ACATS test.
--    18 Mar 2015 RLB Readied test for issuance; split Fortran subtests and
--                    added applicability criteria.
--!

with Report;

procedure C552002 is

   type Fortran_Two_D_Array is array (1 .. 5, 1 .. 3) of Integer
      with Convention => Fortran;            -- N/A => ERROR.

   Fortran_A : Fortran_Two_D_Array :=
     (1 => (1, 6, 11),
      2 => (2, 7, 12),
      3 => (3, 8, 13),
      4 => (4, 9, 14),
      5 => (5, 10, 15));

   type Result_Array is array
     (1 .. Fortran_A'Length (1) * Fortran_A'Length (2)) of Integer;

   Result : Result_Array := (others => 0);
   Expected_Result : constant Result_Array :=
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);

   Expected_Reverse_Result : constant Result_Array :=
     (15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1);

   Count : Natural := 0;

begin

   Report.Test ("C552002",
                "Check correct operation of array component iterators " &
                "for convention Fortran");

   --  Check that a forward array component iterator for a two-dimensional
   --  array in Fortran convention visits each component in canonical order,
   --  with the first dimension varying fastest. Objective (A)
   Count := 0;
   Result := (others => 0);

   for E of Fortran_A loop
      Count := Count + 1;
      Result (Count) := E;
   end loop;

   if Count /= Result_Array'Length or else Result /= Expected_Result then
      Report.Failed
        (DESCR =>
           "1) Each component wasn't visited exactly once " &
           "in Fortran forward canonical order");
   end if;

   --  Check that a reverse array component iterator for a two-dimensional
   --  array in Fortran convention visits each component in canonical order,
   --  with the first dimension varying fastest. Objective (B)
   Count := 0;
   Result := (others => 0);

   for E of reverse Fortran_A loop
      Count := Count + 1;
      Result (Count) := E;
   end loop;

   if Count /= Result_Array'Length or else Result /= Expected_Reverse_Result
   then
      Report.Failed
        (DESCR =>
           "2) Each component wasn't visited exactly once " &
           "in Fortran reverse canonical order");
   end if;

   Report.Result;

end C552002;
