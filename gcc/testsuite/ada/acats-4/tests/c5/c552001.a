--  C552001.A
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
--    Check correct operation of array component iterators:
--       (A) Check that the loop parameter of an array component
--           iterator can be assigned if the array object is a variable view.
--       (B) Check that the execution of an array component iterator is
--           immediately complete if the array is a null array.
--       (C) Check that a forward array component iterator visits each
--           component of a one-dimensional array exactly once, in the order of
--           index values (first to last).
--       (D) Check that a forward array component iterator for a
--           two-dimensional array visits each component in canonical order,
--           with the last dimension varying fastest.
--       (E) Check that a forward array component iterator for a
--           three-dimensional array visits each component in canonical order,
--           with the last dimension varying fastest, and the middle dimension
--           varying the next fastest.
--       (F) Check that a reverse array component iterator visits each
--           component of a one-dimensional array exactly once, in the reverse
--           order of index values (last to first).
--       (G) Check that a reverse array component iterator for a
--           two-dimensional array visits each component in canonical order,
--           with the last dimension varying fastest.
--       (H) Check that the iterable_name of an array component iterator is
--           evaluated exactly once at the start of the loop.
--
--  CHANGE HISTORY:
--    10 Feb 2015  BJM  Created ACATS test.
--    18 Mar 2015  RLB  Readied test for issuance; Fortran subtests split into
--                      a separate file. Added dynamic and parameter cases to
--                      increase coverage.

--!

with Report;

procedure C552001 is

   type One_D_Array is array (Positive range <>) of Integer;

   subtype Ten_Integer_Array is One_D_Array (1 .. 10);

   A : Ten_Integer_Array :=
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

   Backwards_A : constant Ten_Integer_Array :=
     (10, 9, 8, 7, 6, 5, 4, 3, 2, 1);

   type Two_D_Array is array (Positive range <>,
                              Positive range <>) of Integer;

   Null_Array : array (1 .. 0) of Integer := (others => 0);

   Null_2D_Array : Two_D_Array
      (Report.Ident_Int(1) .. 4, 2 .. Report.Ident_Int(1)) :=
      (others => (others => 0));

   C : Ten_Integer_Array := (others => 0);

   Two_D : constant Two_D_Array (1 .. 5, 1 .. 3) := (1 => (1, 2, 3),
                                                     2 => (4, 5, 6),
                                                     3 => (7, 8, 9),
                                                     4 => (10, 11, 12),
                                                     5 => (13, 14, 15));

   Dyn_Two_D : constant Two_D_Array (1 .. Report.Ident_Int(4),
                                     1 .. Report.Ident_Int(2))
        := (1 => (1, 2),
            2 => (3, 4),
            3 => (5, 6),
            4 => (7, 8));

   Three_D_E : constant array (1 .. 2, 1 .. 3, 1 .. 4) of Integer :=
     (1 => (1 => (1, 2, 3, 4),
            2 => (5, 6, 7, 8),
            3 => (9, 10, 11, 12)),
      2 => (1 => (13, 14, 15, 16),
            2 => (17, 18, 19, 20),
            3 => (21, 22, 23, 24)));

   type Result_Array is array
     (1 .. Two_D'Length (1) * Two_D'Length (2)) of Integer;

   Result : Result_Array := (others => 0);
   Expected_Result : constant Result_Array :=
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);

   Expected_Reverse_Result : constant Result_Array :=
     (15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1);

   type Three_D_Result_Array is array
     (1 .. Three_D_E'Length (1) *
          Three_D_E'Length (2) *
          Three_D_E'Length (3)) of Integer;

   Three_D_Result : Three_D_Result_Array := (others => 0);

   Expected_3D_Result : constant Three_D_Result_Array :=
     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24);

   Count : Natural := 0;
   Evaluation_Count : Natural := 0;

   function Get_Array return Ten_Integer_Array is
      Local_Result : constant Ten_Integer_Array :=
        (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   begin
      Evaluation_Count := Evaluation_Count + 1;
      return Local_Result;
   end Get_Array;

   procedure Test_One_D (Arr : in One_D_Array; In_Reverse : in Boolean) is
   begin
      Count := 0;
      C := (others => 0);
      if In_Reverse then
         for E of reverse Arr loop
            Count := Count + 1;
            C (Count) := E;
         end loop;
      else
         for E of Arr loop
            Count := Count + 1;
            C (Count) := E;
         end loop;
      end if;
   end Test_One_D;

   procedure Test_Two_D (Arr : in Two_D_Array; In_Reverse : in Boolean) is
   begin
      Count := 0;
      Result := (others => 0);
      if In_Reverse then
         for E of reverse Arr loop
            Count := Count + 1;
            Result (Count) := E;
         end loop;
      else
         for E of Arr loop
            Count := Count + 1;
            Result (Count) := E;
         end loop;
      end if;
   end Test_Two_D;

   procedure Mod_1D (Arr : in out Ten_Integer_Array) is
   begin
      for E of reverse Arr loop
         E := E + 2;
      end loop;
   end Mod_1D;

begin

   Report.Test ("C552001",
                "Check correct operation of array component iterators");

   --  Loop parameter can be assigned for variable array. Objective (A)
   for E of A loop
      E := E + 1;
   end loop;

   if (for all I in A'Range => A (I) /= I + 1) then
      Report.Failed
        (Descr => "1A) array component iterator should have been modified");
   end if;

   Mod_1D (A);

   if (for all I in A'Range => A (I) /= I + 3) then
      Report.Failed
        (Descr => "1B) array component iterator should have been modified");
   end if;

   --  A null array does not iterate. Objective (B)
   for E of Null_Array loop
      Report.Failed
        (Descr => "2A) null array should not have iterations");
      E := E + 1;
   end loop;

   Test_One_D (One_D_Array(Null_Array), In_Reverse => True);

   if Count /= 0 then
      Report.Failed
        (Descr => "2B) null array should not have iterations");
   end if;

   for E of Null_2D_Array loop
      Report.Failed
        (Descr => "2C) null array should not have iterations");
      E := E + 1;
   end loop;

   Test_Two_D (Null_2D_Array, In_Reverse => False);

   if Count /= 0 then
      Report.Failed
        (Descr => "2D) null array should not have iterations");
   end if;

   --  A forward iterator visits each component of a one-dimensional array
   --  exactly once, in the order of index values (first to last).
   --  Objective (C)
   for E of A loop
      Count := Count + 1;
      C (Count) := E;
   end loop;

   if Count /= 10 or else C /= A then
      Report.Failed
        (Descr =>
           "3A) Each component wasn't visited exactly once in forward order");
   end if;

   Test_One_D (A, In_Reverse => False);

   if Count /= 10 or else C /= A then
      Report.Failed
        (Descr =>
           "3B) Each component wasn't visited exactly once in forward order");
   end if;


   --  Check that a forward array component iterator for a two-dimensional
   --  array visits each component in canonical order, with the last dimension
   --  varying fastest.  Objective (D)
   Count := 0;
   for E of Two_D loop
      Count := Count + 1;
      Result (Count) := E;
   end loop;

   if Count /= Result_Array'Length or else Result /= Expected_Result then
      Report.Failed
        (Descr =>
           "4A) Each component wasn't visited exactly once " &
           "in forward canonical order");
   end if;

   Count := 0;
   for E of Dyn_Two_D loop
      Count := Count + 1;
      Result (Count) := E;
   end loop;

   if Count /= 8 or else Result(1..8) /= Expected_Result(1..8) then
      Report.Failed
        (Descr =>
           "4B) Each component of dynamic array wasn't visited exactly once " &
           "in forward canonical order");
   end if;

   Test_Two_D (Two_D, In_Reverse => False);

   if Count /= Result_Array'Length or else Result /= Expected_Result then
      Report.Failed
        (Descr =>
           "4C) Each component wasn't visited exactly once " &
           "in forward canonical order - subprogram case");
   end if;

   --  Check that a forward array component iterator for a three-dimensional
   --  array visits each component in canonical order, with the last dimension
   --  varying fastest, and the middle dimension varying the next fastest.
   --  Objective (E)
   Count := 0;
   for E of Three_D_E loop
      Count := Count + 1;
      Three_D_Result (Count) := E;
   end loop;

   if Count /= Three_D_Result'Length or else
     Three_D_Result /= Expected_3D_Result
   then
      Report.Failed
        (Descr =>
           "5) Each component wasn't visited exactly once " &
           "in forward canonical order");
   end if;

   Count := 0;
   C := (others => 0);

   A := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

   --  Check that a reverse array component iterator visits each component of a
   --  one-dimensional array exactly once, in the reverse order of index values
   --  (last to first). Objective (F)
   for E of reverse A loop
      Count := Count + 1;
      C (Count) := E;
   end loop;

   if Count /= 10 or else C /= Backwards_A then
      Report.Failed
        (Descr =>
           "6A) Each component wasn't visited exactly once in reverse order");
   end if;

   Test_One_D (A, In_Reverse => True);

   if Count /= 10 or else C /= Backwards_A then
      Report.Failed
        (Descr =>
           "6B) Each component wasn't visited exactly once in reverse order");
   end if;

   --  Check that a reverse array component iterator for a two-dimensional
   --  array visits each component in canonical order, with the last dimension
   --  varying fastest. Objective (G)
   Count := 0;
   Result := (others => 0);

   for E of reverse Two_D loop
      Count := Count + 1;
      Result (Count) := E;
   end loop;

   if Count /= Result_Array'Length or else Result /= Expected_Reverse_Result
   then
      Report.Failed
        (Descr =>
           "7A) Each component wasn't visited exactly once " &
           "in reverse canonical order");
   end if;

   Test_Two_D (Two_D, In_Reverse => True);

   if Count /= Result_Array'Length or else Result /= Expected_Reverse_Result
   then
      Report.Failed
        (Descr =>
           "7B) Each component wasn't visited exactly once " &
           "in reverse canonical order");
   end if;

   --  Check that the iterable_name of an array component iterator is evaluated
   --  exactly once at the start of the loop. Objective (H)
   Count := 0;
   for E of Get_Array loop
      Count := Count + 1;
   end loop;

   if Evaluation_Count /= 1 then
      Report.Failed
        (Descr =>
           "8) Iterable_Name was not evaluated only once");
   elsif Count /= Ten_Integer_Array'Length then
      Report.Failed
        (Descr =>
           "8) Wrong number of iterators for array returned from function");
   end if;

   Report.Result;

end C552001;
