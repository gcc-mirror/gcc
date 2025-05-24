-- C552A02.A
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
--  OBJECTIVE:
--    Check correct operation of container element iterators:
--       (A) Check that the iterable_name of a container element iterator is
--           evaluated exactly once at the start of the loop, followed by a
--           exactly one call on the default iterator function.
--       (B) Check that the execution of a forward container element iterator
--           calls First initially, then Next until Has_Element returns False
--           (assuming no transfer of control), executing the sequence of
--           statements each time.
--       (C) Check that the execution of a forward container element iterator
--           calls First initially, then Next until the loop is left by a
--           transfer of control, executing the sequence of statements each
--           time.
--       (D) Check that the execution of a forward container element iterator
--           never iterates or calls Next if Has_Element is initially False.
--       (E) Check that the execution of a forward container element iterator
--           for a reversible iterator type never calls Last or Previous.
--       (F) Check that the execution of a reverse container element iterator
--           calls Last initially, then Previous until Has_Element returns
--           False (assuming no transfer of control), executing the sequence of
--           statements each time.
--       (G) Check that the execution of a reverse container element iterator
--           calls Last initially, then Previous until the loop is left by a
--           transfer of control, executing the sequence of statements each
--           time.
--       (H) Check that the execution of a reverse container element iterator
--           never iterates or calls Previous if Has_Element is initially
--           False.
--       (I) Check that the execution of a reverse container element iterator
--           never calls First or Next.
--       (J) Check that the default variable indexing of the container type is
--           evaluated once per iteration of the loop for a container element
--           iterator when the container is a variable view and the
--           Variable_Indexing aspect was specified for the container type.
--       (K) Check that the default constant indexing of the container type is
--           evaluated once per iteration of the loop for a container element
--           iterator when the container is a constant view.
--       (L) Check that the loop parameter of a container element iterator
--           denotes the default variable indexing using the current cursor of
--           the container if the container object is a variable view and the
--           Variable_Indexing aspect was specified for the container type,
--           and that this loop parameter can be assigned.
--       (M) Check that the loop parameter of a container element iterator
--           denotes the default constant indexing using the current cursor of
--           the container if the container object is a constant view or the
--           Variable_Indexing aspect was not specified for the container type.


--
--  CHANGE HISTORY:
--    30 Sep 2013 BJM Created ACATS test.
--    02 Jun 2014 RLB ACATS 4.0 version: renamed foundation and test packages.
--                    Improved objective.
--    16 Jun 2014 RLB Removed cursor creation/destruction checks, (as those are
--                    unreliable in the face of allowed optimizations of
--                    non-limited controlled objects).
--    13 Mar 2015 RLB Repaired various violations of ACATS test lexical rules.
--!

with Report;
with Ada.Strings.Unbounded; use Ada;
with F552A00_Sparse_Arrays;
with F552A00_Bingo_Balls;

procedure C552A02 is

   package Sparse_Integer_Arrays is new
     F552A00_Sparse_Arrays (Sparse_Array_Index => Natural,
                    Element_Type => Integer);

   Sparse_Data : Sparse_Integer_Arrays.Sparse_Array
     (Max_Elements => 10);

   Bingo_Game : F552A00_Bingo_Balls.Bingo_Game;

   Result : Ada.Strings.Unbounded.Unbounded_String;
   use type Ada.Strings.Unbounded.Unbounded_String;

   function Get_Constant_Sparse_Data
      return Sparse_Integer_Arrays.Sparse_Array is
   begin
      Ada.Strings.Unbounded.Append (
                                  Sparse_Integer_Arrays.TC_Call_History, "G");
      return Sparse_Data;
   end Get_Constant_Sparse_Data;

   Empty_Constant_Sparse_Data : constant Sparse_Integer_Arrays.Sparse_Array
      := Sparse_Data;

   procedure Reset_Results is
   begin

      F552A00_Bingo_Balls.TC_Call_History :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      F552A00_Bingo_Balls.TC_Used_Constant_Indexing  := False;

      Sparse_Integer_Arrays.TC_Call_History :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count := 0;
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count := 0;

      Result := Ada.Strings.Unbounded.Null_Unbounded_String;

   end Reset_Results;

begin
   Report.Test ("C552A02",
                "Check correct operation of container element iterators");

   --  Forward only container. Objectives (B).
   for Item of Bingo_Game loop
      Ada.Strings.Unbounded.Append (Result,
         ' ' & F552A00_Bingo_Balls.Bingo_Call'Image (Item));
   end loop;

   if not F552A00_Bingo_Balls.TC_Used_Constant_Indexing  then
      Report.Failed (DESCR => "1) Used incorrect indexing");
   end if;

   if Result = "" then
      Report.Failed (DESCR => "1) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   --else
   --   Report.Comment (DESCR => "1) Results:" &
   --                    Ada.Strings.Unbounded.To_String (Result));
   end if;

   if F552A00_Bingo_Balls.TC_Call_History /=
     "I1H:T( 1)N( 2)H:T( 2)N( 3)H:T( 3)N( 4)" &
     "H:T( 4)N( 5)H:T( 5)N( 6)H:T( 6)N( 7)H:T( 7)N( 8)H:T( 8)" &
     "N( 9)H:T( 9)N( 10)H:T( 10)N( 11)H:T( 11)N( 12)H:T( 12)" &
     "N( 13)H:T( 13)N( 14)H:T( 14)N( 15)H:T( 15)N( 16)H:T( 16)" &
     "N( 17)H:T( 17)N( 18)H:T( 18)N( 19)H:T( 19)N( 20)H:T( 20)" &
     "N( 21)H:T( 21)N( 22)H:T( 22)N( 23)H:T( 23)N( 24)H:T( 24)" &
     "N( 25)H:T( 25)N( 26)H:T( 26)N( 27)H:T( 27)N( 28)H:T( 28)" &
     "N( 29)H:T( 29)N( 30)H:T( 30)N( 31)H:T( 31)N( 32)H:T( 32)" &
     "N( 33)H:T( 33)N( 34)H:T( 34)N( 35)H:T( 35)N( 36)H:T( 36)" &
     "N( 37)H:T( 37)N( 38)H:T( 38)N( 39)H:T( 39)N( 40)H:T( 40)" &
     "N( 41)H:T( 41)N( 42)H:T( 42)N( 43)H:T( 43)N( 44)H:T( 44)" &
     "N( 45)H:T( 45)N( 46)H:T( 46)N( 47)H:T( 47)N( 48)H:T( 48)" &
     "N( 49)H:T( 49)N( 50)H:T( 50)N( 51)H:T( 51)N( 52)H:T( 52)" &
     "N( 53)H:T( 53)N( 54)H:T( 54)N( 55)H:T( 55)N( 56)H:T( 56)" &
     "N( 57)H:T( 57)N( 58)H:T( 58)N( 59)H:T( 59)N( 60)H:T( 60)" &
     "N( 61)H:T( 61)N( 62)H:T( 62)N( 63)H:T( 63)N( 64)H:T( 64)" &
     "N( 65)H:T( 65)N( 66)H:T( 66)N( 67)H:T( 67)N( 68)H:T( 68)" &
     "N( 69)H:T( 69)N( 70)H:T( 70)N( 71)H:T( 71)N( 72)H:T( 72)" &
     "N( 73)H:T( 73)N( 74)H:T( 74)N( 75)H:T( 75)N( 76)H:F( 76)" then
      Report.Failed (DESCR => "1) Unexpected Call History:" &
                     Ada.Strings.Unbounded.To_String
                     (F552A00_Bingo_Balls.TC_Call_History));
   end if;

   Reset_Results;

   --  Forward only container, early exit due to transfer of control.
   --  Objectives (C).
   for Item of Bingo_Game loop
      Ada.Strings.Unbounded.Append (Result,
         ' ' & F552A00_Bingo_Balls.Bingo_Call'Image (Item));
      exit;
   end loop;

   if not F552A00_Bingo_Balls.TC_Used_Constant_Indexing  then
      Report.Failed (DESCR => "2) Used incorrect indexing");
   end if;

   if Result = "" then
      Report.Failed (DESCR => "2) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   --else
   --   Report.Comment (DESCR => "2) Results:" &
   --                    Ada.Strings.Unbounded.To_String (Result));
   end if;

   if F552A00_Bingo_Balls.TC_Call_History /= "I1H:T( 1)" then
     Report.Failed (DESCR => "2) Unexpected Call History:" &
                     Ada.Strings.Unbounded.To_String
                      (F552A00_Bingo_Balls.TC_Call_History));
   end if;

   Reset_Results;

   --  Forward iteration over empty container
   --  Objectives (B) (D) (E) (J) (L).
   for Item of Sparse_Data loop
      Item := Item + 2;

      Ada.Strings.Unbounded.Append (Result,
                                    " Value = " & Integer'Image (Item));
   end loop;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 0 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 0 then

      Report.Failed
        (DESCR => "3) Used incorrect indexing, Constant=" &
           Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
           & ", Variable=" & Natural'Image
           (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Result /= "" then
      Report.Failed (DESCR => "3) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "I1H:F( 1)" then
      Report.Failed (DESCR => "3) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Reverse iteration over an empty container.
   --  Objectives (F) (H) (I) (J) (L).
   for Item of reverse Sparse_Data loop
      Item := Item + 2;

      Ada.Strings.Unbounded.Append (Result,
                                    " Value = " & Integer'Image (Item));
   end loop;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 0 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 0 then

      Report.Failed
        (DESCR => "4) Used incorrect indexing, Constant=" &
           Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
           & ", Variable=" & Natural'Image
           (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Result /= "" then
      Report.Failed (DESCR => "4) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "ILH:F( 0)" then
      Report.Failed (DESCR => "4) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   Sparse_Data.Append (Index => 1000, New_Item => -5000);
   Sparse_Data.Append (Index =>  123, New_Item => 26789);

   --  Forward iteration over container with multiple elements.
   --  Objectives (B) (E) (J) (L).
   for Item of Sparse_Data loop
      Item := Item + 2;

      Ada.Strings.Unbounded.Append (Result,
                                    " Value = " & Integer'Image (Item));
   end loop;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 0 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 2 then

      Report.Failed
        (DESCR => "5) Used incorrect indexing, Constant=" &
           Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
           & ", Variable=" & Natural'Image
           (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Result /= " Value = -4998 Value =  26791" then
      Report.Failed (DESCR => "5) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "I1H:T( 1)N( 2)H:T( 2)N( 3)H:F( 3)" then
      Report.Failed (DESCR => "5) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Iterate over a container with multiple elements, but transfer control
   --  out of the loop early. Objectives (C) (E).
   for Item of Sparse_Data loop
      Item := Item + 1;

      Ada.Strings.Unbounded.Append (Result, " Value =" & Integer'Image (Item));

      exit;

   end loop;

   if Result /= " Value =-4997" then
      Report.Failed (DESCR => "6) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 0 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 1 then

      Report.Failed
        (DESCR => "6) Used incorrect indexing, Constant=" &
           Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
           & ", Variable=" & Natural'Image
           (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "I1H:T( 1)" then
      Report.Failed (DESCR => "6) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Reverse iteration over container with multiple elements.
   --    Objectives (F) (I) (J) (L).
   for Item of reverse Sparse_Data loop
      Item := Item + 2;

      Ada.Strings.Unbounded.Append (Result,
                                    " Value = " & Integer'Image (Item));
   end loop;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 0 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 2 then

      Report.Failed
        (DESCR => "7) Used incorrect indexing, Constant=" &
           Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
           & ", Variable=" & Natural'Image
           (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Result /= " Value =  26793 Value = -4995" then
      Report.Failed (DESCR => "7) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "ILH:T( 2)P( 1)H:T( 1)P( 0)H:F( 0)" then
      Report.Failed (DESCR => "7) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Reverse Iterate over a container with multiple elements, but transfer
   --  control out of the loop early. Objectives (A) (G) (I).
   for Item of reverse Sparse_Data loop
      Item := Item + 1;

      Ada.Strings.Unbounded.Append (Result, " Value =" & Integer'Image (Item));

      exit;

   end loop;

   if Result /= " Value = 26794" then
      Report.Failed (DESCR => "8) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 0 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 1 then

      Report.Failed
        (DESCR => "8) Used incorrect indexing, Constant=" &
           Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
           & ", Variable=" & Natural'Image
           (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "ILH:T( 2)" then

      Report.Failed (DESCR => "8) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Forward iteration over constant container with multiple elements.
   --    Objectives (A) (B) (E) (K) (M).
   for Item of Get_Constant_Sparse_Data loop

      Ada.Strings.Unbounded.Append (Result,
                                 " Value = " & Integer'Image (Item));
   end loop;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 2 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 0 then

     Report.Failed
       (DESCR => "9) Used incorrect indexing, Constant=" &
          Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
          & ", Variable=" & Natural'Image
          (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Result /= " Value = -4995 Value =  26794" then
      Report.Failed (DESCR => "9) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "GI1H:T( 1)N( 2)H:T( 2)N( 3)H:F( 3)" then
      Report.Failed (DESCR => "9) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Reverse iteration over constant container with multiple elements
   --    Objectives (A) (F) (I) (K) (M).
   for Item of reverse Get_Constant_Sparse_Data loop

      Ada.Strings.Unbounded.Append (Result,
                                    " Value = " & Integer'Image (Item));
   end loop;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 2 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 0 then

     Report.Failed
       (DESCR => "A) Used incorrect indexing, Constant=" &
          Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
          & ", Variable=" & Natural'Image
          (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Result /= " Value =  26794 Value = -4995" then
      Report.Failed (DESCR => "A) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "GILH:T( 2)P( 1)H:T( 1)P( 0)H:F( 0)" then
      Report.Failed (DESCR => "A) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Forward iteration over empty constant container
   --  Objectives (B) (D) (E) (J) (L).
   for Item of Empty_Constant_Sparse_Data loop
      Ada.Strings.Unbounded.Append (Result,
                                    " Value = " & Integer'Image (Item));
   end loop;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 0 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 0 then

      Report.Failed
        (DESCR => "B) Used incorrect indexing, Constant=" &
           Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
           & ", Variable=" & Natural'Image
           (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Result /= "" then
      Report.Failed (DESCR => "B) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "I1H:F( 1)" then
      Report.Failed (DESCR => "B) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Reverse iteration over an empty container.
   --  Objectives (F) (H) (I) (J) (L).
   for Item of reverse Empty_Constant_Sparse_Data loop
      Ada.Strings.Unbounded.Append (Result,
                                    " Value = " & Integer'Image (Item));
   end loop;

   if Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count /= 0 or else
      Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count /= 0 then

      Report.Failed
        (DESCR => "C) Used incorrect indexing, Constant=" &
           Natural'Image (Sparse_Integer_Arrays.TC_Constant_Indexing_Use_Count)
           & ", Variable=" & Natural'Image
           (Sparse_Integer_Arrays.TC_Variable_Indexing_Use_Count));

   end if;

   if Result /= "" then
      Report.Failed (DESCR => "C) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "ILH:F( 0)" then
      Report.Failed (DESCR => "C) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Report.Result;

end C552A02;
