-- C552A01.A
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
--    Check correct operation of generalized iterators:
--       (A) Check that the iterator_name of a generalized iterator is
--           evaluated exactly once at the start of the loop.
--       (B) Check that the execution of a forward generalized iterator calls
--           First initially, then Next until Has_Element returns False
--           (assuming no transfer of control), executing the sequence of
--           statements each time.
--       (C) Check that the execution of a forward generalized iterator calls
--           First initially, then Next until the loop is left by a transfer of
--           control, executing the sequence of statements each time.
--       (D) Check that the execution of a forward generalized iterator never
--           iterates or calls Next if Has_Element is initially False.
--       (E) Check that the execution of a forward generalized iterator for
--           a reversible iterator type never calls Last or Previous.
--       (F) Check that the execution of a reverse generalized iterator calls
--           Last initially, then Previous until Has_Element returns False
--           (assuming no transfer of control), executing the sequence of
--           statements each time.
--       (G) Check that the execution of a reverse generalized iterator calls
--           Last initially, then Previous until the loop is left by a transfer
--           of control, executing the sequence of statements each time.
--       (H) Check that the execution of a reverse generalized iterator never
--           iterates or calls Previous if Has_Element is initially False.
--       (I) Check that the execution of a reverse generalized iterator never
--           calls First or Next.
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
with F552A00_Prime_Numbers;
with F552A00_Sparse_Arrays;

procedure C552A01 is

   package Sparse_Integer_Arrays is new
     F552A00_Sparse_Arrays (Sparse_Array_Index => Natural,
                    Element_Type => Integer);

   Sparse_Data : aliased Sparse_Integer_Arrays.Sparse_Array
     (Max_Elements => 10);

   No_Primes : F552A00_Prime_Numbers.Prime_Number_Set (Max_Value => 2);
   Primes : F552A00_Prime_Numbers.Prime_Number_Set (Max_Value => 15);

   Result : Ada.Strings.Unbounded.Unbounded_String;
   use type Ada.Strings.Unbounded.Unbounded_String;

   procedure Reset_Results is
   begin
      F552A00_Prime_Numbers.TC_Call_History :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      Sparse_Integer_Arrays.TC_Call_History :=
        Ada.Strings.Unbounded.Null_Unbounded_String;

      Result := Ada.Strings.Unbounded.Null_Unbounded_String;

   end Reset_Results;

begin
   Report.Test ("C552A01",
                "Check correct operation of generalized iterators");

   --  An empty set of prime numbers. Objectives (A) (B) (D).
   for Item in F552A00_Prime_Numbers.Iterate (No_Primes) loop
      Ada.Strings.Unbounded.Append (Result, Natural'Image (Item));
   end loop;

   if Result /= "" then
      Report.Failed (DESCR => "1) Loop statements should not have executed, " &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if F552A00_Prime_Numbers.TC_Call_History /= "I1H:F( 2)" then
      Report.Failed (DESCR => "1) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (F552A00_Prime_Numbers.TC_Call_History));
   end if;

   Reset_Results;

   --  A set of multiple prime numbers. Objectives (A) (B).
   for Item in F552A00_Prime_Numbers.Iterate (Primes) loop
      Ada.Strings.Unbounded.Append (Result, Natural'Image (Item));
   end loop;

   if Result /= " 3 5 7 11 13" then
      Report.Failed (DESCR => "2) Expected prime numbers were not found, " &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if F552A00_Prime_Numbers.TC_Call_History /=
     "I1H:T( 3)N( 5)H:T( 5)N( 7)H:T( 7)" &
     "N( 11)H:T( 11)N( 13)H:T( 13)N( 14)H:F( 14)" then

      Report.Failed (DESCR => "2) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (F552A00_Prime_Numbers.TC_Call_History));
   end if;

   Reset_Results;

   --  A set of multiple prime numbers, but exit loop due to transfer
   --  of control.  Objectives (A) (C).
   for Item in F552A00_Prime_Numbers.Iterate (Primes) loop
      Ada.Strings.Unbounded.Append (Result, Natural'Image (Item));
      exit;
   end loop;

   if Result /= " 3" then
      Report.Failed (DESCR => "2.5) Expected prime numbers were not found, " &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if F552A00_Prime_Numbers.TC_Call_History /=
     "I1H:T( 3)" then

      Report.Failed (DESCR => "2.5) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (F552A00_Prime_Numbers.TC_Call_History));
   end if;

   Reset_Results;

   --  Empty Container. Objectives (A) (B) (D) (E).
   for Item in Sparse_Data.Iterate loop
      Sparse_Data (Item) := Sparse_Data (Item) + 1;

      Ada.Strings.Unbounded.Append
        (Result,
         " Index=" & Natural'Image (Sparse_Integer_Arrays.Index_Of (Item)) &
           ", Value =" & Integer'Image (Sparse_Data (Item)));

   end loop;

   if Result /= "" then
      Report.Failed
        (DESCR => "3) Should not have iterated over an Empty Container, " &
           Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /= "I1H:F( 1)" then
      Report.Failed (DESCR => "3) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Reverse Iterate over an empty container. Objectives (A) (F) (H) (I).
   for Item in reverse Sparse_Data.Iterate loop
      Sparse_Data (Item) := Sparse_Data (Item) + 1;

      Ada.Strings.Unbounded.Append
        (Result,
         " Index=" & Natural'Image (Sparse_Integer_Arrays.Index_Of (Item)) &
           ", Value =" & Integer'Image (Sparse_Data (Item)));

   end loop;

   if Result /= "" then
      Report.Failed
        (DESCR => "4) Should not have iterated over an Empty Container" &
         Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /= "ILH:F( 0)" then
      Report.Failed (DESCR => "4) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   Sparse_Data.Append (Index => 1000, New_Item => -5000);
   Sparse_Data.Append (Index =>  123, New_Item => 26789);

   --  Iterate over a container with multiple elements. Objectives (A) (B) (E).
   for Item in Sparse_Data.Iterate loop
      Sparse_Data (Item) := Sparse_Data (Item) + 1;

      Ada.Strings.Unbounded.Append
        (Result,
         " Index=" & Natural'Image (Sparse_Integer_Arrays.Index_Of (Item)) &
           ", Value =" & Integer'Image (Sparse_Data (Item)));

   end loop;

   if Result /= " Index= 1000, Value =-4999 Index= 123, Value = 26790" then
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
   --  out of the loop early. Objectives (A) (C) (E).
   for Item in Sparse_Data.Iterate loop
      Sparse_Data (Item) := Sparse_Data (Item) + 1;

      Ada.Strings.Unbounded.Append
        (Result,
         " Index=" & Natural'Image (Sparse_Integer_Arrays.Index_Of (Item)) &
           ", Value =" & Integer'Image (Sparse_Data (Item)));

      exit;

   end loop;

   if Result /= " Index= 1000, Value =-4998" then
      Report.Failed (DESCR => "6) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "I1H:T( 1)" then
      Report.Failed (DESCR => "6) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Reset_Results;

   --  Reverse Iterate over a container with multiple elements.
   --    Objectives (A) (F) (I).
   for Item in reverse Sparse_Data.Iterate loop
      Sparse_Data (Item) := Sparse_Data (Item) + 1;

      Ada.Strings.Unbounded.Append
        (Result,
         " Index=" & Natural'Image (Sparse_Integer_Arrays.Index_Of (Item)) &
           ", Value =" & Integer'Image (Sparse_Data (Item)));

   end loop;

   if Result /= " Index= 123, Value = 26791 Index= 1000, Value =-4997" then
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
   for Item in reverse Sparse_Data.Iterate loop
      Sparse_Data (Item) := Sparse_Data (Item) + 1;

      Ada.Strings.Unbounded.Append
        (Result,
         " Index=" & Natural'Image (Sparse_Integer_Arrays.Index_Of (Item)) &
           ", Value =" & Integer'Image (Sparse_Data (Item)));

      exit;

   end loop;

   if Result /= " Index= 123, Value = 26792" then
      Report.Failed (DESCR => "8) Unexpected results," &
                       Ada.Strings.Unbounded.To_String (Result));
   end if;

   if Sparse_Integer_Arrays.TC_Call_History /=
     "ILH:T( 2)" then

      Report.Failed (DESCR => "8) Unexpected sequence of calls, " &
                       Ada.Strings.Unbounded.To_String
                       (Sparse_Integer_Arrays.TC_Call_History));
   end if;

   Report.Result;

end C552A01;
