-- CXAI036.A
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
-- OBJECTIVE:
--      Check that an implementation supports the functionality defined
--      in package Ada.Containers.Bounded_Priority_Queues.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in package Ada.Containers.Bounded_Priority_Queues.
--      Each of the subprograms is exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--      Firstly functions Current_Use and Peak_Use are called to check the
--      initial conditions.
--      Procedure Enqueue is called a number of times, each time followed by a
--      call of function Current_Use to check that the number of items queued
--      has grown as expected.
--      Procedure Dequeue is then called a number of times, each time followed
--      by a check that the item dequeued matched the item enqueued and a
--      call of function Current_Use to check that the number of items queued
--      has shrunk as expected.
--      Lastly functions Current_Use and Peak_Use are called again to check the
--      closing conditions.
--
-- CHANGE HISTORY:
--      09 May 13   JAC     Initial pre-release version.
--      10 Jul 13   JAC     Second pre-release version.
--       4 Apr 14   RLB     Renamed test to create ACATS 4.0 version.
--!
with Ada.Containers.Bounded_Priority_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Impdef;
with Report;

procedure CXAI036 is

   package My_Synchronized_Queue_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Float);

   No_Tests : constant := 10;
   No_Tests_Rounded_Up_To_Even : constant := No_Tests + No_Tests mod 2;

   function My_Get_Priority (Element : Float) return Integer is
   begin

      return Integer (Element);

   end My_Get_Priority;

   package My_Bounded_Priority_Queues is new
     Ada.Containers.Bounded_Priority_Queues
       (Queue_Interfaces => My_Synchronized_Queue_Interfaces,
        Queue_Priority   => Integer,
        Get_Priority     => My_Get_Priority,
        Before           => "<",
        Default_Capacity => No_Tests);
   -- Inserting items that are before in value before in the queue means that
   -- the more negative the value the higher the priority.

   My_Queue_1 : My_Bounded_Priority_Queues.Queue;
   My_Queue_2 : My_Bounded_Priority_Queues.Queue;
   My_Queue_3 : My_Bounded_Priority_Queues.Queue;

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. No_Tests;

   Value_In_Array  : array (Array_Bounds_Type) of Float := (others => 9.9);
   Value_Out_Array : array (Array_Bounds_Type) of Float := (others => 0.0);

   procedure Initialise_In_Items is
   begin

      -- Elements with an odd index are filled with negative elements of
      -- increasing magnitude and elements with an even index are filled with
      -- positive elements of increasing magnitude.

      for I in Array_Bounds_Type loop

         Value_In_Array (I) := (-1.2345) ** Integer (I);

      end loop;

   end Initialise_In_Items;

   use type Ada.Containers.Count_Type;

   procedure Enqueue_Items is
   begin

      for I in Array_Bounds_Type loop

         My_Queue_2.Enqueue
           (New_Item => Value_In_Array (I));

         if My_Queue_2.Current_Use /= I then

            Report.Failed ("Wrong Current_Use when enqueuing");

         end if;

      end loop;

   end Enqueue_Items;

   My_Success : Boolean;


   Reader_1_Blocked : Boolean := False;

   task Reader_1;

   task body Reader_1 is

      Value_Out : Float := 0.0;

   begin

      Reader_1_Blocked := True;

      My_Queue_1.Dequeue (Element => Value_Out);

      Reader_1_Blocked := False;

   end Reader_1;


   Writer_3_Blocked : Boolean := False;

   task Writer_3;

   task body Writer_3 is

      Value_In : constant Float := 1.2;

   begin

      for I in Array_Bounds_Type loop

         My_Queue_3.Enqueue (New_Item => Value_In);

      end loop;

      Writer_3_Blocked := True;

      My_Queue_3.Enqueue (New_Item => Value_In);

      Writer_3_Blocked := False;

   end Writer_3;

begin

   Report.Test
     ("CXAI036",
      "Check that an implementation supports the functionality defined in " &
      "package Ada.Containers.Bounded_Priority_Queues");


   -- Check blocking of reader.

   -- Give time for Reader_1 to activate.

   delay Impdef.Switch_To_New_Task;

   if not Reader_1_Blocked then

      Report.Failed ("Dequeue of empty queue didn't block");

   end if;


   -- Check unblocking of reader by writer.

   declare

      task Writer_1;

      task body Writer_1 is

         Value_In : constant Float := 1.2;

      begin

         My_Queue_1.Enqueue (New_Item => Value_In);

      end Writer_1;

   begin

      -- Give time for Writer_1 to activate;

      delay Impdef.Switch_To_New_Task;

      if Reader_1_Blocked then

         Report.Failed ("Enqueue to empty queue didn't unblock reader");

      end if;

   end;


   -- Check blocking of writer.

   -- Give time for Writer_3 to activate.

   delay Impdef.Switch_To_New_Task;

   if not Writer_3_Blocked then

      Report.Failed ("Enqueue to full queue didn't block");

   end if;


   -- Check unblocking of writer by reader.

   declare

      task Reader_3;

      task body Reader_3 is

         Value_Out : Float := 0.0;

      begin

         My_Queue_3.Dequeue (Element => Value_Out);

      end Reader_3;

   begin

      -- Give time for Reader_3 to activate;

      delay Impdef.Switch_To_New_Task;

      if Writer_3_Blocked then

         Report.Failed ("Dequeue of full queue didn't unblock writer");

      end if;

   end;


   -- Check that the same values come out as went in, and in sorted order.

   if My_Queue_2.Current_Use /= 0 then

      Report.Failed ("Wrong initial Current_Use");

   end if;

   if My_Queue_2.Peak_Use /= 0 then

      Report.Failed ("Wrong initial Peak_Use");

   end if;

   Initialise_In_Items;

   Enqueue_Items;

   for I in Array_Bounds_Type loop

      My_Queue_2.Dequeue
        (Element => Value_Out_Array (I));

      if I <= No_Tests_Rounded_Up_To_Even / 2 then

         -- The negative elements should be extracted first, in reverse order of
         -- enqueuing.

         if Value_Out_Array (I) /=
           Value_In_Array (No_Tests_Rounded_Up_To_Even + 1 - 2 * I) then

            Report.Failed ("Mismatch between dequeued and what was enqueued");

         end if;

      else -- I > No_Tests_Rounded_Up_To_Even / 2

         -- The positive elements should be extracted last, in forward order of
         -- enqueuing.

         if Value_Out_Array (I) /=
           Value_In_Array (2 * I - No_Tests_Rounded_Up_To_Even) then

            Report.Failed ("Mismatch between dequeued and what was enqueued");

         end if;

      end if;

      if My_Queue_2.Current_Use /= No_Tests - I then

         Report.Failed ("Wrong Current_Use when dequeuing");

      end if;

   end loop;

   if My_Queue_2.Current_Use /= 0 then

      Report.Failed ("Wrong intermediate Current_Use");

   end if;


   -- Check that the same high priority values come out as went in, and in
   -- sorted order.

   Initialise_In_Items;

   Enqueue_Items;

   for I in Array_Bounds_Type range 1 .. No_Tests_Rounded_Up_To_Even / 2 loop

      My_Queue_2.Dequeue_Only_High_Priority
        (At_Least => 0,
         Element  => Value_Out_Array (I),
         Success  => My_Success);

      -- The negative elements should be extracted first, in reverse order of
      -- enqueuing.

      if Value_Out_Array (I) /=
        Value_In_Array (No_Tests_Rounded_Up_To_Even + 1 - 2 * I) then

         Report.Failed ("Mismatch between dequeued and what was enqueued");

      end if;

      if not My_Success then

         Report.Failed ("Dequeue_Only_High_Priority unsuccessful");

      end if;

      if My_Queue_2.Current_Use /= No_Tests - I then

         Report.Failed ("Wrong Current_Use when dequeuing");

      end if;

   end loop;


   -- There shouldn't be any high priority items left.

   My_Queue_2.Dequeue_Only_High_Priority
     (At_Least => 0,
      Element  => Value_Out_Array (1),
      Success  => My_Success);

   if My_Success then

      Report.Failed ("Dequeue_Only_High_Priority shouldn't have succeeded");

   end if;

   if My_Queue_2.Current_Use /= No_Tests / 2 then

      Report.Failed ("Wrong final Current_Use");

   end if;

   if My_Queue_2.Peak_Use /= No_Tests then

      Report.Failed ("Wrong final Peak_Use");

   end if;


   Report.Result;

end CXAI036;
