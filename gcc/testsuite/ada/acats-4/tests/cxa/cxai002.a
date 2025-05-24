-- CXAI002.A
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
--      in package Ada.Containers.Doubly_Linked_Lists.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in package Ada.Containers.Doubly_Linked_Lists.
--      Each of the subprograms is exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      23 Sep 13   JAC     Initial pre-release version.
--      30 Dec 13   RLB     Created Amendment version, marked and commented out
--                          Ada 2012 features.
--      10 Mar 14   RLB     Created ACATS 4.0 version, replaced Ada 2012
--                          features.
--      31 Mar 14   RLB     Merged in tampering checks from third pre-release
--                          version.
--!
with Ada.Containers.Doubly_Linked_Lists;
with Report;

procedure CXAI002 is

   My_Default_Value : constant := 999.0;

   type My_Float is new Float
     with Default_Value => My_Default_Value;

   package My_Doubly_Linked_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => My_Float); -- Default =

   package My_Sorting is new
     My_Doubly_Linked_Lists.Generic_Sorting
       ("<" => ">"); -- Sort in reverse order to check is using what specified
                     -- not simply <

   My_List_1 : My_Doubly_Linked_Lists.List;
   My_List_2 : My_Doubly_Linked_Lists.List;

   Num_Tests : constant := 10;

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. Num_Tests;

   -- No fractional parts so that can compare values for equality

   Value_In_Array  : constant array (Array_Bounds_Type) of My_Float :=
     (12.0, 23.0, 34.0, 45.0, 56.0, 67.0, 78.0, 89.0, 90.0, 1.0);

   My_Cursor_1 : My_Doubly_Linked_Lists.Cursor;
   My_Cursor_2 : My_Doubly_Linked_Lists.Cursor;
   My_Cursor_3 : My_Doubly_Linked_Lists.Cursor;

   procedure Tampering_Check
     (Container : in out My_Doubly_Linked_Lists.List;
      Where     : in     String) is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Container.Append (New_Item => Value_In_Array (1));

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Doubly_Linked_Lists.List;


begin

   Report.Test
     ("CXAI002",
      "Check that an implementation supports the functionality defined in " &
      "package Ada.Containers.Doubly_Linked_Lists");


   -- Test empty using Empty_List, Is_Empty and Length

   if My_List_1 /= My_Doubly_Linked_Lists.Empty_List then

      Report.Failed ("Not initially empty #1");

   end if;

   if not My_List_1.Is_Empty then

      Report.Failed ("Not initially empty #2");

   end if;

   if My_List_1.Length /= 0 then

      Report.Failed ("Not initially empty #3");

   end if;


   -- Test Append, First, Element, Query_Element, Next (two forms) and
   -- First_Element

   for I in Array_Bounds_Type loop

      My_List_1.Append (New_Item => Value_In_Array (I));

      if My_List_1.Length /= I then

         Report.Failed ("Wrong Length after appending");

      end if;

   end loop;

   My_Cursor_1 := My_List_1.First;

   for I in Array_Bounds_Type loop

      declare

         procedure My_Query (Element : in My_Float) is
         begin

            Tampering_Check
              (Container => My_List_1,
               Where     => "Query_Element");

            if Element /= Value_In_Array (I) then

               Report.Failed
                 ("Mismatch between element and what was appended #1");

            end if;

         end My_Query;

      begin

         if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
           Value_In_Array (I) then

            Report.Failed ("Mismatch between element and what was appended #2");

         end if;

         My_Doubly_Linked_Lists.Query_Element
           (Position => My_Cursor_1,
            Process  => My_Query'Access);

      end;

      -- Toggle between alternative methods for incrementing cursor

      if I mod 2 = 0 then

         My_Cursor_1 := My_Doubly_Linked_Lists.Next (My_Cursor_1);

      else

         My_Doubly_Linked_Lists.Next (My_Cursor_1);

      end if;

   end loop;

   if My_List_1.First_Element /= Value_In_Array (Value_In_Array'First) then

      Report.Failed ("Mismatch between first element and first appended");

   end if;


   -- Test Prepend, Last, Element, Previous (two forms) and Last_Element

   for I in reverse Array_Bounds_Type loop

      My_List_2.Prepend (New_Item => Value_In_Array (I));

      if My_List_2.Length /= Num_Tests - I + 1 then

         Report.Failed ("Wrong Length after prepending");

      end if;

   end loop;

   My_Cursor_2 := My_List_2.Last;

   for I in reverse Array_Bounds_Type loop

      if My_Doubly_Linked_Lists.Element (Position => My_Cursor_2) /=
        Value_In_Array (I) then

         Report.Failed ("Mismatch between element and what was prepended");

      end if;

      -- Toggle between alternative methods for decrementing cursor

      if I mod 2 = 0 then

         My_Cursor_2 := My_Doubly_Linked_Lists.Previous
                          (Position => My_Cursor_2);

      else

         My_Doubly_Linked_Lists.Previous (Position => My_Cursor_2);

      end if;

   end loop;

   if My_List_1.Last_Element /= Value_In_Array (Value_In_Array'Last) then

      Report.Failed ("Mismatch between last element and last prepended");

   end if;


   -- Test equality

   if My_List_1 /= My_List_2 then

      Report.Failed ("Lists not equal");

   end if;


   -- Test assignment, Iterate and Reverse_Iterate

   declare

      My_List_3 : My_Doubly_Linked_Lists.List := My_List_1;

      I : Array_Bounds_Type := Array_Bounds_Type'First;

      procedure My_Process
        (Position : in My_Doubly_Linked_Lists.Cursor) is
      begin

         Tampering_Check
           (Container => My_List_3,
            Where     => "Iterate");

         if My_Doubly_Linked_Lists.Element (Position) /= Value_In_Array (I) then

            Report.Failed ("Iterate hasn't found the expected value");

         end if;

         if I < Array_Bounds_Type'Last then

            I := I + 1;

         end if;

      end My_Process;

      procedure My_Reverse_Process
        (Position : in My_Doubly_Linked_Lists.Cursor) is
      begin

         Tampering_Check
           (Container => My_List_3,
            Where     => "Reverse_Iterate");

         if My_Doubly_Linked_Lists.Element (Position) /= Value_In_Array (I) then

            Report.Failed ("Reverse_Iterate hasn't found the expected value");

         end if;

         if I > Array_Bounds_Type'First then

            I := I - 1;

         end if;

      end My_Reverse_Process;

   begin

      My_List_3.Iterate (Process => My_Process'Access);

      My_List_3.Reverse_Iterate (Process => My_Reverse_Process'Access);

   end;


   -- Test Replace_Element and Update_Element

   -- Double the values of the two lists by two different methods and check
   -- still equal

   My_Cursor_1 := My_List_1.First;
   My_Cursor_2 := My_List_2.First;

   for I in Array_Bounds_Type loop

      declare

         procedure My_Update (Element : in out My_Float) is
         begin

            Tampering_Check
              (Container => My_List_2,
               Where     => "Update_Element");

            Element := Element * 2.0;

         end My_Update;

      begin

         My_List_1.Replace_Element
           (Position => My_Cursor_1,
            New_Item => Value_In_Array (I) * 2.0);

         My_List_2.Update_Element
           (Position => My_Cursor_2,
            Process  => My_Update'Access);

      end;

      My_Doubly_Linked_Lists.Next (My_Cursor_1);
      My_Doubly_Linked_Lists.Next (My_Cursor_2);

   end loop;

   if My_List_1 /= My_List_2 then

      Report.Failed ("Doubled lists not equal");

   end if;


   -- Test Clear, Reverse_Elements and inequality

   My_List_1.Clear;

   if not My_List_1.Is_Empty then

      Report.Failed ("Failed to clear");

   end if;

   for I in Array_Bounds_Type loop

      My_List_1.Append (New_Item => Value_In_Array (I));

   end loop;

   My_List_1.Reverse_Elements;

   My_Cursor_1 := My_List_1.First;

   for I in Array_Bounds_Type loop

      if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
        Value_In_Array (Num_Tests - I + 1) then

         Report.Failed ("Reversed array not as expected");

      end if;

      My_Doubly_Linked_Lists.Next (My_Cursor_1);

   end loop;

   if My_List_1 = My_List_2 then

      Report.Failed ("Different lists equal");

   end if;


   -- Test Move.  Target has the test values in reverse order, after Move these
   -- should be replaced (not appended) by the test values in forward order

   My_List_2.Clear;

   for I in Array_Bounds_Type loop

      My_List_2.Append (New_Item => Value_In_Array (I));

   end loop;

   My_List_1.Move (Source => My_List_2);

   if not My_List_2.Is_Empty then

      Report.Failed ("Moved source not empty");

   end if;

   My_Cursor_1 := My_List_1.First;

   for I in Array_Bounds_Type loop

      if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
        Value_In_Array (I) then

         Report.Failed ("Target list not as expected after move");

      end if;

      My_Doubly_Linked_Lists.Next (My_Cursor_1);

   end loop;


   -- Test Insert (three forms; using different counts including default),
   -- Swap_Links and Swap

   -- My_List_2 should initially be empty

   My_List_2.Insert
     (Before   => My_Doubly_Linked_Lists.No_Element, -- At end
      New_Item => Value_In_Array (1)); -- Count should default to 1

   My_Cursor_1 := My_List_2.Last; -- Should point to element containing
                                  -- Value_In_Array (1)

   My_List_2.Insert
     (Before   => My_Doubly_Linked_Lists.No_Element, -- At end
      New_Item => Value_In_Array (2),
      Position => My_Cursor_2, -- First of added elements
      Count    => 2);

   -- Elements with Default_Value.  Should insert in-between the previous two
   -- blocks
   My_List_2.Insert
     (Before   => My_Cursor_2,
      Position => My_Cursor_2, -- First of added elements
      Count    => 3);

   -- The order should now be Value_In_Array (1), Default_Value, Default_Value,
   -- Default_Value, Value_In_Array (2), Value_In_Array (2)

   -- This swaps the order of the elements
   My_List_2.Swap_Links
     (I => My_List_2.First,
      J => My_Cursor_2); -- First of added elements

   -- The order should now be Default_Value, Value_In_Array (1), Default_Value,
   -- Default_Value, Value_In_Array (2), Value_In_Array (2)

   -- This exchanges the values between the elements
   My_List_2.Swap
     (I => My_List_2.Last,
      J => My_Doubly_Linked_Lists.Previous (My_Doubly_Linked_Lists.Previous
            (My_List_2.Last)));

   -- The order should now be Default_Value, Value_In_Array (1), Default_Value,
   -- Value_In_Array (2), Value_In_Array (2), Default_Value

   My_Cursor_2 := My_List_2.First;

   -- Check = Default_Value
   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #1");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_2);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_2) /=
     Value_In_Array (1) then

      Report.Failed ("Inserted value not as expected #2");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_2);

   -- Check = Default_Value
   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #3");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_2);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_2) /=
     Value_In_Array (2) then

      Report.Failed ("Inserted value not as expected #4");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_2);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_2) /=
     Value_In_Array (2) then

      Report.Failed ("Inserted value not as expected #5");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_2);

   -- Check = Default_Value
   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #6");

   end if;

   -- For a Doubly_Linked_List (but not necessarily for a vector) a cursor
   -- should stay pointing to the same element even if the order has changed,
   -- e.g. by insertions earlier in the list or swapping

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (1) then

      Report.Failed
        ("Cursor no longer pointing to same element after shuffling about");

   end if;


   -- Test Delete, Delete_First and Delete_Last (using different counts
   -- including default)

   -- My_Cursor_2 should initially be pointing to the last element of My_List_2

   My_List_2.Delete (Position => My_Cursor_2); -- Count should default to 1

   My_List_2.Delete_First (Count => 2);

   My_List_2.Delete_Last (Count => 2);

   if My_List_2.Length /= 1 then

      Report.Failed ("Wrong Length after deleting");

   end if;

   -- Check = Default_Value
   if My_Doubly_Linked_Lists.Element (My_List_2.First) /= My_Default_Value then

      Report.Failed ("Remaining value not as expected");

   end if;


   -- Test Find, Splice (three forms) and Reverse_Find

   -- My_List_1 should still contain the test values (in forward order), and
   -- My_List_2 a single element of the Default_Value

   My_Cursor_1 := My_List_1.Find (Item => Value_In_Array (3));

   My_List_1.Splice
     (Before => My_Cursor_1,
      Source => My_List_2);

   -- The order should now be Value_In_Array (1), Value_In_Array (2),
   -- Default_Value, Value_In_Array (3), Value_In_Array (4), Value_In_Array (5),
   -- Value_In_Array (6), Value_In_Array (7), Value_In_Array (8),
   -- Value_In_Array (9), Value_In_Array (10)

   -- My_List_2 should now be empty so re-fill

   for I in Array_Bounds_Type loop

      My_List_2.Append (New_Item => Value_In_Array (I));

   end loop;

   My_Cursor_1 := My_List_1.Reverse_Find (Item => Value_In_Array (5));

   My_Cursor_2 := My_List_2.Find (Item => Value_In_Array (7));

   My_List_1.Splice
     (Before   => My_Cursor_1,
      Source   => My_List_2,
      Position => My_Cursor_2);

   -- The order should now be Value_In_Array (1), Value_In_Array (2),
   -- Default_Value, Value_In_Array (3), Value_In_Array (4), Value_In_Array (7),
   -- Value_In_Array (5), Value_In_Array (6), Value_In_Array (7),
   -- Value_In_Array (8), Value_In_Array (9), Value_In_Array (10)

   My_Cursor_1 := My_List_1.Find (Item => Value_In_Array (9));

   My_Cursor_3 := My_List_1.Find (Item => Value_In_Array (2));

   My_List_1.Splice
     (Before   => My_Cursor_3,
      Position => My_Cursor_1);

   -- The order should now be Value_In_Array (1), Value_In_Array (9),
   -- Value_In_Array (2), Default_Value, Value_In_Array (3), Value_In_Array (4),
   -- Value_In_Array (7), Value_In_Array (5), Value_In_Array (6),
   -- Value_In_Array (7), Value_In_Array (8), Value_In_Array (10)

   My_Cursor_1 := My_List_1.First;

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (1) then

      Report.Failed ("Spliced value not as expected #1");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (9) then

      Report.Failed ("Spliced value not as expected #2");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (2) then

      Report.Failed ("Spliced value not as expected #3");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   -- Check = Default_Value
   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     My_Default_Value then

      Report.Failed ("Spliced value not as expected #4");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (3) then

      Report.Failed ("Spliced value not as expected #5");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (4) then

      Report.Failed ("Spliced value not as expected #6");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (7) then

      Report.Failed ("Spliced value not as expected #7");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (5) then

      Report.Failed ("Spliced value not as expected #8");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (6) then

      Report.Failed ("Spliced value not as expected #9");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (7) then

      Report.Failed ("Spliced value not as expected #10");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (8) then

      Report.Failed ("Spliced value not as expected #11");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   if My_Doubly_Linked_Lists.Element (Position => My_Cursor_1) /=
     Value_In_Array (10) then

      Report.Failed ("Spliced value not as expected #12");

   end if;


   -- Test Contains

   if not My_List_1.Contains (Item => Value_In_Array (8)) then

      Report.Failed ("Contains failed to find");

   end if;

   if My_List_1.Contains (Item => 0.0) then

      Report.Failed ("Contains found when shouldn't have");

   end if;


   -- Test Has_Element

   -- My_Cursor_1 should still be pointing to the last element

   if not My_Doubly_Linked_Lists.Has_Element (Position => My_Cursor_1) then

      Report.Failed ("Has_Element failed to find");

   end if;

   My_Doubly_Linked_Lists.Next (My_Cursor_1);

   -- My_Cursor_1 should now be pointing off the end

   if My_Doubly_Linked_Lists.Has_Element (Position => My_Cursor_1) then

      Report.Failed ("Has_Element found when shouldn't have");

   end if;


   -- Test Generic_Sorting, Assign and Copy

   if My_Sorting.Is_Sorted (Container => My_List_1) then

      Report.Failed ("Thinks is sorted when isn't");

   end if;

   My_Sorting.Sort (Container => My_List_1);

   if not My_Sorting.Is_Sorted (Container => My_List_1) then

      Report.Failed ("Thinks isn't sorted after Sort when should be");

   end if;

   My_List_2.Assign (Source => My_List_1);

   My_Sorting.Merge
     (Source => My_List_2,
      Target => My_List_1);

   if not My_Sorting.Is_Sorted (Container => My_List_1) then

      Report.Failed ("Thinks isn't sorted after Merge when should be");

   end if;

   if My_List_1.Length /= 2 * Num_Tests + 4 then

      Report.Failed ("Target list hasn't grown as expected after Merge");

   end if;

   if My_List_2.Length /= 0 then

      Report.Failed ("Source list length not 0 after Merge");

   end if;

   My_List_2 := My_Doubly_Linked_Lists.Copy (Source => My_List_1);

   if My_List_2.Length /= 2 * Num_Tests + 4 then

      Report.Failed ("Result length not as expected after Copy");

   end if;

   if My_List_1.Length /= 2 * Num_Tests + 4 then

      Report.Failed ("Source length not left alone by Copy");

   end if;


   Report.Result;

end CXAI002;
