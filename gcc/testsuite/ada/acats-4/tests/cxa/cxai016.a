-- CXAI016.A
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
--      in package Ada.Containers.Bounded_Multiway_Trees.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in package Ada.Containers.Bounded_Multiway_Trees.
--      Each of the subprograms is exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      23 Sep 13   JAC     Initial pre-release version.
--       6 Dec 13   JAC     Second pre-release version.
--      28 Mar 14   RLB     Created ACATS 4.0 version, renamed test.
--                          Added additional capacity error cases.
--       3 Apr 14   RLB     Merged in tampering checks from fourth pre-release
--                          version.
--
--!
with Ada.Containers.Bounded_Multiway_Trees;
with Report;
with Ada.Exceptions;

procedure CXAI016 is

   My_Default_Value : constant := 999.0;

   type My_Float is new Float
     with Default_Value => My_Default_Value;

   package My_Bounded_Multiway_Trees is new
     Ada.Containers.Bounded_Multiway_Trees
       (Element_Type => My_Float); -- Default =

   Num_Tests : constant := 10;

   Capacity_Reqd : constant := 2 * Num_Tests + 4;

   My_Tree_1 : My_Bounded_Multiway_Trees.Tree (Capacity => Capacity_Reqd);
   My_Tree_2 : My_Bounded_Multiway_Trees.Tree (Capacity => Capacity_Reqd);
   Big_Tree  : My_Bounded_Multiway_Trees.Tree (Capacity => Capacity_Reqd);

   -- Test Root

   Root_1   : constant My_Bounded_Multiway_Trees.Cursor := My_Tree_1.Root;
   Root_2   : constant My_Bounded_Multiway_Trees.Cursor := My_Tree_2.Root;
   Big_Root : constant My_Bounded_Multiway_Trees.Cursor := Big_Tree.Root;

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. Num_Tests;

   -- No fractional parts so that can compare values for equality.

   Value_In_Array  : constant array (Array_Bounds_Type) of My_Float :=
     (12.0, 23.0, 34.0, 45.0, 56.0, 67.0, 78.0, 89.0, 90.0, 1.0);

   First_Child_Cursor_1      : My_Bounded_Multiway_Trees.Cursor;
   First_Grandchild_Cursor_1 : My_Bounded_Multiway_Trees.Cursor;
   Greatgrandchild_Cursor_1  : My_Bounded_Multiway_Trees.Cursor;
   Last_Child_Cursor_1       : My_Bounded_Multiway_Trees.Cursor;
   My_Cursor_1               : My_Bounded_Multiway_Trees.Cursor;
   My_Cursor_2               : My_Bounded_Multiway_Trees.Cursor;
   Second_Child_Cursor_1     : My_Bounded_Multiway_Trees.Cursor;

   procedure Tampering_Check
     (Container : in out My_Bounded_Multiway_Trees.Tree;
      Where     : in     String)
     with Pre => not Container.Is_Empty is

      My_Cursor : My_Bounded_Multiway_Trees.Cursor :=
                    My_Bounded_Multiway_Trees.First_Child
                      (Parent => Container.Root);

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Don't try to insert into a full bounded container as it's a grey
         -- area as to whether Capacity_Error or Program_Error should be raised

         Container.Delete_Leaf (Position => My_Cursor);

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Bounded_Multiway_Trees.Cursor;
   use type My_Bounded_Multiway_Trees.Tree;


begin

   Report.Test
     ("CXAI016",
      "Check that an implementation supports the functionality defined in " &
      "package Ada.Containers.Bounded_Multiway_Trees");


   -- Test empty using Empty_Tree, Is_Empty and Node_Count

   if My_Tree_1 /= My_Bounded_Multiway_Trees.Empty_Tree then

      Report.Failed ("Not initially empty #1");

   end if;

   if not My_Tree_1.Is_Empty then

      Report.Failed ("Not initially empty #2");

   end if;

   if My_Tree_1.Node_Count /= 1 then -- Even if empty has a root node

      Report.Failed ("Not initially empty #3");

   end if;


   -- Test Append_Child, Child_Count, First_Child, Is_Root, Is_Leaf, Parent,
   -- Element, Query_Element, Next (two forms) and First_Child_Element

   for I in Array_Bounds_Type loop

      My_Tree_1.Append_Child
        (Parent   => Root_1,
         New_Item => Value_In_Array (I));

      if My_Tree_1.Node_Count /= I + 1 then -- Add 1 for root node

         Report.Failed ("Wrong node count after appending");

      end if;

      if My_Bounded_Multiway_Trees.Child_Count (Parent => Root_1) /= I then
      -- Excludes parent and grandchildren

         Report.Failed ("Wrong child count after appending");

      end if;

   end loop;

   My_Cursor_1 := My_Bounded_Multiway_Trees.First_Child (Parent => Root_1);

   if not My_Bounded_Multiway_Trees.Is_Root (Position => Root_1) then

      Report.Failed ("Thinks root isn't root");

   end if;

   if My_Bounded_Multiway_Trees.Is_Root (Position => My_Cursor_1) then

      Report.Failed ("Thinks leaf is root");

   end if;

   if My_Bounded_Multiway_Trees.Is_Leaf (Position => Root_1) then

      Report.Failed ("Thinks root is leaf");

   end if;

   if not My_Bounded_Multiway_Trees.Is_Leaf (Position => My_Cursor_1) then

      Report.Failed ("Thinks leaf isn't leaf");

   end if;

   if My_Bounded_Multiway_Trees.Parent (Position => My_Cursor_1) /= Root_1 then

      Report.Failed ("Thinks root isn't parent of leaf");

   end if;

   if My_Bounded_Multiway_Trees.Parent (Position => Root_1) = My_Cursor_1 then

      Report.Failed ("Thinks parent of root is leaf");

   end if;

   for I in Array_Bounds_Type loop

      declare

         procedure My_Query (Element : in My_Float) is
         begin

            Tampering_Check
              (Container => My_Tree_1,
               Where     => "Query_Element");

            if Element /= Value_In_Array (I) then

               Report.Failed
                 ("Mismatch between element and what was appended #1");

            end if;

         end My_Query;

      begin

         if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
           Value_In_Array (I) then

            Report.Failed ("Mismatch between element and what was appended #2");

         end if;

         My_Bounded_Multiway_Trees.Query_Element
           (Position => My_Cursor_1,
            Process  => My_Query'Access);

      end;

      -- Toggle between alternative methods for incrementing cursor

      if I mod 2 = 0 then

         My_Cursor_1 := My_Bounded_Multiway_Trees.Next_Sibling (My_Cursor_1);

      else

         My_Bounded_Multiway_Trees.Next_Sibling (My_Cursor_1);

      end if;

   end loop;

   if My_Bounded_Multiway_Trees.First_Child_Element (Parent => Root_1) /=
     Value_In_Array (Value_In_Array'First) then

      Report.Failed ("Mismatch between first element and first appended");

   end if;


   -- Test Prepend_Child, Last_Child, Element, Previous (two forms) and
   -- Last_Child_Element

   for I in reverse Array_Bounds_Type loop

      My_Tree_2.Prepend_Child
        (Parent   => Root_2,
         New_Item => Value_In_Array (I));

      -- Add an extra one for root node
      if My_Tree_2.Node_Count /= Num_Tests - I + 2 then
         Report.Failed ("Wrong node count after prepending");

      end if;

      if My_Bounded_Multiway_Trees.Child_Count (Parent => Root_2) /=
        Num_Tests - I + 1 then
      -- Excludes parent and grandchildren

         Report.Failed ("Wrong child count after prepending");

      end if;

   end loop;

   My_Cursor_2 := My_Bounded_Multiway_Trees.Last_Child (Parent => Root_2);

   for I in reverse Array_Bounds_Type loop

      if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_2) /=
        Value_In_Array (I) then

         Report.Failed ("Mismatch between element and what was prepended");

      end if;

      -- Toggle between alternative methods for decrementing cursor

      if I mod 2 = 0 then

         My_Cursor_2 := My_Bounded_Multiway_Trees.Previous_Sibling
                          (Position => My_Cursor_2);

      else

         My_Bounded_Multiway_Trees.Previous_Sibling (Position => My_Cursor_2);

      end if;

   end loop;

   if My_Bounded_Multiway_Trees.Last_Child_Element (Parent => Root_1) /=
     Value_In_Array (Value_In_Array'Last) then

      Report.Failed ("Mismatch between last element and last prepended");

   end if;


   -- Test equality

   if My_Tree_1 /= My_Tree_2 then

      Report.Failed ("Trees not equal");

   end if;


   -- Test assignment, Iterate and Reverse_Iterate_Children

   declare

      My_Tree_3 : My_Bounded_Multiway_Trees.Tree := My_Tree_1;

      I : Array_Bounds_Type := Array_Bounds_Type'First;

      procedure My_Process
        (Position : in My_Bounded_Multiway_Trees.Cursor) is
      begin

         Tampering_Check
           (Container => My_Tree_3,
            Where     => "Iterate");

         if My_Bounded_Multiway_Trees.Element (Position) /= Value_In_Array (I)
           then

            Report.Failed ("Iterate hasn't found the expected value");

         end if;

         if I < Array_Bounds_Type'Last then

            I := I + 1;

         end if;

      end My_Process;

      procedure My_Reverse_Process
        (Position : in My_Bounded_Multiway_Trees.Cursor) is
      begin

         Tampering_Check
           (Container => My_Tree_3,
            Where     => "Reverse_Iterate");

         if My_Bounded_Multiway_Trees.Element (Position) /= Value_In_Array (I)
           then

            Report.Failed ("Reverse_Iterate hasn't found the expected value");

         end if;

         if I > Array_Bounds_Type'First then

            I := I - 1;

         end if;

      end My_Reverse_Process;

   begin

      My_Tree_3.Iterate (Process => My_Process'Access);

      My_Bounded_Multiway_Trees.Reverse_Iterate_Children
        (Parent  => My_Tree_3.Root,
         Process => My_Reverse_Process'Access);

   end;


   -- Test Replace_Element and Update_Element

   -- Double the values of the two trees by two different methods and check
   -- still equal

   My_Cursor_1 := My_Bounded_Multiway_Trees.First_Child (Parent => Root_1);
   My_Cursor_2 := My_Bounded_Multiway_Trees.First_Child (Parent => Root_2);

   for I in Array_Bounds_Type loop

      declare

         procedure My_Update (Element : in out My_Float) is
         begin

            Tampering_Check
              (Container => My_Tree_2,
               Where     => "Update_Element");

            Element := Element * 2.0;

         end My_Update;

      begin

         My_Tree_1.Replace_Element
           (Position => My_Cursor_1,
            New_Item => Value_In_Array (I) * 2.0);

         My_Tree_2.Update_Element
           (Position => My_Cursor_2,
            Process  => My_Update'Access);

      end;

      My_Bounded_Multiway_Trees.Next_Sibling (My_Cursor_1);
      My_Bounded_Multiway_Trees.Next_Sibling (My_Cursor_2);

   end loop;

   if My_Tree_1 /= My_Tree_2 then

      Report.Failed ("Doubled trees not equal");

   end if;


   -- Test Clear and inequality

   My_Tree_1.Clear;

   if not My_Tree_1.Is_Empty then

      Report.Failed ("Failed to clear");

   end if;

   -- Reverse order

   for I in Array_Bounds_Type loop

      My_Tree_1.Prepend_Child
        (Parent   => Root_1,
         New_Item => Value_In_Array (I));

   end loop;

   if My_Tree_1 = My_Tree_2 then

      Report.Failed ("Different trees equal");

   end if;


   -- Test Move.  Target has the test values in reverse order, after Move these
   -- should be replaced (not appended) by the test values in forward order

   My_Tree_2.Clear;

   for I in Array_Bounds_Type loop

      My_Tree_2.Append_Child
        (Parent   => Root_2,
         New_Item => Value_In_Array (I));

   end loop;

   My_Tree_1.Move (Source => My_Tree_2);

   if not My_Tree_2.Is_Empty then

      Report.Failed ("Moved source not empty");

   end if;

   My_Cursor_1 := My_Bounded_Multiway_Trees.First_Child (Parent => Root_1);

   for I in Array_Bounds_Type loop

      if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
        Value_In_Array (I) then

         Report.Failed ("Target tree not as expected after move");

      end if;

      My_Bounded_Multiway_Trees.Next_Sibling (My_Cursor_1);

   end loop;


   -- Test Insert_Child (three forms; using different counts including default)
   -- and Swap

   -- My_Tree_2 should initially be empty

   My_Tree_2.Insert_Child
     (Parent   => Root_2,
      Before   => My_Bounded_Multiway_Trees.No_Element, -- At end
      New_Item => Value_In_Array (1)); -- Count should default to 1

   My_Cursor_1 := My_Bounded_Multiway_Trees.Last_Child (Parent => Root_2);
   -- Should point to element containing Value_In_Array (1)

   My_Tree_2.Insert_Child
     (Parent   => Root_2,
      Before   => My_Bounded_Multiway_Trees.No_Element, -- At end
      New_Item => Value_In_Array (2),
      Position => My_Cursor_2, -- First of added elements
      Count    => 2);

   -- Elements with Default_Value.  Should insert in-between the previous two
   -- blocks
   My_Tree_2.Insert_Child
     (Parent   => Root_2,
      Before   => My_Cursor_2,
      Position => My_Cursor_2, -- First of added elements
      Count    => 3);

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Elements with Default_Value
         My_Tree_2.Insert_Child
           (Parent   => Root_2,
            Before   => My_Cursor_2,
            Position => My_Cursor_2, -- First of added elements
            Count    => Capacity_Reqd);

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

         when others =>

            Report.Failed
              ("Wrong exception raised when inserting child beyond capacity");

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Inserting child beyond capacity should have failed with " &
            "Capacity_Error");

      end if;

   end;

   -- The order should now be Value_In_Array (1), Default_Value, Default_Value,
   -- Default_Value, Value_In_Array (2), Value_In_Array (2)

   -- This exchanges the values between the elements
   My_Tree_2.Swap
     (I => My_Bounded_Multiway_Trees.Last_Child (Parent => Root_2),
      J => My_Bounded_Multiway_Trees.Previous_Sibling
             (Position => My_Bounded_Multiway_Trees.Previous_Sibling
                (Position => My_Bounded_Multiway_Trees.Last_Child
                   (Parent => Root_2))));

   -- The order should now be Value_In_Array (1), Default_Value, Default_Value,
   -- Value_In_Array (2), Value_In_Array (2), Default_Value

   My_Cursor_2 := My_Bounded_Multiway_Trees.First_Child (Parent => Root_2);

   -- Check = Default_Value
   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_2) /=
     Value_In_Array (1) then

      Report.Failed ("Inserted value not as expected #1");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_2);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #2");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_2);

   -- Check = Default_Value
   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #3");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_2);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_2) /=
     Value_In_Array (2) then

      Report.Failed ("Inserted value not as expected #4");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_2);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_2) /=
     Value_In_Array (2) then

      Report.Failed ("Inserted value not as expected #5");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_2);

   -- Check = Default_Value
   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #6");

   end if;

   -- For a Bounded_Multiway_Tree (but not necessarily for a vector) a cursor
   -- should stay pointing to the same element even if the order has changed,
   -- e.g. by insertions earlier in the tree

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (1) then

      Report.Failed
        ("Cursor no longer pointing to same element after shuffling about");

   end if;


   -- Test Delete_Leaf

   -- My_Cursor_2 should initially be pointing to the last element of My_Tree_2

   My_Tree_2.Delete_Leaf (Position => My_Cursor_2); -- Count should default to 1

   My_Cursor_2 := My_Bounded_Multiway_Trees.First_Child (Parent => Root_2);

   My_Tree_2.Delete_Leaf (Position => My_Cursor_2);

   My_Cursor_2 := My_Bounded_Multiway_Trees.First_Child (Parent => Root_2);

   My_Tree_2.Delete_Leaf (Position => My_Cursor_2);

   My_Cursor_2 := My_Bounded_Multiway_Trees.Last_Child (Parent => Root_2);

   My_Tree_2.Delete_Leaf (Position => My_Cursor_2);

   My_Cursor_2 := My_Bounded_Multiway_Trees.Last_Child (Parent => Root_2);

   My_Tree_2.Delete_Leaf (Position => My_Cursor_2);

   if My_Tree_2.Node_Count /= 2 then -- Extra one for root
      Report.Failed ("Wrong node count after deleting");

   end if;

   -- Check = Default_Value
   if My_Bounded_Multiway_Trees.Element
     (My_Bounded_Multiway_Trees.First_Child (Parent => Root_2)) /=
     My_Default_Value then

      Report.Failed ("Remaining value not as expected");

   end if;


   -- Test Find and Splice_Children (two forms)

   -- My_Tree_1 should still contain the test values (in forward order), and
   -- My_Tree_2 a single element of the Default_Value

   My_Cursor_1 := My_Tree_1.Find (Item => Value_In_Array (3));

   My_Tree_1.Splice_Children
     (Target_Parent => Root_1,
      Before        => My_Cursor_1,
      Source        => My_Tree_2,
      Source_Parent => Root_2); -- Moves all under Root_2

   -- The order should now be Value_In_Array (1), Value_In_Array (2),
   -- Default_Value, Value_In_Array (3), Value_In_Array (4), Value_In_Array (5),
   -- Value_In_Array (6), Value_In_Array (7), Value_In_Array (8),
   -- Value_In_Array (9), Value_In_Array (10)

   -- My_Tree_2 should now be empty so re-fill

   for I in Array_Bounds_Type loop

      My_Tree_2.Append_Child
        (Parent   => Root_2,
         New_Item => Value_In_Array (I));

   end loop;

   -- Fill up the big tree

   for I in Ada.Containers.Count_Type range 0 .. Capacity_Reqd - 1 loop

      Big_Tree.Append_Child
        (Parent   => Big_Root,
         New_Item => Value_In_Array ((I mod Num_Tests) + 1));

   end loop;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Tree.Append_Child
           (Parent   => Big_Root,
            New_Item => Value_In_Array (1));

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

         when others =>

            Report.Failed
              ("Wrong exception raised when appending child when already full");

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Appending child when already full should have failed with " &
            "Capacity_Error");

      end if;

   end;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

      Big_Tree.Prepend_Child
        (Parent   => Big_Root,
         New_Item => Value_In_Array (1));

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

         when others =>

            Report.Failed
              ("Wrong exception raised when prepending child when already " &
               "full");

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Prepending child when already full should have failed with " &
            "Capacity_Error");

      end if;

   end;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Tree.Insert_Child
           (Parent   => Big_Root,
            Before   => My_Bounded_Multiway_Trees.No_Element, -- At end
            New_Item => Value_In_Array (1)); -- Count should default to 1

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

         when others =>

            Report.Failed
              ("Wrong exception raised when inserting child when already full");

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Inserting child when already full should have failed with " &
            "Capacity_Error");

      end if;

   end;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         My_Tree_1.Splice_Children
           (Target_Parent => Root_1,
            Before        => My_Cursor_1,
            Source        => Big_Tree,
            Source_Parent => Big_Root); -- Moves all under Big_Root

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Splicing beyond capacity should have failed with Capacity_Error");

      end if;

   end;

   -- Try not quite full cases of exceeding Capacity_Error.
   if Big_Tree.Node_Count /= Capacity_Reqd + 1 then
      Report.Failed ("Big_Tree not full");
   end if;

   -- Remove the last two elements of Big_Root:
   My_Cursor_1 := My_Bounded_Multiway_Trees.Last_Child(Big_Root);
   Big_Tree.Delete_Leaf (Position => My_Cursor_1);
   My_Cursor_1 := My_Bounded_Multiway_Trees.Last_Child(Big_Root);
   Big_Tree.Delete_Leaf (Position => My_Cursor_1);

   if Big_Tree.Node_Count /= Capacity_Reqd - 1 then
      Report.Failed ("Wrong size for Big_Tree after deleting last elements");
   end if;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Tree.Append_Child (Parent => Big_Root,
                                New_Item => Value_In_Array (1),
                                Count => 3);

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

            if Big_Tree.Node_Count /= Capacity_Reqd - 1 then
               Report.Failed
                   ("Some elements added when Capacity_Error was raised " &
                    "for Append_Child");
            end if;

         when Err:others =>

            Report.Failed
              ("Wrong exception raised when appending 3 elements when " &
               "nearly full");
            Report.Comment
              ("Raised " & Ada.Exceptions.Exception_Information(Err));

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Appending 3 elements when nearly full should have failed with " &
            "Capacity_Error");

      end if;

   end;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Tree.Prepend_Child (Parent => Big_Root,
                                 New_Item => Value_In_Array (1),
                                 Count => 4);

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

            if Big_Tree.Node_Count /= Capacity_Reqd - 1 then
               Report.Failed
                   ("Some elements added when Capacity_Error was raised " &
                    "for Prepend_Child");
            end if;

         when Err:others =>

            Report.Failed
              ("Wrong exception raised when prepending 4 elements when " &
               "nearly full");
            Report.Comment
              ("Raised " & Ada.Exceptions.Exception_Information(Err));

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Prepending 4 elements when nearly full should have failed with " &
            "Capacity_Error");

      end if;

   end;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Tree.Insert_Child
           (Parent   => Big_Root,
            Before   => Big_Tree.Find (Item => Value_In_Array (7)),
            New_Item => Value_In_Array (1),
            Count    => 5);

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

            if Big_Tree.Node_Count /= Capacity_Reqd - 1 then
               Report.Failed
                   ("Some elements added when Capacity_Error was raised " &
                    "for Insert");
            end if;

         when Err:others =>

            Report.Failed
              ("Wrong exception raised when inserting 5 elements when " &
               "nearly full");
            Report.Comment
              ("Raised " & Ada.Exceptions.Exception_Information(Err));

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Inserting 5 elements when nearly full should have failed with " &
            "Capacity_Error");

      end if;

   end;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Tree.Insert_Child
           (Parent   => Big_Root,
            Before   => Big_Tree.Find (Item => Value_In_Array (4)),
            Position => My_Cursor_1,
            Count    => 6);

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

            if Big_Tree.Node_Count /= Capacity_Reqd - 1 then
               Report.Failed
                   ("Some elements added when Capacity_Error was raised " &
                    "for Insert default element");
            end if;

         when Err:others =>

            Report.Failed
              ("Wrong exception raised when inserting 6 default elements " &
               "when nearly full");
            Report.Comment
              ("Raised " & Ada.Exceptions.Exception_Information(Err));

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Inserting 6 default elements when nearly full should have " &
            "failed with Capacity_Error");

      end if;

   end;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Tree.Insert_Child
           (Parent   => Big_Root,
            Before   => Big_Tree.Find (Item => Value_In_Array (7)),
            New_Item => Value_In_Array (1),
            Position => My_Cursor_1,
            Count    => 3);

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

            if Big_Tree.Node_Count /= Capacity_Reqd - 1 then
               Report.Failed
                   ("Some elements added when Capacity_Error was raised " &
                    "for Insert");
            end if;

         when Err:others =>

            Report.Failed
              ("Wrong exception raised when inserting 3 elements when " &
               "nearly full");
            Report.Comment
              ("Raised " & Ada.Exceptions.Exception_Information(Err));

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Inserting 3 elements when nearly full should have failed with " &
            "Capacity_Error");

      end if;

   end;

   -- Restore the list for following tests:
   Big_Tree.Append_Child (Parent => Big_Root, New_Item => Value_In_Array (4));
   Big_Tree.Append_Child (Parent => Big_Root, New_Item => Value_In_Array (5));


   if My_Tree_2.Is_Empty then

      Report.Failed ("Source shouldn't have been emptied");

   end if;

   My_Cursor_1 := My_Tree_1.Find (Item => Value_In_Array (5));

   My_Cursor_2 := My_Tree_1.Find (Item => Value_In_Array (7));

   -- Copies what's beneath My_Cursor_2, i.e. nothing

   My_Tree_1.Splice_Children
     (Target_Parent => Root_1,
      Before        => My_Cursor_1,
      Source_Parent => My_Cursor_2);

   -- The order should still be Value_In_Array (1), Value_In_Array (2),
   -- Default_Value, Value_In_Array (3), Value_In_Array (4), Value_In_Array (5),
   -- Value_In_Array (6), Value_In_Array (7), Value_In_Array (8),
   -- Value_In_Array (9), Value_In_Array (10)

   My_Cursor_1 := My_Bounded_Multiway_Trees.First_Child (Parent => Root_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (1) then

      Report.Failed ("Spliced value not as expected #1");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (2) then

      Report.Failed ("Spliced value not as expected #2");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   -- Check = Default_Value
   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     My_Default_Value then

      Report.Failed ("Spliced value not as expected #3");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (3) then

      Report.Failed ("Spliced value not as expected #4");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (4) then

      Report.Failed ("Spliced value not as expected #5");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (5) then

      Report.Failed ("Spliced value not as expected #6");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (6) then

      Report.Failed ("Spliced value not as expected #7");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (7) then

      Report.Failed ("Spliced value not as expected #8");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (8) then

      Report.Failed ("Spliced value not as expected #9");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (9) then

      Report.Failed ("Spliced value not as expected #10");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   if My_Bounded_Multiway_Trees.Element (Position => My_Cursor_1) /=
     Value_In_Array (10) then

      Report.Failed ("Spliced value not as expected #11");

   end if;


   -- Test Contains

   if not My_Tree_1.Contains (Item => Value_In_Array (8)) then

      Report.Failed ("Contains failed to find");

   end if;

   if My_Tree_1.Contains (Item => 0.0) then

      Report.Failed ("Contains found when shouldn't have");

   end if;


   -- Test Has_Element

   -- My_Cursor_1 should still be pointing to the last element

   if not My_Bounded_Multiway_Trees.Has_Element (Position => My_Cursor_1) then

      Report.Failed ("Has_Element failed to find");

   end if;

   My_Bounded_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

   -- My_Cursor_1 should now be pointing off the end

   if My_Bounded_Multiway_Trees.Has_Element (Position => My_Cursor_1) then

      Report.Failed ("Has_Element found when shouldn't have");

   end if;


   -- Test Assign and Copy

   My_Tree_2.Clear;

   My_Tree_2.Assign (Source => My_Tree_1);

   if My_Tree_2.Node_Count /= Num_Tests + 2 then

      Report.Failed ("Target node count not as expected after Assign");

   end if;

   if My_Tree_2 /= My_Tree_1 then

      Report.Failed ("Assign failed");

   end if;

   if My_Tree_1.Node_Count /= Num_Tests + 2 then

      Report.Failed ("Source node count not left alone by Assign");

   end if;

   My_Tree_2.Clear;

   My_Tree_2 := My_Bounded_Multiway_Trees.Copy
                  (Source   => My_Tree_1,
                   Capacity => Capacity_Reqd);

   if My_Tree_2.Node_Count /= Num_Tests + 2 then

      Report.Failed ("Target node count not as expected after Copy");

   end if;

   if My_Tree_2 /= My_Tree_1 then

      Report.Failed ("Copy failed");

   end if;

   if My_Tree_1.Node_Count /= Num_Tests + 2 then

      Report.Failed ("Source node count not left alone by Copy");

   end if;


   --Subtree stuff

   First_Child_Cursor_1 := My_Bounded_Multiway_Trees.First_Child
                             (Parent => Root_1);

   My_Tree_1.Insert_Child
     (Parent   => First_Child_Cursor_1,
      Before   => My_Bounded_Multiway_Trees.No_Element,
      New_Item => Value_In_Array (2),
      Position => First_Grandchild_Cursor_1);

   My_Tree_1.Insert_Child
     (Parent   => First_Child_Cursor_1,
      Before   => My_Bounded_Multiway_Trees.No_Element,
      New_Item => Value_In_Array (8));

   My_Tree_1.Insert_Child
     (Parent   => First_Grandchild_Cursor_1,
      Before   => My_Bounded_Multiway_Trees.No_Element,
      New_Item => Value_In_Array (9));

   if My_Tree_1.Node_Count /= Num_Tests + 5 then -- Remember one for root node

      Report.Failed ("Wrong node count after subtree inserted");

   end if;

   -- Test Subtree_Node_Count

   if My_Bounded_Multiway_Trees.Subtree_Node_Count
     (Position => First_Child_Cursor_1) /= 4 then
   -- Includes parent and grandchildren

      Report.Failed ("Wrong subtree node count after subtree inserted");

   end if;

   -- Test Child_Count

   if My_Bounded_Multiway_Trees.Child_Count (Parent => First_Child_Cursor_1) /=
     2 then
   -- Excludes parent and grandchildren

      Report.Failed ("Wrong child count after subtree inserted");

   end if;

   -- Test Depth

   -- Root and first child
   if My_Bounded_Multiway_Trees.Depth (Position => First_Child_Cursor_1) /= 2
     then

      Report.Failed ("Wrong depth #1");

   end if;

   -- Root, first child and first grandchild
   if My_Bounded_Multiway_Trees.Depth (Position => First_Grandchild_Cursor_1) /=
     3 then

      Report.Failed ("Wrong depth #2");

   end if;

   -- Test Child_Depth

   if My_Bounded_Multiway_Trees.Child_Depth
     (Parent => First_Child_Cursor_1,
      Child  => First_Grandchild_Cursor_1) /= 1 then

      Report.Failed ("Wrong child depth #1");

   end if;

   if My_Bounded_Multiway_Trees.Child_Depth
     (Parent => Root_1,
      Child  => First_Grandchild_Cursor_1) /= 2 then

      Report.Failed ("Wrong child depth #2");

   end if;

   Second_Child_Cursor_1 := My_Bounded_Multiway_Trees.Next_Sibling
                            (Position => First_Child_Cursor_1);

   My_Tree_1.Insert_Child
     (Parent   => Second_Child_Cursor_1,
      Before   => My_Bounded_Multiway_Trees.No_Element,
      New_Item => Value_In_Array (9));

   -- Root
   -- |
   -- ------------------------------
   -- |    |    |    | | | | | | | |
   -- 1    2 Default 3 4 5 6 7 8 9 10
   -- |    |
   -- ---  9
   -- | |
   -- 2 8
   -- |
   -- 9

   if My_Tree_1.Node_Count /= Num_Tests + 6 then -- Remember one for root node

      Report.Failed ("Wrong node count after subtree inserted");

   end if;

   if My_Bounded_Multiway_Trees.Subtree_Node_Count
       (Position => First_Child_Cursor_1) /= 4 then
   -- Includes parent and grandchildren

      Report.Failed ("Wrong subtree node count after subtree inserted");

   end if;

   if My_Bounded_Multiway_Trees.Child_Count (Parent => First_Child_Cursor_1) /=
     2 then
   -- Excludes parent and grandchildren

      Report.Failed ("Wrong child count after subtree inserted");

   end if;

   -- Test Equal_Subtree

   if not My_Bounded_Multiway_Trees.Equal_Subtree
     (Left_Position  => First_Grandchild_Cursor_1,
      Right_Position => Second_Child_Cursor_1) then

      Report.Failed ("Thinks equal subtrees aren't");

   end if;

   if My_Bounded_Multiway_Trees.Equal_Subtree
     (Left_Position  => Root_1,
      Right_Position => First_Grandchild_Cursor_1) then

      Report.Failed ("Thinks unequal subtrees are");

   end if;

   -- Test Find_In_Subtree

   Greatgrandchild_Cursor_1 := My_Bounded_Multiway_Trees.Find_In_Subtree
                               (Position => First_Grandchild_Cursor_1,
                                Item     => Value_In_Array (9));

   if Greatgrandchild_Cursor_1 /= My_Bounded_Multiway_Trees.First_Child
                                  (Parent => First_Grandchild_Cursor_1) then

      Report.Failed ("Has found wrong child in subtree");

   end if;

   -- Test Ancestor_Find

   if My_Bounded_Multiway_Trees.Ancestor_Find
        (Position => Greatgrandchild_Cursor_1,
         Item     => Value_In_Array (2))
     /= First_Grandchild_Cursor_1 then

      Report.Failed ("Failed to find ancestor");

   end if;

   if My_Bounded_Multiway_Trees.Find_In_Subtree
        (Position => First_Grandchild_Cursor_1,
         Item     => Value_In_Array (7))
     /= My_Bounded_Multiway_Trees.No_Element then

      Report.Failed ("Has found child in subtree when shouldn't have");

   end if;

   if My_Bounded_Multiway_Trees.Ancestor_Find
     (Position => First_Grandchild_Cursor_1,
      Item     => Value_In_Array (7))
     /= My_Bounded_Multiway_Trees.No_Element then

      Report.Failed ("Has found ancestor when shouldn't have");

   end if;

   -- Test Iterate_Subtree

   declare

      I : Array_Bounds_Type := 1;

      procedure My_Process (Position : in My_Bounded_Multiway_Trees.Cursor) is
      begin

         -- Includes parent and grandchildren

         case I is

            when 1 =>

               if My_Bounded_Multiway_Trees.Element (Position) /=
                 Value_In_Array (1) then

                  Report.Failed ("Iterate hasn't found the expected value #1");

               end if;

            when 2 =>

               if My_Bounded_Multiway_Trees.Element (Position) /=
                 Value_In_Array (2) then

                  Report.Failed ("Iterate hasn't found the expected value #2");

               end if;

            when 3 =>

               if My_Bounded_Multiway_Trees.Element (Position) /=
                 Value_In_Array (9) then

                  Report.Failed ("Iterate hasn't found the expected value #3");

               end if;

            when 4 =>

               if My_Bounded_Multiway_Trees.Element (Position) /=
                 Value_In_Array (8) then

                  Report.Failed ("Iterate hasn't found the expected value #4");

               end if;

            when others =>

               Report.Failed ("Iterate has found too many elements");

         end case;

         I := I + 1;

      end My_Process;

   begin

      My_Bounded_Multiway_Trees.Iterate_Subtree
        (Position => My_Bounded_Multiway_Trees.First_Child (Parent => Root_1),
         Process  => My_Process'Access);

   end;

   -- Test Iterate_Children

   declare

      I : Array_Bounds_Type := 1;

      procedure My_Process (Position : in My_Bounded_Multiway_Trees.Cursor) is
      begin

         -- Excludes parent and grandchildren

         case I is

            when 1 =>

               if My_Bounded_Multiway_Trees.Element (Position) /=
                 Value_In_Array (2) then

                  Report.Failed
                    ("Iterate_Children hasn't found the expected value #1");

               end if;

            when 2 =>

               if My_Bounded_Multiway_Trees.Element (Position) /=
                 Value_In_Array (8) then

                  Report.Failed
                    ("Iterate_Children hasn't found the expected value #2");

               end if;

            when others =>

               Report.Failed ("Iterate_Children has found too many elements");

         end case;

         I := I + 1;

      end My_Process;

   begin

      My_Bounded_Multiway_Trees.Iterate_Children
        (Parent   => My_Bounded_Multiway_Trees.First_Child (Parent => Root_1),
         Process  => My_Process'Access);

   end;

   -- Test Copy_Subtree

   My_Tree_1.Copy_Subtree
     (Parent => First_Child_Cursor_1,
      Before => My_Bounded_Multiway_Trees.Last_Child
                  (Parent => First_Child_Cursor_1),
      Source => Second_Child_Cursor_1);

   -- Root
   -- |
   -- ---------------------------------
   -- |       |    |    | | | | | | | |
   -- 1       2 Default 3 4 5 6 7 8 9 10
   -- |       |
   -- -----   9
   -- | | |
   -- 2 2 8
   -- | |
   -- 9 9

   if My_Tree_1.Node_Count /= Num_Tests + 8 then -- Remember one for root node

      Report.Failed ("Wrong node count after Copy_Subtree");

   end if;

   if My_Bounded_Multiway_Trees.Subtree_Node_Count
     (Position => First_Child_Cursor_1) /= 6 then
   -- Includes parent and grandchildren

      Report.Failed ("Wrong subtree node count after Copy_Subtree");

   end if;

   if My_Bounded_Multiway_Trees.Child_Count (Parent => First_Child_Cursor_1) /=
     3 then
   -- Excludes parent and grandchildren

      Report.Failed ("Wrong child count after Copy_Subtree");

   end if;

   -- Test Splice_Subtree (first form)

   My_Cursor_2 := My_Bounded_Multiway_Trees.Last_Child (Parent => Root_2);

   Last_Child_Cursor_1 := My_Bounded_Multiway_Trees.Last_Child
                            (Parent => Root_1);

   My_Tree_1.Splice_Subtree
     (Parent   => Last_Child_Cursor_1,
      Before   => My_Bounded_Multiway_Trees.No_Element,
      Source   => My_Tree_2,
      Position => My_Cursor_2);

   -- Root
   -- |
   -- ---------------------------------
   -- |       |    |    | | | | | | | |
   -- 1       2 Default 3 4 5 6 7 8 9 10
   -- |       |                       |
   -- -----   9                       10
   -- | | |
   -- 2 2 8
   -- | |
   -- 9 9

   -- Check that My_Tree_1 has gained a node

   if My_Tree_1.Node_Count /= Num_Tests + 9 then -- Remember one for root node

      Report.Failed ("Wrong node count for target after first Slice_Subtree");

   end if;

   -- Check that My_Tree_2 has lost a node

   if My_Tree_2.Node_Count /= Num_Tests + 1 then -- Remember one for root node

      Report.Failed ("Wrong node count for source after first Slice_Subtree");

   end if;

   -- Check that My_Tree_1's root has gained a child

   if My_Bounded_Multiway_Trees.Child_Count (Parent => Root_1) /= 11 then
   -- Excludes parent and grandchildren

      Report.Failed ("Wrong child count for root after first Slice_Subtree");

   end if;

   -- Check that My_Tree_1's last child is a subtree of two nodes (itself plus
   -- one child beneath it)

   if My_Bounded_Multiway_Trees.Subtree_Node_Count
     (Position => Last_Child_Cursor_1) /= 2 then
   -- Includes parent and grandchildren

      Report.Failed
        ("Wrong subtree node count for last child after first Slice_Subtree");

   end if;

   -- Check that My_Tree_1's last child has one child beneath it

   if My_Bounded_Multiway_Trees.Child_Count (Parent => Last_Child_Cursor_1) /= 1
     then
   -- Excludes parent and grandchildren

      Report.Failed
        ("Wrong child count for last child after first Slice_Subtree");

   end if;

   -- Test Splice_Subtree (second form)

   My_Tree_1.Splice_Subtree
     (Parent   => Last_Child_Cursor_1,
      Before   => My_Bounded_Multiway_Trees.No_Element,
      Position => First_Grandchild_Cursor_1);

   -- Root
   -- |
   -- -----------------------------
   -- |   |    |    | | | | | | | |
   -- 1   2 Default 3 4 5 6 7 8 9 10
   -- |   |                       |
   -- --- 9                       ----
   -- | |                         |  |
   -- 2 8                         10 2
   -- |                              |
   -- 9                              9

   -- Check that My_Tree_1 has the same number of nodes (just rearranged)

   if My_Tree_1.Node_Count /= Num_Tests + 9 then -- Remember one for root node

      Report.Failed ("Wrong node count after second Slice_Subtree");

   end if;

   -- Check that My_Tree_1's last child is a subtree that has gained two nodes

   if My_Bounded_Multiway_Trees.Subtree_Node_Count
     (Position => Last_Child_Cursor_1) /= 4 then
   -- Includes parent and grandchildren

      Report.Failed
        ("Wrong subtree node count for last child after second Slice_Subtree");

   end if;

   -- Check that My_Tree_1's first child is a subtree that has lost two nodes

   if My_Bounded_Multiway_Trees.Subtree_Node_Count
     (Position => First_Child_Cursor_1) /= 4 then
   -- Includes parent and grandchildren

      Report.Failed
        ("Wrong subtree node count for first child after second Slice_Subtree");

   end if;

   -- Check that My_Tree_1's last child has gained a child

   if My_Bounded_Multiway_Trees.Child_Count (Parent => Last_Child_Cursor_1) /= 2
     then
   -- Excludes parent and grandchildren

      Report.Failed
        ("Wrong child count for last child after second Slice_Subtree");

   end if;

   -- Check that My_Tree_1's first child has lost a child

   if My_Bounded_Multiway_Trees.Child_Count (Parent => First_Child_Cursor_1) /=
     2 then
   -- Excludes parent and grandchildren

      Report.Failed
        ("Wrong child count for first child after second Slice_Subtree");

   end if;

   -- Test Delete_Subtree

   My_Tree_1.Delete_Subtree (Position => Last_Child_Cursor_1);
   -- Includes parent

   -- Root
   -- |
   -- ---------------------------
   -- |   |    |    | | | | | | |
   -- 1   2 Default 3 4 5 6 7 8 9
   -- |   |
   -- --- 9
   -- | |
   -- 2 8
   -- |
   -- 9

   if My_Tree_1.Node_Count /= Num_Tests + 5 then -- Remember one for root node

      Report.Failed ("Wrong node count after Delete_Subtree");

   end if;

   -- Test Delete_Children

   My_Tree_1.Delete_Children (Parent => First_Child_Cursor_1);
   -- Excludes parent

   -- Root
   -- |
   -- -------------------------
   -- | |   |     | | | | | | |
   -- 1 2 Default 3 4 5 6 7 8 9
   --   |
   --   9

   if My_Tree_1.Node_Count /= Num_Tests + 2 then -- Remember one for root node

      Report.Failed ("Wrong node count after Delete_Children");

   end if;


   Report.Result;

end CXAI016;
