-- CXAI013.A
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
--      in package Ada.Containers.Bounded_Ordered_Maps.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in package Ada.Containers.Bounded_Ordered_Maps.
--      Each of the subprograms is exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      23 Sep 13   JAC     Initial pre-release version.
--       6 Dec 13   JAC     Second pre-release version.
--      29 Dec 13   JAC     Third pre-release version.
--      28 Mar 14   RLB     Created ACATS 4.0 version, renamed test.
--                          Added additional capacity error cases.
--       3 Apr 14   RLB     Merged in tampering checks from fourth pre-release
--                          version.
--
--!
with Ada.Containers.Bounded_Ordered_Maps;
with Report;

procedure CXAI013 is

   My_Default_Value : constant := 999.0;

   type My_Float is new Float
     with Default_Value => My_Default_Value;

   type My_Key_Type is new Integer;

   package My_Bounded_Ordered_Maps is new Ada.Containers.Bounded_Ordered_Maps
     (Key_Type     => My_Key_Type,
      Element_Type => My_Float); -- Default < and Default =

   Num_Tests : constant := 10;

   Capacity_Reqd : constant := Num_Tests;

   My_Map_1 : My_Bounded_Ordered_Maps.Map (Capacity => Capacity_Reqd);
   My_Map_2 : My_Bounded_Ordered_Maps.Map (Capacity => Capacity_Reqd);
   Big_Map  : My_Bounded_Ordered_Maps.Map (Capacity => Capacity_Reqd);

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. Num_Tests;

   -- No fractional parts so that can compare values for equality.

   Value_In_Array  : constant array (Array_Bounds_Type) of My_Float :=
     (12.0, 23.0, 34.0, 45.0, 56.0, 67.0, 78.0, 89.0, 90.0, 1.0);

   My_Cursor_1 : My_Bounded_Ordered_Maps.Cursor;
   My_Cursor_2 : My_Bounded_Ordered_Maps.Cursor;

   My_Inserted : Boolean;

   procedure Tampering_Check
     (Container : in out My_Bounded_Ordered_Maps.Map;
      Where     : in     String)
     with Pre => not Container.Is_Empty is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Don't try to insert into a full bounded container as it's a grey
         -- area as to whether Capacity_Error or Program_Error should be raised

         Container.Delete_First;

      exception

         when Ada.Containers.Capacity_Error =>

            Report.Failed ("Capacity_Error when tampering in " & Where);

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Bounded_Ordered_Maps.Cursor;
   use type My_Bounded_Ordered_Maps.Map;


begin

   Report.Test
     ("CXAI013",
      "Check that an implementation supports the functionality defined in " &
      "package Ada.Containers.Bounded_Ordered_Maps");


   -- Test empty using Empty_Map, Is_Empty and Length

   if My_Map_1 /= My_Bounded_Ordered_Maps.Empty_Map then

      Report.Failed ("Not initially empty #1");

   end if;

   if not My_Map_1.Is_Empty then

      Report.Failed ("Not initially empty #2");

   end if;

   if My_Map_1.Length /= 0 then

      Report.Failed ("Not initially empty #3");

   end if;


   -- Test Insert, First, Element (two forms), Key, Equivalent_Keys
   -- Query_Element, Next (two forms), First_Element and First_Key

   for I in Array_Bounds_Type loop

      My_Map_1.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Array (I));

      if My_Map_1.Length /= I then

         Report.Failed ("Wrong Length after inserting");

      end if;

   end loop;

   My_Cursor_1 := My_Map_1.First;

   for I in Array_Bounds_Type loop

      declare

         procedure My_Query
           (Key     : in My_Key_Type;
            Element : in My_Float) is
         begin

            Tampering_Check
              (Container => My_Map_1,
               Where     => "Query_Element");

            if Key /= My_Key_Type (I) then

               Report.Failed ("Mismatch between key and what was inserted");

            end if;

            if Element /= Value_In_Array (I) then

               Report.Failed
                 ("Mismatch between element and what was inserted #1");

            end if;

         end My_Query;

      begin

         if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_1) /=
           Value_In_Array (I) then

            Report.Failed ("Mismatch between element and what was inserted #2");

         end if;

         if My_Map_1.Element (Key => My_Key_Type (I)) /= Value_In_Array (I) then

            Report.Failed ("Mismatch between element and what was inserted #3");

         end if;

         if My_Bounded_Ordered_Maps.Key (Position => My_Cursor_1) /=
           My_Key_Type (I) then

            Report.Failed ("Key not as expected");

         end if;

         if not My_Bounded_Ordered_Maps.Equivalent_Keys
           (Left  => My_Bounded_Ordered_Maps.Key (Position => My_Cursor_1),
            Right => My_Key_Type (I)) then

            Report.Failed ("Keys not equivalent");

         end if;

         My_Bounded_Ordered_Maps.Query_Element
           (Position => My_Cursor_1,
            Process  => My_Query'Access);

      end;

      -- Toggle between alternative methods for incrementing cursor

      if I mod 2 = 0 then

         My_Cursor_1 := My_Bounded_Ordered_Maps.Next (Position => My_Cursor_1);

      else

         My_Bounded_Ordered_Maps.Next (Position => My_Cursor_1);

      end if;

   end loop;

   if My_Map_1.First_Element /= Value_In_Array (Value_In_Array'First) then

      Report.Failed ("Mismatch between first element and first inserted");

   end if;

   if My_Map_1.First_Key /= 1 then

      Report.Failed ("First key not as expected");

   end if;


   -- Test Insert, Last, Element, Previous (two forms), Last_Element and
   -- Last_Key

   for I in reverse Array_Bounds_Type loop

      My_Map_2.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Array (I));

      if My_Map_2.Length /= Num_Tests - I + 1 then

         Report.Failed ("Wrong Length after inserting");

      end if;

   end loop;

   My_Cursor_2 := My_Map_2.Last;

   for I in reverse Array_Bounds_Type loop

      if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_2) /=
        Value_In_Array (I) then

         Report.Failed ("Mismatch between element and what was inserted #4");

      end if;

      -- Toggle between alternative methods for decrementing cursor

      if I mod 2 = 0 then

         My_Cursor_2 := My_Bounded_Ordered_Maps.Previous
                          (Position => My_Cursor_2);

      else

         My_Bounded_Ordered_Maps.Previous (Position => My_Cursor_2);

      end if;

   end loop;

   if My_Map_2.Last_Element /= Value_In_Array (Value_In_Array'Last) then

      Report.Failed ("Mismatch between last element and last inserted");

   end if;

   if My_Map_1.Last_Key /= Num_Tests then

      Report.Failed ("Last key not as expected");

   end if;


   -- Test equality

   if My_Map_1 /= My_Map_2 then

      Report.Failed ("Bounded_Ordered_Maps not equal");

   end if;


   -- Test assignment, Iterate and Reverse_Iterate

   declare

      My_Map_3 : My_Bounded_Ordered_Maps.Map := My_Map_1;

      I : Array_Bounds_Type := Array_Bounds_Type'First;

      procedure My_Process
        (Position : in My_Bounded_Ordered_Maps.Cursor) is
      begin

         Tampering_Check
           (Container => My_Map_3,
            Where     => "Iterate");

         if My_Bounded_Ordered_Maps.Element (Position) /= Value_In_Array (I)
           then

            Report.Failed ("Iterate hasn't found the expected value");

         end if;

         if I < Array_Bounds_Type'Last then

            I := I + 1;

         end if;

      end My_Process;

      procedure My_Reverse_Process
        (Position : in My_Bounded_Ordered_Maps.Cursor) is
      begin

         if My_Bounded_Ordered_Maps.Element (Position) /= Value_In_Array (I)
           then

            Report.Failed ("Reverse_Iterate hasn't found the expected value");

         end if;

         if I > Array_Bounds_Type'First then

            I := I - 1;

         end if;

      end My_Reverse_Process;

   begin

      My_Map_3.Iterate (Process => My_Process'Access);

      My_Map_3.Reverse_Iterate (Process => My_Reverse_Process'Access);

   end;


   -- Test Replace_Element and Update_Element

   -- Double the values of the two Bounded_Ordered_Maps by two different methods
   -- and check still equal

   My_Cursor_1 := My_Map_1.First;
   My_Cursor_2 := My_Map_2.First;

   for I in Array_Bounds_Type loop

      declare

         procedure My_Update
           (Key     : in My_Key_Type;
            Element : in out My_Float) is
         begin

            Tampering_Check
              (Container => My_Map_2,
               Where     => "Update_Element");

            Element := Element * 2.0;

         end My_Update;

      begin

         My_Map_1.Replace_Element
           (Position => My_Cursor_1,
            New_Item => Value_In_Array (I) * 2.0);

         My_Map_2.Update_Element
           (Position => My_Cursor_2,
            Process  => My_Update'Access);

      end;

      My_Bounded_Ordered_Maps.Next (Position => My_Cursor_1);
      My_Bounded_Ordered_Maps.Next (Position => My_Cursor_2);

   end loop;

   if My_Map_1 /= My_Map_2 then

      Report.Failed ("Doubled Bounded_Ordered_Maps not equal");

   end if;


   -- Test Clear and inequality

   My_Map_1.Clear;

   if not My_Map_1.Is_Empty then

      Report.Failed ("Failed to clear");

   end if;

   for I in Array_Bounds_Type loop

      My_Map_1.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Array (I));

   end loop;

   if My_Map_1 = My_Map_2 then

      Report.Failed ("Different Bounded_Ordered_Maps equal");

   end if;


   -- Test Move.  Target has the test values in reverse order, after Move these
   -- should be replaced (not appended) by the test values in forward order

   My_Map_2.Clear;

   for I in Array_Bounds_Type loop

      My_Map_2.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Array (I));

   end loop;

   My_Map_1.Move (Source => My_Map_2);

   if not My_Map_2.Is_Empty then

      Report.Failed ("Moved source not empty");

   end if;

   My_Cursor_1 := My_Map_1.First;

   for I in Array_Bounds_Type loop

      if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_1) /=
        Value_In_Array (I) then

         Report.Failed ("Target Map not as expected after move");

      end if;

      My_Bounded_Ordered_Maps.Next (Position => My_Cursor_1);

   end loop;


   -- Tests for Capacity_Error

   -- Fill up the big map

   for I in Ada.Containers.Count_Type range 0 .. Capacity_Reqd - 1 loop

      Big_Map.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Array ((I mod Num_Tests) + 1));

   end loop;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Map.Insert
           (Key      => My_Key_Type (Capacity_Reqd),
            New_Item => Value_In_Array (1));

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Inserting beyond capacity should have failed with Capacity_Error");

      end if;

   end;

   declare

      Capacity_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Big_Map.Include
           (Key      => My_Key_Type (Capacity_Reqd),
            New_Item => Value_In_Array (1));

      exception

         when Ada.Containers.Capacity_Error =>

            Capacity_Error_Raised := True;

      end;

      if not Capacity_Error_Raised then

         Report.Failed
           ("Including beyond capacity should have failed with Capacity_Error");

      end if;

   end;

   declare

      Inserted : Boolean;
      A_Cursor : My_Bounded_Ordered_Maps.Cursor;

   begin

      declare
      begin

         Big_Map.Insert
           (Key      => My_Key_Type (3), -- A value in the map.
            New_Item => Value_In_Array (1),
            Position => A_Cursor,
            Inserted => Inserted);

         if Inserted then
            Report.Failed
             ("5 parameter Insert inserted existing key into full map");
         -- else expected.
         end if;

      exception

         when Ada.Containers.Capacity_Error =>

            Report.Failed
             ("5 parameter Insert raised Capacity_Error on full container " &
              "with existing key");

      end;

      declare
      begin

         Big_Map.Insert
           (Key      => My_Key_Type (Capacity_Reqd),
            New_Item => Value_In_Array (1),
            Position => A_Cursor,
            Inserted => Inserted);

         if Inserted then
            Report.Failed
             ("5 parameter Insert inserted new key into full map " &
              "without raising Capacity_Error");
         else
            Report.Failed
             ("5 parameter Insert did not insert new key");
         end if;

      exception

         when Ada.Containers.Capacity_Error =>

            null; -- Expected.
      end;

   end;

   declare

      Inserted : Boolean;
      A_Cursor : My_Bounded_Ordered_Maps.Cursor;

   begin

      declare
      begin

         Big_Map.Insert
           (Key      => My_Key_Type (3), -- A value in the map.
            Position => A_Cursor,
            Inserted => Inserted);

         if Inserted then
            Report.Failed
             ("4 parameter Insert inserted existing key into full map");
         -- else expected.
         end if;

      exception

         when Ada.Containers.Capacity_Error =>

            Report.Failed
             ("4 parameter Insert raised Capacity_Error on full container " &
              "with existing key");

      end;

      declare
      begin

         Big_Map.Insert
           (Key      => My_Key_Type (Capacity_Reqd),
            Position => A_Cursor,
            Inserted => Inserted);
               -- The default value is not found in the map.

         if Inserted then
            Report.Failed
             ("4 parameter Insert inserted new key into full map " &
              "without raising Capacity_Error");
         else
            Report.Failed
             ("4 parameter Insert did not insert new key");
         end if;

      exception

         when Ada.Containers.Capacity_Error =>

            null; -- Expected.

      end;

   end;


   -- Test Insert (three forms), Include and Replace

   -- My_Map_2 should initially be empty
   -- Insert in fairly mixed order to check that order is determined by the key,
   -- not the order of insertion

   declare
   begin

      My_Map_2.Insert
        (Key      => 3,
         New_Item => Value_In_Array (1));

   exception

      when Constraint_Error =>

         Report.Failed ("Not inserted #1");

   end;

   declare

      Constraint_Error_Raised : Boolean := False;

   begin

      declare
      begin

         My_Map_2.Insert
           (Key      => 3,
            New_Item => Value_In_Array (1));

      exception

         when Constraint_Error =>

            Constraint_Error_Raised := True;

      end;

      if not Constraint_Error_Raised then

         Report.Failed ("Insert with duplicate key should have failed #1");

      end if;

   end;

   My_Map_2.Insert
     (Key      => 1,
      New_Item => Value_In_Array (2),
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #2");

   end if;

   My_Map_2.Insert
     (Key      => 1,
      New_Item => Value_In_Array (2),
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if My_Inserted then

      Report.Failed ("Insert with duplicate key should have failed #2");

   end if;

   -- Element with Default_Value.  Should insert in-between the previous two
   -- elements
   My_Map_2.Insert
     (Key      => 2,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #3");

   end if;

   -- Element with Default_Value
   My_Map_2.Insert
     (Key      => 2,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if My_Inserted then

      Report.Failed ("Insert with duplicate key should have failed #3");

   end if;

   declare
   begin

      My_Map_2.Insert
        (Key      => 6,
         New_Item => Value_In_Array (7));

   exception

      when Constraint_Error =>

         Report.Failed ("Not inserted #4");

   end;

   declare
   begin

      My_Map_2.Include
        (Key      => 6,
         New_Item => Value_In_Array (2));

   exception

      when Constraint_Error =>

         Report.Failed
           ("Include should replace instead of raising Constraint_Error");

   end;

   My_Map_2.Insert
     (Key      => 4,
      New_Item => Value_In_Array (9),
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #5");

   end if;

   declare
   begin

      My_Map_2.Replace
        (Key      => 4,
         New_Item => Value_In_Array (1));

   exception

      when Constraint_Error =>

         Report.Failed
           ("Replace should not raise Constraint_Error for existing key");

   end;

   -- Element with Default_Value.  Should insert in-between the previous two
   -- elements
   My_Map_2.Insert
     (Key      => 5,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #6");

   end if;

   -- The order should now be Value_In_Array (2), Default_Value,
   -- Value_In_Array (1), Value_In_Array (1), Default_Value, Value_In_Array (2)

   My_Cursor_2 := My_Map_2.First;

   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_2) /=
     Value_In_Array (2) then

      Report.Failed ("Inserted value not as expected #1");

   end if;

   My_Bounded_Ordered_Maps.Next (Position => My_Cursor_2);

   -- Check = Default_Value
   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #2");

   end if;

   My_Bounded_Ordered_Maps.Next (Position => My_Cursor_2);

   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_2) /=
     Value_In_Array (1) then

      Report.Failed ("Inserted value not as expected #3");

   end if;

   My_Bounded_Ordered_Maps.Next (Position => My_Cursor_2);

   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_2) /=
     Value_In_Array (1) then

      Report.Failed ("Inserted value not as expected #4");

   end if;

   My_Bounded_Ordered_Maps.Next (Position => My_Cursor_2);

   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #5");

   end if;

   My_Bounded_Ordered_Maps.Next (Position => My_Cursor_2);

   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_2) /=
     Value_In_Array (2) then

      Report.Failed ("Inserted value not as expected #6");

   end if;


   -- Test Delete, Exclude, Delete_First and Delete_Last

   -- My_Cursor_2 should initially be pointing to the last element of
   -- My_Map_2

   My_Map_2.Delete (Position => My_Cursor_2);

   My_Map_2.Delete (Key => 3);

   My_Map_2.Exclude (Key => 4);

   declare
   begin

      My_Map_2.Exclude (Key => 4);

   exception

      when Constraint_Error =>

         Report.Failed
           ("Exclude should not raise Constraint_Error for absent key");

   end;

   My_Map_2.Delete_First;

   My_Map_2.Delete_Last;

   if My_Map_2.Length /= 1 then

      Report.Failed ("Wrong Length after deleting");

   end if;

   -- Check = Default_Value
   if My_Bounded_Ordered_Maps.Element (My_Map_2.First) /= My_Default_Value then

      Report.Failed ("Remaining value not as expected");

   end if;


   -- Test Find, Floor, Ceiling, < (3 forms) and > (3 forms)

   -- My_Map_1 should still contain the test values (in forward order)

   My_Cursor_1 := My_Map_1.Find (Key => 9);

   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_1) /=
     Value_In_Array (9) then

      Report.Failed ("Found value not as expected");

   end if;

   My_Cursor_2 := My_Map_1.Floor (Key => 8);

   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_2) /=
     Value_In_Array (8) then

      Report.Failed ("Floor value not as expected");

   end if;

   if not (My_Cursor_2 < My_Cursor_1) then

      Report.Failed ("< of cursors not as expected");

   end if;

   if My_Cursor_2 > My_Cursor_1 then

      Report.Failed ("> of cursors not as expected");

   end if;

   My_Cursor_1 := My_Map_1.Ceiling (Key => 10);

   if My_Bounded_Ordered_Maps.Element (Position => My_Cursor_1) /=
     Value_In_Array (10) then

      Report.Failed ("Ceiling value not as expected");

   end if;

   if not (5 < My_Cursor_1) then

      Report.Failed ("< of key and cursor not as expected");

   end if;

   if 6 > My_Cursor_1 then

      Report.Failed ("> of key and cursor not as expected");

   end if;

   if not (My_Cursor_1 < 12) then

      Report.Failed ("< of cursor and key not as expected");

   end if;

   if My_Cursor_1 > 11 then

      Report.Failed ("> of cursor and key not as expected");

   end if;


   -- Test Contains

   if not My_Map_1.Contains (Key => 3) then

      Report.Failed ("Contains failed to find");

   end if;

   if My_Map_1.Contains (Key => 0) then

      Report.Failed ("Contains found when shouldn't have");

   end if;


   -- Test Has_Element

   -- My_Cursor_1 should still be pointing to the last element

   if not My_Bounded_Ordered_Maps.Has_Element (Position => My_Cursor_1) then

      Report.Failed ("Has_Element failed to find");

   end if;

   My_Bounded_Ordered_Maps.Next (Position => My_Cursor_1);

   -- My_Cursor_1 should now be pointing off the end

   if My_Bounded_Ordered_Maps.Has_Element (Position => My_Cursor_1) then

      Report.Failed ("Has_Element found when shouldn't have");

   end if;


   -- Test Assign and Copy

   My_Map_2.Assign (Source => My_Map_1);

   if My_Map_2.Length /= Num_Tests then

      Report.Failed ("Target map length not as expected after Assign");

   end if;

   if My_Map_1.Length /= Num_Tests then

      Report.Failed ("Source map length not left alone by Assign");

   end if;

   My_Map_2 := My_Bounded_Ordered_Maps.Copy (Source => My_Map_1);

   if My_Map_2.Length /= Num_Tests then

      Report.Failed ("Result map length not as expected after Copy");

   end if;

   if My_Map_1.Length /= Num_Tests then

      Report.Failed ("Source map length not left alone by Copy");

   end if;


   Report.Result;

end CXAI013;
