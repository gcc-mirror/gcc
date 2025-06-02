-- CXAIA06.A
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
--      in package Ada.Containers.Indefinite_Ordered_Sets.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in package Ada.Containers.Indefinite_Ordered_Sets.
--      Each of the subprograms is exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      23 Sep 13   JAC     Initial pre-release version.
--       6 Dec 13   JAC     Second pre-release version.
--      30 Dec 13   RLB     Moved common code to foundation. Created Amendment
--                          version, marked and commented out Ada 2012 features.
--      10 Mar 14   RLB     Created ACATS 4.0 version, replaced Ada 2012
--                          features.
--       3 Apr 14   RLB     Merged in tampering checks from fourth pre-release
--                          version.
--
--!
with Ada.Containers.Indefinite_Ordered_Sets;
with Report;
with FXAIA00; -- Foundation.

procedure CXAIA06 is

   My_Default_Value : constant String := "zzz";

   Value_In_Ptr_Array : FXAIA00.Value_In_Ptr_Array_Type
      renames FXAIA00.Value_In_Ptr_Array;

   package My_Indefinite_Ordered_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String); -- Default < and Default =

   type My_Key_Type is new Integer;

   function My_Key (Element : String) return My_Key_Type is
   begin

      return My_Key_Type (Character'Pos (Element (Element'First)));

   end My_Key;

   package My_Keys is new My_Indefinite_Ordered_Sets.Generic_Keys
     (Key_Type => My_Key_Type,
      Key      => My_Key); -- Predefined <

   My_Set_1 : My_Indefinite_Ordered_Sets.Set;
   My_Set_2 : My_Indefinite_Ordered_Sets.Set;
   My_Set_3 : My_Indefinite_Ordered_Sets.Set;

   My_Cursor_1 : My_Indefinite_Ordered_Sets.Cursor;
   My_Cursor_2 : My_Indefinite_Ordered_Sets.Cursor;
   My_Cursor_3 : My_Indefinite_Ordered_Sets.Cursor;

   My_Inserted : Boolean;

   procedure Tampering_Check
     (Container : in out My_Indefinite_Ordered_Sets.Set;
      Where     : in     String) is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Use a value not already in set

         Container.Insert (New_Item => "xyz");

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Indefinite_Ordered_Sets.Cursor;
   use type My_Indefinite_Ordered_Sets.Set;


begin

   Report.Test
     ("CXAIA06",
      "Check that an implementation supports the functionality defined in " &
      "package Ada.Containers.Indefinite_Ordered_Sets");


   -- Test empty using Empty_Set, Is_Empty and Length

   if My_Set_1 /= My_Indefinite_Ordered_Sets.Empty_Set then

      Report.Failed ("Not initially empty #1");

   end if;

   if not My_Set_1.Is_Empty then

      Report.Failed ("Not initially empty #2");

   end if;

   if My_Set_1.Length /= 0 then

      Report.Failed ("Not initially empty #3");

   end if;


   -- Test Insert, First, Element (cursor form), Equivalent_Elements,
   -- Query_Element, Next (two forms) and First_Element

   for I in FXAIA00.Array_Bounds_Type loop

      My_Set_1.Insert (New_Item => Value_In_Ptr_Array (I).all);

      if My_Set_1.Length /= I then

         Report.Failed ("Wrong Length after inserting");

      end if;

   end loop;

   My_Cursor_1 := My_Set_1.First;

   for I in FXAIA00.Array_Bounds_Type loop

      declare

         procedure My_Query (Element : in String) is
         begin

            Tampering_Check
              (Container => My_Set_1,
               Where     => "Query_Element");

            if Element /= Value_In_Ptr_Array (I).all then

               Report.Failed
                 ("Mismatch between element and what was inserted #1");

            end if;

         end My_Query;

      begin

         if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_1) /=
           Value_In_Ptr_Array (I).all then

            Report.Failed ("Mismatch between element and what was inserted #2");

         end if;

         if not My_Indefinite_Ordered_Sets.Equivalent_Elements
           (Left  => My_Indefinite_Ordered_Sets.Element
                       (Position => My_Cursor_1),
            Right => Value_In_Ptr_Array (I).all) then

            Report.Failed ("Elements not equivalent");

         end if;

         My_Indefinite_Ordered_Sets.Query_Element
           (Position => My_Cursor_1,
            Process  => My_Query'Access);

      end;

      -- Toggle between alternative methods for incrementing cursor

      if I mod 2 = 0 then

         My_Cursor_1 := My_Indefinite_Ordered_Sets.Next
                          (Position => My_Cursor_1);

      else

         My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_1);

      end if;

   end loop;

   if My_Set_1.First_Element /=
      Value_In_Ptr_Array (Value_In_Ptr_Array'First).all then

      Report.Failed ("Mismatch between first element and first inserted");

   end if;


   -- Test Insert, Last, Element, Previous (two forms) and Last_Element

   for I in reverse FXAIA00.Array_Bounds_Type loop

      My_Set_2.Insert (New_Item => Value_In_Ptr_Array (I).all);

      if My_Set_2.Length /= FXAIA00.Num_Tests - I + 1 then

         Report.Failed ("Wrong Length after inserting");

      end if;

   end loop;

   My_Cursor_2 := My_Set_2.Last;

   for I in reverse FXAIA00.Array_Bounds_Type loop

      if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_2) /=
        Value_In_Ptr_Array (I).all then

         Report.Failed ("Mismatch between element and what was inserted #3");

      end if;

      -- Toggle between alternative methods for decrementing cursor

      if I mod 2 = 0 then

         My_Cursor_2 := My_Indefinite_Ordered_Sets.Previous
                          (Position => My_Cursor_2);

      else

         My_Indefinite_Ordered_Sets.Previous (Position => My_Cursor_2);

      end if;

   end loop;

   if My_Set_2.Last_Element /= Value_In_Ptr_Array (Value_In_Ptr_Array'Last).all
      then

      Report.Failed ("Mismatch between last element and last inserted");

   end if;


   -- Test equality and Equivalent_Sets

   if My_Set_1 /= My_Set_2 then

      Report.Failed ("Indefinite_Ordered_Sets not equal");

   end if;

   if not My_Indefinite_Ordered_Sets.Equivalent_Sets
     (Left  => My_Set_1,
      Right => My_Set_2) then

      Report.Failed ("Indefinite_Ordered_Sets not equivalent");

   end if;


   -- Test assignment, Iterate and Reverse_Iterate

   declare

      My_Set_3 : My_Indefinite_Ordered_Sets.Set := My_Set_1;

      I : FXAIA00.Array_Bounds_Type := FXAIA00.Array_Bounds_Type'First;

      procedure My_Process
        (Position : in My_Indefinite_Ordered_Sets.Cursor) is
      begin

         Tampering_Check
           (Container => My_Set_3,
            Where     => "Iterate");

         if My_Indefinite_Ordered_Sets.Element (Position) /=
            Value_In_Ptr_Array (I).all then

            Report.Failed ("Iterate hasn't found the expected value");

         end if;

         if I < FXAIA00.Array_Bounds_Type'Last then

            I := I + 1;

         end if;

      end My_Process;

      procedure My_Reverse_Process
        (Position : in My_Indefinite_Ordered_Sets.Cursor) is
      begin

         if My_Indefinite_Ordered_Sets.Element (Position) /=
            Value_In_Ptr_Array (I).all then

            Report.Failed ("Reverse_Iterate hasn't found the expected value");

         end if;

         if I > FXAIA00.Array_Bounds_Type'First then

            I := I - 1;

         end if;

      end My_Reverse_Process;

   begin

      My_Set_3.Iterate (Process => My_Process'Access);

      My_Set_3.Reverse_Iterate (Process => My_Reverse_Process'Access);

   end;


   -- Test Replace_Element

   -- Modify the values of the two Indefinite_Ordered_Sets in the same way and
   -- check still equal.  Only a small adjustment made so as not to change the
   -- order

   My_Cursor_1 := My_Set_1.First;
   My_Cursor_2 := My_Set_2.First;

   for I in FXAIA00.Array_Bounds_Type loop

      -- Replace by the "Ith" string of the test data but with the second
      -- character incremented

      My_Set_1.Replace_Element
        (Position => My_Cursor_1,
         New_Item => (Value_In_Ptr_Array (I).all(1),
                      Character'Succ (Value_In_Ptr_Array (I).all(2))));

      My_Set_2.Replace_Element
        (Position => My_Cursor_2,
         New_Item => (Value_In_Ptr_Array (I).all(1),
                      Character'Succ (Value_In_Ptr_Array (I).all(2))));

      My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_1);
      My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_2);

   end loop;

   if My_Set_1 /= My_Set_2 then

      Report.Failed ("Incremented Indefinite_Ordered_Sets not equal");

   end if;


   -- Test Clear and inequality

   My_Set_1.Clear;

   if not My_Set_1.Is_Empty then

      Report.Failed ("Failed to clear");

   end if;

   for I in FXAIA00.Array_Bounds_Type loop

      My_Set_1.Insert (New_Item => Value_In_Ptr_Array (I).all);

   end loop;

   if My_Set_1 = My_Set_2 then

      Report.Failed ("Different Indefinite_Ordered_Sets equal");

   end if;


   -- Test Move.  Target has the test values in reverse order, after Move these
   -- should be replaced (not appended) by the test values in forward order

   My_Set_2.Clear;

   for I in FXAIA00.Array_Bounds_Type loop

      My_Set_2.Insert (New_Item => Value_In_Ptr_Array (I).all);

   end loop;

   My_Set_1.Move (Source => My_Set_2);

   if not My_Set_2.Is_Empty then

      Report.Failed ("Moved source not empty");

   end if;

   My_Cursor_1 := My_Set_1.First;

   for I in FXAIA00.Array_Bounds_Type loop

      if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_1) /=
        Value_In_Ptr_Array (I).all then

         Report.Failed ("Target Set not as expected after move");

      end if;

      My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_1);

   end loop;


   -- Test Insert (two forms), Include and Replace (element form)

   -- My_Set_2 should initially be empty
   -- Insert in fairly mixed order to check that order is determined by the
   -- element, not the order of insertion

   declare
   begin

      My_Set_2.Insert (New_Item => Value_In_Ptr_Array (1).all);

   exception

      when Constraint_Error =>

         Report.Failed ("Not inserted #1");

   end;

   declare

      Constraint_Error_Raised : Boolean := False;

   begin

      declare
      begin

         My_Set_2.Insert (New_Item => Value_In_Ptr_Array (1).all);

      exception

         when Constraint_Error =>

            Constraint_Error_Raised := True;

      end;

      if not Constraint_Error_Raised then

         Report.Failed ("Insert with duplicate element should have failed #1");

      end if;

   end;

   My_Set_2.Insert
     (New_Item => Value_In_Ptr_Array (2).all,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #2");

   end if;

   My_Set_2.Insert
     (New_Item => Value_In_Ptr_Array (2).all,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if My_Inserted then

      Report.Failed ("Insert with duplicate element should have failed #2");

   end if;

   -- Element with Default_Value
   My_Set_2.Insert
     (New_Item => My_Default_Value,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #3");

   end if;

   -- Element with Default_Value
   My_Set_2.Insert
     (New_Item => My_Default_Value,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if My_Inserted then

      Report.Failed ("Insert with duplicate element should have failed #3");

   end if;

   declare
   begin

      My_Set_2.Insert (New_Item => Value_In_Ptr_Array (7).all);

   exception

      when Constraint_Error =>

         Report.Failed ("Not inserted #4");

   end;

   declare
   begin

      My_Set_2.Include (New_Item => Value_In_Ptr_Array (2).all);

   exception

      when Constraint_Error =>

         Report.Failed
           ("Include should replace instead of raising Constraint_Error");

   end;

   My_Set_2.Insert
     (New_Item => Value_In_Ptr_Array (9).all,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #5");

   end if;

   declare
   begin

      My_Set_2.Replace (New_Item => Value_In_Ptr_Array (1).all);

   exception

      when Constraint_Error =>

         Report.Failed
           ("Replace should not raise Constraint_Error for existing element");

   end;

   -- Element with characters after those of the Default_Value
   My_Set_2.Insert
     (New_Item => "{{{",
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #6");

   end if;

   -- The order should now be Value_In_Ptr_Array (1).all,
   -- Value_In_Ptr_Array (2).all, Value_In_Ptr_Array (7).all,
   -- Value_In_Ptr_Array (9).all, Default_Value, after Default_Value

   My_Cursor_2 := My_Set_2.First;

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_2) /=
     Value_In_Ptr_Array (1).all then

      Report.Failed ("Inserted value not as expected #1");

   end if;

   My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_2);

   -- Check = Default_Value
   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_2) /=
     Value_In_Ptr_Array (2).all then

      Report.Failed ("Inserted value not as expected #2");

   end if;

   My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_2);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_2) /=
     Value_In_Ptr_Array (7).all then

      Report.Failed ("Inserted value not as expected #3");

   end if;

   My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_2);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_2) /=
     Value_In_Ptr_Array (9).all then

      Report.Failed ("Inserted value not as expected #4");

   end if;

   My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_2);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_2) /=
     My_Default_Value then

      Report.Failed ("Inserted value not as expected #5");

   end if;

   My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_2);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_2) /= "{{{" then

      Report.Failed ("Inserted value not as expected #6");

   end if;


   -- Test Delete (cursor and element forms), Exclude (element form),
   -- Delete_First and Delete_Last

   -- My_Cursor_2 should initially be pointing to the last element of
   -- My_Set_2

   My_Set_2.Delete (Position => My_Cursor_2);

   My_Set_2.Delete (Item => Value_In_Ptr_Array (7).all);

   My_Set_2.Exclude (Item => Value_In_Ptr_Array (9).all);

   declare
   begin

      My_Set_2.Exclude (Item => Value_In_Ptr_Array (9).all);

   exception

      when Constraint_Error =>

         Report.Failed
           ("Exclude should not raise Constraint_Error for absent element");

   end;

   My_Set_2.Delete_First;

   My_Set_2.Delete_Last;

   if My_Set_2.Length /= 1 then

      Report.Failed ("Wrong Length after deleting");

   end if;

   if My_Indefinite_Ordered_Sets.Element (My_Set_2.First) /=
      Value_In_Ptr_Array (2).all then

      Report.Failed ("Remaining value not as expected");

   end if;


   -- Test Find (element form), Floor (element form), Ceiling (element form), <
   -- (3 forms) and > (3 forms)

   -- My_Set_1 should still contain the test values (in forward order)

   My_Cursor_1 := My_Set_1.Find (Item => Value_In_Ptr_Array (9).all);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_1) /=
     Value_In_Ptr_Array (9).all then

      Report.Failed ("Found value not as expected");

   end if;

   My_Cursor_2 := My_Set_1.Floor (Item => Value_In_Ptr_Array (8).all);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_2) /=
     Value_In_Ptr_Array (8).all then

      Report.Failed ("Floor (element form) value not as expected");

   end if;

   if not (My_Cursor_2 < My_Cursor_1) then

      Report.Failed ("< of cursors not as expected");

   end if;

   if My_Cursor_2 > My_Cursor_1 then

      Report.Failed ("> of cursors not as expected");

   end if;

   My_Cursor_1 := My_Set_1.Ceiling (Item => Value_In_Ptr_Array (10).all);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_1) /=
     Value_In_Ptr_Array (10).all then

      Report.Failed ("Ceiling (element form) value not as expected");

   end if;

   if not (Value_In_Ptr_Array (5).all < My_Cursor_1) then

      Report.Failed ("< of element and cursor not as expected");

   end if;

   if Value_In_Ptr_Array (6).all > My_Cursor_1 then

      Report.Failed ("> of element and cursor not as expected");

   end if;

   if not (My_Indefinite_Ordered_Sets.Element
           (My_Indefinite_Ordered_Sets.Previous (My_Cursor_1)) < My_Cursor_1)
     then

      Report.Failed ("< of cursor and element not as expected");

   end if;

   if My_Indefinite_Ordered_Sets.Element (My_Indefinite_Ordered_Sets.Previous
                                            (My_Cursor_1)) >
     My_Cursor_1 then

      Report.Failed ("> of cursor and element not as expected");

   end if;


   -- Test Contains (element form)

   if not My_Set_1.Contains (Item => Value_In_Ptr_Array (3).all) then

      Report.Failed ("Contains (element form) failed to find");

   end if;

   if My_Set_1.Contains (Item => "abc") then

      Report.Failed ("Contains (element form) found when shouldn't have");

   end if;


   -- Test Has_Element

   -- My_Cursor_1 should still be pointing to the last element

   if not My_Indefinite_Ordered_Sets.Has_Element (Position => My_Cursor_1) then

      Report.Failed ("Has_Element failed to find");

   end if;

   My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_1);

   -- My_Cursor_1 should now be pointing off the end

   if My_Indefinite_Ordered_Sets.Has_Element (Position => My_Cursor_1) then

      Report.Failed ("Has_Element found when shouldn't have");

   end if;


   -- Test Union, Overlap and Is_Subset

   -- My_Set_1 should still contain the test values (in forward order).
   -- My_Set_2 should still contain test value Value_In_Ptr_Array (2).all.
   -- My_Set_3 should initially be empty

   My_Set_3.Insert (New_Item => My_Default_Value);

   if My_Indefinite_Ordered_Sets.Overlap
     (Left  => My_Set_3,
      Right => My_Set_2) then

      Report.Failed ("Erroneously thinks has overlap");

   end if;

   if not My_Indefinite_Ordered_Sets.Is_Subset
     (Subset => My_Set_2,
      Of_Set => My_Set_1) then

      Report.Failed ("Erroneously doesn't think is subset");

   end if;

   if My_Indefinite_Ordered_Sets.Is_Subset
     (Subset => My_Set_3,
      Of_Set => My_Set_1) then

      Report.Failed ("Erroneously thinks is subset");

   end if;

   My_Set_3.Union (Source => My_Set_2);

   -- My_Set_3 should contain Value_In_Ptr_Array (2).all, followed by
   -- Default_Value

   if My_Set_3.Length /= 2 then

      Report.Failed ("Length not as expected after Union #1");

   end if;

   My_Cursor_3 := My_Set_3.First;

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     Value_In_Ptr_Array (2).all then

      Report.Failed ("Element not as expected after Union #1");

   end if;

   My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_3);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     My_Default_Value then

      Report.Failed ("Element not as expected after Union #2");

   end if;


   if not My_Indefinite_Ordered_Sets.Overlap
     (Left  => My_Set_1,
      Right => My_Set_2) then

      Report.Failed ("Erroneously doesn't think has overlap");

   end if;

   My_Set_3 := My_Indefinite_Ordered_Sets.Union
                 (Left  => My_Set_1,
                  Right => My_Set_2);

   -- My_Set_3 should contain the test values (in forward order)

   if My_Set_3.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Length not as expected after Union #2");

   end if;

   My_Cursor_3 := My_Set_3.First;

   for I in FXAIA00.Array_Bounds_Type loop

      if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
        Value_In_Ptr_Array (I).all then

         Report.Failed ("Element not as expected after Union #3");

      end if;

      My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_3);

   end loop;


   My_Set_3 := My_Set_1 or My_Set_2;

   -- My_Set_3 should contain the test values (in forward order)

   if My_Set_3.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Length not as expected after Union #3");

   end if;

   My_Cursor_3 := My_Set_3.First;

   for I in FXAIA00.Array_Bounds_Type loop

      if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
        Value_In_Ptr_Array (I).all then

         Report.Failed ("Element not as expected after Union #4");

      end if;

      My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_3);

   end loop;


   -- Test Intersection

   -- My_Set_1 should still contain the test values (in forward order).
   -- My_Set_2 should still contain test value Value_In_Ptr_Array (2).all

   My_Set_3.Clear;

   My_Set_3.Insert (New_Item => My_Default_Value);

   My_Set_3.Intersection (Source => My_Set_2);

   if My_Set_3.Length /= 0 then

      Report.Failed ("Length not as expected after Intersection #1");

   end if;


   My_Set_3 := My_Indefinite_Ordered_Sets.Intersection
                 (Left  => My_Set_1,
                  Right => My_Set_2);

   -- My_Set_3 should contain Value_In_Ptr_Array (2).all

   if My_Set_3.Length /= 1 then

      Report.Failed ("Length not as expected after Intersection #2");

   end if;

   My_Cursor_3 := My_Set_3.First;

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     Value_In_Ptr_Array (2).all then

      Report.Failed ("Element not as expected after Intersection #1");

   end if;


   My_Set_3 := My_Set_1 and My_Set_2;

   -- My_Set_3 should contain Value_In_Ptr_Array (2).all

   if My_Set_3.Length /= 1 then

      Report.Failed ("Length not as expected after Intersection #3");

   end if;

   My_Cursor_3 := My_Set_3.First;

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     Value_In_Ptr_Array (2).all then

      Report.Failed ("Element not as expected after Intersection #2");

   end if;


   -- Test Difference

   -- My_Set_1 should still contain the test values (in forward order).
   -- My_Set_2 should still contain test value Value_In_Ptr_Array (2).all

   My_Set_3.Clear;

   My_Set_3.Insert (New_Item => My_Default_Value);

   My_Set_3.Difference (Source => My_Set_2);

   -- My_Set_3 should contain Default_Value

   if My_Set_3.Length /= 1 then

      Report.Failed ("Length not as expected after Difference #1");

   end if;

   My_Cursor_3 := My_Set_3.First;

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     My_Default_Value then

      Report.Failed ("Element not as expected after Difference #1");

   end if;


   My_Set_3 := My_Indefinite_Ordered_Sets.Difference
                 (Left  => My_Set_1,
                  Right => My_Set_2);

   -- My_Set_3 should contain the test values (in forward order) except for
   -- Value_In_Ptr_Array (2).all

   if My_Set_3.Length /= FXAIA00.Num_Tests - 1 then

      Report.Failed ("Length not as expected after Difference #2");

   end if;


   My_Set_3 := My_Set_1 - My_Set_2;

   -- My_Set_3 should contain the test values (in forward order) except for
   -- Value_In_Ptr_Array (2).all

   if My_Set_3.Length /= FXAIA00.Num_Tests - 1 then

      Report.Failed ("Length not as expected after Difference #2");

   end if;


   -- Test Symmetric_Difference

   -- My_Set_1 should still contain the test values (in forward order).
   -- My_Set_2 should still contain test value Value_In_Ptr_Array (2).all

   My_Set_3.Clear;

   My_Set_3.Insert (New_Item => My_Default_Value);

   My_Set_3.Symmetric_Difference (Source => My_Set_2);

   -- My_Set_3 should contain Value_In_Ptr_Array (2).all, followed by
   -- Default_Value

   if My_Set_3.Length /= 2 then

      Report.Failed ("Length not as expected after Symmetric_Difference #1");

   end if;

   My_Cursor_3 := My_Set_3.First;

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     Value_In_Ptr_Array (2).all then

      Report.Failed ("Element not as expected after Symmetric_Difference #1");

   end if;

   My_Indefinite_Ordered_Sets.Next (Position => My_Cursor_3);

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     My_Default_Value then

      Report.Failed ("Element not as expected after Symmetric_Difference #2");

   end if;


   My_Set_3 := My_Indefinite_Ordered_Sets.Symmetric_Difference
                 (Left  => My_Set_1,
                  Right => My_Set_3);

   -- My_Set_3 should contain the test values (in forward order) except for
   -- Value_In_Ptr_Array (2).all, followed by Default_Value

   if My_Set_3.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Length not as expected after Symmetric_Difference #2");

   end if;


   My_Set_3 := My_Set_3 xor My_Set_2;

   -- My_Set_3 should contain the test values (in forward order), followed by
   -- Default_Value

   if My_Set_3.Length /= FXAIA00.Num_Tests + 1 then

      Report.Failed ("Length not as expected after Symmetric_Difference #3");

   end if;


   -- Test Key, Element (key form), Replace (key form), Exclude (key form),
   -- Delete (key form), Find (key form), Contains (key form) and
   -- Update_Element_Preserving_Key

   -- My_Set_3 should still contain the test values (in forward order), followed
   -- by Default_Value

   My_Cursor_3 := My_Set_3.First;

   if My_Keys.Key (Position => My_Cursor_3) /=
     My_Key_Type (Character'Pos (Value_In_Ptr_Array (1).all(1))) then

      Report.Failed ("Wrong key for cursor");

   end if;

   if My_Keys.Element
     (Container => My_Set_3,
      Key       => My_Key_Type (Character'Pos (Value_In_Ptr_Array (1).all (1))))
         /= Value_In_Ptr_Array (1).all then

      Report.Failed ("Wrong element for key");

   end if;

   declare
   begin

      My_Keys.Replace
        (Container => My_Set_3,
         Key       => My_Key_Type (Character'Pos
                                     (Value_In_Ptr_Array (4).all(1))),
         New_Item  => Value_In_Ptr_Array (4).all);

   exception

      when Constraint_Error =>

         Report.Failed
           ("Replace should not raise Constraint_Error for existing key");

   end;

   if My_Set_3.Length /= FXAIA00.Num_Tests + 1 then

      Report.Failed ("Length not as expected after Replace (key form)");

   end if;

   My_Keys.Exclude
     (Container => My_Set_3,
      Key       => My_Key_Type (Character'Pos (Value_In_Ptr_Array (4).all(1))));

   if My_Set_3.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Length not as expected after Exclude (key form) #1");

   end if;

   declare
   begin

      My_Keys.Exclude
        (Container => My_Set_3,
         Key       => My_Key_Type (Character'Pos
                                     (Value_In_Ptr_Array (4).all(1))));

   exception

      when Constraint_Error =>

         Report.Failed
           ("Exclude (key form) should not raise Constraint_Error for absent" &
            "key");

   end;

   if My_Set_3.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Length not as expected after Exclude (key form) #2");

   end if;

   My_Keys.Delete
     (Container => My_Set_3,
      Key       => My_Key_Type (Character'Pos (Value_In_Ptr_Array (3).all(1))));

   if My_Set_3.Length /= FXAIA00.Num_Tests - 1 then

      Report.Failed ("Length not as expected after Delete (key form)");

   end if;

   My_Cursor_3 := My_Keys.Find
     (Container => My_Set_3,
      Key       => My_Key_Type (Character'Pos (Value_In_Ptr_Array (9).all(1))));

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     Value_In_Ptr_Array (9).all then

      Report.Failed ("Found (key form) value not as expected");

   end if;

   if not My_Keys.Contains
     (Container => My_Set_3,
      Key       => My_Key_Type (Character'Pos (Value_In_Ptr_Array (2).all(1))))
     then

      Report.Failed ("Contains (key form) failed to find");

   end if;

   if My_Keys.Contains
     (Container => My_Set_3,
      Key       => 0) then

      Report.Failed ("Contains (key form) found when shouldn't have");

   end if;

   -- My_Cursor_3 should still be pointing to Value_In_Ptr_Array (9).all

   declare

      procedure My_Update (Element : in out String) is
      begin

         Tampering_Check
           (Container => My_Set_3,
            Where     => "Update_Element");

         -- Increment the second character of the string.  Doesn't change key

         Element (Element'First + 1) :=
                                   Character'Succ (Element (Element'First + 1));

      end My_Update;

   begin

      My_Keys.Update_Element_Preserving_Key
        (Container => My_Set_3,
         Position  => My_Cursor_3,
         Process   => My_Update'Access);

   end;

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) (1) /=
     Value_In_Ptr_Array (9).all(1) then

      Report.Failed ("Updated element not as expected");

   end if;

   if not My_Keys.Equivalent_Keys
           (Left  => My_Keys.Key (Position => My_Cursor_3),
            Right => My_Key_Type (Character'Pos
                                    (Value_In_Ptr_Array (9).all(1)))) then

      Report.Failed ("Keys not equivalent");

   end if;

   My_Cursor_3 := My_Keys.Floor
                    (Container => My_Set_3,
                     Key       => My_Key_Type
                                    (Character'Pos
                                       (Value_In_Ptr_Array (8).all(1))));

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     Value_In_Ptr_Array (8).all then

      Report.Failed ("Floor (key Form) value not as expected");

   end if;

   My_Cursor_3 := My_Keys.Ceiling
                    (Container => My_Set_3,
                     Key       => My_Key_Type
                                    (Character'Pos
                                       (Value_In_Ptr_Array (10).all(1))));

   if My_Indefinite_Ordered_Sets.Element (Position => My_Cursor_3) /=
     Value_In_Ptr_Array (10).all then

      Report.Failed ("Ceiling (key Form) value not as expected");

   end if;


   -- Test Assign and Copy

   My_Set_2.Assign (Source => My_Set_1);

   if My_Set_2.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Target set length not as expected after Assign");

   end if;

   if My_Set_1.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Source set length not left alone by Assign");

   end if;

   My_Set_2 := My_Indefinite_Ordered_Sets.Copy (Source => My_Set_1);

   if My_Set_2.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Result set length not as expected after Copy");

   end if;

   if My_Set_1.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Source set length not left alone by Copy");

   end if;


   Report.Result;

end CXAIA06;
