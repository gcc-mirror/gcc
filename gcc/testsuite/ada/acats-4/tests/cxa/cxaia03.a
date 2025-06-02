-- CXAIA03.A
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
--      in package Ada.Containers.Indefinite_Hashed_Maps.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in package Ada.Containers.Indefinite_Hashed_Maps.
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
with Ada.Containers.Indefinite_Hashed_Maps;
with Report;
with FXAIA00; -- Foundation.

procedure CXAIA03 is

   My_Default_Value : constant String := "zzz";

   Value_In_Ptr_Array : FXAIA00.Value_In_Ptr_Array_Type
      renames FXAIA00.Value_In_Ptr_Array;

   type My_Key_Type is new Integer;

   use type Ada.Containers.Hash_Type;

   function My_Hash (Key : My_Key_Type) return Ada.Containers.Hash_Type is
   begin

      return Ada.Containers.Hash_Type (Key) * 17; -- Some prime

   end My_Hash;

   function My_Equivalent_Keys (Left, Right : My_Key_Type) return Boolean is
   begin

      return Left = Right;

   end My_Equivalent_Keys;

   package My_Indefinite_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => My_Key_Type,
      Element_Type    => String,
      Hash            => My_Hash,
      Equivalent_Keys => My_Equivalent_Keys); -- Default =

   My_Map_1 : My_Indefinite_Hashed_Maps.Map;
   My_Map_2 : My_Indefinite_Hashed_Maps.Map;

   My_Cursor_1 : My_Indefinite_Hashed_Maps.Cursor;
   My_Cursor_2 : My_Indefinite_Hashed_Maps.Cursor;

   My_Inserted : Boolean;

   procedure Tampering_Check
     (Container : in out My_Indefinite_Hashed_Maps.Map;
      Where     : in     String) is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Use a key not already in map

         Container.Insert
           (Key      => FXAIA00.Num_Tests + 1,
            New_Item => Value_In_Ptr_Array (1).all);

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Indefinite_Hashed_Maps.Cursor;
   use type My_Indefinite_Hashed_Maps.Map;


begin

   Report.Test
     ("CXAIA03",
      "Check that an implementation supports the functionality defined in " &
      "package Ada.Containers.Indefinite_Hashed_Maps");


   -- Test Reserve_Capacity and Capacity

   My_Map_1.Reserve_Capacity (Capacity => FXAIA00.Num_Tests);

   My_Map_2.Reserve_Capacity (Capacity => FXAIA00.Num_Tests / 2);

   if My_Map_1.Capacity < FXAIA00.Num_Tests then

      Report.Failed ("Capacity less than expected after Reserve_Capacity");

   end if;


   -- Test empty using Empty_Map, Is_Empty and Length

   if My_Map_1 /= My_Indefinite_Hashed_Maps.Empty_Map then

      Report.Failed ("Not initially empty #1");

   end if;

   if not My_Map_1.Is_Empty then

      Report.Failed ("Not initially empty #2");

   end if;

   if My_Map_1.Length /= 0 then

      Report.Failed ("Not initially empty #3");

   end if;


   -- Test Insert, First, Element (two forms), Key, Query_Element and Next (two
   -- forms)

   for I in FXAIA00.Array_Bounds_Type loop

      My_Map_1.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Ptr_Array (I).all);

      if My_Map_1.Length /= I then

         Report.Failed ("Wrong Length after inserting");

      end if;

   end loop;

   My_Cursor_1 := My_Map_1.First;

   for I in FXAIA00.Array_Bounds_Type loop

      -- Hashed maps are not ordered by key value so could be read out in any
      -- order

      declare

         procedure My_Query
           (Key     : in My_Key_Type;
            Element : in String) is
         begin

            Tampering_Check
              (Container => My_Map_1,
               Where     => "Query_Element");

            if My_Map_1.Element (Key => Key) /= Element then

               Report.Failed ("Element doesn't correspond to key");

            end if;

         end My_Query;

      begin

         if My_Map_1.Element (Key => My_Key_Type (I)) /=
            Value_In_Ptr_Array (I).all then

            Report.Failed ("Mismatch between element and what was inserted");

         end if;

         My_Indefinite_Hashed_Maps.Query_Element
           (Position => My_Cursor_1,
            Process  => My_Query'Access);

      end;

      -- Toggle between alternative methods for incrementing cursor

      if I mod 2 = 0 then

         My_Cursor_1 := My_Indefinite_Hashed_Maps.Next
                          (Position => My_Cursor_1);

      else

         My_Indefinite_Hashed_Maps.Next (Position => My_Cursor_1);

      end if;

   end loop;


   -- Test Insert

   for I in reverse FXAIA00.Array_Bounds_Type loop

      -- Insert the "Keyth" string of the test data

      My_Map_2.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Ptr_Array (I).all);

      if My_Map_2.Length /= FXAIA00.Num_Tests - I + 1 then

         Report.Failed ("Wrong Length after inserting");

      end if;

   end loop;


   if My_Map_2.Capacity < FXAIA00.Num_Tests then

      Report.Failed
        ("Capacity not grown as expected after adding elements beyond" &
         "reserved capacity");

   end if;


   -- Test equality

   if My_Map_1 /= My_Map_2 then

      Report.Failed ("Indefinite_Hashed_Maps not equal");

   end if;


   -- Test assignment and Iterate

   -- Hashed maps are not ordered by key value so could be read out in any
   -- order

   declare

      My_Map_3 : My_Indefinite_Hashed_Maps.Map := My_Map_1;

      procedure My_Process
        (Position : in My_Indefinite_Hashed_Maps.Cursor) is
      begin

         Tampering_Check
           (Container => My_Map_3,
            Where     => "Iterate");

         if Position = My_Indefinite_Hashed_Maps.No_Element then

            Report.Failed ("Iterate passed no element");

         end if;

      end My_Process;

   begin

      My_Map_3.Iterate (Process => My_Process'Access);

   end;


   -- Test Replace_Element and Update_Element

   -- Modify the values of the two Indefinite_Hashed_Maps by two different
   -- methods and check still equal

   My_Cursor_1 := My_Map_1.First;
   My_Cursor_2 := My_Map_2.First;

   for I in FXAIA00.Array_Bounds_Type loop

      declare

         New_String : String := Value_In_Ptr_Array
                                  (Ada.Containers.Count_Type
                                    (My_Indefinite_Hashed_Maps.Key
                                      (Position => My_Cursor_1))).all;

         procedure My_Update
           (Key     : in My_Key_Type;
            Element : in out String) is
         begin

            Tampering_Check
              (Container => My_Map_2,
               Where     => "Update_Element");

            -- Increment the second character of the string.

            Element (Element'First + 1) :=
              Character'Succ (Element (Element'First + 1));

         end My_Update;

      begin

         -- Hashed maps are not ordered by key value so could be read out in any
         -- order

         -- Replace by the "Keyth" string of the test data but with the second
         -- character incremented

         New_String (2) := Character'Succ (New_String (2));

         My_Map_1.Replace_Element
           (Position => My_Cursor_1,
            New_Item => New_String);

         My_Map_2.Update_Element
           (Position => My_Cursor_2,
            Process  => My_Update'Access);

      end;

      My_Indefinite_Hashed_Maps.Next (Position => My_Cursor_1);
      My_Indefinite_Hashed_Maps.Next (Position => My_Cursor_2);

   end loop;

   if My_Map_1 /= My_Map_2 then

      Report.Failed ("Modified Indefinite_Hashed_Maps not equal");

   end if;


   -- Test Clear and inequality

   My_Map_1.Clear;

   if not My_Map_1.Is_Empty then

      Report.Failed ("Failed to clear");

   end if;

   for I in FXAIA00.Array_Bounds_Type loop

      My_Map_1.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Ptr_Array (I).all);

   end loop;

   if My_Map_1 = My_Map_2 then

      Report.Failed ("Different Indefinite_Hashed_Maps equal");

   end if;


   -- Test Move.  Target has the test values in reverse order when sorted by
   -- key, after Move these should be replaced (not appended) by the test values
   -- in forward order when sorted by key

   My_Map_2.Clear;

   for I in FXAIA00.Array_Bounds_Type loop

      My_Map_2.Insert
        (Key      => My_Key_Type (I),
         New_Item => Value_In_Ptr_Array (I).all);

   end loop;

   My_Map_1.Move (Source => My_Map_2);

   if not My_Map_2.Is_Empty then

      Report.Failed ("Moved source not empty");

   end if;

   My_Cursor_1 := My_Map_1.First;

   for I in FXAIA00.Array_Bounds_Type loop

      if My_Map_1.Element (Key => My_Key_Type (I)) /=
         Value_In_Ptr_Array (I).all then

         Report.Failed ("Target Map not as expected after move");

      end if;

      My_Indefinite_Hashed_Maps.Next (Position => My_Cursor_1);

   end loop;


   -- Test Insert (three forms), Include and Replace

   -- My_Map_2 should initially be empty
   -- Insert in fairly mixed order to check that order is determined by the key,
   -- not the order of insertion

   declare
   begin

      My_Map_2.Insert
        (Key      => 3,
         New_Item => Value_In_Ptr_Array (1).all);

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
            New_Item => Value_In_Ptr_Array (1).all);

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
      New_Item => Value_In_Ptr_Array (2).all,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #2");

   end if;

   My_Map_2.Insert
     (Key      => 1,
      New_Item => Value_In_Ptr_Array (2).all,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if My_Inserted then

      Report.Failed ("Insert with duplicate key should have failed #2");

   end if;

   -- Element with Default_Value
   My_Map_2.Insert
     (Key      => 2,
      New_Item => My_Default_Value,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #3");

   end if;

   -- Element with Default_Value
   My_Map_2.Insert
     (Key      => 2,
      New_Item => My_Default_Value,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if My_Inserted then

      Report.Failed ("Insert with duplicate key should have failed #3");

   end if;

   declare
   begin

      My_Map_2.Insert
        (Key      => 6,
         New_Item => Value_In_Ptr_Array (7).all);

   exception

      when Constraint_Error =>

         Report.Failed ("Not inserted #4");

   end;

   declare
   begin

      My_Map_2.Include
        (Key      => 6,
         New_Item => Value_In_Ptr_Array (2).all);

   exception

      when Constraint_Error =>

         Report.Failed
           ("Include should replace instead of raising Constraint_Error");

   end;

   My_Map_2.Insert
     (Key      => 4,
      New_Item => Value_In_Ptr_Array (9).all,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #5");

   end if;

   declare
   begin

      My_Map_2.Replace
        (Key      => 4,
         New_Item => Value_In_Ptr_Array (1).all);

   exception

      when Constraint_Error =>

         Report.Failed
           ("Replace should not raise Constraint_Error for existing key");

   end;

   -- Element with Default_Value
   My_Map_2.Insert
     (Key      => 5,
      New_Item => My_Default_Value,
      Position => My_Cursor_2, -- Added element
      Inserted => My_Inserted);

   if not My_Inserted then

      Report.Failed ("Not inserted #6");

   end if;

   -- The order when sorted by key should now be Value_In_Ptr_Array (2).all,
   -- Default_Value, Value_In_Ptr_Array (1).all, Value_In_Ptr_Array (1).all,
   -- Default_Value, Value_In_Ptr_Array (2).all

   if My_Map_2.Element (Key => 1) /= Value_In_Ptr_Array (2).all
     then

      Report.Failed ("Inserted value not as expected #1");

   end if;

   -- Check = Default_Value
   if My_Map_2.Element (Key => 2) /= My_Default_Value then

      Report.Failed ("Inserted value not as expected #2");

   end if;

   if My_Map_2.Element (Key => 3) /= Value_In_Ptr_Array (1).all
     then

      Report.Failed ("Inserted value not as expected #3");

   end if;

   if My_Map_2.Element (Key => 4) /= Value_In_Ptr_Array (1).all
     then

      Report.Failed ("Inserted value not as expected #4");

   end if;

   if My_Map_2.Element (Key => 5) /= My_Default_Value
     then

      Report.Failed ("Inserted value not as expected #5");

   end if;

   if My_Map_2.Element (Key => 6) /= Value_In_Ptr_Array (2).all then

      Report.Failed ("Inserted value not as expected #6");

   end if;


   -- Test Delete, Exclude, Delete_First and Delete_Last

   -- My_Cursor_2 should be pointing to the element of My_Map_2 with a key of 5

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

   My_Map_2.Delete (Key => 1); -- First

   My_Map_2.Delete (Key => 6); -- Last

   if My_Map_2.Length /= 1 then

      Report.Failed ("Wrong Length after deleting");

   end if;

   -- Check = Default_Value
   if My_Indefinite_Hashed_Maps.Element (My_Map_2.First) /= My_Default_Value
     then

      Report.Failed ("Remaining value not as expected");

   end if;


   -- Test Find

   -- My_Map_1 should still contain the test values (in forward order when
   -- sorted by key)

   My_Cursor_1 := My_Map_1.Find (Key => 10);

   if My_Indefinite_Hashed_Maps.Element (Position => My_Cursor_1) /=
     Value_In_Ptr_Array (10).all then

      Report.Failed ("Found value not as expected");

   end if;


   -- Test Contains

   if not My_Map_1.Contains (Key => 3) then

      Report.Failed ("Contains failed to find");

   end if;

   if My_Map_1.Contains (Key => 0) then

      Report.Failed ("Contains found when shouldn't have");

   end if;


   -- Test Has_Element

   -- My_Set_2 should still contain a single element

   My_Cursor_2 := My_Map_2.First;

   if not My_Indefinite_Hashed_Maps.Has_Element (Position => My_Cursor_2) then

      Report.Failed ("Has_Element failed to find");

   end if;

   My_Indefinite_Hashed_Maps.Next (Position => My_Cursor_2);

   -- My_Cursor_2 should now be pointing off the end

   if My_Indefinite_Hashed_Maps.Has_Element (Position => My_Cursor_2) then

      Report.Failed ("Has_Element found when shouldn't have");

   end if;


   -- Test Assign and Copy

   My_Map_2.Assign (Source => My_Map_1);

   if My_Map_2.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Target map length not as expected after Assign");

   end if;

   if My_Map_1.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Source map length not left alone by Assign");

   end if;

   My_Map_2 := My_Indefinite_Hashed_Maps.Copy (Source => My_Map_1);

   if My_Map_2.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Result map length not as expected after Copy");

   end if;

   if My_Map_1.Length /= FXAIA00.Num_Tests then

      Report.Failed ("Source map length not left alone by Copy");

   end if;


   -- Test Equivalent_Keys (three forms)

   if My_Indefinite_Hashed_Maps.Equivalent_Keys
     (Left  => My_Map_1.First,
      Right => My_Indefinite_Hashed_Maps.Next (My_Map_1.First)) then

      Report.Failed ("Incorrect Equivalent_Keys (cursors form)");

   end if;

   if My_Indefinite_Hashed_Maps.Equivalent_Keys
     (Left  => My_Map_1.First,
      Right => My_Indefinite_Hashed_Maps.Key (My_Indefinite_Hashed_Maps.Next
                                                (My_Map_1.First))) then

      Report.Failed ("Incorrect Equivalent_Keys (cursor, key form) #1");

   end if;

   if not My_Indefinite_Hashed_Maps.Equivalent_Keys
     (Left  => My_Map_1.First,
      Right => My_Indefinite_Hashed_Maps.Key (My_Map_1.First)) then

      Report.Failed ("Incorrect Equivalent_Keys (cursor, key form) #2");

   end if;

   if My_Indefinite_Hashed_Maps.Equivalent_Keys
     (Left  => My_Indefinite_Hashed_Maps.Key (My_Map_1.First),
      Right => My_Indefinite_Hashed_Maps.Next (My_Map_1.First)) then

      Report.Failed ("Incorrect Equivalent_Keys (cursor, key form) #2");

   end if;

   if not My_Indefinite_Hashed_Maps.Equivalent_Keys
     (Left  => My_Indefinite_Hashed_Maps.Key (My_Map_1.First),
      Right => My_Map_1.First) then

      Report.Failed ("Incorrect Equivalent_Keys (cursor, key form) #1");

   end if;


   Report.Result;

end CXAIA03;
