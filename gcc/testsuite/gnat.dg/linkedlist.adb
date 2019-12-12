--  { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;
with GNAT;        use GNAT;
with GNAT.Lists;  use GNAT.Lists;

procedure Linkedlist is
   procedure Destroy (Val : in out Integer) is null;

   package Integer_Lists is new Doubly_Linked_Lists
     (Element_Type    => Integer,
      "="             => "=",
      Destroy_Element => Destroy);
   use Integer_Lists;

   procedure Check_Empty
     (Caller    : String;
      L         : Doubly_Linked_List;
      Low_Elem  : Integer;
      High_Elem : Integer);
   --  Ensure that none of the elements in the range Low_Elem .. High_Elem are
   --  present in list L, and that the list's length is 0.

   procedure Check_Locked_Mutations
     (Caller : String;
      L      : in out Doubly_Linked_List);
   --  Ensure that all mutation operations of list L are locked

   procedure Check_Present
     (Caller    : String;
      L         : Doubly_Linked_List;
      Low_Elem  : Integer;
      High_Elem : Integer);
   --  Ensure that all elements in the range Low_Elem .. High_Elem are present
   --  in list L.

   procedure Check_Unlocked_Mutations
     (Caller : String;
      L      : in out Doubly_Linked_List);
   --  Ensure that all mutation operations of list L are unlocked

   procedure Populate_With_Append
     (L         : Doubly_Linked_List;
      Low_Elem  : Integer;
      High_Elem : Integer);
   --  Add elements in the range Low_Elem .. High_Elem in that order in list L

   procedure Test_Append;
   --  Verify that Append properly inserts at the tail of a list

   procedure Test_Contains
     (Low_Elem  : Integer;
      High_Elem : Integer);
   --  Verify that Contains properly identifies that elements in the range
   --  Low_Elem .. High_Elem are within a list.

   procedure Test_Create;
   --  Verify that all list operations fail on a non-created list

   procedure Test_Delete
     (Low_Elem  : Integer;
      High_Elem : Integer);
   --  Verify that Delete properly removes elements in the range Low_Elem ..
   --  High_Elem from a list.

   procedure Test_Delete_First
     (Low_Elem  : Integer;
      High_Elem : Integer);
   --  Verify that Delete properly removes elements in the range Low_Elem ..
   --  High_Elem from the head of a list.

   procedure Test_Delete_Last
     (Low_Elem  : Integer;
      High_Elem : Integer);
   --  Verify that Delete properly removes elements in the range Low_Elem ..
   --  High_Elem from the tail of a list.

   procedure Test_First;
   --  Verify that First properly returns the head of a list

   procedure Test_Insert_After;
   --  Verify that Insert_After properly adds an element after some other
   --  element.

   procedure Test_Insert_Before;
   --  Vefity that Insert_Before properly adds an element before some other
   --  element.

   procedure Test_Is_Empty;
   --  Verify that Is_Empty properly returns this status of a list

   procedure Test_Iterate;
   --  Verify that iterators properly manipulate mutation operations

   procedure Test_Iterate_Empty;
   --  Verify that iterators properly manipulate mutation operations of an
   --  empty list.

   procedure Test_Iterate_Forced
     (Low_Elem  : Integer;
      High_Elem : Integer);
   --  Verify that an iterator that is forcefully advanced by Next properly
   --  unlocks the mutation operations of a list.

   procedure Test_Last;
   --  Verify that Last properly returns the tail of a list

   procedure Test_Prepend;
   --  Verify that Prepend properly inserts at the head of a list

   procedure Test_Replace;
   --  Verify that Replace properly substitutes old elements with new ones

   procedure Test_Size;
   --  Verify that Size returns the correct size of a list

   -----------------
   -- Check_Empty --
   -----------------

   procedure Check_Empty
     (Caller    : String;
      L         : Doubly_Linked_List;
      Low_Elem  : Integer;
      High_Elem : Integer)
   is
      Len : constant Natural := Size (L);

   begin
      for Elem in Low_Elem .. High_Elem loop
         if Contains (L, Elem) then
            Put_Line ("ERROR: " & Caller & ": extra element" & Elem'Img);
         end if;
      end loop;

      if Len /= 0 then
         Put_Line ("ERROR: " & Caller & ": wrong length");
         Put_Line ("expected: 0");
         Put_Line ("got     :" & Len'Img);
      end if;
   end Check_Empty;

   ----------------------------
   -- Check_Locked_Mutations --
   ----------------------------

   procedure Check_Locked_Mutations
     (Caller : String;
      L      : in out Doubly_Linked_List) is
   begin
      begin
         Append (L, 1);
         Put_Line ("ERROR: " & Caller & ": Append: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line ("ERROR: " & Caller & ": Append: unexpected exception");
      end;

      begin
         Delete (L, 1);
         Put_Line ("ERROR: " & Caller & ": Delete: no exception raised");
      exception
         when List_Empty =>
            null;
         when Iterated =>
            null;
         when others =>
            Put_Line ("ERROR: " & Caller & ": Delete: unexpected exception");
      end;

      begin
         Delete_First (L);
         Put_Line ("ERROR: " & Caller & ": Delete_First: no exception raised");
      exception
         when List_Empty =>
            null;
         when Iterated =>
            null;
         when others =>
            Put_Line
              ("ERROR: " & Caller & ": Delete_First: unexpected exception");
      end;

      begin
         Delete_Last (L);
         Put_Line ("ERROR: " & Caller & ": Delete_List: no exception raised");
      exception
         when List_Empty =>
            null;
         when Iterated =>
            null;
         when others =>
            Put_Line 
              ("ERROR: " & Caller & ": Delete_Last: unexpected exception");
      end;

      begin
         Destroy (L);
         Put_Line ("ERROR: " & Caller & ": Destroy: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line ("ERROR: " & Caller & ": Destroy: unexpected exception");
      end;

      begin
         Insert_After (L, 1, 2);
         Put_Line ("ERROR: " & Caller & ": Insert_After: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line 
              ("ERROR: " & Caller & ": Insert_After: unexpected exception");
      end;

      begin
         Insert_Before (L, 1, 2);
         Put_Line
           ("ERROR: " & Caller & ": Insert_Before: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line
              ("ERROR: " & Caller & ": Insert_Before: unexpected exception");
      end;

      begin
         Prepend (L, 1);
         Put_Line ("ERROR: " & Caller & ": Prepend: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line ("ERROR: " & Caller & ": Prepend: unexpected exception");
      end;

      begin
         Replace (L, 1, 2);
         Put_Line ("ERROR: " & Caller & ": Replace: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line ("ERROR: " & Caller & ": Replace: unexpected exception");
      end;
   end Check_Locked_Mutations;

   -------------------
   -- Check_Present --
   -------------------

   procedure Check_Present
     (Caller    : String;
      L         : Doubly_Linked_List;
      Low_Elem  : Integer;
      High_Elem : Integer)
   is
      Elem : Integer;
      Iter : Iterator;

   begin
      Iter := Iterate (L);
      for Exp_Elem in Low_Elem .. High_Elem loop
         Next (Iter, Elem);

         if Elem /= Exp_Elem then
            Put_Line ("ERROR: " & Caller & ": Check_Present: wrong element");
            Put_Line ("expected:" & Exp_Elem'Img);
            Put_Line ("got     :" & Elem'Img);
         end if;
      end loop;

      --  At this point all elements should have been accounted for. Check for
      --  extra elements.

      while Has_Next (Iter) loop
         Next (Iter, Elem);
         Put_Line
           ("ERROR: " & Caller & ": Check_Present: extra element" & Elem'Img);
      end loop;

   exception
      when Iterator_Exhausted =>
         Put_Line
           ("ERROR: "
            & Caller
            & "Check_Present: incorrect number of elements");
   end Check_Present;

   ------------------------------
   -- Check_Unlocked_Mutations --
   ------------------------------

   procedure Check_Unlocked_Mutations
     (Caller : String;
      L      : in out Doubly_Linked_List)
   is
   begin
      Append        (L, 1);
      Append        (L, 2);
      Append        (L, 3);
      Delete        (L, 1);
      Delete_First  (L);
      Delete_Last   (L);
      Insert_After  (L, 2, 3);
      Insert_Before (L, 2, 1);
      Prepend       (L, 0);
      Replace       (L, 3, 4);
   end Check_Unlocked_Mutations;

   --------------------------
   -- Populate_With_Append --
   --------------------------

   procedure Populate_With_Append
     (L         : Doubly_Linked_List;
      Low_Elem  : Integer;
      High_Elem : Integer)
   is
   begin
      for Elem in Low_Elem .. High_Elem loop
         Append (L, Elem);
      end loop;
   end Populate_With_Append;

   -----------------
   -- Test_Append --
   -----------------

   procedure Test_Append is
      L : Doubly_Linked_List := Create;

   begin
      Append (L, 1);
      Append (L, 2);
      Append (L, 3);
      Append (L, 4);
      Append (L, 5);

      Check_Present
        (Caller    => "Test_Append",
         L         => L,
         Low_Elem  => 1,
         High_Elem => 5);

      Destroy (L);
   end Test_Append;

   -------------------
   -- Test_Contains --
   -------------------

   procedure Test_Contains
     (Low_Elem  : Integer;
      High_Elem : Integer)
   is
      Low_Bogus  : constant Integer := Low_Elem  - 1;
      High_Bogus : constant Integer := High_Elem + 1;

      L : Doubly_Linked_List := Create;

   begin
      Populate_With_Append (L, Low_Elem, High_Elem);

      --  Ensure that the elements are contained in the list

      for Elem in Low_Elem .. High_Elem loop
         if not Contains (L, Elem) then
            Put_Line
              ("ERROR: Test_Contains: element" & Elem'Img & " not in list");
         end if;
      end loop;

      --  Ensure that arbitrary elements which were not inserted in the list
      --  are not contained in the list.

      if Contains (L, Low_Bogus) then
         Put_Line
           ("ERROR: Test_Contains: element" & Low_Bogus'Img & " in list");
      end if;

      if Contains (L, High_Bogus) then
         Put_Line
           ("ERROR: Test_Contains: element" & High_Bogus'Img & " in list");
      end if;

      Destroy (L);
   end Test_Contains;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create is
      Count : Natural;
      Flag  : Boolean;
      Iter  : Iterator;
      L     : Doubly_Linked_List;
      Val   : Integer;

   begin
      --  Ensure that every routine defined in the API fails on a list which
      --  has not been created yet.

      begin
         Append (L, 1);
         Put_Line ("ERROR: Test_Create: Append: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Append: unexpected exception");
      end;

      begin
         Flag := Contains (L, 1);
         Put_Line ("ERROR: Test_Create: Contains: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Contains: unexpected exception");
      end;

      begin
         Delete (L, 1);
         Put_Line ("ERROR: Test_Create: Delete: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Delete: unexpected exception");
      end;

      begin
         Delete_First (L);
         Put_Line ("ERROR: Test_Create: Delete_First: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line
              ("ERROR: Test_Create: Delete_First: unexpected exception");
      end;

      begin
         Delete_Last (L);
         Put_Line ("ERROR: Test_Create: Delete_Last: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Delete_Last: unexpected exception");
      end;

      begin
         Val := First (L);
         Put_Line ("ERROR: Test_Create: First: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: First: unexpected exception");
      end;

      begin
         Insert_After (L, 1, 2);
         Put_Line ("ERROR: Test_Create: Insert_After: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line
              ("ERROR: Test_Create: Insert_After: unexpected exception");
      end;

      begin
         Insert_Before (L, 1, 2);
         Put_Line ("ERROR: Test_Create: Insert_Before: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line
              ("ERROR: Test_Create: Insert_Before: unexpected exception");
      end;

      begin
         Flag := Is_Empty (L);
         Put_Line ("ERROR: Test_Create: Is_Empty: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Is_Empty: unexpected exception");
      end;

      begin
         Iter := Iterate (L);
         Put_Line ("ERROR: Test_Create: Iterate: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Iterate: unexpected exception");
      end;

      begin
         Val := Last (L);
         Put_Line ("ERROR: Test_Create: Last: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Last: unexpected exception");
      end;

      begin
         Prepend (L, 1);
         Put_Line ("ERROR: Test_Create: Prepend: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Prepend: unexpected exception");
      end;

      begin
         Replace (L, 1, 2);
         Put_Line ("ERROR: Test_Create: Replace: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Replace: unexpected exception");
      end;

      begin
         Count := Size (L);
         Put_Line ("ERROR: Test_Create: Size: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Size: unexpected exception");
      end;
   end Test_Create;

   -----------------
   -- Test_Delete --
   -----------------

   procedure Test_Delete
     (Low_Elem  : Integer;
      High_Elem : Integer)
   is
      Iter : Iterator;
      L    : Doubly_Linked_List := Create;

   begin
      Populate_With_Append (L, Low_Elem, High_Elem);

      --  Delete the first element, which is technically the head

      Delete (L, Low_Elem);

      --  Ensure that all remaining elements except for the head are present in
      --  the list.

      Check_Present
        (Caller    => "Test_Delete",
         L         => L,
         Low_Elem  => Low_Elem + 1,
         High_Elem => High_Elem);

      --  Delete the last element, which is technically the tail

      Delete (L, High_Elem);

      --  Ensure that all remaining elements except for the head and tail are
      --  present in the list.

      Check_Present
        (Caller    => "Test_Delete",
         L         => L,
         Low_Elem  => Low_Elem  + 1,
         High_Elem => High_Elem - 1);

      --  Delete all even elements

      for Elem in Low_Elem + 1 .. High_Elem - 1 loop
         if Elem mod 2 = 0 then
            Delete (L, Elem);
         end if;
      end loop;

      --  Ensure that all remaining elements except the head, tail, and even
      --  elements are present in the list.

      for Elem in Low_Elem + 1 .. High_Elem - 1 loop
         if Elem mod 2 /= 0 and then not Contains (L, Elem) then
            Put_Line ("ERROR: Test_Delete: missing element" & Elem'Img);
         end if;
      end loop;

      --  Delete all odd elements

      for Elem in Low_Elem + 1 .. High_Elem - 1 loop
         if Elem mod 2 /= 0 then
            Delete (L, Elem);
         end if;
      end loop;

      --  At this point the list should be completely empty

      Check_Empty
        (Caller    => "Test_Delete",
         L         => L,
         Low_Elem  => Low_Elem,
         High_Elem => High_Elem);

      --  Try to delete an element. This operation should raise List_Empty.

      begin
         Delete (L, Low_Elem);
         Put_Line ("ERROR: Test_Delete: List_Empty not raised");
      exception
         when List_Empty =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Delete: unexpected exception");
      end;

      Destroy (L);
   end Test_Delete;

   -----------------------
   -- Test_Delete_First --
   -----------------------

   procedure Test_Delete_First
     (Low_Elem  : Integer;
      High_Elem : Integer)
   is
      L : Doubly_Linked_List := Create;

   begin
      Populate_With_Append (L, Low_Elem, High_Elem);

      --  Delete the head of the list, and verify that the remaining elements
      --  are still present in the list.

      for Elem in Low_Elem .. High_Elem loop
         Delete_First (L);

         Check_Present
           (Caller    => "Test_Delete_First",
            L         => L,
            Low_Elem  => Elem + 1,
            High_Elem => High_Elem);
      end loop;

      --  At this point the list should be completely empty

      Check_Empty 
        (Caller    => "Test_Delete_First",
         L         => L,
         Low_Elem  => Low_Elem,
         High_Elem => High_Elem);

      --  Try to delete an element. This operation should raise List_Empty.

      begin
         Delete_First (L);
         Put_Line ("ERROR: Test_Delete_First: List_Empty not raised");
      exception
         when List_Empty =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Delete_First: unexpected exception");
      end;

      Destroy (L);
   end Test_Delete_First;

   ----------------------
   -- Test_Delete_Last --
   ----------------------

   procedure Test_Delete_Last
     (Low_Elem  : Integer;
      High_Elem : Integer)
   is
      L : Doubly_Linked_List := Create;

   begin
      Populate_With_Append (L, Low_Elem, High_Elem);

      --  Delete the tail of the list, and verify that the remaining elements
      --  are still present in the list.

      for Elem in reverse Low_Elem .. High_Elem loop
         Delete_Last (L);

         Check_Present
           (Caller    => "Test_Delete_Last",
            L         => L,
            Low_Elem  => Low_Elem,
            High_Elem => Elem - 1);
      end loop;

      --  At this point the list should be completely empty

      Check_Empty
        (Caller    => "Test_Delete_Last",
         L         => L,
         Low_Elem  => Low_Elem,
         High_Elem => High_Elem);

      --  Try to delete an element. This operation should raise List_Empty.

      begin
         Delete_Last (L);
         Put_Line ("ERROR: Test_Delete_Last: List_Empty not raised");
      exception
         when List_Empty =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Delete_First: unexpected exception");
      end;

      Destroy (L);
   end Test_Delete_Last;

   ----------------
   -- Test_First --
   ----------------

   procedure Test_First is
      Elem : Integer;
      L    : Doubly_Linked_List := Create;

   begin
      --  Try to obtain the head. This operation should raise List_Empty.

      begin
         Elem := First (L);
         Put_Line ("ERROR: Test_First: List_Empty not raised");
      exception
         when List_Empty =>
            null;
         when others =>
            Put_Line ("ERROR: Test_First: unexpected exception");
      end;

      Populate_With_Append (L, 1, 2);

      --  Obtain the head

      Elem := First (L);

      if Elem /= 1 then
         Put_Line ("ERROR: Test_First: wrong element");
         Put_Line ("expected: 1");
         Put_Line ("got     :" & Elem'Img);
      end if;

      Destroy (L);
   end Test_First;

   -----------------------
   -- Test_Insert_After --
   -----------------------

   procedure Test_Insert_After is
      L : Doubly_Linked_List := Create;

   begin
      --  Try to insert after a non-inserted element, in an empty list

      Insert_After (L, 1, 2);

      --  At this point the list should be completely empty

      Check_Empty
        (Caller    => "Test_Insert_After",
         L         => L,
         Low_Elem  => 0,
         High_Elem => -1);

      Append (L, 1);           --  1

      Insert_After (L, 1, 3);  --  1, 3
      Insert_After (L, 1, 2);  --  1, 2, 3
      Insert_After (L, 3, 4);  --  1, 2, 3, 4

      --  Try to insert after a non-inserted element, in a full list

      Insert_After (L, 10, 11);

      Check_Present
        (Caller    => "Test_Insert_After",
         L         => L,
         Low_Elem  => 1,
         High_Elem => 4);

      Destroy (L);
   end Test_Insert_After;

   ------------------------
   -- Test_Insert_Before --
   ------------------------

   procedure Test_Insert_Before is
      L : Doubly_Linked_List := Create;

   begin
      --  Try to insert before a non-inserted element, in an empty list

      Insert_Before (L, 1, 2);

      --  At this point the list should be completely empty

      Check_Empty
        (Caller    => "Test_Insert_Before",
         L         => L,
         Low_Elem  => 0,
         High_Elem => -1);

      Append (L, 4);            --  4

      Insert_Before (L, 4, 2);  --  2, 4
      Insert_Before (L, 2, 1);  --  1, 2, 4
      Insert_Before (L, 4, 3);  --  1, 2, 3, 4

      --  Try to insert before a non-inserted element, in a full list

      Insert_Before (L, 10, 11);

      Check_Present
        (Caller    => "Test_Insert_Before",
         L         => L,
         Low_Elem  => 1,
         High_Elem => 4);

      Destroy (L);
   end Test_Insert_Before;

   -------------------
   -- Test_Is_Empty --
   -------------------

   procedure Test_Is_Empty is
      L : Doubly_Linked_List := Create;

   begin
      if not Is_Empty (L) then
         Put_Line ("ERROR: Test_Is_Empty: list is not empty");
      end if;

      Append (L, 1);

      if Is_Empty (L) then
         Put_Line ("ERROR: Test_Is_Empty: list is empty");
      end if;

      Delete_First (L);

      if not Is_Empty (L) then
         Put_Line ("ERROR: Test_Is_Empty: list is not empty");
      end if;

      Destroy (L);
   end Test_Is_Empty;

   ------------------
   -- Test_Iterate --
   ------------------

   procedure Test_Iterate is
      Elem   : Integer;
      Iter_1 : Iterator;
      Iter_2 : Iterator;
      L      : Doubly_Linked_List := Create;

   begin
      Populate_With_Append (L, 1, 5);

      --  Obtain an iterator. This action must lock all mutation operations of
      --  the list.

      Iter_1 := Iterate (L);

      --  Ensure that every mutation routine defined in the API fails on a list
      --  with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         L      => L);

      --  Obtain another iterator

      Iter_2 := Iterate (L);

      --  Ensure that every mutation is still locked

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         L      => L);

      --  Exhaust the first itertor

      while Has_Next (Iter_1) loop
         Next (Iter_1, Elem);
      end loop;

      --  Ensure that every mutation is still locked

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         L      => L);

      --  Exhaust the second itertor

      while Has_Next (Iter_2) loop
         Next (Iter_2, Elem);
      end loop;

      --  Ensure that all mutation operations are once again callable

      Check_Unlocked_Mutations
        (Caller => "Test_Iterate",
         L      => L);

      Destroy (L);
   end Test_Iterate;

   ------------------------
   -- Test_Iterate_Empty --
   ------------------------

   procedure Test_Iterate_Empty is
      Elem : Integer;
      Iter : Iterator;
      L    : Doubly_Linked_List := Create;

   begin
      --  Obtain an iterator. This action must lock all mutation operations of
      --  the list.

      Iter := Iterate (L);

      --  Ensure that every mutation routine defined in the API fails on a list
      --  with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate_Empty",
         L      => L);

      --  Attempt to iterate over the elements

      while Has_Next (Iter) loop
         Next (Iter, Elem);

         Put_Line
           ("ERROR: Test_Iterate_Empty: element" & Elem'Img & " exists");
      end loop;

      --  Ensure that all mutation operations are once again callable

      Check_Unlocked_Mutations
        (Caller => "Test_Iterate_Empty",
         L      => L);

      Destroy (L);
   end Test_Iterate_Empty;

   -------------------------
   -- Test_Iterate_Forced --
   -------------------------

   procedure Test_Iterate_Forced
     (Low_Elem  : Integer;
      High_Elem : Integer)
   is
      Elem : Integer;
      Iter : Iterator;
      L    : Doubly_Linked_List := Create;

   begin
      Populate_With_Append (L, Low_Elem, High_Elem);

      --  Obtain an iterator. This action must lock all mutation operations of
      --  the list.

      Iter := Iterate (L);

      --  Ensure that every mutation routine defined in the API fails on a list
      --  with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate_Forced",
         L      => L);

      --  Forcibly advance the iterator until it raises an exception

      begin
         for Guard in Low_Elem .. High_Elem + 1 loop
            Next (Iter, Elem);
         end loop;

         Put_Line
           ("ERROR: Test_Iterate_Forced: Iterator_Exhausted not raised");
      exception
         when Iterator_Exhausted =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Iterate_Forced: unexpected exception");
      end;

      --  Ensure that all mutation operations are once again callable

      Check_Unlocked_Mutations
        (Caller => "Test_Iterate_Forced",
         L      => L);

      Destroy (L);
   end Test_Iterate_Forced;

   ---------------
   -- Test_Last --
   ---------------

   procedure Test_Last is
      Elem : Integer;
      L    : Doubly_Linked_List := Create;

   begin
      --  Try to obtain the tail. This operation should raise List_Empty.

      begin
         Elem := First (L);
         Put_Line ("ERROR: Test_Last: List_Empty not raised");
      exception
         when List_Empty =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Last: unexpected exception");
      end;

      Populate_With_Append (L, 1, 2);

      --  Obtain the tail

      Elem := Last (L);

      if Elem /= 2 then
         Put_Line ("ERROR: Test_Last: wrong element");
         Put_Line ("expected: 2");
         Put_Line ("got     :" & Elem'Img);
      end if;

      Destroy (L);
   end Test_Last;

   ------------------
   -- Test_Prepend --
   ------------------

   procedure Test_Prepend is
      L : Doubly_Linked_List := Create;

   begin
      Prepend (L, 5);
      Prepend (L, 4);
      Prepend (L, 3);
      Prepend (L, 2);
      Prepend (L, 1);

      Check_Present
        (Caller    => "Test_Prepend",
         L         => L,
         Low_Elem  => 1,
         High_Elem => 5);

      Destroy (L);
   end Test_Prepend;

   ------------------
   -- Test_Replace --
   ------------------

   procedure Test_Replace is
      L : Doubly_Linked_List := Create;

   begin
      Populate_With_Append (L, 1, 5);

      Replace (L, 3, 8);
      Replace (L, 1, 6);
      Replace (L, 4, 9);
      Replace (L, 5, 10);
      Replace (L, 2, 7);

      Replace (L, 11, 12);

      Check_Present
        (Caller    => "Test_Replace",
         L         => L,
         Low_Elem  => 6,
         High_Elem => 10);

      Destroy (L);
   end Test_Replace;

   ---------------
   -- Test_Size --
   ---------------

   procedure Test_Size is
      L : Doubly_Linked_List := Create;
      S : Natural;

   begin
      S := Size (L);

      if S /= 0 then
         Put_Line ("ERROR: Test_Size: wrong size");
         Put_Line ("expected: 0");
         Put_Line ("got     :" & S'Img);
      end if;

      Populate_With_Append (L, 1, 2);
      S := Size (L);

      if S /= 2 then
         Put_Line ("ERROR: Test_Size: wrong size");
         Put_Line ("expected: 2");
         Put_Line ("got     :" & S'Img);
      end if;

      Populate_With_Append (L, 3, 6);
      S := Size (L);

      if S /= 6 then
         Put_Line ("ERROR: Test_Size: wrong size");
         Put_Line ("expected: 6");
         Put_Line ("got     :" & S'Img);
      end if;

      Destroy (L);
   end Test_Size;

--  Start of processing for Operations

begin
   Test_Append;

   Test_Contains
     (Low_Elem  => 1,
      High_Elem => 5);

   Test_Create;

   Test_Delete
     (Low_Elem  => 1,
      High_Elem => 10);

   Test_Delete_First
     (Low_Elem  => 1,
      High_Elem => 5);

   Test_Delete_Last
     (Low_Elem  => 1,
      High_Elem => 5);

   Test_First;
   Test_Insert_After;
   Test_Insert_Before;
   Test_Is_Empty;
   Test_Iterate;
   Test_Iterate_Empty;

   Test_Iterate_Forced
     (Low_Elem  => 1,
      High_Elem => 5);

   Test_Last;
   Test_Prepend;
   Test_Replace;
   Test_Size;
end Linkedlist;
