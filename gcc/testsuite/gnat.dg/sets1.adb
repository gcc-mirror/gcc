--  { dg-do run }

with Ada.Text_IO; use Ada.Text_IO;
with GNAT;        use GNAT;
with GNAT.Sets;   use GNAT.Sets;

procedure Sets1 is
   function Hash (Key : Integer) return Bucket_Range_Type;

   package Integer_Sets is new Membership_Set
     (Element_Type => Integer,
      "="          => "=",
      Hash         => Hash);
   use Integer_Sets;

   procedure Check_Empty
     (Caller    : String;
      S         : Instance;
      Low_Elem  : Integer;
      High_Elem : Integer);
   --  Ensure that none of the elements in the range Low_Elem .. High_Elem are
   --  present in set S, and that the set's length is 0.

   procedure Check_Locked_Mutations (Caller : String; S : in out Instance);
   --  Ensure that all mutation operations of set S are locked

   procedure Check_Present
     (Caller    : String;
      S         : Instance;
      Low_Elem  : Integer;
      High_Elem : Integer);
   --  Ensure that all elements in the range Low_Elem .. High_Elem are present
   --  in set S.

   procedure Check_Unlocked_Mutations (Caller : String; S : in out Instance);
   --  Ensure that all mutation operations of set S are unlocked

   procedure Populate
     (S         : Instance;
      Low_Elem  : Integer;
      High_Elem : Integer);
   --  Add elements in the range Low_Elem .. High_Elem in set S

   procedure Test_Contains
     (Low_Elem  : Integer;
      High_Elem : Integer;
      Init_Size : Positive);
   --  Verify that Contains properly identifies that elements in the range
   --  Low_Elem .. High_Elem are within a set. Init_Size denotes the initial
   --  size of the set.

   procedure Test_Create;
   --  Verify that all set operations fail on a non-created set

   procedure Test_Delete
     (Low_Elem  : Integer;
      High_Elem : Integer;
      Init_Size : Positive);
   --  Verify that Delete properly removes elements in the range Low_Elem ..
   --  High_Elem from a set. Init_Size denotes the initial size of the set.

   procedure Test_Is_Empty;
   --  Verify that Is_Empty properly returns this status of a set

   procedure Test_Iterate;
   --  Verify that iterators properly manipulate mutation operations

   procedure Test_Iterate_Empty;
   --  Verify that iterators properly manipulate mutation operations of an
   --  empty set.

   procedure Test_Iterate_Forced
     (Low_Elem  : Integer;
      High_Elem : Integer;
      Init_Size : Positive);
   --  Verify that an iterator that is forcefully advanced by Next properly
   --  unlocks the mutation operations of a set. Init_Size denotes the initial
   --  size of the set.

   procedure Test_Size;
   --  Verify that Size returns the correct size of a set

   -----------------
   -- Check_Empty --
   -----------------

   procedure Check_Empty
     (Caller    : String;
      S         : Instance;
      Low_Elem  : Integer;
      High_Elem : Integer)
   is
      Siz : constant Natural := Size (S);

   begin
      for Elem in Low_Elem .. High_Elem loop
         if Contains (S, Elem) then
            Put_Line ("ERROR: " & Caller & ": extra element" & Elem'Img);
         end if;
      end loop;

      if Siz /= 0 then
         Put_Line ("ERROR: " & Caller & ": wrong size");
         Put_Line ("expected: 0");
         Put_Line ("got     :" & Siz'Img);
      end if;
   end Check_Empty;

   ----------------------------
   -- Check_Locked_Mutations --
   ----------------------------

   procedure Check_Locked_Mutations (Caller : String; S : in out Instance) is
   begin
      begin
         Delete (S, 1);
         Put_Line ("ERROR: " & Caller & ": Delete: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line ("ERROR: " & Caller & ": Delete: unexpected exception");
      end;

      begin
         Destroy (S);
         Put_Line ("ERROR: " & Caller & ": Destroy: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line ("ERROR: " & Caller & ": Destroy: unexpected exception");
      end;

      begin
         Insert (S, 1);
         Put_Line ("ERROR: " & Caller & ": Insert: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
            Put_Line ("ERROR: " & Caller & ": Insert: unexpected exception");
      end;
   end Check_Locked_Mutations;

   -------------------
   -- Check_Present --
   -------------------

   procedure Check_Present
     (Caller    : String;
      S         : Instance;
      Low_Elem  : Integer;
      High_Elem : Integer)
   is
      Elem : Integer;
      Iter : Iterator;

   begin
      Iter := Iterate (S);
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

   procedure Check_Unlocked_Mutations (Caller : String; S : in out Instance) is
   begin
      Delete (S, 1);
      Insert (S, 1);
   end Check_Unlocked_Mutations;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Integer) return Bucket_Range_Type is
   begin
      return Bucket_Range_Type (Key);
   end Hash;

   --------------
   -- Populate --
   --------------

   procedure Populate
     (S         : Instance;
      Low_Elem  : Integer;
      High_Elem : Integer)
   is
   begin
      for Elem in Low_Elem .. High_Elem loop
         Insert (S, Elem);
      end loop;
   end Populate;

   -------------------
   -- Test_Contains --
   -------------------

   procedure Test_Contains
     (Low_Elem  : Integer;
      High_Elem : Integer;
      Init_Size : Positive)
   is
      Low_Bogus  : constant Integer := Low_Elem  - 1;
      High_Bogus : constant Integer := High_Elem + 1;

      S : Instance := Create (Init_Size);

   begin
      Populate (S, Low_Elem, High_Elem);

      --  Ensure that the elements are contained in the set

      for Elem in Low_Elem .. High_Elem loop
         if not Contains (S, Elem) then
            Put_Line
              ("ERROR: Test_Contains: element" & Elem'Img & " not in set");
         end if;
      end loop;

      --  Ensure that arbitrary elements which were not inserted in the set are
      --  not contained in the set.

      if Contains (S, Low_Bogus) then
         Put_Line
           ("ERROR: Test_Contains: element" & Low_Bogus'Img & " in set");
      end if;

      if Contains (S, High_Bogus) then
         Put_Line
           ("ERROR: Test_Contains: element" & High_Bogus'Img & " in set");
      end if;

      Destroy (S);
   end Test_Contains;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create is
      Count : Natural;
      Flag  : Boolean;
      Iter  : Iterator;
      S     : Instance;

   begin
      --  Ensure that every routine defined in the API fails on a set which
      --  has not been created yet.

      begin
         Flag := Contains (S, 1);
         Put_Line ("ERROR: Test_Create: Contains: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Contains: unexpected exception");
      end;

      begin
         Delete (S, 1);
         Put_Line ("ERROR: Test_Create: Delete: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Delete: unexpected exception");
      end;

      begin
         Insert (S, 1);
         Put_Line ("ERROR: Test_Create: Insert: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Insert: unexpected exception");
      end;

      begin
         Flag := Is_Empty (S);
         Put_Line ("ERROR: Test_Create: Is_Empty: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Is_Empty: unexpected exception");
      end;

      begin
         Iter := Iterate (S);
         Put_Line ("ERROR: Test_Create: Iterate: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
            Put_Line ("ERROR: Test_Create: Iterate: unexpected exception");
      end;

      begin
         Count := Size (S);
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
      High_Elem : Integer;
      Init_Size : Positive)
   is
      Iter : Iterator;
      S    : Instance := Create (Init_Size);

   begin
      Populate (S, Low_Elem, High_Elem);

      --  Delete all even elements

      for Elem in Low_Elem .. High_Elem loop
         if Elem mod 2 = 0 then
            Delete (S, Elem);
         end if;
      end loop;

      --  Ensure that all remaining odd elements are present in the set

      for Elem in Low_Elem .. High_Elem loop
         if Elem mod 2 /= 0 and then not Contains (S, Elem) then
            Put_Line ("ERROR: Test_Delete: missing element" & Elem'Img);
         end if;
      end loop;

      --  Delete all odd elements

      for Elem in Low_Elem .. High_Elem loop
         if Elem mod 2 /= 0 then
            Delete (S, Elem);
         end if;
      end loop;

      --  At this point the set should be completely empty

      Check_Empty
        (Caller    => "Test_Delete",
         S         => S,
         Low_Elem  => Low_Elem,
         High_Elem => High_Elem);

      Destroy (S);
   end Test_Delete;

   -------------------
   -- Test_Is_Empty --
   -------------------

   procedure Test_Is_Empty is
      S : Instance := Create (8);

   begin
      if not Is_Empty (S) then
         Put_Line ("ERROR: Test_Is_Empty: set is not empty");
      end if;

      Insert (S, 1);

      if Is_Empty (S) then
         Put_Line ("ERROR: Test_Is_Empty: set is empty");
      end if;

      Delete (S, 1);

      if not Is_Empty (S) then
         Put_Line ("ERROR: Test_Is_Empty: set is not empty");
      end if;

      Destroy (S);
   end Test_Is_Empty;

   ------------------
   -- Test_Iterate --
   ------------------

   procedure Test_Iterate is
      Elem   : Integer;
      Iter_1 : Iterator;
      Iter_2 : Iterator;
      S      : Instance := Create (5);

   begin
      Populate (S, 1, 5);

      --  Obtain an iterator. This action must lock all mutation operations of
      --  the set.

      Iter_1 := Iterate (S);

      --  Ensure that every mutation routine defined in the API fails on a set
      --  with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         S      => S);

      --  Obtain another iterator

      Iter_2 := Iterate (S);

      --  Ensure that every mutation is still locked

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         S      => S);

      --  Exhaust the first itertor

      while Has_Next (Iter_1) loop
         Next (Iter_1, Elem);
      end loop;

      --  Ensure that every mutation is still locked

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         S      => S);

      --  Exhaust the second itertor

      while Has_Next (Iter_2) loop
         Next (Iter_2, Elem);
      end loop;

      --  Ensure that all mutation operations are once again callable

      Check_Unlocked_Mutations
        (Caller => "Test_Iterate",
         S      => S);

      Destroy (S);
   end Test_Iterate;

   ------------------------
   -- Test_Iterate_Empty --
   ------------------------

   procedure Test_Iterate_Empty is
      Elem : Integer;
      Iter : Iterator;
      S    : Instance := Create (5);

   begin
      --  Obtain an iterator. This action must lock all mutation operations of
      --  the set.

      Iter := Iterate (S);

      --  Ensure that every mutation routine defined in the API fails on a set
      --  with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate_Empty",
         S      => S);

      --  Attempt to iterate over the elements

      while Has_Next (Iter) loop
         Next (Iter, Elem);

         Put_Line
           ("ERROR: Test_Iterate_Empty: element" & Elem'Img & " exists");
      end loop;

      --  Ensure that all mutation operations are once again callable

      Check_Unlocked_Mutations
        (Caller => "Test_Iterate_Empty",
         S      => S);

      Destroy (S);
   end Test_Iterate_Empty;

   -------------------------
   -- Test_Iterate_Forced --
   -------------------------

   procedure Test_Iterate_Forced
     (Low_Elem  : Integer;
      High_Elem : Integer;
      Init_Size : Positive)
   is
      Elem : Integer;
      Iter : Iterator;
      S    : Instance := Create (Init_Size);

   begin
      Populate (S, Low_Elem, High_Elem);

      --  Obtain an iterator. This action must lock all mutation operations of
      --  the set.

      Iter := Iterate (S);

      --  Ensure that every mutation routine defined in the API fails on a set
      --  with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate_Forced",
         S      => S);

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
         S      => S);

      Destroy (S);
   end Test_Iterate_Forced;

   ---------------
   -- Test_Size --
   ---------------

   procedure Test_Size is
      S   : Instance := Create (6);
      Siz : Natural;

   begin
      Siz := Size (S);

      if Siz /= 0 then
         Put_Line ("ERROR: Test_Size: wrong size");
         Put_Line ("expected: 0");
         Put_Line ("got     :" & Siz'Img);
      end if;

      Populate (S, 1, 2);
      Siz := Size (S);

      if Siz /= 2 then
         Put_Line ("ERROR: Test_Size: wrong size");
         Put_Line ("expected: 2");
         Put_Line ("got     :" & Siz'Img);
      end if;

      Populate (S, 3, 6);
      Siz := Size (S);

      if Siz /= 6 then
         Put_Line ("ERROR: Test_Size: wrong size");
         Put_Line ("expected: 6");
         Put_Line ("got     :" & Siz'Img);
      end if;

      Destroy (S);
   end Test_Size;

--  Start of processing for Operations

begin
   Test_Contains
     (Low_Elem  => 1,
      High_Elem => 5,
      Init_Size => 5);

   Test_Create;

   Test_Delete
     (Low_Elem  => 1,
      High_Elem => 10,
      Init_Size => 10);

   Test_Is_Empty;
   Test_Iterate;
   Test_Iterate_Empty;

   Test_Iterate_Forced
     (Low_Elem  => 1,
      High_Elem => 5,
      Init_Size => 5);

   Test_Size;
end Sets1;
