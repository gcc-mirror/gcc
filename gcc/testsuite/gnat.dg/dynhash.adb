--  { dg-do run }

with Ada.Text_IO;          use Ada.Text_IO;
with GNAT;                 use GNAT;
with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;

procedure Dynhash is
   function Hash (Key : Integer) return Bucket_Range_Type;

   package DHT is new Dynamic_HTable
     (Key_Type              => Integer,
      Value_Type            => Integer,
      No_Value              => 0,
      Expansion_Threshold   => 1.3,
      Expansion_Factor      => 2,
      Compression_Threshold => 0.3,
      Compression_Factor    => 2,
      "="                   => "=",
      Hash                  => Hash);
   use DHT;

   function Create_And_Populate
     (Low_Key   : Integer;
      High_Key  : Integer;
      Init_Size : Positive) return Instance;
   --  Create a hash table with initial size Init_Size and populate it with
   --  key-value pairs where both keys and values are in the range Low_Key
   --  .. High_Key.

   procedure Check_Empty
     (Caller    : String;
      T         : Instance;
      Low_Key   : Integer;
      High_Key  : Integer);
   --  Ensure that
   --
   --    * The key-value pairs count of hash table T is 0.
   --    * All values for the keys in range Low_Key .. High_Key are 0.

   procedure Check_Keys
     (Caller   : String;
      Iter     : in out Iterator;
      Low_Key  : Integer;
      High_Key : Integer);
   --  Ensure that iterator Iter visits every key in the range Low_Key ..
   --  High_Key exactly once.

   procedure Check_Locked_Mutations (Caller : String; T : in out Instance);
   --  Ensure that all mutation operations of hash table T are locked

   procedure Check_Size
     (Caller    : String;
      T         : Instance;
      Exp_Count : Natural);
   --  Ensure that the count of key-value pairs of hash table T matches
   --  expected count Exp_Count. Emit an error if this is not the case.

   procedure Test_Create (Init_Size : Positive);
   --  Verify that all dynamic hash table operations fail on a non-created
   --  table of size Init_Size.

   procedure Test_Delete_Get_Put_Size
     (Low_Key   : Integer;
      High_Key  : Integer;
      Exp_Count : Natural;
      Init_Size : Positive);
   --  Verify that
   --
   --    * Put properly inserts values in the hash table.
   --    * Get properly retrieves all values inserted in the table.
   --    * Delete properly deletes values.
   --    * The size of the hash table properly reflects the number of key-value
   --      pairs.
   --
   --  Low_Key and High_Key denote the range of keys to be inserted, retrieved,
   --  and deleted. Exp_Count is the expected count of key-value pairs n the
   --  hash table. Init_Size denotes the initial size of the table.

   procedure Test_Iterate
     (Low_Key   : Integer;
      High_Key  : Integer;
      Init_Size : Positive);
   --  Verify that iterators
   --
   --    * Properly visit each key exactly once.
   --    * Mutation operations are properly locked and unlocked during
   --      iteration.
   --
   --  Low_Key and High_Key denote the range of keys to be inserted, retrieved,
   --  and deleted. Init_Size denotes the initial size of the table.

   procedure Test_Iterate_Empty (Init_Size : Positive);
   --  Verify that an iterator over an empty hash table
   --
   --    * Does not visit any key
   --    * Mutation operations are properly locked and unlocked during
   --      iteration.
   --
   --  Init_Size denotes the initial size of the table.

   procedure Test_Iterate_Forced
     (Low_Key   : Integer;
      High_Key  : Integer;
      Init_Size : Positive);
   --  Verify that an iterator that is forcefully advanced by just Next
   --
   --    * Properly visit each key exactly once.
   --    * Mutation operations are properly locked and unlocked during
   --      iteration.
   --
   --  Low_Key and High_Key denote the range of keys to be inserted, retrieved,
   --  and deleted. Init_Size denotes the initial size of the table.

   procedure Test_Replace
     (Low_Val   : Integer;
      High_Val  : Integer;
      Init_Size : Positive);
   --  Verify that Put properly updates the value of a particular key. Low_Val
   --  and High_Val denote the range of values to be updated. Init_Size denotes
   --  the initial size of the table.

   procedure Test_Reset
     (Low_Key   : Integer;
      High_Key  : Integer;
      Init_Size : Positive);
   --  Verify that Reset properly destroy and recreats a hash table. Low_Key
   --  and High_Key denote the range of keys to be inserted in the hash table.
   --  Init_Size denotes the initial size of the table.

   -------------------------
   -- Create_And_Populate --
   -------------------------

   function Create_And_Populate
     (Low_Key   : Integer;
      High_Key  : Integer;
      Init_Size : Positive) return Instance
   is
      T : Instance;

   begin
      T := Create (Init_Size);

      for Key in Low_Key .. High_Key loop
         Put (T, Key, Key);
      end loop;

      return T;
   end Create_And_Populate;

   -----------------
   -- Check_Empty --
   -----------------

   procedure Check_Empty
     (Caller    : String;
      T         : Instance;
      Low_Key   : Integer;
      High_Key  : Integer)
   is
      Val : Integer;

   begin
      Check_Size
        (Caller    => Caller,
         T         => T,
         Exp_Count => 0);

      for Key in Low_Key .. High_Key loop
         Val := Get (T, Key);

         if Val /= 0 then
            Put_Line ("ERROR: " & Caller & ": wrong value");
            Put_Line ("expected: 0");
            Put_Line ("got     :" & Val'Img);
         end if;
      end loop;
   end Check_Empty;

   ----------------
   -- Check_Keys --
   ----------------

   procedure Check_Keys
     (Caller   : String;
      Iter     : in out Iterator;
      Low_Key  : Integer;
      High_Key : Integer)
   is
      type Bit_Vector is array (Low_Key .. High_Key) of Boolean;
      pragma Pack (Bit_Vector);

      Count : Natural;
      Key   : Integer;
      Seen  : Bit_Vector := (others => False);

   begin
      --  Compute the number of outstanding keys that have to be iterated on

      Count := High_Key - Low_Key + 1;

      while Has_Next (Iter) loop
         Next (Iter, Key);

         if Seen (Key) then
            Put_Line
              ("ERROR: " & Caller & ": Check_Keys: duplicate key" & Key'Img);
         else
            Seen (Key) := True;
            Count := Count - 1;
         end if;
      end loop;

      --  In the end, all keys must have been iterated on

      if Count /= 0 then
         for Key in Seen'Range loop
            if not Seen (Key) then
               Put_Line
                 ("ERROR: " & Caller & ": Check_Keys: missing key" & Key'Img);
            end if;
         end loop;
      end if;
   end Check_Keys;

   ----------------------------
   -- Check_Locked_Mutations --
   ----------------------------

   procedure Check_Locked_Mutations (Caller : String; T : in out Instance) is
   begin
      begin
         Delete (T, 1);
         Put_Line ("ERROR: " & Caller & ": Delete: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
           Put_Line ("ERROR: " & Caller & ": Delete: unexpected exception");
      end;

      begin
         Destroy (T);
         Put_Line ("ERROR: " & Caller & ": Destroy: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
           Put_Line ("ERROR: " & Caller & ": Destroy: unexpected exception");
      end;

      begin
         Put (T, 1, 1);
         Put_Line ("ERROR: " & Caller & ": Put: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
           Put_Line ("ERROR: " & Caller & ": Put: unexpected exception");
      end;

      begin
         Reset (T);
         Put_Line ("ERROR: " & Caller & ": Reset: no exception raised");
      exception
         when Iterated =>
            null;
         when others =>
           Put_Line ("ERROR: " & Caller & ": Reset: unexpected exception");
      end;
   end Check_Locked_Mutations;

   ----------------
   -- Check_Size --
   ----------------

   procedure Check_Size 
     (Caller    : String;
      T         : Instance;
      Exp_Count : Natural)
   is
      Count : constant Natural := Size (T);

   begin
      if Count /= Exp_Count then
         Put_Line ("ERROR: " & Caller & ": Size: wrong value");
         Put_Line ("expected:" & Exp_Count'Img);
         Put_Line ("got     :" & Count'Img);
      end if;
   end Check_Size;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Integer) return Bucket_Range_Type is
   begin
      return Bucket_Range_Type (Key);
   end Hash;

   -----------------
   -- Test_Create --
   -----------------

   procedure Test_Create (Init_Size : Positive) is
      Count : Natural;
      Iter  : Iterator;
      T     : Instance;
      Val   : Integer;

   begin
      --  Ensure that every routine defined in the API fails on a hash table
      --  which has not been created yet.

      begin
         Delete (T, 1);
         Put_Line ("ERROR: Test_Create: Delete: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
           Put_Line ("ERROR: Test_Create: Delete: unexpected exception");
      end;

      begin
         Destroy (T);
         Put_Line ("ERROR: Test_Create: Destroy: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
           Put_Line ("ERROR: Test_Create: Destroy: unexpected exception");
      end;

      begin
         Val := Get (T, 1);
         Put_Line ("ERROR: Test_Create: Get: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
           Put_Line ("ERROR: Test_Create: Get: unexpected exception");
      end;

      begin
         Iter := Iterate (T);
         Put_Line ("ERROR: Test_Create: Iterate: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
           Put_Line ("ERROR: Test_Create: Iterate: unexpected exception");
      end;

      begin
         Put (T, 1, 1);
         Put_Line ("ERROR: Test_Create: Put: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
           Put_Line ("ERROR: Test_Create: Put: unexpected exception");
      end;

      begin
         Reset (T);
         Put_Line ("ERROR: Test_Create: Reset: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
           Put_Line ("ERROR: Test_Create: Reset: unexpected exception");
      end;

      begin
         Count := Size (T);
         Put_Line ("ERROR: Test_Create: Size: no exception raised");
      exception
         when Not_Created =>
            null;
         when others =>
           Put_Line ("ERROR: Test_Create: Size: unexpected exception");
      end;

      --  Test create

      T := Create (Init_Size);

      --  Clean up the hash table to prevent memory leaks

      Destroy (T);
   end Test_Create;

   ------------------------------
   -- Test_Delete_Get_Put_Size --
   ------------------------------

   procedure Test_Delete_Get_Put_Size
     (Low_Key   : Integer;
      High_Key  : Integer;
      Exp_Count : Natural;
      Init_Size : Positive)
   is
      Exp_Val : Integer;
      T       : Instance;
      Val     : Integer;

   begin
      T := Create_And_Populate (Low_Key, High_Key, Init_Size);

      --  Ensure that its size matches an expected value

      Check_Size
        (Caller    => "Test_Delete_Get_Put_Size",
         T         => T,
         Exp_Count => Exp_Count);

      --  Ensure that every value for the range of keys exists

      for Key in Low_Key .. High_Key loop
         Val := Get (T, Key);

         if Val /= Key then
            Put_Line ("ERROR: Test_Delete_Get_Put_Size: Get: wrong value");
            Put_Line ("expected:" & Key'Img);
            Put_Line ("got     :" & Val'Img);
         end if;
      end loop;

      --  Delete values whose keys are divisible by 10

      for Key in Low_Key .. High_Key loop
         if Key mod 10 = 0 then
            Delete (T, Key);
         end if;
      end loop;

      --  Ensure that all values whose keys were not deleted still exist

      for Key in Low_Key .. High_Key loop
         if Key mod 10 = 0 then
            Exp_Val := 0;
         else
            Exp_Val := Key;
         end if;

         Val := Get (T, Key);

         if Val /= Exp_Val then
            Put_Line ("ERROR: Test_Delete_Get_Put_Size: Get: wrong value");
            Put_Line ("expected:" & Exp_Val'Img);
            Put_Line ("got     :" & Val'Img);
         end if;
      end loop;

      --  Delete all values

      for Key in Low_Key .. High_Key loop
         Delete (T, Key);
      end loop;

      --  Ensure that the hash table is empty

      Check_Empty
        (Caller   => "Test_Delete_Get_Put_Size",
         T        => T,
         Low_Key  => Low_Key,
         High_Key => High_Key);

      --  Clean up the hash table to prevent memory leaks

      Destroy (T);
   end Test_Delete_Get_Put_Size;

   ------------------
   -- Test_Iterate --
   ------------------

   procedure Test_Iterate
     (Low_Key   : Integer;
      High_Key  : Integer;
      Init_Size : Positive)
   is
      Iter_1 : Iterator;
      Iter_2 : Iterator;
      T      : Instance;

   begin
      T := Create_And_Populate (Low_Key, High_Key, Init_Size);

      --  Obtain an iterator. This action must lock all mutation operations of
      --  the hash table.

      Iter_1 := Iterate (T);

      --  Ensure that every mutation routine defined in the API fails on a hash
      --  table with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         T      => T);

      --  Obtain another iterator

      Iter_2 := Iterate (T);

      --  Ensure that every mutation is still locked

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         T      => T);

      --  Ensure that all keys are iterable. Note that this does not unlock the
      --  mutation operations of the hash table because Iter_2 is not exhausted
      --  yet.

      Check_Keys
        (Caller   => "Test_Iterate",
         Iter     => Iter_1,
         Low_Key  => Low_Key,
         High_Key => High_Key);

      Check_Locked_Mutations
        (Caller => "Test_Iterate",
         T      => T);

      --  Ensure that all keys are iterable. This action unlocks all mutation
      --  operations of the hash table because all outstanding iterators have
      --  been exhausted.

      Check_Keys 
        (Caller   => "Test_Iterate",
         Iter     => Iter_2,
         Low_Key  => Low_Key,
         High_Key => High_Key);

      --  Ensure that all mutation operations are once again callable

      Delete (T, Low_Key);
      Put (T, Low_Key, Low_Key);
      Reset (T);

      --  Clean up the hash table to prevent memory leaks

      Destroy (T);
   end Test_Iterate;

   ------------------------
   -- Test_Iterate_Empty --
   ------------------------

   procedure Test_Iterate_Empty (Init_Size : Positive) is
      Iter : Iterator;
      Key  : Integer;
      T    : Instance;

   begin
      T := Create_And_Populate (0, -1, Init_Size);

      --  Obtain an iterator. This action must lock all mutation operations of
      --  the hash table.

      Iter := Iterate (T);

      --  Ensure that every mutation routine defined in the API fails on a hash
      --  table with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate_Empty",
         T      => T);

      --  Attempt to iterate over the keys

      while Has_Next (Iter) loop
         Next (Iter, Key);

         Put_Line ("ERROR: Test_Iterate_Empty: key" & Key'Img & " exists");
      end loop;

      --  Ensure that all mutation operations are once again callable

      Delete (T, 1);
      Put (T, 1, 1);
      Reset (T);

      --  Clean up the hash table to prevent memory leaks

      Destroy (T);
   end Test_Iterate_Empty;

   -------------------------
   -- Test_Iterate_Forced --
   -------------------------

   procedure Test_Iterate_Forced
     (Low_Key   : Integer;
      High_Key  : Integer;
      Init_Size : Positive)
   is
      Iter : Iterator;
      Key  : Integer;
      T    : Instance;

   begin
      T := Create_And_Populate (Low_Key, High_Key, Init_Size);

      --  Obtain an iterator. This action must lock all mutation operations of
      --  the hash table.

      Iter := Iterate (T);

      --  Ensure that every mutation routine defined in the API fails on a hash
      --  table with at least one outstanding iterator.

      Check_Locked_Mutations
        (Caller => "Test_Iterate_Forced",
         T      => T);

      --  Forcibly advance the iterator until it raises an exception

      begin
         for Guard in Low_Key .. High_Key + 1 loop
            Next (Iter, Key);
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

      Delete (T, Low_Key);
      Put (T, Low_Key, Low_Key);
      Reset (T);

      --  Clean up the hash table to prevent memory leaks

      Destroy (T);
   end Test_Iterate_Forced;

   ------------------
   -- Test_Replace --
   ------------------

   procedure Test_Replace
     (Low_Val   : Integer;
      High_Val  : Integer;
      Init_Size : Positive)
   is
      Key : constant Integer := 1;
      T   : Instance;
      Val : Integer;

   begin
      T := Create (Init_Size);

      --  Ensure the Put properly updates values with the same key

      for Exp_Val in Low_Val .. High_Val loop
         Put (T, Key, Exp_Val);

         Val := Get (T, Key);

         if Val /= Exp_Val then
            Put_Line ("ERROR: Test_Replace: Get: wrong value");
            Put_Line ("expected:" & Exp_Val'Img);
            Put_Line ("got     :" & Val'Img);
         end if;
      end loop;

      --  Clean up the hash table to prevent memory leaks

      Destroy (T);
   end Test_Replace;

   ----------------
   -- Test_Reset --
   ----------------

   procedure Test_Reset 
     (Low_Key   : Integer;
      High_Key  : Integer;
      Init_Size : Positive)
   is
      T : Instance;

   begin
      T := Create_And_Populate (Low_Key, High_Key, Init_Size);

      --  Reset the contents of the hash table

      Reset (T);

      --  Ensure that the hash table is empty

      Check_Empty
        (Caller   => "Test_Reset",
         T        => T,
         Low_Key  => Low_Key,
         High_Key => High_Key);

      --  Clean up the hash table to prevent memory leaks

      Destroy (T);
   end Test_Reset;

--  Start of processing for Operations

begin
   Test_Create (Init_Size => 1);
   Test_Create (Init_Size => 100);

   Test_Delete_Get_Put_Size
     (Low_Key   => 1,
      High_Key  => 1,
      Exp_Count => 1,
      Init_Size => 1);

   Test_Delete_Get_Put_Size
     (Low_Key   => 1,
      High_Key  => 1000,
      Exp_Count => 1000,
      Init_Size => 32);

   Test_Iterate
     (Low_Key   => 1,
      High_Key  => 32,
      Init_Size => 32);

   Test_Iterate_Empty (Init_Size => 32);

   Test_Iterate_Forced
     (Low_Key   => 1,
      High_Key  => 32,
      Init_Size => 32);

   Test_Replace
     (Low_Val   => 1,
      High_Val  => 10,
      Init_Size => 32);

   Test_Reset
     (Low_Key   => 1,
      High_Key  => 1000,
      Init_Size => 100);
end Dynhash;
