------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . A U X _ D E C                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Itanium/VMS version.

--  The Add,Clear_Interlocked subprograms are dubiously implmented due to
--  the lack of a single bit sync_lock_test_and_set builtin.

--  The "Retry" parameter is ignored due to the lack of retry builtins making
--  the subprograms identical to the non-retry versions.

pragma Style_Checks (All_Checks);
--  Turn off alpha ordering check on subprograms, this unit is laid
--  out to correspond to the declarations in the DEC 83 System unit.

with Interfaces;
package body System.Aux_DEC is

   use type Interfaces.Unsigned_8;

   ------------------------
   -- Fetch_From_Address --
   ------------------------

   function Fetch_From_Address (A : Address) return Target is
      type T_Ptr is access all Target;
      function To_T_Ptr is new Ada.Unchecked_Conversion (Address, T_Ptr);
      Ptr : constant T_Ptr := To_T_Ptr (A);
   begin
      return Ptr.all;
   end Fetch_From_Address;

   -----------------------
   -- Assign_To_Address --
   -----------------------

   procedure Assign_To_Address (A : Address; T : Target) is
      type T_Ptr is access all Target;
      function To_T_Ptr is new Ada.Unchecked_Conversion (Address, T_Ptr);
      Ptr : constant T_Ptr := To_T_Ptr (A);
   begin
      Ptr.all := T;
   end Assign_To_Address;

   -----------------------
   -- Clear_Interlocked --
   -----------------------

   procedure Clear_Interlocked
     (Bit       : in out Boolean;
      Old_Value : out Boolean)
   is
      Clr_Bit : Boolean := Bit;
      Old_Uns : Interfaces.Unsigned_8;

      function Sync_Lock_Test_And_Set
        (Ptr   : Address;
         Value : Interfaces.Unsigned_8) return Interfaces.Unsigned_8;
      pragma Import (Intrinsic, Sync_Lock_Test_And_Set,
                     "__sync_lock_test_and_set_1");

   begin
      Old_Uns := Sync_Lock_Test_And_Set (Clr_Bit'Address, 0);
      Bit := Clr_Bit;
      Old_Value := Old_Uns /= 0;
   end Clear_Interlocked;

   procedure Clear_Interlocked
     (Bit          : in out Boolean;
      Old_Value    : out Boolean;
      Retry_Count  : Natural;
      Success_Flag : out Boolean)
   is
      pragma Unreferenced (Retry_Count);

      Clr_Bit : Boolean := Bit;
      Old_Uns : Interfaces.Unsigned_8;

      function Sync_Lock_Test_And_Set
        (Ptr   : Address;
         Value : Interfaces.Unsigned_8) return Interfaces.Unsigned_8;
      pragma Import (Intrinsic, Sync_Lock_Test_And_Set,
                     "__sync_lock_test_and_set_1");

   begin
      Old_Uns := Sync_Lock_Test_And_Set (Clr_Bit'Address, 0);
      Bit := Clr_Bit;
      Old_Value := Old_Uns /= 0;
      Success_Flag := True;
   end Clear_Interlocked;

   ---------------------
   -- Set_Interlocked --
   ---------------------

   procedure Set_Interlocked
     (Bit       : in out Boolean;
      Old_Value : out Boolean)
   is
      Set_Bit : Boolean := Bit;
      Old_Uns : Interfaces.Unsigned_8;

      function Sync_Lock_Test_And_Set
        (Ptr   : Address;
         Value : Interfaces.Unsigned_8) return Interfaces.Unsigned_8;
      pragma Import (Intrinsic, Sync_Lock_Test_And_Set,
                     "__sync_lock_test_and_set_1");

   begin
      Old_Uns := Sync_Lock_Test_And_Set (Set_Bit'Address, 1);
      Bit := Set_Bit;
      Old_Value := Old_Uns /= 0;
   end Set_Interlocked;

   procedure Set_Interlocked
     (Bit          : in out Boolean;
      Old_Value    : out Boolean;
      Retry_Count  : Natural;
      Success_Flag : out Boolean)
   is
      pragma Unreferenced (Retry_Count);

      Set_Bit : Boolean := Bit;
      Old_Uns : Interfaces.Unsigned_8;

      function Sync_Lock_Test_And_Set
        (Ptr   : Address;
         Value : Interfaces.Unsigned_8) return Interfaces.Unsigned_8;
      pragma Import (Intrinsic, Sync_Lock_Test_And_Set,
                     "__sync_lock_test_and_set_1");
   begin
      Old_Uns := Sync_Lock_Test_And_Set (Set_Bit'Address, 1);
      Bit := Set_Bit;
      Old_Value := Old_Uns /= 0;
      Success_Flag := True;
   end Set_Interlocked;

   ---------------------
   -- Add_Interlocked --
   ---------------------

   procedure Add_Interlocked
     (Addend : Short_Integer;
      Augend : in out Aligned_Word;
      Sign   : out Integer)
   is
      Overflowed : Boolean := False;
      Former     : Aligned_Word;

      function Sync_Fetch_And_Add
        (Ptr   : Address;
         Value : Short_Integer) return Short_Integer;
      pragma Import (Intrinsic, Sync_Fetch_And_Add, "__sync_fetch_and_add_2");

   begin
      Former.Value := Sync_Fetch_And_Add (Augend.Value'Address, Addend);

      if Augend.Value < 0 then
         Sign := -1;
      elsif Augend.Value > 0 then
         Sign := 1;
      else
         Sign := 0;
      end if;

      if Former.Value > 0 and then Augend.Value <= 0 then
         Overflowed := True;
      end if;

      if Overflowed then
         raise Constraint_Error;
      end if;
   end Add_Interlocked;

   ----------------
   -- Add_Atomic --
   ----------------

   procedure Add_Atomic
     (To     : in out Aligned_Integer;
      Amount : Integer)
   is
      procedure Sync_Add_And_Fetch
        (Ptr   : Address;
         Value : Integer);
      pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_4");
   begin
      Sync_Add_And_Fetch (To.Value'Address, Amount);
   end Add_Atomic;

   procedure Add_Atomic
     (To           : in out Aligned_Integer;
      Amount       : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      pragma Unreferenced (Retry_Count);

      function Sync_Fetch_And_Add
        (Ptr   : Address;
         Value : Integer) return Integer;
      pragma Import (Intrinsic, Sync_Fetch_And_Add, "__sync_fetch_and_add_4");

   begin
      Old_Value := Sync_Fetch_And_Add (To.Value'Address, Amount);
      Success_Flag := True;
   end Add_Atomic;

   procedure Add_Atomic
     (To     : in out Aligned_Long_Integer;
      Amount : Long_Integer)
   is
      procedure Sync_Add_And_Fetch
        (Ptr   : Address;
         Value : Long_Integer);
      pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_8");
   begin
      Sync_Add_And_Fetch (To.Value'Address, Amount);
   end Add_Atomic;

   procedure Add_Atomic
     (To           : in out Aligned_Long_Integer;
      Amount       : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      pragma Unreferenced (Retry_Count);

      function Sync_Fetch_And_Add
        (Ptr   : Address;
         Value : Long_Integer) return Long_Integer;
      pragma Import (Intrinsic, Sync_Fetch_And_Add, "__sync_fetch_and_add_8");
      --  Why do we keep importing this over and over again???

   begin
      Old_Value := Sync_Fetch_And_Add (To.Value'Address, Amount);
      Success_Flag := True;
   end Add_Atomic;

   ----------------
   -- And_Atomic --
   ----------------

   procedure And_Atomic
     (To   : in out Aligned_Integer;
      From : Integer)
   is
      procedure Sync_And_And_Fetch
        (Ptr   : Address;
         Value : Integer);
      pragma Import (Intrinsic, Sync_And_And_Fetch, "__sync_and_and_fetch_4");
   begin
      Sync_And_And_Fetch (To.Value'Address, From);
   end And_Atomic;

   procedure And_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      pragma Unreferenced (Retry_Count);

      function Sync_Fetch_And_And
        (Ptr   : Address;
         Value : Integer) return Integer;
      pragma Import (Intrinsic, Sync_Fetch_And_And, "__sync_fetch_and_and_4");

   begin
      Old_Value := Sync_Fetch_And_And (To.Value'Address, From);
      Success_Flag := True;
   end And_Atomic;

   procedure And_Atomic
     (To   : in out Aligned_Long_Integer;
      From : Long_Integer)
   is
      procedure Sync_And_And_Fetch
        (Ptr   : Address;
         Value : Long_Integer);
      pragma Import (Intrinsic, Sync_And_And_Fetch, "__sync_and_and_fetch_8");
   begin
      Sync_And_And_Fetch (To.Value'Address, From);
   end And_Atomic;

   procedure And_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      pragma Unreferenced (Retry_Count);

      function Sync_Fetch_And_And
        (Ptr   : Address;
         Value : Long_Integer) return Long_Integer;
      pragma Import (Intrinsic, Sync_Fetch_And_And, "__sync_fetch_and_and_8");

   begin
      Old_Value := Sync_Fetch_And_And (To.Value'Address, From);
      Success_Flag := True;
   end And_Atomic;

   ---------------
   -- Or_Atomic --
   ---------------

   procedure Or_Atomic
     (To   : in out Aligned_Integer;
      From : Integer)
   is
      procedure Sync_Or_And_Fetch
        (Ptr   : Address;
         Value : Integer);
      pragma Import (Intrinsic, Sync_Or_And_Fetch, "__sync_or_and_fetch_4");

   begin
      Sync_Or_And_Fetch (To.Value'Address, From);
   end Or_Atomic;

   procedure Or_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      pragma Unreferenced (Retry_Count);

      function Sync_Fetch_And_Or
        (Ptr   : Address;
         Value : Integer) return Integer;
      pragma Import (Intrinsic, Sync_Fetch_And_Or, "__sync_fetch_and_or_4");

   begin
      Old_Value := Sync_Fetch_And_Or (To.Value'Address, From);
      Success_Flag := True;
   end Or_Atomic;

   procedure Or_Atomic
     (To   : in out Aligned_Long_Integer;
      From : Long_Integer)
   is
      procedure Sync_Or_And_Fetch
        (Ptr   : Address;
         Value : Long_Integer);
      pragma Import (Intrinsic, Sync_Or_And_Fetch, "__sync_or_and_fetch_8");
   begin
      Sync_Or_And_Fetch (To.Value'Address, From);
   end Or_Atomic;

   procedure Or_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      pragma Unreferenced (Retry_Count);

      function Sync_Fetch_And_Or
        (Ptr   : Address;
         Value : Long_Integer) return Long_Integer;
      pragma Import (Intrinsic, Sync_Fetch_And_Or, "__sync_fetch_and_or_8");

   begin
      Old_Value := Sync_Fetch_And_Or (To.Value'Address, From);
      Success_Flag := True;
   end Or_Atomic;

   ------------
   -- Insqhi --
   ------------

   procedure Insqhi
     (Item   : Address;
      Header : Address;
      Status : out Insq_Status) is

      procedure SYS_PAL_INSQHIL
        (STATUS : out Integer; Header : Address; ITEM : Address);
      pragma Import (External, SYS_PAL_INSQHIL);
      pragma Import_Valued_Procedure (SYS_PAL_INSQHIL, "SYS$PAL_INSQHIL",
         (Integer, Address, Address),
         (Value, Value, Value));

      Istat : Integer;

   begin
      SYS_PAL_INSQHIL (Istat, Header, Item);

      if Istat = 0 then
         Status := OK_Not_First;
      elsif Istat = 1 then
         Status := OK_First;

      else
         --  This status is never returned on IVMS

         Status := Fail_No_Lock;
      end if;
   end Insqhi;

   ------------
   -- Remqhi --
   ------------

   procedure Remqhi
     (Header : Address;
      Item   : out Address;
      Status : out Remq_Status)
   is
      --  The removed item is returned in the second function return register,
      --  R9 on IVMS. The VMS ABI calls for "small" records to be returned in
      --  these registers, so inventing this odd looking record type makes that
      --  all work.

      type Remq is record
         Status : Long_Integer;
         Item   : Address;
      end record;

      procedure SYS_PAL_REMQHIL
        (Remret : out Remq; Header : Address);
      pragma Import (External, SYS_PAL_REMQHIL);
      pragma Import_Valued_Procedure
        (SYS_PAL_REMQHIL, "SYS$PAL_REMQHIL",
         (Remq, Address),
         (Value, Value));

      --  Following variables need documentation???

      Rstat  : Long_Integer;
      Remret : Remq;

   begin
      SYS_PAL_REMQHIL (Remret, Header);

      Rstat := Remret.Status;
      Item := Remret.Item;

      if Rstat = 0 then
         Status := Fail_Was_Empty;

      elsif Rstat = 1 then
         Status := OK_Not_Empty;

      elsif Rstat = 2 then
         Status := OK_Empty;

      else
         --  This status is never returned on IVMS

         Status := Fail_No_Lock;
      end if;

   end Remqhi;

   ------------
   -- Insqti --
   ------------

   procedure Insqti
     (Item   : Address;
      Header : Address;
      Status : out Insq_Status) is

      procedure SYS_PAL_INSQTIL
        (STATUS : out Integer; Header : Address; ITEM : Address);
      pragma Import (External, SYS_PAL_INSQTIL);
      pragma Import_Valued_Procedure (SYS_PAL_INSQTIL, "SYS$PAL_INSQTIL",
         (Integer, Address, Address),
         (Value, Value, Value));

      Istat : Integer;

   begin
      SYS_PAL_INSQTIL (Istat, Header, Item);

      if Istat = 0 then
         Status := OK_Not_First;

      elsif Istat = 1 then
         Status := OK_First;

      else
         --  This status is never returned on IVMS

         Status := Fail_No_Lock;
      end if;
   end Insqti;

   ------------
   -- Remqti --
   ------------

   procedure Remqti
     (Header : Address;
      Item   : out Address;
      Status : out Remq_Status)
   is
      --  The removed item is returned in the second function return register,
      --  R9 on IVMS. The VMS ABI calls for "small" records to be returned in
      --  these registers, so inventing (where is rest of this comment???)

      type Remq is record
         Status : Long_Integer;
         Item   : Address;
      end record;

      procedure SYS_PAL_REMQTIL
        (Remret : out Remq; Header : Address);
      pragma Import (External, SYS_PAL_REMQTIL);
      pragma Import_Valued_Procedure (SYS_PAL_REMQTIL, "SYS$PAL_REMQTIL",
         (Remq, Address),
         (Value, Value));

      Rstat  : Long_Integer;
      Remret : Remq;

   begin
      SYS_PAL_REMQTIL (Remret, Header);

      Rstat := Remret.Status;
      Item := Remret.Item;

      --  Wouldn't case be nicer here, and in previous similar cases ???

      if Rstat = 0 then
         Status := Fail_Was_Empty;

      elsif Rstat = 1 then
         Status := OK_Not_Empty;

      elsif Rstat = 2 then
         Status := OK_Empty;
      else
         --  This status is never returned on IVMS

         Status := Fail_No_Lock;
      end if;
   end Remqti;

end System.Aux_DEC;
