------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ P R I M I T I V E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--              Copyright (C) 2012-2019, Free Software Foundation, Inc.     --
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

--  This package contains both atomic primitives defined from gcc built-in
--  functions and operations used by the compiler to generate the lock-free
--  implementation of protected objects.

with Interfaces.C;

package System.Atomic_Primitives is
   pragma Pure;

   type uint is mod 2 ** Long_Integer'Size;

   type uint8  is mod 2**8
     with Size => 8;

   type uint16 is mod 2**16
     with Size => 16;

   type uint32 is mod 2**32
     with Size => 32;

   type uint64 is mod 2**64
     with Size => 64;

   Relaxed : constant := 0;
   Consume : constant := 1;
   Acquire : constant := 2;
   Release : constant := 3;
   Acq_Rel : constant := 4;
   Seq_Cst : constant := 5;
   Last    : constant := 6;

   subtype Mem_Model is Integer range Relaxed .. Last;

   type bool is new Boolean;
   pragma Convention (C, bool);

   ------------------------------------
   -- GCC built-in atomic primitives --
   ------------------------------------

   function Atomic_Load_8
     (Ptr   : Address;
      Model : Mem_Model := Seq_Cst) return uint8;
   pragma Import (Intrinsic, Atomic_Load_8, "__atomic_load_1");

   function Atomic_Load_16
     (Ptr   : Address;
      Model : Mem_Model := Seq_Cst) return uint16;
   pragma Import (Intrinsic, Atomic_Load_16, "__atomic_load_2");

   function Atomic_Load_32
     (Ptr   : Address;
      Model : Mem_Model := Seq_Cst) return uint32;
   pragma Import (Intrinsic, Atomic_Load_32, "__atomic_load_4");

   function Atomic_Load_64
     (Ptr   : Address;
      Model : Mem_Model := Seq_Cst) return uint64;
   pragma Import (Intrinsic, Atomic_Load_64, "__atomic_load_8");

   function Sync_Compare_And_Swap_8
     (Ptr      : Address;
      Expected : uint8;
      Desired  : uint8) return uint8;
   pragma Import (Intrinsic,
                  Sync_Compare_And_Swap_8,
                  "__sync_val_compare_and_swap_1");

   function Sync_Compare_And_Swap_16
     (Ptr      : Address;
      Expected : uint16;
      Desired  : uint16) return uint16;
   pragma Import (Intrinsic,
                  Sync_Compare_And_Swap_16,
                  "__sync_val_compare_and_swap_2");

   function Sync_Compare_And_Swap_32
     (Ptr      : Address;
      Expected : uint32;
      Desired  : uint32) return uint32;
   pragma Import (Intrinsic,
                  Sync_Compare_And_Swap_32,
                  "__sync_val_compare_and_swap_4");

   function Sync_Compare_And_Swap_64
     (Ptr      : Address;
      Expected : uint64;
      Desired  : uint64) return uint64;
   pragma Import (Intrinsic,
                  Sync_Compare_And_Swap_64,
                  "__sync_val_compare_and_swap_8");

   --  ??? We might want to switch to the __atomic series of builtins for
   --  compare-and-swap operations at some point.

   --  function Atomic_Compare_Exchange_8
   --    (Ptr           : Address;
   --     Expected      : Address;
   --     Desired       : uint8;
   --     Weak          : Boolean   := False;
   --     Success_Model : Mem_Model := Seq_Cst;
   --     Failure_Model : Mem_Model := Seq_Cst) return Boolean;
   --  pragma Import (Intrinsic,
   --                 Atomic_Compare_Exchange_8,
   --                 "__atomic_compare_exchange_1");

   function Atomic_Test_And_Set
     (Ptr   : System.Address;
      Model : Mem_Model := Seq_Cst) return bool;
   pragma Import (Intrinsic, Atomic_Test_And_Set, "__atomic_test_and_set");

   procedure Atomic_Clear
     (Ptr   : System.Address;
      Model : Mem_Model := Seq_Cst);
   pragma Import (Intrinsic, Atomic_Clear, "__atomic_clear");

   function Atomic_Always_Lock_Free
     (Size : Interfaces.C.size_t;
      Ptr  : System.Address := System.Null_Address) return bool;
   pragma Import
     (Intrinsic, Atomic_Always_Lock_Free, "__atomic_always_lock_free");

   --------------------------
   -- Lock-free operations --
   --------------------------

   --  The lock-free implementation uses two atomic instructions for the
   --  expansion of protected operations:

   --  * Lock_Free_Read_N atomically loads the value of the protected component
   --    accessed by the current protected operation.

   --  * Lock_Free_Try_Write_N tries to write the Desired value into Ptr only
   --    if Expected and Desired mismatch.

   function Lock_Free_Read_8 (Ptr : Address) return uint8;

   function Lock_Free_Read_16 (Ptr : Address) return uint16;

   function Lock_Free_Read_32 (Ptr : Address) return uint32;

   function Lock_Free_Read_64 (Ptr : Address) return uint64;

   function Lock_Free_Try_Write_8
      (Ptr      : Address;
       Expected : in out uint8;
       Desired  : uint8) return Boolean;

   function Lock_Free_Try_Write_16
      (Ptr      : Address;
       Expected : in out uint16;
       Desired  : uint16) return Boolean;

   function Lock_Free_Try_Write_32
      (Ptr      : Address;
       Expected : in out uint32;
       Desired  : uint32) return Boolean;

   function Lock_Free_Try_Write_64
      (Ptr      : Address;
       Expected : in out uint64;
       Desired  : uint64) return Boolean;

   pragma Inline (Lock_Free_Read_8);
   pragma Inline (Lock_Free_Read_16);
   pragma Inline (Lock_Free_Read_32);
   pragma Inline (Lock_Free_Read_64);
   pragma Inline (Lock_Free_Try_Write_8);
   pragma Inline (Lock_Free_Try_Write_16);
   pragma Inline (Lock_Free_Try_Write_32);
   pragma Inline (Lock_Free_Try_Write_64);
end System.Atomic_Primitives;
