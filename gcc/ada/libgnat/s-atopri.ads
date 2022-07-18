------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ P R I M I T I V E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--              Copyright (C) 2012-2024, Free Software Foundation, Inc.     --
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

--  This package contains atomic primitives defined from GCC built-in
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

   ------------------------------------
   -- GCC built-in atomic primitives --
   ------------------------------------

   generic
      type Atomic_Type is mod <>;
   function Atomic_Load
     (Ptr   : Address;
      Model : Mem_Model := Seq_Cst) return Atomic_Type;
   pragma Import (Intrinsic, Atomic_Load, "__atomic_load_n");

   function Atomic_Load_8  is new Atomic_Load (uint8);
   function Atomic_Load_16 is new Atomic_Load (uint16);
   function Atomic_Load_32 is new Atomic_Load (uint32);
   function Atomic_Load_64 is new Atomic_Load (uint64);

   generic
      type Atomic_Type is mod <>;
   procedure Atomic_Store
     (Ptr   : Address;
      Value : Atomic_Type;
      Model : Mem_Model := Seq_Cst);
   pragma Import (Intrinsic, Atomic_Store, "__atomic_store_n");

   procedure Atomic_Store_8  is new Atomic_Store (uint8);
   procedure Atomic_Store_16 is new Atomic_Store (uint16);
   procedure Atomic_Store_32 is new Atomic_Store (uint32);
   procedure Atomic_Store_64 is new Atomic_Store (uint64);

   generic
      type Atomic_Type is mod <>;
   function Atomic_Compare_Exchange
     (Ptr           : Address;
      Expected      : Address;
      Desired       : Atomic_Type;
      Weak          : Boolean   := False;
      Success_Model : Mem_Model := Seq_Cst;
      Failure_Model : Mem_Model := Seq_Cst) return Boolean;
   pragma Import
     (Intrinsic, Atomic_Compare_Exchange, "__atomic_compare_exchange_n");

   function Atomic_Compare_Exchange_8  is new Atomic_Compare_Exchange (uint8);
   function Atomic_Compare_Exchange_16 is new Atomic_Compare_Exchange (uint16);
   function Atomic_Compare_Exchange_32 is new Atomic_Compare_Exchange (uint32);
   function Atomic_Compare_Exchange_64 is new Atomic_Compare_Exchange (uint64);

   function Atomic_Test_And_Set
     (Ptr   : System.Address;
      Model : Mem_Model := Seq_Cst) return Boolean;
   pragma Import (Intrinsic, Atomic_Test_And_Set, "__atomic_test_and_set");

   procedure Atomic_Clear
     (Ptr   : System.Address;
      Model : Mem_Model := Seq_Cst);
   pragma Import (Intrinsic, Atomic_Clear, "__atomic_clear");

   function Atomic_Always_Lock_Free
     (Size : Interfaces.C.size_t;
      Ptr  : System.Address := System.Null_Address) return Boolean;
   pragma Import
     (Intrinsic, Atomic_Always_Lock_Free, "__atomic_always_lock_free");

   --------------------------
   -- Lock-free operations --
   --------------------------

   --  The lock-free implementation uses two atomic instructions for the
   --  expansion of protected operations:

   --  * Lock_Free_Read atomically loads the value contained in Ptr (with the
   --    Acquire synchronization mode).

   --  * Lock_Free_Try_Write atomically tries to write the Desired value into
   --    Ptr if Ptr contains the Expected value. It returns true if the value
   --    in Ptr was changed, or False if it was not, in which case Expected is
   --    updated to the unexpected value in Ptr. Note that it does nothing and
   --    returns true if Desired and Expected are equal.

   generic
      type Atomic_Type is mod <>;
   function Lock_Free_Read (Ptr : Address) return Atomic_Type;

   function Lock_Free_Read_8  is new Lock_Free_Read (uint8);
   function Lock_Free_Read_16 is new Lock_Free_Read (uint16);
   function Lock_Free_Read_32 is new Lock_Free_Read (uint32);
   function Lock_Free_Read_64 is new Lock_Free_Read (uint64);

   generic
      type Atomic_Type is mod <>;
   function Lock_Free_Try_Write
     (Ptr      : Address;
      Expected : in out Atomic_Type;
      Desired  : Atomic_Type) return Boolean;

   function Lock_Free_Try_Write_8  is new Lock_Free_Try_Write (uint8);
   function Lock_Free_Try_Write_16 is new Lock_Free_Try_Write (uint16);
   function Lock_Free_Try_Write_32 is new Lock_Free_Try_Write (uint32);
   function Lock_Free_Try_Write_64 is new Lock_Free_Try_Write (uint64);

private
   pragma Inline (Lock_Free_Read);
   pragma Inline (Lock_Free_Try_Write);
end System.Atomic_Primitives;
