------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . A T O M I C _ P R I M I T I V E S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--              Copyright (C) 2012, Free Software Foundation, Inc.          --
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

--  ??? Need header saying what this unit is!!!

package System.Atomic_Primitives is
   pragma Preelaborate;

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

   function Atomic_Compare_Exchange_8
     (X             : Address;
      X_Old         : uint8;
      X_Copy        : uint8) return Boolean;
   pragma Import (Intrinsic,
                  Atomic_Compare_Exchange_8,
                  "__sync_bool_compare_and_swap_1");

   --  ??? Should use __atomic_compare_exchange_1 (doesn't work yet):
   --  function Atomic_Compare_Exchange_8
   --    (X             : Address;
   --     X_Old         : Address;
   --     X_Copy        : uint8;
   --     Success_Model : Mem_Model := Seq_Cst;
   --     Failure_Model : Mem_Model := Seq_Cst) return Boolean;
   --  pragma Import (Intrinsic,
   --                 Atomic_Compare_Exchange_8,
   --                 "__atomic_compare_exchange_1");

   function Atomic_Compare_Exchange_16
     (X             : Address;
      X_Old         : uint16;
      X_Copy        : uint16) return Boolean;
   pragma Import (Intrinsic,
                  Atomic_Compare_Exchange_16,
                  "__sync_bool_compare_and_swap_2");

   function Atomic_Compare_Exchange_32
     (X             : Address;
      X_Old         : uint32;
      X_Copy        : uint32) return Boolean;
   pragma Import (Intrinsic,
                  Atomic_Compare_Exchange_32,
                  "__sync_bool_compare_and_swap_4");

   function Atomic_Compare_Exchange_64
     (X             : Address;
      X_Old         : uint64;
      X_Copy        : uint64) return Boolean;
   pragma Import (Intrinsic,
                  Atomic_Compare_Exchange_64,
                  "__sync_bool_compare_and_swap_8");

   function Atomic_Load_8
     (X     : Address;
      Model : Mem_Model := Seq_Cst) return uint8;
   pragma Import (Intrinsic, Atomic_Load_8, "__atomic_load_1");

   function Atomic_Load_16
     (X     : Address;
      Model : Mem_Model := Seq_Cst) return uint16;
   pragma Import (Intrinsic, Atomic_Load_16, "__atomic_load_2");

   function Atomic_Load_32
     (X     : Address;
      Model : Mem_Model := Seq_Cst) return uint32;
   pragma Import (Intrinsic, Atomic_Load_32, "__atomic_load_4");

   function Atomic_Load_64
     (X     : Address;
      Model : Mem_Model := Seq_Cst) return uint64;
   pragma Import (Intrinsic, Atomic_Load_64, "__atomic_load_8");

end System.Atomic_Primitives;
