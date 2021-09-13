------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    System.Atomic_Operations.Exchange                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2019-2021, Free Software Foundation, Inc.  --
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

with System.Atomic_Primitives; use System.Atomic_Primitives;
with Interfaces.C;

package body System.Atomic_Operations.Exchange is

   ---------------------
   -- Atomic_Exchange --
   ---------------------

   function Atomic_Exchange
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      pragma Warnings (Off);
      function Atomic_Exchange_1
        (Ptr   : System.Address;
         Val   : Atomic_Type;
         Model : Mem_Model := Seq_Cst) return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Exchange_1, "__atomic_exchange_1");
      function Atomic_Exchange_2
        (Ptr   : System.Address;
         Val   : Atomic_Type;
         Model : Mem_Model := Seq_Cst) return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Exchange_2, "__atomic_exchange_2");
      function Atomic_Exchange_4
        (Ptr   : System.Address;
         Val   : Atomic_Type;
         Model : Mem_Model := Seq_Cst) return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Exchange_4, "__atomic_exchange_4");
      function Atomic_Exchange_8
        (Ptr   : System.Address;
         Val   : Atomic_Type;
         Model : Mem_Model := Seq_Cst) return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Exchange_8, "__atomic_exchange_8");
      pragma Warnings (On);

   begin
      case Atomic_Type'Object_Size is
         when 8      => return Atomic_Exchange_1 (Item'Address, Value);
         when 16     => return Atomic_Exchange_2 (Item'Address, Value);
         when 32     => return Atomic_Exchange_4 (Item'Address, Value);
         when 64     => return Atomic_Exchange_8 (Item'Address, Value);
         when others => raise Program_Error;
      end case;
   end Atomic_Exchange;

   ---------------------------------
   -- Atomic_Compare_And_Exchange --
   ---------------------------------

   function Atomic_Compare_And_Exchange
     (Item    : aliased in out Atomic_Type;
      Prior   : aliased in out Atomic_Type;
      Desired : Atomic_Type) return Boolean
   is
      pragma Warnings (Off);
      function Atomic_Compare_Exchange_1
        (Ptr           : System.Address;
         Expected      : System.Address;
         Desired       : Atomic_Type;
         Weak          : bool := False;
         Success_Model : Mem_Model := Seq_Cst;
         Failure_Model : Mem_Model := Seq_Cst) return bool;
      pragma Import
        (Intrinsic, Atomic_Compare_Exchange_1, "__atomic_compare_exchange_1");
      function Atomic_Compare_Exchange_2
        (Ptr           : System.Address;
         Expected      : System.Address;
         Desired       : Atomic_Type;
         Weak          : bool := False;
         Success_Model : Mem_Model := Seq_Cst;
         Failure_Model : Mem_Model := Seq_Cst) return bool;
      pragma Import
        (Intrinsic, Atomic_Compare_Exchange_2, "__atomic_compare_exchange_2");
      function Atomic_Compare_Exchange_4
        (Ptr           : System.Address;
         Expected      : System.Address;
         Desired       : Atomic_Type;
         Weak          : bool := False;
         Success_Model : Mem_Model := Seq_Cst;
         Failure_Model : Mem_Model := Seq_Cst) return bool;
      pragma Import
        (Intrinsic, Atomic_Compare_Exchange_4, "__atomic_compare_exchange_4");
      function Atomic_Compare_Exchange_8
        (Ptr           : System.Address;
         Expected      : System.Address;
         Desired       : Atomic_Type;
         Weak          : bool := False;
         Success_Model : Mem_Model := Seq_Cst;
         Failure_Model : Mem_Model := Seq_Cst) return bool;
      pragma Import
        (Intrinsic, Atomic_Compare_Exchange_8, "__atomic_compare_exchange_8");
      pragma Warnings (On);

   begin
      case Atomic_Type'Object_Size is
         when 8 =>
            return Boolean
              (Atomic_Compare_Exchange_1
                (Item'Address, Prior'Address, Desired));
         when 16 =>
            return Boolean
              (Atomic_Compare_Exchange_2
                (Item'Address, Prior'Address, Desired));
         when 32 =>
            return Boolean
              (Atomic_Compare_Exchange_4
                (Item'Address, Prior'Address, Desired));
         when 64 =>
            return Boolean
              (Atomic_Compare_Exchange_8
                (Item'Address, Prior'Address, Desired));
         when others =>
            raise Program_Error;
      end case;
   end Atomic_Compare_And_Exchange;

   ------------------
   -- Is_Lock_Free --
   ------------------

   function Is_Lock_Free (Item : aliased Atomic_Type) return Boolean is
      pragma Unreferenced (Item);
      use type Interfaces.C.size_t;
   begin
      return Boolean (Atomic_Always_Lock_Free (Atomic_Type'Object_Size / 8));
   end Is_Lock_Free;

end System.Atomic_Operations.Exchange;
