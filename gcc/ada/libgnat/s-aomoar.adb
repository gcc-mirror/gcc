------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               System.Atomic_Operations.Modular_Arithmetic                --
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
with System.Atomic_Operations.Exchange;
with Interfaces.C; use Interfaces;

package body System.Atomic_Operations.Modular_Arithmetic is

   package Exchange is new System.Atomic_Operations.Exchange (Atomic_Type);

   ----------------
   -- Atomic_Add --
   ----------------

   procedure Atomic_Add
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type)
   is
      Ignore : constant Atomic_Type := Atomic_Fetch_And_Add (Item, Value);
   begin
      null;
   end Atomic_Add;

   ---------------------
   -- Atomic_Subtract --
   ---------------------

   procedure Atomic_Subtract
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type)
   is
      Ignore : constant Atomic_Type := Atomic_Fetch_And_Subtract (Item, Value);
   begin
      null;
   end Atomic_Subtract;

   --------------------------
   -- Atomic_Fetch_And_Add --
   --------------------------

   function Atomic_Fetch_And_Add
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      pragma Warnings (Off);
      function Atomic_Fetch_Add_1
        (Ptr : System.Address; Val : Atomic_Type; Model : Mem_Model := Seq_Cst)
        return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Fetch_Add_1, "__atomic_fetch_add_1");
      function Atomic_Fetch_Add_2
        (Ptr : System.Address; Val : Atomic_Type; Model : Mem_Model := Seq_Cst)
        return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Fetch_Add_2, "__atomic_fetch_add_2");
      function Atomic_Fetch_Add_4
        (Ptr : System.Address; Val : Atomic_Type; Model : Mem_Model := Seq_Cst)
        return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Fetch_Add_4, "__atomic_fetch_add_4");
      function Atomic_Fetch_Add_8
        (Ptr : System.Address; Val : Atomic_Type; Model : Mem_Model := Seq_Cst)
        return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Fetch_Add_8, "__atomic_fetch_add_8");
      pragma Warnings (On);

   begin
      --  Use the direct intrinsics when possible, and fallback to
      --  compare-and-exchange otherwise.
      --  Also suppress spurious warnings.

      pragma Warnings (Off);
      if Atomic_Type'Base'Last = Atomic_Type'Last
        and then Atomic_Type'First = 0
        and then Atomic_Type'Last
                  in 2 ** 8 - 1 | 2 ** 16 - 1 | 2 ** 32 - 1 | 2 ** 64 - 1
      then
         pragma Warnings (On);
         case Unsigned_64 (Atomic_Type'Last) is
            when 2 ** 8 - 1  =>
               return Atomic_Fetch_Add_1 (Item'Address, Value);
            when 2 ** 16 - 1 =>
               return Atomic_Fetch_Add_2 (Item'Address, Value);
            when 2 ** 32 - 1 =>
               return Atomic_Fetch_Add_4 (Item'Address, Value);
            when 2 ** 64 - 1 =>
               return Atomic_Fetch_Add_8 (Item'Address, Value);
            when others      =>
               raise Program_Error;
         end case;
      else
         declare
            Old_Value : aliased Atomic_Type := Item;
            New_Value : Atomic_Type := Old_Value + Value;
         begin
            --  Keep iterating until the exchange succeeds

            while not Exchange.Atomic_Compare_And_Exchange
                        (Item, Old_Value, New_Value)
            loop
               New_Value := Old_Value + Value;
            end loop;

            return Old_Value;
         end;
      end if;
   end Atomic_Fetch_And_Add;

   -------------------------------
   -- Atomic_Fetch_And_Subtract --
   -------------------------------

   function Atomic_Fetch_And_Subtract
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
   is
      pragma Warnings (Off);
      function Atomic_Fetch_Sub_1
        (Ptr : System.Address; Val : Atomic_Type; Model : Mem_Model := Seq_Cst)
        return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Fetch_Sub_1, "__atomic_fetch_sub_1");
      function Atomic_Fetch_Sub_2
        (Ptr : System.Address; Val : Atomic_Type; Model : Mem_Model := Seq_Cst)
        return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Fetch_Sub_2, "__atomic_fetch_sub_2");
      function Atomic_Fetch_Sub_4
        (Ptr : System.Address; Val : Atomic_Type; Model : Mem_Model := Seq_Cst)
        return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Fetch_Sub_4, "__atomic_fetch_sub_4");
      function Atomic_Fetch_Sub_8
        (Ptr : System.Address; Val : Atomic_Type; Model : Mem_Model := Seq_Cst)
        return Atomic_Type;
      pragma Import (Intrinsic, Atomic_Fetch_Sub_8, "__atomic_fetch_sub_8");
      pragma Warnings (On);

   begin
      --  Use the direct intrinsics when possible, and fallback to
      --  compare-and-exchange otherwise.
      --  Also suppress spurious warnings.

      pragma Warnings (Off);
      if Atomic_Type'Base'Last = Atomic_Type'Last
        and then Atomic_Type'First = 0
        and then Atomic_Type'Last
                  in 2 ** 8 - 1 | 2 ** 16 - 1 | 2 ** 32 - 1 | 2 ** 64 - 1
      then
         pragma Warnings (On);
         case Unsigned_64 (Atomic_Type'Last) is
            when 2 ** 8 - 1  =>
               return Atomic_Fetch_Sub_1 (Item'Address, Value);
            when 2 ** 16 - 1 =>
               return Atomic_Fetch_Sub_2 (Item'Address, Value);
            when 2 ** 32 - 1 =>
               return Atomic_Fetch_Sub_4 (Item'Address, Value);
            when 2 ** 64 - 1 =>
               return Atomic_Fetch_Sub_8 (Item'Address, Value);
            when others      =>
               raise Program_Error;
         end case;
      else
         declare
            Old_Value : aliased Atomic_Type := Item;
            New_Value : Atomic_Type := Old_Value - Value;
         begin
            --  Keep iterating until the exchange succeeds

            while not Exchange.Atomic_Compare_And_Exchange
                        (Item, Old_Value, New_Value)
            loop
               New_Value := Old_Value - Value;
            end loop;

            return Old_Value;
         end;
      end if;
   end Atomic_Fetch_And_Subtract;

   ------------------
   -- Is_Lock_Free --
   ------------------

   function Is_Lock_Free (Item : aliased Atomic_Type) return Boolean is
      pragma Unreferenced (Item);
      use type Interfaces.C.size_t;
   begin
      return Boolean (Atomic_Always_Lock_Free (Atomic_Type'Object_Size / 8));
   end Is_Lock_Free;

end System.Atomic_Operations.Modular_Arithmetic;
