------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . A U X _ D E C                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/Or modify it under --
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

pragma Style_Checks (All_Checks);
--  Turn off alpha ordering check on subprograms, this unit is laid
--  out to correspond to the declarations in the DEC 83 System unit.

with System.Soft_Links;

package body System.Aux_DEC is

   package SSL renames System.Soft_Links;

   -----------------------------------
   -- Operations on Largest_Integer --
   -----------------------------------

   --  It would be nice to replace these with intrinsics, but that does
   --  not work yet (the back end would be ok, but GNAT itself objects)

   type LIU is mod 2 ** Largest_Integer'Size;
   --  Unsigned type of same length as Largest_Integer

   function To_LI   is new Ada.Unchecked_Conversion (LIU, Largest_Integer);
   function From_LI is new Ada.Unchecked_Conversion (Largest_Integer, LIU);

   function "not" (Left : Largest_Integer) return Largest_Integer is
   begin
      return To_LI (not From_LI (Left));
   end "not";

   function "and" (Left, Right : Largest_Integer) return Largest_Integer is
   begin
      return To_LI (From_LI (Left) and From_LI (Right));
   end "and";

   function "or"  (Left, Right : Largest_Integer) return Largest_Integer is
   begin
      return To_LI (From_LI (Left) or From_LI (Right));
   end "or";

   function "xor" (Left, Right : Largest_Integer) return Largest_Integer is
   begin
      return To_LI (From_LI (Left) xor From_LI (Right));
   end "xor";

   --------------------------------------
   -- Arithmetic Operations on Address --
   --------------------------------------

   --  It would be nice to replace these with intrinsics, but that does
   --  not work yet (the back end would be ok, but GNAT itself objects)

   Asiz : constant Integer := Integer (Address'Size) - 1;

   type SA is range -(2 ** Asiz) .. 2 ** Asiz - 1;
   --  Signed type of same size as Address

   function To_A   is new Ada.Unchecked_Conversion (SA, Address);
   function From_A is new Ada.Unchecked_Conversion (Address, SA);

   function "+" (Left : Address; Right : Integer) return Address is
   begin
      return To_A (From_A (Left) + SA (Right));
   end "+";

   function "+" (Left : Integer; Right : Address) return Address is
   begin
      return To_A (SA (Left) + From_A (Right));
   end "+";

   function "-" (Left : Address; Right : Address) return Integer is
      pragma Unsuppress (All_Checks);
      --  Because this can raise Constraint_Error for 64-bit addresses
   begin
      return Integer (From_A (Left) - From_A (Right));
   end "-";

   function "-" (Left : Address; Right : Integer) return Address is
   begin
      return To_A (From_A (Left) - SA (Right));
   end "-";

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

   ---------------------------------
   -- Operations on Unsigned_Byte --
   ---------------------------------

   --  It would be nice to replace these with intrinsics, but that does
   --  not work yet (the back end would be ok, but GNAT itself objects)

   type BU is mod 2 ** Unsigned_Byte'Size;
   --  Unsigned type of same length as Unsigned_Byte

   function To_B   is new Ada.Unchecked_Conversion (BU, Unsigned_Byte);
   function From_B is new Ada.Unchecked_Conversion (Unsigned_Byte, BU);

   function "not" (Left : Unsigned_Byte) return Unsigned_Byte is
   begin
      return To_B (not From_B (Left));
   end "not";

   function "and" (Left, Right : Unsigned_Byte) return Unsigned_Byte is
   begin
      return To_B (From_B (Left) and From_B (Right));
   end "and";

   function "or"  (Left, Right : Unsigned_Byte) return Unsigned_Byte is
   begin
      return To_B (From_B (Left) or From_B (Right));
   end "or";

   function "xor" (Left, Right : Unsigned_Byte) return Unsigned_Byte is
   begin
      return To_B (From_B (Left) xor From_B (Right));
   end "xor";

   ---------------------------------
   -- Operations on Unsigned_Word --
   ---------------------------------

   --  It would be nice to replace these with intrinsics, but that does
   --  not work yet (the back end would be ok, but GNAT itself objects)

   type WU is mod 2 ** Unsigned_Word'Size;
   --  Unsigned type of same length as Unsigned_Word

   function To_W   is new Ada.Unchecked_Conversion (WU, Unsigned_Word);
   function From_W is new Ada.Unchecked_Conversion (Unsigned_Word, WU);

   function "not" (Left : Unsigned_Word) return Unsigned_Word is
   begin
      return To_W (not From_W (Left));
   end "not";

   function "and" (Left, Right : Unsigned_Word) return Unsigned_Word is
   begin
      return To_W (From_W (Left) and From_W (Right));
   end "and";

   function "or"  (Left, Right : Unsigned_Word) return Unsigned_Word is
   begin
      return To_W (From_W (Left) or From_W (Right));
   end "or";

   function "xor" (Left, Right : Unsigned_Word) return Unsigned_Word is
   begin
      return To_W (From_W (Left) xor From_W (Right));
   end "xor";

   -------------------------------------
   -- Operations on Unsigned_Longword --
   -------------------------------------

   --  It would be nice to replace these with intrinsics, but that does
   --  not work yet (the back end would be ok, but GNAT itself objects)

   type LWU is mod 2 ** Unsigned_Longword'Size;
   --  Unsigned type of same length as Unsigned_Longword

   function To_LW   is new Ada.Unchecked_Conversion (LWU, Unsigned_Longword);
   function From_LW is new Ada.Unchecked_Conversion (Unsigned_Longword, LWU);

   function "not" (Left : Unsigned_Longword) return Unsigned_Longword is
   begin
      return To_LW (not From_LW (Left));
   end "not";

   function "and" (Left, Right : Unsigned_Longword) return Unsigned_Longword is
   begin
      return To_LW (From_LW (Left) and From_LW (Right));
   end "and";

   function "or"  (Left, Right : Unsigned_Longword) return Unsigned_Longword is
   begin
      return To_LW (From_LW (Left) or From_LW (Right));
   end "or";

   function "xor" (Left, Right : Unsigned_Longword) return Unsigned_Longword is
   begin
      return To_LW (From_LW (Left) xor From_LW (Right));
   end "xor";

   -------------------------------
   -- Operations on Unsigned_32 --
   -------------------------------

   --  It would be nice to replace these with intrinsics, but that does
   --  not work yet (the back end would be ok, but GNAT itself objects)

   type U32 is mod 2 ** Unsigned_32'Size;
   --  Unsigned type of same length as Unsigned_32

   function To_U32   is new Ada.Unchecked_Conversion (U32, Unsigned_32);
   function From_U32 is new Ada.Unchecked_Conversion (Unsigned_32, U32);

   function "not" (Left : Unsigned_32) return Unsigned_32 is
   begin
      return To_U32 (not From_U32 (Left));
   end "not";

   function "and" (Left, Right : Unsigned_32) return Unsigned_32 is
   begin
      return To_U32 (From_U32 (Left) and From_U32 (Right));
   end "and";

   function "or"  (Left, Right : Unsigned_32) return Unsigned_32 is
   begin
      return To_U32 (From_U32 (Left) or From_U32 (Right));
   end "or";

   function "xor" (Left, Right : Unsigned_32) return Unsigned_32 is
   begin
      return To_U32 (From_U32 (Left) xor From_U32 (Right));
   end "xor";

   -------------------------------------
   -- Operations on Unsigned_Quadword --
   -------------------------------------

   --  It would be nice to replace these with intrinsics, but that does
   --  not work yet (the back end would be ok, but GNAT itself objects)

   type QWU is mod 2 ** 64;  -- 64 = Unsigned_Quadword'Size
   --  Unsigned type of same length as Unsigned_Quadword

   function To_QW   is new Ada.Unchecked_Conversion (QWU, Unsigned_Quadword);
   function From_QW is new Ada.Unchecked_Conversion (Unsigned_Quadword, QWU);

   function "not" (Left : Unsigned_Quadword) return Unsigned_Quadword is
   begin
      return To_QW (not From_QW (Left));
   end "not";

   function "and" (Left, Right : Unsigned_Quadword) return Unsigned_Quadword is
   begin
      return To_QW (From_QW (Left) and From_QW (Right));
   end "and";

   function "or"  (Left, Right : Unsigned_Quadword) return Unsigned_Quadword is
   begin
      return To_QW (From_QW (Left) or From_QW (Right));
   end "or";

   function "xor" (Left, Right : Unsigned_Quadword) return Unsigned_Quadword is
   begin
      return To_QW (From_QW (Left) xor From_QW (Right));
   end "xor";

   -----------------------
   -- Clear_Interlocked --
   -----------------------

   procedure Clear_Interlocked
     (Bit       : in out Boolean;
      Old_Value : out Boolean)
   is
   begin
      SSL.Lock_Task.all;
      Old_Value := Bit;
      Bit := False;
      SSL.Unlock_Task.all;
   end Clear_Interlocked;

   procedure Clear_Interlocked
     (Bit          : in out Boolean;
      Old_Value    : out Boolean;
      Retry_Count  : Natural;
      Success_Flag : out Boolean)
   is
      pragma Warnings (Off, Retry_Count);

   begin
      SSL.Lock_Task.all;
      Old_Value := Bit;
      Bit := False;
      Success_Flag := True;
      SSL.Unlock_Task.all;
   end Clear_Interlocked;

   ---------------------
   -- Set_Interlocked --
   ---------------------

   procedure Set_Interlocked
     (Bit       : in out Boolean;
      Old_Value : out Boolean)
   is
   begin
      SSL.Lock_Task.all;
      Old_Value := Bit;
      Bit := True;
      SSL.Unlock_Task.all;
   end Set_Interlocked;

   procedure Set_Interlocked
     (Bit          : in out Boolean;
      Old_Value    : out Boolean;
      Retry_Count  : Natural;
      Success_Flag : out Boolean)
   is
      pragma Warnings (Off, Retry_Count);

   begin
      SSL.Lock_Task.all;
      Old_Value := Bit;
      Bit := True;
      Success_Flag := True;
      SSL.Unlock_Task.all;
   end Set_Interlocked;

   ---------------------
   -- Add_Interlocked --
   ---------------------

   procedure Add_Interlocked
     (Addend : Short_Integer;
      Augend : in out Aligned_Word;
      Sign   : out Integer)
   is
   begin
      SSL.Lock_Task.all;
      Augend.Value := Augend.Value + Addend;

      if Augend.Value < 0 then
         Sign := -1;
      elsif Augend.Value > 0 then
         Sign := +1;
      else
         Sign := 0;
      end if;

      SSL.Unlock_Task.all;
   end Add_Interlocked;

   ----------------
   -- Add_Atomic --
   ----------------

   procedure Add_Atomic
     (To     : in out Aligned_Integer;
      Amount : Integer)
   is
   begin
      SSL.Lock_Task.all;
      To.Value := To.Value + Amount;
      SSL.Unlock_Task.all;
   end Add_Atomic;

   procedure Add_Atomic
     (To           : in out Aligned_Integer;
      Amount       : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      pragma Warnings (Off, Retry_Count);

   begin
      SSL.Lock_Task.all;
      Old_Value := To.Value;
      To.Value  := To.Value + Amount;
      Success_Flag := True;
      SSL.Unlock_Task.all;
   end Add_Atomic;

   procedure Add_Atomic
     (To     : in out Aligned_Long_Integer;
      Amount : Long_Integer)
   is
   begin
      SSL.Lock_Task.all;
      To.Value := To.Value + Amount;
      SSL.Unlock_Task.all;
   end Add_Atomic;

   procedure Add_Atomic
     (To           : in out Aligned_Long_Integer;
      Amount       : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      pragma Warnings (Off, Retry_Count);

   begin
      SSL.Lock_Task.all;
      Old_Value := To.Value;
      To.Value  := To.Value + Amount;
      Success_Flag := True;
      SSL.Unlock_Task.all;
   end Add_Atomic;

   ----------------
   -- And_Atomic --
   ----------------

   type IU is mod 2 ** Integer'Size;
   type LU is mod 2 ** Long_Integer'Size;

   function To_IU   is new Ada.Unchecked_Conversion (Integer, IU);
   function From_IU is new Ada.Unchecked_Conversion (IU, Integer);

   function To_LU   is new Ada.Unchecked_Conversion (Long_Integer, LU);
   function From_LU is new Ada.Unchecked_Conversion (LU, Long_Integer);

   procedure And_Atomic
     (To   : in out Aligned_Integer;
      From : Integer)
   is
   begin
      SSL.Lock_Task.all;
      To.Value  := From_IU (To_IU (To.Value) and To_IU (From));
      SSL.Unlock_Task.all;
   end And_Atomic;

   procedure And_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      pragma Warnings (Off, Retry_Count);

   begin
      SSL.Lock_Task.all;
      Old_Value := To.Value;
      To.Value  := From_IU (To_IU (To.Value) and To_IU (From));
      Success_Flag := True;
      SSL.Unlock_Task.all;
   end And_Atomic;

   procedure And_Atomic
     (To   : in out Aligned_Long_Integer;
      From : Long_Integer)
   is
   begin
      SSL.Lock_Task.all;
      To.Value  := From_LU (To_LU (To.Value) and To_LU (From));
      SSL.Unlock_Task.all;
   end And_Atomic;

   procedure And_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      pragma Warnings (Off, Retry_Count);

   begin
      SSL.Lock_Task.all;
      Old_Value := To.Value;
      To.Value  := From_LU (To_LU (To.Value) and To_LU (From));
      Success_Flag := True;
      SSL.Unlock_Task.all;
   end And_Atomic;

   ---------------
   -- Or_Atomic --
   ---------------

   procedure Or_Atomic
     (To   : in out Aligned_Integer;
      From : Integer)
   is
   begin
      SSL.Lock_Task.all;
      To.Value  := From_IU (To_IU (To.Value) or To_IU (From));
      SSL.Unlock_Task.all;
   end Or_Atomic;

   procedure Or_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      pragma Warnings (Off, Retry_Count);

   begin
      SSL.Lock_Task.all;
      Old_Value := To.Value;
      To.Value  := From_IU (To_IU (To.Value) or To_IU (From));
      Success_Flag := True;
      SSL.Unlock_Task.all;
   end Or_Atomic;

   procedure Or_Atomic
     (To   : in out Aligned_Long_Integer;
      From : Long_Integer)
   is
   begin
      SSL.Lock_Task.all;
      To.Value  := From_LU (To_LU (To.Value) or To_LU (From));
      SSL.Unlock_Task.all;
   end Or_Atomic;

   procedure Or_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      pragma Warnings (Off, Retry_Count);

   begin
      SSL.Lock_Task.all;
      Old_Value := To.Value;
      To.Value  := From_LU (To_LU (To.Value) or To_LU (From));
      Success_Flag := True;
      SSL.Unlock_Task.all;
   end Or_Atomic;

   ------------------------------------
   -- Declarations for Queue Objects --
   ------------------------------------

   type QR;

   type QR_Ptr is access QR;

   type QR is record
      Forward  : QR_Ptr;
      Backward : QR_Ptr;
   end record;

   function To_QR_Ptr   is new Ada.Unchecked_Conversion (Address, QR_Ptr);
   function From_QR_Ptr is new Ada.Unchecked_Conversion (QR_Ptr, Address);

   ------------
   -- Insqhi --
   ------------

   procedure Insqhi
     (Item   : Address;
      Header : Address;
      Status : out Insq_Status)
   is
      Hedr : constant QR_Ptr := To_QR_Ptr (Header);
      Next : constant QR_Ptr := Hedr.Forward;
      Itm  : constant QR_Ptr := To_QR_Ptr (Item);

   begin
      SSL.Lock_Task.all;

      Itm.Forward  := Next;
      Itm.Backward := Hedr;
      Hedr.Forward := Itm;

      if Next = null then
         Status := OK_First;

      else
         Next.Backward := Itm;
         Status := OK_Not_First;
      end if;

      SSL.Unlock_Task.all;
   end Insqhi;

   ------------
   -- Remqhi --
   ------------

   procedure Remqhi
     (Header : Address;
      Item   : out Address;
      Status : out Remq_Status)
   is
      Hedr : constant QR_Ptr := To_QR_Ptr (Header);
      Next : constant QR_Ptr := Hedr.Forward;

   begin
      SSL.Lock_Task.all;

      Item := From_QR_Ptr (Next);

      if Next = null then
         Status := Fail_Was_Empty;

      else
         Hedr.Forward := To_QR_Ptr (Item).Forward;

         if Hedr.Forward = null then
            Status := OK_Empty;

         else
            Hedr.Forward.Backward := Hedr;
            Status := OK_Not_Empty;
         end if;
      end if;

      SSL.Unlock_Task.all;
   end Remqhi;

   ------------
   -- Insqti --
   ------------

   procedure Insqti
     (Item   : Address;
      Header : Address;
      Status : out Insq_Status)
   is
      Hedr : constant QR_Ptr := To_QR_Ptr (Header);
      Prev : constant QR_Ptr := Hedr.Backward;
      Itm  : constant QR_Ptr := To_QR_Ptr (Item);

   begin
      SSL.Lock_Task.all;

      Itm.Backward  := Prev;
      Itm.Forward   := Hedr;
      Hedr.Backward := Itm;

      if Prev = null then
         Status := OK_First;

      else
         Prev.Forward := Itm;
         Status := OK_Not_First;
      end if;

      SSL.Unlock_Task.all;
   end Insqti;

   ------------
   -- Remqti --
   ------------

   procedure Remqti
     (Header : Address;
      Item   : out Address;
      Status : out Remq_Status)
   is
      Hedr : constant QR_Ptr := To_QR_Ptr (Header);
      Prev : constant QR_Ptr := Hedr.Backward;

   begin
      SSL.Lock_Task.all;

      Item := From_QR_Ptr (Prev);

      if Prev = null then
         Status := Fail_Was_Empty;

      else
         Hedr.Backward := To_QR_Ptr (Item).Backward;

         if Hedr.Backward = null then
            Status := OK_Empty;

         else
            Hedr.Backward.Forward := Hedr;
            Status := OK_Not_Empty;
         end if;
      end if;

      SSL.Unlock_Task.all;
   end Remqti;

end System.Aux_DEC;
