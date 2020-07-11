------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                   G N A T . S E C U R E _ H A S H E S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2009-2020, Free Software Foundation, Inc.         --
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

with System;     use System;
with Interfaces; use Interfaces;

package body GNAT.Secure_Hashes is

   Hex_Digit : constant array (Stream_Element range 0 .. 15) of Character :=
                 "0123456789abcdef";

   type Fill_Buffer_Access is
     access procedure
       (M     : in out Message_State;
        SEA   : Stream_Element_Array;
        First : Stream_Element_Offset;
        Last  : out Stream_Element_Offset);
   --  A procedure to transfer data from SEA, starting at First, into M's block
   --  buffer until either the block buffer is full or all data from S has been
   --  consumed.

   procedure Fill_Buffer_Copy
     (M     : in out Message_State;
      SEA   : Stream_Element_Array;
      First : Stream_Element_Offset;
      Last  : out Stream_Element_Offset);
   --  Transfer procedure which just copies data from S to M

   procedure Fill_Buffer_Swap
     (M     : in out Message_State;
      SEA   : Stream_Element_Array;
      First : Stream_Element_Offset;
      Last  : out Stream_Element_Offset);
   --  Transfer procedure which swaps bytes from S when copying into M. S must
   --  have even length. Note that the swapping is performed considering pairs
   --  starting at S'First, even if S'First /= First (that is, if
   --  First = S'First then the first copied byte is always S (S'First + 1),
   --  and if First = S'First + 1 then the first copied byte is always
   --  S (S'First).

   procedure To_String (SEA : Stream_Element_Array; S : out String);
   --  Return the hexadecimal representation of SEA

   ----------------------
   -- Fill_Buffer_Copy --
   ----------------------

   procedure Fill_Buffer_Copy
     (M     : in out Message_State;
      SEA   : Stream_Element_Array;
      First : Stream_Element_Offset;
      Last  : out Stream_Element_Offset)
   is
      Buf_SEA : Stream_Element_Array (M.Buffer'Range);
      for Buf_SEA'Address use M.Buffer'Address;
      pragma Import (Ada, Buf_SEA);

      Length : constant Stream_Element_Offset :=
                 Stream_Element_Offset'Min
                    (M.Block_Length - M.Last, SEA'Last - First + 1);

   begin
      pragma Assert (Length > 0);

      Buf_SEA (M.Last + 1 .. M.Last + Length) :=
        SEA (First .. First + Length - 1);
      M.Last := M.Last + Length;
      Last := First + Length - 1;
   end Fill_Buffer_Copy;

   ----------------------
   -- Fill_Buffer_Swap --
   ----------------------

   procedure Fill_Buffer_Swap
     (M     : in out Message_State;
      SEA   : Stream_Element_Array;
      First : Stream_Element_Offset;
      Last  : out Stream_Element_Offset)
   is
      pragma Assert (SEA'Length mod 2 = 0);
      Length : constant Stream_Element_Offset :=
                  Stream_Element_Offset'Min
                     (M.Block_Length - M.Last, SEA'Last - First + 1);
   begin
      Last := First;
      while Last - First < Length loop
         M.Buffer (M.Last + 1 + Last - First) :=
           (if (Last - SEA'First) mod 2 = 0
            then SEA (Last + 1)
            else SEA (Last - 1));
         Last := Last + 1;
      end loop;
      M.Last := M.Last + Length;
      Last := First + Length - 1;
   end Fill_Buffer_Swap;

   ---------------
   -- To_String --
   ---------------

   procedure To_String (SEA : Stream_Element_Array; S : out String) is
      pragma Assert (S'Length = 2 * SEA'Length);
   begin
      for J in SEA'Range loop
         declare
            S_J : constant Natural := 1 + Natural (J - SEA'First) * 2;
         begin
            S (S_J)     := Hex_Digit (SEA (J) / 16);
            S (S_J + 1) := Hex_Digit (SEA (J) mod 16);
         end;
      end loop;
   end To_String;

   -------
   -- H --
   -------

   package body H is

      procedure Update
        (C           : in out Context;
         SEA         : Stream_Element_Array;
         Fill_Buffer : Fill_Buffer_Access);
      --  Internal common routine for all Update procedures

      procedure Final
        (C         : Context;
         Hash_Bits : out Ada.Streams.Stream_Element_Array);
      --  Perform final hashing operations (data padding) and extract the
      --  (possibly truncated) state of C into Hash_Bits.

      ------------
      -- Digest --
      ------------

      function Digest (C : Context) return Message_Digest is
         Hash_Bits : Stream_Element_Array (1 .. Hash_Length);
      begin
         Final (C, Hash_Bits);
         return MD : Message_Digest do
            To_String (Hash_Bits, MD);
         end return;
      end Digest;

      function Digest (S : String) return Message_Digest is
         C : Context;
      begin
         Update (C, S);
         return Digest (C);
      end Digest;

      function Digest (A : Stream_Element_Array) return Message_Digest is
         C : Context;
      begin
         Update (C, A);
         return Digest (C);
      end Digest;

      function Digest (C : Context) return Binary_Message_Digest is
         Hash_Bits : Stream_Element_Array (1 .. Hash_Length);
      begin
         Final (C, Hash_Bits);
         return Hash_Bits;
      end Digest;

      function Digest (S : String) return Binary_Message_Digest is
         C : Context;
      begin
         Update (C, S);
         return Digest (C);
      end Digest;

      function Digest
        (A : Stream_Element_Array) return Binary_Message_Digest
      is
         C : Context;
      begin
         Update (C, A);
         return Digest (C);
      end Digest;

      -----------
      -- Final --
      -----------

      --  Once a complete message has been processed, it is padded with one 1
      --  bit followed by enough 0 bits so that the last block is 2 * Word'Size
      --  bits short of being completed. The last 2 * Word'Size bits are set to
      --  the message size in bits (excluding padding).

      procedure Final
        (C         : Context;
         Hash_Bits : out Stream_Element_Array)
      is
         FC : Context := C;

         Zeroes : Stream_Element_Count;
         --  Number of 0 bytes in padding

         Message_Length : Unsigned_64 := FC.M_State.Length;
         --  Message length in bytes

         Size_Length : constant Stream_Element_Count :=
                         2 * Hash_State.Word'Size / 8;
         --  Length in bytes of the size representation

      begin
         Zeroes := (Block_Length - 1 - Size_Length - FC.M_State.Last)
                     mod FC.M_State.Block_Length;
         declare
            Pad : Stream_Element_Array (1 .. 1 + Zeroes + Size_Length) :=
                    (1 => 128, others => 0);

            Index       : Stream_Element_Offset;
            First_Index : Stream_Element_Offset;

         begin
            First_Index := (if Hash_Bit_Order = Low_Order_First
                            then Pad'Last - Size_Length + 1
                            else Pad'Last);

            Index := First_Index;
            while Message_Length > 0 loop
               if Index = First_Index then

                  --  Message_Length is in bytes, but we need to store it as
                  --  a bit count.

                  Pad (Index) := Stream_Element
                                   (Shift_Left (Message_Length and 16#1f#, 3));
                  Message_Length := Shift_Right (Message_Length, 5);

               else
                  Pad (Index) := Stream_Element (Message_Length and 16#ff#);
                  Message_Length := Shift_Right (Message_Length, 8);
               end if;

               Index := Index +
                          (if Hash_Bit_Order = Low_Order_First then 1 else -1);
            end loop;

            Update (FC, Pad);
         end;

         pragma Assert (FC.M_State.Last = 0);

         Hash_State.To_Hash (FC.H_State, Hash_Bits);

         --  HMAC case: hash outer pad

         if C.KL /= 0 then
            declare
               Outer_C : Context;
               Opad    : Stream_Element_Array :=
                 (1 .. Stream_Element_Offset (Block_Length) => 16#5c#);

            begin
               for J in C.Key'Range loop
                  Opad (J) := Opad (J) xor C.Key (J);
               end loop;

               Update (Outer_C, Opad);
               Update (Outer_C, Hash_Bits);

               Final (Outer_C, Hash_Bits);
            end;
         end if;
      end Final;

      --------------------------
      -- HMAC_Initial_Context --
      --------------------------

      function HMAC_Initial_Context (Key : String) return Context is
      begin
         if Key'Length = 0 then
            raise Constraint_Error with "null key";
         end if;

         return C : Context (KL => (if Key'Length <= Key_Length'Last
                                    then Key'Length
                                    else Hash_Length))
         do
            --  Set Key (if longer than block length, first hash it)

            if C.KL = Key'Length then
               declare
                  SK : String (1 .. Key'Length);
                  for SK'Address use C.Key'Address;
                  pragma Import (Ada, SK);
               begin
                  SK := Key;
               end;

            else
               C.Key := Digest (Key);
            end if;

            --  Hash inner pad

            declare
               Ipad : Stream_Element_Array :=
                 (1 .. Stream_Element_Offset (Block_Length) => 16#36#);

            begin
               for J in C.Key'Range loop
                  Ipad (J) := Ipad (J) xor C.Key (J);
               end loop;

               Update (C, Ipad);
            end;
         end return;
      end HMAC_Initial_Context;

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream : in out Hash_Stream;
         Item   : out Stream_Element_Array;
         Last   : out Stream_Element_Offset)
      is
         pragma Unreferenced (Stream, Item, Last);
      begin
         raise Program_Error with "Hash_Stream is write-only";
      end Read;

      ------------
      -- Update --
      ------------

      procedure Update
        (C           : in out Context;
         SEA         : Stream_Element_Array;
         Fill_Buffer : Fill_Buffer_Access)
      is
         First, Last : Stream_Element_Offset;

      begin
         if SEA'Length = 0 then
            return;
         end if;

         C.M_State.Length := C.M_State.Length + SEA'Length;

         First := SEA'First;
         loop
            Fill_Buffer (C.M_State, SEA, First, Last);

            if C.M_State.Last = Block_Length then
               Transform (C.H_State, C.M_State);
               C.M_State.Last := 0;
            end if;

            exit when Last = SEA'Last;
            First := Last + 1;
         end loop;
      end Update;

      ------------
      -- Update --
      ------------

      procedure Update (C : in out Context; Input : Stream_Element_Array) is
      begin
         Update (C, Input, Fill_Buffer_Copy'Access);
      end Update;

      ------------
      -- Update --
      ------------

      procedure Update (C : in out Context; Input : String) is
         pragma Assert (Input'Length <= Stream_Element_Offset'Last);
         SEA : Stream_Element_Array (1 .. Input'Length);
         for SEA'Address use Input'Address;
         pragma Import (Ada, SEA);
      begin
         Update (C, SEA, Fill_Buffer_Copy'Access);
      end Update;

      -----------------
      -- Wide_Update --
      -----------------

      procedure Wide_Update (C : in out Context; Input : Wide_String) is
         SEA : Stream_Element_Array (1 .. 2 * Input'Length);
         for SEA'Address use Input'Address;
         pragma Import (Ada, SEA);
      begin
         Update
           (C, SEA,
            (if System.Default_Bit_Order /= Low_Order_First
             then Fill_Buffer_Swap'Access
             else Fill_Buffer_Copy'Access));
      end Wide_Update;

      -----------------
      -- Wide_Digest --
      -----------------

      function Wide_Digest (W : Wide_String) return Message_Digest is
         C : Context;
      begin
         Wide_Update (C, W);
         return Digest (C);
      end Wide_Digest;

      function Wide_Digest (W : Wide_String) return Binary_Message_Digest is
         C : Context;
      begin
         Wide_Update (C, W);
         return Digest (C);
      end Wide_Digest;

      -----------
      -- Write --
      -----------

      procedure Write
         (Stream : in out Hash_Stream;
          Item   : Stream_Element_Array)
      is
      begin
         Update (Stream.C.all, Item);
      end Write;

   end H;

   -------------------------
   -- Hash_Function_State --
   -------------------------

   package body Hash_Function_State is

      -------------
      -- To_Hash --
      -------------

      procedure To_Hash (H : State; H_Bits : out Stream_Element_Array) is
         Hash_Words : constant Stream_Element_Offset := H'Size / Word'Size;
         Result     : State (1 .. Hash_Words) :=
                        H (H'Last - Hash_Words + 1 .. H'Last);

         R_SEA : Stream_Element_Array (1 .. Result'Size / 8);
         for R_SEA'Address use Result'Address;
         pragma Import (Ada, R_SEA);

      begin
         if System.Default_Bit_Order /= Hash_Bit_Order then
            for J in Result'Range loop
               Swap (Result (J)'Address);
            end loop;
         end if;

         --  Return truncated hash

         pragma Assert (H_Bits'Length <= R_SEA'Length);
         H_Bits := R_SEA (R_SEA'First .. R_SEA'First + H_Bits'Length - 1);
      end To_Hash;

   end Hash_Function_State;

end GNAT.Secure_Hashes;
