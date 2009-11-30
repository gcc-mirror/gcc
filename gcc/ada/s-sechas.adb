------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 S Y S T E M . S E C U R E _ H A S H E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2009, Free Software Foundation, Inc.             --
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

package body System.Secure_Hashes is

   use Ada.Streams;

   Hex_Digit : constant array (Stream_Element range 0 .. 15) of Character :=
                 ('0', '1', '2', '3', '4', '5', '6', '7',
                  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

   type Fill_Buffer_Access is
     access procedure
       (M     : in out Message_State;
        S     : String;
        First : Natural;
        Last  : out Natural);
   --  A procedure to transfer data from S into M's block buffer until either
   --  the block buffer is full or all data from S has been consumed.

   procedure Fill_Buffer_Copy
     (M     : in out Message_State;
      S     : String;
      First : Natural;
      Last  : out Natural);
   --  Transfer procedure which just copies data from S to M

   procedure Fill_Buffer_Swap
     (M     : in out Message_State;
      S     : String;
      First : Natural;
      Last  : out Natural);
   --  Transfer procedure which swaps bytes from S when copying into M

   procedure To_String (SEA : Stream_Element_Array; S : out String);
   --  Return the hexadecimal representation of SEA

   ----------------------
   -- Fill_Buffer_Copy --
   ----------------------

   procedure Fill_Buffer_Copy
     (M     : in out Message_State;
      S     : String;
      First : Natural;
      Last  : out Natural)
   is
      Buf_String : String (M.Buffer'Range);
      for Buf_String'Address use M.Buffer'Address;
      pragma Import (Ada, Buf_String);
      Length : constant Natural :=
                  Natural'Min (M.Block_Length - M.Last, S'Last - First + 1);
   begin
      pragma Assert (Length > 0);

      Buf_String (M.Last + 1 .. M.Last + Length) :=
        S (First .. First + Length - 1);
      M.Last := M.Last + Length;
      Last := First + Length - 1;
   end Fill_Buffer_Copy;

   ----------------------
   -- Fill_Buffer_Swap --
   ----------------------

   procedure Fill_Buffer_Swap
     (M     : in out Message_State;
      S     : String;
      First : Natural;
      Last  : out Natural)
   is
      Length : constant Natural :=
                  Natural'Min (M.Block_Length - M.Last, S'Last - First + 1);
   begin
      Last := First;
      while Last - First < Length loop
         M.Buffer (M.Last + 1 + Last - First) :=
            (if (Last - First) mod 2 = 0 then S (Last + 1) else S (Last - 1));
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
         S           : String;
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
         Hash_Bits : Stream_Element_Array
                       (1 .. Stream_Element_Offset (Hash_Length));
      begin
         Final (C, Hash_Bits);
         return MD : Message_Digest do
            To_String (Hash_Bits, MD);
         end return;
      end Digest;

      ------------
      -- Digest --
      ------------

      function Digest (S : String) return Message_Digest is
         C : Context;
      begin
         Update (C, S);
         return Digest (C);
      end Digest;

      ------------
      -- Digest --
      ------------

      function Digest (A : Stream_Element_Array) return Message_Digest is
         C : Context;
      begin
         Update (C, A);
         return Digest (C);
      end Digest;

      -----------
      -- Final --
      -----------

      --  Once a complete message has been processed, it is padded with one
      --  1 bit followed by enough 0 bits so that the last block is
      --  2 * Word'Size bits short of being completed. The last 2 * Word'Size
      --  bits are set to the message size in bits (excluding padding).

      procedure Final
        (C          : Context;
         Hash_Bits  : out Stream_Element_Array)
      is
         FC : Context := C;

         Zeroes : Natural;
         --  Number of 0 bytes in padding

         Message_Length : Unsigned_64 := FC.M_State.Length;
         --  Message length in bytes

         Size_Length : constant Natural :=
                         2 * Hash_State.Word'Size / 8;
         --  Length in bytes of the size representation

      begin
         Zeroes := (Block_Length - 1 - Size_Length - FC.M_State.Last)
                     mod FC.M_State.Block_Length;
         declare
            Pad : String (1 .. 1 + Zeroes + Size_Length) :=
                    (1 => Character'Val (128), others => ASCII.NUL);
            Index : Natural;
            First_Index : Natural;
         begin
            First_Index := (if Hash_Bit_Order = Low_Order_First then
                              Pad'Last - Size_Length + 1
                            else
                              Pad'Last);

            Index := First_Index;
            while Message_Length > 0 loop
               if Index = First_Index then
                  --  Message_Length is in bytes, but we need to store it as
                  --  a bit count).

                  Pad (Index) := Character'Val
                                   (Shift_Left (Message_Length and 16#1f#, 3));
                  Message_Length := Shift_Right (Message_Length, 5);
               else
                  Pad (Index) := Character'Val (Message_Length and 16#ff#);
                  Message_Length := Shift_Right (Message_Length, 8);
               end if;
               Index := Index +
                          (if Hash_Bit_Order = Low_Order_First then 1 else -1);
            end loop;

            Update (FC, Pad);
         end;

         pragma Assert (FC.M_State.Last = 0);

         Hash_State.To_Hash (FC.H_State, Hash_Bits);
      end Final;

      ------------
      -- Update --
      ------------

      procedure Update
        (C           : in out Context;
         S           : String;
         Fill_Buffer : Fill_Buffer_Access)
      is
         Last : Natural := S'First - 1;
      begin
         C.M_State.Length := C.M_State.Length + S'Length;

         while Last < S'Last loop
            Fill_Buffer (C.M_State, S, Last + 1, Last);

            if C.M_State.Last = Block_Length then
               Transform (C.H_State, C.M_State);
               C.M_State.Last := 0;
            end if;
         end loop;

      end Update;

      ------------
      -- Update --
      ------------

      procedure Update (C : in out Context; Input : String) is
      begin
         Update (C, Input, Fill_Buffer_Copy'Access);
      end Update;

      ------------
      -- Update --
      ------------

      procedure Update (C : in out Context; Input : Stream_Element_Array) is
         S : String (1 .. Input'Length);
         for S'Address use Input'Address;
         pragma Import (Ada, S);
      begin
         Update (C, S, Fill_Buffer_Copy'Access);
      end Update;

      -----------------
      -- Wide_Update --
      -----------------

      procedure Wide_Update (C : in out Context; Input : Wide_String) is
         S : String (1 .. 2 * Input'Length);
         for S'Address use Input'Address;
         pragma Import (Ada, S);
      begin
         Update
           (C, S,
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

   end H;

   -------------------------
   -- Hash_Function_State --
   -------------------------

   package body Hash_Function_State is

      -------------
      -- To_Hash --
      -------------

      procedure To_Hash (H : State; H_Bits : out Stream_Element_Array) is
         Hash_Words : constant Natural := H'Size / Word'Size;
         Result : State (1 .. Hash_Words) :=
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

end System.Secure_Hashes;
