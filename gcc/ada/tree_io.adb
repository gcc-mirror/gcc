------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              T R E E _ I O                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009  Free Software Foundation, Inc.         --
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

with Debug;  use Debug;
with Output; use Output;
with Unchecked_Conversion;

package body Tree_IO is
   Debug_Flag_Tree : Boolean := False;
   --  Debug flag for debug output from tree read/write

   -------------------------------------------
   -- Compression Scheme Used for Tree File --
   -------------------------------------------

   --  We don't just write the data directly, but instead do a mild form
   --  of compression, since we expect lots of compressible zeroes and
   --  blanks. The compression scheme is as follows:

   --    00nnnnnn followed by nnnnnn bytes (non compressed data)
   --    01nnnnnn indicates nnnnnn binary zero bytes
   --    10nnnnnn indicates nnnnnn ASCII space bytes
   --    11nnnnnn bbbbbbbb indicates nnnnnnnn occurrences of byte bbbbbbbb

   --  Since we expect many zeroes in trees, and many spaces in sources,
   --  this compression should be reasonably efficient. We can put in
   --  something better later on.

   --  Note that this compression applies to the Write_Tree_Data and
   --  Read_Tree_Data calls, not to the calls to read and write single
   --  scalar values, which are written in memory format without any
   --  compression.

   C_Noncomp : constant := 2#00_000000#;
   C_Zeros   : constant := 2#01_000000#;
   C_Spaces  : constant := 2#10_000000#;
   C_Repeat  : constant := 2#11_000000#;
   --  Codes for compression sequences

   Max_Count : constant := 63;
   --  Maximum data length for one compression sequence

   --  The above compression scheme applies only to data written with the
   --  Tree_Write routine and read with Tree_Read. Data written using the
   --  Tree_Write_Char or Tree_Write_Int routines and read using the
   --  corresponding input routines is not compressed.

   type Int_Bytes is array (1 .. 4) of Byte;
   for Int_Bytes'Size use 32;

   function To_Int_Bytes is new Unchecked_Conversion (Int, Int_Bytes);
   function To_Int       is new Unchecked_Conversion (Int_Bytes, Int);

   ----------------------
   -- Global Variables --
   ----------------------

   Tree_FD : File_Descriptor;
   --  File descriptor for tree

   Buflen : constant Int := 8_192;
   --  Length of buffer for read and write file data

   Buf : array (Pos range 1 .. Buflen) of Byte;
   --  Read/write file data buffer

   Bufn : Nat;
   --  Number of bytes read/written from/to buffer

   Buft : Nat;
   --  Total number of bytes in input buffer containing valid data. Used only
   --  for input operations. There is data left to be processed in the buffer
   --  if Buft > Bufn. A value of zero for Buft means that the buffer is empty.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Read_Buffer;
   --  Reads data into buffer, setting Bufn appropriately

   function Read_Byte return Byte;
   pragma Inline (Read_Byte);
   --  Returns next byte from input file, raises Tree_Format_Error if none left

   procedure Write_Buffer;
   --  Writes out current buffer contents

   procedure Write_Byte (B : Byte);
   pragma Inline (Write_Byte);
   --  Write one byte to output buffer, checking for buffer-full condition

   -----------------
   -- Read_Buffer --
   -----------------

   procedure Read_Buffer is
   begin
      Buft := Int (Read (Tree_FD, Buf (1)'Address, Integer (Buflen)));

      if Buft = 0 then
         raise Tree_Format_Error;
      else
         Bufn := 0;
      end if;
   end Read_Buffer;

   ---------------
   -- Read_Byte --
   ---------------

   function Read_Byte return Byte is
   begin
      if Bufn = Buft then
         Read_Buffer;
      end if;

      Bufn := Bufn + 1;
      return Buf (Bufn);
   end Read_Byte;

   --------------------
   -- Tree_Read_Bool --
   --------------------

   procedure Tree_Read_Bool (B : out Boolean) is
   begin
      B := Boolean'Val (Read_Byte);

      if Debug_Flag_Tree then
         if B then
            Write_Str ("True");
         else
            Write_Str ("False");
         end if;

         Write_Eol;
      end if;
   end Tree_Read_Bool;

   --------------------
   -- Tree_Read_Char --
   --------------------

   procedure Tree_Read_Char (C : out Character) is
   begin
      C := Character'Val (Read_Byte);

      if Debug_Flag_Tree then
         Write_Str ("==> transmitting Character = ");
         Write_Char (C);
         Write_Eol;
      end if;
   end Tree_Read_Char;

   --------------------
   -- Tree_Read_Data --
   --------------------

   procedure Tree_Read_Data (Addr : Address; Length : Int) is

      type S is array (Pos) of Byte;
      --  This is a big array, for which we have to suppress the warning

      type SP is access all S;

      function To_SP is new Unchecked_Conversion (Address, SP);

      Data : constant SP := To_SP (Addr);
      --  Data buffer to be read as an indexable array of bytes

      OP : Pos := 1;
      --  Pointer to next byte of data buffer to be read into

      B : Byte;
      C : Byte;
      L : Int;

   begin
      if Debug_Flag_Tree then
         Write_Str ("==> transmitting ");
         Write_Int (Length);
         Write_Str (" data bytes");
         Write_Eol;
      end if;

      --  Verify data length

      Tree_Read_Int (L);

      if L /= Length then
         Write_Str ("==> transmitting, expected ");
         Write_Int (Length);
         Write_Str (" bytes, found length = ");
         Write_Int (L);
         Write_Eol;
         raise Tree_Format_Error;
      end if;

      --  Loop to read data

      while OP <= Length loop

         --  Get compression control character

         B := Read_Byte;
         C := B and 2#00_111111#;
         B := B and 2#11_000000#;

         --  Non-repeat case

         if B = C_Noncomp then
            if Debug_Flag_Tree then
               Write_Str ("==>    uncompressed:  ");
               Write_Int (Int (C));
               Write_Str (", starting at ");
               Write_Int (OP);
               Write_Eol;
            end if;

            for J in 1 .. C loop
               Data (OP) := Read_Byte;
               OP := OP + 1;
            end loop;

         --  Repeated zeroes

         elsif B = C_Zeros then
            if Debug_Flag_Tree then
               Write_Str ("==>    zeroes:        ");
               Write_Int (Int (C));
               Write_Str (", starting at ");
               Write_Int (OP);
               Write_Eol;
            end if;

            for J in 1 .. C loop
               Data (OP) := 0;
               OP := OP + 1;
            end loop;

         --  Repeated spaces

         elsif B = C_Spaces then
            if Debug_Flag_Tree then
               Write_Str ("==>    spaces:        ");
               Write_Int (Int (C));
               Write_Str (", starting at ");
               Write_Int (OP);
               Write_Eol;
            end if;

            for J in 1 .. C loop
               Data (OP) := Character'Pos (' ');
               OP := OP + 1;
            end loop;

         --  Specified repeated character

         else -- B = C_Repeat
            B := Read_Byte;

            if Debug_Flag_Tree then
               Write_Str ("==>    other char:    ");
               Write_Int (Int (C));
               Write_Str (" (");
               Write_Int (Int (B));
               Write_Char (')');
               Write_Str (", starting at ");
               Write_Int (OP);
               Write_Eol;
            end if;

            for J in 1 .. C loop
               Data (OP) := B;
               OP := OP + 1;
            end loop;
         end if;
      end loop;

      --  At end of loop, data item must be exactly filled

      if OP /= Length + 1 then
         raise Tree_Format_Error;
      end if;

   end Tree_Read_Data;

   --------------------------
   -- Tree_Read_Initialize --
   --------------------------

   procedure Tree_Read_Initialize (Desc : File_Descriptor) is
   begin
      Buft := 0;
      Bufn := 0;
      Tree_FD := Desc;
      Debug_Flag_Tree := Debug_Flag_5;
   end Tree_Read_Initialize;

   -------------------
   -- Tree_Read_Int --
   -------------------

   procedure Tree_Read_Int (N : out Int) is
      N_Bytes : Int_Bytes;

   begin
      for J in 1 .. 4 loop
         N_Bytes (J) := Read_Byte;
      end loop;

      N := To_Int (N_Bytes);

      if Debug_Flag_Tree then
         Write_Str ("==> transmitting Int = ");
         Write_Int (N);
         Write_Eol;
      end if;
   end Tree_Read_Int;

   -------------------
   -- Tree_Read_Str --
   -------------------

   procedure Tree_Read_Str (S : out String_Ptr) is
      N : Nat;

   begin
      Tree_Read_Int (N);
      S := new String (1 .. Natural (N));
      Tree_Read_Data (S.all (1)'Address, N);
   end Tree_Read_Str;

   -------------------------
   -- Tree_Read_Terminate --
   -------------------------

   procedure Tree_Read_Terminate is
   begin
      --  Must be at end of input buffer, so we should get Tree_Format_Error
      --  if we try to read one more byte, if not, we have a format error.

      declare
         B : Byte;
         pragma Warnings (Off, B);

      begin
         B := Read_Byte;

      exception
         when Tree_Format_Error => return;
      end;

      raise Tree_Format_Error;
   end Tree_Read_Terminate;

   ---------------------
   -- Tree_Write_Bool --
   ---------------------

   procedure Tree_Write_Bool (B : Boolean) is
   begin
      if Debug_Flag_Tree then
         Write_Str ("==> transmitting Boolean = ");

         if B then
            Write_Str ("True");
         else
            Write_Str ("False");
         end if;

         Write_Eol;
      end if;

      Write_Byte (Boolean'Pos (B));
   end Tree_Write_Bool;

   ---------------------
   -- Tree_Write_Char --
   ---------------------

   procedure Tree_Write_Char (C : Character) is
   begin
      if Debug_Flag_Tree then
         Write_Str ("==> transmitting Character = ");
         Write_Char (C);
         Write_Eol;
      end if;

      Write_Byte (Character'Pos (C));
   end Tree_Write_Char;

   ---------------------
   -- Tree_Write_Data --
   ---------------------

   procedure Tree_Write_Data (Addr : Address; Length : Int) is

      type S is array (Pos) of Byte;
      --  This is a big array, for which we have to suppress the warning

      type SP is access all S;

      function To_SP is new Unchecked_Conversion (Address, SP);

      Data : constant SP := To_SP (Addr);
      --  Pointer to data to be written, converted to array type

      IP : Pos := 1;
      --  Input buffer pointer, next byte to be processed

      NC : Nat range 0 .. Max_Count := 0;
      --  Number of bytes of non-compressible sequence

      C  : Byte;

      procedure Write_Non_Compressed_Sequence;
      --  Output currently collected sequence of non-compressible data

      -----------------------------------
      -- Write_Non_Compressed_Sequence --
      -----------------------------------

      procedure Write_Non_Compressed_Sequence is
      begin
         if NC > 0 then
            Write_Byte (C_Noncomp + Byte (NC));

            if Debug_Flag_Tree then
               Write_Str ("==>    uncompressed:  ");
               Write_Int (NC);
               Write_Str (", starting at ");
               Write_Int (IP - NC);
               Write_Eol;
            end if;

            for J in reverse 1 .. NC loop
               Write_Byte (Data (IP - J));
            end loop;

            NC := 0;
         end if;
      end Write_Non_Compressed_Sequence;

   --  Start of processing for Tree_Write_Data

   begin
      if Debug_Flag_Tree then
         Write_Str ("==> transmitting ");
         Write_Int (Length);
         Write_Str (" data bytes");
         Write_Eol;
      end if;

      --  We write the count at the start, so that we can check it on
      --  the corresponding read to make sure that reads and writes match

      Tree_Write_Int (Length);

      --  Conversion loop
      --    IP is index of next input character
      --    NC is number of non-compressible bytes saved up

      loop
         --  If input is completely processed, then we are all done

         if IP > Length then
            Write_Non_Compressed_Sequence;
            return;
         end if;

         --  Test for compressible sequence, must be at least three identical
         --  bytes in a row to be worthwhile compressing.

         if IP + 2 <= Length
           and then Data (IP) = Data (IP + 1)
           and then Data (IP) = Data (IP + 2)
         then
            Write_Non_Compressed_Sequence;

            --  Count length of new compression sequence

            C := 3;
            IP := IP + 3;

            while IP < Length
              and then Data (IP) = Data (IP - 1)
              and then C < Max_Count
            loop
               C := C + 1;
               IP := IP + 1;
            end loop;

            --  Output compression sequence

            if Data (IP - 1) = 0 then
               if Debug_Flag_Tree then
                  Write_Str ("==>    zeroes:        ");
                  Write_Int (Int (C));
                  Write_Str (", starting at ");
                  Write_Int (IP - Int (C));
                  Write_Eol;
               end if;

               Write_Byte (C_Zeros + C);

            elsif Data (IP - 1) = Character'Pos (' ') then
               if Debug_Flag_Tree then
                  Write_Str ("==>    spaces:        ");
                  Write_Int (Int (C));
                  Write_Str (", starting at ");
                  Write_Int (IP - Int (C));
                  Write_Eol;
               end if;

               Write_Byte (C_Spaces + C);

            else
               if Debug_Flag_Tree then
                  Write_Str ("==>    other char:    ");
                  Write_Int (Int (C));
                  Write_Str (" (");
                  Write_Int (Int (Data (IP - 1)));
                  Write_Char (')');
                  Write_Str (", starting at ");
                  Write_Int (IP - Int (C));
                  Write_Eol;
               end if;

               Write_Byte (C_Repeat + C);
               Write_Byte (Data (IP - 1));
            end if;

         --  No compression possible here

         else
            --  Output non-compressed sequence if at maximum length

            if NC = Max_Count then
               Write_Non_Compressed_Sequence;
            end if;

            NC := NC + 1;
            IP := IP + 1;
         end if;
      end loop;

   end Tree_Write_Data;

   ---------------------------
   -- Tree_Write_Initialize --
   ---------------------------

   procedure Tree_Write_Initialize (Desc : File_Descriptor) is
   begin
      Bufn := 0;
      Tree_FD := Desc;
      Set_Standard_Error;
      Debug_Flag_Tree := Debug_Flag_5;
   end Tree_Write_Initialize;

   --------------------
   -- Tree_Write_Int --
   --------------------

   procedure Tree_Write_Int (N : Int) is
      N_Bytes : constant Int_Bytes := To_Int_Bytes (N);

   begin
      if Debug_Flag_Tree then
         Write_Str ("==> transmitting Int = ");
         Write_Int (N);
         Write_Eol;
      end if;

      for J in 1 .. 4 loop
         Write_Byte (N_Bytes (J));
      end loop;
   end Tree_Write_Int;

   --------------------
   -- Tree_Write_Str --
   --------------------

   procedure Tree_Write_Str (S : String_Ptr) is
   begin
      Tree_Write_Int (S'Length);
      Tree_Write_Data (S (1)'Address, S'Length);
   end Tree_Write_Str;

   --------------------------
   -- Tree_Write_Terminate --
   --------------------------

   procedure Tree_Write_Terminate is
   begin
      if Bufn > 0 then
         Write_Buffer;
      end if;
   end Tree_Write_Terminate;

   ------------------
   -- Write_Buffer --
   ------------------

   procedure Write_Buffer is
   begin
      if Integer (Bufn) = Write (Tree_FD, Buf'Address, Integer (Bufn)) then
         Bufn := 0;

      else
         Set_Standard_Error;
         Write_Str ("fatal error: disk full");
         OS_Exit (2);
      end if;
   end Write_Buffer;

   ----------------
   -- Write_Byte --
   ----------------

   procedure Write_Byte (B : Byte) is
   begin
      Bufn := Bufn + 1;
      Buf (Bufn) := B;

      if Bufn = Buflen then
         Write_Buffer;
      end if;
   end Write_Byte;

end Tree_IO;
