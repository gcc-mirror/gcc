------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     G N A T . M E M O R Y _ D U M P                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2003-2010, AdaCore                     --
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

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with GNAT.IO;              use GNAT.IO;
with GNAT.Debug_Utilities; use GNAT.Debug_Utilities;

with Ada.Unchecked_Conversion;

package body GNAT.Memory_Dump is

   ----------
   -- Dump --
   ----------

   procedure Dump (Addr : System.Address; Count : Natural) is
      Ctr : Natural := Count;
      --  Count of bytes left to output

      Adr : Address := Addr;
      --  Current address

      N : Natural := 0;
      --  Number of bytes output on current line

      C : Character;
      --  Character at current storage address

      AIL : constant := Address_Image_Length - 4 + 2;
      --  Number of chars in initial address + colon + space

      Line_Len : constant Natural := AIL + 3 * 16 + 2 + 16;
      --  Line length for entire line

      Line_Buf : String (1 .. Line_Len);

      Hex : constant array (0 .. 15) of Character := "0123456789ABCDEF";

      type Char_Ptr is access all Character;

      function To_Char_Ptr is new Ada.Unchecked_Conversion (Address, Char_Ptr);

   begin
      while Ctr /= 0 loop

         --  Start of line processing

         if N = 0 then
            declare
               S : constant String := Image (Adr);
            begin
               Line_Buf (1 .. AIL) := S (4 .. S'Length - 1) & ": ";
               Line_Buf (AIL + 1 .. Line_Buf'Last) := (others => ' ');
               Line_Buf (AIL + 3 * 16 + 1) := '"';
            end;
         end if;

         --  Add one character to current line

         C := To_Char_Ptr (Adr).all;
         Adr := Adr + 1;
         Ctr := Ctr - 1;

         Line_Buf (AIL + 3 * N + 1) := Hex (Character'Pos (C) / 16);
         Line_Buf (AIL + 3 * N + 2) := Hex (Character'Pos (C) mod 16);

         if C < ' ' or else C = Character'Val (16#7F#) then
            C := '?';
         end if;

         Line_Buf (AIL + 3 * 16 + 2 + N) := C;
         N := N + 1;

         --  End of line processing

         if N = 16 then
            Line_Buf (Line_Buf'Last) := '"';
            GNAT.IO.Put_Line (Line_Buf);
            N := 0;
         end if;
      end loop;

      --  Deal with possible last partial line

      if N /= 0 then
         Line_Buf (AIL + 3 * 16 + 2 + N) := '"';
         GNAT.IO.Put_Line (Line_Buf (1 .. AIL + 3 * 16 + 2 + N));
      end if;

      return;
   end Dump;

end GNAT.Memory_Dump;
