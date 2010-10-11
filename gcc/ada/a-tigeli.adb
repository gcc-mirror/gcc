------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . T E X T _ I O                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

separate (Ada.Text_IO)
procedure Get_Line
  (File : File_Type;
   Item : out String;
   Last : out Natural)
is
   Chunk_Size : constant := 80;
   --  We read into a fixed size auxiliary buffer. Because this buffer
   --  needs to be pre-initialized, there is a trade-off between size and
   --  speed. Experiments find returns are diminishing after 50 and this
   --  size allows most lines to be processed with a single read.

   ch : int;
   N  : Natural;

   procedure memcpy (s1, s2 : chars; n : size_t);
   pragma Import (C, memcpy);

   function memchr (s : chars; ch : int; n : size_t) return chars;
   pragma Import (C, memchr);

   procedure memset (b : chars; ch : int; n : size_t);
   pragma Import (C, memset);

   function Get_Chunk (N : Positive) return Natural;
   --  Reads at most N - 1 characters into Item (Last + 1 .. Item'Last),
   --  updating Last. Raises End_Error if nothing was read (End_Of_File).
   --  Returns number of characters still to read (either 0 or 1) in
   --  case of success.

   ---------------
   -- Get_Chunk --
   ---------------

   function Get_Chunk (N : Positive) return Natural is
      Buf : String (1 .. Chunk_Size);
      S   : constant chars := Buf (1)'Address;
      P   : chars;

   begin
      if N = 1 then
         return N;
      end if;

      memset (S, 10, size_t (N));

      if fgets (S, N, File.Stream) = Null_Address then
         if ferror (File.Stream) /= 0 then
            raise Device_Error;

         --  If incomplete last line, pretend we found a LM

         elsif Last >= Item'First then
            return 0;

         else
            raise End_Error;
         end if;
      end if;

      P := memchr (S, LM, size_t (N));

      --  If no LM is found, the buffer got filled without reading a new
      --  line. Otherwise, the LM is either one from the input, or else one
      --  from the initialization, which means an incomplete end-of-line was
      --  encountered. Only in first case the LM will be followed by a 0.

      if P = Null_Address then
         pragma Assert (Buf (N) = ASCII.NUL);
         memcpy (Item (Last + 1)'Address,
                 Buf (1)'Address, size_t (N - 1));
         Last := Last + N - 1;

         return 1;

      else
         --  P points to the LM character. Set K so Buf (K) is the character
         --  right before.

         declare
            K : Natural := Natural (P - S);

         begin
            --  Now Buf (K + 2) should be 0, or otherwise Buf (K) is the 0
            --  put in by fgets, so compensate.

            if K + 2 > Buf'Last or else Buf (K + 2) /= ASCII.NUL then

               --  Incomplete last line, so remove the extra 0

               pragma Assert (Buf (K) = ASCII.NUL);
               K := K - 1;
            end if;

            memcpy (Item (Last + 1)'Address,
                    Buf (1)'Address, size_t (K));
            Last := Last + K;
         end;

         return 0;
      end if;
   end Get_Chunk;

--  Start of processing for Get_Line

begin
   FIO.Check_Read_Status (AP (File));

   --  Immediate exit for null string, this is a case in which we do not
   --  need to test for end of file and we do not skip a line mark under
   --  any circumstances.

   if Item'First > Item'Last then
      return;
   end if;

   N := Item'Last - Item'First + 1;

   Last := Item'First - 1;

   --  Here we have at least one character, if we are immediately before
   --  a line mark, then we will just skip past it storing no characters.

   if File.Before_LM then
      File.Before_LM := False;
      File.Before_LM_PM := False;

   --  Otherwise we need to read some characters

   else
      while N >= Chunk_Size loop
         if Get_Chunk (Chunk_Size) = 0 then
            N := 0;
         else
            N := N - Chunk_Size + 1;
         end if;
      end loop;

      if N > 1 then
         N := Get_Chunk (N);
      end if;

      --  Almost there, only a little bit more to read

      if N = 1 then
         ch := Getc (File);

         --  If we get EOF after already reading data, this is an incomplete
         --  last line, in which case no End_Error should be raised.

         if ch = EOF and then Last < Item'First then
            raise End_Error;

         elsif ch /= LM then

            --  Buffer really is full without having seen LM, update col

            Last := Last + 1;
            Item (Last) := Character'Val (ch);
            File.Col := File.Col + Count (Last - Item'First + 1);
            return;
         end if;
      end if;
   end if;

   --  We have skipped past, but not stored, a line mark. Skip following
   --  page mark if one follows, but do not do this for a non-regular file
   --  (since otherwise we get annoying wait for an extra character)

   File.Line := File.Line + 1;
   File.Col := 1;

   if File.Before_LM_PM then
      File.Line := 1;
      File.Before_LM_PM := False;
      File.Page := File.Page + 1;

   elsif File.Is_Regular_File then
      ch := Getc (File);

      if ch = PM and then File.Is_Regular_File then
         File.Line := 1;
         File.Page := File.Page + 1;
      else
         Ungetc (ch, File);
      end if;
   end if;
end Get_Line;
