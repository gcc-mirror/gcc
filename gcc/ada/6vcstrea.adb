------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 I N T E R F A C E S . C _ S T R E A M S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1996-1999 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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

--  This is the Alpha/VMS version.

package body Interfaces.C_Streams is

   ------------
   -- fread --
   ------------

   function fread
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t
   is
      Get_Count : size_t := 0;
      type Buffer_Type is array (size_t range 1 .. count,
                                 size_t range 1 .. size) of Character;
      type Buffer_Access is access Buffer_Type;
      function To_BA is new Unchecked_Conversion (voids, Buffer_Access);
      BA : Buffer_Access := To_BA (buffer);
      Ch : int;
   begin

      --  This Fread goes with the Fwrite below.
      --  The C library fread sometimes can't read fputc generated files.

      for C in 1 .. count loop
         for S in 1 .. size loop
            Ch := fgetc (stream);
            if Ch = EOF then
               return 0;
            end if;
            BA.all (C, S) := Character'Val (Ch);
         end loop;
         Get_Count := Get_Count + 1;
      end loop;
      return Get_Count;
   end fread;

   ------------
   -- fread --
   ------------

   function fread
     (buffer : voids;
      index  : size_t;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t
   is
      Get_Count : size_t := 0;
      type Buffer_Type is array (size_t range 1 .. count,
                                 size_t range 1 .. size) of Character;
      type Buffer_Access is access Buffer_Type;
      function To_BA is new Unchecked_Conversion (voids, Buffer_Access);
      BA : Buffer_Access := To_BA (buffer);
      Ch : int;
   begin

      --  This Fread goes with the Fwrite below.
      --  The C library fread sometimes can't read fputc generated files.

      for C in 1 + index .. count + index loop
         for S in 1 .. size loop
            Ch := fgetc (stream);
            if Ch = EOF then
               return 0;
            end if;
            BA.all (C, S) := Character'Val (Ch);
         end loop;
         Get_Count := Get_Count + 1;
      end loop;
      return Get_Count;
   end fread;

   ------------
   -- fwrite --
   ------------

   function fwrite
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t
   is
      Put_Count : size_t := 0;
      type Buffer_Type is array (size_t range 1 .. count,
                                 size_t range 1 .. size) of Character;
      type Buffer_Access is access Buffer_Type;
      function To_BA is new Unchecked_Conversion (voids, Buffer_Access);
      BA : Buffer_Access := To_BA (buffer);
   begin

      --  Fwrite on VMS has the undesirable effect of always generating at
      --  least one record of output per call, regardless of buffering.  To
      --  get around this, we do multiple fputc calls instead.

      for C in 1 .. count loop
         for S in 1 .. size loop
            if fputc (Character'Pos (BA.all (C, S)), stream) = EOF then
               exit;
            end if;
         end loop;
         Put_Count := Put_Count + 1;
      end loop;
      return Put_Count;
   end fwrite;

   -------------
   -- setvbuf --
   -------------

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t)
      return   int
   is
      function C_setvbuf
        (stream : FILEs;
         buffer : chars;
         mode   : int;
         size   : size_t)
         return   int;
      pragma Import (C, C_setvbuf, "setvbuf");

      use type System.Address;
   begin

      --  In order for the above fwrite hack to work, we must always buffer
      --  stdout and stderr. Is_regular_file on VMS cannot detect when
      --  these are redirected to a file, so checking for that condition
      --  doesn't help.

      if mode = IONBF
        and then (stream = stdout or else stream = stderr)
      then
         return C_setvbuf (stream, buffer, IOLBF, size);
      else
         return C_setvbuf (stream, buffer, mode, size);
      end if;
   end setvbuf;

end Interfaces.C_Streams;
