------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                          G N A T . I O _ A U X                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1995-2002 Ada Core Technologies, Inc.            --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C_Streams; use Interfaces.C_Streams;

package body GNAT.IO_Aux is

   Buflen : constant := 2000;
   --  Buffer length. Works for any non-zero value, larger values take
   --  more stack space, smaller values require more recursion.

   -----------------
   -- File_Exists --
   -----------------

   function File_Exists (Name : String) return Boolean
   is
      Namestr : aliased String (1 .. Name'Length + 1);
      --  Name as given with ASCII.NUL appended

   begin
      Namestr (1 .. Name'Length) := Name;
      Namestr (Name'Length + 1)  := ASCII.NUL;
      return file_exists (Namestr'Address) /= 0;
   end File_Exists;

   --------------
   -- Get_Line --
   --------------

   --  Current_Input case

   function Get_Line return String is
      Buffer : String (1 .. Buflen);
      --  Buffer to read in chunks of remaining line. Will work with any
      --  size buffer. We choose a length so that most of the time no
      --  recursion will be required.

      Last : Natural;

   begin
      Ada.Text_IO.Get_Line (Buffer, Last);

      --  If the buffer is not full, then we are all done

      if Last < Buffer'Last then
         return Buffer (1 .. Last);

      --  Otherwise, we still have characters left on the line. Note that
      --  as specified by (RM A.10.7(19)) the end of line is not skipped
      --  in this case, even if we are right at it now.

      else
         return Buffer & GNAT.IO_Aux.Get_Line;
      end if;
   end Get_Line;

   --  Case of reading from a specified file. Note that we could certainly
   --  share code between these two versions, but these are very short
   --  routines, and we may as well aim for maximum speed, cutting out an
   --  intermediate call (calls returning string may be somewhat slow)

   function Get_Line (File : Ada.Text_IO.File_Type) return String is
      Buffer : String (1 .. Buflen);
      Last   : Natural;

   begin
      Ada.Text_IO.Get_Line (File, Buffer, Last);

      if Last < Buffer'Last then
         return Buffer (1 .. Last);
      else
         return Buffer & Get_Line (File);
      end if;
   end Get_Line;

end GNAT.IO_Aux;
