------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . I O _ A U X                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2020, AdaCore                     --
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
