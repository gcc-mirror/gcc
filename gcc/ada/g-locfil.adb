------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      G N A T . L O C K _ F I L E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1998-2001 Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.   --
--                                                                          --
------------------------------------------------------------------------------

with System;

package body GNAT.Lock_Files is

   Dir_Separator : Character;
   pragma Import (C, Dir_Separator, "__gnat_dir_separator");

   ---------------
   -- Lock_File --
   ---------------

   procedure Lock_File
     (Directory      : String;
      Lock_File_Name : String;
      Wait           : Duration := 1.0;
      Retries        : Natural  := Natural'Last)
   is
      Dir  : aliased String := Directory & ASCII.NUL;
      File : aliased String := Lock_File_Name & ASCII.NUL;

      function Try_Lock (Dir, File : System.Address) return Integer;
      pragma Import (C, Try_Lock, "__gnat_try_lock");

   begin
      for I in 0 .. Retries loop
         if Try_Lock (Dir'Address, File'Address) = 1 then
            return;
         end if;
         exit when I = Retries;
         delay Wait;
      end loop;
      raise Lock_Error;
   end Lock_File;

   ---------------
   -- Lock_File --
   ---------------

   procedure Lock_File
     (Lock_File_Name : String;
      Wait           : Duration := 1.0;
      Retries        : Natural  := Natural'Last)
   is
   begin
      for J in reverse Lock_File_Name'Range loop
         if Lock_File_Name (J) = Dir_Separator then
            Lock_File
              (Lock_File_Name (Lock_File_Name'First .. J - 1),
               Lock_File_Name (J + 1 .. Lock_File_Name'Last),
               Wait,
               Retries);
            return;
         end if;
      end loop;

      Lock_File (".", Lock_File_Name, Wait, Retries);
   end Lock_File;

   -----------------
   -- Unlock_File --
   -----------------

   procedure Unlock_File (Lock_File_Name : String) is
      S : aliased String := Lock_File_Name & ASCII.NUL;

      procedure unlink (A : System.Address);
      pragma Import (C, unlink, "unlink");

   begin
      unlink (S'Address);
   end Unlock_File;

   -----------------
   -- Unlock_File --
   -----------------

   procedure Unlock_File (Directory : String; Lock_File_Name : String) is
   begin
      Unlock_File (Directory & Dir_Separator & Lock_File_Name);
   end Unlock_File;

end GNAT.Lock_Files;
