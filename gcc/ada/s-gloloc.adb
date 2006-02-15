------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . G L O B A L _ L O C K S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2006, Free Software Foundation, Inc.         --
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

with System.Soft_Links;
--  used for Lock_Task, Unlock_Task

package body System.Global_Locks is

   type String_Access is access String;

   Dir_Separator : Character;
   pragma Import (C, Dir_Separator, "__gnat_dir_separator");

   type Lock_File_Entry is record
      Dir  : String_Access;
      File : String_Access;
   end record;

   Last_Lock  : Lock_Type := Null_Lock;
   Lock_Table : array (Lock_Type range 1 .. 15) of Lock_File_Entry;

   procedure Lock_File
     (Dir     : String;
      File    : String;
      Wait    : Duration := 0.1;
      Retries : Natural  := Natural'Last);
   --  Create a lock file File in directory Dir. If the file  cannot be
   --  locked because someone already owns the lock, this procedure
   --  waits Wait seconds and retries at most Retries times. If the file
   --  still cannot be locked, Lock_Error is raised. The default is to try
   --  every second, almost forever (Natural'Last times).

   ------------------
   -- Acquire_Lock --
   ------------------

   procedure Acquire_Lock (Lock : in out Lock_Type) is
   begin
      Lock_File
        (Lock_Table (Lock).Dir.all,
         Lock_Table (Lock).File.all);
   end Acquire_Lock;

   -----------------
   -- Create_Lock --
   -----------------

   procedure Create_Lock (Lock : out Lock_Type; Name : String) is
      L : Lock_Type;

   begin
      System.Soft_Links.Lock_Task.all;
      Last_Lock := Last_Lock + 1;
      L := Last_Lock;
      System.Soft_Links.Unlock_Task.all;

      if L > Lock_Table'Last then
         raise Lock_Error;
      end if;

      for J in reverse Name'Range loop
         if Name (J) = Dir_Separator then
            Lock_Table (L).Dir := new String'(Name (Name'First .. J - 1));
            Lock_Table (L).File := new String'(Name (J + 1 .. Name'Last));
            exit;
         end if;
      end loop;

      if Lock_Table (L).Dir = null then
         Lock_Table (L).Dir  := new String'(".");
         Lock_Table (L).File := new String'(Name);
      end if;

      Lock := L;
   end Create_Lock;

   ---------------
   -- Lock_File --
   ---------------

   procedure Lock_File
     (Dir     : String;
      File    : String;
      Wait    : Duration := 0.1;
      Retries : Natural  := Natural'Last)
   is
      C_Dir  : aliased String := Dir & ASCII.NUL;
      C_File : aliased String := File & ASCII.NUL;

      function Try_Lock (Dir, File : System.Address) return Integer;
      pragma Import (C, Try_Lock, "__gnat_try_lock");

   begin
      for I in 0 .. Retries loop
         if Try_Lock (C_Dir'Address, C_File'Address) = 1 then
            return;
         end if;

         exit when I = Retries;
         delay Wait;
      end loop;

      raise Lock_Error;
   end Lock_File;

   ------------------
   -- Release_Lock --
   ------------------

   procedure Release_Lock (Lock : in out Lock_Type) is
      S : aliased String :=
            Lock_Table (Lock).Dir.all & Dir_Separator &
            Lock_Table (Lock).File.all & ASCII.NUL;

      procedure unlink (A : System.Address);
      pragma Import (C, unlink, "unlink");

   begin
      unlink (S'Address);
   end Release_Lock;

end System.Global_Locks;
