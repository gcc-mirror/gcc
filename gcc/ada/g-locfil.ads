------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      G N A T . L O C K _ F I L E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1995-2001 Ada Core Technologies, Inc.            --
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

--  This package contains the necessary routines for using files for the
--  purpose of providing realiable system wide locking capability.

package GNAT.Lock_Files is
pragma Preelaborate;

   Lock_Error : exception;
   --  Exception raised if file cannot be locked

   subtype Path_Name is String;
   --  Pathname is used by all services provided in this unit to specified
   --  directory name and file name. On DOS based systems both directory
   --  separators are handled (i.e. slash and backslash).

   procedure Lock_File
     (Directory      : Path_Name;
      Lock_File_Name : Path_Name;
      Wait           : Duration := 1.0;
      Retries        : Natural  := Natural'Last);
   --  Create a lock file Lock_File_Name in directory Directory. If the file
   --  cannot be locked because someone already owns the lock, this procedure
   --  waits Wait seconds and retries at most Retries times. If the file
   --  still cannot be locked, Lock_Error is raised. The default is to try
   --  every second, almost forever (Natural'Last times). The full path of
   --  the file is constructed by concatenating Directory and Lock_File_Name.
   --  Directory can optionally terminate with a directory separator.

   procedure Lock_File
     (Lock_File_Name : Path_Name;
      Wait           : Duration := 1.0;
      Retries        : Natural  := Natural'Last);
   --  See above. The full lock file path is given as one string.

   procedure Unlock_File (Directory : Path_Name; Lock_File_Name : Path_Name);
   --  Unlock a file. Directory can optionally terminate with a directory
   --  separator.

   procedure Unlock_File (Lock_File_Name : Path_Name);
   --  Unlock a file whose full path is given in Lock_File_Name

end GNAT.Lock_Files;
