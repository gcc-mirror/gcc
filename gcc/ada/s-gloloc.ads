------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . G L O B A L _ L O C K S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2002 Free Software Foundation, Inc.          --
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

   --  This package contains the necessary routines to provide
   --  reliable system wide locking capability.

package System.Global_Locks is

   Lock_Error : exception;
   --  Exception raised if a request cannot be executed on a lock.

   type Lock_Type is private;
   --  Such a lock is a global lock between partitions. This lock is
   --  uniquely defined between the partitions because of its name.

   Null_Lock : constant Lock_Type;
   --  This needs comments ???

   procedure Create_Lock (Lock : out Lock_Type; Name : in String);
   --  Create or retrieve a global lock for the current partition using
   --  its Name.

   procedure Acquire_Lock (Lock : in out Lock_Type);
   --  If the lock cannot be acquired because someone already owns it, this
   --  procedure is supposed to wait and retry forever.

   procedure Release_Lock (Lock : in out Lock_Type);

private

   type Lock_Type is new Natural;

   Null_Lock : constant Lock_Type := 0;

end System.Global_Locks;
