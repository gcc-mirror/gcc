------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . G L O B A L _ L O C K S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2024, Free Software Foundation, Inc.         --
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

   --  This package contains the necessary routines to provide
   --  reliable system wide locking capability.

package System.Global_Locks is

   Lock_Error : exception;
   --  Exception raised if a request cannot be executed on a lock

   type Lock_Type is private;
   --  Such a lock is a global lock between partitions. This lock is
   --  uniquely defined between the partitions because of its name.

   Null_Lock : constant Lock_Type;
   --  This needs comments ???

   procedure Create_Lock (Lock : out Lock_Type; Name : String);
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
