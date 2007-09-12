------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . D S A _ S E R V I C E S                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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

--  This package is for distributed system annex services, which require the
--  partition communication sub-system to be initialized before they are used.

with System.Partition_Interface;
with System.RPC;

package System.DSA_Services is

   function Get_Active_Partition_ID
     (Name : Partition_Interface.Unit_Name) return RPC.Partition_ID
     renames Partition_Interface.Get_Active_Partition_ID;
   --  Return the partition ID of the partition in which unit Name resides

   function Get_Local_Partition_ID return RPC.Partition_ID
     renames Partition_Interface.Get_Local_Partition_ID;
   --  Return the Partition_ID of the current partition

   function Get_Passive_Partition_ID
     (Name : Partition_Interface.Unit_Name) return RPC.Partition_ID
     renames Partition_Interface.Get_Passive_Partition_ID;
   --  Return the Partition_ID of the given shared passive partition

end System.DSA_Services;
