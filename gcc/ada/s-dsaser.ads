------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . D S A _ S E R V I C E S                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--         Copyright (C) 2006-2009  Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
