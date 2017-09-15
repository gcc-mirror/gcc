------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . S T O R A G E _ P O O L S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Ada.Finalization;
with System.Storage_Elements;

package System.Storage_Pools is
   pragma Preelaborate;

   type Root_Storage_Pool is abstract
     new Ada.Finalization.Limited_Controlled with private;
   pragma Preelaborable_Initialization (Root_Storage_Pool);

   procedure Allocate
     (Pool                     : in out Root_Storage_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is abstract;

   procedure Deallocate
     (Pool                     : in out Root_Storage_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is abstract;

   function Storage_Size
     (Pool : Root_Storage_Pool)
      return System.Storage_Elements.Storage_Count
   is abstract;

private
   type Root_Storage_Pool is abstract
     new Ada.Finalization.Limited_Controlled with null record;

   type Root_Storage_Pool_Ptr is access all Root_Storage_Pool'Class;
   for Root_Storage_Pool_Ptr'Storage_Size use 0;
   --  Type of the BIP_Storage_Pool extra parameter (see Exp_Ch6). The
   --  Storage_Size clause is necessary, because otherwise we have a
   --  chicken&egg problem; we can't be creating collection finalization code
   --  in this low-level package, because that involves Pool_Global, which
   --  imports this package.

   --  ??? Are these two still needed? It might be possible to use Subpools.
   --  Allocate_Any_Controlled / Deallocate_Any_Controlled for non-controlled
   --  objects.

   --  The following two procedures support the use of class-wide pool
   --  objects in storage pools. When a local type is given a class-wide
   --  storage pool, allocation and deallocation for the type must dispatch
   --  to the operation of the specific pool, which is achieved by a call
   --  to these procedures. (When the pool type is specific, the back-end
   --  generates a call to the statically identified operation of the type).

   procedure Allocate_Any
    (Pool                     : in out Root_Storage_Pool'Class;
     Storage_Address          : out System.Address;
     Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
     Alignment                : System.Storage_Elements.Storage_Count);

   procedure Deallocate_Any
    (Pool                     : in out Root_Storage_Pool'Class;
     Storage_Address          : System.Address;
     Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
     Alignment                : System.Storage_Elements.Storage_Count);

end System.Storage_Pools;
