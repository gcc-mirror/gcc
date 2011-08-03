------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     A D A . F I N A L I Z A T I O N . H E A P _ M A N A G E M E N T      --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--          Copyright (C) 2008-2011, Free Software Foundation, Inc.         --
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

with System;
with System.Storage_Elements;
with System.Storage_Pools;

package Ada.Finalization.Heap_Management is

   --  A reference to any derivation of Root_Storage_Pool. Since this type may
   --  not be used to allocate objects, its storage size is zero.

   type Any_Storage_Pool_Ptr is
     access System.Storage_Pools.Root_Storage_Pool'Class;
   for Any_Storage_Pool_Ptr'Storage_Size use 0;

   --  ??? Comment needed on overall mechanism

   type Finalization_Collection is
     new Ada.Finalization.Limited_Controlled with private;

   type Finalization_Collection_Ptr is access all Finalization_Collection;
   for Finalization_Collection_Ptr'Storage_Size use 0;

   --  A reference used to describe primitive Finalize_Address

   type Finalize_Address_Ptr is access procedure (Obj : System.Address);

   --  Since RTSfind cannot contain names of the form RE_"+", the following
   --  routine serves as a wrapper around System.Storage_Elements."+".

   function Add_Offset_To_Address
     (Addr   : System.Address;
      Offset : System.Storage_Elements.Storage_Offset) return System.Address;

   procedure Allocate
     (Collection   : in out Finalization_Collection;
      Addr         : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count;
      Needs_Header : Boolean := True);
   --  Allocate a chunk of memory described by Storage_Size and Alignment on
   --  Collection's underlying storage pool. Return the address of the chunk.
   --  The routine creates a list header which precedes the chunk of memory is
   --  flag Needs_Header is set. If allocated, the header is attached to the
   --  Collection's objects. The interface to this routine is provided by
   --  Build_Allocate_Deallocate_Proc.

   function Base_Pool
     (Collection : Finalization_Collection) return Any_Storage_Pool_Ptr;
   --  Return a reference to the underlying storage pool of Collection

   procedure Deallocate
     (Collection   : in out Finalization_Collection;
      Addr         : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count;
      Has_Header   : Boolean := True);
   --  Deallocate a chunk of memory described by Storage_Size and Alignment
   --  from Collection's underlying storage pool. The beginning of the memory
   --  chunk is designated by Addr. The routine detaches and destroys the
   --  preceding list header if flag Has_Header is set. The interface to this
   --  routine is provided by Build_Allocate_Deallocate_Proc.

   overriding procedure Finalize
     (Collection : in out Finalization_Collection);
   --  Traverse the objects of Collection, invoking Finalize_Address on eanch
   --  of them. In the end, the routine destroys its dummy head and tail.

   overriding procedure Initialize
     (Collection : in out Finalization_Collection);
   --  Create a new Collection by allocating a dummy head and tal

   procedure Set_Finalize_Address_Ptr
     (Collection : in out Finalization_Collection;
      Proc_Ptr   : Finalize_Address_Ptr);
   --  Set the finalization address routine of a finalization collection

   procedure Set_Storage_Pool_Ptr
     (Collection : in out Finalization_Collection;
      Pool_Ptr   : Any_Storage_Pool_Ptr);
   --  Set the underlying storage pool of a finalization collection

private
   --  Homogeneous collection types

   type Node;
   type Node_Ptr is access all Node;
   pragma No_Strict_Aliasing (Node_Ptr);

   type Node is record
      Prev : Node_Ptr;
      Next : Node_Ptr;
   end record;

   type Finalization_Collection is
     new Ada.Finalization.Limited_Controlled with
   record
      Base_Pool : Any_Storage_Pool_Ptr;
      --  All objects and node headers are allocated on this underlying pool,
      --  the collection is simply a wrapper around it.

      Objects : Node_Ptr;
      --  The head of a doubly linked list

      Finalize_Address : Finalize_Address_Ptr;
      --  A reference to a routine which finalizes an object denoted by its
      --  address. The collection must be homogenious since the same routine
      --  will be invoked for every allocated object when the pool is
      --  finalized.

      Finalization_Started : Boolean := False;
      --  When the finalization of a collection takes place, any allocations on
      --  the same collection are prohibited and the action must raise Program_
      --  Error.
   end record;

   procedure pcol (Collection : Finalization_Collection);
   --  Output the contents of a collection in a readable form. Intended for
   --  debugging purposes.

end Ada.Finalization.Heap_Management;
