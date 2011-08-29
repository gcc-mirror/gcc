------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . S T O R A G E _ P O O L S . S U B P O O L S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
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

with System.Finalization_Masters;
with System.Storage_Elements;

package System.Storage_Pools.Subpools is
   pragma Preelaborate (System.Storage_Pools.Subpools);

   type Root_Storage_Pool_With_Subpools is abstract
     new Root_Storage_Pool with private;
   --  The base for all implementations of Storage_Pool_With_Subpools. This
   --  type is Limited_Controlled by derivation. To use subpools, an access
   --  type must be associated with an implementation descending from type
   --  Root_Storage_Pool_With_Subpools.

   type Root_Subpool is abstract tagged limited private;
   --  The base for all implementations of Subpool. Objects of this type are
   --  managed by the pool_with_subpools.

   type Subpool_Handle is access all Root_Subpool'Class;
   for Subpool_Handle'Storage_Size use 0;
   --  Since subpools are limited types by definition, a handle is instead used
   --  to manage subpool abstractions.

   overriding procedure Allocate
     (Pool                     : in out Root_Storage_Pool_With_Subpools;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);
   --  Allocate an object described by Size_In_Storage_Elements and Alignment
   --  on the default subpool of Pool.

   procedure Allocate_From_Subpool
     (Pool                     : in out Root_Storage_Pool_With_Subpools;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count;
      Subpool                  : not null Subpool_Handle)
   is abstract;

   --  ??? This precondition causes errors in simple tests, disabled for now

--     with Pre'Class => Pool_Of_Subpool (Subpool) = Pool'Access;
   --  This routine requires implementation. Allocate an object described by
   --  Size_In_Storage_Elements and Alignment on a subpool.

   function Create_Subpool
     (Pool         : in out Root_Storage_Pool_With_Subpools;
      Storage_Size : Storage_Elements.Storage_Count :=
                     Storage_Elements.Storage_Count'Last)
   return not null Subpool_Handle
   is abstract;
   --  This routine requires implementation. Create a subpool within the given
   --  pool_with_subpools.

   overriding procedure Deallocate
     (Pool                     : in out Root_Storage_Pool_With_Subpools;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is null;

   procedure Deallocate_Subpool
     (Pool    : in out Root_Storage_Pool_With_Subpools;
      Subpool : in out Subpool_Handle)
   is abstract;

   --  ??? This precondition causes errors in simple tests, disabled for now

--     with Pre'Class => Pool_Of_Subpool (Subpool) = Pool'Access;
   --  This routine requires implementation. Reclaim the storage a particular
   --  subpool occupies in a pool_with_subpools. This routine is called by
   --  Ada.Unchecked_Deallocate_Subpool.

   function Default_Subpool_For_Pool
     (Pool : Root_Storage_Pool_With_Subpools)
   return not null Subpool_Handle
   is abstract;
   --  This routine requires implementation. Returns a common subpool used for
   --  allocations without Subpool_Handle_name in the allocator.

   function Pool_Of_Subpool
     (Subpool : not null Subpool_Handle)
   return access Root_Storage_Pool_With_Subpools'Class;
   --  Return the owner of the subpool

   procedure Set_Pool_Of_Subpool
     (Subpool : not null Subpool_Handle;
      Pool    : in out Root_Storage_Pool_With_Subpools'Class);
   --  Set the owner of the subpool. This is intended to be called from
   --  Create_Subpool or similar subpool constructors. Raises Program_Error
   --  if the subpool already belongs to a pool.

private
   --  Model
   --                           Pool_With_Subpools
   --                 +----> +---------------------+ <----+
   --                 |  +---------- Subpools      |      |
   --                 |  |   +---------------------+      |
   --                 |  |   :      User data      :      |
   --                 |  |   '.....................'      |
   --                 |  |                                |
   --                 |  |    SP_Node       SP_Node       |
   --                 |  +-> +-------+     +-------+      |
   --                 |      | Prev  <-----> Prev  |      |
   --                 |      +-------+     +-------+      |
   --                 |      | Next  <---->| Next  |      |
   --                 |      +-------+     +-------+      |
   --                 |  +----Subpool|     |Subpool----+  |
   --                 |  |   +-------+     +-------+   |  |
   --                 |  |                             |  |
   --                 |  |    Subpool       Subpool    |  |
   --                 |  +-> +-------+     +-------+ <-+  |
   --                 +------- Owner |     | Owner -------+
   --                        +-------+     +-------+
   --     +------------------- Master|     | Master---------------+
   --     |                  +-------+     +-------+              |
   --     |                  : User  :     : User  :              |
   --     |                  : Data  :     : Data  :              |
   --     |                  '.......'     '.......'              |
   --     |                                                       |
   --     |                           Heap                        |
   --  .. | ..................................................... | ..
   --  :  |                                                       |  :
   --  :  |    Object    Object    Object               Object    |  :
   --  :  +-> +------+  +------+  +------+             +------+ <-+  :
   --  :      | Prev <--> Prev <--> Prev |             | Prev |      :
   --  :      +------+  +------+  +------+             +------+      :
   --  :      | Next <--> Next <--> Next |             | Next |      :
   --  :      +------+  +------+  +------+             +------+      :
   --  :      |  FA  |  |  FA  |  |  FA  |             |  FA  |      :
   --  :      +------+  +------+  +------+             +------+      :
   --  :      :      :  :      :  :      :             :      :      :
   --  :      :      :  :      :  :      :             :      :      :
   --  :      '......'  '......'  '......'             '......'      :
   --  :                                                             :
   --  '.............................................................'

   --  Subpool list types. Each pool_with_subpools contains a list of subpools.

   type SP_Node;
   type SP_Node_Ptr is access all SP_Node;

   type SP_Node is record
      Prev    : SP_Node_Ptr := null;
      Next    : SP_Node_Ptr := null;
      Subpool : Subpool_Handle := null;
   end record;

   --  Root_Storage_Pool_With_Subpools internal structure

   type Root_Storage_Pool_With_Subpools is abstract
     new Root_Storage_Pool with
   record
      Initialized : Boolean := False;
      pragma Atomic (Initialized);
      --  Even though this type is derived from Limited_Controlled, overriding
      --  Initialize would have no effect since the type is abstract. Routine
      --  Set_Pool_Of_Subpool is tasked with the initialization of a pool with
      --  subpools because it has to be called at some point. This flag is used
      --  to prevent the resetting of the subpool chain.

      Subpools : aliased SP_Node;
      --  A doubly linked list of subpools

      Finalization_Started : Boolean := False;
      pragma Atomic (Finalization_Started);
      --  A flag which prevents the creation of new subpools while the master
      --  pool is being finalized. The flag needs to be atomic because it is
      --  accessed without Lock_Task / Unlock_Task.
   end record;

   type Any_Storage_Pool_With_Subpools_Ptr
     is access all Root_Storage_Pool_With_Subpools'Class;
   for Any_Storage_Pool_With_Subpools_Ptr'Storage_Size use 0;

   --  A subpool is an abstraction layer which sits on top of a pool. It
   --  contains links to all controlled objects allocated on a particular
   --  subpool.

   type Root_Subpool is abstract tagged limited record
      Owner : Any_Storage_Pool_With_Subpools_Ptr := null;
      --  A reference to the master pool_with_subpools

      Master : aliased System.Finalization_Masters.Finalization_Master;
      --  A collection of controlled objects
   end record;

   --  ??? Once Storage_Pools.Allocate_Any is removed, this should be renamed
   --  to Allocate_Any.

   procedure Allocate_Any_Controlled
     (Pool            : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle := null;
      Context_Master  : Finalization_Masters.Finalization_Master_Ptr := null;
      Fin_Address     : Finalization_Masters.Finalize_Address_Ptr := null;
      Addr            : out System.Address;
      Storage_Size    : System.Storage_Elements.Storage_Count;
      Alignment       : System.Storage_Elements.Storage_Count;
      Is_Controlled   : Boolean := True);
   --  Compiler interface. This version of Allocate handles all possible cases,
   --  either on a pool or a pool_with_subpools.

   procedure Deallocate_Any_Controlled
     (Pool          : in out Root_Storage_Pool'Class;
      Addr          : System.Address;
      Storage_Size  : System.Storage_Elements.Storage_Count;
      Alignment     : System.Storage_Elements.Storage_Count;
      Is_Controlled : Boolean := True);
   --  Compiler interface. This version of Deallocate handles all possible
   --  cases, either from a pool or a pool_with_subpools.

   overriding procedure Finalize
     (Pool : in out Root_Storage_Pool_With_Subpools);
   --  Iterate over all subpools of Pool, detach them one by one and finalize
   --  their masters. This action first detaches a controlled object from a
   --  particular master, then invokes its Finalize_Address primitive.

   procedure Finalize_Subpool (Subpool : not null Subpool_Handle);
   --  Finalize the master of a subpool

end System.Storage_Pools.Subpools;
