------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . S T O R A G E _ P O O L S . S U B P O O L S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2020, Free Software Foundation, Inc.         --
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
with System.Finalization_Masters;
with System.Storage_Elements;

package System.Storage_Pools.Subpools is
   pragma Preelaborate;

   type Root_Storage_Pool_With_Subpools is abstract
     new Root_Storage_Pool with private;
   pragma Preelaborable_Initialization (Root_Storage_Pool_With_Subpools);
   --  The base for all implementations of Storage_Pool_With_Subpools. This
   --  type is Limited_Controlled by derivation. To use subpools, an access
   --  type must be associated with an implementation descending from type
   --  Root_Storage_Pool_With_Subpools.

   type Root_Subpool is abstract tagged limited private;
   pragma Preelaborable_Initialization (Root_Subpool);
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
   --  on the default subpool of Pool. Controlled types allocated through this
   --  routine will NOT be handled properly.

   procedure Allocate_From_Subpool
     (Pool                     : in out Root_Storage_Pool_With_Subpools;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count;
      Subpool                  : not null Subpool_Handle) is abstract;

   --  ??? This precondition causes errors in simple tests, disabled for now

   --      with Pre'Class => Pool_Of_Subpool (Subpool) = Pool'Access;
   --  This routine requires implementation. Allocate an object described by
   --  Size_In_Storage_Elements and Alignment on a subpool.

   function Create_Subpool
     (Pool : in out Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle is abstract;
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
   --  This precondition causes errors in simple tests, disabled for now???
   --  with Pre'Class => Pool_Of_Subpool (Subpool) = Pool'Access;

   --  This routine requires implementation. Reclaim the storage a particular
   --  subpool occupies in a pool_with_subpools. This routine is called by
   --  Ada.Unchecked_Deallocate_Subpool.

   function Default_Subpool_For_Pool
     (Pool : in out Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle;
   --  Return a common subpool which is used for object allocations without a
   --  Subpool_Handle_Name in the allocator. The default implementation of this
   --  routine raises Program_Error.

   function Pool_Of_Subpool
     (Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class;
   --  Return the owner of the subpool

   procedure Set_Pool_Of_Subpool
     (Subpool : not null Subpool_Handle;
      To      : in out Root_Storage_Pool_With_Subpools'Class);
   --  Set the owner of the subpool. This is intended to be called from
   --  Create_Subpool or similar subpool constructors. Raises Program_Error
   --  if the subpool already belongs to a pool.

   overriding function Storage_Size
     (Pool : Root_Storage_Pool_With_Subpools)
      return System.Storage_Elements.Storage_Count
   is
      (System.Storage_Elements.Storage_Count'Last);

private
   --  Model
   --             Pool_With_Subpools     SP_Node    SP_Node    SP_Node
   --       +-->+--------------------+   +-----+    +-----+    +-----+
   --       |   |      Subpools -------->|  ------->|  ------->|  ------->
   --       |   +--------------------+   +-----+    +-----+    +-----+
   --       |   |Finalization_Started|<------  |<-------  |<-------  |<---
   --       |   +--------------------+   +-----+    +-----+    +-----+
   --       +--- Controller.Encl_Pool|   | nul |    |  +  |    |  +  |
   --       |   +--------------------+   +-----+    +--|--+    +--:--+
   --       |   :                    :    Dummy        |  ^       :
   --       |   :                    :                 |  |       :
   --       |                            Root_Subpool  V  |
   --       |                            +-------------+  |
   --       +-------------------------------- Owner    |  |
   --               FM_Node   FM_Node    +-------------+  |
   --               +-----+   +-----+<-- Master.Objects|  |
   --            <------  |<------  |    +-------------+  |
   --               +-----+   +-----+    |    Node -------+
   --               |  ------>|  ----->  +-------------+
   --               +-----+   +-----+    :             :
   --               |ctrl |    Dummy     :             :
   --               | obj |
   --               +-----+
   --
   --  SP_Nodes are created on the heap. FM_Nodes and associated objects are
   --  created on the pool_with_subpools.

   type Any_Storage_Pool_With_Subpools_Ptr
     is access all Root_Storage_Pool_With_Subpools'Class;
   for Any_Storage_Pool_With_Subpools_Ptr'Storage_Size use 0;

   --  A pool controller is a special controlled object which ensures the
   --  proper initialization and finalization of the enclosing pool.

   type Pool_Controller (Enclosing_Pool : Any_Storage_Pool_With_Subpools_Ptr)
     is new Ada.Finalization.Limited_Controlled with null record;

   --  Subpool list types. Each pool_with_subpools contains a list of subpools.
   --  This is an indirect doubly linked list since subpools are not supposed
   --  to be allocatable by language design.

   type SP_Node;
   type SP_Node_Ptr is access all SP_Node;

   type SP_Node is record
      Prev    : SP_Node_Ptr := null;
      Next    : SP_Node_Ptr := null;
      Subpool : Subpool_Handle := null;
   end record;

   --  Root_Storage_Pool_With_Subpools internal structure. The type uses a
   --  special controller to perform initialization and finalization actions
   --  on itself. This is necessary because the end user of this package may
   --  decide to override Initialize and Finalize, thus disabling the desired
   --  behavior.

   --          Pool_With_Subpools     SP_Node    SP_Node    SP_Node
   --    +-->+--------------------+   +-----+    +-----+    +-----+
   --    |   |      Subpools -------->|  ------->|  ------->|  ------->
   --    |   +--------------------+   +-----+    +-----+    +-----+
   --    |   |Finalization_Started|   :     :    :     :    :     :
   --    |   +--------------------+
   --    +--- Controller.Encl_Pool|
   --        +--------------------+
   --        :       End-user     :
   --        :      components    :

   type Root_Storage_Pool_With_Subpools is abstract
     new Root_Storage_Pool with
   record
      Subpools : aliased SP_Node;
      --  A doubly linked list of subpools

      Finalization_Started : Boolean := False;
      pragma Atomic (Finalization_Started);
      --  A flag which prevents the creation of new subpools while the master
      --  pool is being finalized. The flag needs to be atomic because it is
      --  accessed without Lock_Task / Unlock_Task.

      Controller : Pool_Controller
                     (Root_Storage_Pool_With_Subpools'Unchecked_Access);
      --  A component which ensures that the enclosing pool is initialized and
      --  finalized at the appropriate places.
   end record;

   --  A subpool is an abstraction layer which sits on top of a pool. It
   --  contains links to all controlled objects allocated on a particular
   --  subpool.

   --        Pool_With_Subpools   SP_Node    SP_Node    SP_Node
   --    +-->+----------------+   +-----+    +-----+    +-----+
   --    |   |    Subpools ------>|  ------->|  ------->|  ------->
   --    |   +----------------+   +-----+    +-----+    +-----+
   --    |   :                :<------  |<-------  |<-------  |
   --    |   :                :   +-----+    +-----+    +-----+
   --    |                        |null |    |  +  |    |  +  |
   --    |                        +-----+    +--|--+    +--:--+
   --    |                                      |  ^       :
   --    |                        Root_Subpool  V  |
   --    |                        +-------------+  |
   --    +---------------------------- Owner    |  |
   --                             +-------------+  |
   --                      .......... Master    |  |
   --                             +-------------+  |
   --                             |    Node -------+
   --                             +-------------+
   --                             :   End-user  :
   --                             :  components :

   type Root_Subpool is abstract tagged limited record
      Owner : Any_Storage_Pool_With_Subpools_Ptr := null;
      --  A reference to the master pool_with_subpools

      Master : aliased System.Finalization_Masters.Finalization_Master;
      --  A heterogeneous collection of controlled objects

      Node : SP_Node_Ptr := null;
      --  A link to the doubly linked list node which contains the subpool.
      --  This back pointer is used in subpool deallocation.
   end record;

   procedure Adjust_Controlled_Dereference
     (Addr         : in out System.Address;
      Storage_Size : in out System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);
   --  Given the memory attributes of a heap-allocated object that is known to
   --  be controlled, adjust the address and size of the object to include the
   --  two hidden pointers inserted by the finalization machinery.

   --  ??? Once Storage_Pools.Allocate_Any is removed, this should be renamed
   --  to Allocate_Any.

   procedure Allocate_Any_Controlled
     (Pool            : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle;
      Context_Master  : Finalization_Masters.Finalization_Master_Ptr;
      Fin_Address     : Finalization_Masters.Finalize_Address_Ptr;
      Addr            : out System.Address;
      Storage_Size    : System.Storage_Elements.Storage_Count;
      Alignment       : System.Storage_Elements.Storage_Count;
      Is_Controlled   : Boolean;
      On_Subpool      : Boolean);
   --  Compiler interface. This version of Allocate handles all possible cases,
   --  either on a pool or a pool_with_subpools, regardless of the controlled
   --  status of the allocated object. Parameter usage:
   --
   --    * Pool - The pool associated with the access type. Pool can be any
   --    derivation from Root_Storage_Pool, including a pool_with_subpools.
   --
   --    * Context_Subpool - The subpool handle name of an allocator. If no
   --    subpool handle is present at the point of allocation, the actual
   --    would be null.
   --
   --    * Context_Master - The finalization master associated with the access
   --    type. If the access type's designated type is not controlled, the
   --    actual would be null.
   --
   --    * Fin_Address - TSS routine Finalize_Address of the designated type.
   --    If the designated type is not controlled, the actual would be null.
   --
   --    * Addr - The address of the allocated object.
   --
   --    * Storage_Size - The size of the allocated object.
   --
   --    * Alignment - The alignment of the allocated object.
   --
   --    * Is_Controlled - A flag which determines whether the allocated object
   --    is controlled. When set to True, the machinery generates additional
   --    data.
   --
   --    * On_Subpool - A flag which determines whether the a subpool handle
   --    name is present at the point of allocation. This is used for error
   --    diagnostics.

   procedure Deallocate_Any_Controlled
     (Pool          : in out Root_Storage_Pool'Class;
      Addr          : System.Address;
      Storage_Size  : System.Storage_Elements.Storage_Count;
      Alignment     : System.Storage_Elements.Storage_Count;
      Is_Controlled : Boolean);
   --  Compiler interface. This version of Deallocate handles all possible
   --  cases, either from a pool or a pool_with_subpools, regardless of the
   --  controlled status of the deallocated object. Parameter usage:
   --
   --    * Pool - The pool associated with the access type. Pool can be any
   --    derivation from Root_Storage_Pool, including a pool_with_subpools.
   --
   --    * Addr - The address of the allocated object.
   --
   --    * Storage_Size - The size of the allocated object.
   --
   --    * Alignment - The alignment of the allocated object.
   --
   --    * Is_Controlled - A flag which determines whether the allocated object
   --    is controlled. When set to True, the machinery generates additional
   --    data.

   procedure Detach (N : not null SP_Node_Ptr);
   --  Unhook a subpool node from an arbitrary subpool list

   overriding procedure Finalize (Controller : in out Pool_Controller);
   --  Buffer routine, calls Finalize_Pool

   procedure Finalize_Pool (Pool : in out Root_Storage_Pool_With_Subpools);
   --  Iterate over all subpools of Pool, detach them one by one and finalize
   --  their masters. This action first detaches a controlled object from a
   --  particular master, then invokes its Finalize_Address primitive.

   function Header_Size_With_Padding
     (Alignment : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Count;
   --  Given an arbitrary alignment, calculate the size of the header which
   --  precedes a controlled object as the nearest multiple rounded up of the
   --  alignment.

   overriding procedure Initialize (Controller : in out Pool_Controller);
   --  Buffer routine, calls Initialize_Pool

   procedure Initialize_Pool (Pool : in out Root_Storage_Pool_With_Subpools);
   --  Setup the doubly linked list of subpools

   procedure Print_Pool (Pool : Root_Storage_Pool_With_Subpools);
   --  Debug routine, output the contents of a pool_with_subpools

   procedure Print_Subpool (Subpool : Subpool_Handle);
   --  Debug routine, output the contents of a subpool

end System.Storage_Pools.Subpools;
