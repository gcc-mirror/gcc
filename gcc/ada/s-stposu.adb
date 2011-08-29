------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . S T O R A G E _ P O O L S . S U B P O O L S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with System.Finalization_Masters; use System.Finalization_Masters;
with System.Soft_Links;           use System.Soft_Links;
with System.Storage_Elements;     use System.Storage_Elements;

package body System.Storage_Pools.Subpools is

   procedure Attach (N : not null SP_Node_Ptr; L : not null SP_Node_Ptr);
   --  Attach a subpool node to a pool

   procedure Free is new Ada.Unchecked_Deallocation (SP_Node, SP_Node_Ptr);

   procedure Detach (N : not null SP_Node_Ptr);
   --  Unhook a subpool node from an arbitrary subpool list

   function Nearest_Multiple_Rounded_Up
     (Size      : Storage_Count;
      Alignment : Storage_Count) return Storage_Count;
   --  Given arbitrary values of storage size and alignment, calculate the
   --  nearest multiple of the alignment rounded up where size can fit.

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool                     : in out Root_Storage_Pool_With_Subpools;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
   begin
      --  Dispatch to the user-defined implementations of Allocate_From_Subpool
      --  and Default_Subpool_For_Pool.

      Allocate_From_Subpool
        (Root_Storage_Pool_With_Subpools'Class (Pool),
         Storage_Address,
         Size_In_Storage_Elements,
         Alignment,
         Default_Subpool_For_Pool
           (Root_Storage_Pool_With_Subpools'Class (Pool)));
   end Allocate;

   -----------------------------
   -- Allocate_Any_Controlled --
   -----------------------------

   procedure Allocate_Any_Controlled
     (Pool            : in out Root_Storage_Pool'Class;
      Context_Subpool : Subpool_Handle;
      Context_Master  : Finalization_Masters.Finalization_Master_Ptr;
      Fin_Address     : Finalization_Masters.Finalize_Address_Ptr;
      Addr            : out System.Address;
      Storage_Size    : System.Storage_Elements.Storage_Count;
      Alignment       : System.Storage_Elements.Storage_Count;
      Is_Controlled   : Boolean;
      On_Subpool      : Boolean)
   is
      Is_Subpool_Allocation : constant Boolean :=
                                Pool in Root_Storage_Pool_With_Subpools'Class;

      Master  : Finalization_Master_Ptr := null;
      N_Addr  : Address;
      N_Ptr   : FM_Node_Ptr;
      N_Size  : Storage_Count;
      Subpool : Subpool_Handle := null;

      Header_And_Padding : Storage_Offset;
      --  This offset includes the size of a FM_Node plus any additional
      --  padding due to a larger alignment.

   begin
      --  Step 1: Pool-related runtime checks

      --  Allocation on a pool_with_subpools. In this scenario there is a
      --  master for each subpool. The master of the access type is ignored.

      if Is_Subpool_Allocation then

         --  Case of an allocation without a Subpool_Handle. Dispatch to the
         --  implementation of Default_Subpool_For_Pool.

         if Context_Subpool = null then
            Subpool :=
              Default_Subpool_For_Pool
                (Root_Storage_Pool_With_Subpools'Class (Pool));

         --  Allocation with a Subpool_Handle

         else
            Subpool := Context_Subpool;
         end if;

         --  Ensure proper ownership and chaining of the subpool

         if Subpool.Owner /=
              Root_Storage_Pool_With_Subpools'Class (Pool)'Unchecked_Access
           or else Subpool.Node = null
           or else Subpool.Node.Prev = null
           or else Subpool.Node.Next = null
         then
            raise Program_Error with "incorrect owner of subpool";
         end if;

         Master := Subpool.Master'Unchecked_Access;

      --  Allocation on a simple pool. In this scenario there is a master for
      --  each access-to-controlled type. No context subpool should be present.

      else
         --  If the master is missing, then the expansion of the access type
         --  failed to create one. This is a serious error.

         if Context_Master = null then
            raise Program_Error with "missing master in pool allocation";
         end if;

         --  If a subpool is present, then this is the result of erroneous
         --  allocator expansion. This is not a serious error, but it should
         --  still be detected.

         if Context_Subpool /= null then
            raise Program_Error with "subpool not required in pool allocation";
         end if;

         --  If the allocation is intended to be on a subpool, but the access
         --  type's pool does not support subpools, then this is the result of
         --  erroneous end-user code.

         if On_Subpool then
            raise Program_Error
              with "pool of access type does not support subpools";
         end if;

         Master := Context_Master;
      end if;

      --  Step 2: Master and Finalize_Address-related runtime checks and size
      --  calculations.

      --  Allocation of a descendant from [Limited_]Controlled, a class-wide
      --  object or a record with controlled components.

      if Is_Controlled then

         --  Do not allow the allocation of controlled objects while the
         --  associated master is being finalized.

         if Master.Finalization_Started then
            raise Program_Error with "allocation after finalization started";
         end if;

         --  Check whether primitive Finalize_Address is available. If it is
         --  not, then either the expansion of the designated type failed or
         --  the expansion of the allocator failed. This is a serious error.

         if Fin_Address = null then
            raise Program_Error
              with "primitive Finalize_Address not available";
         end if;

         --  The size must acount for the hidden header preceding the object.
         --  Account for possible padding space before the header due to a
         --  larger alignment.

         Header_And_Padding :=
           Nearest_Multiple_Rounded_Up
             (Size      => Header_Size,
              Alignment => Alignment);

         N_Size := Storage_Size + Header_And_Padding;

      --  Non-controlled allocation

      else
         N_Size := Storage_Size;
      end if;

      --  Step 3: Allocation of object

      --  For descendants of Root_Storage_Pool_With_Subpools, dispatch to the
      --  implementation of Allocate_From_Subpool.

      if Is_Subpool_Allocation then
         Allocate_From_Subpool
           (Root_Storage_Pool_With_Subpools'Class (Pool),
            N_Addr, N_Size, Alignment, Subpool);

      --  For descendants of Root_Storage_Pool, dispatch to the implementation
      --  of Allocate.

      else
         Allocate (Pool, N_Addr, N_Size, Alignment);
      end if;

      --  Step 4: Attachment

      if Is_Controlled then

         --  Map the allocated memory into a FM_Node record. This converts the
         --  top of the allocated bits into a list header. If there is padding
         --  due to larger alignment, the header is placed right next to the
         --  object:

         --     N_Addr  N_Ptr
         --     |       |
         --     V       V
         --     +-------+---------------+----------------------+
         --     |Padding|    Header     |        Object        |
         --     +-------+---------------+----------------------+
         --     ^       ^               ^
         --     |       +- Header_Size -+
         --     |                       |
         --     +- Header_And_Padding --+

         N_Ptr := Address_To_FM_Node_Ptr
                   (N_Addr + Header_And_Padding - Header_Offset);

         --  Prepend the allocated object to the finalization master

         Attach (N_Ptr, Master.Objects'Unchecked_Access);

         if Master.Finalize_Address = null then
            Master.Finalize_Address := Fin_Address;
         end if;

         --  Move the address from the hidden list header to the start of the
         --  object. This operation effectively hides the list header.

         Addr := N_Addr + Header_And_Padding;

      else
         Addr := N_Addr;
      end if;
   end Allocate_Any_Controlled;

   ------------
   -- Attach --
   ------------

   procedure Attach (N : not null SP_Node_Ptr; L : not null SP_Node_Ptr) is
   begin
      --  Ensure that the node has not been attached already

      pragma Assert (N.Prev = null and then N.Next = null);

      Lock_Task.all;

      L.Next.Prev := N;
      N.Next := L.Next;
      L.Next := N;
      N.Prev := L;

      Unlock_Task.all;

      --  Note: No need to unlock in case of an exception because the above
      --  code can never raise one.
   end Attach;

   -------------------------------
   -- Deallocate_Any_Controlled --
   -------------------------------

   procedure Deallocate_Any_Controlled
     (Pool          : in out Root_Storage_Pool'Class;
      Addr          : System.Address;
      Storage_Size  : System.Storage_Elements.Storage_Count;
      Alignment     : System.Storage_Elements.Storage_Count;
      Is_Controlled : Boolean)
   is
      N_Addr : Address;
      N_Ptr  : FM_Node_Ptr;
      N_Size : Storage_Count;

      Header_And_Padding : Storage_Offset;
      --  This offset includes the size of a FM_Node plus any additional
      --  padding due to a larger alignment.

   begin
      --  Step 1: Detachment

      if Is_Controlled then

         --  Account for possible padding space before the header due to a
         --  larger alignment.

         Header_And_Padding :=
           Nearest_Multiple_Rounded_Up
             (Size      => Header_Size,
              Alignment => Alignment);

         --    N_Addr  N_Ptr           Addr (from input)
         --    |       |               |
         --    V       V               V
         --    +-------+---------------+----------------------+
         --    |Padding|    Header     |        Object        |
         --    +-------+---------------+----------------------+
         --    ^       ^               ^
         --    |       +- Header_Size -+
         --    |                       |
         --    +- Header_And_Padding --+

         --  Convert the bits preceding the object into a list header

         N_Ptr := Address_To_FM_Node_Ptr (Addr - Header_Offset);

         --  Detach the object from the related finalization master. This
         --  action does not need to know the prior context used during
         --  allocation.

         Detach (N_Ptr);

         --  Move the address from the object to the beginning of the list
         --  header.

         N_Addr := Addr - Header_And_Padding;

         --  The size of the deallocated object must include the size of the
         --  hidden list header.

         N_Size := Storage_Size + Header_And_Padding;

      else
         N_Addr := Addr;
         N_Size := Storage_Size;
      end if;

      --  Step 2: Deallocation

      --  Dispatch to the proper implementation of Deallocate. This action
      --  covers both Root_Storage_Pool and Root_Storage_Pool_With_Subpools
      --  implementations.

      Deallocate (Pool, N_Addr, N_Size, Alignment);
   end Deallocate_Any_Controlled;

   ------------
   -- Detach --
   ------------

   procedure Detach (N : not null SP_Node_Ptr) is
   begin
      --  Ensure that the node is attached to some list

      pragma Assert (N.Next /= null and then N.Prev /= null);

      Lock_Task.all;

      N.Prev.Next := N.Next;
      N.Next.Prev := N.Prev;

      Unlock_Task.all;

      --  Note: No need to unlock in case of an exception because the above
      --  code can never raise one.
   end Detach;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Controller : in out Pool_Controller) is
   begin
      Finalize_Pool (Controller.Enclosing_Pool.all);
   end Finalize;

   -------------------
   -- Finalize_Pool --
   -------------------

   procedure Finalize_Pool (Pool : in out Root_Storage_Pool_With_Subpools) is
      Curr_Ptr : SP_Node_Ptr;
      Ex_Occur : Exception_Occurrence;
      Next_Ptr : SP_Node_Ptr;
      Raised   : Boolean := False;

   begin
      --  It is possible for multiple tasks to cause the finalization of a
      --  common pool. Allow only one task to finalize the contents.

      if Pool.Finalization_Started then
         return;
      end if;

      --  Lock the pool to prevent the creation of additional subpools while
      --  the available ones are finalized. The pool remains locked because
      --  either it is about to be deallocated or the associated access type
      --  is about to go out of scope.

      Pool.Finalization_Started := True;

      --  Skip the dummy head

      Curr_Ptr := Pool.Subpools.Next;
      while Curr_Ptr /= Pool.Subpools'Unchecked_Access loop
         Next_Ptr := Curr_Ptr.Next;

         --  Perform the following actions:

         --    1) Finalize all objects chained on the subpool's master
         --    2) Remove the the subpool from the owner's list of subpools
         --    3) Deallocate the doubly linked list node associated with the
         --       subpool.

         begin
            Finalize_Subpool (Curr_Ptr.Subpool);

         exception
            when Fin_Occur : others =>
               if not Raised then
                  Raised := True;
                  Save_Occurrence (Ex_Occur, Fin_Occur);
               end if;
         end;

         Curr_Ptr := Next_Ptr;
      end loop;

      --  If the finalization of a particular master failed, reraise the
      --  exception now.

      if Raised then
         Reraise_Occurrence (Ex_Occur);
      end if;
   end Finalize_Pool;

   ----------------------
   -- Finalize_Subpool --
   ----------------------

   procedure Finalize_Subpool (Subpool : not null Subpool_Handle) is
   begin
      --  Do nothing if the subpool was never used

      if Subpool.Owner = null
        or else Subpool.Node = null
      then
         return;
      end if;

      --  Clean up all controlled objects chained on the subpool's master

      Finalize (Subpool.Master);

      --  Remove the subpool from its owner's list of subpools

      Detach (Subpool.Node);

      --  Destroy the associated doubly linked list node which was created in
      --  Set_Pool_Of_Subpool.

      Free (Subpool.Node);
   end Finalize_Subpool;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Controller : in out Pool_Controller) is
   begin
      Initialize_Pool (Controller.Enclosing_Pool.all);
   end Initialize;

   ---------------------
   -- Initialize_Pool --
   ---------------------

   procedure Initialize_Pool (Pool : in out Root_Storage_Pool_With_Subpools) is
   begin
      --  The dummy head must point to itself in both directions

      Pool.Subpools.Next := Pool.Subpools'Unchecked_Access;
      Pool.Subpools.Prev := Pool.Subpools'Unchecked_Access;
   end Initialize_Pool;

   ---------------------------------
   -- Nearest_Multiple_Rounded_Up --
   ---------------------------------

   function Nearest_Multiple_Rounded_Up
     (Size      : Storage_Count;
      Alignment : Storage_Count) return Storage_Count
   is
   begin
      if Size mod Alignment = 0 then
         return Size;

      --  Add enough padding to reach the nearest multiple of the alignment
      --  rounding up.

      else
         return ((Size + Alignment - 1) / Alignment) * Alignment;
      end if;
   end Nearest_Multiple_Rounded_Up;

   ---------------------
   -- Pool_Of_Subpool --
   ---------------------

   function Pool_Of_Subpool (Subpool : not null Subpool_Handle)
     return access Root_Storage_Pool_With_Subpools'Class is
   begin
      return Subpool.Owner;
   end Pool_Of_Subpool;

   -------------------------
   -- Set_Pool_Of_Subpool --
   -------------------------

   procedure Set_Pool_Of_Subpool
     (Subpool : not null Subpool_Handle;
      Pool    : in out Root_Storage_Pool_With_Subpools'Class)
   is
      N_Ptr : SP_Node_Ptr;

   begin
      --  If the subpool is already owned, raise Program_Error. This is a
      --  direct violation of the RM rules.

      if Subpool.Owner /= null then
         raise Program_Error with "subpool already belongs to a pool";
      end if;

      --  Prevent the creation of a new subpool while the owner is being
      --  finalized. This is a serious error.

      if Pool.Finalization_Started then
         raise Program_Error
           with "subpool creation after finalization started";
      end if;

      Subpool.Owner := Pool'Unchecked_Access;

      --  Create a subpool node and decorate it. Since this node is not
      --  allocated on the owner's pool, it must be explicitly destroyed by
      --  Finalize_And_Detach.

      N_Ptr := new SP_Node;
      N_Ptr.Subpool := Subpool;
      Subpool.Node := N_Ptr;

      Attach (N_Ptr, Pool.Subpools'Unchecked_Access);
   end Set_Pool_Of_Subpool;

end System.Storage_Pools.Subpools;
