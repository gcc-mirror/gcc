------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . S T O R A G E _ P O O L S . S U B P O O L S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2025, Free Software Foundation, Inc.         --
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

with Ada.Exceptions; use Ada.Exceptions;

with System.Address_Image;
with System.IO;               use System.IO;
with System.Soft_Links;       use System.Soft_Links;
with System.Storage_Elements; use System.Storage_Elements;

with System.Storage_Pools.Subpools.Finalization;
use  System.Storage_Pools.Subpools.Finalization;

package body System.Storage_Pools.Subpools is

   procedure Attach (N : not null SP_Node_Ptr; L : not null SP_Node_Ptr);
   --  Attach a subpool node to a pool

   -----------------------------------
   -- Adjust_Controlled_Dereference --
   -----------------------------------

   procedure Adjust_Controlled_Dereference
     (Addr         : in out System.Address;
      Storage_Size : in out System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count)
   is
      Header_And_Padding : constant Storage_Offset :=
                             Header_Size_With_Padding (Alignment);
   begin
      --  Expose the header and its padding by shifting the address from the
      --  start of the object to the beginning of the padding.

      Addr := Addr - Header_And_Padding;

      --  Update the size to include the header and its padding

      Storage_Size := Storage_Size + Header_And_Padding;
   end Adjust_Controlled_Dereference;

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
     (Pool          : in out Root_Storage_Pool'Class;
      Named_Subpool : Subpool_Handle;
      Collection    : in out
                        Finalization_Primitives.Finalization_Collection_Ptr;
      Addr          : out System.Address;
      Storage_Size  : System.Storage_Elements.Storage_Count;
      Alignment     : System.Storage_Elements.Storage_Count;
      Is_Controlled : Boolean;
      On_Subpool    : Boolean)
   is
      use type System.Finalization_Primitives.Finalization_Collection_Ptr;

      Is_Subpool_Allocation : constant Boolean :=
                                Pool in Root_Storage_Pool_With_Subpools'Class;

      N_Addr      : Address;
      N_Alignment : Storage_Count;
      N_Size      : Storage_Count;
      Subpool     : Subpool_Handle;

      Header_And_Padding : Storage_Offset;
      --  This offset includes the size of a header plus an additional padding
      --  due to a larger alignment of the object.

   begin
      --  Step 1: Pool-related runtime checks

      --  Allocation on a pool_with_subpools. In this scenario there is a
      --  collection for each subpool. That of the access type is ignored.

      if Is_Subpool_Allocation then

         --  Case of an allocation without a Subpool_Handle. Dispatch to the
         --  implementation of Default_Subpool_For_Pool.

         if Named_Subpool = null then
            Subpool :=
              Default_Subpool_For_Pool
                (Root_Storage_Pool_With_Subpools'Class (Pool));

         --  Allocation with a Subpool_Handle

         else
            Subpool := Named_Subpool;
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

         Collection := Subpool.Collection'Unchecked_Access;

      --  Allocation on a simple pool. In this scenario there is a collection
      --  for each access-to-controlled type. No context subpool is allowed.

      else
         --  If the collection is missing, then the expansion of the access
         --  type has failed to create one. This is a compiler bug.

         pragma Assert
           (Collection /= null, "no collection in pool allocation");

         --  If a subpool is present, then this is the result of erroneous
         --  allocator expansion. This is not a serious error, but it should
         --  still be detected.

         if Named_Subpool /= null then
            raise Program_Error
              with "subpool not required in pool allocation";
         end if;

         --  If the allocation is intended to be on a subpool, but the access
         --  type's pool does not support subpools, then this is the result of
         --  incorrect end-user code.

         if On_Subpool then
            raise Program_Error
              with "pool of access type does not support subpools";
         end if;
      end if;

      --  Step 2: Size and alignment calculations

      --  Allocation of a descendant from [Limited_]Controlled, a class-wide
      --  object or a record with controlled components.

      if Is_Controlled then
         --  The size must account for the hidden header before the object.
         --  Account for possible padding space before the header due to a
         --  larger alignment of the object.

         Header_And_Padding := Header_Size_With_Padding (Alignment);

         N_Size := Storage_Size + Header_And_Padding;

         --  The alignment must account for the hidden header before the object

         N_Alignment :=
           System.Storage_Elements.Storage_Count'Max
             (Alignment, System.Finalization_Primitives.Header_Alignment);

      --  Non-controlled allocation

      else
         N_Size      := Storage_Size;
         N_Alignment := Alignment;
      end if;

      --  Step 3: Allocation of object

      --  For descendants of Root_Storage_Pool_With_Subpools, dispatch to the
      --  implementation of Allocate_From_Subpool.

      if Is_Subpool_Allocation then
         Allocate_From_Subpool
           (Root_Storage_Pool_With_Subpools'Class (Pool),
            N_Addr, N_Size, N_Alignment, Subpool);

      --  For descendants of Root_Storage_Pool, dispatch to the implementation
      --  of Allocate.

      else
         Allocate (Pool, N_Addr, N_Size, N_Alignment);
      end if;

      --  Step 4: Displacement of address

      if Is_Controlled then
         --  Move the address from the hidden list header to the start of the
         --  object. If there is padding due to larger alignment of the object,
         --  the padding is placed at the beginning. This effectively hides the
         --  list header:

         --    N_Addr                  Addr
         --    |                       |
         --    V                       V
         --    +-------+---------------+----------------------+
         --    |Padding|    Header     |        Object        |
         --    +-------+---------------+----------------------+
         --    ^       ^               ^
         --    |       +- Header_Size -+
         --    |                       |
         --    +- Header_And_Padding --+

         Addr := N_Addr + Header_And_Padding;

      --  Non-controlled allocation

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
      N_Addr      : Address;
      N_Alignment : Storage_Count;
      N_Size      : Storage_Count;

      Header_And_Padding : Storage_Offset;
      --  This offset includes the size of a header plus an additional padding
      --  due to a larger alignment of the object.

   begin
      --  Step 1: Displacement of address

      if Is_Controlled then
         --  Account for possible padding space before the header due to a
         --  larger alignment.

         Header_And_Padding := Header_Size_With_Padding (Alignment);

         --    N_Addr                  Addr
         --    |                       |
         --    V                       V
         --    +-------+---------------+----------------------+
         --    |Padding|    Header     |        Object        |
         --    +-------+---------------+----------------------+
         --    ^       ^               ^
         --    |       +- Header_Size -+
         --    |                       |
         --    +- Header_And_Padding --+

         --  Move the address from the object to the beginning of the header

         N_Addr := Addr - Header_And_Padding;

         --  The size of the deallocated object must include that of the header

         N_Size := Storage_Size + Header_And_Padding;

         --  The alignment must account for the hidden header before the object

         N_Alignment :=
           System.Storage_Elements.Storage_Count'Max
             (Alignment, System.Finalization_Primitives.Header_Alignment);

      else
         N_Addr      := Addr;
         N_Size      := Storage_Size;
         N_Alignment := Alignment;
      end if;

      --  Step 2: Deallocation of object

      --  Dispatch to the proper implementation of Deallocate. This action
      --  covers both Root_Storage_Pool and Root_Storage_Pool_With_Subpools
      --  implementations.

      Deallocate (Pool, N_Addr, N_Size, N_Alignment);
   end Deallocate_Any_Controlled;

   ------------------------------
   -- Default_Subpool_For_Pool --
   ------------------------------

   function Default_Subpool_For_Pool
     (Pool : in out Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle
   is
      pragma Unreferenced (Pool);
   begin
      return raise Program_Error with
        "default Default_Subpool_For_Pool called; must be overridden";
   end Default_Subpool_For_Pool;

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
      N.Prev := null;
      N.Next := null;

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
      Raised   : Boolean := False;

      function Is_Empty_List (L : not null SP_Node_Ptr) return Boolean;
      --  Determine whether a list contains only one element, the dummy head

      -------------------
      -- Is_Empty_List --
      -------------------

      function Is_Empty_List (L : not null SP_Node_Ptr) return Boolean is
      begin
         return L.Next = L and then L.Prev = L;
      end Is_Empty_List;

   --  Start of processing for Finalize_Pool

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

      while not Is_Empty_List (Pool.Subpools'Unchecked_Access) loop
         Curr_Ptr := Pool.Subpools.Next;

         --  Perform the following actions:

         --    1) Finalize all objects chained on the subpool's collection
         --    2) Remove the subpool from the owner's list of subpools
         --    3) Deallocate the doubly linked list node associated with the
         --       subpool.
         --    4) Call Deallocate_Subpool

         begin
            Finalize_And_Deallocate (Curr_Ptr.Subpool);

         exception
            when Fin_Occur : others =>
               if not Raised then
                  Raised := True;
                  Save_Occurrence (Ex_Occur, Fin_Occur);
               end if;
         end;
      end loop;

      --  If the finalization of a particular collection failed, reraise the
      --  exception now.

      if Raised then
         Reraise_Occurrence (Ex_Occur);
      end if;
   end Finalize_Pool;

   ------------------------------
   -- Header_Size_With_Padding --
   ------------------------------

   function Header_Size_With_Padding
     (Alignment : System.Storage_Elements.Storage_Count)
      return System.Storage_Elements.Storage_Count
   is
      Size : constant Storage_Count :=
               System.Finalization_Primitives.Header_Size;

   begin
      if Size mod Alignment = 0 then
         return Size;

      --  Add enough padding to reach the nearest multiple of the alignment
      --  rounding up.

      else
         return ((Size + Alignment - 1) / Alignment) * Alignment;
      end if;
   end Header_Size_With_Padding;

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

   ---------------------
   -- Pool_Of_Subpool --
   ---------------------

   function Pool_Of_Subpool
     (Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class
   is
   begin
      return Subpool.Owner;
   end Pool_Of_Subpool;

   ----------------
   -- Print_Pool --
   ----------------

   procedure Print_Pool (Pool : Root_Storage_Pool_With_Subpools) is
      Head      : constant SP_Node_Ptr := Pool.Subpools'Unrestricted_Access;
      Head_Seen : Boolean := False;
      SP_Ptr    : SP_Node_Ptr;

   begin
      --  Output the contents of the pool

      --    Pool      : 0x123456789
      --    Subpools  : 0x123456789
      --    Fin_Start : TRUE <or> FALSE
      --    Controller: OK <or> NOK

      Put ("Pool      : ");
      Put_Line (Address_Image (Pool'Address));

      Put ("Subpools  : ");
      Put_Line (Address_Image (Pool.Subpools'Address));

      Put ("Fin_Start : ");
      Put_Line (Pool.Finalization_Started'Img);

      Put ("Controlled: ");
      if Pool.Controller.Enclosing_Pool = Pool'Unrestricted_Access then
         Put_Line ("OK");
      else
         Put_Line ("NOK (ERROR)");
      end if;

      SP_Ptr := Head;
      while SP_Ptr /= null loop  --  Should never be null
         Put_Line ("V");

         --  We see the head initially; we want to exit when we see the head a
         --  second time.

         if SP_Ptr = Head then
            exit when Head_Seen;

            Head_Seen := True;
         end if;

         --  The current element is null. This should never happend since the
         --  list is circular.

         if SP_Ptr.Prev = null then
            Put_Line ("null (ERROR)");

         --  The current element points back to the correct element

         elsif SP_Ptr.Prev.Next = SP_Ptr then
            Put_Line ("^");

         --  The current element points to an erroneous element

         else
            Put_Line ("? (ERROR)");
         end if;

         --  Output the contents of the node

         Put ("|Header: ");
         Put (Address_Image (SP_Ptr.all'Address));
         if SP_Ptr = Head then
            Put_Line (" (dummy head)");
         else
            Put_Line ("");
         end if;

         Put ("|  Prev: ");

         if SP_Ptr.Prev = null then
            Put_Line ("null");
         else
            Put_Line (Address_Image (SP_Ptr.Prev.all'Address));
         end if;

         Put ("|  Next: ");

         if SP_Ptr.Next = null then
            Put_Line ("null");
         else
            Put_Line (Address_Image (SP_Ptr.Next.all'Address));
         end if;

         Put ("|  Subp: ");

         if SP_Ptr.Subpool = null then
            Put_Line ("null");
         else
            Put_Line (Address_Image (SP_Ptr.Subpool.all'Address));
         end if;

         SP_Ptr := SP_Ptr.Next;
      end loop;
   end Print_Pool;

   -------------------
   -- Print_Subpool --
   -------------------

   procedure Print_Subpool (Subpool : Subpool_Handle) is
   begin
      if Subpool = null then
         Put_Line ("null");
         return;
      end if;

      --  Output the contents of a subpool

      --    Owner     : 0x123456789
      --    Collection: 0x123456789
      --    Node      : 0x123456789

      Put ("Owner : ");
      if Subpool.Owner = null then
         Put_Line ("null");
      else
         Put_Line (Address_Image (Subpool.Owner'Address));
      end if;

      Put ("Collection: ");
      Put_Line (Address_Image (Subpool.Collection'Address));

      Put ("Node  : ");
      if Subpool.Node = null then
         Put ("null");

         if Subpool.Owner = null then
            Put_Line (" OK");
         else
            Put_Line (" (ERROR)");
         end if;
      else
         Put_Line (Address_Image (Subpool.Node'Address));
      end if;
   end Print_Subpool;

   -------------------------
   -- Set_Pool_Of_Subpool --
   -------------------------

   procedure Set_Pool_Of_Subpool
     (Subpool : not null Subpool_Handle;
      To      : in out Root_Storage_Pool_With_Subpools'Class)
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

      if To.Finalization_Started then
         raise Program_Error
           with "subpool creation after finalization started";
      end if;

      Subpool.Owner := To'Unchecked_Access;

      --  Create a subpool node and decorate it. Since this node is not
      --  allocated on the owner's pool, it must be explicitly destroyed by
      --  Finalize_And_Detach.

      N_Ptr := new SP_Node;
      N_Ptr.Subpool := Subpool;
      Subpool.Node := N_Ptr;

      Attach (N_Ptr, To.Subpools'Unchecked_Access);
   end Set_Pool_Of_Subpool;

end System.Storage_Pools.Subpools;
