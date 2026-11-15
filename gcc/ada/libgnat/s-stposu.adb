------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . S T O R A G E _ P O O L S . S U B P O O L S         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2026, Free Software Foundation, Inc.         --
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
with System.Secondary_Stack;  use System.Secondary_Stack;
with System.Soft_Links;       use System.Soft_Links;
with System.Storage_Elements; use System.Storage_Elements;

with System.Storage_Pools.Subpools.Finalization;
use  System.Storage_Pools.Subpools.Finalization;

package body System.Storage_Pools.Subpools is

   procedure Attach
     (Subpool : not null Subpool_Handle;
      Pool    : in out Root_Storage_Pool_With_Subpools'Class);
   --  Attach a subpool to a pool

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

         --  Ensure proper ownership

         if Subpool.Owner /=
              Root_Storage_Pool_With_Subpools'Class (Pool)'Unchecked_Access
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

   procedure Attach
     (Subpool : not null Subpool_Handle;
      Pool    : in out Root_Storage_Pool_With_Subpools'Class)
   is
   begin
      --  Ensure that the subpool has not been attached already

      pragma Assert (Subpool.Prev = null and then Subpool.Next = null);

      Lock_Task.all;

      if Pool.Subpools /= null then
         Pool.Subpools.Prev := Subpool;
         Subpool.Next := Pool.Subpools;
      end if;

      Pool.Subpools := Subpool;

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

   procedure Detach
     (Subpool : not null Subpool_Handle;
      Pool    : in out Root_Storage_Pool_With_Subpools'Class)
   is
   begin
      Lock_Task.all;

      if Subpool.Prev /= null then
         Subpool.Prev.Next := Subpool.Next;
      else
         Pool.Subpools := Subpool.Next;
      end if;

      if Subpool.Next /= null then
         Subpool.Next.Prev := Subpool.Prev;
      end if;

      Subpool.Prev := null;
      Subpool.Next := null;

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
      Ex_Occur : Exception_Occurrence;
      Raised   : Boolean := False;
      Subpool  : Subpool_Handle;

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

      while Pool.Subpools /= null loop
         --  Finalize and deallocate the subpool

         begin
            Subpool := Pool.Subpools;
            Finalize_And_Deallocate (Subpool);

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
   begin
      --  Output the contents of the pool

      --    Pool      : 0x123456789
      --    Subpools  : 0x123456789
      --    Fin_Start : TRUE <or> FALSE
      --    Controller: OK <or> ERROR

      Put ("Pool      : ");
      Put_Line (Address_Image (Pool'Address));

      Put ("Subpools  : ");
      Put_Line (Pool.Subpools'Img);

      Put ("Fin_Start : ");
      Put_Line (Pool.Finalization_Started'Img);

      Put ("Controller: ");
      if Pool.Controller.Enclosing_Pool = Pool'Unrestricted_Access then
         Put_Line ("OK");
      else
         Put_Line ("ERROR");
      end if;
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
      --    Prev      : 0x123456789
      --    Next      : 0x123456789

      Put ("Owner     : ");
      Put_Line (Subpool.Owner'Img);

      Put ("Collection: ");
      Put_Line (Address_Image (Subpool.Collection'Address));

      Put ("Prev      : ");
      Put_Line (Subpool.Prev'Img);

      Put ("Next      : ");
      Put_Line (Subpool.Next'Img);
   end Print_Subpool;

   -------------------------
   -- Set_Pool_Of_Subpool --
   -------------------------

   procedure Set_Pool_Of_Subpool
     (Subpool : not null Subpool_Handle;
      To      : in out Root_Storage_Pool_With_Subpools'Class)
   is
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

      Attach (Subpool, To);
   end Set_Pool_Of_Subpool;

   --------------------------------
   -- SS_Allocate_Any_Controlled --
   --------------------------------

   procedure SS_Allocate_Any_Controlled
     (Addr          : out Address;
      Storage_Size  : Storage_Count;
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
      if Is_Controlled then
         --  The size must account for the hidden header before the object.
         --  Account for possible padding space before the header due to a
         --  larger alignment of the object.

         Header_And_Padding := Header_Size_With_Padding (Alignment);

         N_Size := Storage_Size + Header_And_Padding;

         --  The alignment must account for the hidden header before the object

         N_Alignment :=
           System.Storage_Elements.Storage_Count'Max
             (Alignment, Finalization_Primitives.Header_Alignment);

      --  Non-controlled allocation

      else
         N_Size      := Storage_Size;
         N_Alignment := Alignment;
      end if;

      SS_Allocate (N_Addr, N_Size, N_Alignment);

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
   end SS_Allocate_Any_Controlled;

   -------------------
   -- _Adjust_Clone --
   -------------------

   procedure _Adjust_Clone (Subpool : in out Root_Subpool) is
   begin
      Finalization_Primitives.Initialize (Subpool.Collection);
   end _Adjust_Clone;

end System.Storage_Pools.Subpools;
