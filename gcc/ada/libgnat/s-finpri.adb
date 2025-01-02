------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . F I N A L I Z A T I O N _ P R I M I T I V E S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2023-2025, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with System.Soft_Links; use System.Soft_Links;

package body System.Finalization_Primitives is

   procedure Raise_From_Controlled_Operation (X : Exception_Occurrence);
   pragma Import (Ada, Raise_From_Controlled_Operation,
                              "__gnat_raise_from_controlled_operation");

   function To_Collection_Node_Ptr is
     new Ada.Unchecked_Conversion (Address, Collection_Node_Ptr);

   procedure Detach_Node_From_Collection (Node : not null Collection_Node_Ptr);
   --  Remove a collection node from its associated finalization collection.
   --  Calls to the procedure with a Node that has already been detached have
   --  no effects.

   procedure Lock_Collection (Collection : in out Finalization_Collection);
   --  Lock the finalization collection. Upon return, the caller owns the lock
   --  to the collection and no other call with the same actual parameter will
   --  return until a corresponding call to Unlock_Collection has been made by
   --  the caller. This means that it is not possible to call Lock_Collection
   --  more than once on a collection without a call to Unlock_Collection in
   --  between.

   procedure Unlock_Collection (Collection : in out Finalization_Collection);
   --  Unlock the finalization collection, i.e. relinquish ownership of the
   --  lock to the collection.

   ---------------------------------
   -- Attach_Object_To_Collection --
   ---------------------------------

   procedure Attach_Object_To_Collection
     (Object_Address   : System.Address;
      Finalize_Address : not null Finalize_Address_Ptr;
      Collection       : in out Finalization_Collection)
   is
      Node : constant Collection_Node_Ptr :=
               To_Collection_Node_Ptr (Object_Address - Header_Size);

   begin
      Lock_Collection (Collection);

      --  Do not allow the attachment of controlled objects while the
      --  associated collection is being finalized.

      --  Synchronization:
      --    Read  - attachment, finalization
      --    Write - finalization

      if Collection.Finalization_Started then
         raise Program_Error with "attachment after finalization started";
      end if;

      --  Check whether primitive Finalize_Address is available. If it is
      --  not, then either the expansion of the designated type failed or
      --  the expansion of the allocator failed. This is a compiler bug.

      pragma Assert
        (Finalize_Address /= null, "primitive Finalize_Address not available");

      Node.Enclosing_Collection := Collection'Unrestricted_Access;
      Node.Finalize_Address     := Finalize_Address;
      Node.Prev                 := Collection.Head'Unchecked_Access;
      Node.Next                 := Collection.Head.Next;

      Collection.Head.Next.Prev := Node;
      Collection.Head.Next      := Node;

      Unlock_Collection (Collection);

   exception
      when others =>

         --  Unlock the collection in case the attachment failed and reraise
         --  the exception.

         Unlock_Collection (Collection);
         raise;
   end Attach_Object_To_Collection;

   -----------------------------
   -- Attach_Object_To_Master --
   -----------------------------

   procedure Attach_Object_To_Master
     (Object_Address   : System.Address;
      Finalize_Address : not null Finalize_Address_Ptr;
      Node             : not null Master_Node_Ptr;
      Master           : in out Finalization_Master)
   is
   begin
      Attach_Object_To_Node (Object_Address, Finalize_Address, Node.all);
      Chain_Node_To_Master (Node, Master);
   end Attach_Object_To_Master;

   ---------------------------
   -- Attach_Object_To_Node --
   ---------------------------

   procedure Attach_Object_To_Node
     (Object_Address   : System.Address;
      Finalize_Address : not null Finalize_Address_Ptr;
      Node             : in out Master_Node)
   is
   begin
      pragma Assert (Node.Object_Address = Null_Address
        and then Node.Finalize_Address = null);

      Node.Object_Address   := Object_Address;
      Node.Finalize_Address := Finalize_Address;
   end Attach_Object_To_Node;

   --------------------------
   -- Chain_Node_To_Master --
   --------------------------

   procedure Chain_Node_To_Master
     (Node   : not null Master_Node_Ptr;
      Master : in out Finalization_Master)
   is
   begin
      Node.Next   := Master.Head;
      Master.Head := Node;
   end Chain_Node_To_Master;

   ---------------------------------
   -- Detach_Node_From_Collection --
   ---------------------------------

   procedure Detach_Node_From_Collection
     (Node : not null Collection_Node_Ptr)
   is
   begin
      if Node.Prev /= null and then Node.Next /= null then
         Node.Prev.Next := Node.Next;
         Node.Next.Prev := Node.Prev;
         Node.Prev := null;
         Node.Next := null;
      end if;
   end Detach_Node_From_Collection;

   -----------------------------------
   -- Detach_Object_From_Collection --
   -----------------------------------

   procedure Detach_Object_From_Collection
     (Object_Address : System.Address)
   is
      Node : constant Collection_Node_Ptr :=
               To_Collection_Node_Ptr (Object_Address - Header_Size);

   begin
      Lock_Collection (Node.Enclosing_Collection.all);

      Detach_Node_From_Collection (Node);

      Unlock_Collection (Node.Enclosing_Collection.all);
   end Detach_Object_From_Collection;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Collection : in out Finalization_Collection) is
      Curr_Ptr                      : Collection_Node_Ptr;
      Exc_Occur                     : Exception_Occurrence;
      Finalization_Exception_Raised : Boolean := False;
      Obj_Addr                      : Address;

      function Is_Empty_List (L : not null Collection_Node_Ptr) return Boolean;
      --  Determine whether a list contains only one element, the dummy head

      -------------------
      -- Is_Empty_List --
      -------------------

      function Is_Empty_List (L : not null Collection_Node_Ptr) return Boolean
      is
      begin
         return L.Next = L and then L.Prev = L;
      end Is_Empty_List;

   begin
      Lock_Collection (Collection);

      --  Synchronization:
      --    Read  - attachment, finalization
      --    Write - finalization

      if Collection.Finalization_Started then
         Unlock_Collection (Collection);

         --  Double finalization may occur during the handling of stand-alone
         --  libraries or the finalization of a pool with subpools.

         return;
      end if;

      --  Lock the collection to prevent any attachment while the objects are
      --  being finalized. The collection remains locked because either it is
      --  explicitly deallocated or the associated access type is about to go
      --  out of scope.

      --  Synchronization:
      --    Read  - attachment, finalization
      --    Write - finalization

      Collection.Finalization_Started := True;

      --  Note that we cannot walk the list while finalizing its elements
      --  because the finalization of one may call Unchecked_Deallocation
      --  on another and, therefore, detach it from anywhere on the list.
      --  Instead, we empty the list by repeatedly finalizing the first
      --  element (after the dummy head) and detaching it from the list.

      while not Is_Empty_List (Collection.Head'Unchecked_Access) loop
         Curr_Ptr := Collection.Head.Next;

         --  Synchronization:
         --    Write - attachment, detachment, finalization

         Detach_Node_From_Collection (Curr_Ptr);

         --  Skip the list header in order to offer proper object layout for
         --  finalization.

         Obj_Addr := Curr_Ptr.all'Address + Header_Size;

         --  Temporarily release the lock because the call to Finalize_Address
         --  may ultimately invoke Detach_Object_From_Collection.

         Unlock_Collection (Collection);

         begin
            Curr_Ptr.Finalize_Address (Obj_Addr);
         exception
            when Fin_Occur : others =>
               if not Finalization_Exception_Raised then
                  Finalization_Exception_Raised := True;
                  Save_Occurrence (Exc_Occur, Fin_Occur);
               end if;
         end;

         --  Retake the lock for the next iteration

         Lock_Collection (Collection);
      end loop;

      Unlock_Collection (Collection);

      --  If one of the finalization actions raised an exception, reraise it

      if Finalization_Exception_Raised then
         Raise_From_Controlled_Operation (Exc_Occur);
      end if;
   end Finalize;

   ---------------------
   -- Finalize_Master --
   ---------------------

   procedure Finalize_Master (Master : in out Finalization_Master) is
      Exc_Occur                     : Exception_Occurrence;
      Finalization_Exception_Raised : Boolean := False;
      Node                          : Master_Node_Ptr;

   begin
      Node := Master.Head;

      --  If exceptions are enabled, we catch them locally and reraise one
      --  once all the finalization actions have been completed.

      if Master.Exceptions_OK then
         while Node /= null loop
            begin
               Finalize_Object (Node.all, Node.Finalize_Address);

            exception
               when Exc : others =>
                  if not Finalization_Exception_Raised then
                     Finalization_Exception_Raised := True;

                     if Master.Library_Level then
                        if Master.Extra_Info then
                           Save_Library_Occurrence (Exc'Unrestricted_Access);
                        else
                           Save_Library_Occurrence (null);
                        end if;

                     elsif Master.Extra_Info then
                        Save_Occurrence (Exc_Occur, Exc);
                     end if;
                  end if;
            end;

            Node := Node.Next;
         end loop;

      --  Otherwise we call finalization procedures without protection

      else
         while Node /= null loop
            Finalize_Object (Node.all, Node.Finalize_Address);

            Node := Node.Next;
         end loop;
      end if;

      Master.Head := null;

      --  If one of the finalization actions raised an exception, and we are
      --  not at library level, then reraise the exception.

      if Finalization_Exception_Raised and then not Master.Library_Level then
         if Master.Extra_Info then
            Raise_From_Controlled_Operation (Exc_Occur);
         else
            raise Program_Error with "finalize/adjust raised exception";
         end if;
      end if;
   end Finalize_Master;

   ---------------------
   -- Finalize_Object --
   ---------------------

   procedure Finalize_Object
     (Node             : in out Master_Node;
      Finalize_Address : Finalize_Address_Ptr)
   is
      Addr : constant System.Address := Node.Object_Address;

   begin
      if Addr /= Null_Address then
         Node.Object_Address := Null_Address;

         pragma Assert (Node.Finalize_Address = Finalize_Address);
         Finalize_Address (Addr);
      end if;
   end Finalize_Object;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Collection : in out Finalization_Collection) is
   begin
      --  The dummy head must point to itself in both directions

      Collection.Head.Prev := Collection.Head'Unchecked_Access;
      Collection.Head.Next := Collection.Head'Unchecked_Access;

      Initialize_RTS_Lock (Collection.Lock'Address);

      Collection.Finalization_Started := False;
   end Initialize;

   ---------------------
   -- Lock_Collection --
   ---------------------

   procedure Lock_Collection (Collection : in out Finalization_Collection) is
   begin
      Acquire_RTS_Lock (Collection.Lock'Address);
   end Lock_Collection;

   -------------------------------------
   -- Suppress_Object_Finalize_At_End --
   -------------------------------------

   procedure Suppress_Object_Finalize_At_End (Node : in out Master_Node) is
   begin
      Node.Object_Address := Null_Address;
   end Suppress_Object_Finalize_At_End;

   -----------------------
   -- Unlock_Collection --
   -----------------------

   procedure Unlock_Collection (Collection : in out Finalization_Collection) is
   begin
      Release_RTS_Lock (Collection.Lock'Address);
   end Unlock_Collection;

end System.Finalization_Primitives;
