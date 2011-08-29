------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . F I N A L I Z A T I O N _ M A S T E R S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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

with Ada.Exceptions;          use Ada.Exceptions;

with System.Soft_Links;       use System.Soft_Links;
with System.Storage_Elements; use System.Storage_Elements;

package body System.Finalization_Masters is

   ---------------------------
   -- Add_Offset_To_Address --
   ---------------------------

   function Add_Offset_To_Address
     (Addr   : System.Address;
      Offset : System.Storage_Elements.Storage_Offset) return System.Address
   is
   begin
      return System.Storage_Elements."+" (Addr, Offset);
   end Add_Offset_To_Address;

   ------------
   -- Attach --
   ------------

   procedure Attach (N : not null FM_Node_Ptr; L : not null FM_Node_Ptr) is
   begin
      Lock_Task.all;

      L.Next.Prev := N;
      N.Next := L.Next;
      L.Next := N;
      N.Prev := L;

      Unlock_Task.all;

      --  Note: No need to unlock in case of an exception because the above
      --  code can never raise one.
   end Attach;

   ---------------
   -- Base_Pool --
   ---------------

   function Base_Pool
     (Master : Finalization_Master) return Any_Storage_Pool_Ptr
   is
   begin
      return Master.Base_Pool;
   end Base_Pool;

   ------------
   -- Detach --
   ------------

   procedure Detach (N : not null FM_Node_Ptr) is
   begin
      --  N must be attached to some list

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

   overriding procedure Finalize (Master : in out Finalization_Master) is
      Curr_Ptr : FM_Node_Ptr;
      Ex_Occur : Exception_Occurrence;
      Obj_Addr : Address;
      Raised   : Boolean := False;

   begin
      --  It is possible for multiple tasks to cause the finalization of the
      --  same master. Let only one task finalize the objects.

      if Master.Finalization_Started then
         return;
      end if;

      --  Lock the master to prevent any allocations while the objects are
      --  being finalized. The master remains locked because either the master
      --  is explicitly deallocated or the associated access type is about to
      --  go out of scope.

      Master.Finalization_Started := True;

      --  Skip the dummy head

      Curr_Ptr := Master.Objects.Next;
      while Curr_Ptr /= Master.Objects'Unchecked_Access loop

         --  If primitive Finalize_Address is not set, then the expansion of
         --  the designated type or that of the allocator failed. This is a
         --  serious error.

         if Master.Finalize_Address = null then
            raise Program_Error
              with "primitive Finalize_Address not available";
         end if;

         --  Skip the list header in order to offer proper object layout for
         --  finalization and call Finalize_Address.

         Obj_Addr := Curr_Ptr.all'Address + Header_Offset;

         begin
            Master.Finalize_Address (Obj_Addr);

         exception
            when Fin_Occur : others =>
               if not Raised then
                  Raised := True;
                  Save_Occurrence (Ex_Occur, Fin_Occur);
               end if;
         end;

         Curr_Ptr := Curr_Ptr.Next;
      end loop;

      --  If the finalization of a particular object failed or Finalize_Address
      --  was not set, reraise the exception now.

      if Raised then
         Reraise_Occurrence (Ex_Occur);
      end if;
   end Finalize;

   -----------------
   -- Header_Size --
   -----------------

   function Header_Size return System.Storage_Elements.Storage_Count is
   begin
      return FM_Node'Size / Storage_Unit;
   end Header_Size;

   -------------------
   -- Header_Offset --
   -------------------

   function Header_Offset return System.Storage_Elements.Storage_Offset is
   begin
      return FM_Node'Size / Storage_Unit;
   end Header_Offset;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Master : in out Finalization_Master) is
   begin
      --  The dummy head must point to itself in both directions

      Master.Objects.Next := Master.Objects'Unchecked_Access;
      Master.Objects.Prev := Master.Objects'Unchecked_Access;
   end Initialize;

   -------------------
   -- Set_Base_Pool --
   -------------------

   procedure Set_Base_Pool
     (Master   : in out Finalization_Master;
      Pool_Ptr : Any_Storage_Pool_Ptr)
   is
   begin
      Master.Base_Pool := Pool_Ptr;
   end Set_Base_Pool;

end System.Finalization_Masters;
