------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--    S Y S T E M . F I N A L I Z A T I O N _ I M P L E M E N T A T I O N   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

with System.Storage_Elements;
with System.Finalization_Root;

package System.Finalization_Implementation is
   pragma Elaborate_Body;

   package SSE renames System.Storage_Elements;
   package SFR renames System.Finalization_Root;

   ------------------------------------------------
   -- Finalization Management Abstract Interface --
   ------------------------------------------------

   function To_Finalizable_Ptr is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => SFR.Finalizable_Ptr);

   Collection_Finalization_Started : constant SFR.Finalizable_Ptr :=
                                       To_Finalizable_Ptr (SSE.To_Address (1));
   --  This is used to implement the rule in RM 4.8(10.2/2) that requires an
   --  allocator to raise Program_Error if the collection finalization has
   --  already started. See also Ada.Finalization.List_Controller. Finalize on
   --  List_Controller first sets the list to Collection_Finalization_Started,
   --  to indicate that finalization has started. An allocator will call
   --  Attach_To_Final_List, which checks for the special value and raises
   --  Program_Error if appropriate. The Collection_Finalization_Started value
   --  must be different from 'Access of any finalizable object, and different
   --  from null. See AI-280.

   Global_Final_List : SFR.Finalizable_Ptr;
   --  This list stores the controlled objects defined in library-level
   --  packages. They will be finalized after the main program completion.

   procedure Finalize_Global_List;
   --  The procedure to be called in order to finalize the global list

   procedure Attach_To_Final_List
     (L       : in out SFR.Finalizable_Ptr;
      Obj     : in out SFR.Finalizable;
      Nb_Link : Short_Short_Integer);
   --  Attach finalizable object Obj to the linked list L. Nb_Link controls the
   --  number of link of the linked_list, and is one of: 0 for no attachment, 1
   --  for simple linked lists or 2 for doubly linked lists or even 3 for a
   --  simple attachment of a whole array of elements. Attachment to a simply
   --  linked list is not protected against concurrent access and should only
   --  be used in contexts where it doesn't matter, such as for objects
   --  allocated on the stack. In the case of an attachment on a doubly linked
   --  list, L must not be null and Obj will be inserted AFTER the first
   --  element and the attachment is protected against concurrent call.
   --  Typically used to attach to a dynamically allocated object to a
   --  List_Controller (whose first element is always a dummy element)

   type Finalizable_Ptr_Ptr is access all SFR.Finalizable_Ptr;
   --  A pointer to a finalization list. This is used as the type of the extra
   --  implicit formal which are passed to build-in-place functions that return
   --  controlled types (see Sem_Ch6). That extra formal is then passed on to
   --  Move_Final_List (below).

   procedure Move_Final_List
     (From : in out SFR.Finalizable_Ptr;
      To   : Finalizable_Ptr_Ptr);
   --  Move all objects on From list to To list. This is used to implement
   --  build-in-place function returns. The return object is initially placed
   --  on a finalization list local to the return statement, in case the
   --  return statement is left prematurely (due to raising an exception,
   --  being aborted, or a goto or exit statement). Once the return statement
   --  has completed successfully, Move_Final_List is called to move the
   --  return object to the caller's finalization list.

   procedure Finalize_List (L : SFR.Finalizable_Ptr);
   --  Call Finalize on each element of the list L

   procedure Finalize_One (Obj  : in out SFR.Finalizable);
   --  Call Finalize on Obj and remove its final list

   ---------------------
   -- Deep Procedures --
   ---------------------

   procedure Deep_Tag_Attach
     (L : in out SFR.Finalizable_Ptr;
      A : System.Address;
      B : Short_Short_Integer);
   --  Generic attachment for tagged objects with controlled components.
   --  A is the address of the object, L the finalization list when it needs
   --  to be attached and B the attachment level (see Attach_To_Final_List).

   -----------------------------
   -- Record Controller Types --
   -----------------------------

   --  Definition of the types of the controller component that is included
   --  in records containing controlled components. This controller is
   --  attached to the finalization chain of the upper-level and carries
   --  the pointer of the finalization chain for the lower level.

   type Limited_Record_Controller is new SFR.Root_Controlled with record
      F : SFR.Finalizable_Ptr;
   end record;

   overriding procedure Initialize (Object : in out Limited_Record_Controller);
   --  Does nothing currently

   overriding procedure Finalize (Object : in out Limited_Record_Controller);
   --  Finalize the controlled components of the enclosing record by following
   --  the list starting at Object.F.

   type Record_Controller is
      new Limited_Record_Controller with record
         My_Address : System.Address;
      end record;

   overriding procedure Initialize (Object : in out Record_Controller);
   --  Initialize the field My_Address to the Object'Address

   overriding procedure Adjust (Object : in out Record_Controller);
   --  Adjust the components and their finalization pointers by subtracting by
   --  the offset of the target and the source addresses of the assignment.

   --  Inherit Finalize from Limited_Record_Controller

   procedure Detach_From_Final_List (Obj : in out SFR.Finalizable);
   --  Remove the specified object from its Final list, which must be a doubly
   --  linked list.

end System.Finalization_Implementation;
