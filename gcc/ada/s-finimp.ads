------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--    S Y S T E M . F I N A L I Z A T I O N _ I M P L E M E N T A T I O N   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2.10.1 $
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Finalization_Root;

package System.Finalization_Implementation is
pragma Elaborate_Body (Finalization_Implementation);

   package SFR  renames System.Finalization_Root;

   ------------------------------------------------
   -- Finalization Management Abstract Interface --
   ------------------------------------------------

   Global_Final_List : SFR.Finalizable_Ptr;
   --  This list stores the controlled objects defined in library-level
   --  packages. They will be finalized after the main program completion.

   procedure Finalize_Global_List;
   --  The procedure to be called in order to finalize the global list;

   procedure Attach_To_Final_List
     (L       : in out SFR.Finalizable_Ptr;
      Obj     : in out SFR.Finalizable;
      Nb_Link : Short_Short_Integer);
   --  Attach finalizable object Obj to the linked list L. Nb_Link controls
   --  the number of link of the linked_list, and can be either 0 for no
   --  attachement, 1 for simple linked lists or 2 for doubly linked lists
   --  or even 3 for a simple attachement of a whole array of elements.
   --  Attachement to a simply linked list is not protected against
   --  concurrent access and should only be used in context where it
   --  doesn't matter, such as for objects allocated on the stack. In the
   --  case of an attachment on a doubly linked list, L must not be null
   --  and Obj will be inserted AFTER the first element and the attachment
   --  is protected against concurrent call. Typically used to attach to
   --  a dynamically allocated object to a List_Controller (whose first
   --  element is always a dummy element)

   procedure Finalize_List (L : SFR.Finalizable_Ptr);
   --  Call Finalize on each element of the list L;

   procedure Finalize_One (Obj  : in out SFR.Finalizable);
   --  Call Finalize on Obj and remove its final list.

   ---------------------
   -- Deep Procedures --
   ---------------------

   procedure Deep_Tag_Initialize
     (L : in out SFR.Finalizable_Ptr;
      A : System.Address;
      B : Short_Short_Integer);
   --  Generic initialize for tagged objects with controlled components. A
   --  is the address of the object, L the finalization list when it needs
   --  to be attached and B the attachement level (see Attach_To_Final_List)

   procedure Deep_Tag_Adjust
     (L : in out SFR.Finalizable_Ptr;
      A : System.Address;
      B : Short_Short_Integer);
   --  Generic adjust for tagged objects with controlled components. A
   --  is the address of the object, L the finalization list when it needs
   --  to be attached and B the attachement level (see Attach_To_Final_List)

   procedure Deep_Tag_Finalize
     (L : in out SFR.Finalizable_Ptr;
      A : System.Address;
      B : Boolean);
   --  Generic finalize for tagged objects with controlled components. A
   --  is the address of the object, L the finalization list when it needs
   --  to be attached and B the attachement level (see Attach_To_Final_List)

   procedure Deep_Tag_Attach
     (L : in out SFR.Finalizable_Ptr;
      A : System.Address;
      B : Short_Short_Integer);
   --  Generic attachement for tagged objects with controlled components. A
   --  is the address of the object, L the finalization list when it needs
   --  to be attached and B the attachement level (see Attach_To_Final_List)

   -----------------------------
   -- Record Controller Types --
   -----------------------------

   --  Definition of the types of the controller component that is included
   --  in records containing controlled components. This controller is
   --  attached to the finalization chain of the upper-level and carries
   --  the pointer of the finalization chain for the lower level

   type Limited_Record_Controller is new SFR.Root_Controlled with record
      F : SFR.Finalizable_Ptr;
   end record;

   procedure Initialize (Object : in out Limited_Record_Controller);
   --  Does nothing

   procedure Finalize (Object : in out Limited_Record_Controller);
   --  Finalize the controlled components of the enclosing record by
   --  following the list starting at Object.F

   type Record_Controller is
      new Limited_Record_Controller with record
         My_Address : System.Address;
      end record;

   procedure Initialize (Object : in out Record_Controller);
   --  Initialize the field My_Address to the Object'Address

   procedure Adjust (Object : in out Record_Controller);
   --  Adjust the components and their finalization pointers by subtracting
   --  by the offset of the target and the source addresses of the assignment

   --  Inherit Finalize from Limited_Record_Controller

   procedure Detach_From_Final_List (Obj : in out SFR.Finalizable);
   --  Remove the specified object from its Final list which must be a
   --  doubly linked list.

end System.Finalization_Implementation;
