------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                ADA.CONTAINERS.BOUNDED_INDEFINITE_HOLDERS                 --
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
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocate_Subpool;
with Ada.Unchecked_Deallocation;
with System.Put_Images;

package body Ada.Containers.Bounded_Indefinite_Holders is

   use type System.Address;

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);

   function "=" (Left, Right : Holder) return Boolean is
     (if Left.Element = null or Right.Element = null
       then Left.Element = Right.Element
       else Left.Element.all = Right.Element.all);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Container : in out Holder) is
   begin
      Container.Handle := Create_Subpool (The_Storage_Pool, Container);
      Container.Element :=
        new (Container.Handle) Element_Type'(Container.Element.all);
   end Adjust;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Holder; Source : Holder) is
   begin
      if Target'Address /= Source'Address then
         if Is_Empty (Source) then
            Clear (Target);
         else
            Replace_Element (Target, Source.Element.all);
         end if;
      end if;
   end Assign;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Holder) is
   begin
      if Is_Empty (Container) then
         return; -- nothing to do
      end if;
      if Container.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;
      Free (Container.Element); -- finalize element
      Ada.Unchecked_Deallocate_Subpool (Container.Handle);
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Holder) return Constant_Reference_Type
   is
      Ref : constant Constant_Reference_Type :=
        (Element => Container.Element,
         Control => (Controlled with Container'Unrestricted_Access));
      B : Natural renames Ref.Control.Container.Busy;
   begin
      B := B + 1;
      return Ref;
   end Constant_Reference;

   ----------
   -- Copy --
   ----------

   function Copy (Source : Holder) return Holder is
      (if Is_Empty (Source)
       then Empty_Holder
       else To_Holder (Source.Element.all));

   -------------
   -- Element --
   -------------

   function Element (Container : Holder) return Element_Type is
     (Container.Element.all);

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Container : in out Holder) is
   begin
      Clear (Container);
   end Finalize;

   --  NOTE: No procedure Initialize because requires preelaborable init.
   overriding procedure Finalize (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            B : Natural renames Control.Container.Busy;
         begin
            B := B - 1;
         end;
      end if;

      Control.Container := null;
   end Finalize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Holder) return Boolean
     is (Container.Element = null);

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Holder; Source : in out Holder) is
   begin
      if Target'Address /= Source'Address then
         Assign (Target => Target, Source => Source);
         Clear (Source);
      end if;
   end Move;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image
     (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; V : Holder)
   is
      use System.Put_Images;
   begin
      Array_Before (S);
      if not Is_Empty (V) then
         Element_Type'Put_Image (S, Element (V));
      end if;
      Array_After (S);
   end Put_Image;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Container : Holder;
      Process   : not null access procedure (Element : Element_Type)) is
   begin
      Process.all (Container.Element.all);
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : not null access Root_Stream_Type'Class;
      Container : out Holder)
   is
   begin
      if Boolean'Input (Stream) then
         Clear (Container);
      else
         Replace_Element (Container, Element_Type'Input (Stream));
      end if;
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Read;

   procedure Read
     (Stream : not null access Root_Stream_Type'Class;
      Item   : out Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Read;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Container : aliased in out Holder) return Reference_Type
   is
      Ref : constant Reference_Type :=
              (Element => Container.Element,
               Control => (Controlled with Container'Unrestricted_Access));
   begin
      Container.Busy := Container.Busy + 1;
      return Ref;
   end Reference;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Holder; New_Item : Element_Type)
   is
   begin
      if (New_Item'Size / System.Storage_Unit) +
        Boolean'Pos (New_Item'Size mod System.Storage_Unit /= 0) >
        Max_Element_Size_In_Storage_Elements
      then
         --  New_Item is too big; detect this early, before calling Clear
         --  (as opposed to catching it later in Allocate_From_Subpool).
         raise Program_Error;
      end if;
      Clear (Container);
      if Container.Handle = null then
         Container.Handle := Create_Subpool (The_Storage_Pool, Container);
      end if;
      Container.Element := new (Container.Handle) Element_Type'(New_Item);
   end Replace_Element;

   ---------------------
   -- Subpool_Support --
   ---------------------

   package body Subpool_Support is
      function Create_Subpool (Pool : in out Holder_Pool_Type)
        return not null Subpool_Handle is
      begin
         return (raise Program_Error);
      end Create_Subpool;

      function Aligned_Address
        (Addr : System.Address; Alignment : Storage_Count)
        return System.Address;
      --  Return Addr, rounded up to multiple of Alignment

      function Aligned_Address
        (Addr : System.Address; Alignment : Storage_Count)
        return System.Address
      is
         Initial_Align : constant Storage_Count := Addr mod Alignment;
      begin
         if Initial_Align = 0 then
            --  Already aligned
            return Addr;
         else
            --  Adjust to get into alignment
            return Addr + (Alignment - Initial_Align);
         end if;
      end Aligned_Address;

      function Create_Subpool
        (Pool : in out Holder_Pool_Type'Class; Container : Holder)
        return not null Subpool_Handle
      is
         --  Compute start addresses for subpool and element within Storage
         Subpool_Start : constant System.Address :=
           Aligned_Address
             (Container.Wrapper.Storage'Address, Holder_Subpool'Alignment);

         Element_Start : constant System.Address :=
           Subpool_Start + Holder_Subpool'Max_Size_In_Storage_Elements;
            --  Will deal with alignment on allocation

         Subpool : aliased Holder_Subpool :=
           (Root_Subpool with Start => Element_Start)
           with Address => Subpool_Start;
          --  We depend here on the type Holder_Subpool not having nontrivial
          --  finalization (if it did then this local object would be
          --  finalized earlier than what we want).
      begin
         Set_Pool_Of_Subpool (Subpool'Unchecked_Access, Pool);
         --  Return the handle
         return Subpool'Unchecked_Access;
      end Create_Subpool;

      procedure Allocate_From_Subpool
        (Pool : in out Holder_Pool_Type;
         Storage_Address : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment : Storage_Count;
         Subpool : not null Subpool_Handle) is
      begin
         if Size_In_Storage_Elements + Alignment >
            Max_Element_Size_In_Storage_Elements + Element_Type'Alignment
         then
            --  If we pass the size check in Replace_Element (which we had to
            --  in order to get here) and then fail this check, then that is
            --  a bug (although arguably a corner case).
            --  If we get here, that probably means that the result returned
            --  by Max_Allocation_Overhead_In_Storage_Elements was too small
            --  (with the result that Bound_Range'Last is too small).
            raise Program_Error;
         end if;
         Storage_Address :=
           Aligned_Address (Holder_Subpool (Subpool.all).Start, Alignment);
      end Allocate_From_Subpool;

      procedure Deallocate_Subpool
        (Pool : in out Holder_Pool_Type;
         Subpool : in out Subpool_Handle) is
      begin
         --  Nothing to do
         null;
      end Deallocate_Subpool;

   end Subpool_Support;

   ----------
   -- Swap --
   ----------

   procedure Swap (Left, Right : in out Holder) is
      Temp : Holder;
   begin
      Assign (Target => Temp, Source => Left);
      Move (Target => Left, Source => Right);
      Move (Target => Right, Source => Temp);
   end Swap;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return Result : Holder do
         Replace_Element (Result, New_Item);
      end return;
   end To_Holder;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out Holder;
      Process   : not null access procedure (Element : in out Element_Type)) is
   begin
      Process.all (Container.Element.all);
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

   procedure Write
     (Stream    : not null access Root_Stream_Type'Class;
      Container : Holder)
   is
   begin
      --  Polarity of this Boolean determined by streaming-related
      --  implementation requirements of RM A.18.32.

      Boolean'Write (Stream, Container.Element = null);
      if Container.Element /= null then
         Element_Type'Write (Stream, Container.Element.all);
      end if;
   end Write;

end Ada.Containers.Bounded_Indefinite_Holders;
