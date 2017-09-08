------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--     A D A . C O N T A I N E R S . I N D E F I N I T E _ H O L D E R S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2013-2017, Free Software Foundation, Inc.         --
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

--  Note: special attention must be paid to the case of simultaneous access
--  to internal shared objects and elements by different tasks. The Reference
--  counter of internal shared object is the only component protected using
--  atomic operations; other components and elements can be modified only when
--  reference counter is equal to one (so there are no other references to this
--  internal shared object and element).

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Indefinite_Holders is

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   procedure Detach (Container : Holder);
   --  Detach data from shared copy if necessary. This is necessary to prepare
   --  container to be modified.

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Holder) return Boolean is
   begin
      if Left.Reference = Right.Reference then

         --  Covers both null and not null but the same shared object cases

         return True;

      elsif Left.Reference /= null and Right.Reference /= null then
         return Left.Reference.Element.all = Right.Reference.Element.all;

      else
         return False;
      end if;
   end "=";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Container : in out Holder) is
   begin
      if Container.Reference /= null then
         if Container.Busy = 0 then

            --  Container is not locked, reuse existing internal shared object

            Reference (Container.Reference);
         else
            --  Otherwise, create copy of both internal shared object and
            --  element.

            Container.Reference :=
               new Shared_Holder'
                 (Counter => <>,
                  Element =>
                     new Element_Type'(Container.Reference.Element.all));
         end if;
      end if;

      Container.Busy := 0;
   end Adjust;

   overriding procedure Adjust (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         Reference (Control.Container.Reference);
         Control.Container.Busy := Control.Container.Busy + 1;
      end if;
   end Adjust;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Holder; Source : Holder) is
   begin
      if Target.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      if Target.Reference /= Source.Reference then
         if Target.Reference /= null then
            Unreference (Target.Reference);
         end if;

         Target.Reference := Source.Reference;

         if Source.Reference /= null then
            Reference (Target.Reference);
         end if;
      end if;
   end Assign;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Holder) is
   begin
      if Container.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      if Container.Reference /= null then
         Unreference (Container.Reference);
         Container.Reference := null;
      end if;
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Holder) return Constant_Reference_Type is
   begin
      if Container.Reference = null then
         raise Constraint_Error with "container is empty";
      end if;

      Detach (Container);

      declare
         Ref : constant Constant_Reference_Type :=
                 (Element => Container.Reference.Element.all'Access,
                  Control => (Controlled with Container'Unrestricted_Access));
      begin
         Reference (Ref.Control.Container.Reference);
         Ref.Control.Container.Busy := Ref.Control.Container.Busy + 1;
         return Ref;
      end;
   end Constant_Reference;

   ----------
   -- Copy --
   ----------

   function Copy (Source : Holder) return Holder is
   begin
      if Source.Reference = null then
         return (Controlled with null, 0);

      elsif Source.Busy = 0 then

         --  Container is not locked, reuse internal shared object

         Reference (Source.Reference);

         return (Controlled with Source.Reference, 0);

      else
         --  Otherwise, create copy of both internal shared object and element

         return
           (Controlled with
              new Shared_Holder'
                (Counter => <>,
                 Element => new Element_Type'(Source.Reference.Element.all)),
               0);
      end if;
   end Copy;

   ------------
   -- Detach --
   ------------

   procedure Detach (Container : Holder) is
   begin
      if Container.Busy = 0
        and then not System.Atomic_Counters.Is_One
                       (Container.Reference.Counter)
      then
         --  Container is not locked and internal shared object is used by
         --  other container, create copy of both internal shared object and
         --  element.

         declare
            Old : constant Shared_Holder_Access := Container.Reference;

         begin
            Container'Unrestricted_Access.Reference :=
               new Shared_Holder'
                 (Counter => <>,
                  Element =>
                    new Element_Type'(Container.Reference.Element.all));
            Unreference (Old);
         end;
      end if;
   end Detach;

   -------------
   -- Element --
   -------------

   function Element (Container : Holder) return Element_Type is
   begin
      if Container.Reference = null then
         raise Constraint_Error with "container is empty";
      else
         return Container.Reference.Element.all;
      end if;
   end Element;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Container : in out Holder) is
   begin
      if Container.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      if Container.Reference /= null then
         Unreference (Container.Reference);
         Container.Reference := null;
      end if;
   end Finalize;

   overriding procedure Finalize (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         Unreference (Control.Container.Reference);
         Control.Container.Busy := Control.Container.Busy - 1;
         Control.Container := null;
      end if;
   end Finalize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Reference = null;
   end Is_Empty;

   ----------
   -- Move --
   ----------

   procedure Move (Target : in out Holder; Source : in out Holder) is
   begin
      if Target.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      if Source.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      if Target.Reference /= Source.Reference then
         if Target.Reference /= null then
            Unreference (Target.Reference);
         end if;

         Target.Reference := Source.Reference;
         Source.Reference := null;
      end if;
   end Move;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Container : Holder;
      Process   : not null access procedure (Element : Element_Type))
   is
      B : Natural renames Container'Unrestricted_Access.Busy;

   begin
      if Container.Reference = null then
         raise Constraint_Error with "container is empty";
      end if;

      Detach (Container);

      B := B + 1;

      begin
         Process (Container.Reference.Element.all);
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
      Container : out Holder)
   is
   begin
      Clear (Container);

      if not Boolean'Input (Stream) then
         Container.Reference :=
            new Shared_Holder'
              (Counter => <>,
               Element => new Element_Type'(Element_Type'Input (Stream)));
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

   procedure Reference (Item : not null Shared_Holder_Access) is
   begin
      System.Atomic_Counters.Increment (Item.Counter);
   end Reference;

   function Reference
     (Container : aliased in out Holder) return Reference_Type
   is
   begin
      if Container.Reference = null then
         raise Constraint_Error with "container is empty";
      end if;

      Detach (Container);

      declare
         Ref : constant Reference_Type :=
                 (Element => Container.Reference.Element.all'Access,
                  Control => (Controlled with Container'Unrestricted_Access));
      begin
         Reference (Ref.Control.Container.Reference);
         Ref.Control.Container.Busy := Ref.Control.Container.Busy + 1;
         return Ref;
      end;
   end Reference;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Holder;
      New_Item  : Element_Type)
   is
      --  Element allocator may need an accessibility check in case actual type
      --  is class-wide or has access discriminants (RM 4.8(10.1) and
      --  AI12-0035).

      pragma Unsuppress (Accessibility_Check);

   begin
      if Container.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      if Container.Reference = null then
         --  Holder is empty, allocate new Shared_Holder.

         Container.Reference :=
            new Shared_Holder'
              (Counter => <>,
               Element => new Element_Type'(New_Item));

      elsif System.Atomic_Counters.Is_One (Container.Reference.Counter) then
         --  Shared_Holder can be reused.

         Free (Container.Reference.Element);
         Container.Reference.Element := new Element_Type'(New_Item);

      else
         Unreference (Container.Reference);
         Container.Reference :=
            new Shared_Holder'
              (Counter => <>,
               Element => new Element_Type'(New_Item));
      end if;
   end Replace_Element;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (New_Item : Element_Type) return Holder is
      --  The element allocator may need an accessibility check in the case the
      --  actual type is class-wide or has access discriminants (RM 4.8(10.1)
      --  and AI12-0035).

      pragma Unsuppress (Accessibility_Check);

   begin
      return
        (Controlled with
            new Shared_Holder'
              (Counter => <>,
               Element => new Element_Type'(New_Item)), 0);
   end To_Holder;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Item : not null Shared_Holder_Access) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Shared_Holder, Shared_Holder_Access);

      Aux : Shared_Holder_Access := Item;

   begin
      if System.Atomic_Counters.Decrement (Aux.Counter) then
         Free (Aux.Element);
         Free (Aux);
      end if;
   end Unreference;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out Holder;
      Process   : not null access procedure (Element : in out Element_Type))
   is
      B : Natural renames Container.Busy;

   begin
      if Container.Reference = null then
         raise Constraint_Error with "container is empty";
      end if;

      Detach (Container);

      B := B + 1;

      begin
         Process (Container.Reference.Element.all);
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream    : not null access Ada.Streams.Root_Stream_Type'Class;
      Container : Holder)
   is
   begin
      Boolean'Output (Stream, Container.Reference = null);

      if Container.Reference /= null then
         Element_Type'Output (Stream, Container.Reference.Element.all);
      end if;
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

   procedure Write
     (Stream : not null access Root_Stream_Type'Class;
      Item   : Constant_Reference_Type)
   is
   begin
      raise Program_Error with "attempt to stream reference";
   end Write;

end Ada.Containers.Indefinite_Holders;
