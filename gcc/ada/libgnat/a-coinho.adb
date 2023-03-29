------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--     A D A . C O N T A I N E R S . I N D E F I N I T E _ H O L D E R S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2012-2023, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Deallocation;
with System.Put_Images;

package body Ada.Containers.Indefinite_Holders is

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Holder) return Boolean is
   begin
      if Left.Element = null and Right.Element = null then
         return True;
      elsif Left.Element /= null and Right.Element /= null then
         return Left.Element.all = Right.Element.all;
      else
         return False;
      end if;
   end "=";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Container : in out Holder) is
   begin
      if Container.Element /= null then
         Container.Element := new Element_Type'(Container.Element.all);
      end if;

      Container.Busy := 0;
   end Adjust;

   overriding procedure Adjust (Control : in out Reference_Control_Type) is
   begin
      if Control.Container /= null then
         declare
            B : Natural renames Control.Container.Busy;
         begin
            B := B + 1;
         end;
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

      if Target.Element /= Source.Element then
         Free (Target.Element);

         if Source.Element /= null then
            Target.Element := new Element_Type'(Source.Element.all);
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

      Free (Container.Element);
   end Clear;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Holder) return Constant_Reference_Type
   is
      Ref : constant Constant_Reference_Type :=
              (Element => Container.Element.all'Access,
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
   begin
      if Source.Element = null then
         return (Controlled with null, 0);
      else
         return (Controlled with new Element_Type'(Source.Element.all), 0);
      end if;
   end Copy;

   -------------
   -- Element --
   -------------

   function Element (Container : Holder) return Element_Type is
   begin
      if Container.Element = null then
         raise Constraint_Error with "container is empty";
      else
         return Container.Element.all;
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

      Free (Container.Element);
   end Finalize;

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

   function Is_Empty (Container : Holder) return Boolean is
   begin
      return Container.Element = null;
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

      if Target.Element /= Source.Element then
         Free (Target.Element);
         Target.Element := Source.Element;
         Source.Element := null;
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
      if Container.Element = null then
         raise Constraint_Error with "container is empty";
      end if;

      B := B + 1;

      begin
         Process (Container.Element.all);
      exception
         when others =>
            B := B - 1;
            raise;
      end;

      B := B - 1;
   end Query_Element;

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
         Container.Element := new Element_Type'(Element_Type'Input (Stream));
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
              (Element => Container.Element.all'Access,
               Control => (Controlled with Container'Unrestricted_Access));
   begin
      Container.Busy := Container.Busy + 1;
      return Ref;
   end Reference;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Holder;
      New_Item  : Element_Type)
   is
   begin
      if Container.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      declare
         X : Element_Access := Container.Element;

         --  Element allocator may need an accessibility check in case actual
         --  type is class-wide or has access discriminants (RM 4.8(10.1) and
         --  AI12-0035).

         pragma Unsuppress (Accessibility_Check);

      begin
         Container.Element := new Element_Type'(New_Item);
         Free (X);
      end;
   end Replace_Element;

   ----------
   -- Swap --
   ----------

   procedure Swap (Left, Right : in out Holder) is
   begin
      if Left.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      if Right.Busy /= 0 then
         raise Program_Error with "attempt to tamper with elements";
      end if;

      if Left.Element /= Right.Element then
         declare
            Tmp : constant Element_Access := Left.Element;
         begin
            Left.Element := Right.Element;
            Right.Element := Tmp;
         end;
      end if;
   end Swap;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (New_Item : Element_Type) return Holder is

      --  The element allocator may need an accessibility check in the case the
      --  actual type is class-wide or has access discriminants (RM 4.8(10.1)
      --  and AI12-0035).

      pragma Unsuppress (Accessibility_Check);

   begin
      return (Controlled with new Element_Type'(New_Item), 0);
   end To_Holder;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : in out Holder;
      Process   : not null access procedure (Element : in out Element_Type))
   is
      B : Natural renames Container.Busy;

   begin
      if Container.Element = null then
         raise Constraint_Error with "container is empty";
      end if;

      B := B + 1;

      begin
         Process (Container.Element.all);
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
      Boolean'Output (Stream, Container.Element = null);

      if Container.Element /= null then
         Element_Type'Output (Stream, Container.Element.all);
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
