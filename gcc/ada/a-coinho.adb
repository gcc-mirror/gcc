------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . B O U N D E D _ V E C T O R S        --
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
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

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

   ----------
   -- Copy --
   ----------

   function Copy (Source : Holder) return Holder is
   begin
      if Source.Element = null then
         return (AF.Controlled with null, 0);
      else
         return (AF.Controlled with new Element_Type'(Source.Element.all), 0);
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

      Free (Container.Element);
      Container.Element := new Element_Type'(New_Item);
   end Replace_Element;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return (AF.Controlled with new Element_Type'(New_Item), 0);
   end To_Holder;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Container : Holder;
      Process   : not null access procedure (Element : in out Element_Type))
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

end Ada.Containers.Indefinite_Holders;
