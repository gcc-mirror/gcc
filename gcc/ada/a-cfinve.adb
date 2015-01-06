------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.FORMAL_INDEFINITE_VECTORS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
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

package body Ada.Containers.Formal_Indefinite_Vectors with
  SPARK_Mode => Off
is
   pragma Annotate (CodePeer, Skip_Analysis);

   function H (New_Item : Element_Type) return Holder renames To_Holder;
   function E (Container : Holder) return Element_Type renames Get;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Vector) return Boolean is
      (Left.V = Right.V);

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; New_Item : Vector) is
   begin
      Append (Container.V, New_Item.V);
   end Append;

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type)
   is
   begin
      Append (Container.V, H (New_Item));
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Vector; Source : Vector) is
   begin
      Assign (Target.V, Source.V);
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Vector) return Capacity_Range is
      (Capacity (Container.V));

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Vector) is
   begin
      Clear (Container.V);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean
   is
     (Contains (Container.V, H (Item)));

   ----------
   -- Copy --
   ----------

   function Copy
     (Source   : Vector;
      Capacity : Capacity_Range := 0) return Vector
   is
     ((if Capacity = 0 then Length (Source) else Capacity),
       V => Copy (Source.V, Capacity));

   ---------------------
   -- Current_To_Last --
   ---------------------

   function Current_To_Last
     (Container : Vector;
      Current   : Index_Type) return Vector is
   begin
      return (Length (Container), Current_To_Last (Container.V, Current));
   end Current_To_Last;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last
     (Container : in out Vector)
   is
   begin
      Delete_Last (Container.V);
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type is
     (E (Element (Container.V, Index)));

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   is
     (Find_Index (Container.V, H (Item), Index));

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Vector) return Element_Type is
      (E (First_Element (Container.V)));

   -----------------
   -- First_Index --
   -----------------

   function First_Index (Container : Vector) return Index_Type is
      (First_Index (Container.V));

   -----------------------
   -- First_To_Previous --
   -----------------------

   function First_To_Previous
     (Container : Vector;
      Current   : Index_Type) return Vector is
   begin
      return (Length (Container), First_To_Previous (Container.V, Current));
   end First_To_Previous;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting is

      function "<" (X, Y : Holder) return Boolean is (E (X) < E (Y));
      package Def_Sorting is new Def.Generic_Sorting ("<");
      use Def_Sorting;

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : Vector) return Boolean is
         (Is_Sorted (Container.V));

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out Vector) is
      begin
         Sort (Container.V);
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Container : Vector;
      Position  : Extended_Index) return Boolean
   is
     (Has_Element (Container.V, Position));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Vector) return Boolean is
      (Is_Empty (Container.V));

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Vector) return Element_Type is
      (E (Last_Element (Container.V)));

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Container : Vector) return Extended_Index is
      (Last_Index (Container.V));

   ------------
   -- Length --
   ------------

   function Length (Container : Vector) return Capacity_Range is
      (Length (Container.V));

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type)
   is
   begin
      Replace_Element (Container.V, Index, H (New_Item));
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Capacity_Range)
   is
   begin
      Reserve_Capacity (Container.V, Capacity);
   end Reserve_Capacity;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in out Vector) is
   begin
      Reverse_Elements (Container.V);
   end Reverse_Elements;

   ------------------------
   -- Reverse_Find_Index --
   ------------------------

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   is
     (Reverse_Find_Index (Container.V, H (Item), Index));

   ----------
   -- Swap --
   ----------

   procedure Swap (Container : in out Vector; I, J : Index_Type) is
   begin
      Swap (Container.V, I, J);
   end Swap;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector
     (New_Item : Element_Type;
      Length   : Capacity_Range) return Vector
   is
   begin
      return (Length, To_Vector (H (New_Item), Length));
   end To_Vector;

end Ada.Containers.Formal_Indefinite_Vectors;
