------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         A D A . C O N T A I N E R S . F O R M A L _ V E C T O R S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2014, Free Software Foundation, Inc.         --
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

with Ada.Containers.Generic_Array_Sort;
with Ada.Unchecked_Deallocation;

with System; use type System.Address;

package body Ada.Containers.Formal_Vectors with
  SPARK_Mode => Off
is
   pragma Annotate (CodePeer, Skip_Analysis);

   Growth_Factor : constant := 2;
   --  When growing a container, multiply current capacity by this. Doubling
   --  leads to amortized linear-time copying.

   type Int is range System.Min_Int .. System.Max_Int;
   type UInt is mod System.Max_Binary_Modulus;

   procedure Free is
      new Ada.Unchecked_Deallocation (Elements_Array, Elements_Array_Ptr);

   type Maximal_Array_Ptr is access all Elements_Array (Array_Index)
     with Storage_Size => 0;
   type Maximal_Array_Ptr_Const is access constant Elements_Array (Array_Index)
       with Storage_Size => 0;

   function Elems (Container : in out Vector) return Maximal_Array_Ptr;
   function Elemsc
     (Container : Vector) return Maximal_Array_Ptr_Const;
   --  Returns a pointer to the Elements array currently in use -- either
   --  Container.Elements_Ptr or a pointer to Container.Elements. We work with
   --  pointers to a bogus array subtype that is constrained with the maximum
   --  possible bounds. This means that the pointer is a thin pointer. This is
   --  necessary because 'Unrestricted_Access doesn't work when it produces
   --  access-to-unconstrained and is returned from a function.
   --
   --  Note that this is dangerous: make sure calls to this use an indexed
   --  component or slice that is within the bounds 1 .. Length (Container).

   function Get_Element
     (Container : Vector;
      Position  : Capacity_Range) return Element_Type;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Vector) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Length (Left) /= Length (Right) then
         return False;
      end if;

      for J in 1 .. Length (Left) loop
         if Get_Element (Left, J) /= Get_Element (Right, J) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector; New_Item : Vector) is
   begin
      for X in First_Index (New_Item) .. Last_Index (New_Item)  loop
         Append (Container, Element (New_Item, X));
      end loop;
   end Append;

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type)
   is
      New_Length : constant UInt := UInt (Length (Container) + 1);
   begin
      if not Bounded and then
        Capacity (Container) < Capacity_Range (New_Length)
      then
         Reserve_Capacity
           (Container,
            Capacity_Range'Max (Capacity (Container) * Growth_Factor,
                                Capacity_Range (New_Length)));
      end if;

      if Container.Last = Index_Type'Last then
         raise Constraint_Error with "vector is already at its maximum length";
      end if;

      --  TODO: should check whether length > max capacity (cnt_t'last)  ???

      Container.Last := Container.Last + 1;
      Elems (Container) (Length (Container)) := New_Item;
   end Append;

   ------------
   -- Assign --
   ------------

   procedure Assign (Target : in out Vector; Source : Vector) is
      LS : constant Capacity_Range := Length (Source);

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Bounded and then Target.Capacity < LS then
         raise Constraint_Error;
      end if;

      Clear (Target);
      Append (Target, Source);
   end Assign;

   --------------
   -- Capacity --
   --------------

   function Capacity (Container : Vector) return Capacity_Range is
   begin
      return (if Container.Elements_Ptr = null
              then Container.Elements'Length
              else Container.Elements_Ptr.all'Length);
   end Capacity;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out Vector) is
   begin
      Container.Last := No_Index;

      --  Free element, note that this is OK if Elements_Ptr is null

      Free (Container.Elements_Ptr);
   end Clear;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean
   is
   begin
      return Find_Index (Container, Item) /= No_Index;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy
     (Source   : Vector;
      Capacity : Capacity_Range := 0) return Vector
   is
      LS : constant Capacity_Range := Length (Source);
      C  : Capacity_Range;

   begin
      if Capacity = 0 then
         C := LS;
      elsif Capacity >= LS then
         C := Capacity;
      else
         raise Capacity_Error;
      end if;

      return Target : Vector (C) do
         Elems (Target) (1 .. LS) := Elemsc (Source) (1 .. LS);
         Target.Last := Source.Last;
      end return;
   end Copy;

   ---------------------
   -- Current_To_Last --
   ---------------------

   function Current_To_Last
     (Container : Vector;
      Current   : Index_Type) return Vector
   is
   begin
      return Result : Vector (Count_Type (Container.Last - Current + 1))
      do
         for X in Current .. Container.Last loop
            Append (Result, Element (Container, X));
         end loop;
      end return;
   end Current_To_Last;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last
     (Container : in out Vector)
   is
      Count : constant Capacity_Range := 1;
      Index : Int'Base;

   begin
      Index := Int'Base (Container.Last) - Int'Base (Count);

      if Index < Index_Type'Pos (Index_Type'First) then
         Container.Last := No_Index;
      else
         Container.Last := Index_Type (Index);
      end if;
   end Delete_Last;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type
   is
   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      declare
         II : constant Int'Base := Int (Index) - Int (No_Index);
         I  : constant Capacity_Range := Capacity_Range (II);
      begin
         return Get_Element (Container, I);
      end;
   end Element;

   --------------
   -- Elements --
   --------------

   function Elems (Container : in out Vector) return Maximal_Array_Ptr is
   begin
      return (if Container.Elements_Ptr = null
              then Container.Elements'Unrestricted_Access
              else Container.Elements_Ptr.all'Unrestricted_Access);
   end Elems;

   function Elemsc
     (Container : Vector) return Maximal_Array_Ptr_Const is
   begin
      return (if Container.Elements_Ptr = null
              then Container.Elements'Unrestricted_Access
              else Container.Elements_Ptr.all'Unrestricted_Access);
   end Elemsc;

   ----------------
   -- Find_Index --
   ----------------

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   is
      K    : Capacity_Range;
      Last : constant Index_Type := Last_Index (Container);

   begin
      K := Capacity_Range (Int (Index) - Int (No_Index));
      for Indx in Index .. Last loop
         if Get_Element (Container, K) = Item then
            return Indx;
         end if;

         K := K + 1;
      end loop;

      return No_Index;
   end Find_Index;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Vector) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "Container is empty";
      else
         return Get_Element (Container, 1);
      end if;
   end First_Element;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (Container : Vector) return Index_Type is
      pragma Unreferenced (Container);
   begin
      return Index_Type'First;
   end First_Index;

   -----------------------
   -- First_To_Previous --
   -----------------------

   function First_To_Previous
     (Container : Vector;
      Current   : Index_Type) return Vector
   is
   begin
      return Result : Vector
        (Count_Type (Current - First_Index (Container)))
      do
         for X in First_Index (Container) .. Current - 1 loop
            Append (Result, Element (Container, X));
         end loop;
      end return;
   end First_To_Previous;

   ---------------------
   -- Generic_Sorting --
   ---------------------

   package body Generic_Sorting is

      ---------------
      -- Is_Sorted --
      ---------------

      function Is_Sorted (Container : Vector) return Boolean is
         L : constant Capacity_Range := Length (Container);
      begin
         for J in 1 .. L - 1 loop
            if Get_Element (Container, J + 1) <
               Get_Element (Container, J)
            then
               return False;
            end if;
         end loop;

         return True;
      end Is_Sorted;

      ----------
      -- Sort --
      ----------

      procedure Sort (Container : in out Vector)
      is
         procedure Sort is
           new Generic_Array_Sort
             (Index_Type   => Array_Index,
              Element_Type => Element_Type,
              Array_Type   => Elements_Array,
              "<"          => "<");

         Len : constant Capacity_Range := Length (Container);
      begin
         if Container.Last <= Index_Type'First then
            return;
         else
            Sort (Elems (Container) (1 .. Len));
         end if;
      end Sort;

   end Generic_Sorting;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element
     (Container : Vector;
      Position  : Capacity_Range) return Element_Type
   is
   begin
      return Elemsc (Container) (Position);
   end Get_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Container : Vector; Position : Extended_Index) return Boolean is
   begin
      return Position in First_Index (Container) .. Last_Index (Container);
   end Has_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Last_Index (Container) < Index_Type'First;
   end Is_Empty;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : Vector) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "Container is empty";
      else
         return Get_Element (Container, Length (Container));
      end if;
   end Last_Element;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Container.Last;
   end Last_Index;

   ------------
   -- Length --
   ------------

   function Length (Container : Vector) return Capacity_Range is
      L : constant Int := Int (Last_Index (Container));
      F : constant Int := Int (Index_Type'First);
      N : constant Int'Base := L - F + 1;
   begin
      return Capacity_Range (N);
   end Length;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type)
   is
   begin
      if Index > Container.Last then
         raise Constraint_Error with "Index is out of range";
      end if;

      declare
         II : constant Int'Base := Int (Index) - Int (No_Index);
         I  : constant Capacity_Range := Capacity_Range (II);
      begin
         Elems (Container) (I) := New_Item;
      end;
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Capacity_Range)
   is
   begin
      if Bounded then
         if Capacity > Container.Capacity then
            raise Constraint_Error with "Capacity is out of range";
         end if;
      else
         if Capacity > Formal_Vectors.Capacity (Container) then
            declare
               New_Elements : constant Elements_Array_Ptr :=
                                new Elements_Array (1 .. Capacity);
               L            : constant Capacity_Range := Length (Container);
            begin
               New_Elements (1 .. L) := Elemsc (Container) (1 .. L);
               Free (Container.Elements_Ptr);
               Container.Elements_Ptr := New_Elements;
            end;
         end if;
      end if;
   end Reserve_Capacity;

   ----------------------
   -- Reverse_Elements --
   ----------------------

   procedure Reverse_Elements (Container : in out Vector) is
   begin
      if Length (Container) <= 1 then
         return;
      end if;

      declare
         I, J : Capacity_Range;
         E    : Elements_Array renames
                  Elems (Container) (1 .. Length (Container));

      begin
         I := 1;
         J := Length (Container);
         while I < J loop
            declare
               EI : constant Element_Type := E (I);
            begin
               E (I) := E (J);
               E (J) := EI;
            end;

            I := I + 1;
            J := J - 1;
         end loop;
      end;
   end Reverse_Elements;

   ------------------------
   -- Reverse_Find_Index --
   ------------------------

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   is
      Last : Index_Type'Base;
      K    : Capacity_Range;

   begin
      if Index > Last_Index (Container) then
         Last := Last_Index (Container);
      else
         Last := Index;
      end if;

      K := Capacity_Range (Int (Last) - Int (No_Index));
      for Indx in reverse Index_Type'First .. Last loop
         if Get_Element (Container, K) = Item then
            return Indx;
         end if;

         K := K - 1;
      end loop;

      return No_Index;
   end Reverse_Find_Index;

   ----------
   -- Swap --
   ----------

   procedure Swap (Container : in out Vector; I, J : Index_Type) is
   begin
      if I > Container.Last then
         raise Constraint_Error with "I index is out of range";
      end if;

      if J > Container.Last then
         raise Constraint_Error with "J index is out of range";
      end if;

      if I = J then
         return;
      end if;

      declare
         II : constant Int'Base := Int (I) - Int (No_Index);
         JJ : constant Int'Base := Int (J) - Int (No_Index);

         EI : Element_Type renames Elems (Container) (Capacity_Range (II));
         EJ : Element_Type renames Elems (Container) (Capacity_Range (JJ));

         EI_Copy : constant Element_Type := EI;

      begin
         EI := EJ;
         EJ := EI_Copy;
      end;
   end Swap;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector
     (New_Item : Element_Type;
      Length   : Capacity_Range) return Vector
   is
   begin
      if Length = 0 then
         return Empty_Vector;
      end if;

      declare
         First       : constant Int := Int (Index_Type'First);
         Last_As_Int : constant Int'Base := First + Int (Length) - 1;
         Last        : Index_Type;

      begin
         if Last_As_Int > Index_Type'Pos (Index_Type'Last) then
            raise Constraint_Error with "Length is out of range";  -- ???
         end if;

         Last := Index_Type (Last_As_Int);

         return (Capacity     => Length,
                 Last         => Last,
                 Elements_Ptr => <>,
                 Elements     => (others => New_Item));
      end;
   end To_Vector;

end Ada.Containers.Formal_Vectors;
