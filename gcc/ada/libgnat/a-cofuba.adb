------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_BASE                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2016-2022, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package body Ada.Containers.Functional_Base with SPARK_Mode => Off is

   function To_Count (Idx : Extended_Index) return Count_Type is
     (Count_Type
       (Extended_Index'Pos (Idx) -
        Extended_Index'Pos (Extended_Index'First)));

   function To_Index (Position : Count_Type) return Extended_Index is
     (Extended_Index'Val
       (Position + Extended_Index'Pos (Extended_Index'First)));
   --  Conversion functions between Index_Type and Count_Type

   function Find (C : Container; E : access Element_Type) return Count_Type;
   --  Search a container C for an element equal to E.all, returning the
   --  position in the underlying array.

   procedure Resize (Base : Array_Base_Access);
   --  Resize the underlying array if needed so that it can contain one more
   --  element.

   function Elements (C : Container) return Element_Array_Access is
     (C.Controlled_Base.Base.Elements)
   with
     Global => null,
     Pre    =>
       C.Controlled_Base.Base /= null
       and then C.Controlled_Base.Base.Elements /= null;

   function Get
     (C_E : Element_Array_Access;
      I   : Count_Type)
      return Element_Access
   is
     (C_E (I).Ref.E_Access)
   with
     Global => null,
     Pre    => C_E /= null and then C_E (I).Ref /= null;

   ---------
   -- "=" --
   ---------

   function "=" (C1 : Container; C2 : Container) return Boolean is
   begin
      if C1.Length /= C2.Length then
         return False;
      end if;
      for I in 1 .. C1.Length loop
         if Get (Elements (C1), I).all /= Get (Elements (C2), I).all then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   ----------
   -- "<=" --
   ----------

   function "<=" (C1 : Container; C2 : Container) return Boolean is
   begin
      for I in 1 .. C1.Length loop
         if Find (C2, Get (Elements (C1), I)) = 0 then
            return False;
         end if;
      end loop;

      return True;
   end "<=";

   ---------
   -- Add --
   ---------

   function Add
     (C : Container;
      I : Index_Type;
      E : Element_Type) return Container
   is
      C_B : Array_Base_Access renames C.Controlled_Base.Base;
   begin
      if To_Count (I) = C.Length + 1 and then C.Length = C_B.Max_Length then
         Resize (C_B);
         C_B.Max_Length := C_B.Max_Length + 1;
         C_B.Elements (C_B.Max_Length) := Element_Init (E);

         return Container'(Length          => C_B.Max_Length,
                           Controlled_Base => C.Controlled_Base);
      else
         declare
            A : constant Array_Base_Controlled_Access :=
              Content_Init (C.Length);
            P : Count_Type := 0;
         begin
            A.Base.Max_Length := C.Length + 1;
            for J in 1 .. C.Length + 1 loop
               if J /= To_Count (I) then
                  P := P + 1;
                  A.Base.Elements (J) := C_B.Elements (P);
               else
                  A.Base.Elements (J) := Element_Init (E);
               end if;
            end loop;

            return Container'(Length           => A.Base.Max_Length,
                              Controlled_Base  => A);
         end;
      end if;
   end Add;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Controlled_Base : in out Array_Base_Controlled_Access) is
      C_B : Array_Base_Access renames Controlled_Base.Base;
   begin
      if C_B /= null then
         C_B.Reference_Count := C_B.Reference_Count + 1;
      end if;
   end Adjust;

   procedure Adjust (Ctrl_E : in out Controlled_Element_Access) is
   begin
      if Ctrl_E.Ref /= null then
         Ctrl_E.Ref.Reference_Count := Ctrl_E.Ref.Reference_Count + 1;
      end if;
   end Adjust;

   ------------------
   -- Content_Init --
   ------------------

   function Content_Init
     (L : Count_Type := 0) return Array_Base_Controlled_Access
   is
      Max_Init : constant Count_Type := 100;
      Size     : constant Count_Type :=
        (if L < Count_Type'Last - Max_Init then L + Max_Init
         else Count_Type'Last);

      --  The Access in the array will be initialized to null

      Elements : constant Element_Array_Access :=
        new Element_Array'(1 .. Size => <>);
      B        : constant Array_Base_Access :=
        new Array_Base'(Reference_Count => 1,
                        Max_Length      => 0,
                        Elements        => Elements);
   begin
      return (Ada.Finalization.Controlled with Base => B);
   end Content_Init;

   ------------------
   -- Element_Init --
   ------------------

   function Element_Init (E : Element_Type) return Controlled_Element_Access
   is
      Refcounted_E : constant Refcounted_Element_Access :=
        new Refcounted_Element'(Reference_Count => 1,
                                E_Access        => new Element_Type'(E));
   begin
      return (Ada.Finalization.Controlled with Ref => Refcounted_E);
   end Element_Init;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Controlled_Base : in out Array_Base_Controlled_Access)
   is
      procedure Unchecked_Free_Base is new Ada.Unchecked_Deallocation
        (Object => Array_Base,
         Name   => Array_Base_Access);
      procedure Unchecked_Free_Array is new Ada.Unchecked_Deallocation
        (Object => Element_Array,
         Name   => Element_Array_Access);

      C_B : Array_Base_Access renames Controlled_Base.Base;
   begin
      if C_B /= null then
         C_B.Reference_Count := C_B.Reference_Count - 1;
         if C_B.Reference_Count = 0 then
            Unchecked_Free_Array (Controlled_Base.Base.Elements);
            Unchecked_Free_Base (Controlled_Base.Base);
         end if;
         C_B := null;
      end if;
   end Finalize;

   procedure Finalize (Ctrl_E : in out Controlled_Element_Access) is
      procedure Unchecked_Free_Ref is new Ada.Unchecked_Deallocation
        (Object => Refcounted_Element,
         Name   => Refcounted_Element_Access);

      procedure Unchecked_Free_Element is new Ada.Unchecked_Deallocation
        (Object => Element_Type,
         Name   => Element_Access);

   begin
      if Ctrl_E.Ref /= null then
         Ctrl_E.Ref.Reference_Count := Ctrl_E.Ref.Reference_Count - 1;
         if Ctrl_E.Ref.Reference_Count = 0 then
            Unchecked_Free_Element (Ctrl_E.Ref.E_Access);
            Unchecked_Free_Ref (Ctrl_E.Ref);
         end if;
         Ctrl_E.Ref := null;
      end if;
   end Finalize;

   ----------
   -- Find --
   ----------

   function Find (C : Container; E : access Element_Type) return Count_Type is
   begin
      for I in 1 .. C.Length loop
         if Get (Elements (C), I).all = E.all then
            return I;
         end if;
      end loop;

      return 0;
   end Find;

   function Find (C : Container; E : Element_Type) return Extended_Index is
     (To_Index (Find (C, E'Unrestricted_Access)));

   ---------
   -- Get --
   ---------

   function Get (C : Container; I : Index_Type) return Element_Type is
      (Get (Elements (C), To_Count (I)).all);

   ------------------
   -- Intersection --
   ------------------

   function Intersection (C1 : Container; C2 : Container) return Container is
      L : constant Count_Type := Num_Overlaps (C1, C2);
      A : constant Array_Base_Controlled_Access := Content_Init (L);
      P : Count_Type := 0;

   begin
      A.Base.Max_Length := L;
      for I in 1 .. C1.Length loop
         if Find (C2, Get (Elements (C1), I)) > 0 then
            P := P + 1;
            A.Base.Elements (P) := Elements (C1) (I);
         end if;
      end loop;

      return Container'(Length => P, Controlled_Base => A);
   end Intersection;

   ------------
   -- Length --
   ------------

   function Length (C : Container) return Count_Type is (C.Length);
   ---------------------
   -- Num_Overlaps --
   ---------------------

   function Num_Overlaps (C1 : Container; C2 : Container) return Count_Type is
      P : Count_Type := 0;

   begin
      for I in 1 .. C1.Length loop
         if Find (C2, Get (Elements (C1), I)) > 0 then
            P := P + 1;
         end if;
      end loop;

      return P;
   end Num_Overlaps;

   ------------
   -- Remove --
   ------------

   function Remove (C : Container; I : Index_Type) return Container is
   begin
      if To_Count (I) = C.Length then
         return Container'(Length          => C.Length - 1,
                           Controlled_Base => C.Controlled_Base);
      else
         declare
            A : constant Array_Base_Controlled_Access
              := Content_Init (C.Length - 1);
            P : Count_Type := 0;
         begin
            A.Base.Max_Length := C.Length - 1;
            for J in 1 .. C.Length loop
               if J /= To_Count (I) then
                  P := P + 1;
                  A.Base.Elements (P) := Elements (C) (J);
               end if;
            end loop;

            return Container'(Length => C.Length - 1, Controlled_Base => A);
         end;
      end if;
   end Remove;

   ------------
   -- Resize --
   ------------

   procedure Resize (Base : Array_Base_Access) is
   begin
      if Base.Max_Length < Base.Elements'Length then
         return;
      end if;

      pragma Assert (Base.Max_Length = Base.Elements'Length);

      if Base.Max_Length = Count_Type'Last then
         raise Constraint_Error;
      end if;

      declare
         procedure Finalize is new Ada.Unchecked_Deallocation
           (Object => Element_Array,
            Name   => Element_Array_Access_Base);

         New_Length : constant Positive_Count_Type :=
           (if Base.Max_Length > Count_Type'Last / 2 then Count_Type'Last
            else 2 * Base.Max_Length);
         Elements   : constant Element_Array_Access :=
           new Element_Array (1 .. New_Length);
         Old_Elmts  : Element_Array_Access_Base := Base.Elements;
      begin
         Elements (1 .. Base.Max_Length) := Base.Elements.all;
         Base.Elements := Elements;
         Finalize (Old_Elmts);
      end;
   end Resize;

   ---------
   -- Set --
   ---------

   function Set
     (C : Container;
      I : Index_Type;
      E : Element_Type) return Container
   is
      Result : constant Container :=
                 Container'(Length          => C.Length,
                            Controlled_Base => Content_Init (C.Length));
      R_Base : Array_Base_Access renames Result.Controlled_Base.Base;

   begin
      R_Base.Max_Length := C.Length;
      R_Base.Elements (1 .. C.Length) := Elements (C) (1 .. C.Length);
      R_Base.Elements (To_Count (I)) := Element_Init (E);
      return Result;
   end Set;

   -----------
   -- Union --
   -----------

   function Union (C1 : Container; C2 : Container) return Container is
      N : constant Count_Type := Num_Overlaps (C1, C2);

   begin
      --  if C2 is completely included in C1 then return C1

      if N = Length (C2) then
         return C1;
      end if;

      --  else loop through C2 to find the remaining elements

      declare
         L : constant Count_Type := Length (C1) - N + Length (C2);
         A : constant Array_Base_Controlled_Access := Content_Init (L);
         P : Count_Type := Length (C1);
      begin
         A.Base.Max_Length := L;
         A.Base.Elements (1 .. C1.Length) := Elements (C1) (1 .. C1.Length);
         for I in 1 .. C2.Length loop
            if Find (C1, Get (Elements (C2), I)) = 0 then
               P := P + 1;
               A.Base.Elements (P) := Elements (C2) (I);
            end if;
         end loop;

         return Container'(Length => L, Controlled_Base => A);
      end;
   end Union;

end Ada.Containers.Functional_Base;
