------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_BASE                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2016-2018, Free Software Foundation, Inc.         --
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

   ---------
   -- "=" --
   ---------

   function "=" (C1 : Container; C2 : Container) return Boolean is
   begin
      if C1.Elements'Length /= C2.Elements'Length then
         return False;
      end if;

      for I in C1.Elements'Range loop
         if C1.Elements (I).all /= C2.Elements (I).all then
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
      for I in C1.Elements'Range loop
         if Find (C2, C1.Elements (I)) = 0 then
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
      A : constant Element_Array_Access :=
            new Element_Array'(1 .. C.Elements'Last + 1 => <>);
      P : Count_Type := 0;

   begin
      for J in 1 .. C.Elements'Last + 1 loop
         if J /= To_Count (I) then
            P := P + 1;
            A (J) := C.Elements (P);
         else
            A (J) := new Element_Type'(E);
         end if;
      end loop;

      return Container'(Elements => A);
   end Add;

   ----------
   -- Find --
   ----------

   function Find (C : Container; E : access Element_Type) return Count_Type is
   begin
      for I in C.Elements'Range loop
         if C.Elements (I).all = E.all then
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
     (C.Elements (To_Count (I)).all);

   ------------------
   -- Intersection --
   ------------------

   function Intersection (C1 : Container; C2 : Container) return Container is
      A : constant Element_Array_Access :=
            new Element_Array'(1 .. Num_Overlaps (C1, C2) => <>);
      P : Count_Type := 0;

   begin
      for I in C1.Elements'Range loop
         if Find (C2, C1.Elements (I)) > 0 then
            P := P + 1;
            A (P) := C1.Elements (I);
         end if;
      end loop;

      return Container'(Elements => A);
   end Intersection;

   ------------
   -- Length --
   ------------

   function Length (C : Container) return Count_Type is (C.Elements'Length);

   ---------------------
   -- Num_Overlaps --
   ---------------------

   function Num_Overlaps (C1 : Container; C2 : Container) return Count_Type is
      P : Count_Type := 0;

   begin
      for I in C1.Elements'Range loop
         if Find (C2, C1.Elements (I)) > 0 then
            P := P + 1;
         end if;
      end loop;

      return P;
   end Num_Overlaps;

   ------------
   -- Remove --
   ------------

   function Remove (C : Container; I : Index_Type) return Container is
      A : constant Element_Array_Access :=
            new Element_Array'(1 .. C.Elements'Last - 1 => <>);
      P : Count_Type := 0;

   begin
      for J in C.Elements'Range loop
         if J /= To_Count (I) then
            P := P + 1;
            A (P) := C.Elements (J);
         end if;
      end loop;

      return Container'(Elements => A);
   end Remove;

   ---------
   -- Set --
   ---------

   function Set
     (C : Container;
      I : Index_Type;
      E : Element_Type) return Container
   is
      Result : constant Container :=
                 Container'(Elements => new Element_Array'(C.Elements.all));

   begin
      Result.Elements (To_Count (I)) := new Element_Type'(E);
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
         A : constant Element_Array_Access :=
               new Element_Array'
                     (C1.Elements.all & (Length (C1) + 1 .. L => <>));
         P : Count_Type := Length (C1);

      begin
         for I in C2.Elements'Range loop
            if Find (C1, C2.Elements (I)) = 0 then
               P := P + 1;
               A (P) := C2.Elements (I);
            end if;
         end loop;

         return Container'(Elements => A);
      end;
   end Union;

end Ada.Containers.Functional_Base;
