------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_BASE                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2016-2019, Free Software Foundation, Inc.         --
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

   ---------
   -- "=" --
   ---------

   function "=" (C1 : Container; C2 : Container) return Boolean is
   begin
      if C1.Length /= C2.Length then
         return False;
      end if;

      for I in 1 .. C1.Length loop
         if C1.Base.Elements (I).all /= C2.Base.Elements (I).all then
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
         if Find (C2, C1.Base.Elements (I)) = 0 then
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
   begin
      if To_Count (I) = C.Length + 1 and then C.Length = C.Base.Max_Length then
         Resize (C.Base);
         C.Base.Max_Length := C.Base.Max_Length + 1;
         C.Base.Elements (C.Base.Max_Length) := new Element_Type'(E);

         return Container'(Length => C.Base.Max_Length, Base => C.Base);
      else
         declare
            A : constant Array_Base_Access := Content_Init (C.Length);
            P : Count_Type := 0;
         begin
            A.Max_Length := C.Length + 1;
            for J in 1 .. C.Length + 1 loop
               if J /= To_Count (I) then
                  P := P + 1;
                  A.Elements (J) := C.Base.Elements (P);
               else
                  A.Elements (J) := new Element_Type'(E);
               end if;
            end loop;

            return Container'(Length => A.Max_Length,
                              Base   => A);
         end;
      end if;
   end Add;

   ------------------
   -- Content_Init --
   ------------------

   function Content_Init (L : Count_Type := 0) return Array_Base_Access
   is
      Max_Init : constant Count_Type := 100;
      Size     : constant Count_Type :=
        (if L < Count_Type'Last - Max_Init then L + Max_Init
         else Count_Type'Last);
      Elements : constant Element_Array_Access :=
        new Element_Array'(1 .. Size => <>);
   begin
      return new Array_Base'(Max_Length => 0, Elements => Elements);
   end Content_Init;

   ----------
   -- Find --
   ----------

   function Find (C : Container; E : access Element_Type) return Count_Type is
   begin
      for I in 1 .. C.Length loop
         if C.Base.Elements (I).all = E.all then
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
     (C.Base.Elements (To_Count (I)).all);

   ------------------
   -- Intersection --
   ------------------

   function Intersection (C1 : Container; C2 : Container) return Container is
      L : constant Count_Type := Num_Overlaps (C1, C2);
      A : constant Array_Base_Access := Content_Init (L);
      P : Count_Type := 0;

   begin
      A.Max_Length := L;
      for I in 1 .. C1.Length loop
         if Find (C2, C1.Base.Elements (I)) > 0 then
            P := P + 1;
            A.Elements (P) := C1.Base.Elements (I);
         end if;
      end loop;

      return Container'(Length => P, Base => A);
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
         if Find (C2, C1.Base.Elements (I)) > 0 then
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
         return Container'(Length => C.Length - 1, Base => C.Base);
      else
         declare
            A : constant Array_Base_Access := Content_Init (C.Length - 1);
            P : Count_Type := 0;
         begin
            A.Max_Length := C.Length - 1;
            for J in 1 .. C.Length loop
               if J /= To_Count (I) then
                  P := P + 1;
                  A.Elements (P) := C.Base.Elements (J);
               end if;
            end loop;

            return Container'(Length => C.Length - 1, Base => A);
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
                 Container'(Length => C.Length,
                            Base => Content_Init (C.Length));

   begin
      Result.Base.Max_Length := C.Length;
      Result.Base.Elements (1 .. C.Length) := C.Base.Elements (1 .. C.Length);
      Result.Base.Elements (To_Count (I)) := new Element_Type'(E);
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
         A : constant Array_Base_Access := Content_Init (L);
         P : Count_Type := Length (C1);

      begin
         A.Max_Length := L;
         A.Elements (1 .. C1.Length) := C1.Base.Elements (1 .. C1.Length);
         for I in 1 .. C2.Length loop
            if Find (C1, C2.Base.Elements (I)) = 0 then
               P := P + 1;
               A.Elements (P) := C2.Base.Elements (I);
            end if;
         end loop;

         return Container'(Length => L, Base => A);
      end;
   end Union;

end Ada.Containers.Functional_Base;
