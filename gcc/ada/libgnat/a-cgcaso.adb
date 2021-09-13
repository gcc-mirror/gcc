------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.GENERIC_CONSTRAINED_ARRAY_SORT               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2021, Free Software Foundation, Inc.         --
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
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  This algorithm was adapted from GNAT.Heap_Sort_G (see g-hesorg.ad[sb])

procedure Ada.Containers.Generic_Constrained_Array_Sort
  (Container : in out Array_Type)
is
   subtype T is Long_Long_Integer;

   function To_Index (J : T) return Index_Type;
   pragma Inline (To_Index);

   procedure Sift (S : T);

   A : Array_Type renames Container;

   --------------
   -- To_Index --
   --------------

   function To_Index (J : T) return Index_Type is
      K : constant T'Base := Index_Type'Pos (A'First) + J - T'(1);
   begin
      return Index_Type'Val (K);
   end To_Index;

   Max  : T := A'Length;
   Temp : Element_Type;

   ----------
   -- Sift --
   ----------

   procedure Sift (S : T) is
      C   : T := S;
      Son : T;

   begin
      loop
         Son := 2 * C;

         exit when Son > Max;

         declare
            Son_Index : Index_Type := To_Index (Son);

         begin
            if Son < Max then
               if A (Son_Index) < A (Index_Type'Succ (Son_Index)) then
                  Son := Son + 1;
                  Son_Index := Index_Type'Succ (Son_Index);
               end if;
            end if;

            A (To_Index (C)) := A (Son_Index);  -- Move (Son, C);
         end;

         C := Son;
      end loop;

      while C /= S loop
         declare
            Father : constant T := C / 2;
         begin
            if A (To_Index (Father)) < Temp then           -- Lt (Father, 0)
               A (To_Index (C)) := A (To_Index (Father));  -- Move (Father, C)
               C := Father;
            else
               exit;
            end if;
         end;
      end loop;

      A (To_Index (C)) := Temp; -- Move (0, C);
   end Sift;

--  Start of processing for Generic_Constrained_Array_Sort

begin
   for J in reverse 1 .. Max / 2 loop
      Temp := Container (To_Index (J)); --  Move (J, 0);
      Sift (J);
   end loop;

   while Max > 1 loop
      Temp := A (To_Index (Max));         --  Move (Max, 0);
      A (To_Index (Max)) := A (A'First);  --  Move (1, Max);

      Max := Max - 1;
      Sift (1);
   end loop;
end Ada.Containers.Generic_Constrained_Array_Sort;
