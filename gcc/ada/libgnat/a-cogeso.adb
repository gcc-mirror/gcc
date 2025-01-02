------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                       ADA.CONTAINERS.GENERIC_SORT                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2025, Free Software Foundation, Inc.       --
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

--  This algorithm was adapted from GNAT.Heap_Sort (see g-heasor.ad[sb])

procedure Ada.Containers.Generic_Sort (First, Last : Index_Type'Base) is
   subtype T is Long_Long_Integer;

   function To_Index (J : T) return Index_Type;
   pragma Inline (To_Index);

   function Lt (J, K : T) return Boolean;
   pragma Inline (Lt);

   procedure Xchg (J, K : T);
   pragma Inline (Xchg);

   procedure Sift (S : T);

   --------------
   -- To_Index --
   --------------

   function To_Index (J : T) return Index_Type is
      K : constant T'Base := Index_Type'Pos (First) + J - T'(1);
   begin
      return Index_Type'Val (K);
   end To_Index;

   --------
   -- Lt --
   --------

   function Lt (J, K : T) return Boolean is
   begin
      return Before (To_Index (J), To_Index (K));
   end Lt;

   ----------
   -- Xchg --
   ----------

   procedure Xchg (J, K : T) is
   begin
      Swap (To_Index (J), To_Index (K));
   end Xchg;

   Max : T := Index_Type'Pos (Last) - Index_Type'Pos (First) + T'(1);

   ----------
   -- Sift --
   ----------

   procedure Sift (S : T) is
      C      : T := S;
      Son    : T;
      Father : T;

   begin
      loop
         Son := C + C;

         if Son < Max then
            if Lt (Son, Son + 1) then
               Son := Son + 1;
            end if;
         elsif Son > Max then
            exit;
         end if;

         Xchg (Son, C);
         C := Son;
      end loop;

      while C /= S loop
         Father := C / 2;

         if Lt (Father, C) then
            Xchg (Father, C);
            C := Father;
         else
            exit;
         end if;
      end loop;
   end Sift;

--  Start of processing for Generic_Sort

begin
   for J in reverse 1 .. Max / 2 loop
      Sift (J);
   end loop;

   while Max > 1 loop
      Xchg (1, Max);
      Max := Max - 1;
      Sift (1);
   end loop;
end Ada.Containers.Generic_Sort;
