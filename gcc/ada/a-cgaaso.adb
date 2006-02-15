------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      A D A . C O N T A I N E R S .                       --
--        G E N E R I C _ A N O N Y M O U S _ A R R A Y _ S O R T           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  This algorithm was adapted from GNAT.Heap_Sort (see g-heasor.ad[sb]).

with System;

procedure Ada.Containers.Generic_Anonymous_Array_Sort
  (First, Last : Index_Type'Base)
is
   type T is range System.Min_Int .. System.Max_Int;

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
      return Less (To_Index (J), To_Index (K));
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

--  Start of processing for Generic_Anonymous_Array_Sort

begin
   for J in reverse 1 .. Max / 2 loop
      Sift (J);
   end loop;

   while Max > 1 loop
      Xchg (1, Max);
      Max := Max - 1;
      Sift (1);
   end loop;
end Ada.Containers.Generic_Anonymous_Array_Sort;
