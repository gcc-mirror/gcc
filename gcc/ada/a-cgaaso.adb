------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      A D A . C O N T A I N E R S .                       --
--        G E N E R I C _ A N O N Y M O U S _ A R R A Y _ S O R T           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
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

procedure Ada.Containers.Generic_Anonymous_Array_Sort
  (First, Last : Index_Type'Base)
is
   Pivot, Lo, Mid, Hi : Index_Type;

begin
   if Last <= First then
      return;
   end if;

   Lo := First;
   Hi := Last;

   if Last = Index_Type'Succ (First) then
      if not Less (Lo, Hi) then
         Swap (Lo, Hi);
      end if;

      return;
   end if;

   Mid := Index_Type'Val
     (Index_Type'Pos (Lo) +
      (Index_Type'Pos (Hi) - Index_Type'Pos (Lo)) / 2);

   --  We need to figure out which case we have:
   --  x < y < z
   --  x < z < y
   --  z < x < y
   --  y < x < z
   --  y < z < x
   --  z < y < x

   if Less (Lo, Mid) then
      if Less (Lo, Hi) then
         if Less (Mid, Hi) then
            Swap (Lo, Mid);

         else
            Swap (Lo, Hi);

         end if;

      else
         null;  --  lo is median
      end if;

   elsif Less (Lo, Hi) then
      null; --  lo is median

   elsif Less (Mid, Hi) then
      Swap (Lo, Hi);

   else
      Swap (Lo, Mid);
   end if;

   Pivot := Lo;
   Outer : loop
      loop
         exit Outer when not (Pivot < Hi);

         if Less (Hi, Pivot) then
            Swap (Hi, Pivot);
            Pivot := Hi;
            Lo := Index_Type'Succ (Lo);
            exit;
         else
            Hi := Index_Type'Pred (Hi);
         end if;
      end loop;

      loop
         exit Outer when not (Lo < Pivot);

         if Less (Lo, Pivot) then
            Lo := Index_Type'Succ (Lo);
         else
            Swap (Lo, Pivot);
            Pivot := Lo;
            Hi := Index_Type'Pred (Hi);
            exit;
         end if;
      end loop;
   end loop Outer;

   Generic_Anonymous_Array_Sort (First, Index_Type'Pred (Pivot));
   Generic_Anonymous_Array_Sort (Index_Type'Succ (Pivot), Last);

end Ada.Containers.Generic_Anonymous_Array_Sort;
