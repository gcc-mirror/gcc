------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_SETS                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2016-2017, Free Software Foundation, Inc.         --
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

package body Ada.Containers.Functional_Sets with SPARK_Mode => Off is
   use Containers;

   ---------
   -- "=" --
   ---------

   function "=" (S1, S2 : Set) return Boolean is
     (S1.Content <= S2.Content and S2.Content <= S1.Content);

   ----------
   -- "<=" --
   ----------

   function "<=" (S1, S2 : Set) return Boolean is (S1.Content <= S2.Content);

   ---------
   -- Add --
   ---------

   function Add (S : Set; E : Element_Type) return Set is
     (Content => Add (S.Content, Length (S.Content) + 1, E));

   ------------------
   -- Intersection --
   ------------------

   function Intersection (S1, S2 : Set) return Set is
     (Content => Intersection (S1.Content, S2.Content));

   ------------
   -- Is_Add --
   ------------

   function Is_Add (S : Set; E : Element_Type; Result : Set) return Boolean
   is
     (Mem (Result, E)
      and (for all F of Result => Mem (S, F) or F = E)
      and (for all E of S => Mem (Result, E)));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Set) return Boolean is (Length (S.Content) = 0);

   ---------------------
   -- Is_Intersection --
   ---------------------

   function Is_Intersection (S1, S2, Result : Set) return Boolean is
     ((for all E of Result =>
            Mem (S1, E) and Mem (S2, E))
      and (for all E of S1 =>
               (if Mem (S2, E) then Mem (Result, E))));

   --------------
   -- Is_Union --
   --------------

   function Is_Union (S1, S2, Result : Set) return Boolean is
     ((for all E of Result => Mem (S1, E) or Mem (S2, E))
      and (for all E of S1 => Mem (Result, E))
      and (for all E of S2 => Mem (Result, E)));

   ------------
   -- Length --
   ------------

   function Length (S : Set) return Count_Type is (Length (S.Content));

   ---------
   -- Mem --
   ---------

   function Mem (S : Set; E : Element_Type) return Boolean is
      (Find (S.Content, E) > 0);

   ------------------
   -- Num_Overlaps --
   ------------------

   function Num_Overlaps (S1, S2 : Set) return Count_Type is
      (Num_Overlaps (S1.Content, S2.Content));

   ------------
   -- Remove --
   ------------

   function Remove (S : Set; E : Element_Type) return Set is
     (Content => Remove (S.Content, Find (S.Content, E)));

   -----------
   -- Union --
   -----------

   function Union (S1, S2 : Set) return Set is
     (Content => Union (S1.Content, S2.Content));
end Ada.Containers.Functional_Sets;
