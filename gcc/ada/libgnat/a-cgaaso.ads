------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.GENERIC_ANONYMOUS_ARRAY_SORT                 --
--                                                                          --
--                                 S p e c                                  --
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

--  Allows an anonymous array (or array-like container) to be sorted. Generic
--  formal Less returns the result of comparing the elements designated by the
--  indexes, and generic formal Swap exchanges the designated elements.

generic
   type Index_Type is (<>);
   with function Less (Left, Right : Index_Type) return Boolean is <>;
   with procedure Swap (Left, Right : Index_Type) is <>;

procedure Ada.Containers.Generic_Anonymous_Array_Sort
  (First, Last : Index_Type'Base);
pragma Pure (Ada.Containers.Generic_Anonymous_Array_Sort);
--  Reorders the elements of Container such that the elements are sorted
--  smallest first as determined by the generic formal "<" operator provided.
--  Any exception raised during evaluation of "<" is propagated.
--
--  The actual function for the generic formal function "<" is expected to
--  return the same value each time it is called with a particular pair of
--  element values. It should not modify Container and it should define a
--  strict weak ordering relationship: irreflexive, asymmetric, transitive, and
--  in addition, if x < y for any values x and y, then for all other values z,
--  (x < z) or (z < y).  If the actual for "<" behaves in some other manner,
--  the behavior of the instance of Generic_Anonymous_Array_Sort is
--  unspecified. The number of times Generic_Anonymous_Array_Sort calls "<" is
--  unspecified.
