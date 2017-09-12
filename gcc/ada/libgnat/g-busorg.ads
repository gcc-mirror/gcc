------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   G N A T . B U B B L E _ S O R T _ G                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2017, AdaCore                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Bubblesort generic package using formal procedures

--  This package provides a generic bubble sort routine that can be used with
--  different types of data.

--  See also GNAT.Bubble_Sort, a version that works with subprogram access
--  parameters, allowing code sharing. The generic version is slightly more
--  efficient but does not allow code sharing and has an interface that is
--  more awkward to use.

--  There is also GNAT.Bubble_Sort_A, which is now considered obsolete, but
--  was an older version working with subprogram parameters. This version
--  is retained for backwards compatibility with old versions of GNAT.

generic
   --  The data to be sorted is assumed to be indexed by integer values from
   --  1 to N, where N is the number of items to be sorted. In addition, the
   --  index value zero is used for a temporary location used during the sort.

   with procedure Move (From : Natural; To : Natural);
   --  A procedure that moves the data item with index value From to the data
   --  item with index value To (the old value in To being lost). An index
   --  value of zero is used for moves from and to a single temporary location
   --  used by the sort.

   with function Lt (Op1, Op2 : Natural) return Boolean;
   --  A function that compares two items and returns True if the item with
   --  index Op1 is less than the item with Index Op2, and False if the Op2
   --  item is greater than or equal to the Op1 item.

package GNAT.Bubble_Sort_G is
   pragma Pure;

   procedure Sort (N : Natural);
   --  This procedures sorts items in the range from 1 to N into ascending
   --  order making calls to Lt to do required comparisons, and Move to move
   --  items around. Note that, as described above, both Move and Lt use a
   --  single temporary location with index value zero. This sort is stable,
   --  that is the order of equal elements in the input is preserved.

end GNAT.Bubble_Sort_G;
