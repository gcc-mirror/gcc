------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                   G N A T . B U B B L E _ S O R T _ G                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1995-2002 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
--  more awkward to use. The generic version is also Pure, while the access
--  subprograqm version can only be Preelaborate.

--  There is also GNAT.Bubble_Sort_A, which is now considered obsolete, but
--  was an older version working with subprogram parameters. This version
--  is retained for bacwards compatibility with old versions of GNAT.

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
pragma Pure (Bubble_Sort_G);

   procedure Sort (N : Natural);
   --  This procedures sorts items in the range from 1 to N into ascending
   --  order making calls to Lt to do required comparisons, and Move to move
   --  items around. Note that, as described above, both Move and Lt use a
   --  single temporary location with index value zero. This sort is stable,
   --  that is the order of equal elements in the input is preserved.

end GNAT.Bubble_Sort_G;
