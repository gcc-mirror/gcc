------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     G N A T . H E A P _ S O R T _ G                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1995-2000 Ada Core Technologies, Inc.            --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  Heapsort generic package using formal procedures

--  This package provides a generic heapsort routine that can be used with
--  different types of data. See also GNAT.Heap_Sort_A, a version that works
--  with subprogram parameters, allowing code sharing. The generic version
--  is slightly more efficient but does not allow code sharing. The generic
--  version is also Pure, while the access version can only be Preelaborate.

generic
   --  The data to be sorted is assumed to be indexed by integer values from
   --  1 to N, where N is the number of items to be sorted. In addition, the
   --  index value zero is used for a temporary location used during the sort.

   with procedure Move (From : Natural; To : Natural);
   --  A procedure that moves the data item with index From to the data item
   --  with Index To. An index value of zero is used for moves from and to a
   --  single temporary location used by the sort.

   with function Lt (Op1, Op2 : Natural) return Boolean;
   --  A function that compares two items and returns True if the item with
   --  index Op1 is less than the item with Index Op2, and False if the Op1
   --  item is greater than or equal to the Op2 item.

package GNAT.Heap_Sort_G is
pragma Pure (Heap_Sort_G);

   procedure Sort (N : Natural);
   --  This procedures sorts items in the range from 1 to N into ascending
   --  order making calls to Lt to do required comparisons, and Move to move
   --  items around. Note that, as described above, both Move and Lt use a
   --  single temporary location with index value zero. This sort is not
   --  stable, i.e. the order of equal elements in the input is not preserved.

end GNAT.Heap_Sort_G;
