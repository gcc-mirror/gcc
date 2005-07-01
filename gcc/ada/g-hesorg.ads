------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     G N A T . H E A P _ S O R T _ G                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2005 AdaCore                      --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Heapsort generic package using formal procedures

--  This package provides a generic heapsort routine that can be used with
--  different types of data.

--  See also GNAT.Heap_Sort, a version that works with subprogram access
--  parameters, allowing code sharing. The generic version is slightly more
--  efficient but does not allow code sharing and has an interface that is
--  more awkward to use. The generic version is also Pure, while the access
--  subprogram version can only be Preelaborate.

--  There is also GNAT.Heap_Sort_A, which is now considered obsolete, but
--  was an older version working with subprogram parameters. This version
--  is retained for backwards compatibility with old versions of GNAT.

--  This heapsort algorithm uses approximately N*log(N) compares in the
--  worst case and is in place with no additional storage required. See
--  the body for exact details of the algorithm used.

generic
   --  The data to be sorted is assumed to be indexed by integer values from
   --  1 to N, where N is the number of items to be sorted. In addition, the
   --  index value zero is used for a temporary location used during the sort.

   with procedure Move (From : Natural; To : Natural);
   --  A procedure that moves the data item with index value From to the data
   --  item with index value To (the old value in To being lost). An index
   --  value of zero is used for moves from and to a single temporary location

   with function Lt (Op1, Op2 : Natural) return Boolean;
   --  A function that compares two items and returns True if the item with
   --  index Op1 is less than the item with Index Op2, and False if the Op1
   --  item is greater than the Op2 item. If the two items are equal, then
   --  it does not matter whether True or False is returned (it is slightly
   --  more efficient to return False).

   --  Note on use of temporary location

   --  There are two ways of providing for the index value zero to represent
   --  a temporary value. Either an extra location can be allocated at the
   --  start of the array, or alternatively the Move and Lt subprograms can
   --  test for the case of zero and treat it specially. In any case it is
   --  desirable to specify the two subprograms as inlined and the tests for
   --  zero will in this case be resolved at instantiation time.

package GNAT.Heap_Sort_G is
pragma Pure (Heap_Sort_G);

   procedure Sort (N : Natural);
   --  This procedures sorts items in the range from 1 to N into ascending
   --  order making calls to Lt to do required comparisons, and Move to move
   --  items around. Note that, as described above, both Move and Lt use a
   --  single temporary location with index value zero. This sort is not
   --  stable, i.e. the order of equal elements in the input is not preserved.

end GNAT.Heap_Sort_G;
