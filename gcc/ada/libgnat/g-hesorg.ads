------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     G N A T . H E A P _ S O R T _ G                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1995-2018, AdaCore                     --
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

--  Heapsort generic package using formal procedures

--  This package provides a generic heapsort routine that can be used with
--  different types of data.

--  See also GNAT.Heap_Sort, a version that works with subprogram access
--  parameters, allowing code sharing. The generic version is slightly more
--  efficient but does not allow code sharing and has an interface that is
--  more awkward to use.

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
   --  value of zero is used for moves from and to a single temporary location.
   --  For best efficiency, this routine should be marked as inlined.

   with function Lt (Op1, Op2 : Natural) return Boolean;
   --  A function that compares two items and returns True if the item with
   --  index Op1 is less than the item with Index Op2, and False if the Op1
   --  item is greater than the Op2 item. If the two items are equal, then
   --  it does not matter whether True or False is returned (it is slightly
   --  more efficient to return False). For best efficiency, this routine
   --  should be marked as inlined.

   --  Note on use of temporary location

   --  There are two ways of providing for the index value zero to represent
   --  a temporary value. Either an extra location can be allocated at the
   --  start of the array, or alternatively the Move and Lt subprograms can
   --  test for the case of zero and treat it specially. In any case it is
   --  desirable to specify the two subprograms as inlined and the tests for
   --  zero will in this case be resolved at instantiation time.

package GNAT.Heap_Sort_G is
   pragma Pure;

   procedure Sort (N : Natural);
   --  This procedures sorts items in the range from 1 to N into ascending
   --  order making calls to Lt to do required comparisons, and Move to move
   --  items around. Note that, as described above, both Move and Lt use a
   --  single temporary location with index value zero. This sort is not
   --  stable, i.e. the order of equal elements in the input is not preserved.

end GNAT.Heap_Sort_G;
