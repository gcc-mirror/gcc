------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       G N A T . H E A P _ S O R T                        --
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

--  Sort utility (Using Heapsort Algorithm)

--  This package provides a heapsort routine that works with access to
--  subprogram parameters, so that it can be used with different types with
--  shared sorting code.

--  See also GNAT.Heap_Sort_G and GNAT.Heap_Sort_A. These are older versions
--  of this routine. In some cases GNAT.Heap_Sort_G may be a little faster
--  than GNAT.Heap_Sort, at the expense of generic code duplication and a
--  less convenient interface. The generic version also has the advantage
--  of being Pure, while this unit can only be Preelaborate.

--  This heapsort algorithm uses approximately N*log(N) compares in the
--  worst case and is in place with no additional storage required. See
--  the body for exact details of the algorithm used.

package GNAT.Heap_Sort is
pragma Preelaborate (Heap_Sort);

   --  The data to be sorted is assumed to be indexed by integer values
   --  from 1 to N, where N is the number of items to be sorted.

   type Xchg_Procedure is access procedure (Op1, Op2 : Natural);
   --  A pointer to a procedure that exchanges the two data items whose
   --  index values are Op1 and Op2.

   type Lt_Function is access function (Op1, Op2 : Natural) return Boolean;
   --  A pointer to a function that compares two items and returns True if
   --  the item with index value Op1 is less than the item with Index value
   --  Op2, and False if the Op1 item is greater than the Op2 item. If
   --  the items are equal, then it does not matter if True or False is
   --  returned (but it is slightly more efficient to return False).

   procedure Sort (N : Natural; Xchg : Xchg_Procedure; Lt : Lt_Function);
   --  This procedures sorts items in the range from 1 to N into ascending
   --  order making calls to Lt to do required comparisons, and calls to
   --  Xchg to exchange items. The sort is not stable, that is the order
   --  of equal items in the input data set is not preserved.

end GNAT.Heap_Sort;
