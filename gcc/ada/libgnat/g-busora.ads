------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   G N A T . B U B B L E _ S O R T _ A                    --
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

--  Bubblesort using access to procedure parameters

--  This package provides a bubble sort routine that works with access to
--  subprogram parameters, so that it can be used with different types with
--  shared sorting code. It is considered obsoleted by GNAT.Bubble_Sort which
--  offers a similar routine with a more convenient interface.

package GNAT.Bubble_Sort_A is
   pragma Preelaborate;

   --  The data to be sorted is assumed to be indexed by integer values from
   --  1 to N, where N is the number of items to be sorted. In addition, the
   --  index value zero is used for a temporary location used during the sort.

   type Move_Procedure is access procedure (From : Natural; To : Natural);
   --  A pointer to a procedure that moves the data item with index From to
   --  the data item with index To. An index value of zero is used for moves
   --  from and to the single temporary location used by the sort.

   type Lt_Function is access function (Op1, Op2 : Natural) return Boolean;
   --  A pointer to a function that compares two items and returns True if
   --  the item with index Op1 is less than the item with index Op2, and False
   --  if the Op2 item is greater than or equal to the Op1 item.

   procedure Sort (N : Natural; Move : Move_Procedure; Lt : Lt_Function);
   --  This procedures sorts items in the range from 1 to N into ascending
   --  order making calls to Lt to do required comparisons, and Move to move
   --  items around. Note that, as described above, both Move and Lt use a
   --  single temporary location with index value zero. This sort is not
   --  stable, i.e. the order of equal elements in the input is not preserved.

end GNAT.Bubble_Sort_A;
