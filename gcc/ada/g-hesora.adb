------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     G N A T . H E A P _ S O R T _ A                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1995-1999 Ada Core Technologies, Inc.            --
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

package body GNAT.Heap_Sort_A is

   ----------
   -- Sort --
   ----------

   --  We are using the classical heapsort algorithm (i.e. Floyd's Treesort3)
   --  as described by Knuth ("The Art of Programming", Volume III, first
   --  edition, section 5.2.3, p. 145-147) with the modification that is
   --  mentioned in exercise 18. For more details on this algorithm, see
   --  Robert B. K. Dewar PhD thesis "The use of Computers in the X-ray
   --  Phase Problem". University of Chicago, 1968, which was the first
   --  publication of the modification, which reduces the number of compares
   --  from 2NlogN to NlogN.

   procedure Sort (N : Natural; Move : Move_Procedure; Lt : Lt_Function) is

      Max : Natural := N;
      --  Current Max index in tree being sifted

      procedure Sift (S : Positive);
      --  This procedure sifts up node S, i.e. converts the subtree rooted
      --  at node S into a heap, given the precondition that any sons of
      --  S are already heaps. On entry, the contents of node S is found
      --  in the temporary (index 0), the actual contents of node S on
      --  entry are irrelevant. This is just a minor optimization to avoid
      --  what would otherwise be two junk moves in phase two of the sort.

      procedure Sift (S : Positive) is
         C      : Positive := S;
         Son    : Positive;
         Father : Positive;

      begin
         --  This is where the optimization is done, normally we would do a
         --  comparison at each stage between the current node and the larger
         --  of the two sons, and continue the sift only if the current node
         --  was less than this maximum. In this modified optimized version,
         --  we assume that the current node will be less than the larger
         --  son, and unconditionally sift up. Then when we get to the bottom
         --  of the tree, we check parents to make sure that we did not make
         --  a mistake. This roughly cuts the number of comparisions in half,
         --  since it is almost always the case that our assumption is correct.

         --  Loop to pull up larger sons

         loop
            Son := 2 * C;
            exit when Son > Max;

            if Son < Max and then Lt (Son, Son + 1) then
               Son := Son + 1;
            end if;

            Move (Son, C);
            C := Son;
         end loop;

         --  Loop to check fathers

         while C /= S loop
            Father := C / 2;

            if Lt (Father, 0) then
               Move (Father, C);
               C := Father;
            else
               exit;
            end if;
         end loop;

         --  Last step is to pop the sifted node into place

         Move (0, C);
      end Sift;

   --  Start of processing for Sort

   begin
      --  Phase one of heapsort is to build the heap. This is done by
      --  sifting nodes N/2 .. 1 in sequence.

      for J in reverse 1 .. N / 2 loop
         Move (J, 0);
         Sift (J);
      end loop;

      --  In phase 2, the largest node is moved to end, reducing the size
      --  of the tree by one, and the displaced node is sifted down from
      --  the top, so that the largest node is again at the top.

      while Max > 1 loop
         Move (Max, 0);
         Move (1, Max);
         Max := Max - 1;
         Sift (1);
      end loop;

   end Sort;

end GNAT.Heap_Sort_A;
