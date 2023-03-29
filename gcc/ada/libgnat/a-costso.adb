------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                         --
--                                                                          --
--         A D A . C O N T A I N E R S . S T A B L E _ S O R T I N G        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2023, AdaCore                     --
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

package body Ada.Containers.Stable_Sorting is
   package body List_Descriptors is
      procedure Doubly_Linked_List_Sort (List : List_Descriptor) is

         Empty  : constant List_Descriptor := (Nil, Nil, 0);

         function Merge_Sort (Arg : List_Descriptor) return List_Descriptor;
         --  Sort list of given length using MergeSort; length must be >= 2.
         --  As required by RM, the sort is stable.

         ----------------
         -- Merge_Sort --
         ----------------

         function Merge_Sort (Arg : List_Descriptor) return List_Descriptor
         is
            procedure Split_List
              (Unsplit : List_Descriptor; Part1, Part2 : out List_Descriptor);
            --  Split list into two parts for divide-and-conquer.
            --  Unsplit.Length must be >= 2.

            function Merge_Parts
              (Part1, Part2 : List_Descriptor) return List_Descriptor;
            --  Merge two sorted lists, preserving sorted property.

            ----------------
            -- Split_List --
            ----------------

            procedure Split_List
              (Unsplit : List_Descriptor; Part1, Part2 : out List_Descriptor)
            is
               Rover : Node_Ref := Unsplit.First;
               Bump_Count : constant Count_Type := (Unsplit.Length - 1) / 2;
            begin
               for Iter in 1 .. Bump_Count loop
                  Rover := Next (Rover);
               end loop;

               Part1 := (First  => Unsplit.First,
                         Last   => Rover,
                         Length => Bump_Count + 1);

               Part2 := (First => Next (Rover),
                         Last  => Unsplit.Last,
                         Length => Unsplit.Length - Part1.Length);

               --  Detach
               Set_Next (Part1.Last, Nil);
               Set_Prev (Part2.First, Nil);
            end Split_List;

            -----------------
            -- Merge_Parts --
            -----------------

            function Merge_Parts
              (Part1, Part2 : List_Descriptor) return List_Descriptor
            is
               procedure Detach_First (Source   : in out List_Descriptor;
                                       Detached : out Node_Ref);
               --  Detach the first element from a non-empty list and
               --  return the detached node via the Detached parameter.

               ------------------
               -- Detach_First --
               ------------------

               procedure Detach_First (Source   : in out List_Descriptor;
                                       Detached : out Node_Ref) is
               begin
                  Detached := Source.First;

                  if Source.Length = 1 then
                     Source := Empty;
                  else
                     Source := (Next (Source.First),
                                Source.Last,
                                Source.Length - 1);

                     Set_Prev (Next (Detached), Nil);
                     Set_Next (Detached, Nil);
                  end if;
               end Detach_First;

               P1     : List_Descriptor := Part1;
               P2     : List_Descriptor := Part2;
               Merged : List_Descriptor := Empty;

               Take_From_P2 : Boolean;
               Detached     : Node_Ref;

            --  Start of processing for Merge_Parts

            begin
               while (P1.Length /= 0) or (P2.Length /= 0) loop
                  if P1.Length = 0 then
                     Take_From_P2 := True;
                  elsif P2.Length = 0 then
                     Take_From_P2 := False;
                  else
                     --  If the compared elements are equal then Take_From_P2
                     --  must be False in order to ensure stability.

                     Take_From_P2 := P2.First < P1.First;
                  end if;

                  if Take_From_P2 then
                     Detach_First (P2, Detached);
                  else
                     Detach_First (P1, Detached);
                  end if;

                  if Merged.Length = 0 then
                     Merged := (First | Last => Detached, Length => 1);
                  else
                     Set_Prev (Detached, Merged.Last);
                     Set_Next (Merged.Last, Detached);
                     Merged.Last := Detached;
                     Merged.Length := Merged.Length + 1;
                  end if;
               end loop;
               return Merged;
            end Merge_Parts;

         --  Start of processing for Merge_Sort

         begin
            if Positive (Arg.Length) < 2 then
               --  already sorted
               return Arg;
            end if;

            declare
               Part1, Part2 : List_Descriptor;
            begin
               Split_List (Unsplit => Arg, Part1 => Part1, Part2 => Part2);

               Part1 := Merge_Sort (Part1);
               Part2 := Merge_Sort (Part2);

               return Merge_Parts (Part1, Part2);
            end;
         end Merge_Sort;

      --  Start of processing for Sort

      begin
         if List.Length > 1 then
            --  If a call to the formal "<" op references the container
            --  during sorting, seeing an empty container seems preferable
            --  to seeing an internally inconsistent container.
            --
            Update_Container (Empty);

            Update_Container (Merge_Sort (List));
         end if;
      end Doubly_Linked_List_Sort;
   end List_Descriptors;
end Ada.Containers.Stable_Sorting;
