------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.UNBOUNDED_PRIORITY_QUEUES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2018, Free Software Foundation, Inc.       --
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

package body Ada.Containers.Unbounded_Priority_Queues is

   protected body Queue is

      -----------------
      -- Current_Use --
      -----------------

      function Current_Use return Count_Type is
      begin
         return Q_Elems.Length;
      end Current_Use;

      -------------
      -- Dequeue --
      -------------

      entry Dequeue (Element : out Queue_Interfaces.Element_Type)
        when Q_Elems.Length > 0
      is
         --  Grab the first item of the set, and remove it from the set

         C : constant Cursor := First (Q_Elems);
      begin
         Element := Sets.Element (C).Item;
         Delete_First (Q_Elems);
      end Dequeue;

      --------------------------------
      -- Dequeue_Only_High_Priority --
      --------------------------------

      procedure Dequeue_Only_High_Priority
        (At_Least : Queue_Priority;
         Element  : in out Queue_Interfaces.Element_Type;
         Success  : out Boolean)
      is
         --  Grab the first item. If it exists and has appropriate priority,
         --  set Success to True, and remove that item. Otherwise, set Success
         --  to False.

         C : constant Cursor := First (Q_Elems);
      begin
         Success := Has_Element (C) and then
            not Before (At_Least, Get_Priority (Sets.Element (C).Item));

         if Success then
            Element := Sets.Element (C).Item;
            Delete_First (Q_Elems);
         end if;
      end Dequeue_Only_High_Priority;

      -------------
      -- Enqueue --
      -------------

      entry Enqueue (New_Item : Queue_Interfaces.Element_Type) when True is
      begin
         Insert (Q_Elems, (Next_Sequence_Number, New_Item));
         Next_Sequence_Number := Next_Sequence_Number + 1;

         --  If we reached a new high-water mark, increase Max_Length

         if Q_Elems.Length > Max_Length then
            pragma Assert (Max_Length + 1 = Q_Elems.Length);
            Max_Length := Q_Elems.Length;
         end if;
      end Enqueue;

      --------------
      -- Peak_Use --
      --------------

      function Peak_Use return Count_Type is
      begin
         return Max_Length;
      end Peak_Use;

   end Queue;

end Ada.Containers.Unbounded_Priority_Queues;
