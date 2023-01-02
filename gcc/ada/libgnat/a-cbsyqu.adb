------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                ADA.CONTAINERS.BOUNDED_SYNCHRONIZED_QUEUES                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2023, Free Software Foundation, Inc.       --
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

package body Ada.Containers.Bounded_Synchronized_Queues with
  SPARK_Mode => Off
is

   package body Implementation is

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue
        (List    : in out List_Type;
         Element : out Queue_Interfaces.Element_Type)
      is
         EE : Element_Array renames List.Elements;

      begin
         Element := EE (List.First);
         List.Length := List.Length - 1;

         if List.Length = 0 then
            List.First := 0;
            List.Last := 0;

         elsif List.First <= List.Last then
            List.First := List.First + 1;

         else
            List.First := List.First + 1;

            if List.First > List.Capacity then
               List.First := 1;
            end if;
         end if;
      end Dequeue;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue
        (List     : in out List_Type;
         New_Item : Queue_Interfaces.Element_Type)
      is
      begin
         if List.Length >= List.Capacity then
            raise Capacity_Error with "No capacity for insertion";
         end if;

         if List.Length = 0 then
            List.Elements (1) := New_Item;
            List.First := 1;
            List.Last := 1;

         elsif List.First <= List.Last then
            if List.Last < List.Capacity then
               List.Elements (List.Last + 1) := New_Item;
               List.Last := List.Last + 1;

            else
               List.Elements (1) := New_Item;
               List.Last := 1;
            end if;

         else
            List.Elements (List.Last + 1) := New_Item;
            List.Last := List.Last + 1;
         end if;

         List.Length := List.Length + 1;

         if List.Length > List.Max_Length then
            List.Max_Length := List.Length;
         end if;
      end Enqueue;

      ------------
      -- Length --
      ------------

      function Length (List : List_Type) return Count_Type is
      begin
         return List.Length;
      end Length;

      ----------------
      -- Max_Length --
      ----------------

      function Max_Length (List : List_Type) return Count_Type is
      begin
         return List.Max_Length;
      end Max_Length;

   end Implementation;

   protected body Queue is

      -----------------
      -- Current_Use --
      -----------------

      function Current_Use return Count_Type is
      begin
         return List.Length;
      end Current_Use;

      -------------
      -- Dequeue --
      -------------

      entry Dequeue (Element : out Queue_Interfaces.Element_Type)
        when List.Length > 0
      is
      begin
         List.Dequeue (Element);
      end Dequeue;

      -------------
      -- Enqueue --
      -------------

      entry Enqueue (New_Item : Queue_Interfaces.Element_Type)
        when List.Length < Capacity
      is
      begin
         List.Enqueue (New_Item);
      end Enqueue;

      --------------
      -- Peak_Use --
      --------------

      function Peak_Use return Count_Type is
      begin
         return List.Max_Length;
      end Peak_Use;

   end Queue;

end Ada.Containers.Bounded_Synchronized_Queues;
