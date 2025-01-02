------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               ADA.CONTAINERS.UNBOUNDED_SYNCHRONIZED_QUEUES               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2025, Free Software Foundation, Inc.       --
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

with Ada.Unchecked_Deallocation;

package body Ada.Containers.Unbounded_Synchronized_Queues is

   package body Implementation is

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Free is
         new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue
        (List    : in out List_Type;
         Element : out Queue_Interfaces.Element_Type)
      is
         X : Node_Access;

      begin
         Element := List.First.Element;

         X := List.First;
         List.First := List.First.Next;

         if List.First = null then
            List.Last := null;
         end if;

         List.Length := List.Length - 1;

         Free (X);
      end Dequeue;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue
        (List     : in out List_Type;
         New_Item : Queue_Interfaces.Element_Type)
      is
         Node : Node_Access;

      begin
         Node := new Node_Type'(New_Item, null);

         if List.First = null then
            List.First := Node;
            List.Last := List.First;

         else
            List.Last.Next := Node;
            List.Last := Node;
         end if;

         List.Length := List.Length + 1;

         if List.Length > List.Max_Length then
            List.Max_Length := List.Length;
         end if;
      end Enqueue;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (List : in out List_Type) is
         X : Node_Access;

      begin
         while List.First /= null loop
            X := List.First;
            List.First := List.First.Next;
            Free (X);
         end loop;
      end Finalize;

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

      entry Enqueue (New_Item : Queue_Interfaces.Element_Type) when True is
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

end Ada.Containers.Unbounded_Synchronized_Queues;
