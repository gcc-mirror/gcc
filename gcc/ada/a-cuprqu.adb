------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.UNBOUNDED_PRIORITY_QUEUES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2014, Free Software Foundation, Inc.       --
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

package body Ada.Containers.Unbounded_Priority_Queues is

   pragma Annotate (CodePeer, Skip_Analysis);

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

      procedure Dequeue
        (List     : in out List_Type;
         At_Least : Queue_Priority;
         Element  : in out Queue_Interfaces.Element_Type;
         Success  : out Boolean)
      is
      begin
         --  This operation dequeues a high priority item if it exists in the
         --  queue. By "high priority" we mean an item whose priority is equal
         --  or greater than the value At_Least. The generic formal operation
         --  Before has the meaning "has higher priority than". To dequeue an
         --  item (meaning that we return True as our Success value), we need
         --  as our predicate the equivalent of "has equal or higher priority
         --  than", but we cannot say that directly, so we require some logical
         --  gymnastics to make it so.

         --  If E is the element at the head of the queue, and symbol ">"
         --  refers to the "is higher priority than" function Before, then we
         --  derive our predicate as follows:
         --    original: P(E) >= At_Least
         --    same as:  not (P(E) < At_Least)
         --    same as:  not (At_Least > P(E))
         --    same as:  not Before (At_Least, P(E))

         --  But that predicate needs to be true in order to successfully
         --  dequeue an item. If it's false, it means no item is dequeued, and
         --  we return False as the Success value.

         if List.Length = 0
           or else Before (At_Least, Get_Priority (List.First.Element))
         then
            Success := False;
            return;
         end if;

         List.Dequeue (Element);
         Success := True;
      end Dequeue;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue
        (List     : in out List_Type;
         New_Item : Queue_Interfaces.Element_Type)
      is
         P : constant Queue_Priority := Get_Priority (New_Item);

         Node : Node_Access;
         Prev : Node_Access;

      begin
         Node := new Node_Type'(New_Item, null);

         if List.First = null then
            List.First := Node;
            List.Last := List.First;

         else
            Prev := List.First;

            if Before (P, Get_Priority (Prev.Element)) then
               Node.Next := List.First;
               List.First := Node;

            else
               while Prev.Next /= null loop
                  if Before (P, Get_Priority (Prev.Next.Element)) then
                     Node.Next := Prev.Next;
                     Prev.Next := Node;

                     exit;
                  end if;

                  Prev := Prev.Next;
               end loop;

               if Prev.Next = null then
                  List.Last.Next := Node;
                  List.Last := Node;
               end if;
            end if;
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

      --------------------------------
      -- Dequeue_Only_High_Priority --
      --------------------------------

      procedure Dequeue_Only_High_Priority
        (At_Least : Queue_Priority;
         Element  : in out Queue_Interfaces.Element_Type;
         Success  : out Boolean)
      is
      begin
         List.Dequeue (At_Least, Element, Success);
      end Dequeue_Only_High_Priority;

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

end Ada.Containers.Unbounded_Priority_Queues;
