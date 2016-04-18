------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.UNBOUNDED_PRIORITY_QUEUES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011-2015, Free Software Foundation, Inc.       --
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

   package body Implementation is

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Before_Or_Equal (X, Y : Queue_Priority) return Boolean;
      --  True if X is before or equal to Y. Equal means both Before(X,Y) and
      --  Before(Y,X) are False.

      procedure Free is
        new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

      ---------------------
      -- Before_Or_Equal --
      ---------------------

      function Before_Or_Equal (X, Y : Queue_Priority) return Boolean is
      begin
         return (if Before (X, Y) then True else not Before (Y, X));
      end Before_Or_Equal;

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue
        (List    : in out List_Type;
         Element : out Queue_Interfaces.Element_Type)
      is
         H : constant Node_Access := List.Header'Unchecked_Access;
         pragma Assert (List.Length /= 0);
         pragma Assert (List.Header.Next /= H);
         --  List can't be empty; see the barrier

         pragma Assert
           (List.Header.Next.Next = H or else
            Before_Or_Equal (Get_Priority (List.Header.Next.Element),
                             Get_Priority (List.Header.Next.Next.Element)));
         --  The first item is before-or-equal to the second

         pragma Assert
           (List.Header.Next.Next_Unequal = H or else
            Before (Get_Priority (List.Header.Next.Element),
                    Get_Priority (List.Header.Next.Next_Unequal.Element)));
         --  The first item is before its Next_Unequal item

         --  The highest-priority item is always first; just remove it and
         --  return that element.

         X : Node_Access := List.Header.Next;

      --  Start of processing for Dequeue

      begin
         Element := X.Element;
         X.Next.Prev := H;
         List.Header.Next := X.Next;
         List.Header.Next_Unequal := X.Next;
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

         Success := List.Length > 0
           and then
             not Before (At_Least, Get_Priority (List.Header.Next.Element));

         if Success then
            List.Dequeue (Element);
         end if;
      end Dequeue;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue
        (List     : in out List_Type;
         New_Item : Queue_Interfaces.Element_Type)
      is
         P : constant Queue_Priority := Get_Priority (New_Item);
         H : constant Node_Access := List.Header'Unchecked_Access;

         function Next return Node_Access;
         --  The node before which we wish to insert the new node

         ----------
         -- Next --
         ----------

         function Next return Node_Access is
         begin
            return Result : Node_Access := H.Next_Unequal do
               while Result /= H
                 and then not Before (P, Get_Priority (Result.Element))
               loop
                  Result := Result.Next_Unequal;
               end loop;
            end return;
         end Next;

         --  Local varaibles

         Prev : constant Node_Access := Next.Prev;
         --  The node after which we wish to insert the new node. So Prev must
         --  be the header, or be higher or equal priority to the new item.
         --  Prev.Next must be the header, or be lower priority than the
         --  new item.

         pragma Assert
           (Prev = H or else Before_Or_Equal (Get_Priority (Prev.Element), P));
         pragma Assert
           (Prev.Next = H
              or else Before (P, Get_Priority (Prev.Next.Element)));
         pragma Assert (Prev.Next = Prev.Next_Unequal);

         Node : constant Node_Access :=
                  new Node_Type'(New_Item,
                                 Prev         => Prev,
                                 Next         => Prev.Next,
                                 Next_Unequal => Prev.Next);

      --  Start of processing for Enqueue

      begin
         Prev.Next.Prev := Node;
         Prev.Next := Node;

         if Prev = H then

            --  Make sure Next_Unequal of the Header always points to the first
            --  "real" node. Here, we've inserted a new first "real" node, so
            --  must update.

            List.Header.Next_Unequal := Node;
         end if;

         pragma Assert (List.Header.Next_Unequal = List.Header.Next);

         List.Length := List.Length + 1;

         if List.Length > List.Max_Length then
            List.Max_Length := List.Length;
         end if;
      end Enqueue;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (List : in out List_Type) is
         Ignore : Queue_Interfaces.Element_Type;
      begin
         while List.Length > 0 loop
            List.Dequeue (Ignore);
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
