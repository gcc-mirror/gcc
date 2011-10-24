------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                  ADA.CONTAINERS.BOUNDED_PRIORITY_QUEUES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2011, Free Software Foundation, Inc.            --
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

package body Ada.Containers.Bounded_Priority_Queues is

   package body Implementation is

      -------------
      -- Dequeue --
      -------------

      procedure Dequeue
        (List    : in out List_Type;
         Element : out Queue_Interfaces.Element_Type)
      is
      begin
         Element := List.Container.First_Element;
         List.Container.Delete_First;
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
           or else Before (At_Least,
                           Get_Priority (List.Container.First_Element))
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

         C : List_Types.Cursor;
         use List_Types;

         Count : Count_Type;

      begin
         C := List.Container.First;
         while Has_Element (C) loop

            --  ??? why is following commented out ???
            --  if Before (P, Get_Priority (List.Constant_Reference (C))) then

            if Before (P, Get_Priority (Element (C))) then
               List.Container.Insert (C, New_Item);
               exit;
            end if;

            Next (C);
         end loop;

         if not Has_Element (C) then
            List.Container.Append (New_Item);
         end if;

         Count := List.Container.Length;

         if Count > List.Max_Length then
            List.Max_Length := Count;
         end if;
      end Enqueue;

      -------------------
      -- First_Element --
      -------------------

      function First_Element
        (List : List_Type) return Queue_Interfaces.Element_Type
      is
      begin

         --  Use Constant_Reference for this.  ???

         return List.Container.First_Element;
      end First_Element;

      ------------
      -- Length --
      ------------

      function Length (List : List_Type) return Count_Type is
      begin
         return List.Container.Length;
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

      ------------------
      --  Current_Use --
      ------------------

      function Current_Use return Count_Type is
      begin
         return List.Length;
      end Current_Use;

      --------------
      --  Dequeue --
      --------------

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

      --------------
      --  Enqueue --
      --------------

      entry Enqueue (New_Item : Queue_Interfaces.Element_Type)
        when List.Length < Capacity
      is
      begin
         List.Enqueue (New_Item);
      end Enqueue;

      ---------------
      --  Peak_Use --
      ---------------

      function Peak_Use return Count_Type is
      begin
         return List.Max_Length;
      end Peak_Use;

   end Queue;

end Ada.Containers.Bounded_Priority_Queues;
