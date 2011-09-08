------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                  ADA.CONTAINERS.BOUNDED_PRIORITY_QUEUES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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
            --  ???
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

      --  ???
      --  entry Dequeue_Only_High_Priority
      --    (Low_Priority : Queue_Priority;
      --     Element      : out Queue_Interfaces.Element_Type) when True
      --  is
      --  begin
      --     null;
      --  end Dequeue_Only_High_Priority;

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
