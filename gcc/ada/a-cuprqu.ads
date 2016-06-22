------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.UNBOUNDED_PRIORITY_QUEUES                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2016, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with System;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;

generic
   with package Queue_Interfaces is
     new Ada.Containers.Synchronized_Queue_Interfaces (<>);

   type Queue_Priority is private;

   with function Get_Priority
     (Element : Queue_Interfaces.Element_Type) return Queue_Priority is <>;

   with function Before
     (Left, Right : Queue_Priority) return Boolean is <>;

   Default_Ceiling : System.Any_Priority := System.Priority'Last;

package Ada.Containers.Unbounded_Priority_Queues is
   pragma Annotate (CodePeer, Skip_Analysis);
   pragma Preelaborate;

   package Implementation is

      --  All identifiers in this unit are implementation defined

      pragma Implementation_Defined;

      --  We use an ordered set to hold the queue elements. This gives O(lg N)
      --  performance in the worst case for Enqueue and Dequeue.
      --  Sequence_Number is used to distinguish equivalent items. Each Enqueue
      --  uses a higher Sequence_Number, so that a new item is placed after
      --  already-enqueued equivalent items.
      --
      --  At any time, the first set element is the one to be dequeued next (if
      --  the queue is not empty).

      type Set_Elem is record
         Sequence_Number : Count_Type;
         Item : Queue_Interfaces.Element_Type;
      end record;

      function "=" (X, Y : Queue_Interfaces.Element_Type) return Boolean is
         (not Before (Get_Priority (X), Get_Priority (Y))
            and then not Before (Get_Priority (Y), Get_Priority (X)));
      --  Elements are equal if neither is Before the other

      function "=" (X, Y : Set_Elem) return Boolean is
         (X.Sequence_Number = Y.Sequence_Number and then X.Item = Y.Item);
      --  Set_Elems are equal if the elements are equal, and the
      --  Sequence_Numbers are equal. This is passed to Ordered_Sets.

      function "<" (X, Y : Set_Elem) return Boolean is
         (if X.Item = Y.Item
            then X.Sequence_Number < Y.Sequence_Number
            else Before (Get_Priority (X.Item), Get_Priority (Y.Item)));
      --  If the items are equal, Sequence_Number breaks the tie. Otherwise,
      --  use Before. This is passed to Ordered_Sets.

      pragma Suppress (Container_Checks);
      package Sets is new Ada.Containers.Ordered_Sets (Set_Elem);

   end Implementation;

   use Implementation, Implementation.Sets;

   protected type Queue (Ceiling : System.Any_Priority := Default_Ceiling)
   with
     Priority => Ceiling
   is new Queue_Interfaces.Queue with

      overriding entry Enqueue (New_Item : Queue_Interfaces.Element_Type);

      overriding entry Dequeue (Element : out Queue_Interfaces.Element_Type);

      --  The priority queue operation Dequeue_Only_High_Priority had been a
      --  protected entry in early drafts of AI05-0159, but it was discovered
      --  that that operation as specified was not in fact implementable. The
      --  operation was changed from an entry to a protected procedure per the
      --  ARG meeting in Edinburgh (June 2011), with a different signature and
      --  semantics.

      procedure Dequeue_Only_High_Priority
        (At_Least : Queue_Priority;
         Element  : in out Queue_Interfaces.Element_Type;
         Success  : out Boolean);

      overriding function Current_Use return Count_Type;

      overriding function Peak_Use return Count_Type;

   private
      Q_Elems              : Set;
      --  Elements of the queue

      Max_Length           : Count_Type := 0;
      --  The current length of the queue is the Length of Q_Elems. This is the
      --  maximum value of that, so far. Updated by Enqueue.

      Next_Sequence_Number : Count_Type := 0;
      --  Steadily increasing counter
   end Queue;

end Ada.Containers.Unbounded_Priority_Queues;
