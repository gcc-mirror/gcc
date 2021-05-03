------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            G N A T . L I S T S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2018-2021, Free Software Foundation, Inc.      --
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

with Ada.Unchecked_Deallocation;

package body GNAT.Lists is

   package body Doubly_Linked_Lists is
      procedure Delete_Node
        (L   : Doubly_Linked_List;
         Nod : Node_Ptr);
      pragma Inline (Delete_Node);
      --  Detach and delete node Nod from list L

      procedure Ensure_Circular (Head : Node_Ptr);
      pragma Inline (Ensure_Circular);
      --  Ensure that dummy head Head is circular with respect to itself

      procedure Ensure_Created (L : Doubly_Linked_List);
      pragma Inline (Ensure_Created);
      --  Verify that list L is created. Raise Not_Created if this is not the
      --  case.

      procedure Ensure_Full (L : Doubly_Linked_List);
      pragma Inline (Ensure_Full);
      --  Verify that list L contains at least one element. Raise List_Empty if
      --  this is not the case.

      procedure Ensure_Unlocked (L : Doubly_Linked_List);
      pragma Inline (Ensure_Unlocked);
      --  Verify that list L is unlocked. Raise Iterated if this is not the
      --  case.

      function Find_Node
        (Head : Node_Ptr;
         Elem : Element_Type) return Node_Ptr;
      pragma Inline (Find_Node);
      --  Travers a list indicated by dummy head Head to determine whethe there
      --  exists a node with element Elem. If such a node exists, return it,
      --  otherwise return null;

      procedure Free is
        new Ada.Unchecked_Deallocation
              (Doubly_Linked_List_Attributes, Doubly_Linked_List);

      procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Ptr);

      procedure Insert_Between
        (L     : Doubly_Linked_List;
         Elem  : Element_Type;
         Left  : Node_Ptr;
         Right : Node_Ptr);
      pragma Inline (Insert_Between);
      --  Insert element Elem between nodes Left and Right of list L

      function Is_Valid (Iter : Iterator) return Boolean;
      pragma Inline (Is_Valid);
      --  Determine whether iterator Iter refers to a valid element

      function Is_Valid
        (Nod  : Node_Ptr;
         Head : Node_Ptr) return Boolean;
      pragma Inline (Is_Valid);
      --  Determine whether node Nod is non-null and does not refer to dummy
      --  head Head, thus making it valid.

      procedure Lock (L : Doubly_Linked_List);
      pragma Inline (Lock);
      --  Lock all mutation functionality of list L

      function Present (Nod : Node_Ptr) return Boolean;
      pragma Inline (Present);
      --  Determine whether node Nod exists

      procedure Unlock (L : Doubly_Linked_List);
      pragma Inline (Unlock);
      --  Unlock all mutation functionality of list L

      ------------
      -- Append --
      ------------

      procedure Append
        (L    : Doubly_Linked_List;
         Elem : Element_Type)
      is
         Head : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Unlocked (L);

         --  Ensure that the dummy head of an empty list is circular with
         --  respect to itself.

         Head := L.Nodes'Access;
         Ensure_Circular (Head);

         --  Append the node by inserting it between the last node and the
         --  dummy head.

         Insert_Between
           (L     => L,
            Elem  => Elem,
            Left  => Head.Prev,
            Right => Head);
      end Append;

      ------------
      -- Create --
      ------------

      function Create return Doubly_Linked_List is
      begin
         return new Doubly_Linked_List_Attributes;
      end Create;

      --------------
      -- Contains --
      --------------

      function Contains
        (L    : Doubly_Linked_List;
         Elem : Element_Type) return Boolean
      is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created (L);

         Head := L.Nodes'Access;
         Nod  := Find_Node (Head, Elem);

         return Is_Valid (Nod, Head);
      end Contains;

      ------------
      -- Delete --
      ------------

      procedure Delete
        (L    : Doubly_Linked_List;
         Elem : Element_Type)
      is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Full     (L);
         Ensure_Unlocked (L);

         Head := L.Nodes'Access;
         Nod  := Find_Node (Head, Elem);

         if Is_Valid (Nod, Head) then
            Delete_Node (L, Nod);
         end if;
      end Delete;

      ------------------
      -- Delete_First --
      ------------------

      procedure Delete_First (L : Doubly_Linked_List) is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Full     (L);
         Ensure_Unlocked (L);

         Head := L.Nodes'Access;
         Nod  := Head.Next;

         if Is_Valid (Nod, Head) then
            Delete_Node (L, Nod);
         end if;
      end Delete_First;

      -----------------
      -- Delete_Last --
      -----------------

      procedure Delete_Last (L : Doubly_Linked_List) is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Full     (L);
         Ensure_Unlocked (L);

         Head := L.Nodes'Access;
         Nod  := Head.Prev;

         if Is_Valid (Nod, Head) then
            Delete_Node (L, Nod);
         end if;
      end Delete_Last;

      -----------------
      -- Delete_Node --
      -----------------

      procedure Delete_Node
        (L   : Doubly_Linked_List;
         Nod : Node_Ptr)
      is
         Ref : Node_Ptr := Nod;

         pragma Assert (Present (Ref));

         Next : constant Node_Ptr := Ref.Next;
         Prev : constant Node_Ptr := Ref.Prev;

      begin
         pragma Assert (Present (L));
         pragma Assert (Present (Next));
         pragma Assert (Present (Prev));

         Prev.Next := Next;  --  Prev ---> Next
         Next.Prev := Prev;  --  Prev <--> Next

         Ref.Next := null;
         Ref.Prev := null;

         L.Elements := L.Elements - 1;

         --  Invoke the element destructor before deallocating the node

         Destroy_Element (Nod.Elem);

         Free (Ref);
      end Delete_Node;

      -------------
      -- Destroy --
      -------------

      procedure Destroy (L : in out Doubly_Linked_List) is
         Head : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Unlocked (L);

         Head := L.Nodes'Access;

         while Is_Valid (Head.Next, Head) loop
            Delete_Node (L, Head.Next);
         end loop;

         Free (L);
      end Destroy;

      ---------------------
      -- Ensure_Circular --
      ---------------------

      procedure Ensure_Circular (Head : Node_Ptr) is
         pragma Assert (Present (Head));

      begin
         if not Present (Head.Next) and then not Present (Head.Prev) then
            Head.Next := Head;
            Head.Prev := Head;
         end if;
      end Ensure_Circular;

      --------------------
      -- Ensure_Created --
      --------------------

      procedure Ensure_Created (L : Doubly_Linked_List) is
      begin
         if not Present (L) then
            raise Not_Created;
         end if;
      end Ensure_Created;

      -----------------
      -- Ensure_Full --
      -----------------

      procedure Ensure_Full (L : Doubly_Linked_List) is
      begin
         pragma Assert (Present (L));

         if L.Elements = 0 then
            raise List_Empty;
         end if;
      end Ensure_Full;

      ---------------------
      -- Ensure_Unlocked --
      ---------------------

      procedure Ensure_Unlocked (L : Doubly_Linked_List) is
      begin
         pragma Assert (Present (L));

         --  The list has at least one outstanding iterator

         if L.Iterators > 0 then
            raise Iterated;
         end if;
      end Ensure_Unlocked;

      -----------
      -- Equal --
      -----------

      function Equal
        (Left  : Doubly_Linked_List;
         Right : Doubly_Linked_List) return Boolean
      is
         Left_Head  : Node_Ptr;
         Left_Nod   : Node_Ptr;
         Right_Head : Node_Ptr;
         Right_Nod  : Node_Ptr;

      begin
         --  Two non-existent lists are considered equal

         if Left = Nil and then Right = Nil then
            return True;

         --  A non-existent list is never equal to an already created list

         elsif Left = Nil or else Right = Nil then
            return False;

         --  The two lists must contain the same number of elements to be equal

         elsif Size (Left) /= Size (Right) then
            return False;
         end if;

         --  Compare the two lists element by element

         Left_Head  := Left.Nodes'Access;
         Left_Nod   := Left_Head.Next;
         Right_Head := Right.Nodes'Access;
         Right_Nod  := Right_Head.Next;
         while Is_Valid (Left_Nod,  Left_Head)
                 and then
               Is_Valid (Right_Nod, Right_Head)
         loop
            if Left_Nod.Elem /= Right_Nod.Elem then
               return False;
            end if;

            Left_Nod  := Left_Nod.Next;
            Right_Nod := Right_Nod.Next;
         end loop;

         return True;
      end Equal;

      ---------------
      -- Find_Node --
      ---------------

      function Find_Node
        (Head : Node_Ptr;
         Elem : Element_Type) return Node_Ptr
      is
         pragma Assert (Present (Head));

         Nod : Node_Ptr;

      begin
         --  Traverse the nodes of the list, looking for a matching element

         Nod := Head.Next;
         while Is_Valid (Nod, Head) loop
            if Nod.Elem = Elem then
               return Nod;
            end if;

            Nod := Nod.Next;
         end loop;

         return null;
      end Find_Node;

      -----------
      -- First --
      -----------

      function First (L : Doubly_Linked_List) return Element_Type is
      begin
         Ensure_Created (L);
         Ensure_Full    (L);

         return L.Nodes.Next.Elem;
      end First;

      --------------
      -- Has_Next --
      --------------

      function Has_Next (Iter : Iterator) return Boolean is
         Is_OK : constant Boolean := Is_Valid (Iter);

      begin
         --  The iterator is no longer valid which indicates that it has been
         --  exhausted. Unlock all mutation functionality of the list because
         --  the iterator cannot be advanced any further.

         if not Is_OK then
            Unlock (Iter.List);
         end if;

         return Is_OK;
      end Has_Next;

      ------------------
      -- Insert_After --
      ------------------

      procedure Insert_After
        (L     : Doubly_Linked_List;
         After : Element_Type;
         Elem  : Element_Type)
      is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Unlocked (L);

         Head := L.Nodes'Access;
         Nod  := Find_Node (Head, After);

         if Is_Valid (Nod, Head) then
            Insert_Between
              (L     => L,
               Elem  => Elem,
               Left  => Nod,
               Right => Nod.Next);
         end if;
      end Insert_After;

      -------------------
      -- Insert_Before --
      -------------------

      procedure Insert_Before
        (L      : Doubly_Linked_List;
         Before : Element_Type;
         Elem   : Element_Type)
      is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Unlocked (L);

         Head := L.Nodes'Access;
         Nod  := Find_Node (Head, Before);

         if Is_Valid (Nod, Head) then
            Insert_Between
              (L     => L,
               Elem  => Elem,
               Left  => Nod.Prev,
               Right => Nod);
         end if;
      end Insert_Before;

      --------------------
      -- Insert_Between --
      --------------------

      procedure Insert_Between
        (L     : Doubly_Linked_List;
         Elem  : Element_Type;
         Left  : Node_Ptr;
         Right : Node_Ptr)
      is
         pragma Assert (Present (L));
         pragma Assert (Present (Left));
         pragma Assert (Present (Right));

         Nod : constant Node_Ptr :=
                 new Node'(Elem => Elem,
                           Next => Right,  --  Left      Nod ---> Right
                           Prev => Left);  --  Left <--- Nod ---> Right

      begin
         Left.Next  := Nod;                --  Left <--> Nod ---> Right
         Right.Prev := Nod;                --  Left <--> Nod <--> Right

         L.Elements := L.Elements + 1;
      end Insert_Between;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty (L : Doubly_Linked_List) return Boolean is
      begin
         Ensure_Created (L);

         return L.Elements = 0;
      end Is_Empty;

      --------------
      -- Is_Valid --
      --------------

      function Is_Valid (Iter : Iterator) return Boolean is
      begin
         --  The invariant of Iterate and Next ensures that the iterator always
         --  refers to a valid node if there exists one.

         return Is_Valid (Iter.Curr_Nod, Iter.List.Nodes'Access);
      end Is_Valid;

      --------------
      -- Is_Valid --
      --------------

      function Is_Valid
        (Nod  : Node_Ptr;
         Head : Node_Ptr) return Boolean
      is
      begin
         --  A node is valid if it is non-null, and does not refer to the dummy
         --  head of some list.

         return Present (Nod) and then Nod /= Head;
      end Is_Valid;

      -------------
      -- Iterate --
      -------------

      function Iterate (L : Doubly_Linked_List) return Iterator is
      begin
         Ensure_Created (L);

         --  Lock all mutation functionality of the list while it is being
         --  iterated on.

         Lock (L);

         return (List => L, Curr_Nod => L.Nodes.Next);
      end Iterate;

      ----------
      -- Last --
      ----------

      function Last (L : Doubly_Linked_List) return Element_Type is
      begin
         Ensure_Created (L);
         Ensure_Full   (L);

         return L.Nodes.Prev.Elem;
      end Last;

      ----------
      -- Lock --
      ----------

      procedure Lock (L : Doubly_Linked_List) is
      begin
         pragma Assert (Present (L));

         --  The list may be locked multiple times if multiple iterators are
         --  operating over it.

         L.Iterators := L.Iterators + 1;
      end Lock;

      ----------
      -- Next --
      ----------

      procedure Next
        (Iter : in out Iterator;
         Elem : out Element_Type)
      is
         Is_OK : constant Boolean  := Is_Valid (Iter);
         Saved : constant Node_Ptr := Iter.Curr_Nod;

      begin
         --  The iterator is no linger valid which indicates that it has been
         --  exhausted. Unlock all mutation functionality of the list as the
         --  iterator cannot be advanced any further.

         if not Is_OK then
            Unlock (Iter.List);
            raise Iterator_Exhausted;
         end if;

         --  Advance to the next node along the list

         Iter.Curr_Nod := Iter.Curr_Nod.Next;

         Elem := Saved.Elem;
      end Next;

      -------------
      -- Prepend --
      -------------

      procedure Prepend
        (L    : Doubly_Linked_List;
         Elem : Element_Type)
      is
         Head : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Unlocked (L);

         --  Ensure that the dummy head of an empty list is circular with
         --  respect to itself.

         Head := L.Nodes'Access;
         Ensure_Circular (Head);

         --  Append the node by inserting it between the dummy head and the
         --  first node.

         Insert_Between
           (L     => L,
            Elem  => Elem,
            Left  => Head,
            Right => Head.Next);
      end Prepend;

      -------------
      -- Present --
      -------------

      function Present (L : Doubly_Linked_List) return Boolean is
      begin
         return L /= Nil;
      end Present;

      -------------
      -- Present --
      -------------

      function Present (Nod : Node_Ptr) return Boolean is
      begin
         return Nod /= null;
      end Present;

      -------------
      -- Replace --
      -------------

      procedure Replace
        (L        : Doubly_Linked_List;
         Old_Elem : Element_Type;
         New_Elem : Element_Type)
      is
         Head : Node_Ptr;
         Nod  : Node_Ptr;

      begin
         Ensure_Created  (L);
         Ensure_Unlocked (L);

         Head := L.Nodes'Access;
         Nod  := Find_Node (Head, Old_Elem);

         if Is_Valid (Nod, Head) then
            Nod.Elem := New_Elem;
         end if;
      end Replace;

      ----------
      -- Size --
      ----------

      function Size (L : Doubly_Linked_List) return Natural is
      begin
         Ensure_Created (L);

         return L.Elements;
      end Size;

      ------------
      -- Unlock --
      ------------

      procedure Unlock (L : Doubly_Linked_List) is
      begin
         pragma Assert (Present (L));

         --  The list may be locked multiple times if multiple iterators are
         --  operating over it.

         L.Iterators := L.Iterators - 1;
      end Unlock;
   end Doubly_Linked_Lists;

end GNAT.Lists;
