------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                    ADA.CONTAINERS.DOUBLY_LINKED_LISTS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2004 Free Software Foundation, Inc.            --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

with System;  use type System.Address;
with Ada.Unchecked_Deallocation;

package body Ada.Containers.Doubly_Linked_Lists is

   procedure Free is
     new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Delete_Node
     (Container : in out List;
      Node      : in out Node_Access);

   procedure Insert_Internal
     (Container : in out List;
      Before    : Node_Access;
      New_Node  : Node_Access);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : List) return Boolean is
      L : Node_Access := Left.First;
      R : Node_Access := Right.First;

   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Length /= Right.Length then
         return False;
      end if;

      for J in 1 .. Left.Length loop
         if L.Element /= R.Element then
            return False;
         end if;

         L := L.Next;
         R := R.Next;
      end loop;

      return True;
   end "=";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Container : in out List) is
      Src    : Node_Access := Container.First;
      Length : constant Count_Type := Container.Length;

   begin
      if Src = null then
         pragma Assert (Container.Last = null);
         pragma Assert (Length = 0);
         return;
      end if;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);
      pragma Assert (Length > 0);

      Container.First := null;
      Container.Last := null;
      Container.Length := 0;

      Container.First := new Node_Type'(Src.Element, null, null);

      Container.Last := Container.First;
      loop
         Container.Length := Container.Length + 1;
         Src := Src.Next;
         exit when Src = null;
         Container.Last.Next := new Node_Type'(Element => Src.Element,
                                               Prev    => Container.Last,
                                               Next    => null);
         Container.Last := Container.Last.Next;
      end loop;

      pragma Assert (Container.Length = Length);
   end Adjust;

   ------------
   -- Append --
   ------------

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
   begin
      Insert (Container, No_Element, New_Item, Count);
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Container : in out List) is
   begin
      Delete_Last (Container, Count => Container.Length);
   end Clear;

   --------------
   -- Continue --
   --------------

   function Contains
     (Container : List;
      Item      : Element_Type) return Boolean
   is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out List;
      Position  : in out Cursor;
      Count     : Count_Type := 1)
   is
   begin
      if Position = No_Element then
         return;
      end if;

      if Position.Container /= List_Access'(Container'Unchecked_Access) then
         raise Program_Error;
      end if;

      for Index in 1 .. Count loop
         Delete_Node (Container, Position.Node);

         if Position.Node = null then
            Position.Container := null;
            return;
         end if;
      end loop;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Container : in out List;
      Count     : Count_Type := 1)
   is
      Node : Node_Access := Container.First;
   begin
      for J in 1 .. Count_Type'Min (Count, Container.Length) loop
         Delete_Node (Container, Node);
      end loop;
   end Delete_First;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last
     (Container : in out List;
      Count     : Count_Type := 1)
   is
      Node : Node_Access;
   begin
      for J in 1 .. Count_Type'Min (Count, Container.Length) loop
         Node := Container.Last;
         Delete_Node (Container, Node);
      end loop;
   end Delete_Last;

   -----------------
   -- Delete_Node --
   -----------------

   procedure Delete_Node
     (Container : in out List;
      Node      : in out Node_Access)
   is
      X : Node_Access := Node;

   begin
      Node := X.Next;
      Container.Length := Container.Length - 1;

      if X = Container.First then
         Container.First := X.Next;

         if X = Container.Last then
            pragma Assert (Container.First = null);
            pragma Assert (Container.Length = 0);
            Container.Last := null;
         else
            pragma Assert (Container.Length > 0);
            Container.First.Prev := null;
         end if;

      elsif X = Container.Last then
         pragma Assert (Container.Length > 0);

         Container.Last := X.Prev;
         Container.Last.Next := null;

      else
         pragma Assert (Container.Length > 0);

         X.Next.Prev := X.Prev;
         X.Prev.Next := X.Next;
      end if;

      Free (X);
   end Delete_Node;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Node.Element;
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      Node : Node_Access := Position.Node;

   begin
      if Node = null then
         Node := Container.First;
      elsif Position.Container /= List_Access'(Container'Unchecked_Access) then
         raise Program_Error;
      end if;

      while Node /= null loop
         if Node.Element = Item then
            return Cursor'(Container'Unchecked_Access, Node);
         end if;

         Node := Node.Next;
      end loop;

      return No_Element;
   end Find;

   -----------
   -- First --
   -----------

   function First (Container : List) return Cursor is
   begin
      if Container.First = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Container.First);
   end First;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : List) return Element_Type is
   begin
      return Container.First.Element;
   end First_Element;

   -------------------
   -- Generic_Merge --
   -------------------

   procedure Generic_Merge
     (Target : in out List;
      Source : in out List)
   is
      LI : Cursor := First (Target);
      RI : Cursor := First (Source);

   begin
      if Target'Address = Source'Address then
         return;
      end if;

      while RI.Node /= null loop
         if LI.Node = null then
            Splice (Target, No_Element, Source);
            return;
         end if;

         if RI.Node.Element < LI.Node.Element then
            declare
               RJ : constant Cursor := RI;
            begin
               RI.Node := RI.Node.Next;
               Splice (Target, LI, Source, RJ);
            end;

         else
            LI.Node := LI.Node.Next;
         end if;
      end loop;
   end Generic_Merge;

   ------------------
   -- Generic_Sort --
   ------------------

   procedure Generic_Sort (Container : in out List) is

      procedure Partition
        (Pivot : in Node_Access;
         Back  : in Node_Access);

      procedure Sort (Front, Back : Node_Access);

      ---------------
      -- Partition --
      ---------------

      procedure Partition
        (Pivot : Node_Access;
         Back  : Node_Access)
      is
         Node : Node_Access := Pivot.Next;

      begin
         while Node /= Back loop
            if Node.Element < Pivot.Element then
               declare
                  Prev : constant Node_Access := Node.Prev;
                  Next : constant Node_Access := Node.Next;

               begin
                  Prev.Next := Next;

                  if Next = null then
                     Container.Last := Prev;
                  else
                     Next.Prev := Prev;
                  end if;

                  Node.Next := Pivot;
                  Node.Prev := Pivot.Prev;

                  Pivot.Prev := Node;

                  if Node.Prev = null then
                     Container.First := Node;
                  else
                     Node.Prev.Next := Node;
                  end if;

                  Node := Next;
               end;

            else
               Node := Node.Next;
            end if;
         end loop;
      end Partition;

      ----------
      -- Sort --
      ----------

      procedure Sort (Front, Back : Node_Access) is
         Pivot : Node_Access;

      begin
         if Front = null then
            Pivot := Container.First;
         else
            Pivot := Front.Next;
         end if;

         if Pivot /= Back then
            Partition (Pivot, Back);
            Sort (Front, Pivot);
            Sort (Pivot, Back);
         end if;
      end Sort;

   --  Start of processing for Generic_Sort

   begin
      Sort (Front => null, Back => null);

      pragma Assert (Container.Length = 0
                       or else
                         (Container.First.Prev = null
                            and then Container.Last.Next = null));
   end Generic_Sort;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Container /= null and then Position.Node /= null;
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      New_Node : Node_Access;

   begin
      if Before.Container /= null
        and then Before.Container /= List_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      New_Node := new Node_Type'(New_Item, null, null);
      Insert_Internal (Container, Before.Node, New_Node);

      Position := Cursor'(Before.Container, New_Node);

      for J in Count_Type'(2) .. Count loop
         New_Node := new Node_Type'(New_Item, null, null);
         Insert_Internal (Container, Before.Node, New_Node);
      end loop;
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
      Position : Cursor;
   begin
      Insert (Container, Before, New_Item, Position, Count);
   end Insert;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      Position  : out Cursor;
      Count     : Count_Type := 1)
   is
      New_Node : Node_Access;

   begin
      if Before.Container /= null
        and then Before.Container /= List_Access'(Container'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Count = 0 then
         Position := Before;
         return;
      end if;

      New_Node := new Node_Type;
      Insert_Internal (Container, Before.Node, New_Node);

      Position := Cursor'(Before.Container, New_Node);

      for J in Count_Type'(2) .. Count loop
         New_Node := new Node_Type;
         Insert_Internal (Container, Before.Node, New_Node);
      end loop;
   end Insert;

   ---------------------
   -- Insert_Internal --
   ---------------------

   procedure Insert_Internal
     (Container : in out List;
      Before    : Node_Access;
      New_Node  : Node_Access)
   is
   begin
      if Container.Length = 0 then
         pragma Assert (Before = null);
         pragma Assert (Container.First = null);
         pragma Assert (Container.Last = null);

         Container.First := New_Node;
         Container.Last := New_Node;

      elsif Before = null then
         pragma Assert (Container.Last.Next = null);

         Container.Last.Next := New_Node;
         New_Node.Prev := Container.Last;

         Container.Last := New_Node;

      elsif Before = Container.First then
         pragma Assert (Container.First.Prev = null);

         Container.First.Prev := New_Node;
         New_Node.Next := Container.First;

         Container.First := New_Node;

      else
         pragma Assert (Container.First.Prev = null);
         pragma Assert (Container.Last.Next = null);

         New_Node.Next := Before;
         New_Node.Prev := Before.Prev;

         Before.Prev.Next := New_Node;
         Before.Prev := New_Node;
      end if;

      Container.Length := Container.Length + 1;
   end Insert_Internal;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      Node : Node_Access := Container.First;
   begin
      while Node /= null loop
         Process (Cursor'(Container'Unchecked_Access, Node));
         Node := Node.Next;
      end loop;
   end Iterate;

   ----------
   -- Last --
   ----------

   function Last (Container : List) return Cursor is
   begin
      if Container.Last = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Container.Last);
   end Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Container : List) return Element_Type is
   begin
      return Container.Last.Element;
   end Last_Element;

   ------------
   -- Length --
   ------------

   function Length (Container : List) return Count_Type is
   begin
      return Container.Length;
   end Length;

   ----------
   -- Move --
   ----------

   procedure Move
     (Target : in out List;
      Source : in out List)
   is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Length > 0 then
         raise Constraint_Error;
      end if;

      Target.First := Source.First;
      Source.First := null;

      Target.Last := Source.Last;
      Source.Last := null;

      Target.Length := Source.Length;
      Source.Length := 0;
   end Move;

   ----------
   -- Next --
   ----------

   procedure Next (Position : in out Cursor) is
   begin
      if Position.Node = null then
         return;
      end if;

      Position.Node := Position.Node.Next;

      if Position.Node = null then
         Position.Container := null;
      end if;
   end Next;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node = null then
         return No_Element;
      end if;

      declare
         Next_Node : constant Node_Access := Position.Node.Next;
      begin
         if Next_Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Next_Node);
      end;
   end Next;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type := 1)
   is
   begin
      Insert (Container, First (Container), New_Item, Count);
   end Prepend;

   --------------
   -- Previous --
   --------------

   procedure Previous (Position : in out Cursor) is
   begin
      if Position.Node = null then
         return;
      end if;

      Position.Node := Position.Node.Prev;

      if Position.Node = null then
         Position.Container := null;
      end if;
   end Previous;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Node = null then
         return No_Element;
      end if;

      declare
         Prev_Node : constant Node_Access := Position.Node.Prev;
      begin
         if Prev_Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Prev_Node);
      end;
   end Previous;

   -------------------
   -- Query_Element --
   -------------------

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in Element_Type))
   is
   begin
      Process (Position.Node.Element);
   end Query_Element;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : access Root_Stream_Type'Class;
      Item   : out List)
   is
      N : Count_Type'Base;
      X : Node_Access;

   begin
      Clear (Item);  --  ???
      Count_Type'Base'Read (Stream, N);

      if N = 0 then
         return;
      end if;

      X := new Node_Type;

      begin
         Element_Type'Read (Stream, X.Element);
      exception
         when others =>
            Free (X);
            raise;
      end;

      Item.First := X;
      Item.Last := X;

      loop
         Item.Length := Item.Length + 1;
         exit when Item.Length = N;

         X := new Node_Type;

         begin
            Element_Type'Read (Stream, X.Element);
         exception
            when others =>
               Free (X);
               raise;
         end;

         X.Prev := Item.Last;
         Item.Last.Next := X;
         Item.Last := X;
      end loop;
   end Read;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Position : Cursor;
      By       : Element_Type)
   is
   begin
      Position.Node.Element := By;
   end Replace_Element;

   ------------------
   -- Reverse_Find --
   ------------------

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   is
      Node : Node_Access := Position.Node;

   begin
      if Node = null then
         Node := Container.Last;
      elsif Position.Container /= List_Access'(Container'Unchecked_Access) then
         raise Program_Error;
      end if;

      while Node /= null loop
         if Node.Element = Item then
            return Cursor'(Container'Unchecked_Access, Node);
         end if;

         Node := Node.Prev;
      end loop;

      return No_Element;
   end Reverse_Find;

   ---------------------
   -- Reverse_Iterate --
   ---------------------

   procedure Reverse_Iterate
     (Container : List;
      Process   : not null access procedure (Position : Cursor))
   is
      Node : Node_Access := Container.Last;
   begin
      while Node /= null loop
         Process (Cursor'(Container'Unchecked_Access, Node));
         Node := Node.Prev;
      end loop;
   end Reverse_Iterate;

   ------------------
   -- Reverse_List --
   ------------------

   procedure Reverse_List (Container : in out List) is
      I : Node_Access := Container.First;
      J : Node_Access := Container.Last;

      procedure Swap (L, R : Node_Access);

      ----------
      -- Swap --
      ----------

      procedure Swap (L, R : Node_Access) is
         LN : constant Node_Access := L.Next;
         LP : constant Node_Access := L.Prev;

         RN : constant Node_Access := R.Next;
         RP : constant Node_Access := R.Prev;

      begin
         if LP /= null then
            LP.Next := R;
         end if;

         if RN /= null then
            RN.Prev := L;
         end if;

         L.Next := RN;
         R.Prev := LP;

         if LN = R then
            pragma Assert (RP = L);

            L.Prev := R;
            R.Next := L;

         else
            L.Prev := RP;
            RP.Next := L;

            R.Next := LN;
            LN.Prev := R;
         end if;
      end Swap;

   --  Start of processing for Reverse_List

   begin
      if Container.Length <= 1 then
         return;
      end if;

      Container.First := J;
      Container.Last := I;
      loop
         Swap (L => I, R => J);

         J := J.Next;
         exit when I = J;

         I := I.Prev;
         exit when I = J;

         Swap (L => J, R => I);

         I := I.Next;
         exit when I = J;

         J := J.Prev;
         exit when I = J;
      end loop;

      pragma Assert (Container.First.Prev = null);
      pragma Assert (Container.Last.Next = null);
   end Reverse_List;

   ------------
   -- Splice --
   ------------

   procedure Splice
     (Target : in out List;
      Before : Cursor;
      Source : in out List)
   is
   begin
      if Before.Container /= null
        and then Before.Container /= List_Access'(Target'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Target'Address = Source'Address
        or else Source.Length = 0
      then
         return;
      end if;

      if Target.Length = 0 then
         pragma Assert (Before = No_Element);

         Target.First := Source.First;
         Target.Last := Source.Last;

      elsif Before.Node = null then
         pragma Assert (Target.Last.Next = null);

         Target.Last.Next := Source.First;
         Source.First.Prev := Target.Last;

         Target.Last := Source.Last;

      elsif Before.Node = Target.First then
         pragma Assert (Target.First.Prev = null);

         Source.Last.Next := Target.First;
         Target.First.Prev := Source.Last;

         Target.First := Source.First;

      else
         Before.Node.Prev.Next := Source.First;
         Source.First.Prev := Before.Node.Prev;

         Before.Node.Prev := Source.Last;
         Source.Last.Next := Before.Node;
      end if;

      Source.First := null;
      Source.Last := null;

      Target.Length := Target.Length + Source.Length;
      Source.Length := 0;
   end Splice;

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Position : Cursor)
   is
      X : Node_Access := Position.Node;

   begin
      if Before.Container /= null
        and then Before.Container /= List_Access'(Target'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Position.Container /= null
        and then Position.Container /= List_Access'(Target'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if X = null
        or else X = Before.Node
        or else X.Next = Before.Node
      then
         return;
      end if;

      pragma Assert (Target.Length > 0);

      if Before.Node = null then
         pragma Assert (X /= Target.Last);

         if X = Target.First then
            Target.First := X.Next;
            Target.First.Prev := null;
         else
            X.Prev.Next := X.Next;
            X.Next.Prev := X.Prev;
         end if;

         Target.Last.Next := X;
         X.Prev := Target.Last;

         Target.Last := X;
         Target.Last.Next := null;

         return;
      end if;

      if Before.Node = Target.First then
         pragma Assert (X /= Target.First);

         if X = Target.Last then
            Target.Last := X.Prev;
            Target.Last.Next := null;
         else
            X.Prev.Next := X.Next;
            X.Next.Prev := X.Prev;
         end if;

         Target.First.Prev := X;
         X.Next := Target.First;

         Target.First := X;
         Target.First.Prev := null;

         return;
      end if;

      if X = Target.First then
         Target.First := X.Next;
         Target.First.Prev := null;

      elsif X = Target.Last then
         Target.Last := X.Prev;
         Target.Last.Next := null;

      else
         X.Prev.Next := X.Next;
         X.Next.Prev := X.Prev;
      end if;

      Before.Node.Prev.Next := X;
      X.Prev := Before.Node.Prev;

      Before.Node.Prev := X;
      X.Next := Before.Node;
   end Splice;

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : Cursor)
   is
      X : Node_Access := Position.Node;

   begin
      if Target'Address = Source'Address then
         Splice (Target, Before, Position);
         return;
      end if;

      if Before.Container /= null
        and then Before.Container /= List_Access'(Target'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if Position.Container /= null
        and then Position.Container /= List_Access'(Source'Unchecked_Access)
      then
         raise Program_Error;
      end if;

      if X = null then
         return;
      end if;

      pragma Assert (Source.Length > 0);
      pragma Assert (Source.First.Prev = null);
      pragma Assert (Source.Last.Next = null);

      if X = Source.First then
         Source.First := X.Next;
         Source.First.Prev := null;

         if X = Source.Last then
            pragma Assert (Source.First = null);
            pragma Assert (Source.Length = 1);
            Source.Last := null;
         end if;

      elsif X = Source.Last then
         Source.Last := X.Prev;
         Source.Last.Next := null;

      else
         X.Prev.Next := X.Next;
         X.Next.Prev := X.Prev;
      end if;

      if Target.Length = 0 then
         pragma Assert (Before = No_Element);
         pragma Assert (Target.First = null);
         pragma Assert (Target.Last = null);

         Target.First := X;
         Target.Last := X;

      elsif Before.Node = null then
         Target.Last.Next := X;
         X.Next := Target.Last;

         Target.Last := X;
         Target.Last.Next := null;

      elsif Before.Node = Target.First then
         Target.First.Prev := X;
         X.Next := Target.First;

         Target.First := X;
         Target.First.Prev := null;

      else
         Before.Node.Prev.Next := X;
         X.Prev := Before.Node.Prev;

         Before.Node.Prev := X;
         X.Next := Before.Node;
      end if;

      Target.Length := Target.Length + 1;
      Source.Length := Source.Length - 1;
   end Splice;

   ----------
   -- Swap --
   ----------

   --  Is this defined when I and J designate elements in different containers,
   --  or should it raise an exception (Program_Error)???

   procedure Swap (I, J : in Cursor) is
      EI : constant Element_Type := I.Node.Element;
   begin
      I.Node.Element := J.Node.Element;
      J.Node.Element := EI;
   end Swap;

   ----------------
   -- Swap_Links --
   ----------------

   procedure Swap_Links
     (Container : in out List;
      I, J      : Cursor)
   is
   begin
      if I = No_Element
        or else J = No_Element
      then
         raise Constraint_Error;
      end if;

      if I.Container /= List_Access'(Container'Unchecked_Access) then
         raise Program_Error;
      end if;

      if J.Container /= I.Container then
         raise Program_Error;
      end if;

      pragma Assert (Container.Length >= 1);

      if I.Node = J.Node then
         return;
      end if;

      pragma Assert (Container.Length >= 2);

      declare
         I_Next : constant Cursor := Next (I);

      begin
         if I_Next = J then
            Splice (Container, Before => I, Position => J);

         else
            declare
               J_Next : constant Cursor := Next (J);

            begin
               if J_Next = I then
                  Splice (Container, Before => J, Position => I);

               else
                  pragma Assert (Container.Length >= 3);

                  Splice (Container, Before => I_Next, Position => J);
                  Splice (Container, Before => J_Next, Position => I);
               end if;
            end;
         end if;
      end;
   end Swap_Links;

   --------------------
   -- Update_Element --
   --------------------

   procedure Update_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : in out Element_Type)) is
   begin
      Process (Position.Node.Element);
   end Update_Element;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : access Root_Stream_Type'Class;
      Item   : List)
   is
      Node : Node_Access := Item.First;

   begin
      Count_Type'Base'Write (Stream, Item.Length);

      while Node /= null loop
         Element_Type'Write (Stream, Node.Element);
         Node := Node.Next;
      end loop;
   end Write;

end Ada.Containers.Doubly_Linked_Lists;

