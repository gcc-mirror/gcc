------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                  ADA.CONTAINERS.INDEFINITE_HASHED_SETS                   --
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
-- This unit has originally being developed by Matthew J Heaney.            --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Ada.Containers.Hash_Tables.Generic_Operations;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Operations);

with Ada.Containers.Hash_Tables.Generic_Keys;
pragma Elaborate_All (Ada.Containers.Hash_Tables.Generic_Keys);

with System;  use type System.Address;

with Ada.Containers.Prime_Numbers;

with Ada.Finalization;  use Ada.Finalization;

package body Ada.Containers.Indefinite_Hashed_Sets is

   type Element_Access is access Element_Type;

   type Node_Type is
      limited record
         Element : Element_Access;
         Next    : Node_Access;
      end record;

   function Hash_Node
     (Node : Node_Access) return Hash_Type;
   pragma Inline (Hash_Node);

   function Hash_Node
     (Node : Node_Access) return Hash_Type is
   begin
      return Hash (Node.Element.all);
   end Hash_Node;

   function Next
     (Node : Node_Access) return Node_Access;
   pragma Inline (Next);

   function Next
     (Node : Node_Access) return Node_Access is
   begin
      return Node.Next;
   end Next;

   procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access);
   pragma Inline (Set_Next);

   procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access) is
   begin
      Node.Next := Next;
   end Set_Next;

   function Equivalent_Keys
     (Key  : Element_Type;
      Node : Node_Access) return Boolean;
   pragma Inline (Equivalent_Keys);

   function Equivalent_Keys
     (Key  : Element_Type;
      Node : Node_Access) return Boolean is
   begin
      return Equivalent_Keys (Key, Node.Element.all);
   end Equivalent_Keys;

   function Copy_Node
     (Source : Node_Access) return Node_Access;
   pragma Inline (Copy_Node);

   function Copy_Node
     (Source : Node_Access) return Node_Access is

      Target : constant Node_Access :=
        new Node_Type'(Element => Source.Element,
                       Next    => null);
   begin
      return Target;
   end Copy_Node;


   procedure Free_Element is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   procedure Free (X : in out Node_Access);

   procedure Free (X : in out Node_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   begin
      if X /= null then
         Free_Element (X.Element);
         Deallocate (X);
      end if;
   end Free;

   package HT_Ops is
      new Hash_Tables.Generic_Operations
       (HT_Types          => HT_Types,
        Hash_Table_Type   => Set,
        Null_Node         => null,
        Hash_Node         => Hash_Node,
        Next              => Next,
        Set_Next          => Set_Next,
        Copy_Node         => Copy_Node,
        Free              => Free);

   package Element_Keys is
      new Hash_Tables.Generic_Keys
       (HT_Types  => HT_Types,
        HT_Type   => Set,
        Null_Node => null,
        Next      => Next,
        Set_Next  => Set_Next,
        Key_Type  => Element_Type,
        Hash      => Hash,
        Equivalent_Keys => Equivalent_Keys);


   procedure Adjust (Container : in out Set) renames HT_Ops.Adjust;

   procedure Finalize (Container : in out Set) renames HT_Ops.Finalize;


   function Find_Equal_Key
     (R_Set  : Set;
      L_Node : Node_Access) return Boolean;

   function Find_Equal_Key
     (R_Set  : Set;
      L_Node : Node_Access) return Boolean is

      R_Index : constant Hash_Type :=
        Element_Keys.Index (R_Set, L_Node.Element.all);

      R_Node  : Node_Access := R_Set.Buckets (R_Index);

   begin

      loop

         if R_Node = null then
            return False;
         end if;

         if L_Node.Element.all = R_Node.Element.all then
            return True;
         end if;

         R_Node := Next (R_Node);

      end loop;

   end Find_Equal_Key;

   function Is_Equal is
      new HT_Ops.Generic_Equal (Find_Equal_Key);

   function "=" (Left, Right : Set) return Boolean renames Is_Equal;


   function Length (Container : Set) return Count_Type is
   begin
      return Container.Length;
   end Length;


   function Is_Empty (Container : Set) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;


   procedure Clear (Container : in out Set) renames HT_Ops.Clear;


   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Node.Element.all;
   end Element;


   procedure Query_Element
     (Position : in Cursor;
      Process  : not null access procedure (Element : in Element_Type)) is
   begin
      Process (Position.Node.Element.all);
   end Query_Element;


--  TODO:
--     procedure Replace_Element (Container : in out Set;
--                                Position  : in     Node_Access;
--                                By        : in     Element_Type);

--     procedure Replace_Element (Container : in out Set;
--                                Position  : in     Node_Access;
--                                By        : in     Element_Type) is

--        Node : Node_Access := Position;

--     begin

--        if Equivalent_Keys (Node.Element.all, By) then

--           declare
--              X : Element_Access := Node.Element;
--           begin
--              Node.Element := new Element_Type'(By);
--              --
--              --  NOTE: If there's an exception here, then just
--              --  let it propagate.  We haven't modified the
--              --  state of the container, so there's nothing else
--              --  we need to do.

--              Free_Element (X);
--           end;

--           return;

--        end if;

--        HT_Ops.Delete_Node_Sans_Free (Container, Node);

--        begin
--           Free_Element (Node.Element);
--        exception
--           when others =>
--              Node.Element := null;  --  don't attempt to dealloc X.E again
--              Free (Node);
--              raise;
--        end;

--        begin
--           Node.Element := new Element_Type'(By);
--        exception
--           when others =>
--              Free (Node);
--              raise;
--        end;

--        declare
--           function New_Node (Next : Node_Access) return Node_Access;
--           pragma Inline (New_Node);

--           function New_Node (Next : Node_Access) return Node_Access is
--           begin
--              Node.Next := Next;
--              return Node;
--           end New_Node;

--           procedure Insert is
--              new Element_Keys.Generic_Conditional_Insert (New_Node);

--           Result  : Node_Access;
--           Success : Boolean;
--        begin
--           Insert
--             (HT      => Container,
--              Key     => Node.Element.all,
--              Node    => Result,
--              Success => Success);

--           if not Success then
--              Free (Node);
--              raise Program_Error;
--           end if;

--           pragma Assert (Result = Node);
--        end;

--     end Replace_Element;


--     procedure Replace_Element (Container : in out Set;
--                                Position  : in     Cursor;
--                                By        : in     Element_Type) is
--     begin

--        if Position.Container = null then
--           raise Constraint_Error;
--        end if;

--        if Position.Container /= Set_Access'(Container'Unchecked_Access) then
--           raise Program_Error;
--        end if;

--        Replace_Element (Container, Position.Node, By);

--     end Replace_Element;


   procedure Move (Target : in out Set;
                   Source : in out Set) renames HT_Ops.Move;


   procedure Insert (Container : in out Set;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Inserted  :    out Boolean) is

      function New_Node (Next : Node_Access) return Node_Access;
      pragma Inline (New_Node);

      function New_Node (Next : Node_Access) return Node_Access is
         Element : Element_Access := new Element_Type'(New_Item);
      begin
         return new Node_Type'(Element, Next);
      exception
         when others =>
            Free_Element (Element);
            raise;
      end New_Node;

      procedure Insert is
        new Element_Keys.Generic_Conditional_Insert (New_Node);

   begin

      HT_Ops.Ensure_Capacity (Container, Container.Length + 1);
      Insert (Container, New_Item, Position.Node, Inserted);
      Position.Container := Container'Unchecked_Access;

   end Insert;


   procedure Insert (Container : in out Set;
                     New_Item  : in     Element_Type) is

      Position : Cursor;
      Inserted : Boolean;

   begin

      Insert (Container, New_Item, Position, Inserted);

      if not Inserted then
         raise Constraint_Error;
      end if;

   end Insert;


   procedure Replace (Container : in out Set;
                      New_Item  : in     Element_Type) is

      Node : constant Node_Access :=
        Element_Keys.Find (Container, New_Item);

      X : Element_Access;

   begin

      if Node = null then
         raise Constraint_Error;
      end if;

      X := Node.Element;

      Node.Element := new Element_Type'(New_Item);

      Free_Element (X);

   end Replace;


   procedure Include (Container : in out Set;
                      New_Item  : in     Element_Type) is

      Position : Cursor;
      Inserted : Boolean;

      X : Element_Access;

   begin

      Insert (Container, New_Item, Position, Inserted);

      if not Inserted then

         X := Position.Node.Element;

         Position.Node.Element := new Element_Type'(New_Item);

         Free_Element (X);

      end if;

   end Include;


   procedure Delete (Container : in out Set;
                     Item      : in     Element_Type) is

      X : Node_Access;

   begin

      Element_Keys.Delete_Key_Sans_Free (Container, Item, X);

      if X = null then
         raise Constraint_Error;
      end if;

      Free (X);

   end Delete;


   procedure Exclude (Container : in out Set;
                      Item      : in     Element_Type) is

      X : Node_Access;

   begin

      Element_Keys.Delete_Key_Sans_Free (Container, Item, X);
      Free (X);

   end Exclude;


   procedure Delete (Container : in out Set;
                     Position  : in out Cursor) is
   begin

      if Position = No_Element then
         return;
      end if;

      if Position.Container /= Set_Access'(Container'Unchecked_Access) then
         raise Program_Error;
      end if;

      HT_Ops.Delete_Node_Sans_Free (Container, Position.Node);
      Free (Position.Node);

      Position.Container := null;

   end Delete;



   procedure Union (Target : in out Set;
                    Source : in     Set) is

      procedure Process (Src_Node : in Node_Access);

      procedure Process (Src_Node : in Node_Access) is

         Src : Element_Type renames Src_Node.Element.all;

         function New_Node (Next : Node_Access) return Node_Access;
         pragma Inline (New_Node);

         function New_Node (Next : Node_Access) return Node_Access is
            Tgt : Element_Access := new Element_Type'(Src);
         begin
            return new Node_Type'(Tgt, Next);
         exception
            when others =>
               Free_Element (Tgt);
               raise;
         end New_Node;

         procedure Insert is
            new Element_Keys.Generic_Conditional_Insert (New_Node);

         Tgt_Node : Node_Access;
         Success  : Boolean;

      begin

         Insert (Target, Src, Tgt_Node, Success);

      end Process;

      procedure Iterate is
         new HT_Ops.Generic_Iteration (Process);

   begin

      if Target'Address = Source'Address then
         return;
      end if;

      HT_Ops.Ensure_Capacity (Target, Target.Length + Source.Length);

      Iterate (Source);

   end Union;



   function Union (Left, Right : Set) return Set is

      Buckets : HT_Types.Buckets_Access;
      Length  : Count_Type;

   begin

      if Left'Address = Right'Address then
         return Left;
      end if;

      if Right.Length = 0 then
         return Left;
      end if;

      if Left.Length = 0 then
         return Right;
      end if;

      declare
         Size : constant Hash_Type :=
           Prime_Numbers.To_Prime (Left.Length + Right.Length);
      begin
         Buckets := new Buckets_Type (0 .. Size - 1);
      end;

      declare
         procedure Process (L_Node : Node_Access);

         procedure Process (L_Node : Node_Access) is
            I : constant Hash_Type :=
              Hash (L_Node.Element.all) mod Buckets'Length;
         begin
            Buckets (I) := new Node_Type'(L_Node.Element, Buckets (I));
         end Process;

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);
      begin
         Iterate (Left);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end;

      Length := Left.Length;

      declare
         procedure Process (Src_Node : Node_Access);

         procedure Process (Src_Node : Node_Access) is

            Src : Element_Type renames Src_Node.Element.all;

            I : constant Hash_Type :=
              Hash (Src) mod Buckets'Length;

            Tgt_Node : Node_Access := Buckets (I);

         begin

            while Tgt_Node /= null loop

               if Equivalent_Keys (Src, Tgt_Node.Element.all) then
                  return;
               end if;

               Tgt_Node := Next (Tgt_Node);

            end loop;

            declare
               Tgt : Element_Access := new Element_Type'(Src);
            begin
               Buckets (I) := new Node_Type'(Tgt, Buckets (I));
            exception
               when others =>
                  Free_Element (Tgt);
                  raise;
            end;

            Length := Length + 1;

         end Process;

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);
      begin
         Iterate (Right);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end;

      return (Controlled with Buckets, Length);

   end Union;


   function Is_In
     (HT  : Set;
      Key : Node_Access) return Boolean;
   pragma Inline (Is_In);

   function Is_In
     (HT  : Set;
      Key : Node_Access) return Boolean is
   begin
      return Element_Keys.Find (HT, Key.Element.all) /= null;
   end Is_In;


   procedure Intersection (Target : in out Set;
                           Source : in     Set) is

      Tgt_Node : Node_Access;

   begin

      if Target'Address = Source'Address then
         return;
      end if;

      if Source.Length = 0 then
         Clear (Target);
         return;
      end if;

      --  TODO: optimize this to use an explicit
      --  loop instead of an active iterator
      --  (similar to how a passive iterator is
      --  implemented).
      --
      --  Another possibility is to test which
      --  set is smaller, and iterate over the
      --  smaller set.

      Tgt_Node := HT_Ops.First (Target);

      while Tgt_Node /= null loop

         if Is_In (Source, Tgt_Node) then

            Tgt_Node := HT_Ops.Next (Target, Tgt_Node);

         else

            declare
               X : Node_Access := Tgt_Node;
            begin
               Tgt_Node := HT_Ops.Next (Target, Tgt_Node);
               HT_Ops.Delete_Node_Sans_Free (Target, X);
               Free (X);
            end;

         end if;

      end loop;

   end Intersection;


   function Intersection (Left, Right : Set) return Set is

      Buckets : HT_Types.Buckets_Access;
      Length  : Count_Type;

   begin

      if Left'Address = Right'Address then
         return Left;
      end if;

      Length := Count_Type'Min (Left.Length, Right.Length);

      if Length = 0 then
         return Empty_Set;
      end if;

      declare
         Size : constant Hash_Type := Prime_Numbers.To_Prime (Length);
      begin
         Buckets := new Buckets_Type (0 .. Size - 1);
      end;

      Length := 0;

      declare
         procedure Process (L_Node : Node_Access);

         procedure Process (L_Node : Node_Access) is
         begin
            if Is_In (Right, L_Node) then

               declare
                  I : constant Hash_Type :=
                    Hash (L_Node.Element.all) mod Buckets'Length;
               begin
                  Buckets (I) := new Node_Type'(L_Node.Element, Buckets (I));
               end;

               Length := Length + 1;

            end if;
         end Process;

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);
      begin
         Iterate (Left);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end;

      return (Controlled with Buckets, Length);

   end Intersection;


   procedure Difference (Target : in out Set;
                         Source : in     Set) is


      Tgt_Node : Node_Access;

   begin

      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      if Source.Length = 0 then
         return;
      end if;

      --  TODO: As I noted above, this can be
      --  written in terms of a loop instead as
      --  active-iterator style, sort of like a
      --  passive iterator.

      Tgt_Node := HT_Ops.First (Target);

      while Tgt_Node /= null loop

         if Is_In (Source, Tgt_Node) then

            declare
               X : Node_Access := Tgt_Node;
            begin
               Tgt_Node := HT_Ops.Next (Target, Tgt_Node);
               HT_Ops.Delete_Node_Sans_Free (Target, X);
               Free (X);
            end;

         else

            Tgt_Node := HT_Ops.Next (Target, Tgt_Node);

         end if;

      end loop;

   end Difference;



   function Difference (Left, Right : Set) return Set is

      Buckets : HT_Types.Buckets_Access;
      Length  : Count_Type;

   begin

      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      if Left.Length = 0 then
         return Empty_Set;
      end if;

      if Right.Length = 0 then
         return Left;
      end if;

      declare
         Size : constant Hash_Type := Prime_Numbers.To_Prime (Left.Length);
      begin
         Buckets := new Buckets_Type (0 .. Size - 1);
      end;

      Length := 0;

      declare
         procedure Process (L_Node : Node_Access);

         procedure Process (L_Node : Node_Access) is
         begin
            if not Is_In (Right, L_Node) then

               declare
                  I : constant Hash_Type :=
                    Hash (L_Node.Element.all) mod Buckets'Length;
               begin
                  Buckets (I) := new Node_Type'(L_Node.Element, Buckets (I));
               end;

               Length := Length + 1;

            end if;
         end Process;

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);
      begin
         Iterate (Left);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end;

      return (Controlled with Buckets, Length);

   end Difference;



   procedure Symmetric_Difference (Target : in out Set;
                                   Source : in     Set) is
   begin

      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      HT_Ops.Ensure_Capacity (Target, Target.Length + Source.Length);

      if Target.Length = 0 then

         declare
            procedure Process (Src_Node : Node_Access);

            procedure Process (Src_Node : Node_Access) is
               E : Element_Type renames Src_Node.Element.all;
               B : Buckets_Type renames Target.Buckets.all;
               I : constant Hash_Type := Hash (E) mod B'Length;
               N : Count_Type renames Target.Length;
            begin
               declare
                  X : Element_Access := new Element_Type'(E);
               begin
                  B (I) := new Node_Type'(X, B (I));
               exception
                  when others =>
                     Free_Element (X);
                     raise;
               end;

               N := N + 1;
            end Process;

            procedure Iterate is
               new HT_Ops.Generic_Iteration (Process);
         begin
            Iterate (Source);
         end;

      else

         declare
            procedure Process (Src_Node : Node_Access);

            procedure Process (Src_Node : Node_Access) is
               E : Element_Type renames Src_Node.Element.all;
               B : Buckets_Type renames Target.Buckets.all;
               I : constant Hash_Type := Hash (E) mod B'Length;
               N : Count_Type renames Target.Length;
            begin
               if B (I) = null then

                  declare
                     X : Element_Access := new Element_Type'(E);
                  begin
                     B (I) := new Node_Type'(X, null);
                  exception
                     when others =>
                        Free_Element (X);
                        raise;
                  end;

                  N := N + 1;

               elsif Equivalent_Keys (E, B (I).Element.all) then

                  declare
                     X : Node_Access := B (I);
                  begin
                     B (I) := B (I).Next;
                     N := N - 1;
                     Free (X);
                  end;

               else

                  declare
                     Prev : Node_Access := B (I);
                     Curr : Node_Access := Prev.Next;
                  begin
                     while Curr /= null loop
                        if Equivalent_Keys (E, Curr.Element.all) then
                           Prev.Next := Curr.Next;
                           N := N - 1;
                           Free (Curr);
                           return;
                        end if;

                        Prev := Curr;
                        Curr := Prev.Next;
                     end loop;

                     declare
                        X : Element_Access := new Element_Type'(E);
                     begin
                        B (I) := new Node_Type'(X, B (I));
                     exception
                        when others =>
                           Free_Element (X);
                           raise;
                     end;

                     N := N + 1;
                  end;

               end if;
            end Process;

            procedure Iterate is
               new HT_Ops.Generic_Iteration (Process);
         begin
            Iterate (Source);
         end;

      end if;

   end Symmetric_Difference;


   function Symmetric_Difference (Left, Right : Set) return Set is

      Buckets : HT_Types.Buckets_Access;
      Length  : Count_Type;

   begin

      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      if Right.Length = 0 then
         return Left;
      end if;

      if Left.Length = 0 then
         return Right;
      end if;

      declare
         Size : constant Hash_Type :=
           Prime_Numbers.To_Prime (Left.Length + Right.Length);
      begin
         Buckets := new Buckets_Type (0 .. Size - 1);
      end;

      Length := 0;

      declare
         procedure Process (L_Node : Node_Access);

         procedure Process (L_Node : Node_Access) is
         begin
            if not Is_In (Right, L_Node) then
               declare
                  E : Element_Type renames L_Node.Element.all;
                  I : constant Hash_Type := Hash (E) mod Buckets'Length;
               begin

                  declare
                     X : Element_Access := new Element_Type'(E);
                  begin
                     Buckets (I) := new Node_Type'(X, Buckets (I));
                  exception
                     when others =>
                        Free_Element (X);
                        raise;
                  end;

                  Length := Length + 1;
               end;
            end if;
         end Process;

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);
      begin
         Iterate (Left);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end;

      declare
         procedure Process (R_Node : Node_Access);

         procedure Process (R_Node : Node_Access) is
         begin
            if not Is_In (Left, R_Node) then
               declare
                  E : Element_Type renames R_Node.Element.all;
                  I : constant Hash_Type := Hash (E) mod Buckets'Length;
               begin

                  declare
                     X : Element_Access := new Element_Type'(E);
                  begin
                     Buckets (I) := new Node_Type'(X, Buckets (I));
                  exception
                     when others =>
                        Free_Element (X);
                        raise;
                  end;

                  Length := Length + 1;

               end;
            end if;
         end Process;

         procedure Iterate is
            new HT_Ops.Generic_Iteration (Process);
      begin
         Iterate (Right);
      exception
         when others =>
            HT_Ops.Free_Hash_Table (Buckets);
            raise;
      end;

      return (Controlled with Buckets, Length);

   end Symmetric_Difference;


   function Is_Subset (Subset : Set;
                       Of_Set : Set) return Boolean is

      Subset_Node : Node_Access;

   begin

      if Subset'Address = Of_Set'Address then
         return True;
      end if;

      if Subset.Length > Of_Set.Length then
         return False;
      end if;

      --  TODO: rewrite this to loop in the
      --  style of a passive iterator.

      Subset_Node := HT_Ops.First (Subset);

      while Subset_Node /= null loop
         if not Is_In (Of_Set, Subset_Node) then
            return False;
         end if;

         Subset_Node := HT_Ops.Next (Subset, Subset_Node);
      end loop;

      return True;

   end Is_Subset;


   function Overlap (Left, Right : Set) return Boolean is

      Left_Node : Node_Access;

   begin

      if Right.Length = 0 then
         return False;
      end if;

      if Left'Address = Right'Address then
         return True;
      end if;

      Left_Node := HT_Ops.First (Left);

      while Left_Node /= null loop
         if Is_In (Right, Left_Node) then
            return True;
         end if;

         Left_Node := HT_Ops.Next (Left, Left_Node);
      end loop;

      return False;

   end Overlap;


   function Find (Container : Set;
                  Item      : Element_Type) return Cursor is

      Node : constant Node_Access := Element_Keys.Find (Container, Item);

   begin

      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);

   end Find;


   function Contains (Container : Set;
                      Item      : Element_Type) return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end Contains;



   function First (Container : Set) return Cursor is
      Node : constant Node_Access := HT_Ops.First (Container);
   begin
      if Node = null then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Node);
   end First;


--     function First_Element (Container : Set) return Element_Type is
--        Node : constant Node_Access := HT_Ops.First (Container);
--     begin
--        return Node.Element;
--     end First_Element;


   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Container = null
        or else Position.Node = null
      then
         return No_Element;
      end if;

      declare
         S : Set renames Position.Container.all;
         Node : constant Node_Access := HT_Ops.Next (S, Position.Node);
      begin
         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Position.Container, Node);
      end;
   end Next;


   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;


   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Container = null then
         return False;
      end if;

      if Position.Node = null then
         return False;
      end if;

      return True;
   end Has_Element;


   function Equivalent_Keys (Left, Right : Cursor)
     return Boolean is
   begin
      return Equivalent_Keys (Left.Node.Element.all, Right.Node.Element.all);
   end Equivalent_Keys;


   function Equivalent_Keys (Left  : Cursor;
                             Right : Element_Type)
    return Boolean is
   begin
      return Equivalent_Keys (Left.Node.Element.all, Right);
   end Equivalent_Keys;


   function Equivalent_Keys (Left  : Element_Type;
                             Right : Cursor)
    return Boolean is
   begin
      return Equivalent_Keys (Left, Right.Node.Element.all);
   end Equivalent_Keys;


   procedure Iterate
     (Container : in Set;
      Process   : not null access procedure (Position : in Cursor)) is

      procedure Process_Node (Node : in Node_Access);
      pragma Inline (Process_Node);

      procedure Process_Node (Node : in Node_Access) is
      begin
         Process (Cursor'(Container'Unchecked_Access, Node));
      end Process_Node;

      procedure Iterate is
         new HT_Ops.Generic_Iteration (Process_Node);
   begin
      Iterate (Container);
   end Iterate;


   function Capacity (Container : Set) return Count_Type
     renames HT_Ops.Capacity;

   procedure Reserve_Capacity
     (Container : in out Set;
      Capacity  : in     Count_Type)
     renames HT_Ops.Ensure_Capacity;


   procedure Write_Node
     (Stream : access Root_Stream_Type'Class;
      Node   : in     Node_Access);
   pragma Inline (Write_Node);

   procedure Write_Node
     (Stream : access Root_Stream_Type'Class;
      Node   : in     Node_Access) is
   begin
      Element_Type'Output (Stream, Node.Element.all);
   end Write_Node;

   procedure Write_Nodes is
      new HT_Ops.Generic_Write (Write_Node);

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : in     Set) renames Write_Nodes;


   function Read_Node (Stream : access Root_Stream_Type'Class)
     return Node_Access;
   pragma Inline (Read_Node);

   function Read_Node (Stream : access Root_Stream_Type'Class)
     return Node_Access is

      X : Element_Access := new Element_Type'(Element_Type'Input (Stream));
   begin
      return new Node_Type'(X, null);
   exception
      when others =>
         Free_Element (X);
         raise;
   end Read_Node;

   procedure Read_Nodes is
      new HT_Ops.Generic_Read (Read_Node);

   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container :    out Set) renames Read_Nodes;


   package body Generic_Keys is

      function Equivalent_Keys (Left  : Cursor;
                                Right : Key_Type)
        return Boolean is
      begin
         return Equivalent_Keys (Right, Left.Node.Element.all);
      end Equivalent_Keys;

      function Equivalent_Keys (Left  : Key_Type;
                                Right : Cursor)
        return Boolean is
      begin
         return Equivalent_Keys (Left, Right.Node.Element.all);
      end Equivalent_Keys;

      function Equivalent_Keys
        (Key  : Key_Type;
         Node : Node_Access) return Boolean;
      pragma Inline (Equivalent_Keys);

      function Equivalent_Keys
        (Key  : Key_Type;
         Node : Node_Access) return Boolean is
      begin
         return Equivalent_Keys (Key, Node.Element.all);
      end Equivalent_Keys;

      package Key_Keys is
         new Hash_Tables.Generic_Keys
          (HT_Types  => HT_Types,
           HT_Type   => Set,
           Null_Node => null,
           Next      => Next,
           Set_Next  => Set_Next,
           Key_Type  => Key_Type,
           Hash      => Hash,
           Equivalent_Keys => Equivalent_Keys);


      function Find (Container : Set;
                     Key       : Key_Type)
         return Cursor is

         Node : constant Node_Access :=
           Key_Keys.Find (Container, Key);

      begin

         if Node = null then
            return No_Element;
         end if;

         return Cursor'(Container'Unchecked_Access, Node);

      end Find;


      function Contains (Container : Set;
                         Key       : Key_Type) return Boolean is
      begin
         return Find (Container, Key) /= No_Element;
      end Contains;


      function Element (Container : Set;
                        Key       : Key_Type)
        return Element_Type is

         Node : constant Node_Access := Key_Keys.Find (Container, Key);
      begin
         return Node.Element.all;
      end Element;


      function Key (Position : Cursor) return Key_Type is
      begin
         return Key (Position.Node.Element.all);
      end Key;


--  TODO:
--        procedure Replace (Container : in out Set;
--                           Key       : in     Key_Type;
--                           New_Item  : in     Element_Type) is

--           Node : constant Node_Access :=
--             Key_Keys.Find (Container, Key);

--        begin

--           if Node = null then
--              raise Constraint_Error;
--           end if;

--           Replace_Element (Container, Node, New_Item);

--        end Replace;


      procedure Delete (Container : in out Set;
                        Key       : in     Key_Type) is

         X : Node_Access;

      begin

         Key_Keys.Delete_Key_Sans_Free (Container, Key, X);

         if X = null then
            raise Constraint_Error;
         end if;

         Free (X);

      end Delete;


      procedure Exclude (Container : in out Set;
                         Key       : in     Key_Type) is

         X : Node_Access;

      begin

         Key_Keys.Delete_Key_Sans_Free (Container, Key, X);
         Free (X);

      end Exclude;


      procedure Checked_Update_Element
        (Container : in out Set;
         Position  : in     Cursor;
         Process   : not null access
           procedure (Element : in out Element_Type)) is

      begin

         if Position.Container = null then
            raise Constraint_Error;
         end if;

         if Position.Container /= Set_Access'(Container'Unchecked_Access) then
            raise Program_Error;
         end if;

         declare
            Old_Key : Key_Type renames Key (Position.Node.Element.all);
         begin
            Process (Position.Node.Element.all);

            if Equivalent_Keys (Old_Key, Position.Node.Element.all) then
               return;
            end if;
         end;

         declare
            function New_Node (Next : Node_Access) return Node_Access;
            pragma Inline (New_Node);

            function New_Node (Next : Node_Access) return Node_Access is
            begin
               Position.Node.Next := Next;
               return Position.Node;
            end New_Node;

            procedure Insert is
               new Key_Keys.Generic_Conditional_Insert (New_Node);

            Result  : Node_Access;
            Success : Boolean;
         begin
            HT_Ops.Delete_Node_Sans_Free (Container, Position.Node);

            Insert
              (HT      => Container,
               Key     => Key (Position.Node.Element.all),
               Node    => Result,
               Success => Success);

            if not Success then
               declare
                  X : Node_Access := Position.Node;
               begin
                  Free (X);
               end;

               raise Program_Error;
            end if;

            pragma Assert (Result = Position.Node);
         end;

      end Checked_Update_Element;

   end Generic_Keys;

end Ada.Containers.Indefinite_Hashed_Sets;

