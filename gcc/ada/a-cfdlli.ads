------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.FORMAL_DOUBLY_LINKED_LISTS                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2017, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

with Ada.Containers.Functional_Vectors;
with Ada.Containers.Functional_Maps;

generic
   type Element_Type is private;
package Ada.Containers.Formal_Doubly_Linked_Lists with
  SPARK_Mode
is
   pragma Annotate (CodePeer, Skip_Analysis);

   type List (Capacity : Count_Type) is private with
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Element),
     Default_Initial_Condition => Is_Empty (List);
   pragma Preelaborable_Initialization (List);

   type Cursor is record
      Node : Count_Type := 0;
   end record;

   No_Element : constant Cursor := Cursor'(Node => 0);

   Empty_List : constant List;

   function Length (Container : List) return Count_Type with
     Global => null,
     Post   => Length'Result <= Container.Capacity;

   pragma Unevaluated_Use_Of_Old (Allow);

   package Formal_Model with Ghost is
      subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

      package M is new Ada.Containers.Functional_Vectors
        (Index_Type   => Positive_Count_Type,
         Element_Type => Element_Type);
      function "=" (Left, Right : M.Sequence) return Boolean renames M."=";
      function "<" (Left, Right : M.Sequence) return Boolean renames M."<";
      function "<=" (Left, Right : M.Sequence) return Boolean renames M."<=";

      function M_Elements_Contains
        (S   : M.Sequence;
         Fst : Positive_Count_Type;
         Lst : Count_Type;
         E   : Element_Type)
      return Boolean
      --  E appears in the slice from Fst to Lst in S
      with
        Global => null,
        Pre    => Lst <= M.Length (S),
        Post   =>
          M_Elements_Contains'Result =
            (for some I in Fst .. Lst => Element (S, I) = E);
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Contains);

      function M_Elements_Cst
        (S   : M.Sequence;
         Fst : Positive_Count_Type;
         Lst : Count_Type;
         E   : Element_Type)
      return Boolean
      --  Every element of the slice from Fst to Lst in S is E.
      with
        Global => null,
        Pre    => Lst <= M.Length (S),
        Post   =>
          M_Elements_Cst'Result =
            (for all I in Fst .. Lst => Element (S, I) = E);
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Cst);

      function M_Elements_Equal
        (S1, S2 : M.Sequence;
         Fst    : Positive_Count_Type;
         Lst    : Count_Type)
      return Boolean
      --  The slice from Fst to Lst is the same in S1 and S2
      with
        Global => null,
        Pre    => Lst <= M.Length (S1) and Lst <= M.Length (S2),
        Post   =>
          M_Elements_Equal'Result =
            (for all I in Fst .. Lst => Element (S1, I) = Element (S2, I));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Equal);

      function M_Elements_Shuffle
        (S1, S2 : M.Sequence;
         Fst    : Positive_Count_Type;
         Lst    : Count_Type;
         Offset : Count_Type'Base)
      return Boolean
      --  The slice from Fst to Lst in S1 contains the same elements than the
      --  same slide shifted by Offset in S2
      with
        Global => null,
        Pre    =>
          Lst <= M.Length (S1)
            and Offset in 1 - Fst .. M.Length (S2) - Lst,
        Post   =>
          M_Elements_Shuffle'Result =
          (for all J in Fst + Offset .. Lst + Offset =>
             (for some I in Fst .. Lst =>
                    Element (S1, I) = Element (S2, J)));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Shuffle);

      function M_Elements_Reversed (S1, S2 : M.Sequence) return Boolean
      --  S2 is S1 in reverse order
      with
        Global => null,
        Post   =>
          M_Elements_Reversed'Result =
              (M.Length (S1) = M.Length (S2)
               and (for all I in 1 .. M.Length (S1) =>
                   Element (S1, I) = Element (S2, M.Length (S1) - I + 1))
               and (for all I in 1 .. M.Length (S1) =>
                   Element (S2, I) = Element (S1, M.Length (S1) - I + 1)));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Reversed);

      function M_Elements_Shifted
        (S1, S2   : M.Sequence;
         Fst      : Positive_Count_Type;
         Lst      : Count_Type;
         Offset   : Count_Type'Base := 1)
      return Boolean
      --  The slice from Fst to Lst in S1 has been shifted by Offset in S2.
      with
          Global => null,
        Pre    =>
          Lst <= M.Length (S1)
            and Offset in 1 - Fst .. M.Length (S2) - Lst,
        Post   =>
          M_Elements_Shifted'Result =
            ((for all I in Fst .. Lst =>
                      Element (S1, I) = Element (S2, I + Offset))
             and (for all I in Fst + Offset .. Lst + Offset =>
                          Element (S1, I - Offset) = Element (S2, I)));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Shifted);

      function M_Elements_Swapped
        (S1, S2 : M.Sequence;
         X, Y   : Positive_Count_Type)
      return Boolean
      --  Elements stored at X and Y are reversed in S1 and S2
      with
        Global => null,
        Pre    => X <= M.Length (S1) and Y <= M.Length (S1),
        Post   =>
          M_Elements_Swapped'Result =
            (M.Length (S1) = M.Length (S2)
             and Element (S1, X) = Element (S2, Y)
             and Element (S1, Y) = Element (S2, X)
             and
               (for all I in 1 .. M.Length (S1) =>
                 (if I /= X and I /= Y
                    then Element (S1, I) = Element (S2, I))));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Swapped);

      package P is new Ada.Containers.Functional_Maps
        (Key_Type        => Cursor,
         Element_Type    => Positive_Count_Type,
         Equivalent_Keys => "=");
      function "=" (Left, Right : P.Map) return Boolean renames P."=";
      function "<=" (Left, Right : P.Map) return Boolean renames P."<=";

      function P_Positions_Shifted
        (Small : P.Map;
         Big   : P.Map;
         Cut   : Positive_Count_Type;
         Count : Count_Type := 1) return Boolean
      with
        Global => null,
        Post   =>
          P_Positions_Shifted'Result =

         --  Big contains all cursors of Small
        ((for all I of Small => P.Mem (Big, I))

         --  Cursors located before Cut are not moved, cursors located after
         --  are shifted by Count.
         and
           (for all I of Small =>
                  (if P.Get (Small, I) < Cut
                   then P.Get (Big, I) = P.Get (Small, I)
                   else P.Get (Big, I) - Count = P.Get (Small, I)))

         --  New cursors of Big (if any) are between Cut and Cut - 1 + Count
         and
           (for all I of Big =>
                 P.Mem (Small, I)
              or P.Get (Big, I) - Count in Cut - Count  .. Cut - 1));

      function P_Positions_Swapped
        (M1, M2 : P.Map;
         C1, C2 : Cursor) return Boolean
      --  M1 and M2 contain the same cursors, but the positions of C1 and C2
      --  are reversed.
      with
        Ghost,
        Global => null,
        Post   =>
          P_Positions_Swapped'Result =
              ((for all C of M1 => P.Mem (M2, C))
               and (for all C of M2 => P.Mem (M1, C))
               and
                 (for all C of M1 =>
                    (if C /= C1 and C /= C2
                     then P.Get (M1, C) = P.Get (M2, C)))
               and P.Mem (M1, C1) and P.Mem (M1, C2)
               and P.Get (M1, C1) = P.Get (M2, C2)
               and P.Get (M1, C2) = P.Get (M2, C1));

      function P_Positions_Truncated
        (Small : P.Map;
         Big   : P.Map;
         Cut   : Positive_Count_Type;
         Count : Count_Type := 1) return Boolean
      with
        Ghost,
        Global => null,
        Post   =>
          P_Positions_Truncated'Result =

         --  Big contains all cursors of Small
        ((for all I of Small => P.Mem (Big, I))

         --  The cursors of Small are all bellow Cut
         and (for all I of Small => P.Get (Small, I) < Cut)

         --  The cursors have the same position in Big and Small
         and (for all I of Small => P.Get (Big, I) = P.Get (Small, I))

         --  New cursors of Big (if any) are between Cut and Cut - 1 + Count
         and
           (for all I of Big =>
                P.Mem (Small, I)
             or P.Get (Big, I) - Count in Cut - Count .. Cut - 1));

      function Mapping_Preserved
        (S1, S2 : M.Sequence;
         M1, M2 : P.Map) return Boolean
      with
        Ghost,
        Global => null,
        Post   =>
            (if Mapping_Preserved'Result then

            --  M1 contains all cursors of M2
            (for all I of M2 => P.Mem (M1, I))

            --  M2 contains all cursors of M1
            and (for all I of M1 => P.Mem (M2, I))

            --  Mappings from cursors to elements induced by S1, M1 and S2, M2
            --  are the same.
            and (for all I of M1 =>
                       M.Get (S1, P.Get (M1, I)) = M.Get (S2, P.Get (M2, I))));

      function Model (Container : List) return M.Sequence with
      --  The highlevel model of a list is a sequence of elements. Cursors are
      --  not represented in this model.

        Ghost,
        Global => null,
        Post   => M.Length (Model'Result) = Length (Container);
      pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Model);

      function Positions (Container : List) return P.Map with
      --  The Positions map is used to model cursors. It only contains valid
      --  cursors and map them to their position in the container.

        Ghost,
        Global => null,
        Post   => not P.Mem (Positions'Result, No_Element)
        --  Positions of cursors are smaller than the container's length.
        and then
          (for all I of Positions'Result =>
             P.Get (Positions'Result, I) in 1 .. Length (Container)

               --  No two cursors have the same position. Note that we do not
           --  state that there is a cursor in the map for each position,
           --  as it is rarely needed.
           and then
             (for all J of Positions'Result =>
                (if P.Get (Positions'Result, I) = P.Get (Positions'Result, J)
                     then I = J)));

      procedure Lift_Abstraction_Level (Container : List) with
        --  Lift_Abstraction_Level is a ghost procedure that does nothing but
        --  assume that we can access to the same elements by iterating over
        --  positions or cursors.
        --  This information is not generally useful except when switching from
        --  a lowlevel, cursor aware view of a container, to a highlevel
        --  position based view.

        Ghost,
        Global => null,
        Post   =>
            (for all Elt of Model (Container) =>
               (for some I of Positions (Container) =>
                    M.Get (Model (Container), P.Get (Positions (Container), I))
                    = Elt));

      function Element (S : M.Sequence; I : Count_Type) return Element_Type
                     renames M.Get;
      --  To improve readability of contracts, we rename the function used to
      --  access an element in the model to Element.
   end Formal_Model;
   use Formal_Model;

   function "=" (Left, Right : List) return Boolean with
     Global => null,
     Post   => "="'Result = (Model (Left) = Model (Right));

   function Is_Empty (Container : List) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (Length (Container) = 0);

   procedure Clear (Container : in out List) with
     Global => null,
     Post   => Length (Container) = 0;

   procedure Assign (Target : in out List; Source : List) with
     Global => null,
     Pre    => Target.Capacity >= Length (Source),
     Post   => Model (Target) = Model (Source);

   function Copy (Source : List; Capacity : Count_Type := 0) return List with
     Global => null,
     Pre    => Capacity = 0 or else Capacity >= Source.Capacity,
     Post   => Model (Copy'Result) = Model (Source)
     and Positions (Copy'Result) = Positions (Source)
     and (if Capacity = 0 then Copy'Result.Capacity = Source.Capacity
          else Copy'Result.Capacity = Capacity);

   function Element
     (Container : List;
      Position : Cursor) return Element_Type
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Element'Result =
         Element (Model (Container),
                  P.Get (Positions (Container), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Element);

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   => Length (Container) = Length (Container)'Old

     --  Cursors are preserved.
     and Positions (Container)'Old = Positions (Container)

     --  The element at the position of Position in Container is replaced by
     --  New_Item.
     and M.Is_Set (Model (Container)'Old,
                   P.Get (Positions (Container), Position),
                   New_Item,
                   Model (Container));

   procedure Move (Target : in out List; Source : in out List) with
     Global => null,
     Pre    => Target.Capacity >= Length (Source),
     Post   => Model (Target) = Model (Source'Old)
     and Length (Source) = 0;

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type)
   with
     Global         => null,
     Pre            =>
         Length (Container) < Container.Capacity
       and then (Has_Element (Container, Before)
                 or else Before = No_Element),
     Post           => Length (Container) = Length (Container)'Old + 1,
     Contract_Cases =>
       (Before = No_Element =>

          --  Positions contains a new mapping from the last cursor of
          --  Container to its length.
          P.Is_Add
            (Positions (Container)'Old, Last (Container), Length (Container),
             Result => Positions (Container))

          --  Model contains a new element New_Item at the end
          and M.Is_Add (Model (Container)'Old, New_Item, Model (Container)),
        others              =>

          --  The elements of Container located before Before are preserved.
          M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => P.Get (Positions (Container)'Old, Before) - 1)

          --  Other elements are shifted by 1.
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => P.Get (Positions (Container)'Old, Before),
             Lst    => Length (Container)'Old,
             Offset => 1)

          --  New_Item is stored at the previous position of Before in
          --  Container
          and Element
            (Model (Container), P.Get (Positions (Container)'Old, Before))
               = New_Item

          --  A new cursor has been inserted at position Before in Container
          and P_Positions_Shifted
            (Positions (Container)'Old,
             Positions (Container),
             Cut => P.Get (Positions (Container)'Old, Before)));

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Count     : Count_Type)
   with
     Global => null,
       Pre    =>
         Length (Container) <= Container.Capacity - Count
       and then (Has_Element (Container, Before)
                 or else Before = No_Element),
     Post           => Length (Container) = Length (Container)'Old + Count,
     Contract_Cases =>
       (Before = No_Element =>

          --  The elements of Container are preserved
          M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => Length (Container)'Old)

          --  Container contains Count times New_Item at the end
          and M_Elements_Cst
            (S   => Model (Container),
             Fst => Length (Container)'Old + 1,
             Lst => Length (Container),
             E   => New_Item)

          --  A Count cursors have been inserted at the end of Container
          and P_Positions_Truncated
            (Positions (Container)'Old,
             Positions (Container),
             Cut   => Length (Container)'Old + 1,
             Count => Count),
        others              =>

          --  The elements of Container located before Before are preserved
          M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => P.Get (Positions (Container)'Old, Before) - 1)

          --  Other elements are shifted by Count.
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => P.Get (Positions (Container)'Old, Before),
             Lst    => Length (Container)'Old,
             Offset => Count)

          --  Container contains Count times New_Item after position Before
          and M_Elements_Cst
            (S   => Model (Container),
             Fst => P.Get (Positions (Container)'Old, Before),
             Lst => P.Get (Positions (Container)'Old, Before) - 1 + Count,
             E   => New_Item)

          --  Count cursors have been inserted at position Before in Container
          and P_Positions_Shifted
            (Positions (Container)'Old,
             Positions (Container),
             Cut   => P.Get (Positions (Container)'Old, Before),
             Count => Count));

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor)
   with
     Global => null,
     Pre    =>
         Length (Container) < Container.Capacity
       and then (Has_Element (Container, Before)
                 or else Before = No_Element),
     Post   =>
       Length (Container) = Length (Container)'Old + 1

          --  Positions is valid in Container and it is located either before
          --  Before if it is valid in Container or at the end if it is
          --  No_Element.
          and P.Mem (Positions (Container), Position)
          and (if Before = No_Element
               then P.Get (Positions (Container), Position)
                  = Length (Container)
               else P.Get (Positions (Container), Position)
                  = P.Get (Positions (Container)'Old, Before))

          --  The elements of Container located before Position are preserved.
          and M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => P.Get (Positions (Container), Position) - 1)

          --  Other elements are shifted by 1.
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => P.Get (Positions (Container), Position),
             Lst    => Length (Container)'Old,
             Offset => 1)

          --  New_Item is stored at Position in Container
          and Element
            (Model (Container), P.Get (Positions (Container), Position))
               = New_Item

          --  A new cursor has been inserted at position Position in Container
          and P_Positions_Shifted
            (Positions (Container)'Old,
             Positions (Container),
             Cut => P.Get (Positions (Container), Position));

   procedure Insert
     (Container : in out List;
      Before    : Cursor;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Count     : Count_Type)
   with
     Global         => null,
     Pre            =>
         Length (Container) <= Container.Capacity - Count
       and then (Has_Element (Container, Before)
                 or else Before = No_Element),
     Post           => Length (Container) = Length (Container)'Old + Count,
     Contract_Cases =>
       (Count = 0 => Position = Before
          and Model (Container) = Model (Container)'Old
          and Positions (Container) = Positions (Container)'Old,

        others    =>
          --  Positions is valid in Container and it is located either before
          --  Before if it is valid in Container or at the end if it is
          --  No_Element.
          P.Mem (Positions (Container), Position)
          and (if Before = No_Element
               then P.Get (Positions (Container), Position)
                  = Length (Container)'Old + 1
               else P.Get (Positions (Container), Position)
                  = P.Get (Positions (Container)'Old, Before))

          --  The elements of Container located before Position are preserved.
          and M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => P.Get (Positions (Container), Position) - 1)

          --  Other elements are shifted by Count.
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => P.Get (Positions (Container), Position),
             Lst    => Length (Container)'Old,
             Offset => Count)

          --  Container contains Count times New_Item after position Position
          and M_Elements_Cst
            (S   => Model (Container),
             Fst => P.Get (Positions (Container), Position),
             Lst => P.Get (Positions (Container), Position) - 1 + Count,
             E   => New_Item)

          --  Count cursor have been inserted at Position in Container
          and P_Positions_Shifted
            (Positions (Container)'Old,
             Positions (Container),
             Cut   => P.Get (Positions (Container), Position),
             Count => Count));

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity,
     Post   =>
       Length (Container) = Length (Container)'Old + 1

          --  Elements are shifted by 1
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => 1,
             Lst    => Length (Container)'Old,
             Offset => 1)

          --  New_Item is the first element of Container
          and Element (Model (Container), 1) = New_Item

          --  A new cursor has been inserted at the beginning of Container
          and P_Positions_Shifted
            (Positions (Container)'Old,
             Positions (Container),
             Cut => 1);

   procedure Prepend
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type)
   with
     Global => null,
     Pre    => Length (Container) <= Container.Capacity - Count,
     Post   =>
       Length (Container) = Length (Container)'Old + Count

          --  Elements are shifted by Count.
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => 1,
             Lst    => Length (Container)'Old,
             Offset => Count)

          --  Container starts with Count times New_Item
          and M_Elements_Cst
            (S   => Model (Container),
             Fst => 1,
             Lst => Count,
             E   => New_Item)

          --  Count cursors have been inserted at the beginning of Container
          and P_Positions_Shifted
            (Positions (Container)'Old,
             Positions (Container),
             Cut   => 1,
             Count => Count);

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Length (Container) < Container.Capacity,
     Post   => Length (Container) = Length (Container)'Old + 1

          --  Positions contains a new mapping from the last cursor of
          --  Container to its length.
          and P.Is_Add
            (Positions (Container)'Old, Last (Container), Length (Container),
             Result => Positions (Container))

          --  Model contains a new element New_Item at the end
          and M.Is_Add (Model (Container)'Old, New_Item, Model (Container));

   procedure Append
     (Container : in out List;
      New_Item  : Element_Type;
      Count     : Count_Type)
   with
     Global => null,
     Pre    => Length (Container) <= Container.Capacity - Count,
     Post   =>
       Length (Container) = Length (Container)'Old + Count

          --  The elements of Container are preserved
          and Model (Container)'Old <= Model (Container)

          --  Container contains Count times New_Item at the end
          and M_Elements_Cst
            (S   => Model (Container),
             Fst => Length (Container)'Old + 1,
             Lst => Length (Container),
             E   => New_Item)

          --  Count cursors have been inserted at the end of Container
          and P_Positions_Truncated
            (Positions (Container)'Old,
             Positions (Container),
             Cut   => Length (Container)'Old + 1,
             Count => Count);

   procedure Delete
     (Container : in out List;
      Position  : in out Cursor)
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Length (Container) = Length (Container)'Old - 1

          --  Position is set to No_Element
          and Position = No_Element

          --  The elements of Container located before Position are preserved.
          and M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => P.Get (Positions (Container)'Old, Position'Old) - 1)

          --  The elements located after Position are shifted by 1
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => P.Get (Positions (Container)'Old, Position'Old) + 1,
             Lst    => Length (Container)'Old,
             Offset => -1)

          --  Position has been removed from Container
          and P_Positions_Shifted
            (Positions (Container),
             Positions (Container)'Old,
             Cut   => P.Get (Positions (Container)'Old, Position'Old));

   procedure Delete
     (Container : in out List;
      Position  : in out Cursor;
      Count     : Count_Type)
   with
     Global         => null,
     Pre            => Has_Element (Container, Position),
     Post           =>
          Length (Container) in Length (Container)'Old - Count
                                        .. Length (Container)'Old

          --  Position is set to No_Element
          and Position = No_Element

          --  The elements of Container located before Position are preserved.
          and M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => P.Get (Positions (Container)'Old, Position'Old) - 1),
     Contract_Cases =>

       --  All the elements after Position have been erased
       (Length (Container) - Count < P.Get (Positions (Container), Position)
        =>

          Length (Container) =
            P.Get (Positions (Container)'Old, Position'Old) - 1

          --  At most Count cursors have been removed at the end of Container
          and P_Positions_Truncated
            (Positions (Container),
             Positions (Container)'Old,
             Cut   => P.Get (Positions (Container)'Old, Position'Old),
             Count => Count),
        others =>
          Length (Container) = Length (Container)'Old - Count

          --  Other elements are shifted by Count
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    =>
               P.Get (Positions (Container)'Old, Position'Old) + Count,
             Lst    => Length (Container)'Old,
             Offset => -Count)

          --  Count cursors have been removed from Container at Position
          and P_Positions_Shifted
            (Positions (Container),
             Positions (Container)'Old,
             Cut   => P.Get (Positions (Container)'Old, Position'Old),
             Count => Count));

   procedure Delete_First (Container : in out List)
   with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   =>
       Length (Container) = Length (Container)'Old - 1

          --  The elements of Container are shifted by 1
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => 2,
             Lst    => Length (Container)'Old,
             Offset => -1)

          --  The first cursor of Container has been removed
          and P_Positions_Shifted
            (Positions (Container),
             Positions (Container)'Old,
             Cut   => 1);

   procedure Delete_First
     (Container : in out List;
      Count     : Count_Type)
   with
     Global         => null,
     Contract_Cases =>

       --  All the elements of Container have been erased
       (Length (Container) <= Count => Length (Container) = 0,
        others =>
          Length (Container) = Length (Container)'Old - Count

          --  Elements of Container are shifted by Count
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => Count + 1,
             Lst    => Length (Container)'Old,
             Offset => -Count)

          --  The first Count cursors have been removed from Container
          and P_Positions_Shifted
            (Positions (Container),
             Positions (Container)'Old,
             Cut   => 1,
             Count => Count));

   procedure Delete_Last (Container : in out List)
   with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   =>
       Length (Container) = Length (Container)'Old - 1

          --  The elements of Container are preserved.
          and Model (Container) <= Model (Container)'Old

          --  The last cursor of Container has been removed
          and P.Is_Add (Positions (Container), Last (Container)'Old,
                        Length (Container)'Old, Positions (Container)'Old);

   procedure Delete_Last
     (Container : in out List;
      Count     : Count_Type)
   with
     Global => null,
     Contract_Cases =>

       --  All the elements of Container have been erased
       (Length (Container) <= Count => Length (Container) = 0,
        others =>

          Length (Container) = Length (Container)'Old - Count

          --  The elements of Container are preserved.
          and Model (Container) <= Model (Container)'Old

          --  At most Count cursors have been removed at the end of Container
          and P_Positions_Truncated
            (Positions (Container),
             Positions (Container)'Old,
             Cut   => Length (Container) + 1,
             Count => Count));

   procedure Reverse_Elements (Container : in out List) with
     Global => null,
     Post   => M_Elements_Reversed (Model (Container'Old), Model (Container));

   procedure Swap
     (Container : in out List;
      I, J      : Cursor)
   with
     Global => null,
     Pre    => Has_Element (Container, I) and then Has_Element (Container, J),
     Post   =>
       M_Elements_Swapped
         (Model (Container)'Old, Model (Container),
          X => P.Get (Positions (Container)'Old, I),
          Y => P.Get (Positions (Container)'Old, J))
       and Positions (Container) = Positions (Container)'Old;

   procedure Swap_Links
     (Container : in out List;
      I, J      : Cursor)
   with
     Global => null,
     Pre    => Has_Element (Container, I) and then Has_Element (Container, J),
     Post   =>
       M_Elements_Swapped
         (Model (Container'Old), Model (Container),
          X => P.Get (Positions (Container)'Old, I),
          Y => P.Get (Positions (Container)'Old, J))
       and P_Positions_Swapped
       (Positions (Container)'Old, Positions (Container), I, J);

   procedure Splice
     (Target : in out List;
      Before : Cursor;
      Source : in out List)
   --  Target and Source should not be aliased
   with
     Global         => null,
     Pre            =>
         Length (Source) <= Target.Capacity - Length (Target)
         and then (Has_Element (Target, Before)
                   or else Before = No_Element),
     Post           =>
       Length (Source) = 0
           and Length (Target) = Length (Target)'Old + Length (Source)'Old,
     Contract_Cases =>
       (Before = No_Element =>

          --  The elements of Target are preserved
          M_Elements_Equal
            (S1  => Model (Target)'Old,
             S2  => Model (Target),
             Fst => 1,
             Lst => Length (Target)'Old)

          --  The elements of Source are appended to target, the order is not
          --  specified.
          and M_Elements_Shuffle
            (S1     => Model (Source)'Old,
             S2     => Model (Target),
             Fst    => 1,
             Lst    => Length (Source)'Old,
             Offset => Length (Target)'Old)

          --  Cursors have been inserted at the end of Target
          and P_Positions_Truncated
            (Positions (Target)'Old,
             Positions (Target),
             Cut   => Length (Target)'Old + 1,
             Count => Length (Source)'Old),
        others              =>

          --  The elements of Target located before Before are preserved
          M_Elements_Equal
            (S1  => Model (Target)'Old,
             S2  => Model (Target),
             Fst => 1,
             Lst => P.Get (Positions (Target)'Old, Before) - 1)

          --  The elements of Source are inserted before Before, the order is
          --  not specified.
          and M_Elements_Shuffle
            (S1     => Model (Source)'Old,
             S2     => Model (Target),
             Fst    => 1,
             Lst    => Length (Source)'Old,
             Offset => P.Get (Positions (Target)'Old, Before) - 1)

          --  Other elements are shifted by the length of Source
          and M_Elements_Shifted
            (S1     => Model (Target)'Old,
             S2     => Model (Target),
             Fst    => P.Get (Positions (Target)'Old, Before),
             Lst    => Length (Target)'Old,
             Offset => Length (Source)'Old)

          --  Cursors have been inserted at position Before in Target
          and P_Positions_Shifted
            (Positions (Target)'Old,
             Positions (Target),
             Cut   => P.Get (Positions (Target)'Old, Before),
             Count => Length (Source)'Old));

   procedure Splice
     (Target   : in out List;
      Before   : Cursor;
      Source   : in out List;
      Position : in out Cursor)
   --  Target and Source should not be aliased
   with
     Global => null,
     Pre    =>
         (Has_Element (Target, Before)
          or else Before = No_Element)
          and then Has_Element (Source, Position)
          and then Length (Target) < Target.Capacity,
     Post   =>
          Length (Target) = Length (Target)'Old + 1
          and Length (Source) = Length (Source)'Old - 1

          --  The elements of Source located before Position are preserved.
          and M_Elements_Equal
            (S1  => Model (Source)'Old,
             S2  => Model (Source),
             Fst => 1,
             Lst => P.Get (Positions (Source)'Old, Position'Old) - 1)

          --  The elements located after Position are shifted by 1
          and M_Elements_Shifted
            (S1     => Model (Source)'Old,
             S2     => Model (Source),
             Fst    => P.Get (Positions (Source)'Old, Position'Old) + 1,
             Lst    => Length (Source)'Old,
             Offset => -1)

          --  Position has been removed from Source
          and P_Positions_Shifted
            (Positions (Source),
             Positions (Source)'Old,
             Cut   => P.Get (Positions (Source)'Old, Position'Old))

          --  Positions is valid in Target and it is located either before
          --  Before if it is valid in Target or at the end if it is
          --  No_Element.
          and P.Mem (Positions (Target), Position)
          and (if Before = No_Element
               then P.Get (Positions (Target), Position)
                  = Length (Target)
               else P.Get (Positions (Target), Position)
                  = P.Get (Positions (Target)'Old, Before))

          --  The elements of Target located before Position are preserved.
          and M_Elements_Equal
            (S1  => Model (Target)'Old,
             S2  => Model (Target),
             Fst => 1,
             Lst => P.Get (Positions (Target), Position) - 1)

          --  Other elements are shifted by 1.
          and M_Elements_Shifted
            (S1     => Model (Target)'Old,
             S2     => Model (Target),
             Fst    => P.Get (Positions (Target), Position),
             Lst    => Length (Target)'Old,
             Offset => 1)

          --  The element located at Position in Source is moved to Target
          and Element (Model (Target), P.Get (Positions (Target), Position))
            = Element (Model (Source)'Old,
                       P.Get (Positions (Source)'Old, Position'Old))

          --  A new cursor has been inserted at position Position in Target
          and P_Positions_Shifted
            (Positions (Target)'Old,
             Positions (Target),
             Cut => P.Get (Positions (Target), Position));

   procedure Splice
     (Container : in out List;
      Before    : Cursor;
      Position  : Cursor)
   with
     Global         => null,
     Pre            =>
         (Has_Element (Container, Before) or else Before = No_Element)
         and then Has_Element (Container, Position),
     Post           => Length (Container) = Length (Container)'Old,
     Contract_Cases =>
       (Before = Position   =>
          Model (Container) = Model (Container)'Old
        and Positions (Container) = Positions (Container)'Old,

        Before = No_Element =>

          --  The elements located before Position are preserved
          M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => P.Get (Positions (Container)'Old, Position) - 1)

          --  The elements located after Position are shifted by 1
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => P.Get (Positions (Container)'Old, Position) + 1,
             Lst    => Length (Container)'Old,
             Offset => -1)

          --  The last element of Container is the one that was previously
          --  at Position.
          and Element (Model (Container), Length (Container))
            = Element (Model (Container)'Old,
                       P.Get (Positions (Container)'Old, Position))

          --  Cursors from Container continue designating the same elements
          and Mapping_Preserved
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             M1     => Positions (Container)'Old,
             M2     => Positions (Container)),

        others              =>

          --  The elements located before Position and Before are preserved
          M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => 1,
             Lst => Count_Type'Min
                      (P.Get (Positions (Container)'Old, Position) - 1,
                       P.Get (Positions (Container)'Old, Before) - 1))

          --  The elements located after Position and Before are preserved
          and M_Elements_Equal
            (S1  => Model (Container)'Old,
             S2  => Model (Container),
             Fst => Count_Type'Max
                      (P.Get (Positions (Container)'Old, Position) + 1,
                       P.Get (Positions (Container)'Old, Before) + 1),
             Lst => Length (Container))

          --  The elements located after Before and before Position are shifted
          --  by 1 to the right.
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => P.Get (Positions (Container)'Old, Before) + 1,
             Lst    => P.Get (Positions (Container)'Old, Position) - 1,
             Offset => 1)

          --  The elements located after Position and before Before are shifted
          --  by 1 to the left.
          and M_Elements_Shifted
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             Fst    => P.Get (Positions (Container)'Old, Position) + 1,
             Lst    => P.Get (Positions (Container)'Old, Before) - 1,
             Offset => -1)

          --  The element previously at Position is now before Before
          and Element (Model (Container),
                       P.Get (Positions (Container)'Old, Before))
            = Element (Model (Container)'Old,
                       P.Get (Positions (Container)'Old, Position))

          --  Cursors from Container continue designating the same elements
          and Mapping_Preserved
            (S1     => Model (Container)'Old,
             S2     => Model (Container),
             M1     => Positions (Container)'Old,
             M2     => Positions (Container)));

   function First (Container : List) return Cursor with
     Global         => null,
     Contract_Cases =>
       (Length (Container) = 0 => First'Result = No_Element,
        others            => Has_Element (Container, First'Result)
        and  P.Get (Positions (Container), First'Result) = 1);

   function First_Element (Container : List) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   => First_Element'Result = M.Get (Model (Container), 1);

   function Last (Container : List) return Cursor with
     Global         => null,
     Contract_Cases =>
       (Length (Container) = 0 => Last'Result = No_Element,
        others            => Has_Element (Container, Last'Result)
        and P.Get (Positions (Container), Last'Result) = Length (Container));

   function Last_Element (Container : List) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   => Last_Element'Result
             = M.Get (Model (Container), Length (Container));

   function Next (Container : List; Position : Cursor) return Cursor with
     Global         => null,
     Pre            => Has_Element (Container, Position)
        or else Position = No_Element,
     Contract_Cases =>
       (Position = No_Element
        or else P.Get (Positions (Container), Position) = Length (Container) =>
              Next'Result = No_Element,
        others => Has_Element (Container, Next'Result)
        and then P.Get (Positions (Container), Next'Result) =
          P.Get (Positions (Container), Position) + 1);

   procedure Next (Container : List; Position : in out Cursor) with
     Global         => null,
     Pre            => Has_Element (Container, Position)
        or else Position = No_Element,
     Contract_Cases =>
       (Position = No_Element
        or else P.Get (Positions (Container), Position) = Length (Container) =>
              Position = No_Element,
        others => Has_Element (Container, Position)
        and then P.Get (Positions (Container), Position) =
          P.Get (Positions (Container), Position'Old) + 1);

   function Previous (Container : List; Position : Cursor) return Cursor with
     Global         => null,
     Pre            => Has_Element (Container, Position)
        or else Position = No_Element,
     Contract_Cases =>
       (Position = No_Element
        or else P.Get (Positions (Container), Position) = 1 =>
          Previous'Result = No_Element,
        others =>
          Has_Element (Container, Previous'Result)
        and then P.Get (Positions (Container), Previous'Result) =
          P.Get (Positions (Container), Position) - 1);

   procedure Previous (Container : List; Position : in out Cursor) with
     Global         => null,
     Pre            => Has_Element (Container, Position)
        or else Position = No_Element,
     Contract_Cases =>
       (Position = No_Element
        or else P.Get (Positions (Container), Position) = 1 =>
          Position = No_Element,
        others =>
          Has_Element (Container, Position)
        and then P.Get (Positions (Container), Position) =
          P.Get (Positions (Container), Position'Old) - 1);

   function Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   with
     Global         => null,
     Pre            =>
         Has_Element (Container, Position) or else Position = No_Element,
     Contract_Cases =>

       --  If Item is not is not contained in Container after Position, Find
       --  returns No_Element.
       (not M_Elements_Contains
          (S   => Model (Container),
           Fst => (if Position = No_Element then 1
                   else P.Get (Positions (Container), Position)),
           Lst => Length (Container),
           E   => Item)
        =>
          Find'Result = No_Element,

        --  Otherwise, Find returns a valid cusror in Container
        others =>
          P.Mem (Positions (Container), Find'Result)

        --  The element designated by the result of Find is Item
        and Element (Model (Container),
                     P.Get (Positions (Container), Find'Result)) = Item

        --  The result of Find is located after Position
        and (if Position /= No_Element
             then P.Get (Positions (Container), Find'Result)
               >= P.Get (Positions (Container), Position))

        --  It is the first occurence of Item in this slice
        and not M_Elements_Contains
          (S   => Model (Container),
           Fst => (if Position = No_Element then 1
                   else P.Get (Positions (Container), Position)),
           Lst => P.Get (Positions (Container), Find'Result) - 1,
           E   => Item));

   function Reverse_Find
     (Container : List;
      Item      : Element_Type;
      Position  : Cursor := No_Element) return Cursor
   with
     Global         => null,
     Pre            =>
         Has_Element (Container, Position) or else Position = No_Element,
     Contract_Cases =>

       --  If Item is not is not contained in Container before Position, Find
       --  returns No_Element.
       (not M_Elements_Contains
          (S   => Model (Container),
           Fst => 1,
           Lst => (if Position = No_Element then Length (Container)
                   else P.Get (Positions (Container), Position)),
           E   => Item)
        =>
          Reverse_Find'Result = No_Element,

        --  Otherwise, Find returns a valid cusror in Container
        others =>
          P.Mem (Positions (Container), Reverse_Find'Result)

        --  The element designated by the result of Find is Item
        and Element (Model (Container),
                     P.Get (Positions (Container), Reverse_Find'Result)) = Item

        --  The result of Find is located before Position
        and (if Position /= No_Element
             then P.Get (Positions (Container), Reverse_Find'Result)
               <= P.Get (Positions (Container), Position))

        --  It is the last occurence of Item in this slice
        and not M_Elements_Contains
          (S   => Model (Container),
           Fst => P.Get (Positions (Container), Reverse_Find'Result) + 1,
           Lst => (if Position = No_Element then Length (Container)
                   else P.Get (Positions (Container), Position)),
           E   => Item));

   function Contains
     (Container : List;
      Item      : Element_Type) return Boolean
   with
     Global => null,
     Post   => Contains'Result =
         M_Elements_Contains
          (S   => Model (Container),
           Fst => 1,
           Lst => Length (Container),
           E   => Item);

   function Has_Element (Container : List; Position : Cursor) return Boolean
   with
     Global => null,
     Post   => Has_Element'Result = P.Mem (Positions (Container), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Has_Element);

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting with SPARK_Mode is
      function M_Elements_Sorted (S : M.Sequence) return Boolean with
        Ghost,
        Global => null,
        Post   => M_Elements_Sorted'Result =
          (for all I in 1 .. M.Length (S) =>
             (for all J in I + 1 .. M.Length (S) =>
                  Element (S, I) = Element (S, J)
               or Element (S, I) < Element (S, J)));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Sorted);

      function Is_Sorted (Container : List) return Boolean with
        Global => null,
        Post   => Is_Sorted'Result = M_Elements_Sorted (Model (Container));

      procedure Sort (Container : in out List) with
        Global => null,
        Post   => Length (Container) = Length (Container)'Old
        and M_Elements_Sorted (Model (Container));

      procedure Merge (Target, Source : in out List) with
      --  Target and Source should not be aliased
        Global => null,
        Pre    => Length (Source) <= Target.Capacity - Length (Target),
        Post   => Length (Target) = Length (Target)'Old + Length (Source)'Old
        and Length (Source) = 0
        and (if M_Elements_Sorted (Model (Target)'Old)
               and M_Elements_Sorted (Model (Source)'Old)
             then M_Elements_Sorted (Model (Target)));
   end Generic_Sorting;

private
   pragma SPARK_Mode (Off);

   type Node_Type is record
      Prev    : Count_Type'Base := -1;
      Next    : Count_Type;
      Element : Element_Type;
   end record;

   function "=" (L, R : Node_Type) return Boolean is abstract;

   type Node_Array is array (Count_Type range <>) of Node_Type;
   function "=" (L, R : Node_Array) return Boolean is abstract;

   type List (Capacity : Count_Type) is record
      Free   : Count_Type'Base := -1;
      Length : Count_Type := 0;
      First  : Count_Type := 0;
      Last   : Count_Type := 0;
      Nodes  : Node_Array (1 .. Capacity) := (others => <>);
   end record;

   Empty_List : constant List := (0, others => <>);

end Ada.Containers.Formal_Doubly_Linked_Lists;
