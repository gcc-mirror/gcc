------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . F O R M A L _ H A S H E D _ S E T S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2019, Free Software Foundation, Inc.         --
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

--  This spec is derived from package Ada.Containers.Bounded_Hashed_Sets in the
--  Ada 2012 RM. The modifications are meant to facilitate formal proofs by
--  making it easier to express properties, and by making the specification of
--  this unit compatible with SPARK 2014. Note that the API of this unit may be
--  subject to incompatible changes as SPARK 2014 evolves.

--  The modifications are:

--    A parameter for the container is added to every function reading the
--    content of a container: Element, Next, Query_Element, Has_Element, Key,
--    Iterate, Equivalent_Elements. This change is motivated by the need to
--    have cursors which are valid on different containers (typically a
--    container C and its previous version C'Old) for expressing properties,
--    which is not possible if cursors encapsulate an access to the underlying
--    container.

with Ada.Containers.Functional_Maps;
with Ada.Containers.Functional_Sets;
with Ada.Containers.Functional_Vectors;
private with Ada.Containers.Hash_Tables;

generic
   type Element_Type is private;

   with function Hash (Element : Element_Type) return Hash_Type;

   with function Equivalent_Elements
     (Left  : Element_Type;
      Right : Element_Type) return Boolean is "=";

package Ada.Containers.Formal_Hashed_Sets with
  SPARK_Mode
is
   pragma Annotate (CodePeer, Skip_Analysis);

   type Set (Capacity : Count_Type; Modulus : Hash_Type) is private with
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Element),
     Default_Initial_Condition => Is_Empty (Set);
   pragma Preelaborable_Initialization (Set);

   type Cursor is record
      Node : Count_Type;
   end record;

   No_Element : constant Cursor := (Node => 0);

   function Length (Container : Set) return Count_Type with
     Global => null,
     Post   => Length'Result <= Container.Capacity;

   pragma Unevaluated_Use_Of_Old (Allow);

   package Formal_Model with Ghost is
      subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

      package M is new Ada.Containers.Functional_Sets
        (Element_Type    => Element_Type,
         Equivalent_Elements => Equivalent_Elements);

      function "="
        (Left  : M.Set;
         Right : M.Set) return Boolean renames M."=";

      function "<="
        (Left  : M.Set;
         Right : M.Set) return Boolean renames M."<=";

      package E is new Ada.Containers.Functional_Vectors
        (Element_Type => Element_Type,
         Index_Type   => Positive_Count_Type);

      function "="
        (Left  : E.Sequence;
         Right : E.Sequence) return Boolean renames E."=";

      function "<"
        (Left  : E.Sequence;
         Right : E.Sequence) return Boolean renames E."<";

      function "<="
        (Left  : E.Sequence;
         Right : E.Sequence) return Boolean renames E."<=";

      function Find
        (Container : E.Sequence;
         Item      : Element_Type) return Count_Type
      --  Search for Item in Container

      with
        Global => null,
        Post =>
          (if Find'Result > 0 then
              Find'Result <= E.Length (Container)
                and Equivalent_Elements
                      (Item, E.Get (Container, Find'Result)));

      function E_Elements_Included
        (Left  : E.Sequence;
         Right : E.Sequence) return Boolean
      --  The elements of Left are contained in Right

      with
        Global => null,
        Post   =>
          E_Elements_Included'Result =
            (for all I in 1 .. E.Length (Left) =>
              Find (Right, E.Get (Left, I)) > 0
                and then E.Get (Right, Find (Right, E.Get (Left, I))) =
                         E.Get (Left, I));
      pragma Annotate (GNATprove, Inline_For_Proof, E_Elements_Included);

      function E_Elements_Included
        (Left  : E.Sequence;
         Model : M.Set;
         Right : E.Sequence) return Boolean
      --  The elements of Container contained in Model are in Right

      with
        Global => null,
        Post   =>
          E_Elements_Included'Result =
            (for all I in 1 .. E.Length (Left) =>
              (if M.Contains (Model, E.Get (Left, I)) then
                  Find (Right, E.Get (Left, I)) > 0
                    and then E.Get (Right, Find (Right, E.Get (Left, I))) =
                             E.Get (Left, I)));
      pragma Annotate (GNATprove, Inline_For_Proof, E_Elements_Included);

      function E_Elements_Included
        (Container : E.Sequence;
         Model     : M.Set;
         Left      : E.Sequence;
         Right     : E.Sequence) return Boolean
      --  The elements of Container contained in Model are in Left and others
      --  are in Right.

      with
        Global => null,
        Post   =>
          E_Elements_Included'Result =
            (for all I in 1 .. E.Length (Container) =>
              (if M.Contains (Model, E.Get (Container, I)) then
                  Find (Left, E.Get (Container, I)) > 0
                    and then E.Get (Left, Find (Left, E.Get (Container, I))) =
                             E.Get (Container, I)
               else
                  Find (Right, E.Get (Container, I)) > 0
                    and then E.Get
                               (Right, Find (Right, E.Get (Container, I))) =
                             E.Get (Container, I)));
      pragma Annotate (GNATprove, Inline_For_Proof, E_Elements_Included);

      package P is new Ada.Containers.Functional_Maps
        (Key_Type                       => Cursor,
         Element_Type                   => Positive_Count_Type,
         Equivalent_Keys                => "=",
         Enable_Handling_Of_Equivalence => False);

      function "="
        (Left  : P.Map;
         Right : P.Map) return Boolean renames P."=";

      function "<="
        (Left  : P.Map;
         Right : P.Map) return Boolean renames P."<=";

      function Mapping_Preserved
        (E_Left  : E.Sequence;
         E_Right : E.Sequence;
         P_Left  : P.Map;
         P_Right : P.Map) return Boolean
      with
        Ghost,
        Global => null,
        Post   =>
          (if Mapping_Preserved'Result then

             --  Right contains all the cursors of Left

             P.Keys_Included (P_Left, P_Right)

               --  Right contains all the elements of Left

               and E_Elements_Included (E_Left, E_Right)

               --  Mappings from cursors to elements induced by E_Left, P_Left
               --  and E_Right, P_Right are the same.

               and (for all C of P_Left =>
                     E.Get (E_Left, P.Get (P_Left, C)) =
                     E.Get (E_Right, P.Get (P_Right, C))));

      function Mapping_Preserved_Except
        (E_Left   : E.Sequence;
         E_Right  : E.Sequence;
         P_Left   : P.Map;
         P_Right  : P.Map;
         Position : Cursor) return Boolean
      with
        Ghost,
        Global => null,
        Post   =>
          (if Mapping_Preserved_Except'Result then

             --  Right contains all the cursors of Left

             P.Keys_Included (P_Left, P_Right)

               --  Mappings from cursors to elements induced by E_Left, P_Left
               --  and E_Right, P_Right are the same except for Position.

               and (for all C of P_Left =>
                     (if C /= Position then
                         E.Get (E_Left, P.Get (P_Left, C)) =
                         E.Get (E_Right, P.Get (P_Right, C)))));

      function Model (Container : Set) return M.Set with
      --  The high-level model of a set is a set of elements. Neither cursors
      --  nor order of elements are represented in this model. Elements are
      --  modeled up to equivalence.

        Ghost,
        Global => null,
        Post   => M.Length (Model'Result) = Length (Container);

      function Elements (Container : Set) return E.Sequence with
      --  The Elements sequence represents the underlying list structure of
      --  sets that is used for iteration. It stores the actual values of
      --  elements in the set. It does not model cursors.

        Ghost,
        Global => null,
        Post   =>
          E.Length (Elements'Result) = Length (Container)

            --  It only contains keys contained in Model

            and (for all Item of Elements'Result =>
                  M.Contains (Model (Container), Item))

            --  It contains all the elements contained in Model

            and (for all Item of Model (Container) =>
                  (Find (Elements'Result, Item) > 0
                    and then Equivalent_Elements
                               (E.Get (Elements'Result,
                                       Find (Elements'Result, Item)),
                                Item)))

            --  It has no duplicate

            and (for all I in 1 .. Length (Container) =>
                  Find (Elements'Result, E.Get (Elements'Result, I)) = I)

            and (for all I in 1 .. Length (Container) =>
                  (for all J in 1 .. Length (Container) =>
                    (if Equivalent_Elements
                          (E.Get (Elements'Result, I),
                           E.Get (Elements'Result, J))
                     then I = J)));
      pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Elements);

      function Positions (Container : Set) return P.Map with
      --  The Positions map is used to model cursors. It only contains valid
      --  cursors and maps them to their position in the container.

        Ghost,
        Global => null,
        Post   =>
          not P.Has_Key (Positions'Result, No_Element)

            --  Positions of cursors are smaller than the container's length

            and then
              (for all I of Positions'Result =>
                P.Get (Positions'Result, I) in 1 .. Length (Container)

            --  No two cursors have the same position. Note that we do not
            --  state that there is a cursor in the map for each position, as
            --  it is rarely needed.

            and then
              (for all J of Positions'Result =>
                (if P.Get (Positions'Result, I) = P.Get (Positions'Result, J)
                 then I = J)));

      procedure Lift_Abstraction_Level (Container : Set) with
        --  Lift_Abstraction_Level is a ghost procedure that does nothing but
        --  assume that we can access the same elements by iterating over
        --  positions or cursors.
        --  This information is not generally useful except when switching from
        --  a low-level, cursor-aware view of a container, to a high-level,
        --  position-based view.

        Ghost,
        Global => null,
        Post   =>
          (for all Item of Elements (Container) =>
            (for some I of Positions (Container) =>
              E.Get (Elements (Container), P.Get (Positions (Container), I)) =
                Item));

      function Contains
        (C : M.Set;
         K : Element_Type) return Boolean renames M.Contains;
      --  To improve readability of contracts, we rename the function used to
      --  search for an element in the model to Contains.

   end Formal_Model;
   use Formal_Model;

   Empty_Set : constant Set;

   function "=" (Left, Right : Set) return Boolean with
     Global => null,
     Post   =>
         "="'Result =
           (Length (Left) = Length (Right)
             and E_Elements_Included (Elements (Left), Elements (Right)))
       and
         "="'Result =
           (E_Elements_Included (Elements (Left), Elements (Right))
             and E_Elements_Included (Elements (Right), Elements (Left)));

   function Equivalent_Sets (Left, Right : Set) return Boolean with
     Global => null,
     Post   => Equivalent_Sets'Result = (Model (Left) = Model (Right));

   function To_Set (New_Item : Element_Type) return Set with
     Global => null,
     Post   =>
       M.Is_Singleton (Model (To_Set'Result), New_Item)
         and Length (To_Set'Result) = 1
         and E.Get (Elements (To_Set'Result), 1) = New_Item;

   function Capacity (Container : Set) return Count_Type with
     Global => null,
     Post   => Capacity'Result = Container.Capacity;

   procedure Reserve_Capacity
     (Container : in out Set;
      Capacity  : Count_Type)
   with
     Global => null,
     Pre    => Capacity <= Container.Capacity,
     Post   =>
       Model (Container) = Model (Container)'Old
         and Length (Container)'Old = Length (Container)

         --  Actual elements are preserved

         and E_Elements_Included
              (Elements (Container), Elements (Container)'Old)
         and E_Elements_Included
              (Elements (Container)'Old, Elements (Container));

   function Is_Empty (Container : Set) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (Length (Container) = 0);

   procedure Clear (Container : in out Set) with
     Global => null,
     Post   => Length (Container) = 0 and M.Is_Empty (Model (Container));

   procedure Assign (Target : in out Set; Source : Set) with
     Global => null,
     Pre    => Target.Capacity >= Length (Source),
     Post   =>
       Model (Target) = Model (Source)
         and Length (Target) = Length (Source)

         --  Actual elements are preserved

         and E_Elements_Included (Elements (Target), Elements (Source))
         and E_Elements_Included (Elements (Source), Elements (Target));

   function Copy
     (Source   : Set;
      Capacity : Count_Type := 0) return Set
   with
     Global => null,
     Pre    => Capacity = 0 or else Capacity >= Source.Capacity,
     Post   =>
       Model (Copy'Result) = Model (Source)
         and Elements (Copy'Result) = Elements (Source)
         and Positions (Copy'Result) = Positions (Source)
         and (if Capacity = 0 then
                 Copy'Result.Capacity = Source.Capacity
              else
                 Copy'Result.Capacity = Capacity);

   function Element
     (Container : Set;
      Position  : Cursor) return Element_Type
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Element'Result =
         E.Get (Elements (Container), P.Get (Positions (Container), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Element);

   procedure Replace_Element
     (Container : in out Set;
      Position  : Cursor;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Length (Container) = Length (Container)'Old

          --  Position now maps to New_Item

          and Element (Container, Position) = New_Item

          --  New_Item is contained in Container

          and Contains (Model (Container), New_Item)

          --  Other elements are preserved

          and M.Included_Except
                (Model (Container)'Old,
                 Model (Container),
                 Element (Container, Position)'Old)
          and M.Included_Except
                (Model (Container),
                 Model (Container)'Old,
                 New_Item)

          --  Mapping from cursors to elements is preserved

          and Mapping_Preserved_Except
                (E_Left   => Elements (Container)'Old,
                 E_Right  => Elements (Container),
                 P_Left   => Positions (Container)'Old,
                 P_Right  => Positions (Container),
                 Position => Position)
          and Positions (Container) = Positions (Container)'Old;

   procedure Move (Target : in out Set; Source : in out Set) with
     Global => null,
     Pre    => Target.Capacity >= Length (Source),
     Post   =>
       Length (Source) = 0
         and Model (Target) = Model (Source)'Old
         and Length (Target) = Length (Source)'Old

         --  Actual elements are preserved

         and E_Elements_Included (Elements (Target), Elements (Source)'Old)
         and E_Elements_Included (Elements (Source)'Old, Elements (Target));

   procedure Insert
     (Container : in out Set;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   with
     Global         => null,
     Pre            =>
       Length (Container) < Container.Capacity
         or Contains (Container, New_Item),
     Post           =>
       Contains (Container, New_Item)
         and Has_Element (Container, Position)
         and Equivalent_Elements (Element (Container, Position), New_Item),
     Contract_Cases =>

       --  If New_Item is already in Container, it is not modified and Inserted
       --  is set to False.

       (Contains (Container, New_Item) =>
          not Inserted
            and Model (Container) = Model (Container)'Old
            and Elements (Container) = Elements (Container)'Old
            and Positions (Container) = Positions (Container)'Old,

        --  Otherwise, New_Item is inserted in Container and Inserted is set to
        --  True.

        others =>
          Inserted
            and Length (Container) = Length (Container)'Old + 1

            --  Position now maps to New_Item

            and Element (Container, Position) = New_Item

            --  Other elements are preserved

            and Model (Container)'Old <= Model (Container)
            and M.Included_Except
                  (Model (Container),
                   Model (Container)'Old,
                   New_Item)

            --  Mapping from cursors to elements is preserved

            and Mapping_Preserved
                  (E_Left  => Elements (Container)'Old,
                   E_Right => Elements (Container),
                   P_Left  => Positions (Container)'Old,
                   P_Right => Positions (Container))
            and P.Keys_Included_Except
                  (Positions (Container),
                   Positions (Container)'Old,
                   Position));

   procedure Insert  (Container : in out Set; New_Item : Element_Type) with
     Global => null,
     Pre    => Length (Container) < Container.Capacity
                 and then (not Contains (Container, New_Item)),
     Post   =>
       Length (Container) = Length (Container)'Old + 1
         and Contains (Container, New_Item)
         and Element (Container, Find (Container, New_Item)) = New_Item

         --  Other elements are preserved

         and Model (Container)'Old <= Model (Container)
         and M.Included_Except
               (Model (Container),
                Model (Container)'Old,
                New_Item)

         --  Mapping from cursors to elements is preserved

         and Mapping_Preserved
               (E_Left  => Elements (Container)'Old,
                E_Right => Elements (Container),
                P_Left  => Positions (Container)'Old,
                P_Right => Positions (Container))
         and P.Keys_Included_Except
               (Positions (Container),
                Positions (Container)'Old,
                Find (Container, New_Item));

   procedure Include (Container : in out Set; New_Item : Element_Type) with
     Global         => null,
     Pre            =>
       Length (Container) < Container.Capacity
         or Contains (Container, New_Item),
     Post           =>
       Contains (Container, New_Item)
         and Element (Container, Find (Container, New_Item)) = New_Item,
     Contract_Cases =>

       --  If an element equivalent to New_Item is already in Container, it is
       --  replaced by New_Item.

       (Contains (Container, New_Item) =>

          --  Elements are preserved modulo equivalence

          Model (Container) = Model (Container)'Old

            --  Cursors are preserved

            and Positions (Container) = Positions (Container)'Old

            --  The actual value of other elements is preserved

            and E.Equal_Except
                  (Elements (Container)'Old,
                   Elements (Container),
                   P.Get (Positions (Container), Find (Container, New_Item))),

        --  Otherwise, New_Item is inserted in Container

        others =>
          Length (Container) = Length (Container)'Old + 1

            --  Other elements are preserved

            and Model (Container)'Old <= Model (Container)
            and M.Included_Except
                  (Model (Container),
                   Model (Container)'Old,
                   New_Item)

            --  Mapping from cursors to elements is preserved

            and Mapping_Preserved
                  (E_Left  => Elements (Container)'Old,
                   E_Right => Elements (Container),
                   P_Left  => Positions (Container)'Old,
                   P_Right => Positions (Container))
            and P.Keys_Included_Except
                  (Positions (Container),
                   Positions (Container)'Old,
                   Find (Container, New_Item)));

   procedure Replace (Container : in out Set; New_Item : Element_Type) with
     Global => null,
     Pre    => Contains (Container, New_Item),
     Post   =>

       --  Elements are preserved modulo equivalence

       Model (Container) = Model (Container)'Old
         and Contains (Container, New_Item)

         --  Cursors are preserved

         and Positions (Container) = Positions (Container)'Old

         --  The element equivalent to New_Item in Container is replaced by
         --  New_Item.

         and Element (Container, Find (Container, New_Item)) = New_Item
         and E.Equal_Except
               (Elements (Container)'Old,
                Elements (Container),
                P.Get (Positions (Container), Find (Container, New_Item)));

   procedure Exclude (Container : in out Set; Item : Element_Type) with
     Global         => null,
     Post           => not Contains (Container, Item),
     Contract_Cases =>

       --  If Item is not in Container, nothing is changed

       (not Contains (Container, Item) =>
          Model (Container) = Model (Container)'Old
            and Elements (Container) = Elements (Container)'Old
            and Positions (Container) = Positions (Container)'Old,

        --  Otherwise, Item is removed from Container

        others =>
          Length (Container) = Length (Container)'Old - 1

            --  Other elements are preserved

            and Model (Container) <= Model (Container)'Old
            and M.Included_Except
                  (Model (Container)'Old,
                   Model (Container),
                   Item)

            --  Mapping from cursors to elements is preserved

            and Mapping_Preserved
                  (E_Left  => Elements (Container),
                   E_Right => Elements (Container)'Old,
                   P_Left  => Positions (Container),
                   P_Right => Positions (Container)'Old)
            and P.Keys_Included_Except
                  (Positions (Container)'Old,
                   Positions (Container),
                   Find (Container, Item)'Old));

   procedure Delete  (Container : in out Set; Item : Element_Type) with
     Global => null,
     Pre    => Contains (Container, Item),
     Post   =>
       Length (Container) = Length (Container)'Old - 1

         --  Item is no longer in Container

         and not Contains (Container, Item)

         --  Other elements are preserved

         and Model (Container) <= Model (Container)'Old
         and M.Included_Except
               (Model (Container)'Old,
                Model (Container),
                Item)

         --  Mapping from cursors to elements is preserved

         and Mapping_Preserved
               (E_Left  => Elements (Container),
                E_Right => Elements (Container)'Old,
                P_Left  => Positions (Container),
                P_Right => Positions (Container)'Old)
         and P.Keys_Included_Except
               (Positions (Container)'Old,
                Positions (Container),
                Find (Container, Item)'Old);

   procedure Delete (Container : in out Set; Position : in out Cursor) with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Position = No_Element
         and Length (Container) = Length (Container)'Old - 1

         --  The element at position Position is no longer in Container

         and not Contains (Container, Element (Container, Position)'Old)
         and not P.Has_Key (Positions (Container), Position'Old)

         --  Other elements are preserved

         and Model (Container) <= Model (Container)'Old
         and M.Included_Except
               (Model (Container)'Old,
                Model (Container),
                Element (Container, Position)'Old)

         --  Mapping from cursors to elements is preserved

         and Mapping_Preserved
               (E_Left  => Elements (Container),
                E_Right => Elements (Container)'Old,
                P_Left  => Positions (Container),
                P_Right => Positions (Container)'Old)
         and P.Keys_Included_Except
               (Positions (Container)'Old,
                Positions (Container),
                Position'Old);

   procedure Union (Target : in out Set; Source : Set) with
     Global => null,
     Pre    =>
       Length (Source) - Length (Target and Source) <=
         Target.Capacity - Length (Target),
     Post   =>
       Length (Target) = Length (Target)'Old
         - M.Num_Overlaps (Model (Target)'Old, Model (Source))
         + Length (Source)

         --  Elements already in Target are still in Target

         and Model (Target)'Old <= Model (Target)

         --  Elements of Source are included in Target

         and Model (Source) <= Model (Target)

         --  Elements of Target come from either Source or Target

         and M.Included_In_Union
               (Model (Target), Model (Source), Model (Target)'Old)

         --  Actual value of elements come from either Left or Right

         and E_Elements_Included
               (Elements (Target),
                Model (Target)'Old,
                Elements (Target)'Old,
                Elements (Source))

         and E_Elements_Included
               (Elements (Target)'Old, Model (Target)'Old, Elements (Target))

         and E_Elements_Included
               (Elements (Source),
                Model (Target)'Old,
                Elements (Source),
                Elements (Target))

         --  Mapping from cursors of Target to elements is preserved

         and Mapping_Preserved
               (E_Left  => Elements (Target)'Old,
                E_Right => Elements (Target),
                P_Left  => Positions (Target)'Old,
                P_Right => Positions (Target));

   function Union (Left, Right : Set) return Set with
     Global => null,
     Pre    => Length (Left) <= Count_Type'Last - Length (Right),
     Post   =>
       Length (Union'Result) = Length (Left)
         - M.Num_Overlaps (Model (Left), Model (Right))
         + Length (Right)

         --  Elements of Left and Right are in the result of Union

         and Model (Left) <= Model (Union'Result)
         and Model (Right) <= Model (Union'Result)

         --  Elements of the result of union come from either Left or Right

         and
           M.Included_In_Union
             (Model (Union'Result), Model (Left), Model (Right))

         --  Actual value of elements come from either Left or Right

         and E_Elements_Included
               (Elements (Union'Result),
                Model (Left),
                Elements (Left),
                Elements (Right))

         and E_Elements_Included
               (Elements (Left), Model (Left), Elements (Union'Result))

         and E_Elements_Included
               (Elements (Right),
                Model (Left),
                Elements (Right),
                Elements (Union'Result));

   function "or" (Left, Right : Set) return Set renames Union;

   procedure Intersection (Target : in out Set; Source : Set) with
     Global => null,
     Post   =>
       Length (Target) =
         M.Num_Overlaps (Model (Target)'Old, Model (Source))

         --  Elements of Target were already in Target

         and Model (Target) <= Model (Target)'Old

         --  Elements of Target are in Source

         and Model (Target) <= Model (Source)

         --  Elements both in Source and Target are in the intersection

         and M.Includes_Intersection
               (Model (Target), Model (Source), Model (Target)'Old)

         --  Actual value of elements of Target is preserved

         and E_Elements_Included (Elements (Target), Elements (Target)'Old)
         and E_Elements_Included
               (Elements (Target)'Old, Model (Source), Elements (Target))

         --  Mapping from cursors of Target to elements is preserved

         and Mapping_Preserved
               (E_Left  => Elements (Target),
                E_Right => Elements (Target)'Old,
                P_Left  => Positions (Target),
                P_Right => Positions (Target)'Old);

   function Intersection (Left, Right : Set) return Set with
     Global => null,
     Post   =>
       Length (Intersection'Result) =
         M.Num_Overlaps (Model (Left), Model (Right))

         --  Elements in the result of Intersection are in Left and Right

         and Model (Intersection'Result) <= Model (Left)
         and Model (Intersection'Result) <= Model (Right)

         --  Elements both in Left and Right are in the result of Intersection

         and M.Includes_Intersection
               (Model (Intersection'Result), Model (Left), Model (Right))

         --  Actual value of elements come from Left

         and E_Elements_Included
               (Elements (Intersection'Result), Elements (Left))

         and E_Elements_Included
               (Elements (Left), Model (Right),
                Elements (Intersection'Result));

   function "and" (Left, Right : Set) return Set renames Intersection;

   procedure Difference (Target : in out Set; Source : Set) with
     Global => null,
     Post   =>
       Length (Target) = Length (Target)'Old -
         M.Num_Overlaps (Model (Target)'Old, Model (Source))

         --  Elements of Target were already in Target

         and Model (Target) <= Model (Target)'Old

         --  Elements of Target are not in Source

         and M.No_Overlap (Model (Target), Model (Source))

         --  Elements in Target but not in Source are in the difference

         and M.Included_In_Union
               (Model (Target)'Old, Model (Target), Model (Source))

         --  Actual value of elements of Target is preserved

         and E_Elements_Included (Elements (Target), Elements (Target)'Old)
         and E_Elements_Included
               (Elements (Target)'Old, Model (Target), Elements (Target))

         --  Mapping from cursors of Target to elements is preserved

         and Mapping_Preserved
               (E_Left  => Elements (Target),
                E_Right => Elements (Target)'Old,
                P_Left  => Positions (Target),
                P_Right => Positions (Target)'Old);

   function Difference (Left, Right : Set) return Set with
     Global => null,
     Post   =>
       Length (Difference'Result) = Length (Left) -
         M.Num_Overlaps (Model (Left), Model (Right))

         --  Elements of the result of Difference are in Left

         and Model (Difference'Result) <= Model (Left)

         --  Elements of the result of Difference are in Right

         and M.No_Overlap (Model (Difference'Result), Model (Right))

         --  Elements in Left but not in Right are in the difference

         and M.Included_In_Union
               (Model (Left), Model (Difference'Result), Model (Right))

         --  Actual value of elements come from Left

         and E_Elements_Included
               (Elements (Difference'Result), Elements (Left))

         and E_Elements_Included
               (Elements (Left),
                Model (Difference'Result),
                Elements (Difference'Result));

   function "-" (Left, Right : Set) return Set renames Difference;

   procedure Symmetric_Difference (Target : in out Set; Source : Set) with
     Global => null,
     Pre    =>
       Length (Source) - Length (Target and Source) <=
         Target.Capacity - Length (Target) + Length (Target and Source),
     Post   =>
       Length (Target) = Length (Target)'Old -
         2 * M.Num_Overlaps (Model (Target)'Old, Model (Source)) +
         Length (Source)

         --  Elements of the difference were not both in Source and in Target

         and M.Not_In_Both (Model (Target), Model (Target)'Old, Model (Source))

         --  Elements in Target but not in Source are in the difference

         and M.Included_In_Union
               (Model (Target)'Old, Model (Target), Model (Source))

         --  Elements in Source but not in Target are in the difference

         and M.Included_In_Union
               (Model (Source), Model (Target), Model (Target)'Old)

         --  Actual value of elements come from either Left or Right

         and E_Elements_Included
               (Elements (Target),
                Model (Target)'Old,
                Elements (Target)'Old,
                Elements (Source))

         and E_Elements_Included
               (Elements (Target)'Old, Model (Target), Elements (Target))

         and E_Elements_Included
               (Elements (Source), Model (Target), Elements (Target));

   function Symmetric_Difference (Left, Right : Set) return Set with
     Global => null,
     Pre    => Length (Left) <= Count_Type'Last - Length (Right),
     Post   =>
       Length (Symmetric_Difference'Result) = Length (Left) -
         2 * M.Num_Overlaps (Model (Left), Model (Right)) +
         Length (Right)

         --  Elements of the difference were not both in Left and Right

         and M.Not_In_Both
               (Model (Symmetric_Difference'Result),
                Model (Left),
                Model (Right))

         --  Elements in Left but not in Right are in the difference

         and M.Included_In_Union
               (Model (Left),
                Model (Symmetric_Difference'Result),
                Model (Right))

         --  Elements in Right but not in Left are in the difference

         and M.Included_In_Union
               (Model (Right),
                Model (Symmetric_Difference'Result),
                Model (Left))

         --  Actual value of elements come from either Left or Right

         and E_Elements_Included
               (Elements (Symmetric_Difference'Result),
                Model (Left),
                Elements (Left),
                Elements (Right))

         and E_Elements_Included
               (Elements (Left),
                Model (Symmetric_Difference'Result),
                Elements (Symmetric_Difference'Result))

         and E_Elements_Included
               (Elements (Right),
                Model (Symmetric_Difference'Result),
                Elements (Symmetric_Difference'Result));

   function "xor" (Left, Right : Set) return Set
     renames Symmetric_Difference;

   function Overlap (Left, Right : Set) return Boolean with
     Global => null,
     Post   =>
       Overlap'Result = not (M.No_Overlap (Model (Left), Model (Right)));

   function Is_Subset (Subset : Set; Of_Set : Set) return Boolean with
     Global => null,
     Post   => Is_Subset'Result = (Model (Subset) <= Model (Of_Set));

   function First (Container : Set) return Cursor with
     Global         => null,
     Contract_Cases =>
       (Length (Container) = 0 =>
          First'Result = No_Element,

        others =>
          Has_Element (Container, First'Result)
            and P.Get (Positions (Container), First'Result) = 1);

   function Next (Container : Set; Position : Cursor) return Cursor with
     Global         => null,
     Pre            =>
       Has_Element (Container, Position) or else Position = No_Element,
     Contract_Cases =>
       (Position = No_Element
          or else P.Get (Positions (Container), Position) = Length (Container)
        =>
          Next'Result = No_Element,

        others =>
          Has_Element (Container, Next'Result)
            and then P.Get (Positions (Container), Next'Result) =
                     P.Get (Positions (Container), Position) + 1);

   procedure Next (Container : Set; Position : in out Cursor) with
     Global         => null,
     Pre            =>
       Has_Element (Container, Position) or else Position = No_Element,
     Contract_Cases =>
       (Position = No_Element
          or else P.Get (Positions (Container), Position) = Length (Container)
        =>
          Position = No_Element,

        others =>
          Has_Element (Container, Position)
            and then P.Get (Positions (Container), Position) =
                     P.Get (Positions (Container), Position'Old) + 1);

   function Find
     (Container : Set;
      Item      : Element_Type) return Cursor
   with
     Global         => null,
     Contract_Cases =>

       --  If Item is not contained in Container, Find returns No_Element

       (not Contains (Model (Container), Item) =>
          Find'Result = No_Element,

        --  Otherwise, Find returns a valid cursor in Container

        others =>
          P.Has_Key (Positions (Container), Find'Result)
            and P.Get (Positions (Container), Find'Result) =
                Find (Elements (Container), Item)

            --  The element designated by the result of Find is Item

            and Equivalent_Elements
                  (Element (Container, Find'Result), Item));

   function Contains (Container : Set; Item : Element_Type) return Boolean with
     Global => null,
     Post   => Contains'Result = Contains (Model (Container), Item);
   pragma Annotate (GNATprove, Inline_For_Proof, Contains);

   function Has_Element (Container : Set; Position : Cursor) return Boolean
   with
     Global => null,
     Post   =>
       Has_Element'Result = P.Has_Key (Positions (Container), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Has_Element);

   function Default_Modulus (Capacity : Count_Type) return Hash_Type with
     Global => null;

   generic
      type Key_Type (<>) is private;

      with function Key (Element : Element_Type) return Key_Type;

      with function Hash (Key : Key_Type) return Hash_Type;

      with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;

   package Generic_Keys with SPARK_Mode is

      package Formal_Model with Ghost is

         function M_Included_Except
           (Left  : M.Set;
            Right : M.Set;
            Key   : Key_Type) return Boolean
           with
             Global => null,
             Post   =>
               M_Included_Except'Result =
                 (for all E of Left =>
                   Contains (Right, E)
                     or Equivalent_Keys (Generic_Keys.Key (E), Key));

      end Formal_Model;
      use Formal_Model;

      function Key (Container : Set; Position : Cursor) return Key_Type with
        Global => null,
        Post   => Key'Result = Key (Element (Container, Position));
      pragma Annotate (GNATprove, Inline_For_Proof, Key);

      function Element (Container : Set; Key : Key_Type) return Element_Type
      with
        Global => null,
        Pre    => Contains (Container, Key),
        Post   =>
          Element'Result = Element (Container, Find (Container, Key));
      pragma Annotate (GNATprove, Inline_For_Proof, Element);

      procedure Replace
        (Container : in out Set;
         Key       : Key_Type;
         New_Item  : Element_Type)
      with
        Global => null,
        Pre    => Contains (Container, Key),
        Post   =>
          Length (Container) = Length (Container)'Old

             --  Key now maps to New_Item

             and Element (Container, Key) = New_Item

             --  New_Item is contained in Container

             and Contains (Model (Container), New_Item)

             --  Other elements are preserved

             and M_Included_Except
                   (Model (Container)'Old,
                    Model (Container),
                    Key)
             and M.Included_Except
                   (Model (Container),
                    Model (Container)'Old,
                    New_Item)

             --  Mapping from cursors to elements is preserved

             and Mapping_Preserved_Except
                   (E_Left   => Elements (Container)'Old,
                    E_Right  => Elements (Container),
                    P_Left   => Positions (Container)'Old,
                    P_Right  => Positions (Container),
                    Position => Find (Container, Key))
             and Positions (Container) = Positions (Container)'Old;

      procedure Exclude (Container : in out Set; Key : Key_Type) with
        Global         => null,
        Post           => not Contains (Container, Key),
        Contract_Cases =>

          --  If Key is not in Container, nothing is changed

          (not Contains (Container, Key) =>
             Model (Container) = Model (Container)'Old
               and Elements (Container) = Elements (Container)'Old
               and Positions (Container) = Positions (Container)'Old,

           --  Otherwise, Key is removed from Container

           others =>
             Length (Container) = Length (Container)'Old - 1

               --  Other elements are preserved

               and Model (Container) <= Model (Container)'Old
               and M_Included_Except
                     (Model (Container)'Old,
                      Model (Container),
                      Key)

               --  Mapping from cursors to elements is preserved

               and Mapping_Preserved
                     (E_Left  => Elements (Container),
                      E_Right => Elements (Container)'Old,
                      P_Left  => Positions (Container),
                      P_Right => Positions (Container)'Old)
               and P.Keys_Included_Except
                     (Positions (Container)'Old,
                      Positions (Container),
                      Find (Container, Key)'Old));

      procedure Delete (Container : in out Set; Key : Key_Type) with
        Global => null,
        Pre    => Contains (Container, Key),
        Post   =>
          Length (Container) = Length (Container)'Old - 1

            --  Key is no longer in Container

            and not Contains (Container, Key)

            --  Other elements are preserved

            and Model (Container) <= Model (Container)'Old
            and M_Included_Except
                  (Model (Container)'Old,
                   Model (Container),
                   Key)

            --  Mapping from cursors to elements is preserved

            and Mapping_Preserved
                  (E_Left  => Elements (Container),
                   E_Right => Elements (Container)'Old,
                   P_Left  => Positions (Container),
                   P_Right => Positions (Container)'Old)
            and P.Keys_Included_Except
                  (Positions (Container)'Old,
                   Positions (Container),
                   Find (Container, Key)'Old);

      function Find (Container : Set; Key : Key_Type) return Cursor with
        Global         => null,
        Contract_Cases =>

          --  If Key is not contained in Container, Find returns No_Element

          ((for all E of Model (Container) =>
               not Equivalent_Keys (Key, Generic_Keys.Key (E))) =>
             Find'Result = No_Element,

           --  Otherwise, Find returns a valid cursor in Container

           others =>
             P.Has_Key (Positions (Container), Find'Result)

               --  The key designated by the result of Find is Key

               and Equivalent_Keys
                     (Generic_Keys.Key (Container, Find'Result), Key));

      function Contains (Container : Set; Key : Key_Type) return Boolean with
        Global => null,
        Post   =>
          Contains'Result =
            (for some E of Model (Container) =>
              Equivalent_Keys (Key, Generic_Keys.Key (E)));

   end Generic_Keys;

private
   pragma SPARK_Mode (Off);

   pragma Inline (Next);

   type Node_Type is
      record
         Element     : Element_Type;
         Next        : Count_Type;
         Has_Element : Boolean := False;
      end record;

   package HT_Types is new
     Ada.Containers.Hash_Tables.Generic_Bounded_Hash_Table_Types (Node_Type);

   type Set (Capacity : Count_Type; Modulus : Hash_Type) is
     new HT_Types.Hash_Table_Type (Capacity, Modulus) with null record;

   use HT_Types;

   Empty_Set : constant Set := (Capacity => 0, Modulus => 0, others => <>);

end Ada.Containers.Formal_Hashed_Sets;
