------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . C O N T A I N E R S . F O R M A L _ H A S H E D _ M A P S     --
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

--  This spec is derived from package Ada.Containers.Bounded_Hashed_Maps in the
--  Ada 2012 RM. The modifications are meant to facilitate formal proofs by
--  making it easier to express properties, and by making the specification of
--  this unit compatible with SPARK 2014. Note that the API of this unit may be
--  subject to incompatible changes as SPARK 2014 evolves.

--  The modifications are:

--    A parameter for the container is added to every function reading the
--    contents of a container: Key, Element, Next, Query_Element, Has_Element,
--    Iterate, Equivalent_Keys. This change is motivated by the need to have
--    cursors which are valid on different containers (typically a container C
--    and its previous version C'Old) for expressing properties, which is not
--    possible if cursors encapsulate an access to the underlying container.

--  Iteration over maps is done using the Iterable aspect, which is SPARK
--  compatible. "For of" iteration ranges over keys instead of elements.

with Ada.Containers.Functional_Vectors;
with Ada.Containers.Functional_Maps;
private with Ada.Containers.Hash_Tables;

generic
   type Key_Type is private;
   type Element_Type is private;

   with function Hash (Key : Key_Type) return Hash_Type;
   with function Equivalent_Keys
     (Left  : Key_Type;
      Right : Key_Type) return Boolean is "=";

package Ada.Containers.Formal_Hashed_Maps with
  SPARK_Mode
is
   pragma Annotate (CodePeer, Skip_Analysis);

   type Map (Capacity : Count_Type; Modulus : Hash_Type) is private with
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Key),
     Default_Initial_Condition => Is_Empty (Map);
   pragma Preelaborable_Initialization (Map);

   Empty_Map : constant Map;

   type Cursor is record
      Node : Count_Type;
   end record;

   No_Element : constant Cursor := (Node => 0);

   function Length (Container : Map) return Count_Type with
     Global => null,
     Post   => Length'Result <= Container.Capacity;

   pragma Unevaluated_Use_Of_Old (Allow);

   package Formal_Model with Ghost is
      subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

      package M is new Ada.Containers.Functional_Maps
        (Element_Type    => Element_Type,
         Key_Type        => Key_Type,
         Equivalent_Keys => Equivalent_Keys);

      function "="
        (Left  : M.Map;
         Right : M.Map) return Boolean renames M."=";

      function "<="
        (Left  : M.Map;
         Right : M.Map) return Boolean renames M."<=";

      package K is new Ada.Containers.Functional_Vectors
        (Element_Type => Key_Type,
         Index_Type   => Positive_Count_Type);

      function "="
        (Left  : K.Sequence;
         Right : K.Sequence) return Boolean renames K."=";

      function "<"
        (Left  : K.Sequence;
         Right : K.Sequence) return Boolean renames K."<";

      function "<="
        (Left  : K.Sequence;
         Right : K.Sequence) return Boolean renames K."<=";

      function Find (Container : K.Sequence; Key : Key_Type) return Count_Type
      --  Search for Key in Container

      with
        Global => null,
        Post =>
          (if Find'Result > 0 then
              Find'Result <= K.Length (Container)
                and Equivalent_Keys (Key, K.Get (Container, Find'Result)));

      function K_Keys_Included
        (Left  : K.Sequence;
         Right : K.Sequence) return Boolean
      --  Return True if Right contains all the keys of Left

      with
        Global => null,
        Post   =>
          K_Keys_Included'Result =
            (for all I in 1 .. K.Length (Left) =>
              Find (Right, K.Get (Left, I)) > 0
                and then K.Get (Right, Find (Right, K.Get (Left, I))) =
                         K.Get (Left, I));

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
        (K_Left  : K.Sequence;
         K_Right : K.Sequence;
         P_Left  : P.Map;
         P_Right : P.Map) return Boolean
      with
        Global => null,
        Post   =>
          (if Mapping_Preserved'Result then

             --  Right contains all the cursors of Left

             P.Keys_Included (P_Left, P_Right)

               --  Right contains all the keys of Left

               and K_Keys_Included (K_Left, K_Right)

               --  Mappings from cursors to elements induced by K_Left, P_Left
               --  and K_Right, P_Right are the same.

               and (for all C of P_Left =>
                     K.Get (K_Left, P.Get (P_Left, C)) =
                     K.Get (K_Right, P.Get (P_Right, C))));

      function Model (Container : Map) return M.Map with
      --  The high-level model of a map is a map from keys to elements. Neither
      --  cursors nor order of elements are represented in this model. Keys are
      --  modeled up to equivalence.

        Ghost,
        Global => null;

      function Keys (Container : Map) return K.Sequence with
      --  The Keys sequence represents the underlying list structure of maps
      --  that is used for iteration. It stores the actual values of keys in
      --  the map. It does not model cursors nor elements.

        Ghost,
        Global => null,
        Post   =>
          K.Length (Keys'Result) = Length (Container)

            --  It only contains keys contained in Model

            and (for all Key of Keys'Result =>
                  M.Has_Key (Model (Container), Key))

            --  It contains all the keys contained in Model

            and (for all Key of Model (Container) =>
                  (Find (Keys'Result, Key) > 0
                    and then Equivalent_Keys
                               (K.Get (Keys'Result, Find (Keys'Result, Key)),
                                Key)))

            --  It has no duplicate

            and (for all I in 1 .. Length (Container) =>
                  Find (Keys'Result, K.Get (Keys'Result, I)) = I)

            and (for all I in 1 .. Length (Container) =>
                  (for all J in 1 .. Length (Container) =>
                    (if Equivalent_Keys
                          (K.Get (Keys'Result, I), K.Get (Keys'Result, J))
                     then
                        I = J)));
      pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Keys);

      function Positions (Container : Map) return P.Map with
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

      procedure Lift_Abstraction_Level (Container : Map) with
        --  Lift_Abstraction_Level is a ghost procedure that does nothing but
        --  assume that we can access the same elements by iterating over
        --  positions or cursors.
        --  This information is not generally useful except when switching from
        --  a low-level, cursor-aware view of a container, to a high-level,
        --  position-based view.

        Ghost,
        Global => null,
        Post   =>
          (for all Key of Keys (Container) =>
            (for some I of Positions (Container) =>
              K.Get (Keys (Container), P.Get (Positions (Container), I)) =
                Key));

      function Contains
        (C : M.Map;
         K : Key_Type) return Boolean renames M.Has_Key;
      --  To improve readability of contracts, we rename the function used to
      --  search for a key in the model to Contains.

      function Element
        (C : M.Map;
         K : Key_Type) return Element_Type renames M.Get;
      --  To improve readability of contracts, we rename the function used to
      --  access an element in the model to Element.

   end Formal_Model;
   use Formal_Model;

   function "=" (Left, Right : Map) return Boolean with
     Global => null,
     Post   => "="'Result = (Model (Left) = Model (Right));

   function Capacity (Container : Map) return Count_Type with
     Global => null,
     Post   => Capacity'Result = Container.Capacity;

   procedure Reserve_Capacity
     (Container : in out Map;
      Capacity  : Count_Type)
   with
     Global => null,
     Pre    => Capacity <= Container.Capacity,
     Post   =>
       Model (Container) = Model (Container)'Old
         and Length (Container)'Old = Length (Container)

         --  Actual keys are preserved

         and K_Keys_Included (Keys (Container), Keys (Container)'Old)
         and K_Keys_Included (Keys (Container)'Old, Keys (Container));

   function Is_Empty (Container : Map) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (Length (Container) = 0);

   procedure Clear (Container : in out Map) with
     Global => null,
     Post   => Length (Container) = 0 and M.Is_Empty (Model (Container));

   procedure Assign (Target : in out Map; Source : Map) with
     Global => null,
     Pre    => Target.Capacity >= Length (Source),
     Post   =>
       Model (Target) = Model (Source)
         and Length (Source) = Length (Target)

         --  Actual keys are preserved

         and K_Keys_Included (Keys (Target), Keys (Source))
         and K_Keys_Included (Keys (Source), Keys (Target));

   function Copy
     (Source   : Map;
      Capacity : Count_Type := 0) return Map
   with
     Global => null,
     Pre    => Capacity = 0 or else Capacity >= Source.Capacity,
     Post   =>
       Model (Copy'Result) = Model (Source)
         and Keys (Copy'Result) = Keys (Source)
         and Positions (Copy'Result) = Positions (Source)
         and (if Capacity = 0 then
                 Copy'Result.Capacity = Source.Capacity
              else
                 Copy'Result.Capacity = Capacity);
   --  Copy returns a container stricty equal to Source. It must have the same
   --  cursors associated with each element. Therefore:
   --  - capacity=0 means use Source.Capacity as capacity of target
   --  - the modulus cannot be changed.

   function Key (Container : Map; Position : Cursor) return Key_Type with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Key'Result =
         K.Get (Keys (Container), P.Get (Positions (Container), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Key);

   function Element
     (Container : Map;
      Position  : Cursor) return Element_Type
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Element'Result = Element (Model (Container), Key (Container, Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Element);

   procedure Replace_Element
     (Container : in out Map;
      Position  : Cursor;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>

       --  Order of keys and cursors is preserved

       Keys (Container) = Keys (Container)'Old
         and Positions (Container) = Positions (Container)'Old

         --  New_Item is now associated with the key at position Position in
         --  Container.

         and Element (Container, Position) = New_Item

         --  Elements associated with other keys are preserved

         and M.Same_Keys (Model (Container), Model (Container)'Old)
         and M.Elements_Equal_Except
               (Model (Container),
                Model (Container)'Old,
                Key (Container, Position));

   procedure Move (Target : in out Map; Source : in out Map) with
     Global => null,
     Pre    => Target.Capacity >= Length (Source),
     Post   =>
       Model (Target) = Model (Source)'Old
         and Length (Source)'Old = Length (Target)
         and Length (Source) = 0

         --  Actual keys are preserved

         and K_Keys_Included (Keys (Target), Keys (Source)'Old)
         and K_Keys_Included (Keys (Source)'Old, Keys (Target));

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   with
     Global         => null,
     Pre            =>
       Length (Container) < Container.Capacity or Contains (Container, Key),
     Post           =>
       Contains (Container, Key)
         and Has_Element (Container, Position)
         and Equivalent_Keys
               (Formal_Hashed_Maps.Key (Container, Position), Key),
     Contract_Cases =>

       --  If Key is already in Container, it is not modified and Inserted is
       --  set to False.

       (Contains (Container, Key) =>
          not Inserted
            and Model (Container) = Model (Container)'Old
            and Keys (Container) = Keys (Container)'Old
            and Positions (Container) = Positions (Container)'Old,

        --  Otherwise, Key is inserted in Container and Inserted is set to True

        others =>
          Inserted
            and Length (Container) = Length (Container)'Old + 1

            --  Key now maps to New_Item

            and Formal_Hashed_Maps.Key (Container, Position) = Key
            and Element (Model (Container), Key) = New_Item

            --  Other keys are preserved

            and Model (Container)'Old <= Model (Container)
            and M.Keys_Included_Except
                  (Model (Container),
                   Model (Container)'Old,
                   Key)

            --  Mapping from cursors to keys is preserved

            and Mapping_Preserved
                  (K_Left  => Keys (Container)'Old,
                   K_Right => Keys (Container),
                   P_Left  => Positions (Container)'Old,
                   P_Right => Positions (Container))
            and P.Keys_Included_Except
                  (Positions (Container),
                   Positions (Container)'Old,
                   Position));

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    =>
       Length (Container) < Container.Capacity
        and then (not Contains (Container, Key)),
     Post   =>
       Length (Container) = Length (Container)'Old + 1
         and Contains (Container, Key)

         --  Key now maps to New_Item

         and Formal_Hashed_Maps.Key (Container, Find (Container, Key)) = Key
         and Element (Model (Container), Key) = New_Item

         --  Other keys are preserved

         and Model (Container)'Old <= Model (Container)
         and M.Keys_Included_Except
               (Model (Container),
                Model (Container)'Old,
                Key)

         --  Mapping from cursors to keys is preserved

         and Mapping_Preserved
               (K_Left  => Keys (Container)'Old,
                K_Right => Keys (Container),
                P_Left  => Positions (Container)'Old,
                P_Right => Positions (Container))
         and P.Keys_Included_Except
               (Positions (Container),
                Positions (Container)'Old,
                Find (Container, Key));

   procedure Include
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   with
     Global         => null,
     Pre            =>
       Length (Container) < Container.Capacity or Contains (Container, Key),
     Post           =>
       Contains (Container, Key) and Element (Container, Key) = New_Item,
     Contract_Cases =>

       --  If Key is already in Container, Key is mapped to New_Item

       (Contains (Container, Key) =>

          --  Cursors are preserved

          Positions (Container) = Positions (Container)'Old

            --  The key equivalent to Key in Container is replaced by Key

            and K.Get
                  (Keys (Container),
                   P.Get (Positions (Container), Find (Container, Key))) = Key
            and K.Equal_Except
                  (Keys (Container)'Old,
                   Keys (Container),
                   P.Get (Positions (Container), Find (Container, Key)))

            --  Elements associated with other keys are preserved

            and M.Same_Keys (Model (Container), Model (Container)'Old)
            and M.Elements_Equal_Except
                  (Model (Container),
                   Model (Container)'Old,
                   Key),

        --  Otherwise, Key is inserted in Container

        others =>
          Length (Container) = Length (Container)'Old + 1

            --  Other keys are preserved

            and Model (Container)'Old <= Model (Container)
            and M.Keys_Included_Except
                  (Model (Container),
                   Model (Container)'Old,
                   Key)

            --  Key is inserted in Container

            and K.Get
                  (Keys (Container),
                   P.Get (Positions (Container), Find (Container, Key))) = Key

            --  Mapping from cursors to keys is preserved

            and Mapping_Preserved
                  (K_Left  => Keys (Container)'Old,
                   K_Right => Keys (Container),
                   P_Left  => Positions (Container)'Old,
                   P_Right => Positions (Container))
            and P.Keys_Included_Except
                  (Positions (Container),
                   Positions (Container)'Old,
                   Find (Container, Key)));

   procedure Replace
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Contains (Container, Key),
     Post   =>

       --  Cursors are preserved

       Positions (Container) = Positions (Container)'Old

         --  The key equivalent to Key in Container is replaced by Key

         and K.Get
               (Keys (Container),
                P.Get (Positions (Container), Find (Container, Key))) = Key
         and K.Equal_Except
               (Keys (Container)'Old,
                Keys (Container),
                P.Get (Positions (Container), Find (Container, Key)))

         --  New_Item is now associated with the Key in Container

         and Element (Model (Container), Key) = New_Item

         --  Elements associated with other keys are preserved

         and M.Same_Keys (Model (Container), Model (Container)'Old)
         and M.Elements_Equal_Except
               (Model (Container),
                Model (Container)'Old,
                Key);

   procedure Exclude (Container : in out Map; Key : Key_Type) with
     Global         => null,
     Post           => not Contains (Container, Key),
     Contract_Cases =>

       --  If Key is not in Container, nothing is changed

       (not Contains (Container, Key) =>
          Model (Container) = Model (Container)'Old
            and Keys (Container) = Keys (Container)'Old
            and Positions (Container) = Positions (Container)'Old,

        --  Otherwise, Key is removed from Container

        others =>
          Length (Container) = Length (Container)'Old - 1

            --  Other keys are preserved

            and Model (Container) <= Model (Container)'Old
            and M.Keys_Included_Except
                  (Model (Container)'Old,
                   Model (Container),
                   Key)

            --  Mapping from cursors to keys is preserved

            and Mapping_Preserved
                  (K_Left  => Keys (Container),
                   K_Right => Keys (Container)'Old,
                   P_Left  => Positions (Container),
                   P_Right => Positions (Container)'Old)
            and P.Keys_Included_Except
                  (Positions (Container)'Old,
                   Positions (Container),
                   Find (Container, Key)'Old));

   procedure Delete (Container : in out Map; Key : Key_Type) with
     Global => null,
     Pre    => Contains (Container, Key),
     Post   =>
       Length (Container) = Length (Container)'Old - 1

         --  Key is no longer in Container

         and not Contains (Container, Key)

         --  Other keys are preserved

         and Model (Container) <= Model (Container)'Old
         and M.Keys_Included_Except
               (Model (Container)'Old,
                Model (Container),
                Key)

         --  Mapping from cursors to keys is preserved

         and Mapping_Preserved
               (K_Left  => Keys (Container),
                K_Right => Keys (Container)'Old,
                P_Left  => Positions (Container),
                P_Right => Positions (Container)'Old)
         and P.Keys_Included_Except
               (Positions (Container)'Old,
                Positions (Container),
                Find (Container, Key)'Old);

   procedure Delete (Container : in out Map; Position : in out Cursor) with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Position = No_Element
         and Length (Container) = Length (Container)'Old - 1

         --  The key at position Position is no longer in Container

         and not Contains (Container, Key (Container, Position)'Old)
         and not P.Has_Key (Positions (Container), Position'Old)

         --  Other keys are preserved

         and Model (Container) <= Model (Container)'Old
         and M.Keys_Included_Except
               (Model (Container)'Old,
                Model (Container),
                Key (Container, Position)'Old)

         --  Mapping from cursors to keys is preserved

         and Mapping_Preserved
               (K_Left  => Keys (Container),
                K_Right => Keys (Container)'Old,
                P_Left  => Positions (Container),
                P_Right => Positions (Container)'Old)
         and P.Keys_Included_Except
               (Positions (Container)'Old,
                Positions (Container),
                Position'Old);

   function First (Container : Map) return Cursor with
     Global         => null,
     Contract_Cases =>
       (Length (Container) = 0 =>
          First'Result = No_Element,

        others =>
          Has_Element (Container, First'Result)
            and P.Get (Positions (Container), First'Result) = 1);

   function Next (Container : Map; Position : Cursor) return Cursor with
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

   procedure Next (Container : Map; Position : in out Cursor) with
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

   function Find (Container : Map; Key : Key_Type) return Cursor with
     Global         => null,
     Contract_Cases =>

       --  If Key is not contained in Container, Find returns No_Element

       (not Contains (Model (Container), Key) =>
          Find'Result = No_Element,

        --  Otherwise, Find returns a valid cursor in Container

        others =>
          P.Has_Key (Positions (Container), Find'Result)
            and P.Get (Positions (Container), Find'Result) =
                Find (Keys (Container), Key)

            --  The key designated by the result of Find is Key

            and Equivalent_Keys
                  (Formal_Hashed_Maps.Key (Container, Find'Result), Key));

   function Contains (Container : Map; Key : Key_Type) return Boolean with
     Global => null,
     Post   => Contains'Result = Contains (Model (Container), Key);
   pragma Annotate (GNATprove, Inline_For_Proof, Contains);

   function Element (Container : Map; Key : Key_Type) return Element_Type with
     Global => null,
     Pre    => Contains (Container, Key),
     Post   => Element'Result = Element (Model (Container), Key);
   pragma Annotate (GNATprove, Inline_For_Proof, Element);

   function Has_Element (Container : Map; Position : Cursor) return Boolean
   with
     Global => null,
     Post   =>
       Has_Element'Result = P.Has_Key (Positions (Container), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Has_Element);

   function Default_Modulus (Capacity : Count_Type) return Hash_Type with
     Global => null;

private
   pragma SPARK_Mode (Off);

   pragma Inline (Length);
   pragma Inline (Is_Empty);
   pragma Inline (Clear);
   pragma Inline (Key);
   pragma Inline (Element);
   pragma Inline (Contains);
   pragma Inline (Capacity);
   pragma Inline (Has_Element);
   pragma Inline (Equivalent_Keys);
   pragma Inline (Next);

   type Node_Type is record
      Key         : Key_Type;
      Element     : Element_Type;
      Next        : Count_Type;
      Has_Element : Boolean := False;
   end record;

   package HT_Types is new
     Ada.Containers.Hash_Tables.Generic_Bounded_Hash_Table_Types (Node_Type);

   type Map (Capacity : Count_Type; Modulus : Hash_Type) is
     new HT_Types.Hash_Table_Type (Capacity, Modulus) with null record;

   use HT_Types;

   Empty_Map : constant Map := (Capacity => 0, Modulus => 0, others => <>);

end Ada.Containers.Formal_Hashed_Maps;
