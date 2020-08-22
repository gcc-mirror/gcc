------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 ADA.CONTAINERS.FORMAL_INDEFINITE_VECTORS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2014-2020, Free Software Foundation, Inc.         --
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

--  Similar to Ada.Containers.Formal_Vectors. The main difference is that
--  Element_Type may be indefinite (but not an unconstrained array).

with Ada.Containers.Bounded_Holders;
with Ada.Containers.Functional_Vectors;

generic
   type Index_Type is range <>;
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   Max_Size_In_Storage_Elements : Natural;
   --  Maximum size of Vector elements in bytes. This has the same meaning as
   --  in Ada.Containers.Bounded_Holders, with the same restrictions. Note that
   --  setting this too small can lead to erroneous execution; see comments in
   --  Ada.Containers.Bounded_Holders. If Element_Type is class-wide, it is the
   --  responsibility of clients to calculate the maximum size of all types in
   --  the class.

   Bounded : Boolean := True;
   --  If True, the containers are bounded; the initial capacity is the maximum
   --  size, and heap allocation will be avoided. If False, the containers can
   --  grow via heap allocation.

package Ada.Containers.Formal_Indefinite_Vectors with
  SPARK_Mode => On
is
   pragma Annotate (CodePeer, Skip_Analysis);

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
           Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   Last_Count : constant Count_Type :=
     (if Index_Type'Last < Index_Type'First then
         0
      elsif Index_Type'Last < -1
        or else Index_Type'Pos (Index_Type'First) >
                Index_Type'Pos (Index_Type'Last) - Count_Type'Last
      then
         Index_Type'Pos (Index_Type'Last) -
           Index_Type'Pos (Index_Type'First) + 1
      else
         Count_Type'Last);
   --  Maximal capacity of any vector. It is the minimum of the size of the
   --  index range and the last possible Count_Type.

   subtype Capacity_Range is Count_Type range 0 .. Last_Count;

   type Vector (Capacity : Capacity_Range) is limited private with
     Default_Initial_Condition => Is_Empty (Vector);
   --  In the bounded case, Capacity is the capacity of the container, which
   --  never changes. In the unbounded case, Capacity is the initial capacity
   --  of the container, and operations such as Reserve_Capacity and Append can
   --  increase the capacity. The capacity never shrinks, except in the case of
   --  Clear.
   --
   --  Note that all objects of type Vector are constrained, including in the
   --  unbounded case; you can't assign from one object to another if the
   --  Capacity is different.

   function Length (Container : Vector) return Capacity_Range with
     Global => null,
     Post   => Length'Result <= Capacity (Container);

   pragma Unevaluated_Use_Of_Old (Allow);

   package Formal_Model with Ghost is

      package M is new Ada.Containers.Functional_Vectors
        (Index_Type   => Index_Type,
         Element_Type => Element_Type);

      function "="
        (Left  : M.Sequence;
         Right : M.Sequence) return Boolean renames M."=";

      function "<"
        (Left  : M.Sequence;
         Right : M.Sequence) return Boolean renames M."<";

      function "<="
        (Left  : M.Sequence;
         Right : M.Sequence) return Boolean renames M."<=";

      function M_Elements_In_Union
        (Container : M.Sequence;
         Left      : M.Sequence;
         Right     : M.Sequence) return Boolean
      --  The elements of Container are contained in either Left or Right
      with
        Global => null,
        Post   =>
          M_Elements_In_Union'Result =
            (for all I in Index_Type'First .. M.Last (Container) =>
              (for some J in Index_Type'First .. M.Last (Left) =>
                Element (Container, I) = Element (Left, J))
                  or (for some J in Index_Type'First .. M.Last (Right) =>
                       Element (Container, I) = Element (Right, J)));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_In_Union);

      function M_Elements_Included
        (Left  : M.Sequence;
         L_Fst : Index_Type := Index_Type'First;
         L_Lst : Extended_Index;
         Right : M.Sequence;
         R_Fst : Index_Type := Index_Type'First;
         R_Lst : Extended_Index) return Boolean
      --  The elements of the slice from L_Fst to L_Lst in Left are contained
      --  in the slide from R_Fst to R_Lst in Right.
      with
        Global => null,
        Pre    => L_Lst <= M.Last (Left) and R_Lst <= M.Last (Right),
        Post   =>
          M_Elements_Included'Result =
            (for all I in L_Fst .. L_Lst =>
              (for some J in R_Fst .. R_Lst =>
                Element (Left, I) = Element (Right, J)));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Included);

      function M_Elements_Reversed
        (Left  : M.Sequence;
         Right : M.Sequence) return Boolean
      --  Right is Left in reverse order
      with
        Global => null,
        Post   =>
          M_Elements_Reversed'Result =
            (M.Length (Left) = M.Length (Right)
              and (for all I in Index_Type'First .. M.Last (Left) =>
                    Element (Left, I) =
                      Element (Right, M.Last (Left) - I + 1))
              and (for all I in Index_Type'First .. M.Last (Right) =>
                    Element (Right, I) =
                      Element (Left, M.Last (Left) - I + 1)));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Reversed);

      function M_Elements_Swapped
        (Left  : M.Sequence;
         Right : M.Sequence;
         X     : Index_Type;
         Y     : Index_Type) return Boolean
      --  Elements stored at X and Y are reversed in Left and Right
      with
        Global => null,
        Pre    => X <= M.Last (Left) and Y <= M.Last (Left),
        Post   =>
          M_Elements_Swapped'Result =
            (M.Length (Left) = M.Length (Right)
              and Element (Left, X) = Element (Right, Y)
              and Element (Left, Y) = Element (Right, X)
              and M.Equal_Except (Left, Right, X, Y));
      pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Swapped);

      function Model (Container : Vector) return M.Sequence with
      --  The high-level model of a vector is a sequence of elements. The
      --  sequence really is similar to the vector itself. However, it is not
      --  limited which allows usage of 'Old and 'Loop_Entry attributes.

        Ghost,
        Global => null,
        Post   => M.Length (Model'Result) = Length (Container);

      function Element
        (S : M.Sequence;
         I : Index_Type) return Element_Type renames M.Get;
      --  To improve readability of contracts, we rename the function used to
      --  access an element in the model to Element.

   end Formal_Model;
   use Formal_Model;

   function Empty_Vector return Vector with
     Global => null,
     Post   => Length (Empty_Vector'Result) = 0;

   function "=" (Left, Right : Vector) return Boolean with
     Global => null,
     Post   => "="'Result = (Model (Left) = Model (Right));

   function To_Vector
     (New_Item : Element_Type;
      Length   : Capacity_Range) return Vector
   with
     Global => null,
     Post   =>
       Formal_Indefinite_Vectors.Length (To_Vector'Result) = Length
         and M.Constant_Range
               (Container => Model (To_Vector'Result),
                Fst       => Index_Type'First,
                Lst       => Last_Index (To_Vector'Result),
                Item      => New_Item);

   function Capacity (Container : Vector) return Capacity_Range with
     Global => null,
     Post   =>
       Capacity'Result =
         (if Bounded then
             Container.Capacity
          else
             Capacity_Range'Last);
   pragma Annotate (GNATprove, Inline_For_Proof, Capacity);

   procedure Reserve_Capacity
     (Container : in out Vector;
      Capacity  : Capacity_Range)
   with
     Global => null,
     Pre    => (if Bounded then Capacity <= Container.Capacity),
     Post   => Model (Container) = Model (Container)'Old;

   function Is_Empty (Container : Vector) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (Length (Container) = 0);

   procedure Clear (Container : in out Vector) with
     Global => null,
     Post   => Length (Container) = 0;
   --  Note that this reclaims storage in the unbounded case. You need to call
   --  this before a container goes out of scope in order to avoid storage
   --  leaks. In addition, "X := ..." can leak unless you Clear(X) first.

   procedure Assign (Target : in out Vector; Source : Vector) with
     Global => null,
     Pre    => (if Bounded then Length (Source) <= Target.Capacity),
     Post   => Model (Target) = Model (Source);

   function Copy
     (Source   : Vector;
      Capacity : Capacity_Range := 0) return Vector
   with
     Global => null,
     Pre    => (if Bounded then (Capacity = 0 or Length (Source) <= Capacity)),
     Post   =>
       Model (Copy'Result) = Model (Source)
         and (if Capacity = 0 then
                 Copy'Result.Capacity = Length (Source)
              else
                 Copy'Result.Capacity = Capacity);

   procedure Move (Target : in out Vector; Source : in out Vector)
   with
     Global => null,
     Pre    => (if Bounded then Length (Source) <= Capacity (Target)),
     Post   => Model (Target) = Model (Source)'Old and Length (Source) = 0;

   function Element
     (Container : Vector;
      Index     : Index_Type) return Element_Type
   with
     Global => null,
     Pre    => Index in First_Index (Container) .. Last_Index (Container),
     Post   => Element'Result = Element (Model (Container), Index);
   pragma Annotate (GNATprove, Inline_For_Proof, Element);

   procedure Replace_Element
     (Container : in out Vector;
      Index     : Index_Type;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => Index in First_Index (Container) .. Last_Index (Container),
     Post   =>
       Length (Container) = Length (Container)'Old

         --  Container now has New_Item at index Index

         and Element (Model (Container), Index) = New_Item

         --  All other elements are preserved

         and M.Equal_Except
               (Left     => Model (Container)'Old,
                Right    => Model (Container),
                Position => Index);

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Vector)
   with
     Global => null,
     Pre    =>
       Length (Container) <= Capacity (Container) - Length (New_Item)
         and (Before in Index_Type'First .. Last_Index (Container)
               or (Before /= No_Index
                    and then Before - 1 = Last_Index (Container))),
     Post   =>
       Length (Container) = Length (Container)'Old + Length (New_Item)

         --  Elements located before Before in Container are preserved

         and M.Range_Equal
               (Left  => Model (Container)'Old,
                Right => Model (Container),
                Fst   => Index_Type'First,
                Lst   => Before - 1)

         --  Elements of New_Item are inserted at position Before

         and (if Length (New_Item) > 0 then
                 M.Range_Shifted
                   (Left   => Model (New_Item),
                     Right  => Model (Container),
                     Fst    => Index_Type'First,
                     Lst    => Last_Index (New_Item),
                     Offset => Count_Type (Before - Index_Type'First)))

         --  Elements located after Before in Container are shifted

         and M.Range_Shifted
               (Left   => Model (Container)'Old,
                Right  => Model (Container),
                Fst    => Before,
                Lst    => Last_Index (Container)'Old,
                Offset => Length (New_Item));

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    =>
       Length (Container) < Capacity (Container)
         and then (Before in Index_Type'First .. Last_Index (Container) + 1),
     Post   =>
       Length (Container) = Length (Container)'Old + 1

         --  Elements located before Before in Container are preserved

         and M.Range_Equal
               (Left  => Model (Container)'Old,
                Right => Model (Container),
                Fst   => Index_Type'First,
                Lst   => Before - 1)

         --  Container now has New_Item at index Before

         and Element (Model (Container), Before) = New_Item

         --  Elements located after Before in Container are shifted by 1

         and M.Range_Shifted
               (Left   => Model (Container)'Old,
                Right  => Model (Container),
                Fst    => Before,
                Lst    => Last_Index (Container)'Old,
                Offset => 1);

   procedure Insert
     (Container : in out Vector;
      Before    : Extended_Index;
      New_Item  : Element_Type;
      Count     : Count_Type)
   with
     Global => null,
     Pre    =>
       Length (Container) <= Capacity (Container) - Count
         and (Before in Index_Type'First .. Last_Index (Container)
               or (Before /= No_Index
                    and then Before - 1 = Last_Index (Container))),
     Post   =>
       Length (Container) = Length (Container)'Old + Count

         --  Elements located before Before in Container are preserved

         and M.Range_Equal
               (Left  => Model (Container)'Old,
                Right => Model (Container),
                Fst   => Index_Type'First,
                Lst   => Before - 1)

         --  New_Item is inserted Count times at position Before

         and (if Count > 0 then
                 M.Constant_Range
                   (Container => Model (Container),
                     Fst       => Before,
                     Lst       => Before + Index_Type'Base (Count - 1),
                     Item      => New_Item))

         --  Elements located after Before in Container are shifted

         and M.Range_Shifted
               (Left   => Model (Container)'Old,
                Right  => Model (Container),
                Fst    => Before,
                Lst    => Last_Index (Container)'Old,
                Offset => Count);

   procedure Prepend (Container : in out Vector; New_Item : Vector) with
     Global => null,
     Pre    => Length (Container) <= Capacity (Container) - Length (New_Item),
     Post   =>
       Length (Container) = Length (Container)'Old + Length (New_Item)

         --  Elements of New_Item are inserted at the beginning of Container

         and M.Range_Equal
               (Left  => Model (New_Item),
                Right => Model (Container),
                Fst   => Index_Type'First,
                Lst   => Last_Index (New_Item))

         --  Elements of Container are shifted

         and M.Range_Shifted
               (Left   => Model (Container)'Old,
                Right  => Model (Container),
                Fst    => Index_Type'First,
                Lst    => Last_Index (Container)'Old,
                Offset => Length (New_Item));

   procedure Prepend (Container : in out Vector; New_Item : Element_Type) with
     Global => null,
     Pre    => Length (Container) < Capacity (Container),
     Post   =>
       Length (Container) = Length (Container)'Old + 1

         --  Container now has New_Item at Index_Type'First

         and Element (Model (Container), Index_Type'First) = New_Item

         --  Elements of Container are shifted by 1

         and M.Range_Shifted
               (Left   => Model (Container)'Old,
                Right  => Model (Container),
                Fst    => Index_Type'First,
                Lst    => Last_Index (Container)'Old,
                Offset => 1);

   procedure Prepend
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type)
   with
     Global => null,
     Pre    => Length (Container) <= Capacity (Container) - Count,
     Post   =>
       Length (Container) = Length (Container)'Old + Count

         --  New_Item is inserted Count times at the beginning of Container

         and M.Constant_Range
               (Container => Model (Container),
                Fst       => Index_Type'First,
                Lst       => Index_Type'First + Index_Type'Base (Count - 1),
                Item      => New_Item)

         --  Elements of Container are shifted

         and M.Range_Shifted
               (Left   => Model (Container)'Old,
                Right  => Model (Container),
                Fst    => Index_Type'First,
                Lst    => Last_Index (Container)'Old,
                Offset => Count);

   procedure Append (Container : in out Vector; New_Item : Vector) with
     Global         => null,
     Pre            =>
       Length (Container) <= Capacity (Container) - Length (New_Item),
     Post           =>
       Length (Container) = Length (Container)'Old + Length (New_Item)

         --  The elements of Container are preserved

         and Model (Container)'Old <= Model (Container)

         --  Elements of New_Item are inserted at the end of Container

         and (if Length (New_Item) > 0 then
                 M.Range_Shifted
                   (Left   => Model (New_Item),
                    Right  => Model (Container),
                    Fst    => Index_Type'First,
                    Lst    => Last_Index (New_Item),
                    Offset =>
                      Count_Type
                        (Last_Index (Container)'Old - Index_Type'First + 1)));

   procedure Append (Container : in out Vector; New_Item : Element_Type) with
     Global => null,
     Pre    => Length (Container) < Capacity (Container),
     Post   =>
       Length (Container) = Length (Container)'Old + 1

         --  Elements of Container are preserved

         and Model (Container)'Old < Model (Container)

         --  Container now has New_Item at the end of Container

         and Element
               (Model (Container), Last_Index (Container)'Old + 1) = New_Item;

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type;
      Count     : Count_Type)
   with
     Global => null,
     Pre    => Length (Container) <= Capacity (Container) - Count,
     Post   =>
       Length (Container) = Length (Container)'Old + Count

         --  Elements of Container are preserved

         and Model (Container)'Old <= Model (Container)

         --  New_Item is inserted Count times at the end of Container

         and (if Count > 0 then
                 M.Constant_Range
                   (Container => Model (Container),
                     Fst       => Last_Index (Container)'Old + 1,
                     Lst       =>
                       Last_Index (Container)'Old + Index_Type'Base (Count),
                     Item      => New_Item));

   procedure Delete (Container : in out Vector; Index : Extended_Index) with
     Global => null,
     Pre    => Index in First_Index (Container) .. Last_Index (Container),
     Post   =>
       Length (Container) = Length (Container)'Old - 1

         --  Elements located before Index in Container are preserved

         and M.Range_Equal
               (Left  => Model (Container)'Old,
                Right => Model (Container),
                Fst   => Index_Type'First,
                Lst   => Index - 1)

         --  Elements located after Index in Container are shifted by 1

         and M.Range_Shifted
               (Left   => Model (Container),
                Right  => Model (Container)'Old,
                Fst    => Index,
                Lst    => Last_Index (Container),
                Offset => 1);

   procedure Delete
     (Container : in out Vector;
      Index     : Extended_Index;
      Count     : Count_Type)
   with
     Global         => null,
     Pre            =>
       Index in First_Index (Container) .. Last_Index (Container),
     Post           =>
       Length (Container) in
         Length (Container)'Old - Count .. Length (Container)'Old

         --  The elements of Container located before Index are preserved.

         and M.Range_Equal
               (Left  => Model (Container)'Old,
                Right => Model (Container),
                Fst   => Index_Type'First,
                Lst   => Index - 1),

     Contract_Cases =>

       --  All the elements after Position have been erased

       (Length (Container) - Count <= Count_Type (Index - Index_Type'First) =>
          Length (Container) = Count_Type (Index - Index_Type'First),

        others =>
          Length (Container) = Length (Container)'Old - Count

            --  Other elements are shifted by Count

            and M.Range_Shifted
                  (Left   => Model (Container),
                   Right  => Model (Container)'Old,
                   Fst    => Index,
                   Lst    => Last_Index (Container),
                   Offset => Count));

   procedure Delete_First (Container : in out Vector) with
     Global => null,
     Pre    => Length (Container) > 0,
     Post   =>
       Length (Container) = Length (Container)'Old - 1

         --  Elements of Container are shifted by 1

         and M.Range_Shifted
               (Left   => Model (Container),
                Right  => Model (Container)'Old,
                Fst    => Index_Type'First,
                Lst    => Last_Index (Container),
                Offset => 1);

   procedure Delete_First (Container : in out Vector; Count : Count_Type) with
     Global         => null,
     Contract_Cases =>

       --  All the elements of Container have been erased

       (Length (Container) <= Count => Length (Container) = 0,

        others =>
          Length (Container) = Length (Container)'Old - Count

            --  Elements of Container are shifted by Count

            and M.Range_Shifted
                  (Left   => Model (Container),
                   Right  => Model (Container)'Old,
                   Fst    => Index_Type'First,
                   Lst    => Last_Index (Container),
                   Offset => Count));

   procedure Delete_Last (Container : in out Vector) with
     Global => null,
     Pre    => Length (Container) > 0,
     Post   =>
       Length (Container) = Length (Container)'Old - 1

         --  Elements of Container are preserved

         and Model (Container) < Model (Container)'Old;

   procedure Delete_Last (Container : in out Vector; Count : Count_Type) with
     Global         => null,
     Contract_Cases =>

       --  All the elements after Position have been erased

       (Length (Container) <= Count => Length (Container) = 0,

        others =>
          Length (Container) = Length (Container)'Old - Count

            --  The elements of Container are preserved

            and Model (Container) <= Model (Container)'Old);

   procedure Reverse_Elements (Container : in out Vector) with
     Global => null,
     Post   => M_Elements_Reversed (Model (Container)'Old, Model (Container));

   procedure Swap
     (Container : in out Vector;
      I         : Index_Type;
      J         : Index_Type)
   with
     Global => null,
     Pre    =>
       I in First_Index (Container) .. Last_Index (Container)
         and then J in First_Index (Container) .. Last_Index (Container),
     Post   =>
       M_Elements_Swapped (Model (Container)'Old, Model (Container), I, J);

   function First_Index (Container : Vector) return Index_Type with
     Global => null,
     Post   => First_Index'Result = Index_Type'First;
   pragma Annotate (GNATprove, Inline_For_Proof, First_Index);

   function First_Element (Container : Vector) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   =>
       First_Element'Result = Element (Model (Container), Index_Type'First);
   pragma Annotate (GNATprove, Inline_For_Proof, First_Element);

   function Last_Index (Container : Vector) return Extended_Index with
     Global => null,
     Post   => Last_Index'Result = M.Last (Model (Container));
   pragma Annotate (GNATprove, Inline_For_Proof, Last_Index);

   function Last_Element (Container : Vector) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   =>
       Last_Element'Result =
         Element (Model (Container), Last_Index (Container));
   pragma Annotate (GNATprove, Inline_For_Proof, Last_Element);

   function Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   with
     Global         => null,
     Contract_Cases =>

       --  If Item is not contained in Container after Index, Find_Index
       --  returns No_Index.

       (Index > Last_Index (Container)
         or else not M.Contains
                       (Container => Model (Container),
                        Fst       => Index,
                        Lst       => Last_Index (Container),
                        Item      => Item)
        =>
          Find_Index'Result = No_Index,

        --  Otherwise, Find_Index returns a valid index greater than Index

        others =>
           Find_Index'Result in Index .. Last_Index (Container)

            --  The element at this index in Container is Item

            and Element (Model (Container), Find_Index'Result) = Item

            --  It is the first occurrence of Item after Index in Container

            and not M.Contains
                      (Container => Model (Container),
                       Fst       => Index,
                       Lst       => Find_Index'Result - 1,
                       Item      => Item));

   function Reverse_Find_Index
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   with
     Global         => null,
     Contract_Cases =>

       --  If Item is not contained in Container before Index,
       --  Reverse_Find_Index returns No_Index.

       (not M.Contains
              (Container => Model (Container),
               Fst       => Index_Type'First,
               Lst       => (if Index <= Last_Index (Container) then Index
                             else Last_Index (Container)),
               Item      => Item)
        =>
          Reverse_Find_Index'Result = No_Index,

        --  Otherwise, Reverse_Find_Index returns a valid index smaller than
        --  Index

        others =>
           Reverse_Find_Index'Result in Index_Type'First .. Index
            and Reverse_Find_Index'Result <= Last_Index (Container)

            --  The element at this index in Container is Item

            and Element (Model (Container), Reverse_Find_Index'Result) = Item

            --  It is the last occurrence of Item before Index in Container

            and not M.Contains
                      (Container => Model (Container),
                       Fst       => Reverse_Find_Index'Result + 1,
                       Lst       =>
                         (if Index <= Last_Index (Container) then
                             Index
                          else
                             Last_Index (Container)),
                       Item      => Item));

   function Contains
     (Container : Vector;
      Item      : Element_Type) return Boolean
   with
     Global => null,
     Post   =>
       Contains'Result =
         M.Contains
           (Container => Model (Container),
            Fst       => Index_Type'First,
            Lst       => Last_Index (Container),
            Item      => Item);

   function Has_Element
     (Container : Vector;
      Position  : Extended_Index) return Boolean
   with
     Global => null,
     Post   =>
       Has_Element'Result =
         (Position in Index_Type'First .. Last_Index (Container));
   pragma Annotate (GNATprove, Inline_For_Proof, Has_Element);

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   package Generic_Sorting with SPARK_Mode is

      package Formal_Model with Ghost is

         function M_Elements_Sorted (Container : M.Sequence) return Boolean
         with
           Global => null,
           Post   =>
             M_Elements_Sorted'Result =
               (for all I in Index_Type'First .. M.Last (Container) =>
                 (for all J in I .. M.Last (Container) =>
                   Element (Container, I) = Element (Container, J)
                     or Element (Container, I) < Element (Container, J)));
         pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Sorted);

      end Formal_Model;
      use Formal_Model;

      function Is_Sorted (Container : Vector) return Boolean with
        Global => null,
        Post   => Is_Sorted'Result = M_Elements_Sorted (Model (Container));

      procedure Sort (Container : in out Vector) with
        Global => null,
        Post   =>
          Length (Container) = Length (Container)'Old
            and M_Elements_Sorted (Model (Container))
            and M_Elements_Included
                  (Left  => Model (Container)'Old,
                   L_Lst => Last_Index (Container),
                   Right => Model (Container),
                   R_Lst => Last_Index (Container))
            and M_Elements_Included
                  (Left  => Model (Container),
                   L_Lst => Last_Index (Container),
                   Right => Model (Container)'Old,
                   R_Lst => Last_Index (Container));

      procedure Merge (Target : in out Vector; Source : in out Vector) with
      --  Target and Source should not be aliased
        Global => null,
        Pre    => Length (Source) <= Capacity (Target) - Length (Target),
        Post   =>
          Length (Target) = Length (Target)'Old + Length (Source)'Old
            and Length (Source) = 0
            and (if M_Elements_Sorted (Model (Target)'Old)
                   and M_Elements_Sorted (Model (Source)'Old)
                 then
                    M_Elements_Sorted (Model (Target)))
            and M_Elements_Included
                  (Left  => Model (Target)'Old,
                   L_Lst => Last_Index (Target)'Old,
                   Right => Model (Target),
                   R_Lst => Last_Index (Target))
            and M_Elements_Included
                  (Left  => Model (Source)'Old,
                   L_Lst => Last_Index (Source)'Old,
                   Right => Model (Target),
                   R_Lst => Last_Index (Target))
            and M_Elements_In_Union
                  (Model (Target),
                   Model (Source)'Old,
                   Model (Target)'Old);
   end Generic_Sorting;

private
   pragma SPARK_Mode (Off);

   pragma Inline (First_Index);
   pragma Inline (Last_Index);
   pragma Inline (Element);
   pragma Inline (First_Element);
   pragma Inline (Last_Element);
   pragma Inline (Replace_Element);
   pragma Inline (Contains);

   --  The implementation method is to instantiate Bounded_Holders to get a
   --  definite type for Element_Type.

   package Holders is new Bounded_Holders
     (Element_Type, Max_Size_In_Storage_Elements, "=");
   use Holders;

   subtype Array_Index is Capacity_Range range 1 .. Capacity_Range'Last;
   type Elements_Array is array (Array_Index range <>) of Holder;
   function "=" (L, R : Elements_Array) return Boolean is abstract;

   type Elements_Array_Ptr is access all Elements_Array;

   type Vector (Capacity : Capacity_Range) is limited record

      --  In the bounded case, the elements are stored in Elements. In the
      --  unbounded case, the elements are initially stored in Elements, until
      --  we run out of room, then we switch to Elements_Ptr.

      Last         : Extended_Index := No_Index;
      Elements_Ptr : Elements_Array_Ptr := null;
      Elements     : aliased Elements_Array (1 .. Capacity);
   end record;

   --  The primary reason Vector is limited is that in the unbounded case, once
   --  Elements_Ptr is in use, assignment statements won't work. "X := Y;" will
   --  cause X and Y to share state; that is, X.Elements_Ptr = Y.Elements_Ptr,
   --  so for example "Append (X, ...);" will modify BOTH X and Y. That would
   --  allow SPARK to "prove" things that are false. We could fix that by
   --  making Vector a controlled type, and override Adjust to make a deep
   --  copy, but finalization is not allowed in SPARK.
   --
   --  Note that (unfortunately) this means that 'Old and 'Loop_Entry are not
   --  allowed on Vectors.

   function Empty_Vector return Vector is
     ((Capacity => 0, others => <>));

end Ada.Containers.Formal_Indefinite_Vectors;
