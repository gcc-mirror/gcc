------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                    ADA.CONTAINERS.FUNCTIONAL_VECTORS                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2016-2021, Free Software Foundation, Inc.         --
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

pragma Ada_2012;
private with Ada.Containers.Functional_Base;

generic
   type Index_Type is (<>);
   --  To avoid Constraint_Error being raised at run time, Index_Type'Base
   --  should have at least one more element at the low end than Index_Type.

   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Ada.Containers.Functional_Vectors with SPARK_Mode is

   subtype Extended_Index is Index_Type'Base range
     Index_Type'Pred (Index_Type'First) .. Index_Type'Last;
   --  Index_Type with one more element at the low end of the range.
   --  This type is never used but it forces GNATprove to check that there is
   --  room for one more element at the low end of Index_Type.

   type Sequence is private
     with Default_Initial_Condition => Length (Sequence) = 0,
     Iterable => (First       => Iter_First,
                  Has_Element => Iter_Has_Element,
                  Next        => Iter_Next,
                  Element     => Get);
   --  Sequences are empty when default initialized.
   --  Quantification over sequences can be done using the regular
   --  quantification over its range or directly on its elements with "for of".

   -----------------------
   --  Basic operations --
   -----------------------

   --  Sequences are axiomatized using Length and Get, providing respectively
   --  the length of a sequence and an accessor to its Nth element:

   function Length (Container : Sequence) return Count_Type with
   --  Length of a sequence

     Global => null,
     Post   =>
       (Index_Type'Pos (Index_Type'First) - 1) + Length'Result <=
         Index_Type'Pos (Index_Type'Last);

   function Get
     (Container : Sequence;
      Position  : Extended_Index) return Element_Type
   --  Access the Element at position Position in Container

   with
     Global => null,
     Pre    => Position in Index_Type'First .. Last (Container);

   function Last (Container : Sequence) return Extended_Index with
   --  Last index of a sequence

     Global => null,
     Post =>
       Last'Result =
         Index_Type'Val ((Index_Type'Pos (Index_Type'First) - 1) +
           Length (Container));
   pragma Annotate (GNATprove, Inline_For_Proof, Last);

   function First return Extended_Index is (Index_Type'First) with
     Global => null;
   --  First index of a sequence

   ------------------------
   -- Property Functions --
   ------------------------

   function "=" (Left : Sequence; Right : Sequence) return Boolean with
   --  Extensional equality over sequences

     Global => null,
     Post   =>
       "="'Result =
         (Length (Left) = Length (Right)
           and then (for all N in Index_Type'First .. Last (Left) =>
                      Get (Left, N) = Get (Right, N)));
   pragma Annotate (GNATprove, Inline_For_Proof, "=");

   function "<" (Left : Sequence; Right : Sequence) return Boolean with
   --  Left is a strict subsequence of Right

     Global => null,
     Post   =>
       "<"'Result =
         (Length (Left) < Length (Right)
           and then (for all N in Index_Type'First .. Last (Left) =>
                      Get (Left, N) = Get (Right, N)));
   pragma Annotate (GNATprove, Inline_For_Proof, "<");

   function "<=" (Left : Sequence; Right : Sequence) return Boolean with
   --  Left is a subsequence of Right

     Global => null,
     Post   =>
       "<="'Result =
         (Length (Left) <= Length (Right)
           and then (for all N in Index_Type'First .. Last (Left) =>
                      Get (Left, N) = Get (Right, N)));
   pragma Annotate (GNATprove, Inline_For_Proof, "<=");

   function Contains
     (Container : Sequence;
      Fst       : Index_Type;
      Lst       : Extended_Index;
      Item      : Element_Type) return Boolean
   --  Returns True if Item occurs in the range from Fst to Lst of Container

   with
     Global => null,
     Pre    => Lst <= Last (Container),
     Post   =>
       Contains'Result =
         (for some I in Fst .. Lst => Get (Container, I) = Item);
   pragma Annotate (GNATprove, Inline_For_Proof, Contains);

   function Constant_Range
     (Container : Sequence;
      Fst       : Index_Type;
      Lst       : Extended_Index;
      Item      : Element_Type) return Boolean
   --  Returns True if every element of the range from Fst to Lst of Container
   --  is equal to Item.

   with
     Global => null,
     Pre    => Lst <= Last (Container),
     Post   =>
       Constant_Range'Result =
         (for all I in Fst .. Lst => Get (Container, I) = Item);
   pragma Annotate (GNATprove, Inline_For_Proof, Constant_Range);

   function Equal_Except
     (Left     : Sequence;
      Right    : Sequence;
      Position : Index_Type) return Boolean
   --  Returns True is Left and Right are the same except at position Position

   with
     Global => null,
     Pre    => Position <= Last (Left),
     Post   =>
       Equal_Except'Result =
         (Length (Left) = Length (Right)
           and then (for all I in Index_Type'First .. Last (Left) =>
                      (if I /= Position then Get (Left, I) = Get (Right, I))));
   pragma Annotate (GNATprove, Inline_For_Proof, Equal_Except);

   function Equal_Except
     (Left  : Sequence;
      Right : Sequence;
      X     : Index_Type;
      Y     : Index_Type) return Boolean
   --  Returns True is Left and Right are the same except at positions X and Y

   with
     Global => null,
     Pre    => X <= Last (Left) and Y <= Last (Left),
     Post   =>
       Equal_Except'Result =
         (Length (Left) = Length (Right)
           and then (for all I in Index_Type'First .. Last (Left) =>
                      (if I /= X and I /= Y then
                          Get (Left, I) = Get (Right, I))));
   pragma Annotate (GNATprove, Inline_For_Proof, Equal_Except);

   function Range_Equal
     (Left  : Sequence;
      Right : Sequence;
      Fst   : Index_Type;
      Lst   : Extended_Index) return Boolean
   --  Returns True if the ranges from Fst to Lst contain the same elements in
   --  Left and Right.

   with
     Global => null,
     Pre    => Lst <= Last (Left) and Lst <= Last (Right),
     Post   =>
       Range_Equal'Result =
         (for all I in Fst .. Lst => Get (Left, I) = Get (Right, I));
   pragma Annotate (GNATprove, Inline_For_Proof, Range_Equal);

   function Range_Shifted
     (Left   : Sequence;
      Right  : Sequence;
      Fst    : Index_Type;
      Lst    : Extended_Index;
      Offset : Count_Type'Base) return Boolean
   --  Returns True if the range from Fst to Lst in Left contains the same
   --  elements as the range from Fst + Offset to Lst + Offset in Right.

   with
     Global => null,
     Pre    =>
       Lst <= Last (Left)
         and then
           (if Offset < 0 then
              Index_Type'Pos (Index_Type'Base'First) - Offset <=
              Index_Type'Pos (Index_Type'First))
         and then
           (if Fst <= Lst then
              Offset in
                Index_Type'Pos (Index_Type'First) - Index_Type'Pos (Fst) ..
                  (Index_Type'Pos (Index_Type'First) - 1) + Length (Right) -
                   Index_Type'Pos (Lst)),
     Post   =>
       Range_Shifted'Result =
         ((for all I in Fst .. Lst =>
            Get (Left, I) =
            Get (Right, Index_Type'Val (Index_Type'Pos (I) + Offset)))
          and
            (for all I in Index_Type'Val (Index_Type'Pos (Fst) + Offset) ..
               Index_Type'Val (Index_Type'Pos (Lst) + Offset)
             =>
               Get (Left, Index_Type'Val (Index_Type'Pos (I) - Offset)) =
               Get (Right, I)));
   pragma Annotate (GNATprove, Inline_For_Proof, Range_Shifted);

   ----------------------------
   -- Construction Functions --
   ----------------------------

   --  For better efficiency of both proofs and execution, avoid using
   --  construction functions in annotations and rather use property functions.

   function Set
     (Container : Sequence;
      Position  : Index_Type;
      New_Item  : Element_Type) return Sequence
   --  Returns a new sequence which contains the same elements as Container
   --  except for the one at position Position which is replaced by New_Item.

   with
     Global => null,
     Pre    => Position in Index_Type'First .. Last (Container),
     Post   =>
       Get (Set'Result, Position) = New_Item
         and then Equal_Except (Container, Set'Result, Position);

   function Add (Container : Sequence; New_Item : Element_Type) return Sequence
   --  Returns a new sequence which contains the same elements as Container
   --  plus New_Item at the end.

   with
     Global => null,
     Pre    =>
       Length (Container) < Count_Type'Last
         and then Last (Container) < Index_Type'Last,
     Post   =>
       Length (Add'Result) = Length (Container) + 1
         and then Get (Add'Result, Last (Add'Result)) = New_Item
         and then Container <= Add'Result;

   function Add
     (Container : Sequence;
      Position  : Index_Type;
      New_Item  : Element_Type) return Sequence
   with
   --  Returns a new sequence which contains the same elements as Container
   --  except that New_Item has been inserted at position Position.

     Global => null,
     Pre    =>
       Length (Container) < Count_Type'Last
         and then Last (Container) < Index_Type'Last
         and then Position <= Extended_Index'Succ (Last (Container)),
     Post   =>
       Length (Add'Result) = Length (Container) + 1
         and then Get (Add'Result, Position) = New_Item
         and then Range_Equal
                    (Left  => Container,
                     Right =>  Add'Result,
                     Fst   => Index_Type'First,
                     Lst   => Index_Type'Pred (Position))
         and then Range_Shifted
                    (Left   => Container,
                     Right  => Add'Result,
                     Fst    => Position,
                     Lst    => Last (Container),
                     Offset => 1);

   function Remove
     (Container : Sequence;
      Position : Index_Type) return Sequence
   --  Returns a new sequence which contains the same elements as Container
   --  except that the element at position Position has been removed.

   with
     Global => null,
     Pre    => Position in Index_Type'First .. Last (Container),
     Post   =>
       Length (Remove'Result) = Length (Container) - 1
         and then Range_Equal
                    (Left  => Container,
                     Right => Remove'Result,
                     Fst   => Index_Type'First,
                     Lst   => Index_Type'Pred (Position))
         and then Range_Shifted
                    (Left   => Remove'Result,
                     Right  => Container,
                     Fst    => Position,
                     Lst    => Last (Remove'Result),
                     Offset => 1);

   function Copy_Element (Item : Element_Type) return Element_Type is (Item);
   --  Elements of containers are copied by numerous primitives in this
   --  package. This function causes GNATprove to verify that such a copy is
   --  valid (in particular, it does not break the ownership policy of SPARK,
   --  i.e. it does not contain pointers that could be used to alias mutable
   --  data).

   ---------------------------
   --  Iteration Primitives --
   ---------------------------

   function Iter_First (Container : Sequence) return Extended_Index with
     Global => null;

   function Iter_Has_Element
     (Container : Sequence;
      Position  : Extended_Index) return Boolean
   with
     Global => null,
     Post   =>
       Iter_Has_Element'Result =
         (Position in Index_Type'First .. Last (Container));
   pragma Annotate (GNATprove, Inline_For_Proof, Iter_Has_Element);

   function Iter_Next
     (Container : Sequence;
      Position  : Extended_Index) return Extended_Index
   with
     Global => null,
     Pre    => Iter_Has_Element (Container, Position);

private

   pragma SPARK_Mode (Off);

   package Containers is new Ada.Containers.Functional_Base
     (Index_Type   => Index_Type,
      Element_Type => Element_Type);

   type Sequence is record
      Content : Containers.Container;
   end record;

   function Iter_First (Container : Sequence) return Extended_Index is
     (Index_Type'First);

   function Iter_Next
     (Container : Sequence;
      Position  : Extended_Index) return Extended_Index
   is
     (if Position = Extended_Index'Last then
         Extended_Index'First
      else
         Extended_Index'Succ (Position));

   function Iter_Has_Element
     (Container : Sequence;
      Position  : Extended_Index) return Boolean
   is
     (Position in Index_Type'First ..
       (Index_Type'Val
         ((Index_Type'Pos (Index_Type'First) - 1) + Length (Container))));

end Ada.Containers.Functional_Vectors;
