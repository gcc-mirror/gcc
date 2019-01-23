------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_SETS                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2016-2019, Free Software Foundation, Inc.         --
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
   type Element_Type (<>) is private;

   with function Equivalent_Elements
     (Left  : Element_Type;
      Right : Element_Type) return Boolean is "=";

   Enable_Handling_Of_Equivalence : Boolean := True;
   --  This constant should only be set to False when no particular handling
   --  of equivalence over elements is needed, that is, Equivalent_Elements
   --  defines an element uniquely.

package Ada.Containers.Functional_Sets with SPARK_Mode is

   type Set is private with
     Default_Initial_Condition => Is_Empty (Set),
     Iterable                  => (First       => Iter_First,
                                   Next        => Iter_Next,
                                   Has_Element => Iter_Has_Element,
                                   Element     => Iter_Element);
   --  Sets are empty when default initialized.
   --  "For in" quantification over sets should not be used.
   --  "For of" quantification over sets iterates over elements.
   --  Note that, for proof, "for of" quantification is understood modulo
   --  equivalence (the range of quantification comprises all the elements that
   --  are equivalent to any element of the set).

   -----------------------
   --  Basic operations --
   -----------------------

   --  Sets are axiomatized using Contains, which encodes whether an element is
   --  contained in a set. The length of a set is also added to protect Add
   --  against overflows but it is not actually modeled.

   function Contains (Container : Set; Item : Element_Type) return Boolean with
   --  Return True if Item is contained in Container

     Global => null,
     Post   =>
       (if Enable_Handling_Of_Equivalence then

          --  Contains returns the same result on all equivalent elements

          (if (for some E of Container => Equivalent_Elements (E, Item)) then
              Contains'Result));

   function Length (Container : Set) return Count_Type with
     Global => null;
   --  Return the number of elements in Container

   ------------------------
   -- Property Functions --
   ------------------------

   function "<=" (Left : Set; Right : Set) return Boolean with
   --  Set inclusion

     Global => null,
     Post   => "<="'Result = (for all Item of Left => Contains (Right, Item));

   function "=" (Left : Set; Right : Set) return Boolean with
   --  Extensional equality over sets

     Global => null,
     Post   => "="'Result = (Left <= Right and Right <= Left);

   pragma Warnings (Off, "unused variable ""Item""");
   function Is_Empty (Container : Set) return Boolean with
   --  A set is empty if it contains no element

     Global => null,
     Post   =>
       Is_Empty'Result = (for all Item of Container => False)
         and Is_Empty'Result = (Length (Container) = 0);
   pragma Warnings (On, "unused variable ""Item""");

   function Included_Except
     (Left  : Set;
      Right : Set;
      Item  : Element_Type) return Boolean
   --  Return True if Left contains only elements of Right except possibly
   --  Item.

   with
     Global => null,
     Post   =>
       Included_Except'Result =
         (for all E of Left =>
           Contains (Right, E) or Equivalent_Elements (E, Item));

   function Includes_Intersection
     (Container : Set;
      Left      : Set;
      Right     : Set) return Boolean
   with
   --  Return True if every element of the intersection of Left and Right is
   --  in Container.

     Global => null,
     Post   =>
       Includes_Intersection'Result =
         (for all Item of Left =>
           (if Contains (Right, Item) then Contains (Container, Item)));

   function Included_In_Union
     (Container : Set;
      Left      : Set;
      Right     : Set) return Boolean
   with
   --  Return True if every element of Container is the union of Left and Right

     Global => null,
     Post   =>
       Included_In_Union'Result =
         (for all Item of Container =>
           Contains (Left, Item) or Contains (Right, Item));

   function Is_Singleton
     (Container : Set;
      New_Item  : Element_Type) return Boolean
   with
   --  Return True Container only contains New_Item

     Global => null,
     Post   =>
       Is_Singleton'Result =
         (for all Item of Container => Equivalent_Elements (Item, New_Item));

   function Not_In_Both
     (Container : Set;
      Left      : Set;
      Right     : Set) return Boolean
   --  Return True if there are no elements in Container that are in Left and
   --  Right.

   with
     Global => null,
     Post   =>
       Not_In_Both'Result =
         (for all Item of Container =>
            not Contains (Left, Item) or not Contains (Right, Item));

   function No_Overlap (Left : Set; Right : Set) return Boolean with
   --  Return True if there are no equivalent elements in Left and Right

     Global => null,
     Post   =>
       No_Overlap'Result =
         (for all Item of Left => not Contains (Right, Item));

   function Num_Overlaps (Left : Set; Right : Set) return Count_Type with
   --  Number of elements that are both in Left and Right

     Global => null,
     Post   =>
       Num_Overlaps'Result = Length (Intersection (Left, Right))
         and (if Left <= Right then Num_Overlaps'Result = Length (Left)
              else Num_Overlaps'Result < Length (Left))
         and (if Right <= Left then Num_Overlaps'Result = Length (Right)
              else Num_Overlaps'Result < Length (Right))
         and (Num_Overlaps'Result = 0) = No_Overlap (Left, Right);

   ----------------------------
   -- Construction Functions --
   ----------------------------

   --  For better efficiency of both proofs and execution, avoid using
   --  construction functions in annotations and rather use property functions.

   function Add (Container : Set; Item : Element_Type) return Set with
   --  Return a new set containing all the elements of Container plus E

     Global => null,
     Pre    =>
       not Contains (Container, Item)
       and Length (Container) < Count_Type'Last,
     Post   =>
       Length (Add'Result) = Length (Container) + 1
         and Contains (Add'Result, Item)
         and Container <= Add'Result
         and Included_Except (Add'Result, Container, Item);

   function Remove (Container : Set; Item : Element_Type) return Set with
   --  Return a new set containing all the elements of Container except E

     Global => null,
     Pre    => Contains (Container, Item),
     Post   =>
       Length (Remove'Result) = Length (Container) - 1
         and not Contains (Remove'Result, Item)
         and Remove'Result <= Container
         and Included_Except (Container, Remove'Result, Item);

   function Intersection (Left : Set; Right : Set) return Set with
   --  Returns the intersection of Left and Right

     Global => null,
     Post   =>
       Intersection'Result <= Left
         and Intersection'Result <= Right
         and Includes_Intersection (Intersection'Result, Left, Right);

   function Union (Left : Set; Right : Set) return Set with
   --  Returns the union of Left and Right

     Global => null,
     Pre    =>
       Length (Left) - Num_Overlaps (Left, Right) <=
         Count_Type'Last - Length (Right),
     Post   =>
       Length (Union'Result) =
         Length (Left) - Num_Overlaps (Left, Right) + Length (Right)
           and Left <= Union'Result
           and Right <= Union'Result
           and Included_In_Union (Union'Result, Left, Right);

   ---------------------------
   --  Iteration Primitives --
   ---------------------------

   type Private_Key is private;

   function Iter_First (Container : Set) return Private_Key with
     Global => null;

   function Iter_Has_Element
     (Container : Set;
      Key       : Private_Key) return Boolean
   with
     Global => null;

   function Iter_Next
     (Container : Set;
      Key       : Private_Key) return Private_Key
   with
     Global => null,
     Pre    => Iter_Has_Element (Container, Key);

   function Iter_Element
     (Container : Set;
      Key       : Private_Key) return Element_Type
   with
     Global => null,
     Pre    => Iter_Has_Element (Container, Key);
   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", Contains);

private

   pragma SPARK_Mode (Off);

   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

   function "="
     (Left  : Element_Type;
      Right : Element_Type) return Boolean renames Equivalent_Elements;

   package Containers is new Ada.Containers.Functional_Base
     (Element_Type => Element_Type,
      Index_Type   => Positive_Count_Type);

   type Set is record
      Content : Containers.Container;
   end record;

   type Private_Key is new Count_Type;

   function Iter_First (Container : Set) return Private_Key is (1);

   function Iter_Has_Element
     (Container : Set;
      Key       : Private_Key) return Boolean
   is
     (Count_Type (Key) in 1 .. Containers.Length (Container.Content));

   function Iter_Next
     (Container : Set;
      Key       : Private_Key) return Private_Key
   is
     (if Key = Private_Key'Last then 0 else Key + 1);

   function Iter_Element
     (Container : Set;
      Key       : Private_Key) return Element_Type
   is
     (Containers.Get (Container.Content, Count_Type (Key)));

end Ada.Containers.Functional_Sets;
