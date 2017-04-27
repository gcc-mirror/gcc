------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_SETS                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2016-2017, Free Software Foundation, Inc.         --
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
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Functional_Sets with SPARK_Mode is

   pragma Assertion_Policy (Post => Ignore);

   type Set is private with
     Default_Initial_Condition => Is_Empty (Set) and Length (Set) = 0,
     Iterable                  => (First       => Iter_First,
                                   Next        => Iter_Next,
                                   Has_Element => Iter_Has_Element,
                                   Element     => Iter_Element);
   --  Sets are empty when default initialized.
   --  "For in" quantification over sets should not be used.
   --  "For of" quantification over sets iterates over elements.

   --  Sets are axiomatized using Mem, which encodes whether an element is
   --  contained in a set. The length of a set is also added to protect Add
   --  against overflows but it is not actually modeled.

   function Mem (S : Set; E : Element_Type) return Boolean with
     Global => null;

   function Length (S : Set) return Count_Type with
     Global => null;

   function "<=" (S1, S2 : Set) return Boolean with
   --  Set inclusion

     Global => null,
     Post   => "<="'Result = (for all E of S1 => Mem (S2, E));

   function "=" (S1, S2 : Set) return Boolean with
   --  Extensional equality over sets

     Global => null,
     Post   =>
       "="'Result = ((for all E of S1 => Mem (S2, E))
                     and (for all E of S2 => Mem (S1, E)));

   pragma Warnings (Off, "unused variable ""E""");
   function Is_Empty (S : Set) return Boolean with
   --  A set is empty if it contains no element

     Global => null,
     Post   => Is_Empty'Result = (for all E of S => False);
   pragma Warnings (On, "unused variable ""E""");

   function Is_Add (S : Set; E : Element_Type; Result : Set) return Boolean
   --  Returns True if Result is S augmented with E

   with
     Global => null,
     Post   => Is_Add'Result =
       (Mem (Result, E) and not Mem (S, E)
        and (for all F of Result => Mem (S, F) or F = E)
        and (for all E of S => Mem (Result, E)));

   function Add (S : Set; E : Element_Type) return Set with
   --  Returns S augmented with E.
   --  Is_Add (S, E, Result) should be used instead of Result = Add (S, E)
   --  whenever possible both for execution and for proof.

     Global => null,
     Pre    => not Mem (S, E) and Length (S) < Count_Type'Last,
     Post   => Length (Add'Result) = Length (S) + 1
     and Is_Add (S, E, Add'Result);

   function Remove (S : Set; E : Element_Type) return Set with
   --  Returns S without E.
   --  Is_Add (Result, E, S) should be used instead of Result = Remove (S, E)
   --  whenever possible both for execution and for proof.

     Global => null,
     Pre    => Mem (S, E),
     Post   => Length (Remove'Result) = Length (S) - 1
     and Is_Add (Remove'Result, E, S);

   function Is_Intersection (S1, S2, Result : Set) return Boolean with
   --  Returns True if Result is the intersection of S1 and S2

     Global => null,
     Post   => Is_Intersection'Result =
       ((for all E of Result =>
               Mem (S1, E) and Mem (S2, E))
        and (for all E of S1 =>
               (if Mem (S2, E) then Mem (Result, E))));

   function Num_Overlaps (S1, S2 : Set) return Count_Type with
   --  Number of elements that are both in S1 and S2

     Global => null,
     Post   => Num_Overlaps'Result <= Length (S1)
     and Num_Overlaps'Result <= Length (S2)
     and (if Num_Overlaps'Result = 0 then
            (for all E of S1 => not Mem (S2, E)));

   function Intersection (S1, S2 : Set) return Set with
   --  Returns the intersection of S1 and S2.
   --  Intersection (S1, S2, Result) should be used instead of
   --  Result = Intersection (S1, S2) whenever possible both for execution and
   --  for proof.

     Global => null,
     Post   => Length (Intersection'Result) = Num_Overlaps (S1, S2)
     and Is_Intersection (S1, S2, Intersection'Result);

   function Is_Union (S1, S2, Result : Set) return Boolean with
   --  Returns True if Result is the union of S1 and S2

     Global => null,
     Post   => Is_Union'Result =
       ((for all E of Result => Mem (S1, E) or Mem (S2, E))
        and (for all E of S1 => Mem (Result, E))
        and (for all E of S2 => Mem (Result, E)));

   function Union (S1, S2 : Set) return Set with
   --  Returns the union of S1 and S2.
   --  Is_Union (S1, S2, Result) should be used instead of
   --  Result = Union (S1, S2) whenever possible both for execution and for
   --  proof.

     Global => null,
     Pre    => Length (S1) - Num_Overlaps (S1, S2) <=
     Count_Type'Last - Length (S2),
     Post   => Length (Union'Result) = Length (S1) - Num_Overlaps (S1, S2)
     + Length (S2)
     and Is_Union (S1, S2, Union'Result);

   ---------------------------
   --  Iteration Primitives --
   ---------------------------

   type Private_Key is private;

   function Iter_First (S : Set) return Private_Key with
     Global => null;
   function Iter_Has_Element (S : Set; K : Private_Key) return Boolean with
     Global => null;
   function Iter_Next (S : Set; K : Private_Key) return Private_Key with
     Global => null,
     Pre    => Iter_Has_Element (S, K);
   function Iter_Element (S : Set; K : Private_Key) return Element_Type with
     Global => null,
     Pre    => Iter_Has_Element (S, K);
   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", Mem);

private

   pragma SPARK_Mode (Off);

   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

   package Containers is new Ada.Containers.Functional_Base
     (Element_Type        => Element_Type,
      Index_Type          => Positive_Count_Type);

   type Set is record
      Content : Containers.Container;
   end record;

   type Private_Key is new Count_Type;

   function Iter_First (S : Set) return Private_Key is (1);

   function Iter_Has_Element (S : Set; K : Private_Key) return Boolean is
     (Count_Type (K) in 1 .. Containers.Length (S.Content));

   function Iter_Next (S : Set; K : Private_Key) return Private_Key is
     (if K = Private_Key'Last then 0 else K + 1);

   function Iter_Element (S : Set; K : Private_Key) return Element_Type is
     (Containers.Get (S.Content, Count_Type (K)));

end Ada.Containers.Functional_Sets;
