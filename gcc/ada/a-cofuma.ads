------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_MAPS                      --
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
   type Key_Type (<>) is private;
   type Element_Type (<>)  is private;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Ada.Containers.Functional_Maps with SPARK_Mode is

   pragma Assertion_Policy (Post => Ignore);

   type Map is private with
     Default_Initial_Condition => Is_Empty (Map) and Length (Map) = 0,
     Iterable                  => (First       => Iter_First,
                                   Next        => Iter_Next,
                                   Has_Element => Iter_Has_Element,
                                   Element     => Iter_Element);
   --  Maps are empty when default initialized.
   --  "For in" quantification over maps should not be used.
   --  "For of" quantification over maps iterates over keys.

   --  Maps are axiomatized using Mem and Get, encoding respectively the
   --  presence of a key in a map and an accessor to elements associated to its
   --  keys. The length of a map is also added to protect Add against overflows
   --  but it is not actually modeled.

   function Mem (M : Map; K : Key_Type) return Boolean with
     Global => null;
   function Get (M : Map; K : Key_Type) return Element_Type with
     Global => null,
     Pre    => Mem (M, K);

   function Length (M : Map) return Count_Type with
     Global => null;

   function "<=" (M1, M2 : Map) return Boolean with
   --  Map inclusion

     Global => null,
     Post   => "<="'Result =
       (for all K of M1 => Mem (M2, K)
        and then Get (M2, K) = Get (M1, K));

   function "=" (M1, M2 : Map) return Boolean with
   --  Extensional equality over maps

     Global => null,
     Post   => "="'Result =
       ((for all K of M1 => Mem (M2, K) and then Get (M2, K) = Get (M1, K))
          and (for all K of M2 => Mem (M1, K)));

   pragma Warnings (Off, "unused variable ""K""");
   function Is_Empty (M : Map) return Boolean with
   --  A map is empty if it contains no key
     Global => null,
     Post   => Is_Empty'Result = (for all K of M => False);
   pragma Warnings (On, "unused variable ""K""");

   function Is_Add
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   --  Returns True if Result is M augmented with the mapping K -> E

   with
     Global => null,
     Post   => Is_Add'Result =
         (not Mem (M, K)
          and then (Mem (Result, K) and then Get (Result, K) = E
            and then (for all K of M => Mem (Result, K)
                 and then Get (Result, K) = Get (M, K))
            and then (for all KK of Result =>
                        Equivalent_Keys (KK, K) or Mem (M, KK))));

   function Add (M : Map; K : Key_Type; E : Element_Type) return Map with
   --  Returns M augmented with the mapping K -> E.
   --  Is_Add (M, K, E, Result) should be used instead of
   --  Result = Add (M, K, E) whenever possible both for execution and for
   --  proof.

     Global => null,
     Pre    => not Mem (M, K) and Length (M) < Count_Type'Last,
     Post   => Length (M) + 1 = Length (Add'Result)
               and Is_Add (M, K, E, Add'Result);

   function Is_Set
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   --  Returns True if Result is M, where the element associated to K has been
   --  replaced by E.

   with
     Global => null,
     Post   => Is_Set'Result =
         (Mem (M, K)
          and then Mem (Result, K)
          and then Get (Result, K) = E
          and then (for all KK of M => Mem (Result, KK)
               and then (if not Equivalent_Keys (K, KK)
                         then Get (Result, KK) = Get (M, KK)))
          and then (for all K of Result => Mem (M, K)));

   function Set (M : Map; K : Key_Type; E : Element_Type) return Map with
   --  Returns M, where the element associated to K has been replaced by E.
   --  Is_Set (M, K, E, Result) should be used instead of
   --  Result = Set (M, K, E) whenever possible both for execution and for
   --  proof.

     Global => null,
     Pre    => Mem (M, K),
     Post   => Length (M) = Length (Set'Result)
     and Is_Set (M, K, E, Set'Result);

   ---------------------------
   --  Iteration Primitives --
   ---------------------------

   type Private_Key is private;

   function Iter_First (M : Map) return Private_Key with
     Global => null;
   function Iter_Has_Element (M : Map; K : Private_Key) return Boolean with
     Global => null;
   function Iter_Next (M : Map; K : Private_Key) return Private_Key with
     Global => null,
     Pre    => Iter_Has_Element (M, K);
   function Iter_Element (M : Map; K : Private_Key) return Key_Type with
     Global => null,
     Pre    => Iter_Has_Element (M, K);
   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", Mem);

private

   pragma SPARK_Mode (Off);

   function "="  (Left, Right : Key_Type) return Boolean
                  renames Equivalent_Keys;

   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

   package Element_Containers is new Ada.Containers.Functional_Base
     (Element_Type        => Element_Type,
      Index_Type          => Positive_Count_Type);

   package Key_Containers is new Ada.Containers.Functional_Base
     (Element_Type        => Key_Type,
      Index_Type          => Positive_Count_Type);

   type Map is record
      Keys     : Key_Containers.Container;
      Elements : Element_Containers.Container;
   end record;

   type Private_Key is new Count_Type;

   function Iter_First (M : Map) return Private_Key is (1);

   function Iter_Has_Element (M : Map; K : Private_Key) return Boolean is
     (Count_Type (K) in 1 .. Key_Containers.Length (M.Keys));

   function Iter_Next (M : Map; K : Private_Key) return Private_Key is
     (if K = Private_Key'Last then 0 else K + 1);

   function Iter_Element (M : Map; K : Private_Key) return Key_Type is
     (Key_Containers.Get (M.Keys, Count_Type (K)));

end Ada.Containers.Functional_Maps;
