------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_MAPS                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2016-2020, Free Software Foundation, Inc.         --
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

   with function Equivalent_Keys
     (Left  : Key_Type;
      Right : Key_Type) return Boolean is "=";
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   Enable_Handling_Of_Equivalence : Boolean := True;
   --  This constant should only be set to False when no particular handling
   --  of equivalence over keys is needed, that is, Equivalent_Keys defines a
   --  key uniquely.

package Ada.Containers.Functional_Maps with SPARK_Mode is

   type Map is private with
     Default_Initial_Condition => Is_Empty (Map) and Length (Map) = 0,
     Iterable                  => (First       => Iter_First,
                                   Next        => Iter_Next,
                                   Has_Element => Iter_Has_Element,
                                   Element     => Iter_Element);
   --  Maps are empty when default initialized.
   --  "For in" quantification over maps should not be used.
   --  "For of" quantification over maps iterates over keys.
   --  Note that, for proof, "for of" quantification is understood modulo
   --  equivalence (the range of quantification comprises all the keys that are
   --  equivalent to any key of the map).

   -----------------------
   --  Basic operations --
   -----------------------

   --  Maps are axiomatized using Has_Key and Get, encoding respectively the
   --  presence of a key in a map and an accessor to elements associated with
   --  its keys. The length of a map is also added to protect Add against
   --  overflows but it is not actually modeled.

   function Has_Key (Container : Map; Key : Key_Type) return Boolean with
   --  Return True if Key is present in Container

     Global => null,
     Post   =>
       (if Enable_Handling_Of_Equivalence then

          --  Has_Key returns the same result on all equivalent keys

          (if (for some K of Container => Equivalent_Keys (K, Key)) then
              Has_Key'Result));

   function Get (Container : Map; Key : Key_Type) return Element_Type with
   --  Return the element associated with Key in Container

     Global => null,
     Pre    => Has_Key (Container, Key),
     Post   =>
       (if Enable_Handling_Of_Equivalence then

          --  Get returns the same result on all equivalent keys

          Get'Result = W_Get (Container, Witness (Container, Key))
            and (for all K of Container =>
                  (Equivalent_Keys (K, Key) =
                    (Witness (Container, Key) = Witness (Container, K)))));

   function Length (Container : Map) return Count_Type with
     Global => null;
   --  Return the number of mappings in Container

   ------------------------
   -- Property Functions --
   ------------------------

   function "<=" (Left : Map; Right : Map) return Boolean with
   --  Map inclusion

     Global => null,
     Post   =>
       "<="'Result =
         (for all Key of Left =>
           Has_Key (Right, Key) and then Get (Right, Key) = Get (Left, Key));

   function "=" (Left : Map; Right : Map) return Boolean with
   --  Extensional equality over maps

     Global => null,
     Post   =>
       "="'Result =
         ((for all Key of Left =>
            Has_Key (Right, Key)
              and then Get (Right, Key) = Get (Left, Key))
              and (for all Key of Right => Has_Key (Left, Key)));

   pragma Warnings (Off, "unused variable ""Key""");
   function Is_Empty (Container : Map) return Boolean with
   --  A map is empty if it contains no key

     Global => null,
     Post   => Is_Empty'Result = (for all Key of Container => False);
   pragma Warnings (On, "unused variable ""Key""");

   function Keys_Included (Left : Map; Right : Map) return Boolean
   --  Returns True if every Key of Left is in Right

   with
     Global => null,
     Post   =>
       Keys_Included'Result = (for all Key of Left => Has_Key (Right, Key));

   function Same_Keys (Left : Map; Right : Map) return Boolean
   --  Returns True if Left and Right have the same keys

   with
     Global => null,
     Post   =>
       Same_Keys'Result =
         (Keys_Included (Left, Right)
           and Keys_Included (Left => Right, Right => Left));
   pragma Annotate (GNATprove, Inline_For_Proof, Same_Keys);

   function Keys_Included_Except
     (Left    : Map;
      Right   : Map;
      New_Key : Key_Type) return Boolean
   --  Returns True if Left contains only keys of Right and possibly New_Key

   with
     Global => null,
     Post   =>
       Keys_Included_Except'Result =
         (for all Key of Left =>
           (if not Equivalent_Keys (Key, New_Key) then
               Has_Key (Right, Key)));

   function Keys_Included_Except
     (Left  : Map;
      Right : Map;
      X     : Key_Type;
      Y     : Key_Type) return Boolean
   --  Returns True if Left contains only keys of Right and possibly X and Y

   with
     Global => null,
     Post   =>
       Keys_Included_Except'Result =
         (for all Key of Left =>
           (if not Equivalent_Keys (Key, X)
              and not Equivalent_Keys (Key, Y)
            then
               Has_Key (Right, Key)));

   function Elements_Equal_Except
     (Left    : Map;
      Right   : Map;
      New_Key : Key_Type) return Boolean
   --  Returns True if all the keys of Left are mapped to the same elements in
   --  Left and Right except New_Key.

   with
     Global => null,
     Post   =>
       Elements_Equal_Except'Result =
         (for all Key of Left =>
           (if not Equivalent_Keys (Key, New_Key) then
               Has_Key (Right, Key)
                 and then Get (Left, Key) = Get (Right, Key)));

   function Elements_Equal_Except
     (Left  : Map;
      Right : Map;
      X     : Key_Type;
      Y     : Key_Type) return Boolean
   --  Returns True if all the keys of Left are mapped to the same elements in
   --  Left and Right except X and Y.

   with
     Global => null,
     Post   =>
       Elements_Equal_Except'Result =
         (for all Key of Left =>
           (if not Equivalent_Keys (Key, X)
              and not Equivalent_Keys (Key, Y)
            then
               Has_Key (Right, Key)
                 and then Get (Left, Key) = Get (Right, Key)));

   ----------------------------
   -- Construction Functions --
   ----------------------------

   --  For better efficiency of both proofs and execution, avoid using
   --  construction functions in annotations and rather use property functions.

   function Add
     (Container : Map;
      New_Key   : Key_Type;
      New_Item  : Element_Type) return Map
   --  Returns Container augmented with the mapping Key -> New_Item

   with
     Global => null,
     Pre    =>
       not Has_Key (Container, New_Key)
         and Length (Container) < Count_Type'Last,
     Post   =>
       Length (Container) + 1 = Length (Add'Result)
         and Has_Key (Add'Result, New_Key)
         and Get (Add'Result, New_Key) = New_Item
         and Container <= Add'Result
         and Keys_Included_Except (Add'Result, Container, New_Key);

   function Remove
     (Container : Map;
      Key       : Key_Type) return Map
   --  Returns Container without any mapping for Key

   with
     Global => null,
     Pre    => Has_Key (Container, Key),
     Post   =>
       Length (Container) = Length (Remove'Result) + 1
         and not Has_Key (Remove'Result, Key)
         and Remove'Result <= Container
         and Keys_Included_Except (Container, Remove'Result, Key);

   function Set
     (Container : Map;
      Key       : Key_Type;
      New_Item  : Element_Type) return Map
   --  Returns Container, where the element associated with Key has been
   --  replaced by New_Item.

   with
     Global => null,
     Pre    => Has_Key (Container, Key),
     Post   =>
       Length (Container) = Length (Set'Result)
         and Get (Set'Result, Key) = New_Item
         and Same_Keys (Container, Set'Result)
         and Elements_Equal_Except (Container, Set'Result, Key);

   ------------------------------
   --  Handling of Equivalence --
   ------------------------------

   --  These functions are used to specify that Get returns the same value on
   --  equivalent keys. They should not be used directly in user code.

   function Has_Witness (Container : Map; Witness : Count_Type) return Boolean
   with
     Ghost,
     Global => null;
   --  Returns True if there is a key with witness Witness in Container

   function Witness (Container : Map; Key : Key_Type) return Count_Type with
   --  Returns the witness of Key in Container

     Ghost,
     Global => null,
     Pre    => Has_Key (Container, Key),
     Post   => Has_Witness (Container, Witness'Result);

   function W_Get (Container : Map; Witness : Count_Type) return Element_Type
   with
   --  Returns the element associated with a witness in Container

     Ghost,
     Global => null,
     Pre    => Has_Witness (Container, Witness);

   ---------------------------
   --  Iteration Primitives --
   ---------------------------

   type Private_Key is private;

   function Iter_First (Container : Map) return Private_Key with
     Global => null;

   function Iter_Has_Element
     (Container : Map;
      Key       : Private_Key) return Boolean
   with
     Global => null;

   function Iter_Next (Container : Map; Key : Private_Key) return Private_Key
   with
     Global => null,
     Pre    => Iter_Has_Element (Container, Key);

   function Iter_Element (Container : Map; Key : Private_Key) return Key_Type
   with
     Global => null,
     Pre    => Iter_Has_Element (Container, Key);
   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", Has_Key);

private

   pragma SPARK_Mode (Off);

   function "="
     (Left  : Key_Type;
      Right : Key_Type) return Boolean renames Equivalent_Keys;

   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

   package Element_Containers is new Ada.Containers.Functional_Base
     (Element_Type => Element_Type,
      Index_Type   => Positive_Count_Type);

   package Key_Containers is new Ada.Containers.Functional_Base
     (Element_Type => Key_Type,
      Index_Type   => Positive_Count_Type);

   type Map is record
      Keys     : Key_Containers.Container;
      Elements : Element_Containers.Container;
   end record;

   type Private_Key is new Count_Type;

   function Iter_First (Container : Map) return Private_Key is (1);

   function Iter_Has_Element
     (Container : Map;
      Key       : Private_Key) return Boolean
   is
     (Count_Type (Key) in 1 .. Key_Containers.Length (Container.Keys));

   function Iter_Next
     (Container : Map;
      Key       : Private_Key) return Private_Key
   is
     (if Key = Private_Key'Last then 0 else Key + 1);

   function Iter_Element
     (Container : Map;
      Key       : Private_Key) return Key_Type
   is
     (Key_Containers.Get (Container.Keys, Count_Type (Key)));

end Ada.Containers.Functional_Maps;
