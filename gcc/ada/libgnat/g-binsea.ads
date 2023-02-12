------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           GNAT.BINARY_SEARCH                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

--  Allow binary search of a sorted array (or of an array-like container;
--  the generic does not reference the array directly).

package GNAT.Binary_Search is

   generic
      type Index_Type is (<>);
      type Element_Type (<>) is private;
      with function Get (Index : Index_Type) return Element_Type;
      with function Before (Left, Right : Element_Type) return Boolean;
      Leftmost : Boolean := True;
   function Index
     (First, Last, Start : Index_Type;
      Element            : Element_Type) return Index_Type'Base;
   --  Search for element in sorted container. Function Before should return
   --  True when Left and Right are in the container's sort order and not
   --  equal. Function Get returns the container element indexed by Index;
   --  Index will be in the range First .. Last. If there is at least one index
   --  value in the range First .. Last for which Get would return Element,
   --  then the Leftmost generic parameter indicates whether the least (if
   --  Leftmost is True) or the greatest (if Leftmost is False) such index
   --  value is returned. If no such index value exists, then Leftmost
   --  determines whether to return the greater (if Leftmost is True) or the
   --  smaller (if Leftmost is False) of the two index values between which
   --  Element could be inserted. If First > Last (so that a null range is
   --  being searched), some Index_Type'Base value will be returned.
   --  Start is the index for the first probe of the binary search. It can
   --  improve speed of many search operations when user can guess the most
   --  likely values. If you do not know what value should be used there, use
   --  (First + Last) / 2.

   generic
      type Index_Type is (<>);
      type Element_Type (<>) is private;
      with function Before
        (Index : Index_Type; Element : Element_Type) return Boolean;
   function Leftmost
     (First, Last, Start : Index_Type;
      Element            : Element_Type) return Index_Type'Base
     with Pre => First > Last -- Empty array
                 or else (Start in First .. Last
                          and then ( -- To prevent overflow in function result
                                    Index_Type'Base'Last > Last
                                    or else not Before (Last, Element)));
   --  Leftmost returns the result described for Index in the case where the
   --  Leftmost parameter is True, with Index_Type values mapped to
   --  Element_Type values via Get as needed.

   generic
      type Index_Type is (<>);
      type Element_Type (<>) is private;
      with function Before
        (Element : Element_Type; Index : Index_Type) return Boolean;
   function Rightmost
     (First, Last, Start : Index_Type;
      Element            : Element_Type) return Index_Type'Base
     with Pre => First > Last -- Empty array
                 or else (Start in First .. Last
                          and then ( -- To prevent overflow in function result
                                    Index_Type'Base'First < First
                                    or else not Before (Element, First)));
   --  Rightmost returns the result described for Index in the case where the
   --  Leftmost parameter is False, with Index_Type values mapped to
   --  Element_Type values via Get as needed.

end GNAT.Binary_Search;
