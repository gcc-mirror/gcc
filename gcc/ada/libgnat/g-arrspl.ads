------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     G N A T . A R R A Y _ S P L I T                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2018, Free Software Foundation, Inc.         --
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
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Useful array-manipulation routines: given a set of separators, split
--  an array wherever the separators appear, and provide direct access
--  to the resulting slices.

with Ada.Finalization;

generic
   type Element is (<>);
   --  Element of the array, this must be a discrete type

   type Element_Sequence is array (Positive range <>) of Element;
   --  The array which is a sequence of element

   type Element_Set is private;
   --  This type represent a set of elements. This set does not define a
   --  specific order of the elements. The conversion of a sequence to a
   --  set and membership tests in the set is performed using the routines
   --  To_Set and Is_In defined below.

   with function To_Set (Sequence : Element_Sequence) return Element_Set;
   --  Returns an Element_Set given an Element_Sequence. Duplicate elements
   --  can be ignored during this conversion.

   with function Is_In (Item : Element; Set : Element_Set) return Boolean;
   --  Returns True if Item is found in Set, False otherwise

package GNAT.Array_Split is

   Index_Error : exception;
   --  Raised by all operations below if Index > Field_Count (S)

   type Separator_Mode is
     (Single,
      --  In this mode the array is cut at each element in the separator
      --  set. If two separators are contiguous the result at that position
      --  is an empty slice.

      Multiple
      --  In this mode contiguous separators are handled as a single
      --  separator and no empty slice is created.
     );

   type Slice_Set is private;
   --  This type uses by-reference semantics. This is a set of slices as
   --  returned by Create or Set routines below. The abstraction represents
   --  a set of items. Each item is a part of the original array named a
   --  Slice. It is possible to access individual slices by using the Slice
   --  routine below. The first slice in the Set is at the position/index
   --  1. The total number of slices in the set is returned by Slice_Count.

   procedure Create
     (S          : out Slice_Set;
      From       : Element_Sequence;
      Separators : Element_Sequence;
      Mode       : Separator_Mode := Single);
   --  Create a cut array object. From is the source array, and Separators
   --  is a sequence of Element along which to split the array. The source
   --  array is sliced at separator boundaries. The separators are not
   --  included as part of the resulting slices.
   --
   --  Note that if From is terminated by a separator an extra empty element
   --  is added to the slice set. If From only contains a separator the slice
   --  set contains two empty elements.

   procedure Create
     (S          : out Slice_Set;
      From       : Element_Sequence;
      Separators : Element_Set;
      Mode       : Separator_Mode := Single);
   --  Same as above but using a Element_Set

   procedure Set
     (S          : in out Slice_Set;
      Separators : Element_Sequence;
      Mode       : Separator_Mode := Single);
   --  Change the set of separators. The source array will be split according
   --  to this new set of separators.

   procedure Set
     (S          : in out Slice_Set;
      Separators : Element_Set;
      Mode       : Separator_Mode := Single);
   --  Same as above but using a Element_Set

   type Slice_Number is new Natural;
   --  Type used to count number of slices

   function Slice_Count (S : Slice_Set) return Slice_Number;
   pragma Inline (Slice_Count);
   --  Returns the number of slices (fields) in S

   function Slice
     (S     : Slice_Set;
      Index : Slice_Number) return Element_Sequence;
   pragma Inline (Slice);
   --  Returns the slice at position Index. First slice is 1. If Index is 0
   --  the whole array is returned including the separators (this is the
   --  original source array).

   type Position is (Before, After);
   --  Used to designate position of separator

   type Slice_Separators is array (Position) of Element;
   --  Separators found before and after the slice

   Array_End : constant Element;
   --  This is the separator returned for the start or the end of the array

   function Separators
     (S     : Slice_Set;
      Index : Slice_Number) return Slice_Separators;
   --  Returns the separators used to slice (front and back) the slice at
   --  position Index. For slices at start and end of the original array, the
   --  Array_End value is returned for the corresponding outer bound. In
   --  Multiple mode only the element closest to the slice is returned.
   --  if Index = 0, returns (Array_End, Array_End).

   type Separators_Indexes is array (Positive range <>) of Positive;

   function Separators (S : Slice_Set) return Separators_Indexes;
   --  Returns indexes of all separators used to slice original source array S

private

   Array_End : constant Element := Element'First;

   type Element_Access is access Element_Sequence;

   type Indexes_Access is access Separators_Indexes;

   type Slice_Info is record
      Start : Positive;
      Stop  : Natural;
   end record;
   --  Starting/Ending position of a slice. This does not include separators

   type Slices_Indexes is array (Slice_Number range <>) of Slice_Info;
   type Slices_Access is access Slices_Indexes;
   --  All indexes for fast access to slices. In the Slice_Set we keep only
   --  the original array and the indexes where each slice start and stop.

   type Data is record
      Ref_Counter : Natural;            -- Reference counter, by-address sem
      Source      : Element_Access;
      N_Slice     : Slice_Number := 0;  -- Number of slices found
      Indexes     : Indexes_Access;
      Slices      : Slices_Access;
   end record;
   type Data_Access is access all Data;

   type Slice_Set is new Ada.Finalization.Controlled with record
      D : Data_Access;
   end record;

   procedure Initialize (S : in out Slice_Set);
   procedure Adjust     (S : in out Slice_Set);
   procedure Finalize   (S : in out Slice_Set);

end GNAT.Array_Split;
