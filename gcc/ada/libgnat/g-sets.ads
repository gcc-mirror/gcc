------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            G N A T . S E T S                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

pragma Compiler_Unit_Warning;

with GNAT.Dynamic_HTables; use GNAT.Dynamic_HTables;

package GNAT.Sets is

   --------------------
   -- Membership_Set --
   --------------------

   --  The following package offers a membership set abstraction with the
   --  following characteristics:
   --
   --    * Creation of multiple instances, of different sizes.
   --    * Iterable elements.
   --
   --  The following use pattern must be employed with this set:
   --
   --    Set : Instance := Create (<some size>);
   --
   --    <various operations>
   --
   --    Destroy (Set);
   --
   --  The destruction of the set reclaims all storage occupied by it.

   generic
      type Element_Type is private;

      with function "="
             (Left  : Element_Type;
              Right : Element_Type) return Boolean;

      with function Hash (Key : Element_Type) return Bucket_Range_Type;
      --  Map an arbitrary key into the range of buckets

   package Membership_Set is

      --------------------
      -- Set operations --
      --------------------

      --  The following type denotes a membership set handle. Each instance
      --  must be created using routine Create.

      type Instance is private;
      Nil : constant Instance;

      function Contains (S : Instance; Elem : Element_Type) return Boolean;
      --  Determine whether membership set S contains element Elem

      function Create (Initial_Size : Positive) return Instance;
      --  Create a new membership set with bucket capacity Initial_Size. This
      --  routine must be called at the start of the membership set's lifetime.

      procedure Delete (S : Instance; Elem : Element_Type);
      --  Delete element Elem from membership set S. The routine has no effect
      --  if the element is not present in the membership set. This action will
      --  raise Iterated if the membership set has outstanding iterators.

      procedure Destroy (S : in out Instance);
      --  Destroy the contents of membership set S, rendering it unusable. This
      --  routine must be called at the end of the membership set's lifetime.
      --  This action will raise Iterated if the hash table has outstanding
      --  iterators.

      procedure Insert (S : Instance; Elem : Element_Type);
      --  Insert element Elem in membership set S. The routine has no effect
      --  if the element is already present in the membership set. This action
      --  will raise Iterated if the membership set has outstanding iterators.

      function Is_Empty (S : Instance) return Boolean;
      --  Determine whether set S is empty

      function Size (S : Instance) return Natural;
      --  Obtain the number of elements in membership set S

      -------------------------
      -- Iterator operations --
      -------------------------

      --  The following type represents an element iterator. An iterator locks
      --  all mutation operations, and unlocks them once it is exhausted. The
      --  iterator must be used with the following pattern:
      --
      --    Iter := Iterate (My_Set);
      --    while Has_Next (Iter) loop
      --       Next (Iter, Element);
      --    end loop;
      --
      --  It is possible to advance the iterator by using Next only, however
      --  this risks raising Iterator_Exhausted.

      type Iterator is private;

      function Iterate (S : Instance) return Iterator;
      --  Obtain an iterator over the elements of membership set S. This action
      --  locks all mutation functionality of the associated membership set.

      function Has_Next (Iter : Iterator) return Boolean;
      --  Determine whether iterator Iter has more keys to examine. If the
      --  iterator has been exhausted, restore all mutation functionality of
      --  the associated membership set.

      procedure Next (Iter : in out Iterator; Elem : out Element_Type);
      --  Return the current element referenced by iterator Iter and advance
      --  to the next available element. If the iterator has been exhausted
      --  and further attempts are made to advance it, this routine restores
      --  mutation functionality of the associated membership set, and then
      --  raises Iterator_Exhausted.

   private
      package Hashed_Set is new Dynamic_HTable
        (Key_Type              => Element_Type,
         Value_Type            => Boolean,
         No_Value              => False,
         Expansion_Threshold   => 1.5,
         Expansion_Factor      => 2,
         Compression_Threshold => 0.3,
         Compression_Factor    => 2,
         "="                   => "=",
         Hash                  => Hash);

      type Instance is new Hashed_Set.Instance;
      Nil : constant Instance := Instance (Hashed_Set.Nil);

      type Iterator is new Hashed_Set.Iterator;
   end Membership_Set;

end GNAT.Sets;
