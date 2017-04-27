------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_SETS                      --
--                                                                          --
--                                 B o d y                                  --
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

package body Ada.Containers.Functional_Sets with SPARK_Mode => Off is
   use Containers;

   ---------
   -- "=" --
   ---------

   function "=" (Left : Set; Right : Set) return Boolean is
     (Left.Content <= Right.Content and Right.Content <= Left.Content);

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : Set; Right : Set) return Boolean is
     (Left.Content <= Right.Content);

   ---------
   -- Add --
   ---------

   function Add (Container : Set; Item : Element_Type) return Set is
     (Content =>
       Add (Container.Content, Length (Container.Content) + 1, Item));

   --------------
   -- Contains --
   --------------

   function Contains (Container : Set; Item : Element_Type) return Boolean is
     (Find (Container.Content, Item) > 0);

   ---------------------
   -- Included_Except --
   ---------------------

   function Included_Except
     (Left  : Set;
      Right : Set;
      Item  : Element_Type) return Boolean
   is
     (for all E of Left =>
       Equivalent_Elements (E, Item) or Contains (Right, E));

   -----------------------
   -- Included_In_Union --
   -----------------------

   function Included_In_Union
     (Container : Set;
      Left      : Set;
      Right     : Set) return Boolean
   is
     (for all Item of Container =>
       Contains (Left, Item) or Contains (Right, Item));

   ---------------------------
   -- Includes_Intersection --
   ---------------------------

   function Includes_Intersection
     (Container : Set;
      Left      : Set;
      Right     : Set) return Boolean
   is
     (for all Item of Left =>
       (if Contains (Right, Item) then Contains (Container, Item)));

   ------------------
   -- Intersection --
   ------------------

   function Intersection (Left : Set; Right : Set) return Set is
     (Content => Intersection (Left.Content, Right.Content));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set) return Boolean is
     (Length (Container.Content) = 0);

   ------------------
   -- Is_Singleton --
   ------------------

   function Is_Singleton
     (Container : Set;
      New_Item  : Element_Type) return Boolean
   is
     (Length (Container.Content) = 1
        and New_Item = Get (Container.Content, 1));

   ------------
   -- Length --
   ------------

   function Length (Container : Set) return Count_Type is
     (Length (Container.Content));

   -----------------
   -- Not_In_Both --
   -----------------

   function Not_In_Both
     (Container : Set;
      Left      : Set;
      Right     : Set) return Boolean
   is
     (for all Item of Container =>
       not Contains (Right, Item) or not Contains (Left, Item));

   ----------------
   -- No_Overlap --
   ----------------

   function No_Overlap (Left : Set; Right : Set) return Boolean is
     (Num_Overlaps (Left.Content, Right.Content) = 0);

   ------------------
   -- Num_Overlaps --
   ------------------

   function Num_Overlaps (Left : Set; Right : Set) return Count_Type is
     (Num_Overlaps (Left.Content, Right.Content));

   ------------
   -- Remove --
   ------------

   function Remove (Container : Set; Item : Element_Type) return Set is
     (Content => Remove (Container.Content, Find (Container.Content, Item)));

   -----------
   -- Union --
   -----------

   function Union (Left : Set; Right : Set) return Set is
     (Content => Union (Left.Content, Right.Content));

end Ada.Containers.Functional_Sets;
