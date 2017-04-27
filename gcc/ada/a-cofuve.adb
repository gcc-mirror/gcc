------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                    ADA.CONTAINERS.FUNCTIONAL_VECTORS                     --
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
package body Ada.Containers.Functional_Vectors with SPARK_Mode => Off is
   use Containers;

   ---------
   -- "<" --
   ---------

   function "<" (Left : Sequence; Right : Sequence) return Boolean is
     (Length (Left.Content) < Length (Right.Content)
       and then (for all I in Index_Type'First .. Last (Left) =>
                  Get (Left.Content, I) = Get (Right.Content, I)));

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : Sequence; Right : Sequence) return Boolean is
     (Length (Left.Content) <= Length (Right.Content)
       and then (for all I in Index_Type'First .. Last (Left) =>
                  Get (Left.Content, I) = Get (Right.Content, I)));

   ---------
   -- "=" --
   ---------

   function "=" (Left : Sequence; Right : Sequence) return Boolean is
     (Left.Content = Right.Content);

   ---------
   -- Add --
   ---------

   function Add
     (Container : Sequence;
      New_Item  : Element_Type) return Sequence
   is
     (Content =>
       Add (Container.Content,
            Index_Type'Val (Index_Type'Pos (Index_Type'First) +
                            Length (Container.Content)),
            New_Item));

   function Add
     (Container : Sequence;
      Position  : Index_Type;
      New_Item  : Element_Type) return Sequence
   is
     (Content => Add (Container.Content, Position, New_Item));

   --------------------
   -- Constant_Range --
   --------------------

   function Constant_Range
     (Container : Sequence;
      Fst       : Index_Type;
      Lst       : Extended_Index;
      Item      : Element_Type) return Boolean is
   begin
      for I in Fst .. Lst loop
         if Get (Container.Content, I) /= Item then
            return False;
         end if;
      end loop;

      return True;
   end Constant_Range;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Sequence;
      Fst       : Index_Type;
      Lst       : Extended_Index;
      Item      : Element_Type) return Boolean
   is
   begin
      for I in Fst .. Lst loop
         if Get (Container.Content, I) = Item then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   ------------------
   -- Range_Except --
   ------------------

   function Equal_Except
     (Left     : Sequence;
      Right    : Sequence;
      Position : Index_Type) return Boolean
   is
   begin
      if Length (Left.Content) /= Length (Right.Content) then
         return False;
      end if;

      for I in Index_Type'First .. Last (Left) loop
         if I /= Position
           and then Get (Left.Content, I) /= Get (Right.Content, I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equal_Except;

   function Equal_Except
     (Left  : Sequence;
      Right : Sequence;
      X     : Index_Type;
      Y     : Index_Type) return Boolean
   is
   begin
      if Length (Left.Content) /= Length (Right.Content) then
         return False;
      end if;

      for I in Index_Type'First .. Last (Left) loop
         if I /= X and then I /= Y
           and then Get (Left.Content, I) /= Get (Right.Content, I)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equal_Except;

   ---------
   -- Get --
   ---------

   function Get (Container : Sequence;
                 Position  : Extended_Index) return Element_Type
   is
     (Get (Container.Content, Position));

   ----------
   -- Last --
   ----------

   function Last (Container : Sequence) return Extended_Index is
     (Index_Type'Val
       ((Index_Type'Pos (Index_Type'First) - 1) + Length (Container)));

   ------------
   -- Length --
   ------------

   function Length (Container : Sequence) return Count_Type is
     (Length (Container.Content));

   -----------------
   -- Range_Equal --
   -----------------

   function Range_Equal
     (Left  : Sequence;
      Right : Sequence;
      Fst   : Index_Type;
      Lst   : Extended_Index) return Boolean
   is
   begin
      for I in Fst .. Lst loop
         if Get (Left, I) /= Get (Right, I) then
            return False;
         end if;
      end loop;

      return True;
   end Range_Equal;

   -------------------
   -- Range_Shifted --
   -------------------

   function Range_Shifted
     (Left   : Sequence;
      Right  : Sequence;
      Fst    : Index_Type;
      Lst    : Extended_Index;
      Offset : Count_Type'Base) return Boolean
   is
   begin
      for I in Fst .. Lst loop
         if Get (Left, I) /=
              Get (Right, Index_Type'Val (Index_Type'Pos (I) + Offset))
         then
            return False;
         end if;
      end loop;
      return True;
   end Range_Shifted;

   ------------
   -- Remove --
   ------------

   function Remove
     (Container : Sequence;
      Position : Index_Type) return Sequence
   is
     (Content => Remove (Container.Content, Position));

   ---------
   -- Set --
   ---------

   function Set
     (Container : Sequence;
      Position  : Index_Type;
      New_Item  : Element_Type) return Sequence
   is
     (Content => Set (Container.Content, Position, New_Item));

end Ada.Containers.Functional_Vectors;
