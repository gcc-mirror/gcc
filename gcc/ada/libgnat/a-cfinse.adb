------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                ADA.CONTAINERS.FUNCTIONAL_INFINITE_SEQUENCE               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2022-2022, Free Software Foundation, Inc.         --
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

package body Ada.Containers.Functional_Infinite_Sequences
with SPARK_Mode => Off
is
   use Containers;

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Big_From_Count is new Signed_Conversions
     (Int => Count_Type);

   function Big (C : Count_Type) return Big_Integer renames
     Big_From_Count.To_Big_Integer;

   --  Store Count_Type'Last as a Big Natural because it is often used

   Count_Type_Big_Last : constant Big_Natural := Big (Count_Type'Last);

   function To_Count (C : Big_Natural) return Count_Type;
   --  Convert Big_Natural to Count_Type

   ---------
   -- "<" --
   ---------

   function "<" (Left : Sequence; Right : Sequence) return Boolean is
     (Length (Left) < Length (Right)
      and then (for all N in Left =>
                     Get (Left, N) = Get (Right, N)));

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : Sequence; Right : Sequence) return Boolean is
     (Length (Left) <= Length (Right)
      and then (for all N in Left =>
                     Get (Left, N) = Get (Right, N)));

   ---------
   -- "=" --
   ---------

   function "=" (Left : Sequence; Right : Sequence) return Boolean is
     (Left.Content = Right.Content);

   ---------
   -- Add --
   ---------

   function Add (Container : Sequence; New_Item : Element_Type) return Sequence
   is
     (Add (Container, Last (Container) + 1, New_Item));

   function Add
     (Container : Sequence;
      Position  : Big_Positive;
      New_Item  : Element_Type) return Sequence is
     (Content => Add (Container.Content, To_Count (Position), New_Item));

   --------------------
   -- Constant_Range --
   --------------------

   function Constant_Range
     (Container : Sequence;
      Fst       : Big_Positive;
      Lst       : Big_Natural;
      Item      : Element_Type) return Boolean
   is
      Count_Fst : constant Count_Type := To_Count (Fst);
      Count_Lst : constant Count_Type := To_Count (Lst);

   begin
      for J in Count_Fst .. Count_Lst loop
         if Get (Container.Content, J) /= Item then
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
      Fst       : Big_Positive;
      Lst       : Big_Natural;
      Item      : Element_Type) return Boolean
   is
      Count_Fst : constant Count_Type := To_Count (Fst);
      Count_Lst : constant Count_Type := To_Count (Lst);

   begin
      for J in Count_Fst .. Count_Lst loop
         if Get (Container.Content, J) = Item then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   --------------------
   -- Empty_Sequence --
   --------------------

   function Empty_Sequence return Sequence is
      (Content => <>);

   ------------------
   -- Equal_Except --
   ------------------

   function Equal_Except
     (Left     : Sequence;
      Right    : Sequence;
      Position : Big_Positive) return Boolean
   is
      Count_Pos : constant Count_Type := To_Count (Position);
      Count_Lst : constant Count_Type := To_Count (Last (Left));

   begin
      if Length (Left) /= Length (Right) then
         return False;
      end if;

      for J in 1 .. Count_Lst loop
         if J /= Count_Pos
              and then Get (Left.Content, J) /= Get (Right.Content, J)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equal_Except;

   function Equal_Except
     (Left  : Sequence;
      Right : Sequence;
      X     : Big_Positive;
      Y     : Big_Positive) return Boolean
   is
      Count_X   : constant Count_Type := To_Count (X);
      Count_Y   : constant Count_Type := To_Count (Y);
      Count_Lst : constant Count_Type := To_Count (Last (Left));

   begin
      if Length (Left) /= Length (Right) then
         return False;
      end if;

      for J in 1 .. Count_Lst loop
         if J /= Count_X
              and then J /= Count_Y
              and then Get (Left.Content, J) /= Get (Right.Content, J)
         then
            return False;
         end if;
      end loop;

      return True;
   end Equal_Except;

   ---------
   -- Get --
   ---------

   function Get
     (Container : Sequence;
      Position  : Big_Integer) return Element_Type is
     (Get (Container.Content, To_Count (Position)));

   ----------
   -- Last --
   ----------

   function Last (Container : Sequence) return Big_Natural is
      (Length (Container));

   ------------
   -- Length --
   ------------

   function Length (Container : Sequence) return Big_Natural is
     (Big (Length (Container.Content)));

   -----------------
   -- Range_Equal --
   -----------------

   function Range_Equal
     (Left  : Sequence;
      Right : Sequence;
      Fst   : Big_Positive;
      Lst   : Big_Natural) return Boolean
   is
      Count_Fst : constant Count_Type := To_Count (Fst);
      Count_Lst : constant Count_Type := To_Count (Lst);

   begin
      for J in Count_Fst .. Count_Lst loop
         if Get (Left.Content, J) /= Get (Right.Content, J) then
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
      Fst    : Big_Positive;
      Lst    : Big_Natural;
      Offset : Big_Integer) return Boolean
   is
      Count_Fst : constant Count_Type := To_Count (Fst);
      Count_Lst : constant Count_Type := To_Count (Lst);

   begin
      for J in Count_Fst .. Count_Lst loop
         if Get (Left.Content, J) /= Get (Right, Big (J) + Offset) then
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
      Position : Big_Positive) return Sequence is
     (Content => Remove (Container.Content, To_Count (Position)));

   ---------
   -- Set --
   ---------

   function Set
     (Container : Sequence;
      Position  : Big_Positive;
      New_Item  : Element_Type) return Sequence is
     (Content => Set (Container.Content, To_Count (Position), New_Item));

   --------------
   -- To_Count --
   --------------

   function To_Count (C : Big_Natural) return Count_Type is
   begin
      if C > Count_Type_Big_Last then
         raise Program_Error with "Big_Integer too large for Count_Type";
      end if;
      return Big_From_Count.From_Big_Integer (C);
   end To_Count;

end Ada.Containers.Functional_Infinite_Sequences;
