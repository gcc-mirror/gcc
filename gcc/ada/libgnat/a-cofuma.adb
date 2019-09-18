------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      ADA.CONTAINERS.FUNCTIONAL_MAPS                      --
--                                                                          --
--                                 B o d y                                  --
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
package body Ada.Containers.Functional_Maps with SPARK_Mode => Off is
   use Key_Containers;
   use Element_Containers;

   ---------
   -- "=" --
   ---------

   function "=" (Left : Map; Right : Map) return Boolean is
     (Left.Keys <= Right.Keys and Right <= Left);

   ----------
   -- "<=" --
   ----------

   function "<=" (Left : Map; Right : Map) return Boolean is
      I2 : Count_Type;

   begin
      for I1 in 1 .. Length (Left.Keys) loop
         I2 := Find (Right.Keys, Get (Left.Keys, I1));
         if I2 = 0
           or else Get (Right.Elements, I2) /= Get (Left.Elements, I1)
         then
            return False;
         end if;
      end loop;
      return True;
   end "<=";

   ---------
   -- Add --
   ---------

   function Add
     (Container : Map;
      New_Key   : Key_Type;
      New_Item  : Element_Type) return Map
   is
   begin
      return
        (Keys     =>
           Add (Container.Keys, Length (Container.Keys) + 1, New_Key),
         Elements =>
           Add
             (Container.Elements, Length (Container.Elements) + 1, New_Item));
   end Add;

   ---------------------------
   -- Elements_Equal_Except --
   ---------------------------

   function Elements_Equal_Except
     (Left    : Map;
      Right   : Map;
      New_Key : Key_Type) return Boolean
   is
   begin
      for J in 1 .. Length (Left.Keys) loop
         declare
            K : constant Key_Type := Get (Left.Keys, J);
         begin
            if not Equivalent_Keys (K, New_Key)
              and then
                (Find (Right.Keys, K) = 0
                  or else Get (Right.Elements, Find (Right.Keys, K)) /=
                          Get (Left.Elements, J))
            then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Elements_Equal_Except;

   function Elements_Equal_Except
     (Left  : Map;
      Right : Map;
      X     : Key_Type;
      Y     : Key_Type) return Boolean
   is
   begin
      for J in 1 .. Length (Left.Keys) loop
         declare
            K : constant Key_Type := Get (Left.Keys, J);
         begin
            if not Equivalent_Keys (K, X)
              and then not Equivalent_Keys (K, Y)
              and then
                (Find (Right.Keys, K) = 0
                  or else Get (Right.Elements, Find (Right.Keys, K)) /=
                          Get (Left.Elements, J))
            then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Elements_Equal_Except;

   ---------
   -- Get --
   ---------

   function Get (Container : Map; Key : Key_Type) return Element_Type is
   begin
      return Get (Container.Elements, Find (Container.Keys, Key));
   end Get;

   -------------
   -- Has_Key --
   -------------

   function Has_Key (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Find (Container.Keys, Key) > 0;
   end Has_Key;

   -----------------
   -- Has_Witness --
   -----------------

   function Has_Witness
     (Container : Map;
      Witness   : Count_Type) return Boolean
   is
     (Witness in 1 .. Length (Container.Keys));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Length (Container.Keys) = 0;
   end Is_Empty;

   -------------------
   -- Keys_Included --
   -------------------

   function Keys_Included (Left : Map; Right : Map) return Boolean is
   begin
      for J in 1 .. Length (Left.Keys) loop
         declare
            K : constant Key_Type := Get (Left.Keys, J);
         begin
            if Find (Right.Keys, K) = 0 then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Keys_Included;

   --------------------------
   -- Keys_Included_Except --
   --------------------------

   function Keys_Included_Except
     (Left    : Map;
      Right   : Map;
      New_Key : Key_Type) return Boolean
   is
   begin
      for J in 1 .. Length (Left.Keys) loop
         declare
            K : constant Key_Type := Get (Left.Keys, J);
         begin
            if not Equivalent_Keys (K, New_Key)
              and then Find (Right.Keys, K) = 0
            then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Keys_Included_Except;

   function Keys_Included_Except
     (Left  : Map;
      Right : Map;
      X     : Key_Type;
      Y     : Key_Type) return Boolean
   is
   begin
      for J in 1 .. Length (Left.Keys) loop
         declare
            K : constant Key_Type := Get (Left.Keys, J);
         begin
            if not Equivalent_Keys (K, X)
              and then not Equivalent_Keys (K, Y)
              and then Find (Right.Keys, K) = 0
            then
               return False;
            end if;
         end;
      end loop;

      return True;
   end Keys_Included_Except;

   ------------
   -- Length --
   ------------

   function Length (Container : Map) return Count_Type is
   begin
      return Length (Container.Elements);
   end Length;

   ------------
   -- Remove --
   ------------

   function Remove (Container : Map; Key : Key_Type) return Map is
      J : constant Extended_Index := Find (Container.Keys, Key);
   begin
      return
        (Keys     => Remove (Container.Keys, J),
         Elements => Remove (Container.Elements, J));
   end Remove;

   ---------------
   -- Same_Keys --
   ---------------

   function Same_Keys (Left : Map; Right : Map) return Boolean is
     (Keys_Included (Left, Right)
       and Keys_Included (Left => Right, Right => Left));

   ---------
   -- Set --
   ---------

   function Set
     (Container : Map;
      Key       : Key_Type;
      New_Item  : Element_Type) return Map
   is
     (Keys     => Container.Keys,
      Elements =>
        Set (Container.Elements, Find (Container.Keys, Key), New_Item));

   -----------
   -- W_Get --
   -----------

   function W_Get
     (Container : Map;
      Witness   : Count_Type) return Element_Type
   is
     (Get (Container.Elements, Witness));

   -------------
   -- Witness --
   -------------

   function Witness (Container : Map; Key : Key_Type) return Count_Type is
     (Find (Container.Keys, Key));

end Ada.Containers.Functional_Maps;
