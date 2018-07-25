------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . C O N T A I N E R S . B O U N D E D _ H O L D E R S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2015-2018, Free Software Foundation, Inc.       --
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

with Unchecked_Conversion;

package body Ada.Containers.Bounded_Holders is

   function Size_In_Storage_Elements (Element : Element_Type) return Natural;
   --  This returns the size of Element in storage units. It raises an
   --  exception if the size is not a multiple of Storage_Unit, or if the size
   --  is too big.

   ------------------------------
   -- Size_In_Storage_Elements --
   ------------------------------

   function Size_In_Storage_Elements (Element : Element_Type) return Natural is
      Max_Size : Natural renames Max_Size_In_Storage_Elements;

   begin
      return S : constant Natural := Element'Size / System.Storage_Unit do
         pragma Assert
           (Element'Size mod System.Storage_Unit = 0,
            "Size must be a multiple of Storage_Unit");

         pragma Assert
           (S <= Max_Size, "Size is too big:" & S'Img & " >" & Max_Size'Img);
      end return;
   end Size_In_Storage_Elements;

   function Cast is new
     Unchecked_Conversion (System.Address, Element_Access);

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Holder) return Boolean is
   begin
      return Get (Left) = Get (Right);
   end "=";

   ---------
   -- Get --
   ---------

   function Get (Container : Holder) return Element_Type is
   begin
      return Cast (Container'Address).all;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Container : in out Holder; New_Item  : Element_Type) is
      Storage : Storage_Array
        (1 .. Size_In_Storage_Elements (New_Item)) with
          Address => New_Item'Address;
   begin
      Container.Data (Storage'Range) := Storage;
   end Set;

   ---------------
   -- To_Holder --
   ---------------

   function To_Holder (New_Item : Element_Type) return Holder is
   begin
      return Result : Holder do
         Set (Result, New_Item);
      end return;
   end To_Holder;

end Ada.Containers.Bounded_Holders;
