------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       I N T E R F A C E S . C H E R I                    --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

package body Interfaces.CHERI is

   ----------------------------
   -- Set_Address_And_Bounds --
   ----------------------------

   procedure Set_Address_And_Bounds
     (Cap     : in out Capability;
      Address :        System.Storage_Elements.Integer_Address;
      Length  :        Bounds_Length)
   is
   begin
      Cap := Capability_With_Address_And_Bounds (Cap, Address, Length);
   end Set_Address_And_Bounds;

   ----------------------------------
   -- Set_Address_And_Exact_Bounds --
   ----------------------------------

   procedure Set_Address_And_Exact_Bounds
     (Cap     : in out Capability;
      Address :        System.Storage_Elements.Integer_Address;
      Length  :        Bounds_Length)
   is
   begin
      Cap := Capability_With_Address_And_Exact_Bounds (Cap, Address, Length);
   end Set_Address_And_Exact_Bounds;

   ----------------------
   -- Align_Address_Up --
   ----------------------

   function Align_Address_Up
     (Address : System.Storage_Elements.Integer_Address;
      Length  : Bounds_Length)
      return System.Storage_Elements.Integer_Address
   is
      Mask : constant System.Storage_Elements.Integer_Address :=
        Representable_Alignment_Mask (Length);
   begin
      return (Address + (not Mask)) and Mask;
   end Align_Address_Up;

end Interfaces.CHERI;
