------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--   A D A . S T R I N G S . W I D E _ B O U N D E D . W I D E _ H A S H    --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 2004-2009, Free Software Foundation, Inc.         --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  Note: source of this algorithm: GNAT.HTable.Hash (g-htable.adb)

function Ada.Strings.Wide_Bounded.Wide_Hash
  (Key : Bounded.Bounded_Wide_String)
  return Containers.Hash_Type
is
   use Ada.Containers;

   function Rotate_Left
     (Value  : Hash_Type;
      Amount : Natural) return Hash_Type;
   pragma Import (Intrinsic, Rotate_Left);

   Tmp : Hash_Type;

begin
   Tmp := 0;
   for J in 1 .. Bounded.Length (Key) loop
      Tmp := Rotate_Left (Tmp, 3) +
               Wide_Character'Pos (Bounded.Element (Key, J));
   end loop;

   return Tmp;
end Ada.Strings.Wide_Bounded.Wide_Hash;
