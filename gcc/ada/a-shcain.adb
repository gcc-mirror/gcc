------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--    A D A . S T R I N G S . H A S H _ C A S E _ I N S E N S I T I V E     --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

--  Note: source of this algorithm: GNAT.HTable.Hash (g-htable.adb)

function Ada.Strings.Hash_Case_Insensitive
  (Key : String) return Containers.Hash_Type
is
   use Ada.Containers;

   Tmp : Hash_Type;

   function Rotate_Left
     (Value  : Hash_Type;
      Amount : Natural) return Hash_Type;
   pragma Import (Intrinsic, Rotate_Left);

begin
   Tmp := 0;
   for J in Key'Range loop
      Tmp := Rotate_Left (Tmp, 3) + Character'Pos (To_Lower (Key (J)));
   end loop;

   return Tmp;
end Ada.Strings.Hash_Case_Insensitive;
