------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--       A D A . S T R I N G S . W I D E _ U N B O U N D E D . H A S H      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

--  Note: source of this algorithm: GNAT.HTable.Hash (g-htable.adb)

function Ada.Strings.Wide_Wide_Unbounded.Hash
  (Key : Unbounded_Wide_Wide_String) return Containers.Hash_Type
is
   use Ada.Containers;

   function Rotate_Left
     (Value  : Hash_Type;
      Amount : Natural) return Hash_Type;
   pragma Import (Intrinsic, Rotate_Left);

   Tmp : Hash_Type;

begin
   Tmp := 0;
   for J in 1 .. Key.Last loop
      Tmp := Rotate_Left (Tmp, 1) +
        Wide_Wide_Character'Pos (Key.Reference (J));
   end loop;

   return Tmp;
end Ada.Strings.Wide_Wide_Unbounded.Hash;
