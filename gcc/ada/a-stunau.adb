------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--            A D A . S T R I N G S . U N B O U N D E D . A U X             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2002, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body Ada.Strings.Unbounded.Aux is

   ----------------
   -- Get_String --
   ----------------

   function Get_String (U : Unbounded_String) return String_Access is
   begin
      if U.Last = U.Reference'Length then
         return U.Reference;

      else
         declare
            type Unbounded_String_Access is access all Unbounded_String;

            U_Ptr : constant Unbounded_String_Access := U'Unrestricted_Access;
            --  Unbounded_String is a controlled type which is always passed
            --  by reference.  It is always safe to take the pointer to such
            --  object here.  This pointer is used to set the U.Reference
            --  value which would not be possible otherwise as U is read-only.

            Old : String_Access := U.Reference;
            Ret : String_Access;

         begin
            Ret := new String'(U.Reference (1 .. U.Last));
            U_Ptr.Reference := Ret;
            Free (Old);
            return Ret;
         end;
      end if;
   end Get_String;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (UP : in out Unbounded_String; S : String) is
   begin
      if UP.Last = S'Length then
         UP.Reference.all := S;

      else
         declare
            subtype String_1 is String (1 .. S'Length);
            Tmp : String_Access;

         begin
            Tmp := new String'(String_1 (S));
            Finalize (UP);
            UP.Reference := Tmp;
            UP.Last := UP.Reference'Length;
         end;
      end if;
   end Set_String;

   procedure Set_String (UP : in out Unbounded_String; S : String_Access) is
   begin
      Finalize (UP);
      UP.Reference := S;
      UP.Last := UP.Reference'Length;
   end Set_String;

end Ada.Strings.Unbounded.Aux;
