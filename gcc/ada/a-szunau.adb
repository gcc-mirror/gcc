------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--   A D A . S T R I N G S . W I D E _ W I D E _ U N B O U N D E D . A U X  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

package body Ada.Strings.Wide_Wide_Unbounded.Aux is

   --------------------
   -- Get_Wide_Wide_String --
   ---------------------

   procedure Get_Wide_Wide_String
     (U : Unbounded_Wide_Wide_String;
      S : out Wide_Wide_String_Access;
      L : out Natural)
   is
   begin
      S := U.Reference;
      L := U.Last;
   end Get_Wide_Wide_String;

   ---------------------
   -- Set_Wide_Wide_String --
   ---------------------

   procedure Set_Wide_Wide_String
     (UP : in out Unbounded_Wide_Wide_String;
      S  : Wide_Wide_String)
   is
   begin
      if S'Length > UP.Last then
         Finalize (UP);
         UP.Reference := new Wide_Wide_String (1 .. S'Length);
      end if;

      UP.Reference (1 .. S'Length) := S;
      UP.Last := S'Length;
   end Set_Wide_Wide_String;

   procedure Set_Wide_Wide_String
     (UP : in out Unbounded_Wide_Wide_String;
      S  : Wide_Wide_String_Access)
   is
   begin
      Finalize (UP);
      UP.Reference := S;
      UP.Last := UP.Reference'Length;
   end Set_Wide_Wide_String;

end Ada.Strings.Wide_Wide_Unbounded.Aux;
