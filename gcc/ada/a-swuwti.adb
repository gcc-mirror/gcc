------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                  ADA.STRINGS.WIDE_UNBOUNDED.WIDE_TEXT_IO                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-1999 Free Software Foundation, Inc.          --
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

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;

package body Ada.Strings.Wide_Unbounded.Wide_Text_IO is

   --------------
   -- Get_Line --
   --------------

   function Get_Line return Unbounded_Wide_String is
      Buffer : Wide_String (1 .. 1000);
      Last   : Natural;
      Str1   : Wide_String_Access;
      Str2   : Wide_String_Access;

   begin
      Get_Line (Buffer, Last);
      Str1 := new Wide_String'(Buffer (1 .. Last));

      while Last = Buffer'Last loop
         Get_Line (Buffer, Last);
         Str2 := new Wide_String'(Str1.all & Buffer (1 .. Last));
         Free (Str1);
         Str1 := Str2;
      end loop;

      return To_Unbounded_Wide_String (Str1.all);
   end Get_Line;

   function Get_Line
     (File : Ada.Wide_Text_IO.File_Type)
      return Unbounded_Wide_String
   is
      Buffer : Wide_String (1 .. 1000);
      Last   : Natural;
      Str1   : Wide_String_Access;
      Str2   : Wide_String_Access;

   begin
      Get_Line (File, Buffer, Last);
      Str1 := new Wide_String'(Buffer (1 .. Last));

      while Last = Buffer'Last loop
         Get_Line (File, Buffer, Last);
         Str2 := new Wide_String'(Str1.all & Buffer (1 .. Last));
         Free (Str1);
         Str1 := Str2;
      end loop;

      return To_Unbounded_Wide_String (Str1.all);
   end Get_Line;

   ---------
   -- Put --
   ---------

   procedure Put (U : Unbounded_Wide_String) is
   begin
      Put (To_Wide_String (U));
   end Put;

   procedure Put (File : File_Type; U : Unbounded_Wide_String) is
   begin
      Put (File, To_Wide_String (U));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (U : Unbounded_Wide_String) is
   begin
      Put_Line (To_Wide_String (U));
   end Put_Line;

   procedure Put_Line (File : File_Type; U : Unbounded_Wide_String) is
   begin
      Put_Line (File, To_Wide_String (U));
   end Put_Line;

end Ada.Strings.Wide_Unbounded.Wide_Text_IO;
