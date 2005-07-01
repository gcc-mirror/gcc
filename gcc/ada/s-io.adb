------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                            S Y S T E M . I O                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

package body System.IO is

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Spacing : Positive := 1) is
   begin
      for J in 1 .. Spacing loop
         Put (ASCII.LF);
      end loop;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (X : Integer) is

      procedure Put_Int (X : Integer);
      pragma Import (C, Put_Int, "put_int");

   begin
      Put_Int (X);
   end Put;

   procedure Put (C : Character) is

      procedure Put_Char (C : Character);
      pragma Import (C, Put_Char, "put_char");

   begin
      Put_Char (C);
   end Put;

   procedure Put (S : String) is
   begin
      for J in S'Range loop
         Put (S (J));
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      Put (S);
      New_Line;
   end Put_Line;

end System.IO;
