------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                                X U T I L                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body XUtil is

   use Ada.Streams.Stream_IO;
   use Ada.Strings.Unbounded;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (F : Sfile) is
   begin
      Character'Write (Stream (F), ASCII.LF);
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (F : Sfile; S : String) is
   begin
      String'Write (Stream (F), S);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (F : Sfile; S : VString) is
   begin
      Put (F, To_String (S));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (F : Sfile; S : String) is
   begin
      Put (F, S);
      New_Line (F);
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (F : Sfile; S : VString) is
   begin
      Put_Line (F, To_String (S));
   end Put_Line;

end XUtil;
