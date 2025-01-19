------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I N D E P S W                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2009-2025, Free Software Foundation, Inc.         --
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

--  This is the default version: no switches are supported

with Output; use Output;

package body Indepsw is

   -------------
   -- Convert --
   -------------

   procedure Convert
     (Switch   : Switch_Kind;
      Argument : String;
      To       : out String_List_Access)
   is
      pragma Unreferenced (Argument);
   begin
      case Switch is
         when others =>
            Write_Str ("warning: ");
            Write_Line (No_Support_For (Switch).all);
            To := null;
      end case;
   end Convert;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported (Switch : Switch_Kind) return Boolean is
      pragma Unreferenced (Switch);
   begin
      return False;
   end Is_Supported;

end Indepsw;
