------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I N D E P S W                               --
--                                                                          --
--                                 B o d y                                  --
--                              (GNU version)                               --
--                                                                          --
--          Copyright (C) 2009-2021, Free Software Foundation, Inc.         --
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

--  This is the GNU ld version

package body Indepsw is

   Map_Switch : aliased constant String := "-Wl,-Map,";

   -------------
   -- Convert --
   -------------

   procedure Convert
     (Switch   : Switch_Kind;
      Argument : String;
      To       : out String_List_Access)
   is
   begin
      case Switch is
         when Map_File =>
            To := new Argument_List'(1 => new String'(Map_Switch & Argument));
      end case;
   end Convert;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported (Switch : Switch_Kind) return Boolean is
   begin
      case Switch is
         when Map_File =>
            return True;
      end case;
   end Is_Supported;

end Indepsw;
