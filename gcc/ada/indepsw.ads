------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I N D E P S W                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
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

--  GNATLINK platform-independent switches

--  Used to convert GNAT switches to their platform-dependent switch
--  equivalent for the underlying linker.

with System.OS_Lib; use System.OS_Lib;

package Indepsw is

   type Switch_Kind is
   --  Independent switches currently supported

     (Map_File);
      --  Produce a map file. The path name of the map file to produce
      --  is given as an argument.

   procedure Convert
     (Switch   : Switch_Kind;
      Argument : String;
      To       : out String_List_Access);
   --  Convert Switch to the platform-dependent linker switch (with or without
   --  additional arguments) To. Issue a warning if Switch is not supported
   --  for the platform; in this case, To is set to null.

   function Is_Supported (Switch : Switch_Kind) return Boolean;
   --  Return True for each independent switch supported by the platform

private
   --  Default warning messages when the switches are not supported by the
   --  implementation. These are in the spec so that the platform specific
   --  bodies do not need to redefine them.

   Map_File_Not_Supported : aliased String :=
     "the underlying linker does not allow the output of a map file";

   No_Support_For : constant array (Switch_Kind) of String_Access :=
                      (Map_File => Map_File_Not_Supported'Access);
   --  All implementations of procedure Convert should include a case
   --  statements with a "when others =>" choice that output the default
   --  warning message:

   --   case Switch is
   --      when ... =>
   --         ...
   --      when others =>
   --         Write_Str ("warning: ");
   --         Write_Line (No_Support_For (Switch).all);
   --         To := null;
   --   end case;

end Indepsw;
