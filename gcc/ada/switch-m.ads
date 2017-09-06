------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2017, Free Software Foundation, Inc.         --
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

--  This package scans make switches. Note that the body of Usage must be
--  coordinated with the switches that are recognized by this package.
--  The Usage package also acts as the official documentation for the
--  switches that are recognized. In addition, package Debug documents
--  the otherwise undocumented debug switches that are also recognized.

pragma Warnings (Off);
--  This package is used also by gnatcoll
with System.OS_Lib; use System.OS_Lib;
pragma Warnings (On);

package Switch.M is

   Subdirs : String_Ptr := null;
   --  The value after the equal sign in switch --subdirs=...
   --  Contains the relative subdirectory.

   procedure Scan_Make_Switches
     (Switch_Chars      : String;
      Success           : out Boolean);
   --  Scan a gnatmake switch and act accordingly. For switches that are
   --  recognized, Success is set to True. A switch that is not recognized and
   --  consists of one small letter causes a fatal error exit and control does
   --  not return. For all other not recognized switches, Success is set to
   --  False, so that the switch may be passed to the compiler.
   --
   --  Project_Node_Tree is used to store tree-specific parameters like the
   --  project path.

   procedure Normalize_Compiler_Switches
     (Switch_Chars : String;
      Switches     : in out Argument_List_Access;
      Last         : out Natural);
   --  Takes a compiler switch which potentially is equivalent to more
   --  that one simple switches and returns the equivalent list of simple
   --  switches that are stored in an ALI file. Switches will be extended
   --  if initially null or too short. Last indicates the index in Switches
   --  of the last simple switch. Last is equal to zero, if it has been
   --  determined that Switch_Chars is ill-formed or does not contain any
   --  switch that should be stored in an ALI file. Otherwise, the list of
   --  simple switches is Switches (Switches'First .. Last).
   --
   --    Example: if Switch_Chars is equal to "-gnatAwue", then the list of
   --    simple switches will have 3 components: -gnatA, -gnatwu, -gnatwe.
   --
   --  The String_Access components of Switches should not be deallocated:
   --  they are shallow copies of components in a table in the body.

   function Normalize_Compiler_Switches
     (Switch_Chars : String) return Argument_List;
   --  Similar to the previous procedure. The return value is the list of
   --  simple switches. It may be an empty array if it has been determined
   --  that Switch_Chars is ill-formed or does not contain any switch that
   --  should be stored in an ALI file. The String_Access components of the
   --  returned value should not be deallocated.

end Switch.M;
