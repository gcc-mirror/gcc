------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001 Free Software Foundation, Inc.               --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package scans make switches. Note that the body of Usage must be
--  coordinated with the switches that are recognized by this package.
--  The Usage package also acts as the official documentation for the
--  switches that are recognized. In addition, package Debug documents
--  the otherwise undocumented debug switches that are also recognized.

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Switch.M is

   procedure Scan_Make_Switches (Switch_Chars : String);
   --  Procedures to scan out binder switches stored in the given string.
   --  The first character is known to be a valid switch character, and there
   --  are no blanks or other switch terminator characters in the string, so
   --  the entire string should consist of valid switch characters, except that
   --  an optional terminating NUL character is allowed. A bad switch causes
   --  a fatal error exit and control does not return. The call also sets
   --  Usage_Requested to True if a ? switch is encountered.

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
     (Switch_Chars : String)
      return         Argument_List;
   --  Similar to the previous procedure. The return value is the list of
   --  simple switches. It may be an empty array if it has been determined
   --  that Switch_Chars is ill-formed or does not contain any switch that
   --  should be stored in an ALI file. The String_Access components of the
   --  returned value should not be deallocated.

end Switch.M;
