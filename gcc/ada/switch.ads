------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package scans switches. Note that the body of Usage must be
--  coordinated with the switches that are recognized by this package.
--  The Usage package also acts as the official documentation for the
--  switches that are recognized. In addition, package Debug documents
--  the otherwise undocumented debug switches that are also recognized.

package Switch is

   --  Note: The default switch character is indicated by Switch_Character,
   --  but regardless of what it is, a hyphen is always allowed as an
   --  (alternative) switch character.

   --  Note: In GNAT, the case of switches is not significant if
   --  Switches_Case_Sensitive is False. If this is the case, switch
   --  characters, or letters appearing in the parameter to a switch, may be
   --  either upper case or lower case.

   -----------------
   -- Subprograms --
   -----------------

   function Is_Switch (Switch_Chars : String) return Boolean;
   --  Returns True iff Switch_Chars is at least two characters long,
   --  and the first character indicates it is a switch.

   function Is_Front_End_Switch (Switch_Chars : String) return Boolean;
   --  Returns True iff Switch_Chars represents a front-end switch,
   --  ie. it starts with -I or -gnat.

   procedure Scan_Front_End_Switches (Switch_Chars : String);
   procedure Scan_Binder_Switches (Switch_Chars : String);
   procedure Scan_Make_Switches (Switch_Chars : String);
   --  Procedures to scan out switches stored in the given string. The first
   --  character is known to be a valid switch character, and there are no
   --  blanks or other switch terminator characters in the string, so the
   --  entire string should consist of valid switch characters, except that
   --  an optional terminating NUL character is allowed. A bad switch causes
   --  a fatal error exit and control does not return. The call also sets
   --  Usage_Requested to True if a ? switch is encountered.

end Switch;
