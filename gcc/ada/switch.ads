------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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

--  This package together with a child package appropriate to the client
--  tool scans switches. Note that the body of the appropraite Usage package
--  must be coordinated with the switches that are recognized by this package.
--  These Usage packages also act as the official documentation for the
--  switches that are recognized. In addition, package Debug documents
--  the otherwise undocumented debug switches that are also recognized.

with Types; use Types;

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

private

   --  This section contains some common routines used by the tool dependent
   --  child packages (there is one such child package for each tool that
   --  uses Switches to scan switches - Compiler/gnatbind/gnatmake/.

   Bad_Switch : exception;
   --  Exception raised if bad switch encountered

   Bad_Switch_Value : exception;
   --  Exception raised if bad switch value encountered

   Missing_Switch_Value : exception;
   --  Exception raised if no switch value encountered

   Too_Many_Output_Files : exception;
   --  Exception raised if the -o switch is encountered more than once

   Switch_Max_Value : constant := 999_999;
   --  Maximum value permitted in switches that take a value

   procedure Scan_Nat
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Nat);
   --  Scan natural integer parameter for switch. On entry, Ptr points
   --  just past the switch character, on exit it points past the last
   --  digit of the integer value.

   procedure Scan_Pos
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Pos);
   --  Scan positive integer parameter for switch. On entry, Ptr points
   --  just past the switch character, on exit it points past the last
   --  digit of the integer value.

end Switch;
