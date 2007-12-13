------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

--  This package together with a child package appropriate to the client tool
--  scans switches. Note that the body of the appropraite Usage package must be
--  coordinated with the switches that are recognized by this package. These
--  Usage packages also act as the official documentation for the switches
--  that are recognized. In addition, package Debug documents the otherwise
--  undocumented debug switches that are also recognized.

with Gnatvsn;
with Types; use Types;

------------
-- Switch --
------------

package Switch is

   --  Common switches for GNU tools

   Version_Switch : constant String := "--version";
   Help_Switch    : constant String := "--help";

   -----------------
   -- Subprograms --
   -----------------

   generic
      with procedure Usage;
      --  Print tool-specific part of --help message
   procedure Check_Version_And_Help_G
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String := Gnatvsn.Gnat_Version_String);
   --  Check if switches --version or --help is used. If one of this switch is
   --  used, issue the proper messages and end the process.

   procedure Display_Version
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String := Gnatvsn.Gnat_Version_String);
   --  Display version of a tool when switch --version is used

   function Is_Switch (Switch_Chars : String) return Boolean;
   --  Returns True iff Switch_Chars is at least two characters long, and the
   --  first character is an hyphen ('-').

   function Is_Front_End_Switch (Switch_Chars : String) return Boolean;
   --  Returns True iff Switch_Chars represents a front-end switch, i.e. it
   --  starts with -I, -gnat or -?RTS.

private

   --  This section contains some common routines used by the tool dependent
   --  child packages (there is one such child package for each tool that
   --  uses Switches to scan switches - Compiler/gnatbind/gnatmake/.

   Switch_Max_Value : constant := 999_999;
   --  Maximum value permitted in switches that take a value

   procedure Scan_Nat
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Nat;
      Switch       : Character);
   --  Scan natural integer parameter for switch. On entry, Ptr points just
   --  past the switch character, on exit it points past the last digit of the
   --  integer value.

   procedure Scan_Pos
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Pos;
      Switch       : Character);
   --  Scan positive integer parameter for switch. On entry, Ptr points just
   --  past the switch character, on exit it points past the last digit of the
   --  integer value.

   procedure Bad_Switch (Switch : Character);
   procedure Bad_Switch (Switch : String);
   --  Fail with an appropriate message when a switch is not recognized

end Switch;
