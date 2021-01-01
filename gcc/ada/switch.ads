------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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
--  scans switches. Note that the body of the appropriate Usage package must be
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

   procedure Display_Usage_Version_And_Help;
   --  Output the two lines of usage for switches --version and --help

   function Is_Switch (Switch_Chars : String) return Boolean;
   --  Returns True iff Switch_Chars is at least two characters long, and the
   --  first character is an hyphen ('-').

   function Is_Front_End_Switch (Switch_Chars : String) return Boolean;
   --  Returns True iff Switch_Chars represents a front-end switch, i.e. it
   --  starts with -I, -gnat or -?RTS.

   function Is_Internal_GCC_Switch (Switch_Chars : String) return Boolean;
   --  Returns True iff Switch_Chars represents an internal GCC switch to be
   --  followed by a single argument, such as -dumpbase, or --param.
   --  Even though passed by the "gcc" driver, these need not be stored in ALI
   --  files and may safely be ignored by non GCC back-ends.

   function Switch_Last (Switch_Chars : String) return Natural;
   --  Index in Switch_Chars of the last relevant character for later string
   --  comparison purposes. This is typically 'Last, minus one if there is a
   --  terminating ASCII.NUL.

private
   --  This section contains some common routines used by the tool dependent
   --  child packages (there is one such child package for each tool that uses
   --  Switches to scan switches - Compiler/gnatbind/gnatmake/.

   Switch_Max_Value : constant := 999_999;
   --  Maximum value permitted in switches that take a value

   function Nat_Present
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : Integer) return Boolean;
   --  Returns True if an integer is at the current scan location or an equal
   --  sign. This is used as a guard for calling Scan_Nat. Switch_Chars is the
   --  string containing the switch, and Ptr points just past the switch
   --  character. Max is the maximum allowed value of Ptr.

   procedure Scan_Nat
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Nat;
      Switch       : Character);
   --  Scan natural integer parameter for switch. On entry, Ptr points just
   --  past the switch character, on exit it points past the last digit of the
   --  integer value. Max is the maximum allowed value of Ptr, so the scan is
   --  restricted to Switch_Chars (Ptr .. Max). It is possible for Ptr to be
   --  one greater than Max on return if the entire string is digits. Scan_Nat
   --  will skip an optional equal sign if it is present. Nat_Present must be
   --  True, or an error will be signalled.

   procedure Scan_Pos
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Pos;
      Switch       : Character);
   --  Scan positive integer parameter for switch. Identical to Scan_Nat with
   --  same parameters except that zero is considered out of range.

   procedure Bad_Switch (Switch : Character);
   procedure Bad_Switch (Switch : String);
   pragma No_Return (Bad_Switch);
   --  Fail with an appropriate message when a switch is not recognized

end Switch;
