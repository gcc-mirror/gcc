------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              M A K E U S G                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
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

with Osint;  use Osint;
with Output; use Output;
with Usage;

procedure Makeusg is

--  Start of processing for Makeusg

begin
   --  Usage line

   Write_Str ("Usage: ");
   Osint.Write_Program_Name;
   Write_Str ("  opts  name  ");
   Write_Str ("{[-cargs opts] [-bargs opts] [-largs opts] [-margs opts]}");
   Write_Eol;
   Write_Eol;
   Write_Str ("  name is one or more file name from which you");
   Write_Str (" can omit the .adb or .ads suffix");
   Write_Eol;
   Write_Eol;

   --  GNATMAKE switches

   Write_Str ("gnatmake switches:");
   Write_Eol;

   --  Line for -a

   Write_Str ("  -a       Consider all files, even readonly ali files");
   Write_Eol;

   --  Line for -b

   Write_Str ("  -b       Bind only");
   Write_Eol;

   --  Line for -c

   Write_Str ("  -c       Compile only");
   Write_Eol;

   --  Line for -C

   Write_Str ("  -C       Cache source mappings: " &
              "invoke the compiler with a mapping file");
   Write_Eol;

   --  Line for -f

   Write_Str ("  -f       Force recompilations of non predefined units");
   Write_Eol;

   --  Line for -i

   Write_Str ("  -i       In place. Replace existing ali file, ");
   Write_Str ("or put it with source");
   Write_Eol;

   --  Line for -jnnn

   Write_Str ("  -jnum    Use nnn processes to compile");
   Write_Eol;

   --  Line for -k

   Write_Str ("  -k       Keep going after compilation errors");
   Write_Eol;

   --  Line for -l

   Write_Str ("  -l       Link only");
   Write_Eol;

   --  Line for -m

   Write_Str ("  -m       Minimal recompilation");
   Write_Eol;

   --  Line for -M

   Write_Str ("  -M       List object file dependences for Makefile");
   Write_Eol;

   --  Line for -n

   Write_Str ("  -n       Check objects up to date, output next file ");
   Write_Str ("to compile if not");
   Write_Eol;

   --  Line for -o

   Write_Str ("  -o name  Choose an alternate executable name");
   Write_Eol;

   --  Line for -P

   Write_Str ("  -Pproj   Use GNAT Project File proj");
   Write_Eol;

   --  Line for -q

   Write_Str ("  -q       Be quiet/terse");
   Write_Eol;

   --  Line for -s

   Write_Str ("  -s       Recompile if compiler switches have changed");
   Write_Eol;

   --  Line for -u

   Write_Str ("  -u       Unique compilation. Only compile the given file.");
   Write_Eol;

   --  Line for -v

   Write_Str ("  -v       Display reasons for all (re)compilations");
   Write_Eol;

   --  Line for -vPx

   Write_Str ("  -vPx     Specify verbosity when parsing GNAT Project Files");
   Write_Eol;

   --  Line for -X

   Write_Str ("  -Xnm=val Specify an external reference for GNAT " &
              "Project Files");
   Write_Eol;

   --  Line for -z

   Write_Str ("  -z       No main subprogram (zero main)");
   Write_Eol;
   Write_Eol;

   Write_Str ("  --GCC=command       Use this gcc command");
   Write_Eol;

   Write_Str ("  --GNATBIND=command  Use this gnatbind command");
   Write_Eol;

   Write_Str ("  --GNATLINK=command  Use this gnatlink command");
   Write_Eol;
   Write_Eol;

   --  Source and Library search path switches

   Write_Str ("Source and Library search path switches:");
   Write_Eol;

   --  Line for -aL

   Write_Str ("  -aLdir    Skip missing library sources if ali in dir");
   Write_Eol;

   --  Line for -A

   Write_Str ("  -Adir     like -aLdir -aIdir");
   Write_Eol;

   --  Line for -aO switch

   Write_Str ("  -aOdir    Specify library/object files search path");
   Write_Eol;

   --  Line for -aI switch

   Write_Str ("  -aIdir    Specify source files search path");
   Write_Eol;

   --  Line for -I switch

   Write_Str ("  -Idir     Like -aIdir -aOdir");
   Write_Eol;

   --  Line for -I- switch

   Write_Str ("  -I-       Don't look for sources & library files");
   Write_Str (" in the default directory");
   Write_Eol;

   --  Line for -L

   Write_Str ("  -Ldir     Look for program libraries also in dir");
   Write_Eol;

   --  Line for -nostdinc

   Write_Str ("  -nostdinc Don't look for sources");
   Write_Str (" in the system default directory");
   Write_Eol;

   --  Line for -nostdlib

   Write_Str ("  -nostdlib Don't look for library files");
   Write_Str (" in the system default directory");
   Write_Eol;

   --  Line for --RTS

   Write_Str ("  --RTS=dir specify the default source and object search"
              & " path");
   Write_Eol;
   Write_Eol;

   --  General Compiler, Binder, Linker switches

   Write_Str ("To pass an arbitrary switch to the Compiler, ");
   Write_Str ("Binder or Linker:");
   Write_Eol;

   --  Line for -cargs

   Write_Str ("  -cargs opts   opts are passed to the compiler");
   Write_Eol;

   --  Line for -bargs

   Write_Str ("  -bargs opts   opts are passed to the binder");
   Write_Eol;

   --  Line for -largs

   Write_Str ("  -largs opts   opts are passed to the linker");
   Write_Eol;

   --  Line for -margs

   Write_Str ("  -margs opts   opts are passed to gnatmake");
   Write_Eol;

   --  Add usage information for gcc

   Usage;

end Makeusg;
