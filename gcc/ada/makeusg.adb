------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              M A K E U S G                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

with Make_Util;
with Osint;
with Output;  use Output;
with Switch;  use Switch;
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

   Display_Usage_Version_And_Help;

   --  Line for -P

   Write_Str ("  -Pproj   Build GNAT Project File proj using GPRbuild");
   Write_Eol;
   Write_Str ("           Treats all other switches as GPRbuild switches");
   Write_Eol;

   --  Line for -a

   Write_Str ("  -a       Consider all files, even readonly ali files");
   Write_Eol;

   --  Line for -b

   Write_Str ("  -b       Bind only");
   Write_Eol;

   --  Line for -B

   Write_Str ("  -B       Build, bind and link full project");
   Write_Eol;

   --  Line for -c

   Write_Str ("  -c       Compile only");
   Write_Eol;

   --  Line for -C

   Write_Str ("  -C       Cache source mappings: " &
              "invoke compiler with temp mapping file");
   Write_Eol;

   --  Line for -C=<mapping file>

   Write_Str ("  -C=mapp  Cache source mappings: " &
              "invoke compiler with mapping file mapp");
   Write_Eol;

   --  Line for -d

   Write_Str ("  -d       Display compilation progress");
   Write_Eol;

   --  Line for -D

   Write_Str ("  -D dir   Specify dir as the object directory");
   Write_Eol;

   --  Line for -eI

   Write_Str ("  -eI      Index of unit in multi-unit source file");
   Write_Eol;

   --  Line for -eL

   Write_Str ("  -eL      Follow symbolic links when processing " &
              "project files");
   Write_Eol;

   --  Line for -eS

   Write_Str ("  -eS      Echo commands to stdout instead of stderr");
   Write_Eol;

   --  Line for -f

   Write_Str ("  -f       Force recompilations of non predefined units");
   Write_Eol;

   --  Line for -F

   Write_Str ("  -F       Full project path name in brief error messages");
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

   --  Line for -p

   Write_Str ("  -p       Create missing obj, lib and exec dirs");
   Write_Eol;

   --  Line for -q

   Write_Str ("  -q       Be quiet/terse");
   Write_Eol;

   --  Line for -R

   Write_Str ("  -R       Do not use a run_path_option when linking");
   Write_Eol;

   --  Line for -s

   Write_Str ("  -s       Recompile if compiler switches have changed");
   Write_Eol;

   --  Line for -u

   Write_Str ("  -u       Unique compilation, only compile the given files");
   Write_Eol;

   --  Line for -U

   Write_Str ("  -U       Unique compilation for all sources of all projects");
   Write_Eol;

   --  Line for -v

   Write_Str ("  -v       Display reasons for all (re)compilations");
   Write_Eol;

   --  Line for -vl

   Write_Str ("  -vl      Verbose output (low verbosity)");
   Write_Eol;

   --  Line for -vm

   Write_Str ("  -vm      Verbose output (medium verbosity)");
   Write_Eol;

   --  Line for -vh

   Write_Str ("  -vh      Equivalent to -v (high verbosity)");
   Write_Eol;

   --  Line for -vPx

   Write_Str ("  -vPx     Specify verbosity when parsing GNAT Project Files");
   Write_Eol;

   --  Line for -we

   Write_Str ("  -we      Treat all warnings as errors");
   Write_Eol;

   --  Line for -wn

   Write_Str ("  -wn      Normal warning mode (cancels -we/-ws)");
   Write_Eol;

   --  Line for -ws

   Write_Str ("  -ws      Suppress all warnings");
   Write_Eol;

   --  Line for -x

   Write_Str ("  -x       " &
              "Allow compilation of needed units external to the projects");
   Write_Eol;

   --  Line for -X

   Write_Str ("  -Xnm=val Specify an external reference for GNAT " &
              "Project Files");
   Write_Eol;

   --  Line for -z

   Write_Str ("  -z       No main subprogram (zero main)");
   Write_Eol;
   Write_Eol;

   Write_Str ("  --create-map-file   Create map file mainprog.map");
   Write_Eol;

   Write_Str ("  --create-map-file=mapfile");
   Write_Eol;
   Write_Str ("                      Create map file mapfile");
   Write_Eol;

   Write_Str ("  --keep-temp-files   Keep temporary files");
   Write_Eol;

   Write_Str ("  --GCC=command       Use this gcc command");
   Write_Eol;

   Write_Str ("  --GNATBIND=command  Use this gnatbind command");
   Write_Eol;

   Write_Str ("  --GNATLINK=command  Use this gnatlink command");
   Write_Eol;
   Write_Eol;

   --  Source and Library search path switches

   Write_Str ("Project, Source and Library search path switches:");
   Write_Eol;

   --  Line for -aP

   Write_Str ("  -aPdir    Add directory dir to project search path");
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

   --  Line for --subdirs=

   Write_Str ("  --subdirs=dir real obj/lib/exec dirs are subdirs");
   Write_Eol;

   --  Line for --source-info=

   Write_Str ("  ");
   Write_Str (Make_Util.Source_Info_Option);
   Write_Str ("file specify a source info file");
   Write_Eol;

   --  Line for --unchecked-shared-lib-imports

   Write_Str ("  ");
   Write_Str (Make_Util.Unchecked_Shared_Lib_Imports);
   Write_Eol;
   Write_Str ("            Allow shared libraries to import static libraries");
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
