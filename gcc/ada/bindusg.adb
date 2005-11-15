------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             B I N D U S G                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with Osint;  use Osint;
with Output; use Output;

procedure Bindusg is

--  Start of processing for Bindusg

begin
   --  Usage line

   Write_Str ("Usage: ");
   Write_Program_Name;
   Write_Char (' ');
   Write_Str ("switches lfile");
   Write_Eol;
   Write_Eol;

   --  Line for -aO switch

   Write_Str ("  -aOdir    Specify library files search path");
   Write_Eol;

   --  Line for -aI switch

   Write_Str ("  -aIdir    Specify source files search path");
   Write_Eol;

   --  Line for a switch

   Write_Str ("  -a        Automatically initialize elaboration procedure");
   Write_Eol;

   --  Line for A switch

   Write_Str ("  -A        Generate binder program in Ada (default)");
   Write_Eol;

   --  Line for -b switch

   Write_Str ("  -b        Generate brief messages to std");
   Write_Str ("err even if verbose mode set");
   Write_Eol;

   --  Line for -c switch

   Write_Str ("  -c        Check only, no generation of b");
   Write_Str ("inder output file");
   Write_Eol;

   --  Line for C switch

   Write_Str ("  -C        Generate binder program in C");
   Write_Eol;

   --  Line for D switch

   Write_Str ("  -Dnnn     Default secondary stack size = nnn bytes");
   Write_Eol;

   --  Line for -e switch

   Write_Str ("  -e        Output complete list of elabor");
   Write_Str ("ation order dependencies");
   Write_Eol;

   --  Line for -E switch

   Write_Str ("  -E        Store tracebacks in Exception occurrences");
   Write_Eol;

   --  The -f switch is voluntarily omitted, because it is obsolete

   --  Line for -F switch

   Write_Str ("  -F        Force checking of elaboration Flags");
   Write_Eol;

   --  Line for -h switch

   Write_Str ("  -h        Output this usage (help) infor");
   Write_Str ("mation");
   Write_Eol;

   --  Lines for -I switch

   Write_Str ("  -Idir     Specify library and source files search path");
   Write_Eol;

   Write_Str ("  -I-       Don't look for sources & library files");
   Write_Str (" in default directory");
   Write_Eol;

   --  Line for -K switch

   Write_Str ("  -K        Give list of linker options specified for link");
   Write_Eol;

   --  Line for -l switch

   Write_Str ("  -l        Output chosen elaboration order");
   Write_Eol;

   --  Line of -L switch

   Write_Str ("  -Lxyz     Library build: adainit/final ");
   Write_Str ("renamed to xyzinit/final, implies -n");
   Write_Eol;

   --  Line for -m switch

   Write_Str ("  -mnnn     Limit number of detected error");
   Write_Str ("s to nnn (1-999999)");
   Write_Eol;

   --  Line for -M switch

   Write_Str ("  -Mxyz     Rename generated main program from main to xyz");
   Write_Eol;

   --  Line for -n switch

   Write_Str ("  -n        No Ada main program (foreign main routine)");
   Write_Eol;

   --  Line for -nostdinc

   Write_Str ("  -nostdinc Don't look for source files");
   Write_Str (" in the system default directory");
   Write_Eol;

   --  Line for -nostdlib

   Write_Str ("  -nostdlib Don't look for library files");
   Write_Str (" in the system default directory");
   Write_Eol;

   --  Line for -o switch

   Write_Str ("  -o file   Give the output file name (default is b~xxx.adb) ");
   Write_Eol;

   --  Line for -O switch

   Write_Str ("  -O        Give list of objects required for link");
   Write_Eol;

   --  Line for -p switch

   Write_Str ("  -p        Pessimistic (worst-case) elaborat");
   Write_Str ("ion order");
   Write_Eol;

   --  Line for -r switch

   Write_Str ("  -r        List restrictions that could be a");
   Write_Str ("pplied to this partition");
   Write_Eol;

   --  Line for -s switch

   Write_Str ("  -s        Require all source files to be");
   Write_Str (" present");
   Write_Eol;

   --  Line for -Sxx switch

   Write_Str ("  -S??      Sin/lo/hi/xx for Initialize_Scalars");
   Write_Str (" invalid/low/high/hex");
   Write_Eol;

   --  Line for -static

   Write_Str ("  -static   Link against a static GNAT run time");
   Write_Eol;

   --  Line for -shared

   Write_Str ("  -shared   Link against a shared GNAT run time");
   Write_Eol;

   --  Line for -t switch

   Write_Str ("  -t        Tolerate time stamp and other consistency errors");
   Write_Eol;

   --  Line for -T switch

   Write_Str ("  -Tn       Set time slice value to n milliseconds (n >= 0)");
   Write_Eol;

   --  Line for -v switch

   Write_Str ("  -v        Verbose mode. Error messages, ");
   Write_Str ("header, summary output to stdout");
   Write_Eol;

   --  Lines for -w switch

   Write_Str ("  -wx       Warning mode. (x=s/e for supp");
   Write_Str ("ress/treat as error)");
   Write_Eol;

   --  Line for -x switch

   Write_Str ("  -x        Exclude source files (check ob");
   Write_Str ("ject consistency only)");
   Write_Eol;

   --  Line for X switch

   Write_Str ("  -Xnnn     Default exit status value = nnn");
   Write_Eol;

   --  Line for -z switch

   Write_Str ("  -z        No main subprogram (zero main)");
   Write_Eol;

   --  Line for --RTS

   Write_Str ("  --RTS=dir specify the default source and object search path");
   Write_Eol;

   --  Line for sfile

   Write_Str ("  lfile     Library file names");
   Write_Eol;

end Bindusg;
