------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             B I N D U S G                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Osint;  use Osint;
with Output; use Output;
with Switch; use Switch;

with System.WCh_Con; use System.WCh_Con;

package body Bindusg is

   Already_Displayed : Boolean := False;
   --  Set True if Display called, used to avoid showing usage information
   --  more than once.

   -------------
   -- Display --
   -------------

   procedure Display is
   begin
      if Already_Displayed then
         return;
      else
         Already_Displayed := True;
      end if;

      --  Usage line

      Write_Str ("Usage: ");
      Write_Program_Name;
      Write_Char (' ');
      Write_Str ("switches lfile");
      Write_Eol;
      Write_Eol;

      Display_Usage_Version_And_Help;

      --  Line for @response_file

      Write_Line
        ("  @<resp_file> Get arguments from response file");
      Write_Eol;

      --  Line for -aO switch

      Write_Line
        ("  -aOdir    Specify library files search path");

      --  Line for -aI switch

      Write_Line
        ("  -aIdir    Specify source files search path");

      --  Line for a switch

      Write_Line
        ("  -a        Automatically initialize elaboration procedure");

      --  Lines for -A switch

      Write_Line
        ("  -A        Give list of ALI files in partition");
      Write_Line
        ("  -A=file   Write ALI file list to named file");

      --  Line for -b switch

      Write_Line
        ("  -b        Generate brief messages to stderr even if verbose mode "
         & "set");

      --  Line for -c switch

      Write_Line
        ("  -c        Check only, no generation of binder output file");

      --  Line for -d switch

      Write_Line
        ("  -dnn[k|m] Default primary stack size = nn [kilo|mega] bytes");

      --  Line for D switch

      Write_Line
        ("  -Dnn[k|m] Default secondary stack size = nn [kilo|mega] bytes");

      --  Line for -e switch

      Write_Line
        ("  -e        Output complete list of elaboration order dependencies");

      --  Line for -E switch

      Write_Line
        ("  -Ea       Store tracebacks in exception occurrences");
      Write_Line
        ("  -Es       Store tracebacks in exception occurrences,");
      Write_Line
        ("            and enable symbolic tracebacks");
      Write_Line
        ("  -E        Same as -Ea");

      --  Line for -f switch

      Write_Line
        ("  -ffile    Force elaboration order from given file");

      --  Line for -F switch

      Write_Line
        ("  -F        Force checking of elaboration Flags");

      --  Line for -h switch

      Write_Line
        ("  -h        Output this usage (help) information");

      --  Lines for -I switch

      Write_Line
        ("  -Idir     Specify library and source files search path");
      Write_Line
        ("  -I-       Don't look for sources & library files in default "
         & "directory");

      --  Line for -K switch

      Write_Line
        ("  -K        Give list of linker options specified for link");

      --  Line for -l switch

      Write_Line
        ("  -l        Output chosen elaboration order");

      --  Line of -L switch

      Write_Line
        ("  -Lxyz     Library build: adainit/final renamed to xyzinit/final, "
         & "implies -n");

      --  Line for -m switch

      Write_Line
        ("  -mnnn     Limit number of detected errors/warnings to nnn "
         & "(1-999999)");

      --  Line for -M switch

      Write_Line
        ("  -Mxyz     Rename generated main program from main to xyz");

      --  Line for -n switch

      Write_Line
        ("  -n        No Ada main program (foreign main routine)");

      --  Line for -nostdinc

      Write_Line
        ("  -nostdinc Don't look for source files in the system default "
         & "directory");

      --  Line for -nostdlib

      Write_Line
        ("  -nostdlib Don't look for library files in the system default "
         & "directory");

      --  Line for -o switch

      Write_Line
        ("  -o file   Give the output file name (default is b~xxx.adb)");

      --  Line for -O switch

      Write_Line
        ("  -O        Give list of objects required for link");

      --  Line for -p switch

      Write_Line
        ("  -p        Pessimistic (worst-case) elaboration order");

      --  Line for -P switch

      Write_Line
        ("  -P        Generate binder file suitable for CodePeer");

      --  Line for Q switch

      Write_Line
        ("  -Qnnn     Generate nnn default-sized secondary stacks");

      --  Line for -r switch

      Write_Line
        ("  -r        List restrictions that could be applied to this "
         & "partition");

      --  Line for -R switch

      Write_Line
        ("  -R        List sources referenced in closure");

      --  Line for -s switch

      Write_Line
        ("  -s        Require all source files to be present");

      --  Line for -S?? switch

      Write_Line
        ("  -S??      Sin/lo/hi/xx/ev Initialize_Scalars "
         & "invalid/low/high/hex/env var");

      --  Line for -static

      Write_Line
        ("  -static   Link against a static GNAT run time");

      --  Line for -shared

      Write_Line
        ("  -shared   Link against a shared GNAT run time");

      --  Line for -t switch

      Write_Line
        ("  -t        Tolerate time stamp and other consistency errors");

      --  Line for -T switch

      Write_Line
        ("  -Tn       Set time slice value to n milliseconds (n >= 0)");

      --  Line for -u switch

      Write_Line
        ("  -un       Enable dynamic stack analysis, with n results stored");

      --  Line for -v switch

      Write_Line
        ("  -v        Verbose mode. Error messages, header, summary output to "
         & "stdout");

      --  Line for -V switch

      Write_Line
        ("  -Vkey=val Record bind-time variable key with value val");
      --  Line for -w switch

      Write_Line
        ("  -wx       Warning mode. (x=s/e for suppress/treat as error)");

      --  Line for -W switch

      Write_Str
        ("  -W?       Wide character encoding method (");

      for J in WC_Encoding_Method loop
         Write_Char (WC_Encoding_Letters (J));

         if J = WC_Encoding_Method'Last then
            Write_Char (')');
         else
            Write_Char ('/');
         end if;
      end loop;

      Write_Eol;

      --  Line for -x switch

      Write_Line
        ("  -x        Exclude source files (check object consistency only)");

      --  Line for -X switch

      Write_Line
        ("  -Xnnn     Default exit status value = nnn");

      --  Line for -y switch

      Write_Line
        ("  -y        Enable leap seconds");

      --  Line for -z switch

      Write_Line
        ("  -z        No main subprogram (zero main)");

      --  Line for -Z switch

      Write_Line
        ("  -Z        Zero formatting in auxiliary outputs (-e, -K, -l, -R)");

      --  Line for --RTS

      Write_Line
        ("  --RTS=dir Specify the default source and object search path");

      --  Line for sfile

      Write_Line
        ("  lfile     Library file names");
   end Display;

end Bindusg;
