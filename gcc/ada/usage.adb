------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               U S A G E                                  --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--          Copyright (C) 1992-2004, Free Software Foundation, Inc.         --
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

with Hostparm;
with Namet;          use Namet;
with Osint;          use Osint;
with Output;         use Output;
with System.WCh_Con; use System.WCh_Con;

procedure Usage is

   procedure Write_Switch_Char (Sw : String; Prefix : String := "gnat");
   --  Output two spaces followed by the switch character minus followed
   --  Prefix, followed by the string given as the argument, and then
   --  enough blanks to tab to column 13, i.e. assuming Sw is not longer
   --  than 5 characters, the maximum allowed, Write_Switch_Char will
   --  always output exactly 12 characters.

   procedure Write_Switch_Char (Sw : String; Prefix : String := "gnat") is
   begin
      Write_Str ("  -");
      Write_Str (Prefix);
      Write_Str (Sw);

      for J in 1 .. 12 - 3 - Prefix'Length - Sw'Length loop
         Write_Char (' ');
      end loop;
   end Write_Switch_Char;

--  Start of processing for Usage

begin
   Find_Program_Name;

   --  For gnatmake, we are appending this information to the end of
   --  the normal gnatmake output, so generate appropriate header

   if Name_Len >= 8
     and then (Name_Buffer (Name_Len - 7 .. Name_Len) = "gnatmake"
                 or else
               Name_Buffer (Name_Len - 7 .. Name_Len) = "GNATMAKE")
   then
      Write_Eol;
      Write_Line ("Compiler switches (passed to the compiler by gnatmake):");

   else
      --  Usage line

      Write_Str ("Usage: ");
      Write_Program_Name;
      Write_Char (' ');
      Write_Str ("switches sfile");
      Write_Eol;
      Write_Eol;

      --  Line for sfile

      Write_Line ("  sfile     Source file name");
   end if;

   Write_Eol;

   --  Common GCC switches not available in JGNAT

   if not Hostparm.Java_VM then
      Write_Switch_Char ("fstack-check ", "");
      Write_Line ("Generate stack checking code");

      Write_Switch_Char ("fno-inline   ", "");
      Write_Line ("Inhibit all inlining (makes executable smaller)");
   end if;

   --  Common switches available to both GCC and JGNAT

   Write_Switch_Char ("g            ", "");
   Write_Line ("Generate debugging information");

   Write_Switch_Char ("Idir         ", "");
   Write_Line ("Specify source files search path");

   Write_Switch_Char ("I-           ", "");
   Write_Line ("Do not look for sources in current directory");

   Write_Switch_Char ("O[0123]      ", "");
   Write_Line ("Control the optimization level");

   Write_Eol;

   --  Individual lines for switches. Write_Switch_Char outputs fourteen
   --  characters, so the remaining message is allowed to be a maximum
   --  of 65 characters to be comfortable on an 80 character device.
   --  If the Write_Str fits on one line, it is short enough!

   --  Line for -gnata switch

   Write_Switch_Char ("a");
   Write_Line ("Assertions enabled. Pragma Assert/Debug to be activated");

   --  Line for -gnatA switch

   Write_Switch_Char ("A");
   Write_Line ("Avoid processing gnat.adc, if present file will be ignored");

   --  Line for -gnatb switch

   Write_Switch_Char ("b");
   Write_Line ("Generate brief messages to stderr even if verbose mode set");

   --  Line for -gnatc switch

   Write_Switch_Char ("c");
   Write_Line ("Check syntax and semantics only (no code generation)");

   Write_Switch_Char ("C");
   Write_Line ("Compress names in external names and debug info tables");

   --  Line for -gnatd switch

   Write_Switch_Char ("d?");
   Write_Line ("Compiler debug option ? ([.]a-z,A-Z,0-9), see debug.adb");

   --  Line for -gnatD switch

   Write_Switch_Char ("D");
   Write_Line ("Debug expanded generated code rather than source code");

   --  Line for -gnatec switch

   Write_Switch_Char ("ec=?");
   Write_Line ("Specify configuration pragmas file, e.g. -gnatec=/x/f.adc");

   --  Line for -gnateD switch

   Write_Switch_Char ("eD?");
   Write_Line ("Define or redefine preprocessing symbol, e.g. -gnateDsym=val");

   --  Line for -gnatef switch

   Write_Switch_Char ("ef");
   Write_Line ("Full source path in brief error messages");

   --  Line for -gnatem switch

   Write_Switch_Char ("em=?");
   Write_Line ("Specify mapping file, e.g. -gnatem=mapping");

   --  Line for -gnatep switch

   Write_Switch_Char ("ep=?");
   Write_Line ("Specify preprocessing data file, e.g. -gnatep=prep.data");

   --  Line for -gnatE switch

   Write_Switch_Char ("E");
   Write_Line ("Dynamic elaboration checking mode enabled");

   --  Line for -gnatf switch

   Write_Switch_Char ("f");
   Write_Line ("Full errors. Verbose details, all undefined references");

   --  Line for -gnatF switch

   Write_Switch_Char ("F");
   Write_Line ("Force all import/export external names to all uppercase");

   --  Line for -gnatg switch

   Write_Switch_Char ("g");
   Write_Line ("GNAT implementation mode (used for compiling GNAT units)");

   --  Line for -gnatG switch

   Write_Switch_Char ("G");
   Write_Line ("Output generated expanded code in source form");

   --  Line for -gnath switch

   Write_Switch_Char ("h");
   Write_Line ("Output this usage (help) information");

   --  Line for -gnati switch

   Write_Switch_Char ("i?");
   Write_Line ("Identifier char set (?=1/2/3/4/5/8/9/p/f/n/w)");

   --  Line for -gnatk switch

   Write_Switch_Char ("k");
   Write_Line ("Limit file names to nnn characters (k = krunch)");

   --  Line for -gnatl switch

   Write_Switch_Char ("l");
   Write_Line ("Output full source listing with embedded error messages");

   --  Line for -gnatL switch

   Write_Switch_Char ("L");
   Write_Line ("Use longjmp/setjmp for exception handling");

   --  Line for -gnatm switch

   Write_Switch_Char ("mnnn");
   Write_Line ("Limit number of detected errors to nnn (1-999999)");

   --  Line for -gnatn switch

   Write_Switch_Char ("n");
   Write_Line ("Inlining of subprograms (apply pragma Inline across units)");

   --  Line for -gnatN switch

   Write_Switch_Char ("N");
   Write_Line ("Full (frontend) inlining of subprograms");

   --  Line for -gnato switch

   Write_Switch_Char ("o");
   Write_Line ("Enable overflow checking (off by default)");

   --  Line for -gnatO switch

   Write_Switch_Char ("O nm ");
   Write_Line ("Set name of output ali file (internal switch)");

   --  Line for -gnatp switch

   Write_Switch_Char ("p");
   Write_Line ("Suppress all checks");

   --  Line for -gnatP switch

   Write_Switch_Char ("P");
   Write_Line ("Generate periodic calls to System.Polling.Poll");

   --  Line for -gnatq switch

   Write_Switch_Char ("q");
   Write_Line ("Don't quit, try semantics, even if parse errors");

   --  Line for -gnatQ switch

   Write_Switch_Char ("Q");
   Write_Line ("Don't quit, write ali/tree file even if compile errors");

   --  Line for -gnatR switch

   Write_Switch_Char ("R?");
   Write_Line ("List rep inf (?=0/1/2/3 for none/types/all/variable)");

   --  Lines for -gnats switch

   Write_Switch_Char ("s");
   Write_Line ("Syntax check only");

   --  Lines for -gnatS switch

   Write_Switch_Char ("S");
   Write_Line ("Print listing of package Standard");

   --  Lines for -gnatt switch

   Write_Switch_Char ("t");
   Write_Line ("Tree output file to be generated");

   --  Line for -gnatT switch

   Write_Switch_Char ("Tnnn");
   Write_Line ("All compiler tables start at nnn times usual starting size");

   --  Line for -gnatu switch

   Write_Switch_Char ("u");
   Write_Line ("List units for this compilation");

   --  Line for -gnatU switch

   Write_Switch_Char ("U");
   Write_Line ("Enable unique tag for error messages");

   --  Line for -gnatv switch

   Write_Switch_Char ("v");
   Write_Line ("Verbose mode. Full error output with source lines to stdout");

   --  Line for -gnatV switch

   Write_Switch_Char ("Vxx");
   Write_Line
     ("Enable selected validity checking mode, xx = list of parameters:");
   Write_Line ("        a    turn on all validity checking options");
   Write_Line ("        c    turn on checking for copies");
   Write_Line ("        C    turn off checking for copies");
   Write_Line ("        d    turn on default (RM) checking");
   Write_Line ("        D    turn off default (RM) checking");
   Write_Line ("        f    turn on checking for floating-point");
   Write_Line ("        F    turn off checking for floating-point");
   Write_Line ("        i    turn on checking for in params");
   Write_Line ("        I    turn off checking for in params");
   Write_Line ("        m    turn on checking for in out params");
   Write_Line ("        M    turn off checking for in out params");
   Write_Line ("        o    turn on checking for operators/attributes");
   Write_Line ("        O    turn off checking for operators/attributes");
   Write_Line ("        p    turn on checking for parameters");
   Write_Line ("        P    turn off checking for parameters");
   Write_Line ("        r    turn on checking for returns");
   Write_Line ("        R    turn off checking for returns");
   Write_Line ("        s    turn on checking for subscripts");
   Write_Line ("        S    turn off checking for subscripts");
   Write_Line ("        t    turn on checking for tests");
   Write_Line ("        T    turn off checking for tests");
   Write_Line ("        n    turn off all validity checks (including RM)");

   --  Lines for -gnatw switch

   Write_Switch_Char ("wxx");
   Write_Line ("Enable selected warning modes, xx = list of parameters:");
   Write_Line ("        a    turn on all optional warnings (except d,h,l)");
   Write_Line ("        A    turn off all optional warnings");
   Write_Line ("        c    turn on warnings for constant conditional");
   Write_Line ("        C*   turn off warnings for constant conditional");
   Write_Line ("        d    turn on warnings for implicit dereference");
   Write_Line ("        D*   turn off warnings for implicit dereference");
   Write_Line ("        e    treat all warnings as errors");
   Write_Line ("        f    turn on warnings for unreferenced formal");
   Write_Line ("        F*   turn off warnings for unreferenced formal");
   Write_Line ("        g*   turn on warnings for unrecognized pragma");
   Write_Line ("        G    turn off warnings for unrecognized pragma");
   Write_Line ("        h    turn on warnings for hiding variable ");
   Write_Line ("        H*   turn off warnings for hiding variable");
   Write_Line ("        i*   turn on warnings for implementation unit");
   Write_Line ("        I    turn off warnings for implementation unit");
   Write_Line ("        j    turn on warnings for obsolescent " &
                                                  "(annex J) feature");
   Write_Line ("        J*   turn off warnings for obsolescent " &
                                                  "(annex J) feature");
   Write_Line ("        k    turn on warnings on constant variable");
   Write_Line ("        K*   turn off warnings on constant variable");
   Write_Line ("        l    turn on warnings for missing " &
                                                  "elaboration pragma");
   Write_Line ("        L*   turn off warnings for missing " &
                                                  "elaboration pragma");
   Write_Line ("        m    turn on warnings for variable assigned " &
                                                  "but not read");
   Write_Line ("        M*   turn off warnings for variable assigned " &
                                                  "but not read");
   Write_Line ("        n*   normal warning mode (cancels -gnatws/-gnatwe)");
   Write_Line ("        o*   turn on warnings for address clause overlay");
   Write_Line ("        O    turn off warnings for address clause overlay");
   Write_Line ("        p    turn on warnings for ineffective pragma inline");
   Write_Line ("        P*   turn off warnings for ineffective pragma inline");
   Write_Line ("        r    turn on warnings for redundant construct");
   Write_Line ("        R*   turn off warnings for redundant construct");
   Write_Line ("        s    suppress all warnings");
   Write_Line ("        u    turn on warnings for unused entity");
   Write_Line ("        U*   turn off warnings for unused entity");
   Write_Line ("        v*   turn on warnings for unassigned variable");
   Write_Line ("        V    turn off warnings for unassigned variable");
   Write_Line ("        x*   turn on warnings for export/import");
   Write_Line ("        X*   turn off warnings for export/import");
   Write_Line ("        z*   turn on size/align warnings for " &
                                                  "unchecked conversion");
   Write_Line ("        Z    turn off size/align warnings for " &
                                                  "unchecked conversion");
   Write_Line ("        *    indicates default in above list");

   --  Line for -gnatW switch

   Write_Switch_Char ("W");
   Write_Str ("Wide character encoding method (");

   for J in WC_Encoding_Method loop
      Write_Char (WC_Encoding_Letters (J));

      if J = WC_Encoding_Method'Last then
         Write_Char (')');
      else
         Write_Char ('/');
      end if;
   end loop;

   Write_Eol;

   --  Line for -gnatx switch

   Write_Switch_Char ("x");
   Write_Line ("Suppress output of cross-reference information");

   --  Line for -gnatX switch

   Write_Switch_Char ("X");
   Write_Line ("Language extensions permitted");

   --  Lines for -gnaty switch

   Write_Switch_Char ("y");
   Write_Line ("Enable all style checks except 'o', indent=3");

   Write_Switch_Char ("yxx");
   Write_Line ("Enable selected style checks xx = list of parameters:");
   Write_Line ("        1-9  check indentation");
   Write_Line ("        a    check attribute casing");
   Write_Line ("        b    check no blanks at end of lines");
   Write_Line ("        c    check comment format");
   Write_Line ("        e    check end/exit labels present");
   Write_Line ("        f    check no form feeds/vertical tabs in source");
   Write_Line ("        h    check no horizontal tabs in source");
   Write_Line ("        i    check if-then layout");
   Write_Line ("        k    check casing rules for keywords");
   Write_Line ("        l    check reference manual layout");
   Write_Line ("        m    check line length <= 79 characters");
   Write_Line ("        n    check casing of package Standard identifiers");
   Write_Line ("        Mnnn check line length <= nnn characters");
   Write_Line ("        o    check subprogram bodies in alphabetical order");
   Write_Line ("        p    check pragma casing");
   Write_Line ("        r    check casing for identifier references");
   Write_Line ("        s    check separate subprogram specs present");
   Write_Line ("        t    check token separation rules");

   --  Lines for -gnatz switch

   Write_Switch_Char ("z");
   Write_Line ("Distribution stub generation (r/c for receiver/caller stubs)");

   --  Line for -gnatZ switch

   Write_Switch_Char ("Z");
   Write_Line ("Use zero cost exception handling");

   --  Line for -gnat83 switch

   Write_Switch_Char ("83");
   Write_Line ("Enforce Ada 83 restrictions");

end Usage;
