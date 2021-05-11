------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               U S A G E                                  --
--                                                                          --
--                                B o d y                                   --
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

--  Warning: the output of this usage for warnings is duplicated in the GNAT
--  reference manual. Be sure to update that if you change the warning list.

with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;

with System.WCh_Con; use System.WCh_Con;

procedure Usage is

   procedure Write_Switch_Char (Sw : String; Prefix : String := "gnat");
   --  Output two spaces followed by the switch character minus followed
   --  Prefix, followed by the string given as the argument, and then enough
   --  blanks to tab to column 13, i.e. assuming Sw is not longer than 5
   --  characters, the maximum allowed, Write_Switch_Char will always output
   --  exactly 12 characters.

   -----------------------
   -- Write_Switch_Char --
   -----------------------

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

   --  Common switches available everywhere

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
   --  characters, so the remaining message is allowed to be a maximum of
   --  65 characters to be comfortable in an 80 character window.

   --  Line for -gnata switch

   Write_Switch_Char ("a");
   Write_Line ("Assertions enabled. Pragma Assert/Debug to be activated");

   --  Line for -gnatA switch

   Write_Switch_Char ("A");
   Write_Line ("Avoid processing gnat.adc, if present file will be ignored");

   --  Line for -gnatb switch

   Write_Switch_Char ("b");
   Write_Line ("Generate brief messages to stderr even if verbose mode set");

   --  Line for -gnatB switch

   Write_Switch_Char ("B");
   Write_Line ("Assume no bad (invalid) values except in 'Valid attribute");

   --  Line for -gnatc switch

   Write_Switch_Char ("c");
   Write_Line ("Check syntax and semantics only (no code generation)");

   --  Line for -gnatC switch

   Write_Switch_Char ("C");
   Write_Line ("Generate CodePeer intermediate format (no code generation)");

   --  Line for -gnatd switch

   Write_Switch_Char ("d?");
   Write_Line ("Compiler debug option ? ([.]a-z,A-Z,0-9), see debug.adb");

   --  Line for -gnatD switch

   Write_Switch_Char ("D");
   Write_Line ("Debug expanded generated code (max line length = 72)");
   Write_Switch_Char ("Dnn");
   Write_Line ("Debug expanded generated code (max line length = nn)");

   --  No line for -gnatea : internal switch

   --  Line for -gnateA switch

   Write_Switch_Char ("eA");
   Write_Line ("Aliasing checks on subprogram parameters");

   --  Line for -gnatec switch

   Write_Switch_Char ("ec=?");
   Write_Line ("Specify configuration pragmas file, e.g. -gnatec=/x/f.adc");

   --  Line for -gnateC switch

   Write_Switch_Char ("eC");
   Write_Line ("Generate CodePeer messages (ignored without -gnatcC)");

   --  Line for -gnated switch

   Write_Switch_Char ("ed");
   Write_Line ("Disable synchronization of atomic variables");

   --  Line for -gnateD switch

   Write_Switch_Char ("eD?");
   Write_Line ("Define or redefine preprocessing symbol, e.g. -gnateDsym=val");

   --  Line for -gnateE switch

   Write_Switch_Char ("eE");
   Write_Line ("Generate extra information in exception messages");

   --  Line for -gnatef switch

   Write_Switch_Char ("ef");
   Write_Line ("Full source path in brief error messages");

   --  Line for -gnateF switch

   Write_Switch_Char ("eF");
   Write_Line ("Check overflow on predefined Float types");

   --  Line for -gnateG switch

   Write_Switch_Char ("eG");
   Write_Line ("Generate preprocessed source");

   --  Line for -gnatei switch

   Write_Switch_Char ("einn");
   Write_Line ("Set maximum number of instantiations to nn");

   --  Line for -gnateI switch

   Write_Switch_Char ("eInn");
   Write_Line ("Index in multi-unit source, e.g. -gnateI2");

   --  Line for -gnatel switch

   Write_Switch_Char ("el");
   Write_Line ("Turn on info messages on generated Elaborate[_All] pragmas");

   --  Line for -gnateL switch

   Write_Switch_Char ("eL");
   Write_Line ("Turn off info messages on generated Elaborate[_All] pragmas");

   --  Line for -gnatem switch

   Write_Switch_Char ("em=?");
   Write_Line ("Specify mapping file, e.g. -gnatem=mapping");

   --  No line for -gnateO=? : internal switch

   --  Line for -gnatep switch

   Write_Switch_Char ("ep=?");
   Write_Line ("Specify preprocessing data file, e.g. -gnatep=prep.data");

   --  Line for -gnateP switch

   Write_Switch_Char ("eP");
   Write_Line ("Pure/Prelaborate errors generate warnings rather than errors");

   --  No line for -gnates=? : internal switch

   --  Line for -gnateS switch

   Write_Switch_Char ("eS");
   Write_Line ("Generate SCO (Source Coverage Obligation) information");

   --  Line for -gnatet switch

   Write_Switch_Char ("et=?");
   Write_Line ("Write target dependent information file ?, e.g. gnatet=tdf");

   --  Line for -gnateT switch

   Write_Switch_Char ("eT=?");
   Write_Line ("Read target dependent information file ?, e.g. gnateT=tdf");

   --  Line for -gnateu switch

   Write_Switch_Char ("eu");
   Write_Line ("Ignore unrecognized style/validity/warning switches");

   --  Line for -gnateV switch

   Write_Switch_Char ("eV");
   Write_Line ("Validity checks on subprogram parameters");

   --  Line for -gnateY switch

   Write_Switch_Char ("eY");
   Write_Line ("Ignore all Style_Checks pragmas in source");

   --  No line for -gnatez : internal switch

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

   --  Lines for -gnatG switch

   Write_Switch_Char ("G");
   Write_Line ("Output generated expanded code (max line length = 72)");
   Write_Switch_Char ("Gnn");
   Write_Line ("Output generated expanded code (max line length = nn)");

   --  Line for -gnath switch

   Write_Switch_Char ("h");
   Write_Line ("Output this usage (help) information");

   --  Line for -gnatH switch

   Write_Switch_Char ("H");
   Write_Line ("Legacy elaboration checking mode enabled");

   --  Line for -gnati switch

   Write_Switch_Char ("i?");
   Write_Line ("Identifier char set (?=1/2/3/4/5/8/9/p/f/n/w)");

   --  Line for -gnatI switch

   Write_Switch_Char ("I");
   Write_Line ("Ignore all representation clauses");

   --  Line for -gnatj switch

   Write_Switch_Char ("jnn");
   Write_Line ("Format error and warning messages to fit nn character lines");

   --  Line for -gnatJ switch

   Write_Switch_Char ("J");
   Write_Line ("Relaxed elaboration checking mode enabled");

   --  Line for -gnatk switch

   Write_Switch_Char ("k");
   Write_Line ("Limit file names to nn characters (k = krunch)");

   --  Lines for -gnatl switch

   Write_Switch_Char ("l");
   Write_Line ("Output full source listing with embedded error messages");
   Write_Switch_Char ("l=f");
   Write_Line ("Output full source listing to specified file");

   --  Line for -gnatL switch

   Write_Switch_Char ("L");
   Write_Line ("List corresponding source text in -gnatG or -gnatD output");

   --  Line for -gnatm switch

   Write_Switch_Char ("mnn");
   Write_Line ("Limit number of detected errors/warnings to nn (1-999999)");

   --  Line for -gnatn switch

   Write_Switch_Char ("n[?]");
   Write_Line ("Enable pragma Inline across units (?=1/2 for moderate/full)");

   --  Line for -gnato switch

   Write_Switch_Char ("o0");
   Write_Line ("Disable overflow checking");

   Write_Switch_Char ("o");
   Write_Line ("Enable overflow checking in STRICT (-gnato1) mode (default)");

   --  Lines for -gnato? switches

   Write_Switch_Char ("o?");
   Write_Line
     ("Enable overflow checks in STRICT/MINIMIZED/ELIMINATED (1/2/3) mode ");
   Write_Switch_Char ("o??");
   Write_Line
     ("Set mode for general/assertion expressions separately");

   --  No line for -gnatO : internal switch

   --  Line for -gnatp switch

   Write_Switch_Char ("p");
   Write_Line ("Suppress all checks");

   --  Line for -gnatq switch

   Write_Switch_Char ("q");
   Write_Line ("Don't quit, try semantics, even if parse errors");

   --  Line for -gnatQ switch

   Write_Switch_Char ("Q");
   Write_Line ("Don't quit, write ali/tree file even if compile errors");

   --  Line for -gnatr switch

   Write_Switch_Char ("r");
   Write_Line ("Treat pragma Restrictions as Restriction_Warnings");

   --  Lines for -gnatR switch

   Write_Switch_Char ("R?");
   Write_Line
     ("List rep info (?=0/1/2/3/4/e/m for none/types/all/sym/cg/ext/mech)");
   Write_Switch_Char ("R?j");
   Write_Line ("List rep info in the JSON data interchange format");
   Write_Switch_Char ("R?s");
   Write_Line ("List rep info to file.rep instead of standard output");

   --  Line for -gnats switch

   Write_Switch_Char ("s");
   Write_Line ("Syntax check only");

   --  Line for -gnatS switch

   Write_Switch_Char ("S");
   Write_Line ("Print listing of package Standard");

   --  Line for -gnatTnn switch

   Write_Switch_Char ("Tnn");
   Write_Line ("All compiler tables start at nn times usual starting size");

   --  Line for -gnatu switch

   Write_Switch_Char ("u");
   Write_Line ("List units for this compilation");

   --  Line for -gnatU switch

   Write_Switch_Char ("U");
   Write_Line ("Enable unique tag for error messages");

   --  Line for -gnatv switch

   Write_Switch_Char ("v");
   Write_Line ("Verbose mode. Full error output with source lines to stdout");

   --  Lines for -gnatV switch

   Write_Switch_Char ("Vxx");
   Write_Line
     ("Enable selected validity checking mode, xx = list of parameters:");
   Write_Line ("        a    turn on all validity checking options");
   Write_Line ("        c    turn on checking for copies");
   Write_Line ("        C    turn off checking for copies");
   Write_Line ("        d    turn on default (RM) checking");
   Write_Line ("        D    turn off default (RM) checking");
   Write_Line ("        e    turn on checking for elementary components");
   Write_Line ("        E    turn off checking for elementary components");
   Write_Line ("        f    turn on checking for floating-point");
   Write_Line ("        F    turn off checking for floating-point");
   Write_Line ("        i    turn on checking for in params");
   Write_Line ("        I    turn off checking for in params");
   Write_Line ("        m    turn on checking for in out params");
   Write_Line ("        M    turn off checking for in out params");
   Write_Line ("        n    turn off all validity checks (including RM)");
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

   --  Lines for -gnatw switch

   Write_Switch_Char ("wxx");
   Write_Line ("Enable selected warning modes, xx = list of parameters:");
   Write_Line ("        *    indicates default setting");
   Write_Line ("        +    indicates warning flag included in -gnatwa");
   Write_Line ("        a    turn on all info/warnings marked below with +");
   Write_Line ("        A    turn off all optional info/warnings");
   Write_Line ("        .a*+ turn on warnings for failing assertion");
   Write_Line ("        .A   turn off warnings for failing assertion");
   Write_Line ("        _a*+ turn on warnings for anonymous allocators");
   Write_Line ("        _A   turn off warnings for anonymous allocators");
   Write_Line ("        b+   turn on warnings for bad fixed value " &
                                                  "(not multiple of small)");
   Write_Line ("        B*   turn off warnings for bad fixed value " &
                                                  "(not multiple of small)");
   Write_Line ("        .b*+ turn on warnings for biased representation");
   Write_Line ("        .B   turn off warnings for biased representation");
   Write_Line ("        c+   turn on warnings for constant conditional");
   Write_Line ("        C*   turn off warnings for constant conditional");
   Write_Line ("        .c+  turn on warnings for unrepped components");
   Write_Line ("        .C*  turn off warnings for unrepped components");
   Write_Line ("        _c*  turn on warnings for unknown " &
                                                 "Compile_Time_Warning");
   Write_Line ("        _C   turn off warnings for unknown " &
                                                 "Compile_Time_Warning");
   Write_Line ("        d    turn on warnings for implicit dereference");
   Write_Line ("        D*   turn off warnings for implicit dereference");
   Write_Line ("        .d   turn on tagging of warnings with -gnatw switch");
   Write_Line ("        .D*  turn off tagging of warnings with -gnatw switch");
   Write_Line ("        e    treat all warnings (but not info) as errors");
   Write_Line ("        .e   turn on every optional info/warning " &
                                                  "(no exceptions)");
   Write_Line ("        E    treat all run-time warnings as errors");
   Write_Line ("        f+   turn on warnings for unreferenced formal");
   Write_Line ("        F*   turn off warnings for unreferenced formal");
   Write_Line ("        .f   turn on warnings for suspicious Subp'Access");
   Write_Line ("        .F*  turn off warnings for suspicious Subp'Access");
   Write_Line ("        g*+  turn on warnings for unrecognized pragma");
   Write_Line ("        G    turn off warnings for unrecognized pragma");
   Write_Line ("        .g   turn on GNAT warnings");
   Write_Line ("        h    turn on warnings for hiding declarations");
   Write_Line ("        H*   turn off warnings for hiding declarations");
   Write_Line ("        .h   turn on warnings for holes in records");
   Write_Line ("        .H*  turn off warnings for holes in records");
   Write_Line ("        i*+  turn on warnings for implementation unit");
   Write_Line ("        I    turn off warnings for implementation unit");
   Write_Line ("        .i*+ turn on warnings for overlapping actuals");
   Write_Line ("        .I   turn off warnings for overlapping actuals");
   Write_Line ("        j+   turn on warnings for obsolescent " &
                                                  "(annex J) feature");
   Write_Line ("        J*   turn off warnings for obsolescent " &
                                                  "(annex J) feature");
   Write_Line ("        .j+  turn on warnings for late dispatching " &
                                                  "primitives");
   Write_Line ("        .J*  turn off warnings for late dispatching " &
                                                  "primitives");
   Write_Line ("        k+   turn on warnings on constant variable");
   Write_Line ("        K*   turn off warnings on constant variable");
   Write_Line ("        .k   turn on warnings for standard redefinition");
   Write_Line ("        .K*  turn off warnings for standard redefinition");
   Write_Line ("        l    turn on warnings for elaboration problems");
   Write_Line ("        L*   turn off warnings for elaboration problems");
   Write_Line ("        .l   turn on info messages for inherited aspects");
   Write_Line ("        .L*  turn off info messages for inherited aspects");
   Write_Line ("        m+   turn on warnings for variable assigned " &
                                                  "but not read");
   Write_Line ("        M*   turn off warnings for variable assigned " &
                                                  "but not read");
   Write_Line ("        .m*+ turn on warnings for suspicious usage " &
                                                      "of modular type");
   Write_Line ("        .M   turn off warnings for suspicious usage " &
                                                      "of modular type");
   Write_Line ("        n*   normal warning mode (cancels -gnatws/-gnatwe)");
   Write_Line ("        .n   turn on info messages for atomic " &
                                                  "synchronization");
   Write_Line ("        .N*  turn off info messages for atomic " &
                                                  "synchronization");
   Write_Line ("        o*   turn on warnings for address clause overlay");
   Write_Line ("        O    turn off warnings for address clause overlay");
   Write_Line ("        .o   turn on warnings for out parameters assigned " &
                                                  "but not read");
   Write_Line ("        .O*  turn off warnings for out parameters assigned " &
                                                  "but not read");
   Write_Line ("        p+   turn on warnings for ineffective pragma " &
                                                  "Inline in frontend");
   Write_Line ("        P*   turn off warnings for ineffective pragma " &
                                                  "Inline in frontend");
   Write_Line ("        .p+  turn on warnings for suspicious parameter " &
                                                  "order");
   Write_Line ("        .P*  turn off warnings for suspicious parameter " &
                                                  "order");
   Write_Line ("        q*+  turn on warnings for questionable " &
                                                  "missing parenthesis");
   Write_Line ("        Q    turn off warnings for questionable " &
                                                  "missing parenthesis");
   Write_Line ("        .q+  turn on warnings for questionable layout of " &
                                                  "record types");
   Write_Line ("        .Q*  turn off warnings for questionable layout of " &
                                                  "record types");
   Write_Line ("        r+   turn on warnings for redundant construct");
   Write_Line ("        R*   turn off warnings for redundant construct");
   Write_Line ("        .r+  turn on warnings for object renaming function");
   Write_Line ("        .R*  turn off warnings for object renaming function");
   Write_Line ("        _r   turn on warnings for components out of order");
   Write_Line ("        _R   turn off warnings for components out of order");
   Write_Line ("        s    suppress all info/warnings");
   Write_Line ("        .s   turn on warnings for overridden size clause");
   Write_Line ("        .S*  turn off warnings for overridden size clause");
   Write_Line ("        t    turn on warnings for tracking deleted code");
   Write_Line ("        T*   turn off warnings for tracking deleted code");
   Write_Line ("        .t*+ turn on warnings for suspicious contract");
   Write_Line ("        .T   turn off warnings for suspicious contract");
   Write_Line ("        u+   turn on warnings for unused entity");
   Write_Line ("        U*   turn off warnings for unused entity");
   Write_Line ("        .u   turn on warnings for unordered enumeration");
   Write_Line ("        .U*  turn off warnings for unordered enumeration");
   Write_Line ("        v*+  turn on warnings for unassigned variable");
   Write_Line ("        V    turn off warnings for unassigned variable");
   Write_Line ("        .v*+ turn on info messages for reverse bit order");
   Write_Line ("        .V   turn off info messages for reverse bit order");
   Write_Line ("        w*+  turn on warnings for wrong low bound assumption");
   Write_Line ("        W    turn off warnings for wrong low bound " &
                                                  "assumption");
   Write_Line ("        .w   turn on warnings on pragma Warnings Off");
   Write_Line ("        .W*  turn off warnings on pragma Warnings Off");
   Write_Line ("        x*+  turn on warnings for export/import");
   Write_Line ("        X    turn off warnings for export/import");
   Write_Line ("        .x+  turn on warnings for non-local exception");
   Write_Line ("        .X*  turn off warnings for non-local exception");
   Write_Line ("        y*+  turn on warnings for Ada compatibility issues");
   Write_Line ("        Y    turn off warnings for Ada compatibility issues");
   Write_Line ("        .y   turn on info messages for why pkg body needed");
   Write_Line ("        .Y*  turn off info messages for why pkg body needed");
   Write_Line ("        z*+  turn on warnings for suspicious " &
                                                  "unchecked conversion");
   Write_Line ("        Z    turn off warnings for suspicious " &
                                                  "unchecked conversion");
   Write_Line ("        .z*+ turn on warnings for record size not a " &
                                                  "multiple of alignment");
   Write_Line ("        .Z   turn off warnings for record size not a " &
                                                  "multiple of alignment");

   --  Line for -gnatW switch

   Write_Switch_Char ("W?");
   Write_Str ("Wide character encoding method (?=");

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
   Write_Line ("Enable default style checks (same as -gnaty3abcefhiklmnprst)");
   Write_Switch_Char ("yxx");
   Write_Line ("Enable selected style checks xx = list of parameters:");
   Write_Line ("        1-9  check indentation");
   Write_Line ("        a    check attribute casing");
   Write_Line ("        A    check array attribute indexes");
   Write_Line ("        b    check no blanks at end of lines");
   Write_Line ("        B    check no use of AND/OR for boolean expressions");
   Write_Line ("        c    check comment format (two spaces)");
   Write_Line ("        C    check comment format (one space)");
   Write_Line ("        d    check no DOS line terminators");
   Write_Line ("        e    check end/exit labels present");
   Write_Line ("        f    check no form feeds/vertical tabs in source");
   Write_Line ("        g    check standard GNAT style rules, same as ydISux");
   Write_Line ("        h    check no horizontal tabs in source");
   Write_Line ("        i    check if-then layout");
   Write_Line ("        I    check mode in");
   Write_Line ("        k    check casing rules for keywords");
   Write_Line ("        l    check reference manual layout");
   Write_Line ("        Lnn  check max nest level < nn ");
   Write_Line ("        m    check line length <= 79 characters");
   Write_Line ("        Mnn  check line length <= nn characters");
   Write_Line ("        n    check casing of package Standard identifiers");
   Write_Line ("        N    turn off all checks");
   Write_Line ("        o    check subprogram bodies in alphabetical order");
   Write_Line ("        O    check overriding indicators");
   Write_Line ("        p    check pragma casing");
   Write_Line ("        r    check casing for identifier references");
   Write_Line ("        s    check separate subprogram specs present");
   Write_Line ("        S    check separate lines after THEN or ELSE");
   Write_Line ("        t    check token separation rules");
   Write_Line ("        u    check no unnecessary blank lines");
   Write_Line ("        x    check extra parentheses around conditionals");
   Write_Line ("        y    turn on default style checks");
   Write_Line ("        -    subtract (turn off) subsequent checks");
   Write_Line ("        +    add (turn on) subsequent checks");

   --  Lines for -gnatyN switch

   Write_Switch_Char ("yN");
   Write_Line ("Cancel all previously set style checks");

   --  Lines for -gnatzc switch

   Write_Switch_Char ("zc");
   Write_Line ("Distribution stub generation for caller stubs");

   --  Lines for -gnatzr switch

   Write_Switch_Char ("zr");
   Write_Line ("Distribution stub generation for receiver stubs");

   if not Latest_Ada_Only then

      --  Line for -gnat83 switch

      Write_Switch_Char ("83");
      Write_Line ("Ada 83 mode");

      --  Line for -gnat95 switch

      Write_Switch_Char ("95");

      if Ada_Version_Default = Ada_95 then
         Write_Line ("Ada 95 mode (default)");
      else
         Write_Line ("Ada 95 mode");
      end if;

      --  Line for -gnat2005 switch

      Write_Switch_Char ("2005");

      if Ada_Version_Default = Ada_2005 then
         Write_Line ("Ada 2005 mode (default)");
      else
         Write_Line ("Ada 2005 mode");
      end if;
   end if;

   --  Line for -gnat2012 switch

   Write_Switch_Char ("2012");

   if Ada_Version_Default = Ada_2012 then
      Write_Line ("Ada 2012 mode (default)");
   else
      Write_Line ("Ada 2012 mode");
   end if;

   --  Line for -gnat-p switch

   Write_Switch_Char ("-p");
   Write_Line ("Cancel effect of previous -gnatp switch");

end Usage;
