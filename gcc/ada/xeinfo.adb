------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                               X E I N F O                                --
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

--  Program to construct C header file a-einfo.h (C version of einfo.ads spec)
--  for use by Gigi. This header file contaInF all definitions and access
--  functions, but does not contain set procedures, since Gigi is not allowed
--  to modify the GNAT tree)

--    Input files:

--       einfo.ads     spec of Einfo package
--       einfo.adb     body of Einfo package

--    Output files:

--       a-einfo.h     Corresponding c header file

--  Note: It is assumed that the input files have been compiled without errors

--  An optional argument allows the specification of an output file name to
--  override the default a-einfo.h file name for the generated output file.

--  Most, but not all of the functions in Einfo can be inlined in the C header.
--  They are the functions identified by pragma Inline in the spec. Functions
--  that cannot be inlined are simply defined in the header.

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;              use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;    use Ada.Strings.Maps.Constants;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNAT.Spitbol;                  use GNAT.Spitbol;
with GNAT.Spitbol.Patterns;         use GNAT.Spitbol.Patterns;
with GNAT.Spitbol.Table_Boolean;    use GNAT.Spitbol.Table_Boolean;

procedure XEinfo is

   package TB renames GNAT.Spitbol.Table_Boolean;

   Err : exception;

   A         : VString := Nul;
   B         : VString := Nul;
   C         : VString := Nul;
   Expr      : VString := Nul;
   Filler    : VString := Nul;
   Fline     : VString := Nul;
   Formal    : VString := Nul;
   Formaltyp : VString := Nul;
   FN        : VString := Nul;
   Line      : VString := Nul;
   N         : VString := Nul;
   N1        : VString := Nul;
   N2        : VString := Nul;
   N3        : VString := Nul;
   Nam       : VString := Nul;
   Name      : VString := Nul;
   NewS      : VString := Nul;
   Nextlin   : VString := Nul;
   OldS      : VString := Nul;
   Rtn       : VString := Nul;
   Term      : VString := Nul;

   InF   : File_Type;
   --  Used to read full text of both spec and body

   Ofile : File_Type;
   --  Used to write output file

   wsp      : Pattern := NSpan (' ' & ASCII.HT);
   Comment  : Pattern := wsp & "--";
   For_Rep  : Pattern := wsp & "for";
   Get_Func : Pattern := wsp * A & "function" & wsp & Break (' ') * Name;
   Inline   : Pattern := wsp & "pragma Inline (" & Break (')') * Name;
   Get_Pack : Pattern := wsp & "package ";
   Get_Enam : Pattern := wsp & Break (',') * N & ',';
   Find_Fun : Pattern := wsp & "function";
   F_Subtyp : Pattern := wsp * A & "subtype " & Break (' ') * N;
   G_Subtyp : Pattern := wsp & "subtype" & wsp & Break (' ') * NewS
                           & wsp & "is" & wsp & Break (" ;") * OldS
                           & wsp & ';' & wsp & Rtab (0);
   F_Typ    : Pattern := wsp * A & "type " & Break (' ') * N & " is (";
   Get_Nam  : Pattern := wsp * A & Break (",)") * Nam & Len (1) * Term;
   Get_Styp : Pattern := wsp * A & "subtype " & Break (' ') * N;
   Get_N1   : Pattern := wsp & Break (' ') * N1;
   Get_N2   : Pattern := wsp & "-- " & Rest * N2;
   Get_N3   : Pattern := wsp & Break (';') * N3;
   Get_FN   : Pattern := wsp * C & "function" & wsp & Break (" (") * FN;
   Is_Rturn : Pattern := BreakX ('r') & "return";
   Is_Begin : Pattern := wsp & "begin";
   Get_Asrt : Pattern := wsp & "pragma Assert";
   Semicoln : Pattern := BreakX (';');
   Get_Cmnt : Pattern := BreakX ('-') * A & "--";
   Get_Expr : Pattern := wsp & "return " & Break (';') * Expr;
   Chek_End : Pattern := wsp & "end" & BreakX (';') & ';';
   Get_B1   : Pattern := BreakX (' ') * A & " in " & Rest * B;
   Get_B2   : Pattern := BreakX (' ') * A & " = " & Rest * B;
   Get_B3   : Pattern := BreakX (' ') * A & " /= " & Rest * B;
   To_Paren : Pattern := wsp * Filler & '(';
   Get_Fml  : Pattern := Break (" :") * Formal & wsp & ':' & wsp
                           & BreakX (" );") * Formaltyp;
   Nxt_Fml  : Pattern := wsp & "; ";
   Get_Rtn  : Pattern := wsp & "return" & wsp & BreakX (" ;") * Rtn;
   Rem_Prn  : Pattern := wsp & ')';

   M : Match_Result;

   Lineno : Natural := 0;
   --  Line number in spec

   V   : Natural;
   Ctr : Natural;

   Inlined : TB.Table (200);
   --  Inlined<N> = True for inlined function, False otherwise

   Lastinlined : Boolean;

   procedure Badfunc;
   --  Signal bad function in body

   function Getlin return VString;
   --  Get non-comment line (comment lines skipped, also skips FOR rep clauses)
   --  Fatal error (raises End_Error exception) if end of file encountered

   procedure Must (B : Boolean);
   --  Raises Err if the argument (a Match) call, returns False

   procedure Sethead (Line : in out VString; Term : String);
   --  Process function header into C

   -------------
   -- Badfunc --
   -------------

   procedure Badfunc is
   begin
      Put_Line
        (Standard_Error,
         "Body for function " & FN & " does not meet requirements");
      raise Err;
   end Badfunc;

   -------------
   -- Getlin --
   -------------

   function Getlin return VString is
      Lin : VString;

   begin
      loop
         Lin := Get_Line (InF);
         Lineno := Lineno + 1;

         if Lin /= ""
           and then not Match (Lin, Comment)
           and then not Match (Lin, For_Rep)
         then
            return Lin;
         end if;
      end loop;
   end Getlin;

   ----------
   -- Must --
   ----------

   procedure Must (B : Boolean) is
   begin
      if not B then
         raise Err;
      end if;
   end Must;

   -------------
   -- Sethead --
   -------------

   procedure Sethead (Line : in out VString; Term : String) is
      Args : VString;

   begin
      Must (Match (Line, Get_Func, ""));
      Args := Nul;

      if Match (Line, To_Paren, "") then
         Args := Filler & '(';

         loop
            Must (Match (Line, Get_Fml, ""));
            Append (Args, Formaltyp & ' ' & Formal);
            exit when not Match (Line, Nxt_Fml);
            Append (Args, ",");
         end loop;

         Match (Line, Rem_Prn, "");
         Append (Args, ')');
      end if;

      Must (Match (Line, Get_Rtn));

      if Present (Inlined, Name) then
         Put_Line (Ofile, A & "INLINE " & Rtn & ' ' & Name & Args & Term);
      else
         Put_Line (Ofile, A &  Rtn & ' ' & Name & Args & Term);
      end if;
   end Sethead;

--  Start of processing for XEinfo

begin
   Anchored_Mode := True;

   if Argument_Count > 0 then
      Create (Ofile, Out_File, Argument (1));
   else
      Create (Ofile, Out_File, "a-einfo.h");
   end if;

   Open (InF, In_File, "einfo.ads");

   Lineno := 0;

   --  Write header to output file

   loop
      Line := Get_Line (InF);
      Lineno := Lineno + 1;
      exit when Line = "";

      Match (Line,
             "--                                 S p e c       ",
             "--                              C Header File    ");

      Match (Line, "--", "/*");
      Match (Line, Rtab (2) * A & "--", M);
      Replace (M, A & "*/");
      Put_Line (Ofile, Line);
   end loop;

   Put_Line (Ofile, "");

   --  Find and record pragma Inlines

   loop
      Line := Get_Line (InF);
      exit when Match (Line, "   --  END XEINFO INLINES");

      if Match (Line, Inline) then
         Set (Inlined, Name, True);
      end if;
   end loop;

   --  Skip to package line

   Reset (InF, In_File);
   Lineno := 0;

   loop
      Line := Getlin;
      exit when Match (Line, Get_Pack);
   end loop;

   V := 0;
   Line := Getlin;
   Must (Match (Line, wsp & "type Entity_Kind"));

   --  Process entity kind code definitions

   loop
      Line := Getlin;
      exit when not Match (Line, Get_Enam);
      Put_Line (Ofile, "   #define " & Rpad (N, 32) & " " & V);
      V := V + 1;
   end loop;

   Must (Match (Line, wsp & Rest * N));
   Put_Line (Ofile, "   #define " & Rpad (N, 32) & ' ' & V);
   Line := Getlin;

   Must (Match (Line, wsp & ");"));
   Put_Line (Ofile, "");

   --  Loop through subtype and type declarations

   loop
      Line := Getlin;
      exit when Match (Line, Find_Fun);

      --  Case of a subtype declaration

      if Match (Line, F_Subtyp) then

         --  Case of a subtype declaration that is an abbreviation of the
         --  form subtype x is y, and if so generate the appropriate typedef

         if Match (Line, G_Subtyp) then
            Put_Line (Ofile, A & "typedef " & OldS & ' ' & NewS & ';');

         --  Otherwise the subtype must be declaring a subrange of Entity_Id

         else
            Must (Match (Line, Get_Styp));
            Line := Getlin;
            Must (Match (Line, Get_N1));

            loop
               Line := Get_Line (InF);
               Lineno := Lineno + 1;
               exit when not Match (Line, Get_N2);
            end loop;

            Must (Match (Line, Get_N3));
            Put_Line (Ofile, A & "SUBTYPE (" & N & ", Entity_Kind, ");
            Put_Line (Ofile, A & "   " & N1 & ", " & N3 & ')');
            Put_Line (Ofile, "");
         end if;

      --  Case of type declaration

      elsif Match (Line, F_Typ) then
         --  Process type declaration (must be enumeration type)

         Ctr := 0;
         Put_Line (Ofile, A & "typedef char " & N & ';');

         loop
            Line := Getlin;
            Must (Match (Line, Get_Nam));
            Put_Line (Ofile, A & "#define " & Rpad (Nam, 25) & Ctr);
            Ctr := Ctr + 1;
            exit when Term /= ",";
         end loop;

         Put_Line (Ofile, "");

      --  Neither subtype nor type declaration

      else
         raise Err;
      end if;
   end loop;

   --  Process function declarations
   --  Note: Lastinlined used to control blank lines

   Put_Line (Ofile, "");
   Lastinlined := True;

   --  Loop through function declarations

   while Match (Line, Get_FN) loop

      --  Non-inlined function

      if not Present (Inlined, FN) then
         Put_Line (Ofile, "");
         Put_Line
           (Ofile,
            "   #define " & FN & " einfo__" & Translate (FN, Lower_Case_Map));

      --  Inlined function

      else
         if not Lastinlined then
            Put_Line (Ofile, "");
         end if;
      end if;

      --  Merge here to output spec

      Sethead (Line, ";");
      Lastinlined := Get (Inlined, FN);
      Line := Getlin;
   end loop;

   Put_Line (Ofile, "");

   --  Read body to find inlined functions

   Close (InF);
   Open (InF, In_File, "einfo.adb");
   Lineno := 0;

   --  Loop through input lines to find bodies of inlined functions

   while not End_Of_File (InF) loop
      Fline := Get_Line (InF);

      if Match (Fline, Get_FN)
        and then Get (Inlined, FN)
      then
         --  Here we have an inlined function

         if not Match (Fline, Is_Rturn) then
            Line := Fline;
            Badfunc;
         end if;

         Line := Getlin;

         if not Match (Line, Is_Begin) then
            Badfunc;
         end if;

         --  Skip past pragma Asserts

         loop
            Line := Getlin;
            exit when not Match (Line, Get_Asrt);

            --  Pragma asser found, get its continuation lines

            loop
               exit when Match (Line, Semicoln);
               Line := Getlin;
            end loop;
         end loop;

         --  Process return statement

         Match (Line, Get_Cmnt, M);
         Replace (M, A);

         --  Get continuations of return statemnt

         while not Match (Line, Semicoln) loop
            Nextlin := Getlin;
            Match (Nextlin, wsp, " ");
            Append (Line, Nextlin);
         end loop;

         if not Match (Line, Get_Expr) then
            Badfunc;
         end if;

         Line := Getlin;

         if not Match (Line, Chek_End) then
            Badfunc;
         end if;

         Match (Expr, Get_B1, M);
         Replace (M, "IN (" & A & ", " & B & ')');
         Match (Expr, Get_B2, M);
         Replace (M, A & " == " & B);
         Match (Expr, Get_B3, M);
         Replace (M, A & " != " & B);
         Put_Line (Ofile, "");
         Sethead (Fline, "");
         Put_Line (Ofile, C & "   { return " & Expr & "; }");
      end if;
   end loop;

   Put_Line (Ofile, "");
   Put_Line
     (Ofile,
      "/* End of einfo.h (C version of Einfo package specification) */");

exception
   when Err =>
      Put_Line (Standard_Error, Lineno & ".  " & Line);
      Put_Line (Standard_Error, "**** fatal error ****");
      Set_Exit_Status (1);

   when End_Error =>
      Put_Line (Standard_Error, "unexpected end of file");
      Put_Line (Standard_Error, "**** fatal error ****");

end XEinfo;
