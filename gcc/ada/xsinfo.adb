------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                               X S I N F O                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

--  Program to construct C header file sinfo.h (C version of sinfo.ads spec,
--  for use by Gigi, contains all definitions and access functions, but does
--  not contain set procedures, since Gigi never modifies the GNAT tree)

--    Input files:

--       sinfo.ads     Spec of Sinfo package

--    Output files:

--       sinfo.h       Corresponding c header file

--  Note: this program assumes that sinfo.ads has passed the error checks
--  which are carried out by the CSinfo utility, so it does not duplicate
--  these checks and assumes the soruce is correct.

--  An optional argument allows the specification of an output file name to
--  override the default sinfo.h file name for the generated output file.

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNAT.Spitbol;                  use GNAT.Spitbol;
with GNAT.Spitbol.Patterns;         use GNAT.Spitbol.Patterns;

procedure XSinfo is

   Done : exception;
   Err  : exception;

   A         : VString := Nul;
   Arg       : VString := Nul;
   Comment   : VString := Nul;
   Line      : VString := Nul;
   N         : VString := Nul;
   N1, N2    : VString := Nul;
   Nam       : VString := Nul;
   Rtn       : VString := Nul;
   Term      : VString := Nul;

   InS       : File_Type;
   Ofile     : File_Type;

   wsp     : Pattern := Span (' ' & ASCII.HT);
   Wsp_For : Pattern := wsp & "for";
   Is_Cmnt : Pattern := wsp & "--";
   Typ_Nod : Pattern := wsp * A & "type Node_Kind is";
   Get_Nam : Pattern := wsp * A & "N_" &  Break (",)") * Nam
                          & Len (1) * Term;
   Sub_Typ : Pattern := wsp * A & "subtype " &  Break (' ') * N;
   No_Cont : Pattern := wsp & Break (' ') * N1 & " .. " & Break (';') * N2;
   Cont_N1 : Pattern := wsp & Break (' ') * N1 & " .." & Rpos (0);
   Cont_N2 : Pattern := Span (' ') & Break (';') * N2;
   Is_Func : Pattern := wsp * A & "function " & Rest * Nam;
   Get_Arg : Pattern := wsp & "(N : " & Break (')') * Arg
                          & ") return " & Break (';') * Rtn
                          & ';' & wsp & "--" & wsp & Rest * Comment;

   NKV : Natural;

   M : Match_Result;


   procedure Getline;
   --  Get non-comment, non-blank line. Also skips "for " rep clauses.

   procedure Getline is
   begin
      loop
         Line := Get_Line (InS);

         if Line /= ""
           and then not Match (Line, Wsp_For)
           and then not Match (Line, Is_Cmnt)
         then
            return;

         elsif Match (Line, "   --  End functions (note") then
            raise Done;
         end if;
      end loop;
   end Getline;

--  Start of processing for XSinfo

begin
   Set_Exit_Status (1);
   Anchored_Mode := True;

   if Argument_Count > 0 then
      Create (Ofile, Out_File, Argument (1));
   else
      Create (Ofile, Out_File, "sinfo.h");
   end if;

   Open (InS, In_File, "sinfo.ads");

   --  Write header to output file

   loop
      Line := Get_Line (InS);
      exit when Line = "";

      Match
        (Line,
         "--                                 S p e c       ",
         "--                              C Header File    ");

      Match (Line, "--", "/*");
      Match (Line, Rtab (2) * A & "--", M);
      Replace (M, A & "*/");
      Put_Line (Ofile, Line);
   end loop;

   --  Skip to package line

   loop
      Getline;
      exit when Match (Line, "package");
   end loop;

   --  Skip to first node kind line

   loop
      Getline;
      exit when Match (Line, Typ_Nod);
      Put_Line (Ofile, Line);
   end loop;

   Put_Line (Ofile, "");
   NKV := 0;

   --  Loop through node kind codes

   loop
      Getline;

      if Match (Line, Get_Nam) then
         Put_Line (Ofile, A & "#define N_" & Nam & ' ' & NKV);
         NKV := NKV + 1;
         exit when not Match (Term, ",");

      else
         Put_Line (Ofile, Line);
      end if;
   end loop;

   Put_Line (Ofile, "");
   Put_Line (Ofile, A & "#define Number_Node_Kinds " & NKV);

   --  Loop through subtype declarations

   loop
      Getline;

      if not Match (Line, Sub_Typ) then
         exit when Match (Line, "   function");
         Put_Line (Ofile, Line);

      else
         Put_Line (Ofile, A & "SUBTYPE (" & N & ", Node_Kind, ");
         Getline;

         --  Normal case

         if Match (Line, No_Cont) then
            Put_Line (Ofile, A & "   " & N1 & ", " & N2 & ')');

         --  Continuation case

         else
            if not Match (Line, Cont_N1) then
               raise Err;
            end if;

            Getline;

            if not Match (Line, Cont_N2) then
               raise Err;
            end if;

            Put_Line (Ofile,  A & "   " & N1 & ',');
            Put_Line (Ofile,  A & "   " & N2 & ')');
         end if;
      end if;
   end loop;

   --  Loop through functions. Note that this loop is terminated by
   --  the call to Getfile encountering the end of functions sentinel

   loop
      if Match (Line, Is_Func) then
         Getline;
            if not Match (Line, Get_Arg) then
               raise Err;
            end if;
         Put_Line
           (Ofile,
            A &  "INLINE " & Rpad (Rtn, 9)
            & ' ' & Rpad (Nam, 30) & " (" & Arg & " N)");

         Put_Line (Ofile,  A & "   { return " & Comment & " (N); }");

      else
         Put_Line (Ofile, Line);
      end if;

      Getline;
   end loop;

exception
   when Done =>
      Put_Line (Ofile, "");
      Set_Exit_Status (0);

end XSinfo;
