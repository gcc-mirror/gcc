------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                               X N M A K E                                --
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

--  Program to construct the spec and body of the Nmake package

--    Input files:

--       sinfo.ads     Spec of Sinfo package
--       nmake.adt     Template for Nmake package

--    Output files:

--       nmake.ads     Spec of Nmake package
--       nmake.adb     Body of Nmake package

--  Note: this program assumes that sinfo.ads has passed the error checks that
--  are carried out by the csinfo utility, so it does not duplicate these
--  checks and assumes that sinfo.ads has the correct form.

--   In the absence of any switches, both the ads and adb files are output.
--   The switch -s or /s indicates that only the ads file is to be output.
--   The switch -b or /b indicates that only the adb file is to be output.

--   If a file name argument is given, then the output is written to this file
--   rather than to nmake.ads or nmake.adb. A file name can only be given if
--   exactly one of the -s or -b options is present.

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;              use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;    use Ada.Strings.Maps.Constants;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNAT.Spitbol;                  use GNAT.Spitbol;
with GNAT.Spitbol.Patterns;         use GNAT.Spitbol.Patterns;

procedure XNmake is

   Err : exception;
   --  Raised to terminate execution

   A          : VString := Nul;
   Arg        : VString := Nul;
   Arg_List   : VString := Nul;
   Comment    : VString := Nul;
   Default    : VString := Nul;
   Field      : VString := Nul;
   Line       : VString := Nul;
   Node       : VString := Nul;
   Op_Name    : VString := Nul;
   Prevl      : VString := Nul;
   Synonym    : VString := Nul;
   X          : VString := Nul;

   Lineno : Natural;
   NWidth : Natural;

   FileS : VString := V ("nmake.ads");
   FileB : VString := V ("nmake.adb");
   --  Set to null if corresponding file not to be generated

   Given_File : VString := Nul;
   --  File name given by command line argument

   InS,  InT  : File_Type;
   OutS, OutB : File_Type;

   wsp   : Pattern := Span (' ' & ASCII.HT);

   --  Note: in following patterns, we break up the word revision to
   --  avoid RCS getting enthusiastic about updating the reference!

   Body_Only : Pattern := BreakX (' ') * X & Span (' ') & "--  body only";
   Spec_Only : Pattern := BreakX (' ') * X & Span (' ') & "--  spec only";

   Node_Hdr  : Pattern := wsp & "--  N_" & Rest * Node;
   Punc      : Pattern := BreakX (" .,");

   Binop     : Pattern := wsp & "--  plus fields for binary operator";
   Unop      : Pattern := wsp & "--  plus fields for unary operator";
   Syn       : Pattern := wsp & "--  " & Break (' ') * Synonym
                            & " (" & Break (')') * Field & Rest * Comment;

   Templ     : Pattern := BreakX ('T') * A & "T e m p l a t e";
   Spec      : Pattern := BreakX ('S') * A & "S p e c";

   Sem_Field : Pattern := BreakX ('-') & "-Sem";
   Lib_Field : Pattern := BreakX ('-') & "-Lib";

   Get_Field : Pattern := BreakX (Decimal_Digit_Set) * Field;

   Get_Dflt  : Pattern := BreakX ('(') & "(set to "
                            & Break (" ") * Default & " if";

   Next_Arg  : Pattern := Break (',') * Arg & ',';

   Op_Node   : Pattern := "Op_" & Rest * Op_Name;

   Shft_Rot  : Pattern := "Shift_" or "Rotate_";

   No_Ent    : Pattern := "Or_Else" or "And_Then" or "In" or "Not_In";

   M : Match_Result;

   V_String_Id : constant VString := V ("String_Id");
   V_Node_Id   : constant VString := V ("Node_Id");
   V_Name_Id   : constant VString := V ("Name_Id");
   V_List_Id   : constant VString := V ("List_Id");
   V_Elist_Id  : constant VString := V ("Elist_Id");
   V_Boolean   : constant VString := V ("Boolean");

   procedure WriteS  (S : String);
   procedure WriteB  (S : String);
   procedure WriteBS (S : String);
   procedure WriteS  (S : VString);
   procedure WriteB  (S : VString);
   procedure WriteBS (S : VString);
   --  Write given line to spec or body file or both if active

   procedure WriteB (S : String) is
   begin
      if FileB /= Nul then
         Put_Line (OutB, S);
      end if;
   end WriteB;

   procedure WriteB (S : VString) is
   begin
      if FileB /= Nul then
         Put_Line (OutB, S);
      end if;
   end WriteB;

   procedure WriteBS (S : String) is
   begin
      if FileB /= Nul then
         Put_Line (OutB, S);
      end if;

      if FileS /= Nul then
         Put_Line (OutS, S);
      end if;
   end WriteBS;

   procedure WriteBS (S : VString) is
   begin
      if FileB /= Nul then
         Put_Line (OutB, S);
      end if;

      if FileS /= Nul then
         Put_Line (OutS, S);
      end if;
   end WriteBS;

   procedure WriteS (S : String) is
   begin
      if FileS /= Nul then
         Put_Line (OutS, S);
      end if;
   end WriteS;

   procedure WriteS (S : VString) is
   begin
      if FileS /= Nul then
         Put_Line (OutS, S);
      end if;
   end WriteS;

--  Start of processing for XNmake

begin
   --  Capture our revision (following line updated by RCS)

   Lineno := 0;
   NWidth := 28;
   Anchored_Mode := True;

   for ArgN in 1 .. Argument_Count loop
      declare
         Arg : constant String := Argument (ArgN);

      begin
         if Arg (1) = '-' then
            if Arg'Length = 2
              and then (Arg (2) = 'b' or else Arg (2) = 'B')
            then
               FileS := Nul;

            elsif Arg'Length = 2
              and then (Arg (2) = 's' or else Arg (2) = 'S')
            then
               FileB := Nul;

            else
               raise Err;
            end if;

         else
            if Given_File /= Nul then
               raise Err;
            else
               Given_File := V (Arg);
            end if;
         end if;
      end;
   end loop;

   if FileS = Nul and then FileB = Nul then
      raise Err;

   elsif Given_File /= Nul then
      if FileB = Nul then
         FileS := Given_File;

      elsif FileS = Nul then
         FileB := Given_File;

      else
         raise Err;
      end if;
   end if;

   Open (InS, In_File, "sinfo.ads");
   Open (InT, In_File, "nmake.adt");

   if FileS /= Nul then
      Create (OutS, Out_File, S (FileS));
   end if;

   if FileB /= Nul then
      Create (OutB, Out_File, S (FileB));
   end if;

   Anchored_Mode := True;

   --  Copy initial part of template to spec and body

   loop
      Line := Get_Line (InT);

      --  Skip lines describing the template

      if Match (Line, "--  This file is a template") then
         loop
            Line := Get_Line (InT);
            exit when Line = "";
         end loop;
      end if;

      exit when Match (Line, "package");

      if Match (Line, Body_Only, M) then
         Replace (M, X);
         WriteB (Line);

      elsif Match (Line, Spec_Only, M) then
         Replace (M, X);
         WriteS (Line);

      else
         if Match (Line, Templ, M) then
            Replace (M, A &  "    S p e c    ");
         end if;

         WriteS (Line);

         if Match (Line, Spec, M) then
            Replace (M, A &  "B o d y");
         end if;

         WriteB (Line);
      end if;
   end loop;

   --  Package line reached

   WriteS ("package Nmake is");
   WriteB ("package body Nmake is");
   WriteB ("");

   --  Copy rest of lines up to template insert point to spec only

   loop
      Line := Get_Line (InT);
      exit when Match (Line, "!!TEMPLATE INSERTION POINT");
      WriteS (Line);
   end loop;

   --  Here we are doing the actual insertions, loop through node types

   loop
      Line := Get_Line (InS);

      if Match (Line, Node_Hdr)
        and then not Match (Node, Punc)
        and then Node /= "Unused"
      then
         exit when Node = "Empty";
         Prevl := "   function Make_" & Node & " (Sloc : Source_Ptr";
         Arg_List := Nul;

         --  Loop through fields of one node

         loop
            Line := Get_Line (InS);
            exit when Line = "";

            if Match (Line, Binop) then
               WriteBS (Prevl & ';');
               Append (Arg_List, "Left_Opnd,Right_Opnd,");
               WriteBS (
                 "      " & Rpad ("Left_Opnd",  NWidth) & " : Node_Id;");
               Prevl :=
                 "      " & Rpad ("Right_Opnd", NWidth) & " : Node_Id";

            elsif Match (Line, Unop) then
               WriteBS (Prevl & ';');
               Append (Arg_List, "Right_Opnd,");
               Prevl := "      " & Rpad ("Right_Opnd", NWidth) & " : Node_Id";

            elsif Match (Line, Syn) then
               if         Synonym /= "Prev_Ids"
                 and then Synonym /= "More_Ids"
                 and then Synonym /= "Comes_From_Source"
                 and then Synonym /= "Paren_Count"
                 and then not Match (Field, Sem_Field)
                 and then not Match (Field, Lib_Field)
               then
                  Match (Field, Get_Field);

                  if    Field = "Str"   then Field := V_String_Id;
                  elsif Field = "Node"  then Field := V_Node_Id;
                  elsif Field = "Name"  then Field := V_Name_Id;
                  elsif Field = "List"  then Field := V_List_Id;
                  elsif Field = "Elist" then Field := V_Elist_Id;
                  elsif Field = "Flag"  then Field := V_Boolean;
                  end if;

                  if Field = "Boolean" then
                     Default := V ("False");
                  else
                     Default := Nul;
                  end if;

                  Match (Comment, Get_Dflt);

                  WriteBS (Prevl & ';');
                  Append (Arg_List, Synonym & ',');
                  Rpad (Synonym, NWidth);

                  if Default = "" then
                     Prevl := "      " & Synonym & " : " & Field;
                  else
                     Prevl :=
                       "      " & Synonym & " : " & Field & " := " & Default;
                  end if;
               end if;
            end if;
         end loop;

         WriteBS (Prevl & ')');
         WriteS ("      return Node_Id;");
         WriteS ("   pragma Inline (Make_" & Node & ");");
         WriteB ("      return Node_Id");
         WriteB ("   is");
         WriteB ("      N : constant Node_Id :=");

         if Match (Node, "Defining_Identifier") or else
            Match (Node, "Defining_Character")  or else
            Match (Node, "Defining_Operator")
         then
            WriteB ("            New_Entity (N_" & Node & ", Sloc);");
         else
            WriteB ("            New_Node (N_" & Node & ", Sloc);");
         end if;

         WriteB ("   begin");

         while Match (Arg_List, Next_Arg, "") loop
            if Length (Arg) < NWidth then
               WriteB ("      Set_" & Arg & " (N, " & Arg & ");");
            else
               WriteB ("      Set_" & Arg);
               WriteB ("        (N, " & Arg & ");");
            end if;
         end loop;

         if Match (Node, Op_Node) then
            if Node = "Op_Plus" then
               WriteB ("      Set_Chars (N, Name_Op_Add);");

            elsif Node = "Op_Minus" then
               WriteB ("      Set_Chars (N, Name_Op_Subtract);");

            elsif Match (Op_Name, Shft_Rot) then
               WriteB ("      Set_Chars (N, Name_" & Op_Name & ");");

            else
               WriteB ("      Set_Chars (N, Name_" & Node & ");");
            end if;

            if not Match (Op_Name, No_Ent) then
               WriteB ("      Set_Entity (N, Standard_" & Node & ");");
            end if;
         end if;

         WriteB ("      return N;");
         WriteB ("   end Make_" & Node & ';');
         WriteBS ("");
      end if;
   end loop;

   WriteBS ("end Nmake;");

exception

   when Err =>
      Put_Line (Standard_Error, "usage: xnmake [-b] [-s] [filename]");
      Set_Exit_Status (1);

end XNmake;
