------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                             X T R E E P R S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.
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

--  Program to construct the spec of the Treeprs package

--    Input files:

--       sinfo.ads     Spec of Sinfo package
--       treeprs.adt   Template for Treeprs package

--    Output files:

--       treeprs.ads   Spec of Treeprs package

--  Note: this program assumes that sinfo.ads has passed the error checks which
--  are carried out by the CSinfo utility so it does not duplicate these checks

--  An optional argument allows the specification of an output file name to
--  override the default treeprs.ads file name for the generated output file.

with Ada.Command_Line;              use Ada.Command_Line;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNAT.Spitbol;                  use GNAT.Spitbol;
with GNAT.Spitbol.Patterns;         use GNAT.Spitbol.Patterns;
with GNAT.Spitbol.Table_Boolean;    use GNAT.Spitbol.Table_Boolean;
with GNAT.Spitbol.Table_VString;    use GNAT.Spitbol.Table_VString;

procedure XTreeprs is

   package TB renames GNAT.Spitbol.Table_Boolean;
   package TV renames GNAT.Spitbol.Table_VString;

   Err : exception;
   --  Raised on fatal error

   A          : VString := Nul;
   Ffield     : VString := Nul;
   Field      : VString := Nul;
   Fieldno    : VString := Nul;
   Flagno     : VString := Nul;
   Line       : VString := Nul;
   Name       : VString := Nul;
   Node       : VString := Nul;
   Outstring  : VString := Nul;
   Prefix     : VString := Nul;
   S          : VString := Nul;
   S1         : VString := Nul;
   Syn        : VString := Nul;
   Synonym    : VString := Nul;
   Term       : VString := Nul;

   OutS : File_Type;
   --  Output file

   InS : File_Type;
   --  Read sinfo.ads

   InT : File_Type;
   --  Read treeprs.adt

   Special : TB.Table (20);
   --  Table of special fields. These fields are not included in the table
   --  constructed by Xtreeprs, since they are specially handled in treeprs.
   --  This means these field definitions are completely ignored.

   Names : array (1 .. 500) of VString;
   --  Table of names of synonyms

   Positions : array (1 .. 500) of Natural;
   --  Table of starting positions in Pchars string for synonyms

   Strings : TV.Table (300);
   --  Contribution of each synonym to Pchars string, indexed by name

   Count  : Natural := 0;
   --  Number of synonyms processed so far

   Curpos : Natural := 1;
   --  Number of characters generated in Pchars string so far

   Lineno : Natural := 0;
   --  Line number in sinfo.ads

   Field_Base : constant := Character'Pos ('#');
   --  Fields 1-5 are represented by the characters #$%&' (i.e. by five
   --  contiguous characters starting at # (16#23#)).

   Flag_Base : constant := Character'Pos ('(');
   --  Flags 1-18 are represented by the characters ()*+,-./0123456789
   --  (i.e. by 18 contiguous characters starting at (16#28#)).

   Fieldch : Character;
   --  Field character, as per above tables

   Sp : aliased Natural;
   --  Space left on line for Pchars output

   wsp : Pattern := Span (' ' & ASCII.HT);

   Is_Temp  : Pattern := BreakX ('T') * A & "T e m p l a t e";
   Get_Node : Pattern := wsp & "--  N_" & Rest * Node;
   Tst_Punc : Pattern := Break (" ,.");
   Get_Syn  : Pattern := Span (' ') & "--  " & Break (' ') * Synonym
                & " (" & Break (')') * Field;
   Brk_Min  : Pattern := Break ('-') * Ffield;
   Is_Flag  : Pattern := "Flag" & Rest * Flagno;
   Is_Field : Pattern := Rtab (1) & Len (1) * Fieldno;
   Is_Syn   : Pattern := wsp & "N_" & Break (",)") * Syn & Len (1) * Term;
   Brk_Node : Pattern := Break (' ') * Node & ' ';
   Chop_SP  : Pattern := Len (Sp'Unrestricted_Access) * S1;

   M : Match_Result;

begin
   Anchored_Mode := True;

   if Argument_Count > 0 then
      Create (OutS, Out_File, Argument (1));
   else
      Create (OutS, Out_File, "treeprs.ads");
   end if;

   Open (InS, In_File, "sinfo.ads");
   Open (InT, In_File, "treeprs.adt");

   --  Initialize special fields table

   Set (Special, "Analyzed",                True);
   Set (Special, "Cannot_Be_Constant",      True);
   Set (Special, "Chars",                   True);
   Set (Special, "Comes_From_Source",       True);
   Set (Special, "Error_Posted",            True);
   Set (Special, "Etype",                   True);
   Set (Special, "Has_No_Side_Effects",     True);
   Set (Special, "Is_Controlling_Actual",   True);
   Set (Special, "Is_Overloaded",           True);
   Set (Special, "Is_Static_Expression",    True);
   Set (Special, "Left_Opnd",               True);
   Set (Special, "Must_Check_Expr",         True);
   Set (Special, "No_Overflow_Expr",        True);
   Set (Special, "Paren_Count",             True);
   Set (Special, "Raises_Constraint_Error", True);
   Set (Special, "Right_Opnd",              True);

   --  Read template header and generate new header

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

      if Match (Line, Is_Temp, M) then
         Replace (M, A & "    S p e c    ");
      end if;

      Put_Line (OutS, Line);
   end loop;

   Put_Line (OutS, Line);

   --  Copy rest of comments up to template insert point to spec

   loop
      Line := Get_Line (InT);
      exit when Match (Line, "!!TEMPLATE INSERTION POINT");
      Put_Line (OutS, Line);
   end loop;

   --  Here we are doing the actual insertions

   Put_Line (OutS, "   Pchars : constant String :=");

   --  Loop through comments describing nodes, picking up fields

   loop
      Line := Get_Line (InS);
      Lineno := Lineno + 1;
      exit when Match (Line, "   type Node_Kind");

      if Match (Line, Get_Node)
        and then not Match (Node, Tst_Punc)
      then
         Outstring := Node & ' ';

         loop
            Line := Get_Line (InS);
            exit when Line = "";

            if Match (Line, Get_Syn)
              and then not Match (Synonym, "plus")
              and then not Present (Special, Synonym)
            then
               --  Convert this field into the character used to
               --  represent the field according to the table:

               --    Field1       '#'
               --    Field2       '$'
               --    Field3       '%'
               --    Field4       '&'
               --    Field5       "'"
               --    Flag1        "("
               --    Flag2        ")"
               --    Flag3        '*'
               --    Flag4        '+'
               --    Flag5        ','
               --    Flag6        '-'
               --    Flag7        '.'
               --    Flag8        '/'
               --    Flag9        '0'
               --    Flag10       '1'
               --    Flag11       '2'
               --    Flag12       '3'
               --    Flag13       '4'
               --    Flag14       '5'
               --    Flag15       '6'
               --    Flag16       '7'
               --    Flag17       '8'
               --    Flag18       '9'

               if Match (Field, Brk_Min) then
                  Field := Ffield;
               end if;

               if Match (Field, Is_Flag) then
                  Fieldch := Char (Flag_Base - 1 + N (Flagno));

               elsif Match (Field, Is_Field) then
                  Fieldch := Char (Field_Base - 1 + N (Fieldno));

               else
                  Put_Line
                    (Standard_Error,
                     "*** Line " &
                      Lineno &
                      " has unrecognized field name " &
                      Field);
                  raise Err;
               end if;

               Append (Outstring, Fieldch & Synonym);
            end if;
         end loop;

         Set (Strings, Node, Outstring);
      end if;
   end loop;

   --  Loop through actual definitions of node kind enumeration literals

   loop
      loop
         Line := Get_Line (InS);
         Lineno := Lineno + 1;
         exit when Match (Line, Is_Syn);
      end loop;

      S := Get (Strings, Syn);
      Match (S, Brk_Node, "");
      Count := Count + 1;
      Names (Count) := Syn;
      Positions (Count) := Curpos;
      Curpos := Curpos + Length (S);
      Put_Line (OutS, "      --  " & Node);
      Prefix := V ("      ");
      exit when Term = ")";

      --  Loop to output the string literal for Pchars

      loop
         Sp := 79 - 4 - Length (Prefix);
         exit when (Size (S) <= Sp);
         Match (S, Chop_SP, "");
         Put_Line (OutS, Prefix & '"' & S1 & """ &");
         Prefix := V ("         ");
      end loop;

      Put_Line (OutS, Prefix & '"' & S & """ &");
   end loop;

   Put_Line (OutS, "      """";");
   Put_Line (OutS, "");
   Put_Line
     (OutS, "   type Pchar_Pos_Array is array (Node_Kind) of Positive;");
   Put_Line
     (OutS,
      "   Pchar_Pos : constant Pchar_Pos_Array := Pchar_Pos_Array'(");

   --  Output lines for Pchar_Pos_Array values

   for M in 1 .. Count - 1 loop
      Name := Rpad ("N_" & Names (M), 40);
      Put_Line (OutS, "      " & Name & " => " & Positions (M) & ',');
   end loop;

   Name := Rpad ("N_" & Names (Count), 40);
   Put_Line (OutS, "      " & Name & " => " & Positions (Count) & ");");

   Put_Line (OutS, "");
   Put_Line (OutS, "end Treeprs;");

exception
   when Err =>
      Put_Line (Standard_Error, "*** fatal error");
      Set_Exit_Status (1);

end XTreeprs;
