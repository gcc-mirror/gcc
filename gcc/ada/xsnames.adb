------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                              X S N A M E S                               --
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

--  This utility is used to make a new version of the Snames package when
--  new names are added to the spec, the existing versions of snames.ads and
--  snames.adb are read, and updated to match the set of names in snames.ads.
--  The updated versions are written to snames.ns and snames.nb (new spec/body)

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;              use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;    use Ada.Strings.Maps.Constants;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNAT.Spitbol;                  use GNAT.Spitbol;
with GNAT.Spitbol.Patterns;         use GNAT.Spitbol.Patterns;

procedure XSnames is

   InB  : File_Type;
   InS  : File_Type;
   OutS : File_Type;
   OutB : File_Type;

   A, B    : VString := Nul;
   Line    : VString := Nul;
   Name    : VString := Nul;
   Name1   : VString := Nul;
   Oname   : VString := Nul;
   Oval    : VString := Nul;
   Restl   : VString := Nul;

   Tdigs : Pattern := Any (Decimal_Digit_Set) &
                      Any (Decimal_Digit_Set) &
                      Any (Decimal_Digit_Set);

   Name_Ref : Pattern := Span (' ') * A & Break (' ') * Name
                           & Span (' ') * B
                           & ": constant Name_Id := N + " & Tdigs
                           & ';' & Rest * Restl;

   Get_Name : Pattern := "Name_" & Rest * Name1;

   Chk_Low  : Pattern := Pos (0) & Any (Lower_Set) & Rest & Pos (1);

   Findu    : Pattern := Span ('u') * A;

   Val : Natural;

   Xlate_U_Und : Character_Mapping := To_Mapping ("u", "_");

   M : Match_Result;

begin
   Open (InB, In_File, "snames.adb");
   Open (InS, In_File, "snames.ads");

   Create (OutS, Out_File, "snames.ns");
   Create (OutB, Out_File, "snames.nb");

   Anchored_Mode := True;
   Oname := Nul;
   Val := 0;

   Line := A & (Natural'Value (S (Oldrev)) + 1) & " $";
   Line := Rpad (Line, 76) & "--";
   Put_Line (OutB, Line);

   loop
      Line := Get_Line (InB);
      exit when Match (Line, "   Preset_Names");
      Put_Line (OutB, Line);
   end loop;

   Put_Line (OutB, Line);

   LoopN : while not End_Of_File (InS) loop
      Line := Get_Line (InS);

      if not Match (Line, Name_Ref) then
         Put_Line (OutS, Line);

      else
         Oval := Lpad (V (Val), 3, '0');

         if Match (Name, "Last_") then
            Oval := Lpad (V (Val - 1), 3, '0');
         end if;

         Put_Line
           (OutS, A & Name & B & ": constant Name_Id := N + "
            & Oval & ';' & Restl);

         if Match (Name, Get_Name) then
            Name := Name1;
            Val := Val + 1;

            if Match (Name, Findu, M) then
               Replace (M, Translate (A, Xlate_U_Und));
               Translate (Name, Lower_Case_Map);

            elsif not Match (Name, "Op_", "") then
               Translate (Name, Lower_Case_Map);

            else
               Name := 'O' & Translate (Name, Lower_Case_Map);
            end if;

            if Name = "error" then
               Name := V ("<error>");
            end if;

            if not Match (Name, Chk_Low) then
               Put_Line (OutB, "     """ & Name & "#"" &");
            end if;
         end if;
      end if;
   end loop LoopN;

   loop
      Line := Get_Line (InB);
      exit when Match (Line, "      ""#"";");
   end loop;

   Put_Line (OutB, Line);

   while not End_Of_File (InB) loop
      Put_Line (OutB, Get_Line (InB));
   end loop;

   Put_Line (OutB, "--  Updated to match snames.ads revision " & Specrev);

end XSnames;
