------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                               C E I N F O                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--             Copyright (C) 1998 Free Software Foundation, Inc.            --
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

--  Program to check consistency of einfo.ads and einfo.adb. Checks that
--  field name usage is consistent, including comments mentioning fields.

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;                   use Ada.Text_IO;

with GNAT.Spitbol;                  use GNAT.Spitbol;
with GNAT.Spitbol.Patterns;         use GNAT.Spitbol.Patterns;
with GNAT.Spitbol.Table_VString;

procedure CEinfo is

   package TV renames GNAT.Spitbol.Table_VString;
   use TV;

   Infil  : File_Type;
   Lineno : Natural := 0;

   Err : exception;
   --  Raised on fatal error

   Fieldnm    : VString;
   Accessfunc : VString;
   Line       : VString;

   Fields : GNAT.Spitbol.Table_VString.Table (500);
   --  Maps field names to underlying field access name

   UC : Pattern := Any ("ABCDEFGHIJKLMNOPQRSTUVWXYZ");

   Fnam : Pattern := (UC & Break (' ')) * Fieldnm;

   Field_Def : Pattern := "--    " & Fnam & " (" & Break (')') * Accessfunc;

   Field_Ref : Pattern := "   --    " & Fnam & Break ('(') & Len (1) &
                            Break (')') * Accessfunc;

   Field_Com : Pattern := "   --    " & Fnam & Span (' ') &
                            (Break (' ') or Rest) * Accessfunc;

   Func_Hedr : Pattern := "   function " & Fnam;

   Func_Retn : Pattern := "      return " & Break (' ') * Accessfunc;

   Proc_Hedr : Pattern := "   procedure " & Fnam;

   Proc_Setf : Pattern := "      Set_" & Break (' ') * Accessfunc;

   procedure Next_Line;
   --  Read next line trimmed from Infil into Line and bump Lineno

   procedure Next_Line is
   begin
      Line := Get_Line (Infil);
      Trim (Line);
      Lineno := Lineno + 1;
   end Next_Line;

--  Start of processing for CEinfo

begin
   Anchored_Mode := True;
   New_Line;
   Open (Infil, In_File, "einfo.ads");

   Put_Line ("Acquiring field names from spec");

   loop
      Next_Line;
      exit when Match (Line, "   -- Access Kinds --");

      if Match (Line, Field_Def) then
         Set (Fields, Fieldnm, Accessfunc);
      end if;
   end loop;

   Put_Line ("Checking consistent references in spec");

   loop
      Next_Line;
      exit when Match (Line, "   -- Description of Defined");
   end loop;

   loop
      Next_Line;
      exit when Match (Line, "   -- Component_Alignment Control");

      if Match (Line, Field_Ref) then
         if Accessfunc /= "synth"
              and then
            Accessfunc /= "special"
              and then
            Accessfunc /= Get (Fields, Fieldnm)
         then
            if Present (Fields, Fieldnm) then
               Put_Line ("*** field name incorrect at line " & Lineno);
               Put_Line ("      found field " & Accessfunc);
               Put_Line ("      expecting field " & Get (Fields, Fieldnm));

            else
               Put_Line
                 ("*** unknown field name " & Fieldnm & " at line " & Lineno);
            end if;
         end if;
      end if;
   end loop;

   Close (Infil);
   Open (Infil, In_File, "einfo.adb");
   Lineno := 0;

   Put_Line ("Check listing of fields in body");

   loop
      Next_Line;
      exit when Match (Line, "   -- Attribute Access Functions --");

      if Match (Line, Field_Com)
        and then Fieldnm /= "(unused)"
        and then Accessfunc /= Get (Fields, Fieldnm)
      then
         if Present (Fields, Fieldnm) then
            Put_Line ("*** field name incorrect at line " & Lineno);
            Put_Line ("      found field " & Accessfunc);
            Put_Line ("      expecting field " & Get (Fields, Fieldnm));

         else
            Put_Line
              ("*** unknown field name " & Fieldnm & " at line " & Lineno);
         end if;
      end if;
   end loop;

   Put_Line ("Check references in access routines in body");

   loop
      Next_Line;
      exit when Match (Line, "   -- Classification Functions --");

      if Match (Line, Func_Hedr) then
         null;

      elsif Match (Line, Func_Retn)
        and then Accessfunc /= Get (Fields, Fieldnm)
        and then Fieldnm /= "Mechanism"
      then
         Put_Line ("*** incorrect field at line " & Lineno);
         Put_Line ("      found field " & Accessfunc);
         Put_Line ("      expecting field " & Get (Fields, Fieldnm));
      end if;
   end loop;

   Put_Line ("Check references in set routines in body");

   loop
      Next_Line;
      exit when Match (Line, "   -- Attribute Set Procedures");
   end loop;

   loop
      Next_Line;
      exit when Match (Line, "   ------------");

      if Match (Line, Proc_Hedr) then
         null;

      elsif Match (Line, Proc_Setf)
        and then Accessfunc /= Get (Fields, Fieldnm)
        and then Fieldnm /= "Mechanism"
      then
         Put_Line ("*** incorrect field at line " & Lineno);
         Put_Line ("      found field " & Accessfunc);
         Put_Line ("      expecting field " & Get (Fields, Fieldnm));
      end if;
   end loop;

   Put_Line ("All tests completed successfully, no errors detected");

end CEinfo;
