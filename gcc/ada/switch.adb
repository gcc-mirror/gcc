------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

package body Switch is

   ----------------
   -- Bad_Switch --
   ----------------

   procedure Bad_Switch (Switch : Character) is
   begin
      Osint.Fail ("invalid switch: " & Switch);
   end Bad_Switch;

   procedure Bad_Switch (Switch : String) is
   begin
      Osint.Fail ("invalid switch: " & Switch);
   end Bad_Switch;

   ------------------------------
   -- Check_Version_And_Help_G --
   ------------------------------

   procedure Check_Version_And_Help_G
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String := Gnatvsn.Gnat_Version_String)
   is
      Version_Switch_Present : Boolean := False;
      Help_Switch_Present    : Boolean := False;
      Next_Arg               : Natural;

   begin
      --  First check for --version or --help

      Next_Arg := 1;
      while Next_Arg < Arg_Count loop
         declare
            Next_Argv : String (1 .. Len_Arg (Next_Arg));
         begin
            Fill_Arg (Next_Argv'Address, Next_Arg);

            if Next_Argv = Version_Switch then
               Version_Switch_Present := True;

            elsif Next_Argv = Help_Switch then
               Help_Switch_Present := True;
            end if;

            Next_Arg := Next_Arg + 1;
         end;
      end loop;

      --  If --version was used, display version and exit

      if Version_Switch_Present then
         Set_Standard_Output;
         Display_Version (Tool_Name, Initial_Year, Version_String);
         Write_Str (Gnatvsn.Gnat_Free_Software);
         Write_Eol;
         Write_Eol;
         Exit_Program (E_Success);
      end if;

      --  If --help was used, display help and exit

      if Help_Switch_Present then
         Set_Standard_Output;
         Usage;
         Write_Eol;
         Write_Line ("Report bugs to report@adacore.com");
         Exit_Program (E_Success);
      end if;
   end Check_Version_And_Help_G;

   ------------------------------------
   -- Display_Usage_Version_And_Help --
   ------------------------------------

   procedure Display_Usage_Version_And_Help is
   begin
      Write_Str ("  --version   Display version and exit");
      Write_Eol;

      Write_Str ("  --help      Display usage and exit");
      Write_Eol;
      Write_Eol;
   end Display_Usage_Version_And_Help;

   ---------------------
   -- Display_Version --
   ---------------------

   procedure Display_Version
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String := Gnatvsn.Gnat_Version_String)
   is
   begin
      Write_Str (Tool_Name);
      Write_Char (' ');
      Write_Str (Version_String);
      Write_Eol;

      Write_Str ("Copyright (C) ");
      Write_Str (Initial_Year);
      Write_Char ('-');
      Write_Str (Gnatvsn.Current_Year);
      Write_Str (", ");
      Write_Str (Gnatvsn.Copyright_Holder);
      Write_Eol;
   end Display_Version;

   -------------------------
   -- Is_Front_End_Switch --
   -------------------------

   function Is_Front_End_Switch (Switch_Chars : String) return Boolean is
      Ptr : constant Positive := Switch_Chars'First;
   begin
      return Is_Switch (Switch_Chars)
        and then
          (Switch_Chars (Ptr + 1) = 'I'
            or else (Switch_Chars'Length >= 5
                      and then Switch_Chars (Ptr + 1 .. Ptr + 4) = "gnat")
            or else (Switch_Chars'Length >= 5
                      and then Switch_Chars (Ptr + 2 .. Ptr + 4) = "RTS"));
   end Is_Front_End_Switch;

   ----------------------------
   -- Is_Internal_GCC_Switch --
   ----------------------------

   function Is_Internal_GCC_Switch (Switch_Chars : String) return Boolean is
      First : constant Natural := Switch_Chars'First + 1;
      Last  : constant Natural := Switch_Last (Switch_Chars);
   begin
      return Is_Switch (Switch_Chars)
        and then
          (Switch_Chars (First .. Last) = "-param"        or else
           Switch_Chars (First .. Last) = "dumpdir"       or else
           Switch_Chars (First .. Last) = "dumpbase"      or else
           Switch_Chars (First .. Last) = "dumpbase-ext");
   end Is_Internal_GCC_Switch;

   ---------------
   -- Is_Switch --
   ---------------

   function Is_Switch (Switch_Chars : String) return Boolean is
   begin
      return Switch_Chars'Length > 1
        and then Switch_Chars (Switch_Chars'First) = '-';
   end Is_Switch;

   -----------------
   -- Switch_Last --
   -----------------

   function Switch_Last (Switch_Chars : String) return Natural is
      Last : constant Natural := Switch_Chars'Last;
   begin
      if Last >= Switch_Chars'First
        and then Switch_Chars (Last) = ASCII.NUL
      then
         return Last - 1;
      else
         return Last;
      end if;
   end Switch_Last;

   -----------------
   -- Nat_Present --
   -----------------

   function Nat_Present
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : Integer) return Boolean
   is
   begin
      return (Ptr <= Max
                and then Switch_Chars (Ptr) in '0' .. '9')
        or else
             (Ptr < Max
                and then Switch_Chars (Ptr) = '='
                and then Switch_Chars (Ptr + 1) in '0' .. '9');
   end Nat_Present;

   --------------
   -- Scan_Nat --
   --------------

   procedure Scan_Nat
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Nat;
      Switch       : Character)
   is
   begin
      Result := 0;

      if not Nat_Present (Switch_Chars, Max, Ptr) then
         Osint.Fail ("missing numeric value for switch: " & Switch);
      end if;

      if Switch_Chars (Ptr) = '=' then
         Ptr := Ptr + 1;
      end if;

      while Ptr <= Max and then Switch_Chars (Ptr) in '0' .. '9' loop
         Result :=
           Result * 10 +
             Character'Pos (Switch_Chars (Ptr)) - Character'Pos ('0');
         Ptr := Ptr + 1;

         if Result > Switch_Max_Value then
            Osint.Fail ("numeric value out of range for switch: " & Switch);
         end if;
      end loop;
   end Scan_Nat;

   --------------
   -- Scan_Pos --
   --------------

   procedure Scan_Pos
     (Switch_Chars : String;
      Max          : Integer;
      Ptr          : in out Integer;
      Result       : out Pos;
      Switch       : Character)
   is
      Temp : Nat;

   begin
      Scan_Nat (Switch_Chars, Max, Ptr, Temp, Switch);

      if Temp = 0 then
         Osint.Fail ("numeric value out of range for switch: " & Switch);
      end if;

      Result := Temp;
   end Scan_Pos;

end Switch;
