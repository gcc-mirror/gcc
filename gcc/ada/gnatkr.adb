------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               G N A T K R                                --
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

with Gnatvsn;
with Krunch;
with Switch;  use Switch;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;

with System.IO; use System.IO;

procedure Gnatkr is
   pragma Ident (Gnatvsn.Gnat_Static_Version_String);

   Count        : Natural;
   Maxlen       : Integer;
   Exit_Program : exception;

   function Get_Maximum_File_Name_Length return Integer;
   pragma Import (C, Get_Maximum_File_Name_Length,
                  "__gnat_get_maximum_file_name_length");

   procedure Usage;
   --  Output usage information

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage: gnatkr  filename[.extension]  [krunch-count]");
   end Usage;

   procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

begin
   Check_Version_And_Help ("GNATKR", "1992");
   Count := Argument_Count;

   if Count < 1 or else Count > 2 then
      Usage;
      raise Exit_Program;

   else
      --  If the length (krunch-count) argument is omitted use the system
      --  default if there is one, otherwise use 8.

      if Count = 1 then
         Maxlen := Get_Maximum_File_Name_Length;

         if Maxlen = -1 then
            Maxlen := 8;
         end if;

      else
         Maxlen := 0;

         for J in Argument (2)'Range loop
            if Argument (2) (J) /= ' ' then
               if Argument (2) (J) not in '0' .. '9' then
                  Put_Line ("Illegal argument for krunch-count");
                  raise Exit_Program;
               else
                  Maxlen := Maxlen * 10 +
                    Character'Pos (Argument (2) (J)) - Character'Pos ('0');
               end if;
            end if;
         end loop;

         --  Zero means crunch only system files

         if Maxlen = 0 then
            Maxlen := Natural'Last;
         end if;

      end if;

      declare
         Fname : String  := Argument (1);
         Klen  : Natural := Fname'Length;

         Extp : Boolean := False;
         --  True if extension is present

         Ext : Natural := 0;
         --  If extension is present, points to it (init to prevent warning)

      begin
         --  Remove extension if present (an extension is defined as the
         --  section of the file name after the last dot in the name. If
         --  there is no dot in the name, then
         --  name is all lower case and contains no other instances of dots)

         for J in reverse 1 .. Klen loop
            if Fname (J) = '.' then
               Extp := True;
               Ext := J;
               Klen := J - 1;
               exit;
            end if;
         end loop;

         --  Fold to lower case and replace dots by dashes

         for J in 1 .. Klen loop
            Fname (J) := To_Lower (Fname (J));

            if Fname (J) = '.' then
               Fname (J) := '-';
            end if;
         end loop;

         Krunch (Fname, Klen, Maxlen, False);

         Put (Fname (1 .. Klen));

         if Extp then
            Put (Fname (Ext .. Fname'Length));
         end if;

         New_Line;
      end;
   end if;

   Set_Exit_Status (Success);

exception
   when Exit_Program =>
      Set_Exit_Status (Failure);

end Gnatkr;
