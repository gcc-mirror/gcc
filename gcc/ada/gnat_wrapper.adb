------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         G N A T _ W R A P P E R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2003 Free Software Foundation, Inc.          --
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

--  GNAT_Wrapper is to be used as the starter program for most of the GNAT
--  executables. It sets up the working environment variables and calls the
--  real executable which is to be found under the 'real' sub-directory.
--
--  This avoids using the registry on Windows which is tricky to setup to run
--  multiple compilers (GNAT Pro release and wavefronts for example) at the
--  same time.

with Ada.Command_Line;          use Ada.Command_Line;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

procedure GNAT_Wrapper is
   DS : Character renames Directory_Separator;
   PS : Character renames Path_Separator;

   procedure Split_Command;
   --  Parse Actual_Name and set K and L variables (see below).

   Actual_Name : String_Access := new String'(Command_Name);

   K : Natural;
   --  Index of the directory separator just before program name's first
   --  character.

   L : Natural;
   --  Index of the last character of the GNATPRO install directory.

   LD_LIBRARY_PATH : String_Access := Getenv ("LD_LIBRARY_PATH");
   PATH            : String_Access := Getenv ("PATH");

   -------------------
   -- Split_Command --
   -------------------

   procedure Split_Command is
   begin
      K := Actual_Name'Last;
      loop
         exit when K = 0
           or else Actual_Name (K) = '\' or else Actual_Name (K) = '/';
         K := K - 1;
      end loop;
   end Split_Command;

begin
   Split_Command;

   if K = 0 then
      --  No path information found, locate the program on the path.
      declare
         Old : String_Access := Actual_Name;
      begin
         Actual_Name := Locate_Exec_On_Path (Actual_Name.all);
         Free (Old);

         Split_Command;
      end;
   end if;

   --  Skip 'bin' from directory above. GNAT binaries are always under
   --  <gnatpro>/bin directory.

   L := K - 4;

   declare
      Prog   : constant String := Actual_Name (K + 1 .. Actual_Name'Last);
      Dir    : constant String := Actual_Name (Actual_Name'First .. L - 1);
      Real   : constant String := Dir & DS & ".bin";
      Bin    : constant String := Dir & DS & "bin";
      Args   : Argument_List (1 .. Argument_Count);
      Result : Integer;

   begin
      Setenv ("GCC_ROOT", Dir);
      Setenv ("GNAT_ROOT", Dir);
      Setenv ("BINUTILS_ROOT", Dir);
      Setenv ("LD_LIBRARY_PATH", Dir & DS & "lib" & PS & LD_LIBRARY_PATH.all);
      Setenv ("PATH", Real & PS & Bin & PS & PATH.all);

      --  Call the right executable under "<dir>/.bin"

      for K in 1 .. Argument_Count loop
         Args (K) := new String'(Argument (K));
      end loop;

      Normalize_Arguments (Args);
      Result := Spawn (Real & DS & Prog, Args);

      for K in 1 .. Argument_Count loop
         Free (Args (K));
      end loop;

      OS_Exit (Result);
   end;
end GNAT_Wrapper;
