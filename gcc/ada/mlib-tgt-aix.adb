------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     M L I B . T G T . S P E C I F I C                    --
--                              (AIX Version)                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2003-2007, AdaCore                     --
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

--  This is the AIX version of the body

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with MLib.Fil;
with MLib.Utl;
with Opt;
with Output;   use Output;
with Prj.Com;
with Prj.Util; use Prj.Util;

package body MLib.Tgt.Specific is

   --  Non default subprograms

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Version  : String  := "";
      Auto_Init    : Boolean := False);

   function DLL_Ext return String;

   function Library_Major_Minor_Id_Supported return Boolean;

   function Support_For_Libraries return Library_Support;

   --  Local variables

   No_Arguments        : aliased Argument_List         := (1 .. 0 => null);
   Empty_Argument_List : constant Argument_List_Access := No_Arguments'Access;

   Bexpall : aliased String := "-Wl,-bexpall";
   Bexpall_Option : constant String_Access := Bexpall'Access;
   --  The switch to export all symbols

   Lpthreads : aliased String := "-lpthreads";
   Native_Thread_Options : aliased Argument_List := (1 => Lpthreads'Access);
   --  The switch to use when linking a library against libgnarl when using
   --  Native threads.

   Lgthreads : aliased String := "-lgthreads";
   Lmalloc   : aliased String := "-lmalloc";
   FSU_Thread_Options : aliased Argument_List :=
                          (1 => Lgthreads'Access, 2 => Lmalloc'Access);
   --  The switches to use when linking a library against libgnarl when using
   --  FSU threads.

   Thread_Options : Argument_List_Access := Empty_Argument_List;
   --  Designate the thread switches to used when linking a library against
   --  libgnarl. Depends on the thread library (Native or FSU). Resolved for
   --  the first library linked against libgnarl.

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Version  : String  := "";
      Auto_Init    : Boolean := False)
   is
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Lib_Version);
      pragma Unreferenced (Auto_Init);

      Lib_File : constant String :=
                   Lib_Dir & Directory_Separator & "lib" &
                   MLib.Fil.Append_To (Lib_Filename, DLL_Ext);
      --  The file name of the library

      Thread_Opts : Argument_List_Access := Empty_Argument_List;
      --  Set to Thread_Options if -lgnarl is found in the Options

   begin
      if Opt.Verbose_Mode then
         Write_Str ("building relocatable shared library ");
         Write_Line (Lib_File);
      end if;

      --  Look for -lgnarl in Options. If found, set the thread options

      for J in Options'Range loop
         if Options (J).all = "-lgnarl" then

            --  If Thread_Options is null, read s-osinte.ads to discover the
            --  thread library and set Thread_Options accordingly.

            if Thread_Options = null then
               declare
                  File : Text_File;
                  Line : String (1 .. 100);
                  Last : Natural;

               begin
                  Open
                    (File, Include_Dir_Default_Prefix & "/s-osinte.ads");

                  while not End_Of_File (File) loop
                     Get_Line (File, Line, Last);

                     if Index (Line (1 .. Last), "-lpthreads") /= 0 then
                        Thread_Options := Native_Thread_Options'Access;
                        exit;

                     elsif Index (Line (1 .. Last), "-lgthreads") /= 0 then
                        Thread_Options := FSU_Thread_Options'Access;
                        exit;
                     end if;
                  end loop;

                  Close (File);

                  if Thread_Options = null then
                     Prj.Com.Fail ("cannot find the thread library in use");
                  end if;

               exception
                  when others =>
                     Prj.Com.Fail ("cannot open s-osinte.ads");
               end;
            end if;

            Thread_Opts := Thread_Options;
            exit;
         end if;
      end loop;

      --  Finally, call GCC (or the driver specified) to build the library

      MLib.Utl.Gcc
        (Output_File => Lib_File,
         Objects     => Ofiles,
         Options     => Options & Bexpall_Option,
         Driver_Name => Driver_Name,
         Options_2   => Thread_Opts.all);
   end Build_Dynamic_Library;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "a";
   end DLL_Ext;

   --------------------------------------
   -- Library_Major_Minor_Id_Supported --
   --------------------------------------

   function Library_Major_Minor_Id_Supported return Boolean is
   begin
      return False;
   end Library_Major_Minor_Id_Supported;

   ---------------------------
   -- Support_For_Libraries --
   ---------------------------

   function Support_For_Libraries return Library_Support is
   begin
      return Static_Only;
   end Support_For_Libraries;

begin
   Build_Dynamic_Library_Ptr := Build_Dynamic_Library'Access;
   DLL_Ext_Ptr := DLL_Ext'Access;
   Library_Major_Minor_Id_Supported_Ptr :=
                                Library_Major_Minor_Id_Supported'Access;
   Support_For_Libraries_Ptr := Support_For_Libraries'Access;

end MLib.Tgt.Specific;
