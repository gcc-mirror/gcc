------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M L I B . P R J                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 2001, Ada Core Technologies, Inc.             --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;   use GNAT.OS_Lib;
with MLib.Fil;
with MLib.Tgt;
with Opt;
with Output;        use Output;
with Osint;         use Osint;
with Namet;         use Namet;
with Table;
with Types;         use Types;

package body MLib.Prj is

   package Files  renames MLib.Fil;
   package Target renames MLib.Tgt;

   --  List of objects to put inside the library

   Object_Files : Argument_List_Access;
   package Objects is new Table.Table
     (Table_Name           => "Mlib.Prj.Objects",
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50);

   --  List of non-Ada object files

   Foreign_Objects : Argument_List_Access;
   package Foreigns is new Table.Table
     (Table_Name           => "Mlib.Prj.Foreigns",
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 20);

   --  List of ALI files

   Ali_Files : Argument_List_Access;
   package Alis is new Table.Table
     (Table_Name           => "Mlib.Prj.Alis",
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50);

   --  List of options set in the command line.

   Options : Argument_List_Access;
   package Opts is new Table.Table
     (Table_Name           => "Mlib.Prj.Opts",
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 5);

   type Build_Mode_State is
     (None, Static, Dynamic, Relocatable);

   procedure Check (Filename : String);
   --  Check if filename is a regular file. Fail if it is not.

   procedure Check_Context;
   --  Check each object files in table Object_Files
   --  Fail if any of them is not a regular file

   procedure Reset_Tables;
   --  Make sure that all the above tables are empty
   --  (Objects, Foreign_Objects, Ali_Files, Options)

   -------------------
   -- Build_Library --
   -------------------

   procedure Build_Library (For_Project : Project_Id) is
      Data : constant Project_Data := Projects.Table (For_Project);

      Project_Name : constant String :=
                       Get_Name_String (Data.Name);

      Lib_Filename : String_Access;
      Lib_Dirpath  : String_Access := new String'(".");
      DLL_Address  : String_Access := new String'(Target.Default_DLL_Address);
      Lib_Version  : String_Access := new String'("");

      The_Build_Mode : Build_Mode_State := None;

   begin
      Reset_Tables;

      --  Fail if project is not a library project

      if not Data.Library then
         Fail ("project """, Project_Name, """ has no library");
      end if;

      Lib_Dirpath := new String'(Get_Name_String (Data.Library_Dir));
      Lib_Filename := new String'(Get_Name_String (Data.Library_Name));

      case Data.Library_Kind is
         when Static =>
            The_Build_Mode := Static;

         when Dynamic =>
            The_Build_Mode := Dynamic;

         when Relocatable =>
            The_Build_Mode := Relocatable;

            if Target.PIC_Option /= "" then
               Opts.Increment_Last;
               Opts.Table (Opts.Last) := new String'(Target.PIC_Option);
            end if;
      end case;

      --  Get the library version, if any

      if Data.Lib_Internal_Name /= No_Name then
         Lib_Version := new String'(Get_Name_String (Data.Lib_Internal_Name));
      end if;

      --  Add the objects found in the object directory

      declare
         Object_Dir : Dir_Type;
         Filename : String (1 .. 255);
         Last : Natural;
         Object_Dir_Path : constant String :=
           Get_Name_String (Data.Object_Directory);
      begin
         Open (Dir => Object_Dir, Dir_Name => Object_Dir_Path);

         --  For all entries in the object directory

         loop
            Read (Object_Dir, Filename, Last);

            exit when Last = 0;

            --  Check if it is an object file

            if Files.Is_Obj (Filename (1 .. Last)) then
               --  record this object file

               Objects.Increment_Last;
               Objects.Table (Objects.Last) :=
                 new String' (Object_Dir_Path & Directory_Separator &
                              Filename (1 .. Last));

               if Is_Regular_File
                 (Object_Dir_Path &
                  Files.Ext_To (Object_Dir_Path &
                                Filename (1 .. Last), "ali"))
               then
                  --  Record the corresponding ali file

                  Alis.Increment_Last;
                  Alis.Table (Alis.Last) :=
                    new String' (Object_Dir_Path &
                                 Files.Ext_To
                                 (Filename (1 .. Last), "ali"));

               else
                  --  The object file is a foreign object file

                  Foreigns.Increment_Last;
                  Foreigns.Table (Foreigns.Last) :=
                    new String'(Object_Dir_Path &
                                Filename (1 .. Last));

               end if;
            end if;
         end loop;

         Close (Dir => Object_Dir);

      exception
         when Directory_Error =>
            Fail ("cannot find object directory """,
                  Get_Name_String (Data.Object_Directory),
                  """");
      end;

      --  We want to link some Ada files, so we need to link with
      --  the GNAT runtime (libgnat & libgnarl)

      if The_Build_Mode = Dynamic or else The_Build_Mode = Relocatable then
         Opts.Increment_Last;
         Opts.Table (Opts.Last) := new String' ("-lgnarl");
         Opts.Increment_Last;
         Opts.Table (Opts.Last) := new String' ("-lgnat");
      end if;

      Object_Files :=
        new Argument_List'(Argument_List (Objects.Table (1 .. Objects.Last)));

      Foreign_Objects :=
        new Argument_List'(Argument_List
                           (Foreigns.Table (1 .. Foreigns.Last)));

      Ali_Files :=
        new Argument_List'(Argument_List (Alis.Table (1 .. Alis.Last)));

      Options :=
        new Argument_List'(Argument_List (Opts.Table (1 .. Opts.Last)));

      --  We fail if there are no object to put in the library
      --  (Ada or foreign objects)

      if Object_Files'Length = 0 then
         Fail ("no object files");

      end if;

      if not Opt.Quiet_Output then
         Write_Eol;
         Write_Str  ("building ");
         Write_Str (Ada.Characters.Handling.To_Lower
                    (Build_Mode_State'Image (The_Build_Mode)));
         Write_Str  (" library for project ");
         Write_Line (Project_Name);
         Write_Eol;
      end if;

      --  We check that all object files are regular files

      Check_Context;

      --  And we call the procedure to build the library,
      --  depending on the build mode

      case The_Build_Mode is
         when Dynamic | Relocatable =>
            Target.Build_Dynamic_Library
              (Ofiles        => Object_Files.all,
               Foreign       => Foreign_Objects.all,
               Afiles        => Ali_Files.all,
               Options       => Options.all,
               Lib_Filename  => Lib_Filename.all,
               Lib_Dir       => Lib_Dirpath.all,
               Lib_Address   => DLL_Address.all,
               Lib_Version   => Lib_Version.all,
               Relocatable   => The_Build_Mode = Relocatable);

         when Static =>
            MLib.Build_Library
              (Object_Files.all,
               Ali_Files.all,
               Lib_Filename.all,
               Lib_Dirpath.all);

         when None =>
            null;
      end case;

      --  We need to copy the ALI files from the object directory
      --  to the library directory, so that the linker find them
      --  there, and does not need to look in the object directory
      --  where it would also find the object files; and we don't want
      --  that: we want the linker to use the library.

      Target.Copy_ALI_Files
        (From => Projects.Table (For_Project).Object_Directory,
         To   => Projects.Table (For_Project).Library_Dir);

   end Build_Library;

   -----------
   -- Check --
   -----------

   procedure Check (Filename : String) is
   begin
      if not Is_Regular_File (Filename) then
         Fail (Filename, " not found.");

      end if;
   end Check;

   -------------------
   -- Check_Context --
   -------------------

   procedure Check_Context is
   begin
      --  check that each object file exist

      for F in Object_Files'Range loop
         Check (Object_Files (F).all);
      end loop;
   end Check_Context;

   ------------------
   -- Reset_Tables --
   ------------------

   procedure Reset_Tables is
   begin
      Objects.Init;
      Foreigns.Init;
      Alis.Init;
      Opts.Init;
   end Reset_Tables;

end MLib.Prj;
