------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              M A K E U T L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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

with ALI;      use ALI;
with Debug;
with Err_Vars; use Err_Vars;
with Errutil;
with Fname;
with Hostparm;
with Osint;    use Osint;
with Output;   use Output;
with Opt;      use Opt;
with Prj.Ext;
with Prj.Util;
with Sinput.P;
with Snames;   use Snames;
with Table;
with Tempdir;

with Ada.Command_Line; use Ada.Command_Line;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.HTable;
with GNAT.Regexp;               use GNAT.Regexp;

package body Makeutl is

   type Linker_Options_Data is record
      Project : Project_Id;
      Options : String_List_Id;
   end record;

   Linker_Option_Initial_Count : constant := 20;

   Linker_Options_Buffer : String_List_Access :=
     new String_List (1 .. Linker_Option_Initial_Count);

   Last_Linker_Option : Natural := 0;

   package Linker_Opts is new Table.Table (
     Table_Component_Type => Linker_Options_Data,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Make.Linker_Opts");

   procedure Add_Linker_Option (Option : String);

   ---------
   -- Add --
   ---------

   procedure Add
     (Option : String_Access;
      To     : in out String_List_Access;
      Last   : in out Natural)
   is
   begin
      if Last = To'Last then
         declare
            New_Options : constant String_List_Access :=
                            new String_List (1 .. To'Last * 2);

         begin
            New_Options (To'Range) := To.all;

            --  Set all elements of the original options to null to avoid
            --  deallocation of copies.

            To.all := (others => null);

            Free (To);
            To := New_Options;
         end;
      end if;

      Last := Last + 1;
      To (Last) := Option;
   end Add;

   procedure Add
     (Option : String;
      To     : in out String_List_Access;
      Last   : in out Natural)
   is
   begin
      Add (Option => new String'(Option), To => To, Last => Last);
   end Add;

   -----------------------
   -- Add_Linker_Option --
   -----------------------

   procedure Add_Linker_Option (Option : String) is
   begin
      if Option'Length > 0 then
         if Last_Linker_Option = Linker_Options_Buffer'Last then
            declare
               New_Buffer : constant String_List_Access :=
                              new String_List
                                (1 .. Linker_Options_Buffer'Last +
                                        Linker_Option_Initial_Count);
            begin
               New_Buffer (Linker_Options_Buffer'Range) :=
                 Linker_Options_Buffer.all;
               Linker_Options_Buffer.all := (others => null);
               Free (Linker_Options_Buffer);
               Linker_Options_Buffer := New_Buffer;
            end;
         end if;

         Last_Linker_Option := Last_Linker_Option + 1;
         Linker_Options_Buffer (Last_Linker_Option) := new String'(Option);
      end if;
   end Add_Linker_Option;

   -------------------------
   -- Base_Name_Index_For --
   -------------------------

   function Base_Name_Index_For
     (Main            : String;
      Main_Index      : Int;
      Index_Separator : Character) return File_Name_Type
   is
      Result : File_Name_Type;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Base_Name (Main));

      --  Remove the extension, if any, that is the last part of the base name
      --  starting with a dot and following some characters.

      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Name_Len := J - 1;
            exit;
         end if;
      end loop;

      --  Add the index info, if index is different from 0

      if Main_Index > 0 then
         Add_Char_To_Name_Buffer (Index_Separator);

         declare
            Img : constant String := Main_Index'Img;
         begin
            Add_Str_To_Name_Buffer (Img (2 .. Img'Last));
         end;
      end if;

      Result := Name_Find;
      return Result;
   end Base_Name_Index_For;

   ------------------------------
   -- Check_Source_Info_In_ALI --
   ------------------------------

   function Check_Source_Info_In_ALI
     (The_ALI : ALI_Id;
      Tree    : Project_Tree_Ref) return Boolean
   is
      Unit_Name : Name_Id;

   begin
      --  Loop through units

      for U in ALIs.Table (The_ALI).First_Unit ..
               ALIs.Table (The_ALI).Last_Unit
      loop
         --  Check if the file name is one of the source of the unit

         Get_Name_String (Units.Table (U).Uname);
         Name_Len  := Name_Len - 2;
         Unit_Name := Name_Find;

         if File_Not_A_Source_Of (Tree, Unit_Name, Units.Table (U).Sfile) then
            return False;
         end if;

         --  Loop to do same check for each of the withed units

         for W in Units.Table (U).First_With .. Units.Table (U).Last_With loop
            declare
               WR : ALI.With_Record renames Withs.Table (W);

            begin
               if WR.Sfile /= No_File then
                  Get_Name_String (WR.Uname);
                  Name_Len  := Name_Len - 2;
                  Unit_Name := Name_Find;

                  if File_Not_A_Source_Of (Tree, Unit_Name, WR.Sfile) then
                     return False;
                  end if;
               end if;
            end;
         end loop;
      end loop;

      --  Loop to check subunits and replaced sources

      for D in ALIs.Table (The_ALI).First_Sdep ..
               ALIs.Table (The_ALI).Last_Sdep
      loop
         declare
            SD : Sdep_Record renames Sdep.Table (D);

         begin
            Unit_Name := SD.Subunit_Name;

            if Unit_Name = No_Name then
               --  Check if this source file has been replaced by a source with
               --  a different file name.

               if Tree /= null and then Tree.Replaced_Source_Number > 0 then
                  declare
                     Replacement : constant File_Name_Type :=
                       Replaced_Source_HTable.Get
                         (Tree.Replaced_Sources, SD.Sfile);

                  begin
                     if Replacement /= No_File then
                        if Verbose_Mode then
                           Write_Line
                             ("source file" &
                              Get_Name_String (SD.Sfile) &
                              " has been replaced by " &
                              Get_Name_String (Replacement));
                        end if;

                        return False;
                     end if;
                  end;
               end if;

            else
               --  For separates, the file is no longer associated with the
               --  unit ("proc-sep.adb" is not associated with unit "proc.sep")
               --  so we need to check whether the source file still exists in
               --  the source tree: it will if it matches the naming scheme
               --  (and then will be for the same unit).

               if Find_Source
                    (In_Tree   => Tree,
                     Project   => No_Project,
                     Base_Name => SD.Sfile) = No_Source
               then
                  --  If this is not a runtime file or if, when gnatmake switch
                  --  -a is used, we are not able to find this subunit in the
                  --  source directories, then recompilation is needed.

                  if not Fname.Is_Internal_File_Name (SD.Sfile)
                    or else
                      (Check_Readonly_Files
                        and then Full_Source_Name (SD.Sfile) = No_File)
                  then
                     if Verbose_Mode then
                        Write_Line
                          ("While parsing ALI file, file "
                           & Get_Name_String (SD.Sfile)
                           & " is indicated as containing subunit "
                           & Get_Name_String (Unit_Name)
                           & " but this does not match what was found while"
                           & " parsing the project. Will recompile");
                     end if;

                     return False;
                  end if;
               end if;
            end if;
         end;
      end loop;

      return True;
   end Check_Source_Info_In_ALI;

   --------------------------------
   -- Create_Binder_Mapping_File --
   --------------------------------

   function Create_Binder_Mapping_File
     (Project_Tree : Project_Tree_Ref) return Path_Name_Type
   is
      Mapping_Path : Path_Name_Type := No_Path;

      Mapping_FD : File_Descriptor := Invalid_FD;
      --  A File Descriptor for an eventual mapping file

      ALI_Unit : Unit_Name_Type := No_Unit_Name;
      --  The unit name of an ALI file

      ALI_Name : File_Name_Type := No_File;
      --  The file name of the ALI file

      ALI_Project : Project_Id := No_Project;
      --  The project of the ALI file

      Bytes : Integer;
      OK    : Boolean := False;
      Unit  : Unit_Index;

      Status : Boolean;
      --  For call to Close

   begin
      Tempdir.Create_Temp_File (Mapping_FD, Mapping_Path);
      Record_Temp_File (Project_Tree.Shared, Mapping_Path);

      if Mapping_FD /= Invalid_FD then
         OK := True;

         --  Traverse all units

         Unit := Units_Htable.Get_First (Project_Tree.Units_HT);
         while Unit /= No_Unit_Index loop
            if Unit.Name /= No_Name then

               --  If there is a body, put it in the mapping

               if Unit.File_Names (Impl) /= No_Source
                 and then Unit.File_Names (Impl).Project /= No_Project
               then
                  Get_Name_String (Unit.Name);
                  Add_Str_To_Name_Buffer ("%b");
                  ALI_Unit := Name_Find;
                  ALI_Name :=
                    Lib_File_Name (Unit.File_Names (Impl).Display_File);
                  ALI_Project := Unit.File_Names (Impl).Project;

                  --  Otherwise, if there is a spec, put it in the mapping

               elsif Unit.File_Names (Spec) /= No_Source
                 and then Unit.File_Names (Spec).Project /= No_Project
               then
                  Get_Name_String (Unit.Name);
                  Add_Str_To_Name_Buffer ("%s");
                  ALI_Unit := Name_Find;
                  ALI_Name :=
                    Lib_File_Name (Unit.File_Names (Spec).Display_File);
                  ALI_Project := Unit.File_Names (Spec).Project;

               else
                  ALI_Name := No_File;
               end if;

               --  If we have something to put in the mapping then do it now.
               --  However, if the project is extended, we don't put anything
               --  in the mapping file, since we don't know where the ALI file
               --  is: it might be in the extended project object directory as
               --  well as in the extending project object directory.

               if ALI_Name /= No_File
                 and then ALI_Project.Extended_By = No_Project
                 and then ALI_Project.Extends = No_Project
               then
                  --  First check if the ALI file exists. If it does not, do
                  --  not put the unit in the mapping file.

                  declare
                     ALI : constant String := Get_Name_String (ALI_Name);

                  begin
                     --  For library projects, use the library ALI directory,
                     --  for other projects, use the object directory.

                     if ALI_Project.Library then
                        Get_Name_String
                          (ALI_Project.Library_ALI_Dir.Display_Name);
                     else
                        Get_Name_String
                          (ALI_Project.Object_Directory.Display_Name);
                     end if;

                     Add_Str_To_Name_Buffer (ALI);
                     Add_Char_To_Name_Buffer (ASCII.LF);

                     declare
                        ALI_Path_Name : constant String :=
                                          Name_Buffer (1 .. Name_Len);

                     begin
                        if Is_Regular_File
                             (ALI_Path_Name (1 .. ALI_Path_Name'Last - 1))
                        then
                           --  First line is the unit name

                           Get_Name_String (ALI_Unit);
                           Add_Char_To_Name_Buffer (ASCII.LF);
                           Bytes :=
                             Write
                               (Mapping_FD,
                                Name_Buffer (1)'Address,
                                Name_Len);
                           OK := Bytes = Name_Len;

                           exit when not OK;

                           --  Second line it the ALI file name

                           Get_Name_String (ALI_Name);
                           Add_Char_To_Name_Buffer (ASCII.LF);
                           Bytes :=
                             Write
                               (Mapping_FD,
                                Name_Buffer (1)'Address,
                                Name_Len);
                           OK := (Bytes = Name_Len);

                           exit when not OK;

                           --  Third line it the ALI path name

                           Bytes :=
                             Write
                               (Mapping_FD,
                                ALI_Path_Name (1)'Address,
                                ALI_Path_Name'Length);
                           OK := (Bytes = ALI_Path_Name'Length);

                           --  If OK is False, it means we were unable to
                           --  write a line. No point in continuing with the
                           --  other units.

                           exit when not OK;
                        end if;
                     end;
                  end;
               end if;
            end if;

            Unit := Units_Htable.Get_Next (Project_Tree.Units_HT);
         end loop;

         Close (Mapping_FD, Status);

         OK := OK and Status;
      end if;

      --  If the creation of the mapping file was successful, we add the switch
      --  to the arguments of gnatbind.

      if OK then
         return Mapping_Path;

      else
         return No_Path;
      end if;
   end Create_Binder_Mapping_File;

   -----------------
   -- Create_Name --
   -----------------

   function Create_Name (Name : String) return File_Name_Type is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   function Create_Name (Name : String) return Name_Id is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   function Create_Name (Name : String) return Path_Name_Type is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   ----------------------------
   -- Executable_Prefix_Path --
   ----------------------------

   function Executable_Prefix_Path return String is
      Exec_Name : constant String := Command_Name;

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceded by the absolute or relative path,
      --  e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory where "bin"
      --  lies (in the example "C:\usr"). If the executable is not in a "bin"
      --  directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  := S;
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Exec (J) = Directory_Separator then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                     and then Exec (Path_Last - 3) /= Directory_Separator)
         then
            return "";
         end if;

         return Normalize_Pathname
                  (Exec (Exec'First .. Path_Last - 4),
                   Resolve_Links => Opt.Follow_Links_For_Dirs)
           & Directory_Separator;
      end Get_Install_Dir;

   --  Beginning of Executable_Prefix_Path

   begin
      --  For VMS, the path returned is always /gnu/

      if Hostparm.OpenVMS then
         return "/gnu/";
      end if;

      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Exec_Name (J) = Directory_Separator then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If we get here, the user has typed the executable name with no
      --  directory prefix.

      declare
         Path : String_Access := Locate_Exec_On_Path (Exec_Name);
      begin
         if Path = null then
            return "";
         else
            declare
               Dir : constant String := Get_Install_Dir (Path.all);
            begin
               Free (Path);
               return Dir;
            end;
         end if;
      end;
   end Executable_Prefix_Path;

   ------------------
   -- Fail_Program --
   ------------------

   procedure Fail_Program
     (Project_Tree   : Project_Tree_Ref;
      S              : String;
      Flush_Messages : Boolean := True)
   is
   begin
      if Flush_Messages then
         if Total_Errors_Detected /= 0 or else Warnings_Detected /= 0 then
            Errutil.Finalize;
         end if;
      end if;

      Finish_Program (Project_Tree, E_Fatal, S => S);
   end Fail_Program;

   --------------------
   -- Finish_Program --
   --------------------

   procedure Finish_Program
     (Project_Tree : Project_Tree_Ref;
      Exit_Code    : Osint.Exit_Code_Type := Osint.E_Success;
      S            : String := "")
   is
   begin
      if not Debug.Debug_Flag_N then
         Delete_Temp_Config_Files (Project_Tree);

         if Project_Tree /= null then
            Delete_All_Temp_Files (Project_Tree.Shared);
         end if;
      end if;

      if S'Length > 0 then
         if Exit_Code /= E_Success then
            Osint.Fail (S);
         else
            Write_Str (S);
         end if;
      end if;

      --  Output Namet statistics

      Namet.Finalize;

      Exit_Program (Exit_Code);
   end Finish_Program;

   --------------------------
   -- File_Not_A_Source_Of --
   --------------------------

   function File_Not_A_Source_Of
     (Project_Tree : Project_Tree_Ref;
      Uname        : Name_Id;
      Sfile        : File_Name_Type) return Boolean
   is
      Unit : constant Unit_Index :=
               Units_Htable.Get (Project_Tree.Units_HT, Uname);

      At_Least_One_File : Boolean := False;

   begin
      if Unit /= No_Unit_Index then
         for F in Unit.File_Names'Range loop
            if Unit.File_Names (F) /= null then
               At_Least_One_File := True;
               if Unit.File_Names (F).File = Sfile then
                  return False;
               end if;
            end if;
         end loop;

         if not At_Least_One_File then

            --  The unit was probably created initially for a separate unit
            --  (which are initially created as IMPL when both suffixes are the
            --  same). Later on, Override_Kind changed the type of the file,
            --  and the unit is no longer valid in fact.

            return False;
         end if;

         Verbose_Msg (Uname, "sources do not include ", Name_Id (Sfile));
         return True;
      end if;

      return False;
   end File_Not_A_Source_Of;

   ------------------
   -- Get_Switches --
   ------------------

   procedure Get_Switches
     (Source       : Prj.Source_Id;
      Pkg_Name     : Name_Id;
      Project_Tree : Project_Tree_Ref;
      Value        : out Variable_Value;
      Is_Default   : out Boolean)
   is
   begin
      Get_Switches
        (Source_File  => Source.File,
         Source_Lang  => Source.Language.Name,
         Source_Prj   => Source.Project,
         Pkg_Name     => Pkg_Name,
         Project_Tree => Project_Tree,
         Value        => Value,
         Is_Default   => Is_Default);
   end Get_Switches;

   ------------------
   -- Get_Switches --
   ------------------

   procedure Get_Switches
     (Source_File         : File_Name_Type;
      Source_Lang         : Name_Id;
      Source_Prj          : Project_Id;
      Pkg_Name            : Name_Id;
      Project_Tree        : Project_Tree_Ref;
      Value               : out Variable_Value;
      Is_Default          : out Boolean;
      Test_Without_Suffix : Boolean := False;
      Check_ALI_Suffix    : Boolean := False)
   is
      Project : constant Project_Id :=
                  Ultimate_Extending_Project_Of (Source_Prj);
      Pkg     : constant Package_Id :=
                  Prj.Util.Value_Of
                    (Name        => Pkg_Name,
                     In_Packages => Project.Decl.Packages,
                     Shared      => Project_Tree.Shared);
      Lang : Language_Ptr;

   begin
      Is_Default := False;

      if Source_File /= No_File then
         Value := Prj.Util.Value_Of
           (Name                    => Name_Id (Source_File),
            Attribute_Or_Array_Name => Name_Switches,
            In_Package              => Pkg,
            Shared                  => Project_Tree.Shared,
            Allow_Wildcards         => True);
      end if;

      if Value = Nil_Variable_Value
        and then Test_Without_Suffix
      then
         Lang :=
           Get_Language_From_Name (Project, Get_Name_String (Source_Lang));

         if Lang /= null then
            declare
               Naming      : Lang_Naming_Data renames Lang.Config.Naming_Data;
               SF_Name     : constant String := Get_Name_String (Source_File);
               Last        : Positive := SF_Name'Length;
               Name        : String (1 .. Last + 3);
               Spec_Suffix : String   := Get_Name_String (Naming.Spec_Suffix);
               Body_Suffix : String   := Get_Name_String (Naming.Body_Suffix);
               Truncated   : Boolean  := False;

            begin
               Canonical_Case_File_Name (Spec_Suffix);
               Canonical_Case_File_Name (Body_Suffix);
               Name (1 .. Last) := SF_Name;

               if Last > Body_Suffix'Length
                 and then Name (Last - Body_Suffix'Length + 1 .. Last) =
                   Body_Suffix
               then
                  Truncated := True;
                  Last := Last - Body_Suffix'Length;
               end if;

               if not Truncated
                 and then Last > Spec_Suffix'Length
                 and then Name (Last - Spec_Suffix'Length + 1 .. Last) =
                   Spec_Suffix
               then
                  Truncated := True;
                  Last := Last - Spec_Suffix'Length;
               end if;

               if Truncated then
                  Name_Len := 0;
                  Add_Str_To_Name_Buffer (Name (1 .. Last));

                  Value := Prj.Util.Value_Of
                    (Name                    => Name_Find,
                     Attribute_Or_Array_Name => Name_Switches,
                     In_Package              => Pkg,
                     Shared                  => Project_Tree.Shared,
                     Allow_Wildcards         => True);
               end if;

               if Value = Nil_Variable_Value
                 and then Check_ALI_Suffix
               then
                  Last := SF_Name'Length;
                  while Name (Last) /= '.' loop
                     Last := Last - 1;
                  end loop;

                  Name_Len := 0;
                  Add_Str_To_Name_Buffer (Name (1 .. Last));
                  Add_Str_To_Name_Buffer ("ali");

                  Value := Prj.Util.Value_Of
                    (Name                    => Name_Find,
                     Attribute_Or_Array_Name => Name_Switches,
                     In_Package              => Pkg,
                     Shared                  => Project_Tree.Shared,
                     Allow_Wildcards         => True);
               end if;
            end;
         end if;
      end if;

      if Value = Nil_Variable_Value then
         Is_Default := True;
         Value :=
           Prj.Util.Value_Of
             (Name                    => Source_Lang,
              Attribute_Or_Array_Name => Name_Switches,
              In_Package              => Pkg,
              Shared                  => Project_Tree.Shared,
              Force_Lower_Case_Index  => True);
      end if;

      if Value = Nil_Variable_Value then
         Value :=
           Prj.Util.Value_Of
             (Name                    => All_Other_Names,
              Attribute_Or_Array_Name => Name_Switches,
              In_Package              => Pkg,
              Shared                  => Project_Tree.Shared,
              Force_Lower_Case_Index  => True);
      end if;

      if Value = Nil_Variable_Value then
         Value :=
           Prj.Util.Value_Of
             (Name                    => Source_Lang,
              Attribute_Or_Array_Name => Name_Default_Switches,
              In_Package              => Pkg,
              Shared                  => Project_Tree.Shared);
      end if;
   end Get_Switches;

   ------------
   -- Inform --
   ------------

   procedure Inform (N : File_Name_Type; Msg : String) is
   begin
      Inform (Name_Id (N), Msg);
   end Inform;

   procedure Inform (N : Name_Id := No_Name; Msg : String) is
   begin
      Osint.Write_Program_Name;

      Write_Str (": ");

      if N /= No_Name then
         Write_Str ("""");

         declare
            Name : constant String := Get_Name_String (N);
         begin
            if Debug.Debug_Flag_F and then Is_Absolute_Path (Name) then
               Write_Str (File_Name (Name));
            else
               Write_Str (Name);
            end if;
         end;

         Write_Str (""" ");
      end if;

      Write_Str (Msg);
      Write_Eol;
   end Inform;

   ------------------------------
   -- Initialize_Source_Record --
   ------------------------------

   procedure Initialize_Source_Record (Source : Prj.Source_Id) is
      procedure Set_Object_Project
        (Obj_Dir : String; Obj_Proj : Project_Id; Obj_Path : Path_Name_Type;
         Stamp   : Time_Stamp_Type);
      --  Update information about object file, switches file,...

      ------------------------
      -- Set_Object_Project --
      ------------------------

      procedure Set_Object_Project
        (Obj_Dir : String; Obj_Proj : Project_Id; Obj_Path : Path_Name_Type;
         Stamp   : Time_Stamp_Type) is
      begin
         Source.Object_Project := Obj_Proj;
         Source.Object_Path    := Obj_Path;
         Source.Object_TS      := Stamp;

         if Source.Language.Config.Dependency_Kind /= None then
            declare
               Dep_Path : constant String :=
                 Normalize_Pathname
                   (Name          => Get_Name_String (Source.Dep_Name),
                    Resolve_Links => Opt.Follow_Links_For_Files,
                    Directory     => Obj_Dir);
            begin
               Source.Dep_Path := Create_Name (Dep_Path);
               Source.Dep_TS   := Osint.Unknown_Attributes;
            end;
         end if;

         --  Get the path of the switches file, even if Opt.Check_Switches is
         --  not set, as switch -s may be in the Builder switches that have not
         --  been scanned yet.

         declare
            Switches_Path : constant String :=
              Normalize_Pathname
                (Name          => Get_Name_String (Source.Switches),
                 Resolve_Links => Opt.Follow_Links_For_Files,
                 Directory     => Obj_Dir);
         begin
            Source.Switches_Path := Create_Name (Switches_Path);

            if Stamp /= Empty_Time_Stamp then
               Source.Switches_TS := File_Stamp (Source.Switches_Path);
            end if;
         end;
      end Set_Object_Project;

      Obj_Proj : Project_Id;

   begin
      --  Nothing to do if source record has already been fully initialized

      if Source.Initialized then
         return;
      end if;

      --  Systematically recompute the time stamp

      Source.Source_TS := File_Stamp (Source.Path.Display_Name);

      --  Parse the source file to check whether we have a subunit

      if Source.Language.Config.Kind = Unit_Based
        and then Source.Kind = Impl
        and then Is_Subunit (Source)
      then
         Source.Kind := Sep;
      end if;

      if Source.Language.Config.Object_Generated
        and then Is_Compilable (Source)
      then
         --  First, get the correct object file name and dependency file name
         --  if the source is in a multi-unit file.

         if Source.Index /= 0 then
            Source.Object :=
              Object_Name
                (Source_File_Name   => Source.File,
                 Source_Index       => Source.Index,
                 Index_Separator    =>
                   Source.Language.Config.Multi_Unit_Object_Separator,
                 Object_File_Suffix =>
                   Source.Language.Config.Object_File_Suffix);

            Source.Dep_Name :=
              Dependency_Name
                (Source.Object, Source.Language.Config.Dependency_Kind);
         end if;

         --  Find the object file for that source. It could be either in
         --  the current project or in an extended project (it might actually
         --  not exist yet in the ultimate extending project, but if not found
         --  elsewhere that's where we'll expect to find it).

         Obj_Proj := Source.Project;
         while Obj_Proj /= No_Project loop
            declare
               Dir  : constant String := Get_Name_String
                 (Obj_Proj.Object_Directory.Display_Name);

               Object_Path     : constant String :=
                                   Normalize_Pathname
                                     (Name          =>
                                        Get_Name_String (Source.Object),
                                      Resolve_Links =>
                                        Opt.Follow_Links_For_Files,
                                      Directory     => Dir);

               Obj_Path : constant Path_Name_Type := Create_Name (Object_Path);
               Stamp : Time_Stamp_Type := Empty_Time_Stamp;

            begin
               --  For specs, we do not check object files if there is a body.
               --  This saves a system call. On the other hand, we do need to
               --  know the object_path, in case the user has passed the .ads
               --  on the command line to compile the spec only

               if Source.Kind /= Spec
                 or else Source.Unit = No_Unit_Index
                 or else Source.Unit.File_Names (Impl) = No_Source
               then
                  Stamp := File_Stamp (Obj_Path);
               end if;

               if Stamp /= Empty_Time_Stamp
                 or else (Obj_Proj.Extended_By = No_Project
                          and then Source.Object_Project = No_Project)
               then
                  Set_Object_Project (Dir, Obj_Proj, Obj_Path, Stamp);
               end if;

               Obj_Proj := Obj_Proj.Extended_By;
            end;
         end loop;

      elsif Source.Language.Config.Dependency_Kind = Makefile then
         declare
            Object_Dir : constant String :=
                           Get_Name_String
                             (Source.Project.Object_Directory.Display_Name);
            Dep_Path   : constant String :=
                           Normalize_Pathname
                             (Name        => Get_Name_String (Source.Dep_Name),
                              Resolve_Links =>
                                Opt.Follow_Links_For_Files,
                              Directory     => Object_Dir);
         begin
            Source.Dep_Path := Create_Name (Dep_Path);
            Source.Dep_TS   := Osint.Unknown_Attributes;
         end;
      end if;

      Source.Initialized := True;
   end Initialize_Source_Record;

   ----------------------------
   -- Is_External_Assignment --
   ----------------------------

   function Is_External_Assignment
     (Env  : Prj.Tree.Environment;
      Argv : String) return Boolean
   is
      Start     : Positive := 3;
      Finish    : Natural := Argv'Last;

      pragma Assert (Argv'First = 1);
      pragma Assert (Argv (1 .. 2) = "-X");

   begin
      if Argv'Last < 5 then
         return False;

      elsif Argv (3) = '"' then
         if Argv (Argv'Last) /= '"' or else Argv'Last < 7 then
            return False;
         else
            Start := 4;
            Finish := Argv'Last - 1;
         end if;
      end if;

      return Prj.Ext.Check
        (Self        => Env.External,
         Declaration => Argv (Start .. Finish));
   end Is_External_Assignment;

   ----------------
   -- Is_Subunit --
   ----------------

   function Is_Subunit (Source : Prj.Source_Id) return Boolean is
      Src_Ind : Source_File_Index;
   begin
      if Source.Kind = Sep then
         return True;

      --  A Spec, a file based language source or a body with a spec cannot be
      --  a subunit.

      elsif Source.Kind = Spec or else
        Source.Unit = No_Unit_Index or else
        Other_Part (Source) /= No_Source
      then
         return False;
      end if;

      --  Here, we are assuming that the language is Ada, as it is the only
      --  unit based language that we know.

      Src_Ind :=
        Sinput.P.Load_Project_File
          (Get_Name_String (Source.Path.Display_Name));

      return Sinput.P.Source_File_Is_Subunit (Src_Ind);
   end Is_Subunit;

   -----------------------------
   -- Linker_Options_Switches --
   -----------------------------

   function Linker_Options_Switches
     (Project  : Project_Id;
      Do_Fail  : Fail_Proc;
      In_Tree  : Project_Tree_Ref) return String_List
   is
      procedure Recursive_Add
        (Proj    : Project_Id;
         In_Tree : Project_Tree_Ref;
         Dummy   : in out Boolean);
      --  The recursive routine used to add linker options

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add
        (Proj    : Project_Id;
         In_Tree : Project_Tree_Ref;
         Dummy   : in out Boolean)
      is
         pragma Unreferenced (Dummy);

         Linker_Package : Package_Id;
         Options        : Variable_Value;

      begin
         Linker_Package :=
           Prj.Util.Value_Of
             (Name        => Name_Linker,
              In_Packages => Proj.Decl.Packages,
              Shared      => In_Tree.Shared);

         Options :=
           Prj.Util.Value_Of
             (Name                    => Name_Ada,
              Index                   => 0,
              Attribute_Or_Array_Name => Name_Linker_Options,
              In_Package              => Linker_Package,
              Shared                  => In_Tree.Shared);

         --  If attribute is present, add the project with
         --  the attribute to table Linker_Opts.

         if Options /= Nil_Variable_Value then
            Linker_Opts.Increment_Last;
            Linker_Opts.Table (Linker_Opts.Last) :=
              (Project => Proj, Options => Options.Values);
         end if;
      end Recursive_Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Recursive_Add);

      Dummy : Boolean := False;

   --  Start of processing for Linker_Options_Switches

   begin
      Linker_Opts.Init;

      For_All_Projects (Project, In_Tree, Dummy, Imported_First => True);

      Last_Linker_Option := 0;

      for Index in reverse 1 .. Linker_Opts.Last loop
         declare
            Options : String_List_Id;
            Proj    : constant Project_Id :=
                        Linker_Opts.Table (Index).Project;
            Option  : Name_Id;
            Dir_Path : constant String :=
                         Get_Name_String (Proj.Directory.Name);

         begin
            Options := Linker_Opts.Table (Index).Options;
            while Options /= Nil_String loop
               Option := In_Tree.Shared.String_Elements.Table (Options).Value;
               Get_Name_String (Option);

               --  Do not consider empty linker options

               if Name_Len /= 0 then
                  Add_Linker_Option (Name_Buffer (1 .. Name_Len));

                  --  Object files and -L switches specified with relative
                  --  paths must be converted to absolute paths.

                  Test_If_Relative_Path
                    (Switch  => Linker_Options_Buffer (Last_Linker_Option),
                     Parent  => Dir_Path,
                     Do_Fail => Do_Fail,
                     Including_L_Switch => True);
               end if;

               Options := In_Tree.Shared.String_Elements.Table (Options).Next;
            end loop;
         end;
      end loop;

      return Linker_Options_Buffer (1 .. Last_Linker_Option);
   end Linker_Options_Switches;

   -----------
   -- Mains --
   -----------

   package body Mains is

      package Names is new Table.Table
        (Table_Component_Type => Main_Info,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100,
         Table_Name           => "Makeutl.Mains.Names");
      --  The table that stores the mains

      Current : Natural := 0;
      --  The index of the last main retrieved from the table

      --------------
      -- Add_Main --
      --------------

      procedure Add_Main
        (Name     : String;
         Index    : Int := 0;
         Location : Source_Ptr := No_Location;
         Project  : Project_Id := No_Project;
         Tree     : Project_Tree_Ref := null)
      is
      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

         Names.Increment_Last;
         Names.Table (Names.Last) :=
           (Name_Find, Index, Location, No_Source, Project, Tree);
      end Add_Main;

      --------------------------
      -- Set_Multi_Unit_Index --
      --------------------------

      procedure Set_Multi_Unit_Index
        (Project_Tree : Project_Tree_Ref := null;
         Index        : Int := 0) is
      begin
         if Index /= 0 then
            if Names.Last = 0 then
               Fail_Program
                 (Project_Tree,
                  "cannot specify a multi-unit index but no main " &
                  "on the command line");

            elsif Names.Last > 1 then
               Fail_Program
                 (Project_Tree,
                  "cannot specify several mains with a multi-unit index");

            else
               Names.Table (Names.Last).Index := Index;
            end if;
         end if;
      end Set_Multi_Unit_Index;

      ------------
      -- Delete --
      ------------

      procedure Delete is
      begin
         Names.Set_Last (0);
         Mains.Reset;
      end Delete;

      -----------------------
      -- FIll_From_Project --
      -----------------------

      procedure Fill_From_Project
        (Root_Project : Project_Id;
         Project_Tree : Project_Tree_Ref)
      is
         procedure Add_Mains_From_Project
           (Project : Project_Id; Tree    : Project_Tree_Ref);
         --  Add the main units from this project into Mains

         procedure Add_Mains_From_Project
           (Project : Project_Id;
            Tree    : Project_Tree_Ref)
         is
            List    : String_List_Id;
            Element : String_Element;
            Agg     : Aggregated_Project_List;
         begin
            Debug_Output ("Add_Mains_From_Project", Project.Name);
            case Project.Qualifier is
               when Aggregate =>
                  Agg := Project.Aggregated_Projects;
                  while Agg /= null loop
                     Add_Mains_From_Project (Agg.Project, Agg.Tree);
                     Agg := Agg.Next;
                  end loop;

               when others =>
                  List := Project.Mains;
                  if List /= Prj.Nil_String then
                     --  The attribute Main is not an empty list.
                     --  Get the mains in the list

                     while List /= Prj.Nil_String loop
                        Element := Tree.Shared.String_Elements.Table (List);
                        Debug_Output ("Add_Main", Element.Value);
                        Add_Main (Name     => Get_Name_String (Element.Value),
                                  Index    => Element.Index,
                                  Location => Element.Location,
                                  Project  => Project,
                                  Tree     => Tree);
                        List := Element.Next;
                     end loop;
                  end if;
            end case;
         end Add_Mains_From_Project;

      begin
         if Number_Of_Mains = 0 then
            Add_Mains_From_Project (Root_Project, Project_Tree);
         end if;

         --  If there are mains, check that they are sources of the main
         --  project

         if Mains.Number_Of_Mains > 0 then
            for J in Names.First .. Names.Last loop
               declare
                  File       : Main_Info := Names.Table (J);
                  Main_Id    : File_Name_Type := File.File;
                  Main       : constant String := Get_Name_String (Main_Id);
                  Project    : Project_Id;
                  Source     : Prj.Source_Id := No_Source;
                  Suffix     : File_Name_Type;
                  Iter       : Source_Iterator;

               begin
                  if Base_Name (Main) /= Main then
                     if Is_Absolute_Path (Main) then
                        Main_Id := Create_Name (Base_Name (Main));

                     else
                        Fail_Program
                          (Project_Tree,
                           "mains cannot include directory information (""" &
                           Main & """)");
                     end if;
                  end if;

                  --  If no project or tree was specified for the main, it came
                  --  from the command line. In this case, it needs to belong
                  --  to the root project.
                  --  Note that the assignments below will not modify inside
                  --  the table itself.

                  if File.Project = null then
                     File.Project := Root_Project;
                  end if;

                  if File.Tree = null then
                     File.Tree := Project_Tree;
                  end if;

                  --  First, look for the main as specified.

                  Source := Find_Source
                    (In_Tree   => File.Tree,
                     Project   => File.Project,
                     Base_Name => File.File,
                     Index     => File.Index);

                  if Source = No_Source then
                     --  Now look for the main with a body suffix

                     declare
                        --  Main already has a canonical casing
                        Main : constant String := Get_Name_String (Main_Id);
                     begin
                        Project := File.Project;
                        while Source = No_Source
                          and then Project /= No_Project
                        loop
                           Iter := For_Each_Source (File.Tree, Project);
                           loop
                              Source := Prj.Element (Iter);
                              exit when Source = No_Source;

                              --  Only consider bodies

                              if Source.Kind = Impl then
                                 Get_Name_String (Source.File);

                                 if Name_Len > Main'Length
                                   and then
                                     Name_Buffer (1 .. Main'Length) = Main
                                 then
                                    Suffix :=
                                      Source.Language
                                        .Config.Naming_Data.Body_Suffix;

                                    exit when Suffix /= No_File and then
                                      Name_Buffer (Main'Length + 1 .. Name_Len)
                                      = Get_Name_String (Suffix);
                                 end if;
                              end if;

                              Next (Iter);
                           end loop;

                           Project := Project.Extends;
                        end loop;
                     end;
                  end if;

                  if Source /= No_Source then
                     Names.Table (J).File    := Source.File;
                     Names.Table (J).Project := File.Project;
                     Names.Table (J).Tree    := File.Tree;
                     Names.Table (J).Source  := Source;

                  elsif File.Location /= No_Location then
                     --  If the main is declared in package Builder of the
                     --  main project, report an error. If the main is on
                     --  the command line, it may be a main from another
                     --  project, so do nothing: if the main does not exist
                     --  in another project, an error will be reported
                     --  later.

                     Error_Msg_File_1 := Main_Id;
                     Error_Msg_Name_1 := Root_Project.Name;
                     Errutil.Error_Msg ("{ is not a source of project %%",
                                        File.Location);
                  end if;
               end;
            end loop;
         end if;

         if Total_Errors_Detected > 0 then
            Fail_Program (Project_Tree, "problems with main sources");
         end if;
      end Fill_From_Project;

      ---------------
      -- Next_Main --
      ---------------

      function Next_Main return String is
         Info : Main_Info;
      begin
         Info := Next_Main;
         if Info = No_Main_Info then
            return "";
         else
            return Get_Name_String (Info.File);
         end if;
      end Next_Main;

      ---------------
      -- Next_Main --
      ---------------

      function Next_Main return Main_Info is
      begin
         if Current >= Names.Last then
            return No_Main_Info;
         else
            Current := Current + 1;
            return Names.Table (Current);
         end if;
      end Next_Main;

      ---------------------
      -- Number_Of_Mains --
      ---------------------

      function Number_Of_Mains return Natural is
      begin
         return Names.Last;
      end Number_Of_Mains;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Current := 0;
      end Reset;
   end Mains;

   -----------------------
   -- Path_Or_File_Name --
   -----------------------

   function Path_Or_File_Name (Path : Path_Name_Type) return String is
      Path_Name : constant String := Get_Name_String (Path);
   begin
      if Debug.Debug_Flag_F then
         return File_Name (Path_Name);
      else
         return Path_Name;
      end if;
   end Path_Or_File_Name;

   ---------------------------
   -- Test_If_Relative_Path --
   ---------------------------

   procedure Test_If_Relative_Path
     (Switch               : in out String_Access;
      Parent               : String;
      Do_Fail              : Fail_Proc;
      Including_L_Switch   : Boolean := True;
      Including_Non_Switch : Boolean := True;
      Including_RTS        : Boolean := False)
   is
   begin
      if Switch /= null then
         declare
            Sw    : String (1 .. Switch'Length);
            Start : Positive;

         begin
            Sw := Switch.all;

            if Sw (1) = '-' then
               if Sw'Length >= 3
                 and then (Sw (2) = 'A'
                            or else Sw (2) = 'I'
                            or else (Including_L_Switch and then Sw (2) = 'L'))
               then
                  Start := 3;

                  if Sw = "-I-" then
                     return;
                  end if;

               elsif Sw'Length >= 4
                 and then (Sw (2 .. 3) = "aL"
                            or else Sw (2 .. 3) = "aO"
                            or else Sw (2 .. 3) = "aI")
               then
                  Start := 4;

               elsif Including_RTS
                 and then Sw'Length >= 7
                 and then Sw (2 .. 6) = "-RTS="
               then
                  Start := 7;

               else
                  return;
               end if;

               --  Because relative path arguments to --RTS= may be relative
               --  to the search directory prefix, those relative path
               --  arguments are converted only when they include directory
               --  information.

               if not Is_Absolute_Path (Sw (Start .. Sw'Last)) then
                  if Parent'Length = 0 then
                     Do_Fail
                       ("relative search path switches ("""
                        & Sw
                        & """) are not allowed");

                  elsif Including_RTS then
                     for J in Start .. Sw'Last loop
                        if Sw (J) = Directory_Separator then
                           Switch :=
                             new String'
                               (Sw (1 .. Start - 1) &
                                Parent &
                                Directory_Separator &
                                Sw (Start .. Sw'Last));
                           return;
                        end if;
                     end loop;

                  else
                     Switch :=
                       new String'
                         (Sw (1 .. Start - 1) &
                          Parent &
                          Directory_Separator &
                          Sw (Start .. Sw'Last));
                  end if;
               end if;

            elsif Including_Non_Switch then
               if not Is_Absolute_Path (Sw) then
                  if Parent'Length = 0 then
                     Do_Fail
                       ("relative paths (""" & Sw & """) are not allowed");
                  else
                     Switch := new String'(Parent & Directory_Separator & Sw);
                  end if;
               end if;
            end if;
         end;
      end if;
   end Test_If_Relative_Path;

   -------------------
   -- Unit_Index_Of --
   -------------------

   function Unit_Index_Of (ALI_File : File_Name_Type) return Int is
      Start  : Natural;
      Finish : Natural;
      Result : Int := 0;

   begin
      Get_Name_String (ALI_File);

      --  First, find the last dot

      Finish := Name_Len;

      while Finish >= 1 and then Name_Buffer (Finish) /= '.' loop
         Finish := Finish - 1;
      end loop;

      if Finish = 1 then
         return 0;
      end if;

      --  Now check that the dot is preceded by digits

      Start := Finish;
      Finish := Finish - 1;

      while Start >= 1 and then Name_Buffer (Start - 1) in '0' .. '9' loop
         Start := Start - 1;
      end loop;

      --  If there are no digits, or if the digits are not preceded by the
      --  character that precedes a unit index, this is not the ALI file of
      --  a unit in a multi-unit source.

      if Start > Finish
        or else Start = 1
        or else Name_Buffer (Start - 1) /= Multi_Unit_Index_Character
      then
         return 0;
      end if;

      --  Build the index from the digit(s)

      while Start <= Finish loop
         Result := Result * 10 +
                     Character'Pos (Name_Buffer (Start)) - Character'Pos ('0');
         Start := Start + 1;
      end loop;

      return Result;
   end Unit_Index_Of;

   -----------------
   -- Verbose_Msg --
   -----------------

   procedure Verbose_Msg
     (N1                : Name_Id;
      S1                : String;
      N2                : Name_Id := No_Name;
      S2                : String  := "";
      Prefix            : String := "  -> ";
      Minimum_Verbosity : Opt.Verbosity_Level_Type := Opt.Low)
   is
   begin
      if not Opt.Verbose_Mode
        or else Minimum_Verbosity > Opt.Verbosity_Level
      then
         return;
      end if;

      Write_Str (Prefix);
      Write_Str ("""");
      Write_Name (N1);
      Write_Str (""" ");
      Write_Str (S1);

      if N2 /= No_Name then
         Write_Str (" """);
         Write_Name (N2);
         Write_Str (""" ");
      end if;

      Write_Str (S2);
      Write_Eol;
   end Verbose_Msg;

   -----------------
   -- Verbose_Msg --
   -----------------

   procedure Verbose_Msg
     (N1                : File_Name_Type;
      S1                : String;
      N2                : File_Name_Type := No_File;
      S2                : String  := "";
      Prefix            : String := "  -> ";
      Minimum_Verbosity : Opt.Verbosity_Level_Type := Opt.Low)
   is
   begin
      Verbose_Msg
        (Name_Id (N1), S1, Name_Id (N2), S2, Prefix, Minimum_Verbosity);
   end Verbose_Msg;

   -----------
   -- Queue --
   -----------

   package body Queue is
      type Q_Record is record
         Info      : Source_Info;
         Processed : Boolean;
      end record;

      package Q is new Table.Table
        (Table_Component_Type => Q_Record,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 1000,
         Table_Increment      => 100,
         Table_Name           => "Makeutl.Queue.Q");
      --  This is the actual Queue

      package Busy_Obj_Dirs is new GNAT.HTable.Simple_HTable
        (Header_Num => Prj.Header_Num,
         Element    => Boolean,
         No_Element => False,
         Key        => Path_Name_Type,
         Hash       => Hash,
         Equal      => "=");

      type Mark_Key is record
         File  : File_Name_Type;
         Index : Int;
      end record;
      --  Identify either a mono-unit source (when Index = 0) or a specific
      --  unit (index = 1's origin index of unit) in a multi-unit source.

      Max_Mask_Num : constant := 2048;
      subtype Mark_Num is Union_Id range 0 .. Max_Mask_Num - 1;

      function Hash (Key : Mark_Key) return Mark_Num;

      package Marks is new GNAT.HTable.Simple_HTable
        (Header_Num => Mark_Num,
         Element    => Boolean,
         No_Element => False,
         Key        => Mark_Key,
         Hash       => Hash,
         Equal      => "=");
      --  A hash table to keep tracks of the marked units.
      --  These are the units that have already been processed, when using the
      --  gnatmake format. When using the gprbuild format, we can directly
      --  store in the source_id whether the file has already been processed.

      procedure Mark (Source_File : File_Name_Type; Index : Int := 0);
      --  Mark a unit, identified by its source file and, when Index is not 0,
      --  the index of the unit in the source file. Marking is used to signal
      --  that the unit has already been inserted in the Q.

      function Is_Marked
        (Source_File : File_Name_Type;
         Index       : Int := 0) return Boolean;
      --  Returns True if the unit was previously marked

      Q_Processed           : Natural := 0;
      Q_Initialized         : Boolean := False;

      Q_First               : Natural := 1;
      --  Points to the first valid element in the queue

      One_Queue_Per_Obj_Dir : Boolean := False;
      --  See parameter to Initialize

      function Available_Obj_Dir (S : Source_Info) return Boolean;
      --  Whether the object directory for S is available for a build

      procedure Debug_Display (S : Source_Info);
      --  A debug display for S

      function Was_Processed (S : Source_Info) return Boolean;
      --  Whether S has already been processed. This marks the source as
      --  processed, if it hasn't already been processed.

      function Insert_No_Roots (Source  : Source_Info) return Boolean;
      --  Insert Source, but do not look for its roots (see doc for Insert).

      -------------------
      -- Was_Processed --
      -------------------

      function Was_Processed (S : Source_Info) return Boolean is
      begin
         case S.Format is
            when Format_Gprbuild =>
               if S.Id.In_The_Queue then
                  return True;
               end if;
               S.Id.In_The_Queue := True;

            when Format_Gnatmake =>
               if Is_Marked (S.File, S.Index) then
                  return True;
               end if;
               Mark (S.File, Index => S.Index);
         end case;

         return False;
      end Was_Processed;

      -----------------------
      -- Available_Obj_Dir --
      -----------------------

      function Available_Obj_Dir (S : Source_Info) return Boolean is
      begin
         case S.Format is
            when Format_Gprbuild =>
               return not Busy_Obj_Dirs.Get
                 (S.Id.Project.Object_Directory.Name);

            when Format_Gnatmake =>
               return S.Project = No_Project
                 or else
                   not Busy_Obj_Dirs.Get (S.Project.Object_Directory.Name);
         end case;
      end Available_Obj_Dir;

      -------------------
      -- Debug_Display --
      -------------------

      procedure Debug_Display (S : Source_Info) is
      begin
         case S.Format is
            when Format_Gprbuild =>
               Write_Name (S.Id.File);

               if S.Id.Index /= 0 then
                  Write_Str (", ");
                  Write_Int (S.Id.Index);
               end if;

            when Format_Gnatmake =>
               Write_Name (S.File);

               if S.Index /= 0 then
                  Write_Str (", ");
                  Write_Int (S.Index);
               end if;
         end case;
      end Debug_Display;

      ----------
      -- Hash --
      ----------

      function Hash (Key : Mark_Key) return Mark_Num is
      begin
         return Union_Id (Key.File) mod Max_Mask_Num;
      end Hash;

      ---------------
      -- Is_Marked --
      ---------------

      function Is_Marked
        (Source_File : File_Name_Type;
         Index       : Int := 0) return Boolean is
      begin
         return Marks.Get (K => (File => Source_File, Index => Index));
      end Is_Marked;

      ----------
      -- Mark --
      ----------

      procedure Mark (Source_File : File_Name_Type; Index : Int := 0) is
      begin
         Marks.Set (K => (File => Source_File, Index => Index), E => True);
      end Mark;

      -------------
      -- Extract --
      -------------

      procedure Extract
        (Found  : out Boolean;
         Source : out Source_Info)
      is
      begin
         Found := False;

         if One_Queue_Per_Obj_Dir then
            for J in Q_First .. Q.Last loop
               if not Q.Table (J).Processed
                 and then Available_Obj_Dir (Q.Table (J).Info)
               then
                  Found := True;
                  Source := Q.Table (J).Info;
                  Q.Table (J).Processed := True;

                  if J = Q_First then
                     while Q_First <= Q.Last
                       and then Q.Table (Q_First).Processed
                     loop
                        Q_First := Q_First + 1;
                     end loop;
                  end if;

                  exit;
               end if;
            end loop;

         elsif Q_First <= Q.Last then
            Source := Q.Table (Q_First).Info;
            Q.Table (Q_First).Processed := True;
            Q_First := Q_First + 1;
            Found := True;
         end if;

         if Found then
            Q_Processed := Q_Processed + 1;
         end if;

         if Found and then Debug.Debug_Flag_Q then
            Write_Str ("   Q := Q - [ ");
            Debug_Display (Source);
            Write_Str (" ]");
            Write_Eol;

            Write_Str ("   Q_First =");
            Write_Int (Int (Q_First));
            Write_Eol;

            Write_Str ("   Q.Last =");
            Write_Int (Int (Q.Last));
            Write_Eol;
         end if;
      end Extract;

      ---------------
      -- Processed --
      ---------------

      function Processed return Natural is
      begin
         return Q_Processed;
      end Processed;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Queue_Per_Obj_Dir : Boolean;
         Force             : Boolean := False)
      is
      begin
         if Force or else not Q_Initialized then
            Q_Initialized := True;

            for J in 1 .. Q.Last loop
               case Q.Table (J).Info.Format is
               when Format_Gprbuild =>
                  Q.Table (J).Info.Id.In_The_Queue := False;
               when Format_Gnatmake =>
                  null;
               end case;
            end loop;

            Q.Init;
            Q_Processed := 0;
            Q_First     := 1;
            One_Queue_Per_Obj_Dir := Queue_Per_Obj_Dir;
         end if;
      end Initialize;

      ---------------------
      -- Insert_No_Roots --
      ---------------------

      function Insert_No_Roots (Source  : Source_Info) return Boolean is
      begin
         pragma Assert
           (Source.Format = Format_Gnatmake
            or else Source.Id /= No_Source);

         --  Only insert in the Q if it is not already done, to avoid
         --  simultaneous compilations if -jnnn is used.

         if Was_Processed (Source) then
            return False;
         end if;

         if Current_Verbosity = High then
            Write_Str ("Adding """);
            Debug_Display (Source);
            Write_Line (" to the queue");
         end if;

         Q.Append (New_Val => (Info => Source, Processed => False));

         if Debug.Debug_Flag_Q then
            Write_Str ("   Q := Q + [ ");
            Debug_Display (Source);
            Write_Str (" ] ");
            Write_Eol;

            Write_Str ("   Q_First =");
            Write_Int (Int (Q_First));
            Write_Eol;

            Write_Str ("   Q.Last =");
            Write_Int (Int (Q.Last));
            Write_Eol;
         end if;

         return True;
      end Insert_No_Roots;

      ------------
      -- Insert --
      ------------

      function Insert
        (Source  : Source_Info; With_Roots : Boolean := False) return Boolean
      is
         Root_Arr     : Array_Element_Id;
         Roots        : Variable_Value;
         List         : String_List_Id;
         Elem         : String_Element;
         Unit_Name    : Name_Id;
         Pat_Root     : Boolean;
         Root_Pattern : Regexp;
         Root_Found   : Boolean;
         Roots_Found  : Boolean;
         Dummy        : Boolean;
         Root_Source  : Prj.Source_Id;
         Iter         : Source_Iterator;
         pragma Unreferenced (Dummy);

      begin
         if not Insert_No_Roots (Source) then
            --  Was already in the queue
            return False;
         end if;

         if With_Roots and then Source.Format = Format_Gprbuild then
            Debug_Output ("Looking for roots of", Name_Id (Source.Id.File));

            Root_Arr :=
              Prj.Util.Value_Of
                (Name      => Name_Roots,
                 In_Arrays => Source.Id.Project.Decl.Arrays,
                 Shared    => Source.Tree.Shared);

            Roots :=
              Prj.Util.Value_Of
                (Index     => Name_Id (Source.Id.File),
                 Src_Index => 0,
                 In_Array  => Root_Arr,
                 Shared    => Source.Tree.Shared);

            --  If there is no roots for the specific main, try the language

            if Roots = Nil_Variable_Value then
               Roots :=
                 Prj.Util.Value_Of
                   (Index                  => Source.Id.Language.Name,
                    Src_Index              => 0,
                    In_Array               => Root_Arr,
                    Shared                 => Source.Tree.Shared,
                    Force_Lower_Case_Index => True);
            end if;

            --  Then try "*"

            if Roots = Nil_Variable_Value then
               Name_Len := 1;
               Name_Buffer (1) := '*';

               Roots :=
                 Prj.Util.Value_Of
                   (Index                  => Name_Find,
                    Src_Index              => 0,
                    In_Array               => Root_Arr,
                    Shared                 => Source.Tree.Shared,
                    Force_Lower_Case_Index => True);
            end if;

            if Roots = Nil_Variable_Value then
               Debug_Output ("   -> no roots declared");
            else
               List := Roots.Values;

               Pattern_Loop :
               while List /= Nil_String loop
                  Elem := Source.Tree.Shared.String_Elements.Table (List);
                  Get_Name_String (Elem.Value);
                  To_Lower (Name_Buffer (1 .. Name_Len));
                  Unit_Name := Name_Find;

                  --  Check if it is a unit name or a pattern

                  Pat_Root := False;

                  for J in 1 .. Name_Len loop
                     if Name_Buffer (J) not in 'a' .. 'z'
                       and then Name_Buffer (J) not in '0' .. '9'
                       and then Name_Buffer (J) /= '_'
                       and then Name_Buffer (J) /= '.'
                     then
                        Pat_Root := True;
                        exit;
                     end if;
                  end loop;

                  if Pat_Root then
                     begin
                        Root_Pattern :=
                          Compile
                            (Pattern => Name_Buffer (1 .. Name_Len),
                             Glob    => True);

                     exception
                        when Error_In_Regexp =>
                           Err_Vars.Error_Msg_Name_1 := Unit_Name;
                           Errutil.Error_Msg
                             ("invalid pattern %", Roots.Location);
                           exit Pattern_Loop;
                     end;
                  end if;

                  Roots_Found := False;
                  Iter        := For_Each_Source (Source.Tree);

                  Source_Loop :
                  loop
                     Root_Source := Prj.Element (Iter);
                     exit Source_Loop when Root_Source = No_Source;

                     Root_Found := False;
                     if Pat_Root then
                        Root_Found := Root_Source.Unit /= No_Unit_Index
                          and then Match
                            (Get_Name_String (Root_Source.Unit.Name),
                             Root_Pattern);

                     else
                        Root_Found :=
                          Root_Source.Unit /= No_Unit_Index
                          and then Root_Source.Unit.Name = Unit_Name;
                     end if;

                     if Root_Found then
                        case Root_Source.Kind is
                        when Impl =>
                           null;

                        when Spec =>
                           Root_Found := Other_Part (Root_Source) = No_Source;

                        when Sep =>
                           Root_Found := False;
                        end case;
                     end if;

                     if Root_Found then
                        Roots_Found := True;
                        Debug_Output
                          ("   -> ", Name_Id (Root_Source.Display_File));
                        Dummy := Queue.Insert_No_Roots
                          (Source => (Format => Format_Gprbuild,
                                      Tree   => Source.Tree,
                                      Id     => Root_Source));

                        Initialize_Source_Record (Root_Source);

                        if Other_Part (Root_Source) /= No_Source then
                           Initialize_Source_Record (Other_Part (Root_Source));
                        end if;

                        --  Save the root for the binder.

                        Source.Id.Roots := new Source_Roots'
                          (Root => Root_Source,
                           Next => Source.Id.Roots);

                        exit Source_Loop when not Pat_Root;
                     end if;

                     Next (Iter);
                  end loop Source_Loop;

                  if not Roots_Found then
                     if Pat_Root then
                        if not Quiet_Output then
                           Error_Msg_Name_1 := Unit_Name;
                           Errutil.Error_Msg
                             ("?no unit matches pattern %", Roots.Location);
                        end if;

                     else
                        Errutil.Error_Msg
                          ("Unit " & Get_Name_String (Unit_Name)
                           & " does not exist", Roots.Location);
                     end if;
                  end if;

                  List := Elem.Next;
               end loop Pattern_Loop;
            end if;
         end if;

         return True;
      end Insert;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Source  : Source_Info; With_Roots : Boolean := False)
      is
         Discard : Boolean;
         pragma Unreferenced (Discard);
      begin
         Discard := Insert (Source, With_Roots);
      end Insert;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is
      begin
         return Q_Processed >= Q.Last;
      end Is_Empty;

      ------------------------
      -- Is_Virtually_Empty --
      ------------------------

      function Is_Virtually_Empty return Boolean is
      begin
         if One_Queue_Per_Obj_Dir then
            for J in Q_First .. Q.Last loop
               if not Q.Table (J).Processed
                 and then Available_Obj_Dir (Q.Table (J).Info)
               then
                  return False;
               end if;
            end loop;

            return True;

         else
            return Is_Empty;
         end if;
      end Is_Virtually_Empty;

      ----------------------
      -- Set_Obj_Dir_Busy --
      ----------------------

      procedure Set_Obj_Dir_Busy (Obj_Dir : Path_Name_Type) is
      begin
         if One_Queue_Per_Obj_Dir then
            Busy_Obj_Dirs.Set (Obj_Dir, True);
         end if;
      end Set_Obj_Dir_Busy;

      ----------------------
      -- Set_Obj_Dir_Free --
      ----------------------

      procedure Set_Obj_Dir_Free (Obj_Dir : Path_Name_Type) is
      begin
         if One_Queue_Per_Obj_Dir then
            Busy_Obj_Dirs.Set (Obj_Dir, False);
         end if;
      end Set_Obj_Dir_Free;

      ----------
      -- Size --
      ----------

      function Size return Natural is
      begin
         return Q.Last;
      end Size;

      -------------
      -- Element --
      -------------

      function Element (Rank : Positive) return File_Name_Type is
      begin
         if Rank <= Q.Last then
            case Q.Table (Rank).Info.Format is
               when Format_Gprbuild =>
                  return Q.Table (Rank).Info.Id.File;
               when Format_Gnatmake =>
                  return Q.Table (Rank).Info.File;
            end case;
         else
            return No_File;
         end if;
      end Element;

      ------------------
      -- Remove_Marks --
      ------------------

      procedure Remove_Marks is
      begin
         Marks.Reset;
      end Remove_Marks;

      ----------------------------
      -- Insert_Project_Sources --
      ----------------------------

      procedure Insert_Project_Sources
        (Project      : Project_Id;
         Project_Tree : Project_Tree_Ref;
         All_Projects : Boolean;
         Unit_Based   : Boolean)
      is
         Iter   : Source_Iterator;
         Source : Prj.Source_Id;
      begin
         Iter := For_Each_Source (Project_Tree);
         loop
            Source := Prj.Element (Iter);
            exit when Source = No_Source;

            if Is_Compilable (Source)
              and then
                (All_Projects
                 or else Is_Extending (Project, Source.Project))
              and then not Source.Locally_Removed
              and then Source.Replaced_By = No_Source
              and then
                (not Source.Project.Externally_Built
                 or else
                   (Is_Extending (Project, Source.Project)
                    and then not Project.Externally_Built))
              and then Source.Kind /= Sep
              and then Source.Path /= No_Path_Information
            then
               if Source.Kind = Impl
                 or else (Source.Unit /= No_Unit_Index
                          and then Source.Kind = Spec
                          and then (Other_Part (Source) = No_Source
                                    or else
                                      Other_Part (Source).Locally_Removed))
               then
                  if (Unit_Based
                      or else Source.Unit = No_Unit_Index
                      or else Source.Project.Library)
                    and then not Is_Subunit (Source)
                  then
                     Queue.Insert
                       (Source => (Format => Format_Gprbuild,
                                   Tree   => Project_Tree,
                                   Id     => Source));
                  end if;
               end if;
            end if;

            Next (Iter);
         end loop;
      end Insert_Project_Sources;

      -------------------------------
      -- Insert_Withed_Sources_For --
      -------------------------------

      procedure Insert_Withed_Sources_For
        (The_ALI               : ALI.ALI_Id;
         Project_Tree          : Project_Tree_Ref;
         Excluding_Shared_SALs : Boolean := False)
      is
         Sfile     : File_Name_Type;
         Afile     : File_Name_Type;
         Src_Id    : Prj.Source_Id;

      begin
         --  Insert in the queue the unmarked source files (i.e. those which
         --  have never been inserted in the queue and hence never considered).

         for J in ALI.ALIs.Table (The_ALI).First_Unit ..
           ALI.ALIs.Table (The_ALI).Last_Unit
         loop
            for K in ALI.Units.Table (J).First_With ..
              ALI.Units.Table (J).Last_With
            loop
               Sfile := ALI.Withs.Table (K).Sfile;

               --  Skip generics

               if Sfile /= No_File then
                  Afile := ALI.Withs.Table (K).Afile;
                  Src_Id := Source_Files_Htable.Get
                    (Project_Tree.Source_Files_HT, Sfile);

                  while Src_Id /= No_Source loop
                     Initialize_Source_Record (Src_Id);

                     if Is_Compilable (Src_Id)
                       and then Src_Id.Dep_Name = Afile
                     then
                        case Src_Id.Kind is
                        when Spec =>
                           declare
                              Bdy : constant Prj.Source_Id :=
                                Other_Part (Src_Id);
                           begin
                              if Bdy /= No_Source
                                and then not Bdy.Locally_Removed
                              then
                                 Src_Id := Other_Part (Src_Id);
                              end if;
                           end;

                        when Impl =>
                           if Is_Subunit (Src_Id) then
                              Src_Id := No_Source;
                           end if;

                        when Sep =>
                           Src_Id := No_Source;
                        end case;

                        exit;
                     end if;

                     Src_Id := Src_Id.Next_With_File_Name;
                  end loop;

                  --  If Excluding_Shared_SALs is True, do not insert in the
                  --  queue the sources of a shared Stand-Alone Library.

                  if Src_Id /= No_Source and then
                    (not Excluding_Shared_SALs or else
                       not Src_Id.Project.Standalone_Library or else
                         Src_Id.Project.Library_Kind = Static)
                  then
                     Queue.Insert
                       (Source => (Format => Format_Gprbuild,
                                   Tree   => Project_Tree,
                                   Id     => Src_Id));
                  end if;
               end if;
            end loop;
         end loop;
      end Insert_Withed_Sources_For;

   end Queue;

end Makeutl;
