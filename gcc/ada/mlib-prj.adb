------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M L I B . P R J                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2009, AdaCore                     --
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
with Gnatvsn;  use Gnatvsn;
with MLib.Fil; use MLib.Fil;
with MLib.Tgt; use MLib.Tgt;
with MLib.Utl; use MLib.Utl;
with Opt;
with Output;   use Output;
with Prj.Com;  use Prj.Com;
with Prj.Env;  use Prj.Env;
with Prj.Util; use Prj.Util;
with Sinput.P;
with Snames;   use Snames;
with Switch;   use Switch;
with Table;
with Targparm; use Targparm;
with Tempdir;
with Types;    use Types;

with Ada.Characters.Handling;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.HTable;
with Interfaces.C_Streams;      use Interfaces.C_Streams;
with System;                    use System;
with System.Case_Util;          use System.Case_Util;

package body MLib.Prj is

   Prj_Add_Obj_Files : Types.Int;
   pragma Import (C, Prj_Add_Obj_Files, "__gnat_prj_add_obj_files");
   Add_Object_Files : constant Boolean := Prj_Add_Obj_Files /= 0;
   --  Indicates if object files in pragmas Linker_Options (found in the
   --  binder generated file) should be taken when linking a stand-alone
   --  library. False for Windows, True for other platforms.

   ALI_Suffix : constant String := ".ali";

   B_Start : String_Ptr := new String'("b~");
   --  Prefix of bind file, changed to b__ for VMS

   S_Osinte_Ads : File_Name_Type := No_File;
   --  Name_Id for "s-osinte.ads"

   S_Dec_Ads : File_Name_Type := No_File;
   --  Name_Id for "dec.ads"

   G_Trasym_Ads : File_Name_Type := No_File;
   --  Name_Id for "g-trasym.ads"

   Arguments : String_List_Access := No_Argument;
   --  Used to accumulate arguments for the invocation of gnatbind and of
   --  the compiler. Also used to collect the interface ALI when copying
   --  the ALI files to the library directory.

   Argument_Number : Natural := 0;
   --  Index of the last argument in Arguments

   Initial_Argument_Max : constant := 10;

   No_Main_String : aliased String := "-n";
   No_Main : constant String_Access := No_Main_String'Access;

   Output_Switch_String : aliased String := "-o";
   Output_Switch : constant String_Access := Output_Switch_String'Access;

   Compile_Switch_String : aliased String := "-c";
   Compile_Switch : constant String_Access := Compile_Switch_String'Access;

   Auto_Initialize : constant String := "-a";

   --  List of objects to put inside the library

   Object_Files : Argument_List_Access;

   package Objects is new Table.Table
     (Table_Name           => "Mlib.Prj.Objects",
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100);

   package Objects_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

   --  List of ALI files

   Ali_Files : Argument_List_Access;

   package ALIs is new Table.Table
     (Table_Name           => "Mlib.Prj.Alis",
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100);

   --  List of options set in the command line

   Options : Argument_List_Access;

   package Opts is new Table.Table
     (Table_Name           => "Mlib.Prj.Opts",
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 100);

   --  All the ALI file in the library

   package Library_ALIs is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");

   --  The ALI files in the interface sets

   package Interface_ALIs is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");

   --  The ALI files that have been processed to check if the corresponding
   --  library unit is in the interface set.

   package Processed_ALIs is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");

   --  The projects imported directly or indirectly

   package Processed_Projects is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

   --  The library projects imported directly or indirectly

   package Library_Projs is new Table.Table (
     Table_Component_Type => Project_Id,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 10,
     Table_Name           => "Make.Library_Projs");

   type Build_Mode_State is (None, Static, Dynamic, Relocatable);

   procedure Add_Argument (S : String);
   --  Add one argument to Arguments array, if array is full, double its size

   function ALI_File_Name (Source : String) return String;
   --  Return the ALI file name corresponding to a source

   procedure Check (Filename : String);
   --  Check if filename is a regular file. Fail if it is not

   procedure Check_Context;
   --  Check each object files in table Object_Files
   --  Fail if any of them is not a regular file

   procedure Copy_Interface_Sources
     (For_Project : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Interfaces  : Argument_List;
      To_Dir      : Path_Name_Type);
   --  Copy the interface sources of a SAL to directory To_Dir

   procedure Display (Executable : String);
   --  Display invocation of gnatbind and of the compiler with the arguments
   --  in Arguments, except when Quiet_Output is True.

   function Index (S, Pattern : String) return Natural;
   --  Return the last occurrence of Pattern in S, or 0 if none

   procedure Process_Binder_File (Name : String);
   --  For Stand-Alone libraries, get the Linker Options in the binder
   --  generated file.

   procedure Reset_Tables;
   --  Make sure that all the above tables are empty
   --  (Objects, Ali_Files, Options).

   function SALs_Use_Constructors return Boolean;
   --  Indicate if Stand-Alone Libraries are automatically initialized using
   --  the constructor mechanism.

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument (S : String) is
   begin
      if Argument_Number = Arguments'Last then
         declare
            New_Args : constant String_List_Access :=
              new String_List (1 .. 2 * Arguments'Last);

         begin
            --  Copy the String_Accesses and set them to null in Arguments
            --  so that they will not be deallocated by the call to
            --  Free (Arguments).

            New_Args (Arguments'Range) := Arguments.all;
            Arguments.all := (others => null);
            Free (Arguments);
            Arguments := New_Args;
         end;
      end if;

      Argument_Number := Argument_Number + 1;
      Arguments (Argument_Number) := new String'(S);
   end Add_Argument;

   -------------------
   -- ALI_File_Name --
   -------------------

   function ALI_File_Name (Source : String) return String is
   begin
      --  If the source name has an extension, then replace it with
      --  the ALI suffix.

      for Index in reverse Source'First + 1 .. Source'Last loop
         if Source (Index) = '.' then
            return Source (Source'First .. Index - 1) & ALI_Suffix;
         end if;
      end loop;

      --  If there is no dot, or if it is the first character, just add the
      --  ALI suffix.

      return Source & ALI_Suffix;
   end ALI_File_Name;

   -------------------
   -- Build_Library --
   -------------------

   procedure Build_Library
     (For_Project   : Project_Id;
      In_Tree       : Project_Tree_Ref;
      Gnatbind      : String;
      Gnatbind_Path : String_Access;
      Gcc           : String;
      Gcc_Path      : String_Access;
      Bind          : Boolean := True;
      Link          : Boolean := True)
   is
      Maximum_Size : Integer;
      pragma Import (C, Maximum_Size, "__gnat_link_max");
      --  Maximum number of bytes to put in an invocation of the
      --  gnatbind.

      Size : Integer;
      --  The number of bytes for the invocation of the gnatbind

      Warning_For_Library : Boolean := False;
      --  Set to True for the first warning about a unit missing from the
      --  interface set.

      Current_Proj : Project_Id;

      Libgnarl_Needed   : Yes_No_Unknown := For_Project.Libgnarl_Needed;
      --  Set to True if library needs to be linked with libgnarl

      Libdecgnat_Needed : Boolean := False;
      --  On OpenVMS, set to True if library needs to be linked with libdecgnat

      Gtrasymobj_Needed : Boolean := False;
      --  On OpenVMS, set to True if library needs to be linked with
      --  g-trasym.obj.

      Object_Directory_Path : constant String :=
                                Get_Name_String
                                  (For_Project.Object_Directory.Display_Name);

      Standalone   : constant Boolean := For_Project.Standalone_Library;

      Project_Name : constant String := Get_Name_String (For_Project.Name);

      Current_Dir  : constant String := Get_Current_Dir;

      Lib_Filename : String_Access;
      Lib_Dirpath  : String_Access;
      Lib_Version  : String_Access := new String'("");

      The_Build_Mode : Build_Mode_State := None;

      Success : Boolean := False;

      Library_Options : Variable_Value := Nil_Variable_Value;

      Driver_Name : Name_Id := No_Name;

      In_Main_Object_Directory : Boolean := True;

      Foreign_Sources : Boolean;

      Rpath : String_Access := null;
      --  Allocated only if Path Option is supported

      Rpath_Last : Natural := 0;
      --  Index of last valid character of Rpath

      Initial_Rpath_Length : constant := 200;
      --  Initial size of Rpath, when first allocated

      Path_Option : String_Access := Linker_Library_Path_Option;
      --  If null, Path Option is not supported.
      --  Not a constant so that it can be deallocated.

      First_ALI : File_Name_Type := No_File;
      --  Store the ALI file name of a source of the library (the first found)

      procedure Add_ALI_For (Source : File_Name_Type);
      --  Add the name of the ALI file corresponding to Source to the
      --  Arguments.

      procedure Add_Rpath (Path : String);
      --  Add a path name to Rpath

      function Check_Project (P : Project_Id) return Boolean;
      --  Returns True if P is For_Project or a project extended by For_Project

      procedure Check_Libs (ALI_File : String; Main_Project : Boolean);
      --  Set Libgnarl_Needed if the ALI_File indicates that there is a need
      --  to link with -lgnarl (this is the case when there is a dependency
      --  on s-osinte.ads). On OpenVMS, set Libdecgnat_Needed if the ALI file
      --  indicates that there is a need to link with -ldecgnat (this is the
      --  case when there is a dependency on dec.ads), and set
      --  Gtrasymobj_Needed if there is a dependency on g-trasym.ads.

      procedure Process (The_ALI : File_Name_Type);
      --  Check if the closure of a library unit which is or should be in the
      --  interface set is also in the interface set. Issue a warning for each
      --  missing library unit.

      procedure Process_Imported_Libraries;
      --  Add the -L and -l switches for the imported Library Project Files,
      --  and, if Path Option is supported, the library directory path names
      --  to Rpath.

      -----------------
      -- Add_ALI_For --
      -----------------

      procedure Add_ALI_For (Source : File_Name_Type) is
         ALI    : constant String := ALI_File_Name (Get_Name_String (Source));
         ALI_Id : File_Name_Type;

      begin
         if Bind then
            Add_Argument (ALI);
         end if;

         Name_Len := 0;
         Add_Str_To_Name_Buffer (S => ALI);
         ALI_Id := Name_Find;

         --  Add the ALI file name to the library ALIs

         if Bind then
            Library_ALIs.Set (ALI_Id, True);
         end if;

         --  Set First_ALI, if not already done

         if First_ALI = No_File then
            First_ALI := ALI_Id;
         end if;
      end Add_ALI_For;

      ---------------
      -- Add_Rpath --
      ---------------

      procedure Add_Rpath (Path : String) is

         procedure Double;
         --  Double Rpath size

         ------------
         -- Double --
         ------------

         procedure Double is
            New_Rpath : constant String_Access :=
                          new String (1 .. 2 * Rpath'Length);
         begin
            New_Rpath (1 .. Rpath_Last) := Rpath (1 .. Rpath_Last);
            Free (Rpath);
            Rpath := New_Rpath;
         end Double;

      --  Start of processing for Add_Rpath

      begin
         --  If first path, allocate initial Rpath

         if Rpath = null then
            Rpath := new String (1 .. Initial_Rpath_Length);
            Rpath_Last := 0;

         else
            --  Otherwise, add a path separator between two path names

            if Rpath_Last = Rpath'Last then
               Double;
            end if;

            Rpath_Last := Rpath_Last + 1;
            Rpath (Rpath_Last) := Path_Separator;
         end if;

         --  Increase Rpath size until it is large enough

         while Rpath_Last + Path'Length > Rpath'Last loop
            Double;
         end loop;

         --  Add the path name

         Rpath (Rpath_Last + 1 .. Rpath_Last + Path'Length) := Path;
         Rpath_Last := Rpath_Last + Path'Length;
      end Add_Rpath;

      -------------------
      -- Check_Project --
      -------------------

      function Check_Project (P : Project_Id) return Boolean is
      begin
         if P = For_Project then
            return True;

         elsif P /= No_Project then
            declare
               Proj : Project_Id;

            begin
               Proj := For_Project;
               while Proj.Extends /= No_Project loop
                  if P = Proj.Extends then
                     return True;
                  end if;

                  Proj := Proj.Extends;
               end loop;
            end;
         end if;

         return False;
      end Check_Project;

      ----------------
      -- Check_Libs --
      ----------------

      procedure Check_Libs (ALI_File : String; Main_Project : Boolean) is
         Lib_File : File_Name_Type;
         Text     : Text_Buffer_Ptr;
         Id       : ALI.ALI_Id;

      begin
         if Libgnarl_Needed /= Yes
           or else
            (Main_Project
              and then OpenVMS_On_Target
              and then ((not Libdecgnat_Needed) or (not Gtrasymobj_Needed)))
         then
            --  Scan the ALI file

            Name_Len := ALI_File'Length;
            Name_Buffer (1 .. Name_Len) := ALI_File;
            Lib_File := Name_Find;
            Text := Read_Library_Info (Lib_File, True);

            Id := ALI.Scan_ALI
                    (F          => Lib_File,
                     T          => Text,
                     Ignore_ED  => False,
                     Err        => True,
                     Read_Lines => "D");
            Free (Text);

            --  Look for s-osinte.ads in the dependencies

            for Index in ALI.ALIs.Table (Id).First_Sdep ..
                         ALI.ALIs.Table (Id).Last_Sdep
            loop
               if ALI.Sdep.Table (Index).Sfile = S_Osinte_Ads then
                  Libgnarl_Needed := Yes;

                  if Main_Project then
                     For_Project.Libgnarl_Needed := Yes;
                  else
                     exit;
                  end if;

               elsif OpenVMS_On_Target then
                  if ALI.Sdep.Table (Index).Sfile = S_Dec_Ads then
                     Libdecgnat_Needed := True;

                  elsif ALI.Sdep.Table (Index).Sfile = G_Trasym_Ads then
                     Gtrasymobj_Needed := True;
                  end if;
               end if;
            end loop;
         end if;
      end Check_Libs;

      -------------
      -- Process --
      -------------

      procedure Process (The_ALI : File_Name_Type) is
         Text       : Text_Buffer_Ptr;
         Idread     : ALI_Id;
         First_Unit : ALI.Unit_Id;
         Last_Unit  : ALI.Unit_Id;
         Unit_Data  : Unit_Record;
         Afile      : File_Name_Type;

      begin
         --  Nothing to do if the ALI file has already been processed.
         --  This happens if an interface imports another interface.

         if not Processed_ALIs.Get (The_ALI) then
            Processed_ALIs.Set (The_ALI, True);
            Text := Read_Library_Info (The_ALI);

            if Text /= null then
               Idread :=
                 Scan_ALI
                   (F         => The_ALI,
                    T         => Text,
                    Ignore_ED => False,
                    Err       => True);
               Free (Text);

               if Idread /= No_ALI_Id then
                  First_Unit := ALI.ALIs.Table (Idread).First_Unit;
                  Last_Unit  := ALI.ALIs.Table (Idread).Last_Unit;

                  --  Process both unit (spec and body) if the body is needed
                  --  by the spec (inline or generic). Otherwise, just process
                  --  the spec.

                  if First_Unit /= Last_Unit and then
                    not ALI.Units.Table (Last_Unit).Body_Needed_For_SAL
                  then
                     First_Unit := Last_Unit;
                  end if;

                  for Unit in First_Unit .. Last_Unit loop
                     Unit_Data := ALI.Units.Table (Unit);

                     --  Check if each withed unit which is in the library is
                     --  also in the interface set, if it has not yet been
                     --  processed.

                     for W in Unit_Data.First_With .. Unit_Data.Last_With loop
                        Afile := Withs.Table (W).Afile;

                        if Afile /= No_File and then Library_ALIs.Get (Afile)
                          and then not Processed_ALIs.Get (Afile)
                        then
                           if not Interface_ALIs.Get (Afile) then
                              if not Warning_For_Library then
                                 Write_Str ("Warning: In library project """);
                                 Get_Name_String (Current_Proj.Name);
                                 To_Mixed (Name_Buffer (1 .. Name_Len));
                                 Write_Str (Name_Buffer (1 .. Name_Len));
                                 Write_Line ("""");
                                 Warning_For_Library := True;
                              end if;

                              Write_Str ("         Unit """);
                              Get_Name_String (Withs.Table (W).Uname);
                              To_Mixed (Name_Buffer (1 .. Name_Len - 2));
                              Write_Str (Name_Buffer (1 .. Name_Len - 2));
                              Write_Line (""" is not in the interface set");
                              Write_Str ("         but it is needed by ");

                              case Unit_Data.Utype is
                                 when Is_Spec =>
                                    Write_Str ("the spec of ");

                                 when Is_Body =>
                                    Write_Str ("the body of ");

                                 when others =>
                                    null;
                              end case;

                              Write_Str ("""");
                              Get_Name_String (Unit_Data.Uname);
                              To_Mixed (Name_Buffer (1 .. Name_Len - 2));
                              Write_Str (Name_Buffer (1 .. Name_Len - 2));
                              Write_Line ("""");
                           end if;

                           --  Now, process this unit

                           Process (Afile);
                        end if;
                     end loop;
                  end loop;
               end if;
            end if;
         end if;
      end Process;

      --------------------------------
      -- Process_Imported_Libraries --
      --------------------------------

      procedure Process_Imported_Libraries is
         Current : Project_Id;

         procedure Process_Project (Project : Project_Id);
         --  Process Project and its imported projects recursively.
         --  Add any library projects to table Library_Projs.

         ---------------------
         -- Process_Project --
         ---------------------

         procedure Process_Project (Project : Project_Id) is
            Imported : Project_List;

         begin
            --  Nothing to do if process has already been processed

            if not Processed_Projects.Get (Project.Name) then
               Processed_Projects.Set (Project.Name, True);

               --  Call Process_Project recursively for any imported project.
               --  We first process the imported projects to guarantee that
               --  we have a proper reverse order for the libraries.

               Imported := Project.Imported_Projects;
               while Imported /= null loop
                  if Imported.Project /= No_Project then
                     Process_Project (Imported.Project);
                  end if;

                  Imported := Imported.Next;
               end loop;

               --  If it is a library project, add it to Library_Projs

               if Project /= For_Project and then Project.Library then
                  Library_Projs.Increment_Last;
                  Library_Projs.Table (Library_Projs.Last) := Project;

                  --  Check if because of this library we need to use libgnarl

                  if Libgnarl_Needed = Unknown then
                     if Project.Libgnarl_Needed = Unknown
                       and then Project.Object_Directory /= No_Path_Information
                     then
                        --  Check if libgnarl is needed for this library

                        declare
                           Object_Dir_Path : constant String :=
                                               Get_Name_String
                                                 (Project.Object_Directory.
                                                    Display_Name);
                           Object_Dir      : Dir_Type;
                           Filename        : String (1 .. 255);
                           Last            : Natural;

                        begin
                           Open (Object_Dir, Object_Dir_Path);

                           --  For all entries in the object directory

                           loop
                              Read (Object_Dir, Filename, Last);
                              exit when Last = 0;

                              --  Check if it is an object file

                              if Is_Obj (Filename (1 .. Last)) then
                                 declare
                                    Object_Path : constant String :=
                                                    Normalize_Pathname
                                                      (Object_Dir_Path &
                                                       Directory_Separator &
                                                       Filename (1 .. Last));
                                    ALI_File    : constant String :=
                                                    Ext_To
                                                      (Object_Path, "ali");

                                 begin
                                    if Is_Regular_File (ALI_File) then

                                       --  Find out if for this ALI file,
                                       --  libgnarl is necessary.

                                       Check_Libs
                                         (ALI_File, Main_Project => False);

                                       if Libgnarl_Needed = Yes then
                                          Project.Libgnarl_Needed := Yes;
                                          For_Project.Libgnarl_Needed := Yes;
                                          exit;
                                       end if;
                                    end if;
                                 end;
                              end if;
                           end loop;

                           Close (Object_Dir);
                        end;
                     end if;

                     if Project.Libgnarl_Needed = Yes then
                        Libgnarl_Needed := Yes;
                        For_Project.Libgnarl_Needed := Yes;
                     end if;
                  end if;
               end if;
            end if;
         end Process_Project;

      --  Start of processing for Process_Imported_Libraries

      begin
         --  Build list of library projects imported directly or indirectly,
         --  in the reverse order.

         Process_Project (For_Project);

         --  Add the -L and -l switches and, if the Rpath option is supported,
         --  add the directory to the Rpath. As the library projects are in the
         --  wrong order, process from the last to the first.

         for Index in reverse 1 .. Library_Projs.Last loop
            Current := Library_Projs.Table (Index);

            Get_Name_String (Current.Library_Dir.Display_Name);
            Opts.Increment_Last;
            Opts.Table (Opts.Last) :=
              new String'("-L" & Name_Buffer (1 .. Name_Len));

            if Path_Option /= null then
               Add_Rpath (Name_Buffer (1 .. Name_Len));
            end if;

            Opts.Increment_Last;
            Opts.Table (Opts.Last) :=
              new String'("-l" & Get_Name_String (Current.Library_Name));
         end loop;
      end Process_Imported_Libraries;

   --  Start of processing for Build_Library

   begin
      Reset_Tables;

      --  Fail if project is not a library project

      if not For_Project.Library then
         Com.Fail ("project """ & Project_Name & """ has no library");
      end if;

      --  Do not attempt to build the library if it is externally built

      if For_Project.Externally_Built then
         return;
      end if;

      --  If this is the first time Build_Library is called, get the Name_Id
      --  of "s-osinte.ads".

      if S_Osinte_Ads = No_File then
         Name_Len := 0;
         Add_Str_To_Name_Buffer ("s-osinte.ads");
         S_Osinte_Ads := Name_Find;
      end if;

      if S_Dec_Ads = No_File then
         Name_Len := 0;
         Add_Str_To_Name_Buffer ("dec.ads");
         S_Dec_Ads := Name_Find;
      end if;

      if G_Trasym_Ads = No_File then
         Name_Len := 0;
         Add_Str_To_Name_Buffer ("g-trasym.ads");
         G_Trasym_Ads := Name_Find;
      end if;

      --  We work in the object directory

      Change_Dir (Object_Directory_Path);

      if Standalone then

         --  Call gnatbind only if Bind is True

         if Bind then
            if Gnatbind_Path = null then
               Com.Fail ("unable to locate " & Gnatbind);
            end if;

            if Gcc_Path = null then
               Com.Fail ("unable to locate " & Gcc);
            end if;

            --  Allocate Arguments, if it is the first time we see a standalone
            --  library.

            if Arguments = No_Argument then
               Arguments := new String_List (1 .. Initial_Argument_Max);
            end if;

            --  Add "-n -o b~<lib>.adb (b__<lib>.adb on VMS) -L<lib>"

            Argument_Number := 2;
            Arguments (1) := No_Main;
            Arguments (2) := Output_Switch;

            if OpenVMS_On_Target then
               B_Start := new String'("b__");
            end if;

            Add_Argument
              (B_Start.all
               & Get_Name_String (For_Project.Library_Name) & ".adb");
            Add_Argument ("-L" & Get_Name_String (For_Project.Library_Name));

            if For_Project.Lib_Auto_Init and then SALs_Use_Constructors then
               Add_Argument (Auto_Initialize);
            end if;

            --  Check if Binder'Default_Switches ("Ada") is defined. If it is,
            --  add these switches to call gnatbind.

            declare
               Binder_Package : constant Package_Id :=
                                  Value_Of
                                    (Name        => Name_Binder,
                                     In_Packages => For_Project.Decl.Packages,
                                     In_Tree     => In_Tree);

            begin
               if Binder_Package /= No_Package then
                  declare
                     Defaults : constant Array_Element_Id :=
                                  Value_Of
                                    (Name      => Name_Default_Switches,
                                     In_Arrays =>
                                       In_Tree.Packages.Table
                                         (Binder_Package).Decl.Arrays,
                                     In_Tree   => In_Tree);
                     Switches : Variable_Value := Nil_Variable_Value;

                     Switch : String_List_Id := Nil_String;

                  begin
                     if Defaults /= No_Array_Element then
                        Switches :=
                          Value_Of
                            (Index     => Name_Ada,
                             Src_Index => 0,
                             In_Array  => Defaults,
                             In_Tree   => In_Tree);

                        if not Switches.Default then
                           Switch := Switches.Values;

                           while Switch /= Nil_String loop
                              Add_Argument
                                (Get_Name_String
                                   (In_Tree.String_Elements.Table
                                      (Switch).Value));
                              Switch := In_Tree.String_Elements.
                                          Table (Switch).Next;
                           end loop;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;

         --  Get all the ALI files of the project file. We do that even if
         --  Bind is False, so that First_ALI is set.

         declare
            Unit : Unit_Index;

         begin
            Library_ALIs.Reset;
            Interface_ALIs.Reset;
            Processed_ALIs.Reset;

            Unit := Units_Htable.Get_First (In_Tree.Units_HT);
            while Unit /= No_Unit_Index loop
               if Unit.File_Names (Impl) /= null
                 and then not Unit.File_Names (Impl).Locally_Removed
               then
                  if Check_Project (Unit.File_Names (Impl).Project) then
                     if Unit.File_Names (Spec) = null then
                        declare
                           Src_Ind : Source_File_Index;

                        begin
                           Src_Ind := Sinput.P.Load_Project_File
                                        (Get_Name_String
                                          (Unit.File_Names (Impl).Path.Name));

                           --  Add the ALI file only if it is not a subunit

                           if not
                             Sinput.P.Source_File_Is_Subunit (Src_Ind)
                           then
                              Add_ALI_For (Unit.File_Names (Impl).File);
                              exit when not Bind;
                           end if;
                        end;

                     else
                        Add_ALI_For (Unit.File_Names (Impl).File);
                        exit when not Bind;
                     end if;
                  end if;

               elsif Unit.File_Names (Spec) /= null
                 and then not Unit.File_Names (Spec).Locally_Removed
                 and then Check_Project (Unit.File_Names (Spec).Project)
               then
                  Add_ALI_For (Unit.File_Names (Spec).File);
                  exit when not Bind;
               end if;

               Unit := Units_Htable.Get_Next (In_Tree.Units_HT);
            end loop;
         end;

         --  Continue setup and call gnatbind if Bind is True

         if Bind then

            --  Get an eventual --RTS from the ALI file

            if First_ALI /= No_File then
               declare
                  T : Text_Buffer_Ptr;
                  A : ALI_Id;

               begin
                  --  Load the ALI file

                  T := Read_Library_Info (First_ALI, True);

                  --  Read it

                  A := Scan_ALI
                         (First_ALI, T, Ignore_ED => False, Err => False);

                  if A /= No_ALI_Id then
                     for Index in
                       ALI.Units.Table
                         (ALI.ALIs.Table (A).First_Unit).First_Arg ..
                       ALI.Units.Table
                         (ALI.ALIs.Table (A).First_Unit).Last_Arg
                     loop
                        --  If --RTS found, add switch to call gnatbind

                        declare
                           Arg : String_Ptr renames Args.Table (Index);
                        begin
                           if Arg'Length >= 6 and then
                              Arg (Arg'First + 2 .. Arg'First + 5) = "RTS="
                           then
                              Add_Argument (Arg.all);
                              exit;
                           end if;
                        end;
                     end loop;
                  end if;
               end;
            end if;

            --  Set the paths

            Set_Ada_Paths
              (Project             => For_Project,
               In_Tree             => In_Tree,
               Including_Libraries => True);

            --  Display the gnatbind command, if not in quiet output

            Display (Gnatbind);

            Size := 0;
            for J in 1 .. Argument_Number loop
               Size := Size + Arguments (J)'Length + 1;
            end loop;

            --  Invoke gnatbind with the arguments if the size is not too large

            if Size <= Maximum_Size then
               Spawn
                 (Gnatbind_Path.all,
                  Arguments (1 .. Argument_Number),
                  Success);

            else
               --  Otherwise create a temporary response file

               declare
                  FD            : File_Descriptor;
                  Path          : Path_Name_Type;
                  Args          : Argument_List (1 .. 1);
                  EOL           : constant String (1 .. 1) := (1 => ASCII.LF);
                  Status        : Integer;
                  Succ          : Boolean;
                  Quotes_Needed : Boolean;
                  Last_Char     : Natural;
                  Ch            : Character;

               begin
                  Tempdir.Create_Temp_File (FD, Path);
                  Args (1) := new String'("@" & Get_Name_String (Path));

                  for J in 1 .. Argument_Number loop

                     --  Check if the argument should be quoted

                     Quotes_Needed := False;
                     Last_Char     := Arguments (J)'Length;

                     for K in Arguments (J)'Range loop
                        Ch := Arguments (J) (K);

                        if Ch = ' ' or else Ch = ASCII.HT or else Ch = '"' then
                           Quotes_Needed := True;
                           exit;
                        end if;
                     end loop;

                     if Quotes_Needed then

                        --  Quote the argument, doubling '"'

                        declare
                           Arg : String (1 .. Arguments (J)'Length * 2 + 2);

                        begin
                           Arg (1) := '"';
                           Last_Char := 1;

                           for K in Arguments (J)'Range loop
                              Ch := Arguments (J) (K);
                              Last_Char := Last_Char + 1;
                              Arg (Last_Char) := Ch;

                              if Ch = '"' then
                                 Last_Char := Last_Char + 1;
                                 Arg (Last_Char) := '"';
                              end if;
                           end loop;

                           Last_Char := Last_Char + 1;
                           Arg (Last_Char) := '"';

                           Status := Write (FD, Arg'Address, Last_Char);
                        end;

                     else
                        Status := Write
                          (FD,
                           Arguments (J) (Arguments (J)'First)'Address,
                           Last_Char);
                     end if;

                     if Status /= Last_Char then
                        Fail ("disk full");
                     end if;

                     Status := Write (FD, EOL (1)'Address, 1);

                     if Status /= 1 then
                        Fail ("disk full");
                     end if;
                  end loop;

                  Close (FD);

                  --  And invoke gnatbind with this response file

                  Spawn (Gnatbind_Path.all, Args, Success);

                  Delete_File (Get_Name_String (Path), Succ);

                  if not Succ then
                     null;
                  end if;
               end;
            end if;

            if not Success then
               Com.Fail ("could not bind standalone library "
                         & Get_Name_String (For_Project.Library_Name));
            end if;
         end if;

         --  Compile the binder generated file only if Link is true

         if Link then

            --  Set the paths

            Set_Ada_Paths
              (Project             => For_Project,
               In_Tree             => In_Tree,
               Including_Libraries => True);

            --  Invoke <gcc> -c b__<lib>.adb

            --  Allocate Arguments, if it is the first time we see a standalone
            --  library.

            if Arguments = No_Argument then
               Arguments := new String_List (1 .. Initial_Argument_Max);
            end if;

            Argument_Number := 1;
            Arguments (1) := Compile_Switch;

            if OpenVMS_On_Target then
               B_Start := new String'("b__");
            end if;

            Add_Argument
              (B_Start.all
               & Get_Name_String (For_Project.Library_Name) & ".adb");

            --  If necessary, add the PIC option

            if PIC_Option /= "" then
               Add_Argument (PIC_Option);
            end if;

            --  Get the back-end switches and --RTS from the ALI file

            if First_ALI /= No_File then
               declare
                  T : Text_Buffer_Ptr;
                  A : ALI_Id;

               begin
                  --  Load the ALI file

                  T := Read_Library_Info (First_ALI, True);

                  --  Read it

                  A :=
                    Scan_ALI (First_ALI, T, Ignore_ED => False, Err => False);

                  if A /= No_ALI_Id then
                     for Index in
                       ALI.Units.Table
                         (ALI.ALIs.Table (A).First_Unit).First_Arg ..
                       ALI.Units.Table
                         (ALI.ALIs.Table (A).First_Unit).Last_Arg
                     loop
                        --  Do not compile with the front end switches except
                        --  for --RTS.

                        declare
                           Arg : String_Ptr renames Args.Table (Index);
                        begin
                           if not Is_Front_End_Switch (Arg.all)
                             or else
                               Arg (Arg'First + 2 .. Arg'First + 5) = "RTS="
                           then
                              Add_Argument (Arg.all);
                           end if;
                        end;
                     end loop;
                  end if;
               end;
            end if;

            --  Now that all the arguments are set, compile the binder
            --  generated file.

            Display (Gcc);
            Spawn
              (Gcc_Path.all, Arguments (1 .. Argument_Number), Success);

            if not Success then
               Com.Fail
                ("could not compile binder generated file for library "
                  & Get_Name_String (For_Project.Library_Name));
            end if;

            --  Process binder generated file for pragmas Linker_Options

            Process_Binder_File (Arguments (2).all & ASCII.NUL);
         end if;
      end if;

      --  Build the library only if Link is True

      if Link then

         --  If attributes Library_GCC or Linker'Driver were specified, get the
         --  driver name.

         if For_Project.Config.Shared_Lib_Driver /= No_File then
            Driver_Name := Name_Id (For_Project.Config.Shared_Lib_Driver);
         end if;

         --  If attribute Library_Options was specified, add these additional
         --  options.

         Library_Options := Value_Of
           (Name_Library_Options, For_Project.Decl.Attributes, In_Tree);

         if not Library_Options.Default then
            declare
               Current : String_List_Id;
               Element : String_Element;

            begin
               Current := Library_Options.Values;
               while Current /= Nil_String loop
                  Element := In_Tree.String_Elements.Table (Current);
                  Get_Name_String (Element.Value);

                  if Name_Len /= 0 then
                     Opts.Increment_Last;
                     Opts.Table (Opts.Last) :=
                       new String'(Name_Buffer (1 .. Name_Len));
                  end if;

                  Current := Element.Next;
               end loop;
            end;
         end if;

         Lib_Dirpath  :=
           new String'(Get_Name_String (For_Project.Library_Dir.Display_Name));
         Lib_Filename := new String'
           (Get_Name_String (For_Project.Library_Name));

         case For_Project.Library_Kind is
            when Static =>
               The_Build_Mode := Static;

            when Dynamic =>
               The_Build_Mode := Dynamic;

            when Relocatable =>
               The_Build_Mode := Relocatable;

               if PIC_Option /= "" then
                  Opts.Increment_Last;
                  Opts.Table (Opts.Last) := new String'(PIC_Option);
               end if;
         end case;

         --  Get the library version, if any

         if For_Project.Lib_Internal_Name /= No_Name then
            Lib_Version :=
              new String'(Get_Name_String (For_Project.Lib_Internal_Name));
         end if;

         --  Add the objects found in the object directory and the object
         --  directories of the extended files, if any, except for generated
         --  object files (b~.. or B__..) from extended projects.
         --  When there are one or more extended files, only add an object file
         --  if no object file with the same name have already been added.

         In_Main_Object_Directory := True;

         --  For gnatmake, when the project specifies more than just Ada as a
         --  language (even if course we could not find any source file for
         --  the other languages), we will take all object files found in the
         --  object directories. Since we know the project supports at least
         --  Ada, we just have to test whether it has at least two languages,
         --  and not care about the sources.

         Foreign_Sources := For_Project.Languages.Next /= null;
         Current_Proj := For_Project;
         loop
            if Current_Proj.Object_Directory /= No_Path_Information then

               --  The following code gets far too indented, I suggest some
               --  procedural abstraction here. How about making this declare
               --  block a named procedure???

               declare
                  Object_Dir_Path : constant String :=
                                      Get_Name_String
                                        (Current_Proj.Object_Directory
                                         .Display_Name);

                  Object_Dir : Dir_Type;
                  Filename   : String (1 .. 255);
                  Last       : Natural;
                  Id         : Name_Id;

               begin
                  Open (Dir => Object_Dir, Dir_Name => Object_Dir_Path);

                  --  For all entries in the object directory

                  loop
                     Read (Object_Dir, Filename, Last);

                     exit when Last = 0;

                     --  Check if it is an object file

                     if Is_Obj (Filename (1 .. Last)) then
                        declare
                           Object_Path  : constant String :=
                                            Normalize_Pathname
                                              (Object_Dir_Path
                                               & Directory_Separator
                                               & Filename (1 .. Last));

                           C_Object_Path : String := Object_Path;
                           C_Filename    : String := Filename (1 .. Last);

                        begin
                           Canonical_Case_File_Name (C_Object_Path);
                           Canonical_Case_File_Name (C_Filename);

                           --  If in the object directory of an extended
                           --  project, do not consider generated object files.

                           if In_Main_Object_Directory
                             or else Last < 5
                             or else
                               C_Filename (1 .. B_Start'Length) /= B_Start.all
                           then
                              Name_Len := Last;
                              Name_Buffer (1 .. Name_Len) :=
                                C_Filename (1 .. Last);
                              Id := Name_Find;

                              if not Objects_Htable.Get (Id) then
                                 declare
                                    ALI_File : constant String :=
                                                 Ext_To
                                                   (C_Filename
                                                      (1 .. Last), "ali");

                                    ALI_Path : constant String :=
                                                 Ext_To (C_Object_Path, "ali");

                                    Add_It : Boolean;
                                    Fname  : File_Name_Type;
                                    Proj   : Project_Id;
                                    Index  : Unit_Index;

                                 begin
                                    --  The following assignment could use
                                    --  a comment ???

                                    Add_It :=
                                      Foreign_Sources
                                        or else
                                          (Last >= 5
                                             and then
                                               C_Filename (1 .. B_Start'Length)
                                                 = B_Start.all);

                                    if Is_Regular_File (ALI_Path) then

                                       --  If there is an ALI file, check if
                                       --  the object file should be added to
                                       --  the library. If there are foreign
                                       --  sources we put all object files in
                                       --  the library.

                                       if not Add_It then
                                          Index :=
                                            Units_Htable.Get_First
                                             (In_Tree.Units_HT);
                                          while Index /= null loop
                                             if Index.File_Names (Impl) /=
                                               null
                                             then
                                                Proj :=
                                                  Index.File_Names (Impl)
                                                  .Project;
                                                Fname :=
                                                  Index.File_Names (Impl).File;

                                             elsif Index.File_Names (Spec) /=
                                               null
                                             then
                                                Proj :=
                                                  Index.File_Names (Spec)
                                                  .Project;
                                                Fname :=
                                                  Index.File_Names (Spec).File;

                                             else
                                                Proj := No_Project;
                                             end if;

                                             Add_It := Proj /= No_Project;

                                             --  If the source is in the
                                             --  project or a project it
                                             --  extends, we may put it in
                                             --  the library.

                                             if Add_It then
                                                Add_It := Check_Project (Proj);
                                             end if;

                                             --  But we don't, if the ALI file
                                             --  does not correspond to the
                                             --  unit.

                                             if Add_It then
                                                declare
                                                   F : constant String :=
                                                         Ext_To
                                                           (Get_Name_String
                                                              (Fname), "ali");
                                                begin
                                                   Add_It := F = ALI_File;
                                                end;
                                             end if;

                                             exit when Add_It;

                                             Index :=
                                               Units_Htable.Get_Next
                                                 (In_Tree.Units_HT);
                                          end loop;
                                       end if;

                                       if Add_It then
                                          Objects_Htable.Set (Id, True);
                                          Objects.Append
                                            (new String'(Object_Path));

                                          --  Record the ALI file

                                          ALIs.Append (new String'(ALI_Path));

                                          --  Find out if for this ALI file,
                                          --  libgnarl or libdecgnat or
                                          --  g-trasym.obj (on OpenVMS) is
                                          --  necessary.

                                          Check_Libs (ALI_Path, True);
                                       end if;

                                    elsif Foreign_Sources then
                                       Objects.Append
                                         (new String'(Object_Path));
                                    end if;
                                 end;
                              end if;
                           end if;
                        end;
                     end if;
                  end loop;

                  Close (Dir => Object_Dir);

               exception
                  when Directory_Error =>
                     Com.Fail ("cannot find object directory """
                               & Get_Name_String
                                  (Current_Proj.Object_Directory.Display_Name)
                               & """");
               end;
            end if;

            exit when Current_Proj.Extends = No_Project;

            In_Main_Object_Directory  := False;
            Current_Proj := Current_Proj.Extends;
         end loop;

         --  Add the -L and -l switches for the imported Library Project Files,
         --  and, if Path Option is supported, the library directory path names
         --  to Rpath.

         Process_Imported_Libraries;

         --  Link with libgnat and possibly libgnarl

         Opts.Increment_Last;
         Opts.Table (Opts.Last) := new String'("-L" & Lib_Directory);

         --  If Path Option is supported, add libgnat directory path name to
         --  Rpath.

         if Path_Option /= null then
            declare
               Libdir    : constant String := Lib_Directory;
               GCC_Index : Natural := 0;

            begin
               Add_Rpath (Libdir);

               --  For shared libraries, add to the Path Option the directory
               --  of the shared version of libgcc.

               if The_Build_Mode /= Static then
                  GCC_Index := Index (Libdir, "/lib/");

                  if GCC_Index = 0 then
                     GCC_Index :=
                       Index
                         (Libdir,
                          Directory_Separator & "lib" & Directory_Separator);
                  end if;

                  if GCC_Index /= 0 then
                     Add_Rpath (Libdir (Libdir'First .. GCC_Index + 3));
                  end if;
               end if;
            end;
         end if;

         if Libgnarl_Needed = Yes then
            Opts.Increment_Last;

            if The_Build_Mode = Static then
               Opts.Table (Opts.Last) := new String'("-lgnarl");
            else
               Opts.Table (Opts.Last) := new String'(Shared_Lib ("gnarl"));
            end if;
         end if;

         if Gtrasymobj_Needed then
            Opts.Increment_Last;
            Opts.Table (Opts.Last) :=
              new String'(Lib_Directory & "/g-trasym.obj");
         end if;

         if Libdecgnat_Needed then
            Opts.Increment_Last;

            Opts.Table (Opts.Last) :=
              new String'("-L" & Lib_Directory & "/../declib");

            Opts.Increment_Last;

            if The_Build_Mode = Static then
               Opts.Table (Opts.Last) := new String'("-ldecgnat");
            else
               Opts.Table (Opts.Last) := new String'(Shared_Lib ("decgnat"));
            end if;
         end if;

         Opts.Increment_Last;

         if The_Build_Mode = Static then
            Opts.Table (Opts.Last) := new String'("-lgnat");
         else
            Opts.Table (Opts.Last) := new String'(Shared_Lib ("gnat"));
         end if;

         --  If Path Option is supported, add the necessary switch with the
         --  content of Rpath. As Rpath contains at least libgnat directory
         --  path name, it is guaranteed that it is not null.

         if Path_Option /= null then
            Opts.Increment_Last;
            Opts.Table (Opts.Last) :=
              new String'(Path_Option.all & Rpath (1 .. Rpath_Last));
            Free (Path_Option);
            Free (Rpath);
         end if;

         Object_Files :=
           new Argument_List'
             (Argument_List (Objects.Table (1 .. Objects.Last)));

         Ali_Files :=
           new Argument_List'(Argument_List (ALIs.Table (1 .. ALIs.Last)));

         Options :=
           new Argument_List'(Argument_List (Opts.Table (1 .. Opts.Last)));

         --  We fail if there are no object to put in the library
         --  (Ada or foreign objects).

         if Object_Files'Length = 0 then
            Com.Fail ("no object files for library """ &
                      Lib_Filename.all & '"');
         end if;

         if not Opt.Quiet_Output then
            Write_Eol;
            Write_Str  ("building ");
            Write_Str (Ada.Characters.Handling.To_Lower
                         (Build_Mode_State'Image (The_Build_Mode)));
            Write_Str  (" library for project ");
            Write_Line (Project_Name);

            --  Only output list of object files and ALI files in verbose mode

            if Opt.Verbose_Mode then
               Write_Eol;

               Write_Line ("object files:");

               for Index in Object_Files'Range loop
                  Write_Str  ("   ");
                  Write_Line (Object_Files (Index).all);
               end loop;

               Write_Eol;

               if Ali_Files'Length = 0 then
                  Write_Line ("NO ALI files");

               else
                  Write_Line ("ALI files:");

                  for Index in Ali_Files'Range loop
                     Write_Str  ("   ");
                     Write_Line (Ali_Files (Index).all);
                  end loop;
               end if;

               Write_Eol;
            end if;
         end if;

         --  We check that all object files are regular files

         Check_Context;

         --  Delete the existing library file, if it exists. Fail if the
         --  library file is not writable, or if it is not possible to delete
         --  the file.

         declare
            DLL_Name : aliased String :=
                         Lib_Dirpath.all & Directory_Separator & DLL_Prefix &
                           Lib_Filename.all & "." & DLL_Ext;

            Archive_Name : aliased String :=
                             Lib_Dirpath.all & Directory_Separator & "lib" &
                               Lib_Filename.all & "." & Archive_Ext;

            type Str_Ptr is access all String;
            --  This type is necessary to meet the accessibility rules of Ada.
            --  It is not possible to use String_Access here.

            Full_Lib_Name : Str_Ptr;
            --  Designates the full library path name. Either DLL_Name or
            --  Archive_Name, depending on the library kind.

            Success : Boolean;
            pragma Warnings (Off, Success);
            --  Used to call Delete_File

         begin
            if The_Build_Mode = Static then
               Full_Lib_Name := Archive_Name'Access;
            else
               Full_Lib_Name := DLL_Name'Access;
            end if;

            if Is_Regular_File (Full_Lib_Name.all) then
               if Is_Writable_File (Full_Lib_Name.all) then
                  Delete_File (Full_Lib_Name.all, Success);
               end if;

               if Is_Regular_File (Full_Lib_Name.all) then
                  Com.Fail ("could not delete """ & Full_Lib_Name.all & """");
               end if;
            end if;
         end;

         Argument_Number := 0;

         --  If we have a standalone library, gather all the interface ALI.
         --  They are passed to Build_Dynamic_Library, where they are used by
         --  some platforms (VMS, for example) to decide what symbols should be
         --  exported. They are also flagged as Interface when we copy them to
         --  the library directory (by Copy_ALI_Files, below).

         if Standalone then
            Current_Proj := For_Project;

            declare
               Iface : String_List_Id := For_Project.Lib_Interface_ALIs;
               ALI   : File_Name_Type;

            begin
               while Iface /= Nil_String loop
                  ALI :=
                    File_Name_Type
                      (In_Tree.String_Elements.Table (Iface).Value);
                  Interface_ALIs.Set (ALI, True);
                  Get_Name_String
                    (In_Tree.String_Elements.Table (Iface).Value);
                  Add_Argument (Name_Buffer (1 .. Name_Len));
                  Iface := In_Tree.String_Elements.Table (Iface).Next;
               end loop;

               Iface := For_Project.Lib_Interface_ALIs;

               if not Opt.Quiet_Output then

                  --  Check that the interface set is complete: any unit in the
                  --  library that is needed by an interface should also be an
                  --  interface. If it is not the case, output a warning.

                  while Iface /= Nil_String loop
                     ALI :=
                       File_Name_Type
                         (In_Tree.String_Elements.Table (Iface).Value);
                     Process (ALI);
                     Iface := In_Tree.String_Elements.Table (Iface).Next;
                  end loop;
               end if;
            end;
         end if;

         declare
            Current_Dir  : constant String := Get_Current_Dir;
            Dir          : Dir_Type;

            Name : String (1 .. 200);
            Last : Natural;

            Disregard : Boolean;
            pragma Warnings (Off, Disregard);

            DLL_Name : aliased constant String :=
                         Lib_Filename.all & "." & DLL_Ext;

            Archive_Name : aliased constant String :=
                             Lib_Filename.all & "." & Archive_Ext;

            Delete : Boolean := False;

         begin
            --  Clean the library directory: remove any file with the name of
            --  the library file and any ALI file of a source of the project.

            begin
               Get_Name_String (For_Project.Library_Dir.Name);
               Change_Dir (Name_Buffer (1 .. Name_Len));

            exception
               when others =>
                  Com.Fail
                    ("unable to access library directory """
                     & Name_Buffer (1 .. Name_Len)
                     & """");
            end;

            Open (Dir, ".");

            loop
               Read (Dir, Name, Last);
               exit when Last = 0;

               declare
                  Filename : constant String := Name (1 .. Last);

               begin
                  if Is_Regular_File (Filename) then
                     Canonical_Case_File_Name (Name (1 .. Last));
                     Delete := False;

                     if (The_Build_Mode = Static
                          and then Name (1 .. Last) =  Archive_Name)
                       or else
                         ((The_Build_Mode = Dynamic
                            or else
                           The_Build_Mode = Relocatable)
                          and then Name (1 .. Last) = DLL_Name)
                     then
                        Delete := True;

                     elsif Last > 4
                       and then Name (Last - 3 .. Last) = ".ali"
                     then
                        declare
                           Unit : Unit_Index;

                        begin
                           --  Compare with ALI file names of the project

                           Unit := Units_Htable.Get_First (In_Tree.Units_HT);
                           while Unit /= No_Unit_Index loop
                              if Unit.File_Names (Impl) /= null
                                and then Unit.File_Names (Impl).Project /=
                                                                 No_Project
                              then
                                 if Ultimate_Extending_Project_Of
                                      (Unit.File_Names (Impl).Project) =
                                                                 For_Project
                                 then
                                    Get_Name_String
                                      (Unit.File_Names (Impl).File);
                                    Name_Len :=
                                      Name_Len -
                                        File_Extension
                                          (Name (1 .. Name_Len))'Length;

                                    if Name_Buffer (1 .. Name_Len) =
                                      Name (1 .. Last - 4)
                                    then
                                       Delete := True;
                                       exit;
                                    end if;
                                 end if;

                              elsif Unit.File_Names (Spec) /= null
                                and then Ultimate_Extending_Project_Of
                                           (Unit.File_Names (Spec).Project) =
                                                                   For_Project
                              then
                                 Get_Name_String (Unit.File_Names (Spec).File);
                                 Name_Len :=
                                   Name_Len -
                                     File_Extension (Name (1 .. Last))'Length;

                                 if Name_Buffer (1 .. Name_Len) =
                                      Name (1 .. Last - 4)
                                 then
                                    Delete := True;
                                    exit;
                                 end if;
                              end if;

                              Unit := Units_Htable.Get_Next (In_Tree.Units_HT);
                           end loop;
                        end;
                     end if;

                     if Delete then
                        Set_Writable (Filename);
                        Delete_File (Filename, Disregard);
                     end if;
                  end if;
               end;
            end loop;

            Close (Dir);

            Change_Dir (Current_Dir);
         end;

         --  Call procedure to build the library, depending on the build mode

         case The_Build_Mode is
            when Dynamic | Relocatable =>
               Build_Dynamic_Library
                 (Ofiles        => Object_Files.all,
                  Options       => Options.all,
                  Interfaces    => Arguments (1 .. Argument_Number),
                  Lib_Filename  => Lib_Filename.all,
                  Lib_Dir       => Lib_Dirpath.all,
                  Symbol_Data   => Current_Proj.Symbol_Data,
                  Driver_Name   => Driver_Name,
                  Lib_Version   => Lib_Version.all,
                  Auto_Init     => Current_Proj.Lib_Auto_Init);

            when Static =>
               MLib.Build_Library
                 (Object_Files.all,
                  Lib_Filename.all,
                  Lib_Dirpath.all);

            when None =>
               null;
         end case;

         --  We need to copy the ALI files from the object directory to the
         --  library ALI directory, so that the linker find them there, and
         --  does not need to look in the object directory where it would also
         --  find the object files; and we don't want that: we want the linker
         --  to use the library.

         --  Copy the ALI files and make the copies read-only. For interfaces,
         --  mark the copies as interfaces.

         Copy_ALI_Files
           (Files      => Ali_Files.all,
            To         => For_Project.Library_ALI_Dir.Name,
            Interfaces => Arguments (1 .. Argument_Number));

         --  Copy interface sources if Library_Src_Dir specified

         if Standalone
           and then For_Project.Library_Src_Dir /= No_Path_Information
         then
            --  Clean the interface copy directory: remove any source that
            --  could be a source of the project.

            begin
               Get_Name_String (For_Project.Library_Src_Dir.Name);
               Change_Dir (Name_Buffer (1 .. Name_Len));

            exception
               when others =>
                  Com.Fail
                    ("unable to access library source copy directory """
                     & Name_Buffer (1 .. Name_Len)
                     & """");
            end;

            declare
               Dir    : Dir_Type;
               Delete : Boolean := False;
               Unit   : Unit_Index;

               Name : String (1 .. 200);
               Last : Natural;

               Disregard : Boolean;
               pragma Warnings (Off, Disregard);

            begin
               Open (Dir, ".");

               loop
                  Read (Dir, Name, Last);
                  exit when Last = 0;

                  if Is_Regular_File (Name (1 .. Last)) then
                     Canonical_Case_File_Name (Name (1 .. Last));
                     Delete := False;

                     --  Compare with source file names of the project

                     Unit := Units_Htable.Get_First (In_Tree.Units_HT);
                     while Unit /= No_Unit_Index loop
                        if Unit.File_Names (Impl) /= null
                          and then Ultimate_Extending_Project_Of
                            (Unit.File_Names (Impl).Project) = For_Project
                          and then
                            Get_Name_String
                              (Unit.File_Names (Impl).File) =
                            Name (1 .. Last)
                        then
                           Delete := True;
                           exit;
                        end if;

                        if Unit.File_Names (Spec) /= null
                          and then Ultimate_Extending_Project_Of
                            (Unit.File_Names (Spec).Project) =
                             For_Project
                          and then
                           Get_Name_String
                             (Unit.File_Names (Spec).File) =
                           Name (1 .. Last)
                        then
                           Delete := True;
                           exit;
                        end if;

                        Unit := Units_Htable.Get_Next (In_Tree.Units_HT);
                     end loop;
                  end if;

                  if Delete then
                     Set_Writable (Name (1 .. Last));
                     Delete_File (Name (1 .. Last), Disregard);
                  end if;
               end loop;

               Close (Dir);
            end;

            Copy_Interface_Sources
              (For_Project => For_Project,
               In_Tree     => In_Tree,
               Interfaces  => Arguments (1 .. Argument_Number),
               To_Dir      => For_Project.Library_Src_Dir.Display_Name);
         end if;
      end if;

      --  Reset the current working directory to its previous value

      Change_Dir (Current_Dir);
   end Build_Library;

   -----------
   -- Check --
   -----------

   procedure Check (Filename : String) is
   begin
      if not Is_Regular_File (Filename) then
         Com.Fail (Filename & " not found.");
      end if;
   end Check;

   -------------------
   -- Check_Context --
   -------------------

   procedure Check_Context is
   begin
      --  Check that each object file exists

      for F in Object_Files'Range loop
         Check (Object_Files (F).all);
      end loop;
   end Check_Context;

   -------------------
   -- Check_Library --
   -------------------

   procedure Check_Library
     (For_Project : Project_Id; In_Tree : Project_Tree_Ref)
   is
      Lib_TS  : Time_Stamp_Type;
      Current : constant Dir_Name_Str := Get_Current_Dir;

   begin
      --  No need to build the library if there is no object directory,
      --  hence no object files to build the library.

      if For_Project.Library then
         declare
            Lib_Name : constant File_Name_Type :=
                         Library_File_Name_For (For_Project, In_Tree);
         begin
            Change_Dir (Get_Name_String (For_Project.Library_Dir.Name));
            Lib_TS := File_Stamp (Lib_Name);
            For_Project.Library_TS := Lib_TS;
         end;

         if not For_Project.Externally_Built
           and then not For_Project.Need_To_Build_Lib
           and then For_Project.Object_Directory /= No_Path_Information
         then
            declare
               Obj_TS     : Time_Stamp_Type;
               Object_Dir : Dir_Type;

            begin
               if OpenVMS_On_Target then
                  B_Start := new String'("b__");
               end if;

               --  If the library file does not exist, then the time stamp will
               --  be Empty_Time_Stamp, earlier than any other time stamp.

               Change_Dir
                 (Get_Name_String (For_Project.Object_Directory.Name));
               Open (Dir => Object_Dir, Dir_Name => ".");

               --  For all entries in the object directory

               loop
                  Read (Object_Dir, Name_Buffer, Name_Len);
                  exit when Name_Len = 0;

                  --  Check if it is an object file, but ignore any binder
                  --  generated file.

                  if Is_Obj (Name_Buffer (1 .. Name_Len))
                    and then Name_Buffer (1 .. B_Start'Length) /= B_Start.all
                  then
                     --  Get the object file time stamp

                     Obj_TS := File_Stamp (File_Name_Type'(Name_Find));

                     --  If library file time stamp is earlier, set
                     --  Need_To_Build_Lib and return. String comparison is
                     --  used, otherwise time stamps may be too close and the
                     --  comparison would return True, which would trigger
                     --  an unnecessary rebuild of the library.

                     if String (Lib_TS) < String (Obj_TS) then

                        --  Library must be rebuilt

                        For_Project.Need_To_Build_Lib := True;
                        exit;
                     end if;
                  end if;
               end loop;

               Close (Object_Dir);
            end;
         end if;

         Change_Dir (Current);
      end if;
   end Check_Library;

   ----------------------------
   -- Copy_Interface_Sources --
   ----------------------------

   procedure Copy_Interface_Sources
     (For_Project : Project_Id;
      In_Tree     : Project_Tree_Ref;
      Interfaces  : Argument_List;
      To_Dir      : Path_Name_Type)
   is
      Current : constant Dir_Name_Str := Get_Current_Dir;
      --  The current directory, where to return to at the end

      Target : constant Dir_Name_Str := Get_Name_String (To_Dir);
      --  The directory where to copy sources

      Text     : Text_Buffer_Ptr;
      The_ALI  : ALI.ALI_Id;
      Lib_File : File_Name_Type;

      First_Unit  : ALI.Unit_Id;
      Second_Unit : ALI.Unit_Id;

      Copy_Subunits : Boolean := False;
      --  When True, indicates that subunits, if any, need to be copied too

      procedure Copy (File_Name : File_Name_Type);
      --  Copy one source of the project to the target directory

      ----------
      -- Copy --
      ----------

      procedure Copy (File_Name : File_Name_Type) is
         Success : Boolean;
         pragma Warnings (Off, Success);

         Source : Standard.Prj.Source_Id;
      begin
         Source := Find_Source
           (In_Tree, For_Project,
            In_Extended_Only => True,
            Base_Name => File_Name);

         if Source /= No_Source
           and then not Source.Locally_Removed
           and then Source.Replaced_By = No_Source
         then
            Copy_File
              (Get_Name_String (Source.Path.Name),
               Target,
               Success,
               Mode     => Overwrite,
               Preserve => Preserve);
         end if;
      end Copy;

   --  Start of processing for Copy_Interface_Sources

   begin
      --  Change the working directory to the object directory

      Change_Dir (Get_Name_String (For_Project.Object_Directory.Name));

      for Index in Interfaces'Range loop

         --  First, load the ALI file

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Interfaces (Index).all);
         Lib_File := Name_Find;
         Text := Read_Library_Info (Lib_File);
         The_ALI := Scan_ALI (Lib_File, Text, Ignore_ED => False, Err => True);
         Free (Text);

         Second_Unit := No_Unit_Id;
         First_Unit := ALI.ALIs.Table (The_ALI).First_Unit;
         Copy_Subunits := True;

         --  If there is both a spec and a body, check if they are both needed

         if ALI.Units.Table (First_Unit).Utype = Is_Body then
            Second_Unit := ALI.ALIs.Table (The_ALI).Last_Unit;

            --  If the body is not needed, then reset First_Unit

            if not ALI.Units.Table (Second_Unit).Body_Needed_For_SAL then
               First_Unit := No_Unit_Id;
               Copy_Subunits := False;
            end if;

         elsif ALI.Units.Table (First_Unit).Utype = Is_Spec_Only then
            Copy_Subunits := False;
         end if;

         --  Copy the file(s) that need to be copied

         if First_Unit /= No_Unit_Id then
            Copy (File_Name => ALI.Units.Table (First_Unit).Sfile);
         end if;

         if Second_Unit /= No_Unit_Id then
            Copy (File_Name => ALI.Units.Table (Second_Unit).Sfile);
         end if;

         --  Copy all the separates, if any

         if Copy_Subunits then
            for Dep in ALI.ALIs.Table (The_ALI).First_Sdep ..
              ALI.ALIs.Table (The_ALI).Last_Sdep
            loop
               if Sdep.Table (Dep).Subunit_Name /= No_Name then
                  Copy (File_Name => Sdep.Table (Dep).Sfile);
               end if;
            end loop;
         end if;
      end loop;

      --  Restore the initial working directory

      Change_Dir (Current);
   end Copy_Interface_Sources;

   -------------
   -- Display --
   -------------

   procedure Display (Executable : String) is
   begin
      if not Opt.Quiet_Output then
         Write_Str (Executable);

         for Index in 1 .. Argument_Number loop
            Write_Char (' ');
            Write_Str (Arguments (Index).all);
         end loop;

         Write_Eol;
      end if;
   end Display;

   -----------
   -- Index --
   -----------

   function Index (S, Pattern : String) return Natural is
      Len : constant Natural := Pattern'Length;

   begin
      for J in reverse S'First .. S'Last - Len + 1 loop
         if Pattern = S (J .. J + Len - 1) then
            return J;
         end if;
      end loop;

      return 0;
   end Index;

   -------------------------
   -- Process_Binder_File --
   -------------------------

   procedure Process_Binder_File (Name : String) is
      Fd : FILEs;
      --  Binder file's descriptor

      Read_Mode : constant String := "r" & ASCII.NUL;
      --  For fopen

      Status : Interfaces.C_Streams.int;
      pragma Unreferenced (Status);
      --  For fclose

      Begin_Info : constant String := "--  BEGIN Object file/option list";
      End_Info   : constant String := "--  END Object file/option list   ";

      Next_Line : String (1 .. 1000);
      --  Current line value
      --  Where does this odd constant 1000 come from, looks suspicious ???

      Nlast : Integer;
      --  End of line slice (the slice does not contain the line terminator)

      procedure Get_Next_Line;
      --  Read the next line from the binder file without the line terminator

      -------------------
      -- Get_Next_Line --
      -------------------

      procedure Get_Next_Line is
         Fchars : chars;

      begin
         Fchars := fgets (Next_Line'Address, Next_Line'Length, Fd);

         if Fchars = System.Null_Address then
            Fail ("Error reading binder output");
         end if;

         Nlast := 1;
         while Nlast <= Next_Line'Last
           and then Next_Line (Nlast) /= ASCII.LF
           and then Next_Line (Nlast) /= ASCII.CR
         loop
            Nlast := Nlast + 1;
         end loop;

         Nlast := Nlast - 1;
      end Get_Next_Line;

   --  Start of processing for Process_Binder_File

   begin
      Fd := fopen (Name'Address, Read_Mode'Address);

      if Fd = NULL_Stream then
         Fail ("Failed to open binder output");
      end if;

      --  Skip up to the Begin Info line

      loop
         Get_Next_Line;
         exit when Next_Line (1 .. Nlast) = Begin_Info;
      end loop;

      --  Find the first switch

      loop
         Get_Next_Line;

         exit when Next_Line (1 .. Nlast) = End_Info;

         --  As the binder generated file is in Ada, remove the first eight
         --  characters "   --   ".

         Next_Line (1 .. Nlast - 8) := Next_Line (9 .. Nlast);
         Nlast := Nlast - 8;

         --  Stop when the first switch is found

         exit when Next_Line (1) = '-';
      end loop;

      if Next_Line (1 .. Nlast) /= End_Info then
         loop
            --  Ignore -static and -shared, since -shared will be used
            --  in any case.

            --  Ignore -lgnat, -lgnarl and -ldecgnat as they will be added
            --  later, because they are also needed for non Stand-Alone shared
            --  libraries.

            --  Also ignore the shared libraries which are :

            --  UNIX / Windows    VMS
            --  -lgnat-<version>  -lgnat_<version>  (7 + version'length chars)
            --  -lgnarl-<version> -lgnarl_<version> (8 + version'length chars)

            if Next_Line (1 .. Nlast) /= "-static" and then
               Next_Line (1 .. Nlast) /= "-shared" and then
               Next_Line (1 .. Nlast) /= "-ldecgnat" and then
               Next_Line (1 .. Nlast) /= "-lgnarl" and then
               Next_Line (1 .. Nlast) /= "-lgnat" and then
               Next_Line
                 (1 .. Natural'Min (Nlast, 10 + Library_Version'Length)) /=
                   Shared_Lib ("decgnat") and then
               Next_Line
                 (1 .. Natural'Min (Nlast, 8 + Library_Version'Length)) /=
                   Shared_Lib ("gnarl") and then
               Next_Line
                 (1 .. Natural'Min (Nlast, 7 + Library_Version'Length)) /=
                   Shared_Lib ("gnat")
            then
               if Next_Line (1) /= '-' then

                  --  This is not an option, should we add it?

                  if Add_Object_Files then
                     Opts.Increment_Last;
                     Opts.Table (Opts.Last) :=
                       new String'(Next_Line (1 .. Nlast));
                  end if;

               else
                  --  Add all other options

                  Opts.Increment_Last;
                  Opts.Table (Opts.Last) :=
                    new String'(Next_Line (1 .. Nlast));
               end if;
            end if;

            --  Next option, if any

            Get_Next_Line;
            exit when Next_Line (1 .. Nlast) = End_Info;

            --  Remove first eight characters "   --   "

            Next_Line (1 .. Nlast - 8) := Next_Line (9 .. Nlast);
            Nlast := Nlast - 8;
         end loop;
      end if;

      Status := fclose (Fd);

      --  Is it really right to ignore any close error ???

   end Process_Binder_File;

   ------------------
   -- Reset_Tables --
   ------------------

   procedure Reset_Tables is
   begin
      Objects.Init;
      Objects_Htable.Reset;
      ALIs.Init;
      Opts.Init;
      Processed_Projects.Reset;
      Library_Projs.Init;
   end Reset_Tables;

   ---------------------------
   -- SALs_Use_Constructors --
   ---------------------------

   function SALs_Use_Constructors return Boolean is
      function C_SALs_Init_Using_Constructors return Integer;
      pragma Import (C, C_SALs_Init_Using_Constructors,
                     "__gnat_sals_init_using_constructors");
   begin
      return C_SALs_Init_Using_Constructors /= 0;
   end SALs_Use_Constructors;

end MLib.Prj;
