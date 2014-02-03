------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . A T T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2013, Free Software Foundation, Inc.         --
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

with Osint;
with Prj.Com; use Prj.Com;

with GNAT.Case_Util; use GNAT.Case_Util;

package body Prj.Attr is

   use GNAT;

   --  Data for predefined attributes and packages

   --  Names are in lower case and end with '#'

   --  Package names are preceded by 'P'

   --  Attribute names are preceded by two or three letters:

   --  The first letter is one of
   --    'S' for Single
   --    's' for Single with optional index
   --    'L' for List
   --    'l' for List of strings with optional indexes

   --  The second letter is one of
   --    'V' for single variable
   --    'A' for associative array
   --    'a' for case insensitive associative array
   --    'b' for associative array, case insensitive if file names are case
   --        insensitive
   --    'c' same as 'b', with optional index

   --  The third optional letter is
   --     'R' to indicate that the attribute is read-only
   --     'O' to indicate that others is allowed as an index for an associative
   --     array

   --  End is indicated by two consecutive '#'

   Initialization_Data : constant String :=

   --  project level attributes

   --  General

   "SVRname#" &
   "SVRproject_dir#" &
   "lVmain#" &
   "LVlanguages#" &
   "Lbroots#" &
   "SVexternally_built#" &

   --  Directories

   "SVobject_dir#" &
   "SVexec_dir#" &
   "LVsource_dirs#" &
   "Lainherit_source_path#" &
   "LVexcluded_source_dirs#" &
   "LVignore_source_sub_dirs#" &

   --  Source files

   "LVsource_files#" &
   "LVlocally_removed_files#" &
   "LVexcluded_source_files#" &
   "SVsource_list_file#" &
   "SVexcluded_source_list_file#" &
   "LVinterfaces#" &

   --  Projects (in aggregate projects)

   "LVproject_files#" &
   "LVproject_path#" &
   "SAexternal#" &

   --  Libraries

   "SVlibrary_dir#" &
   "SVlibrary_name#" &
   "SVlibrary_kind#" &
   "SVlibrary_version#" &
   "LVlibrary_interface#" &
   "SVlibrary_standalone#" &
   "LVlibrary_encapsulated_options#" &
   "SVlibrary_encapsulated_supported#" &
   "SVlibrary_auto_init#" &
   "LVleading_library_options#" &
   "LVlibrary_options#" &
   "Lalibrary_rpath_options#" &
   "SVlibrary_src_dir#" &
   "SVlibrary_ali_dir#" &
   "SVlibrary_gcc#" &
   "SVlibrary_symbol_file#" &
   "SVlibrary_symbol_policy#" &
   "SVlibrary_reference_symbol_file#" &

   --  Configuration - General

   "SVdefault_language#" &
   "LVrun_path_option#" &
   "SVrun_path_origin#" &
   "SVseparate_run_path_options#" &
   "Satoolchain_version#" &
   "Satoolchain_description#" &
   "Saobject_generated#" &
   "Saobjects_linked#" &
   "SVtarget#" &

   --  Configuration - Libraries

   "SVlibrary_builder#" &
   "SVlibrary_support#" &

   --  Configuration - Archives

   "LVarchive_builder#" &
   "LVarchive_builder_append_option#" &
   "LVarchive_indexer#" &
   "SVarchive_suffix#" &
   "LVlibrary_partial_linker#" &

   --  Configuration - Shared libraries

   "SVshared_library_prefix#" &
   "SVshared_library_suffix#" &
   "SVsymbolic_link_supported#" &
   "SVlibrary_major_minor_id_supported#" &
   "SVlibrary_auto_init_supported#" &
   "LVshared_library_minimum_switches#" &
   "LVlibrary_version_switches#" &
   "SVlibrary_install_name_option#" &
   "Saruntime_library_dir#" &
   "Saruntime_source_dir#" &

   --  package Naming
   --  Some attributes are obsolescent, and renamed in the tree (see
   --  Prj.Dect.Rename_Obsolescent_Attributes).

   "Pnaming#" &
   "Saspecification_suffix#" &  --  Always renamed to "spec_suffix" in tree
   "Saspec_suffix#" &
   "Saimplementation_suffix#" & --  Always renamed to "body_suffix" in tree
   "Sabody_suffix#" &
   "SVseparate_suffix#" &
   "SVcasing#" &
   "SVdot_replacement#" &
   "saspecification#" &  --  Always renamed to "spec" in project tree
   "saspec#" &
   "saimplementation#" & --  Always renamed to "body" in project tree
   "sabody#" &
   "Laspecification_exceptions#" &
   "Laimplementation_exceptions#" &

   --  package Compiler

   "Pcompiler#" &
   "Ladefault_switches#" &
   "LcOswitches#" &
   "SVlocal_configuration_pragmas#" &
   "Salocal_config_file#" &

   --  Configuration - Compiling

   "Sadriver#" &
   "Salanguage_kind#" &
   "Sadependency_kind#" &
   "Larequired_switches#" &
   "Laleading_required_switches#" &
   "Latrailing_required_switches#" &
   "Lapic_option#" &
   "Sapath_syntax#" &
   "Lasource_file_switches#" &
   "Saobject_file_suffix#" &
   "Laobject_file_switches#" &
   "Lamulti_unit_switches#" &
   "Samulti_unit_object_separator#" &

   --  Configuration - Mapping files

   "Lamapping_file_switches#" &
   "Samapping_spec_suffix#" &
   "Samapping_body_suffix#" &

   --  Configuration - Config files

   "Laconfig_file_switches#" &
   "Saconfig_body_file_name#" &
   "Saconfig_body_file_name_index#" &
   "Saconfig_body_file_name_pattern#" &
   "Saconfig_spec_file_name#" &
   "Saconfig_spec_file_name_index#" &
   "Saconfig_spec_file_name_pattern#" &
   "Saconfig_file_unique#" &

   --  Configuration - Dependencies

   "Ladependency_switches#" &
   "Ladependency_driver#" &

   --  Configuration - Search paths

   "Lainclude_switches#" &
   "Sainclude_path#" &
   "Sainclude_path_file#" &
   "Laobject_path_switches#" &

   --  package Builder

   "Pbuilder#" &
   "Ladefault_switches#" &
   "LcOswitches#" &
   "Lcglobal_compilation_switches#" &
   "Scexecutable#" &
   "SVexecutable_suffix#" &
   "SVglobal_configuration_pragmas#" &
   "Saglobal_config_file#" &

   --  package gnatls

   "Pgnatls#" &
   "LVswitches#" &

   --  package Binder

   "Pbinder#" &
   "Ladefault_switches#" &
   "LcOswitches#" &

   --  Configuration - Binding

   "Sadriver#" &
   "Larequired_switches#" &
   "Saprefix#" &
   "Saobjects_path#" &
   "Saobjects_path_file#" &

   --  package Linker

   "Plinker#" &
   "LVrequired_switches#" &
   "Ladefault_switches#" &
   "LcOleading_switches#" &
   "LcOswitches#" &
   "LcOtrailing_switches#" &
   "LVlinker_options#" &
   "SVmap_file_option#" &

   --  Configuration - Linking

   "SVdriver#" &
   "LVexecutable_switch#" &
   "SVlib_dir_switch#" &
   "SVlib_name_switch#" &

   --  Configuration - Response files

   "SVmax_command_line_length#" &
   "SVresponse_file_format#" &
   "LVresponse_file_switches#" &

   --  package Clean

   "Pclean#" &
   "LVswitches#" &
   "Lasource_artifact_extensions#" &
   "Laobject_artifact_extensions#" &
   "LVartifacts_in_exec_dir#" &
   "LVartifacts_in_object_dir#" &

   --  package Cross_Reference

   "Pcross_reference#" &
   "Ladefault_switches#" &
   "LbOswitches#" &

   --  package Finder

   "Pfinder#" &
   "Ladefault_switches#" &
   "LbOswitches#" &

   --  package Pretty_Printer

   "Ppretty_printer#" &
   "Ladefault_switches#" &
   "LbOswitches#" &

   --  package gnatstub

   "Pgnatstub#" &
   "Ladefault_switches#" &
   "LbOswitches#" &

   --  package Check

   "Pcheck#" &
   "Ladefault_switches#" &
   "LbOswitches#" &

   --  package Synchronize

   "Psynchronize#" &
   "Ladefault_switches#" &
   "LbOswitches#" &

   --  package Eliminate

   "Peliminate#" &
   "Ladefault_switches#" &
   "LbOswitches#" &

   --  package Metrics

   "Pmetrics#" &
   "Ladefault_switches#" &
   "LbOswitches#" &

   --  package Ide

   "Pide#" &
   "Ladefault_switches#" &
   "SVremote_host#" &
   "SVprogram_host#" &
   "SVcommunication_protocol#" &
   "Sacompiler_command#" &
   "SVdebugger_command#" &
   "SVgnatlist#" &
   "SVvcs_kind#" &
   "SVvcs_file_check#" &
   "SVvcs_log_check#" &
   "SVdocumentation_dir#" &

   --  package Install

   "Pinstall#" &
   "SVprefix#" &
   "SVsources_subdir#" &
   "SVexec_subdir#" &
   "SVlib_subdir#" &
   "SVproject_subdir#" &
   "SVactive#" &

   --  package Remote

   "Premote#" &
   "SVroot_dir#" &
   "LVexcluded_patterns#" &

   --  package Stack

   "Pstack#" &
   "LVswitches#" &

   "#";

   Initialized : Boolean := False;
   --  A flag to avoid multiple initialization

   Package_Names     : String_List_Access := new Strings.String_List (1 .. 20);
   Last_Package_Name : Natural := 0;
   --  Package_Names (1 .. Last_Package_Name) contains the list of the known
   --  package names, coming from the Initialization_Data string or from
   --  calls to one of the two procedures Register_New_Package.

   procedure Add_Package_Name (Name : String);
   --  Add a package name in the Package_Name list, extending it, if necessary

   function Name_Id_Of (Name : String) return Name_Id;
   --  Returns the Name_Id for Name in lower case

   ----------------------
   -- Add_Package_Name --
   ----------------------

   procedure Add_Package_Name (Name : String) is
   begin
      if Last_Package_Name = Package_Names'Last then
         declare
            New_List : constant Strings.String_List_Access :=
                         new Strings.String_List (1 .. Package_Names'Last * 2);
         begin
            New_List (Package_Names'Range) := Package_Names.all;
            Package_Names := New_List;
         end;
      end if;

      Last_Package_Name := Last_Package_Name + 1;
      Package_Names (Last_Package_Name) := new String'(Name);
   end Add_Package_Name;

   -----------------------
   -- Attribute_Kind_Of --
   -----------------------

   function Attribute_Kind_Of
     (Attribute : Attribute_Node_Id) return Attribute_Kind
   is
   begin
      if Attribute = Empty_Attribute then
         return Unknown;
      else
         return Attrs.Table (Attribute.Value).Attr_Kind;
      end if;
   end Attribute_Kind_Of;

   -----------------------
   -- Attribute_Name_Of --
   -----------------------

   function Attribute_Name_Of (Attribute : Attribute_Node_Id) return Name_Id is
   begin
      if Attribute = Empty_Attribute then
         return No_Name;
      else
         return Attrs.Table (Attribute.Value).Name;
      end if;
   end Attribute_Name_Of;

   --------------------------
   -- Attribute_Node_Id_Of --
   --------------------------

   function Attribute_Node_Id_Of
     (Name        : Name_Id;
      Starting_At : Attribute_Node_Id) return Attribute_Node_Id
   is
      Id : Attr_Node_Id := Starting_At.Value;

   begin
      while Id /= Empty_Attr
        and then Attrs.Table (Id).Name /= Name
      loop
         Id := Attrs.Table (Id).Next;
      end loop;

      return (Value => Id);
   end Attribute_Node_Id_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Start             : Positive          := Initialization_Data'First;
      Finish            : Positive          := Start;
      Current_Package   : Pkg_Node_Id       := Empty_Pkg;
      Current_Attribute : Attr_Node_Id      := Empty_Attr;
      Is_An_Attribute   : Boolean           := False;
      Var_Kind          : Variable_Kind     := Undefined;
      Optional_Index    : Boolean           := False;
      Attr_Kind         : Attribute_Kind    := Single;
      Package_Name      : Name_Id           := No_Name;
      Attribute_Name    : Name_Id           := No_Name;
      First_Attribute   : Attr_Node_Id      := Attr.First_Attribute;
      Read_Only         : Boolean;
      Others_Allowed    : Boolean;

      function Attribute_Location return String;
      --  Returns a string depending if we are in the project level attributes
      --  or in the attributes of a package.

      ------------------------
      -- Attribute_Location --
      ------------------------

      function Attribute_Location return String is
      begin
         if Package_Name = No_Name then
            return "project level attributes";

         else
            return "attribute of package """ &
            Get_Name_String (Package_Name) & """";
         end if;
      end Attribute_Location;

   --  Start of processing for Initialize

   begin
      --  Don't allow Initialize action to be repeated

      if Initialized then
         return;
      end if;

      --  Make sure the two tables are empty

      Attrs.Init;
      Package_Attributes.Init;

      while Initialization_Data (Start) /= '#' loop
         Is_An_Attribute := True;
         case Initialization_Data (Start) is
            when 'P' =>

               --  New allowed package

               Start := Start + 1;

               Finish := Start;
               while Initialization_Data (Finish) /= '#' loop
                  Finish := Finish + 1;
               end loop;

               Package_Name :=
                 Name_Id_Of (Initialization_Data (Start .. Finish - 1));

               for Index in First_Package .. Package_Attributes.Last loop
                  if Package_Name = Package_Attributes.Table (Index).Name then
                     Osint.Fail ("duplicate name """
                                 & Initialization_Data (Start .. Finish - 1)
                                 & """ in predefined packages.");
                  end if;
               end loop;

               Is_An_Attribute := False;
               Current_Attribute := Empty_Attr;
               Package_Attributes.Increment_Last;
               Current_Package := Package_Attributes.Last;
               Package_Attributes.Table (Current_Package) :=
                 (Name             => Package_Name,
                  Known            => True,
                  First_Attribute  => Empty_Attr);
               Start := Finish + 1;

               Add_Package_Name (Get_Name_String (Package_Name));

            when 'S' =>
               Var_Kind       := Single;
               Optional_Index := False;

            when 's' =>
               Var_Kind       := Single;
               Optional_Index := True;

            when 'L' =>
               Var_Kind       := List;
               Optional_Index := False;

            when 'l' =>
               Var_Kind         := List;
               Optional_Index := True;

            when others =>
               raise Program_Error;
         end case;

         if Is_An_Attribute then

            --  New attribute

            Start := Start + 1;
            case Initialization_Data (Start) is
               when 'V' =>
                  Attr_Kind := Single;

               when 'A' =>
                  Attr_Kind := Associative_Array;

               when 'a' =>
                  Attr_Kind := Case_Insensitive_Associative_Array;

               when 'b' =>
                  if Osint.File_Names_Case_Sensitive then
                     Attr_Kind := Associative_Array;
                  else
                     Attr_Kind := Case_Insensitive_Associative_Array;
                  end if;

               when 'c' =>
                  if Osint.File_Names_Case_Sensitive then
                     Attr_Kind := Optional_Index_Associative_Array;
                  else
                     Attr_Kind :=
                       Optional_Index_Case_Insensitive_Associative_Array;
                  end if;

               when others =>
                  raise Program_Error;
            end case;

            Start := Start + 1;

            Read_Only := False;
            Others_Allowed := False;

            if Initialization_Data (Start) = 'R' then
               Read_Only := True;
               Start := Start + 1;

            elsif Initialization_Data (Start) = 'O' then
               Others_Allowed := True;
               Start := Start + 1;
            end if;

            Finish := Start;

            while Initialization_Data (Finish) /= '#' loop
               Finish := Finish + 1;
            end loop;

            Attribute_Name :=
              Name_Id_Of (Initialization_Data (Start .. Finish - 1));
            Attrs.Increment_Last;

            if Current_Attribute = Empty_Attr then
               First_Attribute := Attrs.Last;

               if Current_Package /= Empty_Pkg then
                  Package_Attributes.Table (Current_Package).First_Attribute
                    := Attrs.Last;
               end if;

            else
               --  Check that there are no duplicate attributes

               for Index in First_Attribute .. Attrs.Last - 1 loop
                  if Attribute_Name = Attrs.Table (Index).Name then
                     Osint.Fail ("duplicate attribute """
                                 & Initialization_Data (Start .. Finish - 1)
                                 & """ in " & Attribute_Location);
                  end if;
               end loop;

               Attrs.Table (Current_Attribute).Next :=
                 Attrs.Last;
            end if;

            Current_Attribute := Attrs.Last;
            Attrs.Table (Current_Attribute) :=
              (Name           => Attribute_Name,
               Var_Kind       => Var_Kind,
               Optional_Index => Optional_Index,
               Attr_Kind      => Attr_Kind,
               Read_Only      => Read_Only,
               Others_Allowed => Others_Allowed,
               Next           => Empty_Attr);
            Start := Finish + 1;
         end if;
      end loop;

      Initialized := True;
   end Initialize;

   ------------------
   -- Is_Read_Only --
   ------------------

   function Is_Read_Only (Attribute : Attribute_Node_Id) return Boolean is
   begin
      return Attrs.Table (Attribute.Value).Read_Only;
   end Is_Read_Only;

   ----------------
   -- Name_Id_Of --
   ----------------

   function Name_Id_Of (Name : String) return Name_Id is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      To_Lower (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end Name_Id_Of;

   --------------------
   -- Next_Attribute --
   --------------------

   function Next_Attribute
     (After : Attribute_Node_Id) return Attribute_Node_Id
   is
   begin
      if After = Empty_Attribute then
         return Empty_Attribute;
      else
         return (Value => Attrs.Table (After.Value).Next);
      end if;
   end Next_Attribute;

   -----------------------
   -- Optional_Index_Of --
   -----------------------

   function Optional_Index_Of (Attribute : Attribute_Node_Id) return Boolean is
   begin
      if Attribute = Empty_Attribute then
         return False;
      else
         return Attrs.Table (Attribute.Value).Optional_Index;
      end if;
   end Optional_Index_Of;

   function Others_Allowed_For
     (Attribute : Attribute_Node_Id) return Boolean
   is
   begin
      if Attribute = Empty_Attribute then
         return False;
      else
         return Attrs.Table (Attribute.Value).Others_Allowed;
      end if;
   end Others_Allowed_For;

   -----------------------
   -- Package_Name_List --
   -----------------------

   function Package_Name_List return Strings.String_List is
   begin
      return Package_Names (1 .. Last_Package_Name);
   end Package_Name_List;

   ------------------------
   -- Package_Node_Id_Of --
   ------------------------

   function Package_Node_Id_Of (Name : Name_Id) return Package_Node_Id is
   begin
      for Index in Package_Attributes.First .. Package_Attributes.Last loop
         if Package_Attributes.Table (Index).Name = Name then
            if Package_Attributes.Table (Index).Known then
               return (Value => Index);
            else
               return Unknown_Package;
            end if;
         end if;
      end loop;

      --  If there is no package with this name, return Empty_Package

      return Empty_Package;
   end Package_Node_Id_Of;

   ----------------------------
   -- Register_New_Attribute --
   ----------------------------

   procedure Register_New_Attribute
     (Name               : String;
      In_Package         : Package_Node_Id;
      Attr_Kind          : Defined_Attribute_Kind;
      Var_Kind           : Defined_Variable_Kind;
      Index_Is_File_Name : Boolean := False;
      Opt_Index          : Boolean := False)
   is
      Attr_Name       : Name_Id;
      First_Attr      : Attr_Node_Id := Empty_Attr;
      Curr_Attr       : Attr_Node_Id;
      Real_Attr_Kind  : Attribute_Kind;

   begin
      if Name'Length = 0 then
         Fail ("cannot register an attribute with no name");
         raise Project_Error;
      end if;

      if In_Package = Empty_Package then
         Fail ("attempt to add attribute """
               & Name
               & """ to an undefined package");
         raise Project_Error;
      end if;

      Attr_Name := Name_Id_Of (Name);

      First_Attr :=
        Package_Attributes.Table (In_Package.Value).First_Attribute;

      --  Check if attribute name is a duplicate

      Curr_Attr := First_Attr;
      while Curr_Attr /= Empty_Attr loop
         if Attrs.Table (Curr_Attr).Name = Attr_Name then
            Fail ("duplicate attribute name """
                  & Name
                  & """ in package """
                  & Get_Name_String
                     (Package_Attributes.Table (In_Package.Value).Name)
                  & """");
            raise Project_Error;
         end if;

         Curr_Attr := Attrs.Table (Curr_Attr).Next;
      end loop;

      Real_Attr_Kind := Attr_Kind;

      --  If Index_Is_File_Name, change the attribute kind if necessary

      if Index_Is_File_Name and then not Osint.File_Names_Case_Sensitive then
         case Attr_Kind is
            when Associative_Array =>
               Real_Attr_Kind := Case_Insensitive_Associative_Array;

            when Optional_Index_Associative_Array =>
               Real_Attr_Kind :=
                 Optional_Index_Case_Insensitive_Associative_Array;

            when others =>
               null;
         end case;
      end if;

      --  Add the new attribute

      Attrs.Increment_Last;
      Attrs.Table (Attrs.Last) :=
        (Name           => Attr_Name,
         Var_Kind       => Var_Kind,
         Optional_Index => Opt_Index,
         Attr_Kind      => Real_Attr_Kind,
         Read_Only      => False,
         Others_Allowed => False,
         Next           => First_Attr);

      Package_Attributes.Table (In_Package.Value).First_Attribute :=
        Attrs.Last;
   end Register_New_Attribute;

   --------------------------
   -- Register_New_Package --
   --------------------------

   procedure Register_New_Package (Name : String; Id : out Package_Node_Id) is
      Pkg_Name : Name_Id;

   begin
      if Name'Length = 0 then
         Fail ("cannot register a package with no name");
         Id := Empty_Package;
         return;
      end if;

      Pkg_Name := Name_Id_Of (Name);

      for Index in Package_Attributes.First .. Package_Attributes.Last loop
         if Package_Attributes.Table (Index).Name = Pkg_Name then
            Fail ("cannot register a package with a non unique name """
                  & Name
                  & """");
            Id := Empty_Package;
            return;
         end if;
      end loop;

      Package_Attributes.Increment_Last;
      Id := (Value => Package_Attributes.Last);
      Package_Attributes.Table (Package_Attributes.Last) :=
        (Name             => Pkg_Name,
         Known            => True,
         First_Attribute  => Empty_Attr);

      Add_Package_Name (Get_Name_String (Pkg_Name));
   end Register_New_Package;

   procedure Register_New_Package
     (Name       : String;
      Attributes : Attribute_Data_Array)
   is
      Pkg_Name   : Name_Id;
      Attr_Name  : Name_Id;
      First_Attr : Attr_Node_Id := Empty_Attr;
      Curr_Attr  : Attr_Node_Id;
      Attr_Kind  : Attribute_Kind;

   begin
      if Name'Length = 0 then
         Fail ("cannot register a package with no name");
         raise Project_Error;
      end if;

      Pkg_Name := Name_Id_Of (Name);

      for Index in Package_Attributes.First .. Package_Attributes.Last loop
         if Package_Attributes.Table (Index).Name = Pkg_Name then
            Fail ("cannot register a package with a non unique name """
                  & Name
                  & """");
            raise Project_Error;
         end if;
      end loop;

      for Index in Attributes'Range loop
         Attr_Name := Name_Id_Of (Attributes (Index).Name);

         Curr_Attr := First_Attr;
         while Curr_Attr /= Empty_Attr loop
            if Attrs.Table (Curr_Attr).Name = Attr_Name then
               Fail ("duplicate attribute name """
                     & Attributes (Index).Name
                     & """ in new package """
                     & Name
                     & """");
               raise Project_Error;
            end if;

            Curr_Attr := Attrs.Table (Curr_Attr).Next;
         end loop;

         Attr_Kind := Attributes (Index).Attr_Kind;

         if Attributes (Index).Index_Is_File_Name
           and then not Osint.File_Names_Case_Sensitive
         then
            case Attr_Kind is
               when Associative_Array =>
                  Attr_Kind := Case_Insensitive_Associative_Array;

               when Optional_Index_Associative_Array =>
                  Attr_Kind :=
                    Optional_Index_Case_Insensitive_Associative_Array;

               when others =>
                  null;
            end case;
         end if;

         Attrs.Increment_Last;
         Attrs.Table (Attrs.Last) :=
           (Name           => Attr_Name,
            Var_Kind       => Attributes (Index).Var_Kind,
            Optional_Index => Attributes (Index).Opt_Index,
            Attr_Kind      => Attr_Kind,
            Read_Only      => False,
            Others_Allowed => False,
            Next           => First_Attr);
         First_Attr := Attrs.Last;
      end loop;

      Package_Attributes.Increment_Last;
      Package_Attributes.Table (Package_Attributes.Last) :=
        (Name             => Pkg_Name,
         Known            => True,
         First_Attribute  => First_Attr);

      Add_Package_Name (Get_Name_String (Pkg_Name));
   end Register_New_Package;

   ---------------------------
   -- Set_Attribute_Kind_Of --
   ---------------------------

   procedure Set_Attribute_Kind_Of
     (Attribute : Attribute_Node_Id;
      To        : Attribute_Kind)
   is
   begin
      if Attribute /= Empty_Attribute then
         Attrs.Table (Attribute.Value).Attr_Kind := To;
      end if;
   end Set_Attribute_Kind_Of;

   --------------------------
   -- Set_Variable_Kind_Of --
   --------------------------

   procedure Set_Variable_Kind_Of
     (Attribute : Attribute_Node_Id;
      To        : Variable_Kind)
   is
   begin
      if Attribute /= Empty_Attribute then
         Attrs.Table (Attribute.Value).Var_Kind := To;
      end if;
   end Set_Variable_Kind_Of;

   ----------------------
   -- Variable_Kind_Of --
   ----------------------

   function Variable_Kind_Of
     (Attribute : Attribute_Node_Id) return Variable_Kind
   is
   begin
      if Attribute = Empty_Attribute then
         return Undefined;
      else
         return Attrs.Table (Attribute.Value).Var_Kind;
      end if;
   end Variable_Kind_Of;

   ------------------------
   -- First_Attribute_Of --
   ------------------------

   function First_Attribute_Of
     (Pkg : Package_Node_Id) return Attribute_Node_Id
   is
   begin
      if Pkg = Empty_Package or else Pkg = Unknown_Package then
         return Empty_Attribute;
      else
         return
           (Value => Package_Attributes.Table (Pkg.Value).First_Attribute);
      end if;
   end First_Attribute_Of;

end Prj.Attr;
