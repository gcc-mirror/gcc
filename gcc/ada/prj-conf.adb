------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . C O N F                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2009, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;  use Ada.Directories;
with GNAT.HTable;      use GNAT.HTable;
with Makeutl;          use Makeutl;
with MLib.Tgt;
with Opt;              use Opt;
with Output;           use Output;
with Prj.Part;
with Prj.PP;
with Prj.Proc;         use Prj.Proc;
with Prj.Tree;         use Prj.Tree;
with Prj.Util;         use Prj.Util;
with Prj;              use Prj;
with Snames;           use Snames;
with System.Case_Util; use System.Case_Util;
with System;

package body Prj.Conf is

   Auto_Cgpr : constant String := "auto.cgpr";

   Default_Name : constant String := "default.cgpr";
   --  Default configuration file that will be used if found

   Config_Project_Env_Var : constant String := "GPR_CONFIG";
   --  Name of the environment variable that provides the name of the
   --  configuration file to use.

   Gprconfig_Name : constant String := "gprconfig";

   package RTS_Languages is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Name_Id,
      No_Element => No_Name,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  Stores the runtime names for the various languages. This is in general
   --  set from a --RTS command line option.

   procedure Add_Attributes
     (Project_Tree : Project_Tree_Ref;
      Conf_Decl    : Declarations;
      User_Decl    : in out Declarations);
   --  Process the attributes in the config declarations.
   --  For single string values, if the attribute is not declared in the user
   --  declarations, declare it with the value in the config declarations.
   --  For string list values, prepend the value in the user declarations with
   --  the value in the config declarations.

   function Locate_Config_File (Name : String) return String_Access;
   --  Search for Name in the config files directory. Return full path if
   --  found, or null otherwise

   function Check_Target
     (Config_File        : Prj.Project_Id;
      Autoconf_Specified : Boolean;
      Project_Tree       : Prj.Project_Tree_Ref;
      Target             : String := "") return Boolean;
   --  Check that the config file's target matches Target.
   --  Target should be set to the empty string when the user did not specify
   --  a target. If the target in the configuration file is invalid, this
   --  function will raise Invalid_Config with an appropriate message.
   --  Autoconf_Specified should be set to True if the user has used --autoconf

   --------------------
   -- Add_Attributes --
   --------------------

   procedure Add_Attributes
     (Project_Tree : Project_Tree_Ref;
      Conf_Decl    : Declarations;
      User_Decl    : in out Declarations)
   is
      Conf_Attr_Id       : Variable_Id;
      Conf_Attr          : Variable;
      Conf_Array_Id      : Array_Id;
      Conf_Array         : Array_Data;
      Conf_Array_Elem_Id : Array_Element_Id;
      Conf_Array_Elem    : Array_Element;
      Conf_List          : String_List_Id;
      Conf_List_Elem     : String_Element;

      User_Attr_Id       : Variable_Id;
      User_Attr          : Variable;
      User_Array_Id      : Array_Id;
      User_Array         : Array_Data;
      User_Array_Elem_Id : Array_Element_Id;
      User_Array_Elem    : Array_Element;

   begin
      Conf_Attr_Id := Conf_Decl.Attributes;
      User_Attr_Id := User_Decl.Attributes;
      while Conf_Attr_Id /= No_Variable loop
         Conf_Attr :=
           Project_Tree.Variable_Elements.Table (Conf_Attr_Id);
         User_Attr :=
           Project_Tree.Variable_Elements.Table (User_Attr_Id);

         if not Conf_Attr.Value.Default then
            if User_Attr.Value.Default then

               --  No attribute declared in user project file: just copy the
               --  value of the configuration attribute.

               User_Attr.Value := Conf_Attr.Value;
               Project_Tree.Variable_Elements.Table (User_Attr_Id) :=
                 User_Attr;

            elsif User_Attr.Value.Kind = List
              and then Conf_Attr.Value.Values /= Nil_String
            then
               --  List attribute declared in both the user project and the
               --  configuration project: prepend the user list with the
               --  configuration list.

               declare
                  Conf_List : String_List_Id := Conf_Attr.Value.Values;
                  Conf_Elem : String_Element;
                  User_List : constant String_List_Id :=
                                User_Attr.Value.Values;
                  New_List : String_List_Id;
                  New_Elem : String_Element;

               begin
                  --  Create new list

                  String_Element_Table.Increment_Last
                    (Project_Tree.String_Elements);
                  New_List := String_Element_Table.Last
                    (Project_Tree.String_Elements);

                  --  Value of attribute is new list

                  User_Attr.Value.Values := New_List;
                  Project_Tree.Variable_Elements.Table (User_Attr_Id) :=
                    User_Attr;

                  loop

                     --  Get each element of configuration list

                     Conf_Elem :=
                       Project_Tree.String_Elements.Table (Conf_List);
                     New_Elem := Conf_Elem;
                     Conf_List := Conf_Elem.Next;

                     if Conf_List = Nil_String then

                        --  If it is the last element in the list, connect to
                        --  first element of user list, and we are done.

                        New_Elem.Next := User_List;
                        Project_Tree.String_Elements.Table
                          (New_List) := New_Elem;
                        exit;

                     else
                        --  If it is not the last element in the list, add to
                        --  new list.

                        String_Element_Table.Increment_Last
                          (Project_Tree.String_Elements);
                        New_Elem.Next :=
                          String_Element_Table.Last
                            (Project_Tree.String_Elements);
                        Project_Tree.String_Elements.Table
                          (New_List) := New_Elem;
                        New_List := New_Elem.Next;
                     end if;
                  end loop;
               end;
            end if;
         end if;

         Conf_Attr_Id := Conf_Attr.Next;
         User_Attr_Id := User_Attr.Next;
      end loop;

      Conf_Array_Id := Conf_Decl.Arrays;
      while Conf_Array_Id /= No_Array loop
         Conf_Array := Project_Tree.Arrays.Table (Conf_Array_Id);

         User_Array_Id := User_Decl.Arrays;
         while User_Array_Id /= No_Array loop
            User_Array := Project_Tree.Arrays.Table (User_Array_Id);
            exit when User_Array.Name = Conf_Array.Name;
            User_Array_Id := User_Array.Next;
         end loop;

         --  If this associative array does not exist in the user project file,
         --  do a shallow copy of the full associative array.

         if User_Array_Id = No_Array then
            Array_Table.Increment_Last (Project_Tree.Arrays);
            User_Array := Conf_Array;
            User_Array.Next := User_Decl.Arrays;
            User_Decl.Arrays := Array_Table.Last (Project_Tree.Arrays);
            Project_Tree.Arrays.Table (User_Decl.Arrays) := User_Array;

         else
            --  Otherwise, check each array element

            Conf_Array_Elem_Id := Conf_Array.Value;
            while Conf_Array_Elem_Id /= No_Array_Element loop
               Conf_Array_Elem :=
                 Project_Tree.Array_Elements.Table (Conf_Array_Elem_Id);

               User_Array_Elem_Id := User_Array.Value;
               while User_Array_Elem_Id /= No_Array_Element loop
                  User_Array_Elem :=
                    Project_Tree.Array_Elements.Table (User_Array_Elem_Id);
                  exit when User_Array_Elem.Index = Conf_Array_Elem.Index;
                  User_Array_Elem_Id := User_Array_Elem.Next;
               end loop;

               --  If the array element does not exist in the user array,
               --  insert a shallow copy of the conf array element in the
               --  user array.

               if User_Array_Elem_Id = No_Array_Element then
                  Array_Element_Table.Increment_Last
                    (Project_Tree.Array_Elements);
                  User_Array_Elem := Conf_Array_Elem;
                  User_Array_Elem.Next := User_Array.Value;
                  User_Array.Value :=
                    Array_Element_Table.Last (Project_Tree.Array_Elements);
                  Project_Tree.Array_Elements.Table (User_Array.Value) :=
                    User_Array_Elem;
                  Project_Tree.Arrays.Table (User_Array_Id) := User_Array;

               --  Otherwise, if the value is a string list, prepend the
               --  user array element with the conf array element value.

               elsif Conf_Array_Elem.Value.Kind = List then
                  Conf_List := Conf_Array_Elem.Value.Values;

                  if Conf_List /= Nil_String then
                     declare
                        Link     : constant String_List_Id :=
                                     User_Array_Elem.Value.Values;
                        Previous : String_List_Id := Nil_String;
                        Next     : String_List_Id;

                     begin
                        loop
                           Conf_List_Elem :=
                             Project_Tree.String_Elements.Table
                               (Conf_List);
                           String_Element_Table.Increment_Last
                             (Project_Tree.String_Elements);
                           Next :=
                             String_Element_Table.Last
                               (Project_Tree.String_Elements);
                           Project_Tree.String_Elements.Table (Next) :=
                             Conf_List_Elem;

                           if Previous = Nil_String then
                              User_Array_Elem.Value.Values := Next;
                              Project_Tree.Array_Elements.Table
                                (User_Array_Elem_Id) := User_Array_Elem;

                           else
                              Project_Tree.String_Elements.Table
                                (Previous).Next := Next;
                           end if;

                           Previous := Next;

                           Conf_List := Conf_List_Elem.Next;

                           if Conf_List = Nil_String then
                              Project_Tree.String_Elements.Table
                                (Previous).Next := Link;
                              exit;
                           end if;
                        end loop;
                     end;
                  end if;
               end if;

               Conf_Array_Elem_Id := Conf_Array_Elem.Next;
            end loop;
         end if;

         Conf_Array_Id := Conf_Array.Next;
      end loop;
   end Add_Attributes;

   ------------------------
   -- Locate_Config_File --
   ------------------------

   function Locate_Config_File (Name : String) return String_Access is
      Prefix_Path : constant String := Executable_Prefix_Path;
   begin
      if Prefix_Path'Length /= 0 then
         return Locate_Regular_File
           (Name,
            "." & Path_Separator &
            Prefix_Path & "share" & Directory_Separator & "gpr");
      else
         return Locate_Regular_File (Name, ".");
      end if;
   end Locate_Config_File;

   ------------------
   -- Check_Target --
   ------------------

   function Check_Target
     (Config_File  : Project_Id;
      Autoconf_Specified : Boolean;
      Project_Tree : Prj.Project_Tree_Ref;
      Target       : String := "") return Boolean
   is
      Variable : constant Variable_Value :=
                   Value_Of
                     (Name_Target, Config_File.Decl.Attributes, Project_Tree);
      Tgt_Name : Name_Id := No_Name;
      OK       : Boolean;

   begin
      if Variable /= Nil_Variable_Value and then not Variable.Default then
         Tgt_Name := Variable.Value;
      end if;

      if Target = "" then
         OK := not Autoconf_Specified or else Tgt_Name = No_Name;
      else
         OK := Tgt_Name /= No_Name
                 and then Target = Get_Name_String (Tgt_Name);
      end if;

      if not OK then
         if Autoconf_Specified then
            if Verbose_Mode then
               Write_Line ("inconsistent targets, performing autoconf");
            end if;

            return False;

         else
            if Tgt_Name /= No_Name then
               raise Invalid_Config
                 with "invalid target name """
                   & Get_Name_String (Tgt_Name) & """ in configuration";

            else
               raise Invalid_Config
                 with "no target specified in configuration file";
            end if;
         end if;
      end if;

      return True;
   end Check_Target;

   --------------------------------------
   -- Get_Or_Create_Configuration_File --
   --------------------------------------

   procedure Get_Or_Create_Configuration_File
     (Project                    : Project_Id;
      Project_Tree               : Project_Tree_Ref;
      Project_Node_Tree          : Prj.Tree.Project_Node_Tree_Ref;
      Allow_Automatic_Generation : Boolean;
      Config_File_Name           : String := "";
      Autoconf_Specified         : Boolean;
      Target_Name                : String := "";
      Normalized_Hostname        : String;
      Packages_To_Check          : String_List_Access := null;
      Config                     : out Prj.Project_Id;
      Config_File_Path           : out String_Access;
      Automatically_Generated    : out Boolean;
      Flags                      : Processing_Flags;
      On_Load_Config             : Config_File_Hook := null)
   is
      function Default_File_Name return String;
      --  Return the name of the default config file that should be tested

      procedure Do_Autoconf;
      --  Generate a new config file through gprconfig.
      --  In case of error, this raises the Invalid_Config exception with an
      --  appropriate message

      function Get_Config_Switches return Argument_List_Access;
      --  Return the --config switches to use for gprconfig

      function Might_Have_Sources (Project : Project_Id) return Boolean;
      --  True if the specified project might have sources (ie the user has not
      --  explicitly specified it. We haven't checked the file system, nor do
      --  we need to at this stage.

      -----------------------
      -- Default_File_Name --
      -----------------------

      function Default_File_Name return String is
         Ada_RTS : constant String := Runtime_Name_For (Name_Ada);
         Tmp     : String_Access;

      begin
         if Target_Name /= "" then
            if Ada_RTS /= "" then
               return Target_Name & '-' & Ada_RTS
                 & Config_Project_File_Extension;
            else
               return Target_Name & Config_Project_File_Extension;
            end if;

         elsif Ada_RTS /= "" then
            return Ada_RTS & Config_Project_File_Extension;

         else
            Tmp := Getenv (Config_Project_Env_Var);

            declare
               T : constant String := Tmp.all;
            begin
               Free (Tmp);

               if T'Length = 0 then
                  return Default_Name;
               else
                  return T;
               end if;
            end;
         end if;
      end Default_File_Name;

      ------------------------
      -- Might_Have_Sources --
      ------------------------

      function Might_Have_Sources (Project : Project_Id) return Boolean is
         Variable : Variable_Value;

      begin
         Variable :=
           Value_Of
             (Name_Source_Dirs,
              Project.Decl.Attributes,
              Project_Tree);

         if Variable = Nil_Variable_Value
           or else Variable.Default
           or else Variable.Values /= Nil_String
         then
            Variable :=
              Value_Of
                (Name_Source_Files,
                 Project.Decl.Attributes,
                 Project_Tree);
            return Variable = Nil_Variable_Value
              or else Variable.Default
              or else Variable.Values /= Nil_String;

         else
            return False;
         end if;
      end Might_Have_Sources;

      -------------------------
      -- Get_Config_Switches --
      -------------------------

      function Get_Config_Switches return Argument_List_Access is
         package Language_Htable is new GNAT.HTable.Simple_HTable
           (Header_Num => Prj.Header_Num,
            Element    => Name_Id,
            No_Element => No_Name,
            Key        => Name_Id,
            Hash       => Prj.Hash,
            Equal      => "=");
         --  Hash table to keep the languages used in the project tree

         IDE : constant Package_Id :=
                 Value_Of
                   (Name_Ide,
                    Project.Decl.Packages,
                    Project_Tree);

         Prj_Iter : Project_List;
         List     : String_List_Id;
         Elem     : String_Element;
         Lang     : Name_Id;
         Variable : Variable_Value;
         Name     : Name_Id;
         Count    : Natural;
         Result   : Argument_List_Access;

      begin
         Prj_Iter := Project_Tree.Projects;
         while Prj_Iter /= null loop
            if Might_Have_Sources (Prj_Iter.Project) then
               Variable :=
                 Value_Of
                   (Name_Languages,
                    Prj_Iter.Project.Decl.Attributes,
                    Project_Tree);

               if Variable = Nil_Variable_Value
                 or else Variable.Default
               then
                  --  Languages is not declared. If it is not an extending
                  --  project, check for Default_Language

                  if Prj_Iter.Project.Extends = No_Project then
                     Variable :=
                       Value_Of
                         (Name_Default_Language,
                          Prj_Iter.Project.Decl.Attributes,
                          Project_Tree);

                     if Variable /= Nil_Variable_Value
                       and then not Variable.Default
                     then
                        Get_Name_String (Variable.Value);
                        To_Lower (Name_Buffer (1 .. Name_Len));
                        Lang := Name_Find;
                        Language_Htable.Set (Lang, Lang);

                     else
                        --  If no language is declared, default to Ada

                        Language_Htable.Set (Name_Ada, Name_Ada);
                     end if;
                  end if;

               elsif Variable.Values /= Nil_String then

                  --  Attribute Languages is declared with a non empty
                  --  list: put all the languages in Language_HTable.

                  List := Variable.Values;
                  while List /= Nil_String loop
                     Elem := Project_Tree.String_Elements.Table (List);

                     Get_Name_String (Elem.Value);
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Lang := Name_Find;
                     Language_Htable.Set (Lang, Lang);

                     List := Elem.Next;
                  end loop;
               end if;
            end if;

            Prj_Iter := Prj_Iter.Next;
         end loop;

         Name  := Language_Htable.Get_First;
         Count := 0;
         while Name /= No_Name loop
            Count := Count + 1;
            Name := Language_Htable.Get_Next;
         end loop;

         Result := new String_List (1 .. Count);

         Count := 1;
         Name  := Language_Htable.Get_First;
         while Name /= No_Name loop
            --  Check if IDE'Compiler_Command is declared for the language.
            --  If it is, use its value to invoke gprconfig.

            Variable :=
              Value_Of
                (Name,
                 Attribute_Or_Array_Name => Name_Compiler_Command,
                 In_Package              => IDE,
                 In_Tree                 => Project_Tree,
                 Force_Lower_Case_Index  => True);

            declare
               Config_Command : constant String :=
                 "--config=" & Get_Name_String (Name);

               Runtime_Name   : constant String :=
                 Runtime_Name_For (Name);

            begin
               if Variable = Nil_Variable_Value
                 or else Length_Of_Name (Variable.Value) = 0
               then
                  Result (Count) :=
                    new String'(Config_Command & ",," & Runtime_Name);

               else
                  declare
                     Compiler_Command : constant String :=
                       Get_Name_String (Variable.Value);

                  begin
                     if Is_Absolute_Path (Compiler_Command) then
                        Result (Count) :=
                          new String'
                            (Config_Command & ",," & Runtime_Name & "," &
                             Containing_Directory (Compiler_Command) & "," &
                             Simple_Name (Compiler_Command));
                     else
                        Result (Count) :=
                          new String'
                            (Config_Command & ",," & Runtime_Name & ",," &
                             Compiler_Command);
                     end if;
                  end;
               end if;
            end;

            Count := Count + 1;
            Name  := Language_Htable.Get_Next;
         end loop;

         return Result;
      end Get_Config_Switches;

      -----------------
      -- Do_Autoconf --
      -----------------

      procedure Do_Autoconf is
         Obj_Dir : constant Variable_Value :=
                     Value_Of
                       (Name_Object_Dir,
                        Project.Decl.Attributes,
                        Project_Tree);

         Gprconfig_Path  : String_Access;
         Success         : Boolean;

      begin
         Gprconfig_Path := Locate_Exec_On_Path (Gprconfig_Name);

         if Gprconfig_Path = null then
            raise Invalid_Config
              with "could not locate gprconfig for auto-configuration";
         end if;

         --  First, find the object directory of the user's project

         if Obj_Dir = Nil_Variable_Value or else Obj_Dir.Default then
            Get_Name_String (Project.Directory.Name);

         else
            if Is_Absolute_Path (Get_Name_String (Obj_Dir.Value)) then
               Get_Name_String (Obj_Dir.Value);

            else
               Name_Len := 0;
               Add_Str_To_Name_Buffer
                 (Get_Name_String (Project.Directory.Name));
               Add_Char_To_Name_Buffer (Directory_Separator);
               Add_Str_To_Name_Buffer (Get_Name_String (Obj_Dir.Value));
            end if;
         end if;

         if Subdirs /= null then
            Add_Char_To_Name_Buffer (Directory_Separator);
            Add_Str_To_Name_Buffer (Subdirs.all);
         end if;

         for J in 1 .. Name_Len loop
            if Name_Buffer (J) = '/' then
               Name_Buffer (J) := Directory_Separator;
            end if;
         end loop;

         declare
            Obj_Dir  : constant String := Name_Buffer (1 .. Name_Len);
            Switches : Argument_List_Access := Get_Config_Switches;
            Args     : Argument_List (1 .. 5);
            Arg_Last : Positive;

         begin
            --  Check if the object directory exists. If Setup_Projects is True
            --  (-p) and directory does not exist, attempt to create it.
            --  Otherwise, if directory does not exist, fail without calling
            --  gprconfig.

            if not Is_Directory (Obj_Dir)
              and then (Setup_Projects or else Subdirs /= null)
            then
               begin
                  Create_Path (Obj_Dir);

                  if not Quiet_Output then
                     Write_Str ("object directory """);
                     Write_Str (Obj_Dir);
                     Write_Line (""" created");
                  end if;

               exception
                  when others =>
                     raise Invalid_Config
                       with "could not create object directory " & Obj_Dir;
               end;
            end if;

            if not Is_Directory (Obj_Dir) then
               raise Invalid_Config
                 with "object directory " & Obj_Dir & " does not exist";
            end if;

            --  Invoke gprconfig

            Args (1) := new String'("--batch");
            Args (2) := new String'("-o");

            --  If no config file was specified, set the auto.cgpr one

            if Config_File_Name = "" then
               Args (3) := new String'
                 (Obj_Dir & Directory_Separator & Auto_Cgpr);
            else
               Args (3) := new String'(Config_File_Name);
            end if;

            if Normalized_Hostname = "" then
               Arg_Last := 3;
            else
               if Target_Name = "" then
                  Args (4) := new String'("--target=" & Normalized_Hostname);
               else
                  Args (4) := new String'("--target=" & Target_Name);
               end if;

               Arg_Last := 4;
            end if;

            if not Verbose_Mode then
               Arg_Last := Arg_Last + 1;
               Args (Arg_Last) := new String'("-q");
            end if;

            if Verbose_Mode then
               Write_Str (Gprconfig_Name);

               for J in 1 .. Arg_Last loop
                  Write_Char (' ');
                  Write_Str (Args (J).all);
               end loop;

               for J in Switches'Range loop
                  Write_Char (' ');
                  Write_Str (Switches (J).all);
               end loop;

               Write_Eol;

            elsif not Quiet_Output then
               Write_Str ("creating ");
               Write_Str (Simple_Name (Args (3).all));
               Write_Eol;
            end if;

            Spawn (Gprconfig_Path.all, Args (1 .. Arg_Last) & Switches.all,
                   Success);

            Free (Switches);

            Config_File_Path := Locate_Config_File (Args (3).all);

            if Config_File_Path = null then
               raise Invalid_Config
                 with "could not create " & Args (3).all;
            end if;

            for F in Args'Range loop
               Free (Args (F));
            end loop;
         end;
      end Do_Autoconf;

      Success             : Boolean;
      Config_Project_Node : Project_Node_Id := Empty_Node;

   begin
      Free (Config_File_Path);
      Config := No_Project;

      if Config_File_Name /= "" then
         Config_File_Path := Locate_Config_File (Config_File_Name);
      else
         Config_File_Path := Locate_Config_File (Default_File_Name);
      end if;

      if Config_File_Path = null then
         if (not Allow_Automatic_Generation) and then
            Config_File_Name /= ""
         then
            raise Invalid_Config
              with "could not locate main configuration project "
                & Config_File_Name;
         end if;
      end if;

      Automatically_Generated :=
        Allow_Automatic_Generation and then Config_File_Path = null;

      <<Process_Config_File>>

      if Automatically_Generated then
         --  This might raise an Invalid_Config exception
         Do_Autoconf;
      end if;

      --  Parse the configuration file

      if Verbose_Mode and then Config_File_Path /= null then
         Write_Str  ("Checking configuration ");
         Write_Line (Config_File_Path.all);
      end if;

      if Config_File_Path /= null then
         Prj.Part.Parse
           (In_Tree                => Project_Node_Tree,
            Project                => Config_Project_Node,
            Project_File_Name      => Config_File_Path.all,
            Always_Errout_Finalize => False,
            Packages_To_Check      => Packages_To_Check,
            Current_Directory      => Current_Directory,
            Is_Config_File         => True,
            Flags                  => Flags);
      else
         --  Maybe the user will want to create his own configuration file
         Config_Project_Node := Empty_Node;
      end if;

      if On_Load_Config /= null then
         On_Load_Config
           (Config_File       => Config_Project_Node,
            Project_Node_Tree => Project_Node_Tree);
      end if;

      if Config_Project_Node /= Empty_Node then
         Prj.Proc.Process_Project_Tree_Phase_1
           (In_Tree                => Project_Tree,
            Project                => Config,
            Success                => Success,
            From_Project_Node      => Config_Project_Node,
            From_Project_Node_Tree => Project_Node_Tree,
            Flags                  => Flags,
            Reset_Tree             => False);
      end if;

      if Config_Project_Node = Empty_Node
        or else Config = No_Project
      then
         raise Invalid_Config
           with "processing of configuration project """
             & Config_File_Path.all & """ failed";
      end if;

      --  Check that the target of the configuration file is the one the user
      --  specified on the command line. We do not need to check that when in
      --  auto-conf mode, since the appropriate target was passed to gprconfig.

      if not Automatically_Generated
        and then not
          Check_Target (Config, Autoconf_Specified, Project_Tree, Target_Name)
      then
         Automatically_Generated := True;
         goto Process_Config_File;
      end if;
   end Get_Or_Create_Configuration_File;

   --------------------------------------
   -- Process_Project_And_Apply_Config --
   --------------------------------------

   procedure Process_Project_And_Apply_Config
     (Main_Project               : out Prj.Project_Id;
      User_Project_Node          : Prj.Tree.Project_Node_Id;
      Config_File_Name           : String := "";
      Autoconf_Specified         : Boolean;
      Project_Tree               : Prj.Project_Tree_Ref;
      Project_Node_Tree          : Prj.Tree.Project_Node_Tree_Ref;
      Packages_To_Check          : String_List_Access;
      Allow_Automatic_Generation : Boolean := True;
      Automatically_Generated    : out Boolean;
      Config_File_Path           : out String_Access;
      Target_Name                : String := "";
      Normalized_Hostname        : String;
      Flags                      : Processing_Flags;
      On_Load_Config             : Config_File_Hook := null;
      Reset_Tree                 : Boolean := True)
   is
      Main_Config_Project : Project_Id;
      Success : Boolean;

   begin
      Main_Project := No_Project;
      Automatically_Generated := False;

      Process_Project_Tree_Phase_1
        (In_Tree                => Project_Tree,
         Project                => Main_Project,
         Success                => Success,
         From_Project_Node      => User_Project_Node,
         From_Project_Node_Tree => Project_Node_Tree,
         Flags                  => Flags,
         Reset_Tree             => Reset_Tree);

      if not Success then
         Main_Project := No_Project;
         return;
      end if;

      --  Find configuration file

      Get_Or_Create_Configuration_File
        (Config                     => Main_Config_Project,
         Project                    => Main_Project,
         Project_Tree               => Project_Tree,
         Project_Node_Tree          => Project_Node_Tree,
         Allow_Automatic_Generation => Allow_Automatic_Generation,
         Config_File_Name           => Config_File_Name,
         Autoconf_Specified         => Autoconf_Specified,
         Target_Name                => Target_Name,
         Normalized_Hostname        => Normalized_Hostname,
         Packages_To_Check          => Packages_To_Check,
         Config_File_Path           => Config_File_Path,
         Automatically_Generated    => Automatically_Generated,
         Flags                      => Flags,
         On_Load_Config             => On_Load_Config);

      Apply_Config_File (Main_Config_Project, Project_Tree);

      --  Finish processing the user's project

      Prj.Proc.Process_Project_Tree_Phase_2
        (In_Tree                    => Project_Tree,
         Project                    => Main_Project,
         Success                    => Success,
         From_Project_Node          => User_Project_Node,
         From_Project_Node_Tree     => Project_Node_Tree,
         Flags                      => Flags);

      if not Success then
         Main_Project := No_Project;
      end if;
   end Process_Project_And_Apply_Config;

   ------------------------------------
   -- Parse_Project_And_Apply_Config --
   ------------------------------------

   procedure Parse_Project_And_Apply_Config
     (Main_Project               : out Prj.Project_Id;
      User_Project_Node          : out Prj.Tree.Project_Node_Id;
      Config_File_Name           : String := "";
      Autoconf_Specified         : Boolean;
      Project_File_Name          : String;
      Project_Tree               : Prj.Project_Tree_Ref;
      Project_Node_Tree          : Prj.Tree.Project_Node_Tree_Ref;
      Packages_To_Check          : String_List_Access;
      Allow_Automatic_Generation : Boolean := True;
      Automatically_Generated    : out Boolean;
      Config_File_Path           : out String_Access;
      Target_Name                : String := "";
      Normalized_Hostname        : String;
      Flags                      : Processing_Flags;
      On_Load_Config             : Config_File_Hook := null)
   is
   begin
      --  Parse the user project tree

      Prj.Tree.Initialize (Project_Node_Tree);
      Prj.Initialize (Project_Tree);

      Main_Project      := No_Project;
      Automatically_Generated := False;

      Prj.Part.Parse
        (In_Tree                => Project_Node_Tree,
         Project                => User_Project_Node,
         Project_File_Name      => Project_File_Name,
         Always_Errout_Finalize => False,
         Packages_To_Check      => Packages_To_Check,
         Current_Directory      => Current_Directory,
         Is_Config_File         => False,
         Flags                  => Flags);

      if User_Project_Node = Empty_Node then
         User_Project_Node := Empty_Node;
         return;
      end if;

      Process_Project_And_Apply_Config
        (Main_Project               => Main_Project,
         User_Project_Node          => User_Project_Node,
         Config_File_Name           => Config_File_Name,
         Autoconf_Specified         => Autoconf_Specified,
         Project_Tree               => Project_Tree,
         Project_Node_Tree          => Project_Node_Tree,
         Packages_To_Check          => Packages_To_Check,
         Allow_Automatic_Generation => Allow_Automatic_Generation,
         Automatically_Generated    => Automatically_Generated,
         Config_File_Path           => Config_File_Path,
         Target_Name                => Target_Name,
         Normalized_Hostname        => Normalized_Hostname,
         Flags                      => Flags,
         On_Load_Config             => On_Load_Config);
   end Parse_Project_And_Apply_Config;

   -----------------------
   -- Apply_Config_File --
   -----------------------

   procedure Apply_Config_File
     (Config_File  : Prj.Project_Id;
      Project_Tree : Prj.Project_Tree_Ref)
   is
      Conf_Decl    : constant Declarations := Config_File.Decl;
      Conf_Pack_Id : Package_Id;
      Conf_Pack    : Package_Element;

      User_Decl    : Declarations;
      User_Pack_Id : Package_Id;
      User_Pack    : Package_Element;
      Proj         : Project_List;

   begin
      Proj := Project_Tree.Projects;
      while Proj /= null loop
         if Proj.Project /= Config_File then
            User_Decl := Proj.Project.Decl;
            Add_Attributes
              (Project_Tree => Project_Tree,
               Conf_Decl    => Conf_Decl,
               User_Decl    => User_Decl);

            Conf_Pack_Id := Conf_Decl.Packages;
            while Conf_Pack_Id /= No_Package loop
               Conf_Pack := Project_Tree.Packages.Table (Conf_Pack_Id);

               User_Pack_Id := User_Decl.Packages;
               while User_Pack_Id /= No_Package loop
                  User_Pack := Project_Tree.Packages.Table (User_Pack_Id);
                  exit when User_Pack.Name = Conf_Pack.Name;
                  User_Pack_Id := User_Pack.Next;
               end loop;

               if User_Pack_Id = No_Package then
                  Package_Table.Increment_Last (Project_Tree.Packages);
                  User_Pack := Conf_Pack;
                  User_Pack.Next := User_Decl.Packages;
                  User_Decl.Packages :=
                    Package_Table.Last (Project_Tree.Packages);
                  Project_Tree.Packages.Table (User_Decl.Packages) :=
                    User_Pack;

               else
                  Add_Attributes
                    (Project_Tree => Project_Tree,
                     Conf_Decl    => Conf_Pack.Decl,
                     User_Decl    => Project_Tree.Packages.Table
                       (User_Pack_Id).Decl);
               end if;

               Conf_Pack_Id := Conf_Pack.Next;
            end loop;

            Proj.Project.Decl := User_Decl;
         end if;

         Proj := Proj.Next;
      end loop;
   end Apply_Config_File;

   ---------------------
   -- Set_Runtime_For --
   ---------------------

   procedure Set_Runtime_For (Language : Name_Id; RTS_Name : String) is
   begin
      Name_Len := RTS_Name'Length;
      Name_Buffer (1 .. Name_Len) := RTS_Name;
      RTS_Languages.Set (Language, Name_Find);
   end Set_Runtime_For;

   ----------------------
   -- Runtime_Name_For --
   ----------------------

   function Runtime_Name_For (Language : Name_Id) return String is
   begin
      if RTS_Languages.Get (Language) /= No_Name then
         return Get_Name_String (RTS_Languages.Get (Language));
      else
         return "";
      end if;
   end Runtime_Name_For;

   ------------------------------------
   -- Add_Default_GNAT_Naming_Scheme --
   ------------------------------------

   procedure Add_Default_GNAT_Naming_Scheme
     (Config_File  : in out Project_Node_Id;
      Project_Tree : Project_Node_Tree_Ref)
   is
      procedure Create_Attribute
        (Name  : Name_Id;
         Value : String;
         Index : String := "";
         Pkg   : Project_Node_Id := Empty_Node);

      ----------------------
      -- Create_Attribute --
      ----------------------

      procedure Create_Attribute
        (Name  : Name_Id;
         Value : String;
         Index : String := "";
         Pkg   : Project_Node_Id := Empty_Node)
      is
         Attr : Project_Node_Id;
         Val  : Name_Id := No_Name;
         Parent : Project_Node_Id := Config_File;
      begin
         if Index /= "" then
            Name_Len := Index'Length;
            Name_Buffer (1 .. Name_Len) := Index;
            Val := Name_Find;
         end if;

         if Pkg /= Empty_Node then
            Parent := Pkg;
         end if;

         Attr := Create_Attribute
           (Tree       => Project_Tree,
            Prj_Or_Pkg => Parent,
            Name       => Name,
            Index_Name => Val,
            Kind       => Prj.Single);

         Name_Len := Value'Length;
         Name_Buffer (1 .. Name_Len) := Value;
         Val := Name_Find;

         Set_Expression_Of
           (Attr, Project_Tree,
            Enclose_In_Expression
              (Create_Literal_String (Val, Project_Tree),
               Project_Tree));
      end Create_Attribute;

      Name   : Name_Id;
      Naming : Project_Node_Id;

   --  Start of processing for Add_Default_GNAT_Naming_Scheme

   begin
      if Config_File = Empty_Node then

         --  Create a dummy config file is none was found

         Name_Len := Auto_Cgpr'Length;
         Name_Buffer (1 .. Name_Len) := Auto_Cgpr;
         Name := Name_Find;

         --  An invalid project name to avoid conflicts with user-created ones

         Name_Len := 5;
         Name_Buffer (1 .. Name_Len) := "_auto";

         Config_File :=
           Create_Project
             (In_Tree        => Project_Tree,
              Name           => Name_Find,
              Full_Path      => Path_Name_Type (Name),
              Is_Config_File => True);

         --  Setup library support

         case MLib.Tgt.Support_For_Libraries is
            when None =>
               null;

            when Static_Only =>
               Create_Attribute (Name_Library_Support, "static_only");

            when Full =>
               Create_Attribute (Name_Library_Support, "full");
         end case;

         if MLib.Tgt.Standalone_Library_Auto_Init_Is_Supported then
            Create_Attribute (Name_Library_Auto_Init_Supported, "true");
         else
            Create_Attribute (Name_Library_Auto_Init_Supported, "false");
         end if;

         --  Setup Ada support (Ada is the default language here, since this
         --  is only called when no config file existed initially, ie for
         --  gnatmake).

         Create_Attribute (Name_Default_Language, "ada");

         Naming := Create_Package (Project_Tree, Config_File, "naming");
         Create_Attribute (Name_Spec_Suffix, ".ads", "ada",     Pkg => Naming);
         Create_Attribute (Name_Separate_Suffix, ".adb", "ada", Pkg => Naming);
         Create_Attribute (Name_Body_Suffix, ".adb", "ada",     Pkg => Naming);
         Create_Attribute (Name_Dot_Replacement, "-",           Pkg => Naming);
         Create_Attribute (Name_Casing,          "lowercase",   Pkg => Naming);

         if Current_Verbosity = High then
            Write_Line ("Automatically generated (in-memory) config file");
            Prj.PP.Pretty_Print
              (Project                => Config_File,
               In_Tree                => Project_Tree,
               Backward_Compatibility => False);
         end if;
      end if;
   end Add_Default_GNAT_Naming_Scheme;

end Prj.Conf;
