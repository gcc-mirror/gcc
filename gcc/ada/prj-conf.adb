------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . C O N F                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2013, Free Software Foundation, Inc.       --
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

with Hostparm;
with Makeutl;  use Makeutl;
with MLib.Tgt;
with Opt;      use Opt;
with Output;   use Output;
with Prj.Env;
with Prj.Err;
with Prj.Part;
with Prj.PP;
with Prj.Proc; use Prj.Proc;
with Prj.Tree; use Prj.Tree;
with Prj.Util; use Prj.Util;
with Prj;      use Prj;
with Snames;   use Snames;

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;  use Ada.Exceptions;

with GNAT.Case_Util; use GNAT.Case_Util;
with GNAT.HTable;    use GNAT.HTable;

package body Prj.Conf is

   Auto_Cgpr : constant String := "auto.cgpr";

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

   -----------------------
   -- Local_Subprograms --
   -----------------------

   function Check_Target
     (Config_File        : Prj.Project_Id;
      Autoconf_Specified : Boolean;
      Project_Tree       : Prj.Project_Tree_Ref;
      Target             : String := "") return Boolean;
   --  Check that the config file's target matches Target.
   --  Target should be set to the empty string when the user did not specify
   --  a target. If the target in the configuration file is invalid, this
   --  function will raise Invalid_Config with an appropriate message.
   --  Autoconf_Specified should be set to True if the user has used
   --  autoconf.

   function Locate_Config_File (Name : String) return String_Access;
   --  Search for Name in the config files directory. Return full path if
   --  found, or null otherwise.

   procedure Raise_Invalid_Config (Msg : String);
   pragma No_Return (Raise_Invalid_Config);
   --  Raises exception Invalid_Config with given message

   procedure Apply_Config_File
     (Config_File  : Prj.Project_Id;
      Project_Tree : Prj.Project_Tree_Ref);
   --  Apply the configuration file settings to all the projects in the
   --  project tree. The Project_Tree must have been parsed first, and
   --  processed through the first phase so that all its projects are known.
   --
   --  Currently, this will add new attributes and packages in the various
   --  projects, so that when the second phase of the processing is performed
   --  these attributes are automatically taken into account.

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
         pragma Unreferenced (Attr);

         Expr   : Name_Id         := No_Name;
         Val    : Name_Id         := No_Name;
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

         Name_Len := Value'Length;
         Name_Buffer (1 .. Name_Len) := Value;
         Expr := Name_Find;

         Attr := Create_Attribute
           (Tree       => Project_Tree,
            Prj_Or_Pkg => Parent,
            Name       => Name,
            Index_Name => Val,
            Kind       => Prj.Single,
            Value      => Create_Literal_String (Expr, Project_Tree));
      end Create_Attribute;

      --  Local variables

      Name     : Name_Id;
      Naming   : Project_Node_Id;
      Compiler : Project_Node_Id;

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

         Compiler := Create_Package (Project_Tree, Config_File, "compiler");
         Create_Attribute
           (Name_Driver, "gcc", "ada", Pkg => Compiler);
         Create_Attribute
           (Name_Language_Kind, "unit_based", "ada", Pkg => Compiler);
         Create_Attribute
           (Name_Dependency_Kind, "ALI_File", "ada", Pkg => Compiler);

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

   -----------------------
   -- Apply_Config_File --
   -----------------------

   procedure Apply_Config_File
     (Config_File  : Prj.Project_Id;
      Project_Tree : Prj.Project_Tree_Ref)
   is
      procedure Add_Attributes
        (Project_Tree : Project_Tree_Ref;
         Conf_Decl    : Declarations;
         User_Decl    : in out Declarations);
      --  Process the attributes in the config declarations.  For
      --  single string values, if the attribute is not declared in
      --  the user declarations, declare it with the value in the
      --  config declarations.  For string list values, prepend the
      --  value in the user declarations with the value in the config
      --  declarations.

      --------------------
      -- Add_Attributes --
      --------------------

      procedure Add_Attributes
        (Project_Tree : Project_Tree_Ref;
         Conf_Decl    : Declarations;
         User_Decl    : in out Declarations)
      is
         Shared             : constant Shared_Project_Tree_Data_Access :=
                                Project_Tree.Shared;
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
            Conf_Attr := Shared.Variable_Elements.Table (Conf_Attr_Id);
            User_Attr := Shared.Variable_Elements.Table (User_Attr_Id);

            if not Conf_Attr.Value.Default then
               if User_Attr.Value.Default then

                  --  No attribute declared in user project file: just copy
                  --  the value of the configuration attribute.

                  User_Attr.Value := Conf_Attr.Value;
                  Shared.Variable_Elements.Table (User_Attr_Id) := User_Attr;

               elsif User_Attr.Value.Kind = List
                 and then Conf_Attr.Value.Values /= Nil_String
               then
                  --  List attribute declared in both the user project and the
                  --  configuration project: prepend the user list with the
                  --  configuration list.

                  declare
                     User_List : constant String_List_Id :=
                                   User_Attr.Value.Values;
                     Conf_List : String_List_Id := Conf_Attr.Value.Values;
                     Conf_Elem : String_Element;
                     New_List  : String_List_Id;
                     New_Elem  : String_Element;

                  begin
                     --  Create new list

                     String_Element_Table.Increment_Last
                       (Shared.String_Elements);
                     New_List :=
                       String_Element_Table.Last (Shared.String_Elements);

                     --  Value of attribute is new list

                     User_Attr.Value.Values := New_List;
                     Shared.Variable_Elements.Table (User_Attr_Id) :=
                       User_Attr;

                     loop
                        --  Get each element of configuration list

                        Conf_Elem := Shared.String_Elements.Table (Conf_List);
                        New_Elem  := Conf_Elem;
                        Conf_List := Conf_Elem.Next;

                        if Conf_List = Nil_String then

                           --  If it is the last element in the list, connect
                           --  to first element of user list, and we are done.

                           New_Elem.Next := User_List;
                           Shared.String_Elements.Table (New_List) := New_Elem;
                           exit;

                        else
                           --  If it is not the last element in the list, add
                           --  to new list.

                           String_Element_Table.Increment_Last
                             (Shared.String_Elements);
                           New_Elem.Next := String_Element_Table.Last
                             (Shared.String_Elements);
                           Shared.String_Elements.Table (New_List) := New_Elem;
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
            Conf_Array := Shared.Arrays.Table (Conf_Array_Id);

            User_Array_Id := User_Decl.Arrays;
            while User_Array_Id /= No_Array loop
               User_Array := Shared.Arrays.Table (User_Array_Id);
               exit when User_Array.Name = Conf_Array.Name;
               User_Array_Id := User_Array.Next;
            end loop;

            --  If this associative array does not exist in the user project
            --  file, do a shallow copy of the full associative array.

            if User_Array_Id = No_Array then
               Array_Table.Increment_Last (Shared.Arrays);
               User_Array := Conf_Array;
               User_Array.Next := User_Decl.Arrays;
               User_Decl.Arrays := Array_Table.Last (Shared.Arrays);
               Shared.Arrays.Table (User_Decl.Arrays) := User_Array;

            --  Otherwise, check each array element

            else
               Conf_Array_Elem_Id := Conf_Array.Value;
               while Conf_Array_Elem_Id /= No_Array_Element loop
                  Conf_Array_Elem :=
                    Shared.Array_Elements.Table (Conf_Array_Elem_Id);

                  User_Array_Elem_Id := User_Array.Value;
                  while User_Array_Elem_Id /= No_Array_Element loop
                     User_Array_Elem :=
                       Shared.Array_Elements.Table (User_Array_Elem_Id);
                     exit when User_Array_Elem.Index = Conf_Array_Elem.Index;
                     User_Array_Elem_Id := User_Array_Elem.Next;
                  end loop;

                  --  If the array element doesn't exist in the user array,
                  --  insert a shallow copy of the conf array element in the
                  --  user array.

                  if User_Array_Elem_Id = No_Array_Element then
                     Array_Element_Table.Increment_Last
                       (Shared.Array_Elements);
                     User_Array_Elem := Conf_Array_Elem;
                     User_Array_Elem.Next := User_Array.Value;
                     User_Array.Value :=
                       Array_Element_Table.Last (Shared.Array_Elements);
                     Shared.Array_Elements.Table (User_Array.Value) :=
                       User_Array_Elem;
                     Shared.Arrays.Table (User_Array_Id) := User_Array;

                  --  Otherwise, if the value is a string list, prepend the
                  --  conf array element value to the array element.

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
                                Shared.String_Elements.Table (Conf_List);
                              String_Element_Table.Increment_Last
                                (Shared.String_Elements);
                              Next :=
                                String_Element_Table.Last
                                (Shared.String_Elements);
                              Shared.String_Elements.Table (Next) :=
                                Conf_List_Elem;

                              if Previous = Nil_String then
                                 User_Array_Elem.Value.Values := Next;
                                 Shared.Array_Elements.Table
                                   (User_Array_Elem_Id) := User_Array_Elem;

                              else
                                 Shared.String_Elements.Table
                                   (Previous).Next := Next;
                              end if;

                              Previous := Next;

                              Conf_List := Conf_List_Elem.Next;

                              if Conf_List = Nil_String then
                                 Shared.String_Elements.Table
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

      Shared : constant Shared_Project_Tree_Data_Access := Project_Tree.Shared;

      Conf_Decl    : constant Declarations := Config_File.Decl;
      Conf_Pack_Id : Package_Id;
      Conf_Pack    : Package_Element;

      User_Decl    : Declarations;
      User_Pack_Id : Package_Id;
      User_Pack    : Package_Element;
      Proj         : Project_List;

   begin
      Debug_Output ("Applying config file to a project tree");

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
               Conf_Pack := Shared.Packages.Table (Conf_Pack_Id);

               User_Pack_Id := User_Decl.Packages;
               while User_Pack_Id /= No_Package loop
                  User_Pack := Shared.Packages.Table (User_Pack_Id);
                  exit when User_Pack.Name = Conf_Pack.Name;
                  User_Pack_Id := User_Pack.Next;
               end loop;

               if User_Pack_Id = No_Package then
                  Package_Table.Increment_Last (Shared.Packages);
                  User_Pack := Conf_Pack;
                  User_Pack.Next := User_Decl.Packages;
                  User_Decl.Packages := Package_Table.Last (Shared.Packages);
                  Shared.Packages.Table (User_Decl.Packages) := User_Pack;

               else
                  Add_Attributes
                    (Project_Tree => Project_Tree,
                     Conf_Decl    => Conf_Pack.Decl,
                     User_Decl    => Shared.Packages.Table
                                       (User_Pack_Id).Decl);
               end if;

               Conf_Pack_Id := Conf_Pack.Next;
            end loop;

            Proj.Project.Decl := User_Decl;

            --  For aggregate projects, we need to apply the config to all
            --  their aggregated trees as well.

            if Proj.Project.Qualifier in Aggregate_Project then
               declare
                  List : Aggregated_Project_List;
               begin
                  List := Proj.Project.Aggregated_Projects;
                  while List /= null loop
                     Debug_Output
                       ("Recursively apply config to aggregated tree",
                        List.Project.Name);
                     Apply_Config_File
                       (Config_File, Project_Tree => List.Tree);
                     List := List.Next;
                  end loop;
               end;
            end if;
         end if;

         Proj := Proj.Next;
      end loop;
   end Apply_Config_File;

   ------------------
   -- Check_Target --
   ------------------

   function Check_Target
     (Config_File        : Project_Id;
      Autoconf_Specified : Boolean;
      Project_Tree       : Prj.Project_Tree_Ref;
      Target             : String := "") return Boolean
   is
      Shared   : constant Shared_Project_Tree_Data_Access :=
                   Project_Tree.Shared;
      Variable : constant Variable_Value :=
                   Value_Of
                     (Name_Target, Config_File.Decl.Attributes, Shared);
      Tgt_Name : Name_Id := No_Name;
      OK       : Boolean;

   begin
      if Variable /= Nil_Variable_Value and then not Variable.Default then
         Tgt_Name := Variable.Value;
      end if;

      OK :=
        Target = ""
          or else (Tgt_Name /= No_Name
                    and then Target = Get_Name_String (Tgt_Name));

      if not OK then
         if Autoconf_Specified then
            if Verbose_Mode then
               Write_Line ("inconsistent targets, performing autoconf");
            end if;

            return False;

         else
            if Tgt_Name /= No_Name then
               Raise_Invalid_Config
                 ("invalid target name """
                  & Get_Name_String (Tgt_Name) & """ in configuration");
            else
               Raise_Invalid_Config
                 ("no target specified in configuration file");
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
      Conf_Project               : Project_Id;
      Project_Tree               : Project_Tree_Ref;
      Project_Node_Tree          : Prj.Tree.Project_Node_Tree_Ref;
      Env                        : in out Prj.Tree.Environment;
      Allow_Automatic_Generation : Boolean;
      Config_File_Name           : String := "";
      Autoconf_Specified         : Boolean;
      Target_Name                : String := "";
      Normalized_Hostname        : String;
      Packages_To_Check          : String_List_Access := null;
      Config                     : out Prj.Project_Id;
      Config_File_Path           : out String_Access;
      Automatically_Generated    : out Boolean;
      On_Load_Config             : Config_File_Hook := null)
   is
      Shared : constant Shared_Project_Tree_Data_Access := Project_Tree.Shared;

      At_Least_One_Compiler_Command : Boolean := False;
      --  Set to True if at least one attribute Ide'Compiler_Command is
      --  specified for one language of the system.

      Conf_File_Name : String_Access := new String'(Config_File_Name);
      --  The configuration project file name. May be modified if there are
      --  switches --config= in the Builder package of the main project.

      Selected_Target : String_Access := new String'(Target_Name);

      function Default_File_Name return String;
      --  Return the name of the default config file that should be tested

      procedure Do_Autoconf;
      --  Generate a new config file through gprconfig. In case of error, this
      --  raises the Invalid_Config exception with an appropriate message

      procedure Check_Builder_Switches;
      --  Check for switches --config and --RTS in package Builder

      procedure Get_Project_Target;
      --  Target_Name is empty, get the specifiedtarget in the project file,
      --  if any.

      function Get_Config_Switches return Argument_List_Access;
      --  Return the --config switches to use for gprconfig

      function Get_Db_Switches return Argument_List_Access;
      --  Return the --db switches to use for gprconfig

      function Might_Have_Sources (Project : Project_Id) return Boolean;
      --  True if the specified project might have sources (ie the user has not
      --  explicitly specified it. We haven't checked the file system, nor do
      --  we need to at this stage.

      ----------------------------
      -- Check_Builder_Switches --
      ----------------------------

      procedure Check_Builder_Switches is
         Get_RTS_Switches : constant Boolean :=
                              RTS_Languages.Get_First = No_Name;
         --  If no switch --RTS have been specified on the command line, look
         --  for --RTS switches in the Builder switches.

         Builder : constant Package_Id :=
                     Value_Of (Name_Builder, Project.Decl.Packages, Shared);

         Switch_Array_Id : Array_Element_Id;
         --  The Switches to be checked

         procedure Check_Switches;
         --  Check the switches in Switch_Array_Id

         --------------------
         -- Check_Switches --
         --------------------

         procedure Check_Switches is
            Switch_Array    : Array_Element;
            Switch_List     : String_List_Id := Nil_String;
            Switch          : String_Element;
            Lang            : Name_Id;
            Lang_Last       : Positive;

         begin
            while Switch_Array_Id /= No_Array_Element loop
               Switch_Array :=
                 Shared.Array_Elements.Table (Switch_Array_Id);

               Switch_List := Switch_Array.Value.Values;
               List_Loop : while Switch_List /= Nil_String loop
                  Switch := Shared.String_Elements.Table (Switch_List);

                  if Switch.Value /= No_Name then
                     Get_Name_String (Switch.Value);

                     if Conf_File_Name'Length = 0
                       and then Name_Len > 9
                       and then Name_Buffer (1 .. 9) = "--config="
                     then
                        Conf_File_Name :=
                          new String'(Name_Buffer (10 .. Name_Len));

                     elsif Get_RTS_Switches
                       and then Name_Len >= 7
                       and then Name_Buffer (1 .. 5) = "--RTS"
                     then
                        if Name_Buffer (6) = '=' then
                           if not Runtime_Name_Set_For (Name_Ada) then
                              Set_Runtime_For
                                (Name_Ada,
                                 Name_Buffer (7 .. Name_Len));
                              Locate_Runtime (Name_Ada, Project_Tree);
                           end if;

                        elsif Name_Len > 7
                          and then Name_Buffer (6) = ':'
                          and then Name_Buffer (7) /= '='
                        then
                           Lang_Last := 7;
                           while Lang_Last < Name_Len
                             and then Name_Buffer (Lang_Last + 1) /= '='
                           loop
                              Lang_Last := Lang_Last + 1;
                           end loop;

                           if Name_Buffer (Lang_Last + 1) = '=' then
                              declare
                                 RTS : constant String :=
                                   Name_Buffer (Lang_Last + 2 .. Name_Len);
                              begin
                                 Name_Buffer (1 .. Lang_Last - 6) :=
                                   Name_Buffer (7 .. Lang_Last);
                                 Name_Len := Lang_Last - 6;
                                 To_Lower (Name_Buffer (1 .. Name_Len));
                                 Lang := Name_Find;

                                 if not Runtime_Name_Set_For (Lang) then
                                    Set_Runtime_For (Lang, RTS);
                                    Locate_Runtime (Lang, Project_Tree);
                                 end if;
                              end;
                           end if;
                        end if;
                     end if;
                  end if;

                  Switch_List := Switch.Next;
               end loop List_Loop;

               Switch_Array_Id := Switch_Array.Next;
            end loop;
         end Check_Switches;

      --  Start of processing for Check_Builder_Switches

      begin
         if Builder /= No_Package then
            Switch_Array_Id :=
              Value_Of
                (Name      => Name_Switches,
                 In_Arrays => Shared.Packages.Table (Builder).Decl.Arrays,
                 Shared    => Shared);
            Check_Switches;

            Switch_Array_Id :=
              Value_Of
                (Name      => Name_Default_Switches,
                 In_Arrays => Shared.Packages.Table (Builder).Decl.Arrays,
                 Shared    => Shared);
            Check_Switches;
         end if;
      end Check_Builder_Switches;

      ------------------------
      -- Get_Project_Target --
      ------------------------

      procedure Get_Project_Target is
      begin
         if Selected_Target'Length = 0 then

            --  Check if attribute Target is specified in the main
            --  project, or in a project it extends. If it is, use this
            --  target to invoke gprconfig.

            declare
               Variable : Variable_Value;
               Proj     : Project_Id;
               Tgt_Name : Name_Id := No_Name;

            begin
               Proj := Project;
               Project_Loop :
               while Proj /= No_Project loop
                  Variable :=
                    Value_Of (Name_Target, Proj.Decl.Attributes, Shared);

                  if Variable /= Nil_Variable_Value
                    and then not Variable.Default
                    and then Variable.Value /= No_Name
                  then
                     Tgt_Name := Variable.Value;
                     exit Project_Loop;
                  end if;

                  Proj := Proj.Extends;
               end loop Project_Loop;

               if Tgt_Name /= No_Name then
                  Selected_Target := new String'(Get_Name_String (Tgt_Name));
               end if;
            end;
         end if;
      end Get_Project_Target;

      -----------------------
      -- Default_File_Name --
      -----------------------

      function Default_File_Name return String is
         Ada_RTS : constant String := Runtime_Name_For (Name_Ada);
         Tmp     : String_Access;

      begin
         if Selected_Target'Length /= 0 then
            if Ada_RTS /= "" then
               return
                 Selected_Target.all & '-' &
                 Ada_RTS & Config_Project_File_Extension;
            else
               return
                 Selected_Target.all & Config_Project_File_Extension;
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
                  return Default_Config_Name;
               else
                  return T;
               end if;
            end;
         end if;
      end Default_File_Name;

      -----------------
      -- Do_Autoconf --
      -----------------

      procedure Do_Autoconf is
         Obj_Dir : constant Variable_Value :=
                     Value_Of
                       (Name_Object_Dir,
                        Conf_Project.Decl.Attributes,
                        Shared);

         Gprconfig_Path  : String_Access;
         Success         : Boolean;

      begin
         Gprconfig_Path := Locate_Exec_On_Path (Gprconfig_Name);

         if Gprconfig_Path = null then
            Raise_Invalid_Config
              ("could not locate gprconfig for auto-configuration");
         end if;

         --  First, find the object directory of the Conf_Project

         if Obj_Dir = Nil_Variable_Value or else Obj_Dir.Default then
            Get_Name_String (Conf_Project.Directory.Display_Name);

         else
            if Is_Absolute_Path (Get_Name_String (Obj_Dir.Value)) then
               Get_Name_String (Obj_Dir.Value);

            else
               Name_Len := 0;
               Add_Str_To_Name_Buffer
                 (Get_Name_String (Conf_Project.Directory.Display_Name));
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

         --  Make sure that Obj_Dir ends with a directory separator

         if Name_Buffer (Name_Len) /= Directory_Separator then
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Directory_Separator;
         end if;

         declare
            Obj_Dir         : constant String := Name_Buffer (1 .. Name_Len);
            Config_Switches : Argument_List_Access;
            Db_Switches     : Argument_List_Access;
            Args            : Argument_List (1 .. 5);
            Arg_Last        : Positive;
            Obj_Dir_Exists  : Boolean := True;

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
                     Raise_Invalid_Config
                       ("could not create object directory " & Obj_Dir);
               end;
            end if;

            if not Is_Directory (Obj_Dir) then
               case Env.Flags.Require_Obj_Dirs is
                  when Error =>
                     Raise_Invalid_Config
                       ("object directory " & Obj_Dir & " does not exist");

                  when Warning =>
                     Prj.Err.Error_Msg
                       (Env.Flags,
                        "?object directory " & Obj_Dir & " does not exist");
                     Obj_Dir_Exists := False;

                  when Silent =>
                     null;
               end case;
            end if;

            --  Get the config switches. This should be done only now, as some
            --  runtimes may have been found if the Builder switches.

            Config_Switches := Get_Config_Switches;

            --  Get eventual --db switches

            Db_Switches := Get_Db_Switches;

            --  Invoke gprconfig

            Args (1) := new String'("--batch");
            Args (2) := new String'("-o");

            --  If no config file was specified, set the auto.cgpr one

            if Conf_File_Name'Length = 0 then
               if Obj_Dir_Exists then
                  Args (3) := new String'(Obj_Dir & Auto_Cgpr);

               else
                  declare
                     Path_FD   : File_Descriptor;
                     Path_Name : Path_Name_Type;

                  begin
                     Prj.Env.Create_Temp_File
                       (Shared    => Project_Tree.Shared,
                        Path_FD   => Path_FD,
                        Path_Name => Path_Name,
                        File_Use  => "configuration file");

                     if Path_FD /= Invalid_FD then
                        declare
                           Temp_Dir : constant String :=
                                        Containing_Directory
                                          (Get_Name_String (Path_Name));
                        begin
                           GNAT.OS_Lib.Close (Path_FD);
                           Args (3) :=
                             new String'(Temp_Dir &
                                         Directory_Separator &
                                         Auto_Cgpr);
                           Delete_File (Get_Name_String (Path_Name));
                        end;

                     else
                        --  We'll have an error message later on

                        Args (3) := new String'(Obj_Dir & Auto_Cgpr);
                     end if;
                  end;
               end if;
            else
               Args (3) := Conf_File_Name;
            end if;

            if Normalized_Hostname = "" then
               Arg_Last := 3;
            else
               if Selected_Target'Length = 0 then
                  if At_Least_One_Compiler_Command then
                     Args (4) :=
                       new String'("--target=all");
                  else
                     Args (4) :=
                       new String'("--target=" & Normalized_Hostname);
                  end if;

               else
                  Args (4) :=
                    new String'("--target=" & Selected_Target.all);
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

               for J in Config_Switches'Range loop
                  Write_Char (' ');
                  Write_Str (Config_Switches (J).all);
               end loop;

               for J in Db_Switches'Range loop
                  Write_Char (' ');
                  Write_Str (Db_Switches (J).all);
               end loop;

               Write_Eol;

            elsif not Quiet_Output then
               --  Display no message if we are creating auto.cgpr, unless in
               --  verbose mode

               if Config_File_Name'Length > 0
                 or else Verbose_Mode
               then
                  Write_Str ("creating ");
                  Write_Str (Simple_Name (Args (3).all));
                  Write_Eol;
               end if;
            end if;

            Spawn (Gprconfig_Path.all, Args (1 .. Arg_Last) &
                   Config_Switches.all & Db_Switches.all,
                   Success);

            Free (Config_Switches);

            Config_File_Path := Locate_Config_File (Args (3).all);

            if Config_File_Path = null then
               Raise_Invalid_Config
                 ("could not create " & Args (3).all);
            end if;

            for F in Args'Range loop
               Free (Args (F));
            end loop;
         end;
      end Do_Autoconf;

      ---------------------
      -- Get_Db_Switches --
      ---------------------

      function Get_Db_Switches return Argument_List_Access is
         Result : Argument_List_Access;
         Nmb_Arg : Natural;
      begin
         Nmb_Arg :=
           (2 * Db_Switch_Args.Last) + Boolean'Pos (not Load_Standard_Base);
         Result := new Argument_List (1 .. Nmb_Arg);

         if Nmb_Arg /= 0 then
            for J in 1 .. Db_Switch_Args.Last loop
               Result (2 * J - 1) :=
                 new String'("--db");
               Result (2 * J) :=
                 new String'(Get_Name_String (Db_Switch_Args.Table (J)));
            end loop;

            if not Load_Standard_Base then
               Result (Result'Last) := new String'("--db-");
            end if;
         end if;

         return Result;
      end Get_Db_Switches;

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
                 Value_Of (Name_Ide, Project.Decl.Packages, Shared);

         procedure Add_Config_Switches_For_Project
           (Project    : Project_Id;
            Tree       : Project_Tree_Ref;
            With_State : in out Integer);
         --  Add all --config switches for this project. This is also called
         --  for aggregate projects.

         -------------------------------------
         -- Add_Config_Switches_For_Project --
         -------------------------------------

         procedure Add_Config_Switches_For_Project
           (Project    : Project_Id;
            Tree       : Project_Tree_Ref;
            With_State : in out Integer)
         is
            pragma Unreferenced (With_State);

            Shared : constant Shared_Project_Tree_Data_Access := Tree.Shared;

            Variable      : Variable_Value;
            Check_Default : Boolean;
            Lang          : Name_Id;
            List          : String_List_Id;
            Elem          : String_Element;

         begin
            if Might_Have_Sources (Project) then
               Variable :=
                 Value_Of (Name_Languages, Project.Decl.Attributes, Shared);

               if Variable = Nil_Variable_Value or else Variable.Default then

                  --  Languages is not declared. If it is not an extending
                  --  project, or if it extends a project with no Languages,
                  --  check for Default_Language.

                  Check_Default := Project.Extends = No_Project;

                  if not Check_Default then
                     Variable :=
                       Value_Of
                         (Name_Languages,
                          Project.Extends.Decl.Attributes,
                          Shared);
                     Check_Default :=
                       Variable /= Nil_Variable_Value
                         and then Variable.Values = Nil_String;
                  end if;

                  if Check_Default then
                     Variable :=
                       Value_Of
                         (Name_Default_Language,
                          Project.Decl.Attributes,
                          Shared);

                     if Variable /= Nil_Variable_Value
                       and then not Variable.Default
                     then
                        Get_Name_String (Variable.Value);
                        To_Lower (Name_Buffer (1 .. Name_Len));
                        Lang := Name_Find;
                        Language_Htable.Set (Lang, Lang);

                     --  If no default language is declared, default to Ada

                     else
                        Language_Htable.Set (Name_Ada, Name_Ada);
                     end if;
                  end if;

               elsif Variable.Values /= Nil_String then

                  --  Attribute Languages is declared with a non empty list:
                  --  put all the languages in Language_HTable.

                  List := Variable.Values;
                  while List /= Nil_String loop
                     Elem := Shared.String_Elements.Table (List);

                     Get_Name_String (Elem.Value);
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Lang := Name_Find;
                     Language_Htable.Set (Lang, Lang);

                     List := Elem.Next;
                  end loop;
               end if;
            end if;
         end Add_Config_Switches_For_Project;

         procedure For_Every_Imported_Project is new For_Every_Project_Imported
           (State => Integer, Action => Add_Config_Switches_For_Project);
         --  Document this procedure ???

         --  Local variables

         Name     : Name_Id;
         Count    : Natural;
         Result   : Argument_List_Access;
         Variable : Variable_Value;
         Dummy    : Integer := 0;

      --  Start of processing for Get_Config_Switches

      begin
         For_Every_Imported_Project
           (By                 => Project,
            Tree               => Project_Tree,
            With_State         => Dummy,
            Include_Aggregated => True);

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
                 Shared                  => Shared,
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
                  At_Least_One_Compiler_Command := True;

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
              Shared);

         if Variable = Nil_Variable_Value
           or else Variable.Default
           or else Variable.Values /= Nil_String
         then
            Variable :=
              Value_Of
                (Name_Source_Files,
                 Project.Decl.Attributes,
                 Shared);
            return Variable = Nil_Variable_Value
              or else Variable.Default
              or else Variable.Values /= Nil_String;

         else
            return False;
         end if;
      end Might_Have_Sources;

      Success             : Boolean;
      Config_Project_Node : Project_Node_Id := Empty_Node;

   begin
      pragma Assert (Prj.Env.Is_Initialized (Env.Project_Path));

      Free (Config_File_Path);
      Config := No_Project;

      Get_Project_Target;
      Check_Builder_Switches;

      if Conf_File_Name'Length > 0 then
         Config_File_Path := Locate_Config_File (Conf_File_Name.all);
      else
         Config_File_Path := Locate_Config_File (Default_File_Name);
      end if;

      if Config_File_Path = null then
         if not Allow_Automatic_Generation
           and then Conf_File_Name'Length > 0
         then
            Raise_Invalid_Config
              ("could not locate main configuration project "
               & Conf_File_Name.all);
         end if;
      end if;

      Automatically_Generated :=
        Allow_Automatic_Generation and then Config_File_Path = null;

      <<Process_Config_File>>

      if Automatically_Generated then
         if Hostparm.OpenVMS then

            --  There is no gprconfig on VMS

            Raise_Invalid_Config
              ("could not locate any configuration project file");

         else
            --  This might raise an Invalid_Config exception

            Do_Autoconf;
         end if;

      --  If the config file is not auto-generated, warn if there is any --RTS
      --  switch, but not when the config file is generated in memory.

      elsif RTS_Languages.Get_First /= No_Name
        and then Opt.Warning_Mode /= Opt.Suppress
        and then On_Load_Config = null
      then
         Write_Line
           ("warning: --RTS is taken into account only in auto-configuration");
      end if;

      --  Parse the configuration file

      if Verbose_Mode and then Config_File_Path /= null then
         Write_Str  ("Checking configuration ");
         Write_Line (Config_File_Path.all);
      end if;

      if On_Load_Config /= null then
         On_Load_Config
           (Config_File       => Config_Project_Node,
            Project_Node_Tree => Project_Node_Tree);

      elsif Config_File_Path /= null then
         Prj.Part.Parse
           (In_Tree           => Project_Node_Tree,
            Project           => Config_Project_Node,
            Project_File_Name => Config_File_Path.all,
            Errout_Handling   => Prj.Part.Finalize_If_Error,
            Packages_To_Check => Packages_To_Check,
            Current_Directory => Current_Directory,
            Is_Config_File    => True,
            Env               => Env);
      else
         Config_Project_Node := Empty_Node;
      end if;

      if Config_Project_Node /= Empty_Node then
         Prj.Proc.Process_Project_Tree_Phase_1
           (In_Tree                => Project_Tree,
            Project                => Config,
            Packages_To_Check      => Packages_To_Check,
            Success                => Success,
            From_Project_Node      => Config_Project_Node,
            From_Project_Node_Tree => Project_Node_Tree,
            Env                    => Env,
            Reset_Tree             => False);
      end if;

      if Config_Project_Node = Empty_Node
        or else Config = No_Project
      then
         Raise_Invalid_Config
           ("processing of configuration project """
            & Config_File_Path.all & """ failed");
      end if;

      --  Check that the target of the configuration file is the one the user
      --  specified on the command line. We do not need to check that when in
      --  auto-conf mode, since the appropriate target was passed to gprconfig.

      if not Automatically_Generated
        and then not
          Check_Target
            (Config, Autoconf_Specified, Project_Tree, Selected_Target.all)
      then
         Automatically_Generated := True;
         goto Process_Config_File;
      end if;
   end Get_Or_Create_Configuration_File;

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

   --------------------
   -- Locate_Runtime --
   --------------------

   procedure Locate_Runtime
     (Language     : Name_Id;
      Project_Tree : Prj.Project_Tree_Ref)
   is
      function Is_Base_Name (Path : String) return Boolean;
      --  Returns True if Path has no directory separator

      ------------------
      -- Is_Base_Name --
      ------------------

      function Is_Base_Name (Path : String) return Boolean is
      begin
         for I in Path'Range loop
            if Path (I) = Directory_Separator or else Path (I) = '/' then
               return False;
            end if;
         end loop;
         return True;
      end Is_Base_Name;

      --  Local declarations

      function Find_Rts_In_Path is new Prj.Env.Find_Name_In_Path
        (Check_Filename => Is_Directory);

      RTS_Name : constant String := Runtime_Name_For (Language);

      Full_Path : String_Access;

   --  Start of processing for Locate_Runtime

   begin
      if not Is_Base_Name (RTS_Name) then
         Full_Path :=
           Find_Rts_In_Path (Root_Environment.Project_Path, RTS_Name);

         if Full_Path = null then
            Fail_Program (Project_Tree, "cannot find RTS " & RTS_Name);
         end if;

         Set_Runtime_For (Language, Normalize_Pathname (Full_Path.all));
         Free (Full_Path);
      end if;
   end Locate_Runtime;

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
      Env                        : in out Prj.Tree.Environment;
      Packages_To_Check          : String_List_Access;
      Allow_Automatic_Generation : Boolean := True;
      Automatically_Generated    : out Boolean;
      Config_File_Path           : out String_Access;
      Target_Name                : String := "";
      Normalized_Hostname        : String;
      On_Load_Config             : Config_File_Hook := null;
      Implicit_Project           : Boolean := False)
   is
   begin
      pragma Assert (Prj.Env.Is_Initialized (Env.Project_Path));

      --  Parse the user project tree

      Prj.Initialize (Project_Tree);

      Main_Project := No_Project;
      Automatically_Generated := False;

      Prj.Part.Parse
        (In_Tree           => Project_Node_Tree,
         Project           => User_Project_Node,
         Project_File_Name => Project_File_Name,
         Errout_Handling   => Prj.Part.Finalize_If_Error,
         Packages_To_Check => Packages_To_Check,
         Current_Directory => Current_Directory,
         Is_Config_File    => False,
         Env               => Env,
         Implicit_Project  => Implicit_Project);

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
         Env                        => Env,
         Packages_To_Check          => Packages_To_Check,
         Allow_Automatic_Generation => Allow_Automatic_Generation,
         Automatically_Generated    => Automatically_Generated,
         Config_File_Path           => Config_File_Path,
         Target_Name                => Target_Name,
         Normalized_Hostname        => Normalized_Hostname,
         On_Load_Config             => On_Load_Config);
   end Parse_Project_And_Apply_Config;

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
      Env                        : in out Prj.Tree.Environment;
      Packages_To_Check          : String_List_Access;
      Allow_Automatic_Generation : Boolean := True;
      Automatically_Generated    : out Boolean;
      Config_File_Path           : out String_Access;
      Target_Name                : String := "";
      Normalized_Hostname        : String;
      On_Load_Config             : Config_File_Hook := null;
      Reset_Tree                 : Boolean := True)
   is
      Shared              : constant Shared_Project_Tree_Data_Access :=
                              Project_Tree.Shared;
      Main_Config_Project : Project_Id;
      Success             : Boolean;

      Conf_Project : Project_Id := No_Project;
      --  The object directory of this project is used to store the config
      --  project file in auto-configuration. Set by Check_Project below.

      procedure Check_Project (Project : Project_Id);
      --  Look for a non aggregate project. If one is found, put its project Id
      --  in Conf_Project.

      -------------------
      -- Check_Project --
      -------------------

      procedure Check_Project (Project : Project_Id) is
      begin
         if Project.Qualifier = Aggregate
              or else
            Project.Qualifier = Aggregate_Library
         then
            declare
               List : Aggregated_Project_List := Project.Aggregated_Projects;

            begin
               --  Look for a non aggregate project until one is found

               while Conf_Project = No_Project and then List /= null loop
                  Check_Project (List.Project);
                  List := List.Next;
               end loop;
            end;

         else
            Conf_Project := Project;
         end if;
      end Check_Project;

   --  Start of processing for Process_Project_And_Apply_Config

   begin
      Main_Project := No_Project;
      Automatically_Generated := False;

      Process_Project_Tree_Phase_1
        (In_Tree                => Project_Tree,
         Project                => Main_Project,
         Packages_To_Check      => Packages_To_Check,
         Success                => Success,
         From_Project_Node      => User_Project_Node,
         From_Project_Node_Tree => Project_Node_Tree,
         Env                    => Env,
         Reset_Tree             => Reset_Tree);

      if not Success then
         Main_Project := No_Project;
         return;
      end if;

      if Project_Tree.Source_Info_File_Name /= null then
         if not Is_Absolute_Path (Project_Tree.Source_Info_File_Name.all) then
            declare
               Obj_Dir : constant Variable_Value :=
                           Value_Of
                             (Name_Object_Dir,
                              Main_Project.Decl.Attributes,
                              Shared);

            begin
               if Obj_Dir = Nil_Variable_Value or else Obj_Dir.Default then
                  Get_Name_String (Main_Project.Directory.Display_Name);

               else
                  if Is_Absolute_Path (Get_Name_String (Obj_Dir.Value)) then
                     Get_Name_String (Obj_Dir.Value);

                  else
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer
                       (Get_Name_String (Main_Project.Directory.Display_Name));
                     Add_Str_To_Name_Buffer (Get_Name_String (Obj_Dir.Value));
                  end if;
               end if;

               Add_Char_To_Name_Buffer (Directory_Separator);
               Add_Str_To_Name_Buffer (Project_Tree.Source_Info_File_Name.all);
               Free (Project_Tree.Source_Info_File_Name);
               Project_Tree.Source_Info_File_Name :=
                 new String'(Name_Buffer (1 .. Name_Len));
            end;
         end if;

         Read_Source_Info_File (Project_Tree);
      end if;

      --  Get the first project that is not an aggregate project or an
      --  aggregate library project. The object directory of this project will
      --  be used to store the config project file in auto-configuration.

      Check_Project (Main_Project);

      --  Fail if there is only aggregate projects and aggregate library
      --  projects in the project tree.

      if Conf_Project = No_Project then
         Raise_Invalid_Config ("there are no non-aggregate projects");
      end if;

      --  Find configuration file

      Get_Or_Create_Configuration_File
        (Config                     => Main_Config_Project,
         Project                    => Main_Project,
         Conf_Project               => Conf_Project,
         Project_Tree               => Project_Tree,
         Project_Node_Tree          => Project_Node_Tree,
         Env                        => Env,
         Allow_Automatic_Generation => Allow_Automatic_Generation,
         Config_File_Name           => Config_File_Name,
         Autoconf_Specified         => Autoconf_Specified,
         Target_Name                => Target_Name,
         Normalized_Hostname        => Normalized_Hostname,
         Packages_To_Check          => Packages_To_Check,
         Config_File_Path           => Config_File_Path,
         Automatically_Generated    => Automatically_Generated,
         On_Load_Config             => On_Load_Config);

      Apply_Config_File (Main_Config_Project, Project_Tree);

      --  Finish processing the user's project

      Prj.Proc.Process_Project_Tree_Phase_2
        (In_Tree                => Project_Tree,
         Project                => Main_Project,
         Success                => Success,
         From_Project_Node      => User_Project_Node,
         From_Project_Node_Tree => Project_Node_Tree,
         Env                    => Env);

      if Success then
         if Project_Tree.Source_Info_File_Name /= null
           and then not Project_Tree.Source_Info_File_Exists
         then
            Write_Source_Info_File (Project_Tree);
         end if;

      else
         Main_Project := No_Project;
      end if;
   end Process_Project_And_Apply_Config;

   --------------------------
   -- Raise_Invalid_Config --
   --------------------------

   procedure Raise_Invalid_Config (Msg : String) is
   begin
      Raise_Exception (Invalid_Config'Identity, Msg);
   end Raise_Invalid_Config;

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

   --------------------------
   -- Runtime_Name_Set_For --
   --------------------------

   function Runtime_Name_Set_For (Language : Name_Id) return Boolean is
   begin
      return RTS_Languages.Get (Language) /= No_Name;
   end Runtime_Name_Set_For;

   ---------------------
   -- Set_Runtime_For --
   ---------------------

   procedure Set_Runtime_For (Language : Name_Id; RTS_Name : String) is
   begin
      Name_Len := RTS_Name'Length;
      Name_Buffer (1 .. Name_Len) := RTS_Name;
      RTS_Languages.Set (Language, Name_Find);
   end Set_Runtime_For;

end Prj.Conf;
