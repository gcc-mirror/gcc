------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . C O N F                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006-2011, Free Software Foundation, Inc.       --
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

--  The following package manipulates the configuration files

with Prj.Tree;

package Prj.Conf is

   type Config_File_Hook is access procedure
     (Config_File       : in out Prj.Tree.Project_Node_Id;
      Project_Node_Tree : Prj.Tree.Project_Node_Tree_Ref);
   --  Hook called after the config file has been parsed. This lets the
   --  application do last minute changes to it (GPS uses this to add the
   --  default naming schemes for instance). At that point, the config file
   --  has not been applied to the project yet. When no config file was found,
   --  and automatic generation is disabled, it is possible that Config_File
   --  is set to Empty_Node when this procedure is called. You can then decide
   --  to create a new config file if you need.

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
      On_Load_Config             : Config_File_Hook := null);
   --  Find the main configuration project and parse the project tree rooted at
   --  this configuration project.
   --
   --  Project_Node_Tree must have been initialized first (and possibly the
   --  value for external references and project path should also have been
   --  set).
   --
   --  If the processing fails, Main_Project is set to No_Project. If the error
   --  happened while parsing the project itself (i.e. creating the tree),
   --  User_Project_Node is also set to Empty_Node.
   --
   --  Autoconf_Specified indicates whether the user has specified --autoconf.
   --  If this is the case, the config file might be (re)generated, as
   --  appropriate, to match languages and target if the one specified doesn't
   --  already match.
   --
   --  Normalized_Hostname is the host on which gprbuild is returned,
   --  normalized so that we can more easily compare it with what is stored in
   --  configuration files. It is used when the target is unspecified, although
   --  we need to know the target specified by the user (Target_Name) when
   --  computing the name of the default config file that should be used.
   --
   --  If specified, On_Load_Config is called just after the config file has
   --  been created/loaded. You can then modify it before it is later applied
   --  to the project itself.
   --
   --  Any error in generating or parsing the config file is reported via the
   --  Invalid_Config exception, with an appropriate message. Any error while
   --  parsing the project file results in No_Project.

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
      Reset_Tree                 : Boolean := True);
   --  Same as above, except the project must already have been parsed through
   --  Prj.Part.Parse, and only the processing of the project and the
   --  configuration is done at this level.
   --
   --  If Reset_Tree is true, all projects are first removed from the tree.
   --  When_No_Sources indicates what should be done when no sources are found
   --  for one of the languages of the project.
   --
   --  If Require_Sources_Other_Lang is true, then all languages must have at
   --  least one source file, or an error is reported via When_No_Sources. If
   --  it is false, this is only required for Ada (and only if it is a language
   --  of the project).

   Invalid_Config : exception;

   procedure Get_Or_Create_Configuration_File
     (Project                    : Prj.Project_Id;
      Project_Tree               : Prj.Project_Tree_Ref;
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
      On_Load_Config             : Config_File_Hook := null);
   --  Compute the name of the configuration file that should be used. If no
   --  default configuration file is found, a new one will be automatically
   --  generated if Allow_Automatic_Generation is true.
   --
   --  Any error in generating or parsing the config file is reported via the
   --  Invalid_Config exception, with an appropriate message.
   --
   --  On exit, Configuration_Project_Path is never null (if none could be
   --  found, Os.Fail was called and the program exited anyway).
   --
   --  The choice and generation of a configuration file depends on several
   --  attributes of the user's project file (given by the Project argument),
   --  e.g. list of languages that must be supported. Project must therefore
   --  have been partially processed (phase one of the processing only).
   --
   --  Config_File_Name should be set to the name of the config file specified
   --  by the user (either through gprbuild's --config or --autoconf switches).
   --  In the latter case, Autoconf_Specified should be set to true to indicate
   --  that the configuration file can be regenerated to match target and
   --  languages. This name can either be an absolute path, or the base name
   --  that will be searched in the default config file directories (which
   --  depends on the installation path for the tools).
   --
   --  Target_Name is used to chose the configuration file that will be used
   --  from among several possibilities.
   --
   --  If a project file could be found, it is automatically parsed and
   --  processed (and Packages_To_Check is used to indicate which packages
   --  should be processed)

   procedure Add_Default_GNAT_Naming_Scheme
     (Config_File  : in out Prj.Tree.Project_Node_Id;
      Project_Tree : Prj.Tree.Project_Node_Tree_Ref);
   --  A hook that will create a new config file (in memory), used for
   --  Get_Or_Create_Configuration_File and Process_Project_And_Apply_Config
   --  and add the default GNAT naming scheme to it. Nothing is done if the
   --  config_file already exists, to avoid overriding what the user might
   --  have put in there.

   --------------
   -- Runtimes --
   --------------

   procedure Set_Runtime_For (Language : Name_Id; RTS_Name : String);
   --  Specifies the runtime to use for a specific language. Most of the time
   --  this should be used for Ada, but other languages can also specify their
   --  own runtime. This is in general specified via the --RTS command line
   --  switch, and results in a specific component passed to gprconfig's
   --  --config switch then automatically generating a configuration file.

   function Runtime_Name_For (Language : Name_Id) return String;
   --  Returns the runtime name for a language. Returns an empty string if no
   --  runtime was specified for the language using option --RTS.

   function Runtime_Name_Set_For (Language : Name_Id) return Boolean;
   --  Returns True only if Set_Runtime_For has been called for the Language

end Prj.Conf;
