------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . P A R S                              --
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

with Ada.Exceptions; use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Output;   use Output;
with Prj.Conf; use Prj.Conf;
with Prj.Err;  use Prj.Err;
with Prj.Part;
with Prj.Tree; use Prj.Tree;
with Sinput.P;

package body Prj.Pars is

   -----------
   -- Parse --
   -----------

   procedure Parse
     (In_Tree           : Project_Tree_Ref;
      Project           : out Project_Id;
      Project_File_Name : String;
      Packages_To_Check : String_List_Access;
      Reset_Tree        : Boolean := True;
      In_Node_Tree      : Prj.Tree.Project_Node_Tree_Ref := null;
      Env               : in out Prj.Tree.Environment)
   is
      Project_Node            : Project_Node_Id := Empty_Node;
      The_Project             : Project_Id      := No_Project;
      Success                 : Boolean         := True;
      Current_Dir             : constant String := Get_Current_Dir;
      Project_Node_Tree       : Prj.Tree.Project_Node_Tree_Ref := In_Node_Tree;
      Automatically_Generated : Boolean;
      Config_File_Path        : String_Access;

   begin
      if Project_Node_Tree = null then
         Project_Node_Tree := new Project_Node_Tree_Data;
         Prj.Tree.Initialize (Project_Node_Tree);
      end if;

      --  Parse the main project file into a tree

      Sinput.P.Reset_First;
      Prj.Part.Parse
        (In_Tree                => Project_Node_Tree,
         Project                => Project_Node,
         Project_File_Name      => Project_File_Name,
         Errout_Handling        => Prj.Part.Finalize_If_Error,
         Packages_To_Check      => Packages_To_Check,
         Current_Directory      => Current_Dir,
         Env                    => Env,
         Is_Config_File         => False);

      --  If there were no error, process the tree

      if Project_Node /= Empty_Node then
         begin
            --  No config file should be read from the disk for gnatmake.
            --  However, we will simulate one that only contains the default
            --  GNAT naming scheme.

            --  We pass an invalid config_file_name, to prevent reading a
            --  default.cgpr that might happen to be in the current directory.

            Process_Project_And_Apply_Config
              (Main_Project               => The_Project,
               User_Project_Node          => Project_Node,
               Config_File_Name           => "",
               Autoconf_Specified         => False,
               Project_Tree               => In_Tree,
               Project_Node_Tree          => Project_Node_Tree,
               Packages_To_Check          => null,
               Allow_Automatic_Generation => False,
               Automatically_Generated    => Automatically_Generated,
               Config_File_Path           => Config_File_Path,
               Env                        => Env,
               Normalized_Hostname        => "",
               On_Load_Config             =>
                 Add_Default_GNAT_Naming_Scheme'Access,
               Reset_Tree                 => Reset_Tree);

            Success := The_Project /= No_Project;

         exception
            when Invalid_Config =>
               Success := False;
         end;

         Prj.Err.Finalize;

         if not Success then
            The_Project := No_Project;
         end if;
      end if;

      Project := The_Project;

      --  ??? Should free the project_node_tree, no longer useful

   exception
      when X : others =>

         --  Internal error

         Write_Line (Exception_Information (X));
         Write_Str  ("Exception ");
         Write_Str  (Exception_Name (X));
         Write_Line (" raised, while processing project file");
         Project := No_Project;
   end Parse;

   -------------------
   -- Set_Verbosity --
   -------------------

   procedure Set_Verbosity (To : Verbosity) is
   begin
      Current_Verbosity := To;
   end Set_Verbosity;

end Prj.Pars;
