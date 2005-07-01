------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . P A R S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2005 Free Software Foundation, Inc.       --
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

with Ada.Exceptions; use Ada.Exceptions;

with Opt;
with Output;   use Output;
with Prj.Err;  use Prj.Err;
with Prj.Part;
with Prj.Proc;
with Prj.Tree; use Prj.Tree;

package body Prj.Pars is

   -----------
   -- Parse --
   -----------

   procedure Parse
     (In_Tree           : Project_Tree_Ref;
      Project           : out Project_Id;
      Project_File_Name : String;
      Packages_To_Check : String_List_Access := All_Packages)
   is
      Project_Node_Tree : constant Project_Node_Tree_Ref :=
                            new Project_Node_Tree_Data;
      Project_Node      : Project_Node_Id := Empty_Node;
      The_Project       : Project_Id      := No_Project;
      Success           : Boolean         := True;

   begin
      Prj.Tree.Initialize (Project_Node_Tree);

      --  Parse the main project file into a tree

      Prj.Part.Parse
        (In_Tree                => Project_Node_Tree,
         Project                => Project_Node,
         Project_File_Name      => Project_File_Name,
         Always_Errout_Finalize => False,
         Packages_To_Check      => Packages_To_Check);

      --  If there were no error, process the tree

      if Project_Node /= Empty_Node then
         Prj.Proc.Process
           (In_Tree                => In_Tree,
            Project                => The_Project,
            Success                => Success,
            From_Project_Node      => Project_Node,
            From_Project_Node_Tree => Project_Node_Tree,
            Report_Error           => null,
            Follow_Links           => Opt.Follow_Links);
         Prj.Err.Finalize;

         if not Success then
            The_Project := No_Project;
         end if;
      end if;

      Project := The_Project;

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

   procedure Set_Verbosity (To : in Verbosity) is
   begin
      Current_Verbosity := To;
   end Set_Verbosity;

end Prj.Pars;
