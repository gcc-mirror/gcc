------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . P A R S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2002 Free Software Foundation, Inc.       --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;

with Prj.Err;  use Prj.Err;
with Output;   use Output;
with Prj.Com;  use Prj.Com;
with Prj.Part;
with Prj.Proc;
with Prj.Tree; use Prj.Tree;

package body Prj.Pars is

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Project           : out Project_Id;
      Project_File_Name : String;
      Packages_To_Check : String_List_Access := All_Packages)
   is
      Project_Tree      : Project_Node_Id := Empty_Node;
      The_Project       : Project_Id      := No_Project;
      Success           : Boolean         := True;

   begin
      --  Parse the main project file into a tree

      Prj.Part.Parse
        (Project                => Project_Tree,
         Project_File_Name      => Project_File_Name,
         Always_Errout_Finalize => False,
         Packages_To_Check      => Packages_To_Check);

      --  If there were no error, process the tree

      if Project_Tree /= Empty_Node then
         Prj.Proc.Process
           (Project           => The_Project,
            Success           => Success,
            From_Project_Node => Project_Tree,
            Report_Error      => null);
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
