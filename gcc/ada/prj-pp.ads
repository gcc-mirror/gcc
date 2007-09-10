------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               P R J . P P                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2007, Free Software Foundation, Inc.         --
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

--  This package is the Project File Pretty Printer

--    Used to output a project file from a project file tree.
--    Used by gnatname to update or create project files.
--    Also used GPS to display project file trees.
--    Also be used for debugging tools that create project file trees.

with Prj.Tree;

package Prj.PP is

   --  The following access to procedure types are used to redirect output when
   --  calling Pretty_Print.

   type Write_Char_Ap is access procedure (C : Character);

   type Write_Eol_Ap  is access procedure;

   type Write_Str_Ap is access procedure (S : String);

   procedure Pretty_Print
     (Project                            : Prj.Tree.Project_Node_Id;
      In_Tree                            : Prj.Tree.Project_Node_Tree_Ref;
      Increment                          : Positive      := 3;
      Eliminate_Empty_Case_Constructions : Boolean       := False;
      Minimize_Empty_Lines               : Boolean       := False;
      W_Char                             : Write_Char_Ap := null;
      W_Eol                              : Write_Eol_Ap  := null;
      W_Str                              : Write_Str_Ap  := null;
      Backward_Compatibility             : Boolean);
   --  Output a project file, using either the default output routines, or the
   --  ones specified by W_Char, W_Eol and W_Str.
   --
   --  Increment is the number of spaces for each indentation level.
   --
   --  W_Char, W_Eol and W_Str can be used to change the default output
   --  procedures. The default values force the output to Standard_Output.
   --
   --  If Eliminate_Empty_Case_Constructions is True, then case constructions
   --  and case items that do not include any declarations will not be output.
   --
   --  If Minimize_Empty_Lines is True, empty lines will be output only after
   --  the last with clause, after the line declaring the project name, after
   --  the last declarative item of the project and before each package
   --  declaration. Otherwise, more empty lines are output.
   --
   --  If Backward_Compatibility is True, then new attributes (Spec,
   --  Spec_Suffix, Body, Body_Suffix) will be replaced by obsolete ones
   --  (Specification, Specification_Suffix, Implementation,
   --  Implementation_Suffix).

private

   procedure Output_Statistics;
   --  This procedure can be used after one or more calls to Pretty_Print to
   --  display what Project_Node_Kinds have not been exercised by the call(s)
   --  to Pretty_Print. It is used only for testing purposes.

end Prj.PP;
