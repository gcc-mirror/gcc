------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M D L L . T O O L S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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

--  Interface to externals tools used to build DLL and import libraries

package MDLL.Tools is

   procedure Delete_File (Filename : in String);
   --  delete the file filename from the file system.

   procedure Dlltool (Def_Filename : in String;
                      DLL_Name     : in String;
                      Library      : in String;
                      Exp_Table    : in String := "";
                      Base_File    : in String := "";
                      Build_Import : in Boolean);
   --  run dlltool binary.
   --  this tools is used to build an import library and an export table

   procedure Gcc (Output_File : in String;
                  Files       : in Argument_List;
                  Options     : in Argument_List;
                  Base_File   : in String := "";
                  Build_Lib   : in Boolean := False);
   --  run gcc binary.

   procedure Gnatbind (Alis : in Argument_List;
                       Args : in Argument_List := Null_Argument_List);
   --  run gnatbind binary to build the binder program.
   --  it runs the command : gnatbind -n alis... to build the binder program.

   procedure Gnatlink (Ali  : in String;
                       Args : in Argument_List := Null_Argument_List);
   --  run gnatlink binary.
   --  it runs the command : gnatlink ali arg1 arg2...

   procedure Locate;
   --  look for the needed tools in the path and record the full path for each
   --  one in a variable.

end MDLL.Tools;
