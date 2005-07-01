------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M D L L . T O O L S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  Interface to externals tools used to build DLL and import libraries

package MDLL.Utl is

   procedure Dlltool
     (Def_Filename : String;
      DLL_Name     : String;
      Library      : String;
      Exp_Table    : String := "";
      Base_File    : String := "";
      Build_Import : Boolean);
   --  Run dlltool binary.
   --  This tools is used to build an import library and an export table

   procedure Gcc
     (Output_File : String;
      Files       : Argument_List;
      Options     : Argument_List;
      Base_File   : String := "";
      Build_Lib   : Boolean := False);
   --  Run gcc binary.

   procedure Gnatbind
     (Alis : Argument_List;
      Args : Argument_List := Null_Argument_List);
   --  Run gnatbind binary to build the binder program.
   --  it Runs the command : gnatbind -n alis... to build the binder program.

   procedure Gnatlink
     (Ali  : String;
      Args : Argument_List := Null_Argument_List);
   --  Run gnatlink binary.
   --  It runs the command : gnatlink ali arg1 arg2...

   procedure Locate;
   --  Look for the needed tools in the path and record the full path for each
   --  one in a variable.

end MDLL.Utl;
