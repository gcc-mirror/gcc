------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M D L L                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the core high level routines used by GNATDLL
--  to build Windows DLL

with GNAT.OS_Lib;

package MDLL is

   subtype Argument_List is GNAT.OS_Lib.Argument_List;
   subtype Argument_List_Access is GNAT.OS_Lib.Argument_List_Access;

   Null_Argument_List : constant Argument_List := (1 .. 0 => new String'(""));

   Null_Argument_List_Access : Argument_List_Access
     := new Argument_List (1 .. 0);

   Tools_Error    : exception;

   Verbose        : Boolean := False;
   Quiet          : Boolean := False;

   --  Kill_Suffix is used by dlltool to know whether or not the @nn suffix
   --  should be removed from the exported names. When Kill_Suffix is set to
   --  True then dlltool -k option is used.

   Kill_Suffix    : Boolean := False;

   procedure Build_Dynamic_Library (Ofiles        : in Argument_List;
                                    Afiles        : in Argument_List;
                                    Options       : in Argument_List;
                                    Bargs_Options : in Argument_List;
                                    Largs_Options : in Argument_List;
                                    Lib_Filename  : in String;
                                    Def_Filename  : in String;
                                    Lib_Address   : in String  := "";
                                    Build_Import  : in Boolean := False;
                                    Relocatable   : in Boolean := False);
   --  build a DLL and the import library to link against the DLL.
   --  this function handles relocatable and non relocatable DLL.
   --  If the Afiles argument list contains some Ada units then it will
   --  generate the right adainit and adafinal and integrate it in the DLL.
   --  If the Afiles argument list is empty (there is only some object files
   --  provided) then it will not try to build a binder file. This is ok to
   --  build DLL containing no Ada code.

   procedure Build_Import_Library (Lib_Filename : in String;
                                   Def_Filename : in String);
   --  Build an import library (.a) from a definition files. An import library
   --  is needed to link against a DLL.

end MDLL;
