------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . T G T                              --
--                            (Default Version)                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--              Copyright (C) 2001, Ada Core Technologies, Inc.             --
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

--  This is the default version which does not support libraries.
--  All subprograms are dummies, because they are never called,
--  except Libraries_Are_Supported which returns False.

package body MLib.Tgt is

   -----------------
   -- Archive_Ext --
   -----------------

   function Archive_Ext return  String is
   begin
      return  "";
   end Archive_Ext;

   -----------------
   -- Base_Option --
   -----------------

   function Base_Option return String is
   begin
      return "";
   end Base_Option;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Foreign      : Argument_List;
      Afiles       : Argument_List;
      Options      : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Lib_Address  : String  := "";
      Lib_Version  : String  := "";
      Relocatable  : Boolean := False)
   is
   begin
      null;
   end Build_Dynamic_Library;

   --------------------
   -- Copy_ALI_Files --
   --------------------

   procedure Copy_ALI_Files
     (From : Name_Id;
      To   : Name_Id)
   is
   begin
      null;
   end Copy_ALI_Files;

   -------------------------
   -- Default_DLL_Address --
   -------------------------

   function Default_DLL_Address return String is
   begin
      return "";
   end Default_DLL_Address;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return  "";
   end DLL_Ext;

   --------------------
   -- Dynamic_Option --
   --------------------

   function Dynamic_Option return String is
   begin
      return  "";
   end Dynamic_Option;

   -------------------
   -- Is_Object_Ext --
   -------------------

   function Is_Object_Ext (Ext : String) return Boolean is
   begin
      return False;
   end Is_Object_Ext;

   --------------
   -- Is_C_Ext --
   --------------

   function Is_C_Ext (Ext : String) return Boolean is
   begin
      return False;
   end Is_C_Ext;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
   begin
      return False;
   end Is_Archive_Ext;

   -------------
   -- Libgnat --
   -------------

   function Libgnat return String is
   begin
      return "libgnat.a";
   end Libgnat;

   -----------------------------
   -- Libraries_Are_Supported --
   -----------------------------

   function Libraries_Are_Supported return Boolean is
   begin
      return False;
   end Libraries_Are_Supported;

   --------------------------------
   -- Linker_Library_Path_Option --
   --------------------------------

   function Linker_Library_Path_Option
     (Directory : String)
      return      String_Access
   is
   begin
      return null;
   end Linker_Library_Path_Option;

   ----------------
   -- Object_Ext --
   ----------------

   function Object_Ext return String is
   begin
      return  "";
   end Object_Ext;

   ----------------
   -- PIC_Option --
   ----------------

   function PIC_Option return String is
   begin
      return  "";
   end PIC_Option;

end MLib.Tgt;
