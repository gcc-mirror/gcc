------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     M L I B . T G T. S P E C I F I C                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2011, Free Software Foundation, Inc.         --
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

--  This is the bare board version of the body

with Sdefault;
with Types; use Types;

package body MLib.Tgt.Specific is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Get_Target_Prefix return String;
   --  Returns the required prefix for some utilities
   --  (such as ar and ranlib) that depend on the real target.

   --  Non default subprograms

   function Archive_Builder return String;

   function Archive_Indexer return String;

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Version  : String  := "";
      Auto_Init    : Boolean := False);

   function DLL_Ext return String;

   function Dynamic_Option return String;

   function Library_Major_Minor_Id_Supported return Boolean;

   function PIC_Option return String;

   function Standalone_Library_Auto_Init_Is_Supported return Boolean;

   function Support_For_Libraries return Library_Support;

   ---------------------
   -- Archive_Builder --
   ---------------------

   function Archive_Builder return String is
   begin
      return Get_Target_Prefix & "ar";
   end Archive_Builder;

   ---------------------
   -- Archive_Indexer --
   ---------------------

   function Archive_Indexer return String is
   begin
      return Get_Target_Prefix & "ranlib";
   end Archive_Indexer;

   ---------------------------
   -- Build_Dynamic_Library --
   ---------------------------

   procedure Build_Dynamic_Library
     (Ofiles       : Argument_List;
      Options      : Argument_List;
      Interfaces   : Argument_List;
      Lib_Filename : String;
      Lib_Dir      : String;
      Symbol_Data  : Symbol_Record;
      Driver_Name  : Name_Id := No_Name;
      Lib_Version  : String  := "";
      Auto_Init    : Boolean := False)
   is
      pragma Unreferenced (Ofiles);
      pragma Unreferenced (Options);
      pragma Unreferenced (Interfaces);
      pragma Unreferenced (Lib_Filename);
      pragma Unreferenced (Lib_Dir);
      pragma Unreferenced (Symbol_Data);
      pragma Unreferenced (Driver_Name);
      pragma Unreferenced (Lib_Version);
      pragma Unreferenced (Auto_Init);

   begin
      null;
   end Build_Dynamic_Library;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "";
   end DLL_Ext;

   --------------------
   -- Dynamic_Option --
   --------------------

   function Dynamic_Option return String is
   begin
      return "";
   end Dynamic_Option;

   -----------------------
   -- Get_Target_Prefix --
   -----------------------

   function Get_Target_Prefix return String is
      Target_Name : constant String_Ptr := Sdefault.Target_Name;

   begin
      --  Target_name is the program prefix without '-' but with a trailing '/'

      return Target_Name (Target_Name'First .. Target_Name'Last - 1) & '-';
   end Get_Target_Prefix;

   --------------------------------------
   -- Library_Major_Minor_Id_Supported --
   --------------------------------------

   function Library_Major_Minor_Id_Supported return Boolean is
   begin
      return False;
   end Library_Major_Minor_Id_Supported;

   ----------------
   -- PIC_Option --
   ----------------

   function PIC_Option return String is
   begin
      return "";
   end PIC_Option;

   -----------------------------------------------
   -- Standalone_Library_Auto_Init_Is_Supported --
   -----------------------------------------------

   function Standalone_Library_Auto_Init_Is_Supported return Boolean is
   begin
      return False;
   end Standalone_Library_Auto_Init_Is_Supported;

   ---------------------------
   -- Support_For_Libraries --
   ---------------------------

   function Support_For_Libraries return Library_Support is
   begin
      return Static_Only;
   end Support_For_Libraries;

begin
   Archive_Builder_Ptr := Archive_Builder'Access;
   Archive_Indexer_Ptr := Archive_Indexer'Access;
   Build_Dynamic_Library_Ptr := Build_Dynamic_Library'Access;
   DLL_Ext_Ptr := DLL_Ext'Access;
   Dynamic_Option_Ptr := Dynamic_Option'Access;
   Library_Major_Minor_Id_Supported_Ptr :=
                                Library_Major_Minor_Id_Supported'Access;
   PIC_Option_Ptr := PIC_Option'Access;
   Standalone_Library_Auto_Init_Is_Supported_Ptr :=
     Standalone_Library_Auto_Init_Is_Supported'Access;
   Support_For_Libraries_Ptr := Support_For_Libraries'Access;
end MLib.Tgt.Specific;
