------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         M L I B . T G T . V M S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2007, Free Software Foundation, Inc.         --
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

--  This is the part of MLib.Tgt.Specific common to both VMS versions

package body MLib.Tgt.VMS is

   --  Non default subprograms. See comments in mlib-tgt.ads

   function Archive_Ext return String;

   function Default_Symbol_File_Name return String;

   function DLL_Ext return String;

   function Is_Object_Ext (Ext : String) return Boolean;

   function Is_Archive_Ext (Ext : String) return Boolean;

   function Libgnat return String;

   function Object_Ext return String;

   function PIC_Option return String;

   -----------------
   -- Archive_Ext --
   -----------------

   function Archive_Ext return String is
   begin
      return "olb";
   end Archive_Ext;

   ------------------------------
   -- Default_Symbol_File_Name --
   ------------------------------

   function Default_Symbol_File_Name return String is
   begin
      return "symvec.opt";
   end Default_Symbol_File_Name;

   -------------
   -- DLL_Ext --
   -------------

   function DLL_Ext return String is
   begin
      return "exe";
   end DLL_Ext;

   -------------------
   -- Is_Object_Ext --
   -------------------

   function Is_Object_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".obj";
   end Is_Object_Ext;

   --------------------
   -- Is_Archive_Ext --
   --------------------

   function Is_Archive_Ext (Ext : String) return Boolean is
   begin
      return Ext = ".olb" or else Ext = ".exe";
   end Is_Archive_Ext;

   -------------
   -- Libgnat --
   -------------

   function Libgnat return String is
      Libgnat_A : constant String := "libgnat.a";
      Libgnat_Olb : constant String := "libgnat.olb";

   begin
      Name_Len := Libgnat_A'Length;
      Name_Buffer (1 .. Name_Len) := Libgnat_A;

      if Osint.Find_File (Name_Enter, Osint.Library) /= No_File then
         return Libgnat_A;
      else
         return Libgnat_Olb;
      end if;
   end Libgnat;

   ----------------
   -- Object_Ext --
   ----------------

   function Object_Ext return String is
   begin
      return "obj";
   end Object_Ext;

   ----------------
   -- PIC_Option --
   ----------------

   function PIC_Option return String is
   begin
      return "";
   end PIC_Option;

--  Package initialization

begin
   Archive_Ext_Ptr              := Archive_Ext'Access;
   Default_Symbol_File_Name_Ptr := Default_Symbol_File_Name'Access;
   DLL_Ext_Ptr                  := DLL_Ext'Access;
   Is_Object_Ext_Ptr            := Is_Object_Ext'Access;
   Is_Archive_Ext_Ptr           := Is_Archive_Ext'Access;
   Libgnat_Ptr                  := Libgnat'Access;
   Object_Ext_Ptr               := Object_Ext'Access;
   PIC_Option_Ptr               := PIC_Option'Access;
end MLib.Tgt.VMS;
