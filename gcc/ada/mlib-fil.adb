------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M L I B . F I L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
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

--  This package provides a set of routines to deal with file extensions

with Ada.Strings.Fixed;
with MLib.Tgt;

package body MLib.Fil is

   use Ada;

   package Target renames MLib.Tgt;

   ------------
   -- Ext_To --
   ------------

   function Ext_To
     (Filename : String;
      New_Ext  : String := "")
      return     String
   is
      use Strings.Fixed;
      J : constant Natural :=
            Index (Source  =>  Filename,
                   Pattern => ".",
                   Going   => Strings.Backward);

   begin
      if J = 0 then
         if New_Ext = "" then
            return Filename;
         else
            return Filename & "." & New_Ext;
         end if;

      else
         if New_Ext = "" then
            return Head (Filename, J - 1);
         else
            return Head (Filename, J - 1) & '.' & New_Ext;
         end if;
      end if;
   end Ext_To;

   -------------
   -- Get_Ext --
   -------------

   function Get_Ext (Filename : in String) return String is
      use Strings.Fixed;

      J : constant Natural :=
            Index (Source  =>  Filename,
                   Pattern => ".",
                   Going   => Strings.Backward);

   begin
      if J = 0 then
         return "";
      else
         return Filename (J .. Filename'Last);
      end if;
   end Get_Ext;

   ----------------
   -- Is_Archive --
   ----------------

   function Is_Archive (Filename : String) return Boolean is
      Ext : constant String := Get_Ext (Filename);

   begin
      return Target.Is_Archive_Ext (Ext);
   end Is_Archive;

   ----------
   -- Is_C --
   ----------

   function Is_C (Filename : in String) return Boolean is
      Ext : constant String := Get_Ext (Filename);

   begin
      return Target.Is_C_Ext (Ext);
   end Is_C;

   ------------
   -- Is_Obj --
   ------------

   function Is_Obj (Filename : in String) return Boolean is
      Ext : constant String := Get_Ext (Filename);

   begin
      return Target.Is_Object_Ext (Ext);
   end Is_Obj;

end MLib.Fil;
