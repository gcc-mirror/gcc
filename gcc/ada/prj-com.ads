------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . C O M                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  The following package declares data types for GNAT project.
--  These data types are used in the bodies of the Prj hierarchy.

with GNAT.HTable;
with Table;
with Types; use Types;

package Prj.Com is

   --  At one point, this package was private.
   --  It cannot be private, because it is used outside of
   --  the Prj hierarchy.

   Tool_Name : Name_Id := No_Name;

   Current_Verbosity : Verbosity := Default;

   type Spec_Or_Body is
     (Specification, Body_Part);

   type File_Name_Data is record
      Name         : Name_Id := No_Name;
      Path         : Name_Id := No_Name;
      Project      : Project_Id := No_Project;
      Needs_Pragma : Boolean := False;
   end record;
   --  File and Path name of a spec or body.

   type File_Names_Data is array (Spec_Or_Body) of File_Name_Data;

   type Unit_Id is new Nat;
   No_Unit : constant Unit_Id := 0;
   type Unit_Data is record
      Name       : Name_Id    := No_Name;
      File_Names : File_Names_Data;
   end record;
   --  File and Path names of a unit, with a reference to its
   --  GNAT Project File.

   package Units is new Table.Table
     (Table_Component_Type => Unit_Data,
      Table_Index_Type     => Unit_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 100,
      Table_Name           => "Prj.Com.Units");

   type Header_Num is range 0 .. 2047;

   function Hash is new GNAT.HTable.Hash (Header_Num => Header_Num);

   function Hash (Name : Name_Id) return Header_Num;

   function Hash (Name : String_Id) return Header_Num;

   package Units_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Unit_Id,
      No_Element => No_Unit,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

end Prj.Com;
