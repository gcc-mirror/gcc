------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . C O M                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2004 Free Software Foundation, Inc.          --
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
with Osint;
with Table;
with Types; use Types;

package Prj.Com is

   --  At one point, this package was private.
   --  It cannot be private, because it is used outside of
   --  the Prj hierarchy.

   type Fail_Proc is access procedure
     (S1 : String; S2 : String := ""; S3 : String := "");

   Fail : Fail_Proc := Osint.Fail'Access;
   --  This procedure is used in the project facility, instead of
   --  directly calling Osint.Fail.
   --  It may be specified by tools to do clean up before calling
   --  Osint.Fail, or to simply report an error and return.

   Tool_Name : Name_Id := No_Name;

   Current_Verbosity : Verbosity := Default;

   type Spec_Or_Body is
     (Specification, Body_Part);

   type File_Name_Data is record
      Name         : Name_Id := No_Name;
      Index        : Int     := 0;
      Display_Name : Name_Id := No_Name;
      Path         : Name_Id := No_Name;
      Display_Path : Name_Id := No_Name;
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

   function Hash (Name : String_Id) return Header_Num;

   package Units_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Unit_Id,
      No_Element => No_Unit,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping of unit names to indexes in the Units table

   type Unit_Project is record
      Unit    : Unit_Id    := No_Unit;
      Project : Project_Id := No_Project;
   end record;

   No_Unit_Project : constant Unit_Project := (No_Unit, No_Project);

   package Files_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Unit_Project,
      No_Element => No_Unit_Project,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Mapping of file names to indexes in the Units table

end Prj.Com;
