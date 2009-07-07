------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . A T T R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2009, Free Software Foundation, Inc.         --
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

--  This package defines packages and attributes in GNAT project files.
--  There are predefined packages and attributes.

--  It is also possible to define new packages with their attributes

with Table;

with GNAT.Strings;

package Prj.Attr is

   function Package_Name_List return GNAT.Strings.String_List;
   --  Returns the list of valid package names, including those added by
   --  procedures Register_New_Package below. The String_Access components of
   --  the returned String_List should never be freed.

   procedure Initialize;
   --  Initialize the predefined project level attributes and the predefined
   --  packages and their attribute. This procedure should be called by
   --  Prj.Initialize.

   type Attribute_Kind is
     (Unknown,
      Single,
      Associative_Array,
      Optional_Index_Associative_Array,
      Case_Insensitive_Associative_Array,
      Optional_Index_Case_Insensitive_Associative_Array);
   --  Characteristics of an attribute. Optional_Index indicates that there
   --  may be an optional index in the index of the associative array, as in
   --     for Switches ("files.ada" at 2) use ...
   --  Above character literals should be documented ???

   subtype Defined_Attribute_Kind is Attribute_Kind
     range Single .. Optional_Index_Case_Insensitive_Associative_Array;
   --  Subset of Attribute_Kinds that may be used for the attributes that is
   --  used when defining a new package.

   Max_Attribute_Name_Length : constant := 64;
   --  The maximum length of attribute names

   subtype Attribute_Name_Length is
     Positive range 1 .. Max_Attribute_Name_Length;

   type Attribute_Data (Name_Length : Attribute_Name_Length := 1) is record
      Name : String (1 .. Name_Length);
      --  The name of the attribute

      Attr_Kind  : Defined_Attribute_Kind;
      --  The type of the attribute

      Index_Is_File_Name : Boolean;
      --  For associative arrays, indicate if the index is a file name, so
      --  that the attribute kind may be modified depending on the case
      --  sensitivity of file names. This is only taken into account when
      --  Attr_Kind is Associative_Array or Optional_Index_Associative_Array.

      Opt_Index : Boolean;
      --  True if there may be an optional index in the value of the index,
      --  as in:
      --    "file.ada" at 2
      --    ("main.adb", "file.ada" at 1)

      Var_Kind : Defined_Variable_Kind;
      --  The attribute value kind: single or list

   end record;
   --  Name and characteristics of an attribute in a package registered
   --  explicitly with Register_New_Package (see below).

   type Attribute_Data_Array is array (Positive range <>) of Attribute_Data;
   --  A list of attribute name/characteristics to be used as parameter of
   --  procedure Register_New_Package below.

   --  In the subprograms below, when it is specified that the subprogram
   --  "fails", procedure Prj.Com.Fail is called. Unless it is specified
   --  otherwise, if Prj.Com.Fail returns, exception Prj.Prj_Error is raised.

   procedure Register_New_Package
     (Name       : String;
      Attributes : Attribute_Data_Array);
   --  Add a new package with its attributes. This procedure can only be
   --  called after Initialize, but before any other call to a service of
   --  the Project Manager. Fail if the name of the package is empty or not
   --  unique, or if the names of the attributes are not different.

   ----------------
   -- Attributes --
   ----------------

   type Attribute_Node_Id is private;
   --  The type to refers to an attribute, self-initialized

   Empty_Attribute : constant Attribute_Node_Id;
   --  Indicates no attribute. Default value of Attribute_Node_Id objects

   Attribute_First : constant Attribute_Node_Id;
   --  First attribute node id of project level attributes

   function Attribute_Node_Id_Of
     (Name        : Name_Id;
      Starting_At : Attribute_Node_Id) return Attribute_Node_Id;
   --  Returns the node id of an attribute at the project level or in
   --  a package. Starting_At indicates the first known attribute node where
   --  to start the search. Returns Empty_Attribute if the attribute cannot
   --  be found.

   function Attribute_Kind_Of
     (Attribute : Attribute_Node_Id) return Attribute_Kind;
   --  Returns the attribute kind of a known attribute. Returns Unknown if
   --  Attribute is Empty_Attribute.

   procedure Set_Attribute_Kind_Of
     (Attribute : Attribute_Node_Id;
      To        : Attribute_Kind);
   --  Set the attribute kind of a known attribute. Does nothing if
   --  Attribute is Empty_Attribute.

   function Attribute_Name_Of (Attribute : Attribute_Node_Id) return Name_Id;
   --  Returns the name of a known attribute. Returns No_Name if Attribute is
   --  Empty_Attribute.

   function Variable_Kind_Of
     (Attribute : Attribute_Node_Id) return Variable_Kind;
   --  Returns the variable kind of a known attribute. Returns Undefined if
   --  Attribute is Empty_Attribute.

   procedure Set_Variable_Kind_Of
     (Attribute : Attribute_Node_Id;
      To        : Variable_Kind);
   --  Set the variable kind of a known attribute. Does nothing if Attribute is
   --  Empty_Attribute.

   function Optional_Index_Of (Attribute : Attribute_Node_Id) return Boolean;
   --  Returns True if Attribute is a known attribute and may have an
   --  optional index. Returns False otherwise.

   function Is_Read_Only (Attribute : Attribute_Node_Id) return Boolean;

   function Next_Attribute
     (After : Attribute_Node_Id) return Attribute_Node_Id;
   --  Returns the attribute that follow After in the list of project level
   --  attributes or the list of attributes in a package.
   --  Returns Empty_Attribute if After is either Empty_Attribute or is the
   --  last of the list.

   function Others_Allowed_For (Attribute : Attribute_Node_Id) return Boolean;
   --  True iff the index for an associative array attributes may be others

   --------------
   -- Packages --
   --------------

   type Package_Node_Id is private;
   --  Type to refer to a package, self initialized

   Empty_Package : constant Package_Node_Id;
   --  Default value of Package_Node_Id objects

   Unknown_Package : constant Package_Node_Id;
   --  Value of an unknown package that has been found but is unknown

   procedure Register_New_Package (Name : String; Id : out Package_Node_Id);
   --  Add a new package. Fails if Name (the package name) is empty or is
   --  already the name of a package, and set Id to Empty_Package,
   --  if Prj.Com.Fail returns. Initially, the new package has no attributes.
   --  Id may be used to add attributes using procedure Register_New_Attribute
   --  below.

   procedure Register_New_Attribute
     (Name               : String;
      In_Package         : Package_Node_Id;
      Attr_Kind          : Defined_Attribute_Kind;
      Var_Kind           : Defined_Variable_Kind;
      Index_Is_File_Name : Boolean := False;
      Opt_Index          : Boolean := False);
   --  Add a new attribute to registered package In_Package. Fails if Name
   --  (the attribute name) is empty, if In_Package is Empty_Package or if
   --  the attribute name has a duplicate name. See definition of type
   --  Attribute_Data above for the meaning of parameters Attr_Kind, Var_Kind,
   --  Index_Is_File_Name and Opt_Index.

   function Package_Node_Id_Of (Name : Name_Id) return Package_Node_Id;
   --  Returns the package node id of the package with name Name. Returns
   --  Empty_Package if there is no package with this name.

   function First_Attribute_Of
     (Pkg : Package_Node_Id) return Attribute_Node_Id;
   --  Returns the first attribute in the list of attributes of package Pkg.
   --  Returns Empty_Attribute if Pkg is Empty_Package.

private
   ----------------
   -- Attributes --
   ----------------

   Attributes_Initial   : constant := 50;
   Attributes_Increment : constant := 100;

   Attribute_Node_Low_Bound  : constant := 0;
   Attribute_Node_High_Bound : constant := 099_999_999;

   type Attr_Node_Id is
     range Attribute_Node_Low_Bound .. Attribute_Node_High_Bound;
   --  Index type for table Attrs in the body

   type Attribute_Node_Id is record
      Value : Attr_Node_Id := Attribute_Node_Low_Bound;
   end record;
   --  Full declaration of self-initialized private type

   Empty_Attr : constant Attr_Node_Id := Attribute_Node_Low_Bound;

   Empty_Attribute : constant Attribute_Node_Id := (Value => Empty_Attr);

   First_Attribute : constant Attr_Node_Id := Attribute_Node_Low_Bound + 1;

   First_Attribute_Node_Id : constant Attribute_Node_Id :=
                               (Value => First_Attribute);

   Attribute_First : constant Attribute_Node_Id := First_Attribute_Node_Id;

   --------------
   -- Packages --
   --------------

   Packages_Initial   : constant := 10;
   Packages_Increment : constant := 100;

   Package_Node_Low_Bound  : constant := 0;
   Package_Node_High_Bound : constant := 099_999_999;

   type Pkg_Node_Id is
     range Package_Node_Low_Bound .. Package_Node_High_Bound;
   --  Index type for table Package_Attributes in the body

   type Package_Node_Id is record
      Value : Pkg_Node_Id := Package_Node_Low_Bound;
   end record;
   --  Full declaration of self-initialized private type

   Empty_Pkg       : constant Pkg_Node_Id     := Package_Node_Low_Bound;
   Empty_Package   : constant Package_Node_Id := (Value => Empty_Pkg);
   Unknown_Pkg     : constant Pkg_Node_Id     := Package_Node_High_Bound;
   Unknown_Package : constant Package_Node_Id := (Value => Unknown_Pkg);
   First_Package   : constant Pkg_Node_Id     := Package_Node_Low_Bound + 1;

   First_Package_Node_Id  : constant Package_Node_Id :=
                              (Value => First_Package);

   Package_First : constant Package_Node_Id := First_Package_Node_Id;

   ----------------
   -- Attributes --
   ----------------

   type Attribute_Record is record
      Name           : Name_Id;
      Var_Kind       : Variable_Kind;
      Optional_Index : Boolean;
      Attr_Kind      : Attribute_Kind;
      Read_Only      : Boolean;
      Others_Allowed : Boolean;
      Next           : Attr_Node_Id;
   end record;
   --  Data for an attribute

   package Attrs is
      new Table.Table (Table_Component_Type => Attribute_Record,
                       Table_Index_Type     => Attr_Node_Id,
                       Table_Low_Bound      => First_Attribute,
                       Table_Initial        => Attributes_Initial,
                       Table_Increment      => Attributes_Increment,
                       Table_Name           => "Prj.Attr.Attrs");
   --  The table of the attributes

   --------------
   -- Packages --
   --------------

   type Package_Record is record
      Name             : Name_Id;
      Known            : Boolean := True;
      First_Attribute  : Attr_Node_Id;
   end record;
   --  Data for a package

   package Package_Attributes is
      new Table.Table (Table_Component_Type => Package_Record,
                       Table_Index_Type     => Pkg_Node_Id,
                       Table_Low_Bound      => First_Package,
                       Table_Initial        => Packages_Initial,
                       Table_Increment      => Packages_Increment,
                       Table_Name           => "Prj.Attr.Packages");
   --  The table of the packages

end Prj.Attr;
