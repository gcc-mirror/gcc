------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        M U T A B L Y _ T A G G E D                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2024-2025, Free Software Foundation, Inc.         --
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

--  Semantic and expansion utility routines dealing with mutably tagged types

with Types; use Types;

package Mutably_Tagged is

   --------------------------------------------
   -- Implementation of Mutably Tagged Types --
   --------------------------------------------

   --  This package implements mutably tagged types via the Size'class aspect
   --  which enables the creation of class-wide types with a specific maximum
   --  size. This allows such types to be used directly in record components,
   --  in object declarations without an initial expression, and to be assigned
   --  a value from any type in a mutably tagged type's hierarchy.

   --  For example, this structure allows Base_Type and its derivatives to be
   --  treated as components with a predictable size:

   --    type Base_Type is tagged null record
   --      with Size'Class => 128;

   --    type Container is record
   --      Component : Base_Type'Class;
   --    end record;

   --  The core of their implementation involves creating an "equivalent" type
   --  for each class-wide type that adheres to the Size'Class constraint. This
   --  is achieved by using the function Make_CW_Equivalent_Type from Exp_Util,
   --  which generates a type that is compatible in size and structure with any
   --  derived type of the base class-wide type.

   --  Once the class-wide equivalent type is generated, all declarations of
   --  mutably tagged typed objects get rewritten as declarations of objects
   --  with the equivalent type. References to these objects also then get
   --  wrapped in unchecked conversions to the mutably tagged class-wide type.

   function Corresponding_Mutably_Tagged_Type
     (CW_Equiv_Typ : Entity_Id) return Entity_Id;
   --  Given a class-wide equivalent type obtain the related mutably tagged
   --  class-wide type.

   function Depends_On_Mutably_Tagged_Ext_Comp (N : Node_Id) return Boolean;
   --  Return true if the given node N contains a reference to a component
   --  of a mutably tagged object which comes from a type extension.

   function Get_Corresponding_Mutably_Tagged_Type_If_Present
     (Typ : Entity_Id) return Entity_Id;
   --  Obtain the corresponding mutably tagged type associated with Typ when
   --  Typ is a mutably tagged class-wide equivalent type. Otherwise, just
   --  return Typ.

   function Get_Corresponding_Tagged_Type_If_Present
     (Typ : Entity_Id) return Entity_Id;
   --  Obtain the corresponding tag type associated with Typ when
   --  Typ is a mutably tagged class-wide equivalent type. Otherwise, Just
   --  return Typ.

   --  This function is mostly used when we need a concrete type to generate
   --  initialization for mutably tagged types.

   function Is_Mutably_Tagged_Conversion (N : Node_Id) return Boolean;
   --  Return True if expression N is an object of a mutably tagged class-wide
   --  equivalent type which has been expanded into a type conversion to
   --  its related mutably tagged class-wide type.

   function Is_Mutably_Tagged_CW_Equivalent_Type
     (Typ : Entity_Id) return Boolean;
   --  Determine if Typ is a class-wide equivalent type

   procedure Make_Mutably_Tagged_Conversion
     (N     : Node_Id;
      Typ   : Entity_Id := Empty;
      Force : Boolean   := False);
   --  Expand a reference N to a given mutably tagged type Typ. When Typ is not
   --  present the closest associated mutably tagged type in the hierarchy is
   --  used.

   --  Force is used to ignore certain predicates which avoid generating the
   --  conversion (e.g. when N is on the left-hand side of an assignment).

   function Make_CW_Size_Compile_Check
     (New_Typ     : Entity_Id;
      Mut_Tag_Typ : Entity_Id) return Node_Id;
   --  Generate a type size check on New_Typ based on the size set in
   --  the mutably tagged type Mut_Tag_Typ.

   function Make_Mutably_Tagged_CW_Check
     (N   : Node_Id;
      Tag : Node_Id) return Node_Id;
   --  Generate class-wide membership test for a given expression N based on
   --  Tag.

end Mutably_Tagged;
