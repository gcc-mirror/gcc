------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I M P U N I T                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2019, Free Software Foundation, Inc.         --
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

--  This package contains data and functions used to determine if a given unit
--  is an internal unit intended only for use by the implementation and which
--  should not be directly WITH'ed by user code. It also checks for Ada 2005
--  units that should only be WITH'ed in Ada 2005 mode, and Ada 2012 units
--  that should only be WITH'ed in Ada 2012 mode.

with Types; use Types;

package Impunit is

   type Kind_Of_Unit is
     (Implementation_Unit,
      --  Unit from predefined library intended to be used only by the compiler
      --  generated code, or from the implementation of the run time. Use of
      --  such a unit generates a warning unless the client is compiled with
      --  the -gnatg switch. If we are being super strict, this should be an
      --  error for the case of Ada units, but that seems over strenuous.

      Not_Predefined_Unit,
      --  This is not a predefined unit, so no checks are needed

      Ada_95_Unit,
      Ada_2005_Unit,
      Ada_2012_Unit,
      Ada_202X_Unit);
      --  This unit is defined in the Ada RM of the given year. This is used to
      --  give a warning when withing a unit from a wrong mode (e.g. withing an
      --  Ada_2012_Unit when compiling with -gnat95). Note that in Ada 83 mode,
      --  no child units are allowed, so you can't even name such a unit.

   function Get_Kind_Of_Unit (U : Unit_Number_Type) return Kind_Of_Unit;
   --  Given the unit number of a unit, this function determines the type
   --  of the unit, as defined above. If the result is Implementation_Unit,
   --  then the name of a possible alternative equivalent unit is placed in
   --  Error_Msg_String/Slen on return. If there is no alternative name, or if
   --  the result is not Implementation_Unit, then Error_Msg_Slen is zero on
   --  return, indicating that no alternative name was found.

   function Get_Kind_Of_File (File : String) return Kind_Of_Unit;
   --  Same as Get_Kind_Of_Unit, for a given filename

   function Is_Known_Unit (Nam : Node_Id) return Boolean;
   --  Nam is the possible name of a child unit, represented as a selected
   --  component node. This function determines whether the name matches one of
   --  the known library units, and if so, returns True. If the name does not
   --  match any known library unit, False is returned.

   function Not_Impl_Defined_Unit (U : Unit_Number_Type) return Boolean;
   --  This function returns True if U represents a unit that is permitted by
   --  the restriction No_Implementation_Units (i.e. a unit in the Ada, System,
   --  and Interfaces hierarchies that is defined in the RM, or a user defined
   --  unit. It returns False if U represents a unit that is not permitted by
   --  this restriction, which includes units in these three hierarchies that
   --  are GNAT implementation defined. It also returns False for any units in
   --  the GNAT hierarchy, which is not strictly conforming, but so obviously
   --  useful that it is a reasonable deviation from the standard.

end Impunit;
