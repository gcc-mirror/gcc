------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I M P U N I T                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2010, Free Software Foundation, Inc.         --
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

--  This package contains data and functions used to determine if a given
--  unit is an internal unit intended only for use by the implementation
--  and which should not be directly WITH'ed by user code. It also checks
--  for Ada 05 units that should only be WITH'ed in Ada 05 mode.

with Types; use Types;

package Impunit is

   type Kind_Of_Unit is
     (Implementation_Unit,
      --  Unit from predefined library intended to be used only by the
      --  compiler generated code, or from the implementation of the run time.
      --  Use of such a unit generates a warning unless the client is compiled
      --  with the -gnatg switch. If we are being super strict, this should be
      --  an error for the case of Ada units, but that seems over strenuous.

      Not_Predefined_Unit,
      --  This is not a predefined unit, so no checks are needed

      Ada_95_Unit,
      --  This unit is defined in the Ada 95 RM, and can be freely with'ed
      --  in both Ada 95 mode and Ada 05 mode. Note that in Ada 83 mode, no
      --  child units are allowed, so you can't even name such a unit.

      Ada_2005_Unit,
      --  This unit is defined in the Ada 2005 RM. Withing this unit from a
      --  Ada 95 mode program will generate a warning (again, strictly speaking
      --  this should be an error, but that seems over-strenuous).

      Ada_2012_Unit);
      --  This unit is defined in the Ada 2012 RM. Withing this unit from a Ada
      --  95 mode or Ada 2005 program will generate a warning (again, strictly
      --  speaking this should be an error, but that seems over-strenuous).

   function Get_Kind_Of_Unit (U : Unit_Number_Type) return Kind_Of_Unit;
   --  Given the unit number of a unit, this function determines the type
   --  of the unit, as defined above. If the result is Implementation_Unit,
   --  then the name of a possible atlernative equivalent unit is placed in
   --  Error_Msg_String/Slen on return. If there is no alternative name, or
   --  if the result is not Implementation_Unit, then Error_Msg_Slen is zero
   --  on return, indicating that no alternative name was found.

   function Is_Known_Unit (Nam : Node_Id) return Boolean;
   --  Nam is the possible name of a child unit, represented as a selected
   --  component node. This function determines whether the name matches
   --  one of the known library units, and if so, returns True. If the name
   --  does not match any known library unit, False is returned.

end Impunit;
