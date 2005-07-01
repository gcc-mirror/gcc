------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I M P U N I T                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2000-2005 Free Software Foundation, Inc.          --
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

      Ada_05_Unit);
   --  This unit is defined in the Ada 05 RM. Withing this unit from a
   --  Ada 95 mode program will generate a warning (again, strictly speaking
   --  this should be an error, but that seems over-strenuous).

   function Get_Kind_Of_Unit (U : Unit_Number_Type) return Kind_Of_Unit;
   --  Given the unit number of a unit, this function determines the type
   --  of the unit, as defined above.

end Impunit;
