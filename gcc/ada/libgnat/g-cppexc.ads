------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  G N A T . C P P _ E X C E P T I O N S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (C) 2013-2020, AdaCore                  --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an interface for raising and handling C++ exceptions

with Ada.Exceptions; use Ada.Exceptions;

package GNAT.CPP_Exceptions is
   generic
      type T is private;
   procedure Raise_Cpp_Exception (Id : Exception_Id; Value : T);
   --  Raise a C++ exception identified by Id. Associate Value with this
   --  occurrence. Id must refer to an exception that has the Cpp convention.

   generic
      type T is private;
   function Get_Object (X : Exception_Occurrence) return T;
   --  Extract the object associated with X. The exception of the occurrence
   --  X must have a Cpp Convention.
end GNAT.CPP_Exceptions;
