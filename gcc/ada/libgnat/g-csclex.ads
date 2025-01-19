------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         G N A T . C P P . S T D . C L A S S _ E X C E P T I O N          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2022-2025, AdaCore                     --
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

--  This package provides an interface to C++'s std::exception objects

with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
--  with GNAT.CPP_Exceptions; use GNAT.CPP_Exceptions;

--  ??? Should we expose, or even keep this unit?  We can't run
--  constructors (they're inline), and destructors have to be declared
--  before the public methods because of their position in the virtual
--  method table, so they have to be public as well. It could be
--  unsafe to run them without running the constructors we can't get
--  at, so deriving from this type would be a challenge, though it
--  could be potentially useful for obscure cross-language exception
--  handling scenarios. Adding this unit to libgnat would require
--  libgnat.so to depend on libsupc++, if the exception declarations
--  were uncommented out.
private package GNAT.CPP.Std.Class_Exception is
   type Class_Exception is abstract tagged limited record
      null;
   end record;
   pragma Import (CPP, Class_Exception);

   --  Defined inline, no out-of-line version available.
   --  function New_Class_Exception return Class_Exception;
   --  pragma Cpp_Constructor (New_Class_Exception);
   --  pragma Machine_Attribute (New_Class_Exception, "nothrow");
   pragma Warnings (Off, "CPP constructor required for type");

   --  Allowing destruction is undesirable, but since these are in
   --  the virtual method table, we have to declare them.
   --  ??? Could we make them private somehow?
   procedure Destructor (Ex : access Class_Exception);
   pragma Import (CPP, Destructor, "_ZNSt9exceptionD1Ev");
   pragma Machine_Attribute (Destructor, "nothrow");
   procedure Delete_Destructor (Ex : access Class_Exception);
   pragma Import (CPP, Delete_Destructor, "_ZNSt9exceptionD0Ev");
   pragma Machine_Attribute (Delete_Destructor, "nothrow");

   function What (Ex : access constant Class_Exception) return chars_ptr;
   pragma Import (CPP, What, "_ZNKSt9exception4whatEv");
   pragma Machine_Attribute (What, "nothrow");
   --  Obtain the C string stored in the std::exception object.

   function What (Ex : access constant Class_Exception'Class) return String is
      (Value (What (Ex)));
   --  Obtain the C string stored in the std::exception object,
   --  converted to an Ada String.

   --  function Get_Class_Exception_From_Occurrence is
   --     new Get_Access_To_Tagged_Object (Class_Exception);
   --  Obtain access to Class_Exception'Class of an
   --  Exception_Occurrence handled as Class_Exception_Only or
   --  Class_Exception_Or_Derived, as defined in comments below.
   --  Adding this to libgnat would cause libgnat.so to depend on
   --  libsupc++.

   --  Class_Exception_Only : exception;
   --  pragma Import (Cpp, Class_Exception_Only,
   --                 "_ZTISt9exception");
   --  Exception to catch std::exception exact matches only.

   --  Class_Exception_Or_Derived : exception;
   --  pragma Import (Cpp, Class_Exception_Or_Derived,
   --                 "_ZTISt9exception'Class");
   --  Exception to catch std::exception or derived types.

end GNAT.CPP.Std.Class_Exception;
