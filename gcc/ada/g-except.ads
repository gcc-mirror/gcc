------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      G N A T . E X C E P T I O N S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2000-2005 Ada Core Technologies, Inc.            --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an interface for raising predefined exceptions
--  with an exception message. It can be used from Pure units.

--  There is no prohibition in Ada that prevents exceptions being raised
--  from within pure units. The raise statement is perfectly acceptable.

--  However, it is not normally possible to raise an exception with a
--  message because the routine Ada.Exceptions.Raise_Exception is not in
--  a Pure unit. This is an annoying and unnecessary restrictiona and this
--  package allows for raising the standard predefined exceptions at least.

package GNAT.Exceptions is
   pragma Pure;

   type Exception_Type is limited null record;
   --  Type used to specify which exception to raise.

   --  Really Exception_Type is Exception_Id, but Exception_Id can't be
   --  used directly since it is declared in the non-pure unit Ada.Exceptions,

   --  Exception_Id is in fact simply a pointer to the type Exception_Data
   --  declared in System.Standard_Library (which is also non-pure). So what
   --  we do is to define it here as a by reference type (any by reference
   --  type would do), and then Import the definitions from Standard_Library.
   --  Since this is a by reference type, these will be passed by reference,
   --  which has the same effect as passing a pointer.

   --  This type is not private because keeping it by reference would require
   --  defining it in a way (e.g a tagged type) that would drag other run time
   --  files, which is unwanted in the case of e.g ravenscar where we want to
   --  minimize the number of run time files needed by default.

   CE : constant Exception_Type;  -- Constraint_Error
   PE : constant Exception_Type;  -- Program_Error
   SE : constant Exception_Type;  -- Storage_Error
   TE : constant Exception_Type;  -- Tasking_Error
   --  One of these constants is used in the call to specify the exception

   procedure Raise_Exception (E : Exception_Type; Message : String);
   pragma Import (Ada, Raise_Exception, "__gnat_raise_exception");
   pragma No_Return (Raise_Exception);
   --  Raise specified exception with specified message

private
   pragma Import (C, CE, "constraint_error");
   pragma Import (C, PE, "program_error");
   pragma Import (C, SE, "storage_error");
   pragma Import (C, TE, "tasking_error");
   --  References to the exception structures in the standard library

end GNAT.Exceptions;
