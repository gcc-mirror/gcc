------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . E X C E P T I O N _ T A B L E              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1996-2003 Free Software Foundation, Inc.          --
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

--  This package implements the interface used to maintain a table of
--  registered exception names, for the implementation of the mapping
--  of names to exceptions (used for exception streams and attributes)

with System.Standard_Library;

package System.Exception_Table is
pragma Elaborate_Body;

   package SSL renames System.Standard_Library;

   procedure Register_Exception (X : SSL.Exception_Data_Ptr);
   pragma Inline (Register_Exception);
   --  Register an exception in the hash table mapping. This function is
   --  called during elaboration of library packages. For exceptions that
   --  are declared within subprograms, the registration occurs the first
   --  time that an exception is elaborated during a call of the subprogram.
   --
   --  Note: all calls to Register_Exception other than those to register the
   --  predefined exceptions are suppressed if the application is compiled
   --  with pragma Restrictions (No_Exception_Registration).

   function Internal_Exception
     (X                   : String;
      Create_If_Not_Exist : Boolean := True) return SSL.Exception_Data_Ptr;
   --  Given an exception_name X, returns a pointer to the actual internal
   --  exception data. A new entry is created in the table if X does not
   --  exist yet and Create_If_Not_Exist is True. If it is false and X
   --  does not exist yet, null is returned.

   function Registered_Exceptions_Count return Natural;
   --  Return the number of currently registered exceptions.

   type Exception_Data_Array is array (Natural range <>)
     of SSL.Exception_Data_Ptr;

   procedure Get_Registered_Exceptions
     (List : out Exception_Data_Array;
      Last : out Integer);
   --  Return the list of registered exceptions.

end System.Exception_Table;
