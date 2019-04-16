------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G N A T . E X C E P T I O N _ A C T I O N S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2019, Free Software Foundation, Inc.         --
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

--  This package provides support for callbacks on exceptions as well as
--  exception-related utility subprograms of possible interest together with
--  exception actions or more generally.

--  The callbacks are called immediately when either a specific exception,
--  or any exception, is raised, before any other actions taken by raise, in
--  particular before any unwinding of the stack occurs.

--  Callbacks for specific exceptions are registered through calls to
--  Register_Id_Action. Here is an example of code that uses this package to
--  automatically core dump when the exception Constraint_Error is raised.

--      Register_Id_Action (Constraint_Error'Identity, Core_Dump'Access);

--  Subprograms are also provided to list the currently registered exceptions,
--  or to convert from a string to an exception id.

--  This package can easily be extended, for instance to provide a callback
--  whenever an exception matching a regular expression is raised. The idea
--  is to register a global action, called whenever any exception is raised.
--  Dispatching can then be done directly in this global action callback.

with Ada.Exceptions; use Ada.Exceptions;

package GNAT.Exception_Actions is

   type Exception_Action is access
     procedure (Occurrence : Exception_Occurrence);
   --  General callback type whenever an exception is raised. The callback
   --  procedure must not propagate an exception (execution of the program
   --  is erroneous if such an exception is propagated).

   procedure Register_Global_Action (Action : Exception_Action);
   --  Action will be called whenever an exception is raised. Only one such
   --  action can be registered at any given time, and registering a new action
   --  will override any previous action that might have been registered.
   --
   --  Action is called before the exception is propagated to user's code.
   --  If Action is null, this will in effect cancel all exception actions.

   procedure Register_Id_Action
     (Id     : Exception_Id;
      Action : Exception_Action);
   --  Action will be called whenever an exception of type Id is raised. Only
   --  one such action can be registered for each exception id, and registering
   --  a new action will override any previous action registered for this
   --  Exception_Id. Program_Error is raised if Id is Null_Id.

   function Name_To_Id (Name : String) return Exception_Id;
   --  Convert an exception name to an exception id. Null_Id is returned
   --  if no such exception exists. Name must be an all upper-case string,
   --  or the exception will not be found. The exception name must be fully
   --  qualified (but not including Standard). It is not possible to convert
   --  an exception that is declared within an unlabeled block.
   --
   --  Note: All non-predefined exceptions will return Null_Id for programs
   --  compiled with pragma Restriction (No_Exception_Registration)

   function Is_Foreign_Exception (E : Exception_Occurrence) return Boolean;
   --  Tell whether the exception occurrence E represents a foreign exception,
   --  such as one raised in C++ and caught by a when others choice in Ada.

   function Registered_Exceptions_Count return Natural;
   --  Return the number of exceptions that have been registered so far.
   --  Exceptions declared locally will not appear in this list until their
   --  block has been executed at least once.
   --
   --  Note: The count includes only predefined exceptions for programs
   --  compiled with pragma Restrictions (No_Exception_Registration).

   type Exception_Id_Array is array (Natural range <>) of Exception_Id;

   procedure Get_Registered_Exceptions
     (List : out Exception_Id_Array;
      Last : out Integer);
   --  Return the list of registered exceptions.
   --  Last is the index in List of the last exception returned.
   --
   --  An exception is registered the first time the block containing its
   --  declaration is elaborated. Exceptions defined at library-level are
   --  therefore immediately visible, whereas exceptions declared in local
   --  blocks will not be visible until the block is executed at least once.
   --
   --  Note: The list contains only the predefined exceptions if the program
   --  is compiled with pragma Restrictions (No_Exception_Registration);

   procedure Core_Dump (Occurrence : Exception_Occurrence);
   --  Dump memory (called a core dump in some systems) if supported by the
   --  OS (most unix systems), and abort execution of the application. Under
   --  Windows this procedure will not dump the memory, it will only abort
   --  execution.

end GNAT.Exception_Actions;
