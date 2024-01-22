------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               G N A T . C U R R E N T _ E X C E P T I O N                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                      Copyright (C) 1996-2024, AdaCore                    --
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

--  This package provides routines for obtaining the current exception
--  information in Ada 83 style. In Ada 83, there was no official method
--  for obtaining exception information, but a number of vendors supplied
--  routines for this purpose, and this package closely approximates the
--  interfaces supplied by DEC Ada 83 and VADS Ada.

--  The routines in this package are associated with a particular exception
--  handler, and can only be called from within an exception handler. See
--  also the package GNAT.Most_Recent_Exception, which provides access to
--  the most recently raised exception, and is not limited to static calls
--  from an exception handler.

package GNAT.Current_Exception is
   pragma Pure;

   -----------------
   -- Subprograms --
   -----------------

   --  Note: the lower bound of returned String values is always one

   function Exception_Information return String;
   --  Returns the result of calling Ada.Exceptions.Exception_Information
   --  with an argument that is the Exception_Occurrence corresponding to
   --  the current exception. Returns the null string if called from outside
   --  an exception handler.

   function Exception_Message return String;
   --  Returns the result of calling Ada.Exceptions.Exception_Message with
   --  an argument that is the Exception_Occurrence corresponding to the
   --  current exception. Returns the null string if called from outside an
   --  exception handler.

   function Exception_Name return String;
   --  Returns the result of calling Ada.Exceptions.Exception_Name with
   --  an argument that is the Exception_Occurrence corresponding to the
   --  current exception. Returns the null string if called from outside
   --  an exception handler.

   --  Note: all these functions return useful information only if
   --  called statically from within an exception handler, and they
   --  return information about the exception corresponding to the
   --  handler in which they appear. This is NOT the same as the most
   --  recently raised exception. Consider the example:

   --     exception
   --        when Constraint_Error =>
   --          begin
   --             ...
   --          exception
   --             when Tasking_Error => ...
   --          end;
   --
   --          -- Exception_xxx at this point returns the information about
   --          -- the constraint error, not about any exception raised within
   --          -- the nested block since it is the static nesting that counts.

   -----------------------------------
   -- Use of Library Level Renaming --
   -----------------------------------

   --  For greater compatibility with existing legacy software, library
   --  level renaming may be used to create a function with a name matching
   --  one that is in use. For example, some versions of VADS Ada provided
   --  a function called Current_Exception whose semantics was identical to
   --  that of GNAT. The following library level renaming declaration:

   --    with GNAT.Current_Exception;
   --    function Current_Exception
   --      renames GNAT.Current_Exception.Exception_Name;

   --  placed in a file called current_exception.ads and compiled into the
   --  application compilation environment, will make the function available
   --  in a manner exactly compatible with that in VADS Ada 83.

private
   pragma Import (Intrinsic, Exception_Information);
   pragma Import (intrinsic, Exception_Message);
   pragma Import (Intrinsic, Exception_Name);

end GNAT.Current_Exception;
