------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--            G N A T . M O S T _ R E C E N T _ E X C E P T I O N           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

--  This package provides routines for accessing the most recently raised
--  exception. This may be useful for certain logging activities. It may
--  also be useful for mimicking implementation dependent capabilities in
--  Ada 83 compilers, but see also GNAT.Current_Exceptions for this usage.

with Ada.Exceptions;
package GNAT.Most_Recent_Exception is

   -----------------
   -- Subprograms --
   -----------------

   function Occurrence
     return Ada.Exceptions.Exception_Occurrence;
   --  Returns the Exception_Occurrence for the most recently raised exception
   --  in the current task. If no exception has been raised in the current task
   --  prior to the call, returns Null_Occurrence.

   function Occurrence_Access
     return Ada.Exceptions.Exception_Occurrence_Access;
   --  Similar to the above, but returns an access to the occurrence value.
   --  This value is in a task specific location, and may be validly accessed
   --  as long as no further exception is raised in the calling task.

   --  Note: unlike the routines in GNAT.Current_Exception, these functions
   --  access the most recently raised exception, regardless of where they
   --  are called. Consider the following example:

   --     exception
   --        when Constraint_Error =>
   --          begin
   --             ...
   --          exception
   --             when Tasking_Error => ...
   --          end;
   --
   --          --  Assuming a Tasking_Error was raised in the inner block,
   --          --  a call to GNAT.Most_Recent_Exception.Occurrence will
   --          --  return information about this Tasking_Error exception,
   --          --  not about the Constraint_Error exception being handled
   --          --  by the current handler code.

end GNAT.Most_Recent_Exception;
