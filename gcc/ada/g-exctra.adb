------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  G N A T . E X C E P T I O N _ T R A C E S               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2000-2002 Ada Core Technologies, Inc.           --
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

with System.Standard_Library; use System.Standard_Library;
with System.Soft_Links;       use System.Soft_Links;

package body GNAT.Exception_Traces is

   --  Calling the decorator directly from where it is needed would require
   --  introducing nasty dependencies upon the spec of this package (typically
   --  in a-except.adb). We also have to deal with the fact that the traceback
   --  array within an exception occurrence and the one the decorator shall
   --  accept are of different types. These are two reasons for which a wrapper
   --  with a System.Address argument is indeed used to call the decorator
   --  provided by the user of this package. This wrapper is called via a
   --  soft-link, which either is null when no decorator is in place or "points
   --  to" the following function otherwise.

   function Decorator_Wrapper
     (Traceback : System.Address;
      Len       : Natural)
      return      String;
   --  The wrapper to be called when a decorator is in place for exception
   --  backtraces.
   --
   --  Traceback is the address of the call chain array as stored in the
   --  exception occurrence and Len is the number of significant addresses
   --  contained in this array.

   Current_Decorator : Traceback_Decorator := null;
   --  The decorator to be called by the wrapper when it is not null, as set
   --  by Set_Trace_Decorator. When this access is null, the wrapper is null
   --  also and shall then not be called.

   -----------------------
   -- Decorator_Wrapper --
   -----------------------

   function Decorator_Wrapper
     (Traceback : System.Address;
      Len       : Natural)
      return      String
   is
      Decorator_Traceback : Tracebacks_Array (1 .. Len);
      for Decorator_Traceback'Address use Traceback;

      --  Handle the "transition" from the array stored in the exception
      --  occurrence to the array expected by the decorator.

      pragma Import (Ada, Decorator_Traceback);

   begin
      return Current_Decorator.all (Decorator_Traceback);
   end Decorator_Wrapper;

   -------------------------
   -- Set_Trace_Decorator --
   -------------------------

   procedure Set_Trace_Decorator (Decorator : Traceback_Decorator) is
   begin
      Current_Decorator := Decorator;

      if Current_Decorator /= null then
         Traceback_Decorator_Wrapper := Decorator_Wrapper'Access;
      else
         Traceback_Decorator_Wrapper := null;
      end if;
   end Set_Trace_Decorator;

   --  Trace_On/Trace_Off control the kind of automatic output to occur
   --  by way of the global Exception_Trace variable.

   ---------------
   -- Trace_Off --
   ---------------

   procedure Trace_Off is
   begin
      Exception_Trace := RM_Convention;
   end Trace_Off;

   --------------
   -- Trace_On --
   --------------

   procedure Trace_On (Kind : Trace_Kind) is
   begin
      case Kind is
         when Every_Raise =>
            Exception_Trace := Every_Raise;
         when Unhandled_Raise =>
            Exception_Trace := Unhandled_Raise;
      end case;
   end Trace_On;

end GNAT.Exception_Traces;
