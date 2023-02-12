------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                S Y S T E M . E X C E P T I O N _ T R A C E S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2023, AdaCore                     --
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

with Ada.Unchecked_Conversion;

with System.Standard_Library; use System.Standard_Library;
with System.Soft_Links;       use System.Soft_Links;

package body System.Exception_Traces is

   --  Calling the decorator directly from where it is needed would require
   --  introducing nasty dependencies upon the spec of this package (typically
   --  in a-except.adb). We also have to deal with the fact that the traceback
   --  array within an exception occurrence and the one the decorator accepts
   --  are of different types. These are two reasons for which a wrapper with
   --  a System.Address argument is indeed used to call the decorator provided
   --  by the user of this package. This wrapper is called via a soft-link,
   --  which either is null when no decorator is in place or "points to" the
   --  following function otherwise.

   function Decorator_Wrapper
     (Traceback : System.Address;
      Len       : Natural) return String;
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
      Len       : Natural) return String
   is
      subtype Trace_Array is Traceback_Entries.Tracebacks_Array (1 .. Len);
      type Trace_Array_Access is access all Trace_Array;

      function To_Trace_Array is new
        Ada.Unchecked_Conversion (Address, Trace_Array_Access);

      Decorator_Traceback : constant Trace_Array_Access :=
                              To_Trace_Array (Traceback);

   begin
      return Current_Decorator.all (Decorator_Traceback.all);
   end Decorator_Wrapper;

   -------------------------
   -- Set_Trace_Decorator --
   -------------------------

   procedure Set_Trace_Decorator (Decorator : Traceback_Decorator) is
   begin
      Current_Decorator := Decorator;
      Traceback_Decorator_Wrapper :=
        (if Current_Decorator /= null
         then Decorator_Wrapper'Access else null);
   end Set_Trace_Decorator;

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

         when Unhandled_Raise_In_Main =>
            Exception_Trace := Unhandled_Raise_In_Main;
      end case;
   end Trace_On;

end System.Exception_Traces;
