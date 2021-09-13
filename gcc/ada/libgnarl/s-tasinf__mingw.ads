------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T A S K _ I N F O                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2021, Free Software Foundation, Inc.          --
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

--  This package contains the definitions and routines associated with the
--  implementation and use of the Task_Info pragma. It is specialized
--  appropriately for targets that make use of this pragma.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

--  The functionality in this unit is now provided by the predefined package
--  System.Multiprocessors and the CPU aspect. This package is obsolescent.

--  This is the Windows (native) version of this module

with System.Win32;

package System.Task_Info is
   pragma Obsolescent (Task_Info, "use System.Multiprocessors and CPU aspect");
   pragma Preelaborate;
   pragma Elaborate_Body;
   --  To ensure that a body is allowed

   use type System.Win32.ProcessorId;

   --  Windows provides a way to define the ideal processor to use for a given
   --  thread. The ideal processor is not necessarily the one that will be used
   --  by the OS but the OS will always try to schedule this thread to the
   --  specified processor if it is available.

   --  The Task_Info pragma:

   --    pragma Task_Info (EXPRESSION);

   --  allows the specification on a task by task basis of a value of type
   --  System.Task_Info.Task_Info_Type to be passed to a task when it is
   --  created. The specification of this type, and the effect on the task
   --  that is created is target dependent.

   --  The Task_Info pragma appears within a task definition (compare the
   --  definition and implementation of pragma Priority). If no such pragma
   --  appears, then the value Unspecified_Task_Info is passed. If a pragma
   --  is present, then it supplies an alternative value. If the argument of
   --  the pragma is a discriminant reference, then the value can be set on
   --  a task by task basis by supplying the appropriate discriminant value.

   --  Note that this means that the type used for Task_Info_Type must be
   --  suitable for use as a discriminant (i.e. a scalar or access type).

   -----------------------
   -- Thread Attributes --
   -----------------------

   subtype CPU_Number is System.Win32.ProcessorId;

   Any_CPU : constant CPU_Number := -1;

   Invalid_CPU_Number : exception;
   --  Raised when an invalid CPU number has been specified
   --  i.e. CPU > Number_Of_Processors.

   type Thread_Attributes is record
      CPU : CPU_Number := Any_CPU;
   end record;

   Default_Thread_Attributes : constant Thread_Attributes := (others => <>);

   type Task_Info_Type is access all Thread_Attributes;

   Unspecified_Task_Info : constant Task_Info_Type := null;

   function Number_Of_Processors return Positive;
   --  Returns the number of processors on the running host

end System.Task_Info;
