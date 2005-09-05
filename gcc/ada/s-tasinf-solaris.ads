------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T A S K _ I N F O                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

--  This package contains the definitions and routines associated with the
--  implementation and use of the Task_Info pragma. It is specialized
--  appropriately for targets that make use of this pragma.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

--  This is the Solaris (native) version of this module.

with System.OS_Interface;

package System.Task_Info is
   pragma Preelaborate;
   pragma Elaborate_Body;
   --  To ensure that a body is allowed

   -----------------------------------------------------
   -- Binding of Tasks to LWPs and LWPs to processors --
   -----------------------------------------------------

   --  The Solaris implementation of the GNU Low-Level Interface (GNULLI)
   --  implements each Ada task as a Solaris thread.  The Solaris thread
   --  library distributes threads across one or more LWPs (Light Weight
   --  Process) that are members of the same process. Solaris distributes
   --  processes and LWPs across the available CPUs on a given machine. The
   --  pragma Task_Info provides the mechanism to control the distribution
   --  of tasks to LWPs, and LWPs to processors.

   --  Each thread has a number of attributes that dictate it's scheduling.
   --  These attributes are:
   --
   --      New_LWP:       whether a new LWP is created for this thread.
   --
   --      Bound_To_LWP:  whether the thread is bound to a specific LWP
   --                     for its entire lifetime.
   --
   --      CPU:           the CPU number associated to the LWP
   --

   --  The Task_Info pragma:

   --    pragma Task_Info (EXPRESSION);

   --  allows the specification on a task by task basis of a value of type
   --  System.Task_Info.Task_Info_Type to be passed to a task when it is
   --  created. The specification of this type, and the effect on the task
   --  that is created is target dependent.

   --  The Task_Info pragma appears within a task definition (compare the
   --  definition and implementation of pragma Priority). If no such pragma
   --  appears, then the value Task_Info_Unspecified is passed. If a pragma
   --  is present, then it supplies an alternative value. If the argument of
   --  the pragma is a discriminant reference, then the value can be set on
   --  a task by task basis by supplying the appropriate discriminant value.

   --  Note that this means that the type used for Task_Info_Type must be
   --  suitable for use as a discriminant (i.e. a scalar or access type).

   -----------------------
   -- Thread Attributes --
   -----------------------

   subtype CPU_Number is System.OS_Interface.processorid_t;

   CPU_UNCHANGED : constant CPU_Number := System.OS_Interface.PBIND_QUERY;
   --  Do not bind the LWP to a specific processor

   ANY_CPU       : constant CPU_Number := System.OS_Interface.PBIND_NONE;
   --  Bind the LWP to any processor

   Invalid_CPU_Number : exception;

   type Thread_Attributes (New_LWP : Boolean) is record
      Bound_To_LWP     : Boolean    := True;
      case New_LWP is
         when False =>
            null;
         when True =>
            CPU        : CPU_Number := CPU_UNCHANGED;
      end case;
   end record;

   Default_Thread_Attributes : constant Thread_Attributes := (False, True);

   function Unbound_Thread_Attributes
      return Thread_Attributes;

   function Bound_Thread_Attributes
      return Thread_Attributes;

   function Bound_Thread_Attributes (CPU : CPU_Number)
      return Thread_Attributes;

   type Task_Info_Type is access all Thread_Attributes;

   function New_Unbound_Thread_Attributes
      return Task_Info_Type;

   function New_Bound_Thread_Attributes
      return Task_Info_Type;

   function New_Bound_Thread_Attributes (CPU : CPU_Number)
      return Task_Info_Type;

   Unspecified_Task_Info : constant Task_Info_Type := null;

end System.Task_Info;
