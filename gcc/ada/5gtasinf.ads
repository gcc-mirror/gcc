------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T A S K _ I N F O                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  This package contains the definitions and routines associated with the
--  implementation of the Task_Info pragma.

--  This is the SGI (libathread) specific version of this module.

with System.OS_Interface;
with Unchecked_Deallocation;

package System.Task_Info is
pragma Elaborate_Body;
--  To ensure that a body is allowed

   ---------------------------------------------------------
   -- Binding of Tasks to sprocs and sprocs to processors --
   ---------------------------------------------------------

   --  The SGI implementation of the GNU Low-Level Interface (GNULLI)
   --  implements each Ada task as a Posix thread (Pthread). The SGI
   --  Pthread library distributes threads across one or more processes
   --  that are members of a common share group. Irix distributes
   --  processes across the available CPUs on a given machine. The
   --  pragma Task_Info provides the mechanism to control the distribution
   --  of tasks to sprocs, and sprocs to processors.

   --  Each thread has a number of attributes that dictate it's scheduling.
   --  These attributes are:
   --
   --      Bound_To_Sproc:  whether the thread is bound to a specific sproc
   --                       for its entire lifetime.
   --
   --      Timeslice:       Amount of time that a thread is allowed to execute
   --                       before the system yeilds control to another thread
   --                       of equal priority.
   --
   --      Resource_Vector: A bitmask used to control the binding of threads
   --                       to sprocs.
   --

   --  Each share group process (sproc)

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

   ----------------------
   -- Resource Vectors --
   ----------------------

   --  <discussion>

   type Resource_Vector_T is array (0 .. 31) of Boolean;
   pragma Pack (Resource_Vector_T);

   NO_RESOURCES : constant Resource_Vector_T := (others => False);

   generic
      type Resource_T is (<>);
      --  Discrete type up to 32 entries

   package Resource_Vector_Functions is
      function "+"
        (R    : Resource_T)
         return Resource_Vector_T;

      function "+"
        (R1   : Resource_T;
         R2   : Resource_T)
         return Resource_Vector_T;

      function "+"
        (R    : Resource_T;
         S    : Resource_Vector_T)
         return Resource_Vector_T;

      function "+"
        (S    : Resource_Vector_T;
         R    : Resource_T)
         return Resource_Vector_T;

      function "+"
        (S1   : Resource_Vector_T;
         S2   : Resource_Vector_T)
         return Resource_Vector_T;

      function "-"
        (S    : Resource_Vector_T;
         R    : Resource_T)
         return Resource_Vector_T;
   end Resource_Vector_Functions;

   ----------------------
   -- Sproc Attributes --
   ----------------------

   subtype sproc_t is System.OS_Interface.sproc_t;

   subtype CPU_Number is Integer range -1 .. Integer'Last;

   ANY_CPU : constant CPU_Number := CPU_Number'First;

   type Non_Degrading_Priority is range 0 .. 255;
   --  Specification of IRIX Non Degrading Priorities.
   --
   --  WARNING: IRIX priorities have the reverse meaning of Ada priorities.
   --           The lower the priority value, the greater the greater the
   --           scheduling preference.
   --
   --  See the schedctl(2) man page for a complete discussion of non-degrading
   --  priorities.

   NDPHIMAX : constant Non_Degrading_Priority := 30;
   NDPHIMIN : constant Non_Degrading_Priority := 39;
   --  These priorities are higher than ALL normal user process priorities

   subtype NDP_High is Non_Degrading_Priority range NDPHIMAX .. NDPHIMIN;

   NDPNORMMAX : constant Non_Degrading_Priority := 40;
   NDPNORMMIN : constant Non_Degrading_Priority := 127;
   --  These priorities overlap normal user process priorities

   subtype NDP_Norm is Non_Degrading_Priority range NDPNORMMAX .. NDPNORMMIN;

   NDPLOMAX : constant Non_Degrading_Priority := 128;
   NDPLOMIN : constant Non_Degrading_Priority := 254;
   --  These priorities are below ALL normal user process priorities

   NDP_NONE   : constant Non_Degrading_Priority := 255;

   subtype NDP_LOW is Non_Degrading_Priority range NDPLOMAX .. NDPLOMIN;

   type Page_Locking is
      (NOLOCK,     --  Do not lock pages in memory
       PROCLOCK,   --  Lock text and data segments into memory (process lock)
       TXTLOCK,    --  Lock text segment into memory (text lock)
       DATLOCK     --  Lock data segment into memory (data lock)
      );

   type Sproc_Attributes is record
      Sproc_Resources : Resource_Vector_T      := NO_RESOURCES;
      CPU             : CPU_Number             := ANY_CPU;
      Resident        : Page_Locking           := NOLOCK;
      NDPRI           : Non_Degrading_Priority := NDP_NONE;
--  ??? why is that commented out, should it be removed ?
--       Sproc_Slice     : Duration               := 0.0;
--       Deadline_Period : Duration               := 0.0;
--       Deadline_Alloc  : Duration               := 0.0;
   end record;

   Default_Sproc_Attributes : constant Sproc_Attributes :=
      (NO_RESOURCES, ANY_CPU, NOLOCK, NDP_NONE);

   function New_Sproc (Attr : Sproc_Attributes) return sproc_t;
   function New_Sproc
     (Sproc_Resources : Resource_Vector_T      := NO_RESOURCES;
      CPU             : CPU_Number             := ANY_CPU;
      Resident        : Page_Locking           := NOLOCK;
      NDPRI           : Non_Degrading_Priority := NDP_NONE)
      return            sproc_t;
   --  Allocates a sproc_t control structure and creates the
   --  corresponding sproc.

   Invalid_CPU_Number : exception;
   Permission_Error   : exception;
   Sproc_Create_Error : exception;

   -----------------------
   -- Thread Attributes --
   -----------------------

   type Thread_Attributes (Bound_To_Sproc : Boolean) is record
      Thread_Resources : Resource_Vector_T := NO_RESOURCES;

      Thread_Timeslice : Duration          := 0.0;

      case Bound_To_Sproc is
         when False =>
            null;
         when True   =>
            Sproc : sproc_t;
      end case;
   end record;

   Default_Thread_Attributes : constant Thread_Attributes :=
     (False, NO_RESOURCES, 0.0);

   function Unbound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T := NO_RESOURCES;
      Thread_Timeslice : Duration          := 0.0)
      return             Thread_Attributes;

   function Bound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T := NO_RESOURCES;
      Thread_Timeslice : Duration          := 0.0;
      Sproc            : sproc_t)
      return             Thread_Attributes;

   function Bound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T      := NO_RESOURCES;
      Thread_Timeslice : Duration               := 0.0;
      Sproc_Resources  : Resource_Vector_T      := NO_RESOURCES;
      CPU              : CPU_Number             := ANY_CPU;
      Resident         : Page_Locking           := NOLOCK;
      NDPRI            : Non_Degrading_Priority := NDP_NONE)
      return             Thread_Attributes;

   type Task_Info_Type is access all Thread_Attributes;

   function New_Unbound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T := NO_RESOURCES;
      Thread_Timeslice : Duration          := 0.0)
      return             Task_Info_Type;

   function New_Bound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T := NO_RESOURCES;
      Thread_Timeslice : Duration          := 0.0;
      Sproc            : sproc_t)
      return             Task_Info_Type;

   function New_Bound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T      := NO_RESOURCES;
      Thread_Timeslice : Duration               := 0.0;
      Sproc_Resources  : Resource_Vector_T      := NO_RESOURCES;
      CPU              : CPU_Number             := ANY_CPU;
      Resident         : Page_Locking           := NOLOCK;
      NDPRI            : Non_Degrading_Priority := NDP_NONE)
      return             Task_Info_Type;

   type Task_Image_Type is access String;
   --  Used to generate a meaningful identifier for tasks that are variables
   --  and components of variables.

   procedure Free_Task_Image is new
     Unchecked_Deallocation (String, Task_Image_Type);

   Unspecified_Task_Info : constant Task_Info_Type := null;

end System.Task_Info;
