------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T A S K _ I N F O                      --
--                                                                          --
--                                 B o d y                                  --
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

--  This package body contains the routines associated with the implementation
--  of the Task_Info pragma.

--  This is the SGI specific version of this module.

with Interfaces.C;
with System.OS_Interface;
with System;
with Unchecked_Conversion;

package body System.Task_Info is

   use System.OS_Interface;
   use type Interfaces.C.int;

   function To_Resource_T is new
     Unchecked_Conversion (Resource_Vector_T, resource_t);

   MP_NPROCS : constant := 1;

   function Sysmp (Cmd : Integer) return Integer;
   pragma Import (C, Sysmp);

   function Num_Processors (Cmd : Integer := MP_NPROCS) return Integer
     renames Sysmp;

   function Geteuid return Integer;
   pragma Import (C, Geteuid);

   Locking_Map : constant array (Page_Locking) of Interfaces.C.int :=
     (NOLOCK   => 0,
      PROCLOCK => 1,
      TXTLOCK  => 2,
      DATLOCK  => 4);

   -------------------------------
   -- Resource_Vector_Functions --
   -------------------------------

   package body Resource_Vector_Functions is

      ---------
      -- "+" --
      ---------

      function "+" (R : Resource_T) return Resource_Vector_T is
         Result  : Resource_Vector_T  := NO_RESOURCES;

      begin
         Result (Resource_T'Pos (R)) := True;
         return Result;
      end "+";

      function "+" (R1, R2 : Resource_T) return Resource_Vector_T is
         Result  : Resource_Vector_T  := NO_RESOURCES;

      begin
         Result (Resource_T'Pos (R1)) := True;
         Result (Resource_T'Pos (R2)) := True;
         return Result;
      end "+";

      function "+"
        (R    : Resource_T;
         S    : Resource_Vector_T)
         return Resource_Vector_T
      is
         Result  : Resource_Vector_T := S;

      begin
         Result (Resource_T'Pos (R)) := True;
         return Result;
      end "+";

      function "+"
        (S    : Resource_Vector_T;
         R    : Resource_T)
         return Resource_Vector_T
      is
         Result  : Resource_Vector_T :=  S;

      begin
         Result (Resource_T'Pos (R)) := True;
         return Result;
      end "+";

      function "+" (S1, S2 : Resource_Vector_T) return Resource_Vector_T is
         Result  : Resource_Vector_T;

      begin
         Result :=  S1 or S2;
         return Result;
      end "+";

      function "-"
        (S    : Resource_Vector_T;
         R    : Resource_T)
         return Resource_Vector_T
      is
         Result  : Resource_Vector_T := S;

      begin
         Result (Resource_T'Pos (R)) := False;
         return Result;
      end "-";

   end Resource_Vector_Functions;

   ---------------
   -- New_Sproc --
   ---------------

   function New_Sproc (Attr : Sproc_Attributes) return sproc_t is
      Sproc_Attr : aliased sproc_attr_t;
      Sproc      : aliased sproc_t;
      Status     : int;

   begin
      Status := sproc_attr_init (Sproc_Attr'Unrestricted_Access);

      if Status = 0 then
         Status := sproc_attr_setresources
           (Sproc_Attr'Unrestricted_Access,
            To_Resource_T (Attr.Sproc_Resources));

         if Attr.CPU /= ANY_CPU then
            if Attr.CPU > Num_Processors then
               raise Invalid_CPU_Number;
            end if;

            Status := sproc_attr_setcpu
              (Sproc_Attr'Unrestricted_Access,
               int (Attr.CPU));
         end if;

         if Attr.Resident /= NOLOCK then
            if Geteuid /= 0 then
               raise Permission_Error;
            end if;

            Status := sproc_attr_setresident
              (Sproc_Attr'Unrestricted_Access,
                Locking_Map (Attr.Resident));
         end if;

         if Attr.NDPRI /= NDP_NONE then
--  ??? why is that comment out, should it be removed ?
--          if Geteuid /= 0 then
--             raise Permission_Error;
--          end if;

            Status := sproc_attr_setprio
              (Sproc_Attr'Unrestricted_Access,
               int (Attr.NDPRI));
         end if;

         Status := sproc_create
           (Sproc'Unrestricted_Access,
            Sproc_Attr'Unrestricted_Access,
            null,
            System.Null_Address);

         if Status /= 0 then
            Status := sproc_attr_destroy (Sproc_Attr'Unrestricted_Access);
            raise Sproc_Create_Error;
         end if;

         Status := sproc_attr_destroy (Sproc_Attr'Unrestricted_Access);

      end if;

      if Status /= 0 then
         raise Sproc_Create_Error;
      end if;

      return Sproc;
   end New_Sproc;

   ---------------
   -- New_Sproc --
   ---------------

   function New_Sproc
     (Sproc_Resources : Resource_Vector_T      := NO_RESOURCES;
      CPU             : CPU_Number             := ANY_CPU;
      Resident        : Page_Locking           := NOLOCK;
      NDPRI           : Non_Degrading_Priority := NDP_NONE)
      return            sproc_t
   is
      Attr : Sproc_Attributes :=
        (Sproc_Resources, CPU, Resident, NDPRI);

   begin
      return New_Sproc (Attr);
   end New_Sproc;

   -------------------------------
   -- Unbound_Thread_Attributes --
   -------------------------------

   function Unbound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T := NO_RESOURCES;
      Thread_Timeslice : Duration          := 0.0)
      return             Thread_Attributes
   is
   begin
      return (False, Thread_Resources, Thread_Timeslice);
   end Unbound_Thread_Attributes;

   -----------------------------
   -- Bound_Thread_Attributes --
   -----------------------------

   function Bound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T := NO_RESOURCES;
      Thread_Timeslice : Duration          := 0.0;
      Sproc            : sproc_t)
      return             Thread_Attributes
   is
   begin
      return (True, Thread_Resources, Thread_Timeslice, Sproc);
   end Bound_Thread_Attributes;

   -----------------------------
   -- Bound_Thread_Attributes --
   -----------------------------

   function Bound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T      := NO_RESOURCES;
      Thread_Timeslice : Duration               := 0.0;
      Sproc_Resources  : Resource_Vector_T      := NO_RESOURCES;
      CPU              : CPU_Number             := ANY_CPU;
      Resident         : Page_Locking           := NOLOCK;
      NDPRI            : Non_Degrading_Priority := NDP_NONE)
      return             Thread_Attributes
   is
      Sproc : sproc_t := New_Sproc
        (Sproc_Resources, CPU, Resident, NDPRI);

   begin
      return (True, Thread_Resources, Thread_Timeslice, Sproc);
   end Bound_Thread_Attributes;

   -----------------------------------
   -- New_Unbound_Thread_Attributes --
   -----------------------------------

   function New_Unbound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T := NO_RESOURCES;
      Thread_Timeslice : Duration          := 0.0)
      return             Task_Info_Type
   is
   begin
      return new Thread_Attributes'
        (False, Thread_Resources, Thread_Timeslice);
   end New_Unbound_Thread_Attributes;

   ---------------------------------
   -- New_Bound_Thread_Attributes --
   ---------------------------------

   function New_Bound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T := NO_RESOURCES;
      Thread_Timeslice : Duration          := 0.0;
      Sproc            : sproc_t)
      return             Task_Info_Type
   is
   begin
      return new Thread_Attributes'
        (True, Thread_Resources, Thread_Timeslice, Sproc);
   end  New_Bound_Thread_Attributes;

   ---------------------------------
   -- New_Bound_Thread_Attributes --
   ---------------------------------

   function New_Bound_Thread_Attributes
     (Thread_Resources : Resource_Vector_T      := NO_RESOURCES;
      Thread_Timeslice : Duration               := 0.0;
      Sproc_Resources  : Resource_Vector_T      := NO_RESOURCES;
      CPU              : CPU_Number             := ANY_CPU;
      Resident         : Page_Locking           := NOLOCK;
      NDPRI            : Non_Degrading_Priority := NDP_NONE)
      return             Task_Info_Type
   is
      Sproc : sproc_t := New_Sproc
        (Sproc_Resources, CPU, Resident, NDPRI);

   begin
      return new Thread_Attributes'
        (True, Thread_Resources, Thread_Timeslice, Sproc);
   end  New_Bound_Thread_Attributes;

end System.Task_Info;
