------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--            Copyright (C) 1991-2017, Florida State University             --
--          Copyright (C) 1995-2023, Free Software Foundation, Inc.         --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
-- The GNARL files that were developed for RTEMS are maintained by  On-Line --
-- Applications Research Corporation (http://www.oarcorp.com)  in  coopera- --
-- tion with Ada Core Technologies Inc. and Florida State University.       --
--                                                                          --
------------------------------------------------------------------------------

--  This is the RTEMS version of this package

--  This package encapsulates all direct interfaces to OS services
--  that are needed by children of System.

with Interfaces.C; use Interfaces.C;

package body System.OS_Interface is

   ---------------
   -- RTEMS API --
   ---------------

   type RTEMS_Attributes is new unsigned;

   RTEMS_SIMPLE_BINARY_SEMAPHORE : constant := 16#00000020#;
   RTEMS_FIFO                    : constant := 16#00000000#;

   type RTEMS_Interval is new unsigned;

   RTEMS_NO_TIMEOUT : constant := 0;

   type RTEMS_Options is new unsigned;

   RTEMS_WAIT             : constant := 16#00000000#;
   RTEMS_INTERRUPT_UNIQUE : constant := 16#00000001#;

   type RTEMS_Name is new unsigned;

   function RTEMS_Build_Name (C1, C2, C3, C4 : Character) return RTEMS_Name
     with Import, External_Name => "rtems_build_name", Convention => C;

   function RTEMS_Semaphore_Create
     (Name             : RTEMS_Name;
      Count            : unsigned;
      Attributes       : RTEMS_Attributes;
      Priority_Ceiling : unsigned;
      Semaphore        : out Binary_Semaphore_Id) return int
     with Import, External_Name => "rtems_semaphore_create", Convention => C;

   function RTEMS_Semaphore_Delete (Semaphore : Binary_Semaphore_Id) return int
     with Import, External_Name => "rtems_semaphore_delete", Convention => C;

   function RTEMS_Semaphore_Flush (Semaphore : Binary_Semaphore_Id)
     return int
     with Import, External_Name => "rtems_semaphore_flush", Convention => C;

   function RTEMS_Semaphore_Obtain
     (Semaphore : Binary_Semaphore_Id;
      Options   : RTEMS_Options;
      Timeout   : RTEMS_Interval) return int
     with Import, External_Name => "rtems_semaphore_obtain", Convention => C;

   function RTEMS_Semaphore_Release (Semaphore : Binary_Semaphore_Id)
     return int
     with Import, External_Name => "rtems_semaphore_release", Convention => C;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : timespec) return Duration is
   begin
      return Duration (TS.tv_sec) + Duration (TS.tv_nsec) / 10#1#E9;
   end To_Duration;

   ------------------------
   -- To_Target_Priority --
   ------------------------

   function To_Target_Priority
     (Prio : System.Any_Priority) return Interfaces.C.int
   is
   begin
      return Interfaces.C.int (Prio);
   end To_Target_Priority;

   -----------------
   -- To_Timespec --
   -----------------

   function To_Timespec (D : Duration) return timespec is
      S : time_t;
      F : Duration;
   begin
      S := time_t (Long_Long_Integer (D));
      F := D - Duration (S);

      --  If F has negative value due to round-up, adjust for positive F value

      if F < 0.0 then
         S := S - 1;
         F := F + 1.0;
      end if;
      return timespec'(tv_sec => S,
                       tv_nsec => long (Long_Long_Integer (F * 10#1#E9)));
   end To_Timespec;

   -----------------------------
   -- Binary_Semaphore_Create --
   -----------------------------

   function Binary_Semaphore_Create return Binary_Semaphore_Id is
      Semaphore : Binary_Semaphore_Id;
      Status    : int;
   begin
      Status :=
        RTEMS_Semaphore_Create
          (Name             => RTEMS_Build_Name ('G', 'N', 'A', 'T'),
           Count            => 0,
           Attributes       => RTEMS_SIMPLE_BINARY_SEMAPHORE or RTEMS_FIFO,
           Priority_Ceiling => 0,
           Semaphore        => Semaphore);

      pragma Assert (Status = 0);

      return Semaphore;
   end Binary_Semaphore_Create;

   -----------------------------
   -- Binary_Semaphore_Delete --
   -----------------------------

   function Binary_Semaphore_Delete (ID : Binary_Semaphore_Id)
     return int is
   begin
      return RTEMS_Semaphore_Delete (ID);
   end Binary_Semaphore_Delete;

   -----------------------------
   -- Binary_Semaphore_Obtain --
   -----------------------------

   function Binary_Semaphore_Obtain (ID : Binary_Semaphore_Id)
     return int is
   begin
      return RTEMS_Semaphore_Obtain (ID, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
   end Binary_Semaphore_Obtain;

   ------------------------------
   -- Binary_Semaphore_Release --
   ------------------------------

   function Binary_Semaphore_Release (ID : Binary_Semaphore_Id)
     return int is
   begin
      return RTEMS_Semaphore_Release (ID);
   end Binary_Semaphore_Release;

   ----------------------------
   -- Binary_Semaphore_Flush --
   ----------------------------

   function Binary_Semaphore_Flush (ID : Binary_Semaphore_Id) return int is
   begin
      return RTEMS_Semaphore_Flush (ID);
   end Binary_Semaphore_Flush;

   -----------------------
   -- Interrupt_Connect --
   -----------------------

   function Interrupt_Connect
     (Vector    : Interrupt_Vector;
      Handler   : Interrupt_Handler;
      Parameter : System.Address := System.Null_Address) return int
   is
      function RTEMS_Interrupt_Handler_Install
         (Vector    : Interrupt_Vector;
          Info      : char_array;
          Options   : RTEMS_Options;
          Handler   : Interrupt_Handler;
          Parameter : System.Address) return int
        with Import,
             External_Name => "rtems_interrupt_handler_install",
             Convention => C;

      Info_String : constant char_array := To_C ("GNAT Interrupt Handler");
      --  Handler name that is registered with RTEMS
   begin
      return
        RTEMS_Interrupt_Handler_Install
          (Vector    => Vector,
           Info      => Info_String,
           Options   => RTEMS_INTERRUPT_UNIQUE,
           Handler   => Handler,
           Parameter => Parameter);
   end Interrupt_Connect;

   --------------------------------
   -- Interrupt_Number_To_Vector --
   --------------------------------

   function Interrupt_Number_To_Vector (intNum : int)
     return Interrupt_Vector
   is
   begin
      return Interrupt_Vector (intNum);
   end Interrupt_Number_To_Vector;

   ------------------
   -- pthread_init --
   ------------------

   procedure pthread_init is
   begin
      null;
   end pthread_init;

   --------------------
   -- Get_Stack_Base --
   --------------------

   function Get_Stack_Base (thread : pthread_t) return Address is
      pragma Warnings (Off, thread);

   begin
      return Null_Address;
   end Get_Stack_Base;

   -----------------
   -- sigaltstack --
   -----------------

   function sigaltstack
     (ss  : not null access stack_t;
      oss : access stack_t) return int is
      pragma Unreferenced (ss);
      pragma Unreferenced (oss);
   begin
      return 0;
   end sigaltstack;

   -----------------------------------
   -- pthread_rwlockattr_setkind_np --
   -----------------------------------

   function pthread_rwlockattr_setkind_np
     (attr : access pthread_rwlockattr_t;
      pref : int) return int is
      pragma Unreferenced (attr);
      pragma Unreferenced (pref);
   begin
      return 0;
   end pthread_rwlockattr_setkind_np;

end System.OS_Interface;
