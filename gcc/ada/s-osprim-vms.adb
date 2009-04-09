------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the OpenVMS/Alpha version of this file

with System.Aux_DEC;

package body System.OS_Primitives is

   --------------------------------------
   -- Local functions and declarations --
   --------------------------------------

   function Get_GMToff return Integer;
   pragma Import (C, Get_GMToff, "get_gmtoff");
   --  Get the offset from GMT for this timezone

   function VMS_Epoch_Offset return Long_Integer;
   pragma Inline (VMS_Epoch_Offset);
   --  The offset between the Unix Epoch and the VMS Epoch

   subtype Cond_Value_Type is System.Aux_DEC.Unsigned_Longword;
   --  Condition Value return type

   ----------------------
   -- VMS_Epoch_Offset --
   ----------------------

   function VMS_Epoch_Offset return Long_Integer is
   begin
      return 10_000_000 * (3_506_716_800 + Long_Integer (Get_GMToff));
   end VMS_Epoch_Offset;

   ----------------
   -- Sys_Schdwk --
   ----------------
   --
   --  Schedule Wakeup
   --
   --  status = returned status
   --  pidadr = address of process id to be woken up
   --  prcnam = name of process to be woken up
   --  daytim = time to wake up
   --  reptim = repetition interval of wakeup calls
   --

   procedure Sys_Schdwk
     (
      Status : out Cond_Value_Type;
      Pidadr : Address := Null_Address;
      Prcnam : String := String'Null_Parameter;
      Daytim : Long_Integer;
      Reptim : Long_Integer := Long_Integer'Null_Parameter
     );

   pragma Interface (External, Sys_Schdwk);
   --  VMS system call to schedule a wakeup event
   pragma Import_Valued_Procedure
     (Sys_Schdwk, "SYS$SCHDWK",
      (Cond_Value_Type, Address, String,         Long_Integer, Long_Integer),
      (Value,           Value,   Descriptor (S), Reference,    Reference)
     );

   ----------------
   -- Sys_Gettim --
   ----------------
   --
   --  Get System Time
   --
   --  status = returned status
   --  tim    = current system time
   --

   procedure Sys_Gettim
     (
      Status : out Cond_Value_Type;
      Tim    : out OS_Time
     );
   --  VMS system call to get the current system time
   pragma Interface (External, Sys_Gettim);
   pragma Import_Valued_Procedure
     (Sys_Gettim, "SYS$GETTIM",
      (Cond_Value_Type, OS_Time),
      (Value,           Reference)
     );

   ---------------
   -- Sys_Hiber --
   ---------------

   --  Hibernate (until woken up)

   --  status = returned status

   procedure Sys_Hiber (Status : out Cond_Value_Type);
   --  VMS system call to hibernate the current process
   pragma Interface (External, Sys_Hiber);
   pragma Import_Valued_Procedure
     (Sys_Hiber, "SYS$HIBER",
      (Cond_Value_Type),
      (Value)
     );

   -----------
   -- Clock --
   -----------

   function OS_Clock return OS_Time is
      Status : Cond_Value_Type;
      T      : OS_Time;
   begin
      Sys_Gettim (Status, T);
      return (T);
   end OS_Clock;

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
   begin
      return To_Duration (OS_Clock, Absolute_Calendar);
   end Clock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   function Monotonic_Clock return Duration renames Clock;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Time : Duration;
      Mode : Integer)
   is
      Sleep_Time : OS_Time;
      Status     : Cond_Value_Type;
      pragma Unreferenced (Status);

   begin
      Sleep_Time := To_OS_Time (Time, Mode);
      Sys_Schdwk (Status => Status, Daytim => Sleep_Time);
      Sys_Hiber (Status);
   end Timed_Delay;

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (T : OS_Time; Mode : Integer) return Duration is
      pragma Warnings (Off, Mode);
   begin
      return Duration'Fixed_Value (T - VMS_Epoch_Offset) * 100;
   end To_Duration;

   ----------------
   -- To_OS_Time --
   ----------------

   function To_OS_Time (D : Duration; Mode : Integer) return OS_Time is
   begin
      if Mode = Relative then
         return -(Long_Integer'Integer_Value (D) / 100);
      else
         return Long_Integer'Integer_Value (D) / 100 + VMS_Epoch_Offset;
      end if;
   end To_OS_Time;

end System.OS_Primitives;
