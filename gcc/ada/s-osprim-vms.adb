------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . O S _ P R I M I T I V E S                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1998-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
   --  reptim = repitition interval of wakeup calls
   --

   procedure Sys_Schdwk
     (
      Status : out Cond_Value_Type;
      Pidadr : in Address := Null_Address;
      Prcnam : in String := String'Null_Parameter;
      Daytim : in Long_Integer;
      Reptim : in Long_Integer := Long_Integer'Null_Parameter
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
