------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . P R O G R A M  _  I N F O                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-1999 Free Software Foundation, Inc.          --
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

--  This is an Irix (old pthread library) version of this package.

--  This package   contains the parameters  used by   the run-time system at
--  program startup.  These parameters are  isolated in this package body to
--  facilitate replacement by the end user.
--
--  To replace the default values, copy this source file into your build
--  directory, edit the file to reflect your desired behavior, and recompile
--  with the command:
--
--     % gcc -c -O2 -gnatpg s-proinf.adb
--
--  then relink your application as usual.
--

with GNAT.OS_Lib;

package body System.Program_Info is

   Kbytes : constant := 1024;

   Default_Initial_Sproc_Count  : constant := 0;
   Default_Max_Sproc_Count      : constant := 128;
   Default_Sproc_Stack_Size     : constant := 16#4000#;
   Default_Stack_Guard_Pages    : constant := 1;
   Default_Default_Time_Slice   : constant := 0.0;
   Default_Default_Task_Stack   : constant := 12 * Kbytes;
   Default_Pthread_Sched_Signal : constant := 35;
   Default_Pthread_Arena_Size   : constant := 16#40000#;
   Default_Os_Default_Priority  : constant := 0;

   -------------------------
   -- Initial_Sproc_Count --
   -------------------------

   function Initial_Sproc_Count return Integer is

      function sysmp (P1 : Integer) return Integer;
      pragma Import (C, sysmp, "sysmp", "sysmp");

      MP_NPROCS : constant := 1; --   # processor in complex

      Pthread_Sproc_Count : constant GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Getenv ("PTHREAD_SPROC_COUNT");

   begin
      if Pthread_Sproc_Count.all'Length = 0 then
         return Default_Initial_Sproc_Count;

      elsif Pthread_Sproc_Count.all = "AUTO" then
         return sysmp (MP_NPROCS);

      else
         return Integer'Value (Pthread_Sproc_Count.all);
      end if;
   exception
      when others =>
         return Default_Initial_Sproc_Count;
   end Initial_Sproc_Count;

   ---------------------
   -- Max_Sproc_Count --
   ---------------------

   function Max_Sproc_Count return Integer is
      Pthread_Max_Sproc_Count : constant GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Getenv ("PTHREAD_MAX_SPROC_COUNT");

   begin
      if Pthread_Max_Sproc_Count.all'Length = 0 then
         return Default_Max_Sproc_Count;
      else
         return Integer'Value (Pthread_Max_Sproc_Count.all);
      end if;
   exception
      when others =>
         return Default_Max_Sproc_Count;
   end Max_Sproc_Count;

   ----------------------
   -- Sproc_Stack_Size --
   ----------------------

   function Sproc_Stack_Size return Integer is
   begin
      return Default_Sproc_Stack_Size;
   end Sproc_Stack_Size;

   ------------------------
   -- Default_Time_Slice --
   ------------------------

   function Default_Time_Slice return Duration is
      Pthread_Time_Slice_Sec : constant GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Getenv ("PTHREAD_TIME_SLICE_SEC");
      Pthread_Time_Slice_Usec : constant GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Getenv ("PTHREAD_TIME_SLICE_USEC");

      Val_Sec, Val_Usec : Integer := 0;

   begin
      if Pthread_Time_Slice_Sec.all'Length /= 0 or
        Pthread_Time_Slice_Usec.all'Length /= 0
      then
         if Pthread_Time_Slice_Sec.all'Length /= 0 then
            Val_Sec := Integer'Value (Pthread_Time_Slice_Sec.all);
         end if;

         if Pthread_Time_Slice_Usec.all'Length /= 0 then
            Val_Usec := Integer'Value (Pthread_Time_Slice_Usec.all);
         end if;

         return Duration (Val_Sec) + Duration (Val_Usec) / 1000.0;
      else
         return Default_Default_Time_Slice;
      end if;

   exception
      when others =>
         return Default_Default_Time_Slice;
   end Default_Time_Slice;

   ------------------------
   -- Default_Task_Stack --
   ------------------------

   function Default_Task_Stack return Integer is
   begin
      return Default_Default_Task_Stack;
   end Default_Task_Stack;

   -----------------------
   -- Stack_Guard_Pages --
   -----------------------

   function Stack_Guard_Pages return Integer is
      Pthread_Stack_Guard_Pages : constant GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Getenv ("PTHREAD_STACK_GUARD_PAGES");

   begin
      if Pthread_Stack_Guard_Pages.all'Length /= 0 then
         return Integer'Value (Pthread_Stack_Guard_Pages.all);
      else
         return Default_Stack_Guard_Pages;
      end if;
   exception
      when others =>
         return Default_Stack_Guard_Pages;
   end Stack_Guard_Pages;

   --------------------------
   -- Pthread_Sched_Signal --
   --------------------------

   function Pthread_Sched_Signal return Integer is
   begin
      return Default_Pthread_Sched_Signal;
   end Pthread_Sched_Signal;

   ------------------------
   -- Pthread_Arena_Size --
   ------------------------

   function Pthread_Arena_Size  return Integer is
      Pthread_Arena_Size : constant GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Getenv ("PTHREAD_ARENA_SIZE");

   begin
      if Pthread_Arena_Size.all'Length = 0 then
         return Default_Pthread_Arena_Size;
      else
         return Integer'Value (Pthread_Arena_Size.all);
      end if;
   exception
      when others =>
         return Default_Pthread_Arena_Size;
   end Pthread_Arena_Size;

   -------------------------
   -- Os_Default_Priority --
   -------------------------

   function Os_Default_Priority return Integer is
   begin
      return Default_Os_Default_Priority;
   end Os_Default_Priority;

end System.Program_Info;
