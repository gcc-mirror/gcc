------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      G N A T . E X P E C T . T T Y                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2000-2019, AdaCore                      --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with System; use System;

package body GNAT.Expect.TTY is

   On_Windows : constant Boolean := Directory_Separator = '\';
   --  True when on Windows

   function Waitpid
     (Process  : System.Address;
      Blocking : Integer) return Integer;
   pragma Import (C, Waitpid, "__gnat_tty_waitpid");
   --  Wait for a specific process id, and return its exit code

   ------------------------
   -- Is_Process_Running --
   ------------------------

   function Is_Process_Running
     (Descriptor : in out TTY_Process_Descriptor) return Boolean
   is
   begin
      if Descriptor.Process = System.Null_Address then
         return False;
      end if;

      Descriptor.Exit_Status := Waitpid (Descriptor.Process, Blocking => 0);

      return Descriptor.Exit_Status = Still_Active;
   end Is_Process_Running;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Descriptor : in out TTY_Process_Descriptor;
      Status     : out Integer)
   is
      procedure Terminate_Process (Process : System.Address);
      pragma Import (C, Terminate_Process, "__gnat_terminate_process");

      procedure Free_Process (Process : System.Address);
      pragma Import (C, Free_Process, "__gnat_free_process");

   begin
      --  If we haven't already closed the process

      if Descriptor.Process = System.Null_Address then
         Status := Descriptor.Exit_Status;

      else
         --  Send a Ctrl-C to the process first. This way, if the launched
         --  process is a "sh" or "cmd", the child processes will get
         --  terminated as well. Otherwise, terminating the main process
         --  brutally will leave the children running.

         --  Note: special characters are sent to the terminal to generate the
         --  signal, so this needs to be done while the file descriptors are
         --  still open (it used to be after the closes and that was wrong).

         Close_Input (Descriptor);

         if Descriptor.Error_Fd /= Descriptor.Output_Fd
           and then Descriptor.Error_Fd /= Invalid_FD
         then
            Close (Descriptor.Error_Fd);
         end if;

         if Descriptor.Output_Fd /= Invalid_FD then
            Close (Descriptor.Output_Fd);
         end if;

         if Descriptor.Exit_Status = Still_Active then
            Status := Waitpid (Descriptor.Process, Blocking => 0);

            if Status = Still_Active then
               --  In theory the process might have died since the check. In
               --  practice the following calls should not cause any issue.

               Interrupt (Descriptor);
               delay (0.05);
               Terminate_Process (Descriptor.Process);
               Status := Waitpid (Descriptor.Process, Blocking => 1);
               Descriptor.Exit_Status := Status;
            end if;

         else
            --  If Exit_Status is not STILL_ACTIVE just retrieve the saved
            --  exit status.

            Status := Descriptor.Exit_Status;
         end if;

         Free_Process (Descriptor.Process'Address);
         Descriptor.Process := System.Null_Address;

         GNAT.OS_Lib.Free (Descriptor.Buffer);
         Descriptor.Buffer_Size := 0;
      end if;
   end Close;

   overriding procedure Close (Descriptor : in out TTY_Process_Descriptor) is
      Status : Integer;
   begin
      Close (Descriptor, Status);
   end Close;

   -----------------
   -- Close_Input --
   -----------------

   overriding procedure Close_Input
     (Descriptor : in out TTY_Process_Descriptor)
   is
      function TTY_FD
        (Handle : System.Address) return GNAT.OS_Lib.File_Descriptor;
      pragma Import (C, TTY_FD, "__gnat_tty_fd");

      procedure Close_TTY (Process : System.Address);
      pragma Import (C, Close_TTY, "__gnat_close_tty");

   begin
      if not On_Windows and then Descriptor.Process /= System.Null_Address then
         --  Check whether input/output/error streams use master descriptor and
         --  reset corresponding members.

         if Descriptor.Input_Fd = TTY_FD (Descriptor.Process) then
            Descriptor.Input_Fd := Invalid_FD;
         end if;

         if Descriptor.Output_Fd = TTY_FD (Descriptor.Process) then
            Descriptor.Output_Fd := Invalid_FD;
         end if;

         if Descriptor.Error_Fd = TTY_FD (Descriptor.Process) then
            Descriptor.Error_Fd := Invalid_FD;
         end if;

         --  Close master descriptor.

         Close_TTY (Descriptor.Process);
      end if;

      --  Call parent's implementation to close all remaining descriptors.

      Process_Descriptor (Descriptor).Close_Input;
   end Close_Input;

   -----------------------------
   -- Close_Pseudo_Descriptor --
   -----------------------------

   procedure Close_Pseudo_Descriptor
     (Descriptor : in out TTY_Process_Descriptor)
   is
   begin
      Descriptor.Buffer_Size := 0;
      GNAT.OS_Lib.Free (Descriptor.Buffer);
   end Close_Pseudo_Descriptor;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt
     (Descriptor : in out TTY_Process_Descriptor)
   is
      procedure Internal (Process : System.Address);
      pragma Import (C, Internal, "__gnat_interrupt_process");
   begin
      if Descriptor.Process /= System.Null_Address then
         Internal (Descriptor.Process);
      end if;
   end Interrupt;

   procedure Interrupt (Pid : Integer) is
      procedure Internal (Pid : Integer);
      pragma Import (C, Internal, "__gnat_interrupt_pid");
   begin
      Internal (Pid);
   end Interrupt;

   -----------------------
   -- Terminate_Process --
   -----------------------

   procedure Terminate_Process (Pid : Integer) is
      procedure Internal (Pid : Integer);
      pragma Import (C, Internal, "__gnat_terminate_pid");
   begin
      Internal (Pid);
   end Terminate_Process;

   -----------------------
   -- Pseudo_Descriptor --
   -----------------------

   procedure Pseudo_Descriptor
     (Descriptor  : out TTY_Process_Descriptor'Class;
      TTY         : GNAT.TTY.TTY_Handle;
      Buffer_Size : Natural := 4096) is
   begin
      Descriptor.Input_Fd  := GNAT.TTY.TTY_Descriptor (TTY);
      Descriptor.Output_Fd := Descriptor.Input_Fd;

      --  Create the buffer

      Descriptor.Buffer_Size := Buffer_Size;

      if Buffer_Size /= 0 then
         Descriptor.Buffer := new String (1 .. Positive (Buffer_Size));
      end if;
   end Pseudo_Descriptor;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Descriptor   : in out TTY_Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False)
   is
      Header : String (1 .. 5);
      Length : Natural;
      Ret    : Natural;

      procedure Internal
        (Process : System.Address;
         S       : in out String;
         Length  : Natural;
         Ret     : out Natural);
      pragma Import (C, Internal, "__gnat_send_header");

   begin
      Length := Str'Length;

      if Add_LF then
         Length := Length + 1;
      end if;

      Internal (Descriptor.Process, Header, Length, Ret);

      if Ret = 1 then

         --  Need to use the header

         GNAT.Expect.Send
           (Process_Descriptor (Descriptor),
            Header & Str, Add_LF, Empty_Buffer);

      else
         GNAT.Expect.Send
           (Process_Descriptor (Descriptor),
            Str, Add_LF, Empty_Buffer);
      end if;
   end Send;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Descriptor : in out TTY_Process_Descriptor'Class;
      Rows       : Natural;
      Columns    : Natural)
   is
      procedure Internal (Process : System.Address; R, C : Integer);
      pragma Import (C, Internal, "__gnat_setup_winsize");
   begin
      if Descriptor.Process /= System.Null_Address then
         Internal (Descriptor.Process, Rows, Columns);
      end if;
   end Set_Size;

   ---------------------------
   -- Set_Up_Communications --
   ---------------------------

   overriding procedure Set_Up_Communications
     (Pid        : in out TTY_Process_Descriptor;
      Err_To_Out : Boolean;
      Pipe1      : access Pipe_Type;
      Pipe2      : access Pipe_Type;
      Pipe3      : access Pipe_Type)
   is
      pragma Unreferenced (Err_To_Out, Pipe1, Pipe2, Pipe3);

      function Internal (Process : System.Address) return Integer;
      pragma Import (C, Internal, "__gnat_setup_communication");

   begin
      Pid.Exit_Status := Still_Active;
      if Internal (Pid.Process'Address) /= 0 then
         raise Invalid_Process with "cannot setup communication.";
      end if;
   end Set_Up_Communications;

   ---------------------------------
   -- Set_Up_Child_Communications --
   ---------------------------------

   overriding procedure Set_Up_Child_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type;
      Cmd   : String;
      Args  : System.Address)
   is
      pragma Unreferenced (Pipe1, Pipe2, Pipe3, Cmd);
      function Internal
        (Process : System.Address; Argv : System.Address; Use_Pipes : Integer)
         return Process_Id;
      pragma Import (C, Internal, "__gnat_setup_child_communication");

   begin
      Pid.Pid := Internal (Pid.Process, Args, Boolean'Pos (Pid.Use_Pipes));
   end Set_Up_Child_Communications;

   ----------------------------------
   -- Set_Up_Parent_Communications --
   ----------------------------------

   overriding procedure Set_Up_Parent_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type)
   is
      pragma Unreferenced (Pipe1, Pipe2, Pipe3);

      procedure Internal
        (Process  : System.Address;
         Inputfp  : out File_Descriptor;
         Outputfp : out File_Descriptor;
         Errorfp  : out File_Descriptor;
         Pid      : out Process_Id);
      pragma Import (C, Internal, "__gnat_setup_parent_communication");

   begin
      Internal
        (Pid.Process, Pid.Input_Fd, Pid.Output_Fd, Pid.Error_Fd, Pid.Pid);
   end Set_Up_Parent_Communications;

   -------------------
   -- Set_Use_Pipes --
   -------------------

   procedure Set_Use_Pipes
     (Descriptor : in out TTY_Process_Descriptor;
      Use_Pipes  : Boolean) is
   begin
      Descriptor.Use_Pipes := Use_Pipes;
   end Set_Use_Pipes;

end GNAT.Expect.TTY;
