------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                      G N A T . E X P E C T . T T Y                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2000-2024, AdaCore                      --
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

with GNAT.TTY;

with System;
with System.OS_Constants;

package GNAT.Expect.TTY is

   pragma Linker_Options (System.OS_Constants.PTY_Library);

   ------------------
   --  TTY_Process --
   ------------------

   type TTY_Process_Descriptor is new Process_Descriptor with private;
   --  Similar to Process_Descriptor, with the parent set up as a full terminal
   --  (Unix sense, see tty(4)).

   procedure Pseudo_Descriptor
     (Descriptor  : out TTY_Process_Descriptor'Class;
      TTY         : GNAT.TTY.TTY_Handle;
      Buffer_Size : Natural := 4096);
   --  Given a terminal descriptor (TTY), create a pseudo process descriptor
   --  to be used with GNAT.Expect.
   --
   --  Note that it is invalid to call Close, Interrupt, Send_Signal on the
   --  resulting descriptor. To deallocate memory associated with Process,
   --  call Close_Pseudo_Descriptor instead.

   procedure Close_Pseudo_Descriptor
     (Descriptor : in out TTY_Process_Descriptor);
   --  Free memory and ressources associated with Descriptor. Will *not*
   --  close the associated TTY, it is the caller's responsibility to call
   --  GNAT.TTY.Close_TTY.

   procedure Interrupt (Pid : Integer);
   --  Interrupt a process given its pid.
   --  This is equivalent to sending a ctrl-c event, or kill -SIGINT.

   procedure Terminate_Process (Pid : Integer);
   --  Terminate abruptly a process given its pid.
   --  This is equivalent to kill -SIGKILL under unix, or TerminateProcess
   --  under Windows.

   overriding procedure Send
     (Descriptor   : in out TTY_Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False);
   --  See parent
   --  What does that comment mean??? what is "parent" here

   procedure Set_Use_Pipes
     (Descriptor : in out TTY_Process_Descriptor;
      Use_Pipes  : Boolean);
   --  Tell Expect.TTY whether to use Pipes or Console (on windows). Needs to
   --  be set before spawning the process. Default is to use Pipes.

   procedure Set_Size
     (Descriptor : in out TTY_Process_Descriptor'Class;
      Rows       : Natural;
      Columns    : Natural);
   --  Sets up the size of the terminal as reported to the spawned process

   function Is_Process_Running
      (Descriptor : in out TTY_Process_Descriptor)
      return Boolean;
   --  Returns True if the process is still alive

private

   --  All declarations in the private part must be fully commented ???

   overriding procedure Close
     (Descriptor : in out TTY_Process_Descriptor;
      Status     : out Integer);

   overriding procedure Close
     (Descriptor : in out TTY_Process_Descriptor);

   overriding procedure Interrupt (Descriptor : in out TTY_Process_Descriptor);
   --  When we use pseudo-terminals, we do not need to use signals to
   --  interrupt the debugger, we can simply send the appropriate character.
   --  This provides a better support for remote debugging for instance.

   procedure Set_Up_Communications
     (Pid        : in out TTY_Process_Descriptor;
      Err_To_Out : Boolean;
      Pipe1      : not null access Pipe_Type;
      Pipe2      : not null access Pipe_Type;
      Pipe3      : not null access Pipe_Type);

   procedure Set_Up_Parent_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type);

   procedure Set_Up_Child_Communications
     (Pid   : in out TTY_Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type;
      Cmd   : String;
      Args  : System.Address);

   procedure Close_Input (Descriptor : in out TTY_Process_Descriptor);

   Still_Active : constant Integer := -1;

   type TTY_Process_Descriptor is new Process_Descriptor with record
      Process     : System.Address := System.Null_Address;
      --  Underlying structure used in C
      Exit_Status : Integer := Still_Active;
      --  Holds the exit status of the process
      Use_Pipes   : Boolean := True;
   end record;

end GNAT.Expect.TTY;
