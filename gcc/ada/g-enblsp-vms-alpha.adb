------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--         G N A T . E X P E C T . N O N _ B L O C K I N G _ S P A W N      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2005-2010, AdaCore                      --
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

--  This package provides a target dependent non-blocking spawn function
--  for use by the VMS GNAT.Expect package (g-expect-vms.adb). This package
--  should not be directly with'ed by an application program.

--  This version is for Alpha/VMS

separate (GNAT.Expect)
procedure Non_Blocking_Spawn
  (Descriptor  : out Process_Descriptor'Class;
   Command     : String;
   Args        : GNAT.OS_Lib.Argument_List;
   Buffer_Size : Natural := 4096;
   Err_To_Out  : Boolean := False)
is
   function Alloc_Vfork_Blocks return Integer;
   pragma Import (C, Alloc_Vfork_Blocks, "decc$$alloc_vfork_blocks");

   function Get_Vfork_Jmpbuf return System.Address;
   pragma Import (C, Get_Vfork_Jmpbuf, "decc$$get_vfork_jmpbuf");

   function Get_Current_Invo_Context
     (Addr : System.Address) return Process_Id;
   pragma Import (C, Get_Current_Invo_Context,
     "LIB$GET_CURRENT_INVO_CONTEXT");

   Pipe1, Pipe2, Pipe3 : aliased Pipe_Type;

   Arg      : String_Access;
   Arg_List : aliased array (1 .. Args'Length + 2) of System.Address;

   Command_With_Path : String_Access;

begin
   --  Create the rest of the pipes

   Set_Up_Communications
     (Descriptor, Err_To_Out, Pipe1'Access, Pipe2'Access, Pipe3'Access);

   Command_With_Path := Locate_Exec_On_Path (Command);

   if Command_With_Path = null then
      raise Invalid_Process;
   end if;

   --  Fork a new process (it is not possible to do this in a subprogram)

   Descriptor.Pid :=
     (if Alloc_Vfork_Blocks >= 0
      then Get_Current_Invo_Context (Get_Vfork_Jmpbuf) else -1);

   --  Are we now in the child

   if Descriptor.Pid = Null_Pid then

      --  Prepare an array of arguments to pass to C

      Arg   := new String (1 .. Command_With_Path'Length + 1);
      Arg (1 .. Command_With_Path'Length) := Command_With_Path.all;
      Arg (Arg'Last)        := ASCII.NUL;
      Arg_List (1)          := Arg.all'Address;

      for J in Args'Range loop
         Arg                     := new String (1 .. Args (J)'Length + 1);
         Arg (1 .. Args (J)'Length)  := Args (J).all;
         Arg (Arg'Last)              := ASCII.NUL;
         Arg_List (J + 2 - Args'First) := Arg.all'Address;
      end loop;

      Arg_List (Arg_List'Last) := System.Null_Address;

      --  This does not return on Unix systems

      Set_Up_Child_Communications
        (Descriptor, Pipe1, Pipe2, Pipe3, Command_With_Path.all,
         Arg_List'Address);
   end if;

   Free (Command_With_Path);

   --  Did we have an error when spawning the child ?

   if Descriptor.Pid < Null_Pid then
      raise Invalid_Process;
   else
      --  We are now in the parent process

      Set_Up_Parent_Communications (Descriptor, Pipe1, Pipe2, Pipe3);
   end if;

   --  Create the buffer

   Descriptor.Buffer_Size := Buffer_Size;

   if Buffer_Size /= 0 then
      Descriptor.Buffer := new String (1 .. Positive (Buffer_Size));
   end if;
end Non_Blocking_Spawn;
