------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--         G N A T . E X P E C T . N O N _ B L O C K I N G _ S P A W N      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 2002 Ada Core Technologies, Inc.              --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  This is the default version. Used everywhere except VMS.

separate (GNAT.Expect)
procedure Non_Blocking_Spawn
  (Descriptor  : out Process_Descriptor'Class;
   Command     : String;
   Args        : GNAT.OS_Lib.Argument_List;
   Buffer_Size : Natural := 4096;
   Err_To_Out  : Boolean := False)
is
   function Fork return Process_Id;
   pragma Import (C, Fork, "__gnat_expect_fork");
   --  Starts a new process if possible.
   --  See the Unix command fork for more information. On systems that
   --  don't support this capability (Windows...), this command does
   --  nothing, and Fork will return Null_Pid.

   Pipe1, Pipe2, Pipe3 : aliased Pipe_Type;

   Arg      : String_Access;
   Arg_List : aliased array (1 .. Args'Length + 2) of System.Address;

   Command_With_Path : String_Access;

begin
   --  Create the rest of the pipes

   Set_Up_Communications
     (Descriptor, Err_To_Out, Pipe1'Access, Pipe2'Access, Pipe3'Access);

   --  Fork a new process

   Descriptor.Pid := Fork;

   --  Are we now in the child (or, for Windows, still in the common
   --  process).

   if Descriptor.Pid = Null_Pid then

      Command_With_Path := Locate_Exec_On_Path (Command);

      --  Prepare an array of arguments to pass to C
      Arg   := new String (1 .. Command_With_Path'Length + 1);
      Arg (1 .. Command_With_Path'Length) := Command_With_Path.all;
      Arg (Arg'Last)        := ASCII.Nul;
      Arg_List (1)          := Arg.all'Address;

      for J in Args'Range loop
         Arg                     := new String (1 .. Args (J)'Length + 1);
         Arg (1 .. Args (J)'Length)  := Args (J).all;
         Arg (Arg'Last)              := ASCII.Nul;
         Arg_List (J + 2 - Args'First) := Arg.all'Address;
      end loop;

      Arg_List (Arg_List'Last) := System.Null_Address;

      --  This does not return on Unix systems

      Set_Up_Child_Communications
        (Descriptor, Pipe1, Pipe2, Pipe3, Command_With_Path.all,
         Arg_List'Address);

      Free (Command_With_Path);
   end if;

   --  Did we have an error when spawning the child ?

   if Descriptor.Pid < Null_Pid then
      null;
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
