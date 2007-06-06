------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                          G N A T . E X P E C T                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2000-2007, AdaCore                     --
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

--  Currently this package is implemented on all native GNAT ports except
--  for VMS. It is not yet implemented for any of the cross-ports (e.g. it
--  is not available for VxWorks or LynxOS).

--  -----------
--  -- Usage --
--  -----------

--  This package provides a set of subprograms similar to what is available
--  with the standard Tcl Expect tool.

--  It allows you to easily spawn and communicate with an external process.
--  You can send commands or inputs to the process, and compare the output
--  with some expected regular expression.

--  Usage example:

--      Non_Blocking_Spawn
--         (Fd, "ftp",
--           (1 => new String' ("machine@domain")));
--      Timeout := 10000;  --  10 seconds
--      Expect (Fd, Result, Regexp_Array'(+"\(user\)", +"\(passwd\)"),
--              Timeout);
--      case Result is
--         when 1 => Send (Fd, "my_name");   --  matched "user"
--         when 2 => Send (Fd, "my_passwd"); --  matched "passwd"
--         when Expect_Timeout => null;      --  timeout
--         when others => null;
--      end case;
--      Close (Fd);

--  You can also combine multiple regular expressions together, and get the
--  specific string matching a parenthesis pair by doing something like. If you
--  expect either "lang=optional ada" or "lang=ada" from the external process,
--  you can group the two together, which is more efficient, and simply get the
--  name of the language by doing:

--      declare
--         Matched : Match_Array (0 .. 2);
--      begin
--         Expect (Fd, Result, "lang=(optional)? ([a-z]+)", Matched);
--         Put_Line ("Seen: " &
--                   Expect_Out (Fd) (Matched (2).First .. Matched (2).Last));
--      end;

--  Alternatively, you might choose to use a lower-level interface to the
--  processes, where you can give your own input and output filters every
--  time characters are read from or written to the process.

--      procedure My_Filter
--        (Descriptor : Process_Descriptor'Class;
--         Str        : String;
--         User_Data  : System.Address)
--      is
--      begin
--         Put_Line (Str);
--      end;

--      Non_Blocking_Spawn
--        (Fd, "tail",
--         (new String' ("-f"), new String' ("a_file")));
--      Add_Filter (Fd, My_Filter'Access, Output);
--      Expect (Fd, Result, "", 0);  --  wait forever

--  The above example should probably be run in a separate task, since it is
--  blocking on the call to Expect.

--  Both examples can be combined, for instance to systematically print the
--  output seen by expect, even though you still want to let Expect do the
--  filtering. You can use the Trace_Filter subprogram for such a filter.

--  If you want to get the output of a simple command, and ignore any previous
--  existing output, it is recommended to do something like:

--      Expect (Fd, Result, ".*", Timeout => 0);
--      -- Empty the buffer, by matching everything (after checking
--      -- if there was any input).

--      Send (Fd, "command");
--      Expect (Fd, Result, ".."); -- match only on the output of command

--  -----------------
--  -- Task Safety --
--  -----------------

--  This package is not task-safe: there should be not concurrent calls to
--  the functions defined in this package. In other words, separate tasks
--  may not access the facilities of this package without synchronization
--  that serializes access.

with System;
with GNAT.OS_Lib;
with GNAT.Regpat;

package GNAT.Expect is

   type Process_Id is new Integer;
   Invalid_Pid : constant Process_Id := -1;
   Null_Pid    : constant Process_Id := 0;

   type Filter_Type is (Output, Input, Died);
   --  The signals that are emitted by the Process_Descriptor upon state
   --  changed in the child. One can connect to any of this signal through
   --  the Add_Filter subprograms.
   --
   --     Output => Every time new characters are read from the process
   --               associated with Descriptor, the filter is called with
   --               these new characters in argument.
   --
   --               Note that output is only generated when the program is
   --               blocked in a call to Expect.
   --
   --     Input  => Every time new characters are written to the process
   --               associated with Descriptor, the filter is called with
   --               these new characters in argument.
   --               Note that input is only generated by calls to Send.
   --
   --     Died   => The child process has died, or was explicitly killed

   type Process_Descriptor is tagged private;
   --  Contains all the components needed to describe a process handled
   --  in this package, including a process identifier, file descriptors
   --  associated with the standard input, output and error, and the buffer
   --  needed to handle the expect calls.

   type Process_Descriptor_Access is access Process_Descriptor'Class;

   ------------------------
   -- Spawning a process --
   ------------------------

   procedure Non_Blocking_Spawn
     (Descriptor  : out Process_Descriptor'Class;
      Command     : String;
      Args        : GNAT.OS_Lib.Argument_List;
      Buffer_Size : Natural := 4096;
      Err_To_Out  : Boolean := False);
   --  This call spawns a new process and allows sending commands to
   --  the process and/or automatic parsing of the output.
   --
   --  The expect buffer associated with that process can contain at most
   --  Buffer_Size characters. Older characters are simply discarded when
   --  this buffer is full. Beware that if the buffer is too big, this could
   --  slow down the Expect calls if not output is matched, since Expect has
   --  to match all the regexp against all the characters in the buffer.
   --  If Buffer_Size is 0, there is no limit (ie all the characters are kept
   --  till Expect matches), but this is slower.
   --
   --  If Err_To_Out is True, then the standard error of the spawned process is
   --  connected to the standard output. This is the only way to get the
   --  Except subprograms also match on output on standard error.
   --
   --  Invalid_Process is raised if the process could not be spawned.

   procedure Close (Descriptor : in out Process_Descriptor);
   --  Terminate the process and close the pipes to it. It implicitly
   --  does the 'wait' command required to clean up the process table.
   --  This also frees the buffer associated with the process id. Raise
   --  Invalid_Process if the process id is invalid.

   procedure Close
     (Descriptor : in out Process_Descriptor;
      Status     : out Integer);
   --  Same as above, but also returns the exit status of the process, as set
   --  for example by the procedure GNAT.OS_Lib.OS_Exit.

   procedure Send_Signal
     (Descriptor : Process_Descriptor;
      Signal     : Integer);
   --  Send a given signal to the process. Raise Invalid_Process if the process
   --  id is invalid.

   procedure Interrupt (Descriptor : in out Process_Descriptor);
   --  Interrupt the process (the equivalent of Ctrl-C on unix and windows)
   --  and call close if the process dies.

   function Get_Input_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor;
   --  Return the input file descriptor associated with Descriptor

   function Get_Output_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor;
   --  Return the output file descriptor associated with Descriptor

   function Get_Error_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor;
   --  Return the error output file descriptor associated with Descriptor

   function Get_Pid
     (Descriptor : Process_Descriptor) return Process_Id;
   --  Return the process id assocated with a given process descriptor

   function Get_Command_Output
     (Command    : String;
      Arguments  : GNAT.OS_Lib.Argument_List;
      Input      : String;
      Status     : not null access Integer;
      Err_To_Out : Boolean := False) return String;
   --  Execute Command with the specified Arguments and Input, and return the
   --  generated standard output data as a single string. If Err_To_Out is
   --  True, generated standard error output is included as well. On return,
   --  Status is set to the command's exit status.

   --------------------
   -- Adding filters --
   --------------------

   --  This is a rather low-level interface to subprocesses, since basically
   --  the filtering is left entirely to the user. See the Expect subprograms
   --  below for higher level functions.

   type Filter_Function is access
     procedure
       (Descriptor : Process_Descriptor'Class;
        Str        : String;
        User_Data  : System.Address := System.Null_Address);
   --  Function called every time new characters are read from or written
   --  to the process.
   --
   --  Str is a string of all these characters.
   --
   --  User_Data, if specified, is a user specific data that will be passed to
   --  the filter. Note that no checks are done on this parameter that should
   --  be used with cautiousness.

   procedure Add_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function;
      Filter_On  : Filter_Type := Output;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False);
   --  Add a new filter for one of the filter type. This filter will be
   --  run before all the existing filters, unless After is set True,
   --  in which case it will be run after existing filters. User_Data
   --  is passed as is to the filter procedure.

   procedure Remove_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function);
   --  Remove a filter from the list of filters (whatever the type of the
   --  filter).

   procedure Trace_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Function that can be used a filter and that simply outputs Str on
   --  Standard_Output. This is mainly used for debugging purposes.
   --  User_Data is ignored.

   procedure Lock_Filters (Descriptor : in out Process_Descriptor);
   --  Temporarily disables all output and input filters. They will be
   --  reactivated only when Unlock_Filters has been called as many times as
   --  Lock_Filters;

   procedure Unlock_Filters (Descriptor : in out Process_Descriptor);
   --  Unlocks the filters. They are reactivated only if Unlock_Filters
   --  has been called as many times as Lock_Filters.

   ------------------
   -- Sending data --
   ------------------

   procedure Send
     (Descriptor   : in out Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False);
   --  Send a string to the file descriptor.
   --
   --  The string is not formatted in any way, except if Add_LF is True,
   --  in which case an ASCII.LF is added at the end, so that Str is
   --  recognized as a command by the external process.
   --
   --  If Empty_Buffer is True, any input waiting from the process (or in the
   --  buffer) is first discarded before the command is sent. The output
   --  filters are of course called as usual.

   -----------------------------------------------------------
   -- Working on the output (single process, simple regexp) --
   -----------------------------------------------------------

   type Expect_Match is new Integer;
   Expect_Full_Buffer : constant Expect_Match := -1;
   --  If the buffer was full and some characters were discarded

   Expect_Timeout : constant Expect_Match := -2;
   --  If not output matching the regexps was found before the timeout

   function "+" (S : String) return GNAT.OS_Lib.String_Access;
   --  Allocate some memory for the string. This is merely a convenience
   --  function to help create the array of regexps in the call to Expect.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Wait till a string matching Fd can be read from Fd, and return 1
   --  if a match was found.
   --
   --  It consumes all the characters read from Fd until a match found, and
   --  then sets the return values for the subprograms Expect_Out and
   --  Expect_Out_Match.
   --
   --  The empty string "" will never match, and can be used if you only want
   --  to match after a specific timeout. Beware that if Timeout is -1 at the
   --  time, the current task will be blocked forever.
   --
   --  This command times out after Timeout milliseconds (or never if Timeout
   --  is -1). In that case, Expect_Timeout is returned. The value returned by
   --  Expect_Out and Expect_Out_Match are meaningless in that case.
   --
   --  Note that using a timeout of 0ms leads to unpredictable behavior, since
   --  the result depends on whether the process has already sent some output
   --  the first time Expect checks, and this depends on the operating system.
   --
   --  The regular expression must obey the syntax described in GNAT.Regpat.
   --
   --  If Full_Buffer is True, then Expect will match if the buffer was too
   --  small and some characters were about to be discarded. In that case,
   --  Expect_Full_Buffer is returned.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as the previous one, but with a precompiled regular expression.
   --  This is more efficient however, especially if you are using this
   --  expression multiple times, since this package won't need to recompile
   --  the regexp every time.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, but it is now possible to get the indexes of the
   --  substrings for the parentheses in the regexp (see the example at the
   --  top of this package, as well as the documentation in the package
   --  GNAT.Regpat).
   --
   --  Matched'First should be 0, and this index will contain the indexes for
   --  the whole string that was matched. The index 1 will contain the indexes
   --  for the first parentheses-pair, and so on.

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, but with a precompiled regular expression

   -------------------------------------------------------------
   -- Working on the output (single process, multiple regexp) --
   -------------------------------------------------------------

   type Regexp_Array is array (Positive range <>) of GNAT.OS_Lib.String_Access;

   type Pattern_Matcher_Access is access all GNAT.Regpat.Pattern_Matcher;
   type Compiled_Regexp_Array is array (Positive range <>)
     of Pattern_Matcher_Access;

   function "+"
     (P    : GNAT.Regpat.Pattern_Matcher)
      return Pattern_Matcher_Access;
   --  Allocate some memory for the pattern matcher.
   --  This is only a convenience function to help create the array of
   --  compiled regular expressoins.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Wait till a string matching one of the regular expressions in Regexps
   --  is found. This function returns the index of the regexp that matched.
   --  This command is blocking, but will timeout after Timeout milliseconds.
   --  In that case, Timeout is returned.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as the previous one, but with precompiled regular expressions.
   --  This can be much faster if you are using them multiple times.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, except that you can also access the parenthesis
   --  groups inside the matching regular expression.
   --  The first index in Matched must be 0, or Constraint_Error will be
   --  raised. The index 0 contains the indexes for the whole string that was
   --  matched, the index 1 contains the indexes for the first parentheses
   --  pair, and so on.

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, but with precompiled regular expressions.
   --  The first index in Matched must be 0, or Constraint_Error will be
   --  raised.

   -------------------------------------------
   -- Working on the output (multi-process) --
   -------------------------------------------

   type Multiprocess_Regexp is record
      Descriptor : Process_Descriptor_Access;
      Regexp     : Pattern_Matcher_Access;
   end record;
   type Multiprocess_Regexp_Array is array (Positive range <>)
     of Multiprocess_Regexp;

   procedure Expect
     (Result      : out Expect_Match;
      Regexps     : Multiprocess_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as above, but for multi processes

   procedure Expect
     (Result      : out Expect_Match;
      Regexps     : Multiprocess_Regexp_Array;
      Timeout     : Integer := 10000;
      Full_Buffer : Boolean := False);
   --  Same as the previous one, but for multiple processes.
   --  This procedure finds the first regexp that match the associated process.

   ------------------------
   -- Getting the output --
   ------------------------

   procedure Flush
     (Descriptor : in out Process_Descriptor;
      Timeout    : Integer := 0);
   --  Discard all output waiting from the process.
   --
   --  This output is simply discarded, and no filter is called. This output
   --  will also not be visible by the next call to Expect, nor will any
   --  output currently buffered.
   --
   --  Timeout is the delay for which we wait for output to be available from
   --  the process. If 0, we only get what is immediately available.

   function Expect_Out (Descriptor : Process_Descriptor) return String;
   --  Return the string matched by the last Expect call.
   --
   --  The returned string is in fact the concatenation of all the strings
   --  read from the file descriptor up to, and including, the characters
   --  that matched the regular expression.
   --
   --  For instance, with an input "philosophic", and a regular expression
   --  "hi" in the call to expect, the strings returned the first and second
   --  time would be respectively "phi" and "losophi".

   function Expect_Out_Match (Descriptor : Process_Descriptor) return String;
   --  Return the string matched by the last Expect call.
   --
   --  The returned string includes only the character that matched the
   --  specific regular expression. All the characters that came before are
   --  simply discarded.
   --
   --  For instance, with an input "philosophic", and a regular expression
   --  "hi" in the call to expect, the strings returned the first and second
   --  time would both be "hi".

   ----------------
   -- Exceptions --
   ----------------

   Invalid_Process : exception;
   --  Raised by most subprograms above when the parameter Descriptor is not a
   --  valid process or is a closed process.

   Process_Died : exception;
   --  Raised by all the expect subprograms if Descriptor was originally a
   --  valid process that died while Expect was executing. It is also raised
   --  when Expect receives an end-of-file.

private
   type Filter_List_Elem;
   type Filter_List is access Filter_List_Elem;
   type Filter_List_Elem is record
      Filter    : Filter_Function;
      User_Data : System.Address;
      Filter_On : Filter_Type;
      Next      : Filter_List;
   end record;

   type Pipe_Type is record
      Input, Output : GNAT.OS_Lib.File_Descriptor;
   end record;
   --  This type represents a pipe, used to communicate between two processes

   procedure Set_Up_Communications
     (Pid        : in out Process_Descriptor;
      Err_To_Out : Boolean;
      Pipe1      : not null access Pipe_Type;
      Pipe2      : not null access Pipe_Type;
      Pipe3      : not null access Pipe_Type);
   --  Set up all the communication pipes and file descriptors prior to
   --  spawning the child process.

   procedure Set_Up_Parent_Communications
     (Pid   : in out Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type);
   --  Finish the set up of the pipes while in the parent process

   procedure Set_Up_Child_Communications
     (Pid   : in out Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type;
      Cmd   : String;
      Args  : System.Address);
   --  Finish the set up of the pipes while in the child process
   --  This also spawns the child process (based on Cmd).
   --  On systems that support fork, this procedure is executed inside the
   --  newly created process.

   type Process_Descriptor is tagged record
      Pid              : aliased Process_Id := Invalid_Pid;
      Input_Fd         : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Output_Fd        : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Error_Fd         : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Filters_Lock     : Integer := 0;

      Filters          : Filter_List := null;

      Buffer           : GNAT.OS_Lib.String_Access := null;
      Buffer_Size      : Natural := 0;
      Buffer_Index     : Natural := 0;

      Last_Match_Start : Natural := 0;
      Last_Match_End   : Natural := 0;
   end record;

   --  The following subprogram is provided for use in the body, and also
   --  possibly in future child units providing extensions to this package.

   procedure Portable_Execvp
     (Pid  : not null access Process_Id;
      Cmd  : String;
      Args : System.Address);
   pragma Import (C, Portable_Execvp, "__gnat_expect_portable_execvp");
   --  Executes, in a portable way, the command Cmd (full path must be
   --  specified), with the given Args. Args must be an array of string
   --  pointers. Note that the first element in Args must be the executable
   --  name, and the last element must be a null pointer. The returned value
   --  in Pid is the process ID, or zero if not supported on the platform.

end GNAT.Expect;
