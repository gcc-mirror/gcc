------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                          G N A T . E X P E C T                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2024, AdaCore                     --
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

with System;              use System;
with System.OS_Constants; use System.OS_Constants;
with Ada.Calendar;        use Ada.Calendar;

with GNAT.IO;      use GNAT.IO;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GNAT.Regpat;  use GNAT.Regpat;

with Ada.Unchecked_Deallocation;

package body GNAT.Expect is

   type Array_Of_Pd is array (Positive range <>) of Process_Descriptor_Access;

   Expect_Process_Died   : constant Expect_Match := -100;
   Expect_Internal_Error : constant Expect_Match := -101;
   --  Additional possible outputs of Expect_Internal. These are not visible in
   --  the spec because the user will never see them.

   procedure Expect_Internal
     (Descriptors : in out Array_Of_Pd;
      Result      : out Expect_Match;
      Timeout     : Integer;
      Full_Buffer : Boolean);
   --  Internal function used to read from the process Descriptor.
   --
   --  Several outputs are possible:
   --     Result=Expect_Timeout, if no output was available before the timeout
   --        expired.
   --     Result=Expect_Full_Buffer, if Full_Buffer is True and some characters
   --        had to be discarded from the internal buffer of Descriptor.
   --     Result=Express_Process_Died if one of the processes was terminated.
   --        That process's Input_Fd is set to Invalid_FD
   --     Result=Express_Internal_Error
   --     Result=<integer>, indicates how many characters were added to the
   --        internal buffer. These characters are from indexes
   --        Descriptor.Buffer_Index - Result + 1 .. Descriptor.Buffer_Index
   --  Process_Died is raised if the process is no longer valid.

   procedure Reinitialize_Buffer
     (Descriptor : in out Process_Descriptor'Class);
   --  Reinitialize the internal buffer.
   --  The buffer is deleted up to the end of the last match.

   procedure Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Filter_List_Elem, Filter_List);

   procedure Call_Filters
     (Pid       : Process_Descriptor'Class;
      Str       : String;
      Filter_On : Filter_Type);
   --  Call all the filters that have the appropriate type.
   --  This function does nothing if the filters are locked

   ------------------------------
   -- Target dependent section --
   ------------------------------

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup);

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);
   pragma Import (C, Dup2);

   procedure Kill (Pid : Process_Id; Sig_Num : Integer);
   pragma Import (C, Kill, "__gnat_kill");
   --  if Close is set to 1 all OS resources used by the Pid must be freed

   function Create_Pipe (Pipe : not null access Pipe_Type) return Integer;
   pragma Import (C, Create_Pipe, "__gnat_pipe");

   function Poll
     (Fds          : System.Address;
      Num_Fds      : Integer;
      Timeout      : Integer;
      Dead_Process : access Integer;
      Is_Set       : System.Address) return Integer;
   pragma Import (C, Poll, "__gnat_expect_poll");
   --  Check whether there is any data waiting on the file descriptors
   --  Fds, and wait if there is none, at most Timeout milliseconds
   --  Returns -1 in case of error, 0 if the timeout expired before
   --  data became available.
   --
   --  Is_Set is an array of the same size as FDs and elements are set to 1 if
   --  data is available for the corresponding File Descriptor, 0 otherwise.
   --
   --  If a process dies, then Dead_Process is set to the index of the
   --  corresponding file descriptor.

   function Waitpid (Pid : Process_Id) return Integer;
   pragma Import (C, Waitpid, "__gnat_waitpid");
   --  Wait for a specific process id, and return its exit code

   ---------
   -- "+" --
   ---------

   function "+" (S : String) return GNAT.OS_Lib.String_Access is
   begin
      return new String'(S);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
     (P : GNAT.Regpat.Pattern_Matcher) return Pattern_Matcher_Access
   is
   begin
      return new GNAT.Regpat.Pattern_Matcher'(P);
   end "+";

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function;
      Filter_On  : Filter_Type := Output;
      User_Data  : System.Address := System.Null_Address;
      After      : Boolean := False)
   is
      Current : Filter_List := Descriptor.Filters;

   begin
      if After then
         while Current /= null and then Current.Next /= null loop
            Current := Current.Next;
         end loop;

         if Current = null then
            Descriptor.Filters :=
              new Filter_List_Elem'
               (Filter => Filter, Filter_On => Filter_On,
                User_Data => User_Data, Next => null);
         else
            Current.Next :=
              new Filter_List_Elem'
              (Filter => Filter, Filter_On => Filter_On,
               User_Data => User_Data, Next => null);
         end if;

      else
         Descriptor.Filters :=
           new Filter_List_Elem'
             (Filter => Filter, Filter_On => Filter_On,
              User_Data => User_Data, Next => Descriptor.Filters);
      end if;
   end Add_Filter;

   ------------------
   -- Call_Filters --
   ------------------

   procedure Call_Filters
     (Pid       : Process_Descriptor'Class;
      Str       : String;
      Filter_On : Filter_Type)
   is
      Current_Filter : Filter_List;

   begin
      if Pid.Filters_Lock = 0 then
         Current_Filter := Pid.Filters;

         while Current_Filter /= null loop
            if Current_Filter.Filter_On = Filter_On then
               Current_Filter.Filter
                 (Pid, Str, Current_Filter.User_Data);
            end if;

            Current_Filter := Current_Filter.Next;
         end loop;
      end if;
   end Call_Filters;

   -----------
   -- Close --
   -----------

   procedure Close
     (Descriptor : in out Process_Descriptor;
      Status     : out Integer)
   is
      Current_Filter : Filter_List;
      Next_Filter    : Filter_List;

   begin
      if Descriptor.Pid > 0 then  --  see comment in Send_Signal
         Kill (Descriptor.Pid, Sig_Num => 9);
      end if;

      Close_Input (Descriptor);

      if Descriptor.Error_Fd /= Descriptor.Output_Fd
        and then Descriptor.Error_Fd /= Invalid_FD
      then
         Close (Descriptor.Error_Fd);
      end if;

      if Descriptor.Output_Fd /= Invalid_FD then
         Close (Descriptor.Output_Fd);
      end if;

      GNAT.OS_Lib.Free (Descriptor.Buffer);
      Descriptor.Buffer_Size := 0;

      Current_Filter := Descriptor.Filters;

      while Current_Filter /= null loop
         Next_Filter := Current_Filter.Next;
         Free (Current_Filter);
         Current_Filter := Next_Filter;
      end loop;

      Descriptor.Filters := null;

      --  Check process id (see comment in Send_Signal)

      if Descriptor.Pid > 0 then
         Status := Waitpid (Descriptor.Pid);
      else
         raise Invalid_Process;
      end if;
   end Close;

   procedure Close (Descriptor : in out Process_Descriptor) is
      Status : Integer;
   begin
      Close (Descriptor, Status);
   end Close;

   -----------------
   -- Close_Input --
   -----------------

   procedure Close_Input (Pid : in out Process_Descriptor) is
   begin
      if Pid.Input_Fd /= Invalid_FD then
         Close (Pid.Input_Fd);
      end if;

      if Pid.Output_Fd = Pid.Input_Fd then
         Pid.Output_Fd := Invalid_FD;
      end if;

      if Pid.Error_Fd = Pid.Input_Fd then
         Pid.Error_Fd := Invalid_FD;
      end if;

      Pid.Input_Fd := Invalid_FD;
   end Close_Input;

   ------------
   -- Expect --
   ------------

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
   begin
      if Regexp = "" then
         Expect (Descriptor, Result, Never_Match, Timeout, Full_Buffer);
      else
         Expect (Descriptor, Result, Compile (Regexp), Timeout, Full_Buffer);
      end if;
   end Expect;

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : String;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
   begin
      pragma Assert (Matched'First = 0);
      if Regexp = "" then
         Expect
           (Descriptor, Result, Never_Match, Matched, Timeout, Full_Buffer);
      else
         Expect
           (Descriptor, Result, Compile (Regexp), Matched, Timeout,
            Full_Buffer);
      end if;
   end Expect;

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      pragma Warnings (Off, Matched);
   begin
      Expect (Descriptor, Result, Regexp, Matched, Timeout, Full_Buffer);
   end Expect;

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexp      : GNAT.Regpat.Pattern_Matcher;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
      N           : Expect_Match;
      Descriptors : Array_Of_Pd := [Descriptor'Unrestricted_Access];
      Try_Until   : constant Time := Clock + Duration (Timeout) / 1000.0;
      Timeout_Tmp : Integer := Timeout;

   begin
      pragma Assert (Matched'First = 0);
      Reinitialize_Buffer (Descriptor);

      loop
         --  First, test if what is already in the buffer matches (This is
         --  required if this package is used in multi-task mode, since one of
         --  the tasks might have added something in the buffer, and we don't
         --  want other tasks to wait for new input to be available before
         --  checking the regexps).

         Match
           (Regexp, Descriptor.Buffer (1 .. Descriptor.Buffer_Index), Matched);

         if Descriptor.Buffer_Index >= 1 and then Matched (0).First /= 0 then
            Result := 1;
            Descriptor.Last_Match_Start := Matched (0).First;
            Descriptor.Last_Match_End := Matched (0).Last;
            return;
         end if;

         --  Else try to read new input

         Expect_Internal (Descriptors, N, Timeout_Tmp, Full_Buffer);

         case N is
            when Expect_Internal_Error
               | Expect_Process_Died
            =>
               raise Process_Died;

            when Expect_Full_Buffer
               | Expect_Timeout
            =>
               Result := N;
               return;

            when others =>
               null;  --  See below
         end case;

         --  Calculate the timeout for the next turn

         --  Note that Timeout is, from the caller's perspective, the maximum
         --  time until a match, not the maximum time until some output is
         --  read, and thus cannot be reused as is for Expect_Internal.

         if Timeout /= -1 then
            Timeout_Tmp := Integer (Try_Until - Clock) * 1000;

            if Timeout_Tmp < 0 then
               Result := Expect_Timeout;
               exit;
            end if;
         end if;
      end loop;

      --  Even if we had the general timeout above, we have to test that the
      --  last test we read from the external process didn't match.

      Match
        (Regexp, Descriptor.Buffer (1 .. Descriptor.Buffer_Index), Matched);

      if Matched (0).First /= 0 then
         Result := 1;
         Descriptor.Last_Match_Start := Matched (0).First;
         Descriptor.Last_Match_End := Matched (0).Last;
         return;
      end if;
   end Expect;

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
      Patterns : Compiled_Regexp_Array (Regexps'Range);

      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      pragma Warnings (Off, Matched);

   begin
      for J in Regexps'Range loop
         Patterns (J) := new Pattern_Matcher'(Compile (Regexps (J).all));
      end loop;

      Expect (Descriptor, Result, Patterns, Matched, Timeout, Full_Buffer);

      for J in Regexps'Range loop
         Free (Patterns (J));
      end loop;
   end Expect;

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      pragma Warnings (Off, Matched);
   begin
      Expect (Descriptor, Result, Regexps, Matched, Timeout, Full_Buffer);
   end Expect;

   procedure Expect
     (Result      : out Expect_Match;
      Regexps     : Multiprocess_Regexp_Array;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      pragma Warnings (Off, Matched);
   begin
      Expect (Result, Regexps, Matched, Timeout, Full_Buffer);
   end Expect;

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
      Patterns : Compiled_Regexp_Array (Regexps'Range);

   begin
      pragma Assert (Matched'First = 0);

      for J in Regexps'Range loop
         Patterns (J) := new Pattern_Matcher'(Compile (Regexps (J).all));
      end loop;

      Expect (Descriptor, Result, Patterns, Matched, Timeout, Full_Buffer);

      for J in Regexps'Range loop
         Free (Patterns (J));
      end loop;
   end Expect;

   procedure Expect
     (Descriptor  : in out Process_Descriptor;
      Result      : out Expect_Match;
      Regexps     : Compiled_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
      N           : Expect_Match;
      Descriptors : Array_Of_Pd := [Descriptor'Unrestricted_Access];

   begin
      pragma Assert (Matched'First = 0);

      Reinitialize_Buffer (Descriptor);

      loop
         --  First, test if what is already in the buffer matches (This is
         --  required if this package is used in multi-task mode, since one of
         --  the tasks might have added something in the buffer, and we don't
         --  want other tasks to wait for new input to be available before
         --  checking the regexps).

         if Descriptor.Buffer /= null then
            for J in Regexps'Range loop
               Match
                 (Regexps (J).all,
                  Descriptor.Buffer (1 .. Descriptor.Buffer_Index),
                  Matched);

               if Matched (0) /= No_Match then
                  Result := Expect_Match (J);
                  Descriptor.Last_Match_Start := Matched (0).First;
                  Descriptor.Last_Match_End := Matched (0).Last;
                  return;
               end if;
            end loop;
         end if;

         Expect_Internal (Descriptors, N, Timeout, Full_Buffer);

         case N is
            when Expect_Internal_Error
               | Expect_Process_Died
            =>
               raise Process_Died;

            when Expect_Full_Buffer
               | Expect_Timeout
            =>
               Result := N;
               return;

            when others =>
               null;  --  Continue
         end case;
      end loop;
   end Expect;

   procedure Expect
     (Result      : out Expect_Match;
      Regexps     : Multiprocess_Regexp_Array;
      Matched     : out GNAT.Regpat.Match_Array;
      Timeout     : Integer := 10_000;
      Full_Buffer : Boolean := False)
   is
      N           : Expect_Match;
      Descriptors : Array_Of_Pd (Regexps'Range);

   begin
      pragma Assert (Matched'First = 0);

      for J in Descriptors'Range loop
         Descriptors (J) := Regexps (J).Descriptor;

         if Descriptors (J) /= null then
            Reinitialize_Buffer (Regexps (J).Descriptor.all);
         end if;
      end loop;

      loop
         --  First, test if what is already in the buffer matches (This is
         --  required if this package is used in multi-task mode, since one of
         --  the tasks might have added something in the buffer, and we don't
         --  want other tasks to wait for new input to be available before
         --  checking the regexps).

         for J in Regexps'Range loop
            if Regexps (J).Regexp /= null
               and then Regexps (J).Descriptor /= null
            then
               Match (Regexps (J).Regexp.all,
                      Regexps (J).Descriptor.Buffer
                        (1 .. Regexps (J).Descriptor.Buffer_Index),
                      Matched);

               if Matched (0) /= No_Match then
                  Result := Expect_Match (J);
                  Regexps (J).Descriptor.Last_Match_Start := Matched (0).First;
                  Regexps (J).Descriptor.Last_Match_End := Matched (0).Last;
                  return;
               end if;
            end if;
         end loop;

         Expect_Internal (Descriptors, N, Timeout, Full_Buffer);

         case N is
            when Expect_Internal_Error
               | Expect_Process_Died
            =>
               raise Process_Died;

            when Expect_Full_Buffer
               | Expect_Timeout
            =>
               Result := N;
               return;

            when others =>
               null;  --  Continue
         end case;
      end loop;
   end Expect;

   ---------------------
   -- Expect_Internal --
   ---------------------

   procedure Expect_Internal
     (Descriptors : in out Array_Of_Pd;
      Result      : out Expect_Match;
      Timeout     : Integer;
      Full_Buffer : Boolean)
   is
      Num_Descriptors : Integer;
      Buffer_Size     : Integer := 0;

      N : Integer;

      type File_Descriptor_Array is
        array (0 .. Descriptors'Length - 1) of File_Descriptor;
      Fds : aliased File_Descriptor_Array;
      Fds_Count : Natural := 0;

      Fds_To_Descriptor : array (Fds'Range) of Integer;
      --  Maps file descriptor entries from Fds to entries in Descriptors.
      --  They do not have the same index when entries in Descriptors are null.

      type Integer_Array is array (Fds'Range) of Integer;
      Is_Set : aliased Integer_Array;

   begin
      for J in Descriptors'Range loop
         if Descriptors (J) /= null
           and then Descriptors (J).Output_Fd /= Invalid_FD
         then
            Fds (Fds'First + Fds_Count) := Descriptors (J).Output_Fd;
            Fds_To_Descriptor (Fds'First + Fds_Count) := J;
            Fds_Count := Fds_Count + 1;

            if Descriptors (J).Buffer_Size = 0 then
               Buffer_Size := Integer'Max (Buffer_Size, 4096);
            else
               Buffer_Size :=
                 Integer'Max (Buffer_Size, Descriptors (J).Buffer_Size);
            end if;
         end if;
      end loop;

      if Fds_Count = 0 then
         --  There are no descriptors to monitor, it means that process died.

         Result := Expect_Process_Died;

         return;
      end if;

      declare
         Buffer : aliased String (1 .. Buffer_Size);
         --  Buffer used for input. This is allocated only once, not for
         --  every iteration of the loop

         D : aliased Integer;
         --  Index in Descriptors

      begin
         --  Loop until we match or we have a timeout

         loop
            --  Poll may be interrupted on Linux by a signal and need to be
            --  repeated. We don't want to check for errno = EINTER, so just
            --  attempt to call Poll a few times.

            for J in 1 .. 3 loop
               Num_Descriptors :=
                 Poll
                   (Fds'Address, Fds_Count, Timeout, D'Access, Is_Set'Address);

               exit when Num_Descriptors /= -1;
            end loop;

            case Num_Descriptors is

               --  Error?

               when -1 =>
                  Result := Expect_Internal_Error;

                  if D /= 0 then
                     Close_Input (Descriptors (D).all);
                  end if;

                  return;

               --  Timeout?

               when 0  =>
                  Result := Expect_Timeout;
                  return;

               --  Some input

               when others =>
                  for F in Fds'Range loop
                     if Is_Set (F) = 1 then
                        D := Fds_To_Descriptor (F);

                        Buffer_Size := Descriptors (D).Buffer_Size;

                        if Buffer_Size = 0 then
                           Buffer_Size := 4096;
                        end if;

                        --  Read may be interrupted on Linux by a signal and
                        --  need to be repeated. We don't want to check for
                        --  errno = EINTER, so just attempt to read a few
                        --  times.

                        for J in 1 .. 3 loop
                           N := Read (Descriptors (D).Output_Fd,
                                      Buffer'Address, Buffer_Size);

                           exit when N > 0;
                        end loop;

                        --  Error or End of file

                        if N <= 0 then
                           Close_Input (Descriptors (D).all);
                           Result := Expect_Process_Died;

                           return;

                        else
                           --  If there is no limit to the buffer size

                           if Descriptors (D).Buffer_Size = 0 then
                              declare
                                 Tmp : String_Access := Descriptors (D).Buffer;

                              begin
                                 if Tmp /= null then
                                    Descriptors (D).Buffer :=
                                      new String (1 .. Tmp'Length + N);
                                    Descriptors (D).Buffer (1 .. Tmp'Length) :=
                                      Tmp.all;
                                    Descriptors (D).Buffer
                                      (Tmp'Length + 1 .. Tmp'Length + N) :=
                                      Buffer (1 .. N);
                                    Free (Tmp);
                                    Descriptors (D).Buffer_Index :=
                                      Descriptors (D).Buffer'Last;

                                 else
                                    Descriptors (D).Buffer :=
                                      new String (1 .. N);
                                    Descriptors (D).Buffer.all :=
                                      Buffer (1 .. N);
                                    Descriptors (D).Buffer_Index := N;
                                 end if;
                              end;

                           else
                              --  Add what we read to the buffer

                              if Descriptors (D).Buffer_Index + N >
                                   Descriptors (D).Buffer_Size
                              then
                                 --  If the user wants to know when we have
                                 --  read more than the buffer can contain.

                                 if Full_Buffer then
                                    Result := Expect_Full_Buffer;
                                    return;
                                 end if;

                                 --  Keep as much as possible from the buffer,
                                 --  and forget old characters.

                                 Descriptors (D).Buffer
                                   (1 .. Descriptors (D).Buffer_Size - N) :=
                                  Descriptors (D).Buffer
                                   (N - Descriptors (D).Buffer_Size +
                                    Descriptors (D).Buffer_Index + 1 ..
                                    Descriptors (D).Buffer_Index);
                                 Descriptors (D).Buffer_Index :=
                                   Descriptors (D).Buffer_Size - N;
                              end if;

                              --  Keep what we read in the buffer

                              Descriptors (D).Buffer
                                (Descriptors (D).Buffer_Index + 1 ..
                                 Descriptors (D).Buffer_Index + N) :=
                                Buffer (1 .. N);
                              Descriptors (D).Buffer_Index :=
                                Descriptors (D).Buffer_Index + N;
                           end if;

                           --  Call each of the output filter with what we
                           --  read.

                           Call_Filters
                             (Descriptors (D).all, Buffer (1 .. N), Output);

                           Result := Expect_Match (D);
                           return;
                        end if;
                     end if;
                  end loop;
            end case;
         end loop;
      end;
   end Expect_Internal;

   ----------------
   -- Expect_Out --
   ----------------

   function Expect_Out (Descriptor : Process_Descriptor) return String is
   begin
      return Descriptor.Buffer (1 .. Descriptor.Last_Match_End);
   end Expect_Out;

   ----------------------
   -- Expect_Out_Match --
   ----------------------

   function Expect_Out_Match (Descriptor : Process_Descriptor) return String is
   begin
      return Descriptor.Buffer
        (Descriptor.Last_Match_Start .. Descriptor.Last_Match_End);
   end Expect_Out_Match;

   ------------------------
   -- First_Dead_Process --
   ------------------------

   function First_Dead_Process
     (Regexp : Multiprocess_Regexp_Array) return Natural is
   begin
      for R in Regexp'Range loop
         if Regexp (R).Descriptor /= null
           and then Regexp (R).Descriptor.Input_Fd = GNAT.OS_Lib.Invalid_FD
         then
            return R;
         end if;
      end loop;

      return 0;
   end First_Dead_Process;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Descriptor : in out Process_Descriptor;
      Timeout    : Integer := 0)
   is
      Buffer_Size     : constant Integer := 8192;
      Num_Descriptors : Integer;
      N               : aliased Integer;
      Is_Set          : aliased Integer;
      Buffer          : aliased String (1 .. Buffer_Size);

   begin
      --  Empty the current buffer

      Descriptor.Last_Match_End := Descriptor.Buffer_Index;
      Reinitialize_Buffer (Descriptor);

      --  Read everything from the process to flush its output

      loop
         Num_Descriptors :=
           Poll (Descriptor.Output_Fd'Address,
                 1,
                 Timeout,
                 N'Access,
                 Is_Set'Address);

         case Num_Descriptors is

            --  Error ?

            when -1 =>
               raise Process_Died;

            --  Timeout => End of flush

            when 0  =>
               return;

            --  Some input

            when others =>
               if Is_Set = 1 then
                  N := Read (Descriptor.Output_Fd, Buffer'Address,
                             Buffer_Size);

                  if N = -1 then
                     raise Process_Died;
                  elsif N = 0 then
                     return;
                  end if;
               end if;
         end case;
      end loop;
   end Flush;

   ----------
   -- Free --
   ----------

   procedure Free (Regexp : in out Multiprocess_Regexp) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Process_Descriptor'Class, Process_Descriptor_Access);
   begin
      Unchecked_Free (Regexp.Descriptor);
      Free (Regexp.Regexp);
   end Free;

   ------------------------
   -- Get_Command_Output --
   ------------------------

   function Get_Command_Output
     (Command    : String;
      Arguments  : GNAT.OS_Lib.Argument_List;
      Input      : String;
      Status     : not null access Integer;
      Err_To_Out : Boolean := False) return String
   is
      Process : Process_Descriptor;

      Output : String_Access := new String (1 .. 1024);
      --  Buffer used to accumulate standard output from the launched
      --  command, expanded as necessary during execution.

      Last : Integer := 0;
      --  Index of the last used character within Output

   begin
      Non_Blocking_Spawn
        (Process, Command, Arguments, Err_To_Out => Err_To_Out,
         Buffer_Size => 0);

      if Input'Length > 0 then
         Send (Process, Input);
      end if;

      Close_Input (Process);

      declare
         Result : Expect_Match;

      begin
         --  This loop runs until the call to Expect raises Process_Died

         loop
            Expect (Process, Result, ".+", Timeout => -1);

            declare
               NOutput : String_Access;
               S       : constant String := Expect_Out (Process);
               pragma Assert (S'Length > 0);

            begin
               --  Expand buffer if we need more space. Note here that we add
               --  S'Length to ensure that S will fit in the new buffer size.

               if Last + S'Length > Output'Last then
                  NOutput := new String (1 .. 2 * Output'Last + S'Length);
                  NOutput (Output'Range) := Output.all;
                  Free (Output);

               --  Here if current buffer size is OK

               else
                  NOutput := Output;
               end if;

               NOutput (Last + 1 .. Last + S'Length) := S;
               Last := Last + S'Length;
               Output := NOutput;
            end;
         end loop;

      exception
         when Process_Died =>
            Close (Process, Status.all);
      end;

      if Last = 0 then
         Free (Output);
         return "";
      end if;

      declare
         S : constant String := Output (1 .. Last);
      begin
         Free (Output);
         return S;
      end;
   end Get_Command_Output;

   ------------------
   -- Get_Error_Fd --
   ------------------

   function Get_Error_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor
   is
   begin
      return Descriptor.Error_Fd;
   end Get_Error_Fd;

   ------------------
   -- Get_Input_Fd --
   ------------------

   function Get_Input_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor
   is
   begin
      return Descriptor.Input_Fd;
   end Get_Input_Fd;

   -------------------
   -- Get_Output_Fd --
   -------------------

   function Get_Output_Fd
     (Descriptor : Process_Descriptor) return GNAT.OS_Lib.File_Descriptor
   is
   begin
      return Descriptor.Output_Fd;
   end Get_Output_Fd;

   -------------
   -- Get_Pid --
   -------------

   function Get_Pid
     (Descriptor : Process_Descriptor) return Process_Id
   is
   begin
      return Descriptor.Pid;
   end Get_Pid;

   -----------------
   -- Has_Process --
   -----------------

   function Has_Process (Regexp : Multiprocess_Regexp_Array) return Boolean is
   begin
      return Regexp /= [Regexp'Range => (null, null)];
   end Has_Process;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Descriptor : in out Process_Descriptor) is
      SIGINT : constant := 2;
   begin
      Send_Signal (Descriptor, SIGINT);
   end Interrupt;

   ------------------
   -- Lock_Filters --
   ------------------

   procedure Lock_Filters (Descriptor : in out Process_Descriptor) is
   begin
      Descriptor.Filters_Lock := Descriptor.Filters_Lock + 1;
   end Lock_Filters;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   procedure Non_Blocking_Spawn
     (Descriptor  : out Process_Descriptor'Class;
      Command     : String;
      Args        : GNAT.OS_Lib.Argument_List;
      Buffer_Size : Natural := 4096;
      Err_To_Out  : Boolean := False)
   is
      function Fork return Process_Id;
      pragma Import (C, Fork, "__gnat_expect_fork");
      --  Starts a new process if possible. See the Unix command fork for more
      --  information. On systems that do not support this capability (such as
      --  Windows...), this command does nothing, and Fork will return
      --  Null_Pid.

      Pipe1, Pipe2, Pipe3 : aliased Pipe_Type;

      Arg        : String_Access;
      Arg_List   : String_List (1 .. Args'Length + 2);
      C_Arg_List : aliased array (1 .. Args'Length + 2) of System.Address;

      Command_With_Path : String_Access;

   begin
      Command_With_Path := Locate_Exec_On_Path (Command);

      if Command_With_Path = null then
         raise Invalid_Process;
      end if;

      --  Create the rest of the pipes once we know we will be able to
      --  execute the process.

      Set_Up_Communications
        (Descriptor, Err_To_Out, Pipe1'Access, Pipe2'Access, Pipe3'Access);

      --  Fork a new process

      Descriptor.Pid := Fork;

      --  Are we now in the child (or, for Windows, still in the common
      --  process).

      if Descriptor.Pid = Null_Pid then
         --  Prepare an array of arguments to pass to C

         Arg := new String (1 .. Command_With_Path'Length + 1);
         Arg (1 .. Command_With_Path'Length) := Command_With_Path.all;
         Arg (Arg'Last)        := ASCII.NUL;
         Arg_List (1)          := Arg;

         for J in Args'Range loop
            Arg                     := new String (1 .. Args (J)'Length + 1);
            Arg (1 .. Args (J)'Length)    := Args (J).all;
            Arg (Arg'Last)                := ASCII.NUL;
            Arg_List (J + 2 - Args'First) := Arg.all'Access;
         end loop;

         Arg_List (Arg_List'Last) := null;

         --  Make sure all arguments are compatible with OS conventions

         Normalize_Arguments (Arg_List);

         --  Prepare low-level argument list from the normalized arguments

         for K in Arg_List'Range loop
            C_Arg_List (K) :=
              (if Arg_List (K) /= null
               then Arg_List (K).all'Address
               else System.Null_Address);
         end loop;

         --  This does not return on Unix systems

         Set_Up_Child_Communications
           (Descriptor, Pipe1, Pipe2, Pipe3, Command_With_Path.all,
            C_Arg_List'Address);

         --  On Windows systems we need to release memory taken for Arg_List

         for A of Arg_List loop
            Free (A);
         end loop;
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

      --  Initialize the filters

      Descriptor.Filters := null;
   end Non_Blocking_Spawn;

   -------------------------
   -- Reinitialize_Buffer --
   -------------------------

   procedure Reinitialize_Buffer
     (Descriptor : in out Process_Descriptor'Class)
   is
   begin
      if Descriptor.Buffer_Size = 0 then
         declare
            Tmp : String_Access := Descriptor.Buffer;

         begin
            Descriptor.Buffer :=
              new String
                (1 .. Descriptor.Buffer_Index - Descriptor.Last_Match_End);

            if Tmp /= null then
               Descriptor.Buffer.all := Tmp
                 (Descriptor.Last_Match_End + 1 .. Descriptor.Buffer_Index);
               Free (Tmp);
            end if;
         end;

         Descriptor.Buffer_Index := Descriptor.Buffer'Last;

      else
         Descriptor.Buffer
           (1 .. Descriptor.Buffer_Index - Descriptor.Last_Match_End) :=
             Descriptor.Buffer
               (Descriptor.Last_Match_End + 1 .. Descriptor.Buffer_Index);

         if Descriptor.Buffer_Index > Descriptor.Last_Match_End then
            Descriptor.Buffer_Index :=
              Descriptor.Buffer_Index - Descriptor.Last_Match_End;
         else
            Descriptor.Buffer_Index := 0;
         end if;
      end if;

      Descriptor.Last_Match_Start := 0;
      Descriptor.Last_Match_End := 0;
   end Reinitialize_Buffer;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
     (Descriptor : in out Process_Descriptor;
      Filter     : Filter_Function)
   is
      Previous : Filter_List := null;
      Current  : Filter_List := Descriptor.Filters;

   begin
      while Current /= null loop
         if Current.Filter = Filter then
            if Previous = null then
               Descriptor.Filters := Current.Next;
            else
               Previous.Next := Current.Next;
            end if;
         end if;

         Previous := Current;
         Current := Current.Next;
      end loop;
   end Remove_Filter;

   ----------
   -- Send --
   ----------

   procedure Send
     (Descriptor   : in out Process_Descriptor;
      Str          : String;
      Add_LF       : Boolean := True;
      Empty_Buffer : Boolean := False)
   is
      Line_Feed   : aliased constant String := [1 .. 1 => ASCII.LF];
      Descriptors : Array_Of_Pd := [Descriptor'Unrestricted_Access];

      Result  : Expect_Match;
      Discard : Natural;
      pragma Warnings (Off, Result);
      pragma Warnings (Off, Discard);

   begin
      if Empty_Buffer then

         --  Force a read on the process if there is anything waiting

         Expect_Internal
           (Descriptors, Result, Timeout => 0, Full_Buffer => False);

         if Result = Expect_Internal_Error
           or else Result = Expect_Process_Died
         then
            raise Process_Died;
         end if;

         Descriptor.Last_Match_End := Descriptor.Buffer_Index;

         --  Empty the buffer

         Reinitialize_Buffer (Descriptor);
      end if;

      Call_Filters (Descriptor, Str, Input);
      Discard :=
        Write (Descriptor.Input_Fd, Str'Address, Str'Last - Str'First + 1);

      if Add_LF then
         Call_Filters (Descriptor, Line_Feed, Input);
         Discard :=
           Write (Descriptor.Input_Fd, Line_Feed'Address, 1);
      end if;
   end Send;

   -----------------
   -- Send_Signal --
   -----------------

   procedure Send_Signal
     (Descriptor : Process_Descriptor;
      Signal     : Integer)
   is
   begin
      --  A nonpositive process id passed to kill has special meanings. For
      --  example, -1 means kill all processes in sight, including self, in
      --  POSIX and Windows (and something slightly different in Linux). See
      --  man pages for details. In any case, we don't want to do that. Note
      --  that Descriptor.Pid will be -1 if the process was not successfully
      --  started; we don't want to kill ourself in that case.

      if Descriptor.Pid > 0 then
         Kill (Descriptor.Pid, Signal);
         --  ??? Need to check process status here
      else
         raise Invalid_Process;
      end if;
   end Send_Signal;

   ---------------------------------
   -- Set_Up_Child_Communications --
   ---------------------------------

   procedure Set_Up_Child_Communications
     (Pid   : in out Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type;
      Cmd   : String;
      Args  : System.Address)
   is
      pragma Warnings (Off, Pid);
      pragma Warnings (Off, Pipe1);
      pragma Warnings (Off, Pipe2);
      pragma Warnings (Off, Pipe3);

      Input  : File_Descriptor;
      Output : File_Descriptor;
      Error  : File_Descriptor;

      No_Fork_On_Target : constant Boolean := Target_OS = Windows;

   begin
      if No_Fork_On_Target then

         --  Since Windows does not have a separate fork/exec, we need to
         --  perform the following actions:

         --    - save stdin, stdout, stderr
         --    - replace them by our pipes
         --    - create the child with process handle inheritance
         --    - revert to the previous stdin, stdout and stderr.

         Input  := Dup (GNAT.OS_Lib.Standin);
         Output := Dup (GNAT.OS_Lib.Standout);
         Error  := Dup (GNAT.OS_Lib.Standerr);
      end if;

      --  Since we are still called from the parent process, there is no way
      --  currently we can cleanly close the unneeded ends of the pipes, but
      --  this doesn't really matter.

      --  We could close Pipe1.Output, Pipe2.Input, Pipe3.Input

      Dup2 (Pipe1.Input,  GNAT.OS_Lib.Standin);
      Dup2 (Pipe2.Output, GNAT.OS_Lib.Standout);
      Dup2 (Pipe3.Output, GNAT.OS_Lib.Standerr);

      Portable_Execvp (Pid.Pid'Access, Cmd & ASCII.NUL, Args);

      --  The following lines are only required for Windows systems and will
      --  not be executed on Unix systems, but we use the same condition as
      --  above to avoid warnings on uninitialized variables on Unix systems.
      --  We are now in the parent process.

      if No_Fork_On_Target then

         --  Restore the old descriptors

         Dup2 (Input,  GNAT.OS_Lib.Standin);
         Dup2 (Output, GNAT.OS_Lib.Standout);
         Dup2 (Error,  GNAT.OS_Lib.Standerr);
         Close (Input);
         Close (Output);
         Close (Error);
      end if;
   end Set_Up_Child_Communications;

   ---------------------------
   -- Set_Up_Communications --
   ---------------------------

   procedure Set_Up_Communications
     (Pid        : in out Process_Descriptor;
      Err_To_Out : Boolean;
      Pipe1      : not null access Pipe_Type;
      Pipe2      : not null access Pipe_Type;
      Pipe3      : not null access Pipe_Type)
   is
      Status : Boolean;
      pragma Warnings (Off, "modified by call, but value overwritten");

   begin
      --  Create the pipes

      if Create_Pipe (Pipe1) /= 0 then
         return;
      end if;

      if Create_Pipe (Pipe2) /= 0 then
         Close (Pipe1.Input);
         Close (Pipe1.Output);
         return;
      end if;

      --  Record the 'parent' end of the two pipes in Pid:
      --    Child stdin  is connected to the 'write' end of Pipe1;
      --    Child stdout is connected to the 'read'  end of Pipe2.
      --  We do not want these descriptors to remain open in the child
      --  process, so we mark them close-on-exec/non-inheritable.

      Pid.Input_Fd  := Pipe1.Output;
      Set_Close_On_Exec (Pipe1.Output, True, Status);
      Pid.Output_Fd := Pipe2.Input;
      Set_Close_On_Exec (Pipe2.Input, True, Status);

      if Err_To_Out then

         --  Reuse the standard output pipe for standard error

         Pipe3.all := Pipe2.all;

      else
         --  Create a separate pipe for standard error

         if Create_Pipe (Pipe3) /= 0 then
            Pipe3.all := Pipe2.all;
         end if;
      end if;

      --  As above, record the proper fd for the child's standard error stream

      Pid.Error_Fd := Pipe3.Input;
      Set_Close_On_Exec (Pipe3.Input, True, Status);
   end Set_Up_Communications;

   ----------------------------------
   -- Set_Up_Parent_Communications --
   ----------------------------------

   procedure Set_Up_Parent_Communications
     (Pid   : in out Process_Descriptor;
      Pipe1 : in out Pipe_Type;
      Pipe2 : in out Pipe_Type;
      Pipe3 : in out Pipe_Type)
   is
      pragma Warnings (Off, Pid);
      pragma Warnings (Off, Pipe1);
      pragma Warnings (Off, Pipe2);
      pragma Warnings (Off, Pipe3);

   begin
      Close (Pipe1.Input);
      Close (Pipe2.Output);

      if Pipe3.Output /= Pipe2.Output then
         Close (Pipe3.Output);
      end if;
   end Set_Up_Parent_Communications;

   ------------------
   -- Trace_Filter --
   ------------------

   procedure Trace_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      pragma Warnings (Off, Descriptor);
      pragma Warnings (Off, User_Data);
   begin
      GNAT.IO.Put (Str);
   end Trace_Filter;

   --------------------
   -- Unlock_Filters --
   --------------------

   procedure Unlock_Filters (Descriptor : in out Process_Descriptor) is
   begin
      if Descriptor.Filters_Lock > 0 then
         Descriptor.Filters_Lock := Descriptor.Filters_Lock - 1;
      end if;
   end Unlock_Filters;

end GNAT.Expect;
