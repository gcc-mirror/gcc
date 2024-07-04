------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . P O L L                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2020-2024, AdaCore                   --
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

with Ada.Calendar;

with GNAT.Sockets.Thin;

package body GNAT.Sockets.Poll is

   To_C : constant array (Wait_Event_Type) of Events_Type :=
            [Input => SOC.POLLIN or SOC.POLLPRI, Output => SOC.POLLOUT];
   --  To convert Wait_Event_Type to C I/O events flags

   procedure Set_Mode (Item : out Pollfd; Mode : Wait_Event_Set);
   --  Set I/O waiting mode on Item

   procedure Set_Event
     (Item : out Pollfd; Event : Wait_Event_Type; Value : Boolean);
   --  Set or reset waiting state on I/O event

   procedure Check_Range (Self : Set; Index : Positive) with Inline;
   --  raise Constraint_Error if Index is more than number of sockets in Self

   function Status (Item : Pollfd) return Event_Set is
     ([Input           => (Item.REvents and To_C (Input)) /= 0,
       Output          => (Item.REvents and To_C (Output)) /= 0,
       Error           => (Item.REvents and SOC.POLLERR) /= 0,
       Hang_Up         => (Item.REvents and SOC.POLLHUP) /= 0,
       Invalid_Request => (Item.REvents and SOC.POLLNVAL) /= 0]);
   --  Get I/O events from C word

   procedure Wait
     (Fds : in out Set; Timeout : Interfaces.C.int; Result : out Integer);
   --  Waits until one or more of the sockets descriptors become ready for some
   --  class of I/O operation or error state occurs on one or more of them.
   --  Timeout is in milliseconds. Result mean how many sockets ready for I/O
   --  or have error state.

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Fds : in out Set; Timeout : Interfaces.C.int; Result : out Integer)
   is separate;

   ------------
   -- Create --
   ------------

   function Create (Size : Positive) return Set is
   begin
      return Result : Set (Size);
   end Create;

   ------------
   -- To_Set --
   ------------

   function To_Set
     (Socket : Socket_Type;
      Events : Wait_Event_Set;
      Size   : Positive := 1) return Set is
   begin
      return Result : Set (Size) do
         Append (Result, Socket, Events);
      end return;
   end To_Set;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Set; Socket : Socket_Type; Events : Wait_Event_Set) is
   begin
      Insert (Self, Socket, Events, Self.Length + 1);
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self       : in out Set;
      Socket     : Socket_Type;
      Events     : Wait_Event_Set;
      Index      : Positive;
      Keep_Order : Boolean := False) is
   begin
      if Self.Size <= Self.Length then
         raise Constraint_Error with "Socket set is full";

      elsif Index > Self.Length + 1 then
         raise Constraint_Error with "Insert out of range";
      end if;

      if Socket < 0 then
         raise Socket_Error with
           "Wrong socket descriptor " & Socket_Type'Image (Socket);
      end if;

      Self.Length := Self.Length + 1;

      if Index /= Self.Length then
         if Keep_Order then
            Self.Fds (Index + 1 .. Self.Length) :=
              Self.Fds (Index .. Self.Length - 1);
         else
            Self.Fds (Self.Length) := Self.Fds (Index);
         end if;

         Self.Fds (Index).Events := 0;
      end if;

      Self.Fds (Index).Socket := FD_Type (Socket);
      Set_Mode (Self.Fds (Index), Events);

      if FD_Type (Socket) > Self.Max_FD then
         Self.Max_FD := FD_Type (Socket);
         Self.Max_OK := True;
      end if;
   end Insert;

   -----------------
   -- Check_Range --
   -----------------

   procedure Check_Range (Self : Set; Index : Positive) is
   begin
      if Index > Self.Length then
         raise Constraint_Error;
      end if;
   end Check_Range;

   ----------
   -- Copy --
   ----------

   procedure Copy (Source : Set; Target : out Set) is
   begin
      if Target.Size < Source.Length then
         raise Constraint_Error with
           "Can't copy because size of target less than source length";
      end if;

      Target.Fds (1 .. Source.Length) := Source.Fds (1 .. Source.Length);

      Target.Length := Source.Length;
      Target.Max_FD := Source.Max_FD;
      Target.Max_OK := Source.Max_OK;
   end Copy;

   ----------------
   -- Get_Events --
   ----------------

   function Get_Events
     (Self : Set; Index  : Positive) return Wait_Event_Set is
   begin
      Check_Range (Self, Index);
      return
        [Input  => (Self.Fds (Index).Events and To_C (Input)) /= 0,
         Output => (Self.Fds (Index).Events and To_C (Output)) /= 0];
   end Get_Events;

   ------------
   -- Growth --
   ------------

   function Growth (Self : Set) return Set is
   begin
      return Resize
        (Self,
         (case Self.Size is
             when 1  .. 20 => 32,
             when 21 .. 50 => 64,
             when 51 .. 99 => Self.Size + Self.Size / 3,
             when others   => Self.Size + Self.Size / 4));
   end Growth;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : in out Set; Index : Positive; Keep_Order : Boolean := False) is
   begin
      Check_Range (Self, Index);

      if Self.Max_FD = Self.Fds (Index).Socket then
         Self.Max_OK := False;
      end if;

      if Index < Self.Length then
         if Keep_Order then
            Self.Fds (Index .. Self.Length - 1) :=
              Self.Fds (Index + 1 .. Self.Length);
         else
            Self.Fds (Index) := Self.Fds (Self.Length);
         end if;
      end if;

      Self.Length := Self.Length - 1;
   end Remove;

   ------------
   -- Resize --
   ------------

   function Resize (Self : Set; Size : Positive) return Set is
   begin
      return Result : Set (Size) do
         Copy (Self, Result);
      end return;
   end Resize;

   ---------------
   -- Set_Event --
   ---------------

   procedure Set_Event
     (Self  : in out Set;
      Index : Positive;
      Event : Wait_Event_Type;
      Value : Boolean) is
   begin
      Check_Range (Self, Index);
      Set_Event (Self.Fds (Index), Event, Value);
   end Set_Event;

   procedure Set_Event
     (Item : out Pollfd; Event : Wait_Event_Type; Value : Boolean) is
   begin
      if Value then
         Item.Events := Item.Events or To_C (Event);
      else
         Item.Events := Item.Events and not To_C (Event);
      end if;
   end Set_Event;

   ----------------
   -- Set_Events --
   ----------------

   procedure Set_Events
     (Self   : in out Set;
      Index  : Positive;
      Events : Wait_Event_Set) is
   begin
      Check_Range (Self, Index);
      Set_Mode (Self.Fds (Index), Events);
   end Set_Events;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (Item : out Pollfd; Mode : Wait_Event_Set) is
   begin
      for J in Mode'Range loop
         Set_Event (Item, J, Mode (J));
      end loop;
   end Set_Mode;

   ------------
   -- Socket --
   ------------

   function Socket (Self : Set; Index : Positive) return Socket_Type is
   begin
      Check_Range (Self, Index);
      return Socket_Type (Self.Fds (Index).Socket);
   end Socket;

   -----------
   -- State --
   -----------

   procedure State
     (Self   : Set;
      Index  : Positive;
      Socket : out Socket_Type;
      Status : out Event_Set) is
   begin
      Check_Range (Self, Index);
      Socket := Socket_Type (Self.Fds (Index).Socket);
      Status := Poll.Status (Self.Fds (Index));
   end State;

   ----------
   -- Wait --
   ----------

   procedure Wait (Self : in out Set; Timeout : Duration; Count : out Natural)
   is
      use Ada.Calendar;
      --  Used to calculate partially consumed timeout on EINTR.
      --  Better to use Ada.Real_Time, but we can't in current GNAT because
      --  Ada.Real_Time is in tasking part of runtime.

      Result       : Integer;
      Poll_Timeout : Duration := Timeout;
      C_Timeout    : Interfaces.C.int;
      Errno        : Integer;
      Stamp        : constant Time := Clock;
   begin
      if Self.Length = 0 then
         Count := 0;
         return;
      end if;

      loop
         if Poll_Timeout >= Duration (Interfaces.C.int'Last - 8) / 1_000 then
            --  Minus 8 is to workaround Linux kernel 2.6.24 bug with close to
            --  Integer'Last poll timeout values.
            --  syscall (SYS_poll, &ufds, 1, 2147483644); // is waiting
            --  syscall (SYS_poll, &ufds, 1, 2147483645); // is not waiting
            --  Timeout values close to maximum could be not safe because of
            --  possible time conversion boundary errors in the kernel.
            --  Use unlimited timeout instead of maximum 24 days timeout for
            --  safety reasons.

            C_Timeout := -1;
         else
            C_Timeout := Interfaces.C.int (Poll_Timeout * 1_000);
         end if;

         Wait (Self, C_Timeout, Result);

         exit when Result >= 0;

         Errno := Thin.Socket_Errno;

         --  In case of EINTR error we have to continue waiting for network
         --  events.

         if Errno = SOC.EINTR then
            if C_Timeout >= 0 then
               Poll_Timeout := Timeout - (Clock - Stamp);

               if Poll_Timeout < 0.0 then
                  Poll_Timeout := 0.0;

               elsif Poll_Timeout > Timeout then
                  --  Clock moved back in time. This should not be happen when
                  --  we use monotonic time.

                  Poll_Timeout := Timeout;
               end if;
            end if;

         else
            Raise_Socket_Error (Errno);
         end if;
      end loop;

      Count := Result;
   end Wait;

   ----------
   -- Next --
   ----------

   procedure Next (Self : Set; Index : in out Natural) is
   begin
      loop
         Index := Index + 1;

         if Index > Self.Length then
            Index := 0;
            return;

         elsif Self.Fds (Index).REvents /= 0 then
            return;
         end if;
      end loop;
   end Next;

   ------------
   -- Status --
   ------------

   function Status (Self : Set; Index : Positive) return Event_Set is
   begin
      Check_Range (Self, Index);
      return Status (Self.Fds (Index));
   end Status;

   --------------
   -- C_Status --
   --------------

   function C_Status
     (Self : Set; Index : Positive) return Interfaces.C.unsigned is
   begin
      Check_Range (Self, Index);
      return Interfaces.C.unsigned (Self.Fds (Index).REvents);
   end C_Status;

end GNAT.Sockets.Poll;
