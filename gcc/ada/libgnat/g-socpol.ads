------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . P O L L                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2020-2025, AdaCore                   --
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

--  This package provides an interface to wait for one of a set of sockets to
--  become ready to perform I/O.

with System.OS_Constants;

package GNAT.Sockets.Poll is

   type Event_Type is (Input, Output, Error, Hang_Up, Invalid_Request);
   --  I/O events we can expect on socket.
   --  Input           - socket ready to read;
   --  Output          - socket available for write;
   --  Error           - socket is in error state;
   --  Hang_Up         - peer closed;
   --  Invalid_Request - invalid socket;

   type Event_Set is array (Event_Type) of Boolean;
   --  The type to get results on events waiting

   subtype Wait_Event_Type is Event_Type range Input .. Output;
   type Wait_Event_Set is array (Wait_Event_Type) of Boolean;
   --  The type to set events to wait. Note that Error event would be waited
   --  anyway.

   -------------------------------
   --  Predefined set of events --
   -------------------------------

   Input_Event  : constant Wait_Event_Set;
   --  Wait for input availability only

   Output_Event : constant Wait_Event_Set;
   --  Wait for output availability only

   Both_Events : constant Wait_Event_Set;
   --  Wait for Input and Output availability

   Error_Event : constant Wait_Event_Set;
   --  Wait only for error state on socket

   type Set (Size : Positive) is private;
   --  Set of sockets with I/O event set to wait on

   function Create (Size : Positive) return Set;
   --  Create empty socket set with defined size

   function To_Set
     (Socket : Socket_Type;
      Events : Wait_Event_Set;
      Size   : Positive := 1) return Set;
   --  Create socket set and put the Socket there at the first place.
   --  Events parameter is defining what state of the socket we are going to
   --  wait.

   procedure Append
     (Self   : in out Set;
      Socket : Socket_Type;
      Events : Wait_Event_Set);
   --  Add Socket and its I/O waiting state at the end of Self

   procedure Insert
     (Self       : in out Set;
      Socket     : Socket_Type;
      Events     : Wait_Event_Set;
      Index      : Positive;
      Keep_Order : Boolean := False);
   --  Insert Socket and its I/O waiting state at the Index position.
   --  If Keep_Order is True then all next elements moved to the next index up.
   --  Otherwise the old element from Index moved to the end of the Self set.

   procedure Remove
     (Self : in out Set; Index : Positive; Keep_Order : Boolean := False);
   --  Remove socket from Index. If Keep_Order is True then move all next
   --  elements after removed one to previous index. If Keep_Order is False
   --  then move the last element on place of the removed one.

   procedure Set_Event
     (Self  : in out Set;
      Index : Positive;
      Event : Wait_Event_Type;
      Value : Boolean);
   --  Set I/O waiting event to Value for the socket at Index position

   procedure Set_Events
     (Self   : in out Set;
      Index  : Positive;
      Events : Wait_Event_Set);
   --  Set I/O waiting events for the socket at Index position

   function Get_Events
     (Self : Set; Index : Positive) return Wait_Event_Set;
   --  Get I/O waiting events for the socket at Index position

   function Length (Self : Set) return Natural;
   --  Get the number of sockets currently in the Self set

   function Full (Self : Set) return Boolean;
   --  Return True if there is no more space in the Self set for new sockets

   procedure Wait (Self : in out Set; Timeout : Duration; Count : out Natural);
   --  Wait no longer than Timeout on the socket set for the I/O events.
   --  Count output parameter is the number of elements in the Self set are
   --  detected for I/O events. Zero Count mean timeout on wait.
   --  The iteration over activated elements in set could be done with routine
   --  Next. The kind of I/O events on element could be cheched with State or
   --  Status routines.

   procedure Next (Self : Set; Index : in out Natural);
   --  Iterate over set looking for the next index with active I/O event state.
   --  Put 0 initially into Index. Each iteration increments Index and then
   --  checks for state. End of iterations can be detected by 0 in the Index.

   procedure Copy (Source : Set; Target : out Set);
   --  Copy sockets and its I/O waiting events from Source set into Target

   function Resize (Self : Set; Size : Positive) return Set;
   --  Returns the copy of Source with modified Size

   function Growth (Self : Set) return Set;
   --  Returns the copy of Source with increased Size

   function Socket (Self : Set; Index : Positive) return Socket_Type;
   --  Returns the Socket from Index position

   function Status (Self : Set; Index : Positive) return Event_Set;
   --  Returns I/O events detected in previous Wait call at Index position

   procedure State
     (Self   : Set;
      Index  : Positive;
      Socket : out Socket_Type;
      Status : out Event_Set);
   --  Returns Socket and its I/O events detected in previous Wait call at
   --  Index position.

   function C_Status
     (Self : Set; Index : Positive) return Interfaces.C.unsigned;
   --  Return word with I/O events detected flags in previous Wait call at
   --  Index position. Possible flags are defined in System.OS_Constants names
   --  starting with POLL prefix.

private

   Input_Event  : constant Wait_Event_Set := [Input => True, Output => False];
   Output_Event : constant Wait_Event_Set := [Input => False, Output => True];
   Both_Events  : constant Wait_Event_Set := [others => True];
   Error_Event  : constant Wait_Event_Set := [others => False];

   package SOC renames System.OS_Constants;

   type nfds_t is mod 2 ** SOC.SIZEOF_nfds_t;
   for nfds_t'Size use SOC.SIZEOF_nfds_t;

   FD_Type_Bound : constant := 2 ** (SOC.SIZEOF_fd_type - 1);

   type FD_Type is range -FD_Type_Bound .. FD_Type_Bound - 1;
   for FD_Type'Size use SOC.SIZEOF_fd_type;

   type Events_Type is mod 2 ** SOC.SIZEOF_pollfd_events;
   for Events_Type'Size use SOC.SIZEOF_pollfd_events;

   type Pollfd is record
      Socket  : FD_Type;
      Events  : Events_Type := 0;
      REvents : Events_Type := 0;
   end record with Convention => C;

   type Poll_Set is array (Positive range <>) of Pollfd with Convention => C;

   type Set (Size : Positive) is record
      Length : Natural := 0;
      Max_FD : FD_Type := 0;
      Max_OK : Boolean;
      --  Is the Max_FD actual. It can became inactual after remove socket with
      --  Max_FD from set and became actual again after add socket with FD more
      --  than Max_FD.
      Fds    : Poll_Set (1 .. Size);
   end record;

   function Length (Self : Set) return Natural
   is (Self.Length);

   function Full (Self : Set) return Boolean
   is (Self.Size = Self.Length);

end GNAT.Sockets.Poll;
