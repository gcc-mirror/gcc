------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2007-2025, AdaCore                      --
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

--  This is the GNU/Linux implementation of this package
--
--  Testing on GNU/Linux can be done with socat & stty tools.
--
--  First in a terminal create a virtual serial port:
--
--  * First solution, the terminal is one of the side of the channel
--    characters written with Write into the port will be displayed
--    there and characters typed into the terminal will be send to the
--    channel and will be received by a Read call.
--
--     $ socat PTY,link=/tmp/virtual-tty,raw,echo=1 -
--
--  * Second solution, the virtual channel contains two side and the
--    program can Read and Write date to it.
--
--     $ socat PTY,link=/tmp/virtual-tty,raw,echo=1 \
--         PTY,link=/tmp/virtual-tty,raw,echo=1
--
--  Connect to this virtual serial port with:
--
--     Open (Port => P, Name => "/tmp/virtual-tty");
--
--  Do any settings using the Set routine below, then you can check
--  the serial port configuration with:
--
--     $ stty --file /tmp/virtual-tty
--

with Ada.Streams;          use Ada.Streams;

with System;               use System;
with System.Communication; use System.Communication;
with System.CRTL;          use System.CRTL;

with GNAT.OS_Lib;          use GNAT.OS_Lib;

package body GNAT.Serial_Communications is

   package OSC renames System.OS_Constants;

   use type Interfaces.C.unsigned;

   subtype unsigned is Interfaces.C.unsigned;
   subtype char is Interfaces.C.char;
   subtype unsigned_char is Interfaces.C.unsigned_char;

   function fcntl (fd : int; cmd : int; value : int) return int;
   pragma Import (C, fcntl, "fcntl");

   C_Data_Rate : constant array (Data_Rate) of unsigned :=
                   [B75      => OSC.B75,
                    B110     => OSC.B110,
                    B150     => OSC.B150,
                    B300     => OSC.B300,
                    B600     => OSC.B600,
                    B1200    => OSC.B1200,
                    B2400    => OSC.B2400,
                    B4800    => OSC.B4800,
                    B9600    => OSC.B9600,
                    B19200   => OSC.B19200,
                    B38400   => OSC.B38400,
                    B57600   => OSC.B57600,
                    B115200  => OSC.B115200,
                    B230400  => OSC.B230400,
                    B460800  => OSC.B460800,
                    B500000  => OSC.B500000,
                    B576000  => OSC.B576000,
                    B921600  => OSC.B921600,
                    B1000000 => OSC.B1000000,
                    B1152000 => OSC.B1152000,
                    B1500000 => OSC.B1500000,
                    B2000000 => OSC.B2000000,
                    B2500000 => OSC.B2500000,
                    B3000000 => OSC.B3000000,
                    B3500000 => OSC.B3500000,
                    B4000000 => OSC.B4000000];

   C_Bits      : constant array (Data_Bits) of unsigned :=
                   [CS7 => OSC.CS7, CS8 => OSC.CS8];

   C_Stop_Bits : constant array (Stop_Bits_Number) of unsigned :=
                   [One => 0, Two => OSC.CSTOPB];

   C_Parity    : constant array (Parity_Check) of unsigned :=
                   [None => 0,
                    Odd  => OSC.PARENB or OSC.PARODD,
                    Even => OSC.PARENB];

   procedure Raise_Error (Message : String; Error : Integer := Errno);
   pragma No_Return (Raise_Error);

   ----------
   -- Name --
   ----------

   function Name (Number : Positive) return Port_Name is
      N     : constant Natural := Number - 1;
      N_Img : constant String  := Natural'Image (N);
   begin
      return Port_Name ("/dev/ttyS" & N_Img (N_Img'First + 1 .. N_Img'Last));
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (Port : out Serial_Port;
      Name : Port_Name)
   is
      use OSC;

      C_Name : constant String := String (Name) & ASCII.NUL;
      Res    : int;

   begin
      Port.H := Serial_Port_Descriptor (open
         (C_Name (C_Name'First)'Address, int (O_RDWR + O_NOCTTY + O_NDELAY)));

      if Port.H = -1 then
         Raise_Error ("open: open failed");
      end if;

      --  By default we are in blocking mode

      Res := fcntl (int (Port.H), F_SETFL, 0);

      if Res = -1 then
         Raise_Error ("open: fcntl failed");
      end if;
   end Open;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Message : String; Error : Integer := Errno) is
   begin
      raise Serial_Error with Message
        & (if Error /= 0
           then " (" & Errno_Message (Err => Error) & ')'
           else "");
   end Raise_Error;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Port   : in out Serial_Port;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Len : constant size_t := Buffer'Length;
      Res : ssize_t;

   begin
      if Port.H = -1 then
         Raise_Error ("read: port not opened", 0);
      end if;

      Res := read (Integer (Port.H), Buffer'Address, Len);

      if Res = -1 then
         Raise_Error ("read failed");
      end if;

      Last := Last_Index (Buffer'First, size_t (Res));
   end Read;

   ---------
   -- Set --
   ---------

   procedure Set
     (Port      : Serial_Port;
      Rate      : Data_Rate        := B9600;
      Bits      : Data_Bits        := CS8;
      Stop_Bits : Stop_Bits_Number := One;
      Parity    : Parity_Check     := None;
      Block     : Boolean          := True;
      Local     : Boolean          := True;
      Flow      : Flow_Control     := None;
      Timeout   : Duration         := 10.0)
   is
      use OSC;

      subtype speed_t is unsigned;

      type termios is record
         c_iflag  : unsigned;
         c_oflag  : unsigned;
         c_cflag  : unsigned;
         c_lflag  : unsigned;
         c_line   : unsigned_char;
         c_cc     : Interfaces.C.char_array (0 .. 31);
         c_ispeed : speed_t;
         c_ospeed : speed_t;
      end record;
      pragma Convention (C, termios);

      function tcgetattr (fd : int; termios_p : Address) return int;
      pragma Import (C, tcgetattr, "tcgetattr");

      function tcsetattr
        (fd : int; action : int; termios_p : Address) return int;
      pragma Import (C, tcsetattr, "tcsetattr");

      function tcflush (fd : int; queue_selector : int) return int;
      pragma Import (C, tcflush, "tcflush");

      function cfsetospeed (termios_p : Address; speed : speed_t) return int;
      pragma Import (C, cfsetospeed, "cfsetospeed");

      function cfsetispeed (termios_p : Address; speed : speed_t) return int;
      pragma Import (C, cfsetispeed, "cfsetispeed");

      Current : termios;

      Res : int := 0;
      pragma Warnings (Off, Res);
      --  Warnings off, since we don't always test the result

   begin
      if Port.H = -1 then
         Raise_Error ("set: port not opened", 0);
      end if;

      --  Get current port settings

      Res := tcgetattr (int (Port.H), Current'Address);

      --  Change settings now

      Current.c_cflag := C_Bits (Bits)
                           or C_Stop_Bits (Stop_Bits)
                           or C_Parity (Parity)
                           or CREAD;

      Current.c_iflag := 0;
      Current.c_lflag := 0;
      Current.c_oflag := 0;

      if Local then
         Current.c_cflag := Current.c_cflag or CLOCAL;
      end if;

      case Flow is
         when None =>
            null;

         when RTS_CTS =>
            Current.c_cflag := Current.c_cflag or CRTSCTS;

         when Xon_Xoff =>
            Current.c_iflag := Current.c_iflag or IXON;
      end case;

      Current.c_ispeed := Data_Rate_Value (Rate);
      Current.c_ospeed := Data_Rate_Value (Rate);

      --  See man termios for descriptions about the different modes

      if Block and then Timeout = 0.0 then
         --  MIN > 0, TIME == 0 (blocking read)
         Current.c_cc (VMIN)  := char'Val (1);
         Current.c_cc (VTIME) := char'Val (0);

      else
         --  MIN == 0, TIME > 0  (read with timeout)
         --  MIN == 0, TIME == 0 (polling read)
         Current.c_cc (VMIN)  := char'Val (0);
         Current.c_cc (VTIME) := char'Val (Natural (Timeout * 10));

         Current.c_lflag := Current.c_lflag or not ICANON;
      end if;

      Res := cfsetispeed (Current'Address, C_Data_Rate (Rate));

      if Res = -1 then
         Raise_Error ("set: cfsetispeed failed");
      end if;

      Res := cfsetospeed (Current'Address, C_Data_Rate (Rate));

      if Res = -1 then
         Raise_Error ("set: cfsetospeed failed");
      end if;

      --  Set port settings

      Res := tcflush (int (Port.H), TCIFLUSH);
      Res := tcsetattr (int (Port.H), TCSANOW, Current'Address);

      --  Block

      if Block then
         --  In blocking mode, remove the non-blocking flags set while
         --  opening the serial port (see Open).
         Res := fcntl (int (Port.H), F_SETFL, 0);
      end if;

      if Res = -1 then
         Raise_Error ("set: fcntl failed");
      end if;
   end Set;

   ------------
   -- To_Ada --
   ------------

   procedure To_Ada (Port : out Serial_Port; Fd : Serial_Port_Descriptor) is
   begin
      Port.H := Fd;
   end To_Ada;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Port   : in out Serial_Port;
      Buffer : Stream_Element_Array)
   is
      Len : constant size_t := Buffer'Length;
      Res : ssize_t;

   begin
      if Port.H = -1 then
         Raise_Error ("write: port not opened", 0);
      end if;

      Res := write (int (Port.H), Buffer'Address, Len);

      if Res = -1 then
         Raise_Error ("write failed");
      end if;

      pragma Assert (size_t (Res) = Len);
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (Port : in out Serial_Port) is
      Res : int;
      pragma Unreferenced (Res);

   begin
      if Port.H /= -1 then
         Res := close (int (Port.H));
         Port.H := -1;
      end if;
   end Close;

end GNAT.Serial_Communications;
