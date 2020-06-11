------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2007-2020, AdaCore                      --
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

--  Serial communications package, implemented on Windows and GNU/Linux

with Ada.Streams;
with Interfaces.C;
with System.OS_Constants;

package GNAT.Serial_Communications is

   --  Following is a simple example of using GNAT.Serial_Communications.
   --
   --  with Ada.Streams;
   --  with GNAT.Serial_Communications;
   --
   --  procedure Serial is
   --     use Ada.Streams;
   --     use GNAT;
   --
   --     subtype Message is Stream_Element_Array (1 .. 20);
   --
   --     Data   : constant String (1 .. 20)  := "ABCDEFGHIJLKMNOPQRST";
   --     Buffer : Message;
   --
   --     S_Port : constant Natural := 5;
   --     --  Serial port number
   --
   --  begin
   --     --  Convert message (String -> Stream_Element_Array)
   --
   --     for K in Data'Range loop
   --        Buffer (Stream_Element_Offset (K)) := Character'Pos (Data (K));
   --     end loop;
   --
   --     declare
   --        Port_Name : constant Serial_Communications.Port_Name :=
   --                      Serial_Communications.Name (Number => S_Port);
   --        Port      : Serial_Communications.Serial_Port;
   --
   --     begin
   --        Serial_Communications.Open
   --          (Port => Port,
   --           Name => Port_Name);
   --
   --        Serial_Communications.Set
   --          (Port      => Port,
   --           Rate      => Serial_Communications.B9600,
   --           Bits      => Serial_Communications.CS8,
   --           Stop_Bits => Serial_Communications.One,
   --           Parity    => Serial_Communications.Even);
   --
   --        Serial_Communications.Write
   --          (Port   => Port,
   --           Buffer => Buffer);
   --
   --        Serial_Communications.Close
   --          (Port => Port);
   --     end;
   --  end Serial;

   Serial_Error : exception;
   --  Raised when a communication problem occurs

   type Port_Name is new String;
   --  A serial com port name

   function Name (Number : Positive) return Port_Name;
   --  Returns a possible port name for the given legacy PC architecture serial
   --  port number (COM<number>: on Windows, ttyS<number-1> on Linux).
   --  Note that this function does not support other kinds of serial ports
   --  nor operating systems other than Windows and Linux. For all other
   --  cases, an explicit port name can be passed directly to Open.

   type Data_Rate is
     (B75, B110, B150, B300, B600, B1200,
      B2400, B4800, B9600,
      B19200, B38400, B57600, B115200,
      B230400, B460800, B500000, B576000, B921600,
      B1000000, B1152000, B1500000,
      B2000000, B2500000, B3000000,
      B3500000, B4000000);
   --  Speed of the communication

   type Data_Bits is (CS8, CS7);
   --  Communication bits

   type Stop_Bits_Number is (One, Two);
   --  One or two stop bits

   type Parity_Check is (None, Even, Odd);
   --  Either no parity check or an even or odd parity

   type Flow_Control is (None, RTS_CTS, Xon_Xoff);
   --  No flow control, hardware flow control, software flow control

   type Serial_Port is new Ada.Streams.Root_Stream_Type with private;
   --  Serial port stream type

   type Serial_Port_Descriptor is
     new System.OS_Constants.Serial_Port_Descriptor;
   --  OS specific serial port descriptor

   procedure Open
     (Port : out Serial_Port;
      Name : Port_Name);
   --  Open the given port name. Raises Serial_Error if the port cannot be
   --  opened.

   procedure Set
     (Port      : Serial_Port;
      Rate      : Data_Rate        := B9600;
      Bits      : Data_Bits        := CS8;
      Stop_Bits : Stop_Bits_Number := One;
      Parity    : Parity_Check     := None;
      Block     : Boolean          := True;
      Local     : Boolean          := True;
      Flow      : Flow_Control     := None;
      Timeout   : Duration         := 10.0);
   --  The communication port settings. If Block is set then a read call
   --  will wait for the whole buffer to be filed. If Block is not set then
   --  the given Timeout (in seconds) is used. If Local is set then modem
   --  control lines (in particular DCD) are ignored (not supported on
   --  Windows). Flow indicates the flow control type as defined above.

   --  Note: the timeout precision may be limited on some implementation
   --  (e.g. on GNU/Linux the maximum precision is a tenth of seconds).

   --  Note: calling this procedure may reinitialize the serial port hardware
   --  and thus cause loss of some buffered data if used during communication.

   overriding procedure Read
     (Port   : in out Serial_Port;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Read a set of bytes, put result into Buffer and set Last accordingly.
   --  Last is set to Buffer'First - 1 if no byte has been read, unless
   --  Buffer'First = Stream_Element_Offset'First, in which case the exception
   --  Constraint_Error is raised instead.

   overriding procedure Write
     (Port   : in out Serial_Port;
      Buffer : Ada.Streams.Stream_Element_Array);
   --  Write buffer into the port

   procedure Close (Port : in out Serial_Port);
   --  Close port

   procedure To_Ada (Port : out Serial_Port; Fd : Serial_Port_Descriptor)
     with Inline;
   --  Convert a serial port descriptor to Serial_Port. This is useful when a
   --  serial port descriptor is obtained from an external library call.

   function To_C
     (Port : Serial_Port) return Serial_Port_Descriptor with Inline;
   --  Return a serial port descriptor to be used by external subprograms.
   --  This is useful for C functions that are not yet interfaced in this
   --  package.

private

   type Serial_Port is new Ada.Streams.Root_Stream_Type with record
      H : Serial_Port_Descriptor := -1;
   end record;

   Data_Rate_Value : constant array (Data_Rate) of Interfaces.C.unsigned :=
                       (B75      =>        75,
                        B110     =>       110,
                        B150     =>       150,
                        B300     =>       300,
                        B600     =>       600,
                        B1200    =>     1_200,
                        B2400    =>     2_400,
                        B4800    =>     4_800,
                        B9600    =>     9_600,
                        B19200   =>    19_200,
                        B38400   =>    38_400,
                        B57600   =>    57_600,
                        B115200  =>   115_200,
                        B230400  =>   230_400,
                        B460800  =>   460_800,
                        B500000  =>   500_000,
                        B576000  =>   576_000,
                        B921600  =>   921_600,
                        B1000000 => 1_000_000,
                        B1152000 => 1_152_000,
                        B1500000 => 1_500_000,
                        B2000000 => 2_000_000,
                        B2500000 => 2_500_000,
                        B3000000 => 3_000_000,
                        B3500000 => 3_500_000,
                        B4000000 => 4_000_000);

   function To_C (Port : Serial_Port) return Serial_Port_Descriptor is
      (Port.H);

end GNAT.Serial_Communications;
