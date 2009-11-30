------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                    Copyright (C) 2007-2009, AdaCore                      --
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

--  Serial communications package, implemented on Windows and GNU/Linux

with Ada.Streams;
with Interfaces.C;

package GNAT.Serial_Communications is

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
     (B1200, B2400, B4800, B9600, B19200, B38400, B57600, B115200);
   --  Speed of the communication

   type Data_Bits is (CS8, CS7);
   --  Communication bits

   type Stop_Bits_Number is (One, Two);
   --  One or two stop bits

   type Parity_Check is (None, Even, Odd);
   --  Either no parity check or an even or odd parity

   type Serial_Port is new Ada.Streams.Root_Stream_Type with private;

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
      Timeout   : Duration         := 10.0);
   --  The communication port settings. If Block is set then a read call
   --  will wait for the whole buffer to be filed. If Block is not set then
   --  the given Timeout (in seconds) is used. Note that the timeout precision
   --  may be limited on some implementation (e.g. on GNU/Linux the maximum
   --  precision is a tenth of seconds).

   overriding procedure Read
     (Port   : in out Serial_Port;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Read a set of bytes, put result into Buffer and set Last accordingly.
   --  Last is set to Buffer'First - 1 if no byte has been read, unless
   --  Buffer'First = Stream_Element_Offset'First, in which case Last is
   --  set to Stream_Element_Offset'Last instead.

   overriding procedure Write
     (Port   : in out Serial_Port;
      Buffer : Ada.Streams.Stream_Element_Array);
   --  Write buffer into the port

   procedure Close (Port : in out Serial_Port);
   --  Close port

private

   type Port_Data;
   type Port_Data_Access is access Port_Data;

   type Serial_Port is new Ada.Streams.Root_Stream_Type with record
      H : Port_Data_Access;
   end record;

   Data_Rate_Value : constant array (Data_Rate) of Interfaces.C.unsigned :=
                       (B1200   =>   1_200,
                        B2400   =>   2_400,
                        B4800   =>   4_800,
                        B9600   =>   9_600,
                        B19200  =>  19_200,
                        B38400  =>  38_400,
                        B57600  =>  57_600,
                        B115200 => 115_200);

end GNAT.Serial_Communications;
