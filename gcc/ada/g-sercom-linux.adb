------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2007-2010, AdaCore                      --
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

with Ada.Streams;                use Ada.Streams;
with Ada;                        use Ada;
with Ada.Unchecked_Deallocation;

with System;               use System;
with System.Communication; use System.Communication;
with System.CRTL;          use System.CRTL;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body GNAT.Serial_Communications is

   use type Interfaces.C.unsigned;

   type Port_Data is new int;

   subtype unsigned is Interfaces.C.unsigned;
   subtype char is Interfaces.C.char;
   subtype unsigned_char is Interfaces.C.unsigned_char;

   function fcntl (fd : int; cmd : int; value : int) return int;
   pragma Import (C, fcntl, "fcntl");

   O_RDWR   : constant := 8#02#;
   O_NOCTTY : constant := 8#0400#;
   O_NDELAY : constant := 8#04000#;
   FNDELAY  : constant := O_NDELAY;
   F_SETFL  : constant := 4;
   TCSANOW  : constant := 0;
   TCIFLUSH : constant := 0;
   CLOCAL   : constant := 8#04000#;
   CREAD    : constant := 8#0200#;
   CSTOPB   : constant := 8#0100#;
   CRTSCTS  : constant := 8#020000000000#;
   PARENB   : constant := 8#00400#;
   PARODD   : constant := 8#01000#;

   --  c_cc indexes

   VTIME : constant := 5;
   VMIN  : constant := 6;

   C_Data_Rate : constant array (Data_Rate) of unsigned :=
                   (B1200   => 8#000011#,
                    B2400   => 8#000013#,
                    B4800   => 8#000014#,
                    B9600   => 8#000015#,
                    B19200  => 8#000016#,
                    B38400  => 8#000017#,
                    B57600  => 8#010001#,
                    B115200 => 8#010002#);

   C_Bits      : constant array (Data_Bits) of unsigned :=
                   (CS7 => 8#040#, CS8 => 8#060#);

   C_Stop_Bits : constant array (Stop_Bits_Number) of unsigned :=
                   (One => 0, Two => CSTOPB);

   C_Parity    : constant array (Parity_Check) of unsigned :=
                   (None => 0, Odd => PARENB or PARODD, Even => PARENB);

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
      C_Name : constant String := String (Name) & ASCII.NUL;
      Res    : int;

   begin
      if Port.H = null then
         Port.H := new Port_Data;
      end if;

      Port.H.all := Port_Data (open
         (C_Name (C_Name'First)'Address, int (O_RDWR + O_NOCTTY + O_NDELAY)));

      if Port.H.all = -1 then
         Raise_Error ("open: open failed");
      end if;

      --  By default we are in blocking mode

      Res := fcntl (int (Port.H.all), F_SETFL, 0);

      if Res = -1 then
         Raise_Error ("open: fcntl failed");
      end if;
   end Open;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Message : String; Error : Integer := Errno) is
   begin
      raise Serial_Error with Message & " (" & Integer'Image (Error) & ')';
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
      if Port.H = null then
         Raise_Error ("read: port not opened", 0);
      end if;

      Res := read (Integer (Port.H.all), Buffer'Address, Len);

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
      Timeout   : Duration         := 10.0)
   is
      type termios is record
         c_iflag  : unsigned;
         c_oflag  : unsigned;
         c_cflag  : unsigned;
         c_lflag  : unsigned;
         c_line   : unsigned_char;
         c_cc     : Interfaces.C.char_array (0 .. 31);
         c_ispeed : unsigned;
         c_ospeed : unsigned;
      end record;
      pragma Convention (C, termios);

      function tcgetattr (fd : int; termios_p : Address) return int;
      pragma Import (C, tcgetattr, "tcgetattr");

      function tcsetattr
        (fd : int; action : int; termios_p : Address) return int;
      pragma Import (C, tcsetattr, "tcsetattr");

      function tcflush (fd : int; queue_selector : int) return int;
      pragma Import (C, tcflush, "tcflush");

      Current : termios;

      Res : int;
      pragma Warnings (Off, Res);
      --  Warnings off, since we don't always test the result

   begin
      if Port.H = null then
         Raise_Error ("set: port not opened", 0);
      end if;

      --  Get current port settings

      Res := tcgetattr (int (Port.H.all), Current'Address);

      --  Change settings now

      Current.c_cflag      := C_Data_Rate (Rate)
                                or C_Bits (Bits)
                                or C_Stop_Bits (Stop_Bits)
                                or C_Parity (Parity)
                                or CLOCAL
                                or CREAD
                                or CRTSCTS;
      Current.c_lflag      := 0;
      Current.c_iflag      := 0;
      Current.c_oflag      := 0;
      Current.c_ispeed     := Data_Rate_Value (Rate);
      Current.c_ospeed     := Data_Rate_Value (Rate);
      Current.c_cc (VMIN)  := char'Val (0);
      Current.c_cc (VTIME) := char'Val (Natural (Timeout * 10));

      --  Set port settings

      Res := tcflush (int (Port.H.all), TCIFLUSH);
      Res := tcsetattr (int (Port.H.all), TCSANOW, Current'Address);

      --  Block

      Res := fcntl (int (Port.H.all), F_SETFL, (if Block then 0 else FNDELAY));

      if Res = -1 then
         Raise_Error ("set: fcntl failed");
      end if;
   end Set;

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
      if Port.H = null then
         Raise_Error ("write: port not opened", 0);
      end if;

      Res := write (int (Port.H.all), Buffer'Address, Len);

      if Res = -1 then
         Raise_Error ("write failed");
      end if;

      pragma Assert (size_t (Res) = Len);
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (Port : in out Serial_Port) is
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Port_Data, Port_Data_Access);

      Res : int;
      pragma Unreferenced (Res);

   begin
      if Port.H /= null then
         Res := close (int (Port.H.all));
         Unchecked_Free (Port.H);
      end if;
   end Close;

end GNAT.Serial_Communications;
