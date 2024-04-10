------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2007-2024, AdaCore                      --
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

--  This is the Windows implementation of this package

with Ada.Streams;          use Ada.Streams, Ada;

with System;               use System;
with System.Communication; use System.Communication;
with System.CRTL;          use System.CRTL;
with System.Win32;         use System.Win32;
with System.Win32.Ext;     use System.Win32.Ext;

with GNAT.OS_Lib;

package body GNAT.Serial_Communications is

   package OSC renames System.OS_Constants;

   --  Common types

   C_Bits      : constant array (Data_Bits) of Interfaces.C.unsigned := (8, 7);
   C_Parity    : constant array (Parity_Check) of Interfaces.C.unsigned :=
                   (None => NOPARITY, Odd => ODDPARITY, Even => EVENPARITY);
   C_Stop_Bits : constant array (Stop_Bits_Number) of Interfaces.C.unsigned :=
                   (One => ONESTOPBIT, Two => TWOSTOPBITS);

   -----------
   -- Files --
   -----------

   procedure Raise_Error (Message : String; Error : DWORD := GetLastError);
   pragma No_Return (Raise_Error);

   -----------
   -- Close --
   -----------

   procedure Close (Port : in out Serial_Port) is
      Success : BOOL;

   begin
      if Port.H /= -1 then
         Success := CloseHandle (HANDLE (Port.H));
         Port.H := -1;

         if Success = Win32.FALSE then
            Raise_Error ("error closing the port");
         end if;
      end if;
   end Close;

   ----------
   -- Name --
   ----------

   function Name (Number : Positive) return Port_Name is
      N_Img : constant String := Positive'Image (Number);
   begin
      if Number > 9 then
         return
           Port_Name ("\\.\COM" & N_Img (N_Img'First + 1 .. N_Img'Last));
      else
         return
           Port_Name ("COM" & N_Img (N_Img'First + 1 .. N_Img'Last) & ':');
      end if;
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (Port : out Serial_Port;
      Name : Port_Name)
   is
      C_Name  : constant String := String (Name) & ASCII.NUL;
      Success : BOOL;
      pragma Unreferenced (Success);

   begin
      if Port.H /= -1 then
         Success := CloseHandle (HANDLE (Port.H));
      end if;

      Port.H := CreateFileA
        (lpFileName            => C_Name (C_Name'First)'Address,
         dwDesiredAccess       => GENERIC_READ or GENERIC_WRITE,
         dwShareMode           => 0,
         lpSecurityAttributes  => null,
         dwCreationDisposition => OPEN_EXISTING,
         dwFlagsAndAttributes  => 0,
         hTemplateFile         => 0);

      pragma Assert (INVALID_HANDLE_VALUE = -1);

      if Port.H = Serial_Port_Descriptor (INVALID_HANDLE_VALUE) then
         Raise_Error ("cannot open com port");
      end if;
   end Open;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Message : String; Error : DWORD := GetLastError) is
   begin
      raise Serial_Error with Message
        & (if Error /= 0
           then " (" & GNAT.OS_Lib.Errno_Message (Err => Integer (Error)) & ')'
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
      Success   : BOOL;
      Read_Last : aliased DWORD;

   begin
      if Port.H = -1 then
         Raise_Error ("read: port not opened", 0);
      end if;

      Success :=
        ReadFile
          (hFile                => HANDLE (Port.H),
           lpBuffer             => Buffer (Buffer'First)'Address,
           nNumberOfBytesToRead => DWORD (Buffer'Length),
           lpNumberOfBytesRead  => Read_Last'Access,
           lpOverlapped         => null);

      if Success = Win32.FALSE then
         Raise_Error ("read error");
      end if;

      Last := Last_Index (Buffer'First, CRTL.size_t (Read_Last));
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
      pragma Unreferenced (Local);

      Success      : BOOL;
      Com_Time_Out : aliased COMMTIMEOUTS;
      Com_Settings : aliased DCB;

   begin
      if Port.H = -1 then
         Raise_Error ("set: port not opened", 0);
      end if;

      Success := GetCommState (HANDLE (Port.H), Com_Settings'Access);

      if Success = Win32.FALSE then
         Success := CloseHandle (HANDLE (Port.H));
         Raise_Error ("set: cannot get comm state");
      end if;

      Com_Settings.BaudRate        := DWORD (Data_Rate_Value (Rate));
      Com_Settings.fParity         := 1;
      Com_Settings.fBinary         := Bits1 (System.Win32.TRUE);
      Com_Settings.fOutxDsrFlow    := 0;
      Com_Settings.fDsrSensitivity := 0;
      Com_Settings.fDtrControl     := OSC.DTR_CONTROL_ENABLE;
      Com_Settings.fInX            := 0;
      Com_Settings.fRtsControl     := OSC.RTS_CONTROL_ENABLE;

      case Flow is
         when None =>
            Com_Settings.fOutX        := 0;
            Com_Settings.fOutxCtsFlow := 0;

         when RTS_CTS =>
            Com_Settings.fOutX        := 0;
            Com_Settings.fOutxCtsFlow := 1;

         when Xon_Xoff =>
            Com_Settings.fOutX        := 1;
            Com_Settings.fOutxCtsFlow := 0;
      end case;

      Com_Settings.fAbortOnError := 0;
      Com_Settings.ByteSize      := BYTE (C_Bits (Bits));
      Com_Settings.Parity        := BYTE (C_Parity (Parity));
      Com_Settings.StopBits      := BYTE (C_Stop_Bits (Stop_Bits));

      Success := SetCommState (HANDLE (Port.H), Com_Settings'Access);

      if Success = Win32.FALSE then
         Success := CloseHandle (HANDLE (Port.H));
         Raise_Error ("cannot set comm state");
      end if;

      --  Set the timeout status, to honor our spec with respect to read
      --  timeouts. Always disconnect write timeouts.

      --  Blocking reads - no timeout at all

      if Block then
         Com_Time_Out := (others => 0);

      --  Non-blocking reads and null timeout - immediate return with what we
      --  have - set ReadIntervalTimeout to MAXDWORD.

      elsif Timeout = 0.0 then
         Com_Time_Out :=
           (ReadIntervalTimeout => DWORD'Last,
            others              => 0);

      --  Non-blocking reads with timeout - set total read timeout accordingly

      else
         Com_Time_Out :=
           (ReadTotalTimeoutConstant => DWORD (1000 * Timeout),
            others                   => 0);
      end if;

      Success :=
        SetCommTimeouts
          (hFile          => HANDLE (Port.H),
           lpCommTimeouts => Com_Time_Out'Access);

      if Success = Win32.FALSE then
         Raise_Error ("cannot set the timeout");
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
      Success   : BOOL;
      Temp_Last : aliased DWORD;

   begin
      if Port.H = -1 then
         Raise_Error ("write: port not opened", 0);
      end if;

      Success :=
        WriteFile
          (hFile                  => HANDLE (Port.H),
           lpBuffer               => Buffer'Address,
           nNumberOfBytesToWrite  => DWORD (Buffer'Length),
           lpNumberOfBytesWritten => Temp_Last'Access,
           lpOverlapped           => null);

      if Success = Win32.FALSE
        or else Stream_Element_Offset (Temp_Last) /= Buffer'Length
      then
         Raise_Error ("failed to write data");
      end if;
   end Write;

end GNAT.Serial_Communications;
