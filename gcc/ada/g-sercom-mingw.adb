------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G N A T . S E R I A L _ C O M M U N I C A T I O N S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                    Copyright (C) 2007-2008, AdaCore                      --
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

--  This is the Windows implementation of this package

with Ada.Unchecked_Deallocation; use Ada;
with Ada.Streams;                use Ada.Streams;
with System;                     use System;

package body GNAT.Serial_Communications is

   --  Common types

   type HANDLE is new Interfaces.C.long;
   type DWORD is new Interfaces.C.unsigned_long;
   type WORD  is new Interfaces.C.unsigned_short;
   subtype PVOID is System.Address;
   type BOOL is new Boolean;
   for BOOL'Size use Interfaces.C.unsigned_long'Size;
   type BYTE is new Interfaces.C.unsigned_char;
   subtype CHAR is Interfaces.C.char;

   type Port_Data is new HANDLE;

   type Bits1  is range 0 .. 2 ** 1 - 1;
   type Bits2  is range 0 .. 2 ** 2 - 1;
   type Bits17 is range 0 .. 2 ** 17 - 1;
   for Bits1'Size  use 1;
   for Bits2'Size  use 2;
   for Bits17'Size use 17;

   -----------
   -- Files --
   -----------

   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");

   GENERIC_READ  : constant := 16#80000000#;
   GENERIC_WRITE : constant := 16#40000000#;
   OPEN_EXISTING : constant := 3;

   type OVERLAPPED is record
      Internal     : DWORD;
      InternalHigh : DWORD;
      Offset       : DWORD;
      OffsetHigh   : DWORD;
      hEvent       : HANDLE;
   end record;

   type SECURITY_ATTRIBUTES is record
      nLength             : DWORD;
      pSecurityDescriptor : PVOID;
      bInheritHandle      : BOOL;
   end record;

   function CreateFile
     (lpFileName            : Address;
      dwDesiredAccess       : DWORD;
      dwShareMode           : DWORD;
      lpSecurityAttributes  : access SECURITY_ATTRIBUTES;
      dwCreationDisposition : DWORD;
      dwFlagsAndAttributes  : DWORD;
      hTemplateFile         : HANDLE) return HANDLE;
   pragma Import (Stdcall, CreateFile, "CreateFileA");

   function WriteFile
     (hFile                  : HANDLE;
      lpBuffer               : Address;
      nNumberOfBytesToWrite  : DWORD;
      lpNumberOfBytesWritten : access DWORD;
      lpOverlapped           : access OVERLAPPED) return BOOL;
   pragma Import (Stdcall, WriteFile, "WriteFile");

   function ReadFile
     (hFile                : HANDLE;
      lpBuffer             : Address;
      nNumberOfBytesToRead : DWORD;
      lpNumberOfBytesRead  : access DWORD;
      lpOverlapped         : access OVERLAPPED) return BOOL;
   pragma Import (Stdcall, ReadFile, "ReadFile");

   function CloseHandle (hObject : HANDLE) return BOOL;
   pragma Import (Stdcall, CloseHandle, "CloseHandle");

   DTR_CONTROL_DISABLE : constant := 16#0#;
   RTS_CONTROL_DISABLE : constant := 16#0#;
   ODDPARITY           : constant := 1;
   ONESTOPBIT          : constant := 0;

   type DCB is record
      DCBLENGTH         : DWORD;
      BaudRate          : DWORD;
      fBinary           : Bits1;
      fParity           : Bits1;
      fOutxCtsFlow      : Bits1;
      fOutxDsrFlow      : Bits1;
      fDtrControl       : Bits2;
      fDsrSensitivity   : Bits1;
      fTXContinueOnXoff : Bits1;
      fOutX             : Bits1;
      fInX              : Bits1;
      fErrorChar        : Bits1;
      fNull             : Bits1;
      fRtsControl       : Bits2;
      fAbortOnError     : Bits1;
      fDummy2           : Bits17;
      wReserved         : WORD;
      XonLim            : WORD;
      XoffLim           : WORD;
      ByteSize          : BYTE;
      Parity            : BYTE;
      StopBits          : BYTE;
      XonChar           : CHAR;
      XoffChar          : CHAR;
      ErrorChar         : CHAR;
      EofChar           : CHAR;
      EvtChar           : CHAR;
      wReserved1        : WORD;
   end record;
   pragma Convention (C, DCB);

   for DCB use record
      DCBLENGTH         at  0 range 0 .. 31;
      BaudRate          at  4 range 0 .. 31;
      fBinary           at  8 range 0 .. 0;
      fParity           at  8 range 1 .. 1;
      fOutxCtsFlow      at  8 range 2 .. 2;
      fOutxDsrFlow      at  8 range 3 .. 3;
      fDtrControl       at  8 range 4 .. 5;
      fDsrSensitivity   at  8 range 6 .. 6;
      fTXContinueOnXoff at  8 range 7 .. 7;
      fOutX             at  9 range 0 .. 0;
      fInX              at  9 range 1 .. 1;
      fErrorChar        at  9 range 2 .. 2;
      fNull             at  9 range 3 .. 3;
      fRtsControl       at  9 range 4 .. 5;
      fAbortOnError     at  9 range 6 .. 6;
      fDummy2           at  9 range 7 .. 23;
      wReserved         at 12 range 0 .. 15;
      XonLim            at 14 range 0 .. 15;
      XoffLim           at 16 range 0 .. 15;
      ByteSize          at 18 range 0 .. 7;
      Parity            at 19 range 0 .. 7;
      StopBits          at 20 range 0 .. 7;
      XonChar           at 21 range 0 .. 7;
      XoffChar          at 22 range 0 .. 7;
      ErrorChar         at 23 range 0 .. 7;
      EofChar           at 24 range 0 .. 7;
      EvtChar           at 25 range 0 .. 7;
      wReserved1        at 26 range 0 .. 15;
   end record;

   type COMMTIMEOUTS is record
      ReadIntervalTimeout         : DWORD;
      ReadTotalTimeoutMultiplier  : DWORD;
      ReadTotalTimeoutConstant    : DWORD;
      WriteTotalTimeoutMultiplier : DWORD;
      WriteTotalTimeoutConstant   : DWORD;
   end record;
   pragma Convention (C, COMMTIMEOUTS);

   function GetCommState
     (hFile : HANDLE;
      lpDCB : access DCB) return BOOL;
   pragma Import (Stdcall, GetCommState, "GetCommState");

   function SetCommState
     (hFile : HANDLE;
      lpDCB : access DCB) return BOOL;
   pragma Import (Stdcall, SetCommState, "SetCommState");

   function SetCommTimeouts
     (hFile          : HANDLE;
      lpCommTimeouts : access COMMTIMEOUTS) return BOOL;
   pragma Import (Stdcall, SetCommTimeouts, "SetCommTimeouts");

   procedure Raise_Error (Message : String; Error : DWORD := GetLastError);
   pragma No_Return (Raise_Error);

   -----------
   -- Close --
   -----------

   procedure Close (Port : in out Serial_Port) is
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Port_Data, Port_Data_Access);

      Success : BOOL;

   begin
      if Port.H /= null then
         Success := CloseHandle (HANDLE (Port.H.all));
         Unchecked_Free (Port.H);
         if not Success then
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
      return Port_Name ("COM" & N_Img (N_Img'First + 1 .. N_Img'Last) & ':');
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
      if Port.H = null then
         Port.H := new Port_Data;
      else
         Success := CloseHandle (HANDLE (Port.H.all));
      end if;

      Port.H.all := Port_Data (CreateFile
        (lpFileName            => C_Name (C_Name'First)'Address,
         dwDesiredAccess       => GENERIC_READ or GENERIC_WRITE,
         dwShareMode           => 0,
         lpSecurityAttributes  => null,
         DwCreationDisposition => OPEN_EXISTING,
         dwFlagsAndAttributes  => 0,
         HTemplateFile         => 0));

      if Port.H.all = 0 then
         Raise_Error ("cannot open com port");
      end if;
   end Open;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Message : String; Error : DWORD := GetLastError) is
   begin
      raise Serial_Error with Message & " (" & DWORD'Image (Error) & ')';
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
      if Port.H = null then
         Raise_Error ("read: port not opened", 0);
      end if;

      Success := ReadFile
        (hFile                => HANDLE (Port.H.all),
         lpBuffer             => Buffer (Buffer'First)'Address,
         nNumberOfBytesToRead => DWORD (Buffer'Length),
         lpNumberOfBytesRead  => Read_Last'Access,
         lpOverlapped         => null);

      if not Success then
         Raise_Error ("read error");
      end if;

      Last := Buffer'First - 1 + Stream_Element_Offset (Read_Last);
   end Read;

   ---------
   -- Set --
   ---------

   procedure Set
     (Port    : Serial_Port;
      Rate    : Data_Rate := B9600;
      Bits    : Data_Bits := B8;
      Block   : Boolean   := True;
      Timeout : Integer   := 10)
   is
      Success      : BOOL;
      Com_Time_Out : aliased COMMTIMEOUTS;
      Com_Settings : aliased DCB;

   begin
      if Port.H = null then
         Raise_Error ("set: port not opened", 0);
      end if;

      Success := GetCommState (HANDLE (Port.H.all), Com_Settings'Access);

      if not Success then
         Success := CloseHandle (HANDLE (Port.H.all));
         Port.H.all := 0;
         Raise_Error ("set: cannot get comm state");
      end if;

      Com_Settings.BaudRate        := DWORD (Data_Rate_Value (Rate));
      Com_Settings.fParity         := 1;
      Com_Settings.fOutxCtsFlow    := 0;
      Com_Settings.fOutxDsrFlow    := 0;
      Com_Settings.fDsrSensitivity := 0;
      Com_Settings.fDtrControl     := DTR_CONTROL_DISABLE;
      Com_Settings.fOutX           := 0;
      Com_Settings.fInX            := 0;
      Com_Settings.fRtsControl     := RTS_CONTROL_DISABLE;
      Com_Settings.fAbortOnError   := 0;
      Com_Settings.ByteSize        := BYTE (Bit_Value (Bits));
      Com_Settings.Parity          := ODDPARITY;
      Com_Settings.StopBits        := ONESTOPBIT;

      Success := SetCommState (HANDLE (Port.H.all), Com_Settings'Access);

      if not Success then
         Success := CloseHandle (HANDLE (Port.H.all));
         Port.H.all := 0;
         Raise_Error ("cannot set comm state");
      end if;

      --  Set the timeout status

      if Block then
         Com_Time_Out := (others => 0);
      else
         Com_Time_Out :=
           (ReadTotalTimeoutConstant => DWORD (1000 * Timeout),
            others                   => 0);
      end if;

      Success := SetCommTimeouts
         (hFile          => HANDLE (Port.H.all),
          lpCommTimeouts => Com_Time_Out'Access);

      if not Success then
         Raise_Error ("cannot set the timeout");
      end if;
   end Set;

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
      if Port.H = null then
         Raise_Error ("write: port not opened", 0);
      end if;

      Success := WriteFile
         (hFile                  => HANDLE (Port.H.all),
          lpBuffer               => Buffer'Address,
          nNumberOfBytesToWrite  => DWORD (Buffer'Length),
          lpNumberOfBytesWritten => Temp_Last'Access,
          lpOverlapped           => null);

      if not Boolean (Success)
        or else Stream_Element_Offset (Temp_Last) /= Buffer'Length
      then
         Raise_Error ("failed to write data");
      end if;
   end Write;

end GNAT.Serial_Communications;
