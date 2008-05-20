------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                     S Y S T E M . W I N 3 2 . E X T                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2008, Free Software Foundation, Inc.            --
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

--  This package provides the part of the low level Win32 interface which is
--  not supported by RTX (but supported by regular Windows platforms).

package System.Win32.Ext is
   pragma Pure;

   ---------------------
   -- Time Management --
   ---------------------

   function QueryPerformanceFrequency
     (lpFrequency : access LARGE_INTEGER) return Win32.BOOL;
   pragma Import
     (Stdcall, QueryPerformanceFrequency, "QueryPerformanceFrequency");

   ---------------
   -- Processor --
   ---------------

   function SetThreadIdealProcessor
     (hThread          : HANDLE;
      dwIdealProcessor : ProcessorId) return DWORD;
   pragma Import (Stdcall, SetThreadIdealProcessor, "SetThreadIdealProcessor");

   --------------
   -- Com Port --
   --------------

   DTR_CONTROL_DISABLE : constant := 16#0#;
   RTS_CONTROL_DISABLE : constant := 16#0#;
   NOPARITY            : constant := 0;
   ODDPARITY           : constant := 1;
   EVENPARITY          : constant := 2;
   ONESTOPBIT          : constant := 0;
   TWOSTOPBITS         : constant := 2;

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
   pragma Pack (DCB);

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

end System.Win32.Ext;
