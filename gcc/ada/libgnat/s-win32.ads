------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                         S Y S T E M . W I N 3 2                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2008-2024, Free Software Foundation, Inc.         --
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

--  This package plus its child provide the low level interface to the Win32
--  API. The core part of the Win32 API (common to RTX and Win32) is in this
--  package, and an additional part of the Win32 API which is not supported by
--  RTX is in package System.Win32.Ext.

with Interfaces.C;

package System.Win32 is
   pragma Pure;

   -------------------
   -- General Types --
   -------------------

   --  The LARGE_INTEGER type is actually a fixed point type
   --  that only can represent integers. The reason for this is
   --  easier conversion to Duration or other fixed point types.
   --  (See System.OS_Primitives.Clock, mingw and rtx versions.)

   type LARGE_INTEGER is delta 1.0 range -2.0**63 .. 2.0**63 - 1.0;

   subtype PVOID is Address;

   type HANDLE is new Interfaces.C.ptrdiff_t;

   INVALID_HANDLE_VALUE : constant HANDLE := -1;
   INVALID_FILE_SIZE    : constant := 16#FFFFFFFF#;

   type ULONG     is new Interfaces.C.unsigned_long;
   type DWORD     is new Interfaces.C.unsigned_long;
   type WORD      is new Interfaces.C.unsigned_short;
   type BYTE      is new Interfaces.C.unsigned_char;
   type LONG      is new Interfaces.C.long;
   type CHAR      is new Interfaces.C.char;
   type SIZE_T    is new Interfaces.C.size_t;
   type DWORD_PTR is mod 2 ** Standard'Address_Size;

   type BOOL      is new Interfaces.C.int;
   for BOOL'Size use Interfaces.C.int'Size;

   type Bits1  is range 0 .. 2 ** 1 - 1;
   type Bits2  is range 0 .. 2 ** 2 - 1;
   type Bits17 is range 0 .. 2 ** 17 - 1;
   for Bits1'Size  use 1;
   for Bits2'Size  use 2;
   for Bits17'Size use 17;

   --  Note that the following clashes with standard names are to stay
   --  compatible with the historical choice of following the C names.

   pragma Warnings (Off);
   FALSE : constant := 0;
   TRUE  : constant := 1;
   pragma Warnings (On);

   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");

   -----------
   -- Files --
   -----------

   CP_UTF8                            : constant := 65001;
   CP_ACP                             : constant := 0;

   GENERIC_READ                       : constant := 16#80000000#;
   GENERIC_WRITE                      : constant := 16#40000000#;

   CREATE_NEW                         : constant := 1;
   CREATE_ALWAYS                      : constant := 2;
   OPEN_EXISTING                      : constant := 3;
   OPEN_ALWAYS                        : constant := 4;
   TRUNCATE_EXISTING                  : constant := 5;

   FILE_SHARE_DELETE                  : constant := 16#00000004#;
   FILE_SHARE_READ                    : constant := 16#00000001#;
   FILE_SHARE_WRITE                   : constant := 16#00000002#;

   FILE_BEGIN                         : constant := 0;
   FILE_CURRENT                       : constant := 1;
   FILE_END                           : constant := 2;

   PAGE_NOACCESS                      : constant := 16#0001#;
   PAGE_READONLY                      : constant := 16#0002#;
   PAGE_READWRITE                     : constant := 16#0004#;
   PAGE_WRITECOPY                     : constant := 16#0008#;
   PAGE_EXECUTE                       : constant := 16#0010#;

   FILE_MAP_ALL_ACCESS                : constant := 16#F001f#;
   FILE_MAP_READ                      : constant := 4;
   FILE_MAP_WRITE                     : constant := 2;
   FILE_MAP_COPY                      : constant := 1;

   FILE_ADD_FILE                      : constant := 16#0002#;
   FILE_ADD_SUBDIRECTORY              : constant := 16#0004#;
   FILE_APPEND_DATA                   : constant := 16#0004#;
   FILE_CREATE_PIPE_INSTANCE          : constant := 16#0004#;
   FILE_DELETE_CHILD                  : constant := 16#0040#;
   FILE_EXECUTE                       : constant := 16#0020#;
   FILE_LIST_DIRECTORY                : constant := 16#0001#;
   FILE_READ_ATTRIBUTES               : constant := 16#0080#;
   FILE_READ_DATA                     : constant := 16#0001#;
   FILE_READ_EA                       : constant := 16#0008#;
   FILE_TRAVERSE                      : constant := 16#0020#;
   FILE_WRITE_ATTRIBUTES              : constant := 16#0100#;
   FILE_WRITE_DATA                    : constant := 16#0002#;
   FILE_WRITE_EA                      : constant := 16#0010#;
   STANDARD_RIGHTS_READ               : constant := 16#20000#;
   STANDARD_RIGHTS_WRITE              : constant := 16#20000#;
   SYNCHRONIZE                        : constant := 16#100000#;

   FILE_ATTRIBUTE_READONLY            : constant := 16#00000001#;
   FILE_ATTRIBUTE_HIDDEN              : constant := 16#00000002#;
   FILE_ATTRIBUTE_SYSTEM              : constant := 16#00000004#;
   FILE_ATTRIBUTE_DIRECTORY           : constant := 16#00000010#;
   FILE_ATTRIBUTE_ARCHIVE             : constant := 16#00000020#;
   FILE_ATTRIBUTE_DEVICE              : constant := 16#00000040#;
   FILE_ATTRIBUTE_NORMAL              : constant := 16#00000080#;
   FILE_ATTRIBUTE_TEMPORARY           : constant := 16#00000100#;
   FILE_ATTRIBUTE_SPARSE_FILE         : constant := 16#00000200#;
   FILE_ATTRIBUTE_REPARSE_POINT       : constant := 16#00000400#;
   FILE_ATTRIBUTE_COMPRESSED          : constant := 16#00000800#;
   FILE_ATTRIBUTE_OFFLINE             : constant := 16#00001000#;
   FILE_ATTRIBUTE_NOT_CONTENT_INDEXED : constant := 16#00002000#;
   FILE_ATTRIBUTE_ENCRYPTED           : constant := 16#00004000#;
   FILE_ATTRIBUTE_VALID_FLAGS         : constant := 16#00007fb7#;
   FILE_ATTRIBUTE_VALID_SET_FLAGS     : constant := 16#000031a7#;

   GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS : constant := 16#00000004#;

   type OVERLAPPED is record
      Internal     : access ULONG;
      InternalHigh : access ULONG;
      Offset       : DWORD;
      OffsetHigh   : DWORD;
      hEvent       : HANDLE;
   end record;
   pragma Convention (C_Pass_By_Copy, OVERLAPPED);

   type SECURITY_ATTRIBUTES is record
      nLength             : DWORD;
      pSecurityDescriptor : PVOID;
      bInheritHandle      : BOOL;
   end record;
   pragma Convention (C_Pass_By_Copy, SECURITY_ATTRIBUTES);

   function CreateFileA
     (lpFileName            : Address;
      dwDesiredAccess       : DWORD;
      dwShareMode           : DWORD;
      lpSecurityAttributes  : access SECURITY_ATTRIBUTES;
      dwCreationDisposition : DWORD;
      dwFlagsAndAttributes  : DWORD;
      hTemplateFile         : HANDLE) return HANDLE;
   pragma Import (Stdcall, CreateFileA, "CreateFileA");

   function CreateFile
     (lpFileName            : Address;
      dwDesiredAccess       : DWORD;
      dwShareMode           : DWORD;
      lpSecurityAttributes  : access SECURITY_ATTRIBUTES;
      dwCreationDisposition : DWORD;
      dwFlagsAndAttributes  : DWORD;
      hTemplateFile         : HANDLE) return HANDLE;
   pragma Import (Stdcall, CreateFile, "CreateFileW");

   function GetFileSize
     (hFile          : HANDLE;
      lpFileSizeHigh : access DWORD) return BOOL;
   pragma Import (Stdcall, GetFileSize, "GetFileSize");

   function SetFilePointer
     (hFile                : HANDLE;
      lDistanceToMove      : LONG;
      lpDistanceToMoveHigh : access LONG;
      dwMoveMethod         : DWORD) return DWORD;
   pragma Import (Stdcall, SetFilePointer, "SetFilePointer");

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

   function CreateFileMapping
     (hFile                : HANDLE;
      lpSecurityAttributes : access SECURITY_ATTRIBUTES;
      flProtect            : DWORD;
      dwMaximumSizeHigh    : DWORD;
      dwMaximumSizeLow     : DWORD;
      lpName               : Address) return HANDLE;
   pragma Import (Stdcall, CreateFileMapping, "CreateFileMappingA");

   function MapViewOfFile
     (hFileMappingObject   : HANDLE;
      dwDesiredAccess      : DWORD;
      dwFileOffsetHigh     : DWORD;
      dwFileOffsetLow      : DWORD;
      dwNumberOfBytesToMap : SIZE_T) return System.Address;
   pragma Import (Stdcall, MapViewOfFile, "MapViewOfFile");

   function UnmapViewOfFile (lpBaseAddress : System.Address) return BOOL;
   pragma Import (Stdcall, UnmapViewOfFile, "UnmapViewOfFile");

   function MultiByteToWideChar
     (CodePage       : WORD;
      dwFlags        : DWORD;
      lpMultiByteStr : System.Address;
      cchMultiByte   : WORD;
      lpWideCharStr  : System.Address;
      cchWideChar    : WORD) return WORD;
   pragma Import (Stdcall, MultiByteToWideChar, "MultiByteToWideChar");

   ------------------------
   -- System Information --
   ------------------------

   subtype ProcessorId is DWORD;

   type SYSTEM_INFO is record
      dwOemId                     : DWORD;
      dwPageSize                  : DWORD;
      lpMinimumApplicationAddress : PVOID;
      lpMaximumApplicationAddress : PVOID;
      dwActiveProcessorMask       : DWORD_PTR;
      dwNumberOfProcessors        : DWORD;
      dwProcessorType             : DWORD;
      dwAllocationGranularity     : DWORD;
      dwReserved                  : DWORD;
   end record;
   pragma Convention (C_Pass_By_Copy, SYSTEM_INFO);

   procedure GetSystemInfo (SI : access SYSTEM_INFO);
   pragma Import (Stdcall, GetSystemInfo, "GetSystemInfo");

   ---------------------
   -- Time Management --
   ---------------------

   type SYSTEMTIME is record
      wYear         : WORD;
      wMonth        : WORD;
      wDayOfWeek    : WORD;
      wDay          : WORD;
      wHour         : WORD;
      wMinute       : WORD;
      wSecond       : WORD;
      wMilliseconds : WORD;
   end record;
   pragma Convention (C_Pass_By_Copy, SYSTEMTIME);

   procedure GetSystemTime (pSystemTime : access SYSTEMTIME);
   pragma Import (Stdcall, GetSystemTime, "GetSystemTime");

   procedure GetSystemTimeAsFileTime (lpFileTime : access Long_Long_Integer);
   pragma Import (Stdcall, GetSystemTimeAsFileTime, "GetSystemTimeAsFileTime");

   function FileTimeToSystemTime
     (lpFileTime   : access Long_Long_Integer;
      lpSystemTime : access SYSTEMTIME) return BOOL;
   pragma Import (Stdcall, FileTimeToSystemTime, "FileTimeToSystemTime");

   function SystemTimeToFileTime
     (lpSystemTime : access SYSTEMTIME;
      lpFileTime   : access Long_Long_Integer) return BOOL;
   pragma Import (Stdcall, SystemTimeToFileTime, "SystemTimeToFileTime");

   function FileTimeToLocalFileTime
     (lpFileTime      : access Long_Long_Integer;
      lpLocalFileTime : access Long_Long_Integer) return BOOL;
   pragma Import (Stdcall, FileTimeToLocalFileTime, "FileTimeToLocalFileTime");

   function LocalFileTimeToFileTime
     (lpFileTime      : access Long_Long_Integer;
      lpLocalFileTime : access Long_Long_Integer) return BOOL;
   pragma Import (Stdcall, LocalFileTimeToFileTime, "LocalFileTimeToFileTime");

   procedure Sleep (dwMilliseconds : DWORD);
   pragma Import (Stdcall, Sleep, External_Name => "Sleep");

   function QueryPerformanceCounter
     (lpPerformanceCount : access LARGE_INTEGER) return BOOL;
   pragma Import
     (Stdcall, QueryPerformanceCounter, "QueryPerformanceCounter");

   ------------
   -- Module --
   ------------

   function GetModuleHandleEx
     (dwFlags      : DWORD;
      lpModuleName : Address;
      phModule     : access HANDLE) return BOOL;
   pragma Import (Stdcall, GetModuleHandleEx, "GetModuleHandleExA");

   function GetModuleFileName
     (hModule    : HANDLE;
      lpFilename : Address;
      nSize      : DWORD) return DWORD;
   pragma Import (Stdcall, GetModuleFileName, "GetModuleFileNameA");

   function FreeLibrary (hModule : HANDLE) return BOOL;
   pragma Import (Stdcall, FreeLibrary, "FreeLibrary");

end System.Win32;
