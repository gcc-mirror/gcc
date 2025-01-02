------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              S Y S T E M . M M A P . O S _ I N T E R F A C E             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2025, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
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

--  OS pecularities abstraction package for Win32 systems.

package System.Mmap.OS_Interface is

   --  The Win package contains copy of definition found in recent System.Win32
   --  unit provided with the GNAT compiler. The copy is needed to be able to
   --  compile this unit with older compilers. Note that this internal Win
   --  package can be removed when GNAT 6.1.0 is not supported anymore.

   package Win is

      subtype PVOID is Standard.System.Address;

      type HANDLE is new Interfaces.C.ptrdiff_t;

      type WORD   is new Interfaces.C.unsigned_short;
      type DWORD  is new Interfaces.C.unsigned_long;
      type LONG   is new Interfaces.C.long;
      type SIZE_T is new Interfaces.C.size_t;

      type BOOL   is new Interfaces.C.int;
      for BOOL'Size use Interfaces.C.int'Size;

      FALSE : constant := 0;

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

      type SYSTEM_INFO is record
         dwOemId : DWORD;
         dwPageSize : DWORD;
         lpMinimumApplicationAddress : PVOID;
         lpMaximumApplicationAddress : PVOID;
         dwActiveProcessorMask       : PVOID;
         dwNumberOfProcessors        : DWORD;
         dwProcessorType             : DWORD;
         dwAllocationGranularity     : DWORD;
         wProcessorLevel             : WORD;
         wProcessorRevision          : WORD;
      end record;
      type LP_SYSTEM_INFO is access all SYSTEM_INFO;

      INVALID_HANDLE_VALUE  : constant HANDLE := -1;
      FILE_BEGIN            : constant := 0;
      FILE_SHARE_READ       : constant := 16#00000001#;
      FILE_ATTRIBUTE_NORMAL : constant := 16#00000080#;
      FILE_MAP_COPY         : constant := 1;
      FILE_MAP_READ         : constant := 4;
      FILE_MAP_WRITE        : constant := 2;
      PAGE_READONLY         : constant := 16#0002#;
      PAGE_READWRITE        : constant := 16#0004#;
      INVALID_FILE_SIZE     : constant := 16#FFFFFFFF#;

      function CreateFile
        (lpFileName            : Standard.System.Address;
         dwDesiredAccess       : DWORD;
         dwShareMode           : DWORD;
         lpSecurityAttributes  : access SECURITY_ATTRIBUTES;
         dwCreationDisposition : DWORD;
         dwFlagsAndAttributes  : DWORD;
         hTemplateFile         : HANDLE) return HANDLE;
      pragma Import (Stdcall, CreateFile, "CreateFileW");

      function WriteFile
        (hFile                  : HANDLE;
         lpBuffer               : Standard.System.Address;
         nNumberOfBytesToWrite  : DWORD;
         lpNumberOfBytesWritten : access DWORD;
         lpOverlapped           : access OVERLAPPED) return BOOL;
      pragma Import (Stdcall, WriteFile, "WriteFile");

      function ReadFile
        (hFile                : HANDLE;
         lpBuffer             : Standard.System.Address;
         nNumberOfBytesToRead : DWORD;
         lpNumberOfBytesRead  : access DWORD;
         lpOverlapped         : access OVERLAPPED) return BOOL;
      pragma Import (Stdcall, ReadFile, "ReadFile");

      function CloseHandle (hObject : HANDLE) return BOOL;
      pragma Import (Stdcall, CloseHandle, "CloseHandle");

      function GetFileSize
        (hFile : HANDLE; lpFileSizeHigh : access DWORD) return DWORD;
      pragma Import (Stdcall, GetFileSize, "GetFileSize");

      function SetFilePointer
        (hFile                : HANDLE;
         lDistanceToMove      : LONG;
         lpDistanceToMoveHigh : access LONG;
         dwMoveMethod         : DWORD) return DWORD;
      pragma Import (Stdcall, SetFilePointer, "SetFilePointer");

      function CreateFileMapping
        (hFile                : HANDLE;
         lpSecurityAttributes : access SECURITY_ATTRIBUTES;
         flProtect            : DWORD;
         dwMaximumSizeHigh    : DWORD;
         dwMaximumSizeLow     : DWORD;
         lpName               : Standard.System.Address) return HANDLE;
      pragma Import (Stdcall, CreateFileMapping, "CreateFileMappingW");

      function MapViewOfFile
        (hFileMappingObject   : HANDLE;
         dwDesiredAccess      : DWORD;
         dwFileOffsetHigh     : DWORD;
         dwFileOffsetLow      : DWORD;
         dwNumberOfBytesToMap : SIZE_T) return Standard.System.Address;
      pragma Import (Stdcall, MapViewOfFile, "MapViewOfFile");

      function UnmapViewOfFile
         (lpBaseAddress : Standard.System.Address) return BOOL;
      pragma Import (Stdcall, UnmapViewOfFile, "UnmapViewOfFile");

      procedure GetSystemInfo (lpSystemInfo : LP_SYSTEM_INFO);
      pragma Import (Stdcall, GetSystemInfo, "GetSystemInfo");

   end Win;

   type System_File is record
      Handle         : Win.HANDLE;

      Mapped         : Boolean;
      --  Whether mapping is requested by the user and available on the system

      Mapping_Handle : Win.HANDLE;

      Write          : Boolean;
      --  Whether this file can be written to

      Length         : File_Size;
      --  Length of the file. Used to know what can be mapped in the file
   end record;

   type System_Mapping is record
      Address        : Standard.System.Address;
      Length         : File_Size;
   end record;

   Invalid_System_File    : constant System_File :=
     (Win.INVALID_HANDLE_VALUE, False, Win.INVALID_HANDLE_VALUE, False, 0);
   Invalid_System_Mapping : constant System_Mapping :=
     (Standard.System.Null_Address, 0);

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return System_File;
   --  Open a file for reading and return the corresponding System_File. Return
   --  Invalid_System_File if unsuccessful.

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return System_File;
   --  Likewise for writing to a file

   procedure Close (File : in out System_File);
   --  Close a system file

   function Read_From_Disk
     (File           : System_File;
      Offset, Length : File_Size) return System.Strings.String_Access;
   --  Read a fragment of a file. It is up to the caller to free the result
   --  when done with it.

   procedure Write_To_Disk
     (File           : System_File;
      Offset, Length : File_Size;
      Buffer         : System.Strings.String_Access);
   --  Write some content to a fragment of a file

   procedure Create_Mapping
     (File           : System_File;
      Offset, Length : in out File_Size;
      Mutable        : Boolean;
      Mapping        : out System_Mapping);
   --  Create a memory mapping for the given File, for the area starting at
   --  Offset and containing Length bytes. Store it to Mapping.
   --  Note that Offset and Length may be modified according to the system
   --  needs (for boudaries, for instance). The caller must cope with actually
   --  wider mapped areas.

   procedure Dispose_Mapping
     (Mapping : in out System_Mapping);
   --  Unmap a previously-created mapping

   function Get_Page_Size return File_Size;
   --  Return the number of bytes in a system page.

end System.Mmap.OS_Interface;
