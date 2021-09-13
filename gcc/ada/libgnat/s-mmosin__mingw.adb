------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              S Y S T E M . M M A P . O S _ I N T E R F A C E             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2007-2021, AdaCore                     --
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

with Ada.IO_Exceptions;
with System.Strings; use System.Strings;

with System.OS_Lib;
pragma Unreferenced (System.OS_Lib);
--  Only used to generate same runtime dependencies and same binder file on
--  GNU/Linux and Windows.

package body System.Mmap.OS_Interface is

   use Win;

   function Align
     (Addr : File_Size) return File_Size;
   --  Align some offset/length to the lowest page boundary

   function Open_Common
     (Filename              : String;
      Use_Mmap_If_Available : Boolean;
      Write                 : Boolean) return System_File;

   function From_UTF8 (Path : String) return Wide_String;
   --  Convert from UTF-8 to Wide_String

   ---------------
   -- From_UTF8 --
   ---------------

   function From_UTF8 (Path : String) return Wide_String is
      function MultiByteToWideChar
        (Codepage : Interfaces.C.unsigned;
         Flags    : Interfaces.C.unsigned;
         Mbstr    : Address;
         Mb       : Natural;
         Wcstr    : Address;
         Wc       : Natural) return Integer;
      pragma Import (Stdcall, MultiByteToWideChar, "MultiByteToWideChar");

      Current_Codepage : Interfaces.C.unsigned;
      pragma Import (C, Current_Codepage, "__gnat_current_codepage");

      Len : Natural;
   begin
      --  Compute length of the result
      Len := MultiByteToWideChar
        (Current_Codepage, 0, Path'Address, Path'Length, Null_Address, 0);
      if Len = 0 then
         raise Constraint_Error;
      end if;

      declare
         --  Declare result
         Res : Wide_String (1 .. Len);
      begin
         --  And compute it
         Len := MultiByteToWideChar
           (Current_Codepage, 0,
            Path'Address, Path'Length,
            Res'Address, Len);
         if Len = 0 then
            raise Constraint_Error;
         end if;
         return Res;
      end;
   end From_UTF8;

   -----------------
   -- Open_Common --
   -----------------

   function Open_Common
     (Filename              : String;
      Use_Mmap_If_Available : Boolean;
      Write                 : Boolean) return System_File
   is
      dwDesiredAccess, dwShareMode : DWORD;
      PageFlags                    : DWORD;

      W_Filename                   : constant Wide_String :=
         From_UTF8 (Filename) & Wide_Character'Val (0);
      File_Handle, Mapping_Handle  : HANDLE;

      SizeH                        : aliased DWORD;
      Size                         : File_Size;
   begin
      if Write then
         dwDesiredAccess := GENERIC_READ + GENERIC_WRITE;
         dwShareMode     := 0;
         PageFlags       := Win.PAGE_READWRITE;
      else
         dwDesiredAccess := GENERIC_READ;
         dwShareMode     := Win.FILE_SHARE_READ;
         PageFlags       := Win.PAGE_READONLY;
      end if;

      --  Actually open the file

      File_Handle := CreateFile
        (W_Filename'Address, dwDesiredAccess, dwShareMode,
         null, OPEN_EXISTING, Win.FILE_ATTRIBUTE_NORMAL, 0);

      if File_Handle = Win.INVALID_HANDLE_VALUE then
         return Invalid_System_File;
      end if;

      --  Compute its size

      Size := File_Size (Win.GetFileSize (File_Handle, SizeH'Access));

      if Size = Win.INVALID_FILE_SIZE then
         return Invalid_System_File;
      end if;

      if SizeH /= 0 and then File_Size'Size > 32 then
         Size := Size + (File_Size (SizeH) * 2 ** 32);
      end if;

      --  Then create a mapping object, if needed. On Win32, file memory
      --  mapping is always available.

      if Use_Mmap_If_Available then
         Mapping_Handle :=
            Win.CreateFileMapping
              (File_Handle, null, PageFlags,
               0, DWORD (Size), Standard.System.Null_Address);
      else
         Mapping_Handle := Win.INVALID_HANDLE_VALUE;
      end if;

      return
        (Handle         => File_Handle,
         Mapped         => Use_Mmap_If_Available,
         Mapping_Handle => Mapping_Handle,
         Write          => Write,
         Length         => Size);
   end Open_Common;

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return System_File is
   begin
      return Open_Common (Filename, Use_Mmap_If_Available, False);
   end Open_Read;

   ----------------
   -- Open_Write --
   ----------------

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return System_File is
   begin
      return Open_Common (Filename, Use_Mmap_If_Available, True);
   end Open_Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out System_File) is
      Ignored : BOOL;
      pragma Unreferenced (Ignored);
   begin
      Ignored := CloseHandle (File.Mapping_Handle);
      Ignored := CloseHandle (File.Handle);
      File.Handle := Win.INVALID_HANDLE_VALUE;
      File.Mapping_Handle := Win.INVALID_HANDLE_VALUE;
   end Close;

   --------------------
   -- Read_From_Disk --
   --------------------

   function Read_From_Disk
     (File           : System_File;
      Offset, Length : File_Size) return System.Strings.String_Access
   is
      Buffer : String_Access := new String (1 .. Integer (Length));

      Pos    : DWORD;
      NbRead : aliased DWORD;
      pragma Unreferenced (Pos);
   begin
      Pos := Win.SetFilePointer
        (File.Handle, LONG (Offset), null, Win.FILE_BEGIN);

      if Win.ReadFile
         (File.Handle, Buffer.all'Address,
          DWORD (Length), NbRead'Unchecked_Access, null) = Win.FALSE
      then
         System.Strings.Free (Buffer);
         raise Ada.IO_Exceptions.Device_Error;
      end if;
      return Buffer;
   end Read_From_Disk;

   -------------------
   -- Write_To_Disk --
   -------------------

   procedure Write_To_Disk
     (File           : System_File;
      Offset, Length : File_Size;
      Buffer         : System.Strings.String_Access)
   is
      Pos       : DWORD;
      NbWritten : aliased DWORD;
      pragma Unreferenced (Pos);
   begin
      pragma Assert (File.Write);
      Pos := Win.SetFilePointer
        (File.Handle, LONG (Offset), null, Win.FILE_BEGIN);

      if Win.WriteFile
         (File.Handle, Buffer.all'Address,
          DWORD (Length), NbWritten'Unchecked_Access, null) = Win.FALSE
      then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
   end Write_To_Disk;

   --------------------
   -- Create_Mapping --
   --------------------

   procedure Create_Mapping
     (File           : System_File;
      Offset, Length : in out File_Size;
      Mutable        : Boolean;
      Mapping        : out System_Mapping)
   is
      Flags : DWORD;
   begin
      if File.Write then
         Flags := Win.FILE_MAP_WRITE;
      elsif Mutable then
         Flags := Win.FILE_MAP_COPY;
      else
         Flags := Win.FILE_MAP_READ;
      end if;

      --  Adjust offset and mapping length to account for the required
      --  alignment of offset on page boundary.

      declare
         Queried_Offset : constant File_Size := Offset;
      begin
         Offset := Align (Offset);

         --  First extend the length to compensate the offset shift, then align
         --  it on the upper page boundary, so that the whole queried area is
         --  covered.

         Length := Length + Queried_Offset - Offset;
         Length := Align (Length + Get_Page_Size - 1);

         --  But do not exceed the length of the file
         if Offset + Length > File.Length then
            Length := File.Length - Offset;
         end if;
      end;

      if Length > File_Size (Integer'Last) then
         raise Ada.IO_Exceptions.Device_Error;
      else
         Mapping := Invalid_System_Mapping;
         Mapping.Address :=
            Win.MapViewOfFile
              (File.Mapping_Handle, Flags,
               0, DWORD (Offset), SIZE_T (Length));
         Mapping.Length := Length;
      end if;
   end Create_Mapping;

   ---------------------
   -- Dispose_Mapping --
   ---------------------

   procedure Dispose_Mapping
     (Mapping : in out System_Mapping)
   is
      Ignored : BOOL;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Win.UnmapViewOfFile (Mapping.Address);
      Mapping := Invalid_System_Mapping;
   end Dispose_Mapping;

   -------------------
   -- Get_Page_Size --
   -------------------

   function Get_Page_Size return File_Size is
      SystemInfo : aliased SYSTEM_INFO;
   begin
      GetSystemInfo (SystemInfo'Unchecked_Access);
      return File_Size (SystemInfo.dwAllocationGranularity);
   end Get_Page_Size;

   -----------
   -- Align --
   -----------

   function Align
     (Addr : File_Size) return File_Size is
   begin
      return Addr - Addr mod Get_Page_Size;
   end Align;

end System.Mmap.OS_Interface;
