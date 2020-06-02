------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          S Y S T E M . M M A P                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2007-2020, AdaCore                     --
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
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System.Strings; use System.Strings;

with System.Mmap.OS_Interface; use System.Mmap.OS_Interface;

package body System.Mmap is

   type Mapped_File_Record is record
      Current_Region     : Mapped_Region;
      --  The legacy API enables only one region to be mapped, directly
      --  associated with the mapped file. This references this region.

      File               : System_File;
      --  Underlying OS-level file
   end record;

   type Mapped_Region_Record is record
      File          : Mapped_File;
      --  The file this region comes from. Be careful: for reading file, it is
      --  valid to have it closed before one of its regions is free'd.

      Write         : Boolean;
      --  Whether the file this region comes from is open for writing.

      Data          : Str_Access;
      --  Unbounded access to the mapped content.

      System_Offset : File_Size;
      --  Position in the file of the first byte actually mapped in memory

      User_Offset   : File_Size;
      --  Position in the file of the first byte requested by the user

      System_Size   : File_Size;
      --  Size of the region actually mapped in memory

      User_Size     : File_Size;
      --  Size of the region requested by the user

      Mapped        : Boolean;
      --  Whether this region is actually memory mapped

      Mutable       : Boolean;
      --  If the file is opened for reading, wheter this region is writable

      Buffer        : System.Strings.String_Access;
      --  When this region is not actually memory mapped, contains the
      --  requested bytes.

      Mapping       : System_Mapping;
      --  Underlying OS-level data for the mapping, if any
   end record;

   Invalid_Mapped_Region_Record : constant Mapped_Region_Record :=
     (null, False, null, 0, 0, 0, 0, False, False, null,
      Invalid_System_Mapping);
   Invalid_Mapped_File_Record : constant Mapped_File_Record :=
     (Invalid_Mapped_Region, Invalid_System_File);

   Empty_String : constant String := "";
   --  Used to provide a valid empty Data for empty files, for instanc.

   procedure Dispose is new Ada.Unchecked_Deallocation
     (Mapped_File_Record, Mapped_File);
   procedure Dispose is new Ada.Unchecked_Deallocation
     (Mapped_Region_Record, Mapped_Region);

   function Convert is new Ada.Unchecked_Conversion
     (Standard.System.Address, Str_Access);

   procedure Compute_Data (Region : Mapped_Region);
   --  Fill the Data field according to system and user offsets. The region
   --  must actually be mapped or bufferized.

   procedure From_Disk (Region : Mapped_Region);
   --  Read a region of some file from the disk

   procedure To_Disk (Region : Mapped_Region);
   --  Write the region of the file back to disk if necessary, and free memory

   ----------------------------
   -- Open_Read_No_Exception --
   ----------------------------

   function Open_Read_No_Exception
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File
   is
      File : constant System_File :=
         Open_Read (Filename, Use_Mmap_If_Available);
   begin
      if File = Invalid_System_File then
         return Invalid_Mapped_File;
      end if;

      return new Mapped_File_Record'
        (Current_Region => Invalid_Mapped_Region,
         File           => File);
   end Open_Read_No_Exception;

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File
   is
      Res : constant Mapped_File :=
        Open_Read_No_Exception (Filename, Use_Mmap_If_Available);
   begin
      if Res = Invalid_Mapped_File then
         raise Ada.IO_Exceptions.Name_Error
           with "Cannot open " & Filename;
      else
         return Res;
      end if;
   end Open_Read;

   ----------------
   -- Open_Write --
   ----------------

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File
   is
      File : constant System_File :=
         Open_Write (Filename, Use_Mmap_If_Available);
   begin
      if File = Invalid_System_File then
         raise Ada.IO_Exceptions.Name_Error
           with "Cannot open " & Filename;
      else
         return new Mapped_File_Record'
           (Current_Region => Invalid_Mapped_Region,
            File           => File);
      end if;
   end Open_Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Mapped_File) is
   begin
      --  Closing a closed file is allowed and should do nothing

      if File = Invalid_Mapped_File then
         return;
      end if;

      if File.Current_Region /= null then
         Free (File.Current_Region);
      end if;

      if File.File /= Invalid_System_File then
         Close (File.File);
      end if;

      Dispose (File);
   end Close;

   ----------
   -- Free --
   ----------

   procedure Free (Region : in out Mapped_Region) is
      Ignored : Integer;
      pragma Unreferenced (Ignored);
   begin
      --  Freeing an already free'd file is allowed and should do nothing

      if Region = Invalid_Mapped_Region then
         return;
      end if;

      if Region.Mapping /= Invalid_System_Mapping then
         Dispose_Mapping (Region.Mapping);
      end if;
      To_Disk (Region);
      Dispose (Region);
   end Free;

   ----------
   -- Read --
   ----------

   procedure Read
     (File    : Mapped_File;
      Region  : in out Mapped_Region;
      Offset  : File_Size := 0;
      Length  : File_Size := 0;
      Mutable : Boolean := False)
   is
      File_Length      : constant File_Size := Mmap.Length (File);

      Req_Offset       : constant File_Size := Offset;
      Req_Length       : File_Size := Length;
      --  Offset and Length of the region to map, used to adjust mapping
      --  bounds, reflecting what the user will see.

      Region_Allocated : Boolean := False;
   begin
      --  If this region comes from another file, or simply if the file is
      --  writeable, we cannot re-use this mapping: free it first.

      if Region /= Invalid_Mapped_Region
        and then
          (Region.File /= File or else File.File.Write)
      then
         Free (Region);
      end if;

      if Region = Invalid_Mapped_Region then
         Region := new Mapped_Region_Record'(Invalid_Mapped_Region_Record);
         Region_Allocated := True;
      end if;

      Region.File := File;

      if Req_Offset >= File_Length then
         --  If the requested offset goes beyond file size, map nothing

         Req_Length := 0;

      elsif Length = 0
        or else
          Length > File_Length - Req_Offset
      then
         --  If Length is 0 or goes beyond file size, map till end of file

         Req_Length := File_Length - Req_Offset;

      else
         Req_Length := Length;
      end if;

      --  Past this point, the offset/length the user will see is fixed. On the
      --  other hand, the system offset/length is either already defined, from
      --  a previous mapping, or it is set to 0. In the latter case, the next
      --  step will set them according to the mapping.

      Region.User_Offset := Req_Offset;
      Region.User_Size := Req_Length;

      --  If the requested region is inside an already mapped region, adjust
      --  user-requested data and do nothing else.

      if (File.File.Write or else Region.Mutable = Mutable)
        and then
        Req_Offset >= Region.System_Offset
        and then
            (Req_Offset + Req_Length
             <= Region.System_Offset + Region.System_Size)
      then
         Region.User_Offset := Req_Offset;
         Compute_Data (Region);
         return;

      elsif Region.Buffer /= null then
         --  Otherwise, as we are not going to re-use the buffer, free it

         System.Strings.Free (Region.Buffer);
         Region.Buffer := null;

      elsif Region.Mapping /= Invalid_System_Mapping then
         --  Otherwise, there is a memory mapping that we need to unmap.
         Dispose_Mapping (Region.Mapping);
      end if;

      --  mmap() will sometimes return NULL when the file exists but is empty,
      --  which is not what we want, so in the case of a zero length file we
      --  fall back to read(2)/write(2)-based mode.

      if File_Length > 0 and then File.File.Mapped then

         Region.System_Offset := Req_Offset;
         Region.System_Size := Req_Length;
         Create_Mapping
           (File.File,
            Region.System_Offset, Region.System_Size,
            Mutable,
            Region.Mapping);
         Region.Mapped := True;
         Region.Mutable := Mutable;

      else
         --  There is no alignment requirement when manually reading the file.

         Region.System_Offset := Req_Offset;
         Region.System_Size := Req_Length;
         Region.Mapped := False;
         Region.Mutable := True;
         From_Disk (Region);
      end if;

      Region.Write := File.File.Write;
      Compute_Data (Region);

   exception
      when others =>
         --  Before propagating any exception, free any region we allocated
         --  here.

         if Region_Allocated then
            Dispose (Region);
         end if;
         raise;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (File    : Mapped_File;
      Offset  : File_Size := 0;
      Length  : File_Size := 0;
      Mutable : Boolean := False)
   is
   begin
      Read (File, File.Current_Region, Offset, Length, Mutable);
   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (File    : Mapped_File;
      Offset  : File_Size := 0;
      Length  : File_Size := 0;
      Mutable : Boolean := False) return Mapped_Region
   is
      Region  : Mapped_Region := Invalid_Mapped_Region;
   begin
      Read (File, Region, Offset, Length, Mutable);
      return Region;
   end Read;

   ------------
   -- Length --
   ------------

   function Length (File : Mapped_File) return File_Size is
   begin
      return File.File.Length;
   end Length;

   ------------
   -- Offset --
   ------------

   function Offset (Region : Mapped_Region) return File_Size is
   begin
      return Region.User_Offset;
   end Offset;

   ------------
   -- Offset --
   ------------

   function Offset (File : Mapped_File) return File_Size is
   begin
      return Offset (File.Current_Region);
   end Offset;

   ----------
   -- Last --
   ----------

   function Last (Region : Mapped_Region) return Integer is
   begin
      return Integer (Region.User_Size);
   end Last;

   ----------
   -- Last --
   ----------

   function Last (File : Mapped_File) return Integer is
   begin
      return Last (File.Current_Region);
   end Last;

   -------------------
   -- To_Str_Access --
   -------------------

   function To_Str_Access
     (Str : System.Strings.String_Access) return Str_Access is
   begin
      if Str = null then
         return null;
      else
         return Convert (Str.all'Address);
      end if;
   end To_Str_Access;

   ----------
   -- Data --
   ----------

   function Data (Region : Mapped_Region) return Str_Access is
   begin
      return Region.Data;
   end Data;

   ----------
   -- Data --
   ----------

   function Data (File : Mapped_File) return Str_Access is
   begin
      return Data (File.Current_Region);
   end Data;

   ----------------
   -- Is_Mutable --
   ----------------

   function Is_Mutable (Region : Mapped_Region) return Boolean is
   begin
      return Region.Mutable or Region.Write;
   end Is_Mutable;

   ----------------
   -- Is_Mmapped --
   ----------------

   function Is_Mmapped (File : Mapped_File) return Boolean is
   begin
      return File.File.Mapped;
   end Is_Mmapped;

   -------------------
   -- Get_Page_Size --
   -------------------

   function Get_Page_Size return Integer is
      Result : constant File_Size := Get_Page_Size;
   begin
      return Integer (Result);
   end Get_Page_Size;

   ---------------------
   -- Read_Whole_File --
   ---------------------

   function Read_Whole_File
     (Filename           : String;
      Empty_If_Not_Found : Boolean := False)
     return System.Strings.String_Access
   is
      File   : Mapped_File := Open_Read (Filename);
      Region : Mapped_Region renames File.Current_Region;
      Result : String_Access;
   begin
      Read (File);

      if Region.Data /= null then
         Result := new String'(String
                               (Region.Data (1 .. Last (Region))));

      elsif Region.Buffer /= null then
         Result := Region.Buffer;
         Region.Buffer := null;  --  So that it is not deallocated
      end if;

      Close (File);

      return Result;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         if Empty_If_Not_Found then
            return new String'("");
         else
            return null;
         end if;

      when others =>
         Close (File);
         return null;
   end Read_Whole_File;

   ---------------
   -- From_Disk --
   ---------------

   procedure From_Disk (Region : Mapped_Region) is
   begin
      pragma Assert (Region.File.all /= Invalid_Mapped_File_Record);
      pragma Assert (Region.Buffer = null);

      Region.Buffer := Read_From_Disk
        (Region.File.File, Region.User_Offset, Region.User_Size);
      Region.Mapped := False;
   end From_Disk;

   -------------
   -- To_Disk --
   -------------

   procedure To_Disk (Region : Mapped_Region) is
   begin
      if Region.Write and then Region.Buffer /= null then
         pragma Assert (Region.File.all /= Invalid_Mapped_File_Record);
         Write_To_Disk
           (Region.File.File,
            Region.User_Offset, Region.User_Size,
            Region.Buffer);
      end if;

      System.Strings.Free (Region.Buffer);
      Region.Buffer := null;
   end To_Disk;

   ------------------
   -- Compute_Data --
   ------------------

   procedure Compute_Data (Region : Mapped_Region) is
      Base_Data : Str_Access;
      --  Address of the first byte actually mapped in memory

      Data_Shift : constant Integer :=
        Integer (Region.User_Offset - Region.System_Offset);
   begin
      if Region.User_Size = 0 then
         Region.Data := Convert (Empty_String'Address);
         return;
      elsif Region.Mapped then
         Base_Data := Convert (Region.Mapping.Address);
      else
         Base_Data := Convert (Region.Buffer.all'Address);
      end if;
      Region.Data := Convert (Base_Data (Data_Shift + 1)'Address);
   end Compute_Data;

end System.Mmap;
