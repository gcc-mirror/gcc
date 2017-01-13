------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          S Y S T E M . M M A P                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2007-2016, AdaCore                     --
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

--  This package provides memory mapping of files. Depending on your operating
--  system, this might provide a more efficient method for accessing the
--  contents of files.
--  A description of memory-mapping is available on the sqlite page, at:
--      http://www.sqlite.org/mmap.html
--
--  The traditional method for reading a file is to allocate a buffer in the
--  application address space, then open the file and copy its contents. When
--  memory mapping is available though, the application asks the operating
--  system to return a pointer to the requested page, if possible. If the
--  requested page has been or can be mapped into the application address
--  space, the system returns a pointer to that page for the application to
--  use without having to copy anything. Skipping the copy step is what makes
--  memory mapped I/O faster.
--
--  When memory mapping is not available, this package automatically falls
--  back to the traditional copy method.
--
--  Example of use for this package, when reading a file that can be fully
--  mapped
--
--  declare
--     File : Mapped_File;
--     Str  : Str_Access;
--  begin
--     File := Open_Read ("/tmp/file_on_disk");
--     Read (File);  --  read the whole file
--     Str := Data (File);
--     for S in 1 .. Last (File) loop
--         Put (Str (S));
--     end loop;
--     Close (File);
--  end;
--
--  When the file is big, or you only want to access part of it at a given
--  time, you can use the following type of code.

--  declare
--     File   : Mapped_File;
--     Str    : Str_Access;
--     Offs   : File_Size := 0;
--     Page   : constant Integer := Get_Page_Size;
--  begin
--     File := Open_Read ("/tmp/file_on_disk");
--     while Offs < Length (File) loop
--         Read (File, Offs, Length => Long_Integer (Page) * 4);
--         Str := Data (File);
--
--         --  Print characters for this chunk:
--         for S in Integer (Offs - Offset (File)) + 1 .. Last (File) loop
--            Put (Str (S));
--         end loop;
--
--         --  Since we are reading multiples of Get_Page_Size, we can simplify
--         --  with
--         --    for S in 1 .. Last (File) loop ...
--
--         Offs := Offs + Long_Integer (Last (File));
--     end loop;

with Interfaces.C;

with System.Strings;

package System.Mmap is

   type Mapped_File is private;
   --  File to be mapped in memory.

   --  This package will use the fastest possible algorithm to load the
   --  file in memory. On systems that support it, the file is not really
   --  loaded in memory. Instead, a call to the mmap() system call (or
   --  CreateFileMapping()) will keep the file on disk, but make it
   --  accessible as if it was in memory.

   --  When the system does not support it, the file is actually loaded in
   --  memory through calls to read(), and written back with write() when you
   --  close it. This is of course much slower.

   --  Legacy: each mapped file has a "default" mapped region in it.

   type Mapped_Region is private;
   --  A representation of part of a file in memory. Actual reading/writing
   --  is done through a mapped region. After being returned by Read, a mapped
   --  region must be free'd when done. If the original Mapped_File was open
   --  for reading, it can be closed before the mapped region is free'd.

   Invalid_Mapped_File : constant Mapped_File;
   Invalid_Mapped_Region : constant Mapped_Region;

   type Unconstrained_String is new String (Positive);
   type Str_Access is access all Unconstrained_String;
   pragma No_Strict_Aliasing (Str_Access);

   type File_Size is new Interfaces.C.size_t;

   function To_Str_Access
     (Str : System.Strings.String_Access) return Str_Access;
   --  Convert Str. The returned value points to the same memory block, but no
   --  longer includes the bounds, which you need to manage yourself

   function Open_Read
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File;
   --  Open a file for reading. The same file can be shared by multiple
   --  processes, that will see each others's changes as they occur.
   --  Any attempt to write the data might result in a segmentation fault,
   --  depending on how the file is open.
   --  Name_Error is raised if the file does not exist.
   --  Filename should be compatible with the filesystem.

   function Open_Read_No_Exception
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File;
   --  Like Open_Read but return Invalid_Mapped_File in case of error

   function Open_Write
     (Filename              : String;
      Use_Mmap_If_Available : Boolean := True) return Mapped_File;
   --  Open a file for writing.
   --  You cannot change the length of the file.
   --  Name_Error is raised if the file does not exist
   --  Filename should be compatible with the filesystem.

   procedure Close (File : in out Mapped_File);
   --  Close the file, and unmap the memory that is used for the region
   --  contained in File. If the system does not support the unmmap() system
   --  call or equivalent, or these were not available for the file itself,
   --  then the file is written back to the disk if it was opened for writing.

   procedure Free (Region : in out Mapped_Region);
   --  Unmap the memory that is used for this region and deallocate the region

   procedure Read
     (File   : Mapped_File;
      Region : in out Mapped_Region;
      Offset : File_Size := 0;
      Length : File_Size := 0;
      Mutable : Boolean := False);
   --  Read a specific part of File and set Region to the corresponding mapped
   --  region, or re-use it if possible.
   --  Offset is the number of bytes since the beginning of the file at which
   --  we should start reading. Length is the number of bytes that should be
   --  read. If set to 0, as much of the file as possible is read (presumably
   --  the whole file unless you are reading a _huge_ file).
   --  Note that no (un)mapping is is done if that part of the file is already
   --  available through Region.
   --  If the file was opened for writing, any modification you do to the
   --  data stored in File will be stored on disk (either immediately when the
   --  file is opened through a mmap() system call, or when the file is closed
   --  otherwise).
   --  Mutable is processed only for reading files. If set to True, the
   --  data can be modified, even through it will not be carried through the
   --  underlying file, nor it is guaranteed to be carried through remapping.
   --  This function takes care of page size alignment issues. The accessors
   --  below only expose the region that has been requested by this call, even
   --  if more bytes were actually mapped by this function.
   --  TODO??? Enable to have a private copy for readable files

   function Read
     (File    : Mapped_File;
      Offset  : File_Size := 0;
      Length  : File_Size := 0;
      Mutable : Boolean := False) return Mapped_Region;
   --  Likewise, return a new mapped region

   procedure Read
     (File    : Mapped_File;
      Offset  : File_Size := 0;
      Length  : File_Size := 0;
      Mutable : Boolean := False);
   --  Likewise, use the legacy "default" region in File

   function Length (File : Mapped_File) return File_Size;
   --  Size of the file on the disk

   function Offset (Region : Mapped_Region) return File_Size;
   --  Return the offset, in the physical file on disk, corresponding to the
   --  requested mapped region. The first byte in the file has offest 0.

   function Offset (File : Mapped_File) return File_Size;
   --  Likewise for the region contained in File

   function Last (Region : Mapped_Region) return Integer;
   --  Return the number of requested bytes mapped in this region. It is
   --  erroneous to access Data for indices outside 1 .. Last (Region).
   --  Such accesses may cause Storage_Error to be raised.

   function Last (File : Mapped_File) return Integer;
   --  Return the number of requested bytes mapped in the region contained in
   --  File. It is erroneous to access Data for indices outside of 1 .. Last
   --  (File); such accesses may cause Storage_Error to be raised.

   function Data (Region : Mapped_Region) return Str_Access;
   --  The data mapped in Region as requested. The result is an unconstrained
   --  string, so you cannot use the usual 'First and 'Last attributes.
   --  Instead, these are respectively 1 and Size.

   function Data (File : Mapped_File) return Str_Access;
   --  Likewise for the region contained in File

   function Is_Mutable (Region : Mapped_Region) return Boolean;
   --  Return whether it is safe to change bytes in Data (Region). This is true
   --  for regions from writeable files, for regions mapped with the "Mutable"
   --  flag set, and for regions that are copied in a buffer. Note that it is
   --  not specified whether empty regions are mutable or not, since there is
   --  no byte no modify.

   function Is_Mmapped (File : Mapped_File) return Boolean;
   --  Whether regions for this file are opened through an mmap() system call
   --  or equivalent. This is in general irrelevant to your application, unless
   --  the file can be accessed by multiple concurrent processes or tasks. In
   --  such a case, and if the file is indeed mmap-ed, then the various parts
   --  of the file can be written simulatenously, and thus you cannot ensure
   --  the integrity of the file. If the file is not mmapped, the latest
   --  process to Close it overwrite what other processes have done.

   function Get_Page_Size return Integer;
   --  Returns the number of bytes in a page. Once a file is mapped from the
   --  disk, its offset and Length should be multiples of this page size (which
   --  is ensured by this package in any case). Knowing this page size allows
   --  you to map as much memory as possible at once, thus potentially reducing
   --  the number of system calls to read the file by chunks.

   function Read_Whole_File
     (Filename           : String;
      Empty_If_Not_Found : Boolean := False)
     return System.Strings.String_Access;
   --  Returns the whole contents of the file.
   --  The returned string must be freed by the user.
   --  This is a convenience function, which is of course slower than the ones
   --  above since we also need to allocate some memory, actually read the file
   --  and copy the bytes.
   --  If the file does not exist, null is returned. However, if
   --  Empty_If_Not_Found is True, then the empty string is returned instead.
   --  Filename should be compatible with the filesystem.

private
   pragma Inline (Data, Length, Last, Offset, Is_Mmapped, To_Str_Access);

   type Mapped_File_Record;
   type Mapped_File is access Mapped_File_Record;

   type Mapped_Region_Record;
   type Mapped_Region is access Mapped_Region_Record;

   Invalid_Mapped_File   : constant Mapped_File := null;
   Invalid_Mapped_Region : constant Mapped_Region := null;

end System.Mmap;
