------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . O B J E C T _ R E A D E R                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2009-2023, Free Software Foundation, Inc.         --
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

--  This package implements a simple, minimal overhead reader for object files
--  composed of sections of untyped heterogeneous binary data.

with Interfaces;
with System.Mmap;

package System.Object_Reader is

   --------------
   --  Limits  --
   --------------

   BUFFER_SIZE : constant := 8 * 1024;

   ---------------------
   -- Object sections --
   ----------------------

   type Object_Section is private;

   Null_Section : constant Object_Section;

   --------------------
   -- Object symbols --
   --------------------

   type Object_Symbol is private;

   ------------------------
   -- Object format type --
   ------------------------

   type Object_Format is
     (ELF32,
      --  Object format is 32-bit ELF

      ELF64,
      --  Object format is 64-bit ELF

      PECOFF,
      --  Object format is Microsoft PECOFF

      PECOFF_PLUS,
      --  Object format is Microsoft PECOFF+

      XCOFF32);
      --  Object format is AIX 32-bit XCOFF

   --  PECOFF | PECOFF_PLUS appears so often as a case choice, would
   --  seem a good idea to have a subtype name covering these two choices ???

   ------------------
   -- Object files --
   ------------------

   type Object_File (Format : Object_Format) is private;

   type Object_File_Access is access Object_File;

   ------------------------------
   -- Object architecture type --
   ------------------------------

   type Object_Arch is
     (Unknown,
      --  The target architecture has not yet been determined

      SPARC,
      --  32-bit SPARC

      SPARC64,
      --  64-bit SPARC

      i386,
      --  Intel IA32

      MIPS,
      --  MIPS Technologies MIPS

      x86_64,
      --  x86-64 (64-bit AMD/Intel)

      IA64,
      --  Intel IA64

      PPC,
      --  32-bit PowerPC

      PPC64,
      --  64-bit PowerPC

      ARM,
      --  32-bit ARM

      AARCH64);
      --  64-bit ARM

   ------------------
   -- Target types --
   ------------------

   subtype Offset is Interfaces.Integer_64;

   subtype uint8  is Interfaces.Unsigned_8;
   subtype uint16 is Interfaces.Unsigned_16;
   subtype uint32 is Interfaces.Unsigned_32;
   subtype uint64 is Interfaces.Unsigned_64;

   subtype int8  is Interfaces.Integer_8;
   subtype int16 is Interfaces.Integer_16;
   subtype int32 is Interfaces.Integer_32;
   subtype int64 is Interfaces.Integer_64;

   type Buffer is array (0 .. BUFFER_SIZE - 1) of uint8;

   type String_Ptr_Len is record
      Ptr : Mmap.Str_Access;
      Len : Natural;
   end record;
   --  A string made from a pointer and a length. Not all strings for name
   --  are C strings: COFF inlined symbol names have a max length of 8.

   -------------------------------------------
   -- Operations on buffers of untyped data --
   -------------------------------------------

   function To_String (Buf : Buffer) return String;
   --  Construct string from C style null-terminated string stored in a buffer

   function To_String_Ptr_Len
     (Ptr : Mmap.Str_Access;
      Max_Len : Natural := Natural'Last) return String_Ptr_Len;
   --  Convert PTR to a String_Ptr_Len.

   function Strlen (Buf : Buffer) return int32;
   --  Return the length of a C style null-terminated string

   -------------------------
   -- Opening and closing --
   -------------------------

   function Open
     (File_Name    : String;
      In_Exception : Boolean := False) return Object_File_Access;
   --  Open the object file and initialize the reader. In_Exception is true
   --  when the parsing is done as part of an exception handler decorator. In
   --  this mode we do not want to raise an exception.

   procedure Close (Obj : in out Object_File);
   --  Close the object file

   -----------------------
   -- Sequential access --
   -----------------------

   type Mapped_Stream is private;
   --  Provide an abstraction of a stream on a memory mapped file

   function Create_Stream (MF : System.Mmap.Mapped_File;
                           File_Offset : System.Mmap.File_Size;
                           File_Length : System.Mmap.File_Size)
                          return Mapped_Stream;
   --  Create a stream from Mf

   procedure Close (S : in out Mapped_Stream);
   --  Close the stream (deallocate memory)

   procedure Read_Raw
     (S   : in out Mapped_Stream;
      Addr  : Address;
      Size  : uint32);
   pragma Inline (Read_Raw);
   --  Read a number of fixed sized records

   procedure Seek (S : in out Mapped_Stream; Off : Offset);
   --  Seek to an absolute offset in bytes

   procedure Tell (Obj : in out Mapped_Stream; Off : out Offset)
     with Inline;
   function Tell (Obj : Mapped_Stream) return Offset
     with Inline;
   --  Fetch the current offset

   function Length (Obj : Mapped_Stream) return Offset
     with Inline;
   --  Length of the stream

   function Read (S : in out Mapped_Stream) return Mmap.Str_Access;
   --  Provide a pointer in memory at the current offset

   function Read (S : in out Mapped_Stream) return String_Ptr_Len;
   --  Provide a pointer in memory at the current offset

   function Read (S : in out Mapped_Stream) return uint8;
   function Read (S : in out Mapped_Stream) return uint16;
   function Read (S : in out Mapped_Stream) return uint32;
   function Read (S : in out Mapped_Stream) return uint64;
   function Read (S : in out Mapped_Stream) return int8;
   function Read (S : in out Mapped_Stream) return int16;
   function Read (S : in out Mapped_Stream) return int32;
   function Read (S : in out Mapped_Stream) return int64;
   --  Read a scalar

   function Read_Address
     (Obj : Object_File; S : in out Mapped_Stream) return uint64;
   --  Read either a 64 or 32 bit address from the file stream depending on the
   --  address size of the target architecture and promote it to a 64 bit type.

   function Read_LEB128 (S : in out Mapped_Stream) return uint32;
   function Read_LEB128 (S : in out Mapped_Stream) return int32;
   --  Read a value encoding in Little-Endian Base 128 format

   procedure Read_C_String (S : in out Mapped_Stream; B : out Buffer);
   function Read_C_String (S : in out Mapped_Stream) return Mmap.Str_Access;
   --  Read a C style NULL terminated string

   function Offset_To_String
     (S : in out Mapped_Stream;
      Off : Offset) return String;
   --  Construct a string from a C style NULL terminated string located at an
   --  offset into the object file.

   ------------------------
   -- Object information --
   ------------------------

   function Arch (Obj : Object_File) return Object_Arch;
   --  Return the object architecture

   function Format (Obj : Object_File) return Object_Format;
   --  Return the object file format

   function Get_Load_Address (Obj : Object_File) return uint64;
   --  Return the load address defined in Obj. May raise Format_Error if not
   --  implemented

   function Num_Sections (Obj : Object_File) return uint32;
   --  Return the number of sections composing the object file

   function Get_Section
     (Obj   : in out Object_File;
      Shnum : uint32) return Object_Section;
   --  Return the Nth section (numbered from zero)

   function Get_Section
     (Obj      : in out Object_File;
      Sec_Name : String) return Object_Section;
   --  Return a section by name

   function Create_Stream
     (Obj : Object_File;
      Sec : Object_Section) return Mapped_Stream;
   --  Create a stream for section Sec

   procedure Get_Xcode_Bounds
     (Obj   : in out Object_File;
      Low, High : out uint64);
   --  Return the low and high addresses of the code for the object file. Can
   --  be used to check if an address lies within this object file. This
   --  procedure is not efficient and the result should be saved to avoid
   --  recomputation.

   -------------------------
   -- Section information --
   -------------------------

   function Name
     (Obj : in out Object_File;
      Sec : Object_Section) return String;
   --  Return the name of a section as a string

   function Size (Sec : Object_Section) return uint64;
   --  Return the size of a section in bytes

   function Num (Sec : Object_Section) return uint32;
   --  Return the index of a section from zero

   function Off (Sec : Object_Section) return Offset;
   --  Return the byte offset of the section within the object

   ------------------------------
   -- Symbol table information --
   ------------------------------

   Null_Symbol : constant Object_Symbol;
   --  An empty symbol table entry.

   function First_Symbol (Obj : in out Object_File) return Object_Symbol;
   --  Return the first element in the symbol table or Null_Symbol if the
   --  symbol table is empty.

   function Next_Symbol
     (Obj  : in out Object_File;
      Prev : Object_Symbol) return Object_Symbol;
   --  Return the element following Prev in the symbol table, or Null_Symbol if
   --  Prev is the last symbol in the table.

   function Read_Symbol
     (Obj : in out Object_File;
      Off : Offset) return Object_Symbol;
   --  Read symbol at Off

   function Name
     (Obj : in out Object_File;
      Sym : Object_Symbol) return String_Ptr_Len;
   --  Return the name of the symbol

   function Decoded_Ada_Name
     (Obj : in out Object_File;
      Sym : String_Ptr_Len) return String;
   --  Return the decoded name of a symbol encoded as per exp_dbug.ads

   function Strip_Leading_Char
     (Obj : in out Object_File;
      Sym : String_Ptr_Len) return Positive;
   --  Return the index of the first character to decode the name. This can
   --  strip one character for ABI with a prefix (like x86 for PECOFF).

   function Value (Sym : Object_Symbol) return uint64;
   --  Return the name of the symbol

   function Size (Sym : Object_Symbol) return uint64;
   --  Return the size of the symbol in bytes

   function Spans (Sym : Object_Symbol; Addr : uint64) return Boolean;
   --  Determine whether a particular address corresponds to the range
   --  referenced by this symbol.

   function Off (Sym : Object_Symbol) return Offset;
   --  Return the offset of the symbol.

   ----------------
   -- Exceptions --
   ----------------

   IO_Error : exception;
   --  Input/Output error reading file

   Format_Error : exception;
   --  Encountered a problem parsing the object

private
   type Mapped_Stream is record
      Region : System.Mmap.Mapped_Region;
      Off    : Offset;
      Len    : Offset;
   end record;

   subtype ELF is Object_Format range ELF32 .. ELF64;
   subtype Any_PECOFF is Object_Format range PECOFF .. PECOFF_PLUS;

   type Object_File (Format : Object_Format) is record
      MF   : System.Mmap.Mapped_File := System.Mmap.Invalid_Mapped_File;
      Arch : Object_Arch := Unknown;

      Num_Sections : uint32 := 0;
      --  Number of sections

      Symtab_Last : Offset;       --  Last offset of symbol table

      In_Exception : Boolean := False;
      --  True if the parsing is done as part of an exception handler

      Sectab_Stream : Mapped_Stream;
      --  Section table

      Symtab_Stream : Mapped_Stream;
      --  Symbol table

      Symstr_Stream : Mapped_Stream;
      --  Symbol strings

      case Format is
         when ELF =>
            Secstr_Stream : Mapped_Stream;
            --  Section strings

         when Any_PECOFF =>
            ImageBase   : uint64;       --  ImageBase value from header

            --  Cache for latest result of Get_Section_Virtual_Address

            GSVA_Sec  : uint32 := uint32'Last;
            GSVA_Addr : uint64;

         when XCOFF32 =>
            null;
      end case;
   end record;

   subtype ELF_Object_File is Object_File
     with Predicate => ELF_Object_File.Format in ELF;

   subtype PECOFF_Object_File is Object_File
     with Predicate => PECOFF_Object_File.Format in Any_PECOFF;

   subtype XCOFF32_Object_File is Object_File
     with Predicate => XCOFF32_Object_File.Format in XCOFF32;

   type Object_Section is record
      Num        : uint32 := 0;
      --  Section index in the section table

      Off        : Offset := 0;
      --  First byte of the section in the object file

      Addr       : uint64 := 0;
      --  Load address of the section. Valid only when Flag_Alloc is true.

      Size       : uint64 := 0;
      --  Length of the section in bytes

      Flag_Xcode : Boolean := False;
      --  True if the section is advertised to contain executable code
   end record;

   Null_Section : constant Object_Section := (0, 0, 0, 0, False);

   type Object_Symbol is record
      Off   : Offset := 0;  --  Offset of underlying symbol on disk
      Next  : Offset := 0;  --  Offset of the following symbol
      Value : uint64 := 0;  --  Value associated with this symbol
      Size  : uint64 := 0;  --  Size of the referenced entity
   end record;

   Null_Symbol : constant Object_Symbol := (0, 0, 0, 0);
end System.Object_Reader;
