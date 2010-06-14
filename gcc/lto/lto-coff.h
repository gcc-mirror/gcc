/* LTO routines for COFF object files.
   Copyright 2009, 2010 Free Software Foundation, Inc.
   Contributed by Dave Korn.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef LTO_COFF_H
#define LTO_COFF_H

/* Rather than implementing a libcoff to match libelf, or attempting to
   integrate libbfd into GCC, this file is a self-contained (and very
   minimal) COFF format object file reader/writer.  The generated files
   will contain a COFF header, a number of COFF section headers, the 
   section data itself, and a trailing string table for section names.  */

/* Alignment of sections in a COFF object file.

   The LTO writer uses zlib compression on the data that it streams into
   LTO sections in the output object file.  Because these streams don't
   have any embedded size information, the section in the object file must
   be exactly sized to the data emitted; any trailing padding bytes will
   be interpreted as partial and/or corrupt compressed data.

   This is easy enough to do on COFF targets (with binutils 2.20.1 or
   above) because we can specify 1-byte alignment for the LTO sections.
   They are then emitted precisely-sized and byte-packed into the object
   and the reader is happy when it parses them later.  This is currently
   implemented in the x86/windows backed in i386_pe_asm_named_section()
   in config/i386/winnt.c by detecting the LTO section name prefix, 

   That would be sufficient, but for one thing.  At the start of the LTO
   data is a header struct with (currently) a couple of version numbers and
   some type info; see struct lto_header in lto-streamer.h.  If the sections
   are byte-packed, this header will not necessarily be correctly-aligned
   when it is read back into memory.

   On x86 targets, which are currently the only LTO-COFF targets, misaligned
   memory accesses aren't problematic (okay, inefficient, but not worth
   worrying about two half-word memory reads per section in the context of
   everything else the compiler has to do at the time!), but RISC targets may
   fail on trying to access the header struct.  In this case, it will be
   necessary to enable (preferably in a target-dependent fashion, but a few
   bytes of padding are hardly an important issue if it comes down to it) the
   COFF_ALIGNMENT macros below.

   As currently implemented, this will emit padding to the necessary number
   of bytes after each LTO section.  These bytes will constitute 'gaps' in
   the object file structure, as they won't be covered by any section header.
   This hasn't yet been tested, because no such RISC LTO-COFF target yet
   exists.  If it causes problems further down the toolchain, it will be
   necessary to adapt the code to emit additional section headers for these
   padding bytes, but the odds are that it will "just work".

  */

#if 0
#define COFF_ALIGNMENT	 (4)
#define COFF_ALIGNMENTM1 (COFF_ALIGNMENT - 1)
#define COFF_ALIGN(x)	 (((x) + COFF_ALIGNMENTM1) & ~COFF_ALIGNMENTM1)
#else
#define COFF_ALIGNMENT	 (1)
#define COFF_ALIGN(x)	 (x)
#endif

/* COFF header machine codes.  */

#define IMAGE_FILE_MACHINE_I386	(0x014c)

/* Known header magics for validation, as an array initialiser.  */

#define COFF_KNOWN_MACHINES \
  { IMAGE_FILE_MACHINE_I386/*, ... add more here when working.  */ }

/* COFF object file header, section and symbol flags and types.  These are
   currently specific to PE-COFF, which is the only LTO-COFF format at the
   time of writing.  Maintainers adding support for new COFF formats will
   need to make these into target macros of some kind.  */

/* COFF header characteristics.  */

#define IMAGE_FILE_EXECUTABLE_IMAGE	(1 << 1)
#define IMAGE_FILE_32BIT_MACHINE	(1 << 8)
#define IMAGE_FILE_SYSTEM		(1 << 12)
#define IMAGE_FILE_DLL			(1 << 13)

/* Desired characteristics (for validation).  */

#define COFF_CHARACTERISTICS \
  (IMAGE_FILE_32BIT_MACHINE)

/* Unwanted characteristics (for validation).  */

#define COFF_NOT_CHARACTERISTICS \
  (IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_SYSTEM | IMAGE_FILE_DLL)

/* Section flags.  LTO emits byte-aligned read-only loadable data sections.  */

#define IMAGE_SCN_CNT_INITIALIZED_DATA	 (1 << 6)
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA (1 << 7)
#define IMAGE_SCN_ALIGN_1BYTES		 (0x1 << 20)
#define IMAGE_SCN_MEM_DISCARDABLE	 (1 << 25)
#define	IMAGE_SCN_MEM_SHARED		 (1 << 28)
#define IMAGE_SCN_MEM_READ		 (1 << 30)

#define COFF_SECTION_CHARACTERISTICS \
  (IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_ALIGN_1BYTES | \
  IMAGE_SCN_MEM_DISCARDABLE | IMAGE_SCN_MEM_SHARED | IMAGE_SCN_MEM_READ)

/* Symbol-related constants.  */

#define IMAGE_SYM_DEBUG		(-2)
#define IMAGE_SYM_TYPE_NULL	(0)
#define IMAGE_SYM_DTYPE_NULL	(0)
#define IMAGE_SYM_CLASS_STATIC	(3)
#define IMAGE_SYM_CLASS_FILE	(103)

#define IMAGE_SYM_TYPE \
  ((IMAGE_SYM_DTYPE_NULL << 4) | IMAGE_SYM_TYPE_NULL)

/* Size of a COFF symbol in bytes.  */

#define COFF_SYMBOL_SIZE	(18)

/* On-disk file structures.  */

struct Coff_header
{
  unsigned char Machine[2];
  unsigned char NumberOfSections[2];
  unsigned char TimeDateStamp[4];
  unsigned char PointerToSymbolTable[4];
  unsigned char NumberOfSymbols[4];
  unsigned char SizeOfOptionalHeader[2];
  unsigned char Characteristics[2];
};
typedef struct Coff_header Coff_header;

struct Coff_section
{
  unsigned char Name[8];
  unsigned char VirtualSize[4];
  unsigned char VirtualAddress[4];
  unsigned char SizeOfRawData[4];
  unsigned char PointerToRawData[4];
  unsigned char PointerToRelocations[4];
  unsigned char PointerToLinenumbers[4];
  unsigned char NumberOfRelocations[2];
  unsigned char NumberOfLinenumbers[2];
  unsigned char Characteristics[4];
};
typedef struct Coff_section Coff_section;

struct Coff_symbol
{
  unsigned char Name[8];
  unsigned char Value[4];
  unsigned char SectionNumber[2];
  unsigned char Type[2];
  unsigned char StorageClass[1];
  unsigned char NumberOfAuxSymbols[1];
};
typedef struct Coff_symbol Coff_symbol;

struct Coff_aux_sym_file
{
  unsigned char FileName[18];
};
typedef struct Coff_aux_sym_file Coff_aux_sym_file;

struct Coff_aux_sym_section
{
  unsigned char Length[4];
  unsigned char NumberOfRelocations[2];
  unsigned char NumberOfLineNumbers[2];
  unsigned char Checksum[4];
  unsigned char Number[2];
  unsigned char Selection[1];
  unsigned char Unused[3];
};
typedef struct Coff_aux_sym_section Coff_aux_sym_section;

/* Accessor macros for the above structures.  */

#define COFF_GET(struc,memb) \
  ((COFFENDIAN ? get_be : get_le) (&(struc)->memb[0], sizeof ((struc)->memb)))

#define COFF_PUT(struc,memb,val) \
  ((COFFENDIAN ? put_be : put_le) (&(struc)->memb[0], sizeof ((struc)->memb), val))

#define COFF_PUT_NDXSZ(struc,memb,val,ndx,sz) \
  ((COFFENDIAN ? put_be : put_le) (&(struc)->memb[ndx], sz, val))

/* In-memory file structures.  */

/* Forward declared structs.  */

struct lto_coff_data;
struct lto_coff_section;
struct lto_coff_file;

/* Section data in output files is made of these.  */

struct lto_coff_data
{
  /* Pointer to data block.  */
  void *d_buf;

  /* Size of data block.  */
  ssize_t d_size;

  /* Next data block for this section.  */
  struct lto_coff_data *next;
};
typedef struct lto_coff_data lto_coff_data;

/* This struct tracks the data for a section.  */

struct lto_coff_section
{
  /* Singly-linked list of section's data blocks.  */
  lto_coff_data *data_chain;

  /* Offset in string table of name.  */
  size_t strtab_offs;

  /* Section type: 0 = real, 1 = dummy.  */
  size_t type;

  /* Section name.  */
  const char *name;

#if COFF_ALIGNMENT > 1
  /* Number of trailing padding bytes needed.  */
  ssize_t pad_needed;
#endif

  /* Raw section header data.  */
  Coff_section coffsec;

  /* Next section for this file.  */
  struct lto_coff_section *next;
};
typedef struct lto_coff_section lto_coff_section;

/* A COFF file.  */

struct lto_coff_file 
{
  /* The base information.  */
  lto_file base;

  /* Common file members:  */

  /* The system file descriptor for the file.  */
  int fd;

  /* The file's overall header.  */
  Coff_header coffhdr;

  /* All sections in a singly-linked list.  */
  lto_coff_section *section_chain;

  /* Readable file members:  */

  /* File total size.  */
  off_t file_size;

  /* String table file offset, relative to base.offset.  */
  off_t strtab_offs;

  /* Writable file members:  */

  /* The currently active section.  */
  lto_coff_section *scn;

  /* The output stream for section header names.  */
  struct lto_output_stream *shstrtab_stream;

  /* Linked list of data which must be freed *after* the file has been
     closed.  This is an annoying limitation of libelf.  Which has been
     faithfully reproduced here.  */
  struct lto_char_ptr_base *data;
};
typedef struct lto_coff_file lto_coff_file;

/* Data hunk iterator.  */

#define COFF_FOR_ALL_DATA(sec,var) \
  for (var = sec->data_chain; var; var = var->next)

/* Section list iterator.  */

#define COFF_FOR_ALL_SECTIONS(file,var) \
  for (var = file->section_chain; var; var = var->next)

/* Very simple endian-ness layer.  */

#ifndef COFFENDIAN
#define COFFENDIAN (BYTES_BIG_ENDIAN)
#endif

static inline unsigned int
get_2_le (const unsigned char *ptr)
{
  return ptr[0] | (ptr[1] << 8);
}

static inline unsigned int
get_4_le (const unsigned char *ptr)
{
  return ptr[0] | (ptr[1] << 8) | (ptr[2] << 16) | (ptr[3] << 24);
}

static inline unsigned int
get_2_be (const unsigned char *ptr)
{
  return ptr[1] | (ptr[0] << 8);
}

static inline unsigned int
get_4_be (const unsigned char *ptr)
{
  return ptr[3] | (ptr[2] << 8) | (ptr[1] << 16) | (ptr[0] << 24);
}

static inline unsigned int
get_be (const unsigned char *ptr, size_t size)
{
  gcc_assert (size == 4 || size == 2);
  return (size == 2) ? get_2_be (ptr) : get_4_be (ptr);
}

static inline unsigned int
get_le (const unsigned char *ptr, size_t size)
{
  gcc_assert (size == 4 || size == 2);
  return (size == 2) ? get_2_le (ptr) : get_4_le (ptr);
}

static inline void
put_2_le (unsigned char *ptr, unsigned int data)
{
  ptr[0] = data & 0xff;
  ptr[1] = (data >> 8) & 0xff;
}

static inline void
put_4_le (unsigned char *ptr, unsigned int data)
{
  ptr[0] = data & 0xff;
  ptr[1] = (data >> 8) & 0xff;
  ptr[2] = (data >> 16) & 0xff;
  ptr[3] = (data >> 24) & 0xff;
}

static inline void
put_2_be (unsigned char *ptr, unsigned int data)
{
  ptr[1] = data & 0xff;
  ptr[0] = (data >> 8) & 0xff;
}

static inline void
put_4_be (unsigned char *ptr, unsigned int data)
{
  ptr[3] = data & 0xff;
  ptr[2] = (data >> 8) & 0xff;
  ptr[1] = (data >> 16) & 0xff;
  ptr[0] = (data >> 24) & 0xff;
}

static inline void
put_le (unsigned char *ptr, size_t size, unsigned int data)
{
  gcc_assert (size == 4 || size == 2);
  (void) (size == 2 ? put_2_le : put_4_le) (ptr, data);
}

static inline void
put_be (unsigned char *ptr, size_t size, unsigned int data)
{
  gcc_assert (size == 4 || size == 2);
  (void) (size == 2 ? put_2_be : put_4_be) (ptr, data);
}

/* We use this for putting the string table size.  */

#define COFF_PUT4(ptr, data) \
  ((COFFENDIAN ? put_4_be : put_4_le) (ptr, data))


#endif /* LTO_COFF_H */
