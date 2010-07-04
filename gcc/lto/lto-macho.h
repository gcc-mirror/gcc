/* LTO routines for Mach-O object files.
   Copyright 2010 Free Software Foundation, Inc.
   Contributed by Steven Bosscher.

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

#ifndef LTO_MACH_O_H
#define LTO_MACH_O_H

/* On-disk file structures.  */

/* Mach-O header (32 bits version).  */
struct mach_o_header_32
{
  unsigned char magic[4];	/* Magic number.  */
  unsigned char cputype[4];	/* CPU that this object is for.  */
  unsigned char cpusubtype[4];	/* CPU subtype.  */
  unsigned char filetype[4];	/* Type of file.  */
  unsigned char ncmds[4];	/* Number of load commands.  */
  unsigned char sizeofcmds[4];	/* Total size of load commands.  */
  unsigned char flags[4];	/* Flags for special featues.  */
};
typedef struct mach_o_header_32 mach_o_header_32;

/* Mach-O header (64 bits version).  */
struct mach_o_header_64
{
  unsigned char magic[4];	/* Magic number.  */
  unsigned char cputype[4];	/* CPU that this object is for.  */
  unsigned char cpusubtype[4];	/* CPU subtype.  */
  unsigned char filetype[4];	/* Type of file.  */
  unsigned char ncmds[4];	/* Number of load commands.  */
  unsigned char sizeofcmds[4];	/* Total size of load commands.  */
  unsigned char flags[4];	/* Flags for special featues.  */
  unsigned char reserved[4];	/* Reserved.  Duh.  */
};
typedef struct mach_o_header_64 mach_o_header_64;

/* Magic number.  */
#define MACH_O_MH_MAGIC			0xfeedface
#define MACH_O_MH_CIGAM			0xcefaedfe
#define MACH_O_MH_MAGIC_64		0xfeedfacf
#define MACH_O_MH_CIGAM_64		0xcffaedfe

/* Supported CPU types.  */
#define MACH_O_CPU_TYPE_I386		7
#define MACH_O_CPU_TYPE_X86_64		7 + 0x1000000
#define MACH_O_CPU_TYPE_POWERPC		18
#define MACH_O_CPU_TYPE_POWERPC_64	18 + 0x1000000

/* Supported file types.  */
#define MACH_O_MH_OBJECT		0x01

/* Mach-O load command data structure.  */
struct mach_o_load_command
{
  unsigned char cmd[4];		/* The type of load command.  */
  unsigned char cmdsize[4];	/* Size in bytes of load command data structure.  */
};
typedef struct mach_o_load_command mach_o_load_command;

/* Supported load commands.  We support only the segment load commands.  */
#define MACH_O_LC_SEGMENT		0x01
#define MACH_O_LC_SEGMENT_64		0x19

/* LC_SEGMENT load command.  */
struct mach_o_segment_command_32
{
  unsigned char cmd[4];		/* The type of load command (LC_SEGMENT).  */
  unsigned char cmdsize[4];	/* Size in bytes of load command data structure.  */
  unsigned char segname[16];	/* Name of this segment.  */
  unsigned char vmaddr[4];	/* Virtual memory address of this segment.  */
  unsigned char vmsize[4];	/* Size there, in bytes.  */
  unsigned char fileoff[4];	/* Offset in bytes of the data to be mapped.  */
  unsigned char filesize[4];	/* Size in bytes on disk.  */
  unsigned char maxprot[4];	/* Maximum permitted vmem protection.  */
  unsigned char initprot[4];	/* Initial vmem protection.  */
  unsigned char nsects[4];	/* Number of sections in this segment.  */
  unsigned char flags[4];	/* Flags that affect the loading.  */
};
typedef struct mach_o_segment_command_32 mach_o_segment_command_32;

/* LC_SEGMENT_64 load command.  Only nsects matters for us, really.  */
struct mach_o_segment_command_64
{
  unsigned char cmd[4];		/* The type of load command (LC_SEGMENT_64).  */
  unsigned char cmdsize[4];	/* Size in bytes of load command data structure.  */
  unsigned char segname[16];	/* Name of this segment.  */
  unsigned char vmaddr[8];	/* Virtual memory address of this segment.  */
  unsigned char vmsize[8];	/* Size there, in bytes.  */
  unsigned char fileoff[8];	/* Offset in bytes of the data to be mapped.  */
  unsigned char filesize[8];	/* Size in bytes on disk.  */
  unsigned char maxprot[4];	/* Maximum permitted vmem protection.  */
  unsigned char initprot[4];	/* Initial vmem protection.  */
  unsigned char nsects[4];	/* Number of sections in this segment.  */
  unsigned char flags[4];	/* Flags that affect the loading.  */
};
typedef struct mach_o_segment_command_64 mach_o_segment_command_64;

/* A Mach-O 32-bits section.  */
struct mach_o_section_32
{
  unsigned char sectname[16];	/* Section name.  */
  unsigned char segname[16];	/* Segment that the section belongs to.  */
  unsigned char addr[4];	/* Address of this section in memory.  */
  unsigned char size[4];	/* Size in bytes of this section.  */
  unsigned char offset[4];	/* File offset of this section.  */
  unsigned char align[4];	/* log2 of this section's alignment.  */
  unsigned char reloff[4];	/* File offset of this section's relocs.  */
  unsigned char nreloc[4];	/* Number of relocs for this section.  */
  unsigned char flags[4];	/* Section flags/attributes.  */
  unsigned char reserved1[4];
  unsigned char reserved2[4];
};
typedef struct mach_o_section_32 mach_o_section_32;

/* A Mach-O 64-bits section.  */
struct mach_o_section_64
{
  unsigned char sectname[16];	/* Section name.  */
  unsigned char segname[16];	/* Segment that the section belongs to.  */
  unsigned char addr[8];	/* Address of this section in memory.  */
  unsigned char size[8];	/* Size in bytes of this section.  */
  unsigned char offset[4];	/* File offset of this section.  */
  unsigned char align[4];	/* log2 of this section's alignment.  */
  unsigned char reloff[4];	/* File offset of this section's relocs.  */
  unsigned char nreloc[4];	/* Number of relocs for this section.  */
  unsigned char flags[4];	/* Section flags/attributes.  */
  unsigned char reserved1[4];
  unsigned char reserved2[4];
  unsigned char reserved3[4];
};
typedef struct mach_o_section_64 mach_o_section_64;

/* Flags for Mach-O sections.  LTO sections are marked with S_ATTR_DEBUG
   to instruct the linker to ignore the sections.  */
#define MACH_O_S_ATTR_DEBUG			0x02000000

/* In-memory file structures.  */

/* Section data in output files is made of these.  */
struct lto_mach_o_data_d
{
  /* Pointer to data block.  */
  void *d_buf;

  /* Size of data block.  */
  ssize_t d_size;

  /* Next data block for this section.  */
  struct lto_mach_o_data_d *next;
};
typedef struct lto_mach_o_data_d *lto_mach_o_data;

/* This struct tracks the data for a section.  */
struct lto_mach_o_section_d
{
  /* Singly-linked list of section's data blocks.  */
  lto_mach_o_data data_chain;

  /* Offset in string table of the section name.  */
  size_t strtab_offs;

  /* Section name.  */
  const char *name;

  /* Number of trailing padding bytes needed.  */
  ssize_t pad_needed;

  /* Raw section header data.  */
  size_t section_size;
  union {
    struct {
      char sectname[16];
      char segname[16];
    } section;
    mach_o_section_32 section_32;
    mach_o_section_64 section_64;
  } u;

  /* Next section for this file.  */
  struct lto_mach_o_section_d *next;
};
typedef struct lto_mach_o_section_d *lto_mach_o_section;
DEF_VEC_P (lto_mach_o_section);
DEF_VEC_ALLOC_P (lto_mach_o_section, heap);

/* A Mach-O file.  */
struct lto_mach_o_file_d
{
  /* The base information.  */
  lto_file base;

  /* Common file members:  */

  /* The system file descriptor for the file.  */
  int fd;

  /* The file's overall header.  */
  union {
    /* We make use here of the fact that section_32 and section_64
       have the same layout (except for section_64.reserved3).  We
       read the struct of proper size, but only address the first
       member of this union.  */
    mach_o_header_64 header;
    mach_o_header_32 header_32;
    mach_o_header_64 header_64;
  } u;

  /* All sections in a varray.  */
  VEC(lto_mach_o_section, heap) *section_vec;

  /* Readable file members:  */

  /* File total size.  */
  off_t file_size;

  /* True if this file is open for writing.  */
  bool writable;

  /* Section containing the __section_names section.  */
  lto_mach_o_section section_names_section;

  /* Writable file members:  */

  /* The currently active section.  */
  lto_mach_o_section scn;

  /* Linked list of data which must be freed *after* the file has been
     closed.  This is an annoying limitation of libelf.  Which has been
     faithfully reproduced here.  */
  struct lto_char_ptr_base *data;
};
typedef struct lto_mach_o_file_d lto_mach_o_file;

#endif /* LTO_MACH_O_H */

