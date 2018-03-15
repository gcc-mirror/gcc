/* -*- C++ -*- modules.  Experimental!
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   Written by Nathan Sidwell <nathan@acm.org> while at FaceBook

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Comments in this file have a non-negligible chance of being wrong
   or at least inaccurate.  Due to (a) my misunderstanding, (b)
   ambiguities that I have interpretted differently to original intent
   (c) changes in the specification, (d) my poor wording, (e) source
   changes.  */

/* (Incomplete) Design Notes

   Each namespace-scope and container-like decl has a MODULE_OWNER.
   This is MODULE_NONE for the global module, MODULE_PURVIEW for the
   current TU and >= MODULE_IMPORT_BASE for imported modules.  During
   compilation, the current module's owner will change from
   MODULE_NONE to MODULE_PURVIEW at the module-declaration.  Any decl
   with MODULE_OWNER != MODULE_NONE is in a module purview.  Builtins
   are always MODULE_NONE. (Note that this is happenstance for decls
   lacking DECL_LANG_SPECIFIC.)

   The decls for a particular module are held located in a sparse
   array hanging off the ns-level binding of the name.  For imported
   modules, the array slot is the same as the module owner.  For the
   current TU, it is MODULE_SLOT_CURRENT.  To keep track of global
   module entities made visible via multiple imports, we use
   MODULE_SLOT_GLOBAL.  That slot is never searched during name
   lookup.  The two reserved slots are always present.  If a name is
   bound only in the current TU, there is a regular binding, not an
   array.  We convert on demand.

   There is only one instance of each extern-linkage namespace.  It
   appears in every module slot that makes it visible.  It also
   appears in MODULE_SLOT_GLOBAL. (it is an ODR violation if they
   collide with some other global module entity.) FIXME:Not yet implemented

   A module import can bring in entities that cannot be found by name
   lookup.  You use decltype tricks to get at it.  I am not sure
   whether these should be DECL_HIDDEN for that import's binding, or
   should just not be in the symbol table.

   WARNING: Lazy loading not implemented:

   A module interface compilation produces a Binary Module Interface
   (BMI).  I use ELROND format, which allows a bunch of named sections
   containing arbitrary data.  Although I don't defend against
   actively hostile BMIs, there is some checksumming involved to
   veryify data integrity.  When dumping out an interface, we generate
   a list of all the namespace-scope DECLS that are needed.  From that
   we determine the strongly connected components (SCC) within this
   TU.  Each SCC is dumped to a separate section of the BMI.  We
   generate a binding table section, mapping each namespace&name to an
   SCC.  This allows lazy loading.

   References to imported decls are done via indexing the imported
   module's decl list.

   References to global-module decls are either via an index to an
   imported module that happens to also make the decl available.  Or
   by value if there is no such import.

   There can be no SCCs containing both current-module and
   global-module decls.  Construction of such an SCC would require the
   global-module decl to reference the current-module decl, and that
   is not possible.

   Classes used:

   data - buffer with CRC capability

   dumper - logger

   elf - ELROND format
   elf_in : elf - ELROND reader
   elf_out : elf - ELROND writer

   bytes - data streamer
   bytes_in : bytes - scalar reader
   bytes_out : bytes - scalar writer

   trees_in : bytes_in - tree reader
   trees_out : bytes_out - tree writer

   module_state - module object   */

/* MODULE_STAMP is a #define passed in from the Makefile.  When
   present, it is used for version stamping the binary files, and
   indicates experimentalness of the module system.  It is very
   experimental right now.  */
#ifndef MODULE_STAMP
#error "Stahp! What are you doing? This is not ready yet."
#define MODULE_STAMP 0
#endif

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "dumpfile.h"
#include "bitmap.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "cpplib.h"
#include "incpath.h"
#include "libiberty.h"
#include "version.h"
#include "tree-diagnostic.h"

/* Id for dumping module information.  */
int module_dump_id;

/* We have a few more special module owners.  */
#define MODULE_UNKNOWN (~0U)    /* Not yet known.  */

/* Mangling for module files.  */
#define MOD_FNAME_SFX ".nms" /* New Module System.  Honest.  */
#define MOD_FNAME_DOT '-' /* Dots convert to ... */

/* Prefix for section names.  */
#define MOD_SNAME_PFX ".gnu.c++"

/* Get the version of this compiler.  See above about MODULE_STAMP.  */

static inline int
get_version ()
{
  /* If the on-disk format changes, update the version number.  */
  int version = 20180101;

#if MODULE_STAMP
  /* MODULE_STAMP is a decimal encoding YYYYMMDDhhmm or YYYYMMDD in
     local timezone.  Using __TIME__ doesn't work very well with
     boostrapping!  */
  version = -(MODULE_STAMP > 2000LL * 10000 * 10000
	      ? int (MODULE_STAMP - 2000LL * 10000 * 10000)
	      : int (MODULE_STAMP - 2000LL * 10000) * 10000);
#endif
  return version;
}

/* Version to date.  Understand both experimental and released
   version dates.  */

static inline int version2date (int v)
{
  if (v < 0)
    return unsigned (-v) / 10000 + 20000000;
  else
    return v;
}

/* Version to time.  Only understand times when experimental.  */

static inline unsigned version2time (int v)
{
  if (MODULE_STAMP && v < 0)
    return unsigned (-v) % 10000;
  else
    return 0;
}

/* Format a version for user consumption.  Only attach time
   information for experimental builds.  */

typedef char verstr_t[32];
static void version2string (int version, verstr_t &out)
{
  unsigned date = version2date (version);
  unsigned time = version2time (version);
  if (MODULE_STAMP)
    sprintf (out, "%04u/%02u/%02u-%02u:%02u",
	     date / 10000, (date / 100) % 100, (date % 100),
	     time / 100, time % 100);
  else
    sprintf (out, "%04u/%02u/%02u%s",
	     date / 10000, (date / 100) % 100, (date % 100),
	     version < 0 ? " (experimental)": "");
}

/* Traits to has an arbitrary pointer into a hash table. Entries are
   not deletable, and removal is a noop (removal needed upon
   destruction).  */
template <typename T>
struct nodel_ptr_hash : pointer_hash<T>, typed_noop_remove <T *>
{
  /* Nothing is deletable.  Everything is insertable.  */
  static bool is_deleted (T *) { return false; }
  static void mark_deleted (T *) { gcc_unreachable (); }
};

/* Map from pointer to signed integer.   */
typedef simple_hashmap_traits<nodel_ptr_hash<void>, int> ptr_int_traits;
typedef hash_map<void *,signed,ptr_int_traits> ptr_int_hash_map;

/* A data buffer, using trailing array hack.  Provides CRC
   capability & buffer extension.  */

struct data {
  size_t size;
  char buffer[1];

private:
  unsigned calc_crc () const;

public:
  /* We store the CRC in the first 4 bytes, using host endianness.  */
  unsigned get_crc () const
  {
    return *(const unsigned *)&buffer[0];
  }
  void set_crc (unsigned *crc_ptr);
  bool check_crc () const;

public:
  /* new & delete semantics don't quite work.  */
  static data *extend (data *, size_t);
  static data *release (data *d)
  {
    free (d);
    return NULL;
  }
};

/* Calculate the crc32 of the buffer.  Note the CRC is stored in the
   first 4 bytes, so don't include them.  */

unsigned
data::calc_crc () const
{
  unsigned crc = 0;
  for (size_t ix = 4; ix < size; ix++)
    crc = crc32_byte (crc, buffer[ix]);
  return crc;
}

/* If CRC_PTR non-null, set the CRC of the buffer.  Mix the CRC into
   that pointed to by CRC_PTR.  Otherwise store zero.  */

void
data::set_crc (unsigned *crc_ptr)
{
  gcc_checking_assert (size >= 4);
  unsigned crc = 0;
  if (crc_ptr)
    {
      crc = calc_crc ();
      unsigned accum = *crc_ptr;
      /* Only mix the existing *CRC_PTR if it is non-zero.  */
      accum = accum ? crc32_unsigned (accum, crc) : crc;
      *crc_ptr = accum;
    }
  *(unsigned *)buffer = crc;
}

/* Verify the buffer's CRC is correct.  */

bool
data::check_crc () const
{
  if (size < 4)
    return false;

  unsigned c_crc = calc_crc ();
  if (c_crc != get_crc ())
    return false;

  return true;
}

/* Extend the buffer to size A.  It is either not allocated, or
   smaller than A.  Returns the new buffer object.  */

data *
data::extend (data *c, size_t a)
{
  gcc_checking_assert (!c || a > c->size);
  c = XRESIZEVAR (data, c, offsetof (data, buffer) + a);
  c->size = a;

  return c;
}

/* Encapsulated Lazy Records Of Named Declarations.
   Header: Stunningly Elf32_Ehdr-like
   Sections: Sectional data
     [1-N) : User data sections
     N .strtab  : strings, stunningly ELF STRTAB-like
   Index: Section table, stunningly ELF32_Shdr-like.   */

class elf {
protected:
  /* Constants used within the format.  */
  enum private_constants
    {
      /* File kind. */
      ET_NONE = 0,
      EM_NONE = 0,
      OSABI_NONE = 0,

      /* File format. */
      EV_CURRENT = 1,
      CLASS32 = 1,
      DATA2LSB = 1,
      DATA2MSB = 2,

      /* Section numbering.  */
      SHN_UNDEF = 0,
      SHN_LORESERVE = 0xff00,
      SHN_XINDEX = 0xffff,

      /* Section types.  */
      SHT_NONE = 0,  /* No contents.  */
      SHT_PROGBITS = 1, /* Random bytes.  */
      SHT_STRTAB = 3,  /* A string table.  */

      /* Section flags.  */
      SHF_NONE = 0x00, /* Nothing.  */
      SHF_STRINGS = 0x20,  /* NUL-Terminated strings.  */

      /* I really hope we do not get BMI files larger than 4GB.  */
      MY_CLASS = CLASS32,
      /* It is host endianness that is relevant.  */
      MY_ENDIAN = DATA2LSB
#ifdef WORDS_BIGENDIAN
		  ^ DATA2LSB ^ DATA2MSB
#endif
    };

public:
  /* Constants visible to users.  */
  enum public_constants
    {
      /* Special error codes.  Breaking layering a bit.  */
      E_BAD_DATA = -1,  /* Random unexpected data errors.  */
      E_BAD_IMPORT = -2 /* A nested import failed.  */
    };

protected:
  /* File identification.  On-disk representation.  */
  struct ident
  {
    uint8_t magic[4];
    uint8_t klass; /* 4:CLASS32 */
    uint8_t data; /* 5:DATA2[LM]SB */
    uint8_t version; /* 6:EV_CURRENT  */
    uint8_t osabi; /* 7:OSABI_NONE */
    uint8_t abiver; /* 8: 0 */
    uint8_t pad[7]; /* 9-15 */
  };
  /* File header.  On-disk representation.  */
  struct header
  {
    struct ident ident;
    uint16_t type; /* ET_NONE */
    uint16_t machine; /* EM_NONE */
    uint32_t version; /* EV_CURRENT */
    uint32_t entry; /* 0 */
    uint32_t phoff; /* 0 */
    uint32_t shoff; /* Section Header Offset in file */
    uint32_t flags; 
    uint16_t ehsize; /* ELROND Header SIZE -- sizeof (header) */
    uint16_t phentsize; /* 0 */
    uint16_t phnum;    /* 0 */
    uint16_t shentsize; /* Section Header SIZE -- sizeof (section) */
    uint16_t shnum;  /* Section Header NUM */
    uint16_t shstrndx; /* Section Header STRing iNDeX */
  };
  /* File section.  On-disk representation.  */
  struct section
  {
    uint32_t name; /* String table offset.  */
    uint32_t type; /* SHT_* */
    uint32_t flags; /* SHF_* */
    uint32_t addr; /* 0 */
    uint32_t offset;  /* OFFSET in file */
    uint32_t size; /* SIZE of section */
    uint32_t link; /* 0 */
    uint32_t info; /* 0 */
    uint32_t addralign; /* 0 */
    uint32_t entsize; /* ENTry SIZE, usually 0 */
  };

protected:
  /* Internal section.  NOT the on-disk representation. */
  struct isection
  {
    unsigned short type;   /* Type of section.  */
    unsigned short flags;  /* Section flags.  */
    unsigned name;   /* Index into string section.  */
    unsigned offset; /* File offset.  */
    unsigned size;   /* Size of data.  */
  };

protected:
  FILE *stream;   /* File stream we're reading or writing.  */
  vec<isection, va_heap, vl_embed> *sections;  /* Section table.  */
  int err; 		/* Sticky error code.  */

public:
  /* Construct from STREAM.  E is errno if STREAM NULL.  */
  elf (FILE *stream, int e)
    :stream (stream), sections (NULL), err (stream ? 0 : e)
  {}
  ~elf ()
  {
    gcc_checking_assert (!stream && !sections);
  }

public:
  /* Return the current error.  Zero if there is none.  */
  int get_error () const
  {
    return err;
  }
  /* Set the error, unless it's already been set.  */
  void set_error (int e = E_BAD_DATA)
  {
    if (!err)
      err = e;
  }

public:
  /* Begin reading/writing file.  Return false on error.  */
  bool begin () const
  {
    gcc_checking_assert (!sections);
    return !get_error ();
  }
  /* Finish reading/writing file.  Return NULL or error string.  */
  const char *end ();
};

/* Close the stream and return NULL, or error string.  */

const char *
elf::end ()
{
  /* Close the stream and free the section table.  */
  if (stream && fclose (stream))
    set_error (errno);
  stream = NULL;
  vec_free (sections);
  sections = NULL;

  /* Return an error message, or NULL.  */
  switch (get_error ())
    {
    case 0:
      return NULL;
    case E_BAD_DATA:
      return "Bad file data";
    case E_BAD_IMPORT:
      return "Bad import dependency";
    default:
      return xstrerror (get_error ());
    }
}

/* ELROND reader.  */

class elf_in : public elf {
  typedef elf parent;

protected:
  data *strings;  /* String table.  */

  public:
  elf_in (FILE *s, int e)
    :parent (s, e), strings (NULL)
  {
  }
  ~elf_in ()
  {
    gcc_checking_assert (!strings);
  }

protected:
  bool read (void *, size_t);

public:
  /* Read section by number.  */
  data *read (unsigned snum);
  /* Find section by name.  */
  unsigned find (const char *name, unsigned type = SHT_PROGBITS);

public:
  /* Release the string table, when we're done with it.  */
  void release ()
  {
    strings = data::release (strings);
  }

public:
  bool begin ();
  const char *end ()
  {
    release ();
    return parent::end ();
  }

public:
  /* Return string name at OFFSET.  Checks OFFSET range.  Always
     returns non-NULL.  */
  const char *name (unsigned offset)
  {
    return &strings->buffer[offset < strings->size ? offset : 0];
  }
};

/* ELROND writer.  */

class elf_out : public elf {
  typedef elf parent;
public:
  /* Builder for string table.  */
  class strtab
  {
  private:
    ptr_int_hash_map ident_map;		/* Map of IDENTIFIERS to offsets. */
    vec<tree, va_gc> *idents;		/* Ordered vector.  */
    vec<const char *, va_gc> *literals; /* Ordered vector.  */
    unsigned size;			/* Next offset.  */

  public:
    strtab (unsigned size = 50)
      :ident_map (size), idents (NULL), literals (NULL), size (0)
    {
      vec_safe_reserve (idents, size);
      vec_safe_reserve (literals, 10);
      name ("");
    }
    ~strtab ()
    {
      vec_free (idents);
      vec_free (literals);
    }

  public:
    /* IDENTIFIER to offset.  */
    unsigned name (const_tree ident);
    /* String literal to offset.  */
    unsigned name (const char *literal);
    /* Write out the string table.  */
    unsigned write (elf_out *out);
  };

private:
  strtab strings;   /* String table.  */

public:
  elf_out (FILE *s, int e)
    :parent (s, e), strings (500)
  {
  }

protected:
  uint32_t pad ();
  unsigned add (unsigned type, unsigned name = 0,
		unsigned off = 0, unsigned size = 0, unsigned flags = SHF_NONE);
  bool write (const void *, size_t);

public:
  /* IDENTIFIER to strtab offset.  */
  unsigned name (const_tree ident)
  {
    return strings.name (ident);
  }
  /* String literal to strtab offset.  */
  unsigned name (const char *n)
  {
    return strings.name (n);
  }

public:
  /* Add a section with contents or strings.  */
  unsigned add (bool strings_p, unsigned name, const data *);

public:
  /* Begin and end writing.  */
  bool begin ();
  const char *end ();
};

/* Read at current position into BUFFER.  Return true on success.  */

bool
elf_in::read (void *buffer, size_t size)
{
  if (fread (buffer, 1, size, stream) != size)
    {
      set_error (errno);
      return false;
    }
  return true;
}

/* Read section SNUM.  Return data buffer, or NULL on error.  */

data *
elf_in::read (unsigned snum)
{
  if (!snum || snum >= sections->length ())
    return NULL;
  const isection *sec = &(*sections)[snum];
  if (fseek (stream, sec->offset, SEEK_SET))
    {
      set_error (errno);
      return NULL;
    }

  data *b = data::extend (NULL, sec->size);
  if (read (b->buffer, b->size))
    return b;
  b = data::release (b);
  return NULL;
}

/* Find a section NAME and TYPE.  Return section number or 0 on
   failure.  */

unsigned
elf_in::find (const char *sname, unsigned type)
{
  unsigned snum = sections->length ();
  while (--snum)
    {
      const isection *isec = &(*sections)[snum];

      if (isec->type == type && !strcmp (sname, name (isec->name)))
	return snum;
    }

  return 0;
}

/* Begin reading file.  Verify header.  Pull in section and string
   tables.  Return true on success.  */

bool
elf_in::begin ()
{
  if (!parent::begin ())
    return false;

  header header;
  if (fseek (stream, 0, SEEK_SET)
      || !read (&header, sizeof (header)))
    return false;
  if (header.ident.magic[0] != 0x7f
      || header.ident.magic[1] != 'E'
      || header.ident.magic[2] != 'L'
      || header.ident.magic[3] != 'F')
    {
      error ("not Encapsulated Lazy Records of Named Declarations");
      return false;
    }

  /* We expect a particular format -- the ELF is not intended to be
     distributable.  */
  if (header.ident.klass != MY_CLASS
      || header.ident.data != MY_ENDIAN
      || header.ident.version != EV_CURRENT)
    {
      error ("unexpected encapsulation format");
      return false;
    }

  /* And the versioning isn't target-specific.  */
  if (header.type != ET_NONE
      || header.machine != EM_NONE
      || header.ident.osabi != OSABI_NONE)
    {
      error ("unexpected encapsulation type");
      return false;
    }

  if (!header.shoff || !header.shnum
      || header.shentsize != sizeof (section))
    {
      error ("section table missing or wrong format");
      return false;
    }
  if (fseek (stream, header.shoff, SEEK_SET))
    {
    section_table_fail:
      set_error (errno);
      error ("cannot read section table");
      return false;
    }

  unsigned strndx = header.shstrndx;
  unsigned shnum = header.shnum;
  vec_alloc (sections, shnum);
  for (unsigned ix = 0; ix != shnum; ix++)
    {
      section section;
      if (fread (&section, 1, sizeof (section), stream) != sizeof (section))
	goto section_table_fail;

      if (!ix)
	{
	  /* Section[0] is where escape values might be stored.  */
	  if (strndx == SHN_XINDEX)
	    strndx = section.link;
	  if (shnum == SHN_XINDEX)
	    {
	      shnum = section.size;
	      section.size = 0;
	      if (!shnum)
		goto section_table_fail;
	    }
	  vec_safe_reserve (sections, shnum, true);
	}

      isection isection;
      isection.type = section.type;
      isection.name = section.name;
      isection.offset = section.offset;
      isection.size = section.size;
      sections->quick_push (isection);
    }

  if (strndx)
    {
      strings = read (strndx);
      /* The string table should be at least one byte, with NUL chars
	 at either end.  */
      if (strings && !(strings->size && !strings->buffer[0]
		       && !strings->buffer[strings->size - 1]))
	strings = data::release (strings);
    }

  if (!strings)
    {
      /* Create a default string table.  */
      strings = data::extend (NULL, 1);
      strings->buffer[0] = 0;
    }

  return true;
}

/* Map IDENTIFIER IDENT to strtab offset.  Inserts into strtab if not
   already there.  */

unsigned
elf_out::strtab::name (const_tree ident)
{
  unsigned result;
  bool existed;
  int *slot = &ident_map.get_or_insert (const_cast <tree> (ident), &existed);
  if (existed)
    result = *slot;
  else
    {
      *slot = (int)size;
      vec_safe_push (idents, const_cast <tree> (ident));
      result = size;
      size += IDENTIFIER_LENGTH (ident) + 1;
    }
  return result;
}

/* Map LITERAL to strtab offset.  Does not detect duplicates and
   expects LITERAL to remain live until strtab is written out.  */

unsigned
elf_out::strtab::name (const char *literal)
{
  vec_safe_push (idents, NULL_TREE);
  vec_safe_push (literals, literal);
  unsigned result = size;
  size += strlen (literal) + 1;
  return result;
}

/* Write the string table to ELF.  section name is .strtab.  */

unsigned
elf_out::strtab::write (elf_out *elf)
{
  unsigned off = elf->pad ();
  if (!off)
    return 0;

  unsigned shname = name (".strtab");
  unsigned lit_ix = 0;
  for (unsigned ix = 0; ix != idents->length (); ix++)
    {
      unsigned len;
      const char *ptr;

      if (const_tree ident = (*idents)[ix])
	{
	  len = IDENTIFIER_LENGTH (ident);
	  ptr = IDENTIFIER_POINTER (ident);
	}
      else
	{
	  ptr = (*literals)[lit_ix++];
	  len = strlen (ptr);
	}
      if (!elf->write (ptr, len + 1))
	return 0;
    }

  gcc_assert (lit_ix == literals->length ());
  return elf->add (SHT_STRTAB, shname, off, size, SHF_STRINGS);
}

/* Padd file to the next 4 byte boundary.  Return the file position or
   zero on error.  (We never need this at the start of file.  */

uint32_t
elf_out::pad ()
{
  long off = ftell (stream);
  if (off < 0)
    off = 0;
  else if (unsigned padding = off & 3)
    {
      /* Align the section on disk, should help the necessary copies.  */
      unsigned zero = 0;
      padding = 4 - padding;
      off += padding;
      if (fwrite (&zero, 1, padding, stream) != padding)
	off = 0;
    }
  if (!off)
    set_error (errno);

  return (uint32_t)off;
}

/* Add section to file.  Return section number.  TYPE & NAME identify
   the section.  OFF and SIZE identify the file location of its
   data.  FLAGS contains additional info.  */

unsigned
elf_out::add (unsigned type, unsigned name, unsigned off, unsigned size,
	      unsigned flags)
{
  isection sec;

  sec.type = type;
  sec.flags = flags;
  sec.name = name;
  sec.offset = off;
  sec.size = size;

  unsigned snum = sections->length ();
  vec_safe_push (sections, sec);
  return snum;
}

/* Write BUFFER of SIZE bytes at current file position.  Return true
   on success.  */

bool
elf_out::write (const void *buffer, size_t size)
{
  if (fwrite (buffer, 1, size, stream) != size)
    {
      set_error (errno);
      return false;
    }

  return true;
}

/* Write data and add section.  STRINGS_P is true for a string
   section, false for PROGBITS.  NAME identifies the section (0 is the
   empty name).  DATA is the contents.  Return section number or 0 on
   failure (0 is the undef section).  */

unsigned
elf_out::add (bool strings_p, unsigned name, const data *data)
{
  uint32_t off = pad ();
  if (!off)
    return 0;
  /* DATA will have included space for a CRC.  We don't care about tht
     for string sections.  */
  uint32_t disp = strings_p ? 4 : 0;
  if (!write (data->buffer + disp, data->size - disp))
    return 0;

  return add (strings_p ? SHT_STRTAB : SHT_PROGBITS,
	      name, off, data->size - disp,
	      strings_p ? SHF_STRINGS : SHF_NONE);
}

/* Begin writing the file.  Initialize the section table and write an
   empty header.  Return false on failure.  */

bool
elf_out::begin ()
{
  if (!parent::begin ())
    return false;

  vec_alloc (sections, 10);

  /* Create the UNDEF section.  */
  add (SHT_NONE);

  /* Write an empty header.  */
  header header;
  memset (&header, 0, sizeof (header));
  return write (&header, sizeof (header));
}

/* Finish writing the file.  Write out the string & section tables.
   Fill in the header.  Return error string or NULL on success.  */

const char *
elf_out::end ()
{
  /* Write the string table.  */
  unsigned strndx = strings.write (this);

  uint32_t shoff = pad ();
  unsigned shnum = sections->length ();

  /* Write section table */
  for (unsigned ix = 0; ix != sections->length (); ix++)
    {
      const isection *isec = &(*sections)[ix];
      section section;
      memset (&section, 0, sizeof (section));
      section.name = isec->name;
      section.type = isec->type;
      section.offset = isec->offset;
      section.size = isec->size;
      section.flags = isec->flags;
      section.entsize = 0;
      if (isec->flags & SHF_STRINGS)
	section.entsize = 1;

      if (!ix)
	{
	  /* Store escape values in section[0].  */
	  if (strndx >= SHN_LORESERVE)
	    section.link = strndx;
	  if (shnum >= SHN_LORESERVE)
	    section.size = shnum;
	}

      if (!write (&section, sizeof (section)))
	goto out;
    }

  /* Write header.  */
  if (fseek (stream, 0, SEEK_SET))
    {
      set_error (errno);
      goto out;
    }

  /* Write the correct header now.  */
  header header;
  memset (&header, 0, sizeof (header));
  header.ident.magic[0] = 0x7f;
  header.ident.magic[1] = 'E';	/* Elrond */
  header.ident.magic[2] = 'L';	/* is an */
  header.ident.magic[3] = 'F';	/* elf.  */
  header.ident.klass = MY_CLASS;
  header.ident.data =  MY_ENDIAN;
  header.ident.version = EV_CURRENT;
  header.ident.osabi = OSABI_NONE;
  header.type = ET_NONE;
  header.machine = EM_NONE;
  header.version = EV_CURRENT;
  header.shoff = shoff;
  header.ehsize = sizeof (header);
  header.shentsize = sizeof (section);
  header.shnum = shnum >= SHN_LORESERVE ? unsigned (SHN_XINDEX) : shnum;
  header.shstrndx = strndx >= SHN_LORESERVE ? unsigned (SHN_XINDEX) : strndx;

  write (&header, sizeof (header));

 out:
  return parent::end ();
}

/* Byte streamer base.   Buffer with read/write position and smarts
   for single bits.  */

class bytes {
protected:
  struct data *data;	/* Buffer being read/written.  */
  size_t pos;		/* Position in buffer.  */
  uint32_t bit_val;	/* Bit buffer.  */
  unsigned bit_pos;	/* Next bit in bit buffer.  */

public:
  bytes ()
    :data (NULL), pos (4), bit_val (0), bit_pos (0)
  {}
  ~bytes () 
  {
    gcc_checking_assert (!data);
  }

protected:
  /* Maximum bytes needed for a SIZE-byte integer in sleb or uleb
     encodings.  Each 7 input bytes need an additional byte of output,
     rounded up.  */
  static unsigned leb_bytes (unsigned size)
  {
    return size + (size + 6) / 7;
  }

protected:
  /* Begin streaming.  Set buffer postion for crc  */
  void begin ()
  {
    gcc_checking_assert (!data && pos == 4);
  }
  /* Complete streaming.  Release the buffer.  */
  void end ()
  {
    data = data::release (data);
  }

protected:
  /* Finish bit packet.  Rewind the bytes not used.  */
  unsigned bit_flush ()
  {
    gcc_assert (bit_pos);
    unsigned bytes = (bit_pos + 7) / 8;
    unuse (4 - bytes);
    bit_pos = 0;
    bit_val = 0;
    return bytes;
  }

protected:
  /* Consume BYTES bytes.  Return pointer to start of used bytes.
     Does NOT check if the bytes are available.  */
  char *use (unsigned bytes)
  {
    char *res = &data->buffer[pos];
    pos += bytes;
    return res;
  }
  /* Rewind BYTES bytes.  */
  void unuse (unsigned bytes)
  {
    pos -= bytes;
  }
};

/* Byte stream reader.  */

class bytes_in : public bytes {
  typedef bytes parent;

protected:
  bool overrun;  /* Sticky read-too-much flag.  */

public:
  bytes_in ()
    : parent (), overrun (false)
  {
  }
  ~bytes_in ()
  {
  }

public:
  /* Begin reading a named section.  */
  bool begin (elf_in *src, const char *name);
  /* Begin reading a numbered section with optional name.  */
  bool begin (elf_in *src, unsigned, const char * = NULL);
  /* Complete reading a buffer.  Propagate errors and return true on
     success.  */
  bool end (elf_in *src)
  {
    if (more_p ())
      set_overrun ();
    if (overrun)
      src->set_error ();
    parent::end ();
    return !overrun;
  }
  /* Return true if there is unread data.  */
  bool more_p () const
  {
    return pos != data->size;
  }
  /* Return the buffer's CRC.  */
  unsigned get_crc () const
  {
    return data->get_crc ();
  }

private:
  /* Consume BYTES bytes of data.  Check there is sufficient data.  if
     AVAIL is non-NULL, allow consuming less data.  Update *AVAIL with
     count of consumption.  Return pointer to consumed data.  */
  const char *use (unsigned bytes, unsigned *avail = NULL)
  {
    unsigned space = data->size - pos;
    if (space < bytes)
      {
	bytes = space;
	if (!avail)
	  {
	    overrun = true;
	    return NULL;
	  }
      }
    if (avail)
      *avail = bytes;
    return parent::use (bytes);
  }

public:
  /* Manipulate the overrun flag.  */
  bool get_overrun () const
  {
    return overrun;
  }
  void set_overrun ()
  {
    overrun = true;
  }

public:
  unsigned u32 ();  	/* Read uncompressed integer.  */

public:
  bool b ();	    	/* Read a bool.  */
  void bflush ();	/* Completed a block of bools.  */
private:
  void bfill ();	/* Get the next block of bools.  */

public:
  int c ();		/* Read a char.  */
  int i ();		/* Read a signed int.  */
  unsigned u ();	/* Read an unsigned int.  */
  size_t z ();		/* Read a size_t.  */
  HOST_WIDE_INT wi ();  /* Read a HOST_WIDE_INT.  */
  unsigned HOST_WIDE_INT wu (); /* Read an unsigned HOST_WIDE_INT.  */
  const char *str (size_t * = NULL); /* Read a string.  */
  const char *buf (size_t); /* Read a fixed-length buffer.  */
};

/* Byte stream writer.  */

class bytes_out : public bytes {
  typedef bytes parent;

public:
  bytes_out ()
    : parent ()
  {
  }
  ~bytes_out ()
  {
  }

public:
  /* Begin writing, reserve space for CRC.  */
  void begin ();
  /* Finish writing.  Spill to section by number.  */
  unsigned end (elf_out *, unsigned, unsigned *crc_ptr = NULL);

private:
  /* Generate space for S bytes, return pointer to them.  */
  char *use (unsigned S);

public:
  void u32 (unsigned);  /* Write uncompressed integer.  */

public:
  void b (bool);	/* Write bool.  */
  void bflush ();	/* Finish block of bools.  */

public:
  void c (unsigned char); /* Write unsigned char.  */
  void i (int);		/* Write signed int.  */
  void u (unsigned);	/* Write unsigned int.  */
  void z (size_t s);	/* Write size_t.  */
  void wi (HOST_WIDE_INT); /* Write HOST_WIDE_INT.  */
  void wu (unsigned HOST_WIDE_INT);  /* Write unsigned HOST_WIDE_INT.  */
  void str (const char *, size_t);  /* Write string of known length.  */
  void buf (const char *, size_t);  /* Write fixed length buffer.  */
  /* Format a NUL-terminated raw string.  */
  void printf (const char *, ...) ATTRIBUTE_PRINTF_2;

public:
  /* Dump instrumentation.  */
  static void instrument ();

protected:
  /* Instrumentation.  */
  static unsigned spans[4];
  static unsigned lengths[4];
  static int is_set;
};

/* Instrumentation.  */
unsigned bytes_out::spans[4];
unsigned bytes_out::lengths[4];
int bytes_out::is_set = -1;

/* Finish a set of bools.  */

void
bytes_out::bflush ()
{
  if (bit_pos)
    {
      u32 (bit_val);
      lengths[2] += bit_flush ();
    }
  spans[2]++;
  is_set = -1;
}

void
bytes_in::bflush ()
{
  if (bit_pos)
    bit_flush ();
}

/* When reading, we don't know how many bools we'll read in.  So read
   4 bytes-worth, and then rewind when flushing if we didn't need them
   all.  You can't have a block of bools closer than 4 bytes to the
   end of the buffer.  */

void
bytes_in::bfill ()
{
  bit_val = u32 ();
}

/* Emit BYTES bytes.  Extend buffer as necessary and return pointer to
   space.  */

char *
bytes_out::use (unsigned bytes)
{
  if (data->size < pos + bytes)
    data = data::extend (data, (pos + bytes) * 3/2);
  return parent::use (bytes);
}

/* Bools are packed into bytes.  You cannot mix bools and non-bools.
   You must call bflush before emitting another type.  So batch your
   bools.

   It may be worth optimizing for most bools being zero.  Some kind of
   run-length encoding?  */

void
bytes_out::b (bool x)
{
  if (is_set != x)
    {
      is_set = x;
      spans[x]++;
    }
  lengths[x]++;
  bit_val |= unsigned (x) << bit_pos++;
  if (bit_pos == 32)
    {
      u32 (bit_val);
      lengths[2] += bit_flush ();
    }
}

bool
bytes_in::b ()
{
  if (!bit_pos)
    bfill ();
  bool v = (bit_val >> bit_pos++) & 1;
  if (bit_pos == 32)
    bit_flush ();
  return v;
}

/* Exactly 4 bytes.  Used internally for bool packing and a few other
   places.  We can't simply use uint32_t because (a) alignment and
   (b) we need little-endian for the bool streaming rewinding to make
   sense.  */

void
bytes_out::u32 (unsigned val)
{
  char *ptr = use (4);
  ptr[0] = val;
  ptr[1] = val >> 8;
  ptr[2] = val >> 16;
  ptr[3] = val >> 24;
}

unsigned
bytes_in::u32 ()
{
  unsigned val = 0;
  if (const char *ptr = use (4))
    {
      val |= (unsigned char)ptr[0];
      val |= (unsigned char)ptr[1] << 8;
      val |= (unsigned char)ptr[2] << 16;
      val |= (unsigned char)ptr[3] << 24;
    }

  return val;
}

/* Chars are unsigned and written as single bytes. */

void
bytes_out::c (unsigned char v)
{
  *use (1) = v;
}

int
bytes_in::c ()
{
  int v = 0;
  if (const char *ptr = use (1))
    v = (unsigned char)ptr[0];
  return v;
}

/* Ints are written as sleb128.  When reading we must be careful to
   cope with slebs right at the end of the buffer.

   I suppose we could always write buffers to have sufficient tail
   padding?  */

void
bytes_out::i (int v)
{
  unsigned max = leb_bytes (sizeof (v));
  char *ptr = use (max);
  unsigned count = 0;

  int end = v < 0 ? -1 : 0;
  bool more;
  do
    {
      unsigned byte = v & 127;
      v >>= 6; /* Signed shift.  */
      more = v != end;
      ptr[count++] = byte | (more << 7);
      v >>= 1; /* Signed shift.  */
    }
  while (more);
  unuse (max - count);
}

int
bytes_in::i ()
{
  int v = 0;
  unsigned max = leb_bytes (sizeof (v));
  const char *ptr = use (max, &max);
  unsigned count = 0;

  unsigned bit = 0;
  unsigned byte;
  do
    {
      if (count == max)
	{
	  overrun = true;
	  return 0;
	}
      byte = ptr[count++];
      v |= (byte & 127) << bit;
      bit += 7;
    }
  while (byte & 128);
  unuse (max - count);

  if (byte & 0x40 && bit < sizeof (v) * 8)
    v |= ~(unsigned)0 << bit;

  return v;
}

/* Unsigned are written as uleb128.  */

void
bytes_out::u (unsigned v)
{
  unsigned max = leb_bytes (sizeof (v));
  char *ptr = use (max);
  unsigned count = 0;

  bool more;
  do
    {
      unsigned byte = v & 127;
      v >>= 7;
      more = v != 0;
      ptr[count++] = byte | (more << 7);
    }
  while (more);
  unuse (max - count);
}

unsigned
bytes_in::u ()
{
  unsigned v = 0;
  unsigned max = leb_bytes (sizeof (v));
  const char *ptr = use (max, &max);
  unsigned count = 0;

  unsigned bit = 0;
  unsigned byte;
  do
    {
      if (count == max)
	{
	  overrun = true;
	  return 0;
	}
      byte = ptr[count++];
      v |= (byte & 127) << bit;
      bit += 7;
    }
  while (byte & 128);
  unuse (max - count);

  return v;
}

/* Wide Ints are written as sleb128.  */

void
bytes_out::wi (HOST_WIDE_INT v)
{
  unsigned max = leb_bytes (sizeof (v));
  char *ptr = use (max);
  unsigned count = 0;

  int end = v < 0 ? -1 : 0;
  bool more;
  do
    {
      unsigned byte = v & 127;
      v >>= 6; /* Signed shift.  */
      more = v != end;
      ptr[count++] = byte | (more << 7);
      v >>= 1; /* Signed shift.  */
    }
  while (more);
  unuse (max - count);
}

HOST_WIDE_INT
bytes_in::wi ()
{
  HOST_WIDE_INT v = 0;
  unsigned max = leb_bytes (sizeof (v));
  const char *ptr = use (max, &max);
  unsigned count = 0;

  unsigned bit = 0;
  unsigned byte;
  do
    {
      if (count == max)
	{
	  overrun = true;
	  return 0;
	}
      byte = ptr[count++];
      v |= (unsigned HOST_WIDE_INT)(byte & 127) << bit;
      bit += 7;
    }
  while (byte & 128);
  unuse (max - count);

  if (byte & 0x40 && bit < sizeof (v) * 8)
    v |= ~(unsigned HOST_WIDE_INT)0 << bit;
  return v;
}

/* unsigned wide ints are just written as signed wide ints.  */

inline void
bytes_out::wu (unsigned HOST_WIDE_INT v)
{
  wi ((HOST_WIDE_INT) v);
}

inline unsigned HOST_WIDE_INT
bytes_in::wu ()
{
  return (unsigned HOST_WIDE_INT) wi ();
}

/* size_t written as unsigned or unsigned wide int.  */

inline void
bytes_out::z (size_t s)
{
  if (sizeof (s) == sizeof (unsigned))
    u (s);
  else
    wu (s);
}

inline size_t
bytes_in::z ()
{
  if (sizeof (size_t) == sizeof (unsigned))
    return u ();
  else
    return wu ();
}

/* Buffer simply memcpied.  */

void
bytes_out::buf (const char *buf, size_t len)
{
  memcpy (use (len), buf, len);
}

const char *
bytes_in::buf (size_t len)
{
  const char *ptr = use (len);

  return ptr;
}

/* strings as an size_t length, followed by the buffer.  Make sure
   there's a NUL terminator on read.  */

void
bytes_out::str (const char *string, size_t len)
{
  z (len);
  buf (string, len + 1);
}

const char *
bytes_in::str (size_t *len_p)
{
  size_t len = z ();

  /* We're about to trust some user data.  */
  if (overrun)
    len = 0;
  *len_p = len;
  const char *str = buf (len + 1);
  if (str[len])
    {
      overrun = true;
      str = "";
    }
  return str;
}

/* Format a string directly to the buffer, including a terminating
   NUL.  Intended for human consumption.  */

void
bytes_out::printf (const char *format, ...)
{
  va_list args;
  /* Exercise buffer expansion.  */
  size_t len = MODULE_STAMP ? 10 : 500;

 again:
  va_start (args, format);
  char *ptr = use (len);
  size_t actual = vsnprintf (ptr, len, format, args) + 1;
  va_end (args);
  if (actual > len)
    {
      unuse (len);
      len = actual;
      goto again;
    }
  unuse (len - actual);
}

/* Begin reading section NAME (of type PROGBITS) from SOURCE.
   Data always checked for CRC.  */

bool
bytes_in::begin (elf_in *source, const char *name)
{
  unsigned snum = source->find (name);

  return begin (source, snum, name);
}

/* Begin reading section numbered SNUM with NAME (may be NULL).  */

bool
bytes_in::begin (elf_in *source, unsigned snum, const char *name)
{
  parent::begin ();

  data = source->read (snum);

  if (!data || !data->check_crc ())
    {
      data = data::release (data);
      source->set_error (elf::E_BAD_DATA);
      if (name)
	error ("section %qs is missing or corrupted", name);
      else
	error ("section #%u is missing or corrupted", snum);
      return false;
    }
  return true;
}

/* Begin writing buffer.  */

void
bytes_out::begin ()
{
  parent::begin ();
  data = data::extend (0, 200);
}

/* Finish writing buffer.  Stream out to SINK as named section NAME.
   Return section number or 0 on failure.  If CRC_PTR is true, crc
   the data.  Otherwise it is a string section.  */

unsigned
bytes_out::end (elf_out *sink, unsigned name, unsigned *crc_ptr)
{
  lengths[3] += pos;
  spans[3]++;

  data->size = pos;
  data->set_crc (crc_ptr);
  unsigned sec_num = sink->add (!crc_ptr, name, data);
  parent::end ();

  return sec_num;
}

class binding
{
  tree ns;
  tree name;
  vec<tree, va_heap> dependencies;
  vec<tree, va_heap, vl_embed> decls;
};

class scc
{
};

// FIXME:Forward declare, until module_state::{read,write}_namespace don't
// need it
class trees_out;

struct module_state;

/* Hash module state by name.  This cannot be a member of
   module_state, because of GTY restrictions.  */

struct module_state_hash : nodel_ptr_hash<module_state> {
  typedef tree compare_type; /* An identifier.  */

  static inline hashval_t hash (const value_type m);
  static inline bool equal (const value_type existing, compare_type candidate);
};

/* State of a particular module. */

struct GTY(()) module_state {
  /* We always import & export ourselves.  */
  bitmap imports;	/* Transitive modules we're importing.  */
  bitmap exports;	/* Subset of that, that we're exporting.  */

  tree name;		/* Name of the module.  */
  tree vec_name;  	/* Name as a vector.  */

  vec<unsigned, va_gc_atomic> *remap; /* Module owner remapping.  */
  elf_in *GTY((skip)) from;     /* Lazy loading info (not implemented) */

  char *filename;	/* Filename */
  char *srcname;	/* Source name, if available.  */
  location_t loc;	/* Its location.  */

  unsigned lazy;	/* Number of lazy sections yet to read.  */

  unsigned mod;		/* Module owner number.  */
  unsigned crc;		/* CRC we saw reading it in. */

  bool imported : 1;	/* Imported via import declaration.  */
  bool exported : 1;	/* The import is exported.  */

 public:
  module_state (tree name = NULL_TREE);
  ~module_state ();

 public:
  void release (bool = true);

 public:
  /* Whether this module is currently occupied.  */
  bool occupied () const
  {
    return vec_name != NULL_TREE;
  }
  void occupy (location_t loc, tree vname);

 public:
  /* Enter and leave the module's location.  */
  void push_location ();
  void pop_location ();

 public:
  void set_import (module_state const *, bool is_export);
  void announce (const char *) const;

 public:
  /* Read and write module.  */
  void write (elf_out *to);
  void read (elf_in *from, unsigned *crc_ptr);

 private:
  /* The context -- global state, imports etc.  */
  void write_context (elf_out *to, unsigned *crc_ptr);
  bool read_context (elf_in *from);
  /* The configuration -- cmd args etc.  */
  void write_config (elf_out *to, unsigned crc);
  bool read_config (elf_in *from, unsigned *crc_ptr);
  
  void record_namespace (elf_out *to, bytes_out &bind,
			 trees_out &trees, tree ns);
  void write_namespace (elf_out *to, bytes_out &bind, trees_out &trees, tree ns,
			auto_vec<tree> &stack, auto_vec<tree> &decls);
  void read_namespace (elf_in *from, bytes_in &bind, tree ctx);
  void write_bindings (elf_out *to, unsigned *crc_ptr);
  bool read_bindings (elf_in *from);

 public:
  /* Import a module.  Possibly ourselves.  */
  static module_state *do_import (location_t, tree, bool module_unit_p,
				  bool import_export_p, unsigned * = NULL);

 public:
  static void init ();
  static void fini ();
  static module_state *get_module (tree name, module_state * = NULL);
  static void print_map ();

 public:
  /* Vector indexed by OWNER.  */
  static vec<module_state *, va_gc> *modules;

 private:
  /* Hash, findable by NAME.  */
  static hash_table<module_state_hash> *hash;

 private:
  /* Global tree context.  */
  static const std::pair<tree *, unsigned> global_trees[];
  static vec<tree, va_gc> *global_vec;
  static unsigned global_crc;
};

/* Hash module state by name.  */
hashval_t module_state_hash::hash (const value_type m)
{
  return IDENTIFIER_HASH_VALUE (m->name);
}
/* Always lookup by IDENTIFIER_NODE.  */
bool module_state_hash::equal (const value_type existing,
			       compare_type candidate)
{
  return existing->name == candidate;
}

/* Some flag values: */

/* Binary module interface output file name. */
static const char *module_output;
/* Pathfragment inserted before module filename.  */
static const char *module_prefix;
/* Set of module-file arguments to process on initialization.  */
static vec<const char *, va_heap> module_file_args;
/* The module wrapper script.  */
static const char *module_wrapper;
/* Print out the module map.  */
static bool module_map_dump;
/* Module search path.  */
static cpp_dir *module_path;
/* Longest module path.  */
static size_t module_path_max;

/* Global trees.  */
const std::pair<tree *, unsigned> module_state::global_trees[] =
  {
    std::pair<tree *, unsigned> (sizetype_tab, stk_type_kind_last),
    std::pair<tree *, unsigned> (integer_types, itk_none),
    std::pair<tree *, unsigned> (::global_trees, TI_MAX),
    std::pair<tree *, unsigned> (cp_global_trees, CPTI_MAX),
    std::pair<tree *, unsigned> (NULL, 0)
  };
vec<tree, va_gc> GTY (()) *module_state::global_vec;
unsigned module_state::global_crc;

/* Vector of module state.  Indexed by OWNER.  Always has 2 slots.  */
vec<module_state *, va_gc> GTY(()) *module_state::modules;

/* Has of module state, findable by NAME. */
hash_table<module_state_hash> *module_state::hash;

/* Tree tags.  */
enum tree_tag
  {
    tt_backref = -1,	/* Back references.  Must be first.  */
    tt_null,		/* NULL_TREE.  */
    tt_fixed,		/* Fixed vector index.  */
    tt_node,		/* New node.  */
    tt_id,  		/* Identifier node.  */
    tt_conv_id,		/* Conversion operator name.  */
    tt_tinfo_var,	/* Typeinfo object. */
    tt_tinfo_pseudo,	/* Typeinfo pseudo type.  */
    tt_type_name,	/* TYPE_DECL for type.  */
    tt_namespace,	/* Namespace.  */
    tt_import,  	/* Import from another module. */
    tt_binfo,		/* A BINFO.  */
    tt_as_base,		/* An As-Base type.  */

    tt_binding,
    tt_definition,

    tt_decl		/* Declaration reference.  Must be last.  */
  };

/* Tree stream reader.  */
class trees_in : public bytes_in {
  typedef bytes_in parent;

private:
  module_state *state;		/* Module being imported.  */
  vec<tree, va_gc> *fixed_refs;	/* Fixed trees.  */
  auto_vec<tree> back_refs;	/* Back references.  */

public:
  trees_in (module_state *, vec<tree, va_gc> *globals);
  ~trees_in ();

public:
  void read ();

public:
  bool tag_binding ();
  tree tag_definition ();

private:
  int insert (tree);
  tree finish_type (tree);

private:
  tree start (tree_code);
  tree finish (tree);
  location_t loc ();

private:
  /* Stream tree_core, lang_decl_specific and lang_type_specific
     bits.  */
  bool core_bools (tree);
  bool core_vals (tree);
  bool lang_type_bools (tree);
  bool lang_type_vals (tree);
  bool lang_decl_bools (tree);
  bool lang_decl_vals (tree);
  bool tree_binfo (tree type);

  /* All the bits of a tree.  */
  bool tree_node_raw (tree);

private:
  tree chained_decls ();  /* Follow DECL_CHAIN.  */
  vec<tree, va_gc> *tree_vec (); /* vec of tree.  */
  vec<tree_pair_s, va_gc> *tree_pair_vec (); /* vec of tree_pair.  */

private:
  tree define_function (tree, tree);
  tree define_var (tree, tree);
  tree define_class (tree, tree);
  tree define_enum (tree, tree);

public:
  /* Read a tree node.  */
  tree tree_node ();
};

trees_in::trees_in (module_state *state, vec<tree, va_gc> *globals)
  :parent (), state (state), fixed_refs (globals), back_refs (500)
{
}

trees_in::~trees_in ()
{
}

/* Tree stream writer.  */
class trees_out : public bytes_out {
  typedef bytes_out parent;

private:
  module_state *state;	/* The module we are writing.  */
  vec<tree, va_gc> *fixed_refs;	/* Fixed trees.  */
  ptr_int_hash_map tree_map; /* Trees to references */
  int ref_num;		/* Back reference number.  */

public:
  trees_out (module_state *, vec<tree, va_gc> *globals);
  ~trees_out ();

public:
  void begin ();
  unsigned end (elf_out *sink, unsigned name, unsigned *crc_ptr);

public:
  void write (auto_vec<tree> &decls);

public:
  void maybe_tag_definition (tree decl);
  void tag_definition (tree node, tree maybe_template);

private:
  void tag (int rt)
  {
    records++;
    i (rt);
  }
  int insert (tree);
  int maybe_insert (tree);
  void start (tree_code, tree);
  void loc (location_t);

private:
  void core_bools (tree);
  void core_vals (tree);
  void lang_type_bools (tree);
  void lang_type_vals (tree);
  void lang_decl_bools (tree);
  void lang_decl_vals (tree);
  void tree_binfo (tree type);
  void tree_node_raw (tree);

private:
  void chained_decls (tree);
  void tree_vec (vec<tree, va_gc> *);
  void tree_pair_vec (vec<tree_pair_s, va_gc> *);

private:
  void define_function (tree, tree);
  void define_var (tree, tree);
  void define_class (tree, tree);
  void define_enum (tree, tree);

public:
  void tree_node (tree);
  vec<tree, va_gc> *bindings (bytes_out *, elf_out *,
			      vec<tree, va_gc> *nest, tree ns);

public:
  static void instrument ();

private:
  /* Tree instrumentation. */
  static unsigned unique;
  static unsigned refs;
  static unsigned nulls;
  static unsigned records;

};

/* Instrumentation counters.  */
unsigned trees_out::unique;
unsigned trees_out::refs;
unsigned trees_out::nulls;
unsigned trees_out::records;

trees_out::trees_out (module_state *state, vec<tree, va_gc> *globals)
  :parent (), state (state), fixed_refs (globals), tree_map (500),
   ref_num (0)
{
}

trees_out::~trees_out ()
{
}

/* Setup and teardown for a tree walk.  */

void
trees_out::begin ()
{
  gcc_assert (!tree_map.elements ());

  /* Install the fixed trees, with +ve references.  */
  unsigned limit = fixed_refs->length ();
  for (unsigned ix = 0; ix != limit; ix++)
    {
      tree val = (*fixed_refs)[ix];
      bool existed = tree_map.put (val, ix);
      gcc_checking_assert (!TREE_VISITED (val) && !existed);
      TREE_VISITED (val) = true;
    }
  parent::begin ();
}

unsigned
trees_out::end (elf_out *sink, unsigned name, unsigned *crc_ptr)
{
  /* Unmark all the trees.  */
  ptr_int_hash_map::iterator end (tree_map.end ());
  for (ptr_int_hash_map::iterator iter (tree_map.begin ()); iter != end; ++iter)
    {
      tree node = reinterpret_cast <tree> ((*iter).first);
      gcc_checking_assert (TREE_VISITED (node));
      TREE_VISITED (node) = false;
    }
  return parent::end (sink, name, crc_ptr);
}

/* A dumping machinery.  */

class dumper {
private:
  struct impl {
    typedef vec<module_state *, va_heap, vl_embed> stack_t;

    FILE *stream;	/* Dump stream.  */
    unsigned indent; 	/* Local indentation.  */
    bool bol; 		/* Beginning of line.  */
    stack_t stack;	/* Trailing array of module_state.  */

    bool nested_name (tree);  /* Dump a name following DECL_CONTEXT.  */
  };

public:
  /* The dumper.  */
  impl *dumps;

public:
  /* Push/pop module state dumping.  */
  unsigned push (module_state *);
  void pop (unsigned);

  /* Change local indentation.  */
  void indent ()
  {
    if (dumps)
      dumps->indent++;
  }
  void outdent ()
  {
    if (dumps)
      {
	gcc_checking_assert (dumps->indent);
	dumps->indent--;
      }
  }

  /* Is dump enabled?.  */
  bool operator () ()
  {
    return dumps && dumps->stream;
  }
  /* Dump some information.  */
  bool operator () (const char *, ...);
};

/* The dumper.  */
static dumper dump = {0};

/* Push to dumping M.  Return previous indentation level.  */

unsigned
dumper::push (module_state *m)
{
  bool blank = false;
  FILE *stream = NULL;
  if (!dumps || !dumps->stack.length ())
    {
      blank = dumps != NULL;
      stream = dump_begin (module_dump_id, NULL);
      if (!stream)
	return 0;
    }

  if (!dumps || !dumps->stack.space (1))
    {
      /* Create or extend the dump implementor.  */
      bool current = dumps ? dumps->stack.length () : 0;
      unsigned count = current ? current *2 : MODULE_STAMP ? 1 : 20;
      size_t alloc = (offsetof (impl, impl::stack)
		      + impl::stack_t::embedded_size (count));
      dumps = XRESIZEVAR (impl, dumps, alloc);
      dumps->stack.embedded_init (count, current);
    }
  if (stream)
    dumps->stream = stream;

  unsigned n = dumps->indent;
  dumps->indent = 0;
  dumps->bol = true;
  dumps->stack.quick_push (m);
  if (blank)
    dump ("");
  if (m)
    {
      module_state *from = (dumps->stack.length () > 1
			    ? dumps->stack[dumps->stack.length () - 2] : NULL);
      dump (from ? "Starting module %M (from %M)"
	    : "Starting module %M", m, from);
    }

  return n;
}

/* Pop from dumping.  Restore indentation to N.  */

void dumper::pop (unsigned n)
{
  if (!dumps)
    return;

  gcc_checking_assert (dump () && !dumps->indent);
  if (module_state *m = dumps->stack[dumps->stack.length () - 1])
    {
      module_state *from = (dumps->stack.length () > 1
			    ? dumps->stack[dumps->stack.length () - 2] : NULL);
      dump (from ? "Finishing module %M (returning to %M)"
	    : "Finishing module %M", m, from);
    }
  dumps->stack.pop ();
  dumps->indent = n;
  if (!dumps->stack.length ())
    {
      dump_end (module_dump_id, dumps->stream);
      dumps->stream = NULL;
    }
  else
    dump ("");
}

/* Dump a nested name for arbitrary tree T.  Sometimes it won't have a
   name.  */

bool
dumper::impl::nested_name (tree t)
{
  if (t && TREE_CODE (t) == TREE_BINFO)
    t = BINFO_TYPE (t);

  if (t && TYPE_P (t))
    t = TYPE_NAME (t);

  if (t && DECL_P (t))
    {
      if (t == global_namespace)
	;
      else if (tree ctx = DECL_CONTEXT (t))
	if (TREE_CODE (ctx) == TRANSLATION_UNIT_DECL
	    || nested_name (ctx))
	  fputs ("::", stream);
      t = DECL_NAME (t);
    }

  if (t)
    switch (TREE_CODE (t))
      {
      case IDENTIFIER_NODE:
	fwrite (IDENTIFIER_POINTER (t), 1, IDENTIFIER_LENGTH (t), stream);
	return true;

      case STRING_CST:
	fwrite (TREE_STRING_POINTER (t), 1, TREE_STRING_LENGTH (t) - 1, stream);
	return true;

      case INTEGER_CST:
	print_hex (wi::to_wide (t), stream);
	return true;

      default:
	break;
      }

  return false;
}

/* Formatted dumping.  FORMAT begins with '+' do not emit a trailing
   new line.  (Normally it is appended.)
   Escapes:
      %C - tree_code
      %I - identifier
      %M - module_state
      %N - name -- DECL_NAME
      %P - context:name pair
      %R - unsigned:unsigned ratio
      %S - symbol -- DECL_ASSEMBLER_NAME
      %U - long unsigned
      %V - version
      --- the following are printf-like, but without its flexibility
      %d - decimal int
      %p - pointer
      %s - string
      %u - unsigned int
      %x - hex int

  We do not implement the printf modifiers.  */

bool
dumper::operator () (const char *format, ...)
{
  if (!(*this) ())
    return false;

  bool no_nl = format[0] == '+';
  format += no_nl;

  if (dumps->bol)
    {
      if (unsigned depth = dumps->stack.length () - 1)
	{
	  /* Module import indenting.  */
	  const char *indent = ">>>>";
	  const char *dots   = ">...>";
	  if (depth > strlen (indent))
	    indent = dots;
	  else
	    indent += strlen (indent) - depth;
	  fputs (indent, dumps->stream);
	}
      if (unsigned indent = dumps->indent)
	{
	  /* Tree indenting.  */
	  const char *spaces = "      ";
	  const char *dots  =  "   ... ";

	  fputs (indent > strlen (spaces) ? dots
		 : &spaces[strlen (spaces) - indent], dumps->stream);
	}
      dumps->bol = false;
    }

  va_list args;
  va_start (args, format);
  while (const char *esc = strchr (format, '%'))
    {
      fwrite (format, 1, (size_t)(esc - format), dumps->stream);
      format = ++esc;
      switch (*format++)
	{
	case 'C': /* Code */
	  {
	    tree_code code = (tree_code)va_arg (args, unsigned);
	    fputs (get_tree_code_name (code), dumps->stream);
	    break;
	  }
	case 'I': /* Identifier.  */
	  {
	    tree t = va_arg (args, tree);
	    dumps->nested_name (t);
	    break;
	  }
	case 'M': /* Module. */
	  {
	    module_state *m = va_arg (args, module_state *);
	    if (m)
	      dumps->nested_name (m->name);
	    else
	      fputs ("(none)", dumps->stream);
	    break;
	  }
	case 'N': /* Name.  */
	  {
	    tree t = va_arg (args, tree);
	    fputc ('\'', dumps->stream);
	    dumps->nested_name (t);
	    fputc ('\'', dumps->stream);
	    break;
	  }
	case 'P': /* Pair.  */
	  {
	    tree ctx = va_arg (args, tree);
	    tree name = va_arg (args, tree);
	    fputc ('\'', dumps->stream);
	    dumps->nested_name (ctx);
	    if (ctx && ctx != global_namespace)
	      fputs ("::", dumps->stream);
	    dumps->nested_name (name);
	    fputc ('\'', dumps->stream);
	    break;
	  }
	case 'R': /* Ratio */
	  {
	    unsigned a = va_arg (args, unsigned);
	    unsigned b = va_arg (args, unsigned);
	    fprintf (dumps->stream, "%.1f", (float) a / (b + !b));
	    break;
	  }
	case 'S': /* Symbol name */
	  {
	    tree t = va_arg (args, tree);
	    if (t && TYPE_P (t))
	      t = TYPE_NAME (t);
	    if (t && HAS_DECL_ASSEMBLER_NAME_P (t)
		&& DECL_ASSEMBLER_NAME_SET_P (t))
	      {
		fputc ('(', dumps->stream);
		fputs (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t)),
		       dumps->stream);
		fputc (')', dumps->stream);
	      }
	    break;
	  }
	case 'U': /* long unsigned.  */
	  {
	    unsigned long u = va_arg (args, unsigned long);
	    fprintf (dumps->stream, "%lu", u);
	    break;
	  }
	case 'V': /* Verson.  */
	  {
	    int v = va_arg (args, unsigned);
	    verstr_t string;

	    version2string (v, string);
	    fputs (string, dumps->stream);
	    break;
	  }
	case 'd': /* Decimal Int.  */
	  {
	    int d = va_arg (args, int);
	    fprintf (dumps->stream, "%d", d);
	    break;
	  }
	case 'p': /* Pointer. */
	  {
	    void *p = va_arg (args, void *);
	    fprintf (dumps->stream, "%p", p);
	    break;
	  }
	case 's': /* String. */
	  {
	    const char *s = va_arg (args, char *);
	    fputs (s, dumps->stream);
	    break;
	  }
	case 'u': /* Unsigned.  */
	  {
	    unsigned u = va_arg (args, unsigned);
	    fprintf (dumps->stream, "%u", u);
	    break;
	  }
	case 'x': /* Hex. */
	  {
	    unsigned x = va_arg (args, unsigned);
	    fprintf (dumps->stream, "%x", x);
	    break;
	  }
	default:
	  gcc_unreachable ();
	}
    }
  fputs (format, dumps->stream);
  va_end (args);
  if (!no_nl)
    {
      dumps->bol = true;
      fputc ('\n', dumps->stream);
    }
  return true;
}

module_state::module_state (tree name)
  : imports (BITMAP_GGC_ALLOC ()), exports (BITMAP_GGC_ALLOC ()),
    name (name), vec_name (NULL_TREE),
    remap (NULL), from (NULL),
    filename (NULL), srcname (NULL), loc (UNKNOWN_LOCATION),
    lazy (0), mod (MODULE_UNKNOWN), crc (0)
{
  imported = exported = false;
}

module_state::~module_state ()
{
  release ();
}

/* Find or create module NAME in the hash table.  */

module_state *
module_state::get_module (tree name, module_state *dflt)
{
  module_state **slot
    = hash->find_slot_with_hash (name, IDENTIFIER_HASH_VALUE (name), INSERT);
  module_state *state = *slot;

  if (dflt)
    {
      /* Don't overwrite occupied default.  */
      if (dflt->occupied ())
	return NULL;

      /* Don't copy an occupied existing module.  */
      if (state && state->occupied ())
	return state;

      /* Copy name and filename from the empty one we found.  */
      if (state)
	{
	  if (!dflt->filename)
	    dflt->filename = state->filename;
	  else
	    free (state->filename);
	}

      /* Use the default we were given, and put it back in the hash
	 table.  */
      dflt->name = name;
      state = dflt;
      *slot = state;
    }
  else if (!state)
    {
      state = new (ggc_alloc<module_state> ()) module_state (name);
      *slot = state;
    }
  return state;
}

/* Initialize module state.  Create the hash table, determine the
   global trees.  Create the module for current TU.  */

void
module_state::init ()
{
  hash = new hash_table<module_state_hash> (30);

  vec_safe_reserve (modules, 20);
  for (unsigned ix = MODULE_IMPORT_BASE; ix--;)
    modules->quick_push (NULL);

  /* Create module for current TU.  */
  module_state *current = new (ggc_alloc <module_state> ()) module_state ();
  current->mod = MODULE_NONE;
  bitmap_set_bit (current->imports, MODULE_NONE);
  (*modules)[MODULE_NONE] = current;

  gcc_checking_assert (!global_vec);

  dump.push (NULL);
  /* Construct the global tree array.  This is an array of unique
     global trees (& types).  */
  // FIXME:Some slots are lazily allocated, we must move them to
  // the end and not stream them here.  They must be locatable via
  // some other means.
  unsigned crc = 0;
  vec_alloc (global_vec, 200);

  dump () && dump ("+Creating globals");
  /* Insert the TRANSLATION_UNIT_DECL.  */
  TREE_VISITED (DECL_CONTEXT (global_namespace)) = true;
  global_vec->quick_push (DECL_CONTEXT (global_namespace));
  for (unsigned jx = 0; global_trees[jx].first; jx++)
    {
      const tree *ptr = global_trees[jx].first;
      unsigned limit = global_trees[jx].second;

      for (unsigned ix = 0; ix != limit; ix++, ptr++)
	{
	  !(ix & 31) && dump ("") && dump ("+\t%u:%u:", jx,ix);
	  unsigned v = 0;
	  if (tree val = *ptr)
	    {
	      if (identifier_p (val))
		continue;
	      if (TREE_VISITED (val))
		continue;
	      TREE_VISITED (val) = true;
	      crc = crc32_unsigned (crc, global_vec->length ());
	      vec_safe_push (global_vec, val);
	      v++;
	      if (CODE_CONTAINS_STRUCT (TREE_CODE (val), TS_TYPED))
		{
		  val = TREE_TYPE (val);
		  if (val && !TREE_VISITED (val))
		    {
		      TREE_VISITED (val) = true;
		      crc = crc32_unsigned (crc, global_vec->length ());
		      vec_safe_push (global_vec, val);
		      v++;
		    }
		}
	    }
	  dump () && dump ("+%u", v);
	}
    }
  global_crc = crc32_unsigned (crc, global_vec->length ());
  dump ("") && dump ("Created %u unique globals, crc=%x",
		     global_vec->length (), global_crc);
  for (unsigned ix = global_vec->length (); ix--;)
    TREE_VISITED ((*global_vec)[ix]) = false;
  dump.pop (0);
}

/* Delete post-parsing state.  */

void
module_state::fini ()
{
  for (unsigned ix = modules->length (); --ix >= MODULE_IMPORT_BASE;)
    (*modules)[ix]->release ();

  delete hash;
  hash = NULL;
}

/* Free up state.  If ALL is true, we're completely done.  If ALL is
   false, we've completed reading in the module (but have not
   completed parsing).  */

void
module_state::release (bool all)
{
  if (all)
    {
      imports = NULL;
      exports = NULL;
    }

  XDELETEVEC (srcname);
  srcname = NULL;

  if (remap)
    {
      vec_free (remap);
      delete from;
      from = NULL;
    }
}

void
module_state::print_map ()
{
  fprintf (stdout, "Module Map:\n");
  hash_table<module_state_hash>::iterator end (hash->end ());
  for (hash_table<module_state_hash>::iterator iter (hash->begin ());
       iter != end; ++iter)
    {
      module_state *state = *iter;
      if (state->name)
	fprintf (stdout, state->srcname ? "%s=%s ;%s\n" : "%s=%s\n",
		 IDENTIFIER_POINTER (state->name), state->filename,
		 state->srcname);
    }
}

/* Announce WHAT about the module.  */

void
module_state::announce (const char *what) const
{
  if (quiet_flag)
    return;

  fprintf (stderr, mod < MODULE_LIMIT ? " %s:%s:%u" : " %s:%s",
	   what, IDENTIFIER_POINTER (name), mod);
  fflush (stderr);
  pp_needs_newline (global_dc->printer) = true;
  diagnostic_set_last_function (global_dc, (diagnostic_info *) NULL);
}

/* Create a module file name from NAME, length NAME_LEN.  NAME might
   not be NUL-terminated.  FROM_IDENT is true if NAME is the
   module-name, false if it is already filenamey.  */

static char *
make_module_filename (const char *name, size_t name_len, bool from_ident)
{
  size_t pfx_len = module_prefix ? strlen (module_prefix) : 0;
  size_t sfx_len = from_ident ? strlen (MOD_FNAME_SFX) : 0;
  char *res = XNEWVEC (char, name_len + sfx_len + pfx_len + (pfx_len != 0) + 1);
  char *ptr = res;

  if (pfx_len)
    {
      memcpy (ptr, module_prefix, pfx_len);
      ptr += pfx_len;
      *ptr++ = DIR_SEPARATOR;
    }
  memcpy (ptr, name, name_len);
  ptr[name_len] = 0;
  if (from_ident)
    {
      strcpy (ptr + name_len, MOD_FNAME_SFX);
      if (MOD_FNAME_DOT != '.')
	for (; name_len--; ptr++)
	  if (*ptr == '.')
	    *ptr = MOD_FNAME_DOT;
    }
  return res;
}

/* Set VEC_NAME fields from incoming VNAME, which may be a regular
   IDENTIFIER.  */

void
module_state::occupy (location_t l, tree maybe_vec)
{
  gcc_assert (!vec_name);
  loc = l;
  if (identifier_p (maybe_vec))
    {
      /* Create a TREE_VEC of components.  */
      auto_vec<tree,5> ids;
      size_t len = IDENTIFIER_LENGTH (maybe_vec);
      const char *ptr = IDENTIFIER_POINTER (maybe_vec);

      while (const char *dot = (const char *)memchr (ptr, '.', len))
	{
	  tree id = get_identifier_with_length (ptr, dot - ptr);
	  len -= dot - ptr + 1;
	  ptr = dot + 1;
	  ids.safe_push (id);
	}
      tree id = (ids.length () ? get_identifier_with_length (ptr, len)
		 : maybe_vec);
      ids.safe_push (id);
      maybe_vec = make_tree_vec (ids.length ());
      for (unsigned ix = ids.length (); ix--;)
	TREE_VEC_ELT (maybe_vec, ix) = ids.pop ();
    }

  vec_name = maybe_vec;

  if (!filename)
    filename = make_module_filename (IDENTIFIER_POINTER (name),
				     IDENTIFIER_LENGTH (name), true);
}

/* Set location to NAME, and then enter the module.  */

void
module_state::push_location ()
{
  // FIXME:We want LC_MODULE_ENTER really.
  linemap_add (line_table, LC_ENTER, false, filename, 0);
  input_location = linemap_line_start (line_table, 0, 0);
}

void
module_state::pop_location ()
{
  linemap_add (line_table, LC_LEAVE, false, NULL, 0);  
  input_location = linemap_line_start (line_table, 0, 0);
}

/* Context of the compilation: MOD_SNAME_PFX .context   

   This is data we need to read in to modify our state.

   u:import_size
   {
     b:imported
     b:exported
     u32:crc
     u:name
   } imports[N]

   We need the complete set, so that we can build up the remapping
   vector on subsequent import.

   The README section is intended for human consumption.  It is a
   STRTAB that may be extracted with:
     readelf -p.gnu.c++.README $(module).nms   */

void
module_state::write_context (elf_out *to, unsigned *crc_p)
{
  bytes_out ctx, readme;

  ctx.begin ();
  readme.begin ();

  /* Write version and module name to readme.  */
  readme.printf ("GNU C++ Module");
  readme.printf ("compiler:%s", version_string);
  verstr_t string;
  version2string (get_version (), string);
  readme.printf ("version:%s", string);
  readme.printf ("module:%s", IDENTIFIER_POINTER (name));
  readme.printf ("source:%s", srcname);

  /* Total number of modules.  */
  ctx.u (modules->length ());
  /* Write the imports, in forward order.  */
  for (unsigned ix = MODULE_IMPORT_BASE; ix < modules->length (); ix++)
    {
      module_state *state = (*modules)[ix];
      dump () && dump ("Writing %simport %I (crc=%x)",
		       state->exported ? "export " :
		       state->imported ? "" : "indirect ",
		       state->name, state->crc);
      ctx.b (state->imported);
      ctx.b (state->exported);
      ctx.bflush ();
      ctx.u32 (state->crc);
      unsigned name = to->name (state->name);
      ctx.u (name);

      if (state->imported)
	/* Write a direct import to README.  */
	readme.printf ("import:%s", IDENTIFIER_POINTER (state->name));
    }
  readme.end (to, to->name (MOD_SNAME_PFX ".README"), NULL);
  ctx.end (to, to->name (MOD_SNAME_PFX ".context"), crc_p);
}

bool
module_state::read_context (elf_in *from)
{
  bytes_in ctx;

  if (!ctx.begin (from, MOD_SNAME_PFX ".context"))
    return false;

  /* Allocate the REMAP vector.  */
  unsigned imports = ctx.u ();
  gcc_assert (!remap);
  remap = NULL;
  vec_safe_reserve (remap, imports);

  /* Allocate the reserved slots.  */
  for (unsigned ix = MODULE_IMPORT_BASE; ix--;)
    remap->quick_push (0);

  /* Read the import table in forward order.  */
  for (unsigned ix = MODULE_IMPORT_BASE; ix < imports; ix++)
    {
      bool imported = ctx.b ();
      bool exported = ctx.b ();
      ctx.bflush ();
      unsigned crc = ctx.u32 ();
      tree name = get_identifier (from->name (ctx.u ()));

      dump () && dump ("Nested %simport %I",
		       exported ? "export " : imported ? "" : "indirect ", name);
      module_state *imp = do_import (input_location, name, /*unit_p=*/false,
				     /*import_p=*/imported, &crc);
      if (!imp)
	{
	  from->set_error (elf::E_BAD_IMPORT);
	  goto fail;
	}
      remap->quick_push (imp->mod);
      if (imported)
	{
	  dump () && dump ("Direct %simport %I %u",
			   exported ? "export " : "", name, imp->mod);
	  set_import (imp, exported);
	}
    }

 fail:
  return ctx.end (from);
}

/* Tool configuration:  MOD_SNAME_PFX .config

   This is data that confirms current state (or fails).

   u32:version
   u32:crc
   u:module-name
   u:<target-triplet>
   u:<host-triplet>
   u:global_vec->length()
   u32:global_crc
   // FIXME CPU,ABI and similar tags
   // FIXME Optimization and similar tags
*/

void
module_state::write_config (elf_out *to, unsigned inner_crc)
{
  bytes_out cfg;

  cfg.begin ();

  /* Write version and inner crc as u32 values, for easier
     debug inspection.  */
  dump () && dump ("Writing version=%V, inner_crc=%x",
		   get_version (), inner_crc);
  cfg.u32 (unsigned (get_version ()));
  cfg.u32 (inner_crc);

  cfg.u (to->name (name));

  /* Configuration. */
  dump () && dump ("Writing target='%s', host='%s'",
		   TARGET_MACHINE, HOST_MACHINE);
  unsigned target = to->name (TARGET_MACHINE);
  unsigned host = (!strcmp (TARGET_MACHINE, HOST_MACHINE)
		   ? target : to->name (HOST_MACHINE));
  cfg.u (target);
  cfg.u (host);

  /* Global tree information.  We write the globals crc separately,
     rather than mix it directly into the overall crc, as it is used
     to ensure data match between instances of the compiler, not
     integrity of the file.  */
  dump () && dump ("Writing globals=%u, crc=%x",
		   global_vec->length (), global_crc);
  cfg.u (global_vec->length ());
  cfg.u32 (global_crc);

  /* Now generate CRC, we'll have incorporated the inner CRC because
     of its serialization above.  */
  cfg.end (to, to->name (MOD_SNAME_PFX ".config"), &crc);
  dump () && dump ("Writing CRC=%x", crc);
}

bool
module_state::read_config (elf_in *from, unsigned *expected_crc)
{
  bytes_in cfg;

  if (!cfg.begin (from, MOD_SNAME_PFX ".config"))
    return false;

  crc = cfg.get_crc ();
  dump () && dump ("Reading CRC=%x", crc);
  if (expected_crc && crc != *expected_crc)
    {
      error ("module %qE CRC mismatch", name);
    fail:
      cfg.set_overrun ();
      return cfg.end (from);
    }

  /* Check version.  */
  int my_ver = get_version ();
  int their_ver = int (cfg.u32 ());
  dump () && dump  (my_ver == their_ver ? "Version %V"
		    : "Expecting %V found %V", my_ver, their_ver);
  if (their_ver != my_ver)
    {
      int my_date = version2date (my_ver);
      int their_date = version2date (their_ver);
      int my_time = version2time (my_ver);
      int their_time = version2time (their_ver);
      verstr_t my_string, their_string;

      version2string (my_ver, my_string);
      version2string (their_ver, their_string);

      if (my_date != their_date)
	{
	  /* Dates differ, decline.  */
	  error ("file is version %s, this is version %s",
		 their_string, my_string);
	  goto fail;
	}
      else if (my_time != their_time)
	/* Times differ, give it a go.  */
	warning (0, "file is version %s, compiler is version %s,"
		 " perhaps close enough? \xc2\xaf\\_(\xe3\x83\x84)_/\xc2\xaf",
		 their_string, my_string);
    }

  /* Read and ignore the inner crc.  We only wrote it to mix it into
     the crc.  */
  cfg.u32 ();

  /* Check module name.  */
  const char *their_name = from->name (cfg.u ());
  if (strlen (their_name) != IDENTIFIER_LENGTH (name)
      || memcmp (their_name, IDENTIFIER_POINTER (name),
		 IDENTIFIER_LENGTH (name)))
    {
      error ("module %qs found, expected module %qE", their_name, name);
      goto fail;
    }

  /* Check target & host.  */
  const char *their_target = from->name (cfg.u ());
  const char *their_host = from->name (cfg.u ());
  dump () && dump ("Read target='%s', host='%s'", their_target, their_host);
  if (strcmp (their_target, TARGET_MACHINE)
      || strcmp (their_host, HOST_MACHINE))
    {
      error ("target & host is %qs:%qs, expected %qs:%qs",
	     their_target, TARGET_MACHINE, their_host, HOST_MACHINE);
      goto fail;
    }

  /* Check global trees.  */
  unsigned their_glength = cfg.u ();
  unsigned their_gcrc = cfg.u32 ();
  dump () && dump ("Read globals=%u, crc=%x", their_glength, their_gcrc);
  if (their_glength != global_vec->length ()
      || their_gcrc != global_crc)
    {
      error ("global tree mismatch");
      goto fail;
    }

  if (cfg.more_p ())
    goto fail;

  return cfg.end (from);
}

void
module_state::record_namespace (elf_out *to, bytes_out &bind, trees_out &/*trees*/,
			       tree ns)
{
  dump () && dump ("Recording namespace %N", ns);
  LOOKUP_FOUND_P (ns) = true;
  // FIXME: register with trees
  bind.u (to->name (DECL_NAME (ns)));
  bind.b (DECL_MODULE_EXPORT_P (ns));
  bind.b (DECL_NAMESPACE_INLINE_P (ns));
  bind.bflush ();
}

/* Walk the bindings of NS, writing out the bindings for the current
   TU.   */

// FIXME: There's a problem with namespaces themselves.  We need to
// know whether the namespace itself is exported, which happens if
// it's explicitly opened in the purview.  (It may exist because of
// being opened in the global module.)  Need flag on namespace,
// perhaps simple as DECL_MODULE_EXPORT.

/* The binding section is a serialized tree.  Each non-namespace
   binding is a <stroff,shnum> tuple.  Each namespace contains a list
   of non-namespace bindings, zero, a list of namespace bindings,
   zero. */

void
module_state::write_namespace (elf_out *to, bytes_out &bind, trees_out &trees,
			       tree ns,
			       auto_vec<tree> &stack, auto_vec<tree> &decls)
{
  dump () && dump ("Writing namespace %N", ns);

  auto_vec<tree> inner;
  inner.reserve (10);

  gcc_checking_assert ((ns == global_namespace) == LOOKUP_FOUND_P (ns));
  if (!LOOKUP_FOUND_P (ns) && DECL_MODULE_EXPORT_P (ns))
    {
      gcc_checking_assert (!stack.length () || LOOKUP_FOUND_P (stack.last ()));
      record_namespace (to, bind, trees, ns);
    }
  stack.safe_push (ns);

  hash_table<named_decl_hash>::iterator end
    (DECL_NAMESPACE_BINDINGS (ns)->end ());
  for (hash_table<named_decl_hash>::iterator iter
	 (DECL_NAMESPACE_BINDINGS (ns)->begin ()); iter != end; ++iter)
    {
      tree binding = *iter;

      if (TREE_CODE (binding) == MODULE_VECTOR)
	binding = (MODULE_VECTOR_CLUSTER
		   (binding, (MODULE_SLOT_CURRENT
			      / MODULE_VECTOR_SLOTS_PER_CLUSTER))
		   .slots[MODULE_SLOT_CURRENT
			  % MODULE_VECTOR_SLOTS_PER_CLUSTER]);

      if (!binding)
	continue;

      // FIXME:slightly awkward for now.
      decls.safe_push (ns);
      decls.safe_push (NULL);
      unsigned hwm = decls.length ();
      tree name = extract_module_decls (binding, decls);
      if (!name)
	{
	  decls.pop ();
	  decls.pop ();
	  continue;
	}

      tree first = decls[hwm];
      if (TREE_CODE (first) == NAMESPACE_DECL && !DECL_NAMESPACE_ALIAS (first))
	{
	  inner.safe_push (decls.pop ());
	  gcc_assert (decls.length () == hwm);
	  decls.pop ();
	  decls.pop ();
	  if (!DECL_MODULE_EXPORT_P (first))
	    continue;
	  first = NULL_TREE;
	}

      /* Emit open scopes.  */
      if (!LOOKUP_FOUND_P (ns))
	for (unsigned ix = 0; ix != stack.length (); ix++)
	  {
	    tree outer = stack[ix];
	    if (!LOOKUP_FOUND_P (outer))
	      {
		record_namespace (to, bind, trees, outer);
		if (ix < stack.length () - 1)
		  bind.u (0); /* It had no bindings of its own.  */
	      }
	  }

      if (first)
	{
	  // FIXME:mark end of this binding
	  decls[hwm-1] = name;
	  decls.safe_push (NULL_TREE);

	  // FIXME: register with trees
	  bind.u (to->name (name));
	  // FIXME, here we'd emit the section number containing the
	  // declaration.
	}
    }

  /* Mark end of non-namespace bindings.  */
  if (LOOKUP_FOUND_P (ns))
    bind.u (0);
  while (inner.length ())
    write_namespace (to, bind, trees, inner.pop (), stack, decls);
  /* Mark end of namespace bindings.  */
  if (LOOKUP_FOUND_P (ns))
    {
      bind.u (0);
      LOOKUP_FOUND_P (ns) = false;
    }
  stack.pop ();

  dump () && dump ("Wrote namespace %N", ns);
}

void
module_state::read_namespace (elf_in *from, bytes_in &bind, tree ns)
{
  dump () && dump ("Reading namespace %N", ns);

  /* Read the non-namespace bound names.  */
  while (unsigned off = bind.u ())
    {
      /* tree name =*/ get_identifier (from->name (off));
      // FIXME:insert?
    }

  /* Read the inner namespaces.  */
  while (unsigned off = bind.u ())
    {
      /*bool exported_p =*/ bind.b (); // FIXME:do something with this
      bool inline_p = bind.b ();
      bind.bflush ();

      tree name = get_identifier (from->name (off));
      tree inner = add_imported_namespace (ns, mod, name, inline_p);
      read_namespace (from, bind, inner);
    }

  dump () && dump ("Read namespace %N", ns);
}

/* Bindings: MOD_SNAME_PFX .bindings

   flattened namespace record of which names are bound.  */

void
module_state::write_bindings (elf_out *to, unsigned *crc_p)
{
  bytes_out bind;
  bind.begin ();
  
  trees_out trees (this, global_vec);
  auto_vec<tree> stack;
  auto_vec<tree> decls;
  stack.reserve (20);
  decls.reserve (20);

  LOOKUP_FOUND_P (global_namespace) = true;
  write_namespace (to, bind, trees, global_namespace, stack, decls);

  trees.begin ();
  trees.write (decls);

  // FIXME: For now, there's only one blob holding all the decls
  unsigned snum = trees.end (to, to->name (DECL_NAME (global_namespace)), crc_p);
  bind.u (snum);
  bind.end (to, to->name (MOD_SNAME_PFX ".bindings"), crc_p);
}

bool
module_state::read_bindings (elf_in *from)
{
  bytes_in bind;

  if (!bind.begin (from, MOD_SNAME_PFX ".bindings"))
    return false;

  trees_in trees (this, global_vec);

  read_namespace (from, bind, global_namespace);

  unsigned snum = bind.u ();

  if (!bind.end (from))
    return false;

  /* We're done with the string table now.  */
  from->release ();

  if (!trees.begin (from, snum))
    return false;

  trees.read ();

  return trees.end (from);
}

/* Use ELROND format to record the following sections:
     1     MOD_SNAME_PFX.README   : human readable, stunningly STRTAB-like
     2     MOD_SNAME_PFX.context  : context data
     [3-N) DECL_NAME() :binding value(s)
     N     MOD_SNAME_PFX.bindings : bindings of namespace names
     N+1   MOD_SNAME_PFX.config   : config data
*/

void
module_state::write (elf_out *to)
{
  unsigned crc = 0;

  write_context (to, &crc);
  write_bindings (to, &crc);
  write_config (to, crc);

  trees_out::instrument ();
}

void
module_state::read (elf_in *from, unsigned *crc_ptr)
{
  if (!read_config (from, crc_ptr))
    return;
  if (!read_context (from))
    return;

  /* Determine the module's number.  */
  unsigned ix = MODULE_PURVIEW;
  if (this != (*modules)[MODULE_NONE])
    {
      ix = modules->length ();
      if (ix == MODULE_LIMIT)
	{
	  sorry ("too many modules loaded (limit is %u)", ix);
	  from->set_error (elf::E_BAD_IMPORT);
	  return;
	}
      else
	{
	  vec_safe_push (modules, this);
	  bitmap_set_bit (imports, ix);
	  bitmap_set_bit (exports, ix);
	}
    }
  mod = ix;
  (*remap)[MODULE_PURVIEW] = ix;
  dump () && dump ("Assigning %N module number %u", name, ix);

  if (!read_bindings (from))
    return;

  return;
}

/* Return the IDENTIFIER_NODE naming module IX.  This is the name
   including dots.  */

tree
module_name (unsigned ix)
{
  return (*module_state::modules)[ix]->name;
}

/* Return the vector of IDENTIFIER_NODES naming module IX.  These are
   individual identifers per sub-module component.  */

tree
module_vec_name (unsigned ix)
{
  return (*module_state::modules)[ix]->vec_name;
}

/* Return the bitmap describing what modules are imported into
   MODULE.  Remember, we always import ourselves.  */

bitmap
module_import_bitmap (unsigned ix)
{
  return (*module_state::modules)[ix]->imports;
}

/* We've just directly imported OTHER.  Update our import/export
   bitmaps.  IS_EXPORT is true if we're reexporting the OTHER.  */

void
module_state::set_import (module_state const *other, bool is_export)
{
  gcc_checking_assert (this != other);
  bitmap_ior_into (imports, other->exports);
  if (is_export)
    bitmap_ior_into (exports, other->exports);
}

/* Instrumentation gathered writing bytes.  */

void
bytes_out::instrument ()
{
  dump ("Wrote %u bytes in %u blocks", lengths[3], spans[3]);
  dump ("Wrote %u bits in %u bytes", lengths[0] + lengths[1], lengths[2]);
  for (unsigned ix = 0; ix < 2; ix++)
    dump ("  %u %s spans of %R bits", spans[ix],
	  ix ? "one" : "zero", lengths[ix], spans[ix]);
  dump ("  %u blocks with %R bits padding", spans[2],
	lengths[2] * 8 - (lengths[0] + lengths[1]), spans[2]);
}

/* Instrumentation gathered writing trees.  */
void
trees_out::instrument ()
{
  if (dump (""))
    {
      bytes_out::instrument ();
      dump ("Wrote %u trees", unique + refs + nulls);
      dump ("  %u unique", unique);
      dump ("  %u references", refs);
      dump ("  %u nulls", nulls);
      dump ("Wrote %u records", records);
    }
}

/* Insert T into the map, return its back reference number.  */

inline int
trees_out::insert (tree t)
{
  int tag = maybe_insert (t);
  gcc_assert (tag);
  return tag;
}

/* Insert T into the map, if it isn't already there.  Return the
   inserted tag, or 0.  */

int
trees_out::maybe_insert (tree t)
{
  if (TREE_VISITED (t))
    return 0;

  TREE_VISITED (t) = true;
  int tag = --ref_num;
  bool existed = tree_map.put (t, tag);
  gcc_assert (!existed);
  return tag;
}

/* Insert T into the backreference array.  Return its back reference
   number.  */
int
trees_in::insert (tree t)
{
  back_refs.safe_push (t);
  return -(int)back_refs.length ();
}

/* BINDING is a vector of decls bound in namespace NS.  Write out the
   binding and definitions of things in the binding list.
   NS must have already have a binding somewhere.

   tree:ns
   tree:name
   tree:binding*
   tree:NULL
*/

bool
trees_in::tag_binding ()
{
  tree ns = tree_node ();
  tree name = tree_node ();
  tree type = NULL_TREE;
  tree decls = NULL_TREE;

  dump () && dump ("Reading %N binding for %N", ns, name);

  while (tree decl = tree_node ())
    {
      if (TREE_CODE (decl) == TYPE_DECL)
	{
	  if (type)
	    set_overrun ();
	  type = decl;
	}
      else if (decls
	       || (TREE_CODE (decl) == TEMPLATE_DECL
		   && TREE_CODE (DECL_TEMPLATE_RESULT (decl)) == FUNCTION_DECL))
	{
	  if (!DECL_DECLARES_FUNCTION_P (decl)
	      || (decls
		  && TREE_CODE (decls) != OVERLOAD
		  && TREE_CODE (decls) != FUNCTION_DECL))
	    set_overrun ();
	  decls = ovl_make (decl, decls);
	  if (DECL_MODULE_EXPORT_P (decl))
	    OVL_EXPORT_P (decls) = true;
	}
      else
	decls = decl;
    }

  if (get_overrun ())
    return false;

  if (!decls && !type)
    return true;

  return push_module_binding (ns, name, state->mod, decls, type);
}

/* Stream a function definition.  */

void
trees_out::define_function (tree decl, tree)
{
  tree_node (DECL_RESULT (decl));
  tree_node (DECL_INITIAL (decl));
  tree_node (DECL_SAVED_TREE (decl));
  if (DECL_DECLARED_CONSTEXPR_P (decl))
    tree_node (find_constexpr_fundef (decl));
}

tree
trees_in::define_function (tree decl, tree maybe_template)
{
  tree result = tree_node ();
  tree initial = tree_node ();
  tree saved = tree_node ();
  tree constexpr_body = (DECL_DECLARED_CONSTEXPR_P (decl)
			 ? tree_node () : NULL_TREE);

  if (get_overrun ())
    return NULL_TREE;

  if (TREE_CODE (CP_DECL_CONTEXT (decl)) == NAMESPACE_DECL)
    {
      unsigned mod = MAYBE_DECL_MODULE_OWNER (decl);
      if (mod != state->mod)
	{
	  error ("unexpected definition of %q#D", decl);
	  set_overrun ();
	  return NULL_TREE;
	}
      if (!MAYBE_DECL_MODULE_PURVIEW_P (maybe_template)
	  && DECL_SAVED_TREE (decl))
	return decl; // FIXME check same
    }

  DECL_RESULT (decl) = result;
  DECL_INITIAL (decl) = initial;
  DECL_SAVED_TREE (decl) = saved;
  if (constexpr_body)
    register_constexpr_fundef (decl, constexpr_body);

  current_function_decl = decl;
  allocate_struct_function (decl, false);
  cfun->language = ggc_cleared_alloc<language_function> ();
  cfun->language->base.x_stmt_tree.stmts_are_full_exprs_p = 1;
  set_cfun (NULL);
  current_function_decl = NULL_TREE;

  if (!DECL_TEMPLATE_INFO (decl) || DECL_USE_TEMPLATE (decl))
    {
      comdat_linkage (decl);
      note_vague_linkage_fn (decl);
      cgraph_node::finalize_function (decl, false);
    }

  return decl;
}

/* Stream a variable definition.  */

void
trees_out::define_var (tree decl, tree)
{
  tree_node (DECL_INITIAL (decl));
}

tree
trees_in::define_var (tree decl, tree)
{
  tree init = tree_node ();

  if (get_overrun ())
    return NULL;

  DECL_INITIAL (decl) = init;

  return decl;
}

/* A chained set of decls.  */

void
trees_out::chained_decls (tree decls)
{
  for (; decls; decls = DECL_CHAIN (decls))
    tree_node (decls);
  tree_node (NULL_TREE);
}

tree
trees_in::chained_decls ()
{
  tree decls = NULL_TREE;
  for (tree *chain = &decls; chain && !get_overrun ();)
    if (tree decl = tree_node ())
      {
	if (!DECL_P (decl))
	  set_overrun ();
	else
	  {
	    gcc_assert (!DECL_CHAIN (decl));
	    *chain = decl;
	    chain = &DECL_CHAIN (decl);
	  }
      }
    else
      chain = NULL;
  return decls;
}

/* A vector of trees.  */

void
trees_out::tree_vec (vec<tree, va_gc> *v)
{
  unsigned len = vec_safe_length (v);
  u (len);
  if (len)
    for (unsigned ix = 0; ix != len; ix++)
      tree_node ((*v)[ix]);
}

vec<tree, va_gc> *
trees_in::tree_vec ()
{
  vec<tree, va_gc> *v = NULL;
  if (unsigned len = u ())
    {
      vec_alloc (v, len);
      for (unsigned ix = 0; ix != len; ix++)
	v->quick_push (tree_node ());
    }
  return v;
}

/* A vector of tree pairs.  */

void
trees_out::tree_pair_vec (vec<tree_pair_s, va_gc> *v)
{
  unsigned len = vec_safe_length (v);
  u (len);
  if (len)
    for (unsigned ix = 0; ix != len; ix++)
      {
	tree_pair_s const &s = (*v)[ix];
	tree_node (s.purpose);
	tree_node (s.value);
      }
}

vec<tree_pair_s, va_gc> *
trees_in::tree_pair_vec ()
{
  vec<tree_pair_s, va_gc> *v = NULL;
  if (unsigned len = u ())
    {
      vec_alloc (v, len);
      for (unsigned ix = 0; ix != len; ix++)
	{
	  tree_pair_s s;
	  s.purpose = tree_node ();
	  s.value = tree_node ();
	  v->quick_push (s);
      }
    }
  return v;
}

/* Stream a class definition.  */

void
trees_out::define_class (tree type, tree maybe_template)
{
  gcc_assert (TYPE_MAIN_VARIANT (type) == type);

  chained_decls (TYPE_FIELDS (type));
  tree_node (TYPE_VFIELD (type));
  if (TYPE_LANG_SPECIFIC (type))
    {
      tree_vec (CLASSTYPE_MEMBER_VEC (type));
      tree_node (CLASSTYPE_FRIEND_CLASSES (type));
      tree_node (CLASSTYPE_LAMBDA_EXPR (type));

      if (TYPE_CONTAINS_VPTR_P (type))
	{
	  tree_vec (CLASSTYPE_PURE_VIRTUALS (type));
	  tree_pair_vec (CLASSTYPE_VCALL_INDICES (type));
	  tree_node (CLASSTYPE_KEY_METHOD (type));
	}
    }

  /* Write the remaining BINFO contents. */
  for (tree binfo = TYPE_BINFO (type); binfo; binfo = TREE_CHAIN (binfo))
    {
      dump () && dump ("Writing binfo:%N of %N contents", binfo, type);
#define WT(X) (tree_node (X))
      WT (binfo->binfo.vtable);
      WT (binfo->binfo.virtuals);
      WT (binfo->binfo.vptr_field);
      WT (binfo->binfo.vtt_subvtt);
      WT (binfo->binfo.vtt_vptr);
#undef WT
      tree_vec (BINFO_BASE_ACCESSES (binfo));
    }

  if (TYPE_LANG_SPECIFIC (type))
    {
      tree vtables = CLASSTYPE_VTABLES (type);
      chained_decls (vtables);
      /* Write the vtable initializers.  */
      for (; vtables; vtables = TREE_CHAIN (vtables))
	tree_node (DECL_INITIAL (vtables));
    }

  if (TREE_CODE (maybe_template) == TEMPLATE_DECL)
    tree_node (CLASSTYPE_DECL_LIST (type));

  // lang->nested_udts

  /* Now define all the members.  */
  for (tree member = TYPE_FIELDS (type); member; member = TREE_CHAIN (member))
    maybe_tag_definition (member);

  /* End of definitions.  */
  tree_node (NULL_TREE);
}

/* Nop sorted needed for resorting the member vec.  */

static void
nop (void *, void *)
{
}

tree
trees_in::define_class (tree type, tree maybe_template)
{
  gcc_assert (TYPE_MAIN_VARIANT (type) == type);

  tree fields = chained_decls ();
  tree vfield = tree_node ();
  vec<tree, va_gc> *member_vec = NULL;
  vec<tree, va_gc> *pure_virts = NULL;
  vec<tree_pair_s, va_gc> *vcall_indices = NULL;
  tree key_method = NULL_TREE;
  tree lambda = NULL_TREE;
  tree friends = NULL_TREE;

  if (TYPE_LANG_SPECIFIC (type))
    {
      member_vec = tree_vec ();
      friends = tree_node ();
      lambda = tree_node ();

      if (TYPE_CONTAINS_VPTR_P (type))
	{
	  pure_virts = tree_vec ();
	  vcall_indices = tree_pair_vec ();
	  key_method = tree_node ();
	}
    }

  // lang->nested_udts

  // FIXME: Sanity check stuff

  if (get_overrun ())
    return NULL_TREE;

  TYPE_FIELDS (type) = fields;
  TYPE_VFIELD (type) = vfield;

  if (TYPE_LANG_SPECIFIC (type))
    {
      CLASSTYPE_FRIEND_CLASSES (type) = friends;
      CLASSTYPE_LAMBDA_EXPR (type) = lambda;

      CLASSTYPE_MEMBER_VEC (type) = member_vec;
      CLASSTYPE_PURE_VIRTUALS (type) = pure_virts;
      CLASSTYPE_VCALL_INDICES (type) = vcall_indices;

      CLASSTYPE_KEY_METHOD (type) = key_method;
      if (!key_method && TYPE_CONTAINS_VPTR_P (type))
	vec_safe_push (keyed_classes, type);

      /* Resort the member vector.  */
      resort_type_member_vec (member_vec, NULL, nop, NULL);
    }
  
  /* Read the remaining BINFO contents. */
  for (tree binfo = TYPE_BINFO (type); binfo; binfo = TREE_CHAIN (binfo))
    {
      dump () && dump ("Reading binfo:%N of %N contents", binfo, type);
#define RT(X) ((X) = tree_node ())
      RT (binfo->binfo.vtable);
      RT (binfo->binfo.virtuals);
      RT (binfo->binfo.vptr_field);
      RT (binfo->binfo.vtt_subvtt);
      RT (binfo->binfo.vtt_vptr);
#undef RT
      BINFO_BASE_ACCESSES (binfo) = tree_vec ();
      if (vec_safe_length (BINFO_BASE_ACCESSES (binfo))
	  != BINFO_N_BASE_BINFOS (binfo))
	set_overrun ();
    }

  if (TYPE_LANG_SPECIFIC (type))
    {
      /* Read the vtables.  */
      tree vtables = chained_decls ();

      CLASSTYPE_VTABLES (type) = vtables;
      for (; vtables; vtables = TREE_CHAIN (vtables))
	DECL_INITIAL (vtables) = tree_node ();
    }

  if (TREE_CODE (maybe_template) == TEMPLATE_DECL)
    CLASSTYPE_DECL_LIST (type) = tree_node ();

  /* Propagate to all variants.  */
  fixup_type_variants (type);

  /* Now define all the members.  */
  while (tree_node ())
    if (get_overrun ())
      break;

  return type;
}

/* Stream an enum definition.  */

void
trees_out::define_enum (tree type, tree)
{
  gcc_assert (TYPE_MAIN_VARIANT (type) == type);

  tree_node (TYPE_VALUES (type));
  tree_node (TYPE_MIN_VALUE (type));
  tree_node (TYPE_MAX_VALUE (type));
}

tree
trees_in::define_enum (tree type, tree)
{
  gcc_assert (TYPE_MAIN_VARIANT (type) == type);

  tree values = tree_node ();
  tree min = tree_node ();
  tree max = tree_node ();

  if (get_overrun ())
    return NULL_TREE;

  TYPE_VALUES (type) = values;
  TYPE_MIN_VALUE (type) = min;
  TYPE_MAX_VALUE (type) = max;

  if (!ENUM_IS_SCOPED (type))
    {
      /* Inject the members into the containing scope.  */
      tree ctx = CP_DECL_CONTEXT (TYPE_NAME (type));
      unsigned mod_ix = DECL_MODULE_OWNER (TYPE_NAME (type));

      if (TREE_CODE (ctx) == NAMESPACE_DECL)
	for (; values; values = TREE_CHAIN (values))
	  {
	    tree cst = TREE_VALUE (values);

	    // FIXME: mark the CST as exported so lookup will find
	    // it.  It'd probably better for this cst's context to be
	    // the ENUM itself, even though it needs to be in the
	    // containing scope.
	    DECL_MODULE_EXPORT_P (cst) = true;
	    push_module_binding (ctx, DECL_NAME (cst), mod_ix, cst, NULL);
	  }
      else
	insert_late_enum_def_bindings (ctx, type);
    }

  return type;
}

/* Write out DECL's definition, if importers need it.  */

void
trees_out::maybe_tag_definition (tree t)
{
  tree maybe_template = NULL_TREE;

 again:
  switch (TREE_CODE (t))
    {
    default:
      return;

    case TEMPLATE_DECL:
      maybe_template = t;
      t = DECL_TEMPLATE_RESULT (t);
      goto again;

    case TYPE_DECL:
      if (!DECL_IMPLICIT_TYPEDEF_P (t))
	return;

      if (TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE
	  ? !TYPE_VALUES (TREE_TYPE (t))
	  : !TYPE_FIELDS (TREE_TYPE (t)))
	return;
      break;

    case FUNCTION_DECL:
      if (!DECL_INITIAL (t))
	/* Not defined.  */
	return;
      if (DECL_TEMPLATE_INFO (t))
	{
	  if (DECL_USE_TEMPLATE (t) & 1)
	    return;
	}
      else if (!DECL_DECLARED_INLINE_P (t))
	return;
      break;

    case VAR_DECL:
      if (!DECL_INITIAL (t))
	/* Nothing to define.  */
	return;

      if (!TREE_CONSTANT (t))
	return;
      break;
    }

  tag_definition (t, maybe_template);
}

/* Write out T's definition  */

void
trees_out::tag_definition (tree t, tree maybe_template)
{
  dump () && dump ("Writing%s definition for %C:%N%S",
		   maybe_template ? " template" : "", TREE_CODE (t), t, t);

  if (!maybe_template)
    maybe_template = t;
  tag (tt_definition);
  tree_node (maybe_template);

 again:
  switch (TREE_CODE (t))
    {
    default:
      // FIXME:Other things
      gcc_unreachable ();

    case FUNCTION_DECL:
      define_function (t, maybe_template);
      break;

    case VAR_DECL:
      define_var (t, maybe_template);
      break;

    case TYPE_DECL:
      if (DECL_IMPLICIT_TYPEDEF_P (t))
	{
	  t = TREE_TYPE (t);
	  goto again;
	}

      // FIXME:Actual typedefs
      gcc_unreachable ();

    case RECORD_TYPE:
    case UNION_TYPE:
      define_class (t, maybe_template);
      break;

    case ENUMERAL_TYPE:
      define_enum (t, maybe_template);
      break;
    }
}

tree
trees_in::tag_definition ()
{
  tree t = tree_node ();
  dump () && dump ("Reading definition for %C:%N%S", TREE_CODE (t), t, t);

  if (get_overrun ())
    return NULL_TREE;

  tree maybe_template = t;
  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

 again:
  switch (TREE_CODE (t))
    {
    default:
      // FIXME: read other things
      t = NULL_TREE;
      break;

    case FUNCTION_DECL:
      t = define_function (t, maybe_template);
      break;

    case VAR_DECL:
      t = define_var (t, maybe_template);
      break;

    case TYPE_DECL:
      if (DECL_IMPLICIT_TYPEDEF_P (t))
	{
	  t = TREE_TYPE (t);
	  goto again;
	}
      // FIXME:Actual typedefs
      t = NULL_TREE;
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      t = define_class (t, maybe_template);
      break;

    case ENUMERAL_TYPE:
      t = define_enum (t, maybe_template);
      break;
    }

  return t;
}

/* Read & write locations.  */

void
trees_out::loc (location_t)
{
  // FIXME:Do something
}

location_t
trees_in::loc ()
{
  // FIXME:Do something^-1
  return UNKNOWN_LOCATION;
}

/* Start tree write.  Write information to allocate the receiving
   node.  */

void
trees_out::start (tree_code code, tree t)
{
  switch (code)
    {
    default:
      if (TREE_CODE_CLASS (code) == tcc_vl_exp)
	u (VL_EXP_OPERAND_LENGTH (t));
      break;
    case IDENTIFIER_NODE:
      gcc_unreachable ();
      break;
    case TREE_BINFO:
      /* BINFOs are streamed specially */
      gcc_unreachable ();
      break;
    case TREE_VEC:
      u (TREE_VEC_LENGTH (t));
      break;
    case STRING_CST:
      str (TREE_STRING_POINTER (t), TREE_STRING_LENGTH (t));
      break;
    case VECTOR_CST:
      u (VECTOR_CST_LOG2_NPATTERNS (t));
      u (VECTOR_CST_NELTS_PER_PATTERN (t));
      break;
    case INTEGER_CST:
      u (TREE_INT_CST_NUNITS (t));
      u (TREE_INT_CST_EXT_NUNITS (t));
      u (TREE_INT_CST_OFFSET_NUNITS (t));
      break;
    case OMP_CLAUSE:
      gcc_unreachable (); // FIXME:
    }
}

/* Start tree read.  Allocate the receiving node.  */

tree
trees_in::start (tree_code code)
{
  tree t = NULL_TREE;

  switch (code)
    {
    default:
      if (TREE_CODE_CLASS (code) == tcc_vl_exp)
	{
	  unsigned ops = u ();
	  t = build_vl_exp (code, ops);
	}
      else
	t = make_node (code);
      break;
    case IDENTIFIER_NODE:
      gcc_unreachable ();
      break;
    case STRING_CST:
      {
	size_t l;
	const char *chars = str (&l);
	t = build_string (l, chars);
      }
      break;
    case TREE_BINFO:
      /* We should never find a naked binfo.  */
      break;
    case TREE_VEC:
      t = make_tree_vec (u ());
      break;
    case VECTOR_CST:
      t = make_vector (u (), u ());
      break;
    case INTEGER_CST:
      {
	unsigned n = u ();
	unsigned e = u ();
	t = make_int_cst (n, e);
	TREE_INT_CST_OFFSET_NUNITS(t) = u ();
      }
      break;
    case OMP_CLAUSE:
      gcc_unreachable (); // FIXME:
    }

  return t;
}

/* Semantic processing.  Add to symbol table etc.  Return
   possibly-remapped tree.  */

tree
trees_in::finish (tree t)
{
  if (TYPE_P (t))
    {
      bool on_pr_list = false;
      if (POINTER_TYPE_P (t))
	{
	  on_pr_list = t->type_non_common.minval != NULL;

	  t->type_non_common.minval = NULL;

	  tree probe = TREE_TYPE (t);
	  for (probe = (TREE_CODE (t) == POINTER_TYPE
			? TYPE_POINTER_TO (probe)
			: TYPE_REFERENCE_TO (probe));
	       probe;
	       probe = (TREE_CODE (t) == POINTER_TYPE
			? TYPE_NEXT_PTR_TO (probe)
			: TYPE_NEXT_REF_TO (probe)))
	    if (TYPE_MODE_RAW (probe) == TYPE_MODE_RAW (t)
		&& (TYPE_REF_CAN_ALIAS_ALL (probe)
		    == TYPE_REF_CAN_ALIAS_ALL (t)))
	      return probe;
	}

      tree remap = finish_type (t);
      if (remap == t && on_pr_list)
	{
	  tree to_type = TREE_TYPE (remap);
	  gcc_assert ((TREE_CODE (remap) == POINTER_TYPE
		       ? TYPE_POINTER_TO (to_type)
		       : TYPE_REFERENCE_TO (to_type)) != remap);
	  if (TREE_CODE (remap) == POINTER_TYPE)
	    {
	      TYPE_NEXT_PTR_TO (remap) = TYPE_POINTER_TO (to_type);
	      TYPE_POINTER_TO (to_type) = remap;
	    }
	  else
	    {
	      TYPE_NEXT_REF_TO (remap) = TYPE_REFERENCE_TO (to_type);
	      TYPE_REFERENCE_TO (to_type) = remap;
	    }
	}
      return remap;
    }

  if (DECL_P (t) && MAYBE_DECL_MODULE_OWNER (t) < MODULE_IMPORT_BASE)
    {
      // FIXME:Revisit
      tree ctx = CP_DECL_CONTEXT (t);

      // We should have dealt with namespaces elsewhere
      gcc_assert (TREE_CODE (t) != NAMESPACE_DECL || DECL_NAMESPACE_ALIAS (t));

      if (TREE_CODE (ctx) == NAMESPACE_DECL)
	{
	  /* A global-module decl.  See if there's already a duplicate.  */
	  tree old = merge_global_decl (ctx, state->mod, t);

	  if (!old)
	    error ("failed to merge %#qD", t);
	  else
	    dump () && dump ("%s decl %N%S, (%p)",
			     old == t ? "New" : "Existing",
			     old, old, (void *)old);

	  return old;
	}
    }

  if (TREE_CODE (t) == TEMPLATE_INFO)
    /* We're not a pending template in this TU.  */
    TI_PENDING_TEMPLATE_FLAG (t) = 0;

  if (TREE_CODE (t) == INTEGER_CST)
    {
      // FIXME:Remap small ints
      // FIXME:other consts too
    }

  return t;
}

/* The structure streamers access the raw fields, because the
   alternative, of using the accessor macros can require using
   different accessors for the same underlying field, depending on the
   tree code.  That's both confusing and annoying.  */

/* Read & write the core boolean flags.  */

void
trees_out::core_bools (tree t)
{
#define WB(X) (b (X))
  tree_code code = TREE_CODE (t);

  WB (t->base.side_effects_flag);
  WB (t->base.constant_flag);
  WB (t->base.addressable_flag);
  WB (t->base.volatile_flag);
  WB (t->base.readonly_flag);
  WB (t->base.asm_written_flag);
  WB (t->base.nowarning_flag);
  // visited is zero
  WB (t->base.used_flag); // FIXME: should we be dumping this?
  WB (t->base.nothrow_flag);
  WB (t->base.static_flag);
  WB (t->base.public_flag);
  WB (t->base.private_flag);
  WB (t->base.protected_flag);
  WB (t->base.deprecated_flag);
  WB (t->base.default_def_flag);

  switch (code)
    {
    case TREE_VEC:
    case INTEGER_CST:
    case CALL_EXPR:
    case SSA_NAME:
    case MEM_REF:
    case TARGET_MEM_REF:
      /* These use different base.u fields.  */
      break;

    case BLOCK:
      WB (t->block.abstract_flag);
      /* FALLTHROUGH  */

    default:
      WB (t->base.u.bits.lang_flag_0);
      WB (t->base.u.bits.lang_flag_1);
      WB (t->base.u.bits.lang_flag_2);
      WB (t->base.u.bits.lang_flag_3);
      WB (t->base.u.bits.lang_flag_4);
      WB (t->base.u.bits.lang_flag_5);
      WB (t->base.u.bits.lang_flag_6);
      WB (t->base.u.bits.saturating_flag);
      WB (t->base.u.bits.unsigned_flag);
      WB (t->base.u.bits.packed_flag);
      WB (t->base.u.bits.user_align);
      WB (t->base.u.bits.nameless_flag);
      WB (t->base.u.bits.atomic_flag);
      break;
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      WB (t->type_common.no_force_blk_flag);
      WB (t->type_common.needs_constructing_flag);
      WB (t->type_common.transparent_aggr_flag);
      WB (t->type_common.restrict_flag);
      WB (t->type_common.string_flag);
      WB (t->type_common.lang_flag_0);
      WB (t->type_common.lang_flag_1);
      WB (t->type_common.lang_flag_2);
      WB (t->type_common.lang_flag_3);
      WB (t->type_common.lang_flag_4);
      WB (t->type_common.lang_flag_5);
      WB (t->type_common.lang_flag_6);
      WB (t->type_common.typeless_storage);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      WB (t->decl_common.nonlocal_flag);
      WB (t->decl_common.virtual_flag);
      WB (t->decl_common.ignored_flag);
      WB (t->decl_common.abstract_flag);
      WB (t->decl_common.artificial_flag);
      WB (t->decl_common.preserve_flag);
      WB (t->decl_common.debug_expr_is_from);
      WB (t->decl_common.lang_flag_0);
      WB (t->decl_common.lang_flag_1);
      WB (t->decl_common.lang_flag_2);
      WB (t->decl_common.lang_flag_3);
      WB (t->decl_common.lang_flag_4);
      WB (t->decl_common.lang_flag_5);
      WB (t->decl_common.lang_flag_6);
      WB (t->decl_common.lang_flag_7);
      WB (t->decl_common.lang_flag_8);
      WB (t->decl_common.decl_flag_0);
      /* static variables become external.  */
      WB (t->decl_common.decl_flag_1
	  || (code == VAR_DECL && TREE_STATIC (t)
	      && !DECL_WEAK (t) && !DECL_VTABLE_OR_VTT_P (t)));
      WB (t->decl_common.decl_flag_2);
      WB (t->decl_common.decl_flag_3);
      WB (t->decl_common.gimple_reg_flag);
      WB (t->decl_common.decl_by_reference_flag);
      WB (t->decl_common.decl_read_flag);
      WB (t->decl_common.decl_nonshareable_flag);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      WB (t->decl_with_vis.defer_output);
      WB (t->decl_with_vis.hard_register);
      WB (t->decl_with_vis.common_flag);
      WB (t->decl_with_vis.in_text_section);
      WB (t->decl_with_vis.in_constant_pool);
      WB (t->decl_with_vis.dllimport_flag);
      WB (t->decl_with_vis.weak_flag);
      WB (t->decl_with_vis.seen_in_bind_expr);
      WB (t->decl_with_vis.comdat_flag);
      WB (t->decl_with_vis.visibility_specified);
      WB (t->decl_with_vis.comdat_flag);
      WB (t->decl_with_vis.init_priority_p);
      WB (t->decl_with_vis.shadowed_for_var_p);
      WB (t->decl_with_vis.cxx_constructor);
      WB (t->decl_with_vis.cxx_destructor);
      WB (t->decl_with_vis.final);
      WB (t->decl_with_vis.regdecl_flag);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      WB (t->function_decl.static_ctor_flag);
      WB (t->function_decl.static_dtor_flag);
      WB (t->function_decl.uninlinable);
      WB (t->function_decl.possibly_inlined);
      WB (t->function_decl.novops_flag);
      WB (t->function_decl.returns_twice_flag);
      WB (t->function_decl.malloc_flag);
      WB (t->function_decl.operator_new_flag);
      WB (t->function_decl.declared_inline_flag);
      WB (t->function_decl.no_inline_warning_flag);
      WB (t->function_decl.no_instrument_function_entry_exit);
      WB (t->function_decl.no_limit_stack);
      WB (t->function_decl.disregard_inline_limits);
      WB (t->function_decl.pure_flag);
      WB (t->function_decl.looping_const_or_pure_flag);
      WB (t->function_decl.has_debug_args_flag);
      WB (t->function_decl.tm_clone_flag);
      WB (t->function_decl.versioned_function);
    }
#undef WB
}

bool
trees_in::core_bools (tree t)
{
#define RB(X) ((X) = b ())
  tree_code code = TREE_CODE (t);

  RB (t->base.side_effects_flag);
  RB (t->base.constant_flag);
  RB (t->base.addressable_flag);
  RB (t->base.volatile_flag);
  RB (t->base.readonly_flag);
  RB (t->base.asm_written_flag);
  RB (t->base.nowarning_flag);
  // visited is zero
  RB (t->base.used_flag);
  RB (t->base.nothrow_flag);
  RB (t->base.static_flag);
  RB (t->base.public_flag);
  RB (t->base.private_flag);
  RB (t->base.protected_flag);
  RB (t->base.deprecated_flag);
  RB (t->base.default_def_flag);

  switch (code)
    {
    case TREE_VEC:
    case INTEGER_CST:
    case CALL_EXPR:
    case SSA_NAME:
    case MEM_REF:
    case TARGET_MEM_REF:
      /* These use different base.u fields.  */
      break;

    case BLOCK:
      RB (t->block.abstract_flag);
      /* FALLTHROUGH.  */

    default:
      RB (t->base.u.bits.lang_flag_0);
      RB (t->base.u.bits.lang_flag_1);
      RB (t->base.u.bits.lang_flag_2);
      RB (t->base.u.bits.lang_flag_3);
      RB (t->base.u.bits.lang_flag_4);
      RB (t->base.u.bits.lang_flag_5);
      RB (t->base.u.bits.lang_flag_6);
      RB (t->base.u.bits.saturating_flag);
      RB (t->base.u.bits.unsigned_flag);
      RB (t->base.u.bits.packed_flag);
      RB (t->base.u.bits.user_align);
      RB (t->base.u.bits.nameless_flag);
      RB (t->base.u.bits.atomic_flag);
      break;
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      RB (t->type_common.no_force_blk_flag);
      RB (t->type_common.needs_constructing_flag);
      RB (t->type_common.transparent_aggr_flag);
      RB (t->type_common.restrict_flag);
      RB (t->type_common.string_flag);
      RB (t->type_common.lang_flag_0);
      RB (t->type_common.lang_flag_1);
      RB (t->type_common.lang_flag_2);
      RB (t->type_common.lang_flag_3);
      RB (t->type_common.lang_flag_4);
      RB (t->type_common.lang_flag_5);
      RB (t->type_common.lang_flag_6);
      RB (t->type_common.typeless_storage);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      RB (t->decl_common.nonlocal_flag);
      RB (t->decl_common.virtual_flag);
      RB (t->decl_common.ignored_flag);
      RB (t->decl_common.abstract_flag);
      RB (t->decl_common.artificial_flag);
      RB (t->decl_common.preserve_flag);
      RB (t->decl_common.debug_expr_is_from);
      RB (t->decl_common.lang_flag_0);
      RB (t->decl_common.lang_flag_1);
      RB (t->decl_common.lang_flag_2);
      RB (t->decl_common.lang_flag_3);
      RB (t->decl_common.lang_flag_4);
      RB (t->decl_common.lang_flag_5);
      RB (t->decl_common.lang_flag_6);
      RB (t->decl_common.lang_flag_7);
      RB (t->decl_common.lang_flag_8);
      RB (t->decl_common.decl_flag_0);
      RB (t->decl_common.decl_flag_1);
      RB (t->decl_common.decl_flag_2);
      RB (t->decl_common.decl_flag_3);
      RB (t->decl_common.gimple_reg_flag);
      RB (t->decl_common.decl_by_reference_flag);
      RB (t->decl_common.decl_read_flag);
      RB (t->decl_common.decl_nonshareable_flag);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      RB (t->decl_with_vis.defer_output);
      RB (t->decl_with_vis.hard_register);
      RB (t->decl_with_vis.common_flag);
      RB (t->decl_with_vis.in_text_section);
      RB (t->decl_with_vis.in_constant_pool);
      RB (t->decl_with_vis.dllimport_flag);
      RB (t->decl_with_vis.weak_flag);
      RB (t->decl_with_vis.seen_in_bind_expr);
      RB (t->decl_with_vis.comdat_flag);
      RB (t->decl_with_vis.visibility_specified);
      RB (t->decl_with_vis.comdat_flag);
      RB (t->decl_with_vis.init_priority_p);
      RB (t->decl_with_vis.shadowed_for_var_p);
      RB (t->decl_with_vis.cxx_constructor);
      RB (t->decl_with_vis.cxx_destructor);
      RB (t->decl_with_vis.final);
      RB (t->decl_with_vis.regdecl_flag);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      RB (t->function_decl.static_ctor_flag);
      RB (t->function_decl.static_dtor_flag);
      RB (t->function_decl.uninlinable);
      RB (t->function_decl.possibly_inlined);
      RB (t->function_decl.novops_flag);
      RB (t->function_decl.returns_twice_flag);
      RB (t->function_decl.malloc_flag);
      RB (t->function_decl.operator_new_flag);
      RB (t->function_decl.declared_inline_flag);
      RB (t->function_decl.no_inline_warning_flag);
      RB (t->function_decl.no_instrument_function_entry_exit);
      RB (t->function_decl.no_limit_stack);
      RB (t->function_decl.disregard_inline_limits);
      RB (t->function_decl.pure_flag);
      RB (t->function_decl.looping_const_or_pure_flag);
      RB (t->function_decl.has_debug_args_flag);
      RB (t->function_decl.tm_clone_flag);
      RB (t->function_decl.versioned_function);
    }
#undef RB
  return !get_overrun ();
}

void
trees_out::lang_decl_bools (tree t)
{
#define WB(X) (b (X))
  const struct lang_decl *lang = DECL_LANG_SPECIFIC (t);

  WB (lang->u.base.language == lang_cplusplus);
  WB ((lang->u.base.use_template >> 0) & 1);
  WB ((lang->u.base.use_template >> 1) & 1);
  /* Vars stop being not really extern */
  WB (lang->u.base.not_really_extern
      && (TREE_CODE (t) != VAR_DECL
	  || DECL_VTABLE_OR_VTT_P (t) || DECL_WEAK (t)));
  WB (lang->u.base.initialized_in_class);
  WB (lang->u.base.repo_available_p);
  WB (lang->u.base.threadprivate_or_deleted_p);
  WB (lang->u.base.anticipated_p);
  WB (lang->u.base.friend_or_tls);
  WB (lang->u.base.odr_used);
  WB (lang->u.base.concept_p);
  WB (lang->u.base.var_declared_inline_p);
  WB (lang->u.base.dependent_init_p);
  gcc_checking_assert (lang->u.base.module_owner < MODULE_IMPORT_BASE);
  WB (lang->u.base.module_owner != 0);
  switch (lang->u.base.selector)
    {
    case lds_fn:  /* lang_decl_fn.  */
      WB (lang->u.fn.global_ctor_p);
      WB (lang->u.fn.global_dtor_p);
      WB (lang->u.fn.static_function);
      WB (lang->u.fn.pure_virtual);
      WB (lang->u.fn.defaulted_p);
      WB (lang->u.fn.has_in_charge_parm_p);
      WB (lang->u.fn.has_vtt_parm_p);
      /* There shouldn't be a pending inline at this point.  */
      gcc_assert (!lang->u.fn.pending_inline_p);
      WB (lang->u.fn.nonconverting);
      WB (lang->u.fn.thunk_p);
      WB (lang->u.fn.this_thunk_p);
      WB (lang->u.fn.hidden_friend_p);
      WB (lang->u.fn.omp_declare_reduction_p);
      /* FALLTHROUGH.  */
    case lds_min:  /* lang_decl_min.  */
      /* No bools.  */
      break;
    case lds_ns:  /* lang_decl_ns.  */
      /* No bools.  */
      break;
    case lds_parm:  /* lang_decl_parm.  */
      /* No bools.  */
      break;
    default:
      gcc_unreachable ();
    }
#undef WB
}

bool
trees_in::lang_decl_bools (tree t)
{
#define RB(X) ((X) = b ())
  struct lang_decl *lang = DECL_LANG_SPECIFIC (t);

  lang->u.base.language = b () ? lang_cplusplus : lang_c;
  unsigned v;
  v = b () << 0;
  v |= b () << 1;
  lang->u.base.use_template = v;
  RB (lang->u.base.not_really_extern);
  RB (lang->u.base.initialized_in_class);
  RB (lang->u.base.repo_available_p);
  RB (lang->u.base.threadprivate_or_deleted_p);
  RB (lang->u.base.anticipated_p);
  RB (lang->u.base.friend_or_tls);
  RB (lang->u.base.odr_used);
  RB (lang->u.base.concept_p);
  RB (lang->u.base.var_declared_inline_p);
  RB (lang->u.base.dependent_init_p);
  lang->u.base.module_owner = b () ? state->mod : MODULE_NONE;
  switch (lang->u.base.selector)
    {
    case lds_fn:  /* lang_decl_fn.  */
      RB (lang->u.fn.global_ctor_p);
      RB (lang->u.fn.global_dtor_p);
      RB (lang->u.fn.static_function);
      RB (lang->u.fn.pure_virtual);
      RB (lang->u.fn.defaulted_p);
      RB (lang->u.fn.has_in_charge_parm_p);
      RB (lang->u.fn.has_vtt_parm_p);
      RB (lang->u.fn.nonconverting);
      RB (lang->u.fn.thunk_p);
      RB (lang->u.fn.this_thunk_p);
      RB (lang->u.fn.hidden_friend_p);
      RB (lang->u.fn.omp_declare_reduction_p);
      /* FALLTHROUGH.  */
    case lds_min:  /* lang_decl_min.  */
      /* No bools.  */
      break;
    case lds_ns:  /* lang_decl_ns.  */
      /* No bools.  */
      break;
    case lds_parm:  /* lang_decl_parm.  */
      /* No bools.  */
      break;
    default:
      gcc_unreachable ();
    }
#undef RB
  return !get_overrun ();
}

void
trees_out::lang_type_bools (tree t)
{
#define WB(X) (b (X))
  const struct lang_type *lang = TYPE_LANG_SPECIFIC (t);

  WB (lang->has_type_conversion);
  WB (lang->has_copy_ctor);
  WB (lang->has_default_ctor);
  WB (lang->const_needs_init);
  WB (lang->ref_needs_init);
  WB (lang->has_const_copy_assign);
  WB ((lang->use_template >> 0) & 1);
  WB ((lang->use_template >> 1) & 1);

  WB (lang->has_mutable);
  WB (lang->com_interface);
  WB (lang->non_pod_class);
  WB (lang->nearly_empty_p);
  WB (lang->user_align);
  WB (lang->has_copy_assign);
  WB (lang->has_new);
  WB (lang->has_array_new);
  WB ((lang->gets_delete >> 0) & 1);
  WB ((lang->gets_delete >> 1) & 1);
  // Interfaceness is recalculated upon reading.  May have to revisit?
  // lang->interface_only
  // lang->interface_unknown
  WB (lang->contains_empty_class_p);
  WB (lang->anon_aggr);
  WB (lang->non_zero_init);
  WB (lang->empty_p);
  WB (lang->vec_new_uses_cookie);
  WB (lang->declared_class);
  WB (lang->diamond_shaped);
  WB (lang->repeated_base);
  gcc_assert (!lang->being_defined);
  WB (lang->debug_requested);
  WB (lang->fields_readonly);
  WB (lang->ptrmemfunc_flag);
  WB (lang->was_anonymous);
  WB (lang->lazy_default_ctor);
  WB (lang->lazy_copy_ctor);
  WB (lang->lazy_copy_assign);
  WB (lang->lazy_destructor);
  WB (lang->has_const_copy_ctor);
  WB (lang->has_complex_copy_ctor);
  WB (lang->has_complex_copy_assign);
  WB (lang->non_aggregate);
  WB (lang->has_complex_dflt);
  WB (lang->has_list_ctor);
  WB (lang->non_std_layout);
  WB (lang->is_literal);
  WB (lang->lazy_move_ctor);
  WB (lang->lazy_move_assign);
  WB (lang->has_complex_move_ctor);
  WB (lang->has_complex_move_assign);
  WB (lang->has_constexpr_ctor);
  WB (lang->unique_obj_representations);
  WB (lang->unique_obj_representations_set);
#undef WB
}

bool
trees_in::lang_type_bools (tree t)
{
#define RB(X) ((X) = b ())
  struct lang_type *lang = TYPE_LANG_SPECIFIC (t);

  RB (lang->has_type_conversion);
  RB (lang->has_copy_ctor);
  RB (lang->has_default_ctor);
  RB (lang->const_needs_init);
  RB (lang->ref_needs_init);
  RB (lang->has_const_copy_assign);
  unsigned v;
  v = b () << 0;
  v |= b () << 1;
  lang->use_template = v;

  RB (lang->has_mutable);
  RB (lang->com_interface);
  RB (lang->non_pod_class);
  RB (lang->nearly_empty_p);
  RB (lang->user_align);
  RB (lang->has_copy_assign);
  RB (lang->has_new);
  RB (lang->has_array_new);
  v = b () << 0;
  v |= b () << 1;
  lang->gets_delete = v;
  // lang->interface_only
  // lang->interface_unknown
  lang->interface_unknown = true; // Redetermine interface
  RB (lang->contains_empty_class_p);
  RB (lang->anon_aggr);
  RB (lang->non_zero_init);
  RB (lang->empty_p);
  RB (lang->vec_new_uses_cookie);
  RB (lang->declared_class);
  RB (lang->diamond_shaped);
  RB (lang->repeated_base);
  gcc_assert (!lang->being_defined);
  RB (lang->debug_requested);
  RB (lang->fields_readonly);
  RB (lang->ptrmemfunc_flag);
  RB (lang->was_anonymous);
  RB (lang->lazy_default_ctor);
  RB (lang->lazy_copy_ctor);
  RB (lang->lazy_copy_assign);
  RB (lang->lazy_destructor);
  RB (lang->has_const_copy_ctor);
  RB (lang->has_complex_copy_ctor);
  RB (lang->has_complex_copy_assign);
  RB (lang->non_aggregate);
  RB (lang->has_complex_dflt);
  RB (lang->has_list_ctor);
  RB (lang->non_std_layout);
  RB (lang->is_literal);
  RB (lang->lazy_move_ctor);
  RB (lang->lazy_move_assign);
  RB (lang->has_complex_move_ctor);
  RB (lang->has_complex_move_assign);
  RB (lang->has_constexpr_ctor);
  RB (lang->unique_obj_representations);
  RB (lang->unique_obj_representations_set);
#undef RB
  return !get_overrun ();
}

/* Read & write the core values and pointers.  */

void
trees_out::core_vals (tree t)
{
#define WU(X) (u (X))
#define WT(X) (tree_node (X))
  tree_code code = TREE_CODE (t);

  switch (code)
    {
    case TREE_VEC:
    case INTEGER_CST:
      /* Length written earlier.  */
      break;
    case CALL_EXPR:
      WU (t->base.u.ifn);
      break;
    case SSA_NAME:
    case MEM_REF:
    case TARGET_MEM_REF:
      /* We shouldn't meet these.  */
      gcc_unreachable ();

    default:
      break;
    }

  /* The ordering here is that in tree-core.h & cp-tree.h.  */
  if (CODE_CONTAINS_STRUCT (code, TS_BASE))
    { /* Nothing to do.  */ }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    WT (t->typed.type);

  if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
    {
      /* Whether TREE_CHAIN is dumped depends on who's containing it.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_INT_CST))
    {
      unsigned num = TREE_INT_CST_EXT_NUNITS (t);
      for (unsigned ix = 0; ix != num; ix++)
	wu (TREE_INT_CST_ELT (t, ix));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    gcc_unreachable (); // FIXME
  
  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    gcc_unreachable (); // FIXME
  
  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    gcc_unreachable (); /* Should never meet.  */

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      loc (t->decl_minimal.locus);
      WT (t->decl_minimal.name);
      WT (t->decl_minimal.context);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      WU (t->decl_common.mode);
      WU (t->decl_common.off_align);
      WU (t->decl_common.align);

      WT (t->decl_common.size_unit);
      WT (t->decl_common.attributes);
      switch (code)
	// FIXME: Perhaps this should be done with the later
	// polymorphic check?
	{
	default:
	  break;
	case VAR_DECL:
	  if (TREE_CODE (DECL_CONTEXT (t)) != FUNCTION_DECL)
	    break;
	  /* FALLTHROUGH */
	case PARM_DECL:
	case CONST_DECL:
	  WT (t->decl_common.initial);
	  break;
	}
      /* decl_common.initial, decl_common.abstract_origin.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    {
      /* decl_non_common.result. */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_PARM_DECL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      WT (t->decl_with_vis.assembler_name);
      WU (t->decl_with_vis.visibility);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VAR_DECL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    {
      WT (t->field_decl.offset);
      WT (t->field_decl.bit_field_type);
      WT (t->field_decl.qualifier);
      WT (t->field_decl.bit_offset);
      WT (t->field_decl.fcontext);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LABEL_DECL))
    {
      WU (t->label_decl.label_decl_uid);
      WU (t->label_decl.eh_landing_pad_nr);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_RESULT_DECL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_CONST_DECL))
    { /* No extra fields.  */ }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      chained_decls (t->function_decl.arguments);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    gcc_unreachable (); /* Should never meet.  */

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      /* By construction we want to make sure we have the canonical
	 and main variants already in the type table, so emit them
	 now.  */
      WT (t->type_common.main_variant);
      WT (t->type_common.canonical);

      /* type_common.next_variant is internally manipulated.  */
      /* type_common.pointer_to, type_common.reference_to.  */

      WU (t->type_common.precision);
      WU (t->type_common.contains_placeholder_bits);
      WU (t->type_common.mode);
      WU (t->type_common.align);

      WT (t->type_common.size);
      WT (t->type_common.size_unit);
      WT (t->type_common.attributes);
      WT (t->type_common.name);
      WT (t->type_common.context);

      WT (t->type_common.common.chain); /* TYPE_STUB_DECL.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_WITH_LANG_SPECIFIC))
    { /* Nothing to do.  */ }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      /* Records and unions hold FIELDS, VFIELD & BINFO on these
	 things.  */
      if (!RECORD_OR_UNION_CODE_P (code) && code != ENUMERAL_TYPE)
	{
	  /* Don't write the cached values vector.  */
	  if (!TYPE_CACHED_VALUES_P (t))
	    WT (t->type_non_common.values);
	  /* POINTER and REFERENCE types hold NEXT_{PTR,REF}_TO */
	  if (POINTER_TYPE_P (t))
	    {
	      /* We need to record whether we're on the
		 TYPE_{POINTER,REFERENCE}_TO list of the type we refer
		 to.  Do that by recording NULL or self reference
		 here.  */
	      tree probe = TREE_TYPE (t);
	      for (probe = (TREE_CODE (t) == POINTER_TYPE
			    ? TYPE_POINTER_TO (probe)
			    : TYPE_REFERENCE_TO (probe));
		   probe && probe != t;
		   probe = (TREE_CODE (t) == POINTER_TYPE
			    ? TYPE_NEXT_PTR_TO (probe)
			    : TYPE_NEXT_REF_TO (probe)))
		continue;
	      WT (probe);
	    }
	  else
	    WT (t->type_non_common.minval);
	  WT (t->type_non_common.maxval);
	}
      WT (t->type_non_common.lang_1);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    {
      WT (t->list.purpose);
      WT (t->list.value);
      WT (t->list.common.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    for (unsigned ix = TREE_VEC_LENGTH (t); ix--;)
      WT (TREE_VEC_ELT (t, ix));

  if (TREE_CODE_CLASS (code) == tcc_vl_exp)
    for (unsigned ix = VL_EXP_OPERAND_LENGTH (t); --ix;)
      WT (TREE_OPERAND (t, ix));
  else if (CODE_CONTAINS_STRUCT (code, TS_EXP)
	   /* FIXME:For some reason, some tcc_expression nodes do not claim
	      to contain TS_EXP.  I think this is a bug. */
	   || TREE_CODE_CLASS (code) == tcc_expression
	   || TREE_CODE_CLASS (code) == tcc_binary
	   || TREE_CODE_CLASS (code) == tcc_unary)
    for (unsigned ix = TREE_OPERAND_LENGTH (t); ix--;)
      WT (TREE_OPERAND (t, ix));

  if (CODE_CONTAINS_STRUCT (code, TS_SSA_NAME))
    gcc_unreachable (); /* Should not see.  */

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    {
      WT (t->block.supercontext);
      chained_decls (t->block.vars);
      WT (t->block.abstract_origin);
      // FIXME nonlocalized_vars, fragment_origin, fragment_chain
      WT (t->block.subblocks);
      WT (t->block.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    /* BINFOs are streamed specially.  */
    gcc_unreachable ();

  if (CODE_CONTAINS_STRUCT (code, TS_STATEMENT_LIST))
    {
      for (tree_stmt_iterator iter = tsi_start (t);
	   !tsi_end_p (iter); tsi_next (&iter))
	if (tree stmt = tsi_stmt (iter))
	  WT (stmt);
      WT (NULL_TREE);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    {
      unsigned len = vec_safe_length (t->constructor.elts);
      WU (len);
      if (len)
	for (unsigned ix = 0; ix != len; ix++)
	  {
	    const constructor_elt &elt = (*t->constructor.elts)[ix];

	    WT (elt.index);
	    WT (elt.value);
	  }
    }

  if (CODE_CONTAINS_STRUCT (code, TS_OMP_CLAUSE))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    gcc_unreachable (); // FIXME

  /* Now the C++-specific nodes.  These are disjoint. While we could
     use CODE directly, going via cp_tree_node_structure makes it
     easy to see whether we're missing cases.  */
  switch (cp_tree_node_structure (code))
    {
    case TS_CP_GENERIC:
      break;

    case TS_CP_TPI:
      WU (((lang_tree_node *)t)->tpi.index);
      WU (((lang_tree_node *)t)->tpi.level);
      WU (((lang_tree_node *)t)->tpi.orig_level);
      WT (((lang_tree_node *)t)->tpi.decl);
      break;
      
    case TS_CP_PTRMEM:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_OVERLOAD:
      WT (((lang_tree_node *)t)->overload.function);
      WT (t->common.chain);
      break;
      
    case TS_CP_MODULE_VECTOR:
      gcc_unreachable (); /* Should never see.  */
      break;

    case TS_CP_BASELINK:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_TEMPLATE_DECL:
      WT (((lang_tree_node *)t)->template_decl.arguments);
      WT (((lang_tree_node *)t)->template_decl.result);
      break;

    case TS_CP_DEFAULT_ARG:
      gcc_unreachable (); /* Should never see.  */
      break;

    case TS_CP_DEFERRED_NOEXCEPT:
      WT (((lang_tree_node *)t)->deferred_noexcept.pattern);
      WT (((lang_tree_node *)t)->deferred_noexcept.args);
      break;

    case TS_CP_IDENTIFIER:
      gcc_unreachable (); /* Should never see.  */
      break;

    case TS_CP_STATIC_ASSERT:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_ARGUMENT_PACK_SELECT:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_TRAIT_EXPR:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_LAMBDA_EXPR:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_TEMPLATE_INFO:
      // TI_TEMPLATE -> TYPE
      WT (t->common.chain); // TI_ARGS
      // FIXME typedefs_needing_access_checking
      break;

    case TS_CP_CONSTRAINT_INFO:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_USERDEF_LITERAL:
      gcc_unreachable (); // FIXME
      break;
    }

#undef WT
#undef WU
}

bool
trees_in::core_vals (tree t)
{
#define RU(X) ((X) = u ())
#define RUC(T,X) ((X) = T (u ()))
#define RT(X) ((X) = tree_node ())
  tree_code code = TREE_CODE (t);

  switch (code)
    {
    case TREE_VEC:
    case INTEGER_CST:
      /* Length read earlier.  */
      break;
    case CALL_EXPR:
      RUC (internal_fn, t->base.u.ifn);
      break;
    case SSA_NAME:
    case MEM_REF:
    case TARGET_MEM_REF:
      /* We shouldn't meet these.  */
      return false;

    default:
      break;
    }

  /* The ordering here is that in tree-core.h & cp-tree.h.  */
  if (CODE_CONTAINS_STRUCT (code, TS_BASE))
    { /* Nothing to do.  */ }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    RT (t->typed.type);

  if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
    {
      /* Whether TREE_CHAIN is dumped depends on who's containing it.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_INT_CST))
    {
      unsigned num = TREE_INT_CST_EXT_NUNITS (t);
      for (unsigned ix = 0; ix != num; ix++)
	TREE_INT_CST_ELT (t, ix) = wu ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    gcc_unreachable (); // FIXME
  
  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    return false; /* Should never meet.  */

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      /* Don't zap the locus just yet, we don't record it correctly
	 and thus lose all location information.  */
      /* t->decl_minimal.locus = */
      loc ();
      RT (t->decl_minimal.name);
      RT (t->decl_minimal.context);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      RUC (machine_mode, t->decl_common.mode);
      RU (t->decl_common.off_align);
      RU (t->decl_common.align);

      RT (t->decl_common.size_unit);
      RT (t->decl_common.attributes);
      switch (code)
	// FIXME: Perhaps this should be done with the later
	// polymorphic check?
	{
	default:
	  break;
	case VAR_DECL:
	  if (TREE_CODE (DECL_CONTEXT (t)) != FUNCTION_DECL)
	    break;
	  /* FALLTHROUGH */
	case PARM_DECL:
	case CONST_DECL:
	  RT (t->decl_common.initial);
	  break;
	}
      /* decl_common.initial, decl_common.abstract_origin.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_NON_COMMON))
    {
      /* decl_non_common.result. */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_PARM_DECL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      RT (t->decl_with_vis.assembler_name);
      RUC (symbol_visibility, t->decl_with_vis.visibility);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VAR_DECL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    {
      RT (t->field_decl.offset);
      RT (t->field_decl.bit_field_type);
      RT (t->field_decl.qualifier);
      RT (t->field_decl.bit_offset);
      RT (t->field_decl.fcontext);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LABEL_DECL))
    {
      RU (t->label_decl.label_decl_uid);
      RU (t->label_decl.eh_landing_pad_nr);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_RESULT_DECL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_CONST_DECL))
    { /* No extra fields.  */ }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      t->function_decl.arguments = chained_decls ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    return false;

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      RT (t->type_common.main_variant);
      RT (t->type_common.canonical);

      /* type_common.next_variant is internally manipulated.  */
      /* type_common.pointer_to, type_common.reference_to.  */

      RU (t->type_common.precision);
      RU (t->type_common.contains_placeholder_bits);
      RUC (machine_mode, t->type_common.mode);
      RU (t->type_common.align);

      RT (t->type_common.size);
      RT (t->type_common.size_unit);
      RT (t->type_common.attributes);
      RT (t->type_common.name);
      RT (t->type_common.context);

      RT (t->type_common.common.chain); /* TYPE_STUB_DECL.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_WITH_LANG_SPECIFIC))
    { /* Nothing to do.  */ }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      /* Records and unions hold FIELDS, VFIELD & BINFO on these
	 things.  */
      if (!RECORD_OR_UNION_CODE_P (code) && code != ENUMERAL_TYPE)
	{
	  if (!TYPE_CACHED_VALUES_P (t))
	    RT (t->type_non_common.values);
	  else
	    /* Clear the type cached values.  */
	    TYPE_CACHED_VALUES_P (t) = 0;

	  /* POINTER and REFERENCE types hold NEXT_{PTR,REF}_TO.  We
	     store a marker there to indicate whether we're on the
	     referred to type's pointer/reference to list.  */
	  RT (t->type_non_common.minval);
	  if (POINTER_TYPE_P (t) && t->type_non_common.minval
	      && t->type_non_common.minval != t)
	    {
	      t->type_non_common.minval = NULL_TREE;
	      set_overrun ();
	    }
	  RT (t->type_non_common.maxval);
	}
      RT (t->type_non_common.lang_1);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    {
      RT (t->list.purpose);
      RT (t->list.value);
      RT (t->list.common.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    for (unsigned ix = TREE_VEC_LENGTH (t); ix--;)
      RT (TREE_VEC_ELT (t, ix));

  if (TREE_CODE_CLASS (code) == tcc_vl_exp)
    for (unsigned ix = VL_EXP_OPERAND_LENGTH (t); --ix;)
      RT (TREE_OPERAND (t, ix));
  else if (CODE_CONTAINS_STRUCT (code, TS_EXP)
	   /* See comment in trees_out::core_vals.  */
	   || TREE_CODE_CLASS (code) == tcc_expression
	   || TREE_CODE_CLASS (code) == tcc_binary
	   || TREE_CODE_CLASS (code) == tcc_unary)
    for (unsigned ix = TREE_OPERAND_LENGTH (t); ix--;)
      RT (TREE_OPERAND (t, ix));

  if (CODE_CONTAINS_STRUCT (code, TS_SSA_NAME))
    return false;

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    {
      RT (t->block.supercontext);
      t->block.vars = chained_decls ();
      RT (t->block.abstract_origin);
      // FIXME nonlocalized_vars, fragment_origin, fragment_chain
      RT (t->block.subblocks);
      RT (t->block.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    /* We should never see a naked binfo.  */
    gcc_unreachable ();

  if (CODE_CONTAINS_STRUCT (code, TS_STATEMENT_LIST))
    {
      tree_stmt_iterator iter = tsi_start (t);
      for (tree stmt; RT (stmt);)
	tsi_link_after (&iter, stmt, TSI_CONTINUE_LINKING);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    {
      if (unsigned len = u ())
	{
	  vec_alloc (t->constructor.elts, len);
	  for (unsigned ix = 0; ix != len; ix++)
	    {
	      constructor_elt elt;

	      RT (elt.index);
	      RT (elt.value);
	      t->constructor.elts->quick_push (elt);
	    }
	}
    }

  if (CODE_CONTAINS_STRUCT (code, TS_OMP_CLAUSE))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    gcc_unreachable (); // FIXME

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    gcc_unreachable (); // FIXME

  /* Now the C++-specific nodes.  These are disjoint. While we could
     use CODE directly, going via cp_tree_node_structure makes it
     easy to see whether we're missing cases.  */
  switch (cp_tree_node_structure (code))
    {
    case TS_CP_GENERIC:
      break;

    case TS_CP_TPI:
      RU (((lang_tree_node *)t)->tpi.index);
      RU (((lang_tree_node *)t)->tpi.level);
      RU (((lang_tree_node *)t)->tpi.orig_level);
      RT (((lang_tree_node *)t)->tpi.decl);
      break;

    case TS_CP_PTRMEM:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_OVERLOAD:
      RT (((lang_tree_node *)t)->overload.function);
      RT (t->common.chain);
      break;

    case TS_CP_MODULE_VECTOR:
      return false;

    case TS_CP_BASELINK:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_TEMPLATE_DECL:
      RT (((lang_tree_node *)t)->template_decl.arguments);
      RT (((lang_tree_node *)t)->template_decl.result);
      break;

    case TS_CP_DEFAULT_ARG:
      return false;

    case TS_CP_DEFERRED_NOEXCEPT:
      RT (((lang_tree_node *)t)->deferred_noexcept.pattern);
      RT (((lang_tree_node *)t)->deferred_noexcept.args);
      break;

    case TS_CP_IDENTIFIER:
      return false; /* Should never see.  */

    case TS_CP_STATIC_ASSERT:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_ARGUMENT_PACK_SELECT:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_TRAIT_EXPR:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_LAMBDA_EXPR:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_TEMPLATE_INFO:
      // TI_TEMPLATE -> TYPE
      RT (t->common.chain); // TI_ARGS
      // FIXME typedefs_needing_access_checking
      break;

    case TS_CP_CONSTRAINT_INFO:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_USERDEF_LITERAL:
      gcc_unreachable (); // FIXME
      break;
    }

#undef RT
#undef RM
#undef RU
  return !get_overrun ();
}

void
trees_out::lang_decl_vals (tree t)
{
  const struct lang_decl *lang = DECL_LANG_SPECIFIC (t);
#define WU(X) (u (X))
#define WT(X) (tree_node (X))
  /* Module index already written.  */
  switch (lang->u.base.selector)
    {
    case lds_fn:  /* lang_decl_fn.  */
      if (DECL_NAME (t) && IDENTIFIER_OVL_OP_P (DECL_NAME (t)))
	WU (lang->u.fn.ovl_op_code);
      if (lang->u.fn.thunk_p)
	wi (lang->u.fn.u5.fixed_offset);
      else
	WT (lang->u.fn.u5.cloned_function);
      /* FALLTHROUGH.  */
    case lds_min:  /* lang_decl_min.  */
      WT (lang->u.min.template_info);
      WT (lang->u.min.access);
      break;
    case lds_ns:  /* lang_decl_ns.  */
      break;
    case lds_parm:  /* lang_decl_parm.  */
      WU (lang->u.parm.level);
      WU (lang->u.parm.index);
      break;
    default:
      gcc_unreachable ();
    }
#undef WU
#undef WT
}

bool
trees_in::lang_decl_vals (tree t)
{
  struct lang_decl *lang = DECL_LANG_SPECIFIC (t);
#define RU(X) ((X) = u ())
#define RT(X) ((X) = tree_node ())

  /* Module index already read.  */

  switch (lang->u.base.selector)
    {
    case lds_fn:  /* lang_decl_fn.  */
      {
	if (DECL_NAME (t) && IDENTIFIER_OVL_OP_P (DECL_NAME (t)))
	  {
	    unsigned code = u ();

	    /* Check consistency.  */
	    if (code >= OVL_OP_MAX
		|| (ovl_op_info[IDENTIFIER_ASSIGN_OP_P (DECL_NAME (t))][code]
		    .ovl_op_code) == OVL_OP_ERROR_MARK)
	      set_overrun ();
	    else
	      lang->u.fn.ovl_op_code = code;
	  }

	if (lang->u.fn.thunk_p)
	  lang->u.fn.u5.fixed_offset = wi ();
	else
	  RT (lang->u.fn.u5.cloned_function);
      }
      /* FALLTHROUGH.  */
    case lds_min:  /* lang_decl_min.  */
      RT (lang->u.min.template_info);
      RT (lang->u.min.access);
      break;
    case lds_ns:  /* lang_decl_ns.  */
      break;
    case lds_parm:  /* lang_decl_parm.  */
      RU (lang->u.parm.level);
      RU (lang->u.parm.index);
      break;
    default:
      gcc_unreachable ();
    }
#undef RU
#undef RT
  return !get_overrun ();
}

/* Most of the value contents of lang_type is streamed in
   define_class.  */

void
trees_out::lang_type_vals (tree t)
{
  const struct lang_type *lang = TYPE_LANG_SPECIFIC (t);
#define WU(X) (u (X))
#define WT(X) (tree_node (X))
  WU (lang->align);
  WT (lang->befriending_classes);
#undef WU
#undef WT
}

bool
trees_in::lang_type_vals (tree t)
{
  struct lang_type *lang = TYPE_LANG_SPECIFIC (t);
#define RU(X) ((X) = u ())
#define RT(X) ((X) = tree_node ())
  RU (lang->align);
  RT (lang->befriending_classes);
#undef RU
#undef RT
  return !get_overrun ();
}

/* Stream the BINFO tree owned by TYPE.  We only write sufficient
   information to recreate the tree -- not populate it.  */

void
trees_out::tree_binfo (tree type)
{
  /* Stream out types and sizes in DFS order, inserting each binfo
     into the map.  */
  for (tree child = TYPE_BINFO (type); child; child = TREE_CHAIN (child))
    {
      tree_node (BINFO_TYPE (child));
      u (BINFO_N_BASE_BINFOS (child));

      int tag = insert (child);
      dump () && dump ("Wrote binfo:%d child %N", tag, BINFO_TYPE (child));
    }
  tree_node (NULL_TREE);
  if (TYPE_LANG_SPECIFIC (type))
    {
      unsigned nvbases = vec_safe_length (CLASSTYPE_VBASECLASSES (type));
      u (nvbases);
      if (nvbases)
	dump () && dump ("Type %N has %u vbases", type, nvbases);
    }

  /* Stream out some contents DFS order.  */
  for (tree child = TYPE_BINFO (type); child; child = TREE_CHAIN (child))
    {
      core_bools (child);
      bflush ();
#define WT(X) (tree_node (X))
      WT (child->binfo.offset);
      WT (child->binfo.inheritance);
#undef WT
      unsigned num = BINFO_N_BASE_BINFOS (child);
      u (num);
      for (unsigned ix = 0; ix != num; ix++)
	tree_node (BINFO_BASE_BINFO (child, ix));
    }
}

bool
trees_in::tree_binfo (tree type)
{
  tree binfo = NULL_TREE;

  /* Stream in the types and sizes in DFS order.  */
  while (tree t = tree_node ())
    {
      unsigned n_children = u ();
      if (get_overrun ())
	return false;
      tree child = make_tree_binfo (n_children);
      BINFO_TYPE (child) = t;

      int tag = insert (child);
      dump () && dump ("Read binfo:%d child %N", tag, BINFO_TYPE (child));
      TREE_CHAIN (child) = binfo;
      binfo = child;
    }
  binfo = nreverse (binfo);
  TYPE_BINFO (type) = binfo;

  unsigned nvbases = 0;
  vec<tree, va_gc> *vbase_vec = NULL;
  if (TYPE_LANG_SPECIFIC (type))
    {
      nvbases = u ();
      if (nvbases)
	{
	  vec_alloc (vbase_vec, nvbases);
	  CLASSTYPE_VBASECLASSES (type) = vbase_vec;
	  dump () && dump ("Type %N has %u vbases", type, nvbases);
	}
    }

  /* Stream in some contents in DFS order.  */
  for (tree child = binfo; child; child = TREE_CHAIN (child))
    {
      core_bools (child);
      bflush ();
#define RT(X) ((X) = tree_node ())
      RT (child->binfo.offset);
      RT (child->binfo.inheritance);
#undef RT
      unsigned num = u ();
      if (get_overrun ())
	return false;
      for (unsigned ix = 0; ix != num; ix++)
	BINFO_BASE_APPEND (child, tree_node ());
      if (get_overrun ())
	return false;
      if (BINFO_VIRTUAL_P (child))
	{
	  if (vec_safe_length (vbase_vec) == nvbases)
	    {
	      set_overrun ();
	      return false;
	    }
	  vbase_vec->quick_push (child);
	}
    }

  return true;
}

/* The raw tree node.  We've already dealt with the code, and in the
   case of decls, determining name, context & module.  Stream the
   bools and vals without post-processing.  */

void
trees_out::tree_node_raw (tree t)
{
  tree_code_class klass = TREE_CODE_CLASS (TREE_CODE (t));
  bool specific = false;

  /* The only decls we should stream out are those from this module,
     or the global module.  */
  gcc_assert (klass != tcc_declaration
	      || MAYBE_DECL_MODULE_OWNER (t) == MODULE_NONE
	      || t != get_module_owner (t)
	      || MAYBE_DECL_MODULE_OWNER (t) == MODULE_PURVIEW);
  if (klass == tcc_type || klass == tcc_declaration)
    {
      if (klass == tcc_declaration)
	specific = DECL_LANG_SPECIFIC (t) != NULL;
      else if (TYPE_MAIN_VARIANT (t) == t)
	specific = TYPE_LANG_SPECIFIC (t) != NULL;
      else
	gcc_assert (TYPE_LANG_SPECIFIC (t)
		    == TYPE_LANG_SPECIFIC (TYPE_MAIN_VARIANT (t)));
      b (specific);
      if (specific && VAR_P (t))
	b (DECL_DECOMPOSITION_P (t));
    }

  core_bools (t);
  if (specific)
    {
      if (klass == tcc_type)
	lang_type_bools (t);
      else
	lang_decl_bools (t);
    }
  bflush ();

  core_vals (t);
  if (specific)
    {
      if (klass == tcc_type)
	lang_type_vals (t);
      else
	lang_decl_vals (t);
    }
}

bool
trees_in::tree_node_raw (tree t)
{
  tree_code_class klass = TREE_CODE_CLASS (TREE_CODE (t));
  bool specific = false;
  bool lied = false;

  if (klass == tcc_type || klass == tcc_declaration)
    {
      specific = b ();
      if (specific
	  &&  (klass == tcc_type
	       ? !maybe_add_lang_type_raw (t)
	       : !maybe_add_lang_decl_raw (t, VAR_P (t) && b ())))
	  lied = true;
    }

  if (!core_bools (t))
    lied = true;
  else if (specific)
    {
      if (klass == tcc_type
	  ? !lang_type_bools (t)
	  : !lang_decl_bools (t))
	lied = true;
    }
  bflush ();
  if (lied || get_overrun ())
    return false;

  if (!core_vals (t))
    return false;

  if (specific)
    {
      if (klass == tcc_type)
	{
	  gcc_assert (TYPE_MAIN_VARIANT (t) == t);
	  if (!lang_type_vals (t))
	    return false;
	}
      else
	{
	  if (!lang_decl_vals (t))
	    return false;
	}
    }
  else if (klass == tcc_type)
    TYPE_LANG_SPECIFIC (t) = TYPE_LANG_SPECIFIC (TYPE_MAIN_VARIANT (t));

  return true;
}

/* Stream out tree node T.  We automatically create local back
   references, which is essentially the lisp self-referential
   structure pretty-printer.  */

void
trees_out::tree_node (tree t)
{
  dump.indent ();
 again:
  if (!t)
    {
      /* NULL_TREE -> tt_null.  */
      nulls++;
      i (tt_null);
      goto done;
    }

  if (TREE_VISITED (t))
    {
      /* An already-visited tree.  It must be in the map.  */
      int val = *tree_map.get (t);

      refs++;
      if (val <= tt_backref)
	/* Back reference -> -ve number  */
	i (val);
      else
	/* Fixed reference -> tt_fixed */
	i (tt_fixed), u (val);
      dump () && dump ("Wrote %s:%d %C:%N%S", val < 0 ? "backref" : "fixed",
		       val, TREE_CODE (t), t, t);
      goto done;
    }

  if (TREE_CODE (t) == NAMESPACE_DECL && !DECL_NAMESPACE_ALIAS (t))
    {
      /* A Namespace -> tt_namespace.  */
      // FIXME: anonymous
      gcc_assert (TREE_PUBLIC (t));
      i (tt_namespace);
      tree_node (CP_DECL_CONTEXT (t));
      tree_node (DECL_NAME (t));
      unsigned tag = insert (t);
      dump () && dump ("Wrote:%d namespace %N", tag, t);
      goto done;
    }

  if (TREE_CODE (t) == VAR_DECL && DECL_TINFO_P (t))
    {
      /* A typeinfo object -> tt_tinfo_var.
	 These need recreating by the loader.  The type it is for is
	 stashed on the name's TREE_TYPE.  */
      tree type = TREE_TYPE (DECL_NAME (t));
      i (tt_tinfo_var);
      tree_node (type);
      int tag = insert (t);
      dump () && dump ("Wrote typeinfo:%d %S for %N", tag, t, type);
      goto done;
    }

  if (TREE_CODE (t) == IDENTIFIER_NODE)
    {
      /* An identifier node -> tt_id or, tt_conv_id.  */
      bool conv_op = IDENTIFIER_CONV_OP_P (t);

      i (conv_op ? tt_conv_id : tt_id);
      if (conv_op)
	tree_node (TREE_TYPE (t));
      else
	str (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));

      int tag = insert (t);
      dump () && dump ("Written:%d %sidentifier:%N",
		       tag, conv_op ? "conv_op_" : "",
		       conv_op ? TREE_TYPE (t) : t);
      goto done;
    }

  if (IS_FAKE_BASE_TYPE (t))
    {
      /* A fake base type -> tt_as_base.  */
      i (tt_as_base);
      dump () && dump ("Writing as_base for %N", TYPE_CONTEXT (t));
      tree_node (TYPE_NAME (TYPE_CONTEXT (t)));
    }

  if (TREE_CODE (t) == TREE_BINFO)
    {
      /* A BINFO -> tt_binfo.
	 We must do this by reference.  We stream the binfo tree
	 itself when streaming its owning RECORD_TYPE.  */
      i (tt_binfo);
      tree inh = BINFO_INHERITANCE_CHAIN (t);
      tree_node (inh);
      if (!inh)
	{
	  /* Dominating binfo.  Find TYPE, then grab it's binfo.  */
	  tree type = BINFO_TYPE (t);
	  gcc_checking_assert (TYPE_BINFO (type) == t);
	  tree_node (TYPE_NAME (t));
	  dump () && dump ("Wrote dominating BINFO %N", t);
	}
      else
	{
	  /* Copied binfo.  Record which child we are.  */
	  gcc_assert (TYPE_BINFO (BINFO_TYPE (t)) != t);
	  vec<tree, va_gc> *binfo_vec;

	  bool is_virt = BINFO_VIRTUAL_P (t);
	  if (is_virt)
	    {
	      /* A virtual base.  Look on the CLASSTYPE_VIRTUALS.  */
	      while (BINFO_INHERITANCE_CHAIN (inh))
		inh = BINFO_INHERITANCE_CHAIN (inh);

	      binfo_vec = CLASSTYPE_VBASECLASSES (BINFO_TYPE (inh));
	    }
	  else
	    /* Look along BINFO_BASE_BINFOS (inh).  */
	    binfo_vec = BINFO_BASE_BINFOS (inh);

	  gcc_assert (vec_safe_length (binfo_vec));
	  unsigned ix;
	  for (ix = 0; (*binfo_vec)[ix] != t; ix++)
	    gcc_assert (ix + 1 < binfo_vec->length ());
	  dump () && dump ("Wrote derived %sBINFO %u %N of %N",
			   is_virt ? "virtual " : "", ix, t, inh);
	  u (ix * 2 | is_virt);
	}

      /* If the dominating type was an import, we will not have put this
	 in the map.  Do that now.  */
      int tag = maybe_insert (t);
      u (tag != 0);
      if (tag)
	dump () && dump ("Inserting binfo:%d %N", tag, t);
      goto done;
    }

  if (TREE_CODE_CLASS (TREE_CODE (t)) == tcc_type && TYPE_NAME (t))
    {
      /* A named type.  */
      tree name = TYPE_NAME (t);

      gcc_assert (TREE_CODE (name) == TYPE_DECL);
      if (DECL_TINFO_P (name))
	{
	  /* A typeinfo pseudo type -> tt_tinfo_pseudo.  */
	  unsigned ix = get_pseudo_tinfo_index (t);

	  /* Make sure we're identifying this exact variant.  */
	  gcc_assert (get_pseudo_tinfo_type (ix) == t);
	  i (tt_tinfo_pseudo);
	  u (ix);
	  unsigned tag = insert (t);
	  dump () && dump ("Wrote:%d typeinfo pseudo %u %N", tag, ix, t);
	  goto done;
	}

      if (!tree_map.get (name))
	{
	  /* A new named type -> tt_type_name.

	     Write the type name as an interstitial, and then start
	     over.  We need to stream the DECL_NAME first.  */
	  dump () && dump ("Writing interstitial type name %C:%N%S",
			   TREE_CODE (name), name, name);
	  /* Make sure this is not a named builtin. We should find
	     those some other way to be canonically correct.  */
	  gcc_assert (TREE_TYPE (DECL_NAME (name)) != t
		      || DECL_SOURCE_LOCATION (name) != BUILTINS_LOCATION);
	  i (tt_type_name);
	  tree_node (name);
	  dump () && dump ("Wrote interstitial type name %C:%N%S",
			   TREE_CODE (name), name, name);
	  /* The type itself could be a variant of TREE_TYPE (name),
	     so stream it out in its own right.  We'll find the name
	     in the map, so not end up here.  */
	  goto again;
	}
    }

  if (TREE_CODE_CLASS (TREE_CODE (t)) == tcc_declaration)
    {
      /* A DECL.  */
      tree owner_decl = get_module_owner (t);
      unsigned owner = MAYBE_DECL_MODULE_OWNER (owner_decl);
      if (owner >= MODULE_IMPORT_BASE)
	{
	  /* An imported decl -> tt_import.  */
	  i (tt_import);
	  u (TREE_CODE (t));
	  tree_node (CP_DECL_CONTEXT (t));
	  u (owner);
	  tree_node (DECL_NAME (t));
	  tree_node (DECL_DECLARES_FUNCTION_P (t) ? TREE_TYPE (t) : NULL_TREE);
	  int tag = insert (t);
	  dump () && dump ("Wrote import:%d %N@%I", tag, t, module_name (owner));
	  if (tree type = TREE_TYPE (t))
	    {
	      /* Make sure the imported type is in the map too.  */
	      tag = maybe_insert (type);
	      u (tag != 0);
	      if (tag)
		dump () && dump ("Wrote imported type:%d %C:%N%S", tag,
				 TREE_CODE (type), type, type);
	    }
	  goto done;
	}
    }

  /* else */
  {
    /* A new node -> tt_node.  */
    tree_code code = TREE_CODE (t);

    unique++;
    i (tt_node);
    u (code);

    start (code, t);

    int tag = insert (t);
    dump () && dump ("Writing:%d %C:%N%S%s", tag, TREE_CODE (t), t, t,
		     TREE_CODE_CLASS (code) == tcc_declaration
		     && DECL_MODULE_EXPORT_P (t) ? " (exported)" : "");
    tree_node_raw (t);
    dump () && dump ("Written:%d %N", tag, t);

    if (RECORD_OR_UNION_CODE_P (TREE_CODE (t)) && TYPE_MAIN_VARIANT (t) == t)
      {
	/* Write out the binfo heirarchy.  */
	tree_binfo (t);
	if (TYPE_LANG_SPECIFIC (t))
	  {
	    tree_node (CLASSTYPE_PRIMARY_BINFO (t));
	    tree as_base = CLASSTYPE_AS_BASE (t);
	    if (as_base && as_base != t)
	      {
		/* A fake base class.  We must break the IS_FAKE_BASE loop,
		   while streaming it out. */
		CLASSTYPE_AS_BASE (t) = NULL_TREE;
		tag_definition (as_base, NULL_TREE);
		CLASSTYPE_AS_BASE (t) = as_base;
	      }
	    else
	      tree_node (as_base);
	  }
      }
  }

 done:
  /* And, breath out.  */
  dump.outdent ();
}

/* Stream in a tree node.  */

tree
trees_in::tree_node ()
{
  if (get_overrun ())
    return NULL_TREE;

  dump.indent ();
 again:
  int tag = i ();
  tree res = NULL_TREE;
  switch (tag)
    {
    case tt_null:
      /* NULL_TREE.  */
      break;

    default:
      /* backref, pull it out of the map.  */
      if (tag <= tt_backref && unsigned (tt_backref - tag) < back_refs.length ())
	res = back_refs[tt_backref - tag];
      else
	{
	  error ("unknown tree reference %qd", tag);
	  set_overrun ();
	}
      if (res)
	dump () && dump ("Read backref:%d found %C:%N%S", tag,
			 TREE_CODE (res), res, res);
      break;

    case tt_fixed:
      {
	/* A fixed ref, find it in the fixed_ref array.   */
	unsigned fix = u ();
	if (fix < (*fixed_refs).length ())
	  {
	    res = (*fixed_refs)[fix];
	    dump () && dump ("Read fixed:%u %C:%N%S", fix,
			     TREE_CODE (res), res, res);
	  }
	else
	  set_overrun ();
	break;
      }

    case tt_namespace:
      {
	/* A namespace, find it in the symbol table.  */
	tree ctx = tree_node ();
	tree name = tree_node ();

	if (get_overrun ())
	  break;
	gcc_assert (TREE_CODE (ctx) == NAMESPACE_DECL);
	res = find_imported_namespace (ctx, state->mod, name);
	if (!res)
	  set_overrun ();
	int tag = insert (res);
	dump () && dump ("Created:%d namespace %N", tag, res);
      }
      break;

    case tt_type_name:
      /* An interstitial type name.  Read the name and then start
	 over.  */
      res = tree_node ();
      dump () && dump ("Read interstitial type name %C:%N%S",
		       res ? TREE_CODE (res) : ERROR_MARK, res, res);
      if (res && TREE_CODE (res) == TYPE_DECL)
	goto again;
      set_overrun ();
      break;

    case tt_tinfo_var:
    case tt_conv_id:
      /* A typeinfo var or conversion operator.  Get the type and
	 recreate the var decl or identifier.  */
      {
	bool is_tinfo = tag == tt_tinfo_var;
	tree type = tree_node ();
	if (type && TYPE_P (type))
	  {
	    res = is_tinfo ? get_tinfo_decl (type) : make_conv_op_name (type);
	    int tag = insert (res);
	    dump () && dump ("Created %s:%d %S for %N",
			     is_tinfo ? "tinfo_var" : "conv_op", tag, res, type);
	  }
	else
	  set_overrun ();
      }
      break;

    case tt_tinfo_pseudo:
      {
	/* A pseuto typeinfo.  Get the index and recreate the pseudo.  */
	unsigned ix = u ();

	res = get_pseudo_tinfo_type (ix);
	int tag = insert (res);
	dump () && dump ("Created tinfo_pseudo:%d %u %N", tag, ix, res);
      }
      break;

    case tt_id:
      {
	/* An identifier node.  */
	size_t l;
	const char *chars = str (&l);
	res = get_identifier_with_length (chars, l);
	int tag = insert (res);
	dump () && dump ("Read identifier:%d%N", tag, res);
      }
      break;

    case tt_import:
      {
	/* An imported decl.  */
	unsigned code = u ();
	tree ctx = tree_node ();
	unsigned owner = u ();
	tree name = tree_node ();
	unsigned remapped = (owner < state->remap->length ()
			     ? (*state->remap)[owner] : MODULE_NONE);
	tree type = tree_node ();
	if (remapped != MODULE_NONE && remapped != state->mod
	    && !get_overrun ())
	  res = lookup_by_ident (ctx, remapped, name, type, code);
	if (!res)
	  {
	    error ("failed to find %<%E%s%E@%E%>",
		   ctx, &"::"[2 * (ctx == global_namespace)],
		   name, module_name (remapped));
	    set_overrun ();
	  }
	int tag = insert (res);
	dump () && dump ("Imported:%d %P@%I", tag,
			 ctx, name, module_name (remapped));
	if (res && TREE_TYPE (res) && u ())
	  {
	    /* Insert the type too.  */
	    tree type = TREE_TYPE (res);
	    tag = insert (type);
	    dump () && dump ("Read imported type:%d %C:%N%S", tag,
			     TREE_CODE (type), type, type);
	  }
      }
      break;

    case tt_binfo:
      {
	/* A BINFO.  Walk the tree of the dominating type.  */
	tree inh = tree_node ();
	if (!inh)
	  {
	    /* Dominating binfo.  Read the type and get its binfo.  */
	    tree decl = tree_node ();
	    if (get_overrun ())
	      break;
	    res = TYPE_BINFO (TREE_TYPE (decl));
	    dump () && dump ("Read dominating binfo %N", res);
	  }
	else
	  {
	    /* Copied binfo.  Find it in our parent.  */
	    unsigned key = u ();
	    bool is_virt = key & 1;
	    unsigned ix = key >> 1;
	    vec<tree, va_gc> *binfo_vec;

	    if (is_virt)
	      {
		/* A virtual base.  Look on the CLASSTYPE_VIRTUALS.  */
		while (BINFO_INHERITANCE_CHAIN (inh))
		  inh = BINFO_INHERITANCE_CHAIN (inh);

		binfo_vec = CLASSTYPE_VBASECLASSES (BINFO_TYPE (inh));
	      }
	    else
	      /* Look along BINFO_BASE_BINFOS (inh).  */
	      binfo_vec = BINFO_BASE_BINFOS (inh);

	    if (vec_safe_length (binfo_vec) < ix)
	      set_overrun ();
	    else
	      res = (*binfo_vec)[ix];
	    dump () && dump ("Read derived %sBINFO %u %N of %N",
			     is_virt ? "virtual " : "", ix, res, inh);
	  }

	if (get_overrun ())
	  break;

	/* Maybe insert binfo into backreferences.  */
	if (!u ())
	  {
	    tag = insert (res);
	    dump () && dump ("Read binfo:%d %N", tag, res);
	  }
      }
      break;

    case tt_as_base:
      {
	/* A fake as base type. */
	res = tree_node ();
	dump () && dump ("Read as-base for %N", res);
	if (res)
	  res = CLASSTYPE_AS_BASE  (TREE_TYPE (res));
      }
      break;

    case tt_node:
      {
	/* A new node.  Stream it in.  */
	unsigned c = u ();
	if (c >= MAX_TREE_CODES)
	  {
	    error ("unknown tree code %qd" , c);
	    set_overrun ();
	  }
	tree_code code = tree_code (c);
	res = start (code);
	if (!res)
	  {
	    set_overrun ();
	    break;
	  }

	/* Insert into map.  */
	tag = insert (res);
	dump () && dump ("Reading:%d %C", tag, code);

	if (!tree_node_raw (res))
	  goto barf;

	if (get_overrun ())
	  {
	  barf:
	    back_refs[tt_backref - tag] = NULL_TREE;
	    set_overrun ();
	    res = NULL_TREE;
	    break;
	  }

	dump () && dump ("Read:%d %C:%N", tag, code, res);
	tree found = finish (res);

	if (found != res)
	  {
	    /* Update the mapping.  */
	    res = found;
	    back_refs[tt_backref - tag] = res;
	    dump () && dump ("Remapping:%d to %C:%N%S", tag,
			     res ? TREE_CODE (res) : ERROR_MARK, res, res);
	  }
	break;
      }
      
    case tt_definition:
      /* An immediate definition.  */
      res = tag_definition ();
      if (res)
	dump () && dump ("Read immediate definition %C:%N%S",
			 TREE_CODE (res), res, res);
      else
	gcc_assert (get_overrun ());
      break;
    }

  dump.outdent ();
  return res;
}

/* Rebuild a streamed in type.  */
// FIXME: c++-specific types are not in the canonical type hash.
// Perhaps that should be changed?

tree
trees_in::finish_type (tree type)
{
  tree main = TYPE_MAIN_VARIANT (type);

  if (main != type)
    {
      /* See if we have this type already on the variant
	 list.  This could only happen if the originally read in main
	 variant was remapped, but we don't have that knowledge.
	 FIXME: Determine if this is a problem, and then maybe fix
	 it?  That would avoid a fruitless search along the variant
	 chain.  */
      for (tree probe = main; probe; probe = TYPE_NEXT_VARIANT (probe))
	{
	  if (!check_base_type (type, probe))
	    continue;

	  if (TYPE_QUALS (type) != TYPE_QUALS (probe))
	    continue;

	  if (FUNC_OR_METHOD_TYPE_P (type))
	    {
	      if (!comp_except_specs (TYPE_RAISES_EXCEPTIONS (type),
				      TYPE_RAISES_EXCEPTIONS (probe),
				      ce_exact))
		continue;

	      if (type_memfn_rqual (type) != type_memfn_rqual (probe))
		continue;
	    }
	  
	  dump () && dump ("Type %p already found as %p variant of %p",
			   (void *)type, (void *)probe, (void *)main);
	  free_node (type);
	  type = probe;
	  goto found_variant;
	}

      /* Splice it into the variant list.  */
      dump () && dump ("Type %p added as variant of %p",
		       (void *)type, (void *)main);
      TYPE_NEXT_VARIANT (type) = TYPE_NEXT_VARIANT (main);
      TYPE_NEXT_VARIANT (main) = type;

      /* CANONICAL_TYPE is either already correctly remapped.  Or
         correctly already us.  */
      // FIXME:Are we sure about this?
    found_variant:;
    }
  else if (TREE_CODE (type) == TEMPLATE_TYPE_PARM
	   || TREE_CODE (type) == TEMPLATE_TEMPLATE_PARM)
    {
      tree canon = canonical_type_parameter (type);
      if (TYPE_CANONICAL (type) == type)
	type = canon;
      else
	TYPE_CANONICAL (type) = canon;
      dump () && dump ("Adding template type %p with canonical %p",
		       (void *)type, (void *)canon);
    }
  else if (!TYPE_STRUCTURAL_EQUALITY_P (type)
	   && !TYPE_NAME (type))
    {
      gcc_assert (TYPE_ALIGN (type));
      hashval_t hash = type_hash_canon_hash (type);
      /* type_hash_canon frees type, if we find it already.  */
      type = type_hash_canon (hash, type);
      // FIXME: This is where it'd be nice to determine if type
      // was already found.  See above.
      dump () && dump ("Adding type %p with canonical %p",
		       (void *)main, (void *)type);
    }

  if (!RECORD_OR_UNION_CODE_P (TREE_CODE (type)))
    ;
  else if (main == type)
    {
      /* Read in the binfo heirarchy.  */
      if (!tree_binfo (type))
	set_overrun ();
      if (TYPE_LANG_SPECIFIC (type))
	{
	  CLASSTYPE_PRIMARY_BINFO (type) = tree_node ();
	  CLASSTYPE_AS_BASE (type) = tree_node ();
	}
    }
  else
    {
      /* The main variant might already have been defined, copy
	 the bits of its definition that we need.  */
      TYPE_BINFO (type) = TYPE_BINFO (main);
      TYPE_VFIELD (type) = TYPE_VFIELD (main);
      TYPE_FIELDS (type) = TYPE_FIELDS (main);
    }

  return type;
}

void
trees_out::write (auto_vec<tree> &decls)
{
  for (unsigned ix = 0; ix != decls.length ();)
    {
      tree ns = decls[ix++];
      tree name = decls[ix++];
      dump () && dump ("Writing %N bindings for %N", ns, name);

      tag (tt_binding);
      tree_node (ns);
      tree_node (name);
      tree decl;
      for (unsigned jx = ix; (decl = decls[jx++]) != NULL_TREE;)
	{
	  gcc_assert (!DECL_IS_BUILTIN (decl));
	  if (TREE_CODE (decl) == CONST_DECL)
	    {
	      gcc_assert (TREE_CODE (CP_DECL_CONTEXT (decl)) == ENUMERAL_TYPE);
	      continue;
	    }
	  tree_node (decl);
	}
      tree_node (NULL_TREE);

      // FIXME:Write during binding?
      while ((decl = decls[ix++]) != NULL_TREE)
	if (!DECL_IS_BUILTIN (decl) && TREE_CODE (decl) != CONST_DECL)
	  maybe_tag_definition (decl);
    }
}

void
trees_in::read ()
{
  bool ok = true;
  while (ok && more_p ())
    {
      int tag = i ();

      switch (tag)
	{
	case tt_binding:
	  ok = tag_binding ();
	  break;
	case tt_definition:
	  ok = tag_definition () != NULL_TREE;
	  break;

	default:
	  error (tag >= 0 ? "unexpected key %qd"
		 : "unexpected tree reference %qd", tag);
	  ok = false;
	  break;
	}
    }

  if (!ok)
    set_overrun ();
}

static GTY(()) tree proclaimer;
static int export_depth; /* -1 for singleton export.  */

/* Nest a module export level.  Return true if we were already in a
   level.  */

int
push_module_export (bool singleton, tree proclaiming)
{
  int previous = export_depth;

  if (proclaiming)
    {
      proclaimer = proclaimer;
      export_depth = -2;
    }
  else if (singleton)
    export_depth = -1;
  else
    export_depth = +1;
  return previous;
}

/* Outdent a module export level.  */

void
pop_module_export (int previous)
{
  proclaimer = NULL;
  export_depth = previous;
}

int
module_exporting_level ()
{
  return export_depth;
}

/* Return the decl that determines the owning module of DECL.  That
   may be DECL itself, or it may DECL's context, or it may be some
   other DECL (for instance an unscoped enum's CONST_DECLs are owned
   by the TYPE_DECL).  */

tree
get_module_owner (tree decl)
{
 again:
  gcc_assert (TREE_CODE_CLASS (TREE_CODE (decl)) == tcc_declaration);

  switch (TREE_CODE (decl))
    {
    case TEMPLATE_DECL:
      /* Although a template-decl has ownership, that's mainly for
         namespace-scope name pushing.  Whether it has one depends on
         the thing it's templating, so look at that directly.  */
      decl = DECL_TEMPLATE_RESULT (decl);
      goto again;

    case NAMESPACE_DECL:
    case FUNCTION_DECL:
      /* Things that are containers hold their own module owner
	 info.  */
      return decl;

    case TYPE_DECL:
      /* The implicit typedef of a structured type has its own module
	 owner.  */
      if (DECL_IMPLICIT_TYPEDEF_P (decl))
	return decl;
      /* Fallthrough.  */

    case VAR_DECL:
      /* Things at namespace scope, have their own module owner ...  */
      if (TREE_CODE (CP_DECL_CONTEXT (decl)) == NAMESPACE_DECL)
	return decl;
      break;

    case CONST_DECL:
      /* ... except enumeration constants.  */
      if (TREE_CODE (TREE_TYPE (decl)) == ENUMERAL_TYPE
	  && DECL_CONTEXT (decl) == DECL_CONTEXT (TYPE_NAME (TREE_TYPE (decl))))
	/* An enumeration is controlled by its enum-decl.  Its
	   enumerations may not have that as DECL_CONTEXT.  */
	return TYPE_NAME (TREE_TYPE (decl));
      break;

    default:
      break;
    }

  /* Otherwise, find this decl's context, which should itself have
     the data.  */
  tree ctx = CP_DECL_CONTEXT (decl);
  gcc_assert (ctx && TREE_CODE (decl) != NAMESPACE_DECL);
  if (TYPE_P (ctx))
    {
    again_again:
      if (tree tn = TYPE_NAME (ctx))
	ctx = tn;
      else if (tree tc = TYPE_CONTEXT (ctx))
	{
	  ctx = tc;
	  goto again_again;
	}
      else
	/* Always return something, global_namespace is a useful
	   non-owning decl.  */
	ctx = global_namespace;
    }
  return ctx;
}

/* Set the module EXPORT and OWNER fields on DECL.  */

void
set_module_owner (tree decl)
{
  /* We should only be setting moduleness on things that are their own
     owners.  */
  gcc_checking_assert (decl == get_module_owner (decl));
  
  if (!module_state::modules)
    /* We can be called when modules are not enabled.  */
    return;

  // FIXME: check ill-formed linkage

  if ((*module_state::modules)[MODULE_PURVIEW])
    {
      if (export_depth)
	{
	  gcc_assert (TREE_CODE (decl) != NAMESPACE_DECL);
	  DECL_MODULE_EXPORT_P (decl) = true;
	}
      retrofit_lang_decl (decl);
      DECL_MODULE_OWNER (decl) = MODULE_PURVIEW;
    }
}

/* DECL has been implicitly declared, set its module owner from
   FROM.  */

void
set_implicit_module_owner (tree decl, tree from)
{
  gcc_checking_assert (decl == get_module_owner (decl));

  if (!module_state::modules)
    return;

  if (unsigned owner = MAYBE_DECL_MODULE_OWNER (from))
    {
      DECL_MODULE_EXPORT_P (decl) = DECL_MODULE_EXPORT_P (from);
      retrofit_lang_decl (decl);
      DECL_MODULE_OWNER (decl) = owner;
    }
}

/* Return true iff we're in the purview of a named module.  */

bool
module_purview_p ()
{
  return (*module_state::modules)[MODULE_PURVIEW];
}

/* Return true iff we're the interface TU (this also means we're in a
   module purview.  */

bool
module_interface_p ()
{
  return ((*module_state::modules)[MODULE_PURVIEW]
	  && (*module_state::modules)[MODULE_PURVIEW]->exported);
}

static FILE *
search_module_path (char *&name, size_t &name_len,
		    const char *rel, size_t rel_len)
{
  char *buffer = XNEWVEC (char, (rel_len > module_path_max
				 ? rel_len : module_path_max) + name_len + 2);
  char *ptr = buffer;

  if (!IS_ABSOLUTE_PATH (name) && rel_len)
    {
      memcpy (ptr, rel, rel_len);
      ptr += rel_len;
    }
  memcpy (ptr, name, name_len + 1);
  if (FILE *stream = fopen (buffer, "r"))
    {
      name = buffer;
      name_len += (ptr - buffer);
      return stream;
    }

  if (!IS_ABSOLUTE_PATH (name))
    for (const cpp_dir *dir = module_path; dir; dir = dir->next)
      {
	ptr = buffer;
	/* Don't prepend '.'.  */
	if (dir->len != 1 || dir->name[0] != '.')
	  {
	    memcpy (ptr, dir->name, dir->len);
	    ptr += dir->len;
	    *ptr++ = DIR_SEPARATOR;
	  }
	memcpy (ptr, name, name_len + 1);
	if (FILE *stream = fopen (buffer, "r"))
	  {
	    name = buffer;
	    name_len += (ptr - buffer);
	    return stream;
	  }
      }

  XDELETEVEC (buffer);
  return NULL;
}

static FILE *
find_module_file (module_state *state)
{
  FILE *stream = fopen (state->filename, "rb");
  if (stream || !module_wrapper)
    return stream;

  inform (state->loc, "installing module %qE (%qs)",
	  state->name, state->filename);

  /* wrapper <module-name> <module-bmi-file> <srcname> <main_input_filename> */
  unsigned len = 0;
  const char *argv[6];
  argv[len++] = module_wrapper;
  argv[len++] = IDENTIFIER_POINTER (state->name);
  argv[len++] = state->filename;
  argv[len++] = state->srcname ? state->srcname : "";
  argv[len++] = main_input_filename;

  if (!quiet_flag)
    {
      if (pp_needs_newline (global_dc->printer))
	{
	  pp_needs_newline (global_dc->printer) = false;
	  fprintf (stderr, "\n");
	}
      fprintf (stderr, "%s module wrapper:", progname);
      for (unsigned ix = 0; ix != len; ix++)
	fprintf (stderr, "%s'%s'", &" "[!ix], argv[ix]);
      fprintf (stderr, "\n");
      fflush (stderr);
    }
  argv[len] = NULL;

  int err;
  const char *errmsg = NULL;
  int status;
  pex_obj *pex = pex_init (0, progname, NULL);
  if (pex)
    errmsg = pex_run (pex, PEX_LAST | PEX_SEARCH, argv[0],
		      const_cast <char **> (argv), NULL, NULL, &err);

  if (!pex || (!errmsg && !pex_get_status (pex, 1, &status)))
    errmsg = "cannot invoke";
  pex_free (pex);

  diagnostic_set_last_function (global_dc, (diagnostic_info *) NULL);

  if (errmsg)
    {
      errno = err;
      error_at (state->loc, "%s %qs %m", errmsg, argv[0]);
    }
  else if (WIFSIGNALED (status))
    error_at (state->loc, "module wrapper %qs died by signal %s",
	      argv[0], strsignal (WTERMSIG (status)));
  else if (WIFEXITED (status) && WEXITSTATUS (status) != 0)
    error_at (state->loc, "module wrapper %qs exit status %d",
	      argv[0], WEXITSTATUS (status));
  else
    inform (state->loc, "installed module %qE", state->name);

  return fopen (state->filename, "rb");
}

static bool
add_module_mapping (const char *arg, const char *rel, size_t rel_len,
		    unsigned depth);

/* File of mappings, another DSL.  Each line is ...
   <ws>#*<ws>*G++Module <word> -- define marker <word>
   <ws>#*<ws>*<word><ws><map> -- a map
   <ws>#<anything>  -- comment ignored
   <ws>*        -- ignored
   <map>        -- if <word> not defined, a map else ignored
*/

// FIXME really need a cmdline loc
#define MAP_LOC BUILTINS_LOCATION

static bool
parse_module_mapping (char *line, const char *rel, size_t rel_len,
		      unsigned depth, char *&marker)
{
  bool comment = false;

  /* Skip leading whitespace and comment markers.  */
  while (*line == '#' ? (comment = true) : ISSPACE (*line))
    line++;

  if (comment || marker)
    {
      /* Look for marker or definition.  */
      char *pos = line;
      while (*pos && !ISSPACE (*pos))
	pos++;
      const char *tag = marker ? marker : "G++Module";
      if (strncmp (line, tag, pos - line)
	  || size_t (pos - line) != strlen (tag))
	/* Not interested.  */
	return true;

      /* Seen the tag, either set marker or accept line  */
      line = pos;
      /* Skip whitespace.  */
      while (ISSPACE (*line))
	line++;
      if (!marker)
	{
	  /* Define a marker string.  */
	  pos = line;
	  /* Scan word.  */
	  while (*pos && !ISSPACE (*pos))
	    pos++;
	  *pos = 0;
	  marker = xstrdup (line);
	  /* We're now done with the line.  */
	  return true;
	}
    }

  if (!*line)
    return true;

  /* Process line as a mapping.  */
  return add_module_mapping (line, rel, rel_len, depth);
}

/* Add a module name to binary interface file mapping
   <name>=<file> -> mapping
   <file> -> file of mappings (recursive)

   We don't detect loops. */

static bool
add_module_mapping (const char *arg, const char *rel = NULL,
		    size_t rel_len = 0, unsigned depth = 0)
{
  if (++depth >= 15)
    {
      /* Possible loop.  I hope 15 levels is deep enough.  */
      error_at (MAP_LOC, "module files too deeply nested");
      return false;
    }
  if (const char *eq = strchr (arg, '='))
    {
      /* A single mapping.  */
      if (eq == arg || !eq[1])
	{
	  error_at (MAP_LOC, "module file map %qs is malformed", arg);
	  return false;
	}

      tree name = get_identifier_with_length (arg, eq - arg);
      module_state *state = module_state::get_module (name);

      /* Do not override already set mappings.  */
      if (!state->filename)
	{
	  const char *pos = eq + 1;
	  while (*pos && !ISSPACE (*pos) && *pos != ';')
	    pos++;
	  state->filename = make_module_filename (eq + 1, pos - eq - 1, false);
	  while (ISSPACE (*pos))
	    pos++;
	  if (*pos == ';')
	    {
	      do
		pos++;
	      while (*pos && ISSPACE (*pos));
	      size_t len = 0;
	      while (pos[len] && !ISSPACE (pos[len]))
		len++;
	      state->srcname = XNEWVEC (char, len + 1);
	      memcpy (state->srcname, pos, len);
	      state->srcname[len] = 0;
	      pos += len;
	      while (ISSPACE (*pos))
		pos++;
	    }
	  if (*pos)
	    {
	      error_at (MAP_LOC,
			"module file map %qs has unexpected trailing %qs",
			arg, pos);
	      return false;
	    }
	}

      return true;
    }

  bool ok = false;
  char *name = const_cast <char *> (arg);
  size_t name_len = strlen (name);
  if (FILE *stream = search_module_path (name, name_len, rel, rel_len))
    {
      /* Find the directory component of PATH.  This will fail if we
         give an absolute names in the root directory.  Why would you
         do that?  */
      while (name_len && !IS_DIR_SEPARATOR (name[name_len - 1]))
	name_len--;

      /* Exercise buffer expansion.  */
      size_t size = MODULE_STAMP ? 3 : PATH_MAX + NAME_MAX;
      char *buffer = XNEWVEC (char, size);
      size_t pos = 0;
      unsigned line = 0;
      char *marker = NULL;
      while (fgets (buffer + pos, size - pos, stream))
	{
	  pos += strlen (buffer + pos);
	  if (buffer[pos - 1] != '\n')
	    {
	      size *= 2;
	      buffer = XRESIZEVEC (char, buffer, size);
	    }
	  else
	    {
	      buffer[pos - 1] = 0;
	      pos = 0;
	      line++;
	      if (!parse_module_mapping (buffer, name, name_len,
					 depth, marker))
		{
		  inform (MAP_LOC, "from module map file %s:%d", name, line);
		  goto fail;
		}
	    }
	}
      if (ferror (stream))
	error_at (MAP_LOC, "failed to read module map file %qs: %m", name);
      else
	ok = true;

    fail:
      free (marker);
      fclose (stream);
      XDELETEVEC (buffer);
      XDELETEVEC (name);
    }
  else
    error_at (MAP_LOC, "module-file %qs not found: %m", name);

  return ok;
}

/* Import the module NAME into the current TU.  If MODULE_P is
   true, we're part of module NAME, and EXPORT_P indicates if
   we're the exporting init (true), or not (false).  If MODULE_P
   is false, NAME is a regular import.  EXPORT_P tells us if
   we're a direct import of the current TU (true), or an indirect
   import (false).  Returns the imported module object (or NULL).

   We don't actually import anything when MODULE_P and IMPORT_EXPORT_P
   are both true.  */

module_state *
module_state::do_import (location_t loc, tree name, bool module_p,
			 bool export_p, unsigned *crc_ptr)
{
  gcc_assert (global_namespace == current_scope ());

  tree sname = name;
  if (TREE_CODE (name) == TREE_VEC)
    {
      /* Create the flat name */
      auto_vec<char> buffer;
      for (int ix = 0; ix < TREE_VEC_LENGTH (name); ix++)
	{
	  tree elt = TREE_VEC_ELT (name, ix);
	  size_t l = IDENTIFIER_LENGTH (elt);
	  buffer.reserve (l + 2);
	  if (ix)
	    buffer.quick_push ('.');
	  size_t len = buffer.length ();
	  buffer.quick_grow (len + l);
	  memcpy (&buffer[len], IDENTIFIER_POINTER (elt), l);
	}
      name = get_identifier_with_length (&buffer[0], buffer.length ());
    }

  module_state *dflt = module_p ? (*modules)[MODULE_NONE] : NULL;
  module_state *state = get_module (name, dflt);

  if (!state)
    {
      /* Already declared the module.  */
      error_at (loc, "cannot declare module in purview of module %qE",
		dflt->name);
      return NULL;
    }

  if (!state->occupied ())
    {
      if (module_p && export_p)
	{
	  /* We're the exporting module unit, so not loading anything.  */
	  state->exported = true;
	  state->mod = MODULE_PURVIEW;
	  if (module_output)
	    {
	      free (state->filename);
	      state->filename = xstrdup (module_output);
	      module_output = NULL;
	    }
	  state->srcname = xstrdup (main_input_filename);
	}
      else if (!module_p && !export_p)
	{
	  /* The ordering of the import table implies that indirect
	     imports should have already been loaded.  */
	  error ("indirect import %qE not present", name);
	  return NULL;
	}
      state->occupy (loc, sname);

      if (!module_p || !export_p)
	{
	  FILE *stream = find_module_file (state);
	  int e = errno;
	  state->push_location ();
	  unsigned n = dump.push (state);
	  state->announce ("importing");

	  elf_in *from = new elf_in (stream, e);
	  if (from->begin ())
	    state->read (from, crc_ptr);
	  gcc_assert (!state->lazy);
	  const char *err = from->end ();
	  if (err)
	    {
	      /* Failure to read a module is going to cause big
		 problems, so bail out, if this is the top level.
		 Otherwise return NULL to let our importer know (and
		 fail).  */
	      (module_p || export_p ? fatal_error
	       : (void (*)(location_t, const char *, ...))error_at)
		(state->loc, "failed to import module %qE: %s",
		 state->name, err);
	    }
	  else
	    gcc_assert (state->mod <= modules->length ());
	  delete from;
	  state->announce ("imported");
	  dump.pop (n);
	  state->pop_location ();
	  if (err)
	    return NULL;
	}
    }
  else if (state->mod == MODULE_PURVIEW)
    {
      /* Cannot import the current module.  */
      error_at (loc, "cannot import module in its own purview");
      inform (state->loc, "module %qE declared here", state->name);
      return NULL;
    }
  else
    {
      /* A circular dependency cannot exist solely in the imported
         unit graph, it must go via the current TU, and we discover
         that differently.  */
      gcc_assert (state->mod != MODULE_UNKNOWN);
      if (module_p)
	{
	  /* Cannot be module unit of an imported module.  */
	  error_at (loc, "cannot declare module after import");
	  inform (state->loc, "module %qE imported here", state->name);
	  return NULL;
	}
    }

  gcc_assert (state);
  if (module_p)
    {
      (*modules)[MODULE_PURVIEW] = state;
      current_module = MODULE_PURVIEW;
    }

  return state;
}

/* Import the module NAME into the current TU and maybe re-export it.  */

void
import_module (const cp_expr &name, bool exporting, tree)
{
  if (export_depth)
    exporting = true;

  gcc_assert (global_namespace == current_scope ());
  if (module_state *imp
      = module_state::do_import (name.get_location (), *name,
				 /*unit_p=*/false, /*import_p=*/true))
    {
      imp->imported = true;
      if (exporting)
	imp->exported = true;
      (*module_state::modules)[MODULE_NONE]->set_import (imp, exporting);
    }
  gcc_assert (global_namespace == current_scope ());
}

/* Declare the name of the current module to be NAME.  EXPORTING_p is
   true if this TU is the exporting module unit.  */

void
declare_module (const cp_expr &name, bool exporting_p, tree)
{
  gcc_assert (global_namespace == current_scope ());

  module_state::do_import (name.get_location (), *name, true, exporting_p);
}

/* Convert the module search path.  */

void
init_module_processing ()
{
  module_state::init ();

  module_path = get_added_cpp_dirs (INC_CXX_MPATH);
  for (const cpp_dir *path = module_path; path; path = path->next)
    if (path->len > module_path_max)
      module_path_max = path->len;

  if (!module_wrapper)
    module_wrapper = getenv ("CXX_MODULE_WRAPPER");
  if (module_wrapper && !module_wrapper[0])
    module_wrapper = NULL;

  for (unsigned ix = 0; ix != module_file_args.length (); ix++)
    if (!add_module_mapping (module_file_args[ix]))
      break;
  module_file_args.release ();

  if (module_map_dump)
    module_state::print_map ();
}

/* Finalize the module at end of parsing.  */

void
finish_module ()
{
  module_state::fini ();

  module_state *state = (*module_state::modules)[MODULE_PURVIEW];
  if (!state || !state->exported)
    {
      if (module_output)
	error ("%<-fmodule-output%> specified for"
	       " non-module interface compilation");
      return;
    }

  if (!errorcount)
    {
      FILE *stream = fopen (state->filename, "wb");
      int e = errno;
      location_t saved_loc = input_location;
      input_location = state->loc;
      unsigned n = dump.push (state);
      state->announce ("creating");

      elf_out to (stream, e);
      if (to.begin ())
	state->write (&to);
      if (const char *err = to.end ())
	error_at (state->loc, "failed to export module %qE: %s", state->name,
		  err);

      dump.pop (n);
      input_location = saved_loc;
    }

  if (errorcount)
    unlink (state->filename);

  state->release ();
}

/* If CODE is a module option, handle it & return true.  Otherwise
   return false.  */

bool
handle_module_option (unsigned code, const char *arg, int)
{
  switch (opt_code (code))
    {
    case OPT_fmodules_atom:
      flag_modules = 2;
      return true;

    case OPT_fmodule_path_:
      add_path (xstrdup (arg), INC_CXX_MPATH, true, true);
      return true;

    case OPT_fmodule_output_:
      module_output = arg;
      return true;

    case OPT_fmodule_prefix_:
      module_prefix = arg;
      return true;

    case OPT_fmodule_wrapper_:
      module_wrapper = arg;
      return true;

    case OPT_fmodule_file_:
      module_file_args.safe_push (arg);
      return true;

    case OPT_fmodule_map_dump:
      module_map_dump = true;
      return true;

    default:
      return false;
    }
}

#include "gt-cp-module.h"

/* Use of vec<unsigned, va_gc_atomic> caused these fns to be needed.  */
void gt_pch_nx (unsigned int &) {}
void gt_pch_nx (unsigned int *, void (*)(void *, void *), void *) {}
