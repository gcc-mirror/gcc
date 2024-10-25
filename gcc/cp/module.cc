/* C++ modules.  Experimental!
   Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

   A hash table contains all module names.  Imported modules are
   present in a modules array, which by construction places an
   import's dependencies before the import itself.  The single
   exception is the current TU, which always occupies slot zero (even
   when it is not a module).

   Imported decls occupy an entity_ary, an array of binding_slots, indexed
   by importing module and index within that module.  A flat index is
   used, as each module reserves a contiguous range of indices.
   Initially each slot indicates the CMI section containing the
   streamed decl.  When the decl is imported it will point to the decl
   itself.

   Additionally each imported decl is mapped in the entity_map via its
   DECL_UID to the flat index in the entity_ary.  Thus we can locate
   the index for any imported decl by using this map and then
   de-flattening the index via a binary seach of the module vector.
   Cross-module references are by (remapped) module number and
   module-local index.

   Each importable DECL contains several flags.  The simple set are
   DECL_MODULE_EXPORT_P, DECL_MODULE_PURVIEW_P, DECL_MODULE_ATTACH_P
   and DECL_MODULE_IMPORT_P.  The first indicates whether it is
   exported, the second whether it is in module or header-unit
   purview.  The third indicates it is attached to the named module in
   whose purview it resides and the fourth indicates whether it was an
   import into this TU or not.  DECL_MODULE_ATTACH_P will be false for
   all decls in a header-unit, and for those in a named module inside
   a linkage declaration.

   The more detailed flags are DECL_MODULE_PARTITION_P,
   DECL_MODULE_ENTITY_P.  The first is set in a primary interface unit
   on decls that were read from module partitions (these will have
   DECL_MODULE_IMPORT_P set too).  Such decls will be streamed out to
   the primary's CMI.  DECL_MODULE_ENTITY_P is set when an entity is
   imported, even if it matched a non-imported entity.  Such a decl
   will not have DECL_MODULE_IMPORT_P set, even though it has an entry
   in the entity map and array.

   Header units are module-like.

   For namespace-scope lookup, the decls for a particular module are
   held located in a sparse array hanging off the binding of the name.
   This is partitioned into two: a few fixed slots at the start
   followed by the sparse slots afterwards.  By construction we only
   need to append new slots to the end -- there is never a need to
   insert in the middle.  The fixed slots are MODULE_SLOT_CURRENT for
   the current TU (regardless of whether it is a module or not),
   MODULE_SLOT_GLOBAL and MODULE_SLOT_PARTITION.  These latter two
   slots are used for merging entities across the global module and
   module partitions respectively.  MODULE_SLOT_PARTITION is only
   present in a module.  Neither of those two slots is searched during
   name lookup -- they are internal use only.  This vector is created
   lazily once we require it, if there is only a declaration from the
   current TU, a regular binding is present.  It is converted on
   demand.

   OPTIMIZATION: Outside of the current TU, we only need ADL to work.
   We could optimize regular lookup for the current TU by glomming all
   the visible decls on its slot.  Perhaps wait until design is a
   little more settled though.

   There is only one instance of each extern-linkage namespace.  It
   appears in every module slot that makes it visible.  It also
   appears in MODULE_SLOT_GLOBAL.  (It is an ODR violation if they
   collide with some other global module entity.)  We also have an
   optimization that shares the slot for adjacent modules that declare
   the same such namespace.

   A module interface compilation produces a Compiled Module Interface
   (CMI).  The format used is Encapsulated Lazy Records Of Numbered
   Declarations, which is essentially ELF's section encapsulation. (As
   all good nerds are aware, Elrond is half Elf.)  Some sections are
   named, and contain information about the module as a whole (indices
   etc), and other sections are referenced by number.  Although I
   don't defend against actively hostile CMIs, there is some
   checksumming involved to verify data integrity.  When dumping out
   an interface, we generate a graph of all the
   independently-redeclarable DECLS that are needed, and the decls
   they reference.  From that we determine the strongly connected
   components (SCC) within this TU.  Each SCC is dumped to a separate
   numbered section of the CMI.  We generate a binding table section,
   mapping each namespace&name to a defining section.  This allows
   lazy loading.

   Lazy loading employs mmap to map a read-only image of the CMI.
   It thus only occupies address space and is paged in on demand,
   backed by the CMI file itself.  If mmap is unavailable, regular
   FILEIO is used.  Also, there's a bespoke ELF reader/writer here,
   which implements just the section table and sections (including
   string sections) of a 32-bit ELF in host byte-order.  You can of
   course inspect it with readelf.  I figured 32-bit is sufficient,
   for a single module.  I detect running out of section numbers, but
   do not implement the ELF overflow mechanism.  At least you'll get
   an error if that happens.

   We do not separate declarations and definitions.  My guess is that
   if you refer to the declaration, you'll also need the definition
   (template body, inline function, class definition etc).  But this
   does mean we can get larger SCCs than if we separated them.  It is
   unclear whether this is a win or not.

   Notice that we embed section indices into the contents of other
   sections.  Thus random manipulation of the CMI file by ELF tools
   may well break it.  The kosher way would probably be to introduce
   indirection via section symbols, but that would require defining a
   relocation type.

   Notice that lazy loading of one module's decls can cause lazy
   loading of other decls in the same or another module.  Clearly we
   want to avoid loops.  In a correct program there can be no loops in
   the module dependency graph, and the above-mentioned SCC algorithm
   places all intra-module circular dependencies in the same SCC.  It
   also orders the SCCs wrt each other, so dependent SCCs come first.
   As we load dependent modules first, we know there can be no
   reference to a higher-numbered module, and because we write out
   dependent SCCs first, likewise for SCCs within the module.  This
   allows us to immediately detect broken references.  When loading,
   we must ensure the rest of the compiler doesn't cause some
   unconnected load to occur (for instance, instantiate a template).

Classes used:

   dumper - logger

   data - buffer

   bytes_in : data - scalar reader
   bytes_out : data - scalar writer

   bytes_in::bits_in - bit stream reader
   bytes_out::bits_out - bit stream writer

   elf - ELROND format
   elf_in : elf - ELROND reader
   elf_out : elf - ELROND writer

   trees_in : bytes_in - tree reader
   trees_out : bytes_out - tree writer

   depset - dependency set
   depset::hash - hash table of depsets
   depset::tarjan - SCC determinator

   uidset<T> - set T's related to a UID
   uidset<T>::hash hash table of uidset<T>

   loc_spans - location map data

   module_state - module object

   slurping - data needed during loading

   macro_import - imported macro data
   macro_export - exported macro data

   The ELROND objects use mmap, for both reading and writing.  If mmap
   is unavailable, fileno IO is used to read and write blocks of data.

   The mapper object uses fileno IO to communicate with the server or
   program.   */

/* In expermental (trunk) sources, MODULE_VERSION is a #define passed
   in from the Makefile.  It records the modification date of the
   source directory -- that's the only way to stay sane.  In release
   sources, we (plan to) use the compiler's major.minor versioning.
   While the format might not change between at minor versions, it
   seems simplest to tie the two together.  There's no concept of
   inter-version compatibility.  */
#define IS_EXPERIMENTAL(V) ((V) >= (1U << 20))
#define MODULE_MAJOR(V) ((V) / 10000)
#define MODULE_MINOR(V) ((V) % 10000)
#define EXPERIMENT(A,B) (IS_EXPERIMENTAL (MODULE_VERSION) ? (A) : (B))
#ifndef MODULE_VERSION
#include "bversion.h"
#define MODULE_VERSION (BUILDING_GCC_MAJOR * 10000U + BUILDING_GCC_MINOR)
#elif !IS_EXPERIMENTAL (MODULE_VERSION)
#error "This is not the version I was looking for."
#endif

#define _DEFAULT_SOURCE 1 /* To get TZ field of struct tm, if available.  */
#include "config.h"
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "timevar.h"
#include "stringpool.h"
#include "dumpfile.h"
#include "bitmap.h"
#include "cgraph.h"
#include "varasm.h"
#include "tree-iterator.h"
#include "cpplib.h"
#include "mkdeps.h"
#include "incpath.h"
#include "libiberty.h"
#include "stor-layout.h"
#include "version.h"
#include "tree-diagnostic.h"
#include "toplev.h"
#include "opts.h"
#include "attribs.h"
#include "intl.h"
#include "langhooks.h"
/* This TU doesn't need or want to see the networking.  */
#define CODY_NETWORKING 0
#include "mapper-client.h"
#include <zlib.h> // for crc32, crc32_combine

#if 0 // 1 for testing no mmap
#define MAPPED_READING 0
#define MAPPED_WRITING 0
#else
#if HAVE_MMAP_FILE && _POSIX_MAPPED_FILES > 0
/* mmap, munmap.  */
#define MAPPED_READING 1
#if HAVE_SYSCONF && defined (_SC_PAGE_SIZE)
/* msync, sysconf (_SC_PAGE_SIZE), ftruncate  */
/* posix_fallocate used if available.  */
#define MAPPED_WRITING 1
#else
#define MAPPED_WRITING 0
#endif
#else
#define MAPPED_READING 0
#define MAPPED_WRITING 0
#endif
#endif

/* Some open(2) flag differences, what a colourful world it is!  */
#if defined (O_CLOEXEC)
// OK
#elif defined (_O_NOINHERIT)
/* Windows' _O_NOINHERIT matches O_CLOEXEC flag */
#define O_CLOEXEC _O_NOINHERIT
#else
#define O_CLOEXEC 0
#endif
#if defined (O_BINARY)
// Ok?
#elif defined (_O_BINARY)
/* Windows' open(2) call defaults to text!  */
#define O_BINARY _O_BINARY
#else
#define O_BINARY 0
#endif

static inline cpp_hashnode *cpp_node (tree id)
{
  return CPP_HASHNODE (GCC_IDENT_TO_HT_IDENT (id));
}

static inline tree identifier (const cpp_hashnode *node)
{
  /* HT_NODE() expands to node->ident that HT_IDENT_TO_GCC_IDENT()
     then subtracts a nonzero constant, deriving a pointer to
     a different member than ident.  That's strictly undefined
     and detected by -Warray-bounds.  Suppress it.  See PR 101372.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
  return HT_IDENT_TO_GCC_IDENT (HT_NODE (const_cast<cpp_hashnode *> (node)));
#pragma GCC diagnostic pop
}

/* Id for dumping module information.  */
int module_dump_id;

/* We have a special module owner.  */
#define MODULE_UNKNOWN (~0U)    /* Not yet known.  */

/* Prefix for section names.  */
#define MOD_SNAME_PFX ".gnu.c++"

/* Format a version for user consumption.  */

typedef char verstr_t[32];
static void
version2string (unsigned version, verstr_t &out)
{
  unsigned major = MODULE_MAJOR (version);
  unsigned minor = MODULE_MINOR (version);

  if (IS_EXPERIMENTAL (version))
    sprintf (out, "%04u/%02u/%02u-%02u:%02u%s",
	     2000 + major / 10000, (major / 100) % 100, (major % 100),
	     minor / 100, minor % 100,
	     EXPERIMENT ("", " (experimental)"));
  else
    sprintf (out, "%u.%u", major, minor);
}

/* Include files to note translation for.  */
static vec<const char *, va_heap, vl_embed> *note_includes;

/* Modules to note CMI pathames.  */
static vec<const char *, va_heap, vl_embed> *note_cmis;

/* Traits to hash an arbitrary pointer.  Entries are not deletable,
   and removal is a noop (removal needed upon destruction).  */
template <typename T>
struct nodel_ptr_hash : pointer_hash<T>, typed_noop_remove <T *> {
  /* Nothing is deletable.  Everything is insertable.  */
  static bool is_deleted (T *) { return false; }
  static void mark_deleted (T *) { gcc_unreachable (); }
};

/* Map from pointer to signed integer.   */
typedef simple_hashmap_traits<nodel_ptr_hash<void>, int> ptr_int_traits;
typedef hash_map<void *,signed,ptr_int_traits> ptr_int_hash_map;

/********************************************************************/
/* Basic streaming & ELF.  Serialization is usually via mmap.  For
   writing we slide a buffer over the output file, syncing it
   approproiately.  For reading we simply map the whole file (as a
   file-backed read-only map -- it's just address space, leaving the
   OS pager to deal with getting the data to us).  Some buffers need
   to be more conventional malloc'd contents.   */

/* Variable length buffer.  */

namespace {

constexpr line_map_uint_t loc_one = 1;

class data {
public:
  class allocator {
  public:
    /* Tools tend to moan if the dtor's not virtual.  */
    virtual ~allocator () {}

  public:
    void grow (data &obj, unsigned needed, bool exact);
    void shrink (data &obj);

  public:
    virtual char *grow (char *ptr, unsigned needed);
    virtual void shrink (char *ptr);
  };

public:
  char *buffer;		/* Buffer being transferred.  */
  /* Although size_t would be the usual size, we know we never get
     more than 4GB of buffer -- because that's the limit of the
     encapsulation format.  And if you need bigger imports, you're
     doing it wrong.  */
  unsigned size;	/* Allocated size of buffer.  */
  unsigned pos;		/* Position in buffer.  */

public:
  data ()
    :buffer (NULL), size (0), pos (0)
  {
  }
  ~data ()
  {
    /* Make sure the derived and/or using class know what they're
       doing.  */
    gcc_checking_assert (!buffer);
  }

protected:
  char *use (unsigned count)
  {
    if (size < pos + count)
      return NULL;
    char *res = &buffer[pos];
    pos += count;
    return res;
  }

  unsigned calc_crc (unsigned) const;

public:
  void unuse (unsigned count)
  {
    pos -= count;
  }

public:
  static allocator simple_memory;
};
} // anon namespace

/* The simple data allocator.  */
data::allocator data::simple_memory;

/* Grow buffer to at least size NEEDED.  */

void
data::allocator::grow (data &obj, unsigned needed, bool exact)
{
  gcc_checking_assert (needed ? needed > obj.size : !obj.size);
  if (!needed)
    /* Pick a default size.  */
    needed = EXPERIMENT (100, 1000);

  if (!exact)
    needed *= 2;
  obj.buffer = grow (obj.buffer, needed);
  if (obj.buffer)
    obj.size = needed;
  else
    obj.pos = obj.size = 0;
}

/* Free a buffer.  */

void
data::allocator::shrink (data &obj)
{
  shrink (obj.buffer);
  obj.buffer = NULL;
  obj.size = 0;
}

char *
data::allocator::grow (char *ptr, unsigned needed)
{
  return XRESIZEVAR (char, ptr, needed);
}

void
data::allocator::shrink (char *ptr)
{
  XDELETEVEC (ptr);
}

/* Calculate the crc32 of the buffer.  Note the CRC is stored in the
   first 4 bytes, so don't include them.  */

unsigned
data::calc_crc (unsigned l) const
{
  return crc32 (0, (unsigned char *)buffer + 4, l - 4);
}

class elf_in;

/* Byte stream reader.  */

namespace {
class bytes_in : public data {
  typedef data parent;

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
  bool begin (location_t loc, elf_in *src, const char *name);
  /* Begin reading a numbered section with optional name.  */
  bool begin (location_t loc, elf_in *src, unsigned, const char * = NULL);
  /* Complete reading a buffer.  Propagate errors and return true on
     success.  */
  bool end (elf_in *src);
  /* Return true if there is unread data.  */
  bool more_p () const
  {
    return pos != size;
  }

public:
  /* Start reading at OFFSET.  */
  void random_access (unsigned offset)
  {
    if (offset > size)
      set_overrun ();
    pos = offset;
  }

public:
  void align (unsigned boundary)
  {
    if (unsigned pad = pos & (boundary - 1))
      read (boundary - pad);
  }

public:
  const char *read (unsigned count)
  {
    char *ptr = use (count);
    if (!ptr)
      set_overrun ();
    return ptr;
  }

public:
  bool check_crc () const;
  /* We store the CRC in the first 4 bytes, using host endianness.  */
  unsigned get_crc () const
  {
    return *(const unsigned *)&buffer[0];
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
  int c () ATTRIBUTE_UNUSED;		/* Read a char.  */
  int i ();		/* Read a signed int.  */
  unsigned u ();	/* Read an unsigned int.  */
  size_t z ();		/* Read a size_t.  */
  location_t loc ();    /* Read a location_t.  */
  HOST_WIDE_INT wi ();  /* Read a HOST_WIDE_INT.  */
  unsigned HOST_WIDE_INT wu (); /* Read an unsigned HOST_WIDE_INT.  */
  const char *str (size_t * = NULL); /* Read a string.  */
  const void *buf (size_t); /* Read a fixed-length buffer.  */
  cpp_hashnode *cpp_node (); /* Read a cpp node.  */

  struct bits_in;
  bits_in stream_bits ();
};
} // anon namespace

/* Verify the buffer's CRC is correct.  */

bool
bytes_in::check_crc () const
{
  if (size < 4)
    return false;

  unsigned c_crc = calc_crc (size);
  if (c_crc != get_crc ())
    return false;

  return true;
}

class elf_out;

/* Byte stream writer.  */

namespace {
class bytes_out : public data {
  typedef data parent;

public:
  allocator *memory;	/* Obtainer of memory.  */

public:
  bytes_out (allocator *memory)
    : parent (), memory (memory)
  {
  }
  ~bytes_out ()
  {
  }

public:
  bool streaming_p () const
  {
    return memory != NULL;
  }

public:
  void set_crc (unsigned *crc_ptr);

public:
  /* Begin writing, maybe reserve space for CRC.  */
  void begin (bool need_crc = true);
  /* Finish writing.  Spill to section by number.  */
  unsigned end (elf_out *, unsigned, unsigned *crc_ptr = NULL);

public:
  void align (unsigned boundary)
  {
    if (unsigned pad = pos & (boundary - 1))
      write (boundary - pad);
  }

public:
  char *write (unsigned count, bool exact = false)
  {
    if (size < pos + count)
      memory->grow (*this, pos + count, exact);
    return use (count);
  }

public:
  void u32 (unsigned);  /* Write uncompressed integer.  */

public:
  void c (unsigned char) ATTRIBUTE_UNUSED; /* Write unsigned char.  */
  void i (int);		/* Write signed int.  */
  void u (unsigned);	/* Write unsigned int.  */
  void z (size_t s);	/* Write size_t.  */
  void loc (location_t); /* Write location_t.  */
  void wi (HOST_WIDE_INT); /* Write HOST_WIDE_INT.  */
  void wu (unsigned HOST_WIDE_INT);  /* Write unsigned HOST_WIDE_INT.  */
  void str (const char *ptr)
  {
    str (ptr, strlen (ptr));
  }
  void cpp_node (const cpp_hashnode *node)
  {
    str ((const char *)NODE_NAME (node), NODE_LEN (node));
  }
  void str (const char *, size_t);  /* Write string of known length.  */
  void buf (const void *, size_t);  /* Write fixed length buffer.  */
  void *buf (size_t); /* Create a writable buffer */

  struct bits_out;
  bits_out stream_bits ();

public:
  /* Format a NUL-terminated raw string.  */
  void printf (const char *, ...) ATTRIBUTE_PRINTF_2;
  void print_time (const char *, const tm *, const char *);

public:
  /* Dump instrumentation.  */
  static void instrument ();

protected:
  /* Instrumentation.  */
  static unsigned spans[4];
  static unsigned lengths[4];
};
} // anon namespace

/* Finish bit packet.  Rewind the bytes not used.  */

static unsigned
bit_flush (data& bits, uint32_t& bit_val, unsigned& bit_pos)
{
  gcc_assert (bit_pos);
  unsigned bytes = (bit_pos + 7) / 8;
  bits.unuse (4 - bytes);
  bit_pos = 0;
  bit_val = 0;
  return bytes;
}

/* Bit stream reader (RAII-enabled).  Bools are packed into bytes.  You
   cannot mix bools and non-bools.  Use bflush to flush the current stream
   of bools on demand.  Upon destruction bflush is called.

   When reading, we don't know how many bools we'll read in.  So read
   4 bytes-worth, and then rewind when flushing if we didn't need them
   all.  You can't have a block of bools closer than 4 bytes to the
   end of the buffer.

   Both bits_in and bits_out maintain the necessary state for bit packing,
   and since these objects are locally constructed the compiler can more
   easily track their state across consecutive reads/writes and optimize
   away redundant buffering checks.  */

struct bytes_in::bits_in {
  bytes_in& in;
  uint32_t bit_val = 0;
  unsigned bit_pos = 0;

  bits_in (bytes_in& in)
    : in (in)
  { }

  ~bits_in ()
  {
    bflush ();
  }

  bits_in(bits_in&&) = default;
  bits_in(const bits_in&) = delete;
  bits_in& operator=(const bits_in&) = delete;

  /* Completed a block of bools.  */
  void bflush ()
  {
    if (bit_pos)
      bit_flush (in, bit_val, bit_pos);
  }

  /* Read one bit.  */
  bool b ()
  {
    if (!bit_pos)
      bit_val = in.u32 ();
    bool x = (bit_val >> bit_pos) & 1;
    bit_pos = (bit_pos + 1) % 32;
    return x;
  }
};

/* Factory function for bits_in.  */

bytes_in::bits_in
bytes_in::stream_bits ()
{
  return bits_in (*this);
}

/* Bit stream writer (RAII-enabled), counterpart to bits_in.  */

struct bytes_out::bits_out {
  bytes_out& out;
  uint32_t bit_val = 0;
  unsigned bit_pos = 0;
  char is_set = -1;

  bits_out (bytes_out& out)
    : out (out)
  { }

  ~bits_out ()
  {
    bflush ();
  }

  bits_out(bits_out&&) = default;
  bits_out(const bits_out&) = delete;
  bits_out& operator=(const bits_out&) = delete;

  /* Completed a block of bools.  */
  void bflush ()
  {
    if (bit_pos)
      {
	out.u32 (bit_val);
	out.lengths[2] += bit_flush (out, bit_val, bit_pos);
      }
    out.spans[2]++;
    is_set = -1;
  }

  /* Write one bit.

     It may be worth optimizing for most bools being zero.  Some kind of
     run-length encoding?  */
  void b (bool x)
  {
    if (is_set != x)
      {
	is_set = x;
	out.spans[x]++;
      }
    out.lengths[x]++;
    bit_val |= unsigned (x) << bit_pos++;
    if (bit_pos == 32)
      {
	out.u32 (bit_val);
	out.lengths[2] += bit_flush (out, bit_val, bit_pos);
      }
  }
};

/* Factory function for bits_out.  */

bytes_out::bits_out
bytes_out::stream_bits ()
{
  return bits_out (*this);
}

/* Instrumentation.  */
unsigned bytes_out::spans[4];
unsigned bytes_out::lengths[4];

/* If CRC_PTR non-null, set the CRC of the buffer.  Mix the CRC into
   that pointed to by CRC_PTR.  */

void
bytes_out::set_crc (unsigned *crc_ptr)
{
  if (crc_ptr)
    {
      gcc_checking_assert (pos >= 4);

      unsigned crc = calc_crc (pos);
      unsigned accum = *crc_ptr;
      /* Only mix the existing *CRC_PTR if it is non-zero.  */
      accum = accum ? crc32_combine (accum, crc, pos - 4) : crc;
      *crc_ptr = accum;

      /* Buffer will be sufficiently aligned.  */
      *(unsigned *)buffer = crc;
    }
}

/* Exactly 4 bytes.  Used internally for bool packing and a few other
   places.  We can't simply use uint32_t because (a) alignment and
   (b) we need little-endian for the bool streaming rewinding to make
   sense.  */

void
bytes_out::u32 (unsigned val)
{
  if (char *ptr = write (4))
    {
      ptr[0] = val;
      ptr[1] = val >> 8;
      ptr[2] = val >> 16;
      ptr[3] = val >> 24;
    }
}

unsigned
bytes_in::u32 ()
{
  unsigned val = 0;
  if (const char *ptr = read (4))
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
  if (char *ptr = write (1))
    *ptr = v;
}

int
bytes_in::c ()
{
  int v = 0;
  if (const char *ptr = read (1))
    v = (unsigned char)ptr[0];
  return v;
}

/* Ints 7-bit as a byte. Otherwise a 3bit count of following bytes in
   big-endian form.  4 bits are in the first byte.  */

void
bytes_out::i (int v)
{
  if (char *ptr = write (1))
    {
      if (v <= 0x3f && v >= -0x40)
	*ptr = v & 0x7f;
      else
	{
	  unsigned bytes = 0;
	  int probe;
	  if (v >= 0)
	    for (probe = v >> 8; probe > 0x7; probe >>= 8)
	      bytes++;
	  else
	    for (probe = v >> 8; probe < -0x8; probe >>= 8)
	      bytes++;
	  *ptr = 0x80 | bytes << 4 | (probe & 0xf);
	  if ((ptr = write (++bytes)))
	    for (; bytes--; v >>= 8)
	      ptr[bytes] = v & 0xff;
	}
    }
}

int
bytes_in::i ()
{
  int v = 0;
  if (const char *ptr = read (1))
    {
      v = *ptr & 0xff;
      if (v & 0x80)
	{
	  unsigned bytes = (v >> 4) & 0x7;
	  v &= 0xf;
	  if (v & 0x8)
	    v |= -1 ^ 0x7;
	  /* unsigned necessary due to left shifts of -ve values.  */
	  unsigned uv = unsigned (v);
	  if ((ptr = read (++bytes)))
	    while (bytes--)
	      uv = (uv << 8) | (*ptr++ & 0xff);
	  v = int (uv);
	}
      else if (v & 0x40)
	v |= -1 ^ 0x3f;
    }

  return v;
}

void
bytes_out::u (unsigned v)
{
  if (char *ptr = write (1))
    {
      if (v <= 0x7f)
	*ptr = v;
      else
	{
	  unsigned bytes = 0;
	  unsigned probe;
	  for (probe = v >> 8; probe > 0xf; probe >>= 8)
	    bytes++;
	  *ptr = 0x80 | bytes << 4 | probe;
	  if ((ptr = write (++bytes)))
	    for (; bytes--; v >>= 8)
	      ptr[bytes] = v & 0xff;
	}
    }
}

unsigned
bytes_in::u ()
{
  unsigned v = 0;

  if (const char *ptr = read (1))
    {
      v = *ptr & 0xff;
      if (v & 0x80)
	{
	  unsigned bytes = (v >> 4) & 0x7;
	  v &= 0xf;
	  if ((ptr = read (++bytes)))
	    while (bytes--)
	      v = (v << 8) | (*ptr++ & 0xff);
	}
    }

  return v;
}

void
bytes_out::wi (HOST_WIDE_INT v)
{
  if (char *ptr = write (1))
    {
      if (v <= 0x3f && v >= -0x40)
	*ptr = v & 0x7f;
      else
	{
	  unsigned bytes = 0;
	  HOST_WIDE_INT probe;
	  if (v >= 0)
	    for (probe = v >> 8; probe > 0x7; probe >>= 8)
	      bytes++;
	  else
	    for (probe = v >> 8; probe < -0x8; probe >>= 8)
	      bytes++;
	  *ptr = 0x80 | bytes << 4 | (probe & 0xf);
	  if ((ptr = write (++bytes)))
	    for (; bytes--; v >>= 8)
	      ptr[bytes] = v & 0xff;
	}
    }
}

HOST_WIDE_INT
bytes_in::wi ()
{
  HOST_WIDE_INT v = 0;
  if (const char *ptr = read (1))
    {
      v = *ptr & 0xff;
      if (v & 0x80)
	{
	  unsigned bytes = (v >> 4) & 0x7;
	  v &= 0xf;
	  if (v & 0x8)
	    v |= -1 ^ 0x7;
	  /* unsigned necessary due to left shifts of -ve values.  */
	  unsigned HOST_WIDE_INT uv = (unsigned HOST_WIDE_INT) v;
	  if ((ptr = read (++bytes)))
	    while (bytes--)
	      uv = (uv << 8) | (*ptr++ & 0xff);
	  v = (HOST_WIDE_INT) uv;
	}
      else if (v & 0x40)
	v |= -1 ^ 0x3f;
    }

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

/* location_t written as 32- or 64-bit as needed.  */

inline void bytes_out::loc (location_t l)
{
  if (sizeof (location_t) > sizeof (unsigned))
    wu (l);
  else
    u (l);
}

inline location_t bytes_in::loc ()
{
  if (sizeof (location_t) > sizeof (unsigned))
    return wu ();
  else
    return u ();
}

/* Buffer simply memcpied.  */
void *
bytes_out::buf (size_t len)
{
  align (sizeof (void *) * 2);
  return write (len);
}

void
bytes_out::buf (const void *src, size_t len)
{
  if (void *ptr = buf (len))
    memcpy (ptr, src, len);
}

const void *
bytes_in::buf (size_t len)
{
  align (sizeof (void *) * 2);
  const char *ptr = read (len);

  return ptr;
}

/* strings as an size_t length, followed by the buffer.  Make sure
   there's a NUL terminator on read.  */

void
bytes_out::str (const char *string, size_t len)
{
  z (len);
  if (len)
    {
      gcc_checking_assert (!string[len]);
      buf (string, len + 1);
    }
}

const char *
bytes_in::str (size_t *len_p)
{
  size_t len = z ();

  /* We're about to trust some user data.  */
  if (overrun)
    len = 0;
  if (len_p)
    *len_p = len;
  const char *str = NULL;
  if (len)
    {
      str = reinterpret_cast<const char *> (buf (len + 1));
      if (!str || str[len])
	{
	  set_overrun ();
	  str = NULL;
	}
    }
  return str ? str : "";
}

cpp_hashnode *
bytes_in::cpp_node ()
{
  size_t len;
  const char *s = str (&len);
  if (!len)
    return NULL;
  return ::cpp_node (get_identifier_with_length (s, len));
}

/* Format a string directly to the buffer, including a terminating
   NUL.  Intended for human consumption.  */

void
bytes_out::printf (const char *format, ...)
{
  va_list args;
  /* Exercise buffer expansion.  */
  size_t len = EXPERIMENT (10, 500);

  while (char *ptr = write (len))
    {
      va_start (args, format);
      size_t actual = vsnprintf (ptr, len, format, args) + 1;
      va_end (args);
      if (actual <= len)
	{
	  unuse (len - actual);
	  break;
	}
      unuse (len);
      len = actual;
    }
}

void
bytes_out::print_time (const char *kind, const tm *time, const char *tz)
{
  printf ("%stime: %4u/%02u/%02u %02u:%02u:%02u %s",
	  kind, time->tm_year + 1900, time->tm_mon + 1, time->tm_mday,
	  time->tm_hour, time->tm_min, time->tm_sec, tz);
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
  enum private_constants {
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
    SHT_NONE = 0,	/* No contents.  */
    SHT_PROGBITS = 1, /* Random bytes.  */
    SHT_STRTAB = 3,	/* A string table.  */

    /* Section flags.  */
    SHF_NONE = 0x00,	/* Nothing.  */
    SHF_STRINGS = 0x20,  /* NUL-Terminated strings.  */

    /* I really hope we do not get CMI files larger than 4GB.  */
    MY_CLASS = CLASS32,
    /* It is host endianness that is relevant.  */
    MY_ENDIAN = DATA2LSB
#ifdef WORDS_BIGENDIAN
    ^ DATA2LSB ^ DATA2MSB
#endif
  };

public:
  /* Constants visible to users.  */
  enum public_constants {
    /* Special error codes.  Breaking layering a bit.  */
    E_BAD_DATA = -1,  /* Random unexpected data errors.  */
    E_BAD_LAZY = -2,  /* Badly ordered laziness.  */
    E_BAD_IMPORT = -3 /* A nested import failed.  */
  };

protected:
  /* File identification.  On-disk representation.  */
  struct ident {
    uint8_t magic[4];	/* 0x7f, 'E', 'L', 'F' */
    uint8_t klass;	/* 4:CLASS32 */
    uint8_t data;	/* 5:DATA2[LM]SB */
    uint8_t version;	/* 6:EV_CURRENT  */
    uint8_t osabi;	/* 7:OSABI_NONE */
    uint8_t abiver;	/* 8: 0 */
    uint8_t pad[7];	/* 9-15 */
  };
  /* File header.  On-disk representation.  */
  struct header {
    struct ident ident;
    uint16_t type;	/* ET_NONE */
    uint16_t machine;	/* EM_NONE */
    uint32_t version;	/* EV_CURRENT */
    uint32_t entry;	/* 0 */
    uint32_t phoff;	/* 0 */
    uint32_t shoff;	/* Section Header Offset in file */
    uint32_t flags;
    uint16_t ehsize;	/* ELROND Header SIZE -- sizeof (header) */
    uint16_t phentsize; /* 0 */
    uint16_t phnum;	/* 0 */
    uint16_t shentsize; /* Section Header SIZE -- sizeof (section) */
    uint16_t shnum;	/* Section Header NUM */
    uint16_t shstrndx;	/* Section Header STRing iNDeX */
  };
  /* File section.  On-disk representation.  */
  struct section {
    uint32_t name;	/* String table offset.  */
    uint32_t type;	/* SHT_* */
    uint32_t flags;	/* SHF_* */
    uint32_t addr;	/* 0 */
    uint32_t offset;	/* OFFSET in file */
    uint32_t size;	/* SIZE of section */
    uint32_t link;	/* 0 */
    uint32_t info;	/* 0 */
    uint32_t addralign; /* 0 */
    uint32_t entsize;	/* ENTry SIZE, usually 0 */
  };

protected:
  data hdr;	/* The header.  */
  data sectab; 	/* The section table.  */
  data strtab;  /* String table.  */
  int fd;   	/* File descriptor we're reading or writing.  */
  int err; 	/* Sticky error code.  */

public:
  /* Construct from STREAM.  E is errno if STREAM NULL.  */
  elf (int fd, int e)
    :hdr (), sectab (), strtab (), fd (fd), err (fd >= 0 ? 0 : e)
  {}
  ~elf ()
  {
    gcc_checking_assert (fd < 0 && !hdr.buffer
			 && !sectab.buffer && !strtab.buffer);
  }

public:
  /* Return the error, if we have an error.  */
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
  /* Get an error string.  */
  const char *get_error (const char *) const;

public:
  /* Begin reading/writing file.  Return false on error.  */
  bool begin () const
  {
    return !get_error ();
  }
  /* Finish reading/writing file.  Return false on error.  */
  bool end ();
};

/* Return error string.  */

const char *
elf::get_error (const char *name) const
{
  if (!name)
    return "Unknown CMI mapping";

  switch (err)
    {
    case 0:
      gcc_unreachable ();
    case E_BAD_DATA:
      return "Bad file data";
    case E_BAD_IMPORT:
      return "Bad import dependency";
    case E_BAD_LAZY:
      return "Bad lazy ordering";
    default:
      return xstrerror (err);
    }
}

/* Finish file, return true if there's an error.  */

bool
elf::end ()
{
  /* Close the stream and free the section table.  */
  if (fd >= 0 && close (fd))
    set_error (errno);
  fd = -1;

  return !get_error ();
}

/* ELROND reader.  */

class elf_in : public elf {
  typedef elf parent;

private:
  /* For freezing & defrosting.  */
#if !defined (HOST_LACKS_INODE_NUMBERS)
  dev_t device;
  ino_t inode;
#endif

public:
  elf_in (int fd, int e)
    :parent (fd, e)
  {
  }
  ~elf_in ()
  {
  }

public:
  bool is_frozen () const
  {
    return fd < 0 && hdr.pos;
  }
  bool is_freezable () const
  {
    return fd >= 0 && hdr.pos;
  }
  void freeze ();
  bool defrost (const char *);

  /* If BYTES is in the mmapped area, allocate a new buffer for it.  */
  void preserve (bytes_in &bytes ATTRIBUTE_UNUSED)
  {
#if MAPPED_READING
    if (hdr.buffer && bytes.buffer >= hdr.buffer
	&& bytes.buffer < hdr.buffer + hdr.pos)
      {
	char *buf = bytes.buffer;
	bytes.buffer = data::simple_memory.grow (NULL, bytes.size);
	memcpy (bytes.buffer, buf, bytes.size);
      }
#endif
  }
  /* If BYTES is not in SELF's mmapped area, free it.  SELF might be
     NULL. */
  static void release (elf_in *self ATTRIBUTE_UNUSED, bytes_in &bytes)
  {
#if MAPPED_READING
    if (!(self && self->hdr.buffer && bytes.buffer >= self->hdr.buffer
	  && bytes.buffer < self->hdr.buffer + self->hdr.pos))
#endif
      data::simple_memory.shrink (bytes.buffer);
    bytes.buffer = NULL;
    bytes.size = 0;
  }

public:
  static void grow (data &data, unsigned needed)
  {
    gcc_checking_assert (!data.buffer);
#if !MAPPED_READING
    data.buffer = XNEWVEC (char, needed);
#endif
    data.size = needed;
  }
  static void shrink (data &data)
  {
#if !MAPPED_READING
    XDELETEVEC (data.buffer);
#endif
    data.buffer = NULL;
    data.size = 0;
  }

public:
  const section *get_section (unsigned s) const
  {
    if (s * sizeof (section) < sectab.size)
      return reinterpret_cast<const section *>
	(&sectab.buffer[s * sizeof (section)]);
    else
      return NULL;
  }
  unsigned get_section_limit () const
  {
    return sectab.size / sizeof (section);
  }

protected:
  const char *read (data *, unsigned, unsigned);

public:
  /* Read section by number.  */
  bool read (data *d, const section *s)
  {
    return s && read (d, s->offset, s->size);
  }

  /* Find section by name.  */
  unsigned find (const char *name);
  /* Find section by index.  */
  const section *find (unsigned snum, unsigned type = SHT_PROGBITS);

public:
  /* Release the string table, when we're done with it.  */
  void release ()
  {
    shrink (strtab);
  }

public:
  bool begin (location_t);
  bool end ()
  {
    release ();
#if MAPPED_READING
    if (hdr.buffer)
      munmap (hdr.buffer, hdr.pos);
    hdr.buffer = NULL;
#endif
    shrink (sectab);

    return parent::end ();
  }

public:
  /* Return string name at OFFSET.  Checks OFFSET range.  Always
     returns non-NULL.  We know offset 0 is an empty string.  */
  const char *name (unsigned offset)
  {
    return &strtab.buffer[offset < strtab.size ? offset : 0];
  }
};

/* ELROND writer.  */

class elf_out : public elf, public data::allocator {
  typedef elf parent;
  /* Desired section alignment on disk.  */
  static const int SECTION_ALIGN = 16;

private:
  ptr_int_hash_map identtab;	/* Map of IDENTIFIERS to strtab offsets. */
  unsigned pos;			/* Write position in file.  */
#if MAPPED_WRITING
  unsigned offset;		/* Offset of the mapping.  */
  unsigned extent;		/* Length of mapping.  */
  unsigned page_size;		/* System page size.  */
#endif

public:
  elf_out (int fd, int e)
    :parent (fd, e), identtab (500), pos (0)
  {
#if MAPPED_WRITING
    offset = extent = 0;
    page_size = sysconf (_SC_PAGE_SIZE);
    if (page_size < SECTION_ALIGN)
      /* Something really strange.  */
      set_error (EINVAL);
#endif
  }
  ~elf_out ()
  {
    data::simple_memory.shrink (hdr);
    data::simple_memory.shrink (sectab);
    data::simple_memory.shrink (strtab);
  }

#if MAPPED_WRITING
private:
  void create_mapping (unsigned ext, bool extending = true);
  void remove_mapping ();
#endif

protected:
  using allocator::grow;
  char *grow (char *, unsigned needed) final override;
#if MAPPED_WRITING
  using allocator::shrink;
  void shrink (char *) final override;
#endif

public:
  unsigned get_section_limit () const
  {
    return sectab.pos / sizeof (section);
  }

protected:
  unsigned add (unsigned type, unsigned name = 0,
		unsigned off = 0, unsigned size = 0, unsigned flags = SHF_NONE);
  unsigned write (const data &);
#if MAPPED_WRITING
  unsigned write (const bytes_out &);
#endif

public:
  /* IDENTIFIER to strtab offset.  */
  unsigned name (tree ident);
  /* String literal to strtab offset.  */
  unsigned name (const char *n);
  /* Qualified name of DECL to strtab offset.  */
  unsigned qualified_name (tree decl, bool is_defn);

private:
  unsigned strtab_write (const char *s, unsigned l);
  void strtab_write (tree decl, int);

public:
  /* Add a section with contents or strings.  */
  unsigned add (const bytes_out &, bool string_p, unsigned name);

public:
  /* Begin and end writing.  */
  bool begin ();
  bool end ();
};

/* Begin reading section NAME (of type PROGBITS) from SOURCE.
   Data always checked for CRC.  */

bool
bytes_in::begin (location_t loc, elf_in *source, const char *name)
{
  unsigned snum = source->find (name);

  return begin (loc, source, snum, name);
}

/* Begin reading section numbered SNUM with NAME (may be NULL).  */

bool
bytes_in::begin (location_t loc, elf_in *source, unsigned snum, const char *name)
{
  if (!source->read (this, source->find (snum))
      || !size || !check_crc ())
    {
      source->set_error (elf::E_BAD_DATA);
      source->shrink (*this);
      if (name)
	error_at (loc, "section %qs is missing or corrupted", name);
      else
	error_at (loc, "section #%u is missing or corrupted", snum);
      return false;
    }
  pos = 4;
  return true;
}

/* Finish reading a section.  */

bool
bytes_in::end (elf_in *src)
{
  if (more_p ())
    set_overrun ();
  if (overrun)
    src->set_error ();

  src->shrink (*this);

  return !overrun;
}

/* Begin writing buffer.  */

void
bytes_out::begin (bool need_crc)
{
  if (need_crc)
    pos = 4;
  memory->grow (*this, 0, false);
}

/* Finish writing buffer.  Stream out to SINK as named section NAME.
   Return section number or 0 on failure.  If CRC_PTR is true, crc
   the data.  Otherwise it is a string section.  */

unsigned
bytes_out::end (elf_out *sink, unsigned name, unsigned *crc_ptr)
{
  lengths[3] += pos;
  spans[3]++;

  set_crc (crc_ptr);
  unsigned sec_num = sink->add (*this, !crc_ptr, name);
  memory->shrink (*this);

  return sec_num;
}

/* Close and open the file, without destroying it.  */

void
elf_in::freeze ()
{
  gcc_checking_assert (!is_frozen ());
#if MAPPED_READING
  if (munmap (hdr.buffer, hdr.pos) < 0)
    set_error (errno);
#endif
  if (close (fd) < 0)
    set_error (errno);
  fd = -1;
}

bool
elf_in::defrost (const char *name)
{
  gcc_checking_assert (is_frozen ());
  struct stat stat;

  fd = open (name, O_RDONLY | O_CLOEXEC | O_BINARY);
  if (fd < 0 || fstat (fd, &stat) < 0)
    set_error (errno);
  else
    {
      bool ok = hdr.pos == unsigned (stat.st_size);
#ifndef HOST_LACKS_INODE_NUMBERS
      if (device != stat.st_dev
	  || inode != stat.st_ino)
	ok = false;
#endif
      if (!ok)
	set_error (EMFILE);
#if MAPPED_READING
      if (ok)
	{
	  char *mapping = reinterpret_cast<char *>
	    (mmap (NULL, hdr.pos, PROT_READ, MAP_SHARED, fd, 0));
	  if (mapping == MAP_FAILED)
	  fail:
	      set_error (errno);
	  else
	    {
	      if (madvise (mapping, hdr.pos, MADV_RANDOM))
		goto fail;

	      /* These buffers are never NULL in this case.  */
	      strtab.buffer = mapping + strtab.pos;
	      sectab.buffer = mapping + sectab.pos;
	      hdr.buffer = mapping;
	    }
	}
#endif
    }

  return !get_error ();
}

/* Read at current position into BUFFER.  Return true on success.  */

const char *
elf_in::read (data *data, unsigned pos, unsigned length)
{
#if MAPPED_READING
  if (pos + length > hdr.pos)
    {
      set_error (EINVAL);
      return NULL;
    }
#else
  if (pos != ~0u && lseek (fd, pos, SEEK_SET) < 0)
    {
      set_error (errno);
      return NULL;
    }
#endif
  grow (*data, length);
#if MAPPED_READING
  data->buffer = hdr.buffer + pos;
#else
  if (::read (fd, data->buffer, data->size) != ssize_t (length))
    {
      set_error (errno);
      shrink (*data);
      return NULL;
    }
#endif

  return data->buffer;
}

/* Read section SNUM of TYPE.  Return section pointer or NULL on error.  */

const elf::section *
elf_in::find (unsigned snum, unsigned type)
{
  const section *sec = get_section (snum);
  if (!snum || !sec || sec->type != type)
    return NULL;
  return sec;
}

/* Find a section NAME and TYPE.  Return section number, or zero on
   failure.  */

unsigned
elf_in::find (const char *sname)
{
  for (unsigned pos = sectab.size; pos -= sizeof (section); )
    {
      const section *sec
	= reinterpret_cast<const section *> (&sectab.buffer[pos]);

      if (0 == strcmp (sname, name (sec->name)))
	return pos / sizeof (section);
    }

  return 0;
}

/* Begin reading file.  Verify header.  Pull in section and string
   tables.  Return true on success.  */

bool
elf_in::begin (location_t loc)
{
  if (!parent::begin ())
    return false;

  struct stat stat;
  unsigned size = 0;
  if (!fstat (fd, &stat))
    {
#if !defined (HOST_LACKS_INODE_NUMBERS)
      device = stat.st_dev;
      inode = stat.st_ino;
#endif
      /* Never generate files > 4GB, check we've not been given one.  */
      if (stat.st_size == unsigned (stat.st_size))
	size = unsigned (stat.st_size);
    }

#if MAPPED_READING
  /* MAP_SHARED so that the file is backing store.  If someone else
     concurrently writes it, they're wrong.  */
  void *mapping = mmap (NULL, size, PROT_READ, MAP_SHARED, fd, 0);
  if (mapping == MAP_FAILED)
    {
    fail:
      set_error (errno);
      return false;
    }
  /* We'll be hopping over this randomly.  Some systems declare the
     first parm as char *, and other declare it as void *.  */
  if (madvise (reinterpret_cast <char *> (mapping), size, MADV_RANDOM))
    goto fail;

  hdr.buffer = (char *)mapping;
#else
  read (&hdr, 0, sizeof (header));
#endif
  hdr.pos = size; /* Record size of the file.  */

  const header *h = reinterpret_cast<const header *> (hdr.buffer);
  if (!h)
    return false;

  if (h->ident.magic[0] != 0x7f
      || h->ident.magic[1] != 'E'
      || h->ident.magic[2] != 'L'
      || h->ident.magic[3] != 'F')
    {
      error_at (loc, "not Encapsulated Lazy Records of Named Declarations");
    failed:
      shrink (hdr);
      return false;
    }

  /* We expect a particular format -- the ELF is not intended to be
     distributable.  */
  if (h->ident.klass != MY_CLASS
      || h->ident.data != MY_ENDIAN
      || h->ident.version != EV_CURRENT
      || h->type != ET_NONE
      || h->machine != EM_NONE
      || h->ident.osabi != OSABI_NONE)
    {
      error_at (loc, "unexpected encapsulation format or type");
      goto failed;
    }

  int e = -1;
  if (!h->shoff || h->shentsize != sizeof (section))
    {
    malformed:
      set_error (e);
      error_at (loc, "encapsulation is malformed");
      goto failed;
    }

  unsigned strndx = h->shstrndx;
  unsigned shnum = h->shnum;
  if (shnum == SHN_XINDEX)
    {
      if (!read (&sectab, h->shoff, sizeof (section)))
	{
	section_table_fail:
	  e = errno;
	  goto malformed;
	}
      shnum = get_section (0)->size;
      /* Freeing does mean we'll re-read it in the case we're not
	 mapping, but this is going to be rare.  */
      shrink (sectab);
    }

  if (!shnum)
    goto malformed;

  if (!read (&sectab, h->shoff, shnum * sizeof (section)))
    goto section_table_fail;

  if (strndx == SHN_XINDEX)
    strndx = get_section (0)->link;

  if (!read (&strtab, find (strndx, SHT_STRTAB)))
    goto malformed;

  /* The string table should be at least one byte, with NUL chars
     at either end.  */
  if (!(strtab.size && !strtab.buffer[0]
	&& !strtab.buffer[strtab.size - 1]))
    goto malformed;

#if MAPPED_READING
  /* Record the offsets of the section and string tables.  */
  sectab.pos = h->shoff;
  strtab.pos = shnum * sizeof (section);
#else
  shrink (hdr);
#endif

  return true;
}

/* Create a new mapping.  */

#if MAPPED_WRITING
void
elf_out::create_mapping (unsigned ext, bool extending)
{
#ifndef HAVE_POSIX_FALLOCATE
#define posix_fallocate(fd,off,len) ftruncate (fd, off + len)
#endif
  void *mapping = MAP_FAILED;
  if (extending && ext < 1024 * 1024)
    {
      if (!posix_fallocate (fd, offset, ext * 2))
	mapping = mmap (NULL, ext * 2, PROT_READ | PROT_WRITE,
			MAP_SHARED, fd, offset);
      if (mapping != MAP_FAILED)
	ext *= 2;
    }
  if (mapping == MAP_FAILED)
    {
      if (!extending || !posix_fallocate (fd, offset, ext))
	mapping = mmap (NULL, ext, PROT_READ | PROT_WRITE,
			MAP_SHARED, fd, offset);
      if (mapping == MAP_FAILED)
	{
	  set_error (errno);
	  mapping = NULL;
	  ext = 0;
	}
    }
#undef posix_fallocate
  hdr.buffer = (char *)mapping;
  extent = ext;
}
#endif

/* Flush out the current mapping.  */

#if MAPPED_WRITING
void
elf_out::remove_mapping ()
{
  if (hdr.buffer)
    {
      /* MS_ASYNC dtrt with the removed mapping, including a
	 subsequent overlapping remap.  */
      if (msync (hdr.buffer, extent, MS_ASYNC)
	  || munmap (hdr.buffer, extent))
	/* We're somewhat screwed at this point.  */
	set_error (errno);
    }

  hdr.buffer = NULL;
}
#endif

/* Grow a mapping of PTR to be NEEDED bytes long.  This gets
   interesting if the new size grows the EXTENT.  */

char *
elf_out::grow (char *data, unsigned needed)
{
  if (!data)
    {
      /* First allocation, check we're aligned.  */
      gcc_checking_assert (!(pos & (SECTION_ALIGN - 1)));
#if MAPPED_WRITING
      data = hdr.buffer + (pos - offset);
#endif
    }

#if MAPPED_WRITING
  unsigned off = data - hdr.buffer;
  if (off + needed > extent)
    {
      /* We need to grow the mapping.  */
      unsigned lwm = off & ~(page_size - 1);
      unsigned hwm = (off + needed + page_size - 1) & ~(page_size - 1);

      gcc_checking_assert (hwm > extent);

      remove_mapping ();

      offset += lwm;
      create_mapping (extent < hwm - lwm ? hwm - lwm : extent);

      data = hdr.buffer + (off - lwm);
    }
#else
  data = allocator::grow (data, needed);
#endif

  return data;
}

#if MAPPED_WRITING
/* Shrinking is a NOP.  */
void
elf_out::shrink (char *)
{
}
#endif

/* Write S of length L to the strtab buffer.  L must include the ending
   NUL, if that's what you want.  */

unsigned
elf_out::strtab_write (const char *s, unsigned l)
{
  if (strtab.pos + l > strtab.size)
    data::simple_memory.grow (strtab, strtab.pos + l, false);
  memcpy (strtab.buffer + strtab.pos, s, l);
  unsigned res = strtab.pos;
  strtab.pos += l;
  return res;
}

/* Write qualified name of decl.  INNER >0 if this is a definition, <0
   if this is a qualifier of an outer name.  */

void
elf_out::strtab_write (tree decl, int inner)
{
  tree ctx = CP_DECL_CONTEXT (decl);
  if (TYPE_P (ctx))
    ctx = TYPE_NAME (ctx);
  if (ctx != global_namespace)
    strtab_write (ctx, -1);

  tree name = DECL_NAME (decl);
  if (!name)
    name = DECL_ASSEMBLER_NAME_RAW (decl);
  strtab_write (IDENTIFIER_POINTER (name), IDENTIFIER_LENGTH (name));

  if (inner)
    strtab_write (&"::{}"[inner+1], 2);
}

/* Map IDENTIFIER IDENT to strtab offset.  Inserts into strtab if not
   already there.  */

unsigned
elf_out::name (tree ident)
{
  unsigned res = 0;
  if (ident)
    {
      bool existed;
      int *slot = &identtab.get_or_insert (ident, &existed);
      if (!existed)
	*slot = strtab_write (IDENTIFIER_POINTER (ident),
			      IDENTIFIER_LENGTH (ident) + 1);
      res = *slot;
    }
  return res;
}

/* Map LITERAL to strtab offset.  Does not detect duplicates and
   expects LITERAL to remain live until strtab is written out.  */

unsigned
elf_out::name (const char *literal)
{
  return strtab_write (literal, strlen (literal) + 1);
}

/* Map a DECL's qualified name to strtab offset.  Does not detect
   duplicates.  */

unsigned
elf_out::qualified_name (tree decl, bool is_defn)
{
  gcc_checking_assert (DECL_P (decl) && decl != global_namespace);
  unsigned result = strtab.pos;

  strtab_write (decl, is_defn);
  strtab_write ("", 1);

  return result;
}

/* Add section to file.  Return section number.  TYPE & NAME identify
   the section.  OFF and SIZE identify the file location of its
   data.  FLAGS contains additional info.  */

unsigned
elf_out::add (unsigned type, unsigned name, unsigned off, unsigned size,
	      unsigned flags)
{
  gcc_checking_assert (!(off & (SECTION_ALIGN - 1)));
  if (sectab.pos + sizeof (section) > sectab.size)
    data::simple_memory.grow (sectab, sectab.pos + sizeof (section), false);
  section *sec = reinterpret_cast<section *> (sectab.buffer + sectab.pos);
  memset (sec, 0, sizeof (section));
  sec->type = type;
  sec->flags = flags;
  sec->name = name;
  sec->offset = off;
  sec->size = size;
  if (flags & SHF_STRINGS)
    sec->entsize = 1;

  unsigned res = sectab.pos;
  sectab.pos += sizeof (section);
  return res / sizeof (section);
}

/* Pad to the next alignment boundary, then write BUFFER to disk.
   Return the position of the start of the write, or zero on failure.   */

unsigned
elf_out::write (const data &buffer)
{
#if MAPPED_WRITING
  /* HDR is always mapped.  */
  if (&buffer != &hdr)
    {
      bytes_out out (this);
      grow (out, buffer.pos, true);
      if (out.buffer)
	memcpy (out.buffer, buffer.buffer, buffer.pos);
      shrink (out);
    }
  else
    /* We should have been aligned during the first allocation.  */
    gcc_checking_assert (!(pos & (SECTION_ALIGN - 1)));
#else
  if (::write (fd, buffer.buffer, buffer.pos) != ssize_t (buffer.pos))
    {
      set_error (errno);
      return 0;
    }
#endif
  unsigned res = pos;
  pos += buffer.pos;

  if (unsigned padding = -pos & (SECTION_ALIGN - 1))
    {
#if !MAPPED_WRITING
      /* Align the section on disk, should help the necessary copies.
	 fseeking to extend is non-portable.  */
      static char zero[SECTION_ALIGN];
      if (::write (fd, &zero, padding) != ssize_t (padding))
	set_error (errno);
#endif
      pos += padding;
    }
  return res;
}

/* Write a streaming buffer.  It must be using us as an allocator.  */

#if MAPPED_WRITING
unsigned
elf_out::write (const bytes_out &buf)
{
  gcc_checking_assert (buf.memory == this);
  /* A directly mapped buffer.  */
  gcc_checking_assert (buf.buffer - hdr.buffer >= 0
		       && buf.buffer - hdr.buffer + buf.size <= extent);
  unsigned res = pos;
  pos += buf.pos;

  /* Align up.  We're not going to advance into the next page. */
  pos += -pos & (SECTION_ALIGN - 1);

  return res;
}
#endif

/* Write data and add section.  STRING_P is true for a string
   section, false for PROGBITS.  NAME identifies the section (0 is the
   empty name).  DATA is the contents.  Return section number or 0 on
   failure (0 is the undef section).  */

unsigned
elf_out::add (const bytes_out &data, bool string_p, unsigned name)
{
  unsigned off = write (data);

  return add (string_p ? SHT_STRTAB : SHT_PROGBITS, name,
	      off, data.pos, string_p ? SHF_STRINGS : SHF_NONE);
}

/* Begin writing the file.  Initialize the section table and write an
   empty header.  Return false on failure.  */

bool
elf_out::begin ()
{
  if (!parent::begin ())
    return false;

  /* Let the allocators pick a default.  */
  data::simple_memory.grow (strtab, 0, false);
  data::simple_memory.grow (sectab, 0, false);

  /* The string table starts with an empty string.  */
  name ("");

  /* Create the UNDEF section.  */
  add (SHT_NONE);

#if MAPPED_WRITING
  /* Start a mapping.  */
  create_mapping (EXPERIMENT (page_size,
			      (32767 + page_size) & ~(page_size - 1)));
  if (!hdr.buffer)
    return false;
#endif

  /* Write an empty header.  */
  grow (hdr, sizeof (header), true);
  header *h = reinterpret_cast<header *> (hdr.buffer);
  memset (h, 0, sizeof (header));
  hdr.pos = hdr.size;
  write (hdr);
  return !get_error ();
}

/* Finish writing the file.  Write out the string & section tables.
   Fill in the header.  Return true on error.  */

bool
elf_out::end ()
{
  if (fd >= 0)
    {
      /* Write the string table.  */
      unsigned strnam = name (".strtab");
      unsigned stroff = write (strtab);
      unsigned strndx = add (SHT_STRTAB, strnam, stroff, strtab.pos,
			     SHF_STRINGS);

      /* Store escape values in section[0].  */
      if (strndx >= SHN_LORESERVE)
	{
	  reinterpret_cast<section *> (sectab.buffer)->link = strndx;
	  strndx = SHN_XINDEX;
	}
      unsigned shnum = sectab.pos / sizeof (section);
      if (shnum >= SHN_LORESERVE)
	{
	  reinterpret_cast<section *> (sectab.buffer)->size = shnum;
	  shnum = SHN_XINDEX;
	}

      unsigned shoff = write (sectab);

#if MAPPED_WRITING
      if (offset)
	{
	  remove_mapping ();
	  offset = 0;
	  create_mapping ((sizeof (header) + page_size - 1) & ~(page_size - 1),
			  false);
	}
      unsigned length = pos;
#else
      if (lseek (fd, 0, SEEK_SET) < 0)
	set_error (errno);
#endif
      /* Write header.  */
      if (!get_error ())
	{
	  /* Write the correct header now.  */
	  header *h = reinterpret_cast<header *> (hdr.buffer);
	  h->ident.magic[0] = 0x7f;
	  h->ident.magic[1] = 'E';	/* Elrond */
	  h->ident.magic[2] = 'L';	/* is an */
	  h->ident.magic[3] = 'F';	/* elf.  */
	  h->ident.klass = MY_CLASS;
	  h->ident.data =  MY_ENDIAN;
	  h->ident.version = EV_CURRENT;
	  h->ident.osabi = OSABI_NONE;
	  h->type = ET_NONE;
	  h->machine = EM_NONE;
	  h->version = EV_CURRENT;
	  h->shoff = shoff;
	  h->ehsize = sizeof (header);
	  h->shentsize = sizeof (section);
	  h->shnum = shnum;
	  h->shstrndx = strndx;

	  pos = 0;
	  write (hdr);
	}

#if MAPPED_WRITING
      remove_mapping ();
      if (ftruncate (fd, length))
	set_error (errno);
#endif
    }

  data::simple_memory.shrink (sectab);
  data::simple_memory.shrink (strtab);

  return parent::end ();
}

/********************************************************************/

/* A dependency set.  This is used during stream out to determine the
   connectivity of the graph.  Every namespace-scope declaration that
   needs writing has a depset.  The depset is filled with the (depsets
   of) declarations within this module that it references.  For a
   declaration that'll generally be named types.  For definitions
   it'll also be declarations in the body.

   From that we can convert the graph to a DAG, via determining the
   Strongly Connected Clusters.  Each cluster is streamed
   independently, and thus we achieve lazy loading.

   Other decls that get a depset are namespaces themselves and
   unnameable declarations.   */

class depset {
private:
  tree entity;  /* Entity, or containing namespace.  */
  uintptr_t discriminator;  /* Flags or identifier.  */

public:
  /* The kinds of entity the depset could describe.  The ordering is
     significant, see entity_kind_name.  */
  enum entity_kind
  {
    EK_DECL,		/* A decl.  */
    EK_SPECIALIZATION,  /* A specialization.  */
    EK_PARTIAL,		/* A partial specialization.  */
    EK_USING,		/* A using declaration (at namespace scope).  */
    EK_NAMESPACE,	/* A namespace.  */
    EK_REDIRECT,	/* Redirect to a template_decl.  */
    EK_EXPLICIT_HWM,
    EK_BINDING = EK_EXPLICIT_HWM, /* Implicitly encoded.  */
    EK_FOR_BINDING,	/* A decl being inserted for a binding.  */
    EK_INNER_DECL,	/* A decl defined outside of its imported
			   context.  */
    EK_DIRECT_HWM = EK_PARTIAL + 1,

    EK_BITS = 3		/* Only need to encode below EK_EXPLICIT_HWM.  */
  };

private:
  /* Placement of bit fields in discriminator.  */
  enum disc_bits
  {
    DB_ZERO_BIT, /* Set to disambiguate identifier from flags  */
    DB_SPECIAL_BIT, /* First dep slot is special.  */
    DB_KIND_BIT, /* Kind of the entity.  */
    DB_KIND_BITS = EK_BITS,
    DB_DEFN_BIT = DB_KIND_BIT + DB_KIND_BITS,
    DB_IS_PENDING_BIT,		/* Is a maybe-pending entity.  */
    DB_IS_INTERNAL_BIT,		/* It is an (erroneous)
				   internal-linkage entity.  */
    DB_REFS_INTERNAL_BIT,	/* Refers to an internal-linkage
				   entity. */
    DB_IMPORTED_BIT,		/* An imported entity.  */
    DB_UNREACHED_BIT,		/* A yet-to-be reached entity.  */
    DB_MAYBE_RECURSIVE_BIT,	/* An entity maybe in a recursive cluster.  */
    DB_ENTRY_BIT,		/* The first reached recursive dep.  */
    DB_HIDDEN_BIT,		/* A hidden binding.  */
    /* The following bits are not independent, but enumerating them is
       awkward.  */
    DB_TYPE_SPEC_BIT,		/* Specialization in the type table.  */
    DB_FRIEND_SPEC_BIT,		/* An instantiated template friend.  */
  };

public:
  /* The first slot is special for EK_SPECIALIZATIONS it is a
     spec_entry pointer.  It is not relevant for the SCC
     determination.  */
  vec<depset *> deps;  /* Depsets we reference.  */

public:
  unsigned cluster; /* Strongly connected cluster, later entity number  */
  unsigned section; /* Section written to.  */
  /* During SCC construction, section is lowlink, until the depset is
     removed from the stack.  See Tarjan algorithm for details.  */

private:
  /* Construction via factories.  Destruction via hash traits.  */
  depset (tree entity);
  ~depset ();

public:
  static depset *make_binding (tree, tree);
  static depset *make_entity (tree, entity_kind, bool = false);
  /* Late setting a binding name -- /then/ insert into hash!  */
  inline void set_binding_name (tree name)
  {
    gcc_checking_assert (!get_name ());
    discriminator = reinterpret_cast<uintptr_t> (name);
  }

private:
  template<unsigned I> void set_flag_bit ()
  {
    gcc_checking_assert (I < 2 || !is_binding ());
    discriminator |= 1u << I;
  }
  template<unsigned I> void clear_flag_bit ()
  {
    gcc_checking_assert (I < 2 || !is_binding ());
    discriminator &= ~(1u << I);
  }
  template<unsigned I> bool get_flag_bit () const
  {
    gcc_checking_assert (I < 2 || !is_binding ());
    return bool ((discriminator >> I) & 1);
  }

public:
  bool is_binding () const
  {
    return !get_flag_bit<DB_ZERO_BIT> ();
  }
  entity_kind get_entity_kind () const
  {
    if (is_binding ())
      return EK_BINDING;
    return entity_kind ((discriminator >> DB_KIND_BIT) & ((1u << EK_BITS) - 1));
  }
  const char *entity_kind_name () const;

public:
  bool has_defn () const
  {
    return get_flag_bit<DB_DEFN_BIT> ();
  }

public:
  /* This entity might be found other than by namespace-scope lookup;
     see module_state::write_pendings for more details.  */
  bool is_pending_entity () const
  {
    return (get_entity_kind () == EK_SPECIALIZATION
	    || get_entity_kind () == EK_PARTIAL
	    || (get_entity_kind () == EK_DECL
		&& get_flag_bit<DB_IS_PENDING_BIT> ()));
  }
public:
  bool is_internal () const
  {
    return get_flag_bit<DB_IS_INTERNAL_BIT> ();
  }
  bool refs_internal () const
  {
    return get_flag_bit<DB_REFS_INTERNAL_BIT> ();
  }
  bool is_import () const
  {
    return get_flag_bit<DB_IMPORTED_BIT> ();
  }
  bool is_unreached () const
  {
    return get_flag_bit<DB_UNREACHED_BIT> ();
  }
  bool is_hidden () const
  {
    return get_flag_bit<DB_HIDDEN_BIT> ();
  }
  bool is_maybe_recursive () const
  {
    return get_flag_bit<DB_MAYBE_RECURSIVE_BIT> ();
  }
  bool is_entry () const
  {
    return get_flag_bit<DB_ENTRY_BIT> ();
  }
  bool is_type_spec () const
  {
    return get_flag_bit<DB_TYPE_SPEC_BIT> ();
  }
  bool is_friend_spec () const
  {
    return get_flag_bit<DB_FRIEND_SPEC_BIT> ();
  }

public:
  /* We set these bit outside of depset.  */
  void set_hidden_binding ()
  {
    set_flag_bit<DB_HIDDEN_BIT> ();
  }
  void clear_hidden_binding ()
  {
    clear_flag_bit<DB_HIDDEN_BIT> ();
  }

public:
  bool is_special () const
  {
    return get_flag_bit<DB_SPECIAL_BIT> ();
  }
  void set_special ()
  {
    set_flag_bit<DB_SPECIAL_BIT> ();
  }

public:
  tree get_entity () const
  {
    return entity;
  }
  tree get_name () const
  {
    gcc_checking_assert (is_binding ());
    return reinterpret_cast <tree> (discriminator);
  }

public:
  /* Traits for a hash table of pointers to bindings.  */
  struct traits {
    /* Each entry is a pointer to a depset. */
    typedef depset *value_type;
    /* We lookup by container:maybe-identifier pair.  */
    typedef std::pair<tree,tree> compare_type;

    static const bool empty_zero_p = true;

    /* hash and equality for compare_type.  */
    inline static hashval_t hash (const compare_type &p)
    {
      hashval_t h = pointer_hash<tree_node>::hash (p.first);
      if (p.second)
	{
	  hashval_t nh = IDENTIFIER_HASH_VALUE (p.second);
	  h = iterative_hash_hashval_t (h, nh);
	}
      return h;
    }
    inline static bool equal (const value_type b, const compare_type &p)
    {
      if (b->entity != p.first)
	return false;

      if (p.second)
	return b->discriminator == reinterpret_cast<uintptr_t> (p.second);
      else
	return !b->is_binding ();
    }

    /* (re)hasher for a binding itself.  */
    inline static hashval_t hash (const value_type b)
    {
      hashval_t h = pointer_hash<tree_node>::hash (b->entity);
      if (b->is_binding ())
	{
	  hashval_t nh = IDENTIFIER_HASH_VALUE (b->get_name ());
	  h = iterative_hash_hashval_t (h, nh);
	}
      return h;
    }

    /* Empty via NULL.  */
    static inline void mark_empty (value_type &p) {p = NULL;}
    static inline bool is_empty (value_type p) {return !p;}

    /* Nothing is deletable.  Everything is insertable.  */
    static bool is_deleted (value_type) { return false; }
    static void mark_deleted (value_type) { gcc_unreachable (); }

    /* We own the entities in the hash table.  */
    static void remove (value_type p)
    {
      delete (p);
    }
  };

public:
  class hash : public hash_table<traits> {
    typedef traits::compare_type key_t;
    typedef hash_table<traits> parent;

  public:
    vec<depset *> worklist;  /* Worklist of decls to walk.  */
    hash *chain;	     /* Original table.  */
    depset *current;         /* Current depset being depended.  */
    unsigned section;	     /* When writing out, the section.  */
    bool reached_unreached;  /* We reached an unreached entity.  */
    bool writing_merge_key;  /* We're writing merge key information.  */

  public:
    hash (size_t size, hash *c = NULL)
      : parent (size), chain (c), current (NULL), section (0),
	reached_unreached (false), writing_merge_key (false)
    {
      worklist.create (size);
    }
    ~hash ()
    {
      worklist.release ();
    }

  public:
    bool is_key_order () const
    {
      return chain != NULL;
    }

  private:
    depset **entity_slot (tree entity, bool = true);
    depset **binding_slot (tree ctx, tree name, bool = true);
    depset *maybe_add_declaration (tree decl);

  public:
    depset *find_dependency (tree entity);
    depset *find_binding (tree ctx, tree name);
    depset *make_dependency (tree decl, entity_kind);
    void add_dependency (depset *);

  public:
    void add_mergeable (depset *);
    depset *add_dependency (tree decl, entity_kind);
    void add_namespace_context (depset *, tree ns);

  private:
    static bool add_binding_entity (tree, WMB_Flags, void *);

  public:
    bool add_namespace_entities (tree ns, bitmap partitions);
    void add_specializations (bool decl_p);
    void add_partial_entities (vec<tree, va_gc> *);
    void add_class_entities (vec<tree, va_gc> *);

  private:
    void add_deduction_guides (tree decl);

  public:
    void find_dependencies (module_state *);
    bool finalize_dependencies ();
    vec<depset *> connect ();
  };

public:
  struct tarjan {
    vec<depset *> result;
    vec<depset *> stack;
    unsigned index;

    tarjan (unsigned size)
      : index (0)
    {
      result.create (size);
      stack.create (50);
    }
    ~tarjan ()
    {
      gcc_assert (!stack.length ());
      stack.release ();
    }

  public:
    void connect (depset *);
  };
};

inline
depset::depset (tree entity)
  :entity (entity), discriminator (0), cluster (0), section (0)
{
  deps.create (0);
}

inline
depset::~depset ()
{
  deps.release ();
}

const char *
depset::entity_kind_name () const
{
  /* Same order as entity_kind.  */
  static const char *const names[] =
    {"decl", "specialization", "partial", "using",
     "namespace", "redirect", "binding"};
  entity_kind kind = get_entity_kind ();
  gcc_checking_assert (kind < ARRAY_SIZE (names));
  return names[kind];
}

/* Create a depset for a namespace binding NS::NAME.  */

depset *depset::make_binding (tree ns, tree name)
{
  depset *binding = new depset (ns);

  binding->discriminator = reinterpret_cast <uintptr_t> (name);

  return binding;
}

depset *depset::make_entity (tree entity, entity_kind ek, bool is_defn)
{
  depset *r = new depset (entity);

  r->discriminator = ((1 << DB_ZERO_BIT)
		      | (ek << DB_KIND_BIT)
		      | is_defn << DB_DEFN_BIT);

  return r;
}

class pending_key
{
public:
  tree ns;
  tree id;
};

template<>
struct default_hash_traits<pending_key>
{
  using value_type = pending_key;

  static const bool empty_zero_p = false;
  static hashval_t hash (const value_type &k)
  {
    hashval_t h = IDENTIFIER_HASH_VALUE (k.id);
    h = iterative_hash_hashval_t (DECL_UID (k.ns), h);

    return h;
  }
  static bool equal (const value_type &k, const value_type &l)
  {
    return k.ns == l.ns && k.id == l.id;
  }
  static void mark_empty (value_type &k)
  {
    k.ns = k.id = NULL_TREE;
  }
  static void mark_deleted (value_type &k)
  {
    k.ns = NULL_TREE;
    gcc_checking_assert (k.id);
  }
  static bool is_empty (const value_type &k)
  {
    return k.ns == NULL_TREE && k.id == NULL_TREE;
  }
  static bool is_deleted (const value_type &k)
  {
    return k.ns == NULL_TREE && k.id != NULL_TREE;
  }
  static void remove (value_type &)
  {
  }
};

typedef hash_map<pending_key, auto_vec<unsigned>> pending_map_t;

/* Not-loaded entities that are keyed to a namespace-scope
   identifier.  See module_state::write_pendings for details.  */
pending_map_t *pending_table;

/* Decls that need some post processing once a batch of lazy loads has
   completed.  */
vec<tree, va_heap, vl_embed> *post_load_decls;

/* Some entities are keyed to another entitity for ODR purposes.
   For example, at namespace scope, 'inline auto var = []{};', that
   lambda is keyed to 'var', and follows its ODRness.  */
typedef hash_map<tree, auto_vec<tree>> keyed_map_t;
static keyed_map_t *keyed_table;

/* Instantiations of temploid friends imported from another module
   need to be attached to the same module as the temploid.  This maps
   these decls to the temploid they are instantiated them, as there is
   no other easy way to get this information.  */
static GTY((cache)) decl_tree_cache_map *imported_temploid_friends;

/********************************************************************/
/* Tree streaming.   The tree streaming is very specific to the tree
   structures themselves.  A tag indicates the kind of tree being
   streamed.  -ve tags indicate backreferences to already-streamed
   trees.  Backreferences are auto-numbered.  */

/* Tree tags.  */
enum tree_tag {
  tt_null,		/* NULL_TREE.  */
  tt_fixed,		/* Fixed vector index.  */

  tt_node,		/* By-value node.  */
  tt_decl,		/* By-value mergeable decl.  */
  tt_tpl_parm,		/* Template parm.  */

  /* The ordering of the following 4 is relied upon in
     trees_out::tree_node.  */
  tt_id,  		/* Identifier node.  */
  tt_conv_id,		/* Conversion operator name.  */
  tt_anon_id,		/* Anonymous name.  */
  tt_lambda_id,		/* Lambda name.  */

  tt_typedef_type,	/* A (possibly implicit) typedefed type.  */
  tt_derived_type,	/* A type derived from another type.  */
  tt_variant_type,	/* A variant of another type.  */

  tt_tinfo_var,		/* Typeinfo object. */
  tt_tinfo_typedef,	/* Typeinfo typedef.  */
  tt_ptrmem_type,	/* Pointer to member type.  */
  tt_nttp_var,		/* NTTP_OBJECT VAR_DECL.  */

  tt_parm,		/* Function parameter or result.  */
  tt_enum_value,	/* An enum value.  */
  tt_enum_decl,		/* An enum decl.  */
  tt_data_member,	/* Data member/using-decl.  */

  tt_binfo,		/* A BINFO.  */
  tt_vtable,		/* A vtable.  */
  tt_thunk,		/* A thunk.  */
  tt_clone_ref,

  tt_entity,		/* A extra-cluster entity.  */

  tt_template,		/* The TEMPLATE_RESULT of a template.  */
};

enum walk_kind {
  WK_none,	/* No walk to do (a back- or fixed-ref happened).  */
  WK_normal,	/* Normal walk (by-name if possible).  */

  WK_value,	/* By-value walk.  */
};

enum merge_kind
{
  MK_unique,	/* Known unique.  */
  MK_named,	/* Found by CTX, NAME + maybe_arg types etc.  */
  MK_field,	/* Found by CTX and index on TYPE_FIELDS  */
  MK_vtable,	/* Found by CTX and index on TYPE_VTABLES  */
  MK_as_base,	/* Found by CTX.  */

  MK_partial,

  MK_enum,	/* Found by CTX, & 1stMemberNAME.  */
  MK_keyed,     /* Found by key & index.  */
  MK_local_type, /* Found by CTX, index.  */

  MK_friend_spec,  /* Like named, but has a tmpl & args too.  */
  MK_local_friend, /* Found by CTX, index.  */

  MK_indirect_lwm = MK_enum,

  /* Template specialization kinds below. These are all found via
     primary template and specialization args.  */
  MK_template_mask = 0x10,  /* A template specialization.  */

  MK_tmpl_decl_mask = 0x4, /* In decl table.  */

  MK_tmpl_tmpl_mask = 0x1, /* We want TEMPLATE_DECL.  */

  MK_type_spec = MK_template_mask,
  MK_decl_spec = MK_template_mask | MK_tmpl_decl_mask,

  MK_hwm = 0x20
};
/* This is more than a debugging array.  NULLs are used to determine
   an invalid merge_kind number.  */
static char const *const merge_kind_name[MK_hwm] =
  {
    "unique", "named", "field", "vtable",	/* 0...3  */
    "asbase", "partial", "enum", "attached",	/* 4...7  */

    "local type", "friend spec", "local friend", NULL,  /* 8...11 */
    NULL, NULL, NULL, NULL,

    "type spec", "type tmpl spec",	/* 16,17 type (template).  */
    NULL, NULL,

    "decl spec", "decl tmpl spec",	/* 20,21 decl (template).  */
    NULL, NULL,
    NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL,
  };

/* Mergeable entity location data.  */
struct merge_key {
  cp_ref_qualifier ref_q : 2;
  unsigned index;

  tree ret;  /* Return type, if appropriate.  */
  tree args; /* Arg types, if appropriate.  */

  tree constraints;  /* Constraints.  */

  merge_key ()
    :ref_q (REF_QUAL_NONE), index (0),
     ret (NULL_TREE), args (NULL_TREE),
     constraints (NULL_TREE)
  {
  }
};

/* Hashmap of merged duplicates.  Usually decls, but can contain
   BINFOs.  */
typedef hash_map<tree,uintptr_t,
		 simple_hashmap_traits<nodel_ptr_hash<tree_node>,uintptr_t> >
duplicate_hash_map;

/* Data needed for post-processing.  */
struct post_process_data {
  tree decl;
  location_t start_locus;
  location_t end_locus;
};

/* Tree stream reader.  Note that reading a stream doesn't mark the
   read trees with TREE_VISITED.  Thus it's quite safe to have
   multiple concurrent readers.  Which is good, because lazy
   loading.

   It's important that trees_in/out have internal linkage so that the
   compiler knows core_bools, lang_type_bools and lang_decl_bools have
   only a single caller (tree_node_bools) and inlines them appropriately.  */
namespace {
class trees_in : public bytes_in {
  typedef bytes_in parent;

private:
  module_state *state;		/* Module being imported.  */
  vec<tree> back_refs;		/* Back references.  */
  duplicate_hash_map *duplicates;	/* Map from existings to duplicate.  */
  vec<post_process_data> post_decls;	/* Decls to post process.  */
  unsigned unused;		/* Inhibit any interior TREE_USED
				   marking.  */

public:
  trees_in (module_state *);
  ~trees_in ();

public:
  int insert (tree);
  tree back_ref (int);

private:
  tree start (unsigned = 0);

public:
  /* Needed for binfo writing  */
  bool core_bools (tree, bits_in&);

private:
  /* Stream tree_core, lang_decl_specific and lang_type_specific
     bits.  */
  bool core_vals (tree);
  bool lang_type_bools (tree, bits_in&);
  bool lang_type_vals (tree);
  bool lang_decl_bools (tree, bits_in&);
  bool lang_decl_vals (tree);
  bool lang_vals (tree);
  bool tree_node_bools (tree);
  bool tree_node_vals (tree);
  tree tree_value ();
  tree decl_value ();
  tree tpl_parm_value ();

private:
  tree chained_decls ();  /* Follow DECL_CHAIN.  */
  vec<tree, va_heap> *vec_chained_decls ();
  vec<tree, va_gc> *tree_vec (); /* vec of tree.  */
  vec<tree_pair_s, va_gc> *tree_pair_vec (); /* vec of tree_pair.  */
  tree tree_list (bool has_purpose);

public:
  /* Read a tree node.  */
  tree tree_node (bool is_use = false);

private:
  bool install_entity (tree decl);
  tree tpl_parms (unsigned &tpl_levels);
  bool tpl_parms_fini (tree decl, unsigned tpl_levels);
  bool tpl_header (tree decl, unsigned *tpl_levels);
  int fn_parms_init (tree);
  void fn_parms_fini (int tag, tree fn, tree existing, bool has_defn);
  unsigned add_indirect_tpl_parms (tree);
public:
  bool add_indirects (tree);

public:
  /* Serialize various definitions. */
  bool read_definition (tree decl);

private:
  bool is_matching_decl (tree existing, tree decl, bool is_typedef);
  static bool install_implicit_member (tree decl);
  bool read_function_def (tree decl, tree maybe_template);
  bool read_var_def (tree decl, tree maybe_template);
  bool read_class_def (tree decl, tree maybe_template);
  bool read_enum_def (tree decl, tree maybe_template);

public:
  tree decl_container ();
  tree key_mergeable (int tag, merge_kind, tree decl, tree inner, tree type,
		      tree container, bool is_attached,
		      bool is_imported_temploid_friend);
  unsigned binfo_mergeable (tree *);

private:
  tree key_local_type (const merge_key&, tree, tree);
  uintptr_t *find_duplicate (tree existing);
  void register_duplicate (tree decl, tree existing);
  /* Mark as an already diagnosed bad duplicate.  */
  void unmatched_duplicate (tree existing)
  {
    *find_duplicate (existing) |= 1;
  }

public:
  bool is_duplicate (tree decl)
  {
    return find_duplicate (decl) != NULL;
  }
  tree maybe_duplicate (tree decl)
  {
    if (uintptr_t *dup = find_duplicate (decl))
      return reinterpret_cast<tree> (*dup & ~uintptr_t (1));
    return decl;
  }
  tree odr_duplicate (tree decl, bool has_defn);

public:
  /* Return the decls to postprocess.  */
  const vec<post_process_data>& post_process ()
  {
    return post_decls;
  }
private:
  /* Register DATA for postprocessing.  */
  void post_process (post_process_data data)
  {
    post_decls.safe_push (data);
  }

private:
  void assert_definition (tree, bool installing);
};
} // anon namespace

trees_in::trees_in (module_state *state)
  :parent (), state (state), unused (0)
{
  duplicates = NULL;
  back_refs.create (500);
  post_decls.create (0);
}

trees_in::~trees_in ()
{
  delete (duplicates);
  back_refs.release ();
  post_decls.release ();
}

/* Tree stream writer.  */
namespace {
class trees_out : public bytes_out {
  typedef bytes_out parent;

private:
  module_state *state;		/* The module we are writing.  */
  ptr_int_hash_map tree_map; 	/* Trees to references */
  depset::hash *dep_hash;    	/* Dependency table.  */
  int ref_num;			/* Back reference number.  */
  unsigned section;
#if CHECKING_P
  int importedness;		/* Checker that imports not occurring
				   inappropriately.  +ve imports ok,
				   -ve imports not ok.  */
#endif

public:
  trees_out (allocator *, module_state *, depset::hash &deps, unsigned sec = 0);
  ~trees_out ();

private:
  void mark_trees ();
  void unmark_trees ();

public:
  /* Hey, let's ignore the well known STL iterator idiom.  */
  void begin ();
  unsigned end (elf_out *sink, unsigned name, unsigned *crc_ptr);
  void end ();

public:
  enum tags
  {
    tag_backref = -1,	/* Upper bound on the backrefs.  */
    tag_value = 0,	/* Write by value.  */
    tag_fixed		/* Lower bound on the fixed trees.  */
  };

public:
  bool is_key_order () const
  {
    return dep_hash->is_key_order ();
  }

public:
  int insert (tree, walk_kind = WK_normal);

private:
  void start (tree, bool = false);

private:
  walk_kind ref_node (tree);
public:
  int get_tag (tree);
  void set_importing (int i ATTRIBUTE_UNUSED)
  {
#if CHECKING_P
    importedness = i;
#endif
  }

private:
  void core_bools (tree, bits_out&);
  void core_vals (tree);
  void lang_type_bools (tree, bits_out&);
  void lang_type_vals (tree);
  void lang_decl_bools (tree, bits_out&);
  void lang_decl_vals (tree);
  void lang_vals (tree);
  void tree_node_bools (tree);
  void tree_node_vals (tree);

private:
  void chained_decls (tree);
  void vec_chained_decls (tree);
  void tree_vec (vec<tree, va_gc> *);
  void tree_pair_vec (vec<tree_pair_s, va_gc> *);
  void tree_list (tree, bool has_purpose);

public:
  /* Mark a node for by-value walking.  */
  void mark_by_value (tree);

public:
  void tree_node (tree);

private:
  void install_entity (tree decl, depset *);
  void tpl_parms (tree parms, unsigned &tpl_levels);
  void tpl_parms_fini (tree decl, unsigned tpl_levels);
  void fn_parms_fini (tree) {}
  unsigned add_indirect_tpl_parms (tree);
public:
  void add_indirects (tree);
  void fn_parms_init (tree);
  void tpl_header (tree decl, unsigned *tpl_levels);

public:
  merge_kind get_merge_kind (tree decl, depset *maybe_dep);
  tree decl_container (tree decl);
  void key_mergeable (int tag, merge_kind, tree decl, tree inner,
		      tree container, depset *maybe_dep);
  void binfo_mergeable (tree binfo);

private:
  void key_local_type (merge_key&, tree, tree);
  bool decl_node (tree, walk_kind ref);
  void type_node (tree);
  void tree_value (tree);
  void tpl_parm_value (tree);

public:
  void decl_value (tree, depset *);

public:
  /* Serialize various definitions. */
  void write_definition (tree decl);
  void mark_declaration (tree decl, bool do_defn);

private:
  void mark_function_def (tree decl);
  void mark_var_def (tree decl);
  void mark_class_def (tree decl);
  void mark_enum_def (tree decl);
  void mark_class_member (tree decl, bool do_defn = true);
  void mark_binfos (tree type);

private:
  void write_var_def (tree decl);
  void write_function_def (tree decl);
  void write_class_def (tree decl);
  void write_enum_def (tree decl);

private:
  static void assert_definition (tree);

public:
  static void instrument ();

private:
  /* Tree instrumentation. */
  static unsigned tree_val_count;
  static unsigned decl_val_count;
  static unsigned back_ref_count;
  static unsigned null_count;
};
} // anon namespace

/* Instrumentation counters.  */
unsigned trees_out::tree_val_count;
unsigned trees_out::decl_val_count;
unsigned trees_out::back_ref_count;
unsigned trees_out::null_count;

trees_out::trees_out (allocator *mem, module_state *state, depset::hash &deps,
		      unsigned section)
  :parent (mem), state (state), tree_map (500),
   dep_hash (&deps), ref_num (0), section (section)
{
#if CHECKING_P
  importedness = 0;
#endif
}

trees_out::~trees_out ()
{
}

/********************************************************************/
/* Location.  We're aware of the line-map concept and reproduce it
   here.  Each imported module allocates a contiguous span of ordinary
   maps, and of macro maps.  adhoc maps are serialized by contents,
   not pre-allocated.   The scattered linemaps of a module are
   coalesced when writing.  */


/* I use half-open [first,second) ranges.  */
typedef std::pair<line_map_uint_t,line_map_uint_t> range_t;

/* A range of locations.  */
typedef std::pair<location_t,location_t> loc_range_t;

/* Spans of the line maps that are occupied by this TU.  I.e. not
   within imports.  Only extended when in an interface unit.
   Interval zero corresponds to the forced header linemap(s).  This
   is a singleton object.  */

class loc_spans {
public:
  /* An interval of line maps.  The line maps here represent a contiguous
     non-imported range.  */
  struct span {
    loc_range_t ordinary;	/* Ordinary map location range. */
    loc_range_t macro;		/* Macro map location range.  */
    /* Add to locs to get serialized loc.  */
    location_diff_t ordinary_delta;
    location_diff_t macro_delta;
  };

private:
  vec<span> *spans;
  bool locs_exhausted_p;

public:
  loc_spans ()
    /* Do not preallocate spans, as that causes
       --enable-detailed-mem-stats problems.  */
    : spans (nullptr), locs_exhausted_p (false)
  {
  }
  ~loc_spans ()
  {
    delete spans;
  }

public:
  span &operator[] (unsigned ix)
  {
    return (*spans)[ix];
  }
  unsigned length () const
  {
    return spans->length ();
  }

public:
  bool init_p () const
  {
    return spans != nullptr;
  }
  /* Initializer.  */
  void init (const line_maps *lmaps, const line_map_ordinary *map);

  /* Slightly skewed preprocessed files can cause us to miss an
     initialization in some places.  Fallback initializer.  */
  void maybe_init ()
  {
    if (!init_p ())
      init (line_table, nullptr);
  }

public:
  enum {
    SPAN_RESERVED = 0,	/* Reserved (fixed) locations.  */
    SPAN_FIRST = 1,	/* LWM of locations to stream  */
    SPAN_MAIN = 2	/* Main file and onwards.  */
  };

public:
  location_t main_start () const
  {
    return (*spans)[SPAN_MAIN].ordinary.first;
  }

public:
  void open (location_t);
  void close ();

public:
  /* Propagate imported linemaps to us, if needed.  */
  bool maybe_propagate (module_state *import, location_t loc);

public:
  /* Whether we can no longer represent new imported locations.  */
  bool locations_exhausted_p () const
  {
    return locs_exhausted_p;
  }
  void report_location_exhaustion (location_t loc)
  {
    if (!locs_exhausted_p)
      {
	/* Just give the notice once.  */
	locs_exhausted_p = true;
	inform (loc, "unable to represent further imported source locations");
      }
  }

public:
  const span *ordinary (location_t);
  const span *macro (location_t);
};

static loc_spans spans;

/* Information about ordinary locations we stream out.  */
struct ord_loc_info
{
  const line_map_ordinary *src; // line map we're based on
  line_map_uint_t offset;       // offset to this line
  line_map_uint_t span;         // number of locs we span
  line_map_uint_t remap;        // serialization

  static int compare (const void *a_, const void *b_)
  {
    auto *a = static_cast<const ord_loc_info *> (a_);
    auto *b = static_cast<const ord_loc_info *> (b_);

    if (a->src != b->src)
      return a->src < b->src ? -1 : +1;

    // Ensure no overlap
    gcc_checking_assert (a->offset + a->span <= b->offset
			 || b->offset + b->span <= a->offset);

    gcc_checking_assert (a->offset != b->offset);
    return a->offset < b->offset ? -1 : +1;
  }
};
struct ord_loc_traits
{
  typedef ord_loc_info value_type;
  typedef value_type compare_type;

  static const bool empty_zero_p = false;

  static hashval_t hash (const value_type &v)
  {
    auto h = pointer_hash<const line_map_ordinary>::hash (v.src);
    return iterative_hash_hashval_t (v.offset, h);
  }
  static bool equal (const value_type &v, const compare_type p)
  {
    return v.src == p.src && v.offset == p.offset;
  }

  static void mark_empty (value_type &v)
  {
    v.src = nullptr;
  }
  static bool is_empty (value_type &v)
  {
    return !v.src;
  }

  static bool is_deleted (value_type &) { return false; }
  static void mark_deleted (value_type &) { gcc_unreachable (); }

  static void remove (value_type &) {}
};
/* Table keyed by ord_loc_info, used for noting.  */
static  hash_table<ord_loc_traits> *ord_loc_table;
/* Sorted vector, used for writing.  */
static vec<ord_loc_info> *ord_loc_remap;

/* Information about macro locations we stream out.  */
struct macro_loc_info
{
  const line_map_macro *src;    // original expansion
  line_map_uint_t remap;	// serialization

  static int compare (const void *a_, const void *b_)
  {
    auto *a = static_cast<const macro_loc_info *> (a_);
    auto *b = static_cast<const macro_loc_info *> (b_);

    gcc_checking_assert (MAP_START_LOCATION (a->src)
			 != MAP_START_LOCATION (b->src));
    if (MAP_START_LOCATION (a->src) < MAP_START_LOCATION (b->src))
      return -1;
    else
      return +1;
  }
};
struct macro_loc_traits
{
  typedef macro_loc_info value_type;
  typedef const line_map_macro *compare_type;

  static const bool empty_zero_p = false;

  static hashval_t hash (compare_type p)
  {
    return pointer_hash<const line_map_macro>::hash (p);
  }
  static hashval_t hash (const value_type &v)
  {
    return hash (v.src);
  }
  static bool equal (const value_type &v, const compare_type p)
  {
    return v.src == p;
  }

  static void mark_empty (value_type &v)
  {
    v.src = nullptr;
  }
  static bool is_empty (value_type &v)
  {
    return !v.src;
  }

  static bool is_deleted (value_type &) { return false; }
  static void mark_deleted (value_type &) { gcc_unreachable (); }

  static void remove (value_type &) {}
};
/* Table keyed by line_map_macro, used for noting.  */
static  hash_table<macro_loc_traits> *macro_loc_table;
/* Sorted vector, used for writing.  */
static vec<macro_loc_info> *macro_loc_remap;

/* Indirection to allow bsearching imports by ordinary location.  */
static vec<module_state *> *ool;

/********************************************************************/
/* Data needed by a module during the process of loading.  */
struct GTY(()) slurping {

  /* Remap import's module numbering to our numbering.  Values are
     shifted by 1.  Bit0 encodes if the import is direct.  */
  vec<unsigned, va_heap, vl_embed> *
    GTY((skip)) remap;			/* Module owner remapping.  */

  elf_in *GTY((skip)) from;     	/* The elf loader.  */

  /* This map is only for header imports themselves -- the global
     headers bitmap hold it for the current TU.  */
  bitmap headers;	/* Transitive set of direct imports, including
			   self.  Used for macro visibility and
			   priority.  */

  /* These objects point into the mmapped area, unless we're not doing
     that, or we got frozen or closed.  In those cases they point to
     buffers we own.  */
  bytes_in macro_defs;	/* Macro definitions.  */
  bytes_in macro_tbl;	/* Macro table.  */

  /* Location remapping.  first->ordinary, second->macro.  */
  range_t GTY((skip)) loc_deltas;

  unsigned current;	/* Section currently being loaded.  */
  unsigned remaining;	/* Number of lazy sections yet to read.  */
  unsigned lru;		/* An LRU counter.  */

 public:
  slurping (elf_in *);
  ~slurping ();

 public:
  /* Close the ELF file, if it's open.  */
  void close ()
  {
    if (from)
      {
	from->end ();
	delete from;
	from = NULL;
      }
  }

 public:
  void release_macros ();

 public:
  void alloc_remap (unsigned size)
  {
    gcc_assert (!remap);
    vec_safe_reserve (remap, size);
    for (unsigned ix = size; ix--;)
      remap->quick_push (0);
  }
  unsigned remap_module (unsigned owner)
  {
    if (owner < remap->length ())
      return (*remap)[owner] >> 1;
    return 0;
  }

 public:
  /* GC allocation.  But we must explicitly delete it.   */
  static void *operator new (size_t x)
  {
    return ggc_alloc_atomic (x);
  }
  static void operator delete (void *p)
  {
    ggc_free (p);
  }
};

slurping::slurping (elf_in *from)
  : remap (NULL), from (from),
    headers (BITMAP_GGC_ALLOC ()), macro_defs (), macro_tbl (),
    loc_deltas (0, 0),
    current (~0u), remaining (0), lru (0)
{
}

slurping::~slurping ()
{
  vec_free (remap);
  remap = NULL;
  release_macros ();
  close ();
}

void slurping::release_macros ()
{
  if (macro_defs.size)
    elf_in::release (from, macro_defs);
  if (macro_tbl.size)
    elf_in::release (from, macro_tbl);
}

/* Flags for extensions that end up being streamed.  */

enum streamed_extensions {
  SE_OPENMP = 1 << 0,
  SE_BITS = 1
};

/* Counter indices.  */
enum module_state_counts
{
  MSC_sec_lwm,
  MSC_sec_hwm,
  MSC_pendings,
  MSC_entities,
  MSC_namespaces,
  MSC_bindings,
  MSC_macros,
  MSC_inits,
  MSC_HWM
};

/********************************************************************/
struct module_state_config;

/* Increasing levels of loadedness.  */
enum module_loadedness {
  ML_NONE,		/* Not loaded.  */
  ML_CONFIG,		/* Config loaed.  */
  ML_PREPROCESSOR,	/* Preprocessor loaded.  */
  ML_LANGUAGE,		/* Language loaded.  */
};

/* Increasing levels of directness (toplevel) of import.  */
enum module_directness {
  MD_NONE,  		/* Not direct.  */
  MD_PARTITION_DIRECT,	/* Direct import of a partition.  */
  MD_DIRECT,		/* Direct import.  */
  MD_PURVIEW_DIRECT,	/* direct import in purview.  */
};

/* State of a particular module. */

class GTY((chain_next ("%h.parent"), for_user)) module_state {
 public:
  /* We always import & export ourselves.  */
  bitmap imports;	/* Transitive modules we're importing.  */
  bitmap exports;	/* Subset of that, that we're exporting.  */

  module_state *parent;
  tree name;		/* Name of the module.  */

  slurping *slurp;	/* Data for loading.  */

  const char *flatname;	/* Flatname of module.  */
  char *filename;	/* CMI Filename */

  /* Indices into the entity_ary.  */
  unsigned entity_lwm;
  unsigned entity_num;

  /* Location ranges for this module.  adhoc-locs are decomposed, so
     don't have a range.  */
  loc_range_t GTY((skip)) ordinary_locs;
  loc_range_t GTY((skip)) macro_locs; // [lwm,num)

  /* LOC is first set too the importing location.  When initially
     loaded it refers to a module loc whose parent is the importing
     location.  */
  location_t loc; 	/* Location referring to module itself.  */
  unsigned crc;		/* CRC we saw reading it in. */

  unsigned mod;		/* Module owner number.  */
  unsigned remap;	/* Remapping during writing.  */

  unsigned short subst;	/* Mangle subst if !0.  */

  /* How loaded this module is.  */
  enum module_loadedness loadedness : 2;

  bool module_p : 1;    /* /The/ module of this TU.  */
  bool header_p : 1;	/* Is a header unit.  */
  bool interface_p : 1; /* An interface.  */
  bool partition_p : 1; /* A partition.  */

  /* How directly this module is imported.  */
  enum module_directness directness : 2;

  bool exported_p : 1;	/* directness != MD_NONE && exported.  */
  bool cmi_noted_p : 1; /* We've told the user about the CMI, don't
			   do it again  */
  bool active_init_p : 1; /* This module's global initializer needs
			   calling.  */
  bool inform_cmi_p : 1; /* Inform of a read/write.  */
  bool visited_p : 1;    /* A walk-once flag. */
  /* Record extensions emitted or permitted.  */
  unsigned extensions : SE_BITS;
  /* 14 bits used, 2 bits remain  */

 public:
  module_state (tree name, module_state *, bool);
  ~module_state ();

 public:
  void release ()
  {
    imports = exports = NULL;
    slurped ();
  }
  void slurped ()
  {
    delete slurp;
    slurp = NULL;
  }
  elf_in *from () const
  {
    return slurp->from;
  }

 public:
  /* Kind of this module.  */
  bool is_module () const
  {
    return module_p;
  }
  bool is_header () const
  {
    return header_p;
  }
  bool is_interface () const
  {
    return interface_p;
  }
  bool is_partition () const
  {
    return partition_p;
  }

  /* How this module is used in the current TU.  */
  bool is_exported () const
  {
    return exported_p;
  }
  bool is_direct () const
  {
    return directness >= MD_DIRECT;
  }
  bool is_purview_direct () const
  {
    return directness == MD_PURVIEW_DIRECT;
  }
  bool is_partition_direct () const
  {
    return directness == MD_PARTITION_DIRECT;
  }

 public:
  /* Is this a real module?  */
  bool has_location () const
  {
    return loc != UNKNOWN_LOCATION;
  }

 public:
  bool check_not_purview (location_t loc);

 public:
  void mangle (bool include_partition);

 public:
  void set_import (module_state const *, bool is_export);
  void announce (const char *) const;

 public:
  /* Read and write module.  */
  bool write_begin (elf_out *to, cpp_reader *,
		    module_state_config &, unsigned &crc);
  void write_end (elf_out *to, cpp_reader *,
		  module_state_config &, unsigned &crc);
  bool read_initial (cpp_reader *);
  bool read_preprocessor (bool);
  bool read_language (bool);

 public:
  /* Read a section.  */
  bool load_section (unsigned snum, binding_slot *mslot);
  /* Lazily read a section.  */
  bool lazy_load (unsigned index, binding_slot *mslot);

 public:
  /* Juggle a limited number of file numbers.  */
  static void freeze_an_elf ();
  bool maybe_defrost ();

 public:
  void maybe_completed_reading ();
  bool check_read (bool outermost, bool ok);

 private:
  /* The README, for human consumption.  */
  void write_readme (elf_out *to, cpp_reader *, const char *dialect);
  void write_env (elf_out *to);

 private:
  /* Import tables. */
  void write_imports (bytes_out &cfg, bool direct);
  unsigned read_imports (bytes_in &cfg, cpp_reader *, line_maps *maps);

 private:
  void write_imports (elf_out *to, unsigned *crc_ptr);
  bool read_imports (cpp_reader *, line_maps *);

 private:
  void write_partitions (elf_out *to, unsigned, unsigned *crc_ptr);
  bool read_partitions (unsigned);

 private:
  void write_config (elf_out *to, struct module_state_config &, unsigned crc);
  bool read_config (struct module_state_config &);
  static void write_counts (elf_out *to, unsigned [MSC_HWM], unsigned *crc_ptr);
  bool read_counts (unsigned *);

 public:
  void note_cmi_name ();

 private:
  static unsigned write_bindings (elf_out *to, vec<depset *> depsets,
				  unsigned *crc_ptr);
  bool read_bindings (unsigned count, unsigned lwm, unsigned hwm);

  static void write_namespace (bytes_out &sec, depset *ns_dep);
  tree read_namespace (bytes_in &sec);

  void write_namespaces (elf_out *to, vec<depset *> spaces,
			 unsigned, unsigned *crc_ptr);
  bool read_namespaces (unsigned);

  void intercluster_seed (trees_out &sec, unsigned index, depset *dep);
  unsigned write_cluster (elf_out *to, depset *depsets[], unsigned size,
			  depset::hash &, unsigned *counts, unsigned *crc_ptr);
  bool read_cluster (unsigned snum);

 private:
  unsigned write_inits (elf_out *to, depset::hash &, unsigned *crc_ptr);
  bool read_inits (unsigned count);

 private:
  unsigned write_pendings (elf_out *to, vec<depset *> depsets,
			   depset::hash &, unsigned *crc_ptr);
  bool read_pendings (unsigned count);

 private:
  void write_entities (elf_out *to, vec<depset *> depsets,
		       unsigned count, unsigned *crc_ptr);
  bool read_entities (unsigned count, unsigned lwm, unsigned hwm);

 private:
  void write_init_maps ();
  range_t write_prepare_maps (module_state_config *, bool);
  bool read_prepare_maps (const module_state_config *);

  void write_ordinary_maps (elf_out *to, range_t &,
			    bool, unsigned *crc_ptr);
  bool read_ordinary_maps (unsigned, unsigned);
  void write_macro_maps (elf_out *to, range_t &, unsigned *crc_ptr);
  bool read_macro_maps (line_map_uint_t);

 private:
  void write_define (bytes_out &, const cpp_macro *);
  cpp_macro *read_define (bytes_in &, cpp_reader *) const;
  vec<cpp_hashnode *> *prepare_macros (cpp_reader *);
  unsigned write_macros (elf_out *to, vec<cpp_hashnode *> *, unsigned *crc_ptr);
  bool read_macros ();
  void install_macros ();

 public:
  void import_macros ();

 public:
  static void undef_macro (cpp_reader *, location_t, cpp_hashnode *);
  static cpp_macro *deferred_macro (cpp_reader *, location_t, cpp_hashnode *);

 public:
  static bool note_location (location_t);
  static void write_location (bytes_out &, location_t);
  location_t read_location (bytes_in &) const;

 public:
  void set_flatname ();
  const char *get_flatname () const
  {
    return flatname;
  }
  location_t imported_from () const;

 public:
  void set_filename (const Cody::Packet &);
  bool do_import (cpp_reader *, bool outermost);
};

/* Hash module state by name.  This cannot be a member of
   module_state, because of GTY restrictions.  We never delete from
   the hash table, but ggc_ptr_hash doesn't support that
   simplification.  */

struct module_state_hash : ggc_ptr_hash<module_state> {
  typedef std::pair<tree,uintptr_t> compare_type; /* {name,parent} */

  static inline hashval_t hash (const value_type m);
  static inline hashval_t hash (const compare_type &n);
  static inline bool equal (const value_type existing,
			    const compare_type &candidate);
};

module_state::module_state (tree name, module_state *parent, bool partition)
  : imports (BITMAP_GGC_ALLOC ()), exports (BITMAP_GGC_ALLOC ()),
    parent (parent), name (name), slurp (NULL),
    flatname (NULL), filename (NULL),
    entity_lwm (~0u >> 1), entity_num (0),
    ordinary_locs (0, 0), macro_locs (0, 0),
    loc (UNKNOWN_LOCATION),
    crc (0), mod (MODULE_UNKNOWN), remap (0), subst (0)
{
  loadedness = ML_NONE;

  module_p = header_p = interface_p = partition_p = false;

  directness = MD_NONE;
  exported_p = false;

  cmi_noted_p = false;
  active_init_p = false;

  partition_p = partition;

  inform_cmi_p = false;
  visited_p = false;

  extensions = 0;
  if (name && TREE_CODE (name) == STRING_CST)
    {
      header_p = true;

      const char *string = TREE_STRING_POINTER (name);
      gcc_checking_assert (string[0] == '.'
			   ? IS_DIR_SEPARATOR (string[1])
			   : IS_ABSOLUTE_PATH (string));
    }

  gcc_checking_assert (!(parent && header_p));
}

module_state::~module_state ()
{
  release ();
}

/* Hash module state.  */
static hashval_t
module_name_hash (const_tree name)
{
  if (TREE_CODE (name) == STRING_CST)
    return htab_hash_string (TREE_STRING_POINTER (name));
  else
    return IDENTIFIER_HASH_VALUE (name);
}

hashval_t
module_state_hash::hash (const value_type m)
{
  hashval_t ph = pointer_hash<void>::hash
    (reinterpret_cast<void *> (reinterpret_cast<uintptr_t> (m->parent)
			       | m->is_partition ()));
  hashval_t nh = module_name_hash (m->name);
  return iterative_hash_hashval_t (ph, nh);
}

/* Hash a name.  */
hashval_t
module_state_hash::hash (const compare_type &c)
{
  hashval_t ph = pointer_hash<void>::hash (reinterpret_cast<void *> (c.second));
  hashval_t nh = module_name_hash (c.first);

  return iterative_hash_hashval_t (ph, nh);
}

bool
module_state_hash::equal (const value_type existing,
			  const compare_type &candidate)
{
  uintptr_t ep = (reinterpret_cast<uintptr_t> (existing->parent)
		  | existing->is_partition ());
  if (ep != candidate.second)
    return false;

  /* Identifier comparison is by pointer.  If the string_csts happen
     to be the same object, then they're equal too.  */
  if (existing->name == candidate.first)
    return true;

  /* If neither are string csts, they can't be equal.  */
  if (TREE_CODE (candidate.first) != STRING_CST
      || TREE_CODE (existing->name) != STRING_CST)
    return false;

  /* String equality.  */
  if (TREE_STRING_LENGTH (existing->name)
      == TREE_STRING_LENGTH (candidate.first)
      && !memcmp (TREE_STRING_POINTER (existing->name),
		  TREE_STRING_POINTER (candidate.first),
		  TREE_STRING_LENGTH (existing->name)))
    return true;

  return false;
}

/********************************************************************/
/* Global state */

/* Mapper name.  */
static const char *module_mapper_name;

/* Deferred import queue (FIFO).  */
static vec<module_state *, va_heap, vl_embed> *pending_imports;

/* CMI repository path and workspace.  */
static char *cmi_repo;
static size_t cmi_repo_length;
static char *cmi_path;
static size_t cmi_path_alloc;

/* Count of available and loaded clusters.  */
static unsigned available_clusters;
static unsigned loaded_clusters;

/* What the current TU is.  */
unsigned module_kind;

/* Global trees.  */
static const std::pair<tree *, unsigned> global_tree_arys[] =
  {
    std::pair<tree *, unsigned> (sizetype_tab, stk_type_kind_last),
    std::pair<tree *, unsigned> (integer_types, itk_none),
    std::pair<tree *, unsigned> (global_trees, TI_MODULE_HWM),
    std::pair<tree *, unsigned> (c_global_trees, CTI_MODULE_HWM),
    std::pair<tree *, unsigned> (cp_global_trees, CPTI_MODULE_HWM),
    std::pair<tree *, unsigned> (NULL, 0)
  };
static GTY(()) vec<tree, va_gc> *fixed_trees;
static unsigned global_crc;

/* Lazy loading can open many files concurrently, there are
   per-process limits on that.  We pay attention to the process limit,
   and attempt to increase it when we run out.  Otherwise we use an
   LRU scheme to figure out who to flush.  Note that if the import
   graph /depth/ exceeds lazy_limit, we'll exceed the limit.  */
static unsigned lazy_lru;  /* LRU counter.  */
static unsigned lazy_open; /* Number of open modules */
static unsigned lazy_limit; /* Current limit of open modules.  */
static unsigned lazy_hard_limit; /* Hard limit on open modules.  */
/* Account for source, assembler and dump files & directory searches.
   We don't keep the source file's open, so we don't have to account
   for #include depth.  I think dump files are opened and closed per
   pass, but ICBW.  */
#define LAZY_HEADROOM 15 /* File descriptor headroom.  */

/* Vector of module state.  Indexed by OWNER.  Has at least 2 slots.  */
static GTY(()) vec<module_state *, va_gc> *modules;

/* Hash of module state, findable by {name, parent}. */
static GTY(()) hash_table<module_state_hash> *modules_hash;

/* Map of imported entities.  We map DECL_UID to index of entity
   vector.  */
typedef hash_map<unsigned/*UID*/, unsigned/*index*/,
		 simple_hashmap_traits<int_hash<unsigned,0>, unsigned>
		 > entity_map_t;
static entity_map_t *entity_map;
/* Doesn't need GTYing, because any tree referenced here is also
   findable by, symbol table, specialization table, return type of
   reachable function.  */
static vec<binding_slot, va_heap, vl_embed> *entity_ary;

/* Members entities of imported classes that are defined in this TU.
   These are where the entity's context is not from the current TU.
   We need to emit the definition (but not the enclosing class).

   We could find these by walking ALL the imported classes that we
   could provide a member definition.  But that's expensive,
   especially when you consider lazy implicit member declarations,
   which could be ANY imported class.  */
static GTY(()) vec<tree, va_gc> *class_members;

/* The same problem exists for class template partial
   specializations.  Now that we have constraints, the invariant of
   expecting them in the instantiation table no longer holds.  One of
   the constrained partial specializations will be there, but the
   others not so much.  It's not even an unconstrained partial
   spacialization in the table :(  so any partial template declaration
   is added to this list too.  */
static GTY(()) vec<tree, va_gc> *partial_specializations;

/********************************************************************/

/* Our module mapper (created lazily).  */
module_client *mapper;

static module_client *make_mapper (location_t loc, class mkdeps *deps);
inline module_client *get_mapper (location_t loc, class mkdeps *deps)
{
  auto *res = mapper;
  if (!res)
    res = make_mapper (loc, deps);
  return res;
}

/********************************************************************/
static tree
get_clone_target (tree decl)
{
  tree target;

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      tree res_orig = DECL_CLONED_FUNCTION (DECL_TEMPLATE_RESULT (decl));

      target = DECL_TI_TEMPLATE (res_orig);
    }
  else
    target = DECL_CLONED_FUNCTION (decl);

  gcc_checking_assert (DECL_MAYBE_IN_CHARGE_CDTOR_P (target));

  return target;
}

/* Like FOR_EACH_CLONE, but will walk cloned templates.  */
#define FOR_EVERY_CLONE(CLONE, FN)			\
  if (!DECL_MAYBE_IN_CHARGE_CDTOR_P (FN));		\
  else							\
    for (CLONE = DECL_CHAIN (FN);			\
	 CLONE && DECL_CLONED_FUNCTION_P (CLONE);	\
	 CLONE = DECL_CHAIN (CLONE))

/* It'd be nice if USE_TEMPLATE was a field of template_info
   (a) it'd solve the enum case dealt with below,
   (b) both class templates and decl templates would store this in the
   same place
   (c) this function wouldn't need the by-ref arg, which is annoying.  */

static tree
node_template_info (tree decl, int &use)
{
  tree ti = NULL_TREE;
  int use_tpl = -1;
  if (DECL_IMPLICIT_TYPEDEF_P (decl))
    {
      tree type = TREE_TYPE (decl);

      ti = TYPE_TEMPLATE_INFO (type);
      if (ti)
	{
	  if (TYPE_LANG_SPECIFIC (type))
	    use_tpl = CLASSTYPE_USE_TEMPLATE (type);
	  else
	    {
	      /* An enum, where we don't explicitly encode use_tpl.
		 If the containing context (a type or a function), is
		 an ({im,ex}plicit) instantiation, then this is too.
		 If it's a partial or explicit specialization, then
		 this is not!.  */
	      tree ctx = CP_DECL_CONTEXT (decl);
	      if (TYPE_P (ctx))
		ctx = TYPE_NAME (ctx);
	      node_template_info (ctx, use);
	      use_tpl = use != 2 ? use : 0;
	    }
	}
    }
  else if (DECL_LANG_SPECIFIC (decl)
	   && (VAR_P (decl)
	       || TREE_CODE (decl) == TYPE_DECL
	       || TREE_CODE (decl) == FUNCTION_DECL
	       || TREE_CODE (decl) == FIELD_DECL
	       || TREE_CODE (decl) == CONCEPT_DECL
	       || TREE_CODE (decl) == TEMPLATE_DECL))
    {
      use_tpl = DECL_USE_TEMPLATE (decl);
      ti = DECL_TEMPLATE_INFO (decl);
    }

  use = use_tpl;
  return ti;
}

/* Find the index in entity_ary for an imported DECL.  It should
   always be there, but bugs can cause it to be missing, and that can
   crash the crash reporting -- let's not do that!  When streaming
   out we place entities from this module there too -- with negated
   indices.  */

static unsigned
import_entity_index (tree decl, bool null_ok = false)
{
  if (unsigned *slot = entity_map->get (DECL_UID (decl)))
    return *slot;

  gcc_checking_assert (null_ok);
  return ~(~0u >> 1);
}

/* Find the module for an imported entity at INDEX in the entity ary.
   There must be one.  */

static module_state *
import_entity_module (unsigned index)
{
  if (index > ~(~0u >> 1))
    /* This is an index for an exported entity.  */
    return (*modules)[0];

  /* Do not include the current TU (not an off-by-one error).  */
  unsigned pos = 1;
  unsigned len = modules->length () - pos;
  while (len)
    {
      unsigned half = len / 2;
      module_state *probe = (*modules)[pos + half];
      if (index < probe->entity_lwm)
	len = half;
      else if (index < probe->entity_lwm + probe->entity_num)
	return probe;
      else
	{
	  pos += half + 1;
	  len = len - (half + 1);
	}
    }
  gcc_unreachable ();
}


/********************************************************************/
/* A dumping machinery.  */

class dumper {
public:
  enum {
    LOCATION = TDF_LINENO,  /* -lineno:Source location streaming.  */
    DEPEND = TDF_GRAPH,	/* -graph:Dependency graph construction.  */
    CLUSTER = TDF_BLOCKS,   /* -blocks:Clusters.  */
    TREE = TDF_UID, 	/* -uid:Tree streaming.  */
    MERGE = TDF_ALIAS,	/* -alias:Mergeable Entities.  */
    ELF = TDF_ASMNAME,	/* -asmname:Elf data.  */
    MACRO = TDF_VOPS	/* -vops:Macros.  */
  };

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
  dump_flags_t flags;

public:
  /* Push/pop module state dumping.  */
  unsigned push (module_state *);
  void pop (unsigned);

public:
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

public:
  /* Is dump enabled?.  */
  bool operator () (int mask = 0)
  {
    if (!dumps || !dumps->stream)
      return false;
    if (mask && !(mask & flags))
      return false;
    return true;
  }
  /* Dump some information.  */
  bool operator () (const char *, ...);
};

/* The dumper.  */
static dumper dump = {0, dump_flags_t (0)};

/* Push to dumping M.  Return previous indentation level.  */

unsigned
dumper::push (module_state *m)
{
  FILE *stream = NULL;
  if (!dumps || !dumps->stack.length ())
    {
      stream = dump_begin (module_dump_id, &flags);
      if (!stream)
	return 0;
    }

  if (!dumps || !dumps->stack.space (1))
    {
      /* Create or extend the dump implementor.  */
      unsigned current = dumps ? dumps->stack.length () : 0;
      unsigned count = current ? current * 2 : EXPERIMENT (1, 20);
      size_t alloc = (offsetof (impl, stack)
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
  if (m)
    {
      module_state *from = NULL;

      if (dumps->stack.length () > 1)
	from = dumps->stack[dumps->stack.length () - 2];
      else
	dump ("");
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
}

/* Dump a nested name for arbitrary tree T.  Sometimes it won't have a
   name.  */

bool
dumper::impl::nested_name (tree t)
{
  tree ti = NULL_TREE;
  int origin = -1;
  tree name = NULL_TREE;

  if (t && TREE_CODE (t) == TREE_BINFO)
    t = BINFO_TYPE (t);

  if (t && TYPE_P (t))
    t = TYPE_NAME (t);

  if (t && DECL_P (t))
    {
      if (t == global_namespace || DECL_TEMPLATE_PARM_P (t))
	;
      else if (tree ctx = DECL_CONTEXT (t))
	if (TREE_CODE (ctx) == TRANSLATION_UNIT_DECL
	    || nested_name (ctx))
	  fputs ("::", stream);

      int use_tpl;
      ti = node_template_info (t, use_tpl);
      if (ti && TREE_CODE (TI_TEMPLATE (ti)) == TEMPLATE_DECL
	  && (DECL_TEMPLATE_RESULT (TI_TEMPLATE (ti)) == t))
	t = TI_TEMPLATE (ti);
      tree not_tmpl = t;
      if (TREE_CODE (t) == TEMPLATE_DECL)
	{
	  fputs ("template ", stream);
	  not_tmpl = DECL_TEMPLATE_RESULT (t);
	}

      if (not_tmpl
	  && DECL_P (not_tmpl)
	  && DECL_LANG_SPECIFIC (not_tmpl)
	  && DECL_MODULE_IMPORT_P (not_tmpl))
	{
	  /* We need to be careful here, so as to not explode on
	     inconsistent data -- we're probably debugging, because
	     Something Is Wrong.  */
	  unsigned index = import_entity_index (t, true);
	  if (!(index & ~(~0u >> 1)))
	    origin = import_entity_module (index)->mod;
	  else if (index > ~(~0u >> 1))
	    /* An imported partition member that we're emitting.  */
	    origin = 0;
	  else
	    origin = -2;
	}

      name = DECL_NAME (t) ? DECL_NAME (t)
	: HAS_DECL_ASSEMBLER_NAME_P (t) ? DECL_ASSEMBLER_NAME_RAW (t)
	: NULL_TREE;
    }
  else
    name = t;

  if (name)
    switch (TREE_CODE (name))
      {
      default:
	fputs ("#unnamed#", stream);
	break;

      case IDENTIFIER_NODE:
	fwrite (IDENTIFIER_POINTER (name), 1, IDENTIFIER_LENGTH (name), stream);
	break;

      case INTEGER_CST:
	print_hex (wi::to_wide (name), stream);
	break;

      case STRING_CST:
	/* If TREE_TYPE is NULL, this is a raw string.  */
	fwrite (TREE_STRING_POINTER (name), 1,
		TREE_STRING_LENGTH (name) - (TREE_TYPE (name) != NULL_TREE),
		stream);
	break;
      }
  else
    fputs ("#null#", stream);

  if (origin >= 0)
    {
      const module_state *module = (*modules)[origin];
      fprintf (stream, "@%s:%d", !module ? "" : !module->name ? "(unnamed)"
	       : module->get_flatname (), origin);
    }
  else if (origin == -2)
    fprintf (stream, "@???");

  if (ti)
    {
      tree args = INNERMOST_TEMPLATE_ARGS (TI_ARGS (ti));
      fputs ("<", stream);
      if (args)
	for (int ix = 0; ix != TREE_VEC_LENGTH (args); ix++)
	  {
	    if (ix)
	      fputs (",", stream);
	    nested_name (TREE_VEC_ELT (args, ix));
	  }
      fputs (">", stream);
    }

  return true;
}

/* Formatted dumping.  FORMAT begins with '+' do not emit a trailing
   new line.  (Normally it is appended.)
   Escapes:
      %C - tree_code
      %I - identifier
      %K - location_t or line_map_uint_t
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
      /* Module import indent.  */
      if (unsigned depth = dumps->stack.length () - 1)
	{
	  const char *prefix = ">>>>";
	  fprintf (dumps->stream, (depth <= strlen (prefix)
				   ? &prefix[strlen (prefix) - depth]
				   : ">.%d.>"), depth);
	}

      /* Local indent.  */
      if (unsigned indent = dumps->indent)
	{
	  const char *prefix = "      ";
	  fprintf (dumps->stream, (indent <= strlen (prefix)
				   ? &prefix[strlen (prefix) - indent]
				   : "  .%d.  "), indent);
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
	default:
	  gcc_unreachable ();

	case '%':
	  fputc ('%', dumps->stream);
	  break;

	case 'C': /* Code */
	  {
	    tree_code code = (tree_code)va_arg (args, unsigned);
	    fputs (get_tree_code_name (code), dumps->stream);
	  }
	  break;

	case 'I': /* Identifier.  */
	  {
	    tree t = va_arg (args, tree);
	    dumps->nested_name (t);
	  }
	  break;

	case 'K': /* location_t, either 32- or 64-bit.  */
	  {
	    unsigned long long u = va_arg (args, location_t);
	    fprintf (dumps->stream, "%llu", u);
	  }
	  break;

	case 'M': /* Module. */
	  {
	    const char *str = "(none)";
	    if (module_state *m = va_arg (args, module_state *))
	      {
		if (!m->has_location ())
		  str = "(detached)";
		else
		  str = m->get_flatname ();
	      }
	    fputs (str, dumps->stream);
	  }
	  break;

	case 'N': /* Name.  */
	  {
	    tree t = va_arg (args, tree);
	    while (t && TREE_CODE (t) == OVERLOAD)
	      t = OVL_FUNCTION (t);
	    fputc ('\'', dumps->stream);
	    dumps->nested_name (t);
	    fputc ('\'', dumps->stream);
	  }
	  break;

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
	  }
	  break;

	case 'R': /* Ratio */
	  {
	    unsigned a = va_arg (args, unsigned);
	    unsigned b = va_arg (args, unsigned);
	    fprintf (dumps->stream, "%.1f", (float) a / (b + !b));
	  }
	  break;

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
	  }
	  break;

	case 'U': /* long unsigned.  */
	  {
	    unsigned long u = va_arg (args, unsigned long);
	    fprintf (dumps->stream, "%lu", u);
	  }
	  break;

	case 'V': /* Version.  */
	  {
	    unsigned v = va_arg (args, unsigned);
	    verstr_t string;

	    version2string (v, string);
	    fputs (string, dumps->stream);
	  }
	  break;

	case 'c': /* Character.  */
	  {
	    int c = va_arg (args, int);
	    fputc (c, dumps->stream);
	  }
	  break;

	case 'd': /* Decimal Int.  */
	  {
	    int d = va_arg (args, int);
	    fprintf (dumps->stream, "%d", d);
	  }
	  break;

	case 'p': /* Pointer. */
	  {
	    void *p = va_arg (args, void *);
	    fprintf (dumps->stream, "%p", p);
	  }
	  break;

	case 's': /* String. */
	  {
	    const char *s = va_arg (args, char *);
	    gcc_checking_assert (s);
	    fputs (s, dumps->stream);
	  }
	  break;

	case 'u': /* Unsigned.  */
	  {
	    unsigned u = va_arg (args, unsigned);
	    fprintf (dumps->stream, "%u", u);
	  }
	  break;

	case 'x': /* Hex. */
	  {
	    unsigned x = va_arg (args, unsigned);
	    fprintf (dumps->stream, "%x", x);
	  }
	  break;
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

struct note_def_cache_hasher : ggc_cache_ptr_hash<tree_node>
{
  static int keep_cache_entry (tree t)
  {
    if (!CHECKING_P)
      /* GTY is unfortunately not clever enough to conditionalize
	 this.  */
      gcc_unreachable ();

    if (ggc_marked_p (t))
      return -1;

    unsigned n = dump.push (NULL);
    /* This might or might not be an error.  We should note its
       dropping whichever.  */
    dump () && dump ("Dropping %N from note_defs table", t);
    dump.pop (n);

    return 0;
  }
};

/* We should stream each definition at most once.
   This needs to be a cache because there are cases where a definition
   ends up being not retained, and we need to drop those so we don't
   get confused if memory is reallocated.  */
typedef hash_table<note_def_cache_hasher> note_defs_table_t;
static GTY((cache)) note_defs_table_t *note_defs;

void
trees_in::assert_definition (tree decl ATTRIBUTE_UNUSED,
			     bool installing ATTRIBUTE_UNUSED)
{
#if CHECKING_P
  tree *slot = note_defs->find_slot (decl, installing ? INSERT : NO_INSERT);
  tree not_tmpl = STRIP_TEMPLATE (decl);
  if (installing)
    {
      /* We must be inserting for the first time.  */
      gcc_assert (!*slot);
      *slot = decl;
    }
  else
    /* If this is not the mergeable entity, it should not be in the
       table.  If it is a non-global-module mergeable entity, it
       should be in the table.  Global module entities could have been
       defined textually in the current TU and so might or might not
       be present.  */
    gcc_assert (!is_duplicate (decl)
		? !slot
		: (slot
		   || !DECL_LANG_SPECIFIC (not_tmpl)
		   || !DECL_MODULE_PURVIEW_P (not_tmpl)
		   || (!DECL_MODULE_IMPORT_P (not_tmpl)
		       && header_module_p ())));

  if (not_tmpl != decl)
    gcc_assert (!note_defs->find_slot (not_tmpl, NO_INSERT));
#endif
}

void
trees_out::assert_definition (tree decl ATTRIBUTE_UNUSED)
{
#if CHECKING_P
  tree *slot = note_defs->find_slot (decl, INSERT);
  gcc_assert (!*slot);
  *slot = decl;
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    gcc_assert (!note_defs->find_slot (DECL_TEMPLATE_RESULT (decl), NO_INSERT));
#endif
}

/********************************************************************/
static bool
noisy_p ()
{
  if (quiet_flag)
    return false;

  pp_needs_newline (global_dc->get_reference_printer ()) = true;
  diagnostic_set_last_function (global_dc, (diagnostic_info *) NULL);

  return true;
}

/* Set the cmi repo.  Strip trailing '/', '.' becomes NULL.  */

static void
set_cmi_repo (const char *r)
{
  XDELETEVEC (cmi_repo);
  XDELETEVEC (cmi_path);
  cmi_path_alloc = 0;

  cmi_repo = NULL;
  cmi_repo_length = 0;

  if (!r || !r[0])
    return;

  size_t len = strlen (r);
  cmi_repo = XNEWVEC (char, len + 1);
  memcpy (cmi_repo, r, len + 1);

  if (len > 1 && IS_DIR_SEPARATOR (cmi_repo[len-1]))
    len--;
  if (len == 1 && cmi_repo[0] == '.')
    len--;
  cmi_repo[len] = 0;
  cmi_repo_length = len;
}

/* TO is a repo-relative name.  Provide one that we may use from where
   we are.  */

static const char *
maybe_add_cmi_prefix (const char *to, size_t *len_p = NULL)
{
  size_t len = len_p || cmi_repo_length ? strlen (to) : 0;

  if (cmi_repo_length && !IS_ABSOLUTE_PATH (to))
    {
      if (cmi_path_alloc < cmi_repo_length + len + 2)
	{
	  XDELETEVEC (cmi_path);
	  cmi_path_alloc = cmi_repo_length + len * 2 + 2;
	  cmi_path = XNEWVEC (char, cmi_path_alloc);

	  memcpy (cmi_path, cmi_repo, cmi_repo_length);
	  cmi_path[cmi_repo_length] = DIR_SEPARATOR;
	}

      memcpy (&cmi_path[cmi_repo_length + 1], to, len + 1);
      len += cmi_repo_length + 1;
      to = cmi_path;
    }

  if (len_p)
    *len_p = len;

  return to;
}

/* Try and create the directories of PATH.  */

static void
create_dirs (char *path)
{
  /* Try and create the missing directories.  */
  for (char *base = path; *base; base++)
    if (IS_DIR_SEPARATOR (*base))
      {
	char sep = *base;
	*base = 0;
	int failed = mkdir (path, S_IRWXU | S_IRWXG | S_IRWXO);
	dump () && dump ("Mkdir ('%s') errno:=%u", path, failed ? errno : 0);
	*base = sep;
	if (failed
	    /* Maybe racing with another creator (of a *different*
	       module).  */
	    && errno != EEXIST)
	  break;
      }
}

/* Given a CLASSTYPE_DECL_LIST VALUE get the template friend decl,
   if that's what this is.  */

static tree
friend_from_decl_list (tree frnd)
{
  tree res = frnd;

  if (TREE_CODE (frnd) != TEMPLATE_DECL)
    {
      tree tmpl = NULL_TREE;
      if (TYPE_P (frnd))
	{
	  res = TYPE_NAME (frnd);
	  if (CLASS_TYPE_P (frnd)
	      && CLASSTYPE_TEMPLATE_INFO (frnd))
	    tmpl = CLASSTYPE_TI_TEMPLATE (frnd);
	}
      else if (DECL_TEMPLATE_INFO (frnd))
	{
	  tmpl = DECL_TI_TEMPLATE (frnd);
	  if (TREE_CODE (tmpl) != TEMPLATE_DECL)
	    tmpl = NULL_TREE;
	}

      if (tmpl && DECL_TEMPLATE_RESULT (tmpl) == res)
	res = tmpl;
    }

  return res;
}

static tree
find_enum_member (tree ctx, tree name)
{
  for (tree values = TYPE_VALUES (ctx);
       values; values = TREE_CHAIN (values))
    if (DECL_NAME (TREE_VALUE (values)) == name)
      return TREE_VALUE (values);

  return NULL_TREE;
}

/********************************************************************/
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
      dump ("Wrote:");
      dump ("  %u decl trees", decl_val_count);
      dump ("  %u other trees", tree_val_count);
      dump ("  %u back references", back_ref_count);
      dump ("  %u null trees", null_count);
    }
}

/* Setup and teardown for a tree walk.  */

void
trees_out::begin ()
{
  gcc_assert (!streaming_p () || !tree_map.elements ());

  mark_trees ();
  if (streaming_p ())
    parent::begin ();
}

unsigned
trees_out::end (elf_out *sink, unsigned name, unsigned *crc_ptr)
{
  gcc_checking_assert (streaming_p ());

  unmark_trees ();
  return parent::end (sink, name, crc_ptr);
}

void
trees_out::end ()
{
  gcc_assert (!streaming_p ());

  unmark_trees ();
  /* Do not parent::end -- we weren't streaming.  */
}

void
trees_out::mark_trees ()
{
  if (size_t size = tree_map.elements ())
    {
      /* This isn't our first rodeo, destroy and recreate the
	 tree_map.  I'm a bad bad man.  Use the previous size as a
	 guess for the next one (so not all bad).  */
      tree_map.~ptr_int_hash_map ();
      new (&tree_map) ptr_int_hash_map (size);
    }

  /* Install the fixed trees, with +ve references.  */
  unsigned limit = fixed_trees->length ();
  for (unsigned ix = 0; ix != limit; ix++)
    {
      tree val = (*fixed_trees)[ix];
      bool existed = tree_map.put (val, ix + tag_fixed);
      gcc_checking_assert (!TREE_VISITED (val) && !existed);
      TREE_VISITED (val) = true;
    }

  ref_num = 0;
}

/* Unmark the trees we encountered  */

void
trees_out::unmark_trees ()
{
  ptr_int_hash_map::iterator end (tree_map.end ());
  for (ptr_int_hash_map::iterator iter (tree_map.begin ()); iter != end; ++iter)
    {
      tree node = reinterpret_cast<tree> ((*iter).first);
      int ref = (*iter).second;
      /* We should have visited the node, and converted its mergeable
	 reference to a regular reference.  */
      gcc_checking_assert (TREE_VISITED (node)
			   && (ref <= tag_backref || ref >= tag_fixed));
      TREE_VISITED (node) = false;
    }
}

/* Mark DECL for by-value walking.  We do this by inserting it into
   the tree map with a reference of zero.  May be called multiple
   times on the same node.  */

void
trees_out::mark_by_value (tree decl)
{
  gcc_checking_assert (DECL_P (decl)
		       /* Enum consts are INTEGER_CSTS.  */
		       || TREE_CODE (decl) == INTEGER_CST
		       || TREE_CODE (decl) == TREE_BINFO);

  if (TREE_VISITED (decl))
    /* Must already be forced or fixed.  */
    gcc_checking_assert (*tree_map.get (decl) >= tag_value);
  else
    {
      bool existed = tree_map.put (decl, tag_value);
      gcc_checking_assert (!existed);
      TREE_VISITED (decl) = true;
    }
}

int
trees_out::get_tag (tree t)
{
  gcc_checking_assert (TREE_VISITED (t));
  return *tree_map.get (t);
}

/* Insert T into the map, return its tag number.    */

int
trees_out::insert (tree t, walk_kind walk)
{
  gcc_checking_assert (walk != WK_normal || !TREE_VISITED (t));
  int tag = --ref_num;
  bool existed;
  int &slot = tree_map.get_or_insert (t, &existed);
  gcc_checking_assert (TREE_VISITED (t) == existed
		       && (!existed
			   || (walk == WK_value && slot == tag_value)));
  TREE_VISITED (t) = true;
  slot = tag;

  return tag;
}

/* Insert T into the backreference array.  Return its back reference
   number.  */

int
trees_in::insert (tree t)
{
  gcc_checking_assert (t || get_overrun ());
  back_refs.safe_push (t);
  return -(int)back_refs.length ();
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
  for (tree *chain = &decls;;)
    if (tree decl = tree_node ())
      {
	if (!DECL_P (decl) || DECL_CHAIN (decl))
	  {
	    set_overrun ();
	    break;
	  }
	*chain = decl;
	chain = &DECL_CHAIN (decl);
      }
    else
      break;

  return decls;
}

/* A vector of decls following DECL_CHAIN.  */

void
trees_out::vec_chained_decls (tree decls)
{
  if (streaming_p ())
    {
      unsigned len = 0;

      for (tree decl = decls; decl; decl = DECL_CHAIN (decl))
	len++;
      u (len);
    }

  for (tree decl = decls; decl; decl = DECL_CHAIN (decl))
    {
      if (DECL_IMPLICIT_TYPEDEF_P (decl)
	  && TYPE_NAME (TREE_TYPE (decl)) != decl)
	/* An anonynmous struct with a typedef name.  An odd thing to
	   write.  */
	tree_node (NULL_TREE);
      else
	tree_node (decl);
    }
}

vec<tree, va_heap> *
trees_in::vec_chained_decls ()
{
  vec<tree, va_heap> *v = NULL;

  if (unsigned len = u ())
    {
      vec_alloc (v, len);

      for (unsigned ix = 0; ix < len; ix++)
	{
	  tree decl = tree_node ();
	  if (decl && !DECL_P (decl))
	    {
	      set_overrun ();
	      break;
	    }
	  v->quick_push (decl);
	}

      if (get_overrun ())
	{
	  vec_free (v);
	  v = NULL;
	}
    }

  return v;
}

/* A vector of trees.  */

void
trees_out::tree_vec (vec<tree, va_gc> *v)
{
  unsigned len = vec_safe_length (v);
  if (streaming_p ())
    u (len);
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
  if (streaming_p ())
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

void
trees_out::tree_list (tree list, bool has_purpose)
{
  for (; list; list = TREE_CHAIN (list))
    {
      gcc_checking_assert (TREE_VALUE (list));
      tree_node (TREE_VALUE (list));
      if (has_purpose)
	tree_node (TREE_PURPOSE (list));
    }
  tree_node (NULL_TREE);
}

tree
trees_in::tree_list (bool has_purpose)
{
  tree res = NULL_TREE;

  for (tree *chain = &res; tree value = tree_node ();
       chain = &TREE_CHAIN (*chain))
    {
      tree purpose = has_purpose ? tree_node () : NULL_TREE;
      *chain = build_tree_list (purpose, value);
    }

  return res;
}
/* Start tree write.  Write information to allocate the receiving
   node.  */

void
trees_out::start (tree t, bool code_streamed)
{
  if (TYPE_P (t))
    {
      enum tree_code code = TREE_CODE (t);
      gcc_checking_assert (TYPE_MAIN_VARIANT (t) == t);
      /* All these types are TYPE_NON_COMMON.  */
      gcc_checking_assert (code == RECORD_TYPE
			   || code == UNION_TYPE
			   || code == ENUMERAL_TYPE
			   || code == TEMPLATE_TYPE_PARM
			   || code == TEMPLATE_TEMPLATE_PARM
			   || code == BOUND_TEMPLATE_TEMPLATE_PARM);
    }

  if (!code_streamed)
    u (TREE_CODE (t));

  switch (TREE_CODE (t))
    {
    default:
      if (VL_EXP_CLASS_P (t))
	u (VL_EXP_OPERAND_LENGTH (t));
      break;

    case INTEGER_CST:
      u (TREE_INT_CST_NUNITS (t));
      u (TREE_INT_CST_EXT_NUNITS (t));
      break;

    case OMP_CLAUSE:
      state->extensions |= SE_OPENMP;
      u (OMP_CLAUSE_CODE (t));
      break;

    case STRING_CST:
      str (TREE_STRING_POINTER (t), TREE_STRING_LENGTH (t));
      break;

    case VECTOR_CST:
      u (VECTOR_CST_LOG2_NPATTERNS (t));
      u (VECTOR_CST_NELTS_PER_PATTERN (t));
      break;

    case TREE_BINFO:
      u (BINFO_N_BASE_BINFOS (t));
      break;

    case TREE_VEC:
      u (TREE_VEC_LENGTH (t));
      break;

    case FIXED_CST:
      gcc_unreachable (); /* Not supported in C++.  */
      break;

    case IDENTIFIER_NODE:
    case SSA_NAME:
    case TARGET_MEM_REF:
    case TRANSLATION_UNIT_DECL:
      /* We shouldn't meet these.  */
      gcc_unreachable ();
      break;
    }
}

/* Start tree read.  Allocate the receiving node.  */

tree
trees_in::start (unsigned code)
{
  tree t = NULL_TREE;

  if (!code)
    code = u ();

  switch (code)
    {
    default:
      if (code >= MAX_TREE_CODES)
	{
	fail:
	  set_overrun ();
	  return NULL_TREE;
	}
      else if (TREE_CODE_CLASS (code) == tcc_vl_exp)
	{
	  unsigned ops = u ();
	  t = build_vl_exp (tree_code (code), ops);
	}
      else
	t = make_node (tree_code (code));
      break;

    case INTEGER_CST:
      {
	unsigned n = u ();
	unsigned e = u ();
	t = make_int_cst (n, e);
      }
      break;

    case OMP_CLAUSE:
      {
	if (!(state->extensions & SE_OPENMP))
	  goto fail;

	unsigned omp_code = u ();
	t = build_omp_clause (UNKNOWN_LOCATION, omp_clause_code (omp_code));
      }
      break;

    case STRING_CST:
      {
	size_t l;
	const char *chars = str (&l);
	t = build_string (l, chars);
      }
      break;

    case VECTOR_CST:
      {
	unsigned log2_npats = u ();
	unsigned elts_per = u ();
	t = make_vector (log2_npats, elts_per);
      }
      break;

    case TREE_BINFO:
      t = make_tree_binfo (u ());
      break;

    case TREE_VEC:
      t = make_tree_vec (u ());
      break;

    case FIXED_CST:
    case IDENTIFIER_NODE:
    case SSA_NAME:
    case TARGET_MEM_REF:
    case TRANSLATION_UNIT_DECL:
      goto fail;
    }

  return t;
}

/* The structure streamers access the raw fields, because the
   alternative, of using the accessor macros can require using
   different accessors for the same underlying field, depending on the
   tree code.  That's both confusing and annoying.  */

/* Read & write the core boolean flags.  */

void
trees_out::core_bools (tree t, bits_out& bits)
{
#define WB(X) (bits.b (X))
/* Stream X if COND holds, and if !COND stream a dummy value so that the
   overall number of bits streamed is independent of the runtime value
   of COND, which allows the compiler to better optimize this function.  */
#define WB_IF(COND, X) WB ((COND) ? (X) : false)
  tree_code code = TREE_CODE (t);

  WB (t->base.side_effects_flag);
  WB (t->base.constant_flag);
  WB (t->base.addressable_flag);
  WB (t->base.volatile_flag);
  WB (t->base.readonly_flag);
  /* base.asm_written_flag is a property of the current TU's use of
     this decl.  */
  WB (t->base.nowarning_flag);
  /* base.visited read as zero (it's set for writer, because that's
     how we mark nodes).  */
  /* base.used_flag is not streamed.  Readers may set TREE_USED of
     decls they use.  */
  WB (t->base.nothrow_flag);
  WB (t->base.static_flag);
  /* This is TYPE_CACHED_VALUES_P for types.  */
  WB_IF (TREE_CODE_CLASS (code) != tcc_type, t->base.public_flag);
  WB (t->base.private_flag);
  WB (t->base.protected_flag);
  WB (t->base.deprecated_flag);
  WB (t->base.default_def_flag);

  switch (code)
    {
    case CALL_EXPR:
    case INTEGER_CST:
    case SSA_NAME:
    case TARGET_MEM_REF:
    case TREE_VEC:
      /* These use different base.u fields.  */
      return;

    default:
      WB (t->base.u.bits.lang_flag_0);
      bool flag_1 = t->base.u.bits.lang_flag_1;
      if (!flag_1)
	;
      else if (code == TEMPLATE_INFO)
	/* This is TI_PENDING_TEMPLATE_FLAG, not relevant to reader.  */
	flag_1 = false;
      else if (code == VAR_DECL)
	{
	  /* This is DECL_INITIALIZED_P.  */
	  if (TREE_CODE (DECL_CONTEXT (t)) != FUNCTION_DECL)
	    /* We'll set this when reading the definition.  */
	    flag_1 = false;
	}
      WB (flag_1);
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
      WB (t->base.u.bits.unavailable_flag);
      break;
    }

  if (TREE_CODE_CLASS (code) == tcc_type)
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

  if (TREE_CODE_CLASS (code) != tcc_declaration)
    return;

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

      {
	/* This is DECL_INTERFACE_KNOWN: We should redetermine whether
	   we need to import or export any vtables or typeinfo objects
	   on stream-in.  */
	bool interface_known = t->decl_common.lang_flag_5;
	if (VAR_P (t) && (DECL_VTABLE_OR_VTT_P (t) || DECL_TINFO_P (t)))
	  interface_known = false;
	WB (interface_known);
      }

      WB (t->decl_common.lang_flag_6);
      WB (t->decl_common.lang_flag_7);
      WB (t->decl_common.lang_flag_8);
      WB (t->decl_common.decl_flag_0);

      {
	/* DECL_EXTERNAL -> decl_flag_1
	     == it is defined elsewhere
	   DECL_NOT_REALLY_EXTERN -> base.not_really_extern
	     == that was a lie, it is here  */

	bool is_external = t->decl_common.decl_flag_1;
	if (!is_external)
	  /* decl_flag_1 is DECL_EXTERNAL. Things we emit here, might
	     well be external from the POV of an importer.  */
	  // FIXME: Do we need to know if this is a TEMPLATE_RESULT --
	  // a flag from the caller?
	  switch (code)
	    {
	    default:
	      break;

	    case VAR_DECL:
	      if (TREE_PUBLIC (t)
		  && !(TREE_STATIC (t)
		       && DECL_FUNCTION_SCOPE_P (t)
		       && DECL_DECLARED_INLINE_P (DECL_CONTEXT (t)))
		  && !DECL_VAR_DECLARED_INLINE_P (t))
		is_external = true;
	      break;

	    case FUNCTION_DECL:
	      if (TREE_PUBLIC (t)
		  && !DECL_DECLARED_INLINE_P (t))
		is_external = true;
	      break;
	    }
	WB (is_external);
      }

      WB (t->decl_common.decl_flag_2);
      WB (t->decl_common.decl_flag_3);
      WB (t->decl_common.not_gimple_reg_flag);
      WB (t->decl_common.decl_by_reference_flag);
      WB (t->decl_common.decl_read_flag);
      WB (t->decl_common.decl_nonshareable_flag);
      WB (t->decl_common.decl_not_flexarray);
    }
  else
    return;

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
      WB (t->decl_with_vis.init_priority_p);
      WB (t->decl_with_vis.shadowed_for_var_p);
      WB (t->decl_with_vis.cxx_constructor);
      WB (t->decl_with_vis.cxx_destructor);
      WB (t->decl_with_vis.final);
      WB (t->decl_with_vis.regdecl_flag);
    }
  else
    return;

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      WB (t->function_decl.static_ctor_flag);
      WB (t->function_decl.static_dtor_flag);
      WB (t->function_decl.uninlinable);
      WB (t->function_decl.possibly_inlined);
      WB (t->function_decl.novops_flag);
      WB (t->function_decl.returns_twice_flag);
      WB (t->function_decl.malloc_flag);
      WB (t->function_decl.declared_inline_flag);
      WB (t->function_decl.no_inline_warning_flag);
      WB (t->function_decl.no_instrument_function_entry_exit);
      WB (t->function_decl.no_limit_stack);
      WB (t->function_decl.disregard_inline_limits);
      WB (t->function_decl.pure_flag);
      WB (t->function_decl.looping_const_or_pure_flag);

      WB (t->function_decl.has_debug_args_flag);
      WB (t->function_decl.versioned_function);

      /* decl_type is a (misnamed) 2 bit discriminator.	 */
      unsigned kind = t->function_decl.decl_type;
      WB ((kind >> 0) & 1);
      WB ((kind >> 1) & 1);
    }
#undef WB_IF
#undef WB
}

bool
trees_in::core_bools (tree t, bits_in& bits)
{
#define RB(X) ((X) = bits.b ())
/* See the comment for WB_IF in trees_out::core_bools.  */
#define RB_IF(COND, X) ((COND) ? RB (X) : bits.b ())

  tree_code code = TREE_CODE (t);

  RB (t->base.side_effects_flag);
  RB (t->base.constant_flag);
  RB (t->base.addressable_flag);
  RB (t->base.volatile_flag);
  RB (t->base.readonly_flag);
  /* base.asm_written_flag is not streamed.  */
  RB (t->base.nowarning_flag);
  /* base.visited is not streamed.  */
  /* base.used_flag is not streamed.  */
  RB (t->base.nothrow_flag);
  RB (t->base.static_flag);
  RB_IF (TREE_CODE_CLASS (code) != tcc_type, t->base.public_flag);
  RB (t->base.private_flag);
  RB (t->base.protected_flag);
  RB (t->base.deprecated_flag);
  RB (t->base.default_def_flag);

  switch (code)
    {
    case CALL_EXPR:
    case INTEGER_CST:
    case SSA_NAME:
    case TARGET_MEM_REF:
    case TREE_VEC:
      /* These use different base.u fields.  */
      goto done;

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
      RB (t->base.u.bits.unavailable_flag);
      break;
    }

  if (TREE_CODE_CLASS (code) == tcc_type)
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

  if (TREE_CODE_CLASS (code) != tcc_declaration)
    goto done;

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
      RB (t->decl_common.not_gimple_reg_flag);
      RB (t->decl_common.decl_by_reference_flag);
      RB (t->decl_common.decl_read_flag);
      RB (t->decl_common.decl_nonshareable_flag);
      RB (t->decl_common.decl_not_flexarray);
    }
  else
    goto done;

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
      RB (t->decl_with_vis.init_priority_p);
      RB (t->decl_with_vis.shadowed_for_var_p);
      RB (t->decl_with_vis.cxx_constructor);
      RB (t->decl_with_vis.cxx_destructor);
      RB (t->decl_with_vis.final);
      RB (t->decl_with_vis.regdecl_flag);
    }
  else
    goto done;

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      RB (t->function_decl.static_ctor_flag);
      RB (t->function_decl.static_dtor_flag);
      RB (t->function_decl.uninlinable);
      RB (t->function_decl.possibly_inlined);
      RB (t->function_decl.novops_flag);
      RB (t->function_decl.returns_twice_flag);
      RB (t->function_decl.malloc_flag);
      RB (t->function_decl.declared_inline_flag);
      RB (t->function_decl.no_inline_warning_flag);
      RB (t->function_decl.no_instrument_function_entry_exit);
      RB (t->function_decl.no_limit_stack);
      RB (t->function_decl.disregard_inline_limits);
      RB (t->function_decl.pure_flag);
      RB (t->function_decl.looping_const_or_pure_flag);

      RB (t->function_decl.has_debug_args_flag);
      RB (t->function_decl.versioned_function);

      /* decl_type is a (misnamed) 2 bit discriminator.	 */
      unsigned kind = 0;
      kind |= unsigned (bits.b ()) << 0;
      kind |= unsigned (bits.b ()) << 1;
      t->function_decl.decl_type = function_decl_type (kind);
    }
#undef RB_IF
#undef RB
done:
  return !get_overrun ();
}

void
trees_out::lang_decl_bools (tree t, bits_out& bits)
{
#define WB(X) (bits.b (X))
  const struct lang_decl *lang = DECL_LANG_SPECIFIC (t);

  bits.bflush ();
  WB (lang->u.base.language == lang_cplusplus);
  WB ((lang->u.base.use_template >> 0) & 1);
  WB ((lang->u.base.use_template >> 1) & 1);
  /* Do not write lang->u.base.not_really_extern, importer will set
     when reading the definition (if any).  */
  WB (lang->u.base.initialized_in_class);
  WB (lang->u.base.threadprivate_or_deleted_p);
  /* Do not write lang->u.base.anticipated_p, it is a property of the
     current TU.  */
  WB (lang->u.base.friend_or_tls);
  WB (lang->u.base.unknown_bound_p);
  /* Do not write lang->u.base.odr_used, importer will recalculate if
     they do ODR use this decl.  */
  WB (lang->u.base.concept_p);
  WB (lang->u.base.var_declared_inline_p);
  WB (lang->u.base.dependent_init_p);
  /* When building a header unit, everthing is marked as purview, (so
     we know which decls to write).  But when we import them we do not
     want to mark them as in module purview.  */
  WB (lang->u.base.module_purview_p && !header_module_p ());
  WB (lang->u.base.module_attach_p);
  WB (lang->u.base.module_keyed_decls_p);
  switch (lang->u.base.selector)
    {
    default:
      gcc_unreachable ();

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
      /* Do not stream lang->u.hidden_friend_p, it is a property of
	 the TU.  */
      WB (lang->u.fn.omp_declare_reduction_p);
      WB (lang->u.fn.has_dependent_explicit_spec_p);
      WB (lang->u.fn.immediate_fn_p);
      WB (lang->u.fn.maybe_deleted);
      /* We do not stream lang->u.fn.implicit_constexpr.  */
      WB (lang->u.fn.escalated_p);
      WB (lang->u.fn.xobj_func);
      goto lds_min;

    case lds_decomp:  /* lang_decl_decomp.  */
      /* No bools.  */
      goto lds_min;

    case lds_min:  /* lang_decl_min.  */
    lds_min:
      /* No bools.  */
      break;

    case lds_ns:  /* lang_decl_ns.  */
      /* No bools.  */
      break;

    case lds_parm:  /* lang_decl_parm.  */
      /* No bools.  */
      break;
    }
#undef WB
}

bool
trees_in::lang_decl_bools (tree t, bits_in& bits)
{
#define RB(X) ((X) = bits.b ())
  struct lang_decl *lang = DECL_LANG_SPECIFIC (t);

  bits.bflush ();
  lang->u.base.language = bits.b () ? lang_cplusplus : lang_c;
  unsigned v;
  v = bits.b () << 0;
  v |= bits.b () << 1;
  lang->u.base.use_template = v;
  /* lang->u.base.not_really_extern is not streamed.  */
  RB (lang->u.base.initialized_in_class);
  RB (lang->u.base.threadprivate_or_deleted_p);
  /* lang->u.base.anticipated_p is not streamed.  */
  RB (lang->u.base.friend_or_tls);
  RB (lang->u.base.unknown_bound_p);
  /* lang->u.base.odr_used is not streamed.  */
  RB (lang->u.base.concept_p);
  RB (lang->u.base.var_declared_inline_p);
  RB (lang->u.base.dependent_init_p);
  RB (lang->u.base.module_purview_p);
  RB (lang->u.base.module_attach_p);
  RB (lang->u.base.module_keyed_decls_p);
  switch (lang->u.base.selector)
    {
    default:
      gcc_unreachable ();

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
      /* lang->u.fn.hidden_friend_p is not streamed.  */
      RB (lang->u.fn.omp_declare_reduction_p);
      RB (lang->u.fn.has_dependent_explicit_spec_p);
      RB (lang->u.fn.immediate_fn_p);
      RB (lang->u.fn.maybe_deleted);
      /* We do not stream lang->u.fn.implicit_constexpr.  */
      RB (lang->u.fn.escalated_p);
      RB (lang->u.fn.xobj_func);
      goto lds_min;

    case lds_decomp:  /* lang_decl_decomp.  */
      /* No bools.  */
      goto lds_min;

    case lds_min:  /* lang_decl_min.  */
    lds_min:
      /* No bools.  */
      break;

    case lds_ns:  /* lang_decl_ns.  */
      /* No bools.  */
      break;

    case lds_parm:  /* lang_decl_parm.  */
      /* No bools.  */
      break;
    }
#undef RB
  return !get_overrun ();
}

void
trees_out::lang_type_bools (tree t, bits_out& bits)
{
#define WB(X) (bits.b (X))
  const struct lang_type *lang = TYPE_LANG_SPECIFIC (t);

  bits.bflush ();
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
  WB (lang->interface_only);
  WB (lang->interface_unknown);
  WB (lang->contains_empty_class_p);
  WB (lang->anon_aggr);
  WB (lang->non_zero_init);
  WB (lang->empty_p);

  WB (lang->vec_new_uses_cookie);
  WB (lang->declared_class);
  WB (lang->diamond_shaped);
  WB (lang->repeated_base);
  gcc_assert (!lang->being_defined);
  // lang->debug_requested
  WB (lang->fields_readonly);
  WB (lang->ptrmemfunc_flag);

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
trees_in::lang_type_bools (tree t, bits_in& bits)
{
#define RB(X) ((X) = bits.b ())
  struct lang_type *lang = TYPE_LANG_SPECIFIC (t);

  bits.bflush ();
  RB (lang->has_type_conversion);
  RB (lang->has_copy_ctor);
  RB (lang->has_default_ctor);
  RB (lang->const_needs_init);
  RB (lang->ref_needs_init);
  RB (lang->has_const_copy_assign);
  unsigned v;
  v = bits.b () << 0;
  v |= bits.b () << 1;
  lang->use_template = v;

  RB (lang->has_mutable);
  RB (lang->com_interface);
  RB (lang->non_pod_class);
  RB (lang->nearly_empty_p);
  RB (lang->user_align);
  RB (lang->has_copy_assign);
  RB (lang->has_new);
  RB (lang->has_array_new);

  v = bits.b () << 0;
  v |= bits.b () << 1;
  lang->gets_delete = v;
  RB (lang->interface_only);
  RB (lang->interface_unknown);
  RB (lang->contains_empty_class_p);
  RB (lang->anon_aggr);
  RB (lang->non_zero_init);
  RB (lang->empty_p);

  RB (lang->vec_new_uses_cookie);
  RB (lang->declared_class);
  RB (lang->diamond_shaped);
  RB (lang->repeated_base);
  gcc_assert (!lang->being_defined);
  gcc_assert (!lang->debug_requested);
  RB (lang->fields_readonly);
  RB (lang->ptrmemfunc_flag);

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

  /* First by shape of the tree.  */

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      /* Write this early, for better log information.  */
      WT (t->decl_minimal.name);
      if (!DECL_TEMPLATE_PARM_P (t))
	WT (t->decl_minimal.context);

      if (state)
	state->write_location (*this, t->decl_minimal.locus);

      if (streaming_p ())
	if (has_warning_spec (t))
	  u (get_warning_spec (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      /* The only types we write also have TYPE_NON_COMMON.  */
      gcc_checking_assert (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON));

      /* We only stream the main variant.  */
      gcc_checking_assert (TYPE_MAIN_VARIANT (t) == t);

      /* Stream the name & context first, for better log information  */
      WT (t->type_common.name);
      WT (t->type_common.context);

      /* By construction we want to make sure we have the canonical
	 and main variants already in the type table, so emit them
	 now.  */
      WT (t->type_common.main_variant);

      tree canonical = t->type_common.canonical;
      if (canonical && DECL_TEMPLATE_PARM_P (TYPE_NAME (t)))
	/* We do not want to wander into different templates.
	   Reconstructed on stream in.  */
	canonical = t;
      WT (canonical);

      /* type_common.next_variant is internally manipulated.  */
      /* type_common.pointer_to, type_common.reference_to.  */

      if (streaming_p ())
	{
	  WU (t->type_common.precision);
	  WU (t->type_common.contains_placeholder_bits);
	  WU (t->type_common.mode);
	  WU (t->type_common.align);
	}

      if (!RECORD_OR_UNION_CODE_P (code))
	{
	  WT (t->type_common.size);
	  WT (t->type_common.size_unit);
	}
      WT (t->type_common.attributes);

      WT (t->type_common.common.chain); /* TYPE_STUB_DECL.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      if (streaming_p ())
	{
	  WU (t->decl_common.mode);
	  WU (t->decl_common.off_align);
	  WU (t->decl_common.align);
	}

      /* For templates these hold instantiation (partial and/or
	 specialization) information.  */
      if (code != TEMPLATE_DECL)
	{
	  WT (t->decl_common.size);
	  WT (t->decl_common.size_unit);
	}

      WT (t->decl_common.attributes);
      // FIXME: Does this introduce cross-decl links?  For instance
      // from instantiation to the template.  If so, we'll need more
      // deduplication logic.  I think we'll need to walk the blocks
      // of the owning function_decl's abstract origin in tandem, to
      // generate the locating data needed?
      WT (t->decl_common.abstract_origin);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      WT (t->decl_with_vis.assembler_name);
      if (streaming_p ())
	WU (t->decl_with_vis.visibility);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      if (code == ENUMERAL_TYPE)
	{
	  /* These fields get set even for opaque enums that lack a
	     definition, so we stream them directly for each ENUMERAL_TYPE.
	     We stream TYPE_VALUES as part of the definition.  */
	  WT (t->type_non_common.maxval);
	  WT (t->type_non_common.minval);
	}
      /* Records and unions hold FIELDS, VFIELD & BINFO on these
	 things.  */
      else if (!RECORD_OR_UNION_CODE_P (code))
	{
	  // FIXME: These are from tpl_parm_value's 'type' writing.
	  // Perhaps it should just be doing them directly?
	  gcc_checking_assert (code == TEMPLATE_TYPE_PARM
			       || code == TEMPLATE_TEMPLATE_PARM
			       || code == BOUND_TEMPLATE_TEMPLATE_PARM);
	  gcc_checking_assert (!TYPE_CACHED_VALUES_P (t));
	  WT (t->type_non_common.values);
	  WT (t->type_non_common.maxval);
	  WT (t->type_non_common.minval);
	}

      WT (t->type_non_common.lang_1);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    {
      if (state)
	state->write_location (*this, t->exp.locus);

      if (streaming_p ())
	if (has_warning_spec (t))
	  u (get_warning_spec (t));

      /* Walk in forward order, as (for instance) REQUIRES_EXPR has a
         bunch of unscoped parms on its first operand.  It's safer to
         create those in order.  */
      bool vl = TREE_CODE_CLASS (code) == tcc_vl_exp;
      for (unsigned limit = (vl ? VL_EXP_OPERAND_LENGTH (t)
			     : TREE_OPERAND_LENGTH (t)),
	     ix = unsigned (vl); ix != limit; ix++)
	WT (TREE_OPERAND (t, ix));
    }
  else
    /* The CODE_CONTAINS tables were inaccurate when I started.  */
    gcc_checking_assert (TREE_CODE_CLASS (code) != tcc_expression
			 && TREE_CODE_CLASS (code) != tcc_binary
			 && TREE_CODE_CLASS (code) != tcc_unary
			 && TREE_CODE_CLASS (code) != tcc_reference
			 && TREE_CODE_CLASS (code) != tcc_comparison
			 && TREE_CODE_CLASS (code) != tcc_statement
			 && TREE_CODE_CLASS (code) != tcc_vl_exp);

  /* Then by CODE.  Special cases and/or 1:1 tree shape
     correspondance. */
  switch (code)
    {
    default:
      break;

    case ARGUMENT_PACK_SELECT:  /* Transient during instantiation.  */
    case DEFERRED_PARSE:	/* Expanded upon completion of
				   outermost class.  */
    case IDENTIFIER_NODE:	/* Streamed specially.  */
    case BINDING_VECTOR:		/* Only in namespace-scope symbol
				   table.  */
    case SSA_NAME:
    case TRANSLATION_UNIT_DECL: /* There is only one, it is a
				   global_tree.  */
    case USERDEF_LITERAL:  	/* Expanded during parsing.  */
      gcc_unreachable (); /* Should never meet.  */

      /* Constants.  */
    case COMPLEX_CST:
      WT (TREE_REALPART (t));
      WT (TREE_IMAGPART (t));
      break;

    case FIXED_CST:
      gcc_unreachable (); /* Not supported in C++.  */

    case INTEGER_CST:
      if (streaming_p ())
	{
	  unsigned num = TREE_INT_CST_EXT_NUNITS (t);
	  for (unsigned ix = 0; ix != num; ix++)
	    wu (TREE_INT_CST_ELT (t, ix));
	}
      break;

    case POLY_INT_CST:
      if (streaming_p ())
	for (unsigned ix = 0; ix != NUM_POLY_INT_COEFFS; ix++)
	  WT (POLY_INT_CST_COEFF (t, ix));
      break;

    case REAL_CST:
      if (streaming_p ())
	buf (TREE_REAL_CST_PTR (t), sizeof (real_value));
      break;

    case STRING_CST:
      /* Streamed during start.  */
      break;

    case VECTOR_CST:
      for (unsigned ix = vector_cst_encoded_nelts (t); ix--;)
	WT (VECTOR_CST_ENCODED_ELT (t, ix));
      break;

      /* Decls.  */
    case VAR_DECL:
      if (DECL_CONTEXT (t)
	  && TREE_CODE (DECL_CONTEXT (t)) != FUNCTION_DECL)
	break;
      /* FALLTHROUGH  */

    case RESULT_DECL:
    case PARM_DECL:
      if (DECL_HAS_VALUE_EXPR_P (t))
	WT (DECL_VALUE_EXPR (t));
      /* FALLTHROUGH  */

    case CONST_DECL:
    case IMPORTED_DECL:
      WT (t->decl_common.initial);
      break;

    case FIELD_DECL:
      WT (t->field_decl.offset);
      WT (t->field_decl.bit_field_type);
      WT (t->field_decl.qualifier); /* bitfield unit.  */
      WT (t->field_decl.bit_offset);
      WT (t->field_decl.fcontext);
      WT (t->decl_common.initial);
      break;

    case LABEL_DECL:
      if (streaming_p ())
	{
	  WU (t->label_decl.label_decl_uid);
	  WU (t->label_decl.eh_landing_pad_nr);
	}
      break;

    case FUNCTION_DECL:
      if (streaming_p ())
	{
	  /* Builtins can be streamed by value when a header declares
	     them.  */
	  WU (DECL_BUILT_IN_CLASS (t));
	  if (DECL_BUILT_IN_CLASS (t) != NOT_BUILT_IN)
	    WU (DECL_UNCHECKED_FUNCTION_CODE (t));
	}

      WT (t->function_decl.personality);
      WT (t->function_decl.function_specific_target);
      WT (t->function_decl.function_specific_optimization);
      WT (t->function_decl.vindex);

      if (DECL_HAS_DEPENDENT_EXPLICIT_SPEC_P (t))
	WT (lookup_explicit_specifier (t));
      break;

    case USING_DECL:
      /* USING_DECL_DECLS  */
      WT (t->decl_common.initial);
      /* FALLTHROUGH  */

    case TYPE_DECL:
      /* USING_DECL: USING_DECL_SCOPE  */
      /* TYPE_DECL: DECL_ORIGINAL_TYPE */
      WT (t->decl_non_common.result);
      break;

      /* Miscellaneous common nodes.  */
    case BLOCK:
      if (state)
	{
	  state->write_location (*this, t->block.locus);
	  state->write_location (*this, t->block.end_locus);
	}

      /* DECL_LOCAL_DECL_P decls are first encountered here and
         streamed by value.  */
      for (tree decls = t->block.vars; decls; decls = DECL_CHAIN (decls))
	{
	  if (VAR_OR_FUNCTION_DECL_P (decls)
	      && DECL_LOCAL_DECL_P (decls))
	    {
	      /* Make sure this is the first encounter, and mark for
		 walk-by-value.  */
	      gcc_checking_assert (!TREE_VISITED (decls)
				   && !DECL_TEMPLATE_INFO (decls));
	      mark_by_value (decls);
	    }
	  tree_node (decls);
	}
      tree_node (NULL_TREE);

      /* nonlocalized_vars is a middle-end thing.  */
      WT (t->block.subblocks);
      WT (t->block.supercontext);
      // FIXME: As for decl's abstract_origin, does this introduce crosslinks?
      WT (t->block.abstract_origin);
      /* fragment_origin, fragment_chain are middle-end things.  */
      WT (t->block.chain);
      /* nonlocalized_vars, block_num & die are middle endy/debug
	 things.  */
      break;

    case CALL_EXPR:
      if (streaming_p ())
	WU (t->base.u.ifn);
      break;

    case CONSTRUCTOR:
      // This must be streamed /after/ we've streamed the type,
      // because it can directly refer to elements of the type. Eg,
      // FIELD_DECLs of a RECORD_TYPE.
      break;

    case OMP_CLAUSE:
      {
	/* The ompcode is serialized in start.  */
	if (streaming_p ())
	  WU (t->omp_clause.subcode.map_kind);
	if (state)
	  state->write_location (*this, t->omp_clause.locus);

	unsigned len = omp_clause_num_ops[OMP_CLAUSE_CODE (t)];
	for (unsigned ix = 0; ix != len; ix++)
	  WT (t->omp_clause.ops[ix]);
      }
      break;

    case STATEMENT_LIST:
      for (tree stmt : tsi_range (t))
	if (stmt)
	  WT (stmt);
      WT (NULL_TREE);
      break;

    case OPTIMIZATION_NODE:
    case TARGET_OPTION_NODE:
      // FIXME: Our representation for these two nodes is a cache of
      // the resulting set of options.  Not a record of the options
      // that got changed by a particular attribute or pragma.  Should
      // we record that, or should we record the diff from the command
      // line options?  The latter seems the right behaviour, but is
      // (a) harder, and I guess could introduce strangeness if the
      // importer has set some incompatible set of optimization flags?
      gcc_unreachable ();
      break;

    case TREE_BINFO:
      {
	WT (t->binfo.common.chain);
	WT (t->binfo.offset);
	WT (t->binfo.inheritance);
	WT (t->binfo.vptr_field);

	WT (t->binfo.vtable);
	WT (t->binfo.virtuals);
	WT (t->binfo.vtt_subvtt);
	WT (t->binfo.vtt_vptr);

	tree_vec (BINFO_BASE_ACCESSES (t));
	unsigned num = vec_safe_length (BINFO_BASE_ACCESSES (t));
	for (unsigned ix = 0; ix != num; ix++)
	  WT (BINFO_BASE_BINFO (t, ix));
      }
      break;

    case TREE_LIST:
      WT (t->list.purpose);
      WT (t->list.value);
      WT (t->list.common.chain);
      break;

    case TREE_VEC:
      for (unsigned ix = TREE_VEC_LENGTH (t); ix--;)
	WT (TREE_VEC_ELT (t, ix));
      /* We stash NON_DEFAULT_TEMPLATE_ARGS_COUNT on TREE_CHAIN!  */
      gcc_checking_assert (!t->type_common.common.chain
			   || (TREE_CODE (t->type_common.common.chain)
			       == INTEGER_CST));
      WT (t->type_common.common.chain);
      break;

      /* C++-specific nodes ...  */
    case BASELINK:
      WT (((lang_tree_node *)t)->baselink.binfo);
      WT (((lang_tree_node *)t)->baselink.functions);
      WT (((lang_tree_node *)t)->baselink.access_binfo);
      break;

    case CONSTRAINT_INFO:
      WT (((lang_tree_node *)t)->constraint_info.template_reqs);
      WT (((lang_tree_node *)t)->constraint_info.declarator_reqs);
      WT (((lang_tree_node *)t)->constraint_info.associated_constr);
      break;

    case DEFERRED_NOEXCEPT:
      WT (((lang_tree_node *)t)->deferred_noexcept.pattern);
      WT (((lang_tree_node *)t)->deferred_noexcept.args);
      break;

    case LAMBDA_EXPR:
      WT (((lang_tree_node *)t)->lambda_expression.capture_list);
      WT (((lang_tree_node *)t)->lambda_expression.this_capture);
      WT (((lang_tree_node *)t)->lambda_expression.extra_scope);
      WT (((lang_tree_node *)t)->lambda_expression.regen_info);
      WT (((lang_tree_node *)t)->lambda_expression.extra_args);
      /* pending_proxies is a parse-time thing.  */
      gcc_assert (!((lang_tree_node *)t)->lambda_expression.pending_proxies);
      if (state)
	state->write_location
	  (*this, ((lang_tree_node *)t)->lambda_expression.locus);
      if (streaming_p ())
	{
	  WU (((lang_tree_node *)t)->lambda_expression.default_capture_mode);
	  WU (((lang_tree_node *)t)->lambda_expression.discriminator_scope);
	  WU (((lang_tree_node *)t)->lambda_expression.discriminator_sig);
	}
      break;

    case OVERLOAD:
      WT (((lang_tree_node *)t)->overload.function);
      WT (t->common.chain);
      break;

    case PTRMEM_CST:
      WT (((lang_tree_node *)t)->ptrmem.member);
      break;

    case STATIC_ASSERT:
      WT (((lang_tree_node *)t)->static_assertion.condition);
      WT (((lang_tree_node *)t)->static_assertion.message);
      if (state)
	state->write_location
	  (*this, ((lang_tree_node *)t)->static_assertion.location);
      break;

    case TEMPLATE_DECL:
      /* Streamed with the template_decl node itself.  */
      gcc_checking_assert
      	(TREE_VISITED (((lang_tree_node *)t)->template_decl.arguments));
      gcc_checking_assert
	(TREE_VISITED (((lang_tree_node *)t)->template_decl.result));
      if (DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (t))
	WT (DECL_CHAIN (t));
      break;

    case TEMPLATE_INFO:
      {
	WT (((lang_tree_node *)t)->template_info.tmpl);
	WT (((lang_tree_node *)t)->template_info.args);
	WT (((lang_tree_node *)t)->template_info.partial);

	const auto *ac = (((lang_tree_node *)t)
			  ->template_info.deferred_access_checks);
	unsigned len = vec_safe_length (ac);
	if (streaming_p ())
	  u (len);
	if (len)
	  {
	    for (unsigned ix = 0; ix != len; ix++)
	      {
		const auto &m = (*ac)[ix];
		WT (m.binfo);
		WT (m.decl);
		WT (m.diag_decl);
		if (state)
		  state->write_location (*this, m.loc);
	      }
	  }
      }
      break;

    case TEMPLATE_PARM_INDEX:
      if (streaming_p ())
	{
	  WU (((lang_tree_node *)t)->tpi.index);
	  WU (((lang_tree_node *)t)->tpi.level);
	  WU (((lang_tree_node *)t)->tpi.orig_level);
	}
      WT (((lang_tree_node *)t)->tpi.decl);
      /* TEMPLATE_PARM_DESCENDANTS (AKA TREE_CHAIN) is an internal
	 cache, do not stream.  */
      break;

    case TRAIT_EXPR:
      WT (((lang_tree_node *)t)->trait_expression.type1);
      WT (((lang_tree_node *)t)->trait_expression.type2);
      if (streaming_p ())
	WU (((lang_tree_node *)t)->trait_expression.kind);
      break;
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      /* We want to stream the type of a expression-like nodes /after/
         we've streamed the operands.  The type often contains (bits
         of the) types of the operands, and with things like decltype
         and noexcept in play, we really want to stream the decls
         defining the type before we try and stream the type on its
         own.  Otherwise we can find ourselves trying to read in a
         decl, when we're already partially reading in a component of
         its type.  And that's bad.  */
      tree type = t->typed.type;
      unsigned prec = 0;

      switch (code)
	{
	default:
	  break;

	case TEMPLATE_DECL:
	  /* We fill in the template's type separately.  */
	  type = NULL_TREE;
	  break;

	case TYPE_DECL:
	  if (DECL_ORIGINAL_TYPE (t) && t == TYPE_NAME (type))
	    /* This is a typedef.  We set its type separately.  */
	    type = NULL_TREE;
	  break;

	case ENUMERAL_TYPE:
	  if (type && !ENUM_FIXED_UNDERLYING_TYPE_P (t))
	    {
	      /* Type is a restricted range integer type derived from the
		 integer_types.  Find the right one.  */
	      prec = TYPE_PRECISION (type);
	      tree name = DECL_NAME (TYPE_NAME (type));

	      for (unsigned itk = itk_none; itk--;)
		if (integer_types[itk]
		    && DECL_NAME (TYPE_NAME (integer_types[itk])) == name)
		  {
		    type = integer_types[itk];
		    break;
		  }
	      gcc_assert (type != t->typed.type);
	    }
	  break;
	}

      WT (type);
      if (prec && streaming_p ())
	WU (prec);
    }

  if (TREE_CODE (t) == CONSTRUCTOR)
    {
      unsigned len = vec_safe_length (t->constructor.elts);
      if (streaming_p ())
	WU (len);
      if (len)
	for (unsigned ix = 0; ix != len; ix++)
	  {
	    const constructor_elt &elt = (*t->constructor.elts)[ix];

	    WT (elt.index);
	    WT (elt.value);
	  }
    }

#undef WT
#undef WU
}

// Streaming in a reference to a decl can cause that decl to be
// TREE_USED, which is the mark_used behaviour we need most of the
// time.  The trees_in::unused can be incremented to inhibit this,
// which is at least needed for vtables.

bool
trees_in::core_vals (tree t)
{
#define RU(X) ((X) = u ())
#define RUC(T,X) ((X) = T (u ()))
#define RT(X) ((X) = tree_node ())
#define RTU(X) ((X) = tree_node (true))
  tree_code code = TREE_CODE (t);

  /* First by tree shape.  */
  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      RT (t->decl_minimal.name);
      if (!DECL_TEMPLATE_PARM_P (t))
	RT (t->decl_minimal.context);

      /* Don't zap the locus just yet, we don't record it correctly
	 and thus lose all location information.  */
      t->decl_minimal.locus = state->read_location (*this);
      if (has_warning_spec (t))
	put_warning_spec (t, u ());
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      RT (t->type_common.name);
      RT (t->type_common.context);

      RT (t->type_common.main_variant);
      RT (t->type_common.canonical);

      /* type_common.next_variant is internally manipulated.  */
      /* type_common.pointer_to, type_common.reference_to.  */

      RU (t->type_common.precision);
      RU (t->type_common.contains_placeholder_bits);
      RUC (machine_mode, t->type_common.mode);
      RU (t->type_common.align);

      if (!RECORD_OR_UNION_CODE_P (code))
	{
	  RT (t->type_common.size);
	  RT (t->type_common.size_unit);
	}
      RT (t->type_common.attributes);

      RT (t->type_common.common.chain); /* TYPE_STUB_DECL.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      RUC (machine_mode, t->decl_common.mode);
      RU (t->decl_common.off_align);
      RU (t->decl_common.align);

      if (code != TEMPLATE_DECL)
	{
	  RT (t->decl_common.size);
	  RT (t->decl_common.size_unit);
	}

      RT (t->decl_common.attributes);
      RT (t->decl_common.abstract_origin);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      RT (t->decl_with_vis.assembler_name);
      RUC (symbol_visibility, t->decl_with_vis.visibility);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      if (code == ENUMERAL_TYPE)
	{
	  /* These fields get set even for opaque enums that lack a
	     definition, so we stream them directly for each ENUMERAL_TYPE.
	     We stream TYPE_VALUES as part of the definition.  */
	  RT (t->type_non_common.maxval);
	  RT (t->type_non_common.minval);
	}
      /* Records and unions hold FIELDS, VFIELD & BINFO on these
	 things.  */
      else if (!RECORD_OR_UNION_CODE_P (code))
	{
	  /* This is not clobbering TYPE_CACHED_VALUES, because this
	     is a type that doesn't have any.  */
	  gcc_checking_assert (!TYPE_CACHED_VALUES_P (t));
	  RT (t->type_non_common.values);
	  RT (t->type_non_common.maxval);
	  RT (t->type_non_common.minval);
	}

      RT (t->type_non_common.lang_1);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    {
      t->exp.locus = state->read_location (*this);
      if (has_warning_spec (t))
	put_warning_spec (t, u ());

      bool vl = TREE_CODE_CLASS (code) == tcc_vl_exp;
      for (unsigned limit = (vl ? VL_EXP_OPERAND_LENGTH (t)
			     : TREE_OPERAND_LENGTH (t)),
	     ix = unsigned (vl); ix != limit; ix++)
	RTU (TREE_OPERAND (t, ix));
    }

  /* Then by CODE.  Special cases and/or 1:1 tree shape
     correspondance. */
  switch (code)
    {
    default:
      break;

    case ARGUMENT_PACK_SELECT:
    case DEFERRED_PARSE:
    case IDENTIFIER_NODE:
    case BINDING_VECTOR:
    case SSA_NAME:
    case TRANSLATION_UNIT_DECL:
    case USERDEF_LITERAL:
      return false; /* Should never meet.  */

      /* Constants.  */
    case COMPLEX_CST:
      RT (TREE_REALPART (t));
      RT (TREE_IMAGPART (t));
      break;

    case FIXED_CST:
      /* Not suported in C++.  */
      return false;

    case INTEGER_CST:
      {
	unsigned num = TREE_INT_CST_EXT_NUNITS (t);
	for (unsigned ix = 0; ix != num; ix++)
	  TREE_INT_CST_ELT (t, ix) = wu ();
      }
      break;

    case POLY_INT_CST:
      for (unsigned ix = 0; ix != NUM_POLY_INT_COEFFS; ix++)
	RT (POLY_INT_CST_COEFF (t, ix));
      break;

    case REAL_CST:
      if (const void *bytes = buf (sizeof (real_value)))
	memcpy (TREE_REAL_CST_PTR (t), bytes, sizeof (real_value));
      break;

    case STRING_CST:
      /* Streamed during start.  */
      break;

    case VECTOR_CST:
      for (unsigned ix = vector_cst_encoded_nelts (t); ix--;)
	RT (VECTOR_CST_ENCODED_ELT (t, ix));
      break;

      /* Decls.  */
    case VAR_DECL:
      if (DECL_CONTEXT (t)
	  && TREE_CODE (DECL_CONTEXT (t)) != FUNCTION_DECL)
	break;
      /* FALLTHROUGH  */

    case RESULT_DECL:
    case PARM_DECL:
      if (DECL_HAS_VALUE_EXPR_P (t))
	{
	  /* The DECL_VALUE hash table is a cache, thus if we're
	     reading a duplicate (which we end up discarding), the
	     value expr will also be cleaned up at the next gc.  */
	  tree val = tree_node ();
	  SET_DECL_VALUE_EXPR (t, val);
	}
      /* FALLTHROUGH  */

    case CONST_DECL:
    case IMPORTED_DECL:
      RT (t->decl_common.initial);
      break;

    case FIELD_DECL:
      RT (t->field_decl.offset);
      RT (t->field_decl.bit_field_type);
      RT (t->field_decl.qualifier);
      RT (t->field_decl.bit_offset);
      RT (t->field_decl.fcontext);
      RT (t->decl_common.initial);
      break;

    case LABEL_DECL:
      RU (t->label_decl.label_decl_uid);
      RU (t->label_decl.eh_landing_pad_nr);
      break;

    case FUNCTION_DECL:
      {
	unsigned bltin = u ();
	t->function_decl.built_in_class = built_in_class (bltin);
	if (bltin != NOT_BUILT_IN)
	  {
	    bltin = u ();
	    DECL_UNCHECKED_FUNCTION_CODE (t) = built_in_function (bltin);
	  }

	RT (t->function_decl.personality);
	RT (t->function_decl.function_specific_target);
	RT (t->function_decl.function_specific_optimization);
	RT (t->function_decl.vindex);

	if (DECL_HAS_DEPENDENT_EXPLICIT_SPEC_P (t))
	  {
	    tree spec;
	    RT (spec);
	    store_explicit_specifier (t, spec);
	  }
      }
      break;

    case USING_DECL:
      /* USING_DECL_DECLS  */
      RT (t->decl_common.initial);
      /* FALLTHROUGH  */

    case TYPE_DECL:
      /* USING_DECL: USING_DECL_SCOPE  */
      /* TYPE_DECL: DECL_ORIGINAL_TYPE */
      RT (t->decl_non_common.result);
      break;

      /* Miscellaneous common nodes.  */
    case BLOCK:
      t->block.locus = state->read_location (*this);
      t->block.end_locus = state->read_location (*this);

      for (tree *chain = &t->block.vars;;)
	if (tree decl = tree_node ())
	  {
	    /* For a deduplicated local type or enumerator, chain the
	       duplicate decl instead of the canonical in-TU decl.  Seeing
	       a duplicate here means the containing function whose body
	       we're streaming in is a duplicate too, so we'll end up
	       discarding this BLOCK (and the rest of the duplicate function
	       body) anyway.  */
	    decl = maybe_duplicate (decl);

	    if (!DECL_P (decl) || DECL_CHAIN (decl))
	      {
		set_overrun ();
		break;
	      }
	    *chain = decl;
	    chain = &DECL_CHAIN (decl);
	  }
	else
	  break;

      /* nonlocalized_vars is middle-end.  */
      RT (t->block.subblocks);
      RT (t->block.supercontext);
      RT (t->block.abstract_origin);
      /* fragment_origin, fragment_chain are middle-end.  */
      RT (t->block.chain);
      /* nonlocalized_vars, block_num, die are middle endy/debug
	 things.  */
      break;

    case CALL_EXPR:
      RUC (internal_fn, t->base.u.ifn);
      break;

    case CONSTRUCTOR:
      // Streamed after the node's type.
      break;

    case OMP_CLAUSE:
      {
	RU (t->omp_clause.subcode.map_kind);
	t->omp_clause.locus = state->read_location (*this);

	unsigned len = omp_clause_num_ops[OMP_CLAUSE_CODE (t)];
	for (unsigned ix = 0; ix != len; ix++)
	  RT (t->omp_clause.ops[ix]);
      }
      break;

    case STATEMENT_LIST:
      {
	tree_stmt_iterator iter = tsi_start (t);
	for (tree stmt; RT (stmt);)
	  {
	    if (TREE_CODE (stmt) == DEBUG_BEGIN_STMT
		&& !MAY_HAVE_DEBUG_MARKER_STMTS)
	      continue;
	    tsi_link_after (&iter, stmt, TSI_CONTINUE_LINKING);
	  }
      }
      break;

    case OPTIMIZATION_NODE:
    case TARGET_OPTION_NODE:
      /* Not yet implemented, see trees_out::core_vals.  */
      gcc_unreachable ();
      break;

    case TREE_BINFO:
      RT (t->binfo.common.chain);
      RT (t->binfo.offset);
      RT (t->binfo.inheritance);
      RT (t->binfo.vptr_field);

      /* Do not mark the vtables as USED in the address expressions
	 here.  */
      unused++;
      RT (t->binfo.vtable);
      RT (t->binfo.virtuals);
      RT (t->binfo.vtt_subvtt);
      RT (t->binfo.vtt_vptr);
      unused--;

      BINFO_BASE_ACCESSES (t) = tree_vec ();
      if (!get_overrun ())
	{
	  unsigned num = vec_safe_length (BINFO_BASE_ACCESSES (t));
	  for (unsigned ix = 0; ix != num; ix++)
	    BINFO_BASE_APPEND (t, tree_node ());
	}
      break;

    case TREE_LIST:
      RT (t->list.purpose);
      RT (t->list.value);
      RT (t->list.common.chain);
      break;

    case TREE_VEC:
      for (unsigned ix = TREE_VEC_LENGTH (t); ix--;)
	RT (TREE_VEC_ELT (t, ix));
      RT (t->type_common.common.chain);
      break;

      /* C++-specific nodes ...  */
    case BASELINK:
      RT (((lang_tree_node *)t)->baselink.binfo);
      RTU (((lang_tree_node *)t)->baselink.functions);
      RT (((lang_tree_node *)t)->baselink.access_binfo);
      break;

    case CONSTRAINT_INFO:
      RT (((lang_tree_node *)t)->constraint_info.template_reqs);
      RT (((lang_tree_node *)t)->constraint_info.declarator_reqs);
      RT (((lang_tree_node *)t)->constraint_info.associated_constr);
      break;

    case DEFERRED_NOEXCEPT:
      RT (((lang_tree_node *)t)->deferred_noexcept.pattern);
      RT (((lang_tree_node *)t)->deferred_noexcept.args);
      break;

    case LAMBDA_EXPR:
      RT (((lang_tree_node *)t)->lambda_expression.capture_list);
      RT (((lang_tree_node *)t)->lambda_expression.this_capture);
      RT (((lang_tree_node *)t)->lambda_expression.extra_scope);
      RT (((lang_tree_node *)t)->lambda_expression.regen_info);
      RT (((lang_tree_node *)t)->lambda_expression.extra_args);
      /* lambda_expression.pending_proxies is NULL  */
      ((lang_tree_node *)t)->lambda_expression.locus
	= state->read_location (*this);
      RUC (cp_lambda_default_capture_mode_type,
	   ((lang_tree_node *)t)->lambda_expression.default_capture_mode);
      RU (((lang_tree_node *)t)->lambda_expression.discriminator_scope);
      RU (((lang_tree_node *)t)->lambda_expression.discriminator_sig);
      break;

    case OVERLOAD:
      RT (((lang_tree_node *)t)->overload.function);
      RT (t->common.chain);
      break;

    case PTRMEM_CST:
      RT (((lang_tree_node *)t)->ptrmem.member);
      break;

    case STATIC_ASSERT:
      RT (((lang_tree_node *)t)->static_assertion.condition);
      RT (((lang_tree_node *)t)->static_assertion.message);
      ((lang_tree_node *)t)->static_assertion.location
	= state->read_location (*this);
      break;

    case TEMPLATE_DECL:
      /* Streamed when reading the raw template decl itself.  */
      gcc_assert (((lang_tree_node *)t)->template_decl.arguments);
      gcc_assert (((lang_tree_node *)t)->template_decl.result);
      if (DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (t))
	RT (DECL_CHAIN (t));
      break;

    case TEMPLATE_INFO:
      RT (((lang_tree_node *)t)->template_info.tmpl);
      RT (((lang_tree_node *)t)->template_info.args);
      RT (((lang_tree_node *)t)->template_info.partial);
      if (unsigned len = u ())
	{
	  auto &ac = (((lang_tree_node *)t)
		      ->template_info.deferred_access_checks);
	  vec_alloc (ac, len);
	  for (unsigned ix = 0; ix != len; ix++)
	    {
	      deferred_access_check m;

	      RT (m.binfo);
	      RT (m.decl);
	      RT (m.diag_decl);
	      m.loc = state->read_location (*this);
	      ac->quick_push (m);
	    }
	}
      break;

    case TEMPLATE_PARM_INDEX:
      RU (((lang_tree_node *)t)->tpi.index);
      RU (((lang_tree_node *)t)->tpi.level);
      RU (((lang_tree_node *)t)->tpi.orig_level);
      RT (((lang_tree_node *)t)->tpi.decl);
      break;

    case TRAIT_EXPR:
      RT (((lang_tree_node *)t)->trait_expression.type1);
      RT (((lang_tree_node *)t)->trait_expression.type2);
      RUC (cp_trait_kind, ((lang_tree_node *)t)->trait_expression.kind);
      break;
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      tree type = tree_node ();

      if (type && code == ENUMERAL_TYPE && !ENUM_FIXED_UNDERLYING_TYPE_P (t))
	{
	  unsigned precision = u ();

	  type = build_distinct_type_copy (type);
	  TYPE_PRECISION (type) = precision;
	  set_min_and_max_values_for_integral_type (type, precision,
						    TYPE_SIGN (type));
	}

      if (code != TEMPLATE_DECL)
	t->typed.type = type;
    }

  if (TREE_CODE (t) == CONSTRUCTOR)
    if (unsigned len = u ())
      {
	vec_alloc (t->constructor.elts, len);
	for (unsigned ix = 0; ix != len; ix++)
	  {
	    constructor_elt elt;

	    RT (elt.index);
	    RTU (elt.value);
	    t->constructor.elts->quick_push (elt);
	  }
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
    default:
      gcc_unreachable ();

    case lds_fn:  /* lang_decl_fn.  */
      if (streaming_p ())
	{
	  if (DECL_NAME (t) && IDENTIFIER_OVL_OP_P (DECL_NAME (t)))
	    WU (lang->u.fn.ovl_op_code);
	}

      if (DECL_CLASS_SCOPE_P (t))
	WT (lang->u.fn.context);

      if (lang->u.fn.thunk_p)
	{
	  /* The thunked-to function.  */
	  WT (lang->u.fn.befriending_classes);
	  if (streaming_p ())
	    wi (lang->u.fn.u5.fixed_offset);
	}
      else if (decl_tls_wrapper_p (t))
	/* The wrapped variable.  */
	WT (lang->u.fn.befriending_classes);
      else
	WT (lang->u.fn.u5.cloned_function);

      if (FNDECL_USED_AUTO (t))
	WT (lang->u.fn.u.saved_auto_return_type);

      goto lds_min;

    case lds_decomp:  /* lang_decl_decomp.  */
      WT (lang->u.decomp.base);
      goto lds_min;

    case lds_min:  /* lang_decl_min.  */
    lds_min:
      WT (lang->u.min.template_info);
      {
	tree access = lang->u.min.access;

	/* DECL_ACCESS needs to be maintained by the definition of the
	   (derived) class that changes the access.  The other users
	   of DECL_ACCESS need to write it here.  */
	if (!DECL_THUNK_P (t)
	    && (DECL_CONTEXT (t) && TYPE_P (DECL_CONTEXT (t))))
	  access = NULL_TREE;

	WT (access);
      }
      break;

    case lds_ns:  /* lang_decl_ns.  */
      break;

    case lds_parm:  /* lang_decl_parm.  */
      if (streaming_p ())
	{
	  WU (lang->u.parm.level);
	  WU (lang->u.parm.index);
	}
      break;
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
    default:
      gcc_unreachable ();

    case lds_fn:  /* lang_decl_fn.  */
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

      if (DECL_CLASS_SCOPE_P (t))
	RT (lang->u.fn.context);

      if (lang->u.fn.thunk_p)
	{
	  RT (lang->u.fn.befriending_classes);
	  lang->u.fn.u5.fixed_offset = wi ();
	}
      else if (decl_tls_wrapper_p (t))
	RT (lang->u.fn.befriending_classes);
      else
	RT (lang->u.fn.u5.cloned_function);

      if (FNDECL_USED_AUTO (t))
	RT (lang->u.fn.u.saved_auto_return_type);
      goto lds_min;

    case lds_decomp:  /* lang_decl_decomp.  */
      RT (lang->u.decomp.base);
      goto lds_min;

    case lds_min:  /* lang_decl_min.  */
    lds_min:
      RT (lang->u.min.template_info);
      RT (lang->u.min.access);
      break;

    case lds_ns:  /* lang_decl_ns.  */
      break;

    case lds_parm:  /* lang_decl_parm.  */
      RU (lang->u.parm.level);
      RU (lang->u.parm.index);
      break;
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
  if (streaming_p ())
    WU (lang->align);
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
#undef RU
#undef RT
  return !get_overrun ();
}

/* Write out the bools of T, including information about any
   LANG_SPECIFIC information.  Including allocation of any lang
   specific object.  */

void
trees_out::tree_node_bools (tree t)
{
  gcc_checking_assert (streaming_p ());

  /* We should never stream a namespace.  */
  gcc_checking_assert (TREE_CODE (t) != NAMESPACE_DECL
		       || DECL_NAMESPACE_ALIAS (t));

  bits_out bits = stream_bits ();
  core_bools (t, bits);

  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case tcc_declaration:
      {
	bool specific = DECL_LANG_SPECIFIC (t) != NULL;
	bits.b (specific);
	if (specific && VAR_P (t))
	  bits.b (DECL_DECOMPOSITION_P (t));
	if (specific)
	  lang_decl_bools (t, bits);
      }
      break;

    case tcc_type:
      {
	bool specific = (TYPE_MAIN_VARIANT (t) == t
			 && TYPE_LANG_SPECIFIC (t) != NULL);
	gcc_assert (TYPE_LANG_SPECIFIC (t)
		    == TYPE_LANG_SPECIFIC (TYPE_MAIN_VARIANT (t)));

	bits.b (specific);
	if (specific)
	  lang_type_bools (t, bits);
      }
      break;

    default:
      break;
    }

  bits.bflush ();
}

bool
trees_in::tree_node_bools (tree t)
{
  bits_in bits = stream_bits ();
  bool ok = core_bools (t, bits);

  if (ok)
    switch (TREE_CODE_CLASS (TREE_CODE (t)))
      {
      case tcc_declaration:
	if (bits.b ())
	  {
	    bool decomp = VAR_P (t) && bits.b ();

	    ok = maybe_add_lang_decl_raw (t, decomp);
	    if (ok)
	      ok = lang_decl_bools (t, bits);
	  }
	break;

      case tcc_type:
	if (bits.b ())
	  {
	    ok = maybe_add_lang_type_raw (t);
	    if (ok)
	      ok = lang_type_bools (t, bits);
	  }
	break;

      default:
	break;
      }

  bits.bflush ();
  if (!ok || get_overrun ())
    return false;

  return true;
}


/* Write out the lang-specifc vals of node T.  */

void
trees_out::lang_vals (tree t)
{
  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case tcc_declaration:
      if (DECL_LANG_SPECIFIC (t))
	lang_decl_vals (t);
      break;

    case tcc_type:
      if (TYPE_MAIN_VARIANT (t) == t && TYPE_LANG_SPECIFIC (t))
	lang_type_vals (t);
      break;

    default:
      break;
    }
}

bool
trees_in::lang_vals (tree t)
{
  bool ok = true;

  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case tcc_declaration:
      if (DECL_LANG_SPECIFIC (t))
	ok = lang_decl_vals (t);
      break;

    case tcc_type:
      if (TYPE_LANG_SPECIFIC (t))
	ok = lang_type_vals (t);
      else
	TYPE_LANG_SPECIFIC (t) = TYPE_LANG_SPECIFIC (TYPE_MAIN_VARIANT (t));
      break;

    default:
      break;
    }

  return ok;
}

/* Write out the value fields of node T.  */

void
trees_out::tree_node_vals (tree t)
{
  core_vals (t);
  lang_vals (t);
}

bool
trees_in::tree_node_vals (tree t)
{
  bool ok = core_vals (t);
  if (ok)
    ok = lang_vals (t);

  return ok;
}


/* If T is a back reference, fixed reference or NULL, write out its
   code and return WK_none.  Otherwise return WK_value if we must write
   by value, or WK_normal otherwise.  */

walk_kind
trees_out::ref_node (tree t)
{
  if (!t)
    {
      if (streaming_p ())
	{
	  /* NULL_TREE -> tt_null.  */
	  null_count++;
	  i (tt_null);
	}
      return WK_none;
    }

  if (!TREE_VISITED (t))
    return WK_normal;

  /* An already-visited tree.  It must be in the map.  */
  int val = get_tag (t);

  if (val == tag_value)
    /* An entry we should walk into.  */
    return WK_value;

  const char *kind;

  if (val <= tag_backref)
    {
      /* Back reference -> -ve number  */
      if (streaming_p ())
	i (val);
      kind = "backref";
    }
  else if (val >= tag_fixed)
    {
      /* Fixed reference -> tt_fixed */
      val -= tag_fixed;
      if (streaming_p ())
	i (tt_fixed), u (val);
      kind = "fixed";
    }

  if (streaming_p ())
    {
      back_ref_count++;
      dump (dumper::TREE)
	&& dump ("Wrote %s:%d %C:%N%S", kind, val, TREE_CODE (t), t, t);
    }
  return WK_none;
}

tree
trees_in::back_ref (int tag)
{
  tree res = NULL_TREE;

  if (tag < 0 && unsigned (~tag) < back_refs.length ())
    res = back_refs[~tag];

  if (!res
      /* Checking TREE_CODE is a dereference, so we know this is not a
	 wild pointer.  Checking the code provides evidence we've not
	 corrupted something.  */
      || TREE_CODE (res) >= MAX_TREE_CODES)
    set_overrun ();
  else
    dump (dumper::TREE) && dump ("Read backref:%d found %C:%N%S", tag,
				 TREE_CODE (res), res, res);
  return res;
}

unsigned
trees_out::add_indirect_tpl_parms (tree parms)
{
  unsigned len = 0;
  for (; parms; parms = TREE_CHAIN (parms), len++)
    {
      if (TREE_VISITED (parms))
	break;

      int tag = insert (parms);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Indirect:%d template's parameter %u %C:%N",
		   tag, len, TREE_CODE (parms), parms);
    }

  if (streaming_p ())
    u (len);

  return len;
}

unsigned
trees_in::add_indirect_tpl_parms (tree parms)
{
  unsigned len = u ();
  for (unsigned ix = 0; ix != len; parms = TREE_CHAIN (parms), ix++)
    {
      int tag = insert (parms);
      dump (dumper::TREE)
	&& dump ("Indirect:%d template's parameter %u %C:%N",
		 tag, ix, TREE_CODE (parms), parms);
    }

  return len;
}

/* We've just found DECL by name.  Insert nodes that come with it, but
   cannot be found by name, so we'll not accidentally walk into them.  */

void
trees_out::add_indirects (tree decl)
{
  unsigned count = 0;

  // FIXME:OPTIMIZATION We'll eventually want default fn parms of
  // templates and perhaps default template parms too.  The former can
  // be referenced from instantiations (as they are lazily
  // instantiated).  Also (deferred?) exception specifications of
  // templates.  See the note about PARM_DECLs in trees_out::decl_node.
  tree inner = decl;
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      count += add_indirect_tpl_parms (DECL_TEMPLATE_PARMS (decl));

      inner = DECL_TEMPLATE_RESULT (decl);
      int tag = insert (inner);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Indirect:%d template's result %C:%N",
		   tag, TREE_CODE (inner), inner);
      count++;
    }

  if (TREE_CODE (inner) == TYPE_DECL)
    {
      /* Make sure the type is in the map too.  Otherwise we get
	 different RECORD_TYPEs for the same type, and things go
	 south.  */
      tree type = TREE_TYPE (inner);
      gcc_checking_assert (DECL_ORIGINAL_TYPE (inner)
			   || TYPE_NAME (type) == inner);
      int tag = insert (type);
      if (streaming_p ())
	dump (dumper::TREE) && dump ("Indirect:%d decl's type %C:%N", tag,
				     TREE_CODE (type), type);
      count++;
    }

  if (streaming_p ())
    {
      u (count);
      dump (dumper::TREE) && dump ("Inserted %u indirects", count);
    }
}

bool
trees_in::add_indirects (tree decl)
{
  unsigned count = 0;

  tree inner = decl;
  if (TREE_CODE (inner) == TEMPLATE_DECL)
    {
      count += add_indirect_tpl_parms (DECL_TEMPLATE_PARMS (decl));

      inner = DECL_TEMPLATE_RESULT (decl);
      int tag = insert (inner);
      dump (dumper::TREE)
	&& dump ("Indirect:%d templates's result %C:%N", tag,
		 TREE_CODE (inner), inner);
      count++;
    }

  if (TREE_CODE (inner) == TYPE_DECL)
    {
      tree type = TREE_TYPE (inner);
      gcc_checking_assert (DECL_ORIGINAL_TYPE (inner)
			   || TYPE_NAME (type) == inner);
      int tag = insert (type);
      dump (dumper::TREE)
	&& dump ("Indirect:%d decl's type %C:%N", tag, TREE_CODE (type), type);
      count++;
    }

  dump (dumper::TREE) && dump ("Inserted %u indirects", count);
  return count == u ();
}

/* Stream a template parameter.  There are 4.5 kinds of parameter:
   a) Template - TEMPLATE_DECL->TYPE_DECL->TEMPLATE_TEMPLATE_PARM
   	TEMPLATE_TYPE_PARM_INDEX TPI
   b) Type - TYPE_DECL->TEMPLATE_TYPE_PARM TEMPLATE_TYPE_PARM_INDEX TPI
   c.1) NonTYPE - PARM_DECL DECL_INITIAL TPI We meet this first
   c.2) NonTYPE - CONST_DECL DECL_INITIAL Same TPI
   d) BoundTemplate - TYPE_DECL->BOUND_TEMPLATE_TEMPLATE_PARM
       TEMPLATE_TYPE_PARM_INDEX->TPI
       TEMPLATE_TEMPLATE_PARM_INFO->TEMPLATE_INFO

   All of these point to a TEMPLATE_PARM_INDEX, and #B also has a TEMPLATE_INFO
*/

void
trees_out::tpl_parm_value (tree parm)
{
  gcc_checking_assert (DECL_P (parm) && DECL_TEMPLATE_PARM_P (parm));

  int parm_tag = insert (parm);
  if (streaming_p ())
    {
      i (tt_tpl_parm);
      dump (dumper::TREE) && dump ("Writing template parm:%d %C:%N",
				   parm_tag, TREE_CODE (parm), parm);
      start (parm);
      tree_node_bools (parm);
    }

  tree inner = parm;
  if (TREE_CODE (inner) == TEMPLATE_DECL)
    {
      inner = DECL_TEMPLATE_RESULT (inner);
      int inner_tag = insert (inner);
      if (streaming_p ())
	{
	  dump (dumper::TREE) && dump ("Writing inner template parm:%d %C:%N",
				       inner_tag, TREE_CODE (inner), inner);
	  start (inner);
	  tree_node_bools (inner);
	}
    }

  tree type = NULL_TREE;
  if (TREE_CODE (inner) == TYPE_DECL)
    {
      type = TREE_TYPE (inner);
      int type_tag = insert (type);
      if (streaming_p ())
	{
	  dump (dumper::TREE) && dump ("Writing template parm type:%d %C:%N",
				       type_tag, TREE_CODE (type), type);
	  start (type);
	  tree_node_bools (type);
	}
    }

  if (inner != parm)
    {
      /* This is a template-template parameter.  */
      unsigned tpl_levels = 0;
      tpl_header (parm, &tpl_levels);
      tpl_parms_fini (parm, tpl_levels);
    }

  tree_node_vals (parm);
  if (inner != parm)
    tree_node_vals (inner);
  if (type)
    {
      tree_node_vals (type);
      if (DECL_NAME (inner) == auto_identifier
	  || DECL_NAME (inner) == decltype_auto_identifier)
	{
	  /* Placeholder auto.  */
	  tree_node (DECL_INITIAL (inner));
	  tree_node (DECL_SIZE_UNIT (inner));
	}
    }

  if (streaming_p ())
    dump (dumper::TREE) && dump ("Wrote template parm:%d %C:%N",
				 parm_tag, TREE_CODE (parm), parm);
}

tree
trees_in::tpl_parm_value ()
{
  tree parm = start ();
  if (!parm || !tree_node_bools (parm))
    return NULL_TREE;

  int parm_tag = insert (parm);
  dump (dumper::TREE) && dump ("Reading template parm:%d %C:%N",
			       parm_tag, TREE_CODE (parm), parm);

  tree inner = parm;
  if (TREE_CODE (inner) == TEMPLATE_DECL)
    {
      inner = start ();
      if (!inner || !tree_node_bools (inner))
	return NULL_TREE;
      int inner_tag = insert (inner);
      dump (dumper::TREE) && dump ("Reading inner template parm:%d %C:%N",
				   inner_tag, TREE_CODE (inner), inner);
      DECL_TEMPLATE_RESULT (parm) = inner;
    }

  tree type = NULL_TREE;
  if (TREE_CODE (inner) == TYPE_DECL)
    {
      type = start ();
      if (!type || !tree_node_bools (type))
	return NULL_TREE;
      int type_tag = insert (type);
      dump (dumper::TREE) && dump ("Reading template parm type:%d %C:%N",
				   type_tag, TREE_CODE (type), type);

      TREE_TYPE (inner) = TREE_TYPE (parm) = type;
      TYPE_NAME (type) = parm;
    }

  if (inner != parm)
    {
      /* A template template parameter.  */
      unsigned tpl_levels = 0;
      tpl_header (parm, &tpl_levels);
      tpl_parms_fini (parm, tpl_levels);
    }

  tree_node_vals (parm);
  if (inner != parm)
    tree_node_vals (inner);
  if (type)
    {
      tree_node_vals (type);
      if (DECL_NAME (inner) == auto_identifier
	  || DECL_NAME (inner) == decltype_auto_identifier)
	{
	  /* Placeholder auto.  */
	  DECL_INITIAL (inner) = tree_node ();
	  DECL_SIZE_UNIT (inner) = tree_node ();
	}
      if (TYPE_CANONICAL (type))
	{
	  gcc_checking_assert (TYPE_CANONICAL (type) == type);
	  TYPE_CANONICAL (type) = canonical_type_parameter (type);
	}
    }

  dump (dumper::TREE) && dump ("Read template parm:%d %C:%N",
			       parm_tag, TREE_CODE (parm), parm);

  return parm;
}

void
trees_out::install_entity (tree decl, depset *dep)
{
  gcc_checking_assert (streaming_p ());

  /* Write the entity index, so we can insert it as soon as we
     know this is new.  */
  u (dep ? dep->cluster + 1 : 0);
  if (CHECKING_P && dep)
    {
      /* Add it to the entity map, such that we can tell it is
	 part of us.  */
      bool existed;
      unsigned *slot = &entity_map->get_or_insert
	(DECL_UID (decl), &existed);
      if (existed)
	/* If it existed, it should match.  */
	gcc_checking_assert (decl == (*entity_ary)[*slot]);
      *slot = ~dep->cluster;
    }
}

bool
trees_in::install_entity (tree decl)
{
  unsigned entity_index = u ();
  if (!entity_index)
    return false;

  if (entity_index > state->entity_num)
    {
      set_overrun ();
      return false;
    }

  /* Insert the real decl into the entity ary.  */
  unsigned ident = state->entity_lwm + entity_index - 1;
  (*entity_ary)[ident] = decl;

  /* And into the entity map, if it's not already there.  */
  tree not_tmpl = STRIP_TEMPLATE (decl);
  if (!DECL_LANG_SPECIFIC (not_tmpl)
      || !DECL_MODULE_ENTITY_P (not_tmpl))
    {
      retrofit_lang_decl (not_tmpl);
      DECL_MODULE_ENTITY_P (not_tmpl) = true;

      /* Insert into the entity hash (it cannot already be there).  */
      bool existed;
      unsigned &slot = entity_map->get_or_insert (DECL_UID (decl), &existed);
      gcc_checking_assert (!existed);
      slot = ident;
    }
  else if (state->is_partition ())
    {
      /* The decl is already in the entity map, but we see it again now from a
	 partition: we want to overwrite if the original decl wasn't also from
	 a (possibly different) partition.  Otherwise, for things like template
	 instantiations, make_dependency might not realise that this is also
	 provided from a partition and should be considered part of this module
	 (and thus always emitted into the primary interface's CMI).  */
      unsigned *slot = entity_map->get (DECL_UID (decl));
      module_state *imp = import_entity_module (*slot);
      if (!imp->is_partition ())
	*slot = ident;
    }

  return true;
}

static bool has_definition (tree decl);

/* DECL is a decl node that must be written by value.  DEP is the
   decl's depset.  */

void
trees_out::decl_value (tree decl, depset *dep)
{
  /* We should not be writing clones or template parms.  */
  gcc_checking_assert (DECL_P (decl)
		       && !DECL_CLONED_FUNCTION_P (decl)
		       && !DECL_TEMPLATE_PARM_P (decl));

  /* We should never be writing non-typedef ptrmemfuncs by value.  */
  gcc_checking_assert (TREE_CODE (decl) != TYPE_DECL
		       || DECL_ORIGINAL_TYPE (decl)
		       || !TYPE_PTRMEMFUNC_P (TREE_TYPE (decl)));

  merge_kind mk = get_merge_kind (decl, dep);
  bool is_imported_temploid_friend = imported_temploid_friends->get (decl);

  if (CHECKING_P)
    {
      /* Never start in the middle of a template.  */
      int use_tpl = -1;
      if (tree ti = node_template_info (decl, use_tpl))
	gcc_checking_assert (TREE_CODE (TI_TEMPLATE (ti)) == OVERLOAD
			     || TREE_CODE (TI_TEMPLATE (ti)) == FIELD_DECL
			     || (DECL_TEMPLATE_RESULT (TI_TEMPLATE (ti))
				 != decl));
    }

  if (streaming_p ())
    {
      /* A new node -> tt_decl.  */
      decl_val_count++;
      i (tt_decl);
      u (mk);
      start (decl);

      if (mk != MK_unique)
	{
	  bits_out bits = stream_bits ();
	  if (!(mk & MK_template_mask) && !state->is_header ())
	    {
	      /* Tell the importer whether this is a global module entity,
		 or a module entity.  */
	      tree o = get_originating_module_decl (decl);
	      bool is_attached = false;

	      tree not_tmpl = STRIP_TEMPLATE (o);
	      if (DECL_LANG_SPECIFIC (not_tmpl)
		  && DECL_MODULE_ATTACH_P (not_tmpl))
		is_attached = true;

	      bits.b (is_attached);

	      /* Also tell the importer whether this is an imported temploid
		 friend, which has implications for merging.  */
	      bits.b (is_imported_temploid_friend);
	    }
	  bits.b (dep && dep->has_defn ());
	}
      tree_node_bools (decl);
    }

  int tag = insert (decl, WK_value);
  if (streaming_p ())
    dump (dumper::TREE)
      && dump ("Writing %s:%d %C:%N%S", merge_kind_name[mk], tag,
	       TREE_CODE (decl), decl, decl);

  tree inner = decl;
  int inner_tag = 0;
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      inner = DECL_TEMPLATE_RESULT (decl);
      inner_tag = insert (inner, WK_value);

      if (streaming_p ())
	{
	  int code = TREE_CODE (inner);
	  u (code);
	  start (inner, true);
	  tree_node_bools (inner);
	  dump (dumper::TREE)
	    && dump ("Writing %s:%d %C:%N%S", merge_kind_name[mk], inner_tag,
		     TREE_CODE (inner), inner, inner);
	}
    }

  tree type = NULL_TREE;
  int type_tag = 0;
  tree stub_decl = NULL_TREE;
  int stub_tag = 0;
  if (TREE_CODE (inner) == TYPE_DECL)
    {
      type = TREE_TYPE (inner);
      bool has_type = (type == TYPE_MAIN_VARIANT (type)
		       && TYPE_NAME (type) == inner);

      if (streaming_p ())
	u (has_type ? TREE_CODE (type) : 0);

      if (has_type)
	{
	  type_tag = insert (type, WK_value);
	  if (streaming_p ())
	    {
	      start (type, true);
	      tree_node_bools (type);
	      dump (dumper::TREE)
		&& dump ("Writing type:%d %C:%N", type_tag,
			 TREE_CODE (type), type);
	    }

	  stub_decl = TYPE_STUB_DECL (type);
	  bool has_stub = inner != stub_decl;
	  if (streaming_p ())
	    u (has_stub ? TREE_CODE (stub_decl) : 0);
	  if (has_stub)
	    {
	      stub_tag = insert (stub_decl);
	      if (streaming_p ())
		{
		  start (stub_decl, true);
		  tree_node_bools (stub_decl);
		  dump (dumper::TREE)
		    && dump ("Writing stub_decl:%d %C:%N", stub_tag,
			     TREE_CODE (stub_decl), stub_decl);
		}
	    }
	  else
	    stub_decl = NULL_TREE;
	}
      else
	/* Regular typedef.  */
	type = NULL_TREE;
    }

  /* Stream the container, we want it correctly canonicalized before
     we start emitting keys for this decl.  */
  tree container = decl_container (decl);
  unsigned tpl_levels = 0;

  {
    auto wmk = make_temp_override (dep_hash->writing_merge_key, true);
    if (decl != inner)
      tpl_header (decl, &tpl_levels);
    if (TREE_CODE (inner) == FUNCTION_DECL)
      fn_parms_init (inner);

    /* Now write out the merging information, and then really
       install the tag values.  */
    key_mergeable (tag, mk, decl, inner, container, dep);

    if (streaming_p ())
      dump (dumper::MERGE)
	&& dump ("Wrote:%d's %s merge key %C:%N", tag,
		 merge_kind_name[mk], TREE_CODE (decl), decl);
  }

  if (TREE_CODE (inner) == FUNCTION_DECL)
    fn_parms_fini (inner);

  if (!is_key_order ())
    tree_node_vals (decl);

  if (inner_tag)
    {
      if (!is_key_order ())
	tree_node_vals (inner);
      tpl_parms_fini (decl, tpl_levels);
    }

  if (type && !is_key_order ())
    {
      tree_node_vals (type);
      if (stub_decl)
	tree_node_vals (stub_decl);
    }

  if (!is_key_order ())
    {
      if (mk & MK_template_mask
	  || mk == MK_partial
	  || mk == MK_friend_spec)
	{
	  if (mk != MK_partial)
	    {
	      // FIXME: We should make use of the merge-key by
	      // exposing it outside of key_mergeable.  But this gets
	      // the job done.
	      auto *entry = reinterpret_cast <spec_entry *> (dep->deps[0]);

	      if (streaming_p ())
		u (get_mergeable_specialization_flags (mk & MK_tmpl_decl_mask,
						       entry->tmpl, decl));
	      tree_node (entry->tmpl);
	      tree_node (entry->args);
	    }
	  else
	    {
	      tree ti = get_template_info (inner);
	      tree_node (TI_TEMPLATE (ti));
	      tree_node (TI_ARGS (ti));
	    }
	}
      tree_node (get_constraints (decl));
    }

  if (streaming_p ())
    {
      /* Do not stray outside this section.  */
      gcc_checking_assert (!dep || dep->section == dep_hash->section);

      /* Write the entity index, so we can insert it as soon as we
	 know this is new.  */
      install_entity (decl, dep);
    }

  if (DECL_LANG_SPECIFIC (inner)
      && DECL_MODULE_KEYED_DECLS_P (inner)
      && !is_key_order ())
    {
      /* Stream the keyed entities.  */
      auto *attach_vec = keyed_table->get (inner);
      unsigned num = attach_vec->length ();
      if (streaming_p ())
	u (num);
      for (unsigned ix = 0; ix != num; ix++)
	{
	  tree attached = (*attach_vec)[ix];
	  tree_node (attached);
	  if (streaming_p ())
	    dump (dumper::MERGE)
	      && dump ("Written %d[%u] attached decl %N", tag, ix, attached);
	}
    }

  if (is_imported_temploid_friend)
    {
      /* Write imported temploid friends so that importers can reconstruct
	 this information on stream-in.  */
      tree* slot = imported_temploid_friends->get (decl);
      tree_node (*slot);
    }

  bool is_typedef = false;
  if (!type && TREE_CODE (inner) == TYPE_DECL)
    {
      tree t = TREE_TYPE (inner);
      unsigned tdef_flags = 0;
      if (DECL_ORIGINAL_TYPE (inner)
	  && TYPE_NAME (TREE_TYPE (inner)) == inner)
	{
	  tdef_flags |= 1;
	  if (TYPE_STRUCTURAL_EQUALITY_P (t)
	      && TYPE_DEPENDENT_P_VALID (t)
	      && TYPE_DEPENDENT_P (t))
	    tdef_flags |= 2;
	}
      if (streaming_p ())
	u (tdef_flags);

      if (tdef_flags & 1)
	{
	  /* A typedef type.  */
	  int type_tag = insert (t);
	  if (streaming_p ())
	    dump (dumper::TREE)
	      && dump ("Cloned:%d %s %C:%N", type_tag,
		       tdef_flags & 2 ? "depalias" : "typedef",
		       TREE_CODE (t), t);

	  is_typedef = true;
	}
    }

  if (streaming_p () && DECL_MAYBE_IN_CHARGE_CDTOR_P (decl))
    {
      bool cloned_p
	= (DECL_CHAIN (decl) && DECL_CLONED_FUNCTION_P (DECL_CHAIN (decl)));
      bool needs_vtt_parm_p
	= (cloned_p && CLASSTYPE_VBASECLASSES (DECL_CONTEXT (decl)));
      bool omit_inherited_parms_p
	= (cloned_p && DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (decl)
	   && base_ctor_omit_inherited_parms (decl));
      unsigned flags = (int (cloned_p) << 0
			| int (needs_vtt_parm_p) << 1
			| int (omit_inherited_parms_p) << 2);
      u (flags);
      dump (dumper::TREE) && dump ("CDTOR %N is %scloned",
				   decl, cloned_p ? "" : "not ");
    }

  if (streaming_p () && VAR_P (decl) && CP_DECL_THREAD_LOCAL_P (decl))
    u (decl_tls_model (decl));

  if (streaming_p ())
    dump (dumper::TREE) && dump ("Written decl:%d %C:%N", tag,
				 TREE_CODE (decl), decl);

  if (NAMESPACE_SCOPE_P (inner))
    gcc_checking_assert (!dep == (VAR_OR_FUNCTION_DECL_P (inner)
				  && DECL_LOCAL_DECL_P (inner)));
  else if ((TREE_CODE (inner) == TYPE_DECL
	    && !is_typedef
	    && TYPE_NAME (TREE_TYPE (inner)) == inner)
	   || TREE_CODE (inner) == FUNCTION_DECL)
    {
      bool write_defn = !dep && has_definition (decl);
      if (streaming_p ())
	u (write_defn);
      if (write_defn)
	write_definition (decl);
    }
}

tree
trees_in::decl_value ()
{
  int tag = 0;
  bool is_attached = false;
  bool is_imported_temploid_friend = false;
  bool has_defn = false;
  unsigned mk_u = u ();
  if (mk_u >= MK_hwm || !merge_kind_name[mk_u])
    {
      set_overrun ();
      return NULL_TREE;
    }

  unsigned saved_unused = unused;
  unused = 0;

  merge_kind mk = merge_kind (mk_u);

  tree decl = start ();
  if (decl)
    {
      if (mk != MK_unique)
	{
	  bits_in bits = stream_bits ();
	  if (!(mk & MK_template_mask) && !state->is_header ())
	    {
	      is_attached = bits.b ();
	      is_imported_temploid_friend = bits.b ();
	    }

	  has_defn = bits.b ();
	}

      if (!tree_node_bools (decl))
	decl = NULL_TREE;
    }

  /* Insert into map.  */
  tag = insert (decl);
  if (decl)
    dump (dumper::TREE)
      && dump ("Reading:%d %C", tag, TREE_CODE (decl));

  tree inner = decl;
  int inner_tag = 0;
  if (decl && TREE_CODE (decl) == TEMPLATE_DECL)
    {
      int code = u ();
      inner = start (code);
      if (inner && tree_node_bools (inner))
	DECL_TEMPLATE_RESULT (decl) = inner;
      else
	decl = NULL_TREE;

      inner_tag = insert (inner);
      if (decl)
	dump (dumper::TREE)
	  && dump ("Reading:%d %C", inner_tag, TREE_CODE (inner));
    }

  tree type = NULL_TREE;
  int type_tag = 0;
  tree stub_decl = NULL_TREE;
  int stub_tag = 0;
  if (decl && TREE_CODE (inner) == TYPE_DECL)
    {
      if (unsigned type_code = u ())
	{
	  type = start (type_code);
	  if (type && tree_node_bools (type))
	    {
	      TREE_TYPE (inner) = type;
	      TYPE_NAME (type) = inner;
	    }
	  else
	    decl = NULL_TREE;

	  type_tag = insert (type);
	  if (decl)
	    dump (dumper::TREE)
	      && dump ("Reading type:%d %C", type_tag, TREE_CODE (type));

	  if (unsigned stub_code = u ())
	    {
	      stub_decl = start (stub_code);
	      if (stub_decl && tree_node_bools (stub_decl))
		{
		  TREE_TYPE (stub_decl) = type;
		  TYPE_STUB_DECL (type) = stub_decl;
		}
	      else
		decl = NULL_TREE;

	      stub_tag = insert (stub_decl);
	      if (decl)
		dump (dumper::TREE)
		  && dump ("Reading stub_decl:%d %C", stub_tag,
			   TREE_CODE (stub_decl));
	    }
	}
    }

  if (!decl)
    {
    bail:
      if (inner_tag != 0)
	back_refs[~inner_tag] = NULL_TREE;
      if (type_tag != 0)
	back_refs[~type_tag] = NULL_TREE;
      if (stub_tag != 0)
	back_refs[~stub_tag] = NULL_TREE;
      if (tag != 0)
	back_refs[~tag] = NULL_TREE;
      set_overrun ();
      /* Bail.  */
      unused = saved_unused;
      return NULL_TREE;
    }

  /* Read the container, to ensure it's already been streamed in.  */
  tree container = decl_container ();
  unsigned tpl_levels = 0;

  /* Figure out if this decl is already known about.  */
  int parm_tag = 0;

  if (decl != inner)
    if (!tpl_header (decl, &tpl_levels))
      goto bail;
  if (TREE_CODE (inner) == FUNCTION_DECL)
    parm_tag = fn_parms_init (inner);

  tree existing = key_mergeable (tag, mk, decl, inner, type, container,
				 is_attached, is_imported_temploid_friend);
  tree existing_inner = existing;
  if (existing)
    {
      if (existing == error_mark_node)
	goto bail;

      if (TREE_CODE (STRIP_TEMPLATE (existing)) == TYPE_DECL)
	{
	  tree etype = TREE_TYPE (existing);
	  if (TYPE_LANG_SPECIFIC (etype)
	      && COMPLETE_TYPE_P (etype)
	      && !CLASSTYPE_MEMBER_VEC (etype))
	    /* Give it a member vec, we're likely gonna be looking
	       inside it.  */
	    set_class_bindings (etype, -1);
	}

      /* Install the existing decl into the back ref array.  */
      register_duplicate (decl, existing);
      back_refs[~tag] = existing;
      if (inner_tag != 0)
	{
	  existing_inner = DECL_TEMPLATE_RESULT (existing);
	  back_refs[~inner_tag] = existing_inner;
	}

      if (type_tag != 0)
	{
	  tree existing_type = TREE_TYPE (existing);
	  back_refs[~type_tag] = existing_type;
	  if (stub_tag != 0)
	    back_refs[~stub_tag] = TYPE_STUB_DECL (existing_type);
	}
    }

  if (parm_tag)
    fn_parms_fini (parm_tag, inner, existing_inner, has_defn);

  if (!tree_node_vals (decl))
    goto bail;

  if (inner_tag)
    {
      gcc_checking_assert (DECL_TEMPLATE_RESULT (decl) == inner);

      if (!tree_node_vals (inner))
	goto bail;

      if (!tpl_parms_fini (decl, tpl_levels))
	goto bail;
    }

  if (type && (!tree_node_vals (type)
	       || (stub_decl && !tree_node_vals (stub_decl))))
    goto bail;

  spec_entry spec;
  unsigned spec_flags = 0;
  if (mk & MK_template_mask
      || mk == MK_partial
      || mk == MK_friend_spec)
    {
      if (mk == MK_partial)
	spec_flags = 2;
      else
	spec_flags = u ();

      spec.tmpl = tree_node ();
      spec.args = tree_node ();
    }
  /* Hold constraints on the spec field, for a short while.  */
  spec.spec = tree_node ();

  dump (dumper::TREE) && dump ("Read:%d %C:%N", tag, TREE_CODE (decl), decl);

  existing = back_refs[~tag];
  bool installed = install_entity (existing);
  bool is_new = existing == decl;

  if (DECL_LANG_SPECIFIC (inner)
      && DECL_MODULE_KEYED_DECLS_P (inner))
    {
      /* Read and maybe install the attached entities.  */
      bool existed;
      auto &set = keyed_table->get_or_insert (STRIP_TEMPLATE (existing),
					      &existed);
      unsigned num = u ();
      if (is_new == existed)
	set_overrun ();
      if (is_new)
	set.reserve (num);
      for (unsigned ix = 0; !get_overrun () && ix != num; ix++)
	{
	  tree attached = tree_node ();
	  dump (dumper::MERGE)
	    && dump ("Read %d[%u] %s attached decl %N", tag, ix,
		     is_new ? "new" : "matched", attached);
	  if (is_new)
	    set.quick_push (attached);
	  else if (set[ix] != attached)
	    set_overrun ();
	}
    }

  if (is_imported_temploid_friend)
    if (tree owner = tree_node ())
      if (is_new)
	imported_temploid_friends->put (decl, owner);

  /* Regular typedefs will have a NULL TREE_TYPE at this point.  */
  unsigned tdef_flags = 0;
  bool is_typedef = false;
  if (!type && TREE_CODE (inner) == TYPE_DECL)
    {
      tdef_flags = u ();
      if (tdef_flags & 1)
	is_typedef = true;
    }

  if (is_new)
    {
      /* A newly discovered node.  */
      if (TREE_CODE (decl) == FUNCTION_DECL && DECL_VIRTUAL_P (decl))
	/* Mark this identifier as naming a virtual function --
	   lookup_overrides relies on this optimization.  */
	IDENTIFIER_VIRTUAL_P (DECL_NAME (decl)) = true;

      if (installed)
	{
	  /* Mark the entity as imported.  */
	  retrofit_lang_decl (inner);
	  DECL_MODULE_IMPORT_P (inner) = true;
	}

      if (spec.spec)
	set_constraints (decl, spec.spec);

      if (TREE_CODE (decl) == INTEGER_CST && !TREE_OVERFLOW (decl))
	{
	  decl = cache_integer_cst (decl, true);
	  back_refs[~tag] = decl;
	}

      if (is_typedef)
	{
	  /* Frob it to be ready for cloning.  */
	  TREE_TYPE (inner) = DECL_ORIGINAL_TYPE (inner);
	  DECL_ORIGINAL_TYPE (inner) = NULL_TREE;
	  set_underlying_type (inner);
	  if (tdef_flags & 2)
	    {
	      /* Match instantiate_alias_template's handling.  */
	      tree type = TREE_TYPE (inner);
	      TYPE_DEPENDENT_P (type) = true;
	      TYPE_DEPENDENT_P_VALID (type) = true;
	      SET_TYPE_STRUCTURAL_EQUALITY (type);
	    }
	}

      if (inner_tag)
	/* Set the TEMPLATE_DECL's type.  */
	TREE_TYPE (decl) = TREE_TYPE (inner);

      /* Redetermine whether we need to import or export this declaration
	 for this TU.  But for extern templates we know we must import:
	 they'll be defined in a different TU.
	 FIXME: How do dllexport and dllimport interact across a module?
	 See also https://github.com/itanium-cxx-abi/cxx-abi/issues/170.
	 May have to revisit?  */
      if (type
	  && CLASS_TYPE_P (type)
	  && TYPE_LANG_SPECIFIC (type)
	  && !(CLASSTYPE_EXPLICIT_INSTANTIATION (type)
	       && CLASSTYPE_INTERFACE_KNOWN (type)
	       && CLASSTYPE_INTERFACE_ONLY (type)))
	{
	  CLASSTYPE_INTERFACE_ONLY (type) = false;
	  CLASSTYPE_INTERFACE_UNKNOWN (type) = true;
	}

      /* Add to specialization tables now that constraints etc are
	 added.  */
      if (mk == MK_partial)
	{
	  bool is_type = TREE_CODE (inner) == TYPE_DECL;
	  spec.spec = is_type ? type : inner;
	  add_mergeable_specialization (!is_type, &spec, decl, spec_flags);
	}
      else if (mk & MK_template_mask)
	{
	  bool is_type = !(mk & MK_tmpl_decl_mask);
	  spec.spec = is_type ? type : mk & MK_tmpl_tmpl_mask ? inner : decl;
	  add_mergeable_specialization (!is_type, &spec, decl, spec_flags);
	}

      if (NAMESPACE_SCOPE_P (decl)
	  && (mk == MK_named || mk == MK_unique
	      || mk == MK_enum || mk == MK_friend_spec)
	  && !(VAR_OR_FUNCTION_DECL_P (decl) && DECL_LOCAL_DECL_P (decl)))
	add_module_namespace_decl (CP_DECL_CONTEXT (decl), decl);

      if (DECL_ARTIFICIAL (decl)
	  && TREE_CODE (decl) == FUNCTION_DECL
	  && !DECL_TEMPLATE_INFO (decl)
	  && DECL_CONTEXT (decl) && TYPE_P (DECL_CONTEXT (decl))
	  && TYPE_SIZE (DECL_CONTEXT (decl))
	  && !DECL_THUNK_P (decl))
	/* A new implicit member function, when the class is
	   complete.  This means the importee declared it, and
	   we must now add it to the class.  Note that implicit
	   member fns of template instantiations do not themselves
	   look like templates.  */
	if (!install_implicit_member (inner))
	  set_overrun ();

      /* When importing a TLS wrapper from a header unit, we haven't
	 actually emitted its definition yet. Remember it so we can
	 do this later.  */
      if (state->is_header ()
	  && decl_tls_wrapper_p (decl))
	note_vague_linkage_fn (decl);

      /* Setup aliases for the declaration.  */
      if (tree alias = lookup_attribute ("alias", DECL_ATTRIBUTES (decl)))
	{
	  alias = TREE_VALUE (TREE_VALUE (alias));
	  alias = get_identifier (TREE_STRING_POINTER (alias));
	  assemble_alias (decl, alias);
	}
    }
  else
    {
      /* DECL is the to-be-discarded decl.  Its internal pointers will
	 be to the EXISTING's structure.  Frob it to point to its
	 own other structures, so loading its definition will alter
	 it, and not the existing decl.  */
      dump (dumper::MERGE) && dump ("Deduping %N", existing);

      if (inner_tag)
	DECL_TEMPLATE_RESULT (decl) = inner;

      if (type)
	{
	  /* Point at the to-be-discarded type & decl.  */
	  TYPE_NAME (type) = inner;
	  TREE_TYPE (inner) = type;

	  TYPE_STUB_DECL (type) = stub_decl ? stub_decl : inner;
	  if (stub_decl)
	    TREE_TYPE (stub_decl) = type;
	}

      if (inner_tag)
	/* Set the TEMPLATE_DECL's type.  */
	TREE_TYPE (decl) = TREE_TYPE (inner);

      if (!is_matching_decl (existing, decl, is_typedef))
	unmatched_duplicate (existing);

      if (TREE_CODE (inner) == FUNCTION_DECL)
	{
	  tree e_inner = STRIP_TEMPLATE (existing);
	  for (auto parm = DECL_ARGUMENTS (inner);
	       parm; parm = DECL_CHAIN (parm))
	    DECL_CONTEXT (parm) = e_inner;
	}

      /* And our result is the existing node.  */
      decl = existing;
    }

  if (mk == MK_friend_spec)
    {
      tree e = match_mergeable_specialization (true, &spec);
      if (!e)
	{
	  spec.spec = inner;
	  add_mergeable_specialization (true, &spec, decl, spec_flags);
	}
      else if (e != existing)
	set_overrun ();
    }

  if (is_typedef)
    {
      /* Insert the type into the array now.  */
      tag = insert (TREE_TYPE (decl));
      dump (dumper::TREE)
	&& dump ("Cloned:%d typedef %C:%N",
		 tag, TREE_CODE (TREE_TYPE (decl)), TREE_TYPE (decl));
    }

  unused = saved_unused;

  if (DECL_MAYBE_IN_CHARGE_CDTOR_P (decl))
    {
      unsigned flags = u ();

      if (is_new)
	{
	  bool cloned_p = flags & 1;
	  dump (dumper::TREE) && dump ("CDTOR %N is %scloned",
				       decl, cloned_p ? "" : "not ");
	  if (cloned_p)
	    build_cdtor_clones (decl, flags & 2, flags & 4,
				/* Update the member vec, if there is
				   one (we're in a different cluster
				   to the class defn).  */
				CLASSTYPE_MEMBER_VEC (DECL_CONTEXT (decl)));
	}
    }

  if (VAR_P (decl) && CP_DECL_THREAD_LOCAL_P (decl))
    {
      enum tls_model model = tls_model (u ());
      if (is_new)
	set_decl_tls_model (decl, model);
    }

  if (!NAMESPACE_SCOPE_P (inner)
      && ((TREE_CODE (inner) == TYPE_DECL
	   && !is_typedef
	   && TYPE_NAME (TREE_TYPE (inner)) == inner)
	  || TREE_CODE (inner) == FUNCTION_DECL)
      && u ())
    read_definition (decl);

  return decl;
}

/* DECL is an unnameable member of CTX.  Return a suitable identifying
   index.  */

static unsigned
get_field_ident (tree ctx, tree decl)
{
  gcc_checking_assert (TREE_CODE (decl) == USING_DECL
		       || !DECL_NAME (decl)
		       || IDENTIFIER_ANON_P (DECL_NAME (decl)));

  unsigned ix = 0;
  for (tree fields = TYPE_FIELDS (ctx);
       fields; fields = DECL_CHAIN (fields))
    {
      if (fields == decl)
	return ix;

      if (DECL_CONTEXT (fields) == ctx
	  && (TREE_CODE (fields) == USING_DECL
	      || (TREE_CODE (fields) == FIELD_DECL
		  && (!DECL_NAME (fields)
		      || IDENTIFIER_ANON_P (DECL_NAME (fields))))))
	/* Count this field.  */
	ix++;
    }
  gcc_unreachable ();
}

static tree
lookup_field_ident (tree ctx, unsigned ix)
{
  for (tree fields = TYPE_FIELDS (ctx);
       fields; fields = DECL_CHAIN (fields))
    if (DECL_CONTEXT (fields) == ctx
	&& (TREE_CODE (fields) == USING_DECL
	    || (TREE_CODE (fields) == FIELD_DECL
		&& (!DECL_NAME (fields)
		    || IDENTIFIER_ANON_P (DECL_NAME (fields))))))
      if (!ix--)
	return fields;

  return NULL_TREE;
}

/* Reference DECL.  REF indicates the walk kind we are performing.
   Return true if we should write this decl by value.  */

bool
trees_out::decl_node (tree decl, walk_kind ref)
{
  gcc_checking_assert (DECL_P (decl) && !DECL_TEMPLATE_PARM_P (decl)
		       && DECL_CONTEXT (decl));

  if (ref == WK_value)
    {
      depset *dep = dep_hash->find_dependency (decl);
      decl_value (decl, dep);
      return false;
    }

  switch (TREE_CODE (decl))
    {
    default:
      break;

    case FUNCTION_DECL:
      gcc_checking_assert (!DECL_LOCAL_DECL_P (decl));
      break;

    case RESULT_DECL:
      /* Unlike PARM_DECLs, RESULT_DECLs are only generated and
         referenced when we're inside the function itself.  */
      return true;

    case PARM_DECL:
      {
	if (streaming_p ())
	  i (tt_parm);
	tree_node (DECL_CONTEXT (decl));
	if (streaming_p ())
	  {
	    /* That must have put this in the map.  */
	    walk_kind ref = ref_node (decl);
	    if (ref != WK_none)
	      // FIXME:OPTIMIZATION We can wander into bits of the
	      // template this was instantiated from.  For instance
	      // deferred noexcept and default parms.  Currently we'll
	      // end up cloning those bits of tree.  It would be nice
	      // to reference those specific nodes.  I think putting
	      // those things in the map when we reference their
	      // template by name.  See the note in add_indirects.
	      return true;

	    dump (dumper::TREE)
	      && dump ("Wrote %s reference %N",
		       TREE_CODE (decl) == PARM_DECL ? "parameter" : "result",
		       decl);
	  }
      }
      return false;

    case IMPORTED_DECL:
      /* This describes a USING_DECL to the ME's debug machinery.  It
	 originates from the fortran FE, and has nothing to do with
	 C++ modules.  */
      return true;

    case LABEL_DECL:
      return true;

    case CONST_DECL:
      {
	/* If I end up cloning enum decls, implementing C++20 using
	   E::v, this will need tweaking.   */
	if (streaming_p ())
	  i (tt_enum_decl);
	tree ctx = DECL_CONTEXT (decl);
	gcc_checking_assert (TREE_CODE (ctx) == ENUMERAL_TYPE);
	tree_node (ctx);
	tree_node (DECL_NAME (decl));

	int tag = insert (decl);
	if (streaming_p ())
	  dump (dumper::TREE)
	    && dump ("Wrote enum decl:%d %C:%N", tag, TREE_CODE (decl), decl);
	return false;
      }
      break;

    case USING_DECL:
      if (TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)
	break;
      /* FALLTHROUGH  */

    case FIELD_DECL:
      {
	if (streaming_p ())
	  i (tt_data_member);

	tree ctx = DECL_CONTEXT (decl);
	tree_node (ctx);

	tree name = NULL_TREE;

	if (TREE_CODE (decl) == USING_DECL)
	  ;
	else
	  {
	    name = DECL_NAME (decl);
	    if (name && IDENTIFIER_ANON_P (name))
	      name = NULL_TREE;
	  }

	tree_node (name);
	if (!name && streaming_p ())
	  {
	    unsigned ix = get_field_ident (ctx, decl);
	    u (ix);
	  }

	int tag = insert (decl);
	if (streaming_p ())
	  dump (dumper::TREE)
	    && dump ("Wrote member:%d %C:%N", tag, TREE_CODE (decl), decl);
	return false;
      }
      break;

    case VAR_DECL:
      gcc_checking_assert (!DECL_LOCAL_DECL_P (decl));
      if (DECL_VTABLE_OR_VTT_P (decl))
	{
	  /* VTT or VTABLE, they are all on the vtables list.  */
	  tree ctx = CP_DECL_CONTEXT (decl);
	  tree vtable = CLASSTYPE_VTABLES (ctx);
	  for (unsigned ix = 0; ; vtable = DECL_CHAIN (vtable), ix++)
	    if (vtable == decl)
	      {
		gcc_checking_assert (DECL_VIRTUAL_P (decl));
		if (streaming_p ())
		  {
		    u (tt_vtable);
		    u (ix);
		    dump (dumper::TREE)
		      && dump ("Writing vtable %N[%u]", ctx, ix);
		  }
		tree_node (ctx);
		return false;
	      }
	  gcc_unreachable ();
	}

      if (DECL_TINFO_P (decl))
	{
	tinfo:
	  /* A typeinfo, tt_tinfo_typedef or tt_tinfo_var.  */
	  bool is_var = VAR_P (decl);
	  tree type = TREE_TYPE (decl);
	  unsigned ix = get_pseudo_tinfo_index (type);
	  if (streaming_p ())
	    {
	      i (is_var ? tt_tinfo_var : tt_tinfo_typedef);
	      u (ix);
	    }

	  if (is_var)
	    {
	      /* We also need the type it is for and mangled name, so
		 the reader doesn't need to complete the type (which
		 would break section ordering).  The type it is for is
		 stashed on the name's TREE_TYPE.  */
	      tree name = DECL_NAME (decl);
	      tree_node (name);
	      type = TREE_TYPE (name);
	      tree_node (type);
	    }

	  int tag = insert (decl);
	  if (streaming_p ())
	    dump (dumper::TREE)
	      && dump ("Wrote tinfo_%s:%d %u %N", is_var ? "var" : "type",
		       tag, ix, type);

	  if (!is_var)
	    {
	      tag = insert (type);
	      if (streaming_p ())
		dump (dumper::TREE)
		  && dump ("Wrote tinfo_type:%d %u %N", tag, ix, type);
	    }
	  return false;
	}

      if (DECL_NTTP_OBJECT_P (decl))
	{
	  /* A NTTP parm object.  */
	  if (streaming_p ())
	    i (tt_nttp_var);
	  tree_node (tparm_object_argument (decl));
	  tree_node (DECL_NAME (decl));
	  int tag = insert (decl);
	  if (streaming_p ())
	    dump (dumper::TREE)
	      && dump ("Wrote nttp object:%d %N", tag, DECL_NAME (decl));
	  return false;
	}

      break;

    case TYPE_DECL:
      if (DECL_TINFO_P (decl))
	goto tinfo;
      break;
    }

  if (DECL_THUNK_P (decl))
    {
      /* Thunks are similar to binfos -- write the thunked-to decl and
	 then thunk-specific key info.  */
      if (streaming_p ())
	{
	  i (tt_thunk);
	  i (THUNK_FIXED_OFFSET (decl));
	}

      tree target = decl;
      while (DECL_THUNK_P (target))
	target = THUNK_TARGET (target);
      tree_node (target);
      tree_node (THUNK_VIRTUAL_OFFSET (decl));
      int tag = insert (decl);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote:%d thunk %N to %N", tag, DECL_NAME (decl), target);
      return false;
    }

  if (DECL_CLONED_FUNCTION_P (decl))
    {
      tree target = get_clone_target (decl);
      if (streaming_p ())
	i (tt_clone_ref);

      tree_node (target);
      tree_node (DECL_NAME (decl));
      if (DECL_VIRTUAL_P (decl))
	tree_node (DECL_VINDEX (decl));
      int tag = insert (decl);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote:%d clone %N of %N", tag, DECL_NAME (decl), target);
      return false;
    }

  /* Everything left should be a thing that is in the entity table.
     Mostly things that can be defined outside of their (original
     declaration) context.  */
  gcc_checking_assert (TREE_CODE (decl) == TEMPLATE_DECL
		       || VAR_P (decl)
		       || TREE_CODE (decl) == FUNCTION_DECL
		       || TREE_CODE (decl) == TYPE_DECL
		       || TREE_CODE (decl) == USING_DECL
		       || TREE_CODE (decl) == CONCEPT_DECL
		       || TREE_CODE (decl) == NAMESPACE_DECL);

  int use_tpl = -1;
  tree ti = node_template_info (decl, use_tpl);
  tree tpl = NULL_TREE;

  /* If this is the TEMPLATE_DECL_RESULT of a TEMPLATE_DECL, get the
     TEMPLATE_DECL.  Note TI_TEMPLATE is not a TEMPLATE_DECL for
     (some) friends, so we need to check that.  */
  // FIXME: Should local friend template specializations be by value?
  // They don't get idents so we'll never know they're imported, but I
  // think we can only reach them from the TU that defines the
  // befriending class?
  if (ti && TREE_CODE (TI_TEMPLATE (ti)) == TEMPLATE_DECL
      && DECL_TEMPLATE_RESULT (TI_TEMPLATE (ti)) == decl)
    {
      tpl = TI_TEMPLATE (ti);
    partial_template:
      if (streaming_p ())
	{
	  i (tt_template);
	  dump (dumper::TREE)
	    && dump ("Writing implicit template %C:%N%S",
		     TREE_CODE (tpl), tpl, tpl);
	}
      tree_node (tpl);

      /* Streaming TPL caused us to visit DECL and maybe its type.  */
      gcc_checking_assert (TREE_VISITED (decl));
      if (DECL_IMPLICIT_TYPEDEF_P (decl))
	gcc_checking_assert (TREE_VISITED (TREE_TYPE (decl)));
      return false;
    }

  tree ctx = CP_DECL_CONTEXT (decl);
  depset *dep = NULL;
  if (streaming_p ())
    dep = dep_hash->find_dependency (decl);
  else if (TREE_CODE (ctx) != FUNCTION_DECL
	   || TREE_CODE (decl) == TEMPLATE_DECL
	   || DECL_IMPLICIT_TYPEDEF_P (decl)
	   || (DECL_LANG_SPECIFIC (decl)
	       && DECL_MODULE_IMPORT_P (decl)))
    {
      auto kind = (TREE_CODE (decl) == NAMESPACE_DECL
		   && !DECL_NAMESPACE_ALIAS (decl)
		   ? depset::EK_NAMESPACE : depset::EK_DECL);
      dep = dep_hash->add_dependency (decl, kind);
    }

  if (!dep)
    {
      /* Some internal entity of context.  Do by value.  */
      decl_value (decl, NULL);
      return false;
    }

  if (dep->get_entity_kind () == depset::EK_REDIRECT)
    {
      /* The DECL_TEMPLATE_RESULT of a partial specialization.
	 Write the partial specialization's template.  */
      depset *redirect = dep->deps[0];
      gcc_checking_assert (redirect->get_entity_kind () == depset::EK_PARTIAL);
      tpl = redirect->get_entity ();
      goto partial_template;
    }

  if (streaming_p ())
    {
      /* Locate the entity.  */
      unsigned index = dep->cluster;
      unsigned import = 0;

      if (dep->is_import ())
	import = dep->section;
      else if (CHECKING_P)
	/* It should be what we put there.  */
	gcc_checking_assert (index == ~import_entity_index (decl));

#if CHECKING_P
      gcc_assert (!import || importedness >= 0);
#endif
      i (tt_entity);
      u (import);
      u (index);
    }

  int tag = insert (decl);
  if (streaming_p () && dump (dumper::TREE))
    {
      char const *kind = "import";
      module_state *from = (*modules)[0];
      if (dep->is_import ())
	/* Rediscover the unremapped index.  */
	from = import_entity_module (import_entity_index (decl));
      else
	{
	  tree o = get_originating_module_decl (decl);
	  o = STRIP_TEMPLATE (o);
	  kind = (DECL_LANG_SPECIFIC (o) && DECL_MODULE_PURVIEW_P (o)
		  ? "purview" : "GMF");
	}
      dump ("Wrote %s:%d %C:%N@%M", kind,
	    tag, TREE_CODE (decl), decl, from);
    }

  add_indirects (decl);

  return false;
}

void
trees_out::type_node (tree type)
{
  gcc_assert (TYPE_P (type));

  tree root = (TYPE_NAME (type)
	       ? TREE_TYPE (TYPE_NAME (type)) : TYPE_MAIN_VARIANT (type));

  if (type != root)
    {
      if (streaming_p ())
	i (tt_variant_type);
      tree_node (root);

      int flags = -1;

      if (TREE_CODE (type) == FUNCTION_TYPE
	  || TREE_CODE (type) == METHOD_TYPE)
	{
	  int quals = type_memfn_quals (type);
	  int rquals = type_memfn_rqual (type);
	  tree raises = TYPE_RAISES_EXCEPTIONS (type);
	  bool late = TYPE_HAS_LATE_RETURN_TYPE (type);

	  if (raises != TYPE_RAISES_EXCEPTIONS (root)
	      || rquals != type_memfn_rqual (root)
	      || quals != type_memfn_quals (root)
	      || late != TYPE_HAS_LATE_RETURN_TYPE (root))
	    flags = rquals | (int (late) << 2) | (quals << 3);
	}
      else
	{
	  if (TYPE_USER_ALIGN (type))
	    flags = TYPE_ALIGN_RAW (type);
	}

      if (streaming_p ())
	i (flags);

      if (flags < 0)
	;
      else if (TREE_CODE (type) == FUNCTION_TYPE
	       || TREE_CODE (type) == METHOD_TYPE)
	{
	  tree raises = TYPE_RAISES_EXCEPTIONS (type);
	  if (raises == TYPE_RAISES_EXCEPTIONS (root))
	    raises = error_mark_node;
	  tree_node (raises);
	}

      tree_node (TYPE_ATTRIBUTES (type));

      if (streaming_p ())
	{
	  /* Qualifiers.  */
	  int rquals = cp_type_quals (root);
	  int quals = cp_type_quals (type);
	  if (quals == rquals)
	    quals = -1;
	  i (quals);
	}

      if (ref_node (type) != WK_none)
	{
	  int tag = insert (type);
	  if (streaming_p ())
	    {
	      i (0);
	      dump (dumper::TREE)
		&& dump ("Wrote:%d variant type %C", tag, TREE_CODE (type));
	    }
	}
      return;
    }

  if (tree name = TYPE_NAME (type))
    if ((TREE_CODE (name) == TYPE_DECL && DECL_ORIGINAL_TYPE (name))
	|| DECL_TEMPLATE_PARM_P (name)
	|| TREE_CODE (type) == RECORD_TYPE
	|| TREE_CODE (type) == UNION_TYPE
	|| TREE_CODE (type) == ENUMERAL_TYPE)
      {
	/* We can meet template parms that we didn't meet in the
	   tpl_parms walk, because we're referring to a derived type
	   that was previously constructed from equivalent template
	   parms. */
	if (streaming_p ())
	  {
	    i (tt_typedef_type);
	    dump (dumper::TREE)
	      && dump ("Writing %stypedef %C:%N",
		       DECL_IMPLICIT_TYPEDEF_P (name) ? "implicit " : "",
		       TREE_CODE (name), name);
	  }
	tree_node (name);
	if (streaming_p ())
	  dump (dumper::TREE) && dump ("Wrote typedef %C:%N%S",
				       TREE_CODE (name), name, name);
	gcc_checking_assert (TREE_VISITED (type));
	return;
      }

  if (TYPE_PTRMEMFUNC_P (type))
    {
      /* This is a distinct type node, masquerading as a structure. */
      tree fn_type = TYPE_PTRMEMFUNC_FN_TYPE (type);
      if (streaming_p ())
	i (tt_ptrmem_type);
      tree_node (fn_type);
      int tag = insert (type);
      if (streaming_p ())
	dump (dumper::TREE) && dump ("Written:%d ptrmem type", tag);
      return;
    }

  if (streaming_p ())
    {
      u (tt_derived_type);
      u (TREE_CODE (type));
    }

  tree_node (TREE_TYPE (type));
  switch (TREE_CODE (type))
    {
    default:
      /* We should never meet a type here that is indescribable in
	 terms of other types.  */
      gcc_unreachable ();

    case ARRAY_TYPE:
      tree_node (TYPE_DOMAIN (type));
      if (streaming_p ())
	/* Dependent arrays are constructed with TYPE_DEPENENT_P
	   already set.  */
	u (TYPE_DEPENDENT_P (type));
      break;

    case COMPLEX_TYPE:
      /* No additional data.  */
      break;

    case BOOLEAN_TYPE:
      /* A non-standard boolean type.  */
      if (streaming_p ())
	u (TYPE_PRECISION (type));
      break;

    case INTEGER_TYPE:
      if (TREE_TYPE (type))
	{
	  /* A range type (representing an array domain).  */
	  tree_node (TYPE_MIN_VALUE (type));
	  tree_node (TYPE_MAX_VALUE (type));
	}
      else
	{
	  /* A new integral type (representing a bitfield).  */
	  if (streaming_p ())
	    {
	      unsigned prec = TYPE_PRECISION (type);
	      bool unsigned_p = TYPE_UNSIGNED (type);

	      u ((prec << 1) | unsigned_p);
	    }
	}
      break;

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      {
	gcc_checking_assert (type_memfn_rqual (type) == REF_QUAL_NONE);

	tree arg_types = TYPE_ARG_TYPES (type);
	if (TREE_CODE (type) == METHOD_TYPE)
	  {
	    tree_node (TREE_TYPE (TREE_VALUE (arg_types)));
	    arg_types = TREE_CHAIN (arg_types);
	  }
	tree_node (arg_types);
      }
      break;

    case OFFSET_TYPE:
      tree_node (TYPE_OFFSET_BASETYPE (type));
      break;

    case POINTER_TYPE:
      /* No additional data.  */
      break;

    case REFERENCE_TYPE:
      if (streaming_p ())
	u (TYPE_REF_IS_RVALUE (type));
      break;

    case DECLTYPE_TYPE:
    case TYPEOF_TYPE:
    case DEPENDENT_OPERATOR_TYPE:
      tree_node (TYPE_VALUES_RAW (type));
      if (TREE_CODE (type) == DECLTYPE_TYPE)
	/* We stash a whole bunch of things into decltype's
	   flags.  */
	if (streaming_p ())
	  tree_node_bools (type);
      break;

    case TRAIT_TYPE:
      tree_node (TRAIT_TYPE_KIND_RAW (type));
      tree_node (TRAIT_TYPE_TYPE1 (type));
      tree_node (TRAIT_TYPE_TYPE2 (type));
      break;

    case TYPE_ARGUMENT_PACK:
      /* No additional data.  */
      break;

    case TYPE_PACK_EXPANSION:
      if (streaming_p ())
	u (PACK_EXPANSION_LOCAL_P (type));
      tree_node (PACK_EXPANSION_PARAMETER_PACKS (type));
      tree_node (PACK_EXPANSION_EXTRA_ARGS (type));
      break;

    case TYPENAME_TYPE:
      {
	tree_node (TYPE_CONTEXT (type));
	tree_node (DECL_NAME (TYPE_NAME (type)));
	tree_node (TYPENAME_TYPE_FULLNAME (type));
	if (streaming_p ())
	  {
	    enum tag_types tag_type = none_type;
	    if (TYPENAME_IS_ENUM_P (type))
	      tag_type = enum_type;
	    else if (TYPENAME_IS_CLASS_P (type))
	      tag_type = class_type;
	    u (int (tag_type));
	  }
	}
      break;

    case UNBOUND_CLASS_TEMPLATE:
      {
	tree decl = TYPE_NAME (type);
	tree_node (DECL_CONTEXT (decl));
	tree_node (DECL_NAME (decl));
	tree_node (DECL_TEMPLATE_PARMS (decl));
      }
      break;

    case VECTOR_TYPE:
      if (streaming_p ())
	{
	  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (type);
	  for (unsigned ix = 0; ix != NUM_POLY_INT_COEFFS; ix++)
	    wu (nunits.coeffs[ix]);
	}
      break;
    }

  /* We may have met the type during emitting the above.  */
  if (ref_node (type) != WK_none)
    {
      int tag = insert (type);
      if (streaming_p ())
	{
	  i (0);
	  dump (dumper::TREE)
	    && dump ("Wrote:%d derived type %C", tag, TREE_CODE (type));
	}
    }

  return;
}

/* T is (mostly*) a non-mergeable node that must be written by value.
   The mergeable case is a BINFO, which are as-if DECLSs.   */

void
trees_out::tree_value (tree t)
{
  /* We should never be writing a type by value.  tree_type should
     have streamed it, or we're going via its TYPE_DECL.  */
  gcc_checking_assert (!TYPE_P (t));

  if (DECL_P (t))
    /* No template, type, var or function, except anonymous
       non-context vars.  */
    gcc_checking_assert ((TREE_CODE (t) != TEMPLATE_DECL
			  && TREE_CODE (t) != TYPE_DECL
			  && (TREE_CODE (t) != VAR_DECL
			      || (!DECL_NAME (t) && !DECL_CONTEXT (t)))
			  && TREE_CODE (t) != FUNCTION_DECL));

  if (streaming_p ())
    {
      /* A new node -> tt_node.  */
      tree_val_count++;
      i (tt_node);
      start (t);
      tree_node_bools (t);
    }

  if  (TREE_CODE (t) == TREE_BINFO)
    /* Binfos are decl-like and need merging information.  */
    binfo_mergeable (t);

  int tag = insert (t, WK_value);
  if (streaming_p ())
    dump (dumper::TREE)
      && dump ("Writing tree:%d %C:%N", tag, TREE_CODE (t), t);

  tree_node_vals (t);

  if (streaming_p ())
    dump (dumper::TREE) && dump ("Written tree:%d %C:%N", tag, TREE_CODE (t), t);
}

tree
trees_in::tree_value ()
{
  tree t = start ();
  if (!t || !tree_node_bools (t))
    return NULL_TREE;

  tree existing = t;
  if (TREE_CODE (t) == TREE_BINFO)
    {
      tree type;
      unsigned ix = binfo_mergeable (&type);
      if (TYPE_BINFO (type))
	{
	  /* We already have a definition, this must be a duplicate.  */
	  dump (dumper::MERGE)
	    && dump ("Deduping binfo %N[%u]", type, ix);
	  existing = TYPE_BINFO (type);
	  while (existing && ix--)
	    existing = TREE_CHAIN (existing);
	  if (existing)
	    register_duplicate (t, existing);
	  else
	    /* Error, mismatch -- diagnose in read_class_def's
	       checking.  */
	    existing = t;
	}
    }

  /* Insert into map.  */
  int tag = insert (existing);
  dump (dumper::TREE)
    && dump ("Reading tree:%d %C", tag, TREE_CODE (t));

  if (!tree_node_vals (t))
    {
      back_refs[~tag] = NULL_TREE;
      set_overrun ();
      /* Bail.  */
      return NULL_TREE;
    }

  if (TREE_CODE (t) == LAMBDA_EXPR
      && CLASSTYPE_LAMBDA_EXPR (TREE_TYPE (t)))
    {
      existing = CLASSTYPE_LAMBDA_EXPR (TREE_TYPE (t));
      back_refs[~tag] = existing;
    }

  dump (dumper::TREE) && dump ("Read tree:%d %C:%N", tag, TREE_CODE (t), t);

  if (TREE_CODE (existing) == INTEGER_CST && !TREE_OVERFLOW (existing))
    {
      existing = cache_integer_cst (t, true);
      back_refs[~tag] = existing;
    }

  return existing;
}

/* Stream out tree node T.  We automatically create local back
   references, which is essentially a single pass lisp
   self-referential structure pretty-printer.  */

void
trees_out::tree_node (tree t)
{
  dump.indent ();
  walk_kind ref = ref_node (t);
  if (ref == WK_none)
    goto done;

  if (ref != WK_normal)
    goto skip_normal;

  if (TREE_CODE (t) == IDENTIFIER_NODE)
    {
      /* An identifier node -> tt_id, tt_conv_id, tt_anon_id, tt_lambda_id.  */
      int code = tt_id;
      if (IDENTIFIER_ANON_P (t))
	code = IDENTIFIER_LAMBDA_P (t) ? tt_lambda_id : tt_anon_id;
      else if (IDENTIFIER_CONV_OP_P (t))
	code = tt_conv_id;

      if (streaming_p ())
	i (code);

      if (code == tt_conv_id)
	{
	  tree type = TREE_TYPE (t);
	  gcc_checking_assert (type || t == conv_op_identifier);
	  tree_node (type);
	}
      else if (code == tt_id && streaming_p ())
	str (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));

      int tag = insert (t);
      if (streaming_p ())
	{
	  /* We know the ordering of the 4 id tags.  */
	  static const char *const kinds[] =
	    {"", "conv_op ", "anon ", "lambda "};
	  dump (dumper::TREE)
	    && dump ("Written:%d %sidentifier:%N", tag,
		     kinds[code - tt_id],
		     code == tt_conv_id ? TREE_TYPE (t) : t);
	}
      goto done;
    }

  if (TREE_CODE (t) == TREE_BINFO)
    {
      /* A BINFO -> tt_binfo.
	 We must do this by reference.  We stream the binfo tree
	 itself when streaming its owning RECORD_TYPE.  That we got
	 here means the dominating type is not in this SCC.  */
      if (streaming_p ())
	i (tt_binfo);
      binfo_mergeable (t);
      gcc_checking_assert (!TREE_VISITED (t));
      int tag = insert (t);
      if (streaming_p ())
	dump (dumper::TREE) && dump ("Inserting binfo:%d %N", tag, t);
      goto done;
    }

  if (TREE_CODE (t) == INTEGER_CST
      && !TREE_OVERFLOW (t)
      && TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
    {
      /* An integral constant of enumeral type.  See if it matches one
	 of the enumeration values.  */
      for (tree values = TYPE_VALUES (TREE_TYPE (t));
	   values; values = TREE_CHAIN (values))
	{
	  tree decl = TREE_VALUE (values);
	  if (tree_int_cst_equal (DECL_INITIAL (decl), t))
	    {
	      if (streaming_p ())
		u (tt_enum_value);
	      tree_node (decl);
	      dump (dumper::TREE) && dump ("Written enum value %N", decl);
	      goto done;
	    }
	}
      /* It didn't match.  We'll write it a an explicit INTEGER_CST
	 node.  */
    }

  if (TYPE_P (t))
    {
      type_node (t);
      goto done;
    }

  if (DECL_P (t))
    {
      if (DECL_TEMPLATE_PARM_P (t))
	{
	  tpl_parm_value (t);
	  goto done;
	}

      if (!DECL_CONTEXT (t))
	{
	  /* There are a few cases of decls with no context.  We'll write
	     these by value, but first assert they are cases we expect.  */
	  gcc_checking_assert (ref == WK_normal);
	  switch (TREE_CODE (t))
	    {
	    default: gcc_unreachable ();

	    case LABEL_DECL:
	      /* CASE_LABEL_EXPRs contain uncontexted LABEL_DECLs.  */
	      gcc_checking_assert (!DECL_NAME (t));
	      break;

	    case VAR_DECL:
	      /* AGGR_INIT_EXPRs cons up anonymous uncontexted VAR_DECLs.  */
	      gcc_checking_assert (!DECL_NAME (t)
				   && DECL_ARTIFICIAL (t));
	      break;

	    case PARM_DECL:
	      /* REQUIRES_EXPRs have a tree list of uncontexted
		 PARM_DECLS.  It'd be nice if they had a
		 distinguishing flag to double check.  */
	      break;
	    }
	  goto by_value;
	}
    }

 skip_normal:
  if (DECL_P (t) && !decl_node (t, ref))
    goto done;

  /* Otherwise by value */
 by_value:
  tree_value (t);

 done:
  /* And, breath out.  */
  dump.outdent ();
}

/* Stream in a tree node.  */

tree
trees_in::tree_node (bool is_use)
{
  if (get_overrun ())
    return NULL_TREE;

  dump.indent ();
  int tag = i ();
  tree res = NULL_TREE;
  switch (tag)
    {
    default:
      /* backref, pull it out of the map.  */
      res = back_ref (tag);
      break;

    case tt_null:
      /* NULL_TREE.  */
      break;

    case tt_fixed:
      /* A fixed ref, find it in the fixed_ref array.   */
      {
	unsigned fix = u ();
	if (fix < (*fixed_trees).length ())
	  {
	    res = (*fixed_trees)[fix];
	    dump (dumper::TREE) && dump ("Read fixed:%u %C:%N%S", fix,
					 TREE_CODE (res), res, res);
	  }

	if (!res)
	  set_overrun ();
      }
      break;

    case tt_parm:
      {
	tree fn = tree_node ();
	if (fn && TREE_CODE (fn) == FUNCTION_DECL)
	  res = tree_node ();
	if (res)
	  dump (dumper::TREE)
	    && dump ("Read %s reference %N",
		     TREE_CODE (res) == PARM_DECL ? "parameter" : "result",
		     res);
      }
      break;

    case tt_node:
      /* A new node.  Stream it in.  */
      res = tree_value ();
      break;

    case tt_decl:
      /* A new decl.  Stream it in.  */
      res = decl_value ();
      break;

    case tt_tpl_parm:
      /* A template parameter.  Stream it in.  */
      res = tpl_parm_value ();
      break;

    case tt_id:
      /* An identifier node.  */
      {
	size_t l;
	const char *chars = str (&l);
	res = get_identifier_with_length (chars, l);
	int tag = insert (res);
	dump (dumper::TREE)
	  && dump ("Read identifier:%d %N", tag, res);
      }
      break;

    case tt_conv_id:
      /* A conversion operator.  Get the type and recreate the
	 identifier.  */
      {
	tree type = tree_node ();
	if (!get_overrun ())
	  {
	    res = type ? make_conv_op_name (type) : conv_op_identifier;
	    int tag = insert (res);
	    dump (dumper::TREE)
	      && dump ("Created conv_op:%d %S for %N", tag, res, type);
	  }
      }
      break;

    case tt_anon_id:
    case tt_lambda_id:
      /* An anonymous or lambda id.  */
      {
	res = make_anon_name ();
	if (tag == tt_lambda_id)
	  IDENTIFIER_LAMBDA_P (res) = true;
	int tag = insert (res);
	dump (dumper::TREE)
	  && dump ("Read %s identifier:%d %N",
		   IDENTIFIER_LAMBDA_P (res) ? "lambda" : "anon", tag, res);
      }
      break;

    case tt_typedef_type:
      res = tree_node ();
      if (res)
	{
	  dump (dumper::TREE)
	    && dump ("Read %stypedef %C:%N",
		     DECL_IMPLICIT_TYPEDEF_P (res) ? "implicit " : "",
		     TREE_CODE (res), res);
	  res = TREE_TYPE (res);
	}
      break;

    case tt_derived_type:
      /* A type derived from some other type.  */
      {
	enum tree_code code = tree_code (u ());
	res = tree_node ();

	switch (code)
	  {
	  default:
	    set_overrun ();
	    break;

	  case ARRAY_TYPE:
	    {
	      tree domain = tree_node ();
	      int dep = u ();
	      if (!get_overrun ())
		res = build_cplus_array_type (res, domain, dep);
	    }
	    break;

	  case COMPLEX_TYPE:
	    if (!get_overrun ())
	      res = build_complex_type (res);
	    break;

	  case BOOLEAN_TYPE:
	    {
	      unsigned precision = u ();
	      if (!get_overrun ())
		res = build_nonstandard_boolean_type (precision);
	    }
	    break;

	  case INTEGER_TYPE:
	    if (res)
	      {
		/* A range type (representing an array domain).  */
		tree min = tree_node ();
		tree max = tree_node ();

		if (!get_overrun ())
		  res = build_range_type (res, min, max);
	      }
	    else
	      {
		/* A new integral type (representing a bitfield).  */
		unsigned enc = u ();
		if (!get_overrun ())
		  res = build_nonstandard_integer_type (enc >> 1, enc & 1);
	      }
	    break;

	  case FUNCTION_TYPE:
	  case METHOD_TYPE:
	    {
	      tree klass =  code == METHOD_TYPE ? tree_node () : NULL_TREE;
	      tree args = tree_node ();
	      if (!get_overrun ())
		{
		  if (klass)
		    res = build_method_type_directly (klass, res, args);
		  else
		    res = build_function_type (res, args);
		}
	    }
	    break;

	  case OFFSET_TYPE:
	    {
	      tree base = tree_node ();
	      if (!get_overrun ())
		res = build_offset_type (base, res);
	    }
	    break;

	  case POINTER_TYPE:
	    if (!get_overrun ())
	      res = build_pointer_type (res);
	    break;

	  case REFERENCE_TYPE:
	    {
	      bool rval = bool (u ());
	      if (!get_overrun ())
		res = cp_build_reference_type (res, rval);
	    }
	    break;

	  case DECLTYPE_TYPE:
	  case TYPEOF_TYPE:
	  case DEPENDENT_OPERATOR_TYPE:
	    {
	      tree expr = tree_node ();
	      if (!get_overrun ())
		{
		  res = cxx_make_type (code);
		  TYPE_VALUES_RAW (res) = expr;
		  if (code == DECLTYPE_TYPE)
		    tree_node_bools (res);
		  SET_TYPE_STRUCTURAL_EQUALITY (res);
		}
	    }
	    break;

	  case TRAIT_TYPE:
	    {
	      tree kind = tree_node ();
	      tree type1 = tree_node ();
	      tree type2 = tree_node ();
	      if (!get_overrun ())
		{
		  res = cxx_make_type (TRAIT_TYPE);
		  TRAIT_TYPE_KIND_RAW (res) = kind;
		  TRAIT_TYPE_TYPE1 (res) = type1;
		  TRAIT_TYPE_TYPE2 (res) = type2;
		  SET_TYPE_STRUCTURAL_EQUALITY (res);
		}
	    }
	    break;

	  case TYPE_ARGUMENT_PACK:
	    if (!get_overrun ())
	      {
		tree pack = cxx_make_type (TYPE_ARGUMENT_PACK);
		ARGUMENT_PACK_ARGS (pack) = res;
		res = pack;
	      }
	    break;

	  case TYPE_PACK_EXPANSION:
	    {
	      bool local = u ();
	      tree param_packs = tree_node ();
	      tree extra_args = tree_node ();
	      if (!get_overrun ())
		{
		  tree expn = cxx_make_type (TYPE_PACK_EXPANSION);
		  SET_TYPE_STRUCTURAL_EQUALITY (expn);
		  PACK_EXPANSION_PATTERN (expn) = res;
		  PACK_EXPANSION_PARAMETER_PACKS (expn) = param_packs;
		  PACK_EXPANSION_EXTRA_ARGS (expn) = extra_args;
		  PACK_EXPANSION_LOCAL_P (expn) = local;
		  res = expn;
		}
	    }
	    break;

	  case TYPENAME_TYPE:
	    {
	      tree ctx = tree_node ();
	      tree name = tree_node ();
	      tree fullname = tree_node ();
	      enum tag_types tag_type = tag_types (u ());

	      if (!get_overrun ())
		res = build_typename_type (ctx, name, fullname, tag_type);
	    }
	    break;

	  case UNBOUND_CLASS_TEMPLATE:
	    {
	      tree ctx = tree_node ();
	      tree name = tree_node ();
	      tree parms = tree_node ();

	      if (!get_overrun ())
		res = make_unbound_class_template_raw (ctx, name, parms);
	    }
	    break;

	  case VECTOR_TYPE:
	    {
	      poly_uint64 nunits;
	      for (unsigned ix = 0; ix != NUM_POLY_INT_COEFFS; ix++)
		nunits.coeffs[ix] = wu ();
	      if (!get_overrun ())
		res = build_vector_type (res, nunits);
	    }
	    break;
	  }

	int tag = i ();
	if (!tag)
	  {
	    tag = insert (res);
	    if (res)
	      dump (dumper::TREE)
		&& dump ("Created:%d derived type %C", tag, code);
	  }
	else
	  res = back_ref (tag);
      }
      break;

    case tt_variant_type:
      /* Variant of some type.  */
      {
	res = tree_node ();
	int flags = i ();
	if (get_overrun ())
	  ;
	else if (flags < 0)
	  /* No change.  */;
	else if (TREE_CODE (res) == FUNCTION_TYPE
		 || TREE_CODE (res) == METHOD_TYPE)
	  {
	    cp_ref_qualifier rqual = cp_ref_qualifier (flags & 3);
	    bool late = (flags >> 2) & 1;
	    cp_cv_quals quals = cp_cv_quals (flags >> 3);

	    tree raises = tree_node ();
	    if (raises == error_mark_node)
	      raises = TYPE_RAISES_EXCEPTIONS (res);

	    res = build_cp_fntype_variant (res, rqual, raises, late);
	    if (TREE_CODE (res) == FUNCTION_TYPE)
	      res = apply_memfn_quals (res, quals, rqual);
	  }
	else
	  {
	    res = build_aligned_type (res, (1u << flags) >> 1);
	    TYPE_USER_ALIGN (res) = true;
	  }

	if (tree attribs = tree_node ())
	  res = cp_build_type_attribute_variant (res, attribs);

	int quals = i ();
	if (quals >= 0 && !get_overrun ())
	  res = cp_build_qualified_type (res, quals);

	int tag = i ();
	if (!tag)
	  {
	    tag = insert (res);
	    if (res)
	      dump (dumper::TREE)
		&& dump ("Created:%d variant type %C", tag, TREE_CODE (res));
	  }
	else
	  res = back_ref (tag);
      }
      break;

    case tt_tinfo_var:
    case tt_tinfo_typedef:
      /* A tinfo var or typedef.  */
      {
	bool is_var = tag == tt_tinfo_var;
	unsigned ix = u ();
	tree type = NULL_TREE;

	if (is_var)
	  {
	    tree name = tree_node ();
	    type = tree_node ();

	    if (!get_overrun ())
	      res = get_tinfo_decl_direct (type, name, int (ix));
	  }
	else
	  {
	    if (!get_overrun ())
	      {
		type = get_pseudo_tinfo_type (ix);
		res = TYPE_NAME (type);
	      }
	  }
	if (res)
	  {
	    int tag = insert (res);
	    dump (dumper::TREE)
	      && dump ("Created tinfo_%s:%d %S:%u for %N",
		       is_var ? "var" : "decl", tag, res, ix, type);
	    if (!is_var)
	      {
		tag = insert (type);
		dump (dumper::TREE)
		  && dump ("Created tinfo_type:%d %u %N", tag, ix, type);
	      }
	  }
      }
      break;

    case tt_ptrmem_type:
      /* A pointer to member function.  */
      {
	tree type = tree_node ();
	if (type && TREE_CODE (type) == POINTER_TYPE
	    && TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)
	  {
	    res = build_ptrmemfunc_type (type);
	    int tag = insert (res);
	    dump (dumper::TREE) && dump ("Created:%d ptrmem type", tag);
	  }
	else
	  set_overrun ();
      }
      break;

    case tt_nttp_var:
      /* An NTTP object. */
      {
	tree init = tree_node ();
	tree name = tree_node ();
	if (!get_overrun ())
	  {
	    /* We don't want to check the initializer as that may require
	       name lookup, which could recursively start lazy loading.
	       Instead we know that INIT is already valid so we can just
	       apply that directly.  */
	    res = get_template_parm_object (init, name, /*check_init=*/false);
	    int tag = insert (res);
	    dump (dumper::TREE)
	      && dump ("Created nttp object:%d %N", tag, name);
	  }
      }
      break;

    case tt_enum_value:
      /* An enum const value.  */
      {
	if (tree decl = tree_node ())
	  {
	    dump (dumper::TREE) && dump ("Read enum value %N", decl);
	    res = DECL_INITIAL (decl);
	  }

	if (!res)
	  set_overrun ();
      }
      break;

    case tt_enum_decl:
      /* An enum decl.  */
      {
	tree ctx = tree_node ();
	tree name = tree_node ();

	if (!get_overrun ()
	    && TREE_CODE (ctx) == ENUMERAL_TYPE)
	  res = find_enum_member (ctx, name);

	if (!res)
	  set_overrun ();
	else
	  {
	    int tag = insert (res);
	    dump (dumper::TREE)
	      && dump ("Read enum decl:%d %C:%N", tag, TREE_CODE (res), res);
	  }
      }
      break;

    case tt_data_member:
      /* A data member.  */
      {
	tree ctx = tree_node ();
	tree name = tree_node ();

	if (!get_overrun ()
	    && RECORD_OR_UNION_TYPE_P (ctx))
	  {
	    if (name)
	      res = lookup_class_binding (ctx, name);
	    else
	      res = lookup_field_ident (ctx, u ());

	    if (!res
		|| TREE_CODE (res) != FIELD_DECL
		|| DECL_CONTEXT (res) != ctx)
	      res = NULL_TREE;
	  }

	if (!res)
	  set_overrun ();
	else
	  {
	    int tag = insert (res);
	    dump (dumper::TREE)
	      && dump ("Read member:%d %C:%N", tag, TREE_CODE (res), res);
	  }
      }
      break;

    case tt_binfo:
      /* A BINFO.  Walk the tree of the dominating type.  */
      {
	tree type;
	unsigned ix = binfo_mergeable (&type);
	if (type)
	  {
	    res = TYPE_BINFO (type);
	    for (; ix && res; res = TREE_CHAIN (res))
	      ix--;
	    if (!res)
	      set_overrun ();
	  }

	if (get_overrun ())
	  break;

	/* Insert binfo into backreferences.  */
	tag = insert (res);
	dump (dumper::TREE) && dump ("Read binfo:%d %N", tag, res);
      }
      break;

    case tt_vtable:
      {
	unsigned ix = u ();
	tree ctx = tree_node ();
	dump (dumper::TREE) && dump ("Reading vtable %N[%u]", ctx, ix);
	if (TREE_CODE (ctx) == RECORD_TYPE && TYPE_LANG_SPECIFIC (ctx))
	  for (res = CLASSTYPE_VTABLES (ctx); res; res = DECL_CHAIN (res))
	    if (!ix--)
	      break;
	if (!res)
	  set_overrun ();
      }
      break;

    case tt_thunk:
      {
	int fixed = i ();
	tree target = tree_node ();
	tree virt = tree_node ();

	for (tree thunk = DECL_THUNKS (target);
	     thunk; thunk = DECL_CHAIN (thunk))
	  if (THUNK_FIXED_OFFSET (thunk) == fixed
	      && !THUNK_VIRTUAL_OFFSET (thunk) == !virt
	      && (!virt
		  || tree_int_cst_equal (virt, THUNK_VIRTUAL_OFFSET (thunk))))
	    {
	      res = thunk;
	      break;
	    }

	int tag = insert (res);
	if (res)
	  dump (dumper::TREE)
	    && dump ("Read:%d thunk %N to %N", tag, DECL_NAME (res), target);
	else
	  set_overrun ();
      }
      break;

    case tt_clone_ref:
      {
	tree target = tree_node ();
	tree name = tree_node ();

	if (DECL_P (target) && DECL_MAYBE_IN_CHARGE_CDTOR_P (target))
	  {
	    tree clone;
	    FOR_EVERY_CLONE (clone, target)
	      if (DECL_NAME (clone) == name)
		{
		  res = clone;
		  break;
		}
	  }

	/* A clone might have a different vtable entry.  */
	if (res && DECL_VIRTUAL_P (res))
	  DECL_VINDEX (res) = tree_node ();

	if (!res)
	  set_overrun ();
	int tag = insert (res);
	if (res)
	  dump (dumper::TREE)
	    && dump ("Read:%d clone %N of %N", tag, DECL_NAME (res), target);
	else
	  set_overrun ();
       }
      break;

    case tt_entity:
      /* Index into the entity table.  Perhaps not loaded yet!  */
      {
	unsigned origin = state->slurp->remap_module (u ());
	unsigned ident = u ();
	module_state *from = (*modules)[origin];

	if (!origin || ident >= from->entity_num)
	  set_overrun ();
	if (!get_overrun ())
	  {
	    binding_slot *slot = &(*entity_ary)[from->entity_lwm + ident];
	    if (slot->is_lazy ())
	      if (!from->lazy_load (ident, slot))
		set_overrun ();
	    res = *slot;
	  }

	if (res)
	  {
	    const char *kind = (origin != state->mod ? "Imported" : "Named");
	    int tag = insert (res);
	    dump (dumper::TREE)
	      && dump ("%s:%d %C:%N@%M", kind, tag, TREE_CODE (res),
		       res, (*modules)[origin]);

	    if (!add_indirects (res))
	      {
		set_overrun ();
		res = NULL_TREE;
	      }
	  }
      }
      break;

    case tt_template:
      /* A template.  */
      if (tree tpl = tree_node ())
	{
	  res = DECL_TEMPLATE_RESULT (tpl);
	  dump (dumper::TREE)
	    && dump ("Read template %C:%N", TREE_CODE (res), res);
	}
      break;
    }

  if (is_use && !unused && res && DECL_P (res) && !TREE_USED (res))
    {
      /* Mark decl used as mark_used does -- we cannot call
	 mark_used in the middle of streaming, we only need a subset
	 of its functionality.   */
      TREE_USED (res) = true;

      /* And for structured bindings also the underlying decl.  */
      if (DECL_DECOMPOSITION_P (res) && !DECL_DECOMP_IS_BASE (res))
	TREE_USED (DECL_DECOMP_BASE (res)) = true;

      if (DECL_CLONED_FUNCTION_P (res))
	TREE_USED (DECL_CLONED_FUNCTION (res)) = true;
    }

  dump.outdent ();
  return res;
}

void
trees_out::tpl_parms (tree parms, unsigned &tpl_levels)
{
  if (!parms)
    return;

  if (TREE_VISITED (parms))
    {
      ref_node (parms);
      return;
    }

  tpl_parms (TREE_CHAIN (parms), tpl_levels);

  tree vec = TREE_VALUE (parms);
  unsigned len = TREE_VEC_LENGTH (vec);
  /* Depth.  */
  int tag = insert (parms);
  if (streaming_p ())
    {
      i (len + 1);
      dump (dumper::TREE)
	&& dump ("Writing template parms:%d level:%N length:%d",
		 tag, TREE_PURPOSE (parms), len);
    }
  tree_node (TREE_PURPOSE (parms));

  for (unsigned ix = 0; ix != len; ix++)
    {
      tree parm = TREE_VEC_ELT (vec, ix);
      tree decl = TREE_VALUE (parm);

      gcc_checking_assert (DECL_TEMPLATE_PARM_P (decl));
      if (CHECKING_P)
	switch (TREE_CODE (decl))
	  {
	  default: gcc_unreachable ();

	  case TEMPLATE_DECL:
	    gcc_assert ((TREE_CODE (TREE_TYPE (decl)) == TEMPLATE_TEMPLATE_PARM)
			&& (TREE_CODE (DECL_TEMPLATE_RESULT (decl)) == TYPE_DECL)
			&& (TYPE_NAME (TREE_TYPE (decl)) == decl));
	    break;

	  case TYPE_DECL:
	    gcc_assert ((TREE_CODE (TREE_TYPE (decl)) == TEMPLATE_TYPE_PARM)
			&& (TYPE_NAME (TREE_TYPE (decl)) == decl));
	    break;

	  case PARM_DECL:
	    gcc_assert ((TREE_CODE (DECL_INITIAL (decl)) == TEMPLATE_PARM_INDEX)
			&& (TREE_CODE (TEMPLATE_PARM_DECL (DECL_INITIAL (decl)))
			    == CONST_DECL)
			&& (DECL_TEMPLATE_PARM_P
			    (TEMPLATE_PARM_DECL (DECL_INITIAL (decl)))));
	    break;
	  }

      tree_node (decl);
      tree_node (TEMPLATE_PARM_CONSTRAINTS (parm));
    }

  tpl_levels++;
}

tree
trees_in::tpl_parms (unsigned &tpl_levels)
{
  tree parms = NULL_TREE;

  while (int len = i ())
    {
      if (len < 0)
	{
	  parms = back_ref (len);
	  continue;
	}

      len -= 1;
      parms = tree_cons (NULL_TREE, NULL_TREE, parms);
      int tag = insert (parms);
      TREE_PURPOSE (parms) = tree_node ();

      dump (dumper::TREE)
	&& dump ("Reading template parms:%d level:%N length:%d",
		 tag, TREE_PURPOSE (parms), len);

      tree vec = make_tree_vec (len);
      for (int ix = 0; ix != len; ix++)
	{
	  tree decl = tree_node ();
	  if (!decl)
	    return NULL_TREE;

	  tree parm = build_tree_list (NULL, decl);
	  TEMPLATE_PARM_CONSTRAINTS (parm) = tree_node ();

	  TREE_VEC_ELT (vec, ix) = parm;
	}

      TREE_VALUE (parms) = vec;
      tpl_levels++;
    }

  return parms;
}

void
trees_out::tpl_parms_fini (tree tmpl, unsigned tpl_levels)
{
  for (tree parms = DECL_TEMPLATE_PARMS (tmpl);
       tpl_levels--; parms = TREE_CHAIN (parms))
    {
      tree vec = TREE_VALUE (parms);

      tree_node (TREE_TYPE (vec));
      for (unsigned ix = TREE_VEC_LENGTH (vec); ix--;)
	{
	  tree parm = TREE_VEC_ELT (vec, ix);
	  tree dflt = TREE_PURPOSE (parm);
	  tree_node (dflt);

	  /* Template template parameters need a context of their owning
	     template. This is quite tricky to infer correctly on stream-in
	     (see PR c++/98881) so we'll just provide it directly.  */
	  tree decl = TREE_VALUE (parm);
	  if (TREE_CODE (decl) == TEMPLATE_DECL)
	    tree_node (DECL_CONTEXT (decl));
	}
    }
}

bool
trees_in::tpl_parms_fini (tree tmpl, unsigned tpl_levels)
{
  for (tree parms = DECL_TEMPLATE_PARMS (tmpl);
       tpl_levels--; parms = TREE_CHAIN (parms))
    {
      tree vec = TREE_VALUE (parms);

      TREE_TYPE (vec) = tree_node ();
      for (unsigned ix = TREE_VEC_LENGTH (vec); ix--;)
	{
	  tree parm = TREE_VEC_ELT (vec, ix);
	  tree dflt = tree_node ();
	  TREE_PURPOSE (parm) = dflt;

	  tree decl = TREE_VALUE (parm);
	  if (TREE_CODE (decl) == TEMPLATE_DECL)
	    DECL_CONTEXT (decl) = tree_node ();

	  if (get_overrun ())
	    return false;
	}
    }
  return true;
}

/* PARMS is a LIST, one node per level.
   TREE_VALUE is a TREE_VEC of parm info for that level.
   each ELT is a TREE_LIST
   TREE_VALUE is PARM_DECL, TYPE_DECL or TEMPLATE_DECL
   TREE_PURPOSE is the default value.  */

void
trees_out::tpl_header (tree tpl, unsigned *tpl_levels)
{
  tree parms = DECL_TEMPLATE_PARMS (tpl);
  tpl_parms (parms, *tpl_levels);

  /* Mark end.  */
  if (streaming_p ())
    u (0);

  if (*tpl_levels)
    tree_node (TEMPLATE_PARMS_CONSTRAINTS (parms));
}

bool
trees_in::tpl_header (tree tpl, unsigned *tpl_levels)
{
  tree parms = tpl_parms (*tpl_levels);
  if (!parms)
    return false;

  DECL_TEMPLATE_PARMS (tpl) = parms;

  if (*tpl_levels)
    TEMPLATE_PARMS_CONSTRAINTS (parms) = tree_node ();

  return true;
}

/* Stream skeleton parm nodes, with their flags, type & parm indices.
   All the parms will have consecutive tags.  */

void
trees_out::fn_parms_init (tree fn)
{
  /* First init them.  */
  int base_tag = ref_num - 1;
  int ix = 0;
  for (tree parm = DECL_ARGUMENTS (fn);
       parm; parm = DECL_CHAIN (parm), ix++)
    {
      if (streaming_p ())
	{
	  start (parm);
	  tree_node_bools (parm);
	}
      int tag = insert (parm);
      gcc_checking_assert (base_tag - ix == tag);
    }
  /* Mark the end.  */
  if (streaming_p ())
    u (0);

  /* Now stream their contents.  */
  ix = 0;
  for (tree parm = DECL_ARGUMENTS (fn);
       parm; parm = DECL_CHAIN (parm), ix++)
    {
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Writing parm:%d %u (%N) of %N",
		   base_tag - ix, ix, parm, fn);
      tree_node_vals (parm);
    }

  if (!streaming_p ())
    {
      /* We must walk contract attrs so the dependency graph is complete. */
      for (tree contract = DECL_CONTRACTS (fn);
	  contract;
	  contract = CONTRACT_CHAIN (contract))
	tree_node (contract);
    }

  /* Write a reference to contracts pre/post functions, if any, to avoid
     regenerating them in importers.  */
  tree_node (DECL_PRE_FN (fn));
  tree_node (DECL_POST_FN (fn));
}

/* Build skeleton parm nodes, read their flags, type & parm indices.  */

int
trees_in::fn_parms_init (tree fn)
{
  int base_tag = ~(int)back_refs.length ();

  tree *parm_ptr = &DECL_ARGUMENTS (fn);
  int ix = 0;
  for (; int code = u (); ix++)
    {
      tree parm = start (code);
      if (!tree_node_bools (parm))
	return 0;

      int tag = insert (parm);
      gcc_checking_assert (base_tag - ix == tag);
      *parm_ptr = parm;
      parm_ptr = &DECL_CHAIN (parm);
    }

  ix = 0;
  for (tree parm = DECL_ARGUMENTS (fn);
       parm; parm = DECL_CHAIN (parm), ix++)
    {
      dump (dumper::TREE)
	&& dump ("Reading parm:%d %u (%N) of %N",
		 base_tag - ix, ix, parm, fn);
      if (!tree_node_vals (parm))
	return 0;
    }

  /* Reload references to contract functions, if any.  */
  tree pre_fn = tree_node ();
  tree post_fn = tree_node ();
  set_contract_functions (fn, pre_fn, post_fn);

  return base_tag;
}

/* Read the remaining parm node data.  Replace with existing (if
   non-null) in the map.  */

void
trees_in::fn_parms_fini (int tag, tree fn, tree existing, bool is_defn)
{
  tree existing_parm = existing ? DECL_ARGUMENTS (existing) : NULL_TREE;
  tree parms = DECL_ARGUMENTS (fn);
  unsigned ix = 0;
  for (tree parm = parms; parm; parm = DECL_CHAIN (parm), ix++)
    {
      if (existing_parm)
	{
	  if (is_defn && !DECL_SAVED_TREE (existing))
	    {
	      /* If we're about to become the definition, set the
		 names of the parms from us.  */
	      DECL_NAME (existing_parm) = DECL_NAME (parm);
	      DECL_SOURCE_LOCATION (existing_parm) = DECL_SOURCE_LOCATION (parm);
	    }

	  back_refs[~tag] = existing_parm;
	  existing_parm = DECL_CHAIN (existing_parm);
	}
      tag--;
    }
}

/* Encode into KEY the position of the local type (class or enum)
   declaration DECL within FN.  The position is encoded as the
   index of the innermost BLOCK (numbered in BFS order) along with
   the index within its BLOCK_VARS list.  */

void
trees_out::key_local_type (merge_key& key, tree decl, tree fn)
{
  auto_vec<tree, 4> blocks;
  blocks.quick_push (DECL_INITIAL (fn));
  unsigned block_ix = 0;
  while (block_ix != blocks.length ())
    {
      tree block = blocks[block_ix];
      unsigned decl_ix = 0;
      for (tree var = BLOCK_VARS (block); var; var = DECL_CHAIN (var))
	{
	  if (TREE_CODE (var) != TYPE_DECL)
	    continue;
	  if (var == decl)
	    {
	      key.index = (block_ix << 10) | decl_ix;
	      return;
	    }
	  ++decl_ix;
	}
      for (tree sub = BLOCK_SUBBLOCKS (block); sub; sub = BLOCK_CHAIN (sub))
	blocks.safe_push (sub);
      ++block_ix;
    }

  /* Not-found value.  */
  key.index = 1023;
}

/* Look up the local type corresponding at the position encoded by
   KEY within FN and named NAME.  */

tree
trees_in::key_local_type (const merge_key& key, tree fn, tree name)
{
  if (!DECL_INITIAL (fn))
    return NULL_TREE;

  const unsigned block_pos = key.index >> 10;
  const unsigned decl_pos = key.index & 1023;

  if (decl_pos == 1023)
    return NULL_TREE;

  auto_vec<tree, 4> blocks;
  blocks.quick_push (DECL_INITIAL (fn));
  unsigned block_ix = 0;
  while (block_ix != blocks.length ())
    {
      tree block = blocks[block_ix];
      if (block_ix == block_pos)
	{
	  unsigned decl_ix = 0;
	  for (tree var = BLOCK_VARS (block); var; var = DECL_CHAIN (var))
	    {
	      if (TREE_CODE (var) != TYPE_DECL)
		continue;
	      /* Prefer using the identifier as the key for more robustness
		 to ODR violations, except for anonymous types since their
		 compiler-generated identifiers aren't stable.  */
	      if (IDENTIFIER_ANON_P (name)
		  ? decl_ix == decl_pos
		  : DECL_NAME (var) == name)
		return var;
	      ++decl_ix;
	    }
	  return NULL_TREE;
	}
      for (tree sub = BLOCK_SUBBLOCKS (block); sub; sub = BLOCK_CHAIN (sub))
	blocks.safe_push (sub);
      ++block_ix;
    }

  return NULL_TREE;
}

/* DEP is the depset of some decl we're streaming by value.  Determine
   the merging behaviour.  */

merge_kind
trees_out::get_merge_kind (tree decl, depset *dep)
{
  if (!dep)
    {
      if (VAR_OR_FUNCTION_DECL_P (decl))
	{
	  /* Any var or function with template info should have DEP.  */
	  gcc_checking_assert (!DECL_LANG_SPECIFIC (decl)
			       || !DECL_TEMPLATE_INFO (decl));
	  if (DECL_LOCAL_DECL_P (decl))
	    return MK_unique;
	}

      /* Either unique, or some member of a class that cannot have an
	 out-of-class definition.  For instance a FIELD_DECL.  */
      tree ctx = CP_DECL_CONTEXT (decl);
      if (TREE_CODE (ctx) == FUNCTION_DECL)
	{
	  /* USING_DECLs and NAMESPACE_DECLs cannot have DECL_TEMPLATE_INFO --
	     this isn't permitting them to have one.   */
	  gcc_checking_assert (TREE_CODE (decl) == USING_DECL
			       || TREE_CODE (decl) == NAMESPACE_DECL
			       || !DECL_LANG_SPECIFIC (decl)
			       || !DECL_TEMPLATE_INFO (decl));

	  return MK_unique;
	}

      if (TREE_CODE (decl) == TEMPLATE_DECL
	  && DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (decl))
	return MK_local_friend;

      gcc_checking_assert (TYPE_P (ctx));
      if (TREE_CODE (decl) == USING_DECL)
	return MK_field;

      if (TREE_CODE (decl) == FIELD_DECL)
	{
	  if (DECL_NAME (decl))
	    {
	      /* Anonymous FIELD_DECLs have a NULL name.  */
	      gcc_checking_assert (!IDENTIFIER_ANON_P (DECL_NAME (decl)));
	      return MK_named;
	    }

	  if (!DECL_NAME (decl)
	      && !RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl))
	      && !DECL_BIT_FIELD_REPRESENTATIVE (decl))
	    {
	      /* The underlying storage unit for a bitfield.  We do not
		 need to dedup it, because it's only reachable through
		 the bitfields it represents.  And those are deduped.  */
	      // FIXME: Is that assertion correct -- do we ever fish it
	      // out and put it in an expr?
	      gcc_checking_assert ((TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
				    ? TREE_CODE (TREE_TYPE (TREE_TYPE (decl)))
				    : TREE_CODE (TREE_TYPE (decl)))
				   == INTEGER_TYPE);
	      return MK_unique;
	    }

	  return MK_field;
	}

      if (TREE_CODE (decl) == CONST_DECL)
	return MK_named;

      if (TREE_CODE (decl) == VAR_DECL
	  && DECL_VTABLE_OR_VTT_P (decl))
	return MK_vtable;

      if (DECL_THUNK_P (decl))
	/* Thunks are unique-enough, because they're only referenced
	   from the vtable.  And that's either new (so we want the
	   thunks), or it's a duplicate (so it will be dropped).  */
	return MK_unique;

      /* There should be no other cases.  */
      gcc_unreachable ();
    }

  gcc_checking_assert (TREE_CODE (decl) != FIELD_DECL
		       && TREE_CODE (decl) != USING_DECL
		       && TREE_CODE (decl) != CONST_DECL);

  if (is_key_order ())
    {
      /* When doing the mergeablilty graph, there's an indirection to
	 the actual depset.  */
      gcc_assert (dep->is_special ());
      dep = dep->deps[0];
    }

  gcc_checking_assert (decl == dep->get_entity ());

  merge_kind mk = MK_named;
  switch (dep->get_entity_kind ())
    {
    default:
      gcc_unreachable ();

    case depset::EK_PARTIAL:
      mk = MK_partial;
      break;

    case depset::EK_DECL:
      {
	tree ctx = CP_DECL_CONTEXT (decl);

	switch (TREE_CODE (ctx))
	  {
	  default:
	    gcc_unreachable ();

	  case FUNCTION_DECL:
	    gcc_checking_assert
	      (DECL_IMPLICIT_TYPEDEF_P (STRIP_TEMPLATE (decl)));

	    mk = MK_local_type;
	    break;

	  case RECORD_TYPE:
	  case UNION_TYPE:
	  case NAMESPACE_DECL:
	    if (DECL_NAME (decl) == as_base_identifier)
	      {
		mk = MK_as_base;
		break;
	      }

	    /* A lambda may have a class as its context, even though it
	       isn't a member in the traditional sense; see the test
	       g++.dg/modules/lambda-6_a.C.  */
	    if (DECL_IMPLICIT_TYPEDEF_P (STRIP_TEMPLATE (decl))
		&& LAMBDA_TYPE_P (TREE_TYPE (decl)))
	      if (tree scope = LAMBDA_TYPE_EXTRA_SCOPE (TREE_TYPE (decl)))
		{
		  /* Lambdas attached to fields are keyed to its class.  */
		  if (TREE_CODE (scope) == FIELD_DECL)
		    scope = TYPE_NAME (DECL_CONTEXT (scope));
		  if (DECL_LANG_SPECIFIC (scope)
		      && DECL_MODULE_KEYED_DECLS_P (scope))
		    {
		      mk = MK_keyed;
		      break;
		    }
		}

	    if (TREE_CODE (decl) == TEMPLATE_DECL
		&& DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (decl))
	      {
		mk = MK_local_friend;
		break;
	      }

	    if (IDENTIFIER_ANON_P (DECL_NAME (decl)))
	      {
		if (RECORD_OR_UNION_TYPE_P (ctx))
		  mk = MK_field;
		else if (DECL_IMPLICIT_TYPEDEF_P (decl)
			 && UNSCOPED_ENUM_P (TREE_TYPE (decl))
			 && TYPE_VALUES (TREE_TYPE (decl)))
		  /* Keyed by first enum value, and underlying type.  */
		  mk = MK_enum;
		else
		  /* No way to merge it, it is an ODR land-mine.  */
		  mk = MK_unique;
	      }
	  }
      }
      break;

    case depset::EK_SPECIALIZATION:
      {
	gcc_checking_assert (dep->is_special ());

	if (TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)
	  /* An block-scope classes of templates are themselves
	     templates.  */
	  gcc_checking_assert (DECL_IMPLICIT_TYPEDEF_P (decl));

	if (dep->is_friend_spec ())
	  mk = MK_friend_spec;
	else if (dep->is_type_spec ())
	  mk = MK_type_spec;
	else
	  mk = MK_decl_spec;

	if (TREE_CODE (decl) == TEMPLATE_DECL)
	  {
	    spec_entry *entry = reinterpret_cast <spec_entry *> (dep->deps[0]);
	    if (TREE_CODE (entry->spec) != TEMPLATE_DECL)
	      mk = merge_kind (mk | MK_tmpl_tmpl_mask);
	  }
      }
      break;
    }

  return mk;
}


/* The container of DECL -- not necessarily its context!  */

tree
trees_out::decl_container (tree decl)
{
  int use_tpl;
  tree tpl = NULL_TREE;
  if (tree template_info = node_template_info (decl, use_tpl))
    tpl = TI_TEMPLATE (template_info);
  if (tpl == decl)
    tpl = nullptr;

  /* Stream the template we're instantiated from.  */
  tree_node (tpl);

  tree container = NULL_TREE;
  if (TREE_CODE (decl) == TEMPLATE_DECL
      && DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (decl))
    container = DECL_CHAIN (decl);
  else
    container = CP_DECL_CONTEXT (decl);

  if (TYPE_P (container))
    container = TYPE_NAME (container);

  tree_node (container);

  return container;
}

tree
trees_in::decl_container ()
{
  /* The maybe-template.  */
  (void)tree_node ();

  tree container = tree_node ();

  return container;
}

/* Write out key information about a mergeable DEP.  Does not write
   the contents of DEP itself.  The context has already been
   written.  The container has already been streamed.  */

void
trees_out::key_mergeable (int tag, merge_kind mk, tree decl, tree inner,
			  tree container, depset *dep)
{
  if (dep && is_key_order ())
    {
      gcc_checking_assert (dep->is_special ());
      dep = dep->deps[0];
    }

  if (streaming_p ())
    dump (dumper::MERGE)
      && dump ("Writing:%d's %s merge key (%s) %C:%N", tag, merge_kind_name[mk],
	       dep ? dep->entity_kind_name () : "contained",
	       TREE_CODE (decl), decl);

  /* Now write the locating information. */
  if (mk & MK_template_mask)
    {
      /* Specializations are located via their originating template,
	 and the set of template args they specialize.  */
      gcc_checking_assert (dep && dep->is_special ());
      spec_entry *entry = reinterpret_cast <spec_entry *> (dep->deps[0]);

      tree_node (entry->tmpl);
      tree_node (entry->args);
      if (mk & MK_tmpl_decl_mask)
	if (flag_concepts && TREE_CODE (inner) == VAR_DECL)
	  {
	    /* Variable template partial specializations might need
	       constraints (see spec_hasher::equal).  It's simpler to
	       write NULL when we don't need them.  */
	    tree constraints = NULL_TREE;

	    if (uses_template_parms (entry->args))
	      constraints = get_constraints (inner);
	    tree_node (constraints);
	  }

      if (CHECKING_P)
	{
	  /* Make sure we can locate the decl.  */
	  tree existing = match_mergeable_specialization
	    (bool (mk & MK_tmpl_decl_mask), entry);

	  gcc_assert (existing);
	  if (mk & MK_tmpl_decl_mask)
	    {
	      if (mk & MK_tmpl_tmpl_mask)
		existing = DECL_TI_TEMPLATE (existing);
	    }
	  else
	    {
	      if (mk & MK_tmpl_tmpl_mask)
		existing = CLASSTYPE_TI_TEMPLATE (existing);
	      else
		existing = TYPE_NAME (existing);
	    }

	  /* The walkabout should have found ourselves.  */
	  gcc_checking_assert (TREE_CODE (decl) == TYPE_DECL
			       ? same_type_p (TREE_TYPE (decl),
					      TREE_TYPE (existing))
			       : existing == decl);
	}
    }
  else if (mk != MK_unique)
    {
      merge_key key;
      tree name = DECL_NAME (decl);

      switch (mk)
	{
	default:
	  gcc_unreachable ();

	case MK_named:
	case MK_friend_spec:
	  if (IDENTIFIER_CONV_OP_P (name))
	    name = conv_op_identifier;

	  if (TREE_CODE (inner) == FUNCTION_DECL)
	    {
	      /* Functions are distinguished by parameter types.  */
	      tree fn_type = TREE_TYPE (inner);

	      key.ref_q = type_memfn_rqual (fn_type);
	      key.args = TYPE_ARG_TYPES (fn_type);

	      if (tree reqs = get_constraints (inner))
		{
		  if (cxx_dialect < cxx20)
		    reqs = CI_ASSOCIATED_CONSTRAINTS (reqs);
		  else
		    reqs = CI_DECLARATOR_REQS (reqs);
		  key.constraints = reqs;
		}

	      if (IDENTIFIER_CONV_OP_P (name)
		  || (decl != inner
		      && !(name == fun_identifier
			   /* In case the user names something _FUN  */
			   && LAMBDA_TYPE_P (DECL_CONTEXT (inner)))))
		/* And a function template, or conversion operator needs
		   the return type.  Except for the _FUN thunk of a
		   generic lambda, which has a recursive decl_type'd
		   return type.  */
		// FIXME: What if the return type is a voldemort?
		key.ret = fndecl_declared_return_type (inner);
	    }
	  break;

	case MK_field:
	  {
	    unsigned ix = 0;
	    if (TREE_CODE (inner) != FIELD_DECL)
	      name = NULL_TREE;
	    else
	      gcc_checking_assert (!name || !IDENTIFIER_ANON_P (name));

	    for (tree field = TYPE_FIELDS (TREE_TYPE (container));
		 ; field = DECL_CHAIN (field))
	      {
		tree finner = STRIP_TEMPLATE (field);
		if (TREE_CODE (finner) == TREE_CODE (inner))
		  {
		    if (finner == inner)
		      break;
		    ix++;
		  }
	      }
	    key.index = ix;
	  }
	  break;

	case MK_vtable:
	  {
	    tree vtable = CLASSTYPE_VTABLES (TREE_TYPE (container));
	    for (unsigned ix = 0; ; vtable = DECL_CHAIN (vtable), ix++)
	      if (vtable == decl)
		{
		  key.index = ix;
		  break;
		}
	    name = NULL_TREE;
	  }
	  break;

	case MK_as_base:
	  gcc_checking_assert
	    (decl == TYPE_NAME (CLASSTYPE_AS_BASE (TREE_TYPE (container))));
	  break;

	case MK_local_friend:
	  {
	    /* Find by index on the class's DECL_LIST  */
	    unsigned ix = 0;
	    for (tree decls = CLASSTYPE_DECL_LIST (TREE_CHAIN (decl));
		 decls; decls = TREE_CHAIN (decls))
	      if (!TREE_PURPOSE (decls))
		{
		  tree frnd = friend_from_decl_list (TREE_VALUE (decls));
		  if (frnd == decl)
		    break;
		  ix++;
		}
	    key.index = ix;
	    name = NULL_TREE;
	  }
	  break;

	case MK_local_type:
	  key_local_type (key, STRIP_TEMPLATE (decl), container);
	  break;

	case MK_enum:
	  {
	    /* Anonymous enums are located by their first identifier,
	       and underlying type.  */
	    tree type = TREE_TYPE (decl);

	    gcc_checking_assert (UNSCOPED_ENUM_P (type));
	    /* Using the type name drops the bit precision we might
	       have been using on the enum.  */
	    key.ret = TYPE_NAME (ENUM_UNDERLYING_TYPE (type));
	    if (tree values = TYPE_VALUES (type))
	      name = DECL_NAME (TREE_VALUE (values));
	  }
	  break;

	case MK_keyed:
	  {
	    gcc_checking_assert (LAMBDA_TYPE_P (TREE_TYPE (inner)));
	    tree scope = LAMBDA_TYPE_EXTRA_SCOPE (TREE_TYPE (inner));
	    gcc_checking_assert (TREE_CODE (scope) == VAR_DECL
				 || TREE_CODE (scope) == FIELD_DECL
				 || TREE_CODE (scope) == PARM_DECL
				 || TREE_CODE (scope) == TYPE_DECL);
	    /* Lambdas attached to fields are keyed to the class.  */
	    if (TREE_CODE (scope) == FIELD_DECL)
	      scope = TYPE_NAME (DECL_CONTEXT (scope));
	    auto *root = keyed_table->get (scope);
	    unsigned ix = root->length ();
	    /* If we don't find it, we'll write a really big number
	       that the reader will ignore.  */
	    while (ix--)
	      if ((*root)[ix] == inner)
		break;

	    /* Use the keyed-to decl as the 'name'.  */
	    name = scope;
	    key.index = ix;
	  }
	  break;

	case MK_partial:
	  {
	    tree ti = get_template_info (inner);
	    key.constraints = get_constraints (inner);
	    key.ret = TI_TEMPLATE (ti);
	    key.args = TI_ARGS (ti);
	  }
	  break;
	}

      tree_node (name);
      if (streaming_p ())
	{
	  unsigned code = (key.ref_q << 0) | (key.index << 2);
	  u (code);
	}

      if (mk == MK_enum)
	tree_node (key.ret);
      else if (mk == MK_partial
	       || (mk == MK_named && inner
		   && TREE_CODE (inner) == FUNCTION_DECL))
	{
	  tree_node (key.ret);
	  tree arg = key.args;
	  if (mk == MK_named)
	    while (arg && arg != void_list_node)
	      {
		tree_node (TREE_VALUE (arg));
		arg = TREE_CHAIN (arg);
	      }
	  tree_node (arg);
	  tree_node (key.constraints);
	}
    }
}

/* DECL is a new declaration that may be duplicated in OVL.  Use RET &
   ARGS to find its clone, or NULL.  If DECL's DECL_NAME is NULL, this
   has been found by a proxy.  It will be an enum type located by its
   first member.

   We're conservative with matches, so ambiguous decls will be
   registered as different, then lead to a lookup error if the two
   modules are both visible.  Perhaps we want to do something similar
   to duplicate decls to get ODR errors on loading?  We already have
   some special casing for namespaces.  */

static tree
check_mergeable_decl (merge_kind mk, tree decl, tree ovl, merge_key const &key)
{
  tree found = NULL_TREE;
  for (ovl_iterator iter (ovl); !found && iter; ++iter)
    {
      tree match = *iter;

      tree d_inner = decl;
      tree m_inner = match;

    again:
      if (TREE_CODE (d_inner) != TREE_CODE (m_inner))
	{
	  if (TREE_CODE (match) == NAMESPACE_DECL
	      && !DECL_NAMESPACE_ALIAS (match))
	    /* Namespaces are never overloaded.  */
	    found = match;

	  continue;
	}

      switch (TREE_CODE (d_inner))
	{
	case TEMPLATE_DECL:
	  if (template_heads_equivalent_p (d_inner, m_inner))
	    {
	      d_inner = DECL_TEMPLATE_RESULT (d_inner);
	      m_inner = DECL_TEMPLATE_RESULT (m_inner);
	      if (d_inner == error_mark_node
		  && TYPE_DECL_ALIAS_P (m_inner))
		{
		  found = match;
		  break;
		}
	      goto again;
	    }
	  break;

	case FUNCTION_DECL:
	  if (tree m_type = TREE_TYPE (m_inner))
	    if ((!key.ret
		 || same_type_p (key.ret, fndecl_declared_return_type (m_inner)))
		&& type_memfn_rqual (m_type) == key.ref_q
		&& compparms (key.args, TYPE_ARG_TYPES (m_type))
		/* Reject if old is a "C" builtin and new is not "C".
		   Matches decls_match behaviour.  */
		&& (!DECL_IS_UNDECLARED_BUILTIN (m_inner)
		    || !DECL_EXTERN_C_P (m_inner)
		    || DECL_EXTERN_C_P (d_inner))
		/* Reject if one is a different member of a
		   guarded/pre/post fn set.  */
		&& (!flag_contracts
		    || (DECL_IS_PRE_FN_P (d_inner)
			== DECL_IS_PRE_FN_P (m_inner)))
		&& (!flag_contracts
		    || (DECL_IS_POST_FN_P (d_inner)
			== DECL_IS_POST_FN_P (m_inner))))
	      {
		tree m_reqs = get_constraints (m_inner);
		if (m_reqs)
		  {
		    if (cxx_dialect < cxx20)
		      m_reqs = CI_ASSOCIATED_CONSTRAINTS (m_reqs);
		    else
		      m_reqs = CI_DECLARATOR_REQS (m_reqs);
		  }

		if (cp_tree_equal (key.constraints, m_reqs))
		  found = match;
	      }
	  break;

	case TYPE_DECL:
	  if (DECL_IMPLICIT_TYPEDEF_P (d_inner)
	      == DECL_IMPLICIT_TYPEDEF_P (m_inner))
	    {
	      if (!IDENTIFIER_ANON_P (DECL_NAME (m_inner)))
		return match;
	      else if (mk == MK_enum
		       && (TYPE_NAME (ENUM_UNDERLYING_TYPE (TREE_TYPE (m_inner)))
			   == key.ret))
		found = match;
	    }
	  break;

	default:
	  found = match;
	  break;
	}
    }

  return found;
}

/* DECL, INNER & TYPE are a skeleton set of nodes for a decl.  Only
   the bools have been filled in.  Read its merging key and merge it.
   Returns the existing decl if there is one.  */

tree
trees_in::key_mergeable (int tag, merge_kind mk, tree decl, tree inner,
			 tree type, tree container, bool is_attached,
			 bool is_imported_temploid_friend)
{
  const char *kind = "new";
  tree existing = NULL_TREE;

  if (mk & MK_template_mask)
    {
      // FIXME: We could stream the specialization hash?
      spec_entry spec;
      spec.tmpl = tree_node ();
      spec.args = tree_node ();

      if (get_overrun ())
	return error_mark_node;

      DECL_NAME (decl) = DECL_NAME (spec.tmpl);
      DECL_CONTEXT (decl) = DECL_CONTEXT (spec.tmpl);
      DECL_NAME (inner) = DECL_NAME (decl);
      DECL_CONTEXT (inner) = DECL_CONTEXT (decl);

      tree constr = NULL_TREE;
      bool is_decl = mk & MK_tmpl_decl_mask;
      if (is_decl)
	{
	  if (flag_concepts && TREE_CODE (inner) == VAR_DECL)
	    {
	      constr = tree_node ();
	      if (constr)
		set_constraints (inner, constr);
	    }
	  spec.spec = (mk & MK_tmpl_tmpl_mask) ? inner : decl;
	}
      else
	spec.spec = type;
      existing = match_mergeable_specialization (is_decl, &spec);
      if (constr)
	/* We'll add these back later, if this is the new decl.  */
	remove_constraints (inner);

      if (!existing)
	; /* We'll add to the table once read.  */
      else if (mk & MK_tmpl_decl_mask)
	{
	  /* A declaration specialization.  */
	  if (mk & MK_tmpl_tmpl_mask)
	    existing = DECL_TI_TEMPLATE (existing);
	}
      else
	{
	  /* A type specialization.  */
	  if (mk & MK_tmpl_tmpl_mask)
	    existing = CLASSTYPE_TI_TEMPLATE (existing);
	  else
	    existing = TYPE_NAME (existing);
	}
    }
  else if (mk == MK_unique)
    kind = "unique";
  else
    {
      tree name = tree_node ();

      merge_key key;
      unsigned code = u ();
      key.ref_q = cp_ref_qualifier ((code >> 0) & 3);
      key.index = code >> 2;

      if (mk == MK_enum)
	key.ret = tree_node ();
      else if (mk == MK_partial
	       || ((mk == MK_named || mk == MK_friend_spec)
		   && TREE_CODE (inner) == FUNCTION_DECL))
	{
	  key.ret = tree_node ();
	  tree arg, *arg_ptr = &key.args;
	  while ((arg = tree_node ())
		 && arg != void_list_node
		 && mk != MK_partial)
	    {
	      *arg_ptr = tree_cons (NULL_TREE, arg, NULL_TREE);
	      arg_ptr = &TREE_CHAIN (*arg_ptr);
	    }
	  *arg_ptr = arg;
	  key.constraints = tree_node ();
	}

      if (get_overrun ())
	return error_mark_node;

      if (mk < MK_indirect_lwm)
	{
	  DECL_NAME (decl) = name;
	  DECL_CONTEXT (decl) = FROB_CONTEXT (container);
	}
      DECL_NAME (inner) = DECL_NAME (decl);
      DECL_CONTEXT (inner) = DECL_CONTEXT (decl);

      if (mk == MK_partial)
	{
	  for (tree spec = DECL_TEMPLATE_SPECIALIZATIONS (key.ret);
	       spec; spec = TREE_CHAIN (spec))
	    {
	      tree tmpl = TREE_VALUE (spec);
	      tree ti = get_template_info (tmpl);
	      if (template_args_equal (key.args, TI_ARGS (ti))
		  && cp_tree_equal (key.constraints,
				    get_constraints
				    (DECL_TEMPLATE_RESULT (tmpl))))
		{
		  existing = tmpl;
		  break;
		}
	    }
	}
      else if (mk == MK_keyed
	       && DECL_LANG_SPECIFIC (name)
	       && DECL_MODULE_KEYED_DECLS_P (name))
	{
	  gcc_checking_assert (TREE_CODE (container) == NAMESPACE_DECL
			       || TREE_CODE (container) == TYPE_DECL);
	  if (auto *set = keyed_table->get (name))
	    if (key.index < set->length ())
	      {
		existing = (*set)[key.index];
		if (existing)
		  {
		    gcc_checking_assert
		      (DECL_IMPLICIT_TYPEDEF_P (existing));
		    if (inner != decl)
		      existing
			= CLASSTYPE_TI_TEMPLATE (TREE_TYPE (existing));
		  }
	      }
	}
      else
	switch (TREE_CODE (container))
	  {
	  default:
	    gcc_unreachable ();

	  case NAMESPACE_DECL:
	    if (is_attached
		&& !is_imported_temploid_friend
		&& !(state->is_module () || state->is_partition ()))
	      kind = "unique";
	    else
	      {
		gcc_checking_assert (mk == MK_named || mk == MK_enum);
		tree mvec;
		tree *vslot = mergeable_namespace_slots (container, name,
							 is_attached, &mvec);
		existing = check_mergeable_decl (mk, decl, *vslot, key);
		if (!existing)
		  add_mergeable_namespace_entity (vslot, decl);
		else
		  {
		    /* Note that we now have duplicates to deal with in
		       name lookup.  */
		    if (is_attached)
		      BINDING_VECTOR_PARTITION_DUPS_P (mvec) = true;
		    else
		      BINDING_VECTOR_GLOBAL_DUPS_P (mvec) = true;
		  }
	      }
	    break;

	  case FUNCTION_DECL:
	    gcc_checking_assert (mk == MK_local_type);
	    existing = key_local_type (key, container, name);
	    if (existing && inner != decl)
	      existing = TYPE_TI_TEMPLATE (TREE_TYPE (existing));
	    break;

	  case TYPE_DECL:
	    gcc_checking_assert (!is_imported_temploid_friend);
	    if (is_attached && !(state->is_module () || state->is_partition ())
		/* Implicit member functions can come from
		   anywhere.  */
		&& !(DECL_ARTIFICIAL (decl)
		     && TREE_CODE (decl) == FUNCTION_DECL
		     && !DECL_THUNK_P (decl)))
	      kind = "unique";
	    else
	      {
		tree ctx = TREE_TYPE (container);

		/* For some reason templated enumeral types are not marked
		   as COMPLETE_TYPE_P, even though they have members.
		   This may well be a bug elsewhere.  */
		if (TREE_CODE (ctx) == ENUMERAL_TYPE)
		  existing = find_enum_member (ctx, name);
		else if (COMPLETE_TYPE_P (ctx))
		  {
		    switch (mk)
		      {
		      default:
			gcc_unreachable ();

		      case MK_named:
			existing = lookup_class_binding (ctx, name);
			if (existing)
			  {
			    tree inner = decl;
			    if (TREE_CODE (inner) == TEMPLATE_DECL
				&& !DECL_MEMBER_TEMPLATE_P (inner))
			      inner = DECL_TEMPLATE_RESULT (inner);

			    existing = check_mergeable_decl
			      (mk, inner, existing, key);

			    if (!existing && DECL_ALIAS_TEMPLATE_P (decl))
			      {} // FIXME: Insert into specialization
			    // tables, we'll need the arguments for that!
			  }
			break;

		      case MK_field:
			{
			  unsigned ix = key.index;
			  for (tree field = TYPE_FIELDS (ctx);
			       field; field = DECL_CHAIN (field))
			    {
			      tree finner = STRIP_TEMPLATE (field);
			      if (TREE_CODE (finner) == TREE_CODE (inner))
				if (!ix--)
				  {
				    existing = field;
				    break;
				  }
			    }
			}
			break;

		      case MK_vtable:
			{
			  unsigned ix = key.index;
			  for (tree vtable = CLASSTYPE_VTABLES (ctx);
			       vtable; vtable = DECL_CHAIN (vtable))
			    if (!ix--)
			      {
				existing = vtable;
				break;
			      }
			}
			break;

		      case MK_as_base:
			{
			  tree as_base = CLASSTYPE_AS_BASE (ctx);
			  if (as_base && as_base != ctx)
			    existing = TYPE_NAME (as_base);
			}
			break;

		      case MK_local_friend:
			{
			  unsigned ix = key.index;
			  for (tree decls = CLASSTYPE_DECL_LIST (ctx);
			       decls; decls = TREE_CHAIN (decls))
			    if (!TREE_PURPOSE (decls) && !ix--)
			      {
				existing
				  = friend_from_decl_list (TREE_VALUE (decls));
				break;
			      }
			}
			break;
		      }

		    if (existing && mk < MK_indirect_lwm && mk != MK_partial
			&& TREE_CODE (decl) == TEMPLATE_DECL
			&& !DECL_MEMBER_TEMPLATE_P (decl))
		      {
			tree ti;
			if (DECL_IMPLICIT_TYPEDEF_P (existing))
			  ti = TYPE_TEMPLATE_INFO (TREE_TYPE (existing));
			else
			  ti = DECL_TEMPLATE_INFO (existing);
			existing = TI_TEMPLATE (ti);
		      }
		  }
	      }
	  }
    }

  dump (dumper::MERGE)
    && dump ("Read:%d's %s merge key (%s) %C:%N", tag, merge_kind_name[mk],
	     existing ? "matched" : kind, TREE_CODE (decl), decl);

  return existing;
}

void
trees_out::binfo_mergeable (tree binfo)
{
  tree dom = binfo;
  while (tree parent = BINFO_INHERITANCE_CHAIN (dom))
    dom = parent;
  tree type = BINFO_TYPE (dom);
  gcc_checking_assert (TYPE_BINFO (type) == dom);
  tree_node (type);
  if (streaming_p ())
    {
      unsigned ix = 0;
      for (; dom != binfo; dom = TREE_CHAIN (dom))
	ix++;
      u (ix);
    }
}

unsigned
trees_in::binfo_mergeable (tree *type)
{
  *type = tree_node ();
  return u ();
}

/* DECL is a just streamed mergeable decl that should match EXISTING.  Check
   it does and issue an appropriate diagnostic if not.  Merge any
   bits from DECL to EXISTING.  This is stricter matching than
   decls_match, because we can rely on ODR-sameness, and we cannot use
   decls_match because it can cause instantiations of constraints.  */

bool
trees_in::is_matching_decl (tree existing, tree decl, bool is_typedef)
{
  // FIXME: We should probably do some duplicate decl-like stuff here
  // (beware, default parms should be the same?)  Can we just call
  // duplicate_decls and teach it how to handle the module-specific
  // permitted/required duplications?

  // We know at this point that the decls have matched by key, so we
  // can elide some of the checking
  gcc_checking_assert (TREE_CODE (existing) == TREE_CODE (decl));

  tree d_inner = decl;
  tree e_inner = existing;
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      d_inner = DECL_TEMPLATE_RESULT (d_inner);
      e_inner = DECL_TEMPLATE_RESULT (e_inner);
      gcc_checking_assert (TREE_CODE (e_inner) == TREE_CODE (d_inner));
    }

  if (TREE_CODE (d_inner) == FUNCTION_DECL)
    {
      tree e_ret = fndecl_declared_return_type (existing);
      tree d_ret = fndecl_declared_return_type (decl);

      if (decl != d_inner && DECL_NAME (d_inner) == fun_identifier
	  && LAMBDA_TYPE_P (DECL_CONTEXT (d_inner)))
	/* This has a recursive type that will compare different.  */;
      else if (!same_type_p (d_ret, e_ret))
	goto mismatch;

      tree e_type = TREE_TYPE (e_inner);
      tree d_type = TREE_TYPE (d_inner);

      if (DECL_EXTERN_C_P (d_inner) != DECL_EXTERN_C_P (e_inner))
	goto mismatch;

      for (tree e_args = TYPE_ARG_TYPES (e_type),
	     d_args = TYPE_ARG_TYPES (d_type);
	   e_args != d_args && (e_args || d_args);
	   e_args = TREE_CHAIN (e_args), d_args = TREE_CHAIN (d_args))
	{
	  if (!(e_args && d_args))
	    goto mismatch;

	  if (!same_type_p (TREE_VALUE (d_args), TREE_VALUE (e_args)))
	    goto mismatch;
	}

      /* If EXISTING has an undeduced or uninstantiated exception
	 specification, but DECL does not, propagate the exception
	 specification.  Otherwise we end up asserting or trying to
	 instantiate it in the middle of loading.   */
      tree e_spec = TYPE_RAISES_EXCEPTIONS (e_type);
      tree d_spec = TYPE_RAISES_EXCEPTIONS (d_type);
      if (DEFERRED_NOEXCEPT_SPEC_P (e_spec))
	{
	  if (!DEFERRED_NOEXCEPT_SPEC_P (d_spec)
	      || (UNEVALUATED_NOEXCEPT_SPEC_P (e_spec)
		  && !UNEVALUATED_NOEXCEPT_SPEC_P (d_spec)))
	    {
	      dump (dumper::MERGE)
		&& dump ("Propagating instantiated noexcept to %N", existing);
	      TREE_TYPE (existing) = d_type;

	      /* Propagate to existing clones.  */
	      tree clone;
	      FOR_EACH_CLONE (clone, existing)
		{
		  if (TREE_TYPE (clone) == e_type)
		    TREE_TYPE (clone) = d_type;
		  else
		    TREE_TYPE (clone)
		      = build_exception_variant (TREE_TYPE (clone), d_spec);
		}
	    }
	}
      else if (!DEFERRED_NOEXCEPT_SPEC_P (d_spec)
	       && !comp_except_specs (d_spec, e_spec, ce_type))
	goto mismatch;

      /* Similarly if EXISTING has an undeduced return type, but DECL's
	 is already deduced.  */
      if (undeduced_auto_decl (existing) && !undeduced_auto_decl (decl))
	{
	  dump (dumper::MERGE)
	    && dump ("Propagating deduced return type to %N", existing);
	  TREE_TYPE (existing) = change_return_type (TREE_TYPE (d_type), e_type);
	}
    }
  else if (is_typedef)
    {
      if (!DECL_ORIGINAL_TYPE (e_inner)
	  || !same_type_p (DECL_ORIGINAL_TYPE (d_inner),
			   DECL_ORIGINAL_TYPE (e_inner)))
	goto mismatch;
    }
  /* Using cp_tree_equal because we can meet TYPE_ARGUMENT_PACKs
     here. I suspect the entities that directly do that are things
     that shouldn't go to duplicate_decls (FIELD_DECLs etc).   */
  else if (!cp_tree_equal (TREE_TYPE (decl), TREE_TYPE (existing)))
    {
    mismatch:
      if (DECL_IS_UNDECLARED_BUILTIN (existing))
	/* Just like duplicate_decls, presum the user knows what
	   they're doing in overriding a builtin.   */
	TREE_TYPE (existing) = TREE_TYPE (decl);
      else if (decl_function_context (decl))
	/* The type of a mergeable local entity (such as a function scope
	   capturing lambda's closure type fields) can depend on an
	   unmergeable local entity (such as a local variable), so type
	   equality isn't feasible in general for local entities.  */;
      else
	{
	  // FIXME:QOI Might be template specialization from a module,
	  // not necessarily global module
	  auto_diagnostic_group d;
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "conflicting global module declaration %#qD", decl);
	  inform (DECL_SOURCE_LOCATION (existing),
		  "existing declaration %#qD", existing);
	  return false;
	}
    }

  if (DECL_IS_UNDECLARED_BUILTIN (existing)
      && !DECL_IS_UNDECLARED_BUILTIN (decl))
    {
      /* We're matching a builtin that the user has yet to declare.
	 We are the one!  This is very much duplicate-decl
	 shenanigans. */
      DECL_SOURCE_LOCATION (existing) = DECL_SOURCE_LOCATION (decl);
      if (TREE_CODE (decl) != TYPE_DECL)
	{
	  /* Propagate exceptions etc.  */
	  TREE_TYPE (existing) = TREE_TYPE (decl);
	  TREE_NOTHROW (existing) = TREE_NOTHROW (decl);
	}
      /* This is actually an import! */
      DECL_MODULE_IMPORT_P (existing) = true;

      /* Yay, sliced!  */
      existing->base = decl->base;

      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  /* Ew :(  */
	  memcpy (&existing->decl_common.size,
		  &decl->decl_common.size,
		  (offsetof (tree_decl_common, pt_uid)
		   - offsetof (tree_decl_common, size)));
	  auto bltin_class = DECL_BUILT_IN_CLASS (decl);
	  existing->function_decl.built_in_class = bltin_class;
	  auto fncode = DECL_UNCHECKED_FUNCTION_CODE (decl);
	  DECL_UNCHECKED_FUNCTION_CODE (existing) = fncode;
	  if (existing->function_decl.built_in_class == BUILT_IN_NORMAL)
	    {
	      if (builtin_decl_explicit_p (built_in_function (fncode)))
		switch (fncode)
		  {
		  case BUILT_IN_STPCPY:
		    set_builtin_decl_implicit_p
		      (built_in_function (fncode), true);
		    break;
		  default:
		    set_builtin_decl_declared_p
		      (built_in_function (fncode), true);
		    break;
		  }
	      copy_attributes_to_builtin (decl);
	    }
	}
    }

  if (VAR_OR_FUNCTION_DECL_P (decl)
      && DECL_TEMPLATE_INSTANTIATED (decl))
    /* Don't instantiate again!  */
    DECL_TEMPLATE_INSTANTIATED (existing) = true;

  if (TREE_CODE (d_inner) == FUNCTION_DECL
      && DECL_DECLARED_INLINE_P (d_inner))
    DECL_DECLARED_INLINE_P (e_inner) = true;
  if (!DECL_EXTERNAL (d_inner))
    DECL_EXTERNAL (e_inner) = false;

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      /* Merge default template arguments.  */
      tree d_parms = DECL_INNERMOST_TEMPLATE_PARMS (decl);
      tree e_parms = DECL_INNERMOST_TEMPLATE_PARMS (existing);
      gcc_checking_assert (TREE_VEC_LENGTH (d_parms)
			   == TREE_VEC_LENGTH (e_parms));
      for (int i = 0; i < TREE_VEC_LENGTH (d_parms); ++i)
	{
	  tree d_default = TREE_PURPOSE (TREE_VEC_ELT (d_parms, i));
	  tree& e_default = TREE_PURPOSE (TREE_VEC_ELT (e_parms, i));
	  if (e_default == NULL_TREE)
	    e_default = d_default;
	  else if (d_default != NULL_TREE
		   && !cp_tree_equal (d_default, e_default))
	    {
	      auto_diagnostic_group d;
	      tree d_parm = TREE_VALUE (TREE_VEC_ELT (d_parms, i));
	      tree e_parm = TREE_VALUE (TREE_VEC_ELT (e_parms, i));
	      error_at (DECL_SOURCE_LOCATION (d_parm),
			"conflicting default argument for %#qD", d_parm);
	      inform (DECL_SOURCE_LOCATION (e_parm),
		      "existing default declared here");
	      return false;
	    }
	}
    }

  if (TREE_CODE (d_inner) == FUNCTION_DECL)
    {
      /* Merge default function arguments.  */
      tree d_parm = FUNCTION_FIRST_USER_PARMTYPE (d_inner);
      tree e_parm = FUNCTION_FIRST_USER_PARMTYPE (e_inner);
      int i = 0;
      for (; d_parm && d_parm != void_list_node;
	   d_parm = TREE_CHAIN (d_parm), e_parm = TREE_CHAIN (e_parm), ++i)
	{
	  tree d_default = TREE_PURPOSE (d_parm);
	  tree& e_default = TREE_PURPOSE (e_parm);
	  if (e_default == NULL_TREE)
	    e_default = d_default;
	  else if (d_default != NULL_TREE
		   && !cp_tree_equal (d_default, e_default))
	    {
	      auto_diagnostic_group d;
	      error_at (get_fndecl_argument_location (d_inner, i),
			"conflicting default argument for parameter %P of %#qD",
			i, decl);
	      inform (get_fndecl_argument_location (e_inner, i),
		      "existing default declared here");
	      return false;
	    }
	}
    }

  return true;
}

/* FN is an implicit member function that we've discovered is new to
   the class.  Add it to the TYPE_FIELDS chain and the method vector.
   Reset the appropriate classtype lazy flag.   */

bool
trees_in::install_implicit_member (tree fn)
{
  tree ctx = DECL_CONTEXT (fn);
  tree name = DECL_NAME (fn);
  /* We know these are synthesized, so the set of expected prototypes
     is quite restricted.  We're not validating correctness, just
     distinguishing beteeen the small set of possibilities.  */
  tree parm_type = TREE_VALUE (FUNCTION_FIRST_USER_PARMTYPE (fn));
  if (IDENTIFIER_CTOR_P (name))
    {
      if (CLASSTYPE_LAZY_DEFAULT_CTOR (ctx)
	  && VOID_TYPE_P (parm_type))
	CLASSTYPE_LAZY_DEFAULT_CTOR (ctx) = false;
      else if (!TYPE_REF_P (parm_type))
	return false;
      else if (CLASSTYPE_LAZY_COPY_CTOR (ctx)
	       && !TYPE_REF_IS_RVALUE (parm_type))
	CLASSTYPE_LAZY_COPY_CTOR (ctx) = false;
      else if (CLASSTYPE_LAZY_MOVE_CTOR (ctx))
	CLASSTYPE_LAZY_MOVE_CTOR (ctx) = false;
      else
	return false;
    }
  else if (IDENTIFIER_DTOR_P (name))
    {
      if (CLASSTYPE_LAZY_DESTRUCTOR (ctx))
	CLASSTYPE_LAZY_DESTRUCTOR (ctx) = false;
      else
	return false;
      if (DECL_VIRTUAL_P (fn))
	/* A virtual dtor should have been created when the class
	   became complete.  */
	return false;
    }
  else if (name == assign_op_identifier)
    {
      if (!TYPE_REF_P (parm_type))
	return false;
      else if (CLASSTYPE_LAZY_COPY_ASSIGN (ctx)
	       && !TYPE_REF_IS_RVALUE (parm_type))
	CLASSTYPE_LAZY_COPY_ASSIGN (ctx) = false;
      else if (CLASSTYPE_LAZY_MOVE_ASSIGN (ctx))
	CLASSTYPE_LAZY_MOVE_ASSIGN (ctx) = false;
      else
	return false;
    }
  else
    return false;

  dump (dumper::MERGE) && dump ("Adding implicit member %N", fn);

  DECL_CHAIN (fn) = TYPE_FIELDS (ctx);
  TYPE_FIELDS (ctx) = fn;

  add_method (ctx, fn, false);

    /* Propagate TYPE_FIELDS.  */
  fixup_type_variants (ctx);

  return true;
}

/* Return non-zero if DECL has a definition that would be interesting to
   write out.  */

static bool
has_definition (tree decl)
{
  bool is_tmpl = TREE_CODE (decl) == TEMPLATE_DECL;
  if (is_tmpl)
    decl = DECL_TEMPLATE_RESULT (decl);

  switch (TREE_CODE (decl))
    {
    default:
      break;

    case FUNCTION_DECL:
      if (!DECL_SAVED_TREE (decl))
	/* Not defined.  */
	break;

      if (DECL_DECLARED_INLINE_P (decl))
	return true;

      if (header_module_p ())
	/* We always need to write definitions in header modules,
	   since there's no TU to emit them in otherwise.  */
	return true;

      if (DECL_TEMPLATE_INFO (decl))
	{
	  int use_tpl = DECL_USE_TEMPLATE (decl);

	  // FIXME: Partial specializations have definitions too.
	  if (use_tpl < 2)
	    return true;
	}
      break;

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	if (type == TYPE_MAIN_VARIANT (type)
	    && decl == TYPE_NAME (type)
	    && (TREE_CODE (type) == ENUMERAL_TYPE
		? TYPE_VALUES (type) : TYPE_FIELDS (type)))
	  return true;
      }
      break;

    case VAR_DECL:
      /* DECL_INITIALIZED_P might not be set on a dependent VAR_DECL.  */
      if (DECL_LANG_SPECIFIC (decl)
	  && DECL_TEMPLATE_INFO (decl)
	  && DECL_INITIAL (decl))
	return true;
      else
	{
	  if (!DECL_INITIALIZED_P (decl))
	    /* Not defined.  */
	    return false;

	  if (header_module_p ())
	    /* We always need to write definitions in header modules,
	       since there's no TU to emit them in otherwise.  */
	    return true;

	  if (!decl_maybe_constant_var_p (decl)
	      && !DECL_INLINE_VAR_P (decl))
	    return false;

	  return true;
	}
      break;

    case CONCEPT_DECL:
      if (DECL_INITIAL (decl))
	return true;

      break;
    }

  return false;
}

uintptr_t *
trees_in::find_duplicate (tree existing)
{
  if (!duplicates)
    return NULL;

  return duplicates->get (existing);
}

/* We're starting to read a duplicate DECL.  EXISTING is the already
   known node.  */

void
trees_in::register_duplicate (tree decl, tree existing)
{
  if (!duplicates)
    duplicates = new duplicate_hash_map (40);

  bool existed;
  uintptr_t &slot = duplicates->get_or_insert (existing, &existed);
  gcc_checking_assert (!existed);
  slot = reinterpret_cast<uintptr_t> (decl);

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    /* Also register the DECL_TEMPLATE_RESULT as a duplicate so
       that passing decl's _RESULT to maybe_duplicate naturally
       gives us existing's _RESULT back.  */
    register_duplicate (DECL_TEMPLATE_RESULT (decl),
			DECL_TEMPLATE_RESULT (existing));
}

/* We've read a definition of MAYBE_EXISTING.  If not a duplicate,
   return MAYBE_EXISTING (into which the definition should be
   installed).  Otherwise return NULL if already known bad, or the
   duplicate we read (for ODR checking, or extracting additional merge
   information).  */

tree
trees_in::odr_duplicate (tree maybe_existing, bool has_defn)
{
  tree res = NULL_TREE;

  if (uintptr_t *dup = find_duplicate (maybe_existing))
    {
      if (!(*dup & 1))
	res = reinterpret_cast<tree> (*dup);
    }
  else
    res = maybe_existing;

  assert_definition (maybe_existing, res && !has_defn);

  // FIXME: We probably need to return the template, so that the
  // template header can be checked?
  return res ? STRIP_TEMPLATE (res) : NULL_TREE;
}

/* The following writer functions rely on the current behaviour of
   depset::hash::add_dependency making the decl and defn depset nodes
   depend on eachother.  That way we don't have to worry about seeding
   the tree map with named decls that cannot be looked up by name (I.e
   template and function parms).  We know the decl and definition will
   be in the same cluster, which is what we want.  */

void
trees_out::write_function_def (tree decl)
{
  tree_node (DECL_RESULT (decl));
  tree_node (DECL_INITIAL (decl));
  tree_node (DECL_SAVED_TREE (decl));
  tree_node (DECL_FRIEND_CONTEXT (decl));

  constexpr_fundef *cexpr = retrieve_constexpr_fundef (decl);

  if (streaming_p ())
    u (cexpr != nullptr);
  if (cexpr)
    {
      chained_decls (cexpr->parms);
      tree_node (cexpr->result);
      tree_node (cexpr->body);
    }

  function* f = DECL_STRUCT_FUNCTION (decl);

  if (streaming_p ())
    {
      unsigned flags = 0;

      if (f)
	flags |= 2;
      if (DECL_NOT_REALLY_EXTERN (decl))
	flags |= 1;

      u (flags);
    }

  if (state && f)
    {
      state->write_location (*this, f->function_start_locus);
      state->write_location (*this, f->function_end_locus);
    }
}

void
trees_out::mark_function_def (tree)
{
}

bool
trees_in::read_function_def (tree decl, tree maybe_template)
{
  dump () && dump ("Reading function definition %N", decl);
  tree result = tree_node ();
  tree initial = tree_node ();
  tree saved = tree_node ();
  tree context = tree_node ();
  constexpr_fundef cexpr;
  post_process_data pdata {};
  pdata.decl = maybe_template;

  tree maybe_dup = odr_duplicate (maybe_template, DECL_SAVED_TREE (decl));
  bool installing = maybe_dup && !DECL_SAVED_TREE (decl);

  if (u ())
    {
      cexpr.parms = chained_decls ();
      cexpr.result = tree_node ();
      cexpr.body = tree_node ();
      cexpr.decl = decl;
    }
  else
    cexpr.decl = NULL_TREE;

  unsigned flags = u ();

  if (flags & 2)
    {
      pdata.start_locus = state->read_location (*this);
      pdata.end_locus = state->read_location (*this);
    }

  if (get_overrun ())
    return NULL_TREE;

  if (installing)
    {
      DECL_NOT_REALLY_EXTERN (decl) = flags & 1;
      DECL_RESULT (decl) = result;
      DECL_INITIAL (decl) = initial;
      DECL_SAVED_TREE (decl) = saved;

      if (context)
	SET_DECL_FRIEND_CONTEXT (decl, context);
      if (cexpr.decl)
	register_constexpr_fundef (cexpr);
      post_process (pdata);
    }
  else if (maybe_dup)
    {
      // FIXME:QOI Check matching defn
    }

  return true;
}

/* Also for CONCEPT_DECLs.  */

void
trees_out::write_var_def (tree decl)
{
  tree init = DECL_INITIAL (decl);
  tree_node (init);
  if (!init)
    {
      tree dyn_init = NULL_TREE;

      /* We only need to write initializers in header modules.  */
      if (header_module_p () && DECL_NONTRIVIALLY_INITIALIZED_P (decl))
	{
	  dyn_init = value_member (decl,
				   CP_DECL_THREAD_LOCAL_P (decl)
				   ? tls_aggregates : static_aggregates);
	  gcc_checking_assert (dyn_init);
	  /* Mark it so write_inits knows this is needed.  */
	  TREE_LANG_FLAG_0 (dyn_init) = true;
	  dyn_init = TREE_PURPOSE (dyn_init);
	}
      tree_node (dyn_init);
    }
}

void
trees_out::mark_var_def (tree)
{
}

bool
trees_in::read_var_def (tree decl, tree maybe_template)
{
  /* Do not mark the virtual table entries as used.  */
  bool vtable = VAR_P (decl) && DECL_VTABLE_OR_VTT_P (decl);
  unused += vtable;
  tree init = tree_node ();
  tree dyn_init = init ? NULL_TREE : tree_node ();
  unused -= vtable;

  if (get_overrun ())
    return false;

  bool initialized = (VAR_P (decl) ? bool (DECL_INITIALIZED_P (decl))
		      : bool (DECL_INITIAL (decl)));
  tree maybe_dup = odr_duplicate (maybe_template, initialized);
  bool installing = maybe_dup && !initialized;
  if (installing)
    {
      if (DECL_EXTERNAL (decl))
	DECL_NOT_REALLY_EXTERN (decl) = true;
      if (VAR_P (decl))
	{
	  DECL_INITIALIZED_P (decl) = true;
	  if (maybe_dup && DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (maybe_dup))
	    DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl) = true;
	  if (DECL_IMPLICIT_INSTANTIATION (decl)
	      || (DECL_CLASS_SCOPE_P (decl)
		  && !DECL_VTABLE_OR_VTT_P (decl)
		  && !DECL_TEMPLATE_INFO (decl)))
	    note_vague_linkage_variable (decl);
	}
      DECL_INITIAL (decl) = init;
      if (!dyn_init)
	;
      else if (CP_DECL_THREAD_LOCAL_P (decl))
	tls_aggregates = tree_cons (dyn_init, decl, tls_aggregates);
      else
	static_aggregates = tree_cons (dyn_init, decl, static_aggregates);
    }
  else if (maybe_dup)
    {
      // FIXME:QOI Check matching defn
    }

  return true;
}

/* If MEMBER doesn't have an independent life outside the class,
   return it (or its TEMPLATE_DECL).  Otherwise NULL.  */

static tree
member_owned_by_class (tree member)
{
  gcc_assert (DECL_P (member));

  /* Clones are owned by their origin.  */
  if (DECL_CLONED_FUNCTION_P (member))
    return NULL;

  if (TREE_CODE (member) == FIELD_DECL)
    /* FIELD_DECLS can have template info in some cases.  We always
       want the FIELD_DECL though, as there's never a TEMPLATE_DECL
       wrapping them.  */
    return member;

  int use_tpl = -1;
  if (tree ti = node_template_info (member, use_tpl))
    {
      // FIXME: Don't bail on things that CANNOT have their own
      // template header.  No, make sure they're in the same cluster.
      if (use_tpl > 0)
	return NULL_TREE;

      if (DECL_TEMPLATE_RESULT (TI_TEMPLATE (ti)) == member)
	member = TI_TEMPLATE (ti);
    }
  return member;
}

void
trees_out::write_class_def (tree defn)
{
  gcc_assert (DECL_P (defn));
  if (streaming_p ())
    dump () && dump ("Writing class definition %N", defn);

  tree type = TREE_TYPE (defn);
  tree_node (TYPE_SIZE (type));
  tree_node (TYPE_SIZE_UNIT (type));
  tree_node (TYPE_VFIELD (type));
  tree_node (TYPE_BINFO (type));

  vec_chained_decls (TYPE_FIELDS (type));

  /* Every class but __as_base has a type-specific.  */
  gcc_checking_assert (!TYPE_LANG_SPECIFIC (type) == IS_FAKE_BASE_TYPE (type));

  if (TYPE_LANG_SPECIFIC (type))
    {
      {
	vec<tree, va_gc> *v = CLASSTYPE_MEMBER_VEC (type);
	if (!v)
	  {
	    gcc_checking_assert (!streaming_p ());
	    /* Force a class vector.  */
	    v = set_class_bindings (type, -1);
	    gcc_checking_assert (v);
	  }

	unsigned len = v->length ();
	if (streaming_p ())
	  u (len);
	for (unsigned ix = 0; ix != len; ix++)
	  {
	    tree m = (*v)[ix];
	    if (TREE_CODE (m) == TYPE_DECL
		&& DECL_ARTIFICIAL (m)
		&& TYPE_STUB_DECL (TREE_TYPE (m)) == m)
	      /* This is a using-decl for a type, or an anonymous
		 struct (maybe with a typedef name).  Write the type.  */
	      m = TREE_TYPE (m);
	    tree_node (m);
	  }
      }
      tree_node (CLASSTYPE_LAMBDA_EXPR (type));

      /* TYPE_CONTAINS_VPTR_P looks at the vbase vector, which the
	 reader won't know at this point.  */
      int has_vptr = TYPE_CONTAINS_VPTR_P (type);

      if (streaming_p ())
	{
	  unsigned nvbases = vec_safe_length (CLASSTYPE_VBASECLASSES (type));
	  u (nvbases);
	  i (has_vptr);
	}

      if (has_vptr)
	{
	  tree_vec (CLASSTYPE_PURE_VIRTUALS (type));
	  tree_pair_vec (CLASSTYPE_VCALL_INDICES (type));
	  tree_node (CLASSTYPE_KEY_METHOD (type));
	}
    }

  if (TYPE_LANG_SPECIFIC (type))
    {
      tree_node (CLASSTYPE_PRIMARY_BINFO (type));

      tree as_base = CLASSTYPE_AS_BASE (type);
      if (as_base)
	as_base = TYPE_NAME (as_base);
      tree_node (as_base);

      /* Write the vtables.  */
      tree vtables = CLASSTYPE_VTABLES (type);
      vec_chained_decls (vtables);
      for (; vtables; vtables = TREE_CHAIN (vtables))
	write_definition (vtables);

      /* Write the friend classes.  */
      tree_list (CLASSTYPE_FRIEND_CLASSES (type), false);

      /* Write the friend functions.  */
      for (tree friends = DECL_FRIENDLIST (defn);
	   friends; friends = TREE_CHAIN (friends))
	{
	  /* Name of these friends.  */
	  tree_node (TREE_PURPOSE (friends));
	  tree_list (TREE_VALUE (friends), false);
	}
      /* End of friend fns.  */
      tree_node (NULL_TREE);

      /* Write the decl list.  */
      tree_list (CLASSTYPE_DECL_LIST (type), true);

      if (TYPE_CONTAINS_VPTR_P (type))
	{
	  /* Write the thunks.  */
	  for (tree decls = TYPE_FIELDS (type);
	       decls; decls = DECL_CHAIN (decls))
	    if (TREE_CODE (decls) == FUNCTION_DECL
		&& DECL_VIRTUAL_P (decls)
		&& DECL_THUNKS (decls))
	      {
		tree_node (decls);
		/* Thunks are always unique, so chaining is ok.  */
		chained_decls (DECL_THUNKS (decls));
	      }
	  tree_node (NULL_TREE);
	}
    }
}

void
trees_out::mark_class_member (tree member, bool do_defn)
{
  gcc_assert (DECL_P (member));

  member = member_owned_by_class (member);
  if (member)
    mark_declaration (member, do_defn && has_definition (member));
}

void
trees_out::mark_class_def (tree defn)
{
  gcc_assert (DECL_P (defn));
  tree type = TREE_TYPE (defn);
  /* Mark the class members that are not type-decls and cannot have
     independent definitions.  */
  for (tree member = TYPE_FIELDS (type); member; member = DECL_CHAIN (member))
    if (TREE_CODE (member) == FIELD_DECL
	|| TREE_CODE (member) == USING_DECL
	/* A cloned enum-decl from 'using enum unrelated;'   */
	|| (TREE_CODE (member) == CONST_DECL
	    && DECL_CONTEXT (member) == type))
      {
	mark_class_member (member);
	if (TREE_CODE (member) == FIELD_DECL)
	  if (tree repr = DECL_BIT_FIELD_REPRESENTATIVE (member))
	    /* If we're marking a class template definition, then
	       this'll contain the width (as set by grokbitfield)
	       instead of a decl.  */
	    if (DECL_P (repr))
	      mark_declaration (repr, false);
      }

  /* Mark the binfo hierarchy.  */
  for (tree child = TYPE_BINFO (type); child; child = TREE_CHAIN (child))
    mark_by_value (child);

  if (TYPE_LANG_SPECIFIC (type))
    {
      for (tree vtable = CLASSTYPE_VTABLES (type);
	   vtable; vtable = TREE_CHAIN (vtable))
	mark_declaration (vtable, true);

      if (TYPE_CONTAINS_VPTR_P (type))
	/* Mark the thunks, they belong to the class definition,
	   /not/ the thunked-to function.  */
	for (tree decls = TYPE_FIELDS (type);
	     decls; decls = DECL_CHAIN (decls))
	  if (TREE_CODE (decls) == FUNCTION_DECL)
	    for (tree thunks = DECL_THUNKS (decls);
		 thunks; thunks = DECL_CHAIN (thunks))
	      mark_declaration (thunks, false);
    }
}

/* Nop sorting, needed for resorting the member vec.  */

static void
nop (void *, void *, void *)
{
}

bool
trees_in::read_class_def (tree defn, tree maybe_template)
{
  gcc_assert (DECL_P (defn));
  dump () && dump ("Reading class definition %N", defn);
  tree type = TREE_TYPE (defn);
  tree size = tree_node ();
  tree size_unit = tree_node ();
  tree vfield = tree_node ();
  tree binfo = tree_node ();
  vec<tree, va_gc> *vbase_vec = NULL;
  vec<tree, va_gc> *member_vec = NULL;
  vec<tree, va_gc> *pure_virts = NULL;
  vec<tree_pair_s, va_gc> *vcall_indices = NULL;
  tree key_method = NULL_TREE;
  tree lambda = NULL_TREE;

  /* Read the fields.  */
  vec<tree, va_heap> *fields = vec_chained_decls ();

  if (TYPE_LANG_SPECIFIC (type))
    {
      if (unsigned len = u ())
	{
	  vec_alloc (member_vec, len);
	  for (unsigned ix = 0; ix != len; ix++)
	    {
	      tree m = tree_node ();
	      if (get_overrun ())
		break;
	      if (TYPE_P (m))
		m = TYPE_STUB_DECL (m);
	      member_vec->quick_push (m);
	    }
	}
      lambda = tree_node ();

      if (!get_overrun ())
	{
	  unsigned nvbases = u ();
	  if (nvbases)
	    {
	      vec_alloc (vbase_vec, nvbases);
	      for (tree child = binfo; child; child = TREE_CHAIN (child))
		if (BINFO_VIRTUAL_P (child))
		  vbase_vec->quick_push (child);
	    }
	}

      if (!get_overrun ())
	{
	  int has_vptr = i ();
	  if (has_vptr)
	    {
	      pure_virts = tree_vec ();
	      vcall_indices = tree_pair_vec ();
	      key_method = tree_node ();
	    }
	}
    }

  tree maybe_dup = odr_duplicate (maybe_template, TYPE_SIZE (type));
  bool installing = maybe_dup && !TYPE_SIZE (type);
  if (installing)
    {
      if (maybe_dup != defn)
	{
	  // FIXME: This is needed on other defns too, almost
	  // duplicate-decl like?  See is_matching_decl too.
	  /* Copy flags from the duplicate.  */
	  tree type_dup = TREE_TYPE (maybe_dup);

	  /* Core pieces.  */
	  TYPE_MODE_RAW (type) = TYPE_MODE_RAW (type_dup);
	  TYPE_ALIGN_RAW (type) = TYPE_ALIGN_RAW (type_dup);
	  TYPE_WARN_IF_NOT_ALIGN_RAW (type)
	    = TYPE_WARN_IF_NOT_ALIGN_RAW (type_dup);
	  TYPE_USER_ALIGN (type) = TYPE_USER_ALIGN (type_dup);

	  SET_DECL_MODE (defn, DECL_MODE (maybe_dup));
	  DECL_SIZE (defn) = DECL_SIZE (maybe_dup);
	  DECL_SIZE_UNIT (defn) = DECL_SIZE_UNIT (maybe_dup);
	  DECL_ALIGN_RAW (defn) = DECL_ALIGN_RAW (maybe_dup);
	  DECL_WARN_IF_NOT_ALIGN_RAW (defn)
	    = DECL_WARN_IF_NOT_ALIGN_RAW (maybe_dup);
	  DECL_USER_ALIGN (defn) = DECL_USER_ALIGN (maybe_dup);

	  TYPE_TYPELESS_STORAGE (type) = TYPE_TYPELESS_STORAGE (type_dup);
	  TYPE_CXX_ODR_P (type) = TYPE_CXX_ODR_P (type_dup);
	  TYPE_NO_FORCE_BLK (type) = TYPE_NO_FORCE_BLK (type_dup);
	  TYPE_TRANSPARENT_AGGR (type) = TYPE_TRANSPARENT_AGGR (type_dup);
	  TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type)
	    = TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type_dup);

	  TYPE_EMPTY_P (type) = TYPE_EMPTY_P (type_dup);
	  TREE_ADDRESSABLE (type) = TREE_ADDRESSABLE (type_dup);

	  /* C++ pieces.  */
	  TYPE_POLYMORPHIC_P (type) = TYPE_POLYMORPHIC_P (type_dup);
	  CLASSTYPE_FINAL (type) = CLASSTYPE_FINAL (type_dup);

	  TYPE_HAS_USER_CONSTRUCTOR (type)
	    = TYPE_HAS_USER_CONSTRUCTOR (type_dup);
	  TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
	    = TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type_dup);
	  TYPE_NEEDS_CONSTRUCTING (type)
	    = TYPE_NEEDS_CONSTRUCTING (type_dup);

	  if (auto ls = TYPE_LANG_SPECIFIC (type_dup))
	    {
	      if (TYPE_LANG_SPECIFIC (type))
		{
		  CLASSTYPE_BEFRIENDING_CLASSES (type_dup)
		    = CLASSTYPE_BEFRIENDING_CLASSES (type);
		  if (!ANON_AGGR_TYPE_P (type))
		    CLASSTYPE_TYPEINFO_VAR (type_dup)
		      = CLASSTYPE_TYPEINFO_VAR (type);
		}
	      for (tree v = type; v; v = TYPE_NEXT_VARIANT (v))
		TYPE_LANG_SPECIFIC (v) = ls;
	    }
	}

      TYPE_SIZE (type) = size;
      TYPE_SIZE_UNIT (type) = size_unit;

      if (fields)
	{
	  tree *chain = &TYPE_FIELDS (type);
	  unsigned len = fields->length ();
	  for (unsigned ix = 0; ix != len; ix++)
	    {
	      tree decl = (*fields)[ix];

	      if (!decl)
		{
		  /* An anonymous struct with typedef name.  */
		  tree tdef = (*fields)[ix+1];
		  decl = TYPE_STUB_DECL (TREE_TYPE (tdef));
		  gcc_checking_assert (IDENTIFIER_ANON_P (DECL_NAME (decl))
				       && decl != tdef);
		}

	      gcc_checking_assert (!*chain == !DECL_CLONED_FUNCTION_P (decl));
	      *chain = decl;
	      chain = &DECL_CHAIN (decl);

	      if (TREE_CODE (decl) == FIELD_DECL
		  && ANON_AGGR_TYPE_P (TREE_TYPE (decl)))
		{
		  tree anon_type = TYPE_MAIN_VARIANT (TREE_TYPE (decl));
		  if (DECL_NAME (defn) == as_base_identifier)
		    /* ANON_AGGR_TYPE_FIELD should already point to the
		       original FIELD_DECL; don't overwrite it to point
		       to the as-base FIELD_DECL copy.  */
		    gcc_checking_assert (ANON_AGGR_TYPE_FIELD (anon_type));
		  else
		    ANON_AGGR_TYPE_FIELD (anon_type) = decl;
		}

	      if (TREE_CODE (decl) == USING_DECL
		  && TREE_CODE (USING_DECL_SCOPE (decl)) == RECORD_TYPE)
		{
		  /* Reconstruct DECL_ACCESS.  */
		  tree decls = USING_DECL_DECLS (decl);
		  tree access = declared_access (decl);

		  for (ovl_iterator iter (decls); iter; ++iter)
		    {
		      tree d = *iter;

		      retrofit_lang_decl (d);
		      tree list = DECL_ACCESS (d);

		      if (!purpose_member (type, list))
			DECL_ACCESS (d) = tree_cons (type, access, list);
		    }
		}
	    }
	}

      TYPE_VFIELD (type) = vfield;
      TYPE_BINFO (type) = binfo;

      if (TYPE_LANG_SPECIFIC (type))
	{
	  CLASSTYPE_LAMBDA_EXPR (type) = lambda;

	  CLASSTYPE_MEMBER_VEC (type) = member_vec;
	  CLASSTYPE_PURE_VIRTUALS (type) = pure_virts;
	  CLASSTYPE_VCALL_INDICES (type) = vcall_indices;

	  CLASSTYPE_KEY_METHOD (type) = key_method;

	  CLASSTYPE_VBASECLASSES (type) = vbase_vec;

	  /* Resort the member vector.  */
	  resort_type_member_vec (member_vec, NULL, nop, NULL);
	}
    }
  else if (maybe_dup)
    {
      // FIXME:QOI Check matching defn
    }

  if (TYPE_LANG_SPECIFIC (type))
    {
      tree primary = tree_node ();
      tree as_base = tree_node ();

      if (as_base)
	as_base = TREE_TYPE (as_base);

      /* Read the vtables.  */
      vec<tree, va_heap> *vtables = vec_chained_decls ();
      if (vtables)
	{
	  unsigned len = vtables->length ();
	  for (unsigned ix = 0; ix != len; ix++)
	    {
	      tree vtable = (*vtables)[ix];
	      read_var_def (vtable, vtable);
	    }
	}

      tree friend_classes = tree_list (false);
      tree friend_functions = NULL_TREE;
      for (tree *chain = &friend_functions;
	   tree name = tree_node (); chain = &TREE_CHAIN (*chain))
	{
	  tree val = tree_list (false);
	  *chain = build_tree_list (name, val);
	}
      tree decl_list = tree_list (true);

      if (installing)
	{
	  CLASSTYPE_PRIMARY_BINFO (type) = primary;
	  CLASSTYPE_AS_BASE (type) = as_base;

	  if (vtables)
	    {
	      if ((!CLASSTYPE_KEY_METHOD (type)
		   /* Sneaky user may have defined it inline
		      out-of-class.  */
		   || DECL_DECLARED_INLINE_P (CLASSTYPE_KEY_METHOD (type)))
		  /* An imported non-template class attached to a module
		     doesn't need to have its vtables emitted here.  */
		  && (CLASSTYPE_USE_TEMPLATE (type)
		      || !DECL_MODULE_ATTACH_P (defn)))
		vec_safe_push (keyed_classes, type);
	      unsigned len = vtables->length ();
	      tree *chain = &CLASSTYPE_VTABLES (type);
	      for (unsigned ix = 0; ix != len; ix++)
		{
		  tree vtable = (*vtables)[ix];
		  gcc_checking_assert (!*chain);
		  *chain = vtable;
		  chain = &DECL_CHAIN (vtable);
		}
	    }
	  CLASSTYPE_FRIEND_CLASSES (type) = friend_classes;
	  DECL_FRIENDLIST (defn) = friend_functions;
	  CLASSTYPE_DECL_LIST (type) = decl_list;

	  for (; friend_classes; friend_classes = TREE_CHAIN (friend_classes))
	    {
	      tree f = TREE_VALUE (friend_classes);
	      if (TREE_CODE (f) == TEMPLATE_DECL)
		f = TREE_TYPE (f);

	      if (CLASS_TYPE_P (f))
		{
		  CLASSTYPE_BEFRIENDING_CLASSES (f)
		    = tree_cons (NULL_TREE, type,
				 CLASSTYPE_BEFRIENDING_CLASSES (f));
		  dump () && dump ("Class %N befriending %C:%N",
				   type, TREE_CODE (f), f);
		}
	    }

	  for (; friend_functions;
	       friend_functions = TREE_CHAIN (friend_functions))
	    for (tree friend_decls = TREE_VALUE (friend_functions);
		 friend_decls; friend_decls = TREE_CHAIN (friend_decls))
	      {
		tree f = TREE_VALUE (friend_decls);

		DECL_BEFRIENDING_CLASSES (f)
		  = tree_cons (NULL_TREE, type, DECL_BEFRIENDING_CLASSES (f));
		dump () && dump ("Class %N befriending %C:%N",
				 type, TREE_CODE (f), f);
	      }
	}

      if (TYPE_CONTAINS_VPTR_P (type))
	/* Read and install the thunks.  */
	while (tree vfunc = tree_node ())
	  {
	    tree thunks = chained_decls ();
	    if (installing)
	      SET_DECL_THUNKS (vfunc, thunks);
	  }

      vec_free (vtables);
    }

  /* Propagate to all variants.  */
  if (installing)
    fixup_type_variants (type);

  /* IS_FAKE_BASE_TYPE is inaccurate at this point, because if this is
     the fake base, we've not hooked it into the containing class's
     data structure yet.  Fortunately it has a unique name.  */
  if (installing
      && DECL_NAME (defn) != as_base_identifier
      && (!CLASSTYPE_TEMPLATE_INFO (type)
	  || !uses_template_parms (TI_ARGS (CLASSTYPE_TEMPLATE_INFO (type)))))
    /* Emit debug info.  It'd be nice to know if the interface TU
       already emitted this.  */
    rest_of_type_compilation (type, !LOCAL_CLASS_P (type));

  vec_free (fields);

  return !get_overrun ();
}

void
trees_out::write_enum_def (tree decl)
{
  tree type = TREE_TYPE (decl);

  tree_node (TYPE_VALUES (type));
  /* Note that we stream TYPE_MIN/MAX_VALUE directly as part of the
     ENUMERAL_TYPE.  */
}

void
trees_out::mark_enum_def (tree decl)
{
  tree type = TREE_TYPE (decl);

  for (tree values = TYPE_VALUES (type); values; values = TREE_CHAIN (values))
    {
      tree cst = TREE_VALUE (values);
      mark_by_value (cst);
      /* We must mark the init to avoid circularity in tt_enum_int.  */
      if (tree init = DECL_INITIAL (cst))
	if (TREE_CODE (init) == INTEGER_CST)
	  mark_by_value (init);
    }
}

bool
trees_in::read_enum_def (tree defn, tree maybe_template)
{
  tree type = TREE_TYPE (defn);
  tree values = tree_node ();

  if (get_overrun ())
    return false;

  tree maybe_dup = odr_duplicate (maybe_template, TYPE_VALUES (type));
  bool installing = maybe_dup && !TYPE_VALUES (type);

  if (installing)
    {
      TYPE_VALUES (type) = values;
      /* Note that we stream TYPE_MIN/MAX_VALUE directly as part of the
	 ENUMERAL_TYPE.  */

      rest_of_type_compilation (type, DECL_NAMESPACE_SCOPE_P (defn));
    }
  else if (maybe_dup)
    {
      tree known = TYPE_VALUES (type);
      for (; known && values;
	   known = TREE_CHAIN (known), values = TREE_CHAIN (values))
	{
	  tree known_decl = TREE_VALUE (known);
	  tree new_decl = TREE_VALUE (values);

	  if (DECL_NAME (known_decl) != DECL_NAME (new_decl))
	    break;

	  new_decl = maybe_duplicate (new_decl);

	  if (!cp_tree_equal (DECL_INITIAL (known_decl),
			      DECL_INITIAL (new_decl)))
	    break;
	}

      if (known || values)
	{
	  auto_diagnostic_group d;
	  error_at (DECL_SOURCE_LOCATION (maybe_dup),
		    "definition of %qD does not match", maybe_dup);
	  inform (DECL_SOURCE_LOCATION (defn),
		  "existing definition %qD", defn);

	  tree known_decl = NULL_TREE, new_decl = NULL_TREE;

	  if (known)
	    known_decl = TREE_VALUE (known);
	  if (values)
	    new_decl = maybe_duplicate (TREE_VALUE (values));

	  if (known_decl && new_decl)
	    {
	      inform (DECL_SOURCE_LOCATION (new_decl),
		      "enumerator %qD does not match ...", new_decl);
	      inform (DECL_SOURCE_LOCATION (known_decl),
		      "... this enumerator %qD", known_decl);
	    }
	  else if (known_decl || new_decl)
	    {
	      tree extra = known_decl ? known_decl : new_decl;
	      inform (DECL_SOURCE_LOCATION (extra),
		      "additional enumerators beginning with %qD", extra);
	    }
	  else
	    inform (DECL_SOURCE_LOCATION (maybe_dup),
		    "enumeration range differs");

	  /* Mark it bad.  */
	  unmatched_duplicate (maybe_template);
	}
    }

  return true;
}

/* Write out the body of DECL.  See above circularity note.  */

void
trees_out::write_definition (tree decl)
{
  if (streaming_p ())
    {
      assert_definition (decl);
      dump ()
	&& dump ("Writing definition %C:%N", TREE_CODE (decl), decl);
    }
  else
    dump (dumper::DEPEND)
      && dump ("Depending definition %C:%N", TREE_CODE (decl), decl);

 again:
  switch (TREE_CODE (decl))
    {
    default:
      gcc_unreachable ();

    case TEMPLATE_DECL:
      decl = DECL_TEMPLATE_RESULT (decl);
      goto again;

    case FUNCTION_DECL:
      write_function_def (decl);
      break;

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (TYPE_MAIN_VARIANT (type) == type
		    && TYPE_NAME (type) == decl);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  write_enum_def (decl);
	else
	  write_class_def (decl);
      }
      break;

    case VAR_DECL:
    case CONCEPT_DECL:
      write_var_def (decl);
      break;
    }
}

/* Mark a declaration for by-value walking.  If DO_DEFN is true, mark
   its body too.  */

void
trees_out::mark_declaration (tree decl, bool do_defn)
{
  mark_by_value (decl);

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    decl = DECL_TEMPLATE_RESULT (decl);

  if (!do_defn)
    return;

  switch (TREE_CODE (decl))
    {
    default:
      gcc_unreachable ();

    case FUNCTION_DECL:
      mark_function_def (decl);
      break;

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (TYPE_MAIN_VARIANT (type) == type
		    && TYPE_NAME (type) == decl);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  mark_enum_def (decl);
	else
	  mark_class_def (decl);
      }
      break;

    case VAR_DECL:
    case CONCEPT_DECL:
      mark_var_def (decl);
      break;
    }
}

/* Read in the body of DECL.  See above circularity note.  */

bool
trees_in::read_definition (tree decl)
{
  dump () && dump ("Reading definition %C %N", TREE_CODE (decl), decl);

  tree maybe_template = decl;

 again:
  switch (TREE_CODE (decl))
    {
    default:
      break;

    case TEMPLATE_DECL:
      decl = DECL_TEMPLATE_RESULT (decl);
      goto again;

    case FUNCTION_DECL:
      return read_function_def (decl, maybe_template);

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (TYPE_MAIN_VARIANT (type) == type
		    && TYPE_NAME (type) == decl);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  return read_enum_def (decl, maybe_template);
	else
	  return read_class_def (decl, maybe_template);
      }
      break;

    case VAR_DECL:
    case CONCEPT_DECL:
      return read_var_def (decl, maybe_template);
    }

  return false;
}

/* Lookup an maybe insert a slot for depset for KEY.  */

depset **
depset::hash::entity_slot (tree entity, bool insert)
{
  traits::compare_type key (entity, NULL);
  depset **slot = find_slot_with_hash (key, traits::hash (key),
				       insert ? INSERT : NO_INSERT);

  return slot;
}

depset **
depset::hash::binding_slot (tree ctx, tree name, bool insert)
{
  traits::compare_type key (ctx, name);
  depset **slot = find_slot_with_hash (key, traits::hash (key),
				       insert ? INSERT : NO_INSERT);

  return slot;
}

depset *
depset::hash::find_dependency (tree decl)
{
  depset **slot = entity_slot (decl, false);

  return slot ? *slot : NULL;
}

depset *
depset::hash::find_binding (tree ctx, tree name)
{
  depset **slot = binding_slot (ctx, name, false);

  return slot ? *slot : NULL;
}

/* DECL is a newly discovered dependency.  Create the depset, if it
   doesn't already exist.  Add it to the worklist if so.

   DECL will be an OVL_USING_P OVERLOAD, if it's from a binding that's
   a using decl.

   We do not have to worry about adding the same dependency more than
   once.  First it's harmless, but secondly the TREE_VISITED marking
   prevents us wanting to do it anyway.  */

depset *
depset::hash::make_dependency (tree decl, entity_kind ek)
{
  /* Make sure we're being told consistent information.  */
  gcc_checking_assert ((ek == EK_NAMESPACE)
		       == (TREE_CODE (decl) == NAMESPACE_DECL
			   && !DECL_NAMESPACE_ALIAS (decl)));
  gcc_checking_assert (ek != EK_BINDING && ek != EK_REDIRECT);
  gcc_checking_assert (TREE_CODE (decl) != FIELD_DECL
		       && (TREE_CODE (decl) != USING_DECL
			   || TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL));
  gcc_checking_assert (!is_key_order ());
  if (ek == EK_USING)
    gcc_checking_assert (TREE_CODE (decl) == OVERLOAD);

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    /* The template should have copied these from its result decl.  */
    gcc_checking_assert (DECL_MODULE_EXPORT_P (decl)
			 == DECL_MODULE_EXPORT_P (DECL_TEMPLATE_RESULT (decl)));

  depset **slot = entity_slot (decl, true);
  depset *dep = *slot;
  bool for_binding = ek == EK_FOR_BINDING;

  if (!dep)
    {
      if ((DECL_IMPLICIT_TYPEDEF_P (decl)
	   /* ... not an enum, for instance.  */
	   && RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl))
	   && TYPE_LANG_SPECIFIC (TREE_TYPE (decl))
	   && CLASSTYPE_USE_TEMPLATE (TREE_TYPE (decl)) == 2)
	  || (VAR_P (decl)
	      && DECL_LANG_SPECIFIC (decl)
	      && DECL_USE_TEMPLATE (decl) == 2))
	{
	  /* A partial or explicit specialization. Partial
	     specializations might not be in the hash table, because
	     there can be multiple differently-constrained variants.

	     template<typename T> class silly;
	     template<typename T> requires true class silly {};

	     We need to find them, insert their TEMPLATE_DECL in the
	     dep_hash, and then convert the dep we just found into a
	     redirect.  */

	  tree ti = get_template_info (decl);
	  tree tmpl = TI_TEMPLATE (ti);
	  tree partial = NULL_TREE;
	  for (tree spec = DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
	       spec; spec = TREE_CHAIN (spec))
	    if (DECL_TEMPLATE_RESULT (TREE_VALUE (spec)) == decl)
	      {
		partial = TREE_VALUE (spec);
		break;
	      }

	  if (partial)
	    {
	      /* Eagerly create an empty redirect.  The following
	         make_dependency call could cause hash reallocation,
	         and invalidate slot's value.  */
	      depset *redirect = make_entity (decl, EK_REDIRECT);

	      /* Redirects are never reached -- always snap to their target.  */
	      redirect->set_flag_bit<DB_UNREACHED_BIT> ();

	      *slot = redirect;

	      depset *tmpl_dep = make_dependency (partial, EK_PARTIAL);
	      gcc_checking_assert (tmpl_dep->get_entity_kind () == EK_PARTIAL);

	      redirect->deps.safe_push (tmpl_dep);

	      return redirect;
	    }
	}

      bool has_def = ek != EK_USING && has_definition (decl);
      if (ek > EK_BINDING)
	ek = EK_DECL;

      /* The only OVERLOADS we should see are USING decls from
	 bindings.  */
      *slot = dep = make_entity (decl, ek, has_def);

      if (CHECKING_P && TREE_CODE (decl) == TEMPLATE_DECL)
	/* The template_result should otherwise not be in the
	   table, or be an empty redirect (created above).  */
	if (auto *eslot = entity_slot (DECL_TEMPLATE_RESULT (decl), false))
	  gcc_checking_assert ((*eslot)->get_entity_kind () == EK_REDIRECT
			       && !(*eslot)->deps.length ());

      if (ek != EK_USING)
	{
	  tree not_tmpl = STRIP_TEMPLATE (decl);

	  if (DECL_LANG_SPECIFIC (not_tmpl)
	      && DECL_MODULE_IMPORT_P (not_tmpl))
	    {
	      /* Store the module number and index in cluster/section,
		 so we don't have to look them up again.  */
	      unsigned index = import_entity_index (decl);
	      module_state *from = import_entity_module (index);
	      /* Remap will be zero for imports from partitions, which
		 we want to treat as-if declared in this TU.  */
	      if (from->remap)
		{
		  dep->cluster = index - from->entity_lwm;
		  dep->section = from->remap;
		  dep->set_flag_bit<DB_IMPORTED_BIT> ();
		}
	    }

	  if (ek == EK_DECL
	      && !dep->is_import ()
	      && TREE_CODE (CP_DECL_CONTEXT (decl)) == NAMESPACE_DECL
	      && !(TREE_CODE (decl) == TEMPLATE_DECL
		   && DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (decl)))
	    {
	      tree ctx = CP_DECL_CONTEXT (decl);

	      if (!TREE_PUBLIC (ctx))
		/* Member of internal namespace.  */
		dep->set_flag_bit<DB_IS_INTERNAL_BIT> ();
	      else if (VAR_OR_FUNCTION_DECL_P (not_tmpl)
		       && DECL_THIS_STATIC (not_tmpl))
		{
		  /* An internal decl.  This is ok in a GM entity.  */
		  if (!(header_module_p ()
			|| !DECL_LANG_SPECIFIC (not_tmpl)
			|| !DECL_MODULE_PURVIEW_P (not_tmpl)))
		    dep->set_flag_bit<DB_IS_INTERNAL_BIT> ();
		}
	    }

	  /* A namespace-scope type may be declared in one module unit
	     and defined in another; make sure that we're found when
	     completing the class.  */
	  if (ek == EK_DECL
	      && !dep->is_import ()
	      && dep->has_defn ()
	      && DECL_NAMESPACE_SCOPE_P (not_tmpl)
	      && DECL_IMPLICIT_TYPEDEF_P (not_tmpl)
	      /* Anonymous types can't be forward-declared.  */
	      && !IDENTIFIER_ANON_P (DECL_NAME (not_tmpl)))
	    dep->set_flag_bit<DB_IS_PENDING_BIT> ();
	}

      if (!dep->is_import ())
	worklist.safe_push (dep);
    }

  dump (dumper::DEPEND)
    && dump ("%s on %s %C:%N found",
	     ek == EK_REDIRECT ? "Redirect"
	     : for_binding ? "Binding" : "Dependency",
	     dep->entity_kind_name (), TREE_CODE (decl), decl);

  return dep;
}

/* DEP is a newly discovered dependency.  Append it to current's
   depset.  */

void
depset::hash::add_dependency (depset *dep)
{
  gcc_checking_assert (current && !is_key_order ());
  current->deps.safe_push (dep);

  if (dep->is_internal () && !current->is_internal ())
    current->set_flag_bit<DB_REFS_INTERNAL_BIT> ();

  if (current->get_entity_kind () == EK_USING
      && DECL_IMPLICIT_TYPEDEF_P (dep->get_entity ())
      && TREE_CODE (TREE_TYPE (dep->get_entity ())) == ENUMERAL_TYPE)
    {
      /* CURRENT is an unwrapped using-decl and DECL is an enum's
	 implicit typedef.  Is CURRENT a member of the enum?  */
      tree c_decl = OVL_FUNCTION (current->get_entity ());

      if (TREE_CODE (c_decl) == CONST_DECL
	  && (current->deps[0]->get_entity ()
	      == CP_DECL_CONTEXT (dep->get_entity ())))
	/* Make DECL depend on CURRENT.  */
	dep->deps.safe_push (current);
    }

  /* If two dependencies recursively depend on each other existing within
     their own merge keys, we must ensure that the first dep we saw while
     walking is written first in this cluster.  See sort_cluster for more
     details.  */
  if (writing_merge_key)
    {
      dep->set_flag_bit<DB_MAYBE_RECURSIVE_BIT> ();
      if (!current->is_maybe_recursive ())
	current->set_flag_bit<DB_ENTRY_BIT> ();
    }

  if (dep->is_unreached ())
    {
      /* The dependency is reachable now.  */
      reached_unreached = true;
      dep->clear_flag_bit<DB_UNREACHED_BIT> ();
      dump (dumper::DEPEND)
	&& dump ("Reaching unreached %s %C:%N", dep->entity_kind_name (),
		 TREE_CODE (dep->get_entity ()), dep->get_entity ());
    }
}

depset *
depset::hash::add_dependency (tree decl, entity_kind ek)
{
  depset *dep;

  if (is_key_order ())
    {
      dep = find_dependency (decl);
      if (dep)
	{
	  current->deps.safe_push (dep);
	  dump (dumper::MERGE)
	    && dump ("Key dependency on %s %C:%N found",
		     dep->entity_kind_name (), TREE_CODE (decl), decl);
	}
      else
	{
	  /* It's not a mergeable decl, look for it in the original
	     table.  */
	  dep = chain->find_dependency (decl);
	  gcc_checking_assert (dep);
	}
    }
  else
    {
      dep = make_dependency (decl, ek);
      if (dep->get_entity_kind () != EK_REDIRECT)
	add_dependency (dep);
    }

  return dep;
}

void
depset::hash::add_namespace_context (depset *dep, tree ns)
{
  depset *ns_dep = make_dependency (ns, depset::EK_NAMESPACE);
  dep->deps.safe_push (ns_dep);

  /* Mark it as special if imported so we don't walk connect when
     SCCing.  */
  if (!dep->is_binding () && ns_dep->is_import ())
    dep->set_special ();
}

struct add_binding_data
{
  tree ns;
  bitmap partitions;
  depset *binding;
  depset::hash *hash;
  bool met_namespace;
};

/* Return true if we are, or contain something that is exported.  */

bool
depset::hash::add_binding_entity (tree decl, WMB_Flags flags, void *data_)
{
  auto data = static_cast <add_binding_data *> (data_);
  decl = strip_using_decl (decl);

  if (!(TREE_CODE (decl) == NAMESPACE_DECL && !DECL_NAMESPACE_ALIAS (decl)))
    {
      tree inner = decl;

      if (TREE_CODE (inner) == CONST_DECL
	  && TREE_CODE (DECL_CONTEXT (inner)) == ENUMERAL_TYPE
	  /* A using-decl could make a CONST_DECL purview for a non-purview
	     enumeration.  */
	  && (!DECL_LANG_SPECIFIC (inner) || !DECL_MODULE_PURVIEW_P (inner)))
	inner = TYPE_NAME (DECL_CONTEXT (inner));
      else if (TREE_CODE (inner) == TEMPLATE_DECL)
	inner = DECL_TEMPLATE_RESULT (inner);

      if ((!DECL_LANG_SPECIFIC (inner) || !DECL_MODULE_PURVIEW_P (inner))
	  && !((flags & WMB_Using) && (flags & WMB_Purview)))
	/* Ignore entities not within the module purview.  */
	return false;

      if (VAR_OR_FUNCTION_DECL_P (inner)
	  && DECL_THIS_STATIC (inner))
	{
	  if (!header_module_p ())
	    /* Ignore internal-linkage entitites.  */
	    return false;
	}

      if ((TREE_CODE (decl) == VAR_DECL
	   || TREE_CODE (decl) == TYPE_DECL)
	  && DECL_TINFO_P (decl))
	/* Ignore TINFO things.  */
	return false;

      if (TREE_CODE (decl) == VAR_DECL && DECL_NTTP_OBJECT_P (decl))
	/* Ignore NTTP objects.  */
	return false;

      if (deduction_guide_p (decl))
	{
	  /* Ignore deduction guides, bindings for them will be created within
	     find_dependencies for their class template.  But still build a dep
	     for them so that we don't discard them.  */
	  data->hash->make_dependency (decl, EK_FOR_BINDING);
	  return false;
	}

      if (!(flags & WMB_Using) && CP_DECL_CONTEXT (decl) != data->ns)
	{
	  /* An unscoped enum constant implicitly brought into the containing
	     namespace.  We treat this like a using-decl.  */
	  gcc_checking_assert (TREE_CODE (decl) == CONST_DECL);

	  flags = WMB_Flags (flags | WMB_Using);
	  if (DECL_MODULE_EXPORT_P (TYPE_NAME (TREE_TYPE (decl)))
	      /* A using-decl can make an enum constant exported for a
		 non-exported enumeration.  */
	      || (DECL_LANG_SPECIFIC (decl) && DECL_MODULE_EXPORT_P (decl)))
	    flags = WMB_Flags (flags | WMB_Export);
	}

      if (!data->binding)
	/* No binding to check.  */;
      else if (flags & WMB_Using)
	{
	  /* Look in the binding to see if we already have this
	     using.  */
	  for (unsigned ix = data->binding->deps.length (); --ix;)
	    {
	      depset *d = data->binding->deps[ix];
	      if (d->get_entity_kind () == EK_USING
		  && OVL_FUNCTION (d->get_entity ()) == decl)
		{
		  if (!(flags & WMB_Hidden))
		    d->clear_hidden_binding ();
		  OVL_PURVIEW_P (d->get_entity ()) = true;
		  if (flags & WMB_Export)
		    OVL_EXPORT_P (d->get_entity ()) = true;
		  return bool (flags & WMB_Export);
		}
	    }
	}
      else if (flags & WMB_Dups)
	{
	  /* Look in the binding to see if we already have this decl.  */
	  for (unsigned ix = data->binding->deps.length (); --ix;)
	    {
	      depset *d = data->binding->deps[ix];
	      if (d->get_entity () == decl)
		{
		  if (!(flags & WMB_Hidden))
		    d->clear_hidden_binding ();
		  return false;
		}
	    }
	}

      /* We're adding something.  */
      if (!data->binding)
	{
	  data->binding = make_binding (data->ns, DECL_NAME (decl));
	  data->hash->add_namespace_context (data->binding, data->ns);

	  depset **slot = data->hash->binding_slot (data->ns,
						    DECL_NAME (decl), true);
	  gcc_checking_assert (!*slot);
	  *slot = data->binding;
	}

      /* Make sure nobody left a tree visited lying about.  */
      gcc_checking_assert (!TREE_VISITED (decl));

      if (flags & WMB_Using)
	{
	  decl = ovl_make (decl, NULL_TREE);
	  OVL_USING_P (decl) = true;
	  OVL_PURVIEW_P (decl) = true;
	  if (flags & WMB_Export)
	    OVL_EXPORT_P (decl) = true;
	}

      depset *dep = data->hash->make_dependency
	(decl, flags & WMB_Using ? EK_USING : EK_FOR_BINDING);
      if (flags & WMB_Hidden)
	dep->set_hidden_binding ();
      data->binding->deps.safe_push (dep);
      /* Binding and contents are mutually dependent.  */
      dep->deps.safe_push (data->binding);

      return (flags & WMB_Using
	      ? flags & WMB_Export : DECL_MODULE_EXPORT_P (decl));
    }
  else if (DECL_NAME (decl) && !data->met_namespace)
    {
      /* Namespace, walk exactly once.  */
      gcc_checking_assert (TREE_PUBLIC (decl));
      data->met_namespace = true;
      if (data->hash->add_namespace_entities (decl, data->partitions))
	{
	  /* It contains an exported thing, so it is exported.  */
	  gcc_checking_assert (DECL_MODULE_PURVIEW_P (decl));
	  DECL_MODULE_EXPORT_P (decl) = true;
	}

      if (DECL_MODULE_PURVIEW_P (decl))
	{
	  data->hash->make_dependency (decl, depset::EK_NAMESPACE);

	  return DECL_MODULE_EXPORT_P (decl);
	}
    }

  return false;
}

/* Recursively find all the namespace bindings of NS.  Add a depset
   for every binding that contains an export or module-linkage entity.
   Add a defining depset for every such decl that we need to write a
   definition.  Such defining depsets depend on the binding depset.
   Returns true if we contain something exported.  */

bool
depset::hash::add_namespace_entities (tree ns, bitmap partitions)
{
  dump () && dump ("Looking for writables in %N", ns);
  dump.indent ();

  unsigned count = 0;
  add_binding_data data;
  data.ns = ns;
  data.partitions = partitions;
  data.hash = this;

  hash_table<named_decl_hash>::iterator end
    (DECL_NAMESPACE_BINDINGS (ns)->end ());
  for (hash_table<named_decl_hash>::iterator iter
	 (DECL_NAMESPACE_BINDINGS (ns)->begin ()); iter != end; ++iter)
    {
      data.binding = nullptr;
      data.met_namespace = false;
      if (walk_module_binding (*iter, partitions, add_binding_entity, &data))
	count++;
    }

  if (count)
    dump () && dump ("Found %u entries", count);
  dump.outdent ();

  return count != 0;
}

void
depset::hash::add_partial_entities (vec<tree, va_gc> *partial_classes)
{
  for (unsigned ix = 0; ix != partial_classes->length (); ix++)
    {
      tree inner = (*partial_classes)[ix];

      depset *dep = make_dependency (inner, depset::EK_DECL);

      if (dep->get_entity_kind () == depset::EK_REDIRECT)
	{
	  dep = dep->deps[0];
	  /* We should have recorded the template as a partial
	     specialization.  */
	  gcc_checking_assert (dep->get_entity_kind ()
			       == depset::EK_PARTIAL);

	  /* Only emit GM entities if reached.  */
	  if (!DECL_LANG_SPECIFIC (inner)
	      || !DECL_MODULE_PURVIEW_P (inner))
	    dep->set_flag_bit<DB_UNREACHED_BIT> ();
	}
      else
	{
	  /* It was an explicit specialization, not a partial one.
	     We should have already added this.  */
	  gcc_checking_assert (dep->get_entity_kind ()
			       == depset::EK_SPECIALIZATION);
	  gcc_checking_assert (dep->is_special ());
	}
    }
}

/* Add the members of imported classes that we defined in this TU.
   This will also include lazily created implicit member function
   declarations.  (All others will be definitions.)  */

void
depset::hash::add_class_entities (vec<tree, va_gc> *class_members)
{
  for (unsigned ix = 0; ix != class_members->length (); ix++)
    {
      tree defn = (*class_members)[ix];
      depset *dep = make_dependency (defn, EK_INNER_DECL);

      if (dep->get_entity_kind () == EK_REDIRECT)
	dep = dep->deps[0];

      /* Only non-instantiations need marking as pendings.  */
      if (dep->get_entity_kind () == EK_DECL)
	dep->set_flag_bit <DB_IS_PENDING_BIT> ();
    }
}

/* We add the partial & explicit specializations, and the explicit
   instantiations.  */

static void
specialization_add (bool decl_p, spec_entry *entry, void *data_)
{
  vec<spec_entry *> *data = reinterpret_cast <vec<spec_entry *> *> (data_);

  if (!decl_p)
    {
      /* We exclusively use decls to locate things.  Make sure there's
	 no mismatch between the two specialization tables we keep.
	 pt.cc optimizes instantiation lookup using a complicated
	 heuristic.  We don't attempt to replicate that algorithm, but
	 observe its behaviour and reproduce it upon read back.  */

       gcc_checking_assert (TREE_CODE (entry->spec) == ENUMERAL_TYPE
			   || DECL_CLASS_TEMPLATE_P (entry->tmpl));

       gcc_checking_assert (!match_mergeable_specialization (true, entry));
    }
  else if (VAR_OR_FUNCTION_DECL_P (entry->spec))
    gcc_checking_assert (!DECL_LOCAL_DECL_P (entry->spec));

  data->safe_push (entry);
}

/* Arbitrary stable comparison.  */

static int
specialization_cmp (const void *a_, const void *b_)
{
  const spec_entry *ea = *reinterpret_cast<const spec_entry *const *> (a_);
  const spec_entry *eb = *reinterpret_cast<const spec_entry *const *> (b_);

  if (ea == eb)
    return 0;

  tree a = ea->spec;
  tree b = eb->spec;
  if (TYPE_P (a))
    {
      a = TYPE_NAME (a);
      b = TYPE_NAME (b);
    }

  if (a == b)
    /* This can happen with friend specializations.  Just order by
       entry address.  See note in depset_cmp.  */
    return ea < eb ? -1 : +1;

  return DECL_UID (a) < DECL_UID (b) ? -1 : +1;
}

/* We add all kinds of specialializations.  Implicit specializations
   should only streamed and walked if they are reachable from
   elsewhere.  Hence the UNREACHED flag.  This is making the
   assumption that it is cheaper to reinstantiate them on demand
   elsewhere, rather than stream them in when we instantiate their
   general template.  Also, if we do stream them, we can only do that
   if they are not internal (which they can become if they themselves
   touch an internal entity?).  */

void
depset::hash::add_specializations (bool decl_p)
{
  vec<spec_entry *> data;
  data.create (100);
  walk_specializations (decl_p, specialization_add, &data);
  data.qsort (specialization_cmp);
  while (data.length ())
    {
      spec_entry *entry = data.pop ();
      tree spec = entry->spec;
      int use_tpl = 0;
      bool is_friend = false;

      if (decl_p && DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (entry->tmpl))
	/* A friend of a template.  This is keyed to the
	   instantiation.  */
	is_friend = true;

      if (decl_p)
	{
	  if (tree ti = DECL_TEMPLATE_INFO (spec))
	    {
	      tree tmpl = TI_TEMPLATE (ti);

	      use_tpl = DECL_USE_TEMPLATE (spec);
	      if (spec == DECL_TEMPLATE_RESULT (tmpl))
		{
		  spec = tmpl;
		  gcc_checking_assert (DECL_USE_TEMPLATE (spec) == use_tpl);
		}
	      else if (is_friend)
		{
		  if (TI_TEMPLATE (ti) != entry->tmpl
		      || !template_args_equal (TI_ARGS (ti), entry->tmpl))
		    goto template_friend;
		}
	    }
	  else
	    {
	    template_friend:;
	      gcc_checking_assert (is_friend);
	      /* This is a friend of a template class, but not the one
		 that generated entry->spec itself (i.e. it's an
		 equivalent clone).  We do not need to record
		 this.  */
	      continue;
	    }
	}
      else
	{
	  if (TREE_CODE (spec) == ENUMERAL_TYPE)
	    {
	      tree ctx = DECL_CONTEXT (TYPE_NAME (spec));

	      if (TYPE_P (ctx))
		use_tpl = CLASSTYPE_USE_TEMPLATE (ctx);
	      else
		use_tpl = DECL_USE_TEMPLATE (ctx);
	    }
	  else
	    use_tpl = CLASSTYPE_USE_TEMPLATE (spec);

	  tree ti = TYPE_TEMPLATE_INFO (spec);
	  tree tmpl = TI_TEMPLATE (ti);

	  spec = TYPE_NAME (spec);
	  if (spec == DECL_TEMPLATE_RESULT (tmpl))
	    {
	      spec = tmpl;
	      use_tpl = DECL_USE_TEMPLATE (spec);
	    }
	}

      bool needs_reaching = false;
      if (use_tpl == 1)
	/* Implicit instantiations only walked if we reach them.  */
	needs_reaching = true;
      else if (!DECL_LANG_SPECIFIC (STRIP_TEMPLATE (spec))
	       || !DECL_MODULE_PURVIEW_P (STRIP_TEMPLATE (spec)))
	/* Likewise, GMF explicit or partial specializations.  */
	needs_reaching = true;

#if false && CHECKING_P
      /* The instantiation isn't always on
	 DECL_TEMPLATE_INSTANTIATIONS, */
      // FIXME: we probably need to remember this information?
      /* Verify the specialization is on the
	 DECL_TEMPLATE_INSTANTIATIONS of the template.  */
      for (tree cons = DECL_TEMPLATE_INSTANTIATIONS (entry->tmpl);
	   cons; cons = TREE_CHAIN (cons))
	if (TREE_VALUE (cons) == entry->spec)
	  {
	    gcc_assert (entry->args == TREE_PURPOSE (cons));
	    goto have_spec;
	  }
      gcc_unreachable ();
    have_spec:;
#endif

      /* Make sure nobody left a tree visited lying about.  */
      gcc_checking_assert (!TREE_VISITED (spec));
      depset *dep = make_dependency (spec, depset::EK_SPECIALIZATION);
      if (dep->is_special ())
	gcc_unreachable ();
      else
	{
	  if (dep->get_entity_kind () == depset::EK_REDIRECT)
	    dep = dep->deps[0];
	  else if (dep->get_entity_kind () == depset::EK_SPECIALIZATION)
	    {
	      dep->set_special ();
	      dep->deps.safe_push (reinterpret_cast<depset *> (entry));
	      if (!decl_p)
		dep->set_flag_bit<DB_TYPE_SPEC_BIT> ();
	    }

	  if (needs_reaching)
	    dep->set_flag_bit<DB_UNREACHED_BIT> ();
	  if (is_friend)
	    dep->set_flag_bit<DB_FRIEND_SPEC_BIT> ();
	}
    }
  data.release ();
}

/* Add a depset into the mergeable hash.  */

void
depset::hash::add_mergeable (depset *mergeable)
{
  gcc_checking_assert (is_key_order ());
  entity_kind ek = mergeable->get_entity_kind ();
  tree decl = mergeable->get_entity ();
  gcc_checking_assert (ek < EK_DIRECT_HWM);

  depset **slot = entity_slot (decl, true);
  gcc_checking_assert (!*slot);
  depset *dep = make_entity (decl, ek);
  *slot = dep;

  worklist.safe_push (dep);

  /* So we can locate the mergeable depset this depset refers to,
     mark the first dep.  */
  dep->set_special ();
  dep->deps.safe_push (mergeable);
}

/* Find the innermost-namespace scope of DECL, and that
   namespace-scope decl.  */

tree
find_pending_key (tree decl, tree *decl_p = nullptr)
{
  tree ns = decl;
  do
    {
      decl = ns;
      ns = CP_DECL_CONTEXT (ns);
      if (TYPE_P (ns))
	ns = TYPE_NAME (ns);
    }
  while (TREE_CODE (ns) != NAMESPACE_DECL);

  if (decl_p)
    *decl_p = decl;

  return ns;
}

/* Creates bindings and dependencies for all deduction guides of
   the given class template DECL as needed.  */

void
depset::hash::add_deduction_guides (tree decl)
{
  /* Alias templates never have deduction guides.  */
  if (DECL_ALIAS_TEMPLATE_P (decl))
    return;

  /* We don't need to do anything for class-scope deduction guides,
     as they will be added as members anyway.  */
  if (!DECL_NAMESPACE_SCOPE_P (decl))
    return;

  tree ns = CP_DECL_CONTEXT (decl);
  tree name = dguide_name (decl);

  /* We always add all deduction guides with a given name at once,
     so if there's already a binding there's nothing to do.  */
  if (find_binding (ns, name))
    return;

  tree guides = lookup_qualified_name (ns, name, LOOK_want::ANY_REACHABLE,
				       /*complain=*/false);
  if (guides == error_mark_node)
    return;

  /* We have bindings to add.  */
  depset *binding = make_binding (ns, name);
  add_namespace_context (binding, ns);

  depset **slot = binding_slot (ns, name, /*insert=*/true);
  *slot = binding;

  for (lkp_iterator it (guides); it; ++it)
    {
      gcc_checking_assert (!TREE_VISITED (*it));
      depset *dep = make_dependency (*it, EK_FOR_BINDING);
      binding->deps.safe_push (dep);
      dep->deps.safe_push (binding);
    }
}

/* Iteratively find dependencies.  During the walk we may find more
   entries on the same binding that need walking.  */

void
depset::hash::find_dependencies (module_state *module)
{
  trees_out walker (NULL, module, *this);
  vec<depset *> unreached;
  unreached.create (worklist.length ());

  for (;;)
    {
      reached_unreached = false;
      while (worklist.length ())
	{
	  depset *item = worklist.pop ();

	  gcc_checking_assert (!item->is_binding ());
	  if (item->is_unreached ())
	    unreached.quick_push (item);
	  else
	    {
	      current = item;
	      tree decl = current->get_entity ();
	      dump (is_key_order () ? dumper::MERGE : dumper::DEPEND)
		&& dump ("Dependencies of %s %C:%N",
			 is_key_order () ? "key-order"
			 : current->entity_kind_name (), TREE_CODE (decl), decl);
	      dump.indent ();
	      walker.begin ();
	      if (current->get_entity_kind () == EK_USING)
		walker.tree_node (OVL_FUNCTION (decl));
	      else if (TREE_VISITED (decl))
		/* A global tree.  */;
	      else if (item->get_entity_kind () == EK_NAMESPACE)
		{
		  module->note_location (DECL_SOURCE_LOCATION (decl));
		  add_namespace_context (current, CP_DECL_CONTEXT (decl));
		}
	      else
		{
		  walker.mark_declaration (decl, current->has_defn ());

		  if (!walker.is_key_order ()
		      && item->is_pending_entity ())
		    {
		      tree ns = find_pending_key (decl, nullptr);
		      add_namespace_context (item, ns);
		    }

		  walker.decl_value (decl, current);
		  if (current->has_defn ())
		    walker.write_definition (decl);
		}
	      walker.end ();

	      if (!walker.is_key_order ()
		  && DECL_CLASS_TEMPLATE_P (decl))
		add_deduction_guides (decl);

	      if (!walker.is_key_order ()
		  && TREE_CODE (decl) == TEMPLATE_DECL
		  && !DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (decl))
		{
		  /* Mark all the explicit & partial specializations as
		     reachable.  We search both specialization lists as some
		     constrained partial specializations for class types are
		     only found in DECL_TEMPLATE_SPECIALIZATIONS.  */
		  auto mark_reached = [this](tree spec)
		    {
		      if (TYPE_P (spec))
			spec = TYPE_NAME (spec);
		      int use_tpl;
		      node_template_info (spec, use_tpl);
		      if (use_tpl & 2)
			{
			  depset *spec_dep = find_dependency (spec);
			  if (spec_dep->get_entity_kind () == EK_REDIRECT)
			    spec_dep = spec_dep->deps[0];
			  if (spec_dep->is_unreached ())
			    {
			      reached_unreached = true;
			      spec_dep->clear_flag_bit<DB_UNREACHED_BIT> ();
			      dump (dumper::DEPEND)
				&& dump ("Reaching unreached specialization"
					 " %C:%N", TREE_CODE (spec), spec);
			    }
			}
		    };

		  for (tree cons = DECL_TEMPLATE_INSTANTIATIONS (decl);
		       cons; cons = TREE_CHAIN (cons))
		    mark_reached (TREE_VALUE (cons));
		  for (tree cons = DECL_TEMPLATE_SPECIALIZATIONS (decl);
		       cons; cons = TREE_CHAIN (cons))
		    mark_reached (TREE_VALUE (cons));
		}

	      dump.outdent ();
	      current = NULL;
	    }
	}

      if (!reached_unreached)
	break;

      /* It's possible the we reached the unreached before we
	 processed it in the above loop, so we'll be doing this an
	 extra time.  However, to avoid that we have to do some
	 bit shuffling that also involves a scan of the list.
	 Swings & roundabouts I guess.  */
      std::swap (worklist, unreached);
    }

  unreached.release ();
}

/* Compare two entries of a single binding.  TYPE_DECL before
   non-exported before exported.  */

static int
binding_cmp (const void *a_, const void *b_)
{
  depset *a = *(depset *const *)a_;
  depset *b = *(depset *const *)b_;

  tree a_ent = a->get_entity ();
  tree b_ent = b->get_entity ();
  gcc_checking_assert (a_ent != b_ent
		       && !a->is_binding ()
		       && !b->is_binding ());

  /* Implicit typedefs come first.  */
  bool a_implicit = DECL_IMPLICIT_TYPEDEF_P (a_ent);
  bool b_implicit = DECL_IMPLICIT_TYPEDEF_P (b_ent);
  if (a_implicit || b_implicit)
    {
      /* A binding with two implicit type decls?  That's unpossible!  */
      gcc_checking_assert (!(a_implicit && b_implicit));
      return a_implicit ? -1 : +1;  /* Implicit first.  */
    }

  /* Hidden before non-hidden.  */
  bool a_hidden = a->is_hidden ();
  bool b_hidden = b->is_hidden ();
  if (a_hidden != b_hidden)
    return a_hidden ? -1 : +1;

  bool a_using = a->get_entity_kind () == depset::EK_USING;
  bool a_export;
  if (a_using)
    {
      a_export = OVL_EXPORT_P (a_ent);
      a_ent = OVL_FUNCTION (a_ent);
    }
  else if (TREE_CODE (a_ent) == CONST_DECL
	   && DECL_LANG_SPECIFIC (a_ent)
	   && DECL_MODULE_EXPORT_P (a_ent))
    a_export = true;
  else
    a_export = DECL_MODULE_EXPORT_P (TREE_CODE (a_ent) == CONST_DECL
				     ? TYPE_NAME (TREE_TYPE (a_ent))
				     : STRIP_TEMPLATE (a_ent));

  bool b_using = b->get_entity_kind () == depset::EK_USING;
  bool b_export;
  if (b_using)
    {
      b_export = OVL_EXPORT_P (b_ent);
      b_ent = OVL_FUNCTION (b_ent);
    }
  else if (TREE_CODE (b_ent) == CONST_DECL
	   && DECL_LANG_SPECIFIC (b_ent)
	   && DECL_MODULE_EXPORT_P (b_ent))
    b_export = true;
  else
    b_export = DECL_MODULE_EXPORT_P (TREE_CODE (b_ent) == CONST_DECL
				     ? TYPE_NAME (TREE_TYPE (b_ent))
				     : STRIP_TEMPLATE (b_ent));

  /* Non-exports before exports.  */
  if (a_export != b_export)
    return a_export ? +1 : -1;

  /* At this point we don't care, but want a stable sort.  */

  if (a_using != b_using)
    /* using first.  */
    return a_using? -1 : +1;

  return DECL_UID (a_ent) < DECL_UID (b_ent) ? -1 : +1;
}

/* Sort the bindings, issue errors about bad internal refs.  */

bool
depset::hash::finalize_dependencies ()
{
  bool ok = true;
  depset::hash::iterator end (this->end ());
  for (depset::hash::iterator iter (begin ()); iter != end; ++iter)
    {
      depset *dep = *iter;
      if (dep->is_binding ())
	{
	  /* Keep the containing namespace dep first.  */
	  gcc_checking_assert (dep->deps.length () > 1
			       && (dep->deps[0]->get_entity_kind ()
				   == EK_NAMESPACE)
			       && (dep->deps[0]->get_entity ()
				   == dep->get_entity ()));
	  if (dep->deps.length () > 2)
	    gcc_qsort (&dep->deps[1], dep->deps.length () - 1,
		       sizeof (dep->deps[1]), binding_cmp);
	}
      else if (dep->refs_internal ())
	{
	  for (unsigned ix = dep->deps.length (); ix--;)
	    {
	      depset *rdep = dep->deps[ix];
	      if (rdep->is_internal ())
		{
		  // FIXME:QOI Better location information?  We're
		  // losing, so it doesn't matter about efficiency
		  tree decl = dep->get_entity ();
		  error_at (DECL_SOURCE_LOCATION (decl),
			    "%q#D references internal linkage entity %q#D",
			    decl, rdep->get_entity ());
		  break;
		}
	    }
	  ok = false;
	}
    }

  return ok;
}

/* Core of TARJAN's algorithm to find Strongly Connected Components
   within a graph.  See https://en.wikipedia.org/wiki/
   Tarjan%27s_strongly_connected_components_algorithm for details.

   We use depset::section as lowlink.  Completed nodes have
   depset::cluster containing the cluster number, with the top
   bit set.

   A useful property is that the output vector is a reverse
   topological sort of the resulting DAG.  In our case that means
   dependent SCCs are found before their dependers.  We make use of
   that property.  */

void
depset::tarjan::connect (depset *v)
{
  gcc_checking_assert (v->is_binding ()
		       || !(v->is_unreached () || v->is_import ()));

  v->cluster = v->section = ++index;
  stack.safe_push (v);

  /* Walk all our dependencies, ignore a first marked slot  */
  for (unsigned ix = v->is_special (); ix != v->deps.length (); ix++)
    {
      depset *dep = v->deps[ix];

      if (dep->is_binding () || !dep->is_import ())
	{
	  unsigned lwm = dep->cluster;

	  if (!dep->cluster)
	    {
	      /* A new node.  Connect it.  */
	      connect (dep);
	      lwm = dep->section;
	    }

	  if (dep->section && v->section > lwm)
	    v->section = lwm;
	}
    }

  if (v->section == v->cluster)
    {
      /* Root of a new SCC.  Push all the members onto the result list. */
      unsigned num = v->cluster;
      depset *p;
      do
	{
	  p = stack.pop ();
	  p->cluster = num;
	  p->section = 0;
	  result.quick_push (p);
	}
      while (p != v);
    }
}

/* Compare two depsets.  The specific ordering is unimportant, we're
   just trying to get consistency.  */

static int
depset_cmp (const void *a_, const void *b_)
{
  depset *a = *(depset *const *)a_;
  depset *b = *(depset *const *)b_;

  depset::entity_kind a_kind = a->get_entity_kind ();
  depset::entity_kind b_kind = b->get_entity_kind ();

  if  (a_kind != b_kind)
    /* Different entity kinds, order by that.  */
    return a_kind < b_kind ? -1 : +1;

  tree a_decl = a->get_entity ();
  tree b_decl = b->get_entity ();
  if (a_kind == depset::EK_USING)
    {
      /* If one is a using, the other must be too.  */
      a_decl = OVL_FUNCTION (a_decl);
      b_decl = OVL_FUNCTION (b_decl);
    }

  if (a_decl != b_decl)
    /* Different entities, order by their UID.  */
    return DECL_UID (a_decl) < DECL_UID (b_decl) ? -1 : +1;

  if (a_kind == depset::EK_BINDING)
    {
      /* Both are bindings.  Order by identifier hash.  */
      gcc_checking_assert (a->get_name () != b->get_name ());
      hashval_t ah = IDENTIFIER_HASH_VALUE (a->get_name ());
      hashval_t bh = IDENTIFIER_HASH_VALUE (b->get_name ());
      return (ah == bh ? 0 : ah < bh ? -1 : +1);
    }

  /* They are the same decl.  This can happen with two using decls
     pointing to the same target.  The best we can aim for is
     consistently telling qsort how to order them.  Hopefully we'll
     never have to debug a case that depends on this.  Oh, who am I
     kidding?  Good luck.  */
  gcc_checking_assert (a_kind == depset::EK_USING);

  /* Order by depset address.  Not the best, but it is something.  */
  return a < b ? -1 : +1;
}

/* Sort the clusters in SCC such that those that depend on one another
   are placed later.   */

// FIXME: I am not convinced this is needed and, if needed,
// sufficient.  We emit the decls in this order but that emission
// could walk into later decls (from the body of the decl, or default
// arg-like things).  Why doesn't that walk do the right thing?  And
// if it DTRT why do we need to sort here -- won't things naturally
// work?  I think part of the issue is that when we're going to refer
// to an entity by name, and that entity is in the same cluster as us,
// we need to actually walk that entity, if we've not already walked
// it.
static void
sort_cluster (depset::hash *original, depset *scc[], unsigned size)
{
  depset::hash table (size, original);

  dump.indent ();

  /* Place bindings last, usings before that.  It's not strictly
     necessary, but it does make things neater.  Says Mr OCD.  */
  unsigned bind_lwm = size;
  unsigned use_lwm = size;
  for (unsigned ix = 0; ix != use_lwm;)
    {
      depset *dep = scc[ix];
      switch (dep->get_entity_kind ())
	{
	case depset::EK_BINDING:
	  /* Move to end.  No increment.  Notice this could be moving
	     a using decl, which we'll then move again.  */
	  if (--bind_lwm != ix)
	    {
	      scc[ix] = scc[bind_lwm];
	      scc[bind_lwm] = dep;
	    }
	  if (use_lwm > bind_lwm)
	    {
	      use_lwm--;
	      break;
	    }
	  /* We must have copied a using, so move it too.  */
	  dep = scc[ix];
	  gcc_checking_assert (dep->get_entity_kind () == depset::EK_USING);
	  /* FALLTHROUGH  */

	case depset::EK_USING:
	  if (--use_lwm != ix)
	    {
	      scc[ix] = scc[use_lwm];
	      scc[use_lwm] = dep;
	    }
	  break;

	case depset::EK_DECL:
	case depset::EK_SPECIALIZATION:
	case depset::EK_PARTIAL:
	  table.add_mergeable (dep);
	  ix++;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  gcc_checking_assert (use_lwm <= bind_lwm);
  dump (dumper::MERGE) && dump ("Ordering %u/%u depsets", use_lwm, size);

  table.find_dependencies (nullptr);

  auto_vec<depset *> order = table.connect ();
  gcc_checking_assert (order.length () == use_lwm);

  /* Now rewrite entries [0,lwm), in the dependency order we
     discovered.  Usually each entity is in its own cluster.  Rarely,
     we can get multi-entity clusters, in which case all but one must
     only be reached from within the cluster.  This happens for
     something like:

     template<typename T>
     auto Foo (const T &arg) -> TPL<decltype (arg)>;

     The instantiation of TPL will be in the specialization table, and
     refer to Foo via arg.  But we can only get to that specialization
     from Foo's declaration, so we only need to treat Foo as mergable
     (We'll do structural comparison of TPL<decltype (arg)>).

     We approximate finding the single cluster entry dep by checking for
     entities recursively depending on a dep first seen when streaming
     its own merge key; the first dep we see in such a cluster should be
     the first one streamed.  */
  unsigned entry_pos = ~0u;
  unsigned cluster = ~0u;
  for (unsigned ix = 0; ix != order.length (); ix++)
    {
      gcc_checking_assert (order[ix]->is_special ());
      bool tight = order[ix]->cluster == cluster;
      depset *dep = order[ix]->deps[0];
      dump (dumper::MERGE)
	&& dump ("Mergeable %u is %N%s%s", ix, dep->get_entity (),
		 tight ? " (tight)" : "", dep->is_entry () ? " (entry)" : "");
      scc[ix] = dep;
      if (tight)
	{
	  gcc_checking_assert (dep->is_maybe_recursive ());
	  if (dep->is_entry ())
	    {
	      /* There should only be one entry dep in a cluster.  */
	      gcc_checking_assert (!scc[entry_pos]->is_entry ());
	      gcc_checking_assert (scc[entry_pos]->is_maybe_recursive ());
	      scc[ix] = scc[entry_pos];
	      scc[entry_pos] = dep;
	    }
	}
      else
	entry_pos = ix;
      cluster = order[ix]->cluster;
    }

  dump (dumper::MERGE) && dump ("Ordered %u keys", order.length ());
  dump.outdent ();
}

/* Reduce graph to SCCS clusters.  SCCS will be populated with the
   depsets in dependency order.  Each depset's CLUSTER field contains
   its cluster number.  Each SCC has a unique cluster number, and are
   contiguous in SCCS. Cluster numbers are otherwise arbitrary.  */

vec<depset *>
depset::hash::connect ()
{
  tarjan connector (size ());
  vec<depset *> deps;
  deps.create (size ());
  iterator end (this->end ());
  for (iterator iter (begin ()); iter != end; ++iter)
    {
      depset *item = *iter;

      entity_kind kind = item->get_entity_kind ();
      if (kind == EK_BINDING
	  || !(kind == EK_REDIRECT
	       || item->is_unreached ()
	       || item->is_import ()))
	deps.quick_push (item);
    }

  /* Iteration over the hash table is an unspecified ordering.  While
     that has advantages, it causes 2 problems.  Firstly repeatable
     builds are tricky.  Secondly creating testcases that check
     dependencies are correct by making sure a bad ordering would
     happen if that was wrong.  */
  deps.qsort (depset_cmp);

  while (deps.length ())
    {
      depset *v = deps.pop ();
      dump (dumper::CLUSTER) &&
	(v->is_binding ()
	 ? dump ("Connecting binding %P", v->get_entity (), v->get_name ())
	 : dump ("Connecting %s %s %C:%N",
		 is_key_order () ? "key-order"
		 : !v->has_defn () ? "declaration" : "definition",
		 v->entity_kind_name (), TREE_CODE (v->get_entity ()),
		 v->get_entity ()));
      if (!v->cluster)
	connector.connect (v);
    }

  deps.release ();
  return connector.result;
}

/* Initialize location spans.  */

void
loc_spans::init (const line_maps *lmaps, const line_map_ordinary *map)
{
  gcc_checking_assert (!init_p ());
  spans = new vec<span> ();
  spans->reserve (20);

  span interval;
  interval.ordinary.first = 0;
  interval.macro.second = MAX_LOCATION_T + 1;
  interval.ordinary_delta = interval.macro_delta = 0;

  /* A span for reserved fixed locs.  */
  interval.ordinary.second
    = MAP_START_LOCATION (LINEMAPS_ORDINARY_MAP_AT (line_table, 0));
  interval.macro.first = interval.macro.second;
  dump (dumper::LOCATION)
    && dump ("Fixed span %u ordinary:[%K,%K) macro:[%K,%K)", spans->length (),
	     interval.ordinary.first, interval.ordinary.second,
	     interval.macro.first, interval.macro.second);
  spans->quick_push (interval);

  /* A span for command line & forced headers.  */
  interval.ordinary.first = interval.ordinary.second;
  interval.macro.second = interval.macro.first;
  if (map)
    {
      interval.ordinary.second = map->start_location;
      interval.macro.first = LINEMAPS_MACRO_LOWEST_LOCATION (lmaps);
    }
  dump (dumper::LOCATION)
    && dump ("Pre span %u ordinary:[%K,%K) macro:[%K,%K)", spans->length (),
	     interval.ordinary.first, interval.ordinary.second,
	     interval.macro.first, interval.macro.second);
  spans->quick_push (interval);

  /* Start an interval for the main file.  */
  interval.ordinary.first = interval.ordinary.second;
  interval.macro.second = interval.macro.first;
  dump (dumper::LOCATION)
    && dump ("Main span %u ordinary:[%K,*) macro:[*,%K)", spans->length (),
	     interval.ordinary.first, interval.macro.second);
  spans->quick_push (interval);
}

/* Reopen the span, if we want the about-to-be-inserted set of maps to
   be propagated in our own location table.  I.e. we are the primary
   interface and we're importing a partition.  */

bool
loc_spans::maybe_propagate (module_state *import, location_t hwm)
{
  bool opened = (module_interface_p () && !module_partition_p ()
		 && import->is_partition ());
  if (opened)
    open (hwm);
  return opened;
}

/* Open a new linemap interval.  The just-created ordinary map is the
   first map of the interval.  */

void
loc_spans::open (location_t hwm)
{
  span interval;
  interval.ordinary.first = interval.ordinary.second = hwm;
  interval.macro.first = interval.macro.second
    = LINEMAPS_MACRO_LOWEST_LOCATION (line_table);
  interval.ordinary_delta = interval.macro_delta = 0;
  dump (dumper::LOCATION)
    && dump ("Opening span %u ordinary:[%K,... macro:...,%K)",
	     spans->length (), interval.ordinary.first,
	     interval.macro.second);
  if (spans->length ())
    {
      /* No overlapping!  */
      auto &last = spans->last ();
      gcc_checking_assert (interval.ordinary.first >= last.ordinary.second);
      gcc_checking_assert (interval.macro.second <= last.macro.first);
    }
  spans->safe_push (interval);
}

/* Close out the current linemap interval.  The last maps are within
   the interval.  */

void
loc_spans::close ()
{
  span &interval = spans->last ();

  interval.ordinary.second
    = ((line_table->highest_location
	+ (loc_one << line_table->default_range_bits))
       & ~((loc_one << line_table->default_range_bits) - 1));
  interval.macro.first = LINEMAPS_MACRO_LOWEST_LOCATION (line_table);
  dump (dumper::LOCATION)
    && dump ("Closing span %u ordinary:[%K,%K) macro:[%K,%K)",
	     spans->length () - 1,
	     interval.ordinary.first,interval.ordinary.second,
	     interval.macro.first, interval.macro.second);
}

/* Given an ordinary location LOC, return the lmap_interval it resides
   in.  NULL if it is not in an interval.  */

const loc_spans::span *
loc_spans::ordinary (location_t loc)
{
  unsigned len = spans->length ();
  unsigned pos = 0;
  while (len)
    {
      unsigned half = len / 2;
      const span &probe = (*spans)[pos + half];
      if (loc < probe.ordinary.first)
	len = half;
      else if (loc < probe.ordinary.second)
	return &probe;
      else
	{
	  pos += half + 1;
	  len = len - (half + 1);
	}
    }
  return NULL;
}

/* Likewise, given a macro location LOC, return the lmap interval it
   resides in.   */

const loc_spans::span *
loc_spans::macro (location_t loc)
{
  unsigned len = spans->length ();
  unsigned pos = 0;
  while (len)
    {
      unsigned half = len / 2;
      const span &probe = (*spans)[pos + half];
      if (loc >= probe.macro.second)
	len = half;
      else if (loc >= probe.macro.first)
	return &probe;
      else
	{
	  pos += half + 1;
	  len = len - (half + 1);
	}
    }
  return NULL;
}

/* Return the ordinary location closest to FROM.  */

static location_t
ordinary_loc_of (line_maps *lmaps, location_t from)
{
  while (!IS_ORDINARY_LOC (from))
    {
      if (IS_ADHOC_LOC (from))
	from = get_location_from_adhoc_loc (lmaps, from);
      if (from >= LINEMAPS_MACRO_LOWEST_LOCATION (lmaps))
	{
	  /* Find the ordinary location nearest FROM.  */
	  const line_map *map = linemap_lookup (lmaps, from);
	  const line_map_macro *mac_map = linemap_check_macro (map);
	  from = mac_map->get_expansion_point_location ();
	}
    }
  return from;
}

static module_state **
get_module_slot (tree name, module_state *parent, bool partition, bool insert)
{
  module_state_hash::compare_type ct (name, uintptr_t (parent) | partition);
  hashval_t hv = module_state_hash::hash (ct);

  return modules_hash->find_slot_with_hash (ct, hv, insert ? INSERT : NO_INSERT);
}

static module_state *
get_primary (module_state *parent)
{
  while (parent->is_partition ())
    parent = parent->parent;

  if (!parent->name)
    // Implementation unit has null name
    parent = parent->parent;

  return parent;
}

/* Find or create module NAME & PARENT in the hash table.  */

module_state *
get_module (tree name, module_state *parent, bool partition)
{
  /* We might be given an empty NAME if preprocessing fails to handle
     a header-name token.  */
  if (name && TREE_CODE (name) == STRING_CST
      && TREE_STRING_LENGTH (name) == 0)
    return nullptr;

  if (partition)
    {
      if (!parent)
	parent = get_primary ((*modules)[0]);

      if (!parent->is_partition () && !parent->flatname)
	parent->set_flatname ();
    }

  module_state **slot = get_module_slot (name, parent, partition, true);
  module_state *state = *slot;
  if (!state)
    {
      state = (new (ggc_alloc<module_state> ())
	       module_state (name, parent, partition));
      *slot = state;
    }
  return state;
}

/* Process string name PTR into a module_state.  */

static module_state *
get_module (const char *ptr)
{
  /* On DOS based file systems, there is an ambiguity with A:B which can be
     interpreted as a module Module:Partition or Drive:PATH.  Interpret strings
     which clearly starts as pathnames as header-names and everything else is
     treated as a (possibly malformed) named moduled.  */
  if (IS_DIR_SEPARATOR (ptr[ptr[0] == '.']) // ./FOO or /FOO
#if HAVE_DOS_BASED_FILE_SYSTEM
      || (HAS_DRIVE_SPEC (ptr) && IS_DIR_SEPARATOR (ptr[2])) // A:/FOO
#endif
      || false)
    /* A header name.  */
    return get_module (build_string (strlen (ptr), ptr));

  bool partition = false;
  module_state *mod = NULL;

  for (const char *probe = ptr;; probe++)
    if (!*probe || *probe == '.' || *probe == ':')
      {
	if (probe == ptr)
	  return NULL;

	mod = get_module (get_identifier_with_length (ptr, probe - ptr),
			  mod, partition);
	ptr = probe;
	if (*ptr == ':')
	  {
	    if (partition)
	      return NULL;
	    partition = true;
	  }

	if (!*ptr++)
	  break;
      }
    else if (!(ISALPHA (*probe) || *probe == '_'
	       || (probe != ptr && ISDIGIT (*probe))))
      return NULL;

  return mod;
}

/* Create a new mapper connecting to OPTION.  */

module_client *
make_mapper (location_t loc, class mkdeps *deps)
{
  timevar_start (TV_MODULE_MAPPER);
  const char *option = module_mapper_name;
  if (!option)
    option = getenv ("CXX_MODULE_MAPPER");

  mapper = module_client::open_module_client
    (loc, option, deps, &set_cmi_repo,
     (save_decoded_options[0].opt_index == OPT_SPECIAL_program_name)
     && save_decoded_options[0].arg != progname
     ? save_decoded_options[0].arg : nullptr);

  timevar_stop (TV_MODULE_MAPPER);

  return mapper;
}

static unsigned lazy_snum;

static bool
recursive_lazy (unsigned snum = ~0u)
{
  if (lazy_snum)
    {
      error_at (input_location, "recursive lazy load");
      return true;
    }

  lazy_snum = snum;
  return false;
}

/* If THIS is the current purview, issue an import error and return false.  */

bool
module_state::check_not_purview (location_t from)
{
  module_state *imp = (*modules)[0];
  if (imp && !imp->name)
    imp = imp->parent;
  if (imp == this)
    {
      /* Cannot import the current module.  */
      auto_diagnostic_group d;
      error_at (from, "cannot import module in its own purview");
      inform (loc, "module %qs declared here", get_flatname ());
      return false;
    }
  return true;
}

/* Module name substitutions.  */
static vec<module_state *,va_heap> substs;

void
module_state::mangle (bool include_partition)
{
  if (subst)
    mangle_module_substitution (subst);
  else
    {
      if (parent)
	parent->mangle (include_partition);
      if (include_partition || !is_partition ())
	{
	  // Partitions are significant for global initializer
	  // functions
	  bool partition = is_partition () && !parent->is_partition ();
	  subst = mangle_module_component (name, partition);
	  substs.safe_push (this);
	}
    }
}

void
mangle_module (int mod, bool include_partition)
{
  module_state *imp = (*modules)[mod];

  gcc_checking_assert (!imp->is_header ());

  if (!imp->name)
    /* Set when importing the primary module interface.  */
    imp = imp->parent;

  imp->mangle (include_partition);
}

/* Clean up substitutions.  */
void
mangle_module_fini ()
{
  while (substs.length ())
    substs.pop ()->subst = 0;
}

/* Announce WHAT about the module.  */

void
module_state::announce (const char *what) const
{
  if (noisy_p ())
    {
      fprintf (stderr, " %s:%s", what, get_flatname ());
      fflush (stderr);
    }
}

/* A human-readable README section.  The contents of this section to
   not contribute to the CRC, so the contents can change per
   compilation.  That allows us to embed CWD, hostname, build time and
   what not.  It is a STRTAB that may be extracted with:
     readelf -pgnu.c++.README $(module).gcm */

void
module_state::write_readme (elf_out *to, cpp_reader *reader, const char *dialect)
{
  bytes_out readme (to);

  readme.begin (false);

  readme.printf ("GNU C++ %s",
		 is_header () ? "header unit"
		 : !is_partition () ? "primary interface"
		 : is_interface () ? "interface partition"
		 : "internal partition");

  /* Compiler's version.  */
  readme.printf ("compiler: %s", version_string);

  /* Module format version.  */
  verstr_t string;
  version2string (MODULE_VERSION, string);
  readme.printf ("version: %s", string);

  /* Module information.  */
  readme.printf ("module: %s", get_flatname ());
  readme.printf ("source: %s", main_input_filename);
  readme.printf ("dialect: %s", dialect);
  if (extensions)
    readme.printf ("extensions: %s",
		   extensions & SE_OPENMP ? "-fopenmp" : "");

  /* The following fields could be expected to change between
     otherwise identical compilations.  Consider a distributed build
     system.  We should have a way of overriding that.  */
  if (char *cwd = getcwd (NULL, 0))
    {
      readme.printf ("cwd: %s", cwd);
      free (cwd);
    }
  readme.printf ("repository: %s", cmi_repo ? cmi_repo : ".");
#if NETWORKING
  {
    char hostname[64];
    if (!gethostname (hostname, sizeof (hostname)))
      readme.printf ("host: %s", hostname);
  }
#endif
  {
    /* This of course will change!  */
    time_t stampy;
    auto kind = cpp_get_date (reader, &stampy);
    if (kind != CPP_time_kind::UNKNOWN)
      {
	struct tm *time;

	time = gmtime (&stampy);
	readme.print_time ("build", time, "UTC");

	if (kind == CPP_time_kind::DYNAMIC)
	  {
	    time = localtime (&stampy);
	    readme.print_time ("local", time,
#if defined (__USE_MISC) || defined (__USE_BSD) /* Is there a better way?  */
			       time->tm_zone
#else
			       ""
#endif
			       );
	  }
      }
  }

  /* Its direct imports.  */
  for (unsigned ix = 1; ix < modules->length (); ix++)
    {
      module_state *state = (*modules)[ix];

      if (state->is_direct ())
	readme.printf ("%s: %s %s", state->exported_p ? "export" : "import",
		       state->get_flatname (), state->filename);
    }

  readme.end (to, to->name (MOD_SNAME_PFX ".README"), NULL);
}

/* Sort environment var names in reverse order.  */

static int
env_var_cmp (const void *a_, const void *b_)
{
  const unsigned char *a = *(const unsigned char *const *)a_;
  const unsigned char *b = *(const unsigned char *const *)b_;

  for (unsigned ix = 0; ; ix++)
    {
      bool a_end = !a[ix] || a[ix] == '=';
      if (a[ix] == b[ix])
	{
	  if (a_end)
	    break;
	}
      else
	{
	  bool b_end = !b[ix] || b[ix] == '=';

	  if (!a_end && !b_end)
	    return a[ix] < b[ix] ? +1 : -1;
	  if (a_end && b_end)
	    break;
	  return a_end ? +1 : -1;
	}
    }

  return 0;
}

/* Write the environment. It is a STRTAB that may be extracted with:
     readelf -pgnu.c++.ENV $(module).gcm */

void
module_state::write_env (elf_out *to)
{
  vec<const char *> vars;
  vars.create (20);

  extern char **environ;
  while (const char *var = environ[vars.length ()])
    vars.safe_push (var);
  vars.qsort (env_var_cmp);

  bytes_out env (to);
  env.begin (false);
  while (vars.length ())
    env.printf ("%s", vars.pop ());
  env.end (to, to->name (MOD_SNAME_PFX ".ENV"), NULL);

  vars.release ();
}

/* Write the direct or indirect imports.
   u:N
   {
     u:index
     s:name
     u32:crc
     s:filename (direct)
     u:exported (direct)
   } imports[N]
 */

void
module_state::write_imports (bytes_out &sec, bool direct)
{
  unsigned count = 0;

  for (unsigned ix = 1; ix < modules->length (); ix++)
    {
      module_state *imp = (*modules)[ix];

      if (imp->remap && imp->is_direct () == direct)
	count++;
    }

  gcc_assert (!direct || count);

  sec.u (count);
  for (unsigned ix = 1; ix < modules->length (); ix++)
    {
      module_state *imp = (*modules)[ix];

      if (imp->remap && imp->is_direct () == direct)
	{
	  dump () && dump ("Writing %simport:%u->%u %M (crc=%x)",
			   !direct ? "indirect "
			   : imp->exported_p ? "exported " : "",
			   ix, imp->remap, imp, imp->crc);
	  sec.u (imp->remap);
	  sec.str (imp->get_flatname ());
	  sec.u32 (imp->crc);
	  if (direct)
	    {
	      write_location (sec, imp->imported_from ());
	      sec.str (imp->filename);
	      int exportedness = 0;
	      if (imp->exported_p)
		exportedness = +1;
	      else if (!imp->is_purview_direct ())
		exportedness = -1;
	      sec.i (exportedness);
	    }
	}
    }
}

/* READER, LMAPS  != NULL == direct imports,
   == NUL == indirect imports.  */

unsigned
module_state::read_imports (bytes_in &sec, cpp_reader *reader, line_maps *lmaps)
{
  unsigned count = sec.u ();
  unsigned loaded = 0;

  while (count--)
    {
      unsigned ix = sec.u ();
      if (ix >= slurp->remap->length () || !ix || (*slurp->remap)[ix])
	{
	  sec.set_overrun ();
	  break;
	}

      const char *name = sec.str (NULL);
      module_state *imp = get_module (name);
      unsigned crc = sec.u32 ();
      int exportedness = 0;

      /* If the import is a partition, it must be the same primary
	 module as this TU.  */
      if (imp && imp->is_partition () &&
	  (!named_module_p ()
	   || (get_primary ((*modules)[0]) != get_primary (imp))))
	imp = NULL;

      if (!imp)
	sec.set_overrun ();
      if (sec.get_overrun ())
	break;

      if (lmaps)
	{
	  /* A direct import, maybe load it.  */
	  location_t floc = read_location (sec);
	  const char *fname = sec.str (NULL);
	  exportedness = sec.i ();

	  if (sec.get_overrun ())
	    break;

	  if (!imp->check_not_purview (loc))
	    continue;

	  if (imp->loadedness == ML_NONE)
	    {
	      imp->loc = floc;
	      imp->crc = crc;
	      if (!imp->get_flatname ())
		imp->set_flatname ();

	      unsigned n = dump.push (imp);

	      if (!imp->filename && fname)
		imp->filename = xstrdup (fname);

	      if (imp->is_partition ())
		dump () && dump ("Importing elided partition %M", imp);

	      if (!imp->do_import (reader, false))
		imp = NULL;
	      dump.pop (n);
	      if (!imp)
		continue;
	    }

	  if (is_partition ())
	    {
	      if (!imp->is_direct ())
		imp->directness = MD_PARTITION_DIRECT;
	      if (exportedness > 0)
		imp->exported_p = true;
	    }
	}
      else
	{
	  /* An indirect import, find it, it should already be here.  */
	  if (imp->loadedness == ML_NONE)
	    {
	      error_at (loc, "indirect import %qs is not already loaded", name);
	      continue;
	    }
	}

      if (imp->crc != crc)
	error_at (loc, "import %qs has CRC mismatch", imp->get_flatname ());

      (*slurp->remap)[ix] = (imp->mod << 1) | (lmaps != NULL);

      if (lmaps && exportedness >= 0)
	set_import (imp, bool (exportedness));
      dump () && dump ("Found %simport:%u %M->%u", !lmaps ? "indirect "
		       : exportedness > 0 ? "exported "
		       : exportedness < 0 ? "gmf" : "", ix, imp,
		       imp->mod);
      loaded++;
    }

  return loaded;
}

/* Write the import table to MOD_SNAME_PFX.imp.  */

void
module_state::write_imports (elf_out *to, unsigned *crc_ptr)
{
  dump () && dump ("Writing imports");
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  write_imports (sec, true);
  write_imports (sec, false);

  sec.end (to, to->name (MOD_SNAME_PFX ".imp"), crc_ptr);
  dump.outdent ();
}

bool
module_state::read_imports (cpp_reader *reader, line_maps *lmaps)
{
  bytes_in sec;

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".imp"))
    return false;

  dump () && dump ("Reading %u imports", slurp->remap->length () - 1);
  dump.indent ();

  /* Read the imports.  */
  unsigned direct = read_imports (sec, reader, lmaps);
  unsigned indirect = read_imports (sec, NULL, NULL);
  if (direct + indirect + 1 != slurp->remap->length ())
    from ()->set_error (elf::E_BAD_IMPORT);

  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* We're the primary module interface, but have partitions.  Document
   them so that non-partition module implementation units know which
   have already been loaded.  */

void
module_state::write_partitions (elf_out *to, unsigned count, unsigned *crc_ptr)
{
  dump () && dump ("Writing %u elided partitions", count);
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  for (unsigned ix = 1; ix != modules->length (); ix++)
    {
      module_state *imp = (*modules)[ix];
      if (imp->is_partition ())
	{
	  dump () && dump ("Writing elided partition %M (crc=%x)",
			   imp, imp->crc);
	  sec.str (imp->get_flatname ());
	  sec.u32 (imp->crc);
	  write_location (sec, imp->is_direct ()
			  ? imp->imported_from () : UNKNOWN_LOCATION);
	  sec.str (imp->filename);
	}
    }

  sec.end (to, to->name (MOD_SNAME_PFX ".prt"), crc_ptr);
  dump.outdent ();
}

bool
module_state::read_partitions (unsigned count)
{
  bytes_in sec;
  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".prt"))
    return false;

  dump () && dump ("Reading %u elided partitions", count);
  dump.indent ();

  while (count--)
    {
      const char *name = sec.str (NULL);
      unsigned crc = sec.u32 ();
      location_t floc = read_location (sec);
      const char *fname = sec.str (NULL);

      if (sec.get_overrun ())
	break;

      dump () && dump ("Reading elided partition %s (crc=%x)", name, crc);

      module_state *imp = get_module (name);
      if (!imp	/* Partition should be ...  */
	  || !imp->is_partition () /* a partition ...  */
	  || imp->loadedness != ML_NONE  /* that is not yet loaded ...  */
	  || get_primary (imp) != this) /* whose primary is this.  */
	{
	  sec.set_overrun ();
	  break;
	}

      if (!imp->has_location ())
	imp->loc = floc;
      imp->crc = crc;
      if (!imp->filename && fname[0])
	imp->filename = xstrdup (fname);
    }

  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* Data for config reading and writing.  */
struct module_state_config {
  const char *dialect_str = get_dialect ();
  line_map_uint_t ordinary_locs = 0;
  line_map_uint_t macro_locs = 0;
  unsigned num_imports = 0;
  unsigned num_partitions = 0;
  unsigned num_entities = 0;
  unsigned loc_range_bits = 0;
  unsigned active_init = 0;

  static void release ()
  {
    XDELETEVEC (dialect);
    dialect = NULL;
  }

private:
  static const char *get_dialect ();
  static char *dialect;
};

char *module_state_config::dialect;

/* Generate a string of the significant compilation options.
   Generally assume the user knows what they're doing, in the same way
   that object files can be mixed.  */

const char *
module_state_config::get_dialect ()
{
  if (!dialect)
    dialect = concat (get_cxx_dialect_name (cxx_dialect),
		      /* C++ implies these, only show if disabled.  */
		      flag_exceptions ? "" : "/no-exceptions",
		      flag_rtti ? "" : "/no-rtti",
		      flag_new_inheriting_ctors ? "" : "/old-inheriting-ctors",
		      /* C++ 20 implies concepts and coroutines.  */
		      cxx_dialect < cxx20 && flag_concepts ? "/concepts" : "",
		      (cxx_dialect < cxx20 && flag_coroutines
		       ? "/coroutines" : ""),
		      flag_module_implicit_inline ? "/implicit-inline" : "",
		      flag_contracts ? "/contracts" : "",
		      NULL);

  return dialect;
}

/* Contents of a cluster.  */
enum cluster_tag {
  ct_decl,	/* A decl.  */
  ct_defn,	/* A definition.  */
  ct_bind,	/* A binding.  */
  ct_hwm
};

/* Binding modifiers.  */
enum ct_bind_flags
{
  cbf_export = 0x1,	/* An exported decl.  */
  cbf_hidden = 0x2,	/* A hidden (friend) decl.  */
  cbf_using = 0x4,	/* A using decl.  */
};

/* DEP belongs to a different cluster, seed it to prevent
   unfortunately timed duplicate import.  */
// FIXME: QOI For inter-cluster references we could just only pick
// one entity from an earlier cluster.  Even better track
// dependencies between earlier clusters

void
module_state::intercluster_seed (trees_out &sec, unsigned index_hwm, depset *dep)
{
  if (dep->is_import ()
      || dep->cluster < index_hwm)
    {
      tree ent = dep->get_entity ();
      if (!TREE_VISITED (ent))
	{
	  sec.tree_node (ent);
	  dump (dumper::CLUSTER)
	    && dump ("Seeded %s %N",
		     dep->is_import () ? "import" : "intercluster", ent);
	}
    }
}

/* Write the cluster of depsets in SCC[0-SIZE).
   dep->section -> section number
   dep->cluster -> entity number
 */

unsigned
module_state::write_cluster (elf_out *to, depset *scc[], unsigned size,
			     depset::hash &table, unsigned *counts,
			     unsigned *crc_ptr)
{
  dump () && dump ("Writing section:%u %u depsets", table.section, size);
  dump.indent ();

  trees_out sec (to, this, table, table.section);
  sec.begin ();
  unsigned index_lwm = counts[MSC_entities];

  /* Determine entity numbers, mark for writing.   */
  dump (dumper::CLUSTER) && dump ("Cluster members:") && (dump.indent (), true);
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];

      switch (b->get_entity_kind ())
	{
	default:
	  gcc_unreachable ();

	case depset::EK_BINDING:
	  {
	    dump (dumper::CLUSTER)
	      && dump ("[%u]=%s %P", ix, b->entity_kind_name (),
		       b->get_entity (), b->get_name ());
	    depset *ns_dep = b->deps[0];
	    gcc_checking_assert (ns_dep->get_entity_kind ()
				 == depset::EK_NAMESPACE
				 && ns_dep->get_entity () == b->get_entity ());
	    for (unsigned jx = b->deps.length (); --jx;)
	      {
		depset *dep = b->deps[jx];
		// We could be declaring something that is also a
		// (merged) import
		gcc_checking_assert (dep->is_import ()
				     || TREE_VISITED (dep->get_entity ())
				     || (dep->get_entity_kind ()
					 == depset::EK_USING));
	      }
	  }
	  break;

	case depset::EK_DECL:
	case depset::EK_SPECIALIZATION:
	case depset::EK_PARTIAL:
	  b->cluster = counts[MSC_entities]++;
	  sec.mark_declaration (b->get_entity (), b->has_defn ());
	  /* FALLTHROUGH  */

	case depset::EK_USING:
	  gcc_checking_assert (!b->is_import ()
			       && !b->is_unreached ());
	  dump (dumper::CLUSTER)
	    && dump ("[%u]=%s %s %N", ix, b->entity_kind_name (),
		     b->has_defn () ? "definition" : "declaration",
		     b->get_entity ());
	  break;
	}
    }
  dump (dumper::CLUSTER) && (dump.outdent (), true);

  /* Ensure every out-of-cluster decl is referenced before we start
     streaming.  We must do both imports *and* earlier clusters,
     because the latter could reach into the former and cause a
     duplicate loop.   */
  sec.set_importing (+1);
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];
      for (unsigned jx = b->is_special (); jx != b->deps.length (); jx++)
	{
	  depset *dep = b->deps[jx];

	  if (dep->is_binding ())
	    {
	      for (unsigned ix = dep->deps.length (); --ix;)
		{
		  depset *bind = dep->deps[ix];
		  if (bind->get_entity_kind () == depset::EK_USING)
		    bind = bind->deps[1];

		  intercluster_seed (sec, index_lwm, bind);
		}
	      /* Also check the namespace itself.  */
	      dep = dep->deps[0];
	    }

	  intercluster_seed (sec, index_lwm, dep);
	}
    }
  sec.tree_node (NULL_TREE);
  /* We're done importing now.  */
  sec.set_importing (-1);

  /* Write non-definitions.  */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];
      tree decl = b->get_entity ();
      switch (b->get_entity_kind ())
	{
	default:
	  gcc_unreachable ();
	  break;

	case depset::EK_BINDING:
	  {
	    gcc_assert (TREE_CODE (decl) == NAMESPACE_DECL);
	    dump () && dump ("Depset:%u binding %C:%P", ix, TREE_CODE (decl),
			     decl, b->get_name ());
	    sec.u (ct_bind);
	    sec.tree_node (decl);
	    sec.tree_node (b->get_name ());

	    /* Write in reverse order, so reading will see the exports
	       first, thus building the overload chain will be
	       optimized.  */
	    for (unsigned jx = b->deps.length (); --jx;)
	      {
		depset *dep = b->deps[jx];
		tree bound = dep->get_entity ();
		unsigned flags = 0;
		if (dep->get_entity_kind () == depset::EK_USING)
		  {
		    tree ovl = bound;
		    bound = OVL_FUNCTION (bound);
		    if (!(TREE_CODE (bound) == CONST_DECL
			  && UNSCOPED_ENUM_P (TREE_TYPE (bound))
			  && decl == TYPE_NAME (TREE_TYPE (bound))))
		      /* An unscoped enumerator in its enumeration's
			 scope is not a using.  */
		      flags |= cbf_using;
		    if (OVL_EXPORT_P (ovl))
		      flags |= cbf_export;
		  }
		else
		  {
		    /* An implicit typedef must be at one.  */
		    gcc_assert (!DECL_IMPLICIT_TYPEDEF_P (bound) || jx == 1);
		    if (dep->is_hidden ())
		      flags |= cbf_hidden;
		    else if (DECL_MODULE_EXPORT_P (STRIP_TEMPLATE (bound)))
		      flags |= cbf_export;
		  }

		gcc_checking_assert (DECL_P (bound));

		sec.i (flags);
		sec.tree_node (bound);
	      }

	    /* Terminate the list.  */
	    sec.i (-1);
	  }
	  break;

	case depset::EK_USING:
	  dump () && dump ("Depset:%u %s %C:%N", ix, b->entity_kind_name (),
			   TREE_CODE (decl), decl);
	  break;

	case depset::EK_SPECIALIZATION:
	case depset::EK_PARTIAL:
	case depset::EK_DECL:
	  dump () && dump ("Depset:%u %s entity:%u %C:%N", ix,
			   b->entity_kind_name (), b->cluster,
			   TREE_CODE (decl), decl);

	  sec.u (ct_decl);
	  sec.tree_node (decl);

	  dump () && dump ("Wrote declaration entity:%u %C:%N",
			   b->cluster, TREE_CODE (decl), decl);
	  break;
	}
    }

  depset *namer = NULL;

  /* Write out definitions  */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];
      tree decl = b->get_entity ();
      switch (b->get_entity_kind ())
	{
	default:
	  break;

	case depset::EK_SPECIALIZATION:
	case depset::EK_PARTIAL:
	case depset::EK_DECL:
	  if (!namer)
	    namer = b;

	  if (b->has_defn ())
	    {
	      sec.u (ct_defn);
	      sec.tree_node (decl);
	      dump () && dump ("Writing definition %N", decl);
	      sec.write_definition (decl);

	      if (!namer->has_defn ())
		namer = b;
	    }
	  break;
	}
    }

  /* We don't find the section by name.  Use depset's decl's name for
     human friendliness.  */
  unsigned name = 0;
  tree naming_decl = NULL_TREE;
  if (namer)
    {
      naming_decl = namer->get_entity ();
      if (namer->get_entity_kind () == depset::EK_USING)
	/* This unfortunately names the section from the target of the
	   using decl.  But the name is only a guide, so Do Not Care.  */
	naming_decl = OVL_FUNCTION (naming_decl);
      if (DECL_IMPLICIT_TYPEDEF_P (naming_decl))
	/* Lose any anonymousness.  */
	naming_decl = TYPE_NAME (TREE_TYPE (naming_decl));
      name = to->qualified_name (naming_decl, namer->has_defn ());
    }

  unsigned bytes = sec.pos;
  unsigned snum = sec.end (to, name, crc_ptr);

  for (unsigned ix = size; ix--;)
    gcc_checking_assert (scc[ix]->section == snum);

  dump.outdent ();
  dump () && dump ("Wrote section:%u named-by:%N", table.section, naming_decl);

  return bytes;
}

/* Read a cluster from section SNUM.  */

bool
module_state::read_cluster (unsigned snum)
{
  trees_in sec (this);

  if (!sec.begin (loc, from (), snum))
    return false;

  dump () && dump ("Reading section:%u", snum);
  dump.indent ();

  /* We care about structural equality.  */
  comparing_dependent_aliases++;

  /* First seed the imports.  */
  while (tree import = sec.tree_node ())
    dump (dumper::CLUSTER) && dump ("Seeded import %N", import);

  while (!sec.get_overrun () && sec.more_p ())
    {
      unsigned ct = sec.u ();
      switch (ct)
	{
	default:
	  sec.set_overrun ();
	  break;

	case ct_bind:
	  /* A set of namespace bindings.  */
	  {
	    tree ns = sec.tree_node ();
	    tree name = sec.tree_node ();
	    tree decls = NULL_TREE;
	    tree visible = NULL_TREE;
	    tree type = NULL_TREE;
	    bool dedup = false;
	    bool global_p = is_header ();

	    /* We rely on the bindings being in the reverse order of
	       the resulting overload set.  */
	    for (;;)
	      {
		int flags = sec.i ();
		if (flags < 0)
		  break;

		if ((flags & cbf_hidden)
		    && (flags & (cbf_using | cbf_export)))
		  sec.set_overrun ();

		tree decl = sec.tree_node ();
		if (sec.get_overrun ())
		  break;

		if (!global_p)
		  {
		    /* Check if the decl could require GM merging.  */
		    tree orig = get_originating_module_decl (decl);
		    tree inner = STRIP_TEMPLATE (orig);
		    if (!DECL_LANG_SPECIFIC (inner)
			|| !DECL_MODULE_ATTACH_P (inner))
		      global_p = true;
		  }

		if (decls && TREE_CODE (decl) == TYPE_DECL)
		  {
		    /* Stat hack.  */
		    if (type || !DECL_IMPLICIT_TYPEDEF_P (decl))
		      sec.set_overrun ();

		    if (flags & cbf_using)
		      {
			type = build_lang_decl_loc (UNKNOWN_LOCATION,
						    USING_DECL,
						    DECL_NAME (decl),
						    NULL_TREE);
			USING_DECL_DECLS (type) = decl;
			USING_DECL_SCOPE (type) = CP_DECL_CONTEXT (decl);
			DECL_CONTEXT (type) = ns;

			DECL_MODULE_PURVIEW_P (type) = true;
			if (flags & cbf_export)
			  DECL_MODULE_EXPORT_P (type) = true;
		      }
		    else
		      type = decl;
		  }
		else
		  {
		    if ((flags & cbf_using) &&
			!DECL_DECLARES_FUNCTION_P (decl))
		      {
			/* We should only see a single non-function using-decl
			   for a binding; more than that would clash.  */
			if (decls)
			  sec.set_overrun ();

			/* FIXME: Propagate the location of the using-decl
			   for use in diagnostics.  */
			decls = build_lang_decl_loc (UNKNOWN_LOCATION,
						     USING_DECL,
						     DECL_NAME (decl),
						     NULL_TREE);
			USING_DECL_DECLS (decls) = decl;
			/* We don't currently record the actual scope of the
			   using-declaration, but this approximation should
			   generally be good enough.  */
			USING_DECL_SCOPE (decls) = CP_DECL_CONTEXT (decl);
			DECL_CONTEXT (decls) = ns;

			DECL_MODULE_PURVIEW_P (decls) = true;
			if (flags & cbf_export)
			  DECL_MODULE_EXPORT_P (decls) = true;
		      }
		    else if (decls
			     || (flags & (cbf_hidden | cbf_using))
			     || DECL_FUNCTION_TEMPLATE_P (decl))
		      {
			decls = ovl_make (decl, decls);
			if (flags & cbf_using)
			  {
			    dedup = true;
			    OVL_USING_P (decls) = true;
			    OVL_PURVIEW_P (decls) = true;
			    if (flags & cbf_export)
			      OVL_EXPORT_P (decls) = true;
			  }

			if (flags & cbf_hidden)
			  OVL_HIDDEN_P (decls) = true;
			else if (dedup)
			  OVL_DEDUP_P (decls) = true;
		      }
		    else
		      decls = decl;

		    if (flags & cbf_export
			|| (!(flags & cbf_hidden)
			    && (is_module () || is_partition ())))
		      visible = decls;
		  }
	      }

	    if (!decls)
	      sec.set_overrun ();

	    if (sec.get_overrun ())
	      break; /* Bail.  */

	    dump () && dump ("Binding of %P", ns, name);
	    if (!set_module_binding (ns, name, mod, global_p,
				     is_module () || is_partition (),
				     decls, type, visible))
	      sec.set_overrun ();
	  }
	  break;

	case ct_decl:
	  /* A decl.  */
	  {
	    tree decl = sec.tree_node ();
	    dump () && dump ("Read declaration of %N", decl);
	  }
	  break;

	case ct_defn:
	  {
	    tree decl = sec.tree_node ();
	    dump () && dump ("Reading definition of %N", decl);
	    sec.read_definition (decl);
	  }
	  break;
	}
    }

  /* When lazy loading is in effect, we can be in the middle of
     parsing or instantiating a function.  Save it away.
     push_function_context does too much work.   */
  tree old_cfd = current_function_decl;
  struct function *old_cfun = cfun;
  for (const post_process_data& pdata : sec.post_process ())
    {
      tree decl = pdata.decl;

      bool abstract = false;
      if (TREE_CODE (decl) == TEMPLATE_DECL)
	{
	  abstract = true;
	  decl = DECL_TEMPLATE_RESULT (decl);
	}

      current_function_decl = decl;
      allocate_struct_function (decl, abstract);
      cfun->language = ggc_cleared_alloc<language_function> ();
      cfun->language->base.x_stmt_tree.stmts_are_full_exprs_p = 1;
      cfun->function_start_locus = pdata.start_locus;
      cfun->function_end_locus = pdata.end_locus;

      if (abstract)
	;
      else if (DECL_MAYBE_IN_CHARGE_CDTOR_P (decl))
	vec_safe_push (post_load_decls, decl);
      else
	{
	  bool aggr = aggregate_value_p (DECL_RESULT (decl), decl);
#ifdef PCC_STATIC_STRUCT_RETURN
	  cfun->returns_pcc_struct = aggr;
#endif
	  cfun->returns_struct = aggr;

	  if (DECL_COMDAT (decl))
	    // FIXME: Comdat grouping?
	    comdat_linkage (decl);
	  note_vague_linkage_fn (decl);
	  cgraph_node::finalize_function (decl, true);
	}

    }
  /* Look, function.cc's interface to cfun does too much for us, we
     just need to restore the old value.  I do not want to go
     redesigning that API right now.  */
#undef cfun
  cfun = old_cfun;
  current_function_decl = old_cfd;
  comparing_dependent_aliases--;

  dump.outdent ();
  dump () && dump ("Read section:%u", snum);

  loaded_clusters++;

  if (!sec.end (from ()))
    return false;

  return true;
}

void
module_state::write_namespace (bytes_out &sec, depset *dep)
{
  unsigned ns_num = dep->cluster;
  unsigned ns_import = 0;

  if (dep->is_import ())
    ns_import = dep->section;
  else if (dep->get_entity () != global_namespace)
    ns_num++;

  sec.u (ns_import);
  sec.u (ns_num);
}

tree
module_state::read_namespace (bytes_in &sec)
{
  unsigned ns_import = sec.u ();
  unsigned ns_num = sec.u ();
  tree ns = NULL_TREE;

  if (ns_import || ns_num)
    {
      if (!ns_import)
	ns_num--;

      if (unsigned origin = slurp->remap_module (ns_import))
	{
	  module_state *from = (*modules)[origin];
	  if (ns_num < from->entity_num)
	    {
	      binding_slot &slot = (*entity_ary)[from->entity_lwm + ns_num];

	      if (!slot.is_lazy ())
		ns = slot;
	    }
	}
      else
	sec.set_overrun ();
    }
  else
    ns = global_namespace;

  return ns;
}

/* SPACES is a sorted vector of namespaces.  Write out the namespaces
   to MOD_SNAME_PFX.nms section.   */

void
module_state::write_namespaces (elf_out *to, vec<depset *> spaces,
				unsigned num, unsigned *crc_p)
{
  dump () && dump ("Writing namespaces");
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  for (unsigned ix = 0; ix != num; ix++)
    {
      depset *b = spaces[ix];
      tree ns = b->get_entity ();

      gcc_checking_assert (TREE_CODE (ns) == NAMESPACE_DECL);
      /* P1815 may have something to say about this.  */
      gcc_checking_assert (TREE_PUBLIC (ns));

      unsigned flags = 0;
      if (TREE_PUBLIC (ns))
	flags |= 1;
      if (DECL_NAMESPACE_INLINE_P (ns))
	flags |= 2;
      if (DECL_MODULE_PURVIEW_P (ns))
	flags |= 4;
      if (DECL_MODULE_EXPORT_P (ns))
	flags |= 8;

      dump () && dump ("Writing namespace:%u %N%s%s%s%s",
		       b->cluster, ns,
		       flags & 1 ? ", public" : "",
		       flags & 2 ? ", inline" : "",
		       flags & 4 ? ", purview" : "",
		       flags & 8 ? ", export" : "");
      sec.u (b->cluster);
      sec.u (to->name (DECL_NAME (ns)));
      write_namespace (sec, b->deps[0]);

      sec.u (flags);
      write_location (sec, DECL_SOURCE_LOCATION (ns));

      if (DECL_NAMESPACE_INLINE_P (ns))
	{
	  if (tree attr = lookup_attribute ("abi_tag", DECL_ATTRIBUTES (ns)))
	    {
	      tree tags = TREE_VALUE (attr);
	      sec.u (list_length (tags));
	      for (tree tag = tags; tag; tag = TREE_CHAIN (tag))
		sec.str (TREE_STRING_POINTER (TREE_VALUE (tag)));
	    }
	  else
	    sec.u (0);
	}
    }

  sec.end (to, to->name (MOD_SNAME_PFX ".nms"), crc_p);
  dump.outdent ();
}

/* Read the namespace hierarchy from MOD_SNAME_PFX.namespace.  Fill in
   SPACES from that data.  */

bool
module_state::read_namespaces (unsigned num)
{
  bytes_in sec;

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".nms"))
    return false;

  dump () && dump ("Reading namespaces");
  dump.indent ();

  for (unsigned ix = 0; ix != num; ix++)
    {
      unsigned entity_index = sec.u ();
      unsigned name = sec.u ();

      tree parent = read_namespace (sec);

      /* See comment in write_namespace about why not bits.  */
      unsigned flags = sec.u ();
      location_t src_loc = read_location (sec);
      unsigned tags_count = (flags & 2) ? sec.u () : 0;

      if (entity_index >= entity_num
	  || !parent
	  || (flags & 0xc) == 0x8)
	sec.set_overrun ();

      tree tags = NULL_TREE;
      while (tags_count--)
	{
	  size_t len;
	  const char *str = sec.str (&len);
	  tags = tree_cons (NULL_TREE, build_string (len + 1, str), tags);
	  tags = nreverse (tags);
	}

      if (sec.get_overrun ())
	break;

      tree id = name ? get_identifier (from ()->name (name)) : NULL_TREE;

      dump () && dump ("Read namespace:%u %P%s%s%s%s",
		       entity_index, parent, id,
		       flags & 1 ? ", public" : "",
		       flags & 2 ? ", inline" : "",
		       flags & 4 ? ", purview" : "",
		       flags & 8 ? ", export" : "");
      bool visible_p = ((flags & 8)
			|| ((flags & 1)
			    && (flags & 4)
			    && (is_partition () || is_module ())));
      tree inner = add_imported_namespace (parent, id, src_loc, mod,
					   bool (flags & 2), visible_p);
      if (!inner)
	{
	  sec.set_overrun ();
	  break;
	}

      if (is_partition ())
	{
	  if (flags & 4)
	    DECL_MODULE_PURVIEW_P (inner) = true;
	  if (flags & 8)
	    DECL_MODULE_EXPORT_P (inner) = true;
	}

      if (tags)
	DECL_ATTRIBUTES (inner)
	  = tree_cons (get_identifier ("abi_tag"), tags, DECL_ATTRIBUTES (inner));

      /* Install the namespace.  */
      (*entity_ary)[entity_lwm + entity_index] = inner;
      if (DECL_MODULE_IMPORT_P (inner))
	{
	  bool existed;
	  unsigned *slot = &entity_map->get_or_insert
	    (DECL_UID (inner), &existed);
	  if (existed)
	    /* If it existed, it should match.  */
	    gcc_checking_assert (inner == (*entity_ary)[*slot]);
	  else
	    *slot = entity_lwm + entity_index;
	}
    }
  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* Write the binding TABLE to MOD_SNAME_PFX.bnd   */

unsigned
module_state::write_bindings (elf_out *to, vec<depset *> sccs, unsigned *crc_p)
{
  dump () && dump ("Writing binding table");
  dump.indent ();

  unsigned num = 0;
  bytes_out sec (to);
  sec.begin ();

  for (unsigned ix = 0; ix != sccs.length (); ix++)
    {
      depset *b = sccs[ix];
      if (b->is_binding ())
	{
	  tree ns = b->get_entity ();
	  dump () && dump ("Bindings %P section:%u", ns, b->get_name (),
			   b->section);
	  sec.u (to->name (b->get_name ()));
	  write_namespace (sec, b->deps[0]);
	  sec.u (b->section);
	  num++;
	}
    }

  sec.end (to, to->name (MOD_SNAME_PFX ".bnd"), crc_p);
  dump.outdent ();

  return num;
}

/* Read the binding table from MOD_SNAME_PFX.bind.  */

bool
module_state::read_bindings (unsigned num, unsigned lwm, unsigned hwm)
{
  bytes_in sec;

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".bnd"))
    return false;

  dump () && dump ("Reading binding table");
  dump.indent ();
  for (; !sec.get_overrun () && num--;)
    {
      const char *name = from ()->name (sec.u ());
      tree ns = read_namespace (sec);
      unsigned snum = sec.u ();

      if (!ns || !name || (snum - lwm) >= (hwm - lwm))
	sec.set_overrun ();
      if (!sec.get_overrun ())
	{
	  tree id = get_identifier (name);
	  dump () && dump ("Bindings %P section:%u", ns, id, snum);
	  if (mod && !import_module_binding (ns, id, mod, snum))
	    break;
	}
    }

  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* Write the entity table to MOD_SNAME_PFX.ent

   Each entry is a section number.  */

void
module_state::write_entities (elf_out *to, vec<depset *> depsets,
			      unsigned count, unsigned *crc_p)
{
  dump () && dump ("Writing entities");
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  unsigned current = 0;
  for (unsigned ix = 0; ix < depsets.length (); ix++)
    {
      depset *d = depsets[ix];

      switch (d->get_entity_kind ())
	{
	default:
	  break;

	case depset::EK_NAMESPACE:
	  if (!d->is_import () && d->get_entity () != global_namespace)
	    {
	      gcc_checking_assert (d->cluster == current);
	      current++;
	      sec.u (0);
	    }
	  break;

	case depset::EK_DECL:
	case depset::EK_SPECIALIZATION:
	case depset::EK_PARTIAL:
	  gcc_checking_assert (!d->is_unreached ()
			       && !d->is_import ()
			       && d->cluster == current
			       && d->section);
	  current++;
	  sec.u (d->section);
	  break;
	}
    }
  gcc_assert (count == current);
  sec.end (to, to->name (MOD_SNAME_PFX ".ent"), crc_p);
  dump.outdent ();
}

bool
module_state::read_entities (unsigned count, unsigned lwm, unsigned hwm)
{
  trees_in sec (this);

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".ent"))
    return false;

  dump () && dump ("Reading entities");
  dump.indent ();

  for (binding_slot *slot = entity_ary->begin () + entity_lwm; count--; slot++)
    {
      unsigned snum = sec.u ();
      if (snum && (snum - lwm) >= (hwm - lwm))
	sec.set_overrun ();
      if (sec.get_overrun ())
	break;

      if (snum)
	slot->set_lazy (snum << 2);
    }

  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* Write the pending table to MOD_SNAME_PFX.pnd

   The pending table holds information about clusters that need to be
   loaded because they contain information about something that is not
   found by namespace-scope lookup.

   The three cases are:

   (a) Template (maybe-partial) specializations that we have
   instantiated or defined.  When an importer needs to instantiate
   that template, they /must have/ the partial, explicit & extern
   specializations available.  If they have the other specializations
   available, they'll have less work to do.  Thus, when we're about to
   instantiate FOO, we have to be able to ask 'are there any
   specialization of FOO in our imports?'.

   (b) (Maybe-implicit) member functions definitions.  A class could
   be defined in one header, and an inline member defined in a
   different header (this occurs in the STL).  Similarly, like the
   specialization case, an implicit member function could have been
   'instantiated' in one module, and it'd be nice to not have to
   reinstantiate it in another.

   (c) Classes completed elsewhere.  A class could be declared in one
   header and defined in another.  We need to know to load the class
   definition before looking in it.  It does highlight an issue --
   there could be an intermediate import between the outermost containing
   namespace-scope class and the innermost being-defined class.  This is
   actually possible with all of these cases, so be aware -- we're not
   just talking of one level of import to get to the innermost namespace.

   This gets complicated fast, it took me multiple attempts to even
   get something remotely working.  Partially because I focussed on
   optimizing what I think turns out to be a smaller problem, given
   the known need to do the more general case *anyway*.  I document
   the smaller problem, because it does appear to be the natural way
   to do it.  It's trap!

   **** THE TRAP

   Let's refer to the primary template or the containing class as the
   KEY.  And the specialization or member as the PENDING-ENTITY.  (To
   avoid having to say those mouthfuls all the time.)

   In either case, we have an entity and we need some way of mapping
   that to a set of entities that need to be loaded before we can
   proceed with whatever processing of the entity we were going to do.

   We need to link the key to the pending-entity in some way.  Given a
   key, tell me the pending-entities I need to have loaded.  However
   we tie the key to the pending-entity must not rely on the key being
   loaded -- that'd defeat the lazy loading scheme.

   As the key will be an import in we know its entity number (either
   because we imported it, or we're writing it out too).  Thus we can
   generate a map of key-indices to pending-entities.  The
   pending-entity indices will be into our span of the entity table,
   and thus allow them to be lazily loaded.  The key index will be
   into another slot of the entity table.  Notice that this checking
   could be expensive, we don't want to iterate over a bunch of
   pending-entity indices (across multiple imports), every time we're
   about do to the thing with the key.  We need to quickly determine
   'definitely nothing needed'.

   That's almost good enough, except that key indices are not unique
   in a couple of cases :( Specifically the Global Module or a module
   partition can result in multiple modules assigning an entity index
   for the key.  The decl-merging on loading will detect that so we
   only have one Key loaded, and in the entity hash it'll indicate the
   entity index of first load.  Which might be different to how we
   know it.  Notice this is restricted to GM entities or this-module
   entities.  Foreign imports cannot have this.

   We can simply resolve this in the direction of how this module
   referred to the key to how the importer knows it.  Look in the
   entity table slot that we nominate, maybe lazy load it, and then
   lookup the resultant entity in the entity hash to learn how the
   importer knows it.

   But we need to go in the other direction :( Given the key, find all
   the index-aliases of that key.  We can partially solve that by
   adding an alias hash table.  Whenever we load a merged decl, add or
   augment a mapping from the entity (or its entity-index) to the
   newly-discovered index.  Then when we look for pending entities of
   a key, we also iterate over this aliases this mapping provides.

   But that requires the alias to be loaded.  And that's not
   necessarily true.

   *** THE SIMPLER WAY

   The remaining fixed thing we have is the innermost namespace
   containing the ultimate namespace-scope container of the key and
   the name of that container (which might be the key itself).  I.e. a
   namespace-decl/identifier/module tuple.  Let's call this the
   top-key.  We'll discover that the module is not important here,
   because of cross-module possibilities mentioned in case #c above.
   We can't markup namespace-binding slots.  The best we can do is
   mark the binding vector with 'there's something here', and have
   another map from namespace/identifier pairs to a vector of pending
   entity indices.

   Maintain a pending-entity map.  This is keyed by top-key, and
   maps to a vector of pending-entity indices.  On the binding vector
   have flags saying whether the pending-name-entity map has contents.
   (We might want to further extend the key to be GM-vs-Partition and
   specialization-vs-member, but let's not get ahead of ourselves.)

   For every key-like entity, find the outermost namespace-scope
   name.  Use that to lookup in the pending-entity map and then make
   sure the specified entities are loaded.

   An optimization might be to have a flag in each key-entity saying
   that its top key might be in the entity table.  It's not clear to
   me how to set that flag cheaply -- cheaper than just looking.

   FIXME: It'd be nice to have a bit in decls to tell us whether to
   even try this.  We can have a 'already done' flag, that we set when
   we've done KLASS's lazy pendings.  When we import a module that
   registers pendings on the same top-key as KLASS we need to clear
   the flag.  A recursive walk of the top-key clearing the bit will
   suffice.  Plus we only need to recurse on classes that have the bit
   set.  (That means we need to set the bit on parents of KLASS here,
   don't forget.)  However, first: correctness, second: efficiency.  */

unsigned
module_state::write_pendings (elf_out *to, vec<depset *> depsets,
			      depset::hash &table, unsigned *crc_p)
{
  dump () && dump ("Writing pending-entities");
  dump.indent ();

  trees_out sec (to, this, table);
  sec.begin ();

  unsigned count = 0;
  tree cache_ns = NULL_TREE;
  tree cache_id = NULL_TREE;
  unsigned cache_section = ~0;
  for (unsigned ix = 0; ix < depsets.length (); ix++)
    {
      depset *d = depsets[ix];

      if (d->is_binding ())
	continue;

      if (d->is_import ())
	continue;

      if (!d->is_pending_entity ())
	continue;

      tree key_decl = nullptr;
      tree key_ns = find_pending_key (d->get_entity (), &key_decl);
      tree key_name = DECL_NAME (key_decl);

      if (IDENTIFIER_ANON_P (key_name))
	{
	  gcc_checking_assert (IDENTIFIER_LAMBDA_P (key_name));
	  if (tree attached = LAMBDA_TYPE_EXTRA_SCOPE (TREE_TYPE (key_decl)))
	    key_name = DECL_NAME (attached);
	  else
	    {
	      /* There's nothing to attach it to.  Must
		 always reinstantiate.  */
	      dump ()
		&& dump ("Unattached lambda %N[%u] section:%u",
			 d->get_entity_kind () == depset::EK_DECL
			 ? "Member" : "Specialization", d->get_entity (),
			 d->cluster, d->section);
	      continue;
	    }
	}

      char const *also = "";
      if (d->section == cache_section
	  && key_ns == cache_ns
	  && key_name == cache_id)
	/* Same section & key as previous, no need to repeat ourselves.  */
	also = "also ";
      else
	{
	  cache_ns = key_ns;
	  cache_id = key_name;
	  cache_section = d->section;
	  gcc_checking_assert (table.find_dependency (cache_ns));
	  sec.tree_node (cache_ns);
	  sec.tree_node (cache_id);
	  sec.u (d->cluster);
	  count++;
	}
      dump () && dump ("Pending %s %N entity:%u section:%u %skeyed to %P",
		       d->get_entity_kind () == depset::EK_DECL
		       ? "member" : "specialization", d->get_entity (),
		       d->cluster, cache_section, also, cache_ns, cache_id);
      }
  sec.end (to, to->name (MOD_SNAME_PFX ".pnd"), crc_p);
  dump.outdent ();

  return count;
}

bool
module_state::read_pendings (unsigned count)
{
  trees_in sec (this);

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".pnd"))
    return false;

  dump () && dump ("Reading %u pendings", count);
  dump.indent ();

  for (unsigned ix = 0; ix != count; ix++)
    {
      pending_key key;
      unsigned index;

      key.ns = sec.tree_node ();
      key.id = sec.tree_node ();
      index = sec.u ();

      if (!key.ns || !key.id
	  || !(TREE_CODE (key.ns) == NAMESPACE_DECL
	       && !DECL_NAMESPACE_ALIAS (key.ns))
	  || !identifier_p (key.id)
	  || index >= entity_num)
	sec.set_overrun ();

      if (sec.get_overrun ())
	break;

      dump () && dump ("Pending:%u keyed to %P", index, key.ns, key.id);

      index += entity_lwm;
      auto &vec = pending_table->get_or_insert (key);
      vec.safe_push (index);
    }

  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* Read & write locations.  */
enum loc_kind {
  LK_ORDINARY,
  LK_MACRO,
  LK_IMPORT_ORDINARY,
  LK_IMPORT_MACRO,
  LK_ADHOC,
  LK_RESERVED,
};

static const module_state *
module_for_ordinary_loc (location_t loc)
{
  unsigned pos = 0;
  unsigned len = ool->length () - pos;

  while (len)
    {
      unsigned half = len / 2;
      module_state *probe = (*ool)[pos + half];
      if (loc < probe->ordinary_locs.first)
	len = half;
      else if (loc < probe->ordinary_locs.first + probe->ordinary_locs.second)
	return probe;
      else
	{
	  pos += half + 1;
	  len = len - (half + 1);
	}
    }

  return nullptr;
}

static const module_state *
module_for_macro_loc (location_t loc)
{
  unsigned pos = 1;
  unsigned len = modules->length () - pos;

  while (len)
    {
      unsigned half = len / 2;
      module_state *probe = (*modules)[pos + half];
      if (loc < probe->macro_locs.first)
	{
	  pos += half + 1;
	  len = len - (half + 1);
	}
      else if (loc >= probe->macro_locs.first + probe->macro_locs.second)
	len = half;
      else
	return probe;
    }

  return NULL;
}

location_t
module_state::imported_from () const
{
  location_t from = loc;
  line_map_ordinary const *fmap
    = linemap_check_ordinary (linemap_lookup (line_table, from));

  if (MAP_MODULE_P (fmap))
    from = linemap_included_from (fmap);

  return from;
}

/* Note that LOC will need writing.  This allows us to prune locations
   that are not needed.  */

bool
module_state::note_location (location_t loc)
{
  bool added = false;
  if (!macro_loc_table && !ord_loc_table)
    ;
  else if (loc < RESERVED_LOCATION_COUNT)
    ;
  else if (IS_ADHOC_LOC (loc))
    {
      location_t locus = get_location_from_adhoc_loc (line_table, loc);
      note_location (locus);
      source_range range = get_range_from_loc (line_table, loc);
      if (range.m_start != locus)
	note_location (range.m_start);
      note_location (range.m_finish);
    }
  else if (loc >= LINEMAPS_MACRO_LOWEST_LOCATION (line_table))
    {
      if (spans.macro (loc))
	{
	  const line_map *map = linemap_lookup (line_table, loc);
	  const line_map_macro *mac_map = linemap_check_macro (map);
	  hashval_t hv = macro_loc_traits::hash (mac_map);
	  macro_loc_info *slot
	    = macro_loc_table->find_slot_with_hash (mac_map, hv, INSERT);
	  if (!slot->src)
	    {
	      slot->src = mac_map;
	      slot->remap = 0;
	      // Expansion locations could themselves be from a
	      // macro, we need to note them all.
	      note_location (mac_map->m_expansion);
	      gcc_checking_assert (mac_map->n_tokens);
	      location_t tloc = UNKNOWN_LOCATION;
	      for (unsigned ix = mac_map->n_tokens * 2; ix--;)
		if (mac_map->macro_locations[ix] != tloc)
		  {
		    tloc = mac_map->macro_locations[ix];
		    note_location (tloc);
		  }
	      added = true;
	    }
	}
    }
  else if (IS_ORDINARY_LOC (loc))
    {
      if (spans.ordinary (loc))
	{
	  const line_map *map = linemap_lookup (line_table, loc);
	  const line_map_ordinary *ord_map = linemap_check_ordinary (map);
	  ord_loc_info lkup;
	  lkup.src = ord_map;
	  lkup.span = loc_one << ord_map->m_column_and_range_bits;
	  lkup.offset = (loc - MAP_START_LOCATION (ord_map)) & ~(lkup.span - 1);
	  lkup.remap = 0;
	  ord_loc_info *slot = (ord_loc_table->find_slot_with_hash
				(lkup, ord_loc_traits::hash (lkup), INSERT));
	  if (!slot->src)
	    {
	      *slot = lkup;
	      added = true;
	    }
	}
    }
  else
    gcc_unreachable ();
  return added;
}

/* If we're not streaming, record that we need location LOC.
   Otherwise stream it.  */

void
module_state::write_location (bytes_out &sec, location_t loc)
{
  if (!sec.streaming_p ())
    {
      note_location (loc);
      return;
    }

  if (loc < RESERVED_LOCATION_COUNT)
    {
      dump (dumper::LOCATION) && dump ("Reserved location %K", loc);
      sec.loc (LK_RESERVED + loc);
    }
  else if (IS_ADHOC_LOC (loc))
    {
      dump (dumper::LOCATION) && dump ("Adhoc location");
      sec.u (LK_ADHOC);
      location_t locus = get_location_from_adhoc_loc (line_table, loc);
      write_location (sec, locus);
      source_range range = get_range_from_loc (line_table, loc);
      if (range.m_start == locus)
	/* Compress.  */
	range.m_start = UNKNOWN_LOCATION;
      write_location (sec, range.m_start);
      write_location (sec, range.m_finish);
      unsigned discriminator = get_discriminator_from_adhoc_loc (line_table, loc);
      sec.u (discriminator);
    }
  else if (loc >= LINEMAPS_MACRO_LOWEST_LOCATION (line_table))
    {
      const macro_loc_info *info = nullptr;
      line_map_uint_t offset = 0;
      if (unsigned hwm = macro_loc_remap->length ())
	{
	  info = macro_loc_remap->begin ();
	  while (hwm != 1)
	    {
	      unsigned mid = hwm / 2;
	      if (MAP_START_LOCATION (info[mid].src) <= loc)
		{
		  info += mid;
		  hwm -= mid;
		}
	      else
		hwm = mid;
	    }
	  offset = loc - MAP_START_LOCATION (info->src);
	  if (offset > info->src->n_tokens)
	    info = nullptr;
	}

      gcc_checking_assert (bool (info) == bool (spans.macro (loc)));

      if (info)
	{
	  offset += info->remap;
	  sec.u (LK_MACRO);
	  sec.loc (offset);
	  dump (dumper::LOCATION)
	    && dump ("Macro location %K output %K", loc, offset);
	}
      else if (const module_state *import = module_for_macro_loc (loc))
	{
	  auto off = loc - import->macro_locs.first;
	  sec.u (LK_IMPORT_MACRO);
	  sec.u (import->remap);
	  sec.loc (off);
	  dump (dumper::LOCATION)
	    && dump ("Imported macro location %K output %u:%K",
		     loc, import->remap, off);
	}
      else
	gcc_unreachable ();
    }
  else if (IS_ORDINARY_LOC (loc))
    {
      /* If we ran out of locations for imported decls, this location could
	 be a module unit's location.  In that case, remap the location
	 to be where we imported the module from.  */
      if (spans.locations_exhausted_p () || CHECKING_P)
	{
	  const line_map_ordinary *map
	    = linemap_check_ordinary (linemap_lookup (line_table, loc));
	  if (MAP_MODULE_P (map) && loc == MAP_START_LOCATION (map))
	    {
	      gcc_checking_assert (spans.locations_exhausted_p ());
	      write_location (sec, linemap_included_from (map));
	      return;
	    }
	}

      const ord_loc_info *info = nullptr;
      line_map_uint_t offset = 0;
      if (line_map_uint_t hwm = ord_loc_remap->length ())
	{
	  info = ord_loc_remap->begin ();
	  while (hwm != 1)
	    {
	      auto mid = hwm / 2;
	      if (MAP_START_LOCATION (info[mid].src) + info[mid].offset <= loc)
		{
		  info += mid;
		  hwm -= mid;
		}
	      else
		hwm = mid;
	    }
	  offset = loc - MAP_START_LOCATION (info->src) - info->offset;
	  if (offset > info->span)
	    info = nullptr;
	}

      gcc_checking_assert (bool (info) == bool (spans.ordinary (loc)));

      if (info)
	{
	  offset += info->remap;
	  sec.u (LK_ORDINARY);
	  sec.loc (offset);

	  dump (dumper::LOCATION)
	    && dump ("Ordinary location %K output %K", loc, offset);
	}
      else if (const module_state *import = module_for_ordinary_loc (loc))
	{
	  auto off = loc - import->ordinary_locs.first;
	  sec.u (LK_IMPORT_ORDINARY);
	  sec.u (import->remap);
	  sec.loc (off);
	  dump (dumper::LOCATION)
	    && dump ("Imported ordinary location %K output %u:%K",
		     loc, import->remap, off);
	}
      else
	gcc_unreachable ();
    }
  else
    gcc_unreachable ();
}

location_t
module_state::read_location (bytes_in &sec) const
{
  location_t locus = UNKNOWN_LOCATION;
  unsigned kind = sec.u ();
  switch (kind)
     {
    default:
      {
	if (kind < LK_RESERVED + RESERVED_LOCATION_COUNT)
	  locus = location_t (kind - LK_RESERVED);
	else
	  sec.set_overrun ();
	dump (dumper::LOCATION)
	  && dump ("Reserved location %K", locus);
      }
      break;

     case LK_ADHOC:
      {
	dump (dumper::LOCATION) && dump ("Adhoc location");
	locus = read_location (sec);
	source_range range;
	range.m_start = read_location (sec);
	if (range.m_start == UNKNOWN_LOCATION)
	  range.m_start = locus;
	range.m_finish = read_location (sec);
	unsigned discriminator = sec.u ();
	if (locus != loc && range.m_start != loc && range.m_finish != loc)
	  locus = line_table->get_or_create_combined_loc (locus, range,
							  nullptr, discriminator);
      }
      break;

    case LK_MACRO:
      {
	auto off = sec.loc ();

	if (macro_locs.second)
	  {
	    if (off < macro_locs.second)
	      locus = off + macro_locs.first;
	    else
	      sec.set_overrun ();
	  }
	else
	  locus = loc;
	dump (dumper::LOCATION)
	  && dump ("Macro %K becoming %K", off, locus);
      }
      break;

    case LK_ORDINARY:
      {
	auto off = sec.loc ();
	if (ordinary_locs.second)
	  {
	    if (off < ordinary_locs.second)
	      locus = off + ordinary_locs.first;
	    else
	      sec.set_overrun ();
	  }
	else
	  locus = loc;

	dump (dumper::LOCATION)
	  && dump ("Ordinary location %K becoming %K", off, locus);
      }
      break;

     case LK_IMPORT_MACRO:
     case LK_IMPORT_ORDINARY:
       {
	 unsigned mod = sec.u ();
	 location_t off = sec.loc ();
	 const module_state *import = NULL;

	 if (!mod && !slurp->remap)
	   /* This is an early read of a partition location during the
	      read of our ordinary location map.  */
	   import = this;
	 else
	   {
	     mod = slurp->remap_module (mod);
	     if (!mod)
	       sec.set_overrun ();
	     else
	       import = (*modules)[mod];
	   }

	 if (import)
	   {
	     if (kind == LK_IMPORT_MACRO)
	       {
		 if (!import->macro_locs.second)
		   locus = import->loc;
		 else if (off < import->macro_locs.second)
		   locus = off + import->macro_locs.first;
		 else
		   sec.set_overrun ();
	       }
	     else
	       {
		 if (!import->ordinary_locs.second)
		   locus = import->loc;
		 else if (off < import->ordinary_locs.second)
		   locus = import->ordinary_locs.first + off;
		 else
		   sec.set_overrun ();
	       }
	   }
       }
       break;
    }

  return locus;
}

/* Allocate hash tables to record needed locations.  */

void
module_state::write_init_maps ()
{
  macro_loc_table = new hash_table<macro_loc_traits> (EXPERIMENT (1, 400));
  ord_loc_table = new hash_table<ord_loc_traits> (EXPERIMENT (1, 400));
}

/* Prepare the span adjustments.  We prune unneeded locations -- at
   this point every needed location must have been seen by
   note_location.  */

range_t
module_state::write_prepare_maps (module_state_config *cfg, bool has_partitions)
{
  dump () && dump ("Preparing locations");
  dump.indent ();

  dump () && dump ("Reserved locations [%K,%K) macro [%K,%K)",
		   spans[loc_spans::SPAN_RESERVED].ordinary.first,
		   spans[loc_spans::SPAN_RESERVED].ordinary.second,
		   spans[loc_spans::SPAN_RESERVED].macro.first,
		   spans[loc_spans::SPAN_RESERVED].macro.second);

  range_t info {0, 0};

  // Sort the noted lines.
  vec_alloc (ord_loc_remap, ord_loc_table->size ());
  for (auto iter = ord_loc_table->begin (), end = ord_loc_table->end ();
       iter != end; ++iter)
    ord_loc_remap->quick_push (*iter);
  ord_loc_remap->qsort (&ord_loc_info::compare);

  // Note included-from maps.
  bool added = false;
  const line_map_ordinary *current = nullptr;
  for (auto iter = ord_loc_remap->begin (), end = ord_loc_remap->end ();
       iter != end; ++iter)
    if (iter->src != current)
      {
	current = iter->src;
	for (auto probe = current;
	     auto from = linemap_included_from (probe);
	     probe = linemap_check_ordinary (linemap_lookup (line_table, from)))
	  {
	    if (has_partitions)
	      {
		// Partition locations need to elide their module map
		// entry.
		probe
		  = linemap_check_ordinary (linemap_lookup (line_table, from));
		if (MAP_MODULE_P (probe))
		  from = linemap_included_from (probe);
	      }

	    if (!note_location (from))
	      break;
	    added = true;
	  }
      }
  if (added)
    {
      // Reconstruct the line array as we added items to the hash table.
      vec_free (ord_loc_remap);
      vec_alloc (ord_loc_remap, ord_loc_table->size ());
      for (auto iter = ord_loc_table->begin (), end = ord_loc_table->end ();
	   iter != end; ++iter)
	ord_loc_remap->quick_push (*iter);
      ord_loc_remap->qsort (&ord_loc_info::compare);
    }
  delete ord_loc_table;
  ord_loc_table = nullptr;

  // Merge (sufficiently) adjacent spans, and calculate remapping.
  constexpr line_map_uint_t adjacency = 2; // Allow 2 missing lines.
  auto begin = ord_loc_remap->begin (), end = ord_loc_remap->end ();
  auto dst = begin;
  line_map_uint_t offset = 0;
  unsigned range_bits = 0;
  ord_loc_info *base = nullptr;
  for (auto iter = begin; iter != end; ++iter)
    {
      if (base && iter->src == base->src)
	{
	  if (base->offset + base->span +
	      ((adjacency << base->src->m_column_and_range_bits)
	       // If there are few c&r bits, allow further separation.
	       | (adjacency << 4))
	      >= iter->offset)
	    {
	      // Merge.
	      offset -= base->span;
	      base->span = iter->offset + iter->span - base->offset;
	      offset += base->span;
	      continue;
	    }
	}
      else if (range_bits < iter->src->m_range_bits)
	range_bits = iter->src->m_range_bits;

      offset += ((loc_one << iter->src->m_range_bits) - 1);
      offset &= ~((loc_one << iter->src->m_range_bits) - 1);
      iter->remap = offset;
      offset += iter->span;
      base = dst;
      *dst++ = *iter;
    }
  ord_loc_remap->truncate (dst - begin);

  info.first = ord_loc_remap->length ();
  cfg->ordinary_locs = offset;
  cfg->loc_range_bits = range_bits;
  dump () && dump ("Ordinary maps:%K locs:%K range_bits:%u",
		   info.first,
		   cfg->ordinary_locs,
		   cfg->loc_range_bits);

  // Remap the macro locations.
  vec_alloc (macro_loc_remap, macro_loc_table->size ());
  for (auto iter = macro_loc_table->begin (), end = macro_loc_table->end ();
       iter != end; ++iter)
    macro_loc_remap->quick_push (*iter);
  delete macro_loc_table;
  macro_loc_table = nullptr;

  macro_loc_remap->qsort (&macro_loc_info::compare);
  offset = 0;
  for (auto iter = macro_loc_remap->begin (), end = macro_loc_remap->end ();
       iter != end; ++iter)
    {
      auto mac = iter->src;
      iter->remap = offset;
      offset += mac->n_tokens;
    }
  info.second = macro_loc_remap->length ();
  cfg->macro_locs = offset;

  dump () && dump ("Macro maps:%K locs:%K", info.second, cfg->macro_locs);

  dump.outdent ();

  // If we have no ordinary locs, we must also have no macro locs.
  gcc_checking_assert (cfg->ordinary_locs || !cfg->macro_locs);

  return info;
}

bool
module_state::read_prepare_maps (const module_state_config *cfg)
{
  location_t ordinary = line_table->highest_location + 1;
  ordinary += cfg->ordinary_locs;

  location_t macro = LINEMAPS_MACRO_LOWEST_LOCATION (line_table);
  macro -= cfg->macro_locs;

  if (ordinary < LINE_MAP_MAX_LOCATION_WITH_COLS
      && macro >= LINE_MAP_MAX_LOCATION)
    /* OK, we have enough locations.  */
    return true;

  ordinary_locs.first = ordinary_locs.second = 0;
  macro_locs.first = macro_locs.second = 0;

  spans.report_location_exhaustion (loc);

  return false;
}

/* Write & read the location maps. Not called if there are no
   locations.   */

void
module_state::write_ordinary_maps (elf_out *to, range_t &info,
				   bool has_partitions, unsigned *crc_p)
{
  dump () && dump ("Writing ordinary location maps");
  dump.indent ();

  vec<const char *> filenames;
  filenames.create (20);

  /* Determine the unique filenames.  */
  const line_map_ordinary *current = nullptr;
  for (auto iter = ord_loc_remap->begin (), end = ord_loc_remap->end ();
       iter != end; ++iter)
    if (iter->src != current)
      {
	current = iter->src;
	const char *fname = ORDINARY_MAP_FILE_NAME (iter->src);

	/* We should never find a module linemap in an interval.  */
	gcc_checking_assert (!MAP_MODULE_P (iter->src));

	/* We expect very few filenames, so just an array.
	   (Not true when headers are still in play :()  */
	for (unsigned jx = filenames.length (); jx--;)
	  {
	    const char *name = filenames[jx];
	    if (0 == strcmp (name, fname))
	      {
		/* Reset the linemap's name, because for things like
		   preprocessed input we could have multiple instances
		   of the same name, and we'd rather not percolate
		   that.  */
		const_cast<line_map_ordinary *> (iter->src)->to_file = name;
		fname = NULL;
		break;
	      }
	  }
	if (fname)
	  filenames.safe_push (fname);
      }

  bytes_out sec (to);
  sec.begin ();

  /* Write the filenames.  */
  unsigned len = filenames.length ();
  sec.u (len);
  dump () && dump ("%u source file names", len);
  for (unsigned ix = 0; ix != len; ix++)
    {
      const char *fname = filenames[ix];
      dump (dumper::LOCATION) && dump ("Source file[%u]=%s", ix, fname);
      sec.str (fname);
    }

  sec.loc (info.first);	/* Num maps.  */
  const ord_loc_info *base = nullptr;
  for (auto iter = ord_loc_remap->begin (), end = ord_loc_remap->end ();
       iter != end; ++iter)
    {
      dump (dumper::LOCATION)
	&& dump ("Span:%K ordinary [%K+%K,+%K)->[%K,+%K)",
		 (location_t) (iter - ord_loc_remap->begin ()),
		 MAP_START_LOCATION (iter->src),
		 iter->offset, iter->span, iter->remap,
		 iter->span);

      if (!base || iter->src != base->src)
	base = iter;
      sec.loc (iter->offset - base->offset);
      if (base == iter)
	{
	  sec.u (iter->src->sysp);
	  sec.u (iter->src->m_range_bits);
	  sec.u (iter->src->m_column_and_range_bits - iter->src->m_range_bits);

	  const char *fname = ORDINARY_MAP_FILE_NAME (iter->src);
	  for (unsigned ix = 0; ix != filenames.length (); ix++)
	    if (filenames[ix] == fname)
	      {
		sec.u (ix);
		break;
	      }
	  unsigned line = ORDINARY_MAP_STARTING_LINE_NUMBER (iter->src);
	  line += iter->offset >> iter->src->m_column_and_range_bits;
	  sec.u (line);
	}
      sec.loc (iter->remap);
      if (base == iter)
	{
	  /* Write the included from location, which means reading it
	     while reading in the ordinary maps.  So we'd better not
	     be getting ahead of ourselves.  */
	  location_t from = linemap_included_from (iter->src);
	  gcc_checking_assert (from < MAP_START_LOCATION (iter->src));
	  if (from != UNKNOWN_LOCATION && has_partitions)
	    {
	      /* A partition's span will have a from pointing at a
		 MODULE_INC.  Find that map's from.  */
	      line_map_ordinary const *fmap
		= linemap_check_ordinary (linemap_lookup (line_table, from));
	      if (MAP_MODULE_P (fmap))
		from = linemap_included_from (fmap);
	    }
	  write_location (sec, from);
	}
    }

  filenames.release ();

  sec.end (to, to->name (MOD_SNAME_PFX ".olm"), crc_p);
  dump.outdent ();
}

void
module_state::write_macro_maps (elf_out *to, range_t &info, unsigned *crc_p)
{
  dump () && dump ("Writing macro location maps");
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  dump () && dump ("Macro maps:%K", info.second);
  sec.loc (info.second);

  line_map_uint_t macro_num = 0;
  for (auto iter = macro_loc_remap->end (), begin = macro_loc_remap->begin ();
       iter-- != begin;)
    {
      auto mac = iter->src;
      sec.loc (iter->remap);
      sec.u (mac->n_tokens);
      sec.cpp_node (mac->macro);
      write_location (sec, mac->m_expansion);
      const location_t *locs = mac->macro_locations;
      /* There are lots of identical runs.  */
      location_t prev = UNKNOWN_LOCATION;
      unsigned count = 0;
      unsigned runs = 0;
      for (unsigned jx = mac->n_tokens * 2; jx--;)
	{
	  location_t tok_loc = locs[jx];
	  if (tok_loc == prev)
	    {
	      count++;
	      continue;
	    }
	  runs++;
	  sec.u (count);
	  count = 1;
	  prev = tok_loc;
	  write_location (sec, tok_loc);
	}
      sec.u (count);
      dump (dumper::LOCATION)
	&& dump ("Macro:%K %I %u/%u*2 locations [%K,%K)->%K",
		 macro_num, identifier (mac->macro),
		 runs, mac->n_tokens,
		 MAP_START_LOCATION (mac),
		 MAP_START_LOCATION (mac) + mac->n_tokens,
		 iter->remap);
      macro_num++;
    }
  gcc_assert (macro_num == info.second);

  sec.end (to, to->name (MOD_SNAME_PFX ".mlm"), crc_p);
  dump.outdent ();
}

bool
module_state::read_ordinary_maps (unsigned num_ord_locs, unsigned range_bits)
{
  bytes_in sec;

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".olm"))
    return false;
  dump () && dump ("Reading ordinary location maps");
  dump.indent ();

  /* Read the filename table.  */
  unsigned len = sec.u ();
  dump () && dump ("%u source file names", len);
  vec<const char *> filenames;
  filenames.create (len);
  for (unsigned ix = 0; ix != len; ix++)
    {
      size_t l;
      const char *buf = sec.str (&l);
      char *fname = XNEWVEC (char, l + 1);
      memcpy (fname, buf, l + 1);
      dump (dumper::LOCATION) && dump ("Source file[%u]=%s", ix, fname);
      /* We leak these names into the line-map table.  But it
	 doesn't own them.  */
      filenames.quick_push (fname);
    }

  line_map_uint_t num_ordinary = sec.loc ();
  dump () && dump ("Ordinary maps:%K, range_bits:%u",
		   num_ordinary, range_bits);

  location_t offset = line_table->highest_location + 1;
  offset += ((loc_one << range_bits) - 1);
  offset &= ~((loc_one << range_bits) - 1);
  ordinary_locs.first = offset;

  bool propagated = spans.maybe_propagate (this, offset);
  line_map_ordinary *maps = static_cast<line_map_ordinary *>
    (line_map_new_raw (line_table, false, num_ordinary));

  const line_map_ordinary *base = nullptr;
  for (line_map_uint_t ix = 0; ix != num_ordinary && !sec.get_overrun (); ix++)
    {
      line_map_ordinary *map = &maps[ix];

      location_t offset = sec.loc ();
      if (!offset)
	{
	  map->reason = LC_RENAME;
	  map->sysp = sec.u ();
	  map->m_range_bits = sec.u ();
	  map->m_column_and_range_bits = sec.u () + map->m_range_bits;
	  unsigned fnum = sec.u ();
	  map->to_file = (fnum < filenames.length () ? filenames[fnum] : "");
	  map->to_line = sec.u ();
	  base = map;
	}
      else
	{
	  *map = *base;
	  map->to_line += offset >> map->m_column_and_range_bits;
	}
      location_t remap = sec.loc ();
      map->start_location = remap + ordinary_locs.first;
      if (base == map)
	{
	  /* Root the outermost map at our location.  */
	  ordinary_locs.second = remap;
	  location_t from = read_location (sec);
	  map->included_from = from != UNKNOWN_LOCATION ? from : loc;
	}
    }

  ordinary_locs.second = num_ord_locs;
  /* highest_location is the one handed out, not the next one to
     hand out.  */
  line_table->highest_location = ordinary_locs.first + ordinary_locs.second - 1;

  if (line_table->highest_location >= LINE_MAP_MAX_LOCATION_WITH_COLS)
    /* We shouldn't run out of locations, as we checked before
       starting.  */
    sec.set_overrun ();
  dump () && dump ("Ordinary location [%K,+%K)",
		   ordinary_locs.first, ordinary_locs.second);

  if (propagated)
    spans.close ();

  filenames.release ();

  dump.outdent ();
  if (!sec.end (from ()))
    return false;

  return true;
}

bool
module_state::read_macro_maps (line_map_uint_t num_macro_locs)
{
  bytes_in sec;

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".mlm"))
    return false;
  dump () && dump ("Reading macro location maps");
  dump.indent ();

  line_map_uint_t num_macros = sec.loc ();
  dump () && dump ("Macro maps:%K locs:%K",
		   num_macros, num_macro_locs);

  bool propagated = spans.maybe_propagate (this,
					   line_table->highest_location + 1);

  location_t offset = LINEMAPS_MACRO_LOWEST_LOCATION (line_table);
  macro_locs.second = num_macro_locs;
  macro_locs.first = offset - num_macro_locs;

  dump () && dump ("Macro loc delta %K", offset);
  dump () && dump ("Macro locations [%K,%K)",
		   macro_locs.first, macro_locs.second);

  for (line_map_uint_t ix = 0; ix != num_macros && !sec.get_overrun (); ix++)
    {
      location_t offset = sec.loc ();
      unsigned n_tokens = sec.u ();
      cpp_hashnode *node = sec.cpp_node ();
      location_t exp_loc = read_location (sec);

      const line_map_macro *macro
	= linemap_enter_macro (line_table, node, exp_loc, n_tokens);
      if (!macro)
	/* We shouldn't run out of locations, as we checked that we
	   had enough before starting.  */
	break;
      gcc_checking_assert (MAP_START_LOCATION (macro)
			   == offset + macro_locs.first);

      location_t *locs = macro->macro_locations;
      location_t tok_loc = UNKNOWN_LOCATION;
      unsigned count = sec.u ();
      unsigned runs = 0;
      for (unsigned jx = macro->n_tokens * 2; jx-- && !sec.get_overrun ();)
	{
	  while (!count-- && !sec.get_overrun ())
	    {
	      runs++;
	      tok_loc = read_location (sec);
	      count = sec.u ();
	    }
	  locs[jx] = tok_loc;
	}
      if (count)
	sec.set_overrun ();
      dump (dumper::LOCATION)
	&& dump ("Macro:%K %I %u/%u*2 locations [%K,%K)",
		 ix, identifier (node), runs, n_tokens,
		 MAP_START_LOCATION (macro),
		 MAP_START_LOCATION (macro) + n_tokens);
    }

  dump () && dump ("Macro location lwm:%K", macro_locs.first);
  if (propagated)
    spans.close ();

  dump.outdent ();
  if (!sec.end (from ()))
    return false;

  return true;
}

/* Serialize the definition of MACRO.  */

void
module_state::write_define (bytes_out &sec, const cpp_macro *macro)
{
  sec.u (macro->count);

  bytes_out::bits_out bits = sec.stream_bits ();
  bits.b (macro->fun_like);
  bits.b (macro->variadic);
  bits.b (macro->syshdr);
  bits.bflush ();

  write_location (sec, macro->line);
  if (macro->fun_like)
    {
      sec.u (macro->paramc);
      const cpp_hashnode *const *parms = macro->parm.params;
      for (unsigned ix = 0; ix != macro->paramc; ix++)
	sec.cpp_node (parms[ix]);
    }

  unsigned len = 0;
  for (unsigned ix = 0; ix != macro->count; ix++)
    {
      const cpp_token *token = &macro->exp.tokens[ix];
      write_location (sec, token->src_loc);
      sec.u (token->type);
      sec.u (token->flags);
      switch (cpp_token_val_index (token))
	{
	default:
	  gcc_unreachable ();

	case CPP_TOKEN_FLD_ARG_NO:
	  /* An argument reference.  */
	  sec.u (token->val.macro_arg.arg_no);
	  sec.cpp_node (token->val.macro_arg.spelling);
	  break;

	case CPP_TOKEN_FLD_NODE:
	  /* An identifier.  */
	  sec.cpp_node (token->val.node.node);
	  if (token->val.node.spelling == token->val.node.node)
	    /* The spelling will usually be the same.  so optimize
	       that.  */
	    sec.str (NULL, 0);
	  else
	    sec.cpp_node (token->val.node.spelling);
	  break;

	case CPP_TOKEN_FLD_NONE:
	  break;

	case CPP_TOKEN_FLD_STR:
	  /* A string, number or comment.  Not always NUL terminated,
	     we stream out in a single contatenation with embedded
	     NULs as that's a safe default.  */
	  len += token->val.str.len + 1;
	  sec.u (token->val.str.len);
	  break;

	case CPP_TOKEN_FLD_SOURCE:
	case CPP_TOKEN_FLD_TOKEN_NO:
	case CPP_TOKEN_FLD_PRAGMA:
	  /* These do not occur inside a macro itself.  */
	  gcc_unreachable ();
	}
    }

  if (len)
    {
      char *ptr = reinterpret_cast<char *> (sec.buf (len));
      len = 0;
      for (unsigned ix = 0; ix != macro->count; ix++)
	{
	  const cpp_token *token = &macro->exp.tokens[ix];
	  if (cpp_token_val_index (token) == CPP_TOKEN_FLD_STR)
	    {
	      memcpy (ptr + len, token->val.str.text,
		      token->val.str.len);
	      len += token->val.str.len;
	      ptr[len++] = 0;
	    }
	}
    }
}

/* Read a macro definition.  */

cpp_macro *
module_state::read_define (bytes_in &sec, cpp_reader *reader) const
{
  unsigned count = sec.u ();
  /* We rely on knowing cpp_reader's hash table is ident_hash, and
     its subobject allocator is stringpool_ggc_alloc and that is just
     a wrapper for ggc_alloc_atomic.  */
  cpp_macro *macro
    = (cpp_macro *)ggc_alloc_atomic (sizeof (cpp_macro)
				     + sizeof (cpp_token) * (count - !!count));
  memset (macro, 0, sizeof (cpp_macro) + sizeof (cpp_token) * (count - !!count));

  macro->count = count;
  macro->kind = cmk_macro;
  macro->imported_p = true;

  bytes_in::bits_in bits = sec.stream_bits ();
  macro->fun_like = bits.b ();
  macro->variadic = bits.b ();
  macro->syshdr = bits.b ();
  bits.bflush ();

  macro->line = read_location (sec);

  if (macro->fun_like)
    {
      unsigned paramc = sec.u ();
      cpp_hashnode **params
	= (cpp_hashnode **)ggc_alloc_atomic (sizeof (cpp_hashnode *) * paramc);
      macro->paramc = paramc;
      macro->parm.params = params;
      for (unsigned ix = 0; ix != paramc; ix++)
	params[ix] = sec.cpp_node ();
    }

  unsigned len = 0;
  for (unsigned ix = 0; ix != count && !sec.get_overrun (); ix++)
    {
      cpp_token *token = &macro->exp.tokens[ix];
      token->src_loc = read_location (sec);
      token->type = cpp_ttype (sec.u ());
      token->flags = sec.u ();
      switch (cpp_token_val_index (token))
	{
	default:
	  sec.set_overrun ();
	  break;

	case CPP_TOKEN_FLD_ARG_NO:
	  /* An argument reference.  */
	  {
	    unsigned arg_no = sec.u ();
	    if (arg_no - 1 >= macro->paramc)
	      sec.set_overrun ();
	    token->val.macro_arg.arg_no = arg_no;
	    token->val.macro_arg.spelling = sec.cpp_node ();
	  }
	  break;

	case CPP_TOKEN_FLD_NODE:
	  /* An identifier.  */
	  token->val.node.node = sec.cpp_node ();
	  token->val.node.spelling = sec.cpp_node ();
	  if (!token->val.node.spelling)
	    token->val.node.spelling = token->val.node.node;
	  break;

	case CPP_TOKEN_FLD_NONE:
	  break;

	case CPP_TOKEN_FLD_STR:
	  /* A string, number or comment.  */
	  token->val.str.len = sec.u ();
	  len += token->val.str.len + 1;
	  break;
	}
    }

  if (len)
    if (const char *ptr = reinterpret_cast<const char *> (sec.buf (len)))
      {
	/* There should be a final NUL.  */
	if (ptr[len-1])
	  sec.set_overrun ();
	/* cpp_alloc_token_string will add a final NUL.  */
	const unsigned char *buf
	  = cpp_alloc_token_string (reader, (const unsigned char *)ptr, len - 1);
	len = 0;
	for (unsigned ix = 0; ix != count && !sec.get_overrun (); ix++)
	  {
	    cpp_token *token = &macro->exp.tokens[ix];
	    if (cpp_token_val_index (token) == CPP_TOKEN_FLD_STR)
	      {
		token->val.str.text = buf + len;
		len += token->val.str.len;
		if (buf[len++])
		  sec.set_overrun ();
	      }
	  }
      }

  if (sec.get_overrun ())
    return NULL;
  return macro;
}

/* Exported macro data.  */
struct GTY(()) macro_export {
  cpp_macro *def;
  location_t undef_loc;

  macro_export ()
    :def (NULL), undef_loc (UNKNOWN_LOCATION)
  {
  }
};

/* Imported macro data.  */
class macro_import {
public:
  struct slot {
#if defined (WORDS_BIGENDIAN) && SIZEOF_VOID_P == 8
    int offset;
#endif
    /* We need to ensure we don't use the LSB for representation, as
       that's the union discriminator below.  */
    unsigned bits;

#if !(defined (WORDS_BIGENDIAN) && SIZEOF_VOID_P == 8)
    int offset;
#endif

  public:
    enum Layout {
      L_DEF = 1,
      L_UNDEF = 2,
      L_BOTH = 3,
      L_MODULE_SHIFT = 2
    };

  public:
    /* Not a regular ctor, because we put it in a union, and that's
       not allowed in C++ 98.  */
    static slot ctor (unsigned module, unsigned defness)
    {
      gcc_checking_assert (defness);
      slot s;
      s.bits = defness | (module << L_MODULE_SHIFT);
      s.offset = -1;
      return s;
    }

  public:
    unsigned get_defness () const
    {
      return bits & L_BOTH;
    }
    unsigned get_module () const
    {
      return bits >> L_MODULE_SHIFT;
    }
    void become_undef ()
    {
      bits &= ~unsigned (L_DEF);
      bits |= unsigned (L_UNDEF);
    }
  };

private:
  typedef vec<slot, va_heap, vl_embed> ary_t;
  union either {
    /* Discriminated by bits 0|1 != 0.  The expected case is that
       there will be exactly one slot per macro, hence the effort of
       packing that.  */
    ary_t *ary;
    slot single;
  } u;

public:
  macro_import ()
  {
    u.ary = NULL;
  }

private:
  bool single_p () const
  {
    return u.single.bits & slot::L_BOTH;
  }
  bool occupied_p () const
  {
    return u.ary != NULL;
  }

public:
  unsigned length () const
  {
    gcc_checking_assert (occupied_p ());
    return single_p () ? 1 : u.ary->length ();
  }
  slot &operator[] (unsigned ix)
  {
    gcc_checking_assert (occupied_p ());
    if (single_p ())
      {
	gcc_checking_assert (!ix);
	return u.single;
      }
    else
      return (*u.ary)[ix];
  }

public:
  slot &exported ();
  slot &append (unsigned module, unsigned defness);
};

/* O is a new import to append to the list for.  If we're an empty
   set, initialize us.  */

macro_import::slot &
macro_import::append (unsigned module, unsigned defness)
{
  if (!occupied_p ())
    {
      u.single = slot::ctor (module, defness);
      return u.single;
    }
  else
    {
      bool single = single_p ();
      ary_t *m = single ? NULL : u.ary;
      vec_safe_reserve (m, 1 + single);
      if (single)
	m->quick_push (u.single);
      u.ary = m;
      return *u.ary->quick_push (slot::ctor (module, defness));
    }
}

/* We're going to export something.  Make sure the first import slot
   is us.  */

macro_import::slot &
macro_import::exported ()
{
  if (occupied_p () && !(*this)[0].get_module ())
    {
      slot &res = (*this)[0];
      res.bits |= slot::L_DEF;
      return res;
    }

  slot *a = &append (0, slot::L_DEF);
  if (!single_p ())
    {
      slot &f = (*this)[0];
      std::swap (f, *a);
      a = &f;
    }
  return *a;
}

/* The import (&exported) macros.  cpp_hasnode's deferred field
   indexes this array (offset by 1, so zero means 'not present'.  */

static vec<macro_import, va_heap, vl_embed> *macro_imports;

/* The exported macros.  A macro_import slot's zeroth element's offset
   indexes this array.  If the zeroth slot is not for module zero,
   there is no export.  */

static GTY(()) vec<macro_export, va_gc> *macro_exports;

/* The reachable set of header imports from this TU.  */

static GTY(()) bitmap headers;

/* Get the (possibly empty) macro imports for NODE.  */

static macro_import &
get_macro_imports (cpp_hashnode *node)
{
  if (node->deferred)
    return (*macro_imports)[node->deferred - 1];

  vec_safe_reserve (macro_imports, 1);
  node->deferred = macro_imports->length () + 1;
  return *vec_safe_push (macro_imports, macro_import ());
}

/* Get the macro export for export EXP of NODE.  */

static macro_export &
get_macro_export (macro_import::slot &slot)
{
  if (slot.offset >= 0)
    return (*macro_exports)[slot.offset];

  vec_safe_reserve (macro_exports, 1);
  slot.offset = macro_exports->length ();
  return *macro_exports->quick_push (macro_export ());
}

/* If NODE is an exportable macro, add it to the export set.  */

static int
maybe_add_macro (cpp_reader *, cpp_hashnode *node, void *data_)
{
  bool exporting = false;

  if (cpp_user_macro_p (node))
    if (cpp_macro *macro = node->value.macro)
      /* Ignore imported, builtins, command line and forced header macros.  */
      if (!macro->imported_p
	  && !macro->lazy && macro->line >= spans.main_start ())
	{
	  gcc_checking_assert (macro->kind == cmk_macro);
	  /* I don't want to deal with this corner case, that I suspect is
	     a devil's advocate reading of the standard.  */
	  gcc_checking_assert (!macro->extra_tokens);

	  macro_import::slot &slot = get_macro_imports (node).exported ();
	  macro_export &exp = get_macro_export (slot);
	  exp.def = macro;
	  exporting = true;
	}

  if (!exporting && node->deferred)
    {
      macro_import &imports = (*macro_imports)[node->deferred - 1];
      macro_import::slot &slot = imports[0];
      if (!slot.get_module ())
	{
	  gcc_checking_assert (slot.get_defness ());
	  exporting = true;
	}
    }

  if (exporting)
    static_cast<vec<cpp_hashnode *> *> (data_)->safe_push (node);

  return 1; /* Don't stop.  */
}

/* Order cpp_hashnodes A_ and B_ by their exported macro locations.  */

static int
macro_loc_cmp (const void *a_, const void *b_)
{
  const cpp_hashnode *node_a = *(const cpp_hashnode *const *)a_;
  macro_import &import_a = (*macro_imports)[node_a->deferred - 1];
  const macro_export &export_a = (*macro_exports)[import_a[0].offset];
  location_t loc_a = export_a.def ? export_a.def->line : export_a.undef_loc;

  const cpp_hashnode *node_b = *(const cpp_hashnode *const *)b_;
  macro_import &import_b = (*macro_imports)[node_b->deferred - 1];
  const macro_export &export_b = (*macro_exports)[import_b[0].offset];
  location_t loc_b = export_b.def ? export_b.def->line : export_b.undef_loc;

  if (loc_a < loc_b)
    return +1;
  else if (loc_a > loc_b)
    return -1;
  else
    return 0;
}

/* Gather the macro definitions and undefinitions that we will need to
   write out.   */

vec<cpp_hashnode *> *
module_state::prepare_macros (cpp_reader *reader)
{
  vec<cpp_hashnode *> *macros;
  vec_alloc (macros, 100);

  cpp_forall_identifiers (reader, maybe_add_macro, macros);

  dump (dumper::MACRO) && dump ("No more than %u macros", macros->length ());

  macros->qsort (macro_loc_cmp);

  // Note the locations.
  for (unsigned ix = macros->length (); ix--;)
    {
      cpp_hashnode *node = (*macros)[ix];
      macro_import::slot &slot = (*macro_imports)[node->deferred - 1][0];
      macro_export &mac = (*macro_exports)[slot.offset];

      if (IDENTIFIER_KEYWORD_P (identifier (node)))
	continue;

      if (mac.undef_loc != UNKNOWN_LOCATION)
	note_location (mac.undef_loc);
      if (mac.def)
	{
	  note_location (mac.def->line);
	  for (unsigned ix = 0; ix != mac.def->count; ix++)
	    note_location (mac.def->exp.tokens[ix].src_loc);
	}
    }

  return macros;
}

/* Write out the exported defines.  This is two sections, one
   containing the definitions, the other a table of node names.  */

unsigned
module_state::write_macros (elf_out *to, vec<cpp_hashnode *> *macros,
			    unsigned *crc_p)
{
  dump () && dump ("Writing macros");
  dump.indent ();

  /* Write the defs */
  bytes_out sec (to);
  sec.begin ();

  unsigned count = 0;
  for (unsigned ix = macros->length (); ix--;)
    {
      cpp_hashnode *node = (*macros)[ix];
      macro_import::slot &slot = (*macro_imports)[node->deferred - 1][0];
      gcc_assert (!slot.get_module () && slot.get_defness ());

      macro_export &mac = (*macro_exports)[slot.offset];
      gcc_assert (!!(slot.get_defness () & macro_import::slot::L_UNDEF)
		  == (mac.undef_loc != UNKNOWN_LOCATION)
		  && !!(slot.get_defness () & macro_import::slot::L_DEF)
		  == (mac.def != NULL));

      if (IDENTIFIER_KEYWORD_P (identifier (node)))
	{
	  warning_at (mac.def->line, 0,
		      "not exporting %<#define %E%> as it is a keyword",
		      identifier (node));
	  slot.offset = 0;
	  continue;
	}

      count++;
      slot.offset = sec.pos;
      dump (dumper::MACRO)
	&& dump ("Writing macro %s%s%s %I at %u",
		 slot.get_defness () & macro_import::slot::L_UNDEF
		 ? "#undef" : "",
		 slot.get_defness () == macro_import::slot::L_BOTH
		 ? " & " : "",
		 slot.get_defness () & macro_import::slot::L_DEF
		 ? "#define" : "",
		 identifier (node), slot.offset);
      if (mac.undef_loc != UNKNOWN_LOCATION)
	write_location (sec, mac.undef_loc);
      if (mac.def)
	write_define (sec, mac.def);
    }
  if (count)
    // We may have ended on a tokenless macro with a very short
    // location, that will cause problems reading its bit flags.
    sec.u (0);
  sec.end (to, to->name (MOD_SNAME_PFX ".def"), crc_p);

  if (count)
    {
      /* Write the table.  */
      bytes_out sec (to);
      sec.begin ();
      sec.u (count);

      for (unsigned ix = macros->length (); ix--;)
	{
	  const cpp_hashnode *node = (*macros)[ix];
	  macro_import::slot &slot = (*macro_imports)[node->deferred - 1][0];

	  if (slot.offset)
	    {
	      sec.cpp_node (node);
	      sec.u (slot.get_defness ());
	      sec.u (slot.offset);
	    }
	}
      sec.end (to, to->name (MOD_SNAME_PFX ".mac"), crc_p);
    }

  dump.outdent ();
  return count;
}

bool
module_state::read_macros ()
{
  /* Get the def section.  */
  if (!slurp->macro_defs.begin (loc, from (), MOD_SNAME_PFX ".def"))
    return false;

  /* Get the tbl section, if there are defs. */
  if (slurp->macro_defs.more_p ()
      && !slurp->macro_tbl.begin (loc, from (), MOD_SNAME_PFX ".mac"))
    return false;

  return true;
}

/* Install the macro name table.  */

void
module_state::install_macros ()
{
  bytes_in &sec = slurp->macro_tbl;
  if (!sec.size)
    return;

  dump () && dump ("Reading macro table %M", this);
  dump.indent ();

  unsigned count = sec.u ();
  dump () && dump ("%u macros", count);
  while (count--)
    {
      cpp_hashnode *node = sec.cpp_node ();
      macro_import &imp = get_macro_imports (node);
      unsigned flags = sec.u () & macro_import::slot::L_BOTH;
      if (!flags)
	sec.set_overrun ();

      if (sec.get_overrun ())
	break;

      macro_import::slot &slot = imp.append (mod, flags);
      slot.offset = sec.u ();

      dump (dumper::MACRO)
	&& dump ("Read %s macro %s%s%s %I at %u",
		 imp.length () > 1 ? "add" : "new",
		 flags & macro_import::slot::L_UNDEF ? "#undef" : "",
		 flags == macro_import::slot::L_BOTH ? " & " : "",
		 flags & macro_import::slot::L_DEF ? "#define" : "",
		 identifier (node), slot.offset);

      /* We'll leak an imported definition's TOKEN_FLD_STR's data
	 here.  But that only happens when we've had to resolve the
	 deferred macro before this import -- why are you doing
	 that?  */
      if (cpp_macro *cur = cpp_set_deferred_macro (node))
	if (!cur->imported_p)
	  {
	    macro_import::slot &slot = imp.exported ();
	    macro_export &exp = get_macro_export (slot);
	    exp.def = cur;
	    dump (dumper::MACRO)
	      && dump ("Saving current #define %I", identifier (node));
	  }
    }

  /* We're now done with the table.  */
  elf_in::release (slurp->from, sec);

  dump.outdent ();
}

/* Import the transitive macros.  */

void
module_state::import_macros ()
{
  bitmap_ior_into (headers, slurp->headers);

  bitmap_iterator bititer;
  unsigned bitnum;
  EXECUTE_IF_SET_IN_BITMAP (slurp->headers, 0, bitnum, bititer)
    (*modules)[bitnum]->install_macros ();
}

/* NODE is being undefined at LOC.  Record it in the export table, if
   necessary.  */

void
module_state::undef_macro (cpp_reader *, location_t loc, cpp_hashnode *node)
{
  if (!node->deferred)
    /* The macro is not imported, so our undef is irrelevant.  */
    return;

  unsigned n = dump.push (NULL);

  macro_import::slot &slot = (*macro_imports)[node->deferred - 1].exported ();
  macro_export &exp = get_macro_export (slot);

  exp.undef_loc = loc;
  slot.become_undef ();
  exp.def = NULL;

  dump (dumper::MACRO) && dump ("Recording macro #undef %I", identifier (node));

  dump.pop (n);
}

/* NODE is a deferred macro node.  Determine the definition and return
   it, with NULL if undefined.  May issue diagnostics.

   This can leak memory, when merging declarations -- the string
   contents (TOKEN_FLD_STR) of each definition are allocated in
   unreclaimable cpp objstack.  Only one will win.  However, I do not
   expect this to be common -- mostly macros have a single point of
   definition.  Perhaps we could restore the objstack to its position
   after the first imported definition (if that wins)?  The macros
   themselves are GC'd.  */

cpp_macro *
module_state::deferred_macro (cpp_reader *reader, location_t loc,
			      cpp_hashnode *node)
{
  macro_import &imports = (*macro_imports)[node->deferred - 1];

  unsigned n = dump.push (NULL);
  dump (dumper::MACRO) && dump ("Deferred macro %I", identifier (node));

  bitmap visible (BITMAP_GGC_ALLOC ());

  if (!((imports[0].get_defness () & macro_import::slot::L_UNDEF)
	&& !imports[0].get_module ()))
    {
      /* Calculate the set of visible header imports.  */
      bitmap_copy (visible, headers);
      for (unsigned ix = imports.length (); ix--;)
	{
	  const macro_import::slot &slot = imports[ix];
	  unsigned mod = slot.get_module ();
	  if ((slot.get_defness () & macro_import::slot::L_UNDEF)
	      && bitmap_bit_p (visible, mod))
	    {
	      bitmap arg = mod ? (*modules)[mod]->slurp->headers : headers;
	      bitmap_and_compl_into (visible, arg);
	      bitmap_set_bit (visible, mod);
	    }
	}
    }
  bitmap_set_bit (visible, 0);

  /* Now find the macros that are still visible.  */
  bool failed = false;
  cpp_macro *def = NULL;
  vec<macro_export> defs;
  defs.create (imports.length ());
  for (unsigned ix = imports.length (); ix--;)
    {
      const macro_import::slot &slot = imports[ix];
      unsigned mod = slot.get_module ();
      if (bitmap_bit_p (visible, mod))
	{
	  macro_export *pushed = NULL;
	  if (mod)
	    {
	      const module_state *imp = (*modules)[mod];
	      bytes_in &sec = imp->slurp->macro_defs;
	      if (!sec.get_overrun ())
		{
		  dump (dumper::MACRO)
		    && dump ("Reading macro %s%s%s %I module %M at %u",
			     slot.get_defness () & macro_import::slot::L_UNDEF
			     ? "#undef" : "",
			     slot.get_defness () == macro_import::slot::L_BOTH
			     ? " & " : "",
			     slot.get_defness () & macro_import::slot::L_DEF
			     ? "#define" : "",
			     identifier (node), imp, slot.offset);
		  sec.random_access (slot.offset);

		  macro_export exp;
		  if (slot.get_defness () & macro_import::slot::L_UNDEF)
		    exp.undef_loc = imp->read_location (sec);
		  if (slot.get_defness () & macro_import::slot::L_DEF)
		    exp.def = imp->read_define (sec, reader);
		  if (sec.get_overrun ())
		    error_at (loc, "macro definitions of %qE corrupted",
			      imp->name);
		  else
		    pushed = defs.quick_push (exp);
		}
	    }
	  else
	    pushed = defs.quick_push ((*macro_exports)[slot.offset]);
	  if (pushed && pushed->def)
	    {
	      if (!def)
		def = pushed->def;
	      else if (cpp_compare_macros (def, pushed->def))
		failed = true;
	    }
	}
    }

  if (failed)
    {
      /* If LOC is the first loc, this is the end of file check, which
	 is a warning.  */
      auto_diagnostic_group d;
      if (loc == MAP_START_LOCATION (LINEMAPS_ORDINARY_MAP_AT (line_table, 0)))
	warning_at (loc, OPT_Winvalid_imported_macros,
		    "inconsistent imported macro definition %qE",
		    identifier (node));
      else
	error_at (loc, "inconsistent imported macro definition %qE",
		  identifier (node));
      for (unsigned ix = defs.length (); ix--;)
	{
	  macro_export &exp = defs[ix];
	  if (exp.undef_loc)
	    inform (exp.undef_loc, "%<#undef %E%>", identifier (node));
	  if (exp.def)
	    inform (exp.def->line, "%<#define %s%>",
		    cpp_macro_definition (reader, node, exp.def));
	}
      def = NULL;
    }

  defs.release ();

  dump.pop (n);

  return def;
}

/* Stream the static aggregates.  Sadly some headers (ahem:
   iostream) contain static vars, and rely on them to run global
   ctors.  */
unsigned
module_state::write_inits (elf_out *to, depset::hash &table, unsigned *crc_ptr)
{
  if (!static_aggregates && !tls_aggregates)
    return 0;

  dump () && dump ("Writing initializers");
  dump.indent ();

  static_aggregates = nreverse (static_aggregates);
  tls_aggregates = nreverse (tls_aggregates);

  unsigned count = 0;
  trees_out sec (to, this, table, ~0u);
  sec.begin ();

  tree list = static_aggregates;
  for (int passes = 0; passes != 2; passes++)
    {
      for (tree init = list; init; init = TREE_CHAIN (init))
	if (TREE_LANG_FLAG_0 (init))
	  {
	    tree decl = TREE_VALUE (init);

	    dump ("Initializer:%u for %N", count, decl);
	    sec.tree_node (decl);
	    ++count;
	  }

      list = tls_aggregates;
    }

  sec.end (to, to->name (MOD_SNAME_PFX ".ini"), crc_ptr);
  dump.outdent ();

  return count;
}

/* We have to defer some post-load processing until we've completed
   reading, because they can cause more reading.  */

static void
post_load_processing ()
{
  /* We mustn't cause a GC, our caller should have arranged for that
     not to happen.  */
  gcc_checking_assert (function_depth);

  if (!post_load_decls)
    return;

  tree old_cfd = current_function_decl;
  struct function *old_cfun = cfun;
  while (post_load_decls->length ())
    {
      tree decl = post_load_decls->pop ();

      dump () && dump ("Post-load processing of %N", decl);

      gcc_checking_assert (DECL_MAYBE_IN_CHARGE_CDTOR_P (decl));

      if (DECL_COMDAT (decl))
	comdat_linkage (decl);
      if (!TREE_ASM_WRITTEN (decl))
	{
	  /* Cloning can cause loading -- specifically operator delete for
	     the deleting dtor.  */
	  if (maybe_clone_body (decl))
	    TREE_ASM_WRITTEN (decl) = 1;
	  else
	    {
	      /* We didn't clone the cdtor, make sure we emit it.  */
	      note_vague_linkage_fn (decl);
	      cgraph_node::finalize_function (decl, true);
	    }
	}
    }

  cfun = old_cfun;
  current_function_decl = old_cfd;
}

bool
module_state::read_inits (unsigned count)
{
  trees_in sec (this);
  if (!sec.begin (loc, from (), from ()->find (MOD_SNAME_PFX ".ini")))
    return false;
  dump () && dump ("Reading %u initializers", count);
  dump.indent ();

  lazy_snum = ~0u;
  for (unsigned ix = 0; ix != count; ix++)
    {
      /* Merely referencing the decl causes its initializer to be read
	 and added to the correct list.  */
      tree decl = sec.tree_node ();

      if (sec.get_overrun ())
	break;
      if (decl)
	dump ("Initializer:%u for %N", count, decl);
    }
  lazy_snum = 0;
  post_load_processing ();
  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

void
module_state::write_counts (elf_out *to, unsigned counts[MSC_HWM],
			    unsigned *crc_ptr)
{
  bytes_out cfg (to);

  cfg.begin ();

  for (unsigned ix = MSC_HWM; ix--;)
    cfg.u (counts[ix]);

  if (dump ())
    {
      dump ("Cluster sections are [%u,%u)",
	    counts[MSC_sec_lwm], counts[MSC_sec_hwm]);
      dump ("Bindings %u", counts[MSC_bindings]);
      dump ("Pendings %u", counts[MSC_pendings]);
      dump ("Entities %u", counts[MSC_entities]);
      dump ("Namespaces %u", counts[MSC_namespaces]);
      dump ("Macros %u", counts[MSC_macros]);
      dump ("Initializers %u", counts[MSC_inits]);
    }

  cfg.end (to, to->name (MOD_SNAME_PFX ".cnt"), crc_ptr);
}

bool
module_state::read_counts (unsigned counts[MSC_HWM])
{
  bytes_in cfg;

  if (!cfg.begin (loc, from (), MOD_SNAME_PFX ".cnt"))
    return false;

  for (unsigned ix = MSC_HWM; ix--;)
    counts[ix] = cfg.u ();

  if (dump ())
    {
      dump ("Declaration sections are [%u,%u)",
	    counts[MSC_sec_lwm], counts[MSC_sec_hwm]);
      dump ("Bindings %u", counts[MSC_bindings]);
      dump ("Pendings %u", counts[MSC_pendings]);
      dump ("Entities %u", counts[MSC_entities]);
      dump ("Namespaces %u", counts[MSC_namespaces]);
      dump ("Macros %u", counts[MSC_macros]);
      dump ("Initializers %u", counts[MSC_inits]);
    }

  return cfg.end (from ());
}

/* Tool configuration:  MOD_SNAME_PFX .config

   This is data that confirms current state (or fails).  */

void
module_state::write_config (elf_out *to, module_state_config &config,
			    unsigned inner_crc)
{
  bytes_out cfg (to);

  cfg.begin ();

  /* Write version and inner crc as u32 values, for easier
     debug inspection.  */
  dump () && dump ("Writing version=%V, inner_crc=%x",
		   MODULE_VERSION, inner_crc);
  cfg.u32 (unsigned (MODULE_VERSION));
  cfg.u32 (inner_crc);

  cfg.u (to->name (is_header () ? "" : get_flatname ()));

  /* Configuration. */
  dump () && dump ("Writing target='%s', host='%s'",
		   TARGET_MACHINE, HOST_MACHINE);
  unsigned target = to->name (TARGET_MACHINE);
  unsigned host = (!strcmp (TARGET_MACHINE, HOST_MACHINE)
		   ? target : to->name (HOST_MACHINE));
  cfg.u (target);
  cfg.u (host);

  cfg.str (config.dialect_str);
  cfg.u (extensions);

  /* Global tree information.  We write the globals crc separately,
     rather than mix it directly into the overall crc, as it is used
     to ensure data match between instances of the compiler, not
     integrity of the file.  */
  dump () && dump ("Writing globals=%u, crc=%x",
		   fixed_trees->length (), global_crc);
  cfg.u (fixed_trees->length ());
  cfg.u32 (global_crc);

  if (is_partition ())
    cfg.u (is_interface ());

  cfg.u (config.num_imports);
  cfg.u (config.num_partitions);
  cfg.u (config.num_entities);

  cfg.loc (config.ordinary_locs);
  cfg.loc (config.macro_locs);
  cfg.u (config.loc_range_bits);

  cfg.u (config.active_init);

  /* Now generate CRC, we'll have incorporated the inner CRC because
     of its serialization above.  */
  cfg.end (to, to->name (MOD_SNAME_PFX ".cfg"), &crc);
  dump () && dump ("Writing CRC=%x", crc);
}

void
module_state::note_cmi_name ()
{
  if (!cmi_noted_p && filename)
    {
      cmi_noted_p = true;
      inform (loc, "compiled module file is %qs",
	      maybe_add_cmi_prefix (filename));
    }
}

bool
module_state::read_config (module_state_config &config)
{
  bytes_in cfg;

  if (!cfg.begin (loc, from (), MOD_SNAME_PFX ".cfg"))
    return false;

  /* Check version.  */
  unsigned my_ver = MODULE_VERSION;
  unsigned their_ver = cfg.u32 ();
  dump () && dump  (my_ver == their_ver ? "Version %V"
		    : "Expecting %V found %V", my_ver, their_ver);
  if (their_ver != my_ver)
    {
      /* The compiler versions differ.  Close enough? */
      verstr_t my_string, their_string;

      version2string (my_ver, my_string);
      version2string (their_ver, their_string);

      /* Reject when either is non-experimental or when experimental
	 major versions differ.  */
      auto_diagnostic_group d;
      bool reject_p = ((!IS_EXPERIMENTAL (my_ver)
			|| !IS_EXPERIMENTAL (their_ver)
			|| MODULE_MAJOR (my_ver) != MODULE_MAJOR (their_ver))
		       /* The 'I know what I'm doing' switch.  */
		       && !flag_module_version_ignore);
      bool inform_p = true;
      if (reject_p)
	{
	  cfg.set_overrun ();
	  error_at (loc, "compiled module is %sversion %s",
		    IS_EXPERIMENTAL (their_ver) ? "experimental " : "",
		    their_string);
	}
      else
	inform_p = warning_at (loc, 0, "compiled module is %sversion %s",
			     IS_EXPERIMENTAL (their_ver) ? "experimental " : "",
			     their_string);

      if (inform_p)
	{
	  inform (loc, "compiler is %sversion %s%s%s",
		  IS_EXPERIMENTAL (my_ver) ? "experimental " : "",
		  my_string,
		  reject_p ? "" : flag_module_version_ignore
		  ? ", be it on your own head!" : ", close enough?",
		  reject_p ? "" : " \xc2\xaf\\_(\xe3\x83\x84)_/\xc2\xaf");
	  note_cmi_name ();
	}

      if (reject_p)
	goto done;
    }

  /*  We wrote the inner crc merely to merge it, so simply read it
      back and forget it.  */
  cfg.u32 ();

  /* Check module name.  */
  {
    const char *their_name = from ()->name (cfg.u ());
    const char *our_name = "";

    if (!is_header ())
      our_name = get_flatname ();

    /* Header units can be aliased, so name checking is
       inappropriate.  */
    if (0 != strcmp (their_name, our_name))
      {
	error_at (loc,
		  their_name[0] && our_name[0] ? G_("module %qs found")
		  : their_name[0]
		  ? G_("header module expected, module %qs found")
		  : G_("module %qs expected, header module found"),
		  their_name[0] ? their_name : our_name);
	cfg.set_overrun ();
	goto done;
      }
  }

  /* Check the CRC after the above sanity checks, so that the user is
     clued in.  */
  {
    unsigned e_crc = crc;
    crc = cfg.get_crc ();
    dump () && dump ("Reading CRC=%x", crc);
    if (!is_direct () && crc != e_crc)
      {
	error_at (loc, "module %qs CRC mismatch", get_flatname ());
	cfg.set_overrun ();
	goto done;
      }
  }

  /* Check target & host.  */
  {
    const char *their_target = from ()->name (cfg.u ());
    const char *their_host = from ()->name (cfg.u ());
    dump () && dump ("Read target='%s', host='%s'", their_target, their_host);
    if (strcmp (their_target, TARGET_MACHINE)
	|| strcmp (their_host, HOST_MACHINE))
      {
	error_at (loc, "target & host is %qs:%qs, expected %qs:%qs",
		  their_target, TARGET_MACHINE, their_host, HOST_MACHINE);
	cfg.set_overrun ();
	goto done;
      }
  }

  /* Check compilation dialect.  This must match.  */
  {
    const char *their_dialect = cfg.str ();
    if (strcmp (their_dialect, config.dialect_str))
      {
	error_at (loc, "language dialect differs %qs, expected %qs",
		  their_dialect, config.dialect_str);
	cfg.set_overrun ();
	goto done;
      }
  }

  /* Check for extensions.  If they set any, we must have them set
     too.  */
  {
    unsigned ext = cfg.u ();
    unsigned allowed = (flag_openmp ? SE_OPENMP : 0);

    if (unsigned bad = ext & ~allowed)
      {
	if (bad & SE_OPENMP)
	  error_at (loc, "module contains OpenMP, use %<-fopenmp%> to enable");
	cfg.set_overrun ();
	goto done;
      }
    extensions = ext;
  }

  /* Check global trees.  */
  {
    unsigned their_fixed_length = cfg.u ();
    unsigned their_fixed_crc = cfg.u32 ();
    dump () && dump ("Read globals=%u, crc=%x",
		     their_fixed_length, their_fixed_crc);
    if (!flag_preprocess_only
	&& (their_fixed_length != fixed_trees->length ()
	    || their_fixed_crc != global_crc))
      {
	error_at (loc, "fixed tree mismatch");
	cfg.set_overrun ();
	goto done;
      }
  }

  /* All non-partitions are interfaces.  */
  interface_p = !is_partition () || cfg.u ();

  config.num_imports = cfg.u ();
  config.num_partitions = cfg.u ();
  config.num_entities = cfg.u ();

  config.ordinary_locs = cfg.loc ();
  config.macro_locs = cfg.loc ();
  config.loc_range_bits = cfg.u ();

  config.active_init = cfg.u ();

 done:
  return cfg.end (from ());
}

/* Comparator for ordering the Ordered Ordinary Location array.  */

static int
ool_cmp (const void *a_, const void *b_)
{
  auto *a = *static_cast<const module_state *const *> (a_);
  auto *b = *static_cast<const module_state *const *> (b_);
  if (a == b)
    return 0;
  else if (a->ordinary_locs.first < b->ordinary_locs.first)
    return -1;
  else
    return +1;
}

/* Use ELROND format to record the following sections:
     qualified-names	    : binding value(s)
     MOD_SNAME_PFX.README   : human readable, strings
     MOD_SNAME_PFX.ENV      : environment strings, strings
     MOD_SNAME_PFX.nms 	    : namespace hierarchy
     MOD_SNAME_PFX.bnd      : binding table
     MOD_SNAME_PFX.spc      : specialization table
     MOD_SNAME_PFX.imp      : import table
     MOD_SNAME_PFX.ent      : entity table
     MOD_SNAME_PFX.prt      : partitions table
     MOD_SNAME_PFX.olm      : ordinary line maps
     MOD_SNAME_PFX.mlm      : macro line maps
     MOD_SNAME_PFX.def      : macro definitions
     MOD_SNAME_PFX.mac      : macro index
     MOD_SNAME_PFX.ini      : inits
     MOD_SNAME_PFX.cnt      : counts
     MOD_SNAME_PFX.cfg      : config data
*/

bool
module_state::write_begin (elf_out *to, cpp_reader *reader,
			   module_state_config &config, unsigned &crc)
{
  /* Figure out remapped module numbers, which might elide
     partitions.  */
  bitmap partitions = NULL;
  if (!is_header () && !is_partition ())
    partitions = BITMAP_GGC_ALLOC ();
  write_init_maps ();

  unsigned mod_hwm = 1;
  for (unsigned ix = 1; ix != modules->length (); ix++)
    {
      module_state *imp = (*modules)[ix];

      /* Promote any non-partition direct import from a partition, unless
	 we're a partition.  */
      if (!is_partition () && !imp->is_partition ()
	  && imp->is_partition_direct ())
	imp->directness = MD_PURVIEW_DIRECT;

      /* Write any import that is not a partition, unless we're a
	 partition.  */
      if (!partitions || !imp->is_partition ())
	imp->remap = mod_hwm++;
      else
	{
	  dump () && dump ("Partition %M %u", imp, ix);
	  bitmap_set_bit (partitions, ix);
	  imp->remap = 0;
	  /* All interface partitions must be exported.  */
	  if (imp->is_interface () && !bitmap_bit_p (exports, imp->mod))
	    {
	      error_at (imp->loc, "interface partition is not exported");
	      bitmap_set_bit (exports, imp->mod);
	    }

	  /* All the partition entities should have been loaded when
	     loading the partition.  */
	  if (CHECKING_P)
	    for (unsigned jx = 0; jx != imp->entity_num; jx++)
	      {
		binding_slot *slot = &(*entity_ary)[imp->entity_lwm + jx];
		gcc_checking_assert (!slot->is_lazy ());
	      }
	}

      if (imp->is_direct () && (imp->remap || imp->is_partition ()))
	note_location (imp->imported_from ());
    }

  if (partitions && bitmap_empty_p (partitions))
    /* No partitions present.  */
    partitions = nullptr;

  /* Find the set of decls we must write out.  */
  depset::hash table (DECL_NAMESPACE_BINDINGS (global_namespace)->size () * 8);
  /* Add the specializations before the writables, so that we can
     detect injected friend specializations.  */
  table.add_specializations (true);
  table.add_specializations (false);
  if (partial_specializations)
    {
      table.add_partial_entities (partial_specializations);
      partial_specializations = NULL;
    }
  table.add_namespace_entities (global_namespace, partitions);
  if (class_members)
    {
      table.add_class_entities (class_members);
      class_members = NULL;
    }

  /* Now join everything up.  */
  table.find_dependencies (this);

  if (!table.finalize_dependencies ())
    return false;

#if CHECKING_P
  /* We're done verifying at-most once reading, reset to verify
     at-most once writing.  */
  note_defs = note_defs_table_t::create_ggc (1000);
#endif

  /* Determine Strongy Connected Components.  */
  vec<depset *> sccs = table.connect ();

  vec_alloc (ool, modules->length ());
  for (unsigned ix = modules->length (); --ix;)
    {
      auto *import = (*modules)[ix];
      if (import->loadedness > ML_NONE
	  && !(partitions && bitmap_bit_p (partitions, import->mod)))
	ool->quick_push (import);
    }
  ool->qsort (ool_cmp);

  vec<cpp_hashnode *> *macros = nullptr;
  if (is_header ())
    macros = prepare_macros (reader);

  config.num_imports = mod_hwm;
  config.num_partitions = modules->length () - mod_hwm;
  auto map_info = write_prepare_maps (&config, bool (config.num_partitions));
  unsigned counts[MSC_HWM];
  memset (counts, 0, sizeof (counts));

  /* depset::cluster is the cluster number,
     depset::section is unspecified scratch value.

     The following loops make use of the tarjan property that
     dependencies will be earlier in the SCCS array.  */

  /* This first loop determines the number of depsets in each SCC, and
     also the number of namespaces we're dealing with.  During the
     loop, the meaning of a couple of depset fields now change:

     depset::cluster -> size_of cluster, if first of cluster & !namespace
     depset::section -> section number of cluster (if !namespace). */

  unsigned n_spaces = 0;
  counts[MSC_sec_lwm] = counts[MSC_sec_hwm] = to->get_section_limit ();
  for (unsigned size, ix = 0; ix < sccs.length (); ix += size)
    {
      depset **base = &sccs[ix];

      if (base[0]->get_entity_kind () == depset::EK_NAMESPACE)
	{
	  n_spaces++;
	  size = 1;
	}
      else
	{
	  /* Count the members in this cluster.  */
	  for (size = 1; ix + size < sccs.length (); size++)
	    if (base[size]->cluster != base[0]->cluster)
	      break;

	  for (unsigned jx = 0; jx != size; jx++)
	    {
	      /* Set the section number.  */
	      base[jx]->cluster = ~(~0u >> 1); /* A bad value.  */
	      base[jx]->section = counts[MSC_sec_hwm];
	    }

	  /* Save the size in the first member's cluster slot.  */
	  base[0]->cluster = size;

	  counts[MSC_sec_hwm]++;
	}
    }

  /* Write the clusters.  Namespace decls are put in the spaces array.
     The meaning of depset::cluster changes to provide the
     unnamed-decl count of the depset's decl (and remains zero for
     non-decls and non-unnamed).  */
  unsigned bytes = 0;
  vec<depset *> spaces;
  spaces.create (n_spaces);

  for (unsigned size, ix = 0; ix < sccs.length (); ix += size)
    {
      depset **base = &sccs[ix];

      if (base[0]->get_entity_kind () == depset::EK_NAMESPACE)
	{
	  tree decl = base[0]->get_entity ();
	  if (decl == global_namespace)
	    base[0]->cluster = 0;
	  else if (!base[0]->is_import ())
	    {
	      base[0]->cluster = counts[MSC_entities]++;
	      spaces.quick_push (base[0]);
	      counts[MSC_namespaces]++;
	      if (CHECKING_P)
		{
		  /* Add it to the entity map, such that we can tell it is
		     part of us.  */
		  bool existed;
		  unsigned *slot = &entity_map->get_or_insert
		    (DECL_UID (decl), &existed);
		  if (existed)
		    /* It must have come from a partition.  */
		    gcc_checking_assert
		      (import_entity_module (*slot)->is_partition ());
		  *slot = ~base[0]->cluster;
		}
	      dump (dumper::CLUSTER) && dump ("Cluster namespace %N", decl);
	    }
	  size = 1;
	}
      else
	{
	  size = base[0]->cluster;

	  /* Cluster is now used to number entities.  */
	  base[0]->cluster = ~(~0u >> 1); /* A bad value.  */

	  sort_cluster (&table, base, size);

	  /* Record the section for consistency checking during stream
	     out -- we don't want to start writing decls in different
	     sections.  */
	  table.section = base[0]->section;
	  bytes += write_cluster (to, base, size, table, counts, &crc);
	  table.section = 0;
	}
    }

  /* depset::cluster - entity number (on entities)
     depset::section - cluster number  */
  /* We'd better have written as many sections and found as many
     namespaces as we predicted.  */
  gcc_assert (counts[MSC_sec_hwm] == to->get_section_limit ()
	      && spaces.length () == counts[MSC_namespaces]);

  /* Write the entitites.  None happens if we contain namespaces or
     nothing. */
  config.num_entities = counts[MSC_entities];
  if (counts[MSC_entities])
    write_entities (to, sccs, counts[MSC_entities], &crc);

  /* Write the namespaces.  */
  if (counts[MSC_namespaces])
    write_namespaces (to, spaces, counts[MSC_namespaces], &crc);

  /* Write the bindings themselves.  */
  counts[MSC_bindings] = write_bindings (to, sccs, &crc);

  /* Write the unnamed.  */
  counts[MSC_pendings] = write_pendings (to, sccs, table, &crc);

  /* Write the import table.  */
  if (config.num_imports > 1)
    write_imports (to, &crc);

  /* Write elided partition table.  */
  if (config.num_partitions)
    write_partitions (to, config.num_partitions, &crc);

  /* Write the line maps.  */
  if (config.ordinary_locs)
    write_ordinary_maps (to, map_info, bool (config.num_partitions), &crc);
  if (config.macro_locs)
    write_macro_maps (to, map_info, &crc);

  if (is_header ())
    {
      counts[MSC_macros] = write_macros (to, macros, &crc);
      counts[MSC_inits] = write_inits (to, table, &crc);
      vec_free (macros);
    }

  unsigned clusters = counts[MSC_sec_hwm] - counts[MSC_sec_lwm];
  dump () && dump ("Wrote %u clusters, average %u bytes/cluster",
		   clusters, (bytes + clusters / 2) / (clusters + !clusters));
  trees_out::instrument ();

  write_counts (to, counts, &crc);

  spaces.release ();
  sccs.release ();

  vec_free (macro_loc_remap);
  vec_free (ord_loc_remap);
  vec_free (ool);

  // FIXME:QOI:  Have a command line switch to control more detailed
  // information (which might leak data you do not want to leak).
  // Perhaps (some of) the write_readme contents should also be
  // so-controlled.
  if (false)
    write_env (to);

  return true;
}

// Finish module writing after we've emitted all dynamic initializers.

void
module_state::write_end (elf_out *to, cpp_reader *reader,
			 module_state_config &config, unsigned &crc)
{
  /* And finish up.  */
  write_config (to, config, crc);

  /* Human-readable info.  */
  write_readme (to, reader, config.dialect_str);

  dump () && dump ("Wrote %u sections", to->get_section_limit ());
}

/* Initial read of a CMI.  Checks config, loads up imports and line
   maps.  */

bool
module_state::read_initial (cpp_reader *reader)
{
  module_state_config config;
  bool ok = true;

  if (ok && !from ()->begin (loc))
    ok = false;

  if (ok && !read_config (config))
    ok = false;

  bool have_locs = ok && read_prepare_maps (&config);

  /* Ordinary maps before the imports.  */
  if (!(have_locs && config.ordinary_locs))
    ordinary_locs.first = line_table->highest_location + 1;
  else if (!read_ordinary_maps (config.ordinary_locs, config.loc_range_bits))
    ok = false;

  /* Allocate the REMAP vector.  */
  slurp->alloc_remap (config.num_imports);

  if (ok)
    {
      /* Read the import table.  Decrement current to stop this CMI
	 from being evicted during the import. */
      slurp->current--;
      if (config.num_imports > 1 && !read_imports (reader, line_table))
	ok = false;
      slurp->current++;
    }

  /* Read the elided partition table, if we're the primary partition.  */
  if (ok && config.num_partitions && is_module ()
      && !read_partitions (config.num_partitions))
    ok = false;

  /* Determine the module's number.  */
  gcc_checking_assert (mod == MODULE_UNKNOWN);
  gcc_checking_assert (this != (*modules)[0]);

  {
    /* Allocate space in the entities array now -- that array must be
       monotonically in step with the modules array.  */
    entity_lwm = vec_safe_length (entity_ary);
    entity_num = config.num_entities;
    gcc_checking_assert (modules->length () == 1
			 || modules->last ()->entity_lwm <= entity_lwm);
    vec_safe_reserve (entity_ary, config.num_entities);

    binding_slot slot;
    slot.u.binding = NULL_TREE;
    for (unsigned count = config.num_entities; count--;)
      entity_ary->quick_push (slot);
  }

  /* We'll run out of other resources before we run out of module
     indices.  */
  mod = modules->length ();
  vec_safe_push (modules, this);

  /* We always import and export ourselves. */
  bitmap_set_bit (imports, mod);
  bitmap_set_bit (exports, mod);

  if (ok)
    (*slurp->remap)[0] = mod << 1;
  dump () && dump ("Assigning %M module number %u", this, mod);

  /* We should not have been frozen during the importing done by
     read_config.  */
  gcc_assert (!from ()->is_frozen ());

  /* Macro maps after the imports.  */
  if (!(ok && have_locs && config.macro_locs))
    macro_locs.first = LINEMAPS_MACRO_LOWEST_LOCATION (line_table);
  else if (!read_macro_maps (config.macro_locs))
    ok = false;

  /* Note whether there's an active initializer.  */
  active_init_p = !is_header () && bool (config.active_init);

  gcc_assert (slurp->current == ~0u);
  return ok;
}

/* Read a preprocessor state.  */

bool
module_state::read_preprocessor (bool outermost)
{
  gcc_checking_assert (is_header () && slurp
		       && slurp->remap_module (0) == mod);

  if (loadedness == ML_PREPROCESSOR)
    return !(from () && from ()->get_error ());

  bool ok = true;

  /* Read direct header imports.  */
  unsigned len = slurp->remap->length ();
  for (unsigned ix = 1; ok && ix != len; ix++)
    {
      unsigned map = (*slurp->remap)[ix];
      if (map & 1)
	{
	  module_state *import = (*modules)[map >> 1];
	  if (import->is_header ())
	    {
	      ok = import->read_preprocessor (false);
	      bitmap_ior_into (slurp->headers, import->slurp->headers);
	    }
	}
    }

  /* Record as a direct header.  */
  if (ok)
    bitmap_set_bit (slurp->headers, mod);

  if (ok && !read_macros ())
    ok = false;

  loadedness = ML_PREPROCESSOR;
  announce ("macros");

  if (flag_preprocess_only)
    /* We're done with the string table.  */
    from ()->release ();

  return check_read (outermost, ok);
}

/* Read language state.  */

bool
module_state::read_language (bool outermost)
{
  gcc_checking_assert (!lazy_snum);

  if (loadedness == ML_LANGUAGE)
    return !(slurp && from () && from ()->get_error ());

  gcc_checking_assert (slurp && slurp->current == ~0u
		       && slurp->remap_module (0) == mod);

  bool ok = true;

  /* Read direct imports.  */
  unsigned len = slurp->remap->length ();
  for (unsigned ix = 1; ok && ix != len; ix++)
    {
      unsigned map = (*slurp->remap)[ix];
      if (map & 1)
	{
	  module_state *import = (*modules)[map >> 1];
	  if (!import->read_language (false))
	    ok = false;
	}
    }

  unsigned counts[MSC_HWM];

  if (ok && !read_counts (counts))
    ok = false;

  function_depth++; /* Prevent unexpected GCs.  */

  if (ok && counts[MSC_entities] != entity_num)
    ok = false;
  if (ok && counts[MSC_entities]
      && !read_entities (counts[MSC_entities],
			 counts[MSC_sec_lwm], counts[MSC_sec_hwm]))
    ok = false;

  /* Read the namespace hierarchy. */
  if (ok && counts[MSC_namespaces]
      && !read_namespaces (counts[MSC_namespaces]))
    ok = false;

  if (ok && !read_bindings (counts[MSC_bindings],
			    counts[MSC_sec_lwm], counts[MSC_sec_hwm]))
    ok = false;

  /* And unnamed.  */
  if (ok && counts[MSC_pendings] && !read_pendings (counts[MSC_pendings]))
    ok = false;

  if (ok)
    {
      slurp->remaining = counts[MSC_sec_hwm] - counts[MSC_sec_lwm];
      available_clusters += counts[MSC_sec_hwm] - counts[MSC_sec_lwm];
    }

  if (!flag_module_lazy
      || (is_partition ()
	  && module_interface_p ()
	  && !module_partition_p ()))
    {
      /* Read the sections in forward order, so that dependencies are read
	 first.  See note about tarjan_connect.  */
      ggc_collect ();

      lazy_snum = ~0u;

      unsigned hwm = counts[MSC_sec_hwm];
      for (unsigned ix = counts[MSC_sec_lwm]; ok && ix != hwm; ix++)
	if (!load_section (ix, NULL))
	  {
	    ok = false;
	    break;
	  }
      lazy_snum = 0;
      post_load_processing ();

      ggc_collect ();

      if (ok && CHECKING_P)
	for (unsigned ix = 0; ix != entity_num; ix++)
	  gcc_assert (!(*entity_ary)[ix + entity_lwm].is_lazy ());
    }

  // If the import is a header-unit, we need to register initializers
  // of any static objects it contains (looking at you _Ioinit).
  // Notice, the ordering of these initializers will be that of a
  // dynamic initializer at this point in the current TU.  (Other
  // instances of these objects in other TUs will be initialized as
  // part of that TU's global initializers.)
  if (ok && counts[MSC_inits] && !read_inits (counts[MSC_inits]))
    ok = false;

  function_depth--;

  announce (flag_module_lazy ? "lazy" : "imported");
  loadedness = ML_LANGUAGE;

  gcc_assert (slurp->current == ~0u);

  /* We're done with the string table.  */
  from ()->release ();

  return check_read (outermost, ok);
}

bool
module_state::maybe_defrost ()
{
  bool ok = true;
  if (from ()->is_frozen ())
    {
      if (lazy_open >= lazy_limit)
	freeze_an_elf ();
      dump () && dump ("Defrosting '%s'", filename);
      ok = from ()->defrost (maybe_add_cmi_prefix (filename));
      lazy_open++;
    }

  return ok;
}

/* Load section SNUM, dealing with laziness.  It doesn't matter if we
   have multiple concurrent loads, because we do not use TREE_VISITED
   when reading back in.  */

bool
module_state::load_section (unsigned snum, binding_slot *mslot)
{
  if (from ()->get_error ())
    return false;

  if (snum >= slurp->current)
    from ()->set_error (elf::E_BAD_LAZY);
  else if (maybe_defrost ())
    {
      unsigned old_current = slurp->current;
      slurp->current = snum;
      slurp->lru = 0;  /* Do not swap out.  */
      slurp->remaining--;
      read_cluster (snum);
      slurp->lru = ++lazy_lru;
      slurp->current = old_current;
    }

  if (mslot && mslot->is_lazy ())
    {
      /* Oops, the section didn't set this slot.  */
      from ()->set_error (elf::E_BAD_DATA);
      *mslot = NULL_TREE;
    }

  bool ok = !from ()->get_error ();
  if (!ok)
    {
      error_at (loc, "failed to read compiled module cluster %u: %s",
		snum, from ()->get_error (filename));
      note_cmi_name ();
    }

  maybe_completed_reading ();

  return ok;
}

void
module_state::maybe_completed_reading ()
{
  if (loadedness == ML_LANGUAGE && slurp->current == ~0u && !slurp->remaining)
    {
      lazy_open--;
      /* We no longer need the macros, all tokenizing has been done.  */
      slurp->release_macros ();

      from ()->end ();
      slurp->close ();
      slurped ();
    }
}

/* After a reading operation, make sure things are still ok.  If not,
   emit an error and clean up.  */

bool
module_state::check_read (bool outermost, bool ok)
{
  gcc_checking_assert (!outermost || slurp->current == ~0u);

  if (!ok)
    from ()->set_error ();

  if (int e = from ()->get_error ())
    {
      auto_diagnostic_group d;
      error_at (loc, "failed to read compiled module: %s",
		from ()->get_error (filename));
      note_cmi_name ();

      if (e == EMFILE
	  || e == ENFILE
#if MAPPED_READING
	  || e == ENOMEM
#endif
	  || false)
	inform (loc, "consider using %<-fno-module-lazy%>,"
		" increasing %<-param-lazy-modules=%u%> value,"
		" or increasing the per-process file descriptor limit",
		param_lazy_modules);
      else if (e == ENOENT)
	inform (loc, "imports must be built before being imported");

      if (outermost)
	fatal_error (loc, "returning to the gate for a mechanical issue");

      ok = false;
    }

  maybe_completed_reading ();

  return ok;
}

/* Return the IDENTIFIER_NODE naming module IX.  This is the name
   including dots.  */

char const *
module_name (unsigned ix, bool header_ok)
{
  if (modules)
    {
      module_state *imp = (*modules)[ix];

      if (ix && !imp->name)
	imp = imp->parent;

      if (header_ok || !imp->is_header ())
	return imp->get_flatname ();
    }

  return NULL;
}

/* Return the bitmap describing what modules are imported.  Remember,
   we always import ourselves.  */

bitmap
get_import_bitmap ()
{
  return (*modules)[0]->imports;
}

/* Return the visible imports and path of instantiation for an
   instantiation at TINST.  If TINST is nullptr, we're not in an
   instantiation, and thus will return the visible imports of the
   current TU (and NULL *PATH_MAP_P).   We cache the information on
   the tinst level itself.  */

static bitmap
path_of_instantiation (tinst_level *tinst,  bitmap *path_map_p)
{
  gcc_checking_assert (modules_p ());

  if (!tinst)
    {
      /* Not inside an instantiation, just the regular case.  */
      *path_map_p = nullptr;
      return get_import_bitmap ();
    }

  if (!tinst->path)
    {
      /* Calculate.  */
      bitmap visible = path_of_instantiation (tinst->next, path_map_p);
      bitmap path_map = *path_map_p;

      if (!path_map)
	{
	  path_map = BITMAP_GGC_ALLOC ();
	  bitmap_set_bit (path_map, 0);
	}

      tree decl = tinst->tldcl;
      if (TREE_CODE (decl) == TREE_LIST)
	decl = TREE_PURPOSE (decl);
      if (TYPE_P (decl))
	decl = TYPE_NAME (decl);

      if (unsigned mod = get_originating_module (decl))
	if (!bitmap_bit_p (path_map, mod))
	  {
	    /* This is brand new information!  */
	    bitmap new_path = BITMAP_GGC_ALLOC ();
	    bitmap_copy (new_path, path_map);
	    bitmap_set_bit (new_path, mod);
	    path_map = new_path;

	    bitmap imports = (*modules)[mod]->imports;
	    if (bitmap_intersect_compl_p (imports, visible))
	      {
		/* IMPORTS contains additional modules to VISIBLE.  */
		bitmap new_visible = BITMAP_GGC_ALLOC ();

		bitmap_ior (new_visible, visible, imports);
		visible = new_visible;
	      }
	  }

      tinst->path = path_map;
      tinst->visible = visible;
    }

  *path_map_p = tinst->path;
  return tinst->visible;
}

/* Return the bitmap describing what modules are visible along the
   path of instantiation.  If we're not an instantiation, this will be
   the visible imports of the TU.  *PATH_MAP_P is filled in with the
   modules owning the instantiation path -- we see the module-linkage
   entities of those modules.  */

bitmap
visible_instantiation_path (bitmap *path_map_p)
{
  if (!modules_p ())
    return NULL;

  return path_of_instantiation (current_instantiation (), path_map_p);
}

/* We've just directly imported IMPORT.  Update our import/export
   bitmaps.  IS_EXPORT is true if we're reexporting the OTHER.  */

void
module_state::set_import (module_state const *import, bool is_export)
{
  gcc_checking_assert (this != import);

  /* We see IMPORT's exports (which includes IMPORT).  If IMPORT is
     the primary interface or a partition we'll see its imports.  */
  bitmap_ior_into (imports, import->is_module () || import->is_partition ()
		   ? import->imports : import->exports);

  if (is_export)
    /* We'll export OTHER's exports.  */
    bitmap_ior_into (exports, import->exports);
}

/* Return the declaring entity of DECL.  That is the decl determining
   how to decorate DECL with module information.  Returns NULL_TREE if
   it's the global module.  */

tree
get_originating_module_decl (tree decl)
{
  /* An enumeration constant.  */
  if (TREE_CODE (decl) == CONST_DECL
      && DECL_CONTEXT (decl)
      && (TREE_CODE (DECL_CONTEXT (decl)) == ENUMERAL_TYPE))
    decl = TYPE_NAME (DECL_CONTEXT (decl));
  else if (TREE_CODE (decl) == FIELD_DECL
	   || TREE_CODE (decl) == USING_DECL
	   || CONST_DECL_USING_P (decl))
    {
      decl = DECL_CONTEXT (decl);
      if (TREE_CODE (decl) != FUNCTION_DECL)
	decl = TYPE_NAME (decl);
    }

  gcc_checking_assert (TREE_CODE (decl) == TEMPLATE_DECL
		       || TREE_CODE (decl) == FUNCTION_DECL
		       || TREE_CODE (decl) == TYPE_DECL
		       || TREE_CODE (decl) == VAR_DECL
		       || TREE_CODE (decl) == CONCEPT_DECL
		       || TREE_CODE (decl) == NAMESPACE_DECL);

  for (;;)
    {
      /* Uninstantiated template friends are owned by the befriending
	 class -- not their context.  */
      if (TREE_CODE (decl) == TEMPLATE_DECL
	  && DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P (decl))
	decl = TYPE_NAME (DECL_CHAIN (decl));

      /* An imported temploid friend is attached to the same module the
	 befriending class was.  */
      if (imported_temploid_friends)
	if (tree *slot = imported_temploid_friends->get (decl))
	  decl = *slot;

      int use;
      if (tree ti = node_template_info (decl, use))
	{
	  decl = TI_TEMPLATE (ti);
	  if (TREE_CODE (decl) != TEMPLATE_DECL)
	    {
	      /* A friend template specialization.  */
	      gcc_checking_assert (OVL_P (decl));
	      return global_namespace;
	    }
	}
      else
	{
	  tree ctx = CP_DECL_CONTEXT (decl);
	  if (TREE_CODE (ctx) == NAMESPACE_DECL)
	    break;

	  if (TYPE_P (ctx))
	    {
	      ctx = TYPE_NAME (ctx);
	      if (!ctx)
		{
		  /* Some kind of internal type.  */
		  gcc_checking_assert (DECL_ARTIFICIAL (decl));
		  return global_namespace;
		}
	    }
	  decl = ctx;
	}
    }

  return decl;
}

int
get_originating_module (tree decl, bool for_mangle)
{
  tree owner = get_originating_module_decl (decl);
  tree not_tmpl = STRIP_TEMPLATE (owner);

  if (!DECL_LANG_SPECIFIC (not_tmpl))
    return for_mangle ? -1 : 0;

  if (for_mangle && !DECL_MODULE_ATTACH_P (not_tmpl))
    return -1;

  int mod = !DECL_MODULE_IMPORT_P (not_tmpl) ? 0 : get_importing_module (owner);
  gcc_checking_assert (!for_mangle || !(*modules)[mod]->is_header ());
  return mod;
}

unsigned
get_importing_module (tree decl, bool flexible)
{
  unsigned index = import_entity_index (decl, flexible);
  if (index == ~(~0u >> 1))
    return -1;
  module_state *module = import_entity_module (index);

  return module->mod;
}

/* Is it permissible to redeclare OLDDECL with NEWDECL.

   If NEWDECL is NULL, assumes that OLDDECL will be redeclared using
   the current scope's module and attachment.  */

bool
module_may_redeclare (tree olddecl, tree newdecl)
{
  tree decl = olddecl;
  for (;;)
    {
      tree ctx = CP_DECL_CONTEXT (decl);
      if (TREE_CODE (ctx) == NAMESPACE_DECL)
	// Found the namespace-scope decl.
	break;
      if (!CLASS_TYPE_P (ctx))
	// We've met a non-class scope.  Such a thing is not
	// reopenable, so we must be ok.
	return true;
      decl = TYPE_NAME (ctx);
    }

  int use_tpl = 0;
  if (node_template_info (STRIP_TEMPLATE (decl), use_tpl) && use_tpl)
    // Specializations of any kind can be redeclared anywhere.
    // FIXME: Should we be checking this in more places on the scope chain?
    return true;

  module_state *old_mod = (*modules)[0];
  module_state *new_mod = old_mod;

  tree old_origin = get_originating_module_decl (decl);
  tree old_inner = STRIP_TEMPLATE (old_origin);
  bool olddecl_attached_p = (DECL_LANG_SPECIFIC (old_inner)
			     && DECL_MODULE_ATTACH_P (old_inner));
  if (DECL_LANG_SPECIFIC (old_inner) && DECL_MODULE_IMPORT_P (old_inner))
    {
      unsigned index = import_entity_index (old_origin);
      old_mod = import_entity_module (index);
    }

  bool newdecl_attached_p = module_attach_p ();
  if (newdecl)
    {
      tree new_origin = get_originating_module_decl (newdecl);
      tree new_inner = STRIP_TEMPLATE (new_origin);
      newdecl_attached_p = (DECL_LANG_SPECIFIC (new_inner)
			    && DECL_MODULE_ATTACH_P (new_inner));
      if (DECL_LANG_SPECIFIC (new_inner) && DECL_MODULE_IMPORT_P (new_inner))
	{
	  unsigned index = import_entity_index (new_origin);
	  new_mod = import_entity_module (index);
	}
    }

  /* Module attachment needs to match.  */
  if (olddecl_attached_p == newdecl_attached_p)
    {
      if (!olddecl_attached_p)
	/* Both are GM entities, OK.  */
	return true;

      if (new_mod == old_mod
	  || (new_mod && get_primary (new_mod) == get_primary (old_mod)))
	/* Both attached to same named module, OK.  */
	return true;
    }

  /* Attached to different modules, error.  */
  decl = newdecl ? newdecl : olddecl;
  location_t loc = newdecl ? DECL_SOURCE_LOCATION (newdecl) : input_location;
  if (DECL_IS_UNDECLARED_BUILTIN (olddecl))
    {
      if (newdecl_attached_p)
	error_at (loc, "declaring %qD in module %qs conflicts with builtin "
		  "in global module", decl, new_mod->get_flatname ());
      else
	error_at (loc, "declaration %qD conflicts with builtin", decl);
    }
  else if (DECL_LANG_SPECIFIC (old_inner) && DECL_MODULE_IMPORT_P (old_inner))
    {
      auto_diagnostic_group d;
      if (newdecl_attached_p)
	error_at (loc, "redeclaring %qD in module %qs conflicts with import",
		  decl, new_mod->get_flatname ());
      else
	error_at (loc, "redeclaring %qD in global module conflicts with import",
		  decl);

      if (olddecl_attached_p)
	inform (DECL_SOURCE_LOCATION (olddecl),
		"import declared attached to module %qs",
		old_mod->get_flatname ());
      else
	inform (DECL_SOURCE_LOCATION (olddecl),
		"import declared in global module");
    }
  else
    {
      auto_diagnostic_group d;
      if (newdecl_attached_p)
	error_at (loc, "conflicting declaration of %qD in module %qs",
		  decl, new_mod->get_flatname ());
      else
	error_at (loc, "conflicting declaration of %qD in global module",
		  decl);

      if (olddecl_attached_p)
	inform (DECL_SOURCE_LOCATION (olddecl),
		"previously declared in module %qs",
		old_mod->get_flatname ());
      else
	inform (DECL_SOURCE_LOCATION (olddecl),
		"previously declared in global module");
    }
  return false;
}

/* DECL is being created by this TU.  Record it came from here.  We
   record module purview, so we can see if partial or explicit
   specialization needs to be written out, even though its purviewness
   comes from the most general template.  */

void
set_instantiating_module (tree decl)
{
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
	      || VAR_P (decl)
	      || TREE_CODE (decl) == TYPE_DECL
	      || TREE_CODE (decl) == CONCEPT_DECL
	      || TREE_CODE (decl) == TEMPLATE_DECL
	      || TREE_CODE (decl) == CONST_DECL
	      || (TREE_CODE (decl) == NAMESPACE_DECL
		  && DECL_NAMESPACE_ALIAS (decl)));

  if (!modules_p ())
    return;

  decl = STRIP_TEMPLATE (decl);

  if (!DECL_LANG_SPECIFIC (decl) && module_purview_p ())
    retrofit_lang_decl (decl);

  if (DECL_LANG_SPECIFIC (decl))
    {
      DECL_MODULE_PURVIEW_P (decl) = module_purview_p ();
      /* If this was imported, we'll still be in the entity_hash.  */
      DECL_MODULE_IMPORT_P (decl) = false;
    }
}

/* If DECL is a class member, whose class is not defined in this TU
   (it was imported), remember this decl.  */

void
set_defining_module (tree decl)
{
  gcc_checking_assert (!DECL_LANG_SPECIFIC (decl)
		       || !DECL_MODULE_IMPORT_P (decl));

  if (module_maybe_has_cmi_p ())
    {
      /* We need to track all declarations within a module, not just those
	 in the module purview, because we don't necessarily know yet if
	 this module will require a CMI while in the global fragment.  */
      tree ctx = DECL_CONTEXT (decl);
      if (ctx
	  && (TREE_CODE (ctx) == RECORD_TYPE || TREE_CODE (ctx) == UNION_TYPE)
	  && DECL_LANG_SPECIFIC (TYPE_NAME (ctx))
	  && DECL_MODULE_IMPORT_P (TYPE_NAME (ctx)))
	{
	  /* This entity's context is from an import.  We may need to
	     record this entity to make sure we emit it in the CMI.
	     Template specializations are in the template hash tables,
	     so we don't need to record them here as well.  */
	  int use_tpl = -1;
	  tree ti = node_template_info (decl, use_tpl);
	  if (use_tpl <= 0)
	    {
	      if (ti)
		{
		  gcc_checking_assert (!use_tpl);
		  /* Get to the TEMPLATE_DECL.  */
		  decl = TI_TEMPLATE (ti);
		}

	      /* Record it on the class_members list.  */
	      vec_safe_push (class_members, decl);
	    }
	}
    }
}

/* Also remember DECL if it's a newly declared class template partial
   specialization, because these are not necessarily added to the
   instantiation tables.  */

void
set_defining_module_for_partial_spec (tree decl)
{
  if (module_maybe_has_cmi_p ()
      && DECL_IMPLICIT_TYPEDEF_P (decl)
      && CLASSTYPE_TEMPLATE_SPECIALIZATION (TREE_TYPE (decl)))
    vec_safe_push (partial_specializations, decl);
}

void
set_originating_module (tree decl, bool friend_p ATTRIBUTE_UNUSED)
{
  set_instantiating_module (decl);

  if (!DECL_NAMESPACE_SCOPE_P (decl))
    return;

  gcc_checking_assert (friend_p || decl == get_originating_module_decl (decl));

  if (module_attach_p ())
    {
      retrofit_lang_decl (decl);
      DECL_MODULE_ATTACH_P (decl) = true;
    }

  if (!module_exporting_p ())
    return;

  // FIXME: Check ill-formed linkage
  DECL_MODULE_EXPORT_P (decl) = true;
}

/* DECL is keyed to CTX for odr purposes.  */

void
maybe_key_decl (tree ctx, tree decl)
{
  if (!modules_p ())
    return;

  /* We only need to deal with lambdas attached to var, field,
     parm, or type decls.  */
  if (TREE_CODE (ctx) != VAR_DECL
      && TREE_CODE (ctx) != FIELD_DECL
      && TREE_CODE (ctx) != PARM_DECL
      && TREE_CODE (ctx) != TYPE_DECL)
    return;

  /* For fields, key it to the containing type to handle deduplication
     correctly.  */
  if (TREE_CODE (ctx) == FIELD_DECL)
    ctx = TYPE_NAME (DECL_CONTEXT (ctx));

  if (!keyed_table)
    keyed_table = new keyed_map_t (EXPERIMENT (1, 400));

  auto &vec = keyed_table->get_or_insert (ctx);
  if (!vec.length ())
    {
      retrofit_lang_decl (ctx);
      DECL_MODULE_KEYED_DECLS_P (ctx) = true;
    }
  vec.safe_push (decl);
}

/* DECL is an instantiated friend that should be attached to the same
   module that ORIG is.  */

void
propagate_defining_module (tree decl, tree orig)
{
  if (!modules_p ())
    return;

  tree not_tmpl = STRIP_TEMPLATE (orig);
  if (DECL_LANG_SPECIFIC (not_tmpl) && DECL_MODULE_ATTACH_P (not_tmpl))
    {
      tree inner = STRIP_TEMPLATE (decl);
      retrofit_lang_decl (inner);
      DECL_MODULE_ATTACH_P (inner) = true;
    }

  if (DECL_LANG_SPECIFIC (not_tmpl) && DECL_MODULE_IMPORT_P (not_tmpl))
    {
      bool exists = imported_temploid_friends->put (decl, orig);

      /* We should only be called if lookup for an existing decl
	 failed, in which case there shouldn't already be an entry
	 in the map.  */
      gcc_assert (!exists);
    }
}

/* DECL is being freed, clear data we don't need anymore.  */

void
remove_defining_module (tree decl)
{
  if (!modules_p ())
    return;

  if (imported_temploid_friends)
    imported_temploid_friends->remove (decl);
}

/* Create the flat name string.  It is simplest to have it handy.  */

void
module_state::set_flatname ()
{
  gcc_checking_assert (!flatname);
  if (parent)
    {
      auto_vec<tree,5> ids;
      size_t len = 0;
      char const *primary = NULL;
      size_t pfx_len = 0;

      for (module_state *probe = this;
	   probe;
	   probe = probe->parent)
	if (is_partition () && !probe->is_partition ())
	  {
	    primary = probe->get_flatname ();
	    pfx_len = strlen (primary);
	    break;
	  }
	else
	  {
	    ids.safe_push (probe->name);
	    len += IDENTIFIER_LENGTH (probe->name) + 1;
	  }

      char *flat = XNEWVEC (char, pfx_len + len + is_partition ());
      flatname = flat;

      if (primary)
	{
	  memcpy (flat, primary, pfx_len);
	  flat += pfx_len;
	  *flat++ = ':';
	}

      for (unsigned len = 0; ids.length ();)
	{
	  if (len)
	    flat[len++] = '.';
	  tree elt = ids.pop ();
	  unsigned l = IDENTIFIER_LENGTH (elt);
	  memcpy (flat + len, IDENTIFIER_POINTER (elt), l + 1);
	  len += l;
	}
    }
  else if (is_header ())
    flatname = TREE_STRING_POINTER (name);
  else
    flatname = IDENTIFIER_POINTER (name);
}

/* Read the CMI file for a module.  */

bool
module_state::do_import (cpp_reader *reader, bool outermost)
{
  gcc_assert (global_namespace == current_scope () && loadedness == ML_NONE);

  loc = linemap_module_loc (line_table, loc, get_flatname ());

  if (lazy_open >= lazy_limit)
    freeze_an_elf ();

  int fd = -1;
  int e = ENOENT;
  if (filename)
    {
      const char *file = maybe_add_cmi_prefix (filename);
      dump () && dump ("CMI is %s", file);
      if (note_module_cmi_yes || inform_cmi_p)
	inform (loc, "reading CMI %qs", file);
      /* Add the CMI file to the dependency tracking. */
      if (cpp_get_deps (reader))
	deps_add_dep (cpp_get_deps (reader), file);
      fd = open (file, O_RDONLY | O_CLOEXEC | O_BINARY);
      e = errno;
    }

  gcc_checking_assert (!slurp);
  slurp = new slurping (new elf_in (fd, e));

  bool ok = true;
  if (!from ()->get_error ())
    {
      announce ("importing");
      loadedness = ML_CONFIG;
      lazy_open++;
      ok = read_initial (reader);
      slurp->lru = ++lazy_lru;
    }

  gcc_assert (slurp->current == ~0u);

  return check_read (outermost, ok);
}

/* Attempt to increase the file descriptor limit.  */

static bool
try_increase_lazy (unsigned want)
{
  gcc_checking_assert (lazy_open >= lazy_limit);

  /* If we're increasing, saturate at hard limit.  */
  if (want > lazy_hard_limit && lazy_limit < lazy_hard_limit)
    want = lazy_hard_limit;

#if HAVE_SETRLIMIT
  if ((!lazy_limit || !param_lazy_modules)
      && lazy_hard_limit
      && want <= lazy_hard_limit)
    {
      struct rlimit rlimit;
      rlimit.rlim_cur = want + LAZY_HEADROOM;
      rlimit.rlim_max = lazy_hard_limit + LAZY_HEADROOM;
      if (!setrlimit (RLIMIT_NOFILE, &rlimit))
	lazy_limit = want;
    }
#endif

  return lazy_open < lazy_limit;
}

/* Pick a victim module to freeze its reader.  */

void
module_state::freeze_an_elf ()
{
  if (try_increase_lazy (lazy_open * 2))
    return;

  module_state *victim = NULL;
  for (unsigned ix = modules->length (); ix--;)
    {
      module_state *candidate = (*modules)[ix];
      if (candidate && candidate->slurp && candidate->slurp->lru
	  && candidate->from ()->is_freezable ()
	  && (!victim || victim->slurp->lru > candidate->slurp->lru))
	victim = candidate;
    }

  if (victim)
    {
      dump () && dump ("Freezing '%s'", victim->filename);
      if (victim->slurp->macro_defs.size)
	/* Save the macro definitions to a buffer.  */
	victim->from ()->preserve (victim->slurp->macro_defs);
      if (victim->slurp->macro_tbl.size)
	/* Save the macro definitions to a buffer.  */
	victim->from ()->preserve (victim->slurp->macro_tbl);
      victim->from ()->freeze ();
      lazy_open--;
    }
  else
    dump () && dump ("No module available for freezing");
}

/* Load the lazy slot *MSLOT, INDEX'th slot of the module.  */

bool
module_state::lazy_load (unsigned index, binding_slot *mslot)
{
  unsigned n = dump.push (this);

  gcc_checking_assert (function_depth);

  unsigned cookie = mslot->get_lazy ();
  unsigned snum = cookie >> 2;
  dump () && dump ("Loading entity %M[%u] section:%u", this, index, snum);

  bool ok = load_section (snum, mslot);

  dump.pop (n);

  return ok;
}

/* Load MOD's binding for NS::ID into *MSLOT.  *MSLOT contains the
   lazy cookie.  OUTER is true if this is the outermost lazy, (used
   for diagnostics).  */

void
lazy_load_binding (unsigned mod, tree ns, tree id, binding_slot *mslot)
{
  int count = errorcount + warningcount;

  bool timer_running = timevar_cond_start (TV_MODULE_IMPORT);

  /* Make sure lazy loading from a template context behaves as if
     from a non-template context.  */
  processing_template_decl_sentinel ptds;

  /* Stop GC happening, even in outermost loads (because our caller
     could well be building up a lookup set).  */
  function_depth++;

  gcc_checking_assert (mod);
  module_state *module = (*modules)[mod];
  unsigned n = dump.push (module);

  unsigned snum = mslot->get_lazy ();
  dump () && dump ("Lazily binding %P@%N section:%u", ns, id,
		   module->name, snum);

  bool ok = !recursive_lazy (snum);
  if (ok)
    {
      ok = module->load_section (snum, mslot);
      lazy_snum = 0;
      post_load_processing ();
    }

  dump.pop (n);

  function_depth--;

  timevar_cond_stop (TV_MODULE_IMPORT, timer_running);

  if (!ok)
    fatal_error (input_location,
		 module->is_header ()
		 ? G_("failed to load binding %<%E%s%E%>")
		 : G_("failed to load binding %<%E%s%E@%s%>"),
		 ns, &"::"[ns == global_namespace ? 2 : 0], id,
		 module->get_flatname ());

  if (count != errorcount + warningcount)
    inform (input_location,
	    module->is_header ()
	    ? G_("during load of binding %<%E%s%E%>")
	    : G_("during load of binding %<%E%s%E@%s%>"),
	    ns, &"::"[ns == global_namespace ? 2 : 0], id,
	    module->get_flatname ());
}

/* Load any pending entities keyed to the top-key of DECL.  */

void
lazy_load_pendings (tree decl)
{
  /* Make sure lazy loading from a template context behaves as if
     from a non-template context.  */
  processing_template_decl_sentinel ptds;

  tree key_decl;
  pending_key key;
  key.ns = find_pending_key (decl, &key_decl);
  key.id = DECL_NAME (key_decl);

  auto *pending_vec = pending_table ? pending_table->get (key) : nullptr;
  if (!pending_vec)
    return;

  int count = errorcount + warningcount;

  bool timer_running = timevar_cond_start (TV_MODULE_IMPORT);
  bool ok = !recursive_lazy ();
  if (ok)
    {
      function_depth++; /* Prevent GC */
      unsigned n = dump.push (NULL);
      dump () && dump ("Reading %u pending entities keyed to %P",
		       pending_vec->length (), key.ns, key.id);
      for (unsigned ix = pending_vec->length (); ix--;)
	{
	  unsigned index = (*pending_vec)[ix];
	  binding_slot *slot = &(*entity_ary)[index];

	  if (slot->is_lazy ())
	    {
	      module_state *import = import_entity_module (index);
	      if (!import->lazy_load (index - import->entity_lwm, slot))
		ok = false;
	    }
	  else if (dump ())
	    {
	      module_state *import = import_entity_module (index);
	      dump () && dump ("Entity %M[%u] already loaded",
			       import, index - import->entity_lwm);
	    }
	}

      pending_table->remove (key);
      dump.pop (n);
      lazy_snum = 0;
      post_load_processing ();
      function_depth--;
    }

  timevar_cond_stop (TV_MODULE_IMPORT, timer_running);

  if (!ok)
    fatal_error (input_location, "failed to load pendings for %<%E%s%E%>",
		 key.ns, &"::"[key.ns == global_namespace ? 2 : 0], key.id);

  if (count != errorcount + warningcount)
    inform (input_location, "during load of pendings for %<%E%s%E%>",
	    key.ns, &"::"[key.ns == global_namespace ? 2 : 0], key.id);
}

static void
direct_import (module_state *import, cpp_reader *reader)
{
  timevar_start (TV_MODULE_IMPORT);
  unsigned n = dump.push (import);

  gcc_checking_assert (import->is_direct () && import->has_location ());
  if (import->loadedness == ML_NONE)
    if (!import->do_import (reader, true))
      gcc_unreachable ();

  if (import->loadedness < ML_LANGUAGE)
    {
      if (!keyed_table)
	keyed_table = new keyed_map_t (EXPERIMENT (1, 400));
      import->read_language (true);
    }

  (*modules)[0]->set_import (import, import->exported_p);

  dump.pop (n);
  timevar_stop (TV_MODULE_IMPORT);
}

/* Import module IMPORT.  */

void
import_module (module_state *import, location_t from_loc, bool exporting_p,
	       tree, cpp_reader *reader)
{
  if (!import->check_not_purview (from_loc))
    return;

  if (!import->is_header () && current_lang_depth ())
    /* Only header units should appear inside language
       specifications.  The std doesn't specify this, but I think
       that's an error in resolving US 033, because language linkage
       is also our escape clause to getting things into the global
       module, so we don't want to confuse things by having to think
       about whether 'extern "C++" { import foo; }' puts foo's
       contents into the global module all of a sudden.  */
    warning (0, "import of named module %qs inside language-linkage block",
	     import->get_flatname ());

  if (exporting_p || module_exporting_p ())
    import->exported_p = true;

  if (import->loadedness != ML_NONE)
    {
      from_loc = ordinary_loc_of (line_table, from_loc);
      linemap_module_reparent (line_table, import->loc, from_loc);
    }
  gcc_checking_assert (!import->module_p);
  gcc_checking_assert (import->is_direct () && import->has_location ());

  direct_import (import, reader);
}

/* Declare the name of the current module to be NAME.  EXPORTING_p is
   true if this TU is the exporting module unit.  */

void
declare_module (module_state *module, location_t from_loc, bool exporting_p,
		tree, cpp_reader *reader)
{
  gcc_assert (global_namespace == current_scope ());

  module_state *current = (*modules)[0];
  if (module_purview_p () || module->loadedness > ML_CONFIG)
    {
      auto_diagnostic_group d;
      error_at (from_loc, module_purview_p ()
		? G_("module already declared")
		: G_("module already imported"));
      if (module_purview_p ())
	module = current;
      inform (module->loc, module_purview_p ()
	      ? G_("module %qs declared here")
	      : G_("module %qs imported here"),
	      module->get_flatname ());
      return;
    }

  gcc_checking_assert (module->module_p);
  gcc_checking_assert (module->is_direct () && module->has_location ());

  /* Yer a module, 'arry.  */
  module_kind = module->is_header () ? MK_HEADER : MK_NAMED | MK_ATTACH;

  // Even in header units, we consider the decls to be purview
  module_kind |= MK_PURVIEW;

  if (module->is_partition ())
    module_kind |= MK_PARTITION;
  if (exporting_p)
    {
      module->interface_p = true;
      module_kind |= MK_INTERFACE;
    }

  if (module_has_cmi_p ())
    {
      /* Copy the importing information we may have already done.  We
	 do not need to separate out the imports that only happen in
	 the GMF, inspite of what the literal wording of the std
	 might imply.  See p2191, the core list had a discussion
	 where the module implementors agreed that the GMF of a named
	 module is invisible to importers.  */
      module->imports = current->imports;

      module->mod = 0;
      (*modules)[0] = module;
    }
  else
    {
      module->interface_p = true;
      current->parent = module; /* So mangler knows module identity. */
      direct_import (module, reader);
    }
}

/* Return true IFF we must emit a module global initializer function
   (which will be called by importers' init code).  */

bool
module_global_init_needed ()
{
  return module_has_cmi_p () && !header_module_p ();
}

/* Calculate which, if any, import initializers need calling.  */

bool
module_determine_import_inits ()
{
  if (!modules || header_module_p ())
    return false;

  /* Prune active_init_p.  We need the same bitmap allocation
     scheme as for the imports member.  */
  function_depth++; /* Disable GC.  */
  bitmap covered_imports (BITMAP_GGC_ALLOC ());

  bool any = false;

  /* Because indirect imports are before their direct import, and
     we're scanning the array backwards, we only need one pass!  */
  for (unsigned ix = modules->length (); --ix;)
    {
      module_state *import = (*modules)[ix];

      if (!import->active_init_p)
	;
      else if (bitmap_bit_p (covered_imports, ix))
	import->active_init_p = false;
      else
	{
	  /* Everything this imports is therefore handled by its
	     initializer, so doesn't need initializing by us.  */
	  bitmap_ior_into (covered_imports, import->imports);
	  any = true;
	}
    }
  function_depth--;

  return any;
}

/* Emit calls to each direct import's global initializer.  Including
   direct imports of directly imported header units.  The initializers
   of (static) entities in header units will be called by their
   importing modules (for the instance contained within that), or by
   the current TU (for the instances we've brought in).  Of course
   such header unit behaviour is evil, but iostream went through that
   door some time ago.  */

void
module_add_import_initializers ()
{
  if (!modules || header_module_p ())
    return;

  tree fntype = build_function_type (void_type_node, void_list_node);
  releasing_vec args;  // There are no args

  for (unsigned ix = modules->length (); --ix;)
    {
      module_state *import = (*modules)[ix];
      if (import->active_init_p)
	{
	  tree name = mangle_module_global_init (ix);
	  tree fndecl = build_lang_decl (FUNCTION_DECL, name, fntype);

	  DECL_CONTEXT (fndecl) = FROB_CONTEXT (global_namespace);
	  SET_DECL_ASSEMBLER_NAME (fndecl, name);
	  TREE_PUBLIC (fndecl) = true;
	  determine_visibility (fndecl);

	  tree call = cp_build_function_call_vec (fndecl, &args,
						  tf_warning_or_error);
	  finish_expr_stmt (call);
	}
    }
}

/* NAME & LEN are a preprocessed header name, possibly including the
   surrounding "" or <> characters.  Return the raw string name of the
   module to which it refers.  This will be an absolute path, or begin
   with ./, so it is immediately distinguishable from a (non-header
   unit) module name.  If READER is non-null, ask the preprocessor to
   locate the header to which it refers using the appropriate include
   path.  Note that we do never do \ processing of the string, as that
   matches the preprocessor's behaviour.  */

static const char *
canonicalize_header_name (cpp_reader *reader, location_t loc, bool unquoted,
			  const char *str, size_t &len_r)
{
  size_t len = len_r;
  static char *buf = 0;
  static size_t alloc = 0;

  if (!unquoted)
    {
      gcc_checking_assert (len >= 2
			   && ((reader && str[0] == '<' && str[len-1] == '>')
			       || (str[0] == '"' && str[len-1] == '"')));
      str += 1;
      len -= 2;
    }

  if (reader)
    {
      gcc_assert (!unquoted);

      if (len >= alloc)
	{
	  alloc = len + 1;
	  buf = XRESIZEVEC (char, buf, alloc);
	}
      memcpy (buf, str, len);
      buf[len] = 0;

      if (const char *hdr
	  = cpp_probe_header_unit (reader, buf, str[-1] == '<', loc))
	{
	  len = strlen (hdr);
	  str = hdr;
	}
      else
	str = buf;
    }

  if (!(str[0] == '.' ? IS_DIR_SEPARATOR (str[1]) : IS_ABSOLUTE_PATH (str)))
    {
      /* Prepend './'  */
      if (len + 3 > alloc)
	{
	  alloc = len + 3;
	  buf = XRESIZEVEC (char, buf, alloc);
	}

      buf[0] = '.';
      buf[1] = DIR_SEPARATOR;
      memmove (buf + 2, str, len);
      len += 2;
      buf[len] = 0;
      str = buf;
    }

  len_r = len;
  return str;
}

/* Set the CMI name from a cody packet.  Issue an error if
   ill-formed.  */

void module_state::set_filename (const Cody::Packet &packet)
{
  if (packet.GetCode () == Cody::Client::PC_PATHNAME)
    {
      /* If we've seen this import before we better have the same CMI.  */
      const std::string &path = packet.GetString ();
      if (!filename)
	filename = xstrdup (packet.GetString ().c_str ());
      else if (filename != path)
	error_at (loc, "mismatching compiled module interface: "
		  "had %qs, got %qs", filename, path.c_str ());
    }
  else
    {
      gcc_checking_assert (packet.GetCode () == Cody::Client::PC_ERROR);
      fatal_error (loc, "unknown compiled module interface: %s",
		   packet.GetString ().c_str ());
    }
}

/* Figure out whether to treat HEADER as an include or an import.  */

static char *
maybe_translate_include (cpp_reader *reader, line_maps *lmaps, location_t loc,
			 const char *path)
{
  if (!modules_p ())
    {
      /* Turn off.  */
      cpp_get_callbacks (reader)->translate_include = NULL;
      return nullptr;
    }

  dump.push (NULL);

  dump () && dump ("Checking include translation '%s'", path);
  auto *mapper = get_mapper (cpp_main_loc (reader), cpp_get_deps (reader));

  size_t len = strlen (path);
  path = canonicalize_header_name (NULL, loc, true, path, len);
  auto packet = mapper->IncludeTranslate (path, Cody::Flags::None, len);

  enum class xlate_kind {
    unknown, text, import,
  } translate = xlate_kind::unknown;

  if (packet.GetCode () == Cody::Client::PC_BOOL)
    translate = packet.GetInteger () ? xlate_kind::text : xlate_kind::unknown;
  else if (packet.GetCode () == Cody::Client::PC_PATHNAME)
    {
      /* Record the CMI name for when we do the import.
	 We may already know about this import, but libcpp doesn't yet.  */
      module_state *import = get_module (build_string (len, path));
      import->set_filename (packet);
      translate = xlate_kind::import;
    }
  else
    {
      gcc_checking_assert (packet.GetCode () == Cody::Client::PC_ERROR);
      error_at (loc, "cannot determine %<#include%> translation of %s: %s",
		path, packet.GetString ().c_str ());
    }

  bool note = false;
  if (note_include_translate_yes && translate == xlate_kind::import)
    note = true;
  else if (note_include_translate_no && translate == xlate_kind::unknown)
    note = true;
  else if (note_includes)
    /* We do not expect the note_includes vector to be large, so O(N)
       iteration.  */
    for (unsigned ix = note_includes->length (); !note && ix--;)
      if (!strcmp ((*note_includes)[ix], path))
	note = true;

  if (note)
    inform (loc, translate == xlate_kind::import
	    ? G_("include %qs translated to import")
	    : G_("include %qs processed textually"), path);

  dump () && dump (translate == xlate_kind::import
		   ? "Translating include to import"
		   : "Keeping include as include");
  dump.pop (0);

  if (translate != xlate_kind::import)
    return nullptr;

  /* Create the translation text.  */
  loc = ordinary_loc_of (lmaps, loc);
  const line_map_ordinary *map
    = linemap_check_ordinary (linemap_lookup (lmaps, loc));
  unsigned col = SOURCE_COLUMN (map, loc);
  col -= (col != 0); /* Columns are 1-based.  */

  unsigned alloc = len + col + 60;
  char *res = XNEWVEC (char, alloc);

  strcpy (res, "__import");
  unsigned actual = 8;
  if (col > actual)
    {
      /* Pad out so the filename appears at the same position.  */
      memset (res + actual, ' ', col - actual);
      actual = col;
    }
  /* No need to encode characters, that's not how header names are
     handled.  */
  actual += snprintf (res + actual, alloc - actual,
		      "\"%s\" [[__translated]];\n", path);
  gcc_checking_assert (actual < alloc);

  /* cpplib will delete the buffer.  */
  return res;
}

static void
begin_header_unit (cpp_reader *reader)
{
  /* Set the module header name from the main_input_filename.  */
  const char *main = main_input_filename;
  size_t len = strlen (main);
  main = canonicalize_header_name (NULL, 0, true, main, len);
  module_state *module = get_module (build_string (len, main));

  preprocess_module (module, cpp_main_loc (reader), false, false, true, reader);
}

/* We've just properly entered the main source file.  I.e. after the
   command line, builtins and forced headers.  Record the line map and
   location of this map.  Note we may be called more than once.  The
   first call sticks.  */

void
module_begin_main_file (cpp_reader *reader, line_maps *lmaps,
		       const line_map_ordinary *map)
{
  gcc_checking_assert (lmaps == line_table);
  if (modules_p () && !spans.init_p ())
    {
      unsigned n = dump.push (NULL);
      spans.init (lmaps, map);
      dump.pop (n);
      if (flag_header_unit && !cpp_get_options (reader)->preprocessed)
	{
	  /* Tell the preprocessor this is an include file.  */
	  cpp_retrofit_as_include (reader);
	  begin_header_unit (reader);
	}
    }
}

/* Process the pending_import queue, making sure we know the
   filenames.   */

static void
name_pending_imports (cpp_reader *reader)
{
  auto *mapper = get_mapper (cpp_main_loc (reader), cpp_get_deps (reader));

  if (!vec_safe_length (pending_imports))
    /* Not doing anything.  */
    return;

  timevar_start (TV_MODULE_MAPPER);

  auto n = dump.push (NULL);
  dump () && dump ("Resolving direct import names");
  bool want_deps = (bool (mapper->get_flags () & Cody::Flags::NameOnly)
		    || cpp_get_deps (reader));
  bool any = false;

  for (unsigned ix = 0; ix != pending_imports->length (); ix++)
    {
      module_state *module = (*pending_imports)[ix];
      gcc_checking_assert (module->is_direct ());
      if (!module->filename && !module->visited_p)
	{
	  bool export_p = (module->module_p
			   && (module->is_partition () || module->exported_p));

	  Cody::Flags flags = Cody::Flags::None;
	  if (flag_preprocess_only
	      && !(module->is_header () && !export_p))
	    {
	      if (!want_deps)
		continue;
	      flags = Cody::Flags::NameOnly;
	    }

	  if (!any)
	    {
	      any = true;
	      mapper->Cork ();
	    }
	  if (export_p)
	    mapper->ModuleExport (module->get_flatname (), flags);
	  else
	    mapper->ModuleImport (module->get_flatname (), flags);
	  module->visited_p = true;
	}
    }

  if (any)
    {
      auto response = mapper->Uncork ();
      auto r_iter = response.begin ();
      for (unsigned ix = 0; ix != pending_imports->length (); ix++)
	{
	  module_state *module = (*pending_imports)[ix];
	  if (module->visited_p)
	    {
	      module->visited_p = false;
	      gcc_checking_assert (!module->filename);

	      module->set_filename (*r_iter);
	      ++r_iter;
	    }
	}
    }

  dump.pop (n);

  timevar_stop (TV_MODULE_MAPPER);
}

/* We've just lexed a module-specific control line for MODULE.  Mark
   the module as a direct import, and possibly load up its macro
   state.  Returns the primary module, if this is a module
   declaration.  */
/* Perhaps we should offer a preprocessing mode where we read the
   directives from the header unit, rather than require the header's
   CMI.  */

module_state *
preprocess_module (module_state *module, location_t from_loc,
		   bool in_purview, bool is_import, bool is_export,
		   cpp_reader *reader)
{
  if (!is_import)
    {
      if (module->loc)
	/* It's already been mentioned, so ignore its module-ness.  */
	is_import = true;
      else
	{
	  /* Record it is the module.  */
	  module->module_p = true;
	  if (is_export)
	    {
	      module->exported_p = true;
	      module->interface_p = true;
	    }
	}
    }

  if (module->directness < MD_DIRECT + in_purview)
    {
      /* Mark as a direct import.  */
      module->directness = module_directness (MD_DIRECT + in_purview);

      /* Set the location to be most informative for users.  */
      from_loc = ordinary_loc_of (line_table, from_loc);
      if (module->loadedness != ML_NONE)
	linemap_module_reparent (line_table, module->loc, from_loc);
      else
	{
	  module->loc = from_loc;
	  if (!module->flatname)
	    module->set_flatname ();
	}
    }

  auto desired = ML_CONFIG;
  if (is_import
      && module->is_header ()
      && (!cpp_get_options (reader)->preprocessed
	  || cpp_get_options (reader)->directives_only))
    /* We need preprocessor state now.  */
    desired = ML_PREPROCESSOR;

  if (!is_import || module->loadedness < desired)
    {
      vec_safe_push (pending_imports, module);

      if (desired == ML_PREPROCESSOR)
	{
	  unsigned n = dump.push (NULL);

	  dump () && dump ("Reading %M preprocessor state", module);
	  name_pending_imports (reader);

	  /* Preserve the state of the line-map.  */
	  auto pre_hwm = LINEMAPS_ORDINARY_USED (line_table);

	  /* We only need to close the span, if we're going to emit a
	     CMI.  But that's a little tricky -- our token scanner
	     needs to be smarter -- and this isn't much state.
	     Remember, we've not parsed anything at this point, so
	     our module state flags are inadequate.  */
	  spans.maybe_init ();
	  spans.close ();

	  timevar_start (TV_MODULE_IMPORT);

	  /* Load the config of each pending import -- we must assign
	     module numbers monotonically.  */
	  for (unsigned ix = 0; ix != pending_imports->length (); ix++)
	    {
	      auto *import = (*pending_imports)[ix];
	      if (!(import->module_p
		    && (import->is_partition () || import->exported_p))
		  && import->loadedness == ML_NONE
		  && (import->is_header () || !flag_preprocess_only))
		{
		  unsigned n = dump.push (import);
		  import->do_import (reader, true);
		  dump.pop (n);
		}
	    }
	  vec_free (pending_imports);

	  /* Restore the line-map state.  */
	  spans.open (linemap_module_restore (line_table, pre_hwm));

	  /* Now read the preprocessor state of this particular
	     import.  */
	  if (module->loadedness == ML_CONFIG
	      && module->read_preprocessor (true))
	    module->import_macros ();

	  timevar_stop (TV_MODULE_IMPORT);

	  dump.pop (n);
	}
    }

  return is_import ? NULL : get_primary (module);
}

/* We've completed phase-4 translation.  Emit any dependency
   information for the not-yet-loaded direct imports, and fill in
   their file names.  We'll have already loaded up the direct header
   unit wavefront.  */

void
preprocessed_module (cpp_reader *reader)
{
  unsigned n = dump.push (NULL);

  dump () && dump ("Completed phase-4 (tokenization) processing");

  name_pending_imports (reader);
  vec_free (pending_imports);

  spans.maybe_init ();
  spans.close ();

  using iterator = hash_table<module_state_hash>::iterator;
  if (mkdeps *deps = cpp_get_deps (reader))
    {
      /* Walk the module hash, informing the dependency machinery.  */
      iterator end = modules_hash->end ();
      for (iterator iter = modules_hash->begin (); iter != end; ++iter)
	{
	  module_state *module = *iter;

	  if (module->is_direct ())
	    {
	      if (module->is_module ()
		  && (module->is_interface () || module->is_partition ()))
		deps_add_module_target (deps, module->get_flatname (),
					maybe_add_cmi_prefix (module->filename),
					module->is_header (),
					module->is_exported ());
	      else
		deps_add_module_dep (deps, module->get_flatname ());
	    }
	}
    }

  if (flag_header_unit && !flag_preprocess_only)
    {
      /* Find the main module -- remember, it's not yet in the module
	 array.  */
      iterator end = modules_hash->end ();
      for (iterator iter = modules_hash->begin (); iter != end; ++iter)
	{
	  module_state *module = *iter;
	  if (module->is_module ())
	    {
	      declare_module (module, cpp_main_loc (reader), true, NULL, reader);
	      module_kind |= MK_EXPORTING;
	      break;
	    }
	}
    }

  dump.pop (n);
}

/* VAL is a global tree, add it to the global vec if it is
   interesting.  Add some of its targets, if they too are
   interesting.  We do not add identifiers, as they can be re-found
   via the identifier hash table.  There is a cost to the number of
   global trees.  */

static int
maybe_add_global (tree val, unsigned &crc)
{
  int v = 0;

  if (val && !(identifier_p (val) || TREE_VISITED (val)))
    {
      TREE_VISITED (val) = true;
      crc = crc32_unsigned (crc, fixed_trees->length ());
      vec_safe_push (fixed_trees, val);
      v++;

      if (CODE_CONTAINS_STRUCT (TREE_CODE (val), TS_TYPED))
	v += maybe_add_global (TREE_TYPE (val), crc);
      if (CODE_CONTAINS_STRUCT (TREE_CODE (val), TS_TYPE_COMMON))
	v += maybe_add_global (TYPE_NAME (val), crc);
    }

  return v;
}

/* Initialize module state.  Create the hash table, determine the
   global trees.  Create the module for current TU.  */

void
init_modules (cpp_reader *reader)
{
  /* PCH should not be reachable because of lang-specs, but the
     user could have overriden that.  */
  if (pch_file)
    fatal_error (input_location,
		 "C++ modules are incompatible with precompiled headers");

  if (cpp_get_options (reader)->traditional)
    fatal_error (input_location,
		 "C++ modules are incompatible with traditional preprocessing");

  /* :: is always exported.  */
  DECL_MODULE_EXPORT_P (global_namespace) = true;

  modules_hash = hash_table<module_state_hash>::create_ggc (31);
  vec_safe_reserve (modules, 20);

  /* Create module for current TU.  */
  module_state *current
    = new (ggc_alloc<module_state> ()) module_state (NULL_TREE, NULL, false);
  current->mod = 0;
  bitmap_set_bit (current->imports, 0);
  modules->quick_push (current);

  gcc_checking_assert (!fixed_trees);

  headers = BITMAP_GGC_ALLOC ();

  if (note_includes)
    /* Canonicalize header names.  */
    for (unsigned ix = 0; ix != note_includes->length (); ix++)
      {
	const char *hdr = (*note_includes)[ix];
	size_t len = strlen (hdr);

	bool system = hdr[0] == '<';
	bool user = hdr[0] == '"';
	bool delimed = system || user;

	if (len <= (delimed ? 2 : 0)
	    || (delimed && hdr[len-1] != (system ? '>' : '"')))
	  error ("invalid header name %qs", hdr);

	hdr = canonicalize_header_name (delimed ? reader : NULL,
					0, !delimed, hdr, len);
	char *path = XNEWVEC (char, len + 1);
	memcpy (path, hdr, len);
	path[len] = 0;

	(*note_includes)[ix] = path;
      }

  if (note_cmis)
    /* Canonicalize & mark module names.  */
    for (unsigned ix = 0; ix != note_cmis->length (); ix++)
      {
	const char *name = (*note_cmis)[ix];
	size_t len = strlen (name);

	bool is_system = name[0] == '<';
	bool is_user = name[0] == '"';
	bool is_pathname = false;
	if (!(is_system || is_user))
	  for (unsigned ix = len; !is_pathname && ix--;)
	    is_pathname = IS_DIR_SEPARATOR (name[ix]);
	if (is_system || is_user || is_pathname)
	  {
	    if (len <= (is_pathname ? 0 : 2)
		|| (!is_pathname && name[len-1] != (is_system ? '>' : '"')))
	      {
		error ("invalid header name %qs", name);
		continue;
	      }
	    else
	      name = canonicalize_header_name (is_pathname ? nullptr : reader,
					       0, is_pathname, name, len);
	  }
	if (auto module = get_module (name))
	  module->inform_cmi_p = 1;
	else
	  error ("invalid module name %qs", name);
      }

  dump.push (NULL);

  /* Determine lazy handle bound.  */
  {
    unsigned limit = 1000;
#if HAVE_GETRLIMIT
    struct rlimit rlimit;
    if (!getrlimit (RLIMIT_NOFILE, &rlimit))
      {
	lazy_hard_limit = (rlimit.rlim_max < 1000000
			   ? unsigned (rlimit.rlim_max) : 1000000);
	lazy_hard_limit = (lazy_hard_limit > LAZY_HEADROOM
			   ? lazy_hard_limit - LAZY_HEADROOM : 0);
	if (rlimit.rlim_cur < limit)
	  limit = unsigned (rlimit.rlim_cur);
      }
#endif
    limit = limit > LAZY_HEADROOM ? limit - LAZY_HEADROOM : 1;

    if (unsigned parm = param_lazy_modules)
      {
	if (parm <= limit || !lazy_hard_limit || !try_increase_lazy (parm))
	  lazy_limit = parm;
      }
    else
      lazy_limit = limit;
  }

  if (dump ())
    {
      verstr_t ver;
      version2string (MODULE_VERSION, ver);
      dump ("Source: %s", main_input_filename);
      dump ("Compiler: %s", version_string);
      dump ("Modules: %s", ver);
      dump ("Checking: %s",
#if CHECKING_P
	    "checking"
#elif ENABLE_ASSERT_CHECKING
	    "asserting"
#else
	    "release"
#endif
	    );
      dump ("Compiled by: "
#ifdef __GNUC__
	    "GCC %d.%d, %s", __GNUC__, __GNUC_MINOR__,
#ifdef __OPTIMIZE__
	    "optimizing"
#else
	    "not optimizing"
#endif
#else
	    "not GCC"
#endif
	    );
      dump ("Reading: %s", MAPPED_READING ? "mmap" : "fileio");
      dump ("Writing: %s", MAPPED_WRITING ? "mmap" : "fileio");
      dump ("Lazy limit: %u", lazy_limit);
      dump ("Lazy hard limit: %u", lazy_hard_limit);
      dump ("");
    }

  /* Construct the global tree array.  This is an array of unique
     global trees (& types).  Do this now, rather than lazily, as
     some global trees are lazily created and we don't want that to
     mess with our syndrome of fixed trees.  */
  unsigned crc = 0;
  vec_alloc (fixed_trees, 250);

  dump () && dump ("+Creating globals");
  /* Insert the TRANSLATION_UNIT_DECL.  */
  TREE_VISITED (DECL_CONTEXT (global_namespace)) = true;
  fixed_trees->quick_push (DECL_CONTEXT (global_namespace));
  for (unsigned jx = 0; global_tree_arys[jx].first; jx++)
    {
      const tree *ptr = global_tree_arys[jx].first;
      unsigned limit = global_tree_arys[jx].second;

      for (unsigned ix = 0; ix != limit; ix++, ptr++)
	{
	  !(ix & 31) && dump ("") && dump ("+\t%u:%u:", jx, ix);
	  unsigned v = maybe_add_global (*ptr, crc);
	  dump () && dump ("+%u", v);
	}
    }
  /* OS- and machine-specific types are dynamically registered at
     runtime, so cannot be part of global_tree_arys.  */
  registered_builtin_types && dump ("") && dump ("+\tB:");
  for (tree t = registered_builtin_types; t; t = TREE_CHAIN (t))
    {
      unsigned v = maybe_add_global (TREE_VALUE (t), crc);
      dump () && dump ("+%u", v);
    }
  global_crc = crc32_unsigned (crc, fixed_trees->length ());
  dump ("") && dump ("Created %u unique globals, crc=%x",
		     fixed_trees->length (), global_crc);
  for (unsigned ix = fixed_trees->length (); ix--;)
    TREE_VISITED ((*fixed_trees)[ix]) = false;

  dump.pop (0);

  if (!flag_module_lazy)
    /* Get the mapper now, if we're not being lazy.  */
    get_mapper (cpp_main_loc (reader), cpp_get_deps (reader));

  if (!flag_preprocess_only)
    {
      pending_table = new pending_map_t (EXPERIMENT (1, 400));
      entity_map = new entity_map_t (EXPERIMENT (1, 400));
      vec_safe_reserve (entity_ary, EXPERIMENT (1, 400));
      imported_temploid_friends
	= decl_tree_cache_map::create_ggc (EXPERIMENT (1, 400));
    }

#if CHECKING_P
  note_defs = note_defs_table_t::create_ggc (1000);
#endif

  if (flag_header_unit && cpp_get_options (reader)->preprocessed)
    begin_header_unit (reader);

  /* Collect here to make sure things are tagged correctly (when
     aggressively GC'd).  */
  ggc_collect ();
}

/* If NODE is a deferred macro, load it.  */

static int
load_macros (cpp_reader *reader, cpp_hashnode *node, void *)
{
  location_t main_loc
    = MAP_START_LOCATION (LINEMAPS_ORDINARY_MAP_AT (line_table, 0));

  if (cpp_user_macro_p (node)
      && !node->value.macro)
    {
      cpp_macro *macro = cpp_get_deferred_macro (reader, node, main_loc);
      dump () && dump ("Loaded macro #%s %I",
		       macro ? "define" : "undef", identifier (node));
    }

  return 1;
}

/* At the end of tokenizing, we no longer need the macro tables of
   imports.  But the user might have requested some checking.  */

void
maybe_check_all_macros (cpp_reader *reader)
{
  if (!warn_imported_macros)
    return;

  /* Force loading of any remaining deferred macros.  This will
     produce diagnostics if they are ill-formed.  */
  unsigned n = dump.push (NULL);
  cpp_forall_identifiers (reader, load_macros, NULL);
  dump.pop (n);
}

// State propagated from finish_module_processing to fini_modules

struct module_processing_cookie
{
  elf_out out;
  module_state_config config;
  char *cmi_name;
  char *tmp_name;
  unsigned crc;
  bool began;

  module_processing_cookie (char *cmi, char *tmp, int fd, int e)
    : out (fd, e), cmi_name (cmi), tmp_name (tmp), crc (0), began (false)
  {
  }
  ~module_processing_cookie ()
  {
    XDELETEVEC (tmp_name);
    XDELETEVEC (cmi_name);
  }
};

/* Write the CMI, if we're a module interface.  */

void *
finish_module_processing (cpp_reader *reader)
{
  module_processing_cookie *cookie = nullptr;

  if (header_module_p ())
    module_kind &= ~MK_EXPORTING;

  if (!modules || !(*modules)[0]->name)
    {
      if (flag_module_only)
	warning (0, "%<-fmodule-only%> used for non-interface");
    }
  else if (!flag_syntax_only)
    {
      int fd = -1;
      int e = -1;

      timevar_start (TV_MODULE_EXPORT);

      /* Force a valid but empty line map at the end.  This simplifies
	 the line table preparation and writing logic.  */
      linemap_add (line_table, LC_ENTER, false, "", 0);

      /* We write to a tmpname, and then atomically rename.  */
      char *cmi_name = NULL;
      char *tmp_name = NULL;
      module_state *state = (*modules)[0];

      unsigned n = dump.push (state);
      state->announce ("creating");
      if (state->filename)
	{
	  size_t len = 0;
	  cmi_name = xstrdup (maybe_add_cmi_prefix (state->filename, &len));
	  tmp_name = XNEWVEC (char, len + 3);
	  memcpy (tmp_name, cmi_name, len);
	  strcpy (&tmp_name[len], "~");

	  if (!errorcount)
	    for (unsigned again = 2; ; again--)
	      {
		fd = open (tmp_name,
			   O_RDWR | O_CREAT | O_TRUNC | O_CLOEXEC | O_BINARY,
			   S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
		e = errno;
		if (fd >= 0 || !again || e != ENOENT)
		  break;
		create_dirs (tmp_name);
	      }
	  if (note_module_cmi_yes || state->inform_cmi_p)
	    inform (state->loc, "writing CMI %qs", cmi_name);
	  dump () && dump ("CMI is %s", cmi_name);
	}

      cookie = new module_processing_cookie (cmi_name, tmp_name, fd, e);

      if (errorcount)
	/* Don't write the module if we have reported errors.  */;
      else if (erroneous_templates
	       && !erroneous_templates->is_empty ())
	{
	  /* Don't write the module if it contains an erroneous template.
	     Also emit notes about where errors occurred in case
	     -Wno-template-body was passed.  */
	  auto_diagnostic_group d;
	  error_at (state->loc, "not writing module %qs due to errors "
		    "in template bodies", state->get_flatname ());
	  if (!warn_template_body)
	    inform (state->loc, "enable %<-Wtemplate-body%> for more details");
	  for (auto e : *erroneous_templates)
	    inform (e.second, "first error in %qD appeared here", e.first);
	}
      else if (cookie->out.begin ())
	{
	  /* So crashes finger-point the module decl.  */
	  iloc_sentinel ils = state->loc;
	  if (state->write_begin (&cookie->out, reader, cookie->config,
				  cookie->crc))
	    cookie->began = true;
	}

      dump.pop (n);
      timevar_stop (TV_MODULE_EXPORT);

      ggc_collect ();
    }

  if (modules)
    {
      unsigned n = dump.push (NULL);
      dump () && dump ("Imported %u modules", modules->length () - 1);
      dump () && dump ("Containing %u clusters", available_clusters);
      dump () && dump ("Loaded %u clusters (%u%%)", loaded_clusters,
		       (loaded_clusters * 100 + available_clusters / 2) /
		       (available_clusters + !available_clusters));
      dump.pop (n);
    }

  return cookie;
}

// Do the final emission of a module.  At this point we know whether
// the module static initializer is a NOP or not.

static void
late_finish_module (cpp_reader *reader,  module_processing_cookie *cookie,
		    bool init_fn_non_empty)
{
  timevar_start (TV_MODULE_EXPORT);

  module_state *state = (*modules)[0];
  unsigned n = dump.push (state);
  state->announce ("finishing");

  cookie->config.active_init = init_fn_non_empty;
  if (cookie->began)
    state->write_end (&cookie->out, reader, cookie->config, cookie->crc);

  if (cookie->out.end () && cookie->cmi_name)
    {
      /* Some OS's do not replace NEWNAME if it already exists.
	 This'll have a race condition in erroneous concurrent
	 builds.  */
      unlink (cookie->cmi_name);
      if (rename (cookie->tmp_name, cookie->cmi_name))
	{
	  dump () && dump ("Rename ('%s','%s') errno=%u",
			   cookie->tmp_name, cookie->cmi_name, errno);
	  cookie->out.set_error (errno);
	}
    }

  if (cookie->out.get_error () && cookie->began)
    {
      error_at (state->loc, "failed to write compiled module: %s",
		cookie->out.get_error (state->filename));
      state->note_cmi_name ();
    }

  if (!errorcount)
    {
      auto *mapper = get_mapper (cpp_main_loc (reader), cpp_get_deps (reader));
      mapper->ModuleCompiled (state->get_flatname ());
    }
  else if (cookie->cmi_name)
    {
      /* We failed, attempt to erase all evidence we even tried.  */
      unlink (cookie->tmp_name);
      unlink (cookie->cmi_name);
    }

  delete cookie;
  dump.pop (n);
  timevar_stop (TV_MODULE_EXPORT);
}

void
fini_modules (cpp_reader *reader, void *cookie, bool has_inits)
{
  if (cookie)
    late_finish_module (reader,
			static_cast<module_processing_cookie *> (cookie),
			has_inits);

  /* We're done with the macro tables now.  */
  vec_free (macro_exports);
  vec_free (macro_imports);
  headers = NULL;

  /* We're now done with everything but the module names.  */
  set_cmi_repo (NULL);
  if (mapper)
    {
      timevar_start (TV_MODULE_MAPPER);
      module_client::close_module_client (0, mapper);
      mapper = nullptr;
      timevar_stop (TV_MODULE_MAPPER);
    }
  module_state_config::release ();

#if CHECKING_P
  note_defs = NULL;
#endif

  if (modules)
    for (unsigned ix = modules->length (); --ix;)
      if (module_state *state = (*modules)[ix])
	state->release ();

  /* No need to lookup modules anymore.  */
  modules_hash = NULL;

  /* Or entity array.  We still need the entity map to find import numbers.  */
  vec_free (entity_ary);
  entity_ary = NULL;

  /* Or remember any pending entities.  */
  delete pending_table;
  pending_table = NULL;

  /* Or any keys -- Let it go!  */
  delete keyed_table;
  keyed_table = NULL;

  /* Allow a GC, we've possibly made much data unreachable.  */
  ggc_collect ();
}

/* If CODE is a module option, handle it & return true.  Otherwise
   return false.  For unknown reasons I cannot get the option
   generation machinery to set fmodule-mapper or -fmodule-header to
   make a string type option variable.  */

bool
handle_module_option (unsigned code, const char *str, int)
{
  auto hdr = CMS_header;

  switch (opt_code (code))
    {
    case OPT_fmodule_mapper_:
      module_mapper_name = str;
      return true;

    case OPT_fmodule_header_:
      {
	if (!strcmp (str, "user"))
	  hdr = CMS_user;
	else if (!strcmp (str, "system"))
	  hdr = CMS_system;
	else
	  error ("unknown header kind %qs", str);
      }
      /* Fallthrough.  */

    case OPT_fmodule_header:
      flag_header_unit = hdr;
      flag_modules = 1;
      return true;

    case OPT_flang_info_include_translate_:
      vec_safe_push (note_includes, str);
      return true;

    case OPT_flang_info_module_cmi_:
      vec_safe_push (note_cmis, str);
      return true;

    default:
      return false;
    }
}

/* Set preprocessor callbacks and options for modules.  */

void
module_preprocess_options (cpp_reader *reader)
{
  gcc_checking_assert (!lang_hooks.preprocess_undef);
  if (modules_p ())
    {
      auto *cb = cpp_get_callbacks (reader);

      cb->translate_include = maybe_translate_include;
      cb->user_deferred_macro = module_state::deferred_macro;
      if (flag_header_unit)
	{
	  /* If the preprocessor hook is already in use, that
	     implementation will call the undef langhook.  */
	  if (cb->undef)
	    lang_hooks.preprocess_undef = module_state::undef_macro;
	  else
	    cb->undef = module_state::undef_macro;
	}
      auto *opt = cpp_get_options (reader);
      opt->module_directives = true;
      if (flag_no_output)
	opt->directives_only = true;
      if (opt->main_search == CMS_none)
	opt->main_search = cpp_main_search (flag_header_unit);
    }
}

#include "gt-cp-module.h"
