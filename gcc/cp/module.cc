/* C++ modules.  Experimental!
   Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

   The merged modules proposal, p1103r1, allows me to drop support for
   two different schemes.
   
   Each namespace-scope decl has a MODULE_OWNER.  This is MODULE_NONE
   for entities declared in the Global Module Fragment.  In the
   purview of the current module, it is MODULE_PURVIEW.  For any
   imported declaration it is >= MODULE_IMPORT_BASE.  Decls from
   header imports have such a MODULE_OWNER, even though they are in the
   global module.  Builtins are always MODULE_NONE. (Note that
   this is happenstance for decls lacking DECL_LANG_SPECIFIC.)

   Each template instantiation also has a MODULE_OWNER, set according
   to the instantiating translation unit.  This may well be different
   to either the containing instantiation, or the template
   definition.

   The decls for a particular module are held located in a sparse
   array hanging off the ns-level binding of the name.  This is
   partitioned into two: a set of Fixed slots at the start followed by
   the sparse slots afterwards.  By construction we only need to
   append new slots to the end -- there is never a need to insert in
   the middle.  The fixed slots are MODULE_SLOT_CURRENT for the
   current TU (regardless of whether it is a module or not),
   MODULE_SLOT_GLOBAL and MODULE_SLOT_PARTITION.  These latter two
   slots are used for merging entities across the global module and
   module partitions respectively.  MODULE_SLOT_PARTITION is only
   present in a module.  Neither slot is searched during name lookup
   -- they are internal use only.  This vector is created lazily once
   we require it, if there is only a declaration from the current TU, a
   regular binding is present.  It is converted on demand.

   OPTIMIZATION: Outside of the current TU, we only need ADL to work.
   We could optimize regular lookup for the current TU by glomming all
   the visible decls on its slot.  Perhaps wait until design is a
   little more settled though.

   There is only one instance of each extern-linkage namespace.  It
   appears in every module slot that makes it visible.  It also
   appears in MODULE_SLOT_GLOBAL. (it is an ODR violation if they
   collide with some other global module entity.)  We also have an
   optimization that shares the slot for adjacent modules that declare
   the same such namespace.

   A module interface compilation produces a Compiled Module Interface
   (CMI).  I use ELROND format, which allows a bunch of named sections
   containing arbitrary data.  Although I don't defend against
   actively hostile CMIs, there is some checksumming involved to
   verify data integrity.  When dumping out an interface, we generate
   a list of all the namespace-scope DECLS that are needed.  From that
   we determine the strongly connected components (SCC) within this
   TU.  Each SCC is dumped to a separate section of the CMI.  We
   generate a binding table section, mapping each namespace&name to a
   defining section.  This allows lazy loading.

   Notice this means we embed section indices into the contents of
   other sections.  Thus random manipulation of the CMI file by ELF
   tools may well break it.  The kosher way would probably be to
   introduce indirection via section symbols, but that would require
   defining a relocation type.

   References to decls not in the same SCC are by two different
   mechanisms.

   The simplest is for extern or module linkage entities, which are by
   context, name, module & overload index.  We look in exactly that
   scope, get the specified module binding and element from the
   overload set (or type).  Getting the module binding might cause
   lazy loading of that module's binding.

   There are some entities are unnameable -- a local type returned
   by a function (eg, a lambda).  These types must be referencable by
   importing modules.  We construct a per-module vector of such types
   and refer to them by index.

   Notice that lazy loading of one module's binding can cause lazy
   loading of other bindings of the same or other modules.  Clearly we
   want to avoid loops.  In a correct program there can be no loops in
   the module dependency graph, and the above-mentioned SCC algorithm
   places all intra-module circular dependencies in the same SCC.  It
   also orders the SCCs wrt each other, so dependent SCCs come first.
   As we load dependent modules first, we know there can be no
   reference to a higher-numbered module, and because we write out
   dependent SCCs first likewise for SCCs within the module.  This
   allows us to immediately detect broken references.

Classes used:

   dumper - logger

   data - buffer

   bytes - data streamer
   bytes_in : bytes - scalar reader
   bytes_out : bytes - scalar writer

   elf - ELROND format
   elf_in : elf - ELROND reader
   elf_out : elf - ELROND writer

   trees_in : bytes_in - tree reader
   trees_out : bytes_out - tree writer

   depset - dependency set
   depset::hash - hash table of depsets
   depset::tarjan - SCC determinator

   specset - specialization set
   specset::hash - hash table of specsets

   loc_spans - location map data

   module_state - module object

   slurping - data needed during loading

   macro_import - imported macro data
   macro_export - exported macro data

   module_mapper - mapper object

   The ELROND objects use mmap, for both reading and writing.  If mmap
   is unavailable, fileno IO is used to read and write blocks of data.

   The mapper object uses fileno IO to communicate with the server or
   program.

   I have a confession: tri-valued bools are not the worst thing in
   this file.  */

// FIXME: I'm probably using TYPE_NAME in places TYPE_STUB_DECL is
// correct.  They are usually the same, except when 'typedef struct {} foo'

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
#error "Shtopp! What are you doing? This is not ready yet."
#include "bversion.h"
#define MODULE_VERSION (BUILDING_GCC_MAJOR * 10000U + BUILDING_GCC_MINOR)
#elif !IS_EXPERIMENTAL (MODULE_VERSION)
#error "This is not the version I was looking for."
#endif

/* Mapper Protocol version.  Very new.  */
#define MAPPER_VERSION 0
#define _DEFAULT_SOURCE 1 /* To get TZ field of struct tm, if available.  */
#include "config.h"

/* Include network stuff first.  Excitingly OSX10.14 uses bcmp here, which
   we poison later!  */
#if defined (HAVE_AF_UNIX) || defined (HAVE_AF_INET6)
/* socket, connect, shutdown  */
# define NETWORKING 1
# include <sys/socket.h>
# ifdef HAVE_AF_UNIX
/* sockaddr_un  */
#  include <sys/un.h>
# endif
# include <netinet/in.h>
# ifdef HAVE_AF_INET6
/* sockaddr_in6, getaddrinfo, freeaddrinfo, gai_strerror, ntohs, htons.  */
#  include <netdb.h>
# endif
#endif
#ifndef HAVE_AF_INET6
# define gai_strerror(X) ""
#endif

#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "dumpfile.h"
#include "bitmap.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "cpplib.h"
#include "mkdeps.h"
#include "incpath.h"
#include "libiberty.h"
#include "stor-layout.h"
#include "version.h"
#include "tree-diagnostic.h"
#include "params.h"
#include "toplev.h"
#include "opts.h"
#include "attribs.h"
#include "intl.h"
#include "langhooks.h"

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

#ifndef HAVE_MEMRCHR
/* Some unfortunate souls do not have memrchr.
   Everyone is fighting a struggle you know nothing about.  */
static void *
memrchr (void *s_, int c, size_t n)
{
  unsigned char *s = (unsigned char *)s_;
  while (n--)
    if (s[n] == c)
      return &s[n];
  return NULL;
}
#endif
#ifndef HAVE_SIGHANDLER_T
typedef void (*sighandler_t) (int);
#endif

static inline cpp_hashnode *cpp_node (tree id)
{
  return CPP_HASHNODE (GCC_IDENT_TO_HT_IDENT (id));
}
static inline tree identifier (cpp_hashnode *node)
{
  return HT_IDENT_TO_GCC_IDENT (HT_NODE (node));
}
static inline const_tree identifier (const cpp_hashnode *node)
{
  return identifier (const_cast <cpp_hashnode *> (node));
}

/* Id for dumping module information.  */
int module_dump_id;

/* We have a special module owner.  */
#define MODULE_UNKNOWN (unsigned short)(~0U)    /* Not yet known.  */

/* Prefix for section names.  (Not system-defined, so no leading dot.)  */
#define MOD_SNAME_PFX "gnu.c++"

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

class data {
public:
  class allocator {
  public:
    /* Tools tend to moan if the dtor's not virtual.  */
    virtual ~allocator () {}

    void grow (data &obj, unsigned needed, bool exact);
    void shrink (data &obj);
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

public:
  void unuse (unsigned count)
  {
    pos -= count;
  }

public:
  static allocator simple_memory;
};

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

/* Byte streamer base.   Buffer with read/write position and smarts
   for single bits.  */

class bytes : public data {
public:
  typedef data parent;

protected:
  uint32_t bit_val;	/* Bit buffer.  */
  unsigned bit_pos;	/* Next bit in bit buffer.  */

public:
  bytes ()
    :parent (), bit_val (0), bit_pos (0)
  {}
  ~bytes () 
  {
  }

protected:
  unsigned calc_crc (unsigned) const;

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
};

/* Calculate the crc32 of the buffer.  Note the CRC is stored in the
   first 4 bytes, so don't include them.  */

unsigned
bytes::calc_crc (unsigned l) const
{
  unsigned crc = 0;
  for (size_t ix = 4; ix < l; ix++)
    crc = crc32_byte (crc, buffer[ix]);
  return crc;
}

class elf_in;

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
  /* Seek to end.  */
  void no_more ()
  {
    pos = size;
  }

public:
  /* Start reading at OFFSET.  */
  void random_access (unsigned offset)
  {
    if (offset > size)
      set_overrun ();
    pos = offset;
    bit_pos = bit_val = 0;
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
  const void *buf (size_t); /* Read a fixed-length buffer.  */
  cpp_hashnode *cpp_node (); /* Read a cpp node.  */
};

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

class bytes_out : public bytes {
  typedef bytes parent;

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
  void b (bool);	/* Write bool.  */
  void bflush ();	/* Finish block of bools.  */

public:
  void c (unsigned char); /* Write unsigned char.  */
  void i (int);		/* Write signed int.  */
  void u (unsigned);	/* Write unsigned int.  */
  void z (size_t s);	/* Write size_t.  */
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
  static int is_set;
};

/* Instrumentation.  */
unsigned bytes_out::spans[4];
unsigned bytes_out::lengths[4];
int bytes_out::is_set = -1;

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
      accum = accum ? crc32_unsigned (accum, crc) : crc;
      *crc_ptr = accum;

      /* Buffer will be sufficiently aligned.  */
      *(unsigned *)buffer = crc;
    }
}

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
	  if ((ptr = read (++bytes)))
	    while (bytes--)
	      v = (v << 8) | (*ptr++ & 0xff);
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
	  if ((ptr = read (++bytes)))
	    while (bytes--)
	      v = (v << 8) | (*ptr++ & 0xff);
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
    case E_BAD_LAZY:
      return "Bad lazy ordering";
    case E_BAD_IMPORT:
      return "Bad import dependency";
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
  void defrost (const char *);

  /* If BYTES is in the mmapped area, allocate a new buffer for it.  */
  void preserve (bytes_in &bytes)
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
  static void release (elf_in *self, bytes_in &bytes)
  {
#if MAPPED_READING
    if (!(self && self->hdr.buffer && bytes.buffer >= self->hdr.buffer
	  && bytes.buffer < self->hdr.buffer + self->hdr.pos))
#endif
      data::simple_memory.shrink (bytes.buffer);
    bytes.buffer = NULL;
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
  virtual char *grow (char *, unsigned needed);
#if MAPPED_WRITING
  using allocator::shrink;
  virtual void shrink (char *);
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

void
elf_in::defrost (const char *name)
{
  gcc_checking_assert (is_frozen ());
  struct stat stat;

  fd = open (name, O_RDONLY | O_CLOEXEC);
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
	    set_error (errno);
	  else
	    {
	      /* These buffers are never NULL in this case.  */
	      strtab.buffer = mapping + strtab.pos;
	      sectab.buffer = mapping + sectab.pos;
	      hdr.buffer = mapping;
	    }
	}
#endif
    }
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
  if (::read (fd, data->buffer, data->size) != length)
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
      set_error (errno);
      return false;
    }
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
  if (!hdr.buffer)
    return NULL;

  if (!data)
    {
      /* First allocation, align to SECTION_ALIGN now.  */
      if (unsigned padding = pos & (SECTION_ALIGN - 1))
	{
	  padding = SECTION_ALIGN - padding;
#if !MAPPED_WRITING
	  /* Align the section on disk, should help the necessary copies.
	     fseeking to extend is non-portable.  */
	  static char zero[SECTION_ALIGN];
	  if (::write (fd, &zero, padding) != padding)
	    set_error (errno);
#endif
	  pos += padding;
	}
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
  if (decl != global_namespace)
    {
      if (TYPE_P (decl))
	decl = TYPE_NAME (decl);

      strtab_write (CP_DECL_CONTEXT (decl), -1);

      tree name = DECL_NAME (decl);
      if (!name)
	name = DECL_ASSEMBLER_NAME_RAW (decl);
      strtab_write (IDENTIFIER_POINTER (name), IDENTIFIER_LENGTH (name));
    }

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
  if (::write (fd, buffer.buffer, buffer.pos) != buffer.pos)
    {
      set_error (errno);
      return 0;
    }
#endif
  unsigned res = pos;
  pos += buffer.pos;
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
  /* The kinds of entity the depset could describe.  */
  enum entity_kind
  {
    EK_DECL,		/* A decl.  */
    EK_UNNAMED,		/* An unnameable entity.  */
    EK_CLONE,		/* A clone.  */
    EK_SPECIALIZATION,  /* A specialization.  */
    EK_USING,		/* A using declaration.  */
    EK_NAMESPACE,	/* A namespace.  */
    EK_REDIRECT,	/* Redirect to a template_decl.  */
    EK_EXPLICIT_HWM,  
    EK_BINDING = EK_EXPLICIT_HWM, /* Implicitly encoded.  */
    EK_MAYBE_SPEC,	/* Potentially a specialization, else a DECL,
			   never a returned kind.  */
    EK_BITS = 3, /* Only need to encode below EK_EXPLICIT_HWM.  */
  };

private:
  /* Placement of bit fields in discriminator.  */
  enum disc_bits 
  {
    DB_ZERO_BIT, /* Set to disambiguate identifier from flags  */
    DB_MARK_BIT, /* First dep slot is special.  */
    DB_KIND_BIT, /* Kind of the entity.  */
    DB_KIND_BITS = EK_BITS,
    DB_DEFN_BIT = DB_KIND_BIT + DB_KIND_BITS,
    DB_IS_INTERNAL_BIT,		/* It is an internal-linkage entity.  */
    DB_REFS_UNNAMED_BIT,	/* Refer to a voldemort entity.  */
    DB_REFS_INTERNAL_BIT,	/* Refers to an internal-linkage entity. */
    DB_GLOBAL_BIT,		/* Global module entity.  */
    DB_IMPORTED_BIT,		/* An imported entity.  */
    DB_PARTIAL_BIT,		/* A partial instantiation or
				   specialization.  */
    DB_UNREACHED_BIT,		/* A yet-to-be reached entity.  */
    DB_HIDDEN_BIT,		/* A hidden binding.  */
    DB_MERGEABLE_BIT,		/* An entity that needs merging.  */
    DB_PSEUDO_SPEC_BIT,		/* A non-specialization
				   specialization.  */
  };

public:
  /* The first slot may be special
     1) when doing the mergeable sort, it refers to the original
     depset of the decl being ordered.
     2) for EK_SPECIALIZATIONS it is a spec_entry pointer.
     Neither are relevant for the SCC determination.  */
  vec<depset *> deps;  /* Depsets we reference.  */

public:
  unsigned cluster; /* Strongly connected cluster.  */
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
  bool is_internal () const
  {
    return get_flag_bit<DB_IS_INTERNAL_BIT> ();
  }
  bool refs_unnamed () const
  {
    return get_flag_bit<DB_REFS_UNNAMED_BIT> ();
  }
  bool refs_internal () const
  {
    return get_flag_bit<DB_REFS_INTERNAL_BIT> ();
  }
  bool is_global () const
  {
    return get_flag_bit<DB_GLOBAL_BIT> ();
  }
  bool is_import () const
  {
    return get_flag_bit<DB_IMPORTED_BIT> ();
  }
  bool is_unreached () const
  {
    return get_flag_bit<DB_UNREACHED_BIT> ();
  }
  bool is_partial () const
  {
    return get_flag_bit<DB_PARTIAL_BIT> ();
  }
  bool is_hidden () const
  {
    return get_flag_bit<DB_HIDDEN_BIT> ();
  }
  bool is_mergeable () const
  {
    return get_flag_bit<DB_MERGEABLE_BIT> ();
  }
  bool is_pseudo_spec () const
  {
    return get_flag_bit<DB_PSEUDO_SPEC_BIT> ();
  }

public:
  /* We set these bit outside of depset.  */
  void set_hidden_binding ()
  {
    set_flag_bit<DB_HIDDEN_BIT> ();
  }

public:
  bool is_marked () const
  {
    return get_flag_bit<DB_MARK_BIT> ();
  }
  void set_marked ()
  {
    set_flag_bit<DB_MARK_BIT> ();
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
    depset *current;         /* Current depset being depended.  */
    bool for_mergeable;	     /* For mergeables.  */
    bool sneakoscope;        /* Detecting dark magic (of a voldemort
				type).  */
    bool reached_unreached;  /* We reached an unreached entity.  */

  public:
    hash (size_t size, bool for_merge = false)
      : parent (size), current (NULL),
	for_mergeable (for_merge), sneakoscope (false),
	reached_unreached (false)
    {
      worklist.create (size);
    }
    ~hash ()
    {
      worklist.release ();
    }

  public:
    bool is_for_mergeable () const 
    {
      return for_mergeable;
    }

  private:
    depset **entity_slot (tree entity, bool = true);
    depset **binding_slot (tree ctx, tree name, bool = true);
    depset *maybe_add_declaration (tree decl);

  public:
    depset *find_entity (tree entity);
    depset *find_binding (tree ctx, tree name);

  public:
    void add_mergeable (depset *);
    void add_mergeable_horcrux (depset *);
    depset *add_dependency (tree decl, entity_kind, bool is_import = false);
    depset *add_redirect (depset *target);
    depset *add_clone (tree clone, tree target);
    bool add_binding (tree ns, tree value);
    bool add_writables (tree ns, bitmap partitions);
    void add_specializations (bool, bitmap partitions);
    void find_dependencies ();
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
    {"decl", "unnamed", "clone", "specialization",
     "using", "namespace", "redirect",
     "binding"};
  entity_kind kind = get_entity_kind ();
  gcc_checking_assert (kind < sizeof (names) / sizeof(names[0]));
  return names[get_entity_kind ()];
}

depset *depset::make_binding (tree ns, tree name)
{
  depset *r = new depset (ns);

  r->discriminator = reinterpret_cast <uintptr_t> (name);
  return r;
}

depset *depset::make_entity (tree entity, entity_kind ek, bool is_defn)
{
  depset *r = new depset (entity);

  r->discriminator = ((1 << DB_ZERO_BIT)
		      | (ek << DB_KIND_BIT)
		      | is_defn << DB_DEFN_BIT);

  return r;
}

/* Specializations that have yet to be loaded.  These are keyed to
   the name of the namespace-scope entity containing their
   most-general template.  */

class specset {
public:
  /* key  */
  tree ns;  /* Namespace containing the template.  */
  tree name;  /* Name of the entity.  */

  /* Payload.  */
  unsigned short allocp2;  /* Allocated pending  */
  unsigned num;    /* Number of pending.  */

  /* Trailing array of pending specializations.  These are indices
     into the unnamed entity array.  */
  unsigned pending[1];

public:
  /* Even with ctors, we're very pod-like.  */
  specset (tree ns, tree name)
    : ns (ns), name (name),
      allocp2 (0), num (0)
  {
  }
  specset (const specset *from)
  {
    size_t size = (offsetof (specset, pending)
		   + sizeof (specset::pending) * from->num);
    memmove (this, from, size);
    if (from->num)
      allocp2++;
  }

public:
  struct traits : delete_ptr_hash<specset> {
    /* hash and equality for compare_type.  */
    inline static hashval_t hash (const compare_type p)
    {
      hashval_t h = pointer_hash<tree_node>::hash (p->ns);
      hashval_t nh = pointer_hash<tree_node>::hash (p->name);
      h = iterative_hash_hashval_t (h, nh);

      return h;
    }
    inline static bool equal (const value_type b, const compare_type p)
    {
      if (b->ns != p->ns)
	return false;

      if (b->name != p->name)
	return false;

      return true;
    }
  };

public:
  class hash : public hash_table<traits> 
  {
    typedef traits::compare_type key_t;
    typedef hash_table<traits> parent;

  public:
    hash (size_t size)
      : parent (size)
    {
    }
    ~hash ()
    {
    }

  public:
    bool add (tree ns, tree name, unsigned index);
    specset *lookup (tree ns, tree name);
  };

  static hash *table;
};

specset::hash *specset::table;

/********************************************************************/
/* Tree streaming.   The tree streaming is very specific to the tree
   structures themselves.  A tag indicates the kind of tree being
   streamed.  -ve tags indicate backreferences to already-streamed
   trees.  Backreferences are auto-numbered.  */

/* Tree tags.  */
enum tree_tag {
  tt_null,		/* NULL_TREE.  */
  tt_fixed,		/* Fixed vector index.  */

  /* These three tags match walk_kind body, mergeable, clone.  */
  tt_node,		/* New node.  */
  // FIXME: Think about these being a sub-field?
  tt_mergeable,		/* Mergeable entity.  */
  tt_clone,		/* A clone.  */

  tt_id,  		/* Identifier node.  */
  tt_conv_id,		/* Conversion operator name.  */
  tt_anon_id,		/* Anonymous name.  */
  tt_lambda_id,		/* Lambda name.  */

  tt_typedef_type,	/* A (possibly implicit) typedefed type.  */
  tt_typename_decl,	/* A type decl for a typedef.  */

  tt_derived_type,	/* A type derived from another type.  */
  tt_variant_type,	/* A variant of another type.  */

  tt_tinfo_var,		/* Typeinfo object. */
  tt_tinfo_typedef,	/* Typeinfo typedef.  */
  tt_ptrmem_type,	/* Pointer to member type.  */

  tt_enum_int,		/* An enum const.  */

  tt_namespace,		/* Namespace reference.  */
  tt_binfo,		/* A BINFO.  */
  tt_vtable,		/* A vtable.  */
  tt_thunk,		/* A thunk.  */
  tt_clone_ref,

  tt_named, 	 	/* Named decl. */
  tt_anon,		/* Anonymous decl.  */
  tt_template,		/* The TEMPLATE_RESULT of a template.  */
  tt_implicit_template, /* An immplicit member template.  */
  tt_friend_template,   /* A friend of a template class.  */
};

enum walk_kind {
  WK_none,   /* No walk to do (a back- or fixed-ref happened).  */
  WK_normal, /* Normal walk (by-name if possible).  */

  /* These three tags match tree_tag node, mergeable & clone.  */
  WK_body,   /* By-value walk.  */
  WK_mergeable, /* By-value mergeable entity walk.  */
  WK_clone,  /* By-value clone walk.  */

  /* The sole purpose of the following kind is logical error
     detection.  */
  WK_merging,   /* By-value merging entity walk.  */
};
static char const *const walk_kind_name[] =
  {NULL, NULL, "body", "mergeable", "clone", NULL};

enum merge_kind
{
  MK_named,
  MK_spec,
  MK_clone,
  MK_none,
};
static char const *const merge_kind_name[] =
  {"named", "specialization", "clone"};

/* Tree stream reader.  Note that reading a stream doesn't mark the
   read trees with TREE_VISITED.  Thus it's quite safe to have
   multiple concurrent readers.  Which is good, because lazy
   loading. */
class trees_in : public bytes_in {
  typedef bytes_in parent;

private:
  module_state *state;		/* Module being imported.  */
  vec<tree> back_refs;		/* Back references.  */
  vec<tree> mergeables;		/* (Duplicate) mergeable decls.  */
  vec<intptr_t> skip_defns;	/* Definitions to skip.  */
  vec<tree> post_decls;		/* Decls to post process.  */

public:
  trees_in (module_state *);
  ~trees_in ();

public:
  int insert (tree);

private:
  tree finish_type (tree);

private:
  tree start (unsigned, int = -1);
  tree finish (tree);

public:
  /* Needed for binfo writing  */
  bool core_bools (tree);

private:
  /* Stream tree_core, lang_decl_specific and lang_type_specific
     bits.  */
  bool core_vals (tree);
  bool lang_type_bools (tree);
  bool lang_type_vals (tree);
  bool lang_decl_bools (tree);
  bool lang_decl_vals (tree);
  bool lang_vals (tree);
  tree tree_binfo ();
  bool tree_node_bools (tree);
  bool tree_node_vals (tree);
  tree tree_value (walk_kind);

private:
  tree chained_decls ();  /* Follow DECL_CHAIN.  */
  vec<tree, va_gc> *tree_vec (); /* vec of tree.  */
  vec<tree_pair_s, va_gc> *tree_pair_vec (); /* vec of tree_pair.  */

public:
  /* Read a tree node.  */
  tree tree_node ();
  bool tpl_header (tree);
  tree fn_arg_types ();
  int fn_parms_init (tree);
  tree fn_parms_fini (int, tree, tree);

public:
  /* Serialize various definitions. */
  bool read_definition (tree decl);
  
private:
  bool is_matching_decl (tree existing, tree node);
  int is_skippable_defn (tree node, bool have_defn);
  bool read_function_def (tree decl, tree maybe_template);
  bool read_var_def (tree decl, tree maybe_template);
  bool read_class_def (tree decl, tree maybe_template);
  bool read_enum_def (tree decl, tree maybe_template);

public:
  merge_kind key_mergeable (tree, tree, tree,
			    tree *, tree *, tree *, tree *, int *);

public:
  void reserve_mergeable (unsigned len)
  {
    /* We push both the existing decl and the duplicate.  */
    mergeables.reserve (len * 2);
  }
  bool register_mergeable (tree decl, tree existing)
  {
    bool ok = mergeables.space (2);
    if (ok)
      {
	mergeables.quick_push (decl);
	mergeables.quick_push (existing);
      }
    return ok;
  }
  tree *lookup_mergeable (unsigned ix)
  {
    ix *= 2;
    if (ix >= mergeables.length ())
      return NULL;

    return &mergeables[ix];
  }
  bool is_existing_mergeable (tree decl)
  {
    for (unsigned ix = mergeables.length (); ix;)
      {
	ix -= 2;
	if (mergeables[ix + 1] == decl)
	  return true;
      }
    return false;
  }
  tree post_process ()
  {
    return post_decls.length () ? post_decls.pop () : NULL_TREE;
  }

private:
  /* We expect very few bad decls, usually none!.  */
  void record_skip_defn (tree defn, bool informed, bool existing = false);
  int is_skip_defn (tree defn);
  bool any_skip_defns () const
  {
    return skip_defns.length () != 0;
  }
  void post_process (tree decl)
  {
    post_decls.safe_push (decl);
  }

private:
  void assert_definition (tree, int);
};

trees_in::trees_in (module_state *state)
  :parent (), state (state)
{
  back_refs.create (500);
  mergeables.create (0);
  skip_defns.create (0);
  post_decls.create (0);
}

trees_in::~trees_in ()
{
  back_refs.release ();
  mergeables.release ();
  skip_defns.release ();
  post_decls.release ();
}

/* Tree stream writer.  */
class trees_out : public bytes_out {
  typedef bytes_out parent;

private:
  module_state *state;		/* The module we are writing.  */
  ptr_int_hash_map tree_map; 	/* Trees to references */
  depset::hash *dep_hash;    	/* Dependency table.  */
  int ref_num;			/* Back reference number.  */
  unsigned section;

public:
  trees_out (allocator *, module_state *, depset::hash &deps, unsigned sec = 0);
  ~trees_out ();

private:
  void mark_trees ();
  void unmark_trees ();

public:
  void begin ();
  unsigned end (elf_out *sink, unsigned name, unsigned *crc_ptr);
  void end ();

private:
  void tag (int rt)
  {
    records++;
    i (rt);
  }

public:
  enum tags 
  {
    tag_backref = -1,	/* Upper bound on the backrefs.  */
    tag_forced = 0,	/* Write by value.  */
    tag_mergeable,	/* Write by value with merge info.  */
    tag_clone,		/* Write by value with clone info.  */
    tag_merging,  	/* Needed for logical error detection.  */
    tag_fixed,		/* Lower bound on the fixed trees.  */
  };

public:
  int insert (tree, walk_kind = WK_normal);

private:
  void start (tree);

private:
  walk_kind ref_node (tree);

private:
  void core_bools (tree);
  void core_vals (tree);
  void lang_type_bools (tree);
  void lang_type_vals (tree);
  void lang_decl_bools (tree);
  void lang_decl_vals (tree);
  void lang_vals (tree);
  tree tree_binfo (tree, int, bool);
  void tree_node_bools (tree);
  void tree_node_vals (tree);

private:
  void chained_decls (tree);
  void tree_vec (vec<tree, va_gc> *);
  void tree_pair_vec (vec<tree_pair_s, va_gc> *);

public:
  /* Mark a node for by-value walking.  */
  void mark_node (tree);
  /* Reset it for mergeableness.  */
  void mark_mergeable (tree, int);
  /* Reset it for having been merged.  */
  void mark_merged (tree, int);

public:
  void tree_node (tree);
  void tpl_header (tree);
  void fn_arg_types (tree);
  void fn_parms_init (tree);
  void fn_parms_fini (tree fn, tree maybe_template);

public:
  merge_kind key_mergeable (depset *);

public:
  bool is_for_mergeable () const
  {
    return dep_hash->is_for_mergeable ();
  }

public:
  void tree_value (tree, walk_kind ref);
  void tree_ctx (tree, bool need_contents, tree inner_decl);

private:
  bool tree_decl (tree, walk_kind ref, bool looking_inside);
  bool tree_type (tree, walk_kind ref, bool looking_inside);
  void tree_namespace (tree, walk_kind ref, tree inner_decl);

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

trees_out::trees_out (allocator *mem, module_state *state, depset::hash &deps,
		      unsigned section)
  :parent (mem), state (state), tree_map (500),
   dep_hash (&deps), ref_num (0), section (section)
{
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
typedef std::pair<unsigned,unsigned> range_t;

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
    int ordinary_delta;	/* Add to ordinary loc to get serialized loc.  */
    int macro_delta;	/* Likewise for macro loc.  */
  };

private:
  vec<span> spans;

public:
  loc_spans ()
  {
    spans.create (20);
  }
  ~loc_spans ()
  {
    spans.release ();
  }

public:
  span &operator[] (unsigned ix)
  {
    return spans[ix];
  }
  unsigned length () const
  {
    return spans.length ();
  }

public:
  bool init_p () const
  {
    return spans.length () != 0;
  }
  /* Initializer.  */
  void init (const line_maps *lmaps, const line_map_ordinary *map);

public:
  enum {
    SPAN_RESERVED = 0,	/* Reserved (fixed) locations.  */
    SPAN_FIRST = 1,	/* LWM of locations to stream  */
    SPAN_MAIN = 2	/* Main file and onwards.  */
  };

public:
  location_t main_start () const
  {
    return spans[SPAN_MAIN].ordinary.first;
  }

public:
  void open (location_t);
  void close ();

public:
  const span *ordinary (location_t);
  const span *macro (location_t);
};

static loc_spans spans;

/********************************************************************/
/* Unnamed declarations.  (a) voldemort types, (b) instantiations.  */

struct GTY(()) unnamed_entity
{
  mc_slot slot;  /* The decl, or section number. */
  tree ns;	 /* If a specialization, the ns::id it specializes.  */
  tree id;

  unnamed_entity ()
    :ns (NULL), id (NULL)
  {
    slot.u.binding = NULL;
  }
};

static GTY(()) vec<unnamed_entity, va_gc> *unnamed_ary;
typedef hash_map<unsigned/*UID*/, unsigned/*index*/,
		 simple_hashmap_traits<int_hash<unsigned,0>,
				       unsigned> > unnamed_map_t;
static unnamed_map_t *unnamed_map;

/********************************************************************/
/* Data needed by a module during the process of loading.  */
struct GTY(()) slurping {
  vec<unsigned, va_heap, vl_embed> *
    GTY((skip)) remap;			/* Module owner remapping.  */
  elf_in *GTY((skip)) from;     	/* The elf loader.  */

  /* This map is only for header imports themselves -- the global
     headers bitmap hold it for the current TU.  */
  bitmap headers;	/* Transitive direct header import graph. */

  /* These objects point into the mmapped area, unless we're not doing
     that, or we got frozen or closed.  In those cases they point to
     buffers we own.  */
  bytes_in macro_defs;	/* Macro definitions.  */
  bytes_in macro_tbl;	/* Macro table.  */

  /* Location remapping.  */
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
  void alloc_remap (unsigned size)
  {
    gcc_assert (!remap);
    vec_safe_reserve (remap, size);
    for (unsigned ix = size; ix--;)
      remap->quick_push (0);
  }
  unsigned remap_module (unsigned owner)
  {
    return owner < remap->length () ? (*remap)[owner] : MODULE_NONE;
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
  if (macro_defs.size)
    elf_in::release (from, macro_defs);
  if (macro_tbl.size)
    elf_in::release (from, macro_tbl);
  close ();
}

#if CHECKING_P
/* We should stream each definition at most once.  */
static hash_set<tree> *note_defs;
#endif

void
trees_in::assert_definition (tree decl ATTRIBUTE_UNUSED, int odr ATTRIBUTE_UNUSED)
{
#if CHECKING_P
  if (!odr)
    {
      /* We must be inserting for the first time.  */
      bool existed = note_defs->add (decl);
      gcc_assert (!existed);
    }
  else
    /* If this is the mergeable entity, it should already be there.
       Otherwise it should not be.  */
    gcc_assert (note_defs->contains (decl) == is_existing_mergeable (decl));
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    gcc_assert (!note_defs->contains (DECL_TEMPLATE_RESULT (decl)));
#endif
}

void
trees_out::assert_definition (tree decl ATTRIBUTE_UNUSED)
{
#if CHECKING_P
  bool existed = note_defs->add (decl);
  gcc_assert (!existed);
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    gcc_assert (!note_defs->contains (DECL_TEMPLATE_RESULT (decl)));
#endif
}

/********************************************************************/
/* State of a particular module. */

class GTY((chain_next ("%h.parent"), for_user)) module_state {
 public:
  /* We always import & export ourselves.  */
  bitmap imports;	/* Transitive modules we're importing.  */
  bitmap exports;	/* Subset of that, that we're exporting.  */

  module_state *parent;
  tree name;		/* Name of the module.  */

  /* Sadly this cannot be anonymous, because GTY.  */
  union {
    slurping *GTY ((tag ("false"))) slurp;	/* Data for loading.  */
    module_state *GTY ((tag ("true"))) alias;	/* Alias of */
  } GTY ((desc ("%1.alias_p"))) u1;

  const char *flatname;	/* Flatname of module.  */
  char *filename;	/* CMI Filename */

  /* Unnnamed decls can be referred to transitively.  Base and number
     of them for this module.  */
  unsigned unnamed_lwm;
  unsigned unnamed_num;

  /* Location ranges for this module.  adhoc-locs are decomposed, so
     don't have a range.  */
  loc_range_t GTY((skip)) ordinary_locs;
  loc_range_t GTY((skip)) macro_locs;

  /* The LOC is unset until we import the module.  */
  location_t loc; 	/* Location referring to module itself.  */
  /* The FROM_LOC is unset until we process a declaration.  */
  location_t from_loc;  /* Location module was imported at.  */

  unsigned short mod;		/* Module owner number.  */
  unsigned short subst;		/* Mangle subst if !0.  */
  unsigned crc;		/* CRC we saw reading it in. */

  unsigned short remap;		/* Remapping during writing.  */
  bool header_p : 1;	/* Is a header import.  */
  bool direct_p : 1;	/* A direct import of TU (includes interface
			   of implementation for which primary_p).  */
  bool primary_p : 1;   /* Is the primary interface of this
			   implementation unit.  */
  bool interface_p : 1; /* Is an interface (partition or primary).  */
  bool exported_p : 1;	/* Direct_p && exported.  */
  bool imported_p : 1;	/* Import has been done.  */
  bool alias_p : 1;	/* Alias for other module.  */
  bool partition_p : 1; /* A partition.  */
  bool from_partition_p : 1; /* Direct import of a partition.  */
  bool cmi_noted_p : 1;

 public:
  module_state (tree name, module_state *, bool);
  ~module_state ();

 public:
  void release ()
  {
    imports = exports = NULL;
    if (!alias_p)
      slurped ();
  }
  void slurped ()
  {
    gcc_checking_assert (!alias_p);
    delete u1.slurp;
    u1.slurp = NULL;
  }
  slurping *slurp () const
  {
    gcc_checking_assert (!alias_p);
    return u1.slurp;
  }
  elf_in *from () const
  {
    elf_in *from = slurp ()->from;
    gcc_checking_assert (from);
    return from;
  }

 public:
  /* Is this not a real module?  */
  bool is_detached () const
  {
    return from_loc == UNKNOWN_LOCATION;
  }
  bool is_direct () const
  {
    return direct_p;
  }
  bool is_imported () const
  {
    return imported_p;
  }
  bool is_primary () const
  {
    return primary_p;
  }
  bool is_interface () const
  {
    return interface_p;
  }
  bool is_header () const
  {
    return header_p;
  }
  bool is_alias () const
  {
    return alias_p;
  }
  bool is_partition () const
  {
    return partition_p;
  }

 public:
  module_state *resolve_alias ();
  bool check_not_purview (location_t loc);

 public:
  void mangle ();

 public:
  void set_import (module_state const *, bool is_export);
  void announce (const char *) const;

 public:
  /* Read and write module.  */
  void write (elf_out *to, cpp_reader *);
  module_state *read (int fd, int e, cpp_reader *);

  /* Read a section.  */
  void load_section (unsigned snum);

  /* Juggle a limited number of file numbers.  */
  static void freeze_an_elf ();
  void maybe_defrost ();

  /* Lazily read a section.  */
  bool lazy_load (tree ns, tree id, mc_slot *mslot, bool outermost = false);
  bool lazy_load (tree decl, bool outermost = false);

 private:
  /* Check or complete a read.  */
  bool check_read (unsigned count, tree ns = NULL_TREE, tree id = NULL_TREE);

 private:
  /* The README, for human consumption.  */
  void write_readme (elf_out *to, const char *opts, const cpp_hashnode *node);
  void write_env (elf_out *to);

 private:
  /* Import tables. */
  void write_imports (bytes_out &cfg, bool direct);
  unsigned read_imports (bytes_in &cfg, cpp_reader *, line_maps *maps);
  void write_imports (elf_out *to, unsigned *crc_ptr);
  bool read_imports (cpp_reader *, line_maps *);

 private:
  void write_partitions (elf_out *to, unsigned, unsigned *crc_ptr);
  bool read_partitions (unsigned);

 private:
  void write_config (elf_out *to, struct module_state_config &, unsigned crc);
  bool read_config (cpp_reader *, struct module_state_config &);

 public:
  void note_cmi_name ();

 private:
  static unsigned write_bindings (elf_out *to, vec<depset *> depsets,
				  depset::hash &table, unsigned *crc_ptr);
  bool read_bindings (auto_vec<tree> &spaces, unsigned, const range_t &range);

  void write_namespaces (elf_out *to, depset::hash &table,
			 auto_vec<depset *> &spaces, unsigned *crc_ptr);
  bool read_namespaces (auto_vec<tree> &spaces);

  void write_cluster (elf_out *to, depset *depsets[], unsigned size,
		      depset::hash &, unsigned &unnamed, unsigned *crc_ptr);
  bool read_cluster (unsigned snum);

 private:
  void write_unnamed (elf_out *to, vec<depset *> depsets,
		      depset::hash &, unsigned count, unsigned *crc_ptr);
  bool read_unnamed (unsigned count, const range_t &range);

 private:
  unsigned prepare_locations ();
  void write_locations (elf_out *to, unsigned, bool, unsigned *crc_ptr);
  bool read_locations ();

 private:
  void write_define (bytes_out &, const cpp_macro *, bool located = true);
  cpp_macro *read_define (bytes_in &, cpp_reader *, bool located = true) const;
  unsigned write_macros (elf_out *to, cpp_reader *, unsigned *crc_ptr);
  bool read_macros ();
  void install_macros ();

 public:
  void import_macros ();

 public:
  static void undef_macro (cpp_reader *, location_t, cpp_hashnode *);
  static cpp_macro *deferred_macro (cpp_reader *, location_t, cpp_hashnode *);

 public:
  void write_location (bytes_out &, location_t);
  location_t read_location (bytes_in &) const;

 public:
  void set_flatname ();
  const char *get_flatname () const
  {
    gcc_checking_assert (flatname);
    return flatname;
  }

 public:
  /* Create a location for module.   */
  void maybe_create_loc ()
  {
    gcc_checking_assert (from_loc != UNKNOWN_LOCATION);
    if (loc == UNKNOWN_LOCATION)
      /* Error paths can cause this to be set and then repeated.  */
      loc = linemap_module_loc (line_table, from_loc, get_flatname ());
  }
  void attach (location_t from)
  {
    from_loc = from;

    if (!flatname)
      set_flatname ();
  }

 public:
  bool do_import (const char *filename, cpp_reader *);
  void direct_import (cpp_reader *, bool lazy);
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
    parent (parent), name (name), flatname (NULL), filename (NULL),
    unnamed_lwm (0), unnamed_num (0),
    ordinary_locs (0, 0), macro_locs (0, 0),
    loc (UNKNOWN_LOCATION), from_loc (UNKNOWN_LOCATION),
    mod (MODULE_UNKNOWN), subst (0), crc (0), remap (0),
    partition_p (partition)
{
  u1.slurp = NULL;
  header_p = direct_p = primary_p = interface_p = exported_p
    = imported_p = alias_p = from_partition_p = cmi_noted_p = false;
  if (name && TREE_CODE (name) == STRING_CST)
    header_p = true;
  gcc_checking_assert (header_p
		       ? (IS_ABSOLUTE_PATH (TREE_STRING_POINTER (name))
			  || TREE_STRING_POINTER (name)[0] == '.')
		       : !name || ISALPHA (IDENTIFIER_POINTER (name)[0]));
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

/* Some flag values: */

/* Mapper name.  */
static const char *module_mapper_name;

/* Deferred imports.  */
static vec<module_state *, va_heap, vl_embed> *pending_imports;

/* CMI repository path and workspace.  */
static char *cmi_repo;
static size_t cmi_repo_length;
static char *cmi_path;
static size_t cmi_path_alloc;

/* Global variables.  */
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

/* Mapper to query and inform of modular compilations.  This is a
   singleton.  It contains both FILE and fd entities.  The PEX
   interface provides the former, so we need to keep them around.
   the fd entities are used when networking is supported.  */

class module_mapper {
  const char *name;
  FILE *from;   /* Read from mapper.  */
  FILE *to;	/* Write to mapper.  */
  pex_obj *pex; /* If it's a subprocess.  */
  sighandler_t sigpipe; /* Original sigpipe disposition.  */

  char *buffer; /* Line buffer.  */
  size_t size;  /* Allocated size of buffer.  */
  char *pos;	/* Read/Write point in buffer.  */
  char *end;	/* Ending NUL byte.  */
  char *start;  /* Start of current response line.  */
  int fd_from;	/* Fileno from mapper. */
  int fd_to;	/* Fileno to mapper. */
  bool batching;/* Batching requests or responses.  */

private:
  /* Construction always succeeds, but may result in a dead mapper.  */
  module_mapper (location_t loc, const char *connection);
  ~module_mapper ()
  {
    gcc_assert (!from);
  }

private:
  void kill (location_t);
  static module_mapper *make (location_t loc);

public:
  static module_mapper *get (location_t loc)
  {
    if (!mapper)
      mapper = make (loc);
    return mapper;
  }
  static void fini (location_t loc)
  {
    if (mapper)
      {
	mapper->kill (loc);
	delete mapper;
	mapper = NULL;
      }
  }

public:
  bool is_live () const
  {
    return fd_from >= 0;
  }
  bool is_server () const
  {
    return is_live () && fd_to >= 0;
  }
  bool is_file () const
  {
    return is_live () && fd_to < 0;
  }

public:
  static char *import_export (const module_state *, bool export_p);
  static bool export_done (const module_state *);

public:
  bool cork ()
  {
    batching = true;
    return batching;
  }
  void uncork (location_t loc)
  {
    if (batching)
      {
	batching = false;
	/* Need to disable gnu-printf zero-length format warning.  */
	send_command (loc, "%s", "");
      }
  }
  bool is_corked () const
  {
    return batching;
  }
  bool eol_p () const
  {
    return pos == end;
  }

public:
  void imex_query (const module_state *, bool exporting);
  char *imex_response (const module_state *state)
  {
    return get_response (state->from_loc) > 0 ? cmi_response (state) : NULL;
  }
  bool translate_include (location_t, const char *);

public:
  /* After a response that may be corked, eat blank lines until it is
     uncorked.  */
  void maybe_uncork (location_t loc)
  {
    while (is_corked ())
      if (get_response (loc) > 0)
	response_unexpected (loc);
  }

private:
  bool handshake (location_t, const char *main_src);
  void send_command (location_t, const char * = NULL, ...) ATTRIBUTE_PRINTF_3;
  int get_response (location_t);
  char *response_token (location_t, bool all = false);
  int response_word (location_t, const char *, ...);
  const char *response_error ()
  {
    const char *result = pos != end ? pos : "unspecified error";
    pos = end;
    return result;
  }
  void response_unexpected (location_t);
  bool response_eol (location_t, bool ignore = false);
  char *cmi_response (const module_state *);

private:
  static module_mapper *mapper;
};

/* Our module mapper (created lazily).  */
module_mapper *module_mapper::mapper;

static tree
get_clone_target (tree decl)
{
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      tree res_orig = DECL_CLONED_FUNCTION (DECL_TEMPLATE_RESULT (decl));
      return DECL_TI_TEMPLATE (res_orig);
    }

  return DECL_CLONED_FUNCTION (decl);
}

/* Like FOR_EACH_CLONE, but will walk cloned templates.  */
#define FOR_EVERY_CLONE(CLONE, FN)			\
  if (!DECL_MAYBE_IN_CHARGE_CDTOR_P (FN));		\
  else							\
    for (CLONE = DECL_CHAIN (FN);			\
	 CLONE && DECL_CLONED_FUNCTION_P (CLONE);	\
	 CLONE = DECL_CHAIN (CLONE))

static tree
node_template_info (tree decl, int &use)
{
  tree ti = NULL_TREE;
  int use_tpl = -1;
  if (DECL_IMPLICIT_TYPEDEF_P (decl))
    {
      tree type = TREE_TYPE (decl);
      /* During read in, type might not have been set yet!  */
      if (type)
	ti = TYPE_TEMPLATE_INFO (type);

      if (ti)
	{
	  if (TYPE_LANG_SPECIFIC (type))
	    use_tpl = CLASSTYPE_USE_TEMPLATE (type);
	  else
	    {
	      /* An enum, where we don't explicitly encode use_tpl.
		 If the containing type (there must be one), is an
		 ({im,ex}plicit) instantiation, then this is too.  If
		 it's a partial or explicit specialization, then this
		 is not!.  */
	      use_tpl = CLASSTYPE_USE_TEMPLATE (DECL_CONTEXT (decl));
	      if (use_tpl == 2)
		use_tpl = 0;
	    }
	}
    }
  else if (DECL_LANG_SPECIFIC (decl)
	   && (TREE_CODE (decl) == VAR_DECL
	       || TREE_CODE (decl) == TYPE_DECL
	       || TREE_CODE (decl) == FUNCTION_DECL
	       || TREE_CODE (decl) == FIELD_DECL
	       || TREE_CODE (decl) == TEMPLATE_DECL))
    {
      use_tpl = DECL_USE_TEMPLATE (decl);
      ti = DECL_TEMPLATE_INFO (decl);
    }

  use = use_tpl;
  return ti;
}

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
    MAPPER = TDF_EH,	/* -eh:Mapper.  */
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
  unsigned owner = MODULE_NONE;
  bool ttp = false;

  if (t && TREE_CODE (t) == TREE_BINFO)
    t = BINFO_TYPE (t);

  if (t && TYPE_P (t))
    {
      if (TREE_CODE (t) == TEMPLATE_TEMPLATE_PARM)
	ttp = true;
      t = TYPE_NAME (t);
    }

  if (t && DECL_P (t))
    {
      if (t == global_namespace || ttp)
	;
      else if (tree ctx = DECL_CONTEXT (t))
	if (TREE_CODE (ctx) == TRANSLATION_UNIT_DECL
	    || nested_name (ctx))
	  fputs ("::", stream);

      owner = MAYBE_DECL_MODULE_OWNER (t);

      int use_tpl;
      ti = node_template_info (t, use_tpl);

      t = DECL_NAME (t) ? DECL_NAME (t)
	: HAS_DECL_ASSEMBLER_NAME_P (t) ? DECL_ASSEMBLER_NAME_RAW (t)
	: NULL_TREE;
    }

  if (t)
    switch (TREE_CODE (t))
      {
      case IDENTIFIER_NODE:
	fwrite (IDENTIFIER_POINTER (t), 1, IDENTIFIER_LENGTH (t), stream);
	break;

      case STRING_CST:
	/* If TREE_TYPE is NULL, this is a raw string.  */
	fwrite (TREE_STRING_POINTER (t), 1,
		TREE_STRING_LENGTH (t) - (TREE_TYPE (t) != NULL_TREE), stream);
	break;

      case INTEGER_CST:
	print_hex (wi::to_wide (t), stream);
	break;

      default:
	fputs ("#unnamed#", stream);
	break;
      }
  else
    fputs ("#null#", stream);

  if (owner != MODULE_NONE)
    {
      const module_state *mod = (*modules)[owner];
      fprintf (stream, "@%s:%d", !mod ? "" : !mod->name ? "(unnamed)"
	       : mod->get_flatname (), owner);
    }

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
	case 'M': /* Module. */
	  {
	    const char *str = "(none)";
	    if (module_state *m = va_arg (args, module_state *))
	      {
		if (m->is_detached ())
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
	    if (t && TREE_CODE (t) == OVERLOAD)
	      t = OVL_FIRST (t);
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
	case 'V': /* Verson.  */
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

static bool
noisy_p ()
{
  if (quiet_flag)
    return false;

  pp_needs_newline (global_dc->printer) = true;
  diagnostic_set_last_function (global_dc, (diagnostic_info *) NULL);

  return true;
}

/* Set the bmi repo.  Strip trailing '/', '.' becomes NULL.  */

static void
set_cmi_repo (char *r)
{
  XDELETEVEC (cmi_repo);
  XDELETEVEC (cmi_path);
  cmi_path_alloc = 0;

  cmi_repo = NULL;
  cmi_repo_length = 0;
  if (r)
    {
      size_t len = strlen (r);
      if (len > 1 && IS_DIR_SEPARATOR (r[len-1]))
	r[--len] = 0;
      if (0 != strcmp (r, "."))
	{
	  cmi_repo = XNEWVEC (char, len + 1);
	  memcpy (cmi_repo, r, len + 1);
	  cmi_repo_length = len;
	}
    }
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
	*base = sep;
	if (failed
	    /* Maybe racing with another creator (of a *different*
	       submodule).  */
	    && errno != EEXIST)
	  break;
      }
}

/* If CMI path TO begins with the prefix, return a pointer to the
   trailing suffix.  Otherwise return TO.  */

static char *
maybe_strip_cmi_prefix (char *to)
{
  if (cmi_repo)
    {
      if (0 == strncmp (to, cmi_repo, cmi_repo_length))
	{
	  char *res = to;
	  for (size_t probe = cmi_repo_length;
	       IS_DIR_SEPARATOR (to[probe]);)
	    res = &to[++probe];
	  to = res;
	}
    }
  return to;
}

/* Given a CLASSTYPE_DECL_LIST VALUE get the friend decl, if that's
   what this is.  */

static tree
friend_from_decl_list (tree frnd)
{
  if (TYPE_P (frnd))
    {
      if (!CLASSTYPE_TEMPLATE_INFO (frnd))
	frnd = NULL_TREE; // reachable?
      else
	frnd = CLASSTYPE_TI_TEMPLATE (frnd);
    }
  else if (TREE_CODE (frnd) == TEMPLATE_DECL)
    ;
  else if (!DECL_TEMPLATE_INFO (frnd))
    frnd = NULL; // FIXME: Is this ever reachable?
  else if (TREE_CODE (DECL_TI_TEMPLATE (frnd)) == TEMPLATE_DECL)
    frnd = DECL_TI_TEMPLATE (frnd);

  return frnd;
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
trees_out::mark_node (tree decl)
{
  gcc_checking_assert (DECL_P (decl)
		       /* Enum consts are INTEGER_CSTS.  */
		       || TREE_CODE (decl) == INTEGER_CST
		       || TREE_CODE (decl) == TREE_BINFO);

  if (TREE_VISITED (decl))
    /* Must already be forced or fixed.  */
    gcc_checking_assert (*tree_map.get (decl) >= tag_forced);
  else
    {
      bool existed = tree_map.put (decl, tag_forced);
      gcc_checking_assert (!existed);
      TREE_VISITED (decl) = true;
    }
}

/* Mark DECL as a mergeable node.  */

void
trees_out::mark_mergeable (tree decl, int tag = tag_mergeable)
{
  int *slot = tree_map.get (decl);
  gcc_assert (slot && *slot == tag_forced);
  *slot = tag;
}

/* Mark decl as a now-merged node.  */

void
trees_out::mark_merged (tree decl, int tag)
{
  int *slot = tree_map.get (decl);
  gcc_assert (slot && *slot == tag_merging);
  *slot = tag;
}

/* Insert T into the map, return its tag number.    */

int
trees_out::insert (tree t, walk_kind walk)
{
  gcc_checking_assert (walk == WK_normal || walk == WK_merging
		       ? !TREE_VISITED (t)
		       : walk == WK_mergeable || walk == WK_clone
		       ? TREE_VISITED (t) : walk == WK_body);
  int tag = --ref_num;
  if (walk != WK_mergeable && walk != WK_clone)
    {
      bool existed;
      int &slot = tree_map.get_or_insert (t, &existed);
      gcc_checking_assert (TREE_VISITED (t) == existed
			   && (!existed
			       || (walk == WK_body && slot == tag_forced)));
      TREE_VISITED (t) = true;

      slot = walk == WK_merging ? tag_merging : tag;
    }

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
  for (tree *chain = &decls; chain && !get_overrun ();)
    if (tree decl = tree_node ())
      {
	if (!DECL_P (decl))
	  set_overrun ();
	else
	  {
	    if (*chain)
	      {
		if (DECL_CLONED_FUNCTION_P (decl))
		  gcc_checking_assert (*chain == decl);
		else
		  gcc_checking_assert (TREE_CODE (decl) == PARM_DECL
				       && TREE_CODE (*chain) == PARM_DECL);
	      }
	    *chain = decl;
	    chain = &DECL_CHAIN (decl);
	  }
      }
    else
      {
	gcc_checking_assert (!*chain);
	chain = NULL;
      }

  return decls;
}

/* A vector of trees.  */

void
trees_out::tree_vec (vec<tree, va_gc> *v)
{
  unsigned len = vec_safe_length (v);
  if (streaming_p ())
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

/* Start tree write.  Write information to allocate the receiving
   node.  */

void
trees_out::start (tree t)
{
  switch (TREE_CODE (t))
    {
    default:
      if (TREE_CODE_CLASS (TREE_CODE (t)) == tcc_vl_exp)
	u (VL_EXP_OPERAND_LENGTH (t));
      break;

    case IDENTIFIER_NODE:
      gcc_unreachable ();
      break;

    case TREE_BINFO:
      u (BINFO_N_BASE_BINFOS (t));
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

    case POLY_INT_CST:
      gcc_unreachable (); /* Not generated by C++.  */
      break;

    case FIXED_CST:
      gcc_unreachable (); /* Not supported in C++.  */
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
trees_in::start (unsigned code, int klass)
{
  tree t = NULL_TREE;

  if (code >= MAX_TREE_CODES ||
      (klass >= 0 && TREE_CODE_CLASS (code) != klass))
    {
    fail:
      set_overrun ();
      return NULL_TREE;
    }

  switch (code)
    {
    default:
      if (TREE_CODE_CLASS (code) == tcc_vl_exp)
	{
	  unsigned ops = u ();
	  t = build_vl_exp (tree_code (code), ops);
	}
      else
	t = make_node (tree_code (code));
      break;

    case IDENTIFIER_NODE:
      /* We should never find naked identifiers.  */
      goto fail;

    case STRING_CST:
      {
	size_t l;
	const char *chars = str (&l);
	t = build_string (l, chars);
      }
      break;

    case TREE_BINFO:
      t = make_tree_binfo (u ());
      break;

    case TREE_VEC:
      t = make_tree_vec (u ());
      break;

    case VECTOR_CST:
      {
	unsigned log2_npats = u ();
	unsigned elts_per = u ();
	t = make_vector (log2_npats, elts_per);
      }
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

  if (TREE_CODE (t) == TEMPLATE_INFO)
    /* We're not a pending template in this TU.  */
    TI_PENDING_TEMPLATE_FLAG (t) = 0;

  if (TREE_CODE (t) == INTEGER_CST && !TREE_OVERFLOW (t))
    t = cache_integer_cst (t, true);

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
  if (TREE_CODE_CLASS (code) != tcc_type)
    /* This is TYPE_CACHED_VALUES_P for types.  */
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
      WB (t->function_decl.lambda_function);
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
  if (TREE_CODE_CLASS (code) != tcc_type)
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
      RB (t->function_decl.lambda_function);
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
  gcc_checking_assert ((*modules)[lang->u.base.module_owner]->remap
		       < MODULE_IMPORT_BASE);
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
      if (streaming_p ())
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

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      /* Write this early, for better log information.  */
      WT (t->decl_minimal.name);
      if (TREE_TYPE (t)
	  && TREE_CODE (TREE_TYPE (t)) == TEMPLATE_TEMPLATE_PARM)
	tree_node (NULL_TREE);
      else
	tree_ctx (t->decl_minimal.context, true, t);

      if (streaming_p ())
	state->write_location (*this, t->decl_minimal.locus);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      /* Likewise, stream the name first.  */
      WT (t->type_common.name);
      tree_ctx (t->type_common.context, true, TYPE_NAME (t));

      /* By construction we want to make sure we have the canonical
	 and main variants already in the type table, so emit them
	 now.  */
      WT (t->type_common.main_variant);
      tree canonical = t->type_common.canonical;
      if (code == TEMPLATE_TYPE_PARM
	  || code == TEMPLATE_TEMPLATE_PARM)
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

      if (TREE_CODE (t) != RECORD_TYPE
	  && TREE_CODE (t) != UNION_TYPE)
	{
	  WT (t->type_common.size);
	  WT (t->type_common.size_unit);
	}
      WT (t->type_common.attributes);

      WT (t->type_common.common.chain); /* TYPE_STUB_DECL.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      tree inner = code == TEMPLATE_DECL ? DECL_TEMPLATE_RESULT (t) : t;
      if (TREE_CODE (inner) == TYPE_DECL && DECL_ORIGINAL_TYPE (inner))
	/* This is a typedecl'd.  We set its type separately.  */
	WT (NULL_TREE);
      else if (code != ENUMERAL_TYPE || ENUM_IS_SCOPED (t))
	WT (t->typed.type);
      else if (streaming_p ())
	{
	  // FIXME: I wonder if we can make DECL_ORIGINAL_TYPE point at
	  // the (unrestricted) underlying type?
	  // Unscoped enums have an integral type, but with a
	  // restricted precision.  The type's name matches one of the
	  // known integer types.

	  tree type = t->typed.type;
	  unsigned itk = itk_none;
	  if (type)
	    {
	      tree name = DECL_NAME (TYPE_NAME (type));
	      for (itk = itk_none; itk--;)
		if (integer_types[itk]
		    && DECL_NAME (TYPE_NAME (integer_types[itk])) == name)
		  break;
	      gcc_assert (itk != itk_none);
	    }
	  WU (itk);
	  if (type)
	    WU (TYPE_PRECISION (type));
	}
    }

  if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
    {
      /* Whether TREE_CHAIN is dumped depends on who's containing it.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_INT_CST))
    if (streaming_p ())
      {
	unsigned num = TREE_INT_CST_EXT_NUNITS (t);
	for (unsigned ix = 0; ix != num; ix++)
	  wu (TREE_INT_CST_ELT (t, ix));
      }

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    if (streaming_p ())
      buf (TREE_REAL_CST_PTR (t), sizeof (real_value));

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    gcc_unreachable (); /* Not supported in C++.  */

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    for (unsigned ix = vector_cst_encoded_nelts (t); ix--;)
      WT (VECTOR_CST_ENCODED_ELT (t, ix));

  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    /* Streamed during start.  */
    gcc_checking_assert (code == STRING_CST);

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    {
      WT (TREE_REALPART (t));
      WT (TREE_IMAGPART (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    gcc_unreachable (); /* Should never meet.  */

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
      switch (code)
	// FIXME: Perhaps this should be done with the later
	// polymorphic check?
	{
	default:
	  break;
	case VAR_DECL:
	  // FIXME: Perhaps always write DECL_INITIAL?
	  if (DECL_CONTEXT (t)
	      && TREE_CODE (DECL_CONTEXT (t)) != FUNCTION_DECL)
	    break;
	  /* FALLTHROUGH  */
	case PARM_DECL:
	  if (DECL_HAS_VALUE_EXPR_P (t))
	    WT (DECL_VALUE_EXPR (t));
	  /* FALLTHROUGH  */
	case CONST_DECL:
	  WT (t->decl_common.initial);
	  break;
	}
      WT (t->decl_common.abstract_origin);
      /* decl_common.initial.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    {
      // FIXME: spurious behaviour with other nodes, probably wrong elsewhere
      if (code == TYPE_DECL)
	WT (t->decl_non_common.result);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_PARM_DECL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    if (streaming_p ())
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
      WT (t->field_decl.qualifier); /* bitfield unit.  */
      WT (t->field_decl.bit_offset);
      WT (t->field_decl.fcontext);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LABEL_DECL))
    if (streaming_p ())
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
      WT (t->function_decl.personality);
      WT (t->function_decl.function_specific_target);
      WT (t->function_decl.function_specific_optimization);
      WT (t->function_decl.vindex);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    gcc_unreachable (); /* Should never meet.  */

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_WITH_LANG_SPECIFIC))
    { /* Nothing to do.  */ }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      /* Records and unions hold FIELDS, VFIELD & BINFO on these
	 things.  */
      if (!RECORD_OR_UNION_CODE_P (code) && code != ENUMERAL_TYPE)
	{
	  /* Don't write the cached values vector.  */
	  WT (TYPE_CACHED_VALUES_P (t) ? NULL_TREE : t->type_non_common.values);
	  WT (t->type_non_common.maxval);

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
	}

      /* Pointers use lang slot for caching pointer to member fn
	 record_type.  */
      if (TREE_CODE (t) != POINTER_TYPE)
	WT (t->type_non_common.lang_1);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    {
      WT (t->list.purpose);
      WT (t->list.value);
      WT (t->list.common.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    {
      for (unsigned ix = TREE_VEC_LENGTH (t); ix--;)
	WT (TREE_VEC_ELT (t, ix));
      /* We stash NON_DEFAULT_TEMPLATE_ARGS_COUNT on TREE_CHAIN!  */
      gcc_checking_assert (!t->type_common.common.chain
			   || (TREE_CODE (t->type_common.common.chain)
			       == INTEGER_CST));
      WT (t->type_common.common.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    {
      if (streaming_p ())
	state->write_location (*this, t->exp.locus);
      bool vl = TREE_CODE_CLASS (code) == tcc_vl_exp;
      for (unsigned ix = (vl ? VL_EXP_OPERAND_LENGTH (t)
			  : TREE_OPERAND_LENGTH (t)); ix-- != vl;)
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

  if (CODE_CONTAINS_STRUCT (code, TS_SSA_NAME))
    gcc_unreachable (); /* Should not see.  */

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    {
      WT (t->block.supercontext);
      chained_decls (t->block.vars);
      WT (t->block.abstract_origin);
      // FIXME: nonlocalized_vars, fragment_origin, fragment_chain
      WT (t->block.subblocks);
      WT (t->block.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    {
      WT (t->binfo.common.chain);
      WT (t->binfo.offset);
      WT (t->binfo.inheritance);
      WT (t->binfo.vtable);
      WT (t->binfo.virtuals);
      WT (t->binfo.vptr_field);
      WT (t->binfo.vtt_subvtt);
      WT (t->binfo.vtt_vptr);

      tree_vec (BINFO_BASE_ACCESSES (t));
      unsigned num = vec_safe_length (BINFO_BASE_ACCESSES (t));
      for (unsigned ix = 0; ix != num; ix++)
	WT (BINFO_BASE_BINFO (t, ix));
    }

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
      if (streaming_p ())
	{
	  WU (((lang_tree_node *)t)->tpi.index);
	  WU (((lang_tree_node *)t)->tpi.level);
	  WU (((lang_tree_node *)t)->tpi.orig_level);
	}
      WT (((lang_tree_node *)t)->tpi.decl);
      break;
      
    case TS_CP_PTRMEM:
      WT (((lang_tree_node *)t)->ptrmem.member);
      break;

    case TS_CP_OVERLOAD:
      WT (((lang_tree_node *)t)->overload.function);
      WT (t->common.chain);
      break;
      
    case TS_CP_MODULE_VECTOR:
      gcc_unreachable (); /* Should never see.  */
      break;

    case TS_CP_BASELINK:
      WT (((lang_tree_node *)t)->baselink.binfo);
      WT (((lang_tree_node *)t)->baselink.functions);
      WT (((lang_tree_node *)t)->baselink.access_binfo);
      break;

    case TS_CP_TEMPLATE_DECL:
      /* Streamed with the template_decl node itself.  */
      gcc_checking_assert
	(TREE_VISITED (((lang_tree_node *)t)->template_decl.arguments));
      gcc_checking_assert
	(TREE_VISITED (((lang_tree_node *)t)->template_decl.result));
      break;

    case TS_CP_DEFERRED_PARSE:
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
      WT (((lang_tree_node *)t)->static_assertion.condition);
      WT (((lang_tree_node *)t)->static_assertion.message);
      if (streaming_p ())
	state->write_location
	  (*this, ((lang_tree_node *)t)->static_assertion.location);
      break;

    case TS_CP_ARGUMENT_PACK_SELECT:
      gcc_unreachable (); // FIXME:  Only reachable when we stream instantiations
      break;

    case TS_CP_TRAIT_EXPR:
      WT (((lang_tree_node *)t)->trait_expression.type1);
      WT (((lang_tree_node *)t)->trait_expression.type2);
      if (streaming_p ())
	WU (((lang_tree_node *)t)->trait_expression.kind);
      break;

    case TS_CP_LAMBDA_EXPR:
      WT (((lang_tree_node *)t)->lambda_expression.capture_list);
      WT (((lang_tree_node *)t)->lambda_expression.this_capture);
      WT (((lang_tree_node *)t)->lambda_expression.extra_scope);
      /* pending_proxies is a parse-time thing.  */
      gcc_assert (!((lang_tree_node *)t)->lambda_expression.pending_proxies);
      if (streaming_p ())
	{
	  state->write_location
	    (*this, ((lang_tree_node *)t)->lambda_expression.locus);
	  WU (((lang_tree_node *)t)->lambda_expression.default_capture_mode);
	  WU (((lang_tree_node *)t)->lambda_expression.discriminator);
	}
      break;

    case TS_CP_TEMPLATE_INFO:
      // TI_TEMPLATE -> TYPE
      WT (t->common.chain); // TI_ARGS
      // FIXME: typedefs_needing_access_checking
      break;

    case TS_CP_CONSTRAINT_INFO:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_USERDEF_LITERAL:
      gcc_unreachable (); /* Always expanded during parsing.  */
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

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      RT (t->decl_minimal.name);
      RT (t->decl_minimal.context);

      /* Don't zap the locus just yet, we don't record it correctly
	 and thus lose all location information.  */
      t->decl_minimal.locus = state->read_location (*this);
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

      if (TREE_CODE (t) != RECORD_TYPE
	  && TREE_CODE (t) != UNION_TYPE)
	{
	  RT (t->type_common.size);
	  RT (t->type_common.size_unit);
	}
      RT (t->type_common.attributes);

      RT (t->type_common.common.chain); /* TYPE_STUB_DECL.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      if (code != ENUMERAL_TYPE || ENUM_IS_SCOPED (t))
	RT (t->typed.type);
      else
	{
	  tree type = NULL;
	  unsigned itk;
	  RU (itk);
	  if (itk < itk_none)
	    {
	      unsigned precision;
	      RU (precision);
	      type = integer_types[itk];
	      if (!type || precision > TYPE_PRECISION (type))
		set_overrun ();
	      else if (precision != TYPE_PRECISION (type))
		{
		  type = build_distinct_type_copy (type);
		  TYPE_PRECISION (type) = precision;
		  set_min_and_max_values_for_integral_type (type, precision,
							    TYPE_SIGN (type));
		}
	    }
	  t->typed.type = type;
	}
    }

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
    if (const void *bytes = buf (sizeof (real_value)))
      TREE_REAL_CST_PTR (t)
	= reinterpret_cast<real_value *> (memcpy (ggc_alloc<real_value> (),
						  bytes, sizeof (real_value)));

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    gcc_unreachable (); /* Not suported in C++.  */

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    for (unsigned ix = vector_cst_encoded_nelts (t); ix--;)
      RT (VECTOR_CST_ENCODED_ELT (t, ix));

  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    gcc_checking_assert (code == STRING_CST);

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    {
      RT (TREE_REALPART (t));
      RT (TREE_IMAGPART (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    return false; /* Should never meet.  */

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
      switch (code)
	// FIXME: Perhaps this should be done with the later
	// polymorphic check?
	{
	default:
	  break;
	case VAR_DECL:
	  if (DECL_CONTEXT (t)
	      && TREE_CODE (DECL_CONTEXT (t)) != FUNCTION_DECL)
	    break;
	  /* FALLTHROUGH */
	case PARM_DECL:
	  if (DECL_HAS_VALUE_EXPR_P (t))
	    {
	      tree val = tree_node ();
	      SET_DECL_VALUE_EXPR (t, val);
	    }
	  /* FALLTHROUGH  */
	case CONST_DECL:
	  RT (t->decl_common.initial);
	  break;
	}
      RT (t->decl_common.abstract_origin);
      /* decl_common.initial.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    {} // FIXME?

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    {
      if (code == TYPE_DECL)
	RT (t->decl_non_common.result);
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
      RT (t->function_decl.personality);
      RT (t->function_decl.function_specific_target);
      RT (t->function_decl.function_specific_optimization);
      RT (t->function_decl.vindex);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    return false;

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_WITH_LANG_SPECIFIC))
    { /* Nothing to do.  */ }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      /* Records and unions hold FIELDS, VFIELD & BINFO on these
	 things.  */
      if (!RECORD_OR_UNION_CODE_P (code) && code != ENUMERAL_TYPE)
	{
	  /* This is not clobbering TYPE_CACHED_VALUES, because this
	     is a new type being read in, so there aren't any.  */
	  gcc_checking_assert (!TYPE_CACHED_VALUES_P (t));
	  RT (t->type_non_common.values);
	  RT (t->type_non_common.maxval);

	  /* POINTER and REFERENCE types hold NEXT_{PTR,REF}_TO.  We
	     store a marker there to indicate whether we're on the
	     referred to type's pointer/reference to list.  */
	  RT (t->type_non_common.minval);
	  if (POINTER_TYPE_P (t))
	    {
	      if (t->type_non_common.minval
		  && t->type_non_common.minval != t)
		{
		  t->type_non_common.minval = NULL_TREE;
		  set_overrun ();
		}
	    }
	}

      if (TREE_CODE (t) != POINTER_TYPE)
	RT (t->type_non_common.lang_1);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    {
      RT (t->list.purpose);
      RT (t->list.value);
      RT (t->list.common.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    {
      for (unsigned ix = TREE_VEC_LENGTH (t); ix--;)
	RT (TREE_VEC_ELT (t, ix));
      RT (t->type_common.common.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    {
      t->exp.locus = state->read_location (*this);
      bool vl = TREE_CODE_CLASS (code) == tcc_vl_exp;
      for (unsigned ix = (vl ? VL_EXP_OPERAND_LENGTH (t)
			  : TREE_OPERAND_LENGTH (t)); ix-- != vl;)
	RT (TREE_OPERAND (t, ix));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_SSA_NAME))
    return false;

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    {
      RT (t->block.supercontext);
      t->block.vars = chained_decls ();
      RT (t->block.abstract_origin);
      // FIXME: nonlocalized_vars, fragment_origin, fragment_chain
      RT (t->block.subblocks);
      RT (t->block.chain);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    {
      RT (t->binfo.common.chain);
      RT (t->binfo.offset);
      RT (t->binfo.inheritance);
      RT (t->binfo.vtable);
      RT (t->binfo.virtuals);
      RT (t->binfo.vptr_field);
      RT (t->binfo.vtt_subvtt);
      RT (t->binfo.vtt_vptr);

      BINFO_BASE_ACCESSES (t) = tree_vec ();
      if (!get_overrun ())
	{
	  unsigned num = vec_safe_length (BINFO_BASE_ACCESSES (t));
	  for (unsigned ix = 0; ix != num; ix++)
	    BINFO_BASE_APPEND (t, tree_node ());
	}
    }

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
      RT (((lang_tree_node *)t)->ptrmem.member);
      break;

    case TS_CP_OVERLOAD:
      RT (((lang_tree_node *)t)->overload.function);
      RT (t->common.chain);
      break;

    case TS_CP_MODULE_VECTOR:
      return false;

    case TS_CP_BASELINK:
      RT (((lang_tree_node *)t)->baselink.binfo);
      RT (((lang_tree_node *)t)->baselink.functions);
      RT (((lang_tree_node *)t)->baselink.access_binfo);
      break;

    case TS_CP_TEMPLATE_DECL:
      /* Streamed when reading the raw template decl itself.  */
      gcc_assert (((lang_tree_node *)t)->template_decl.arguments);
      gcc_assert (((lang_tree_node *)t)->template_decl.result);
      break;

    case TS_CP_DEFERRED_PARSE:
      return false;

    case TS_CP_DEFERRED_NOEXCEPT:
      RT (((lang_tree_node *)t)->deferred_noexcept.pattern);
      RT (((lang_tree_node *)t)->deferred_noexcept.args);
      break;

    case TS_CP_IDENTIFIER:
      return false; /* Should never see.  */

    case TS_CP_STATIC_ASSERT:
      RT (((lang_tree_node *)t)->static_assertion.condition);
      RT (((lang_tree_node *)t)->static_assertion.message);
      ((lang_tree_node *)t)->static_assertion.location
	= state->read_location (*this);
      break;

    case TS_CP_ARGUMENT_PACK_SELECT:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_TRAIT_EXPR:
      RT (((lang_tree_node *)t)->trait_expression.type1);
      RT (((lang_tree_node *)t)->trait_expression.type2);
      RUC (cp_trait_kind, ((lang_tree_node *)t)->trait_expression.kind);
      break;

    case TS_CP_LAMBDA_EXPR:
      RT (((lang_tree_node *)t)->lambda_expression.capture_list);
      RT (((lang_tree_node *)t)->lambda_expression.this_capture);
      RT (((lang_tree_node *)t)->lambda_expression.extra_scope);
      /* lambda_expression.pending_proxies is NULL  */
      ((lang_tree_node *)t)->lambda_expression.locus
	= state->read_location (*this);
      RUC (cp_lambda_default_capture_mode_type,
	   ((lang_tree_node *)t)->lambda_expression.default_capture_mode);
      RU (((lang_tree_node *)t)->lambda_expression.discriminator);
      break;

    case TS_CP_TEMPLATE_INFO:
      // TI_TEMPLATE -> TYPE
      RT (t->common.chain); // TI_ARGS
      // FIXME: typedefs_needing_access_checking
      break;

    case TS_CP_CONSTRAINT_INFO:
      gcc_unreachable (); // FIXME
      break;

    case TS_CP_USERDEF_LITERAL:
      return false;  /* Should never see.  */
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
      else
	WT (lang->u.fn.u5.cloned_function);

      if (FNDECL_USED_AUTO (t))
	WT (lang->u.fn.u.saved_auto_return_type);

      /* FALLTHROUGH.  */

    case lds_min:  /* lang_decl_min.  */
      WT (lang->u.min.template_info);
      WT (lang->u.min.access);
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

	if (DECL_CLASS_SCOPE_P (t))
	  RT (lang->u.fn.context);

	if (lang->u.fn.thunk_p)
	  {
	    RT (lang->u.fn.befriending_classes);
	    lang->u.fn.u5.fixed_offset = wi ();
	  }
	else
	  RT (lang->u.fn.u5.cloned_function);

	if (FNDECL_USED_AUTO (t))
	  RT (lang->u.fn.u.saved_auto_return_type);
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

tree
trees_out::tree_binfo (tree binfo, int depth, bool via_virt)
{
  tree dom;

  if (tree inh = BINFO_INHERITANCE_CHAIN (binfo))
    {
      bool is_virt = BINFO_VIRTUAL_P (binfo);
      dom = tree_binfo (inh, depth + !via_virt, is_virt || via_virt);
      if (!via_virt && streaming_p ())
	{
	  vec<tree, va_gc> *binfo_vec;
	  if (is_virt)
	    /* A virtual base.  Look on the CLASSTYPE_VIRTUALS.  */
	    binfo_vec = CLASSTYPE_VBASECLASSES (dom);
	  else
	    /* Look along BINFO_BASE_BINFOS (inh).  */
	    binfo_vec = BINFO_BASE_BINFOS (inh);
	  unsigned ix;
	  for (ix = 0; (*binfo_vec)[ix] != binfo; ix++)
	    ;
	  dump (dumper::TREE)
	    && dump ("Wrote derived %sBINFO %u %N of %N",
		     is_virt ? "virtual " : "", ix, binfo, inh);
	  u (ix);
	}
    }
  else
    {
      dom = BINFO_TYPE (binfo);
      tree_ctx (dom, false, NULL);

      if (streaming_p ())
	{
	  dump (dumper::TREE) && dump ("Wrote dominating BINFO %N", dom);
	  i (via_virt ? -depth : depth);
	}
    }
  return dom;
}

tree
trees_in::tree_binfo ()
{
  tree dom = tree_node ();
  dump (dumper::TREE) && dump ("Read dominating binfo %N", dom);
  int depth = i ();
  tree binfo = TYPE_BINFO (dom);
  if (depth)
    {
      vec<tree, va_gc> *binfo_vec = NULL;
      if (depth < 0)
	{
	  /* A virtual base.  Look on the CLASSTYPE_VIRTUALS.  */
	  binfo_vec = CLASSTYPE_VBASECLASSES (dom);
	  depth = -depth;
	}
      for (; depth--; binfo_vec = NULL)
	{
	  if (!binfo_vec)
	    binfo_vec = BINFO_BASE_BINFOS (binfo);
	  unsigned ix = u ();
	  if (vec_safe_length (binfo_vec) < ix)
	    {
	      set_overrun ();
	      binfo = NULL_TREE;
	      break;
	    }
	  else
	    binfo = (*binfo_vec)[ix];
	  dump (dumper::TREE) && dump ("Read derived BINFO %N", binfo);
	}
    }
  return binfo;
}

/* Write out the bools of T, including information about any
   LANG_SPECIFIC information.  Including allocation of any lang
   specific object.  */

void
trees_out::tree_node_bools (tree t)
{
  gcc_checking_assert (streaming_p ());

  /* We should never stream a namespace.  */
  gcc_checking_assert (TREE_CODE (t) != NAMESPACE_DECL);

  core_bools (t);

  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case tcc_declaration:
      {
	/* The only decls we should stream out are those from this
	   module, a partition or the global module.  All other decls
	   should be by name.  */
	gcc_checking_assert ((*modules)[MAYBE_DECL_MODULE_OWNER (t)]->remap
			     < MODULE_IMPORT_BASE || t != get_module_owner (t));

	bool specific = DECL_LANG_SPECIFIC (t) != NULL;
	b (specific);
	if (specific && VAR_P (t))
	  b (DECL_DECOMPOSITION_P (t));
	if (specific)
	  lang_decl_bools (t);
      }
      break;

    case tcc_type:
      {
	bool specific = (TYPE_MAIN_VARIANT (t) == t
			 && TYPE_LANG_SPECIFIC (t) != NULL);
	gcc_assert (TYPE_LANG_SPECIFIC (t)
		    == TYPE_LANG_SPECIFIC (TYPE_MAIN_VARIANT (t)));

	b (specific);
	if (specific)
	  lang_type_bools (t);
      }
      break;

    default:
      break;
    }

  bflush ();
}

bool
trees_in::tree_node_bools (tree t)
{
  bool ok = core_bools (t);

  if (ok)
    switch (TREE_CODE_CLASS (TREE_CODE (t)))
      {
      case tcc_declaration:
	if (b ())
	  {
	    bool decomp = VAR_P (t) && b ();

	    ok = maybe_add_lang_decl_raw (t, decomp);
	    if (ok)
	      ok = lang_decl_bools (t);
	}
	break;

      case tcc_type:
	if (b ())
	  {
	    ok = maybe_add_lang_type_raw (t);
	    if (ok)
	      ok = lang_type_bools (t);
	  }
	break;

      default:
	break;
      }

  bflush ();
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


/* If T is a back reference, fixed reference or NULL, write out it's
   code and return WK_none.  Otherwise return WK_body if we must write
   by value, or WK_normal otherwise.  */

walk_kind
trees_out::ref_node (tree t)
{
  if (!t)
    {
      if (streaming_p ())
	{
	  /* NULL_TREE -> tt_null.  */
	  nulls++;
	  i (tt_null);
	}
      return WK_none;
    }

  if (!TREE_VISITED (t))
    return WK_normal;

  /* An already-visited tree.  It must be in the map.  */
  int *val_ptr = tree_map.get (t);
  int val = *val_ptr;

  if (val == tag_forced)
    /* An entry we should walk into.  */
    return WK_body;
  else if (val == tag_mergeable
	   || val == tag_clone)
    {
      gcc_assert (streaming_p ());
      /* We're now merging this tag.  */
      *val_ptr = tag_merging;
      return walk_kind (val - tag_mergeable + WK_mergeable);
    }

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
  else
    /* We should never meet a node being merged.  */
    gcc_unreachable ();

  if (streaming_p ())
    {
      refs++;
      dump (dumper::TREE)
	&& dump ("Wrote %s:%d %C:%N%S", kind, val, TREE_CODE (t), t, t);
    }
  return WK_none;
}

/* CTX is a context of some node.  NEED_CONTENTS is true if we're
   ultimately looking for something inside CTX.  */
// FIXME: return indicator if we discoverd a voldemort
void
trees_out::tree_ctx (tree ctx, bool need_contents, tree inner_decl)
{
  walk_kind walk = ref_node (ctx);
  if (walk != WK_none)
    {
      bool by_value = false;

      if (TYPE_P (ctx))
	by_value = tree_type (ctx, walk, need_contents);
      else if (TREE_CODE (ctx) == NAMESPACE_DECL)
	tree_namespace (ctx, walk, inner_decl);
      else
	by_value = tree_decl (ctx, walk, need_contents);

      if (by_value)
	tree_value (ctx, walk);
    }
}

void
trees_out::tree_namespace (tree ns, walk_kind ref, tree inner_decl)
{
  if (streaming_p ())
    {
      i (tt_namespace);
      unsigned owner = MODULE_NONE;
      if (!TREE_PUBLIC (ns))
	{
	  owner = MAYBE_DECL_MODULE_OWNER (inner_decl);
	  if (owner >= MODULE_IMPORT_BASE)
	    owner = (*modules)[owner]->remap;
	}
      u (owner);
      tree_ctx (CP_DECL_CONTEXT (ns), true, ns);
      tree_node (DECL_NAME (ns));
    }
  else if (ref == WK_body)
    tree_ctx (CP_DECL_CONTEXT (ns), true, ns);
  else if (DECL_SOURCE_LOCATION (ns) != BUILTINS_LOCATION)
    dep_hash->add_dependency (ns, depset::EK_NAMESPACE);

  int tag = insert (ns, ref);
  if (streaming_p ())
    dump (dumper::TREE)
      && dump ("Wrote%s namespace:%d %C:%N@%M",
	       TREE_PUBLIC (ns) ? " public" : "", tag, TREE_CODE (ns), ns,
	       TREE_PUBLIC (ns) ? NULL
	       : (*modules)[MAYBE_DECL_MODULE_OWNER (inner_decl)]);
}

/* Reference DECL.  REF indicates the walk kind we are performing.
   LOOKING_INSIDE is true if we're looking up a member inside the
   scope of DECL.  Return true if we should write this decl by
   value.  */

bool
trees_out::tree_decl (tree decl, walk_kind ref, bool looking_inside)
{
  gcc_checking_assert (DECL_P (decl)
		       && TREE_CODE (decl) != NAMESPACE_DECL);

  if (ref == WK_body || ref == WK_mergeable || ref == WK_clone)
    {
      /* If we requested by-value, this better not be a cross-module
	 import.  */
      gcc_checking_assert ((*modules)[MAYBE_DECL_MODULE_OWNER
				      (get_module_owner (decl, true))]->remap
			   < MODULE_IMPORT_BASE);

      if (!streaming_p ()
	  && DECL_MAYBE_IN_CHARGE_CDTOR_P (decl))
	{
	  // FIXME: check we're the template?

	  /* Insert a dependency the clones of DECL.  */
	  tree clone_decl;
	  FOR_EVERY_CLONE (clone_decl, decl)
	    dep_hash->add_clone (clone_decl, decl);
	}

      return true;
    }

  if (ref_node (decl) == WK_none)
    /* If this is a fixed decl, we're done.  */
    return false;

  if (TREE_CODE (decl) == VAR_DECL && DECL_TINFO_P (decl))
    {
      /* A typeinfo object -> tt_tinfo_var.  These need recreating by
	 the loader.  The type it is for is stashed on the name's
	 TREE_TYPE.  */
      tree type = TREE_TYPE (DECL_NAME (decl));
      if (streaming_p ())
	i (tt_tinfo_var);
      tree_node (type);
      int tag = insert (decl);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote typeinfo:%d %S for %N", tag, decl, type);
      return false;
    }

  if (TREE_CODE (decl) == VAR_DECL && DECL_ARTIFICIAL (decl))
    {
      tree ctx = CP_DECL_CONTEXT (decl);
      if (TREE_CODE (ctx) == RECORD_TYPE && TYPE_LANG_SPECIFIC (ctx))
	{
	  /* Try a VTABLE.  */
	  unsigned ix = 0;
	  for (tree vtables = CLASSTYPE_VTABLES (ctx);
	       vtables; ix++, vtables = DECL_CHAIN (vtables))
	    if (vtables == decl)
	      {
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
	}
    }

  if (TREE_CODE (decl) == TYPE_DECL && DECL_TINFO_P (decl))
    {
      /* A typeinfo pseudo type -> tt_tinfo_typedef.  */
      unsigned ix = get_pseudo_tinfo_index (TREE_TYPE (decl));

      if (streaming_p ())
	{
	  i (tt_tinfo_typedef);
	  u (ix);
	}
      int tag = insert (decl);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote:%d typeinfo decl %u %N", tag, ix, decl);
      tag = insert (TREE_TYPE (decl));
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote:%d typeinfo type %u %N", tag, ix, decl);
      return false;
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

  if (TREE_CODE (decl) == TYPE_DECL
      && decl == TYPE_STUB_DECL (TREE_TYPE (decl))
      && TREE_CODE (TREE_TYPE (decl)) == TYPENAME_TYPE)
    {
      /* A type_decl of a typename.  */
      if (streaming_p ())
	i (tt_typename_decl);
      tree type = TREE_TYPE (decl);
      tree_node (TYPE_CONTEXT (type));
      tree_node (DECL_NAME (decl));
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

      int tag = insert (decl);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote:%d typename decl %P", tag,
		   TYPE_CONTEXT (type), DECL_NAME (decl));
      int type_tag = insert (type);
      if (streaming_p ())
	dump (dumper::TREE) && dump ("Wrote:%d typename type", type_tag);

      return false;
    }

  if (TREE_CODE (decl) == PARM_DECL
      || !DECL_CONTEXT (decl)
      || (TREE_CODE (decl) == TEMPLATE_DECL &&
	  TREE_CODE (TREE_TYPE (decl)) == TEMPLATE_TEMPLATE_PARM))
    {
      /* If we cannot name this, it better be the inner-most decl we
	 asked about.  */
      gcc_assert (!looking_inside);
      return true;
    }

  int use_tpl = -1;
  tree ti = node_template_info (decl, use_tpl);
  tree tpl = NULL_TREE;
  /* TI_TEMPLATE is not a TEMPLATE_DECL for (some) friends, because
     we matched a non-template.  */
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
      gcc_checking_assert (TREE_VISITED (decl));
      if (DECL_IMPLICIT_TYPEDEF_P (decl))
	gcc_checking_assert (TREE_VISITED (TREE_TYPE (decl)));
      return false;
    }

  if (DECL_CLONED_FUNCTION_P (decl))
    {
      tree target = get_clone_target (decl);
      if (streaming_p ())
	i (tt_clone_ref);

      tree_node (target);
      tree_node (DECL_NAME (decl));
      int tag = insert (decl);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote:%d clone %N of %N", tag, DECL_NAME (decl), target);
      return false;
    }

  if (TREE_CODE (decl) == TEMPLATE_DECL
      && DECL_CHAIN (decl)
      && RECORD_OR_UNION_CODE_P (TREE_CODE (DECL_CHAIN (decl))))
    {
      /* A (local template) friend of a template.  */
      if (streaming_p ())
	{
	  i (tt_friend_template);
	  dump (dumper::TREE)
	    && dump ("Writing friend template %C:%N", TREE_CODE (decl), decl);
	}

      tree klass = DECL_CHAIN (decl);
      tree_node (klass);

      if (streaming_p ())
	{
	  unsigned ix = 0;
	  for (tree decls = CLASSTYPE_DECL_LIST (klass);;
	       decls = TREE_CHAIN (decls))
	    if (!TREE_PURPOSE (decls))
	      {
		if (tree frnd = friend_from_decl_list (TREE_VALUE (decls)))
		  if (frnd == decl)
		    break;

		/* Count every friend to make streaming in simpler.  */
		ix++;
	      }
	  u (ix);
	  dump (dumper::TREE)
	    && dump ("Wrote friend %N[%u], %C:%N",
		     klass, ix, TREE_CODE (decl), decl);
	}

      return false;
    }

  const char *kind = NULL;
  unsigned owner = MODULE_UNKNOWN;
  if (use_tpl > 0)
    {
      /* Some kind of specialization.   */
      /* Not all specializations are in the table, so we have to query
         it.  Those that are not there are findable by name, but a
         specializations in that their definitions can be generated
         anywhere -- we need to write them out if we did it.  */
      depset *dep;
      if (!streaming_p ())
	{
	  owner = MAYBE_DECL_MODULE_OWNER (decl);
	  if (owner >= MODULE_IMPORT_BASE)
	    owner = (*modules)[owner]->remap;

	  dep = dep_hash->add_dependency (decl, depset::EK_MAYBE_SPEC,
					  owner >= MODULE_IMPORT_BASE);
	  /* We should always insert or find something.  */
	  gcc_assert (dep);
	}
      else
	dep = dep_hash->find_entity (decl);

      if (dep->get_entity_kind () == depset::EK_REDIRECT)
	{
	  /* The DECL_TEMPLATE_RESULT of a partial specialization.
	     Write the partial specialization's template.  */
	  depset *redirect = dep->deps[0];
	  gcc_checking_assert ((redirect->get_entity_kind ()
				== depset::EK_SPECIALIZATION)
			       && redirect->is_partial ());
	  tpl = redirect->get_entity ();
	  goto partial_template;
	}
      else if (dep->get_entity_kind () == depset::EK_SPECIALIZATION)
	{
	  kind = "specialization";
	  goto insert;
	}
      else
	gcc_assert (dep->get_entity_kind () == depset::EK_DECL);
    }

  {
    /* Find the owning module and determine what to do.  */
    tree owner_decl = get_module_owner (decl, true);
    owner = MAYBE_DECL_MODULE_OWNER (owner_decl);
    if (owner >= MODULE_IMPORT_BASE)
      owner = (*modules)[owner]->remap;

    tree ctx = CP_DECL_CONTEXT (decl);
    bool is_import = owner >= MODULE_IMPORT_BASE;

    if (TREE_CODE (ctx) == FUNCTION_DECL)
      {
	/* We cannot lookup by name inside a function.  */
	if (!streaming_p ()
	    && (dep_hash->sneakoscope || is_import))
	  {
	    /* We've found a voldemort type.  Add it as a
	       dependency.  */
	    dep_hash->add_dependency (decl, depset::EK_UNNAMED, is_import);
	    kind = "unnamed";
	    goto insert;
	  }

	/* Some internal entity of the function.  Do by value.  */
	gcc_assert (!is_import);
	return true;
      }

    /* A named decl -> tt_named_decl.  */
    tree name = DECL_NAME (decl);
    if (streaming_p ())
      {
	unsigned code = tt_named;
	int ident = -2;

	tree proxy = decl;
	if (TREE_CODE (decl) == TEMPLATE_DECL
	    && (RECORD_OR_UNION_CODE_P (TREE_CODE (ctx))
		|| TREE_CODE (ctx) == ENUMERAL_TYPE)
	    && !DECL_MEMBER_TEMPLATE_P (decl))
	  {
	    /* An implicit member template.  Look for the templated
	       decl.  */
	    proxy = DECL_TEMPLATE_RESULT (decl);
	    code = tt_implicit_template;
	  }

	if (name && IDENTIFIER_ANON_P (name))
	  {
	    if (TREE_CODE (ctx) == NAMESPACE_DECL)
	      {
		gcc_assert (DECL_IMPLICIT_TYPEDEF_P (decl) && code == tt_named);
		proxy = TYPE_NAME (TREE_TYPE (decl));
		name = DECL_NAME (proxy);
		gcc_checking_assert (MAYBE_DECL_MODULE_OWNER (proxy) == owner);
		gcc_checking_assert (TYPE_STUB_DECL (TREE_TYPE (proxy)) == decl);
		gcc_checking_assert (code == tt_named);
		code = tt_anon;
	      }
	    else
	      name = NULL_TREE;
	  }

	if (TREE_CODE (ctx) == NAMESPACE_DECL && owner < MODULE_IMPORT_BASE)
	  {
	    /* Look directly into the binding depset, as that's
	       what importers will observe.  */
	    depset *bind = dep_hash->find_binding (ctx, name);
	    /* Only reference earlier bindings.  */
	    gcc_assert (bind && bind->section < section);

	    for (ident = bind->deps.length (); ident--;)
	      if (bind->deps[ident]->get_entity () == proxy)
		break;
	    gcc_assert (ident >= 0);

	    if (bind->deps.length () > 1
		&& DECL_IMPLICIT_TYPEDEF_P (bind->deps[0]->get_entity ()))
	      ident--;

	    kind = owner == MODULE_NONE ? "GMF" : "purview";
	    /* Any GMF entities need to be looked up on this module's slot.  */
	    owner = MODULE_PURVIEW;
	  }
	else
	  {
	    ident = get_lookup_ident (ctx, name, owner, proxy);
	    /* Make sure we can find it by name.  */
	    gcc_checking_assert
	      (proxy == lookup_by_ident (ctx, name, owner, ident));
	    kind = is_import ? "import" : "member";
	  }

	i (code);
	tree_ctx (ctx, true, decl);
	tree_node (name);
	u (owner);
	i (ident);
      }
    else
      {
	if (is_import)
	  ;
	else if (TREE_CODE (ctx) != NAMESPACE_DECL)
	  tree_ctx (ctx, true, decl);
	else
	  dep_hash->add_dependency (decl, depset::EK_DECL);

	tree_node (name);
      }
  }

 insert:
  int tag = insert (decl);
  if (streaming_p ())
    dump (dumper::TREE)
      && dump ("Wrote %s:%d %C:%N@%M", kind, tag, TREE_CODE (decl), decl,
	       owner == MODULE_UNKNOWN ? NULL : (*modules)[owner]);

  tree proxy = decl;
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      proxy = DECL_TEMPLATE_RESULT (decl);

      tag = insert (proxy);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote template's result:%d %C:%N",
		   tag, TREE_CODE (proxy), proxy);
    }

  if (TREE_CODE (proxy) == TYPE_DECL
      && (DECL_ORIGINAL_TYPE (proxy)
	  || TYPE_STUB_DECL (TREE_TYPE (proxy)) == proxy))
    {
      /* Make sure the type is in the map too.  Otherwise we get
	 different RECORD_TYPEs for the same type, and things go
	 south.  */
      tree proxy_type = TREE_TYPE (proxy);
      tag = insert (proxy_type);
      if (streaming_p ())
	dump (dumper::TREE) && dump ("Wrote decl's type:%d %C:%N", tag,
				     TREE_CODE (proxy_type), proxy_type);
    }

  return false;
}

bool
trees_out::tree_type (tree type, walk_kind ref, bool looking_inside)
{
  gcc_assert (TYPE_P (type));
  if (ref == WK_body)
    return true;

  tree name = TYPE_NAME (type);
  
  if (name
      && TREE_CODE (name) == TYPE_DECL
      && DECL_ORIGINAL_TYPE (name))
    /* A typedef type that is not the original type.  */;
  else if (TYPE_PTRMEMFUNC_P (type))
    {
      // FIXME: Qualified ptr to mem?  Move this portion after
      // tt_variant_type emission.
      tree fn_type = TYPE_PTRMEMFUNC_FN_TYPE (type);
      if (streaming_p ())
	i (tt_ptrmem_type);
      tree_node (fn_type);
      int tag = insert (type);
      if (streaming_p ())
	dump (dumper::TREE) && dump ("Writen:%d ptrmem type", tag);
      return false;
    }
  else
    {
      name = TYPE_STUB_DECL (type);
      if (name && DECL_IMPLICIT_TYPEDEF_P (name))
	/* Implicit typedef.  */;
      else if (name && TREE_CODE (type) == TYPENAME_TYPE)
	/* A typename type.  */;
      else if (name && TREE_CODE (name) == TYPE_DECL && DECL_TINFO_P (name))
	/* A tinfo type.  */;
      else if (type == TYPE_MAIN_VARIANT (type)
	       && (TREE_CODE (type) == TEMPLATE_TEMPLATE_PARM
		   || TREE_CODE (type) == TEMPLATE_TYPE_PARM
		   || TREE_CODE (type) == BOUND_TEMPLATE_TEMPLATE_PARM))
	/* A template parameter.  */;
      else
	{
	  gcc_checking_assert (TREE_CODE (type) != TEMPLATE_TEMPLATE_PARM
			       || (type != TYPE_MAIN_VARIANT (type)
				   && TREE_VISITED (TYPE_MAIN_VARIANT (type))
				   && TREE_VISITED (name)));
	  name = NULL_TREE;
	}
    }

  if (name)
    {
      /* Make sure this is not a named builtin. We should find
	 those some other way to be canonically correct.  */
      gcc_assert (DECL_SOURCE_LOCATION (name) != BUILTINS_LOCATION
		  || DECL_TINFO_P (name));

      if (streaming_p ())
	{
	  i (tt_typedef_type);
	  dump (dumper::TREE)
	    && dump ("Writing typedef %C:%N%S",
		     TREE_CODE (name), name, name);
	}
      tree_ctx (name, looking_inside, NULL_TREE);
      if (streaming_p ())
	dump (dumper::TREE) && dump ("Wrote typedef %C:%N%S",
				     TREE_CODE (name), name, name);
      if (ref_node (type) == WK_none)
	/* We emitted the type node via the name.  */
	return false;
    }

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
	  // FIXME: Align
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

      tree_node (error_mark_node);  // FIXME: Attribs

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
      return false;
    }

  switch (TREE_CODE (type))
    {
    default:
      // FIXME: Hm, does this have NULL TREE_TYPE?
      // FIXME: Hook other C++-specific types for reconstruction?
      break;

    case VECTOR_TYPE:
    case ARRAY_TYPE:
    case OFFSET_TYPE:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    case REFERENCE_TYPE:
    case POINTER_TYPE:
    case COMPLEX_TYPE:
    case TYPE_ARGUMENT_PACK:
    case TYPE_PACK_EXPANSION:
    case DECLTYPE_TYPE:
    case TYPEOF_TYPE:
    case UNDERLYING_TYPE:
      {
	if (streaming_p ())
	  {
	    u (tt_derived_type);
	    u (TREE_CODE (type));
	  }
	tree_node (TREE_TYPE (type));
	switch (TREE_CODE (type))
	  {
	  default:
	    gcc_unreachable ();

	  case TYPEOF_TYPE:
	  case DECLTYPE_TYPE:
	  case UNDERLYING_TYPE:
	    tree_node (TYPE_VALUES_RAW (type));
	    if (TREE_CODE (type) == DECLTYPE_TYPE)
	      /* We stash a whole bunch of things into decltype's
		 flags.  */
	      if (streaming_p ())
		tree_node_bools (type);
	    break;

	  case TYPE_PACK_EXPANSION:
	    if (streaming_p ())
	      u (PACK_EXPANSION_LOCAL_P (type));
	    tree_node (PACK_EXPANSION_PARAMETER_PACKS (type));
	    break;

	  case TYPE_ARGUMENT_PACK:
	    /* No additional data.  */
	    break;

	  case VECTOR_TYPE:
	    if (streaming_p ())
	      {
		gcc_checking_assert(NUM_POLY_INT_COEFFS == 1);
		poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (type);
		wu (static_cast<unsigned HOST_WIDE_INT> (nunits.to_constant ()));
	      }
	    break;

	  case COMPLEX_TYPE:
	    /* No additional data.  */
	    break;

	  case ARRAY_TYPE:
	    tree_node (TYPE_DOMAIN (type));
	    break;

	  case OFFSET_TYPE:
	    tree_node (TYPE_OFFSET_BASETYPE (type));
	    break;

	  case FUNCTION_TYPE:
	    tree_node (TYPE_ARG_TYPES (type));
	    break;

	  case METHOD_TYPE:
	    tree_node (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (type))));
	    tree_node (TREE_CHAIN (TYPE_ARG_TYPES (type)));
	    break;

	  case REFERENCE_TYPE:
	    if (streaming_p ())
	      u (TYPE_REF_IS_RVALUE (type));
	    break;

	  case POINTER_TYPE:
	    /* No additional data.  */
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
	  
	return false;
      }
    }

  return true;
}

/* T is a node that must be written by value.  WALK indicates the kind
   of by-valueness.  */

void
trees_out::tree_value (tree t, walk_kind walk)
{
  /* We should never walk into a thunk by accident.  */
  gcc_assert (walk == WK_body || !DECL_THUNK_P (t));
#if 0
  // FIXME: Remove all the get-outs for this assert.
  gcc_assert (!TYPE_P (t)
	      /* Restricted range integer type.  */
	      || TREE_CODE (t) == INTEGER_TYPE);
#endif

  if (walk == WK_normal)
    walk = WK_body;
  gcc_checking_assert (unsigned (walk - WK_body) <= (WK_clone - WK_body));

  if (streaming_p ())
    {
      if (CHECKING_P && DECL_P (t))
	{
	  /* Never start in the middle of a template.  */
	  int use_tpl = -1;
	  if (tree ti = node_template_info (t, use_tpl))
	    gcc_checking_assert (TREE_CODE (TI_TEMPLATE (ti)) == OVERLOAD
				 || (DECL_TEMPLATE_RESULT (TI_TEMPLATE (ti))
				     != t));
	}

      /* A new node -> tt_node, tt_mergeable or tt_clone.  */
      unique++;
      i (walk - WK_body + tt_node);
      u (TREE_CODE (t));
      start (t);

      if (walk == WK_mergeable && !state->is_header ())
	/* Tell the importer whether this is a global module entity,
	   or a module entity.  This bool merges into the next block
	   of bools.  Sneaky.  */
	b (MAYBE_DECL_MODULE_OWNER (t) != MODULE_NONE);
      tree_node_bools (t);
    }

  int tag = insert (t, walk);
  if (streaming_p ())
    dump (dumper::TREE)
      && dump ("Writing:%d %C:%N%S%s", tag, TREE_CODE (t), t, t,
	       DECL_P (t) && DECL_MODULE_EXPORT_P (t) ? " (exported)" : "");

  if (walk != WK_body)
    walk = WK_merging;
  // FIXME: If mergeable, mark function parms etc as mergeable too

  tree inner = t;
  int inner_tag = 0;

  if (TREE_CODE (t) == TEMPLATE_DECL)
    {
      inner = DECL_TEMPLATE_RESULT (t);

      if (streaming_p ())
	{
	  u (TREE_CODE (inner));
	  start (inner);
	  tree_node_bools (inner);
	}

      inner_tag = insert (inner, walk);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Writing:%d %C:%N%S", inner_tag,
		   TREE_CODE (inner), inner, inner);
    }

  tree type = NULL_TREE;
  int type_tag = 0;
  if (TREE_CODE (inner) == TYPE_DECL)
    {
      // FIXME: anon-structured type with typedef name?
      type = TREE_TYPE (inner);
      bool is_stub = ((TREE_CODE (type) == TEMPLATE_TEMPLATE_PARM ? t : inner)
		      == TYPE_STUB_DECL (type));

      if (streaming_p ())
	u (is_stub ? TREE_CODE (type) : 0);

      if (is_stub)
	{
	  if (streaming_p ())
	    {
	      start (type);
	      tree_node_bools (type);
	    }

	  type_tag = insert (type, walk);
	  if (streaming_p ())
	    dump (dumper::TREE)
	      && dump ("Writing:%d %C:%N%S", type_tag,
		       TREE_CODE (type), type, type);
	}
      else
	/* Regular typedef.  */
	type = NULL_TREE;
      gcc_assert (!type || !DECL_ORIGINAL_TYPE (inner));
    }

  if (walk != WK_body)
    {
      /* Now write out the merging information, and then really
	 install the tag values.  */
      merge_kind mk = key_mergeable (dep_hash->find_entity (t));

      dump (dumper::MERGE)
	&& dump ("Wrote:%d's %s merge key %C:%N", tag,
		 merge_kind_name[mk], TREE_CODE (t), t);

      /* This is the point where the importer determines whether it
	 really has a new decl or not.  So it is safe to refer to
	 these nodes.  */
      mark_merged (t, tag);
      if (inner_tag != 0)
	mark_merged (inner, inner_tag);
      if (type_tag != 0)
	mark_merged (type, type_tag);
      if (TREE_CODE (inner) == FUNCTION_DECL)
	fn_parms_fini (inner, t);
    }

  if (inner_tag != 0)
    {
      /* Template pieces.  */
      tree_node (DECL_TEMPLATE_PARMS (t));
      /* DECL_TEMPLATE_RESULT is inner, so no need to stream that.  */
      tree_node_vals (t);
    }

  tree_node_vals (inner);

  if (type)
    tree_node_vals (type);

  if (TREE_CODE (inner) == TYPE_DECL
      && DECL_ORIGINAL_TYPE (inner))
    {
      /* A typedef type.  */
      int type_tag = insert (TREE_TYPE (inner));
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Cloned:%d typedef %C:%N", type_tag,
		   TREE_CODE (TREE_TYPE (inner)), TREE_TYPE (inner));
    }
    
  if (streaming_p ())
    dump (dumper::TREE) && dump ("Written:%d %C:%N", tag, TREE_CODE (t), t);
}

tree
trees_in::tree_value (walk_kind walk)
{
  int tag = 0;
  bool is_mod = false;

  tree res = start (u ());
  if (res)
    {
      if (walk == WK_mergeable && !state->is_header ())
	/* See note in trees_out about where this bool is sequenced.  */
	is_mod = b ();

      if (!tree_node_bools (res))
	res = NULL_TREE;
    }
  
  /* Insert into map.  */
  tag = insert (res);
  if (res)
    dump (dumper::TREE)
      && dump ("Reading %s:%d %C", walk_kind_name[walk], tag, TREE_CODE (res));

  tree inner = res;
  int inner_tag = 0;
  if (res && TREE_CODE (res) == TEMPLATE_DECL)
    {
      inner = start (u ());
      if (inner)
	{
	  DECL_TEMPLATE_RESULT (res) = inner;

	  if (!tree_node_bools (inner))
	    res = NULL_TREE;
	}
      else
	res = NULL_TREE;

      inner_tag = insert (inner);
      if (res)
	dump (dumper::TREE)
	  && dump ("Reading %s:%d %C", walk_kind_name[walk],
		   inner_tag, TREE_CODE (inner));
    }

  tree type = NULL_TREE;
  int type_tag = 0;
  if (res && TREE_CODE (inner) == TYPE_DECL)
    {
      if (unsigned type_code = u ())
	{
	  type = start (type_code);
	  if (type)
	    {
	      TREE_TYPE (res) = TREE_TYPE (inner) = type;
	      TYPE_NAME (type) = inner;

	      if (!tree_node_bools (type))
		res = NULL_TREE;
	    }
	  else
	    res = NULL_TREE;

	  type_tag = insert (type);
	  if (res)
	    dump (dumper::TREE)
	      && dump ("Reading %s:%d %C", walk_kind_name[walk], type_tag,
		       TREE_CODE (type));
	}
    }

  if (!res)
    {
    bail:
      if (inner_tag != 0)
	back_refs[~inner_tag] = NULL_TREE;
      if (type_tag != 0)
	back_refs[~type_tag] = NULL_TREE;
      if (tag != 0)
	back_refs[~tag] = NULL_TREE;
      set_overrun ();
      /* Bail.  */
      return NULL_TREE;
    }

  tree parms = NULL_TREE;
  if (walk != WK_body)
    {
      /* Figure out if this decl is already known about.  */
      tree container, key;
      tree fn_args = NULL_TREE;
      tree r_type = NULL_TREE;
      int parm_tag = 0;

      merge_kind mk = key_mergeable (res, inner, type,
				     &container, &key, &fn_args, &r_type,
				     &parm_tag);

      if (mk == MK_none)
	goto bail;

      if (mk == MK_spec)
	{
	  /* A specialization of some kind.  */
	  DECL_NAME (res) = DECL_NAME (container);
	  DECL_CONTEXT (res) = DECL_CONTEXT (container);
	}
      else if (mk == MK_clone)
	DECL_CONTEXT (res) = FROB_CONTEXT (container);
      else
	{
	  /* A named decl.  */
	  DECL_NAME (res) = key;
	  DECL_CONTEXT (res) = FROB_CONTEXT (container);
	}

      if (inner_tag != 0)
	{
	  DECL_NAME (inner) = DECL_NAME (res);
	  DECL_CONTEXT (inner) = DECL_CONTEXT (res);
	}

      const char *kind = "new";
      tree existing = NULL_TREE;

      if (mk == MK_spec)
	{
	  if (type)
	    {
	      existing = match_mergeable_specialization (type, container, key);

	      if (existing)
		{
		  if (inner_tag != 0)
		    {
		      /* This is a partial specialization, we need to get
			 back to its TEMPLATE_DECL.  */
		      for (tree partial
			     = DECL_TEMPLATE_SPECIALIZATIONS (container);
			   partial; partial = TREE_CHAIN (partial))
			if (TREE_TYPE (partial) == existing)
			  {
			    existing = TREE_VALUE (partial);
			    break;
			  }
		      gcc_assert (TREE_CODE (existing) == TEMPLATE_DECL);
		    }
		  else
		    existing = TYPE_NAME (existing);
		}
	    }
	  else
	    existing = match_mergeable_specialization (res, container, key);
	}
      else if (mk == MK_clone)
	{
	  kind = "clone";
	  /* Our merging is the same as that of the thing we cloned.  */
	  // FIXME: do we need to check skip_decls too?
	  if (is_existing_mergeable (container))
	    /* The existing clone will be the one following KEY.  */
	    existing = DECL_CHAIN (key);
	  else
	    {
	      /* Chain us in.  */
	      DECL_CHAIN (key) = res;
	      if (inner != res)
		DECL_CHAIN (DECL_TEMPLATE_RESULT (key)) = inner;
	    }
	}
      else if (is_mod && !(state->is_primary () || state->is_partition ()))
	/* This is a module-purview entity, and we're not loading part
	   of the current module, so it must be unique.  */
	kind = "unique";
      else
	existing = match_mergeable_decl (res, container, key, is_mod,
					 r_type, fn_args);

      if (!register_mergeable (res, existing))
	goto bail;

      if (existing)
	{
	  /* Install the existing decl into the back ref array.  */
	  back_refs[~tag] = existing;
	  if (inner_tag != 0)
	    {
	      existing = DECL_TEMPLATE_RESULT (existing);
	      back_refs[~inner_tag] = existing;
	    }

	  if (type_tag != 0)
	    back_refs[~type_tag] = TREE_TYPE (existing);

	  kind = "matched";
	}

      if (parm_tag)
	/* EXISTING is the template result (or NULL).  */
	parms = fn_parms_fini (parm_tag, inner, existing);

      dump (dumper::MERGE)
	&& dump ("Read:%d's %s merge key (%s) %C:%N", tag,
		 merge_kind_name[mk], kind, TREE_CODE (res), res);
    }

  if (inner_tag != 0)
    {
      /* Template pieces.  */
      tree parms = tree_node ();
      DECL_TEMPLATE_PARMS (res) = parms;

      // FIXME: From push_template_decl_real, not sure how correct
      // this is?
      /* Give template template parms a DECL_CONTEXT of the template
	 for which they are a parameter.  */
      tree innermost = INNERMOST_TEMPLATE_PARMS (parms);
      for (int i = TREE_VEC_LENGTH (innermost) - 1; i >= 0; --i)
	{
	  tree parm = TREE_VALUE (TREE_VEC_ELT (innermost, i));
	  if (TREE_CODE (parm) == TEMPLATE_DECL)
	    DECL_CONTEXT (parm) = res;
	}

      gcc_checking_assert (DECL_TEMPLATE_RESULT (res) == inner);
      if (!tree_node_vals (res))
	goto bail;
    }

  if (!tree_node_vals (inner))
    goto bail;

  if (type)
    {
      if (!tree_node_vals (type))
	goto bail;

      if (back_refs[~type_tag] == type)
	{
	  type = finish (type);
	  back_refs[~type_tag] = type;
	}
    }

  dump (dumper::TREE) && dump ("Read:%d %C:%N", tag, TREE_CODE (res), res);

  /* Regular typedefs will have a NULL TREE_TYPE at this point.  */
  bool is_typedef = TREE_CODE (inner) == TYPE_DECL && !TREE_TYPE (inner);
  if (is_typedef)
    {
      /* Frob it to be ready for cloning.  */
      TREE_TYPE (inner) = DECL_ORIGINAL_TYPE (inner);
      DECL_ORIGINAL_TYPE (inner) = NULL_TREE;
    }

  tree existing = back_refs[~tag];
  if (existing == res)
    {
      /* A newly discovered node.  */
      tree found = finish (res);

      if (found != res)
	{
	  gcc_assert (res == inner && !type);

	  /* Update the mapping.  */
	  res = found;
	  back_refs[~tag] = res;

	  dump (dumper::TREE)
	    && dump ("Remapping:%d to %C:%N%S", tag,
		     res ? TREE_CODE (res) : ERROR_MARK, res, res);
	}
      else if (is_typedef)
	{
	  set_underlying_type (inner);

	  if (res != inner)
	    TREE_TYPE (res) = TREE_TYPE (inner);
	}
    }
  else
    {
      /* RES is the to-be-discarded decl.  Its internal pointers will
	 be to the EXISTING's structure.  Frob it to point to its
	 own other structures, so loading its definition will alter
	 it, and not the existing decl.  */
      dump (dumper::MERGE) && dump ("Deduping %N", existing);

      if (parms)
	/* Restore the to-be-discarded parms.  */
	DECL_ARGUMENTS (inner) = parms;

      if (inner_tag != 0)
	DECL_TEMPLATE_RESULT (res) = inner;
      if (type)
	{
	  TYPE_NAME (type) = inner;
	  TREE_TYPE (inner) = type;
	}

      bool matched = is_matching_decl (existing, res);
      /* Record EXISTING as the skip defn, because that's what we'll
	 see when reading a definition.  */
      record_skip_defn (existing, !matched, false);

      /* And our result is the existing node.  */
      res = existing;
    }

  if (is_typedef)
    {
      /* Insert the type into the array now.  */
      tag = insert (TREE_TYPE (res));
      dump (dumper::TREE)
	&& dump ("Cloned:%d typedef %C:%N",
		 tag, TREE_CODE (TREE_TYPE (res)), TREE_TYPE (res));
    }

  return res;
}

/* Stream out tree node T.  We automatically create local back
   references, which is essentially the lisp self-referential
   structure pretty-printer.  */

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
	tree_node (TREE_TYPE (t));
      else if (code == tt_id && streaming_p ())
	str (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));

      int tag = insert (t);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Written:%d %sidentifier:%N", tag,
		   code == tt_conv_id ? "conv_op "
		   : code == tt_anon_id ? "anon "
		   : code == tt_lambda_id ? "lambda "
		   : "",
		   code == tt_conv_id ? TREE_TYPE (t) : t);
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
      tree_binfo (t, 0, false);

      gcc_checking_assert (!TREE_VISITED (t));
      int tag = insert (t);
      if (streaming_p ())
	dump (dumper::TREE) && dump ("Inserting binfo:%d %N", tag, t);
      goto done;
    }

  if (TREE_CODE (t) == INTEGER_CST
      && !TREE_OVERFLOW (t) && TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
    {
      unsigned ix = 0;
      for (tree values = TYPE_VALUES (TREE_TYPE (t));
	   values; ix++, values = TREE_CHAIN (values))
	if (tree_int_cst_equal (DECL_INITIAL (TREE_VALUE (values)), t))
	  {
	    if (streaming_p ())
	      u (tt_enum_int);
	    tree_node (TYPE_NAME (TREE_TYPE (t)));
	    if (streaming_p ())
	      u (ix);
	    dump (dumper::TREE)
	      && dump ("Written enum value %N[%u]", TREE_TYPE (t), ix);
	    goto done;
	  }
    }

 skip_normal:
  if (TYPE_P (t) && !tree_type (t, ref, false))
    goto done;

  if (DECL_P (t) && !tree_decl (t, ref, false))
    goto done;

  /* Otherwise by value */
  tree_value (t, ref);

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
  int tag = i ();
  tree res = NULL_TREE;
  switch (tag)
    {
    case tt_null:
      /* NULL_TREE.  */
      break;

    default:
      /* backref, pull it out of the map.  */
      if (tag < 0 && unsigned (~tag) < back_refs.length ())
	res = back_refs[~tag];
      if (!res)
	set_overrun ();
      if (res)
	dump (dumper::TREE) && dump ("Read backref:%d found %C:%N%S", tag,
				     TREE_CODE (res), res, res);
      break;

    case tt_fixed:
      {
	/* A fixed ref, find it in the fixed_ref array.   */
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

    case tt_typedef_type:
      res = tree_node ();
      dump (dumper::TREE)
	&& dump ("Read typedef %C:%N%S",
		 res ? TREE_CODE (res) : ERROR_MARK, res, res);
      res = tree_node ();
      break;

    case tt_typename_decl:
      {
	tree context = tree_node ();
	tree name = tree_node ();
	tree fullname = tree_node ();
	enum tag_types tag_type = tag_types (u ());

	if (!get_overrun ())
	  {
	    res = build_typename_type (context, name, fullname, tag_type);
	    tree decl = TYPE_STUB_DECL (res);
	    int decl_tag = insert (decl);
	    dump (dumper::TREE)
	      && dump ("Created:%d typename decl %P", decl_tag, context, name);

	    int type_tag = insert (res);
	    dump (dumper::TREE)
	      && dump ("Created:%d typename type", type_tag);
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

	  case TYPEOF_TYPE:
	  case DECLTYPE_TYPE:
	  case UNDERLYING_TYPE:
	    {
	      tree expr = tree_node ();
	      res = cxx_make_type (code);
	      TYPE_VALUES_RAW (res) = expr;
	      if (code == DECLTYPE_TYPE)
		tree_node_bools (res);
	      SET_TYPE_STRUCTURAL_EQUALITY (res);
	      
	    }
	    break;

	  case TYPE_PACK_EXPANSION:
	    {
	      bool local = u ();
	      tree param_packs = tree_node ();
	      tree expn = cxx_make_type (TYPE_PACK_EXPANSION);
	      SET_TYPE_STRUCTURAL_EQUALITY (expn);
	      SET_PACK_EXPANSION_PATTERN (expn, res);
	      PACK_EXPANSION_PARAMETER_PACKS (expn) = param_packs;
	      PACK_EXPANSION_LOCAL_P (expn) = local;
	      res = expn;
	    }
	    break;

	  case TYPE_ARGUMENT_PACK:
	    {
	      tree pack = cxx_make_type (TYPE_ARGUMENT_PACK);
	      SET_TYPE_STRUCTURAL_EQUALITY (pack);
	      SET_ARGUMENT_PACK_ARGS (pack, res);
	      res = pack;
	    }
	    break;

	  case VECTOR_TYPE:
	    {
	      unsigned HOST_WIDE_INT nunits = wu ();
	      res = build_vector_type (res, static_cast<poly_int64> (nunits));
	    }
	    break;

	  case COMPLEX_TYPE:
	    res = build_complex_type (res);
	    break;

	  case ARRAY_TYPE:
	    {
	      tree domain = tree_node ();
	      res = build_cplus_array_type (res, domain);
	    }
	    break;

	  case OFFSET_TYPE:
	    {
	      tree base = tree_node ();
	      res = build_offset_type (base, res);
	    }
	    break;

	  case FUNCTION_TYPE:
	    {
	      tree args = tree_node ();
	      res = build_function_type (res, args);
	    }
	    break;

	  case METHOD_TYPE:
	    {
	      tree klass = tree_node ();
	      tree args = tree_node ();

	      res = build_method_type_directly (klass, res, args);
	    }
	    break;
	    
	  case REFERENCE_TYPE:
	    {
	      bool rval = bool (u ());
	      res = cp_build_reference_type (res, rval);
	    }
	    break;

	  case POINTER_TYPE:
	    res = build_pointer_type (res);
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
	else if (unsigned (~tag) < back_refs.length ())
	  {
	    res = back_refs[~tag];
	    if (res)
	      dump (dumper::TREE)
		&& dump ("Found:%d already derived type %C", tag, code);
	  }
	else
	  {
	    set_overrun ();
	    res = NULL_TREE;
	  }
      }
      break;

    case tt_variant_type:
      /* Variant of some type.  */
      {
	res = tree_node ();
	int flags = i ();
	if (flags < 0)
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
	    res = build_aligned_type (res, flags);
	    TYPE_USER_ALIGN (res) = true;
	  }

	tree attribs = tree_node ();
	if (attribs != error_mark_node)
	  res = cp_build_type_attribute_variant (res, attribs);

	int quals = i ();
	if (quals >= 0)
	  res = cp_build_qualified_type (res, quals);

	int tag = i ();
	if (tag)
	  {
	    res = back_refs[~tag];
	    if (res)
	      dump (dumper::TREE)
		&& dump ("Found:%d already variant type %C",
			 tag, TREE_CODE (res));
	  }
	else
	  {
	    tag = insert (res);
	    if (res)
	      dump (dumper::TREE)
		&& dump ("Created:%d variant type %C", tag, TREE_CODE (res));
	  }
      }
      break;

    case tt_enum_int:
      /* An enum const.  */
      {
	tree decl = tree_node ();
	unsigned ix = u ();

	if (!decl)
	  break;

	dump (dumper::TREE) && dump ("Read enum value %N[%u]", decl, ix);
	for (tree values = TYPE_VALUES (TREE_TYPE (decl));
	     values; values = TREE_CHAIN (values))
	  if (!ix--)
	    {
	      res = DECL_INITIAL (TREE_VALUE (values));
	      break;
	    }

	if (!res)
	  set_overrun ();
      }
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
	    dump (dumper::TREE)
	      && dump ("Created %s:%d %S for %N",
		       is_tinfo ? "tinfo_var" : "conv_op", tag, res, type);
	  }
	else
	  set_overrun ();
      }
      break;

    case tt_tinfo_typedef:
      {
	/* A pseudo typeinfo typedef.  Get the index and recreate the pseudo.  */
	unsigned ix = u ();

	res = TYPE_NAME (get_pseudo_tinfo_type (ix));
	int tag = insert (res);
	dump (dumper::TREE)
	  && dump ("Created tinfo_decl:%d %u %N", tag, ix, res);
	tag = insert (TREE_TYPE (res));
	dump (dumper::TREE)
	  && dump ("Created tinfo_type:%d %u %N", tag, ix, TREE_TYPE (res));
      }
      break;

    case tt_anon_id:
    case tt_lambda_id:
      {
	/* An anonymous or lambda id.  */
	res = make_anon_name ();
	if (tag == tt_lambda_id)
	  IDENTIFIER_LAMBDA_P (res) = true;
	int tag = insert (res);
	dump (dumper::TREE)
	  && dump ("Read %s identifier:%d %N",
		   IDENTIFIER_LAMBDA_P (res) ? "lambda" : "anon", tag, res);
      }
      break;

    case tt_id:
      {
	/* An identifier node.  */
	size_t l;
	const char *chars = str (&l);
	res = get_identifier_with_length (chars, l);
	int tag = insert (res);
	dump (dumper::TREE)
	  && dump ("Read identifier:%d %N", tag, res);
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

    case tt_friend_template:
      /* A (local template) friend of a template class.  */
      if (tree klass = tree_node ())
	{
	  unsigned ix = u ();
	  unsigned u = ix;
	  for (tree decls = CLASSTYPE_DECL_LIST (klass);
	       decls; decls = TREE_CHAIN (decls))
	    if (!TREE_PURPOSE (decls) && !u--)
	      {
		res = TREE_VALUE (decls);
		break;
	      }

	  if (res)
	    {
	      if (TREE_CODE (res) != TEMPLATE_DECL)
		res = DECL_TI_TEMPLATE (res);
	      dump (dumper::TREE)
		&& dump ("Read friend %N[%d] %C:%N",
			 klass, ix, TREE_CODE (res), res);
	    }
	}
      break;

    case tt_named:
    case tt_implicit_template:
    case tt_anon:
      {
	/* A named decl.  */
	unsigned owner;
	tree ctx = tree_node ();
	tree name = tree_node ();

	// FIXME: I think owner is only needed for namespace-scope CTX?
	owner = u ();
	owner = state->slurp ()->remap_module (owner);
	int ident = i ();
	if ((owner != MODULE_NONE
	     || TREE_CODE (ctx) != NAMESPACE_DECL)
	    && !get_overrun ())
	  {
	    res = lookup_by_ident (ctx, name, owner, ident);
	    if (!res)
	      ;
	    else if (tag == tt_anon)
	      res = TYPE_STUB_DECL (TREE_TYPE (res));
	    else if (tag == tt_implicit_template)
	      {
		int use_tpl = -1;
		tree ti = node_template_info (res, use_tpl);
		res = TI_TEMPLATE (ti);
	      }
	  }

	if (!res)
	  {
	    error_at (state->loc, "failed to find %<%E%s%E%s%s%>",
		      ctx, &"::"[2 * (ctx == global_namespace)],
		      name ? name : get_identifier ("<anonymous>"),
		      owner ? "@" : "",
		      owner ? (*modules)[owner]->get_flatname () : "");
	    set_overrun ();
	  }
	else if (TREE_CODE (res) != TYPE_DECL
		 && owner != state->mod)
	  mark_used (res, tf_none);

	const char *kind = (owner != state->mod ? "Imported" : "Named");
	int tag = insert (res);
	if (res)
	  {
	    dump (dumper::TREE)
	      && dump ("%s:%d %C:%N@%M", kind, tag, TREE_CODE (res),
		       res, (*modules)[owner]);

	    tree proxy = res;
	    if (TREE_CODE (proxy) == TEMPLATE_DECL)
	      {
		proxy = DECL_TEMPLATE_RESULT (res);
		tag = insert (proxy);
		dump (dumper::TREE)
		  && dump ("Read templates's result:%d %C:%N", tag,
			   TREE_CODE (proxy), proxy);
	      }

	    if (TREE_CODE (proxy) == TYPE_DECL
		&& (DECL_ORIGINAL_TYPE (proxy)
		    || TYPE_STUB_DECL (TREE_TYPE (proxy)) == proxy))
	      {
		tree proxy_type = TREE_TYPE (proxy);
		tag = insert (proxy_type);
		dump (dumper::TREE)
		  && dump ("Read decl's type:%d %C:%N", tag,
			   TREE_CODE (proxy_type), proxy_type);
	      }
	  }
      }
      break;

    case tt_namespace:
      {
	/* Namespace reference.  */
	unsigned owner = u ();
	tree ctx = tree_node ();
	tree name = tree_node ();
	if (owner != MODULE_NONE)
	  owner = state->slurp ()->remap_module (owner);

	res = get_imported_namespace (ctx, name, owner);
	if (!res || TREE_CODE (res) != NAMESPACE_DECL)
	  {
	    error_at (state->loc, "failed to find namespace %<%E%s%E%>",
		      ctx, &"::"[2 * (ctx == global_namespace)],
		      name);
	    set_overrun ();
	    res = NULL;
	  }

	if (res)
	  {
	    int tag = insert (res);
	    dump (dumper::TREE)
	      && dump ("Namespace:%d %C:%N@%M", tag, TREE_CODE (res),
		       res, owner == MODULE_NONE ? NULL : (*modules)[owner]);
	  }
      }
      break;

    case tt_binfo:
      {
	/* A BINFO.  Walk the tree of the dominating type.  */
	res = tree_binfo ();
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
	if (res)
	  mark_used (res, tf_none);
	else
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
	tree clone;

	FOR_EVERY_CLONE (clone, target)
	  if (DECL_NAME (clone) == name)
	    {
	      res = clone;
	      break;
	    }

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

    case tt_mergeable:
    case tt_node:
    case tt_clone:
      /* A new node.  Stream it in.  */
      res = tree_value (walk_kind (tag - tt_node + WK_body));
      break;
    }

  dump.outdent ();
  return res;
}

void
trees_in::record_skip_defn (tree defn, bool informed, bool existing)
{
  dump (dumper::MERGE)
    && dump ("Recording %s skippable %C:%N",
	     existing ? "existing" : "new", TREE_CODE (defn), defn);

  if (!existing)
    {
      gcc_checking_assert (!is_skip_defn (defn));
      skip_defns.safe_push (intptr_t (defn) | intptr_t (informed));
    }
  else if (informed)
    {
      /* Record that we informed the user of a problem.  So we don't
	 give a whole slew of mismatch diagnostics.  */
      for (unsigned ix = skip_defns.length (); ix--;)
	if ((skip_defns[ix] & ~intptr_t (1)) == intptr_t (defn))
	  {
	    skip_defns[ix] |= 1;
	    break;
	  }
    }
}

/* Return non-zero if DEFN is a skippable entity.  >0 if we already
   warned, < 0 if we have not.  */

int
trees_in::is_skip_defn (tree defn)
{
  for (unsigned ix = skip_defns.length (); ix--;)
    if ((skip_defns[ix] & ~intptr_t (1)) == intptr_t (defn))
      return (skip_defns[ix] & 1) ? +1 : -1;
  return 0;
}

/* PARMS is a LIST, one node per level.
   TREE_VALUE is a TREE_VEC of parm info for that level.
   each ELT is a TREE_LIST
   TREE_VALUE is PARM_DECL, TYPE_DECL or TEMPLATE_DECL
   TREE_PURPOSE is the default value.  */

void
trees_out::tpl_header (tree tpl)
{
  for (tree parms = DECL_TEMPLATE_PARMS (tpl); parms; parms = TREE_CHAIN (parms))
    {
      tree vec = TREE_VALUE (parms);
      unsigned len = TREE_VEC_LENGTH (vec);
      if (streaming_p ())
	u (len);
      for (unsigned ix = 0; ix != len; ix++)
	{
	  tree parm_decl = TREE_VALUE (TREE_VEC_ELT (vec, ix));

	  if (TREE_CODE (parm_decl) == TEMPLATE_TYPE_PARM
	      && TEMPLATE_TYPE_PARAMETER_PACK (parm_decl))
	    gcc_unreachable (); // FIXME: Something

	  tree_node (parm_decl);
	}
    }

  /* Mark end.  */
  if (streaming_p ())
    u (0);
}

bool
trees_in::tpl_header (tree tpl)
{
  tree parms = NULL_TREE;
  while (unsigned len = u ())
    {
      tree vec = make_tree_vec (len);
      for (unsigned ix = 0; ix != len; ix++)
	{
	  tree parm_decl = tree_node ();
	  if (!parm_decl)
	    return false;
	  TREE_VEC_ELT (vec, ix) = tree_cons (NULL_TREE, parm_decl, NULL_TREE);
	}
      parms = tree_cons (NULL_TREE, vec, parms);
    }

  DECL_TEMPLATE_PARMS (tpl) = nreverse (parms);
  return true;
}

/* PARMS is a LIST whose TREE_VALUE is the type of the parm.  */

void
trees_out::fn_arg_types (tree arg_types)
{
  for (; arg_types; arg_types = TREE_CHAIN (arg_types))
    tree_node (TREE_VALUE (arg_types));
  tree_node (NULL_TREE);
}

tree
trees_in::fn_arg_types ()
{
  tree arg_types = NULL_TREE;
  tree *arg_ptr = &arg_types;

  while (tree arg = tree_node ())
    {
      *arg_ptr = tree_cons (NULL_TREE, arg, NULL_TREE);
      arg_ptr = &TREE_CHAIN (*arg_ptr);
    }

  return arg_types;
}

/* Stream skeleton parm nodes, with their flags, type & parm indices.
   All the parms will have consecutive tags.  */

void
trees_out::fn_parms_init (tree fn)
{
  unsigned ix = 0;
  for (tree parm = DECL_ARGUMENTS (fn);
       parm; parm = DECL_CHAIN (parm), ix++)
    {
      gcc_checking_assert (TREE_CODE (parm) == PARM_DECL);
      if (streaming_p ())
	{
	  u (TREE_CODE (parm));
	  start (parm);
	  tree_node_bools (parm);
	  if (DECL_LANG_SPECIFIC (parm))
	    {
	      u (DECL_PARM_INDEX (parm));
	      /* Parm types that are pointers to fns will themselves
		 contain parms, that could be referred to, but there's no
		 function_decl to grab hold of them from.  So don't use
		 late return type referring to them!  */
	      u (DECL_PARM_LEVEL (parm));
	    }
	}

      int tag = insert (parm);
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Writing:%d parm %u (%N) of %N",
		   tag, ix, parm, fn);
    }
  /* Mark the end.  */
  if (streaming_p ())
    u (0);

  ix = 0;
  for (tree parm = DECL_ARGUMENTS (fn);
       parm; parm = DECL_CHAIN (parm), ix++)
    {
      /* Write the name for friendliness.  Write the type for tree
	 comparisons.  */
      tree_node (DECL_NAME (parm));
      if (DECL_LANG_SPECIFIC (parm))
	/* If there are no indices, then we cannot be checking the
	   type either.  */
	tree_node (TREE_TYPE (parm));
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Wrote details for parm %u (%N) of %N", ix, parm, fn);
    }
}

/* Stream the remaining parm node data.  */

void
trees_out::fn_parms_fini (tree fn, tree maybe_template)
{
  if (streaming_p ())
    {
      depset *dep = dep_hash->find_entity (maybe_template);
      u (dep->has_defn ());
    }

  unsigned ix = 0;
  for (tree parm = DECL_ARGUMENTS (fn);
       parm; parm = DECL_CHAIN (parm), ix++)
    {
      if (streaming_p ())
	dump (dumper::TREE)
	  && dump ("Writing contents for parm %u (%N) of %N", ix, parm, fn);
      tree_node_vals (parm);
    }
}

/* Build skeleton parm nodes, read their flags, type & parm indices.  */

int
trees_in::fn_parms_init (tree fn)
{
  int base_tag = ~(int)back_refs.length ();

  tree *parm_ptr = &DECL_ARGUMENTS (fn);
  unsigned ix = 0;
  while (int code = u ())
    {
      if (code != PARM_DECL)
	{
	  set_overrun ();
	  return 0;
	}

      tree parm = start (code);
      if (!tree_node_bools (parm))
	return 0;

      if (DECL_LANG_SPECIFIC (parm))
	{
	  DECL_PARM_INDEX (parm) = u ();
	  DECL_PARM_LEVEL (parm) = u ();
	}

      int tag = insert (parm);
      dump (dumper::TREE)
	&& dump ("Reading:%d param %u of %N", tag, ix, fn);
      *parm_ptr = parm;
      parm_ptr = &DECL_CHAIN (parm);
      ix++;
    }

  ix = 0;
  for (tree parm = DECL_ARGUMENTS (fn);
       parm; parm = DECL_CHAIN (parm))
    {
      DECL_NAME (parm) = tree_node ();
      if (DECL_LANG_SPECIFIC (parm))
	TREE_TYPE (parm) = tree_node ();
      dump (dumper::TREE)
	&& dump ("Read details for parm %u (%N) of %N", ix, parm, fn);
    }

  return base_tag;
}

/* Read the remaining parm node data.  Replace with existing (if
   non-null) in the map.  */

tree
trees_in::fn_parms_fini (int tag, tree fn, tree existing)
{
  bool is_defn = u ();

  tree existing_parm = existing ? DECL_ARGUMENTS (existing) : NULL_TREE;
  tree parms = DECL_ARGUMENTS (fn);
  unsigned ix = 0;
  for (tree parm = parms; parm; parm = DECL_CHAIN (parm), ix++)
    {
      dump (dumper::TREE)
	&& dump ("Reading contents for parm %u (%N) of %N", ix, parm, fn);
      gcc_checking_assert (back_refs[~tag] == parm);
      tree_node_vals (parm);
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

  /* When we read the values for FN, we'll overwrite its version of
     the arguments with the existing version.  So return what we have
     now.  */
  if (!existing)
    parms = NULL_TREE;

  return parms;
}


/* Write out key information about a mergeable DEP.  Does not write
   the contents of DEP itself.  */
// FIXME: constraints probably need streaming too?

merge_kind
trees_out::key_mergeable (depset *dep)
{
  tree decl = dep->get_entity ();
  tree inner = STRIP_TEMPLATE (decl);

  merge_kind mk = MK_named;
  switch (dep->get_entity_kind ())
    {
    case depset::EK_SPECIALIZATION:
      mk = MK_spec;
      break;

    case depset::EK_CLONE:
      mk = MK_clone;
      break;

    default:
      break;
    }

  if (streaming_p ())
    {
      dump (dumper::MERGE)
	&& dump ("Writing key for mergeable %s %C:%N",
		 dep->entity_kind_name (), TREE_CODE (decl), decl);
      u (mk);
    }

  /* Always stream the context, even though it's only used to locate
     named decls -- other decls will still refer to it, and we want it
     correctly canonicalized before we start emitting keys for this
     decl.  */
  tree_ctx (CP_DECL_CONTEXT (decl), true, decl);

  if (mk != MK_clone && decl != inner)
    /* A template needs its template parms for identification.  */
    // FIXME: Don't write the context of the tpl_tpl_parms!  We should
    // probably never write them out, and leave it to the owning
    // template to set it on import?
    tpl_header (decl);

  if (TREE_CODE (inner) == FUNCTION_DECL)
    fn_parms_init (inner);

  /* Now write the locating information. */
  if (mk == MK_spec)
    {
      /* Specializations are located via their originating template,
         and the set of template args they specialize.  */
      depset *spec = dep;
      if (!streaming_p ())
	{
	  /* When /determining/ mergeable ordering, there's an indirection.  */
	  gcc_assert (spec->is_marked ());
	  spec = dep->deps[0];
	}

      tree tmpl, args;
      gcc_assert (spec->is_marked ());
      spec_entry *entry = reinterpret_cast <spec_entry *> (spec->deps[0]);
      tmpl = entry->tmpl;
      args = entry->args;

      tree_ctx (tmpl, false, NULL_TREE);
      tree_node (args);
    }
  else if (mk == MK_clone)
    {
      /* Clones are located by the thing they clone.  We also use their
	 predecessor entity, to force their ordering.  */
      tree target = get_clone_target (decl);
      tree predecessor = target;
      tree clone;
      FOR_EVERY_CLONE (clone, target)
	{
	  if (clone == decl)
	    break;
	  predecessor = clone;
	}

      gcc_assert (clone);
      tree_ctx (target, false, NULL_TREE);
      tree_node (predecessor);
    }
  else
    {
      /* Regular decls are located by their context, name, and
	 additional disambiguating data.  */
      tree_node (DECL_NAME (decl));

      if (TREE_CODE (inner) == FUNCTION_DECL)
	{
	  /* Functions are distinguished by parameter types.  */
	  tree fn_type = TREE_TYPE (inner);
	  fn_arg_types (TYPE_ARG_TYPES (fn_type));

	  if (decl != inner)
	    /* And a function template needs the return type.  */
	    // FIXME: What if the return type is a voldemort?  We
	    // should be using the declared return type.
	    tree_node (TREE_TYPE (fn_type));
	}
    }

  return mk;
}

/* DECL, INNER & TYPE are a skeleton set of nodes for a decl.  Only
   the bools have been filled in.  Read its merging key.  Returns
   IS_SPECIALIZATION.  *CONTAINER will be NULL on error.  */

merge_kind
trees_in::key_mergeable (tree decl, tree inner, tree,
			 tree *container, tree *key, tree *fn_args, tree *r_type,
			 int *parm_tag)
{
  *container = *key = NULL_TREE;
  *fn_args = *r_type = NULL_TREE;

  unsigned i = u ();
  if (i > MK_clone)
    return MK_none;
  merge_kind mk = merge_kind (i);

  tree ctx = tree_node ();

  if (mk != MK_clone && decl != inner)
    /* A template needs its template parms for identification.  */
    if (!tpl_header (decl))
      return MK_none;

  if (TREE_CODE (inner) == FUNCTION_DECL)
    *parm_tag = fn_parms_init (inner);

  /* Now read the locating information. */
  if (mk == MK_named)
    {
      *container = ctx;
      *key = tree_node ();

      if (TREE_CODE (inner) == FUNCTION_DECL)
	{
	  *fn_args = fn_arg_types ();
	  if (decl != inner)
	    *r_type = tree_node ();
	}
    }
  else
    {
      *container = tree_node ();
      *key = tree_node ();
      }

  return get_overrun () ? MK_none : mk;
}

/* Rebuild a streamed in type.  */
// FIXME: C++-specific types are not in the canonical type hash.
// Perhaps that should be changed?

tree
trees_in::finish_type (tree type)
{
  tree main = TYPE_MAIN_VARIANT (type);

  if (main != type)
    {
      // FIXME: Review given we now have tt_derived_type & tt_variant_type.
      /* See if we have this type already on the variant
	 list.  This could only happen if the originally read in main
	 variant was remapped, but we don't have that knowledge.
	 FIXME: Determine if this is a problem, and then maybe fix
	 it?  That would avoid a fruitless search along the variant
	 chain.  */
      for (tree probe = main; probe; probe = TYPE_NEXT_VARIANT (probe))
	{
	  if (!check_base_type (probe, type))
	    continue;

	  if (!check_lang_type (probe, type))
	    continue;

	  if (TYPE_ALIGN (probe) != TYPE_ALIGN (type))
	    continue;

	  if (TYPE_QUALS (probe) != TYPE_QUALS (type))
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
	  
	  dump (dumper::TREE)
	    && dump ("Type %p already found as %p variant of %p",
		     (void *)type, (void *)probe, (void *)main);
	  free_node (type);
	  type = probe;
	  goto found_variant;
	}

      /* Splice it into the variant list.  */
      dump (dumper::TREE) && dump ("Type %p added as variant of %p",
				   (void *)type, (void *)main);
      TYPE_NEXT_VARIANT (type) = TYPE_NEXT_VARIANT (main);
      TYPE_NEXT_VARIANT (main) = type;

      /* CANONICAL_TYPE is either already correctly remapped.  Or
         correctly already us.  */
      // FIXME: Are we sure about this?
    found_variant:;
    }
  else if (TREE_CODE (type) == TEMPLATE_TYPE_PARM
	   || TREE_CODE (type) == TEMPLATE_TEMPLATE_PARM)
    {
      tree canon = canonical_type_parameter (type);
      TYPE_CANONICAL (type) = canon;
      dump (dumper::TREE) && dump ("Adding template type %p with canonical %p",
				   (void *)type, (void *)canon);
    }
  else if (!TYPE_STRUCTURAL_EQUALITY_P (type)
	   && !RECORD_OR_UNION_CODE_P (TREE_CODE (type))
	   && TREE_CODE (type) != ENUMERAL_TYPE
	   && !TYPE_NAME (type)) // FIXME: why this check?
    {
      gcc_assert (TYPE_ALIGN (type));
      hashval_t hash = type_hash_canon_hash (type);
      /* type_hash_canon frees type, if we find it already.  */
      type = type_hash_canon (hash, type);
      // FIXME: This is where it'd be nice to determine if type
      // was already found.  See above.
      dump (dumper::TREE) && dump ("Adding type %p with canonical %p",
				   (void *)main, (void *)type);
    }

  if (RECORD_OR_UNION_CODE_P (TREE_CODE (type))
      // FIXME: enums?
      && main != type)
    {
      /* The main variant might already have been defined, copy
	 the bits of its definition that we need.  */
      TYPE_BINFO (type) = TYPE_BINFO (main);
      TYPE_VFIELD (type) = TYPE_VFIELD (main);
      TYPE_FIELDS (type) = TYPE_FIELDS (main);
    }

  return type;
}

/* DECL is a just streamed mergeable decl that should match EXISTING.  Check
   it does and issue an appropriate diagnostic if not.  */

bool
trees_in::is_matching_decl (tree existing, tree decl)
{
  // FIXME: Inhibit TYPENAME_TYPE resolution, all the way down!
  if (!comptypes (TREE_TYPE (existing), TREE_TYPE (decl),
		  COMPARE_STRUCTURAL))
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"conflicting global module declaration %#qD", decl);
      inform (DECL_SOURCE_LOCATION (existing),
	      "existing declaration %#qD", existing);
      return false;
    }

  return true;
}

/* Return non-zero if DECL has a definition that would be interesting to
   write out.  */

static bool
has_definition (tree decl)
{
  decl = STRIP_TEMPLATE (decl);

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

      if (DECL_TEMPLATE_INFO (decl))
	{
	  int use_tpl = DECL_USE_TEMPLATE (decl);

	  // FIXME: partial specializations have definitions too.
	  if (use_tpl < 2)
	    return true;
	}
      break;

    case VAR_DECL:
      /* Variables should be written inline.  */
      if (!DECL_INITIAL (decl))
	/* Nothing to define.  */
	break;

      if (TREE_CONSTANT (decl))
	return true;

      break;

    case TYPE_DECL:
      {
	if (!DECL_IMPLICIT_TYPEDEF_P (decl))
	  break;

	tree type = TREE_TYPE (decl);
	if (TREE_CODE (type) == ENUMERAL_TYPE
	    ? TYPE_VALUES (type) : TYPE_FIELDS (type))
	  return true;
      }
      break;
    }

  return false;
}

/* Return the namespace scope DECL enclosing T.  */
// FIXME: Isn't this the wrong thing for instantiations?
// And to deal with partial specializations, I think we have to go
// from the skip_defn array to each scope enclosing defn

static tree
topmost_decl (tree t)
{
  for (;;)
    {
      while (TYPE_P (t))
	{
	  if (tree name = TYPE_NAME (t))
	    t = name;
	  else
	    return NULL_TREE;
	}

      gcc_checking_assert (DECL_P (t));
      tree ctx = CP_DECL_CONTEXT (t);
      if (!ctx || TREE_CODE (ctx) == NAMESPACE_DECL)
	return t;
      t = ctx;
    }
}

/* We're reading a definition of DEFN (a _DECL or structured _TYPE).
   Check that makes sense and issue a diagnostic on errors.  Return
   zero if the new defn should be added.  Return +1 if it should be
   ignored and -1 if it should be checked for ODR.  */

int
trees_in::is_skippable_defn (tree defn, bool have_defn)
{
  gcc_assert (DECL_P (defn));

  if (get_overrun ())
    return +1;

  /* The most common case is to have nothing to skip.  Short circuit
     the complexity in that case. */
  if (!any_skip_defns ())
    return 0;

  /* If there are skip defns, we're merging entities.  Find the
     namespace-scope dominating decl. */
  // FIXME: Breaks for specializations
  tree top = topmost_decl (defn);
  if (int skip = is_skip_defn (top))
    {
      dump (dumper::MERGE)
	&& dump ("Skipping definition %N%s",
		 defn, skip < 0 ? " check ODR" : "");
      return skip;
    }

  /* This isn't skippable.  There'd better not be an existing
     defn.  */
  if (!have_defn)
    return 0;

  if (defn)
    {
      record_skip_defn (top, true, false);
      error_at (state->loc, "unexpected definition of %q#D", defn);
      inform (DECL_SOURCE_LOCATION (defn), "existing definition here");
    }

  return +1;
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

  if (constexpr_fundef *cexpr = retrieve_constexpr_fundef (decl))
    {
      tree_node (cexpr->decl);
      tree_node (cexpr->body);
      chained_decls (cexpr->parms);
      tree_node (cexpr->result);
    }
  else
    tree_node (NULL_TREE);
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

  cexpr.decl = tree_node ();
  if (cexpr.decl)
    {
      cexpr.body = tree_node ();
      cexpr.parms = chained_decls ();
      cexpr.result = tree_node ();
    }

  if (get_overrun ())
    return NULL_TREE;

  int odr = is_skippable_defn (maybe_template,
			       DECL_SAVED_TREE (decl) != NULL_TREE);
  assert_definition (maybe_template, odr);

  if (!odr)
    {
      DECL_RESULT (decl) = result;
      DECL_INITIAL (decl) = initial;
      DECL_SAVED_TREE (decl) = saved;
      if (context)
	SET_DECL_FRIEND_CONTEXT (decl, context);
      if (cexpr.decl)
	register_constexpr_fundef (cexpr);
      post_process (maybe_template);
    }
  else if (odr < 0)
    {
      // FIXME: Check matching defn
    }
  
  return true;
}

void
trees_out::write_var_def (tree decl)
{
  tree_node (DECL_INITIAL (decl));
}

void
trees_out::mark_var_def (tree)
{
}

bool
trees_in::read_var_def (tree decl, tree maybe_template)
{
  tree init = tree_node ();

  if (get_overrun ())
    return false;

  int odr = is_skippable_defn (maybe_template,
			       DECL_INITIAL (decl) != NULL_TREE);
  assert_definition (maybe_template, odr);
  if (!odr)
    {
      DECL_INITIAL (decl) = init;
    }
  else if (odr < 0)
    {
      // FIXME: Check matching defn
    }

  return true;
}

/* If MEMBER doesn't have an independent life outside the class,
   return it (or it's TEMPLATE_DECL).  Otherwise NULL.  */

static tree
member_owned_by_class (tree member)
{
  gcc_assert (DECL_P (member));

  /* Clones are owned by their origin.  */
  if (DECL_CLONED_FUNCTION_P (member))
    return NULL;

  int use_tpl = -1;
  if (tree ti = node_template_info (member, use_tpl))
    {
      // FIXME: don't bail on things that CANNOT have their own
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
  chained_decls (TYPE_FIELDS (type));
  tree_node (TYPE_VFIELD (type));
  tree_node (TYPE_BINFO (type));

  if (TYPE_LANG_SPECIFIC (type))
    {
      tree_vec (CLASSTYPE_MEMBER_VEC (type));
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
	as_base = TYPE_STUB_DECL (as_base);
      tree_node (as_base);
      if (as_base && as_base != defn)
	write_class_def (as_base);

      tree vtables = CLASSTYPE_VTABLES (type);
      chained_decls (vtables);
      /* Write the vtable initializers.  */
      for (; vtables; vtables = TREE_CHAIN (vtables))
	write_definition (vtables);

      /* Write the friend classes.  */
      for (tree friends = CLASSTYPE_FRIEND_CLASSES (type);
	   friends; friends = TREE_CHAIN (friends))
	/* TREE_VALUE is what we want.  */
	tree_node (TREE_VALUE (friends));
      /* End of friends.  */
      tree_node (NULL_TREE);

      /* Write the friend functions.  */
      for (tree friends = DECL_FRIENDLIST (defn);
	   friends; friends = TREE_CHAIN (friends))
	{
	  /* Name of these friends.  */
	  tree_node (TREE_PURPOSE (friends));
	  for (tree decls = TREE_VALUE (friends);
	       decls; decls = TREE_CHAIN (decls))
	    tree_node (TREE_VALUE (decls));
	  /* End of these friends called this.  */
	  tree_node (NULL_TREE);
	}
      /* End of friends.  */
      tree_node (NULL_TREE);

      /* Write the decl list.  */
      for (tree decls = CLASSTYPE_DECL_LIST (type); decls;
	   decls = TREE_CHAIN (decls))
	{
	  tree decl = TREE_VALUE (decls);
	  tree_node (decl);
	  tree_node (TREE_PURPOSE (decls));
	  if (!TREE_PURPOSE (decls))
	    if (tree frnd = friend_from_decl_list (TREE_VALUE (decls)))
	      {
		bool local_friend = frnd && TREE_CHAIN (frnd) == type;
		bool has_def = local_friend && has_definition (frnd);

		if (streaming_p ())
		  u (unsigned (local_friend) * 2 | unsigned (has_def));

		if (has_def)
		  write_definition (frnd);
	      }
	}
      /* End of decls.  */
      tree_node (NULL_TREE);

      if (TYPE_CONTAINS_VPTR_P (type))
	{
	  /* Write the thunks.  */
	  for (tree decls = TYPE_FIELDS (type);
	       decls; decls = DECL_CHAIN (decls))
	    if (TREE_CODE (decls) == FUNCTION_DECL
		&& DECL_VIRTUAL_P (decls))
	      {
		tree_node (decls);
		chained_decls (DECL_THUNKS (decls));
	      }
	  tree_node (NULL_TREE);
	}
    }

  // FIXME: lang->nested_udts

  /* Now define all the members that do not have independent definitions.  */
  for (tree member = TYPE_FIELDS (type); member; member = TREE_CHAIN (member))
    if (has_definition (member))
      if (tree mine = member_owned_by_class (member))
	{
	  tree_node (mine);
	  write_definition (mine);
	}

  /* End of definitions.  */
  tree_node (NULL_TREE);
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
  for (tree member = TYPE_FIELDS (type); member; member = DECL_CHAIN (member))
    /* Do not mark enum consts here.  */
    if (TREE_CODE (member) != CONST_DECL)
      {
	mark_class_member (member);
	if (TREE_CODE (member) == FIELD_DECL)
	  if (tree repr = DECL_BIT_FIELD_REPRESENTATIVE (member))
	    mark_declaration (repr, false);
      }

  /* Mark the binfo heirarchy.  */
  for (tree child = TYPE_BINFO (type); child; child = TREE_CHAIN (child))
    mark_node (child);

  if (TYPE_LANG_SPECIFIC (type))
    {
      if (tree as_base = CLASSTYPE_AS_BASE (type))
	if (as_base != type)
	  mark_declaration (TYPE_NAME (as_base), true);

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

      for (tree decls = CLASSTYPE_DECL_LIST (type);
	   decls; decls = TREE_CHAIN (decls))
	{
	  tree decl = TREE_VALUE (decls);

	  if (TREE_PURPOSE (decls))
	    {
	      /* There may be decls here, that are not on the member vector.
		 for instance forward declarations of member tagged types.  */
	      if (TYPE_P (decl))
		/* In spite of its name, non-decls appear :(.  */
		decl = TYPE_NAME (decl);

	      /* Static asserts are on this chain too.  */
	      if (DECL_P (decl))
		{
		  gcc_assert (DECL_CONTEXT (decl) == type);
		  mark_class_member (decl, false);
		}
	    }
	  else if (tree frnd = friend_from_decl_list (decl))
	    {
	      /* A friend declaration.  If we're a template, and this
		 friend is either a template or a dependent decl, it's
		 local to us.  Such have their DECL_CHAIN set to the
		 befriending type.  */
	      if (DECL_CHAIN (frnd) == type)
		mark_declaration (frnd, has_definition (frnd));
	    }
	}
    }
}

/* Nop sorting, needed for resorting the member vec.  */

static void
nop (void *, void *)
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
  tree fields = chained_decls ();
  tree vfield = tree_node ();
  tree binfo = tree_node ();
  vec<tree, va_gc> *vbase_vec = NULL;
  vec<tree, va_gc> *member_vec = NULL;
  vec<tree, va_gc> *pure_virts = NULL;
  vec<tree_pair_s, va_gc> *vcall_indices = NULL;
  tree key_method = NULL_TREE;
  tree lambda = NULL_TREE;

  if (TYPE_LANG_SPECIFIC (type))
    {
      member_vec = tree_vec ();
      lambda = tree_node ();

      unsigned nvbases = u ();
      if (nvbases)
	{
	  vec_alloc (vbase_vec, nvbases);
	  for (tree child = binfo; child; child = TREE_CHAIN (child))
	    if (BINFO_VIRTUAL_P (child))
	      vbase_vec->quick_push (child);
	}

      int has_vptr = i ();
      if (has_vptr)
	{
	  pure_virts = tree_vec ();
	  vcall_indices = tree_pair_vec ();
	  key_method = tree_node ();
	}
    }

  // FIXME: Read more stuff!
  // lang->nested_udts

  int odr = is_skippable_defn (maybe_template, TYPE_SIZE (type) != NULL_TREE);
  assert_definition (maybe_template, odr);
  if (!odr)
    {
      TYPE_SIZE (type) = size;
      TYPE_SIZE_UNIT (type) = size_unit;

      TYPE_FIELDS (type) = fields;
      TYPE_VFIELD (type) = vfield;
      TYPE_BINFO (type) = binfo;

      // FIXME: Do I need to rewire DECL_CHAIN on template clones?
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
  else if (odr < 0)
    {
      // FIXME: Check matching defn
    }

  if (TYPE_LANG_SPECIFIC (type))
    {
      tree primary = tree_node ();
      tree as_base = tree_node ();

      if (as_base)
	{
	  if (as_base != defn)
	    read_class_def (as_base, as_base);
	  as_base = TREE_TYPE (as_base);
	}

      /* Read the vtables.  */
      tree vtables = chained_decls ();
      for (tree vt = vtables; vt; vt = TREE_CHAIN (vt))
	read_var_def (vt, vt);

      // FIXME: We should be able to reverse the lists in the writer
      tree friend_classes = NULL_TREE;
      while (tree val = tree_node ())
	friend_classes = tree_cons (NULL_TREE, val, friend_classes);
      friend_classes = nreverse (friend_classes);

      tree friend_functions = NULL_TREE;
      while (tree name = tree_node ())
	{
	  tree val = NULL_TREE;
	  while (tree decl = tree_node ())
	    val = tree_cons (NULL_TREE, decl, val);
	  nreverse (val);
	  friend_functions = tree_cons (name, val, friend_functions);
	}
      friend_functions = nreverse (friend_functions);

      tree decl_list = NULL_TREE;
      while (tree decl = tree_node ())
	{
	  tree purpose = tree_node ();
	  if (!purpose)
	    if (tree frnd = friend_from_decl_list (decl))
	      {
		unsigned local = u ();
		bool local_friend = (local >> 1) & 1;
		bool has_def = local & 1;

		if (local_friend)
		  {
		    gcc_assert (!DECL_CHAIN (frnd));
		    DECL_CHAIN (frnd) = type;
		  }

		if (has_def)
		  if (!read_definition (frnd))
		    break;
	      }
	  decl_list = tree_cons (purpose, decl, decl_list);
	}
      decl_list = nreverse (decl_list);

      if (!odr)
	{
	  CLASSTYPE_PRIMARY_BINFO (type) = primary;
	  CLASSTYPE_AS_BASE (type) = as_base;
	  if (!CLASSTYPE_KEY_METHOD (type) && vtables)
	    vec_safe_push (keyed_classes, type);
	  CLASSTYPE_VTABLES (type) = vtables;
	  CLASSTYPE_FRIEND_CLASSES (type) = friend_classes;
	  DECL_FRIENDLIST (defn) = friend_functions;
	  CLASSTYPE_DECL_LIST (type) = decl_list;

	  for (; friend_classes; friend_classes = TREE_CHAIN (friend_classes))
	    {
	      tree f = TREE_VALUE (friend_classes);

	      if (TYPE_P (f))
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
	    if (!odr)
	      SET_DECL_THUNKS (vfunc, thunks);
	  }
    }

  /* Propagate to all variants.  */
  if (!odr)
    fixup_type_variants (type);

  /* Now define all the members.  */
  while (tree member = tree_node ())
    {
      if (get_overrun ())
	break;
      if (!read_definition (member))
	break;
    }

  return !get_overrun ();
}

void
trees_out::write_enum_def (tree decl)
{
  tree type = TREE_TYPE (decl);

  tree_node (TYPE_VALUES (type));
  tree_node (TYPE_MIN_VALUE (type));
  tree_node (TYPE_MAX_VALUE (type));
}

void
trees_out::mark_enum_def (tree decl)
{
  tree type = TREE_TYPE (decl);

  for (tree values = TYPE_VALUES (type); values; values = TREE_CHAIN (values))
    {
      tree cst = TREE_VALUE (values);
      mark_node (cst);
      /* We must mark the init to avoid circularity in tt_enum_int.  */
      if (tree init = DECL_INITIAL (cst))
	if (TREE_CODE (init) == INTEGER_CST)
	  mark_node (init);
    }
}

bool
trees_in::read_enum_def (tree defn, tree maybe_template)
{
  tree type = TREE_TYPE (defn);
  tree values = tree_node ();
  tree min = tree_node ();
  tree max = tree_node ();

  if (get_overrun ())
    return false;

  // FIXME: ODR ?
  int odr = 0;
  assert_definition (maybe_template, odr);

  TYPE_VALUES (type) = values;
  TYPE_MIN_VALUE (type) = min;
  TYPE_MAX_VALUE (type) = max;

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

    case VAR_DECL:
      write_var_def (decl);
      break;

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (DECL_IMPLICIT_TYPEDEF_P (decl)
		    && TYPE_MAIN_VARIANT (type) == type);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  write_enum_def (decl);
	else
	  write_class_def (decl);
      }
      break;
    }
}

/* Mark a declaration for by-value walking.  If DO_DEFN is true, mark
   its body too.  */

void
trees_out::mark_declaration (tree decl, bool do_defn)
{
  mark_node (decl);

  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      /* Mark the template parms.  */
      for (tree parms = DECL_TEMPLATE_PARMS (decl);
	   parms; parms = TREE_CHAIN (parms))
	{
	  tree vec = TREE_VALUE (parms);
	  for (unsigned ix = TREE_VEC_LENGTH (vec); ix--;)
	    {
	      tree parm_decl = TREE_VALUE (TREE_VEC_ELT (vec, ix));
	      mark_declaration (parm_decl, false);
	    }
	}
      decl = DECL_TEMPLATE_RESULT (decl);
    }

  if (!do_defn)
    return;

  switch (TREE_CODE (decl))
    {
    default:
      gcc_unreachable ();

    case FUNCTION_DECL:
      mark_function_def (decl);
      break;

    case VAR_DECL:
      mark_var_def (decl);
      break;

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (DECL_IMPLICIT_TYPEDEF_P (decl)
		    && TYPE_MAIN_VARIANT (type) == type);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  mark_enum_def (decl);
	else
	  mark_class_def (decl);
      }
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

    case VAR_DECL:
      return read_var_def (decl, maybe_template);

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (DECL_IMPLICIT_TYPEDEF_P (decl)
		    && TYPE_MAIN_VARIANT (type) == type);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  return read_enum_def (decl, maybe_template);
	else
	  return read_class_def (decl, maybe_template);
      }
      break;
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
depset::hash::find_entity (tree entity)
{
  depset **slot = entity_slot (entity, false);

  return slot ? *slot : NULL;
}

depset *
depset::hash::find_binding (tree ctx, tree name)
{
  depset **slot = binding_slot (ctx, name, false);

  return slot ? *slot : NULL;
}

/* DECL is a newly discovered dependency of current.  Create the
   depset, if it doesn't already exist.  Add it to the worklist if so.
   Append it to current's depset.  The decls newly discovered at this
   point are not export or module linkage.  They may be voldemort
   types, internal-linkage entities or reachable global module
   fragment entities.

   IS_IMPORT indicates a reference to an EK_UNNAMED or
   EK_SPECIALIZATION from another module.  We need to insert horcrux
   information for them, and then lazy load.  If we used their general
   template and instantiation args, we'd load all the instantiations
   of the template, which might be suboptimal.

   DECL will be an OVL_USING_P OVERLOAD, if it's from a binding that's
   a using decl.  */

depset *
depset::hash::add_dependency (tree decl, entity_kind ek, bool is_import)
{
  /* Make sure we're being told consistent information.  */
  gcc_checking_assert ((ek == EK_USING) == (TREE_CODE (decl) == OVERLOAD));
  gcc_checking_assert ((ek == EK_NAMESPACE)
		       == (TREE_CODE (decl) == NAMESPACE_DECL
			   && !DECL_NAMESPACE_ALIAS (decl)));
  gcc_checking_assert (ek != EK_BINDING && ek != EK_REDIRECT);
  if (is_import)
    /* Only (potentially) unnameable imports need to be so marked.  */
    gcc_checking_assert (ek == EK_UNNAMED || ek == EK_SPECIALIZATION
			 || ek == EK_MAYBE_SPEC);

  depset *dep = NULL;
  if (depset **slot = entity_slot (decl, !is_for_mergeable ()))
    {
      bool binding_p = current && current->is_binding ();
      dep = *slot;

      if (is_for_mergeable ())
	/* Bindings are never mergeable.  */
	gcc_checking_assert (!binding_p);
      if (ek == EK_USING)
	/* Usings only occur in bindings.  */
	gcc_checking_assert (OVL_USING_P (decl) && binding_p);
      if (ek == EK_UNNAMED)
	/* Unnameable things are not namespace scope  */
	gcc_checking_assert (TREE_CODE (CP_DECL_CONTEXT (decl))
			     != NAMESPACE_DECL);

      if (!dep)
	{
	  bool has_def = (!is_for_mergeable () && ek != EK_USING
			  && has_definition (decl));
	  bool maybe_spec = ek == EK_MAYBE_SPEC;
	  if (maybe_spec)
	    {
	      /* We were probing, and didn't find anything.  Therefore
		 this is a regular-ish decl, but mark it as a pseudo
		 spec so we know to write a definition.  */
	      ek = EK_DECL;
	      dump (dumper::DEPEND)
		&& dump ("Pseudo specialization %N discovered", decl);
	    }

	  /* The only OVERLOADS we should see are USING decls from
	     bindings.  */
	  *slot = dep = make_entity (decl, ek, has_def);

	  if (TREE_CODE (decl) == TEMPLATE_DECL)
	    /* If we add a template, its result better not also be
	       present.  */
	    gcc_checking_assert
	      (!entity_slot (DECL_TEMPLATE_RESULT (decl), false));

	  if (binding_p)
	    /* Dependency of a namespace binding.  */;
	  else if (ek == EK_NAMESPACE)
	    /* Dependency is a namespace.  */;
	  else if (maybe_spec)
	    {
	      /* Remember this has some specialization properties.  */
	      dep->set_flag_bit<DB_PSEUDO_SPEC_BIT> ();
	      if (is_import)
		dep->set_flag_bit<DB_IMPORTED_BIT> ();
	    }
	  else if (ek == EK_UNNAMED
		   || ek == EK_SPECIALIZATION)
	    {
	      /* Dependency is unnameable.  We do not have to apply
		 the below checks to this entity, because we can only
		 refer to it by depending on its containing entity,
		 and that entity will have the below checks applied to
		 it.  */
	      if (is_import)
		/* Note this entity came from elsewhere.  */
		dep->set_flag_bit<DB_IMPORTED_BIT> ();
	    }
	  else
	    {
	      tree ctx = CP_DECL_CONTEXT (decl);
	      gcc_checking_assert (TREE_CODE (ctx) == NAMESPACE_DECL);
	      tree not_tmpl = STRIP_TEMPLATE (decl);

	      if (!TREE_PUBLIC (ctx))
		/* Member of internal namespace.  */
		dep->set_flag_bit<DB_IS_INTERNAL_BIT> ();
	      else if (TREE_CODE (not_tmpl) != TYPE_DECL
		       && TREE_CODE (not_tmpl) != CONST_DECL
		       && DECL_THIS_STATIC (not_tmpl))
		{
		  /* An internal decl.  In global module permit
		     extern "C" static inline functions
		     ... because.  */
		  if (!(header_module_p ()
			|| (TREE_CODE (not_tmpl) == FUNCTION_DECL
			    && DECL_DECLARED_INLINE_P (not_tmpl)
#if 0
			    // FIXME: Disregard language linkage, because our
			    // std lib is borked.  Templates cannot
			    // have "C" linkage!
			    && DECL_EXTERN_C_P (not_tmpl)
#endif
			    && true)))
		    dep->set_flag_bit<DB_IS_INTERNAL_BIT> ();
		}
	      else if (DECL_IMPLICIT_TYPEDEF_P (decl)
		       && IDENTIFIER_ANON_P (DECL_NAME (decl)))
		/* No linkage or linkage from typedef name (which
		   cannot be internal, because that's from the linkage
		   of the context.  */;
	      else
		{
		  // FIXME: We have to walk the non-emitted entities
		  // in the module's purview too.  Discussing this in
		  // CWG, it is weird.
		  /* A reachable global module fragment entity.  Add
		     it to its scope's binding depset.  */
		  gcc_checking_assert (MAYBE_DECL_MODULE_OWNER (decl)
				       == MODULE_NONE);
		  depset **bslot = binding_slot (ctx, DECL_NAME (decl), true);
		  depset *bdep = *bslot;
		  if (!bdep)
		    *bslot = bdep = make_binding (ctx, DECL_NAME (decl));
		  bdep->deps.safe_push (dep);
		  dep->deps.safe_push (bdep);
		  dep->set_flag_bit<DB_GLOBAL_BIT> ();
		  dump (dumper::DEPEND)
		    && dump ("Reachable GMF %N added", decl);
		}
	    }

	  if (ek == depset::EK_SPECIALIZATION
	      || (ek == depset::EK_DECL
		  && TREE_PUBLIC (CP_DECL_CONTEXT (decl))))
	    /* Decl that could be declared in multiple CMIs (even
	       module-owned declarations could be defined in a
	       partition.  */
	    dep->set_flag_bit<DB_MERGEABLE_BIT> ();

	  if (!dep->is_import ())
	    worklist.safe_push (dep);
	}
      else if (ek == EK_MAYBE_SPEC)
	/* It's whatever we already found.  */
	ek = dep->get_entity_kind ();
      else if (ek == EK_DECL && dep->get_entity_kind () == EK_SPECIALIZATION)
	{
	  /* decl is a friend of a template instantiation.  These
	     have some of the properties of regular decls.  */
	  dump (dumper::DEPEND)
	    && dump ("Template friend %N discovered", decl);
	}
      else
	/* Make sure we have consistent categorization.  */
	gcc_checking_assert (dep->get_entity_kind () == ek);

      dump (dumper::DEPEND)
	&& dump ("%s on %s %C:%N added", binding_p ? "Binding" : "Dependency",
		 dep->entity_kind_name (), TREE_CODE (decl), decl);

      if (current
	  && dep->get_entity_kind () != EK_NAMESPACE
	  && dep->get_entity_kind () != EK_REDIRECT)
	{
	  current->deps.safe_push (dep);
	  if (current->is_binding ())
	    dep->deps.safe_push (current);
	  else
	    {
	      if (dep->get_entity_kind () == EK_UNNAMED
		  || dep->get_entity_kind () == EK_SPECIALIZATION)
		current->set_flag_bit<DB_REFS_UNNAMED_BIT> ();

	      if (dep->is_internal ())
		current->set_flag_bit<DB_REFS_INTERNAL_BIT> ();

	      if (TREE_CODE (dep->get_entity ()) == TYPE_DECL
		  && UNSCOPED_ENUM_P (TREE_TYPE (dep->get_entity ()))
		  && (CP_DECL_CONTEXT (current->get_entity ())
		      == TREE_TYPE (dep->get_entity ())))
		/* Unscoped enum values are pushed into the containing
		   scope.  Insert a dependency to the current binding, if it
		   is one of the enum constants.  */
		dep->deps.safe_push (current);
	    }

	  if (dep->is_unreached ())
	    {
	      /* The dependency is reachable now.  */
	      reached_unreached = true;
	      dump (dumper::DEPEND)
		&& dump ("Reaching unreached %s %C:%N", dep->entity_kind_name (),
			 TREE_CODE (dep->get_entity ()), dep->get_entity ());
	      dep->clear_flag_bit<DB_UNREACHED_BIT> ();
	    }
	}
    }
 
  return dep;
}

/* VALUE is an overload of decls that is bound in this module.  Create
   the relevant depsets for the binding and its conents.  MAYBE_TYPE
   is used for struct stat hack behaviour.  */

bool
depset::hash::add_binding (tree ns, tree value)
{
  current = make_binding (ns, NULL_TREE);

  tree name = NULL_TREE;
  gcc_checking_assert (!is_for_mergeable () && TREE_PUBLIC (ns));
  for (ovl_iterator iter (value); iter; ++iter)
    {
      tree decl = *iter;
      name = DECL_NAME (decl);

      gcc_checking_assert (!IDENTIFIER_ANON_P (name));
      gcc_checking_assert (!(TREE_CODE (decl) == NAMESPACE_DECL
			     && !DECL_NAMESPACE_ALIAS (decl)));
      // FIXME:Distinguish GMF usings from purview usings.

      if (MAYBE_DECL_MODULE_OWNER (decl) == MODULE_NONE)
	/* Ignore global module fragment entities.  */
	continue;

      tree not_tmpl = STRIP_TEMPLATE (decl);
      if (TREE_CODE (not_tmpl) != CONST_DECL
	  && TREE_CODE (not_tmpl) != TYPE_DECL
	  && DECL_THIS_STATIC (not_tmpl))
	{
	  if (!header_module_p ())
	    /* Ignore internal-linkage entitites.  */
	    continue;
	  // FIXME: Promote here?  Or should we have done that already?
	}

      if ((TREE_CODE (decl) == VAR_DECL
	   || TREE_CODE (decl) == TYPE_DECL)
	  && DECL_TINFO_P (decl))
	/* Ignore TINFO things.  */
	continue;

      bool using_p = iter.using_p ();
      depset::entity_kind ek = depset::EK_DECL;
      tree maybe_using = decl;
      if (using_p)
	{
	  maybe_using = iter.get_using ();
	  ek = depset::EK_USING;
	}
      else if (CP_DECL_CONTEXT (decl) != ns
	       && TREE_CODE (decl) != CONST_DECL)
	{
	  /* A using that lost its wrapper.  */
	  // FIXME: name-lookup should preserve this, but we tend to
	  // drop it for non-function decls :(
	  using_p = true;
	  maybe_using = ovl_make (decl, NULL_TREE);
	  OVL_USING_P (maybe_using) = true;
	  ek = depset::EK_USING;
	}

      depset *dep = add_dependency (maybe_using, ek);
      if (iter.hidden_p ())
	{
	  /* It is safe to mark the target decl with the hidden bit,
	     because we cannot have other bindings to this decl in
	     this TU.  The same is not true for export_p. */
	  // FIXME: check whether a partition can make the entity
	  // unhidden, perhaps the right things already happen?
	  gcc_checking_assert (!using_p);
	  dep->set_hidden_binding ();
	}
    }

  bool added = current->deps.length () != 0;
  if (added)
    {
      current->set_binding_name (name);
      depset **slot = binding_slot (ns, name, true);
      gcc_checking_assert (!*slot);
      *slot = current;
    }
  else
    delete current;
  current = NULL;

  return added;
}

/* Compare two writable bindings.  We don't particularly care on the
   ordering, just so long as it reproduces across builds.  */

static int
writable_cmp (const void *a_, const void *b_)
{
  tree a = *(const tree *)a_;
  tree b = *(const tree *)b_;

  tree first_a = OVL_FIRST (a);
  tree first_b = OVL_FIRST (b);

  gcc_checking_assert (first_a != first_b);
  return DECL_UID (first_a) < DECL_UID (first_b) ? -1 : +1;
}

/* Recursively find all the namespace bindings of NS.
   Add a depset for every binding that contains an export or
   module-linkage entity.  Add a defining depset for every such decl
   that we need to write a definition.  Such defining depsets depend
   on the binding depset.  Returns true if we contain something
   explicitly exported.  */

bool
depset::hash::add_writables (tree ns, bitmap partitions)
{
  dump () && dump ("Finding writables in %N", ns);
  dump.indent ();

  unsigned count = 0;
  auto_vec<tree> bindings (DECL_NAMESPACE_BINDINGS (ns)->size ());
  hash_table<named_decl_hash>::iterator end
    (DECL_NAMESPACE_BINDINGS (ns)->end ());
  for (hash_table<named_decl_hash>::iterator iter
	 (DECL_NAMESPACE_BINDINGS (ns)->begin ()); iter != end; ++iter)
    if (tree value = extract_module_binding (*iter, ns, partitions))
      bindings.quick_push (value);

  /* Sort for reproducibility.  */
  bindings.qsort (writable_cmp);
  while (bindings.length ())
    {
      tree value = bindings.pop ();
      if (TREE_CODE (value) != NAMESPACE_DECL)
	{
	  if (add_binding (ns, value))
	    count++;
	}
      else if (DECL_NAMESPACE_ALIAS (value))
	// FIXME: Not sure what to do with namespace aliases
	gcc_unreachable ();
      else if (DECL_NAME (value))
	{
	  gcc_checking_assert (TREE_PUBLIC (value));
	  bool not_empty = add_writables (value, partitions);
	  if (not_empty || DECL_MODULE_EXPORT_P (value))
	    {
	      add_dependency (value, depset::EK_NAMESPACE);
	      count++;
	    }
	}
    }

  if (count)
    dump () && dump ("Found %u entries", count);
  dump.outdent ();

  return count != 0;
}

typedef std::pair<bitmap, auto_vec<spec_entry *> > spec_tuple;

/* We add the partial & explicit specializations, and the explicit
   instntiations.  */

static void
specialization_add (bool decl_p, spec_entry *entry, void *data_)
{
  spec_tuple *data = reinterpret_cast <spec_tuple *> (data_);
  tree spec = entry->spec;

#if CHECKING_P
  /* We exclusively use decls to locate things.  Make sure there's
     no mismatch between the two specialization tables we keep.
     pt.c optimizes instantiation lookup using a complicated
     heuristic.  We don't attempt to replicate that algorithm, but
     observe its behaviour and reproduce it upon read back.  */
  if (TREE_CODE (DECL_TEMPLATE_RESULT (entry->tmpl)) == TYPE_DECL
      && TYPE_DECL_ALIAS_P (DECL_TEMPLATE_RESULT (entry->tmpl)))
    {
      // FIXME: It's not entirely clear to me the relationship when
      // the template's an alias.
    }
  else if (!decl_p)
    {
      /* If it's a (non-template-alias) type, we don't expect to find
	 its TYPE_STUB_DECL.  */
      gcc_checking_assert (DECL_CLASS_TEMPLATE_P (entry->tmpl));
      tree existing = match_mergeable_specialization
	(TYPE_STUB_DECL (spec), entry->tmpl, entry->args, false);
      gcc_checking_assert (!existing);
    }
#endif

  if (!decl_p)
    {
      if (TYPE_DECL_ALIAS_P (DECL_TEMPLATE_RESULT (entry->tmpl)))
	/* This will be found via the decl walk.  We only want it
	   once.  */
	// FIXME: Sure?
	return;

      if (TREE_CODE (TREE_TYPE (DECL_TEMPLATE_RESULT (entry->tmpl)))
	  == ENUMERAL_TYPE)
	// FIXME: Likewise. Hey, make anon enum tags anon
	return;

      spec = TYPE_STUB_DECL (spec);
    }

  data->second.safe_push (entry);
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

/* Insert a redirect for the DECL_TEMPLATE_RESULT, as we're unable to
   go from there to here (without repeating the above instantiation
   search for *every* MAYBE_SPEC dependency add.  */

depset *
depset::hash::add_redirect (depset *target)
{
  target->set_flag_bit<DB_PARTIAL_BIT> ();

  tree inner = DECL_TEMPLATE_RESULT (target->get_entity ());
  depset *redirect = make_entity (inner, EK_REDIRECT);
  depset **slot = entity_slot (inner, true);
  gcc_checking_assert (!*slot);
  *slot = redirect;
  redirect->deps.safe_push (target);

  return redirect;
}

/* CLONE is a clone of TARGET.  Note that CURRENT might not be
   TARGET, but a container of TARGET.  */

depset *
depset::hash::add_clone (tree clone, tree target)
{
  gcc_checking_assert (!is_for_mergeable ()
		       && !current->is_import ());

  if (TREE_CODE (clone) == FUNCTION_DECL)
    /* If we're a template, we should be registering the template.  */
    gcc_checking_assert (!DECL_TEMPLATE_INFO (clone)
			 || (DECL_TEMPLATE_RESULT (DECL_TI_TEMPLATE (clone))
			     != clone));

  depset *clone_dep = make_entity (clone, EK_CLONE);
  depset **slot = entity_slot (clone, true);

  dump (dumper::DEPEND)
    && dump ("Adding clone %C:%N of %C:%N",
	     TREE_CODE (clone), clone,
	     TREE_CODE (target), target);

  if (has_definition (clone))
    clone_dep->set_flag_bit<DB_DEFN_BIT> ();

  /* Clones are always mergeable, because we're keyed to the cloned
     function.  */
  // FIXME: We should probably separate mergeable from location
  clone_dep->set_flag_bit<DB_MERGEABLE_BIT> ();

  gcc_checking_assert (!*slot);
  *slot = clone_dep;

  current->deps.safe_push (clone_dep);

  worklist.safe_push (clone_dep);

  return clone_dep;
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
depset::hash::add_specializations (bool decl_p, bitmap partitions)
{
  spec_tuple data (partitions, 100);
  walk_specializations (decl_p, specialization_add, &data);
  data.second.qsort (specialization_cmp);
  while (data.second.length ())
    {
      spec_entry *entry = data.second.pop ();
      tree spec = entry->spec;
      bool is_partial = false;
      int use_tpl = 0;
      if (!decl_p)
	{
	  use_tpl = CLASSTYPE_USE_TEMPLATE (spec);

	  tree partial = DECL_TEMPLATE_SPECIALIZATIONS (entry->tmpl);
	  for (; partial; partial = TREE_CHAIN (partial))
	    if (TREE_TYPE (partial) == spec)
	      break;

	  if (partial)
	    {
	      gcc_checking_assert (entry->args == TREE_PURPOSE (partial));
	      is_partial = true;
	      /* Get the TEMPLATE_DECL for the partial
		 specialization.  */
	      spec = TREE_VALUE (partial);
	      gcc_assert (DECL_USE_TEMPLATE (spec) == use_tpl);
	    }
	  else
	    {
	      tree ti = CLASSTYPE_TEMPLATE_INFO (spec);
	      tree tmpl = TI_TEMPLATE (ti);

	      spec = TYPE_NAME (spec);
	      if (spec == DECL_TEMPLATE_RESULT (tmpl))
		{
		  spec = tmpl;
		  use_tpl = DECL_USE_TEMPLATE (spec);
		}
	    }
	}
      else if (tree ti = DECL_TEMPLATE_INFO (spec))
	{
	  tree tmpl = TI_TEMPLATE (ti);

	  use_tpl = DECL_USE_TEMPLATE (spec);
	  if (spec == DECL_TEMPLATE_RESULT (tmpl))
	    {
	      spec = tmpl;
	      gcc_checking_assert (DECL_USE_TEMPLATE (spec) == use_tpl);
	    }
	}
      else
	/* This is some kind of friend.  */
	gcc_assert (!DECL_USE_TEMPLATE (spec));

      bool needs_reaching = false;
      if (use_tpl == 1)
	/* Implicit instantiations only walked if we reach them.  */
	needs_reaching = true;
      else if (!MAYBE_DECL_MODULE_OWNER (spec))
	/* Likewise, GMF entities.  */
	needs_reaching = true;

#if 0 && CHECKING_P
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


      unsigned owner = MAYBE_DECL_MODULE_OWNER (spec);
      if (owner >= MODULE_IMPORT_BASE)
	owner = (*modules)[owner]->remap;
      depset *dep = add_dependency (spec, depset::EK_SPECIALIZATION,
				    owner >= MODULE_IMPORT_BASE);
      if (dep->is_marked ())
	{
	  /* An already located specialization, this must be a friend
	     with a different entry->tmpl.  */
	  spec_entry *other = reinterpret_cast <spec_entry *> (dep->deps[0]);
	  gcc_checking_assert (other->tmpl != entry->tmpl);
	  gcc_checking_assert (!is_partial
			       && needs_reaching == dep->is_unreached ());
	  // FIXME: Should we record this alias and do something with
	  // it?
	}
      else
	{
	  dep->set_marked ();
	  dep->deps.safe_push (reinterpret_cast<depset *> (entry));
	  if (needs_reaching)
	    dep->set_flag_bit<DB_UNREACHED_BIT> ();
	  if (is_partial)
	    add_redirect (dep);
	}
    }
}

/* Iteratively find dependencies.  During the walk we may find more
   entries on the same binding that need walking.  */

void
depset::hash::find_dependencies ()
{
  trees_out walker (NULL, NULL, *this);
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
	      dump (dumper::DEPEND)
		&& dump ("Dependencies of %s %C:%N",
			 is_for_mergeable () ? "mergeable"
			 : current->entity_kind_name (), TREE_CODE (decl), decl);
	      dump.indent ();
	      walker.begin ();
	      if (is_for_mergeable ())
		{
		  walker.key_mergeable (current);
		  if (current->get_entity_kind () == EK_SPECIALIZATION)
		    /* We also want to make specializations of members
		       depend on their container -- that might well be
		       in the same SCC.  */
		    walker.tree_ctx (CP_DECL_CONTEXT (decl), true, decl);
		}
	      else if (current->get_entity_kind () == EK_USING)
		walker.tree_ctx (OVL_FUNCTION (decl), false, NULL_TREE);
	      else if (TREE_VISITED (decl))
		/* A global tree.  */;
	      else
		{
		  walker.mark_declaration (decl, current->has_defn ());

		  if (current->get_entity_kind () == EK_SPECIALIZATION)
		    {
		      gcc_assert (current->is_marked ());
		      /* Even though specializations end up keyed to
		         their primary template, we need to explicitly
		         make them depend on both that and the
		         args.  */
		      spec_entry *entry
			= reinterpret_cast <spec_entry *> (current->deps[0]);
		      walker.tree_node (entry->tmpl);
		      walker.tree_node (entry->args);
		    }

		  /* Turn the Sneakoscope on when depending the decl.  */
		  sneakoscope = true;
		  walker.tree_ctx (decl, false, NULL_TREE);
		  sneakoscope = false;
		  if (current->has_defn ())
		    walker.write_definition (decl);
		}
	      walker.end ();
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

/* Add a decl into the mergeable hash.  */

void
depset::hash::add_mergeable (depset *mergeable)
{
  gcc_checking_assert (is_for_mergeable () && !mergeable->is_binding ());
  tree decl = mergeable->get_entity ();

  /* All entities go into the table.  */
  depset **slot = entity_slot (decl, true);
  gcc_checking_assert (!*slot);
  depset *dep = make_entity (decl, mergeable->get_entity_kind ());
  *slot = dep;

  /* Only add mergeable depsets for walking.  */
  if (mergeable->is_mergeable ())
    worklist.safe_push (dep);

  /* So we can locate the mergeable depset this depset refers to,
     mark the first dep.  */
  dep->set_marked ();
  dep->deps.safe_push (mergeable);

  if (mergeable->is_partial ())
    add_redirect (dep);
}

void
depset::hash::add_mergeable_horcrux (depset *unnamed)
{
  gcc_checking_assert (is_for_mergeable ());
  tree decl = unnamed->get_entity ();

  /* All horcruxes go into the dependency table.  */
  depset **slot = entity_slot (decl, true);
  depset *dep = *slot;
  if (dep)
    gcc_checking_assert (!dep->is_marked ());
  else
    {
      dep = make_entity (decl, unnamed->get_entity_kind ());
      *slot = dep;
      if (unnamed->is_partial ())
	add_redirect (dep);
    }
}

/* Compare two binding entries.  TYPE_DECL before non-exported before
   exported.  */
// FIXME: Reachable globals are not findable by name

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
  else
    a_export = DECL_MODULE_EXPORT_P (a_ent);
  
  bool b_using = b->get_entity_kind () == depset::EK_USING;
  bool b_export;
  if (b_using)
    {
      b_export = OVL_EXPORT_P (b_ent);
      b_ent = OVL_FUNCTION (b_ent);
    }
  else
    b_export = DECL_MODULE_EXPORT_P (b_ent);

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
	dep->deps.qsort (binding_cmp);
      else if (dep->refs_internal ())
	{
	  ok = false;
	  tree decl = dep->get_entity ();
#if 1
	  // FIXME: __thread_active_p is borked, so allow it.
	  if (CP_DECL_CONTEXT (decl) == global_namespace
	      && DECL_NAME (decl)
	      && !strcmp (IDENTIFIER_POINTER (DECL_NAME (decl)),
			  "__gthread_active_p"))
	    ok = true;
#endif
	  if (!ok)
	    for (unsigned ix = dep->deps.length (); ix--;)
	      {
		depset *rdep = dep->deps[ix];
		if (rdep->is_internal ())
		  {
		    // FIXME: Better location information?  We're
		    // losing, so it doesn't matter about efficiency
		    error_at (DECL_SOURCE_LOCATION (decl),
			      "%q#D references internal linkage entity %q#D",
			      decl, rdep->get_entity ());
		    break;
		  }
	      }
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
  gcc_checking_assert (v->is_binding () || !v->is_unreached ());

  v->cluster = v->section = ++index;
  stack.safe_push (v);

  /* Walk all our dependencies, ignore a first marked slot  */
  for (unsigned ix = v->is_marked (); ix != v->deps.length (); ix++)
    {
      depset *dep = v->deps[ix];
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
      /* Both are bindings.  */
      /* Order by identifier hash (hey, it's a consistent number).  */
      // FIXME: strcmp for user-meaningful order?
      gcc_checking_assert (a->get_name () != b->get_name ());
      return (IDENTIFIER_HASH_VALUE (a->get_name ())
	      < IDENTIFIER_HASH_VALUE (b->get_name ())
	      ? -1 : +1);
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

/* Compare members of a cluster.  Order defn < decl < bind.  depsets
   of the same kind can be arbitrary, but we want something
   stable.  */
// FIXME: Use entity_kind more

static int
cluster_cmp (const void *a_, const void *b_)
{
  depset *a = *(depset *const *)a_;
  depset *b = *(depset *const *)b_;

  bool is_bind = a->is_binding ();
  if (is_bind != b->is_binding ())
    /* Exactly one is a binding, it comes last.  */
    return is_bind ? +1 : -1;

  if (!is_bind)
    {
      /* Neither is a binding, try order-by-defn.  */
      bool is_defn = a->has_defn ();
      if (is_defn != b->has_defn ())
	/* Exactly one is a defn.  It comes first.  */
	return is_defn ? -1 : +1;

      if (!is_defn)
	{
	  /* Neither is a defn, try order-by-using.  */
	  bool is_using = a->get_entity_kind () == depset::EK_USING;
	  if (is_using != (b->get_entity_kind () == depset::EK_USING))
	    /* Exactly one is a using.  It comes last.  */
	    return is_using ? +1 : -1;
	}
    }

  /* They are both the same kind.  Order for qsort stability.  */
  tree a_decl = a->get_entity ();
  tree b_decl = b->get_entity ();

  if (a->get_entity_kind () == depset::EK_USING)
    {
      /* If one is a using, the other must be too.  */
      a_decl = OVL_FUNCTION (a_decl);
      b_decl = OVL_FUNCTION (b_decl);
    }

  if (a_decl != b_decl)
    /* Different entities, order by their UID.  */
    return DECL_UID (a_decl) < DECL_UID (b_decl) ? -1 : +1;

  /* Same decl.  They must be bindings or using_decls (when not in the
     same cluster).  */

  if (a->is_binding ())
    {
      /* Order by identifier hash (hey, it's a consistent number).  */
      // FIXME: strcmp for user-meaningful order?
      gcc_checking_assert (a->get_name () != b->get_name ());
      return (IDENTIFIER_HASH_VALUE (a->get_name ())
	      < IDENTIFIER_HASH_VALUE (b->get_name ())
	      ? -1 : +1);
    }

  /* Must be doing global ordering.  */
  gcc_checking_assert (a->get_entity_kind () == depset::EK_USING
		       && !a->section && !b->section);
  /* Order by depset address.  Not the best, but it is something.  */
  return a < b ? -1 : +1;
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

      if (item->is_binding () || !item->is_unreached ())
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
		 is_for_mergeable () ? "mergeable"
		 : !v->has_defn () ? "declaration" : "definition",
		 v->entity_kind_name (), TREE_CODE (v->get_entity ()),
		 v->get_entity ()));
      if (!v->cluster)
	connector.connect (v);
    }

  deps.release ();
  return connector.result;
}

bool
specset::hash::add (tree ns, tree name, unsigned index)
{
  specset key (ns, name);
  specset **slot = find_slot (&key, INSERT);
  specset *set = *slot;
  bool is_new = !set;

  if (is_new || set->num == (1u << set->allocp2))
    {
      unsigned n = set ? set->num * 2 : 1;
      size_t new_size = (offsetof (specset, pending)
			 + sizeof (specset::pending) * n);
      specset *new_set = (new (::operator new (new_size))
			  specset (set ? set : &key));
      delete set;
      set = new_set;
      *slot = set;
    }

  set->pending[set->num++] = index;

  return is_new;
}

specset *
specset::hash::lookup (tree ns, tree name)
{
  specset key (ns, name);
  specset *res = NULL;

  if (specset **slot = find_slot (&key, NO_INSERT))
    {
      res = *slot;
      /* We need to remove the specset without deleting it. */
      traits::mark_deleted (*slot);
    }

  return res;
}

/* Initialize location spans.  */

void
loc_spans::init (const line_maps *lmaps, const line_map_ordinary *map)
{
  gcc_checking_assert (!init_p ());
  spans.reserve (20);

  span interval;
  interval.ordinary.first = 0;
  interval.macro.second = MAX_LOCATION_T + 1;
  interval.ordinary_delta = interval.macro_delta = 0;

  /* A span for reserved fixed locs.  */
  interval.ordinary.second
    = MAP_START_LOCATION (LINEMAPS_ORDINARY_MAP_AT (line_table, 0));
  interval.macro.first = interval.macro.second;
  dump (dumper::LOCATION)
    && dump ("Fixed span %u ordinary:[%u,%u) macro:[%u,%u)", spans.length (),
	     interval.ordinary.first, interval.ordinary.second,
	     interval.macro.first, interval.macro.second);
  spans.quick_push (interval);

  /* A span for command line & forced headers.  */
  interval.ordinary.first = interval.ordinary.second;
  interval.macro.second = interval.macro.first;
  interval.ordinary.second = map->start_location;
  interval.macro.first = LINEMAPS_MACRO_LOWEST_LOCATION (lmaps);
  dump (dumper::LOCATION)
    && dump ("Pre span %u ordinary:[%u,%u) macro:[%u,%u)", spans.length (),
	     interval.ordinary.first, interval.ordinary.second,
	     interval.macro.first, interval.macro.second);
  spans.quick_push (interval);
  
  /* Start an interval for the main file.  */
  interval.ordinary.first = interval.ordinary.second;
  interval.macro.second = interval.macro.first;
  dump (dumper::LOCATION)
    && dump ("Main span %u ordinary:[%u,*) macro:[*,%u)", spans.length (),
	     interval.ordinary.first, interval.macro.second);
  spans.quick_push (interval);
}

/* Open a new linemap interval.  The just-created ordinary map is the
   first map of the interval.  */

void
loc_spans::open (location_t hwm = UNKNOWN_LOCATION)
{
  if (hwm == UNKNOWN_LOCATION)
    hwm = MAP_START_LOCATION (LINEMAPS_LAST_ORDINARY_MAP (line_table));

  span interval;
  interval.ordinary.first = interval.ordinary.second = hwm;
  interval.macro.first = interval.macro.second
    = LINEMAPS_MACRO_LOWEST_LOCATION (line_table);
  interval.ordinary_delta = interval.macro_delta = 0;
  dump (dumper::LOCATION)
    && dump ("Opening span %u ordinary:[%u,... macro:...,%u)",
	     spans.length (), interval.ordinary.first,
	     interval.macro.second);
  spans.safe_push (interval);
}

/* Close out the current linemap interval.  The last maps are within
   the interval.  */

void
loc_spans::close ()
{
  span &interval = spans.last ();

  interval.ordinary.second
    = ((line_table->highest_location + (1 << line_table->default_range_bits))
       & ~((1u << line_table->default_range_bits) - 1));
  interval.macro.first = LINEMAPS_MACRO_LOWEST_LOCATION (line_table);
  dump (dumper::LOCATION)
    && dump ("Closing span %u ordinary:[%u,%u) macro:[%u,%u)",
	     spans.length () - 1,
	     interval.ordinary.first,interval.ordinary.second,
	     interval.macro.first, interval.macro.second);
}

/* Given an ordinary location LOC, return the lmap_interval it resides
   in.  NULL if it is not in an interval.  */

const loc_spans::span *
loc_spans::ordinary (location_t loc)
{
  unsigned len = spans.length ();
  unsigned pos = 0;
  while (len)
    {
      unsigned half = len / 2;
      const span &probe = spans[pos + half];
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
  unsigned len = spans.length ();
  unsigned pos = 0;
  while (len)
    {
      unsigned half = len / 2;
      const span &probe = spans[pos + half];
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
      if (IS_MACRO_LOC (from))
	{
	  /* Find the ordinary location nearest FROM.  */
	  const line_map *map = linemap_lookup (lmaps, from);
	  const line_map_macro *mac_map = linemap_check_macro (map);
	  from = MACRO_MAP_EXPANSION_POINT_LOCATION (mac_map);
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
  return parent;
}

/* Find or create module NAME & PARENT in the hash table.  */

module_state *
get_module (tree name, module_state *parent, bool partition)
{
  if (partition)
    {
      if (!parent)
	parent = get_primary ((*modules)[MODULE_PURVIEW]);

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
  if (IS_ABSOLUTE_PATH (ptr) || ptr[0] == '.')
    /* A header name.  */
    return get_module (build_string (strlen (ptr), ptr));

  bool partition = false;
  module_state *mod = NULL;

  for (const char *probe = ptr;; probe++)
    if (!*probe || *probe == '.' || *probe == ':')
      {
	size_t len = probe - ptr;
	if (!len)
	  return NULL;
	mod = get_module (get_identifier_with_length (ptr, len), mod, partition);
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

  return mod;
}

bool
module_normal_import_p (unsigned m)
{
  module_state *module = (*modules)[m];

  return !(module->is_header () || module->is_partition ()
	   || module->is_primary ());
}

/* Create a mapper.  The mapper may be dead.  Yes, I'm embedding some
   client-side socket handling in the compiler.  At least it's not
   ipv4.  */

module_mapper::module_mapper (location_t loc, const char *option)
  : name (NULL), from (NULL), to (NULL), pex (NULL), sigpipe (SIG_IGN),
    /* Exercise buffer expansion code.  */
    buffer (NULL), size (EXPERIMENT (3, 200)), pos (NULL), end (NULL),
    start (NULL), fd_from (-1), fd_to (-1), batching (false)
{
  const char *dflt = "|cxx-mapper";
  pex = NULL;

  /* We set name as soon as we know what kind of mapper this is.  */
  if (!option)
    option = dflt;

  dump () && dump ("Initializing mapper %s", option);

  int err = 0;
  const char *errmsg = NULL;

  /* First copy.  */
  unsigned spaces = 0;
  unsigned len = 0;
  char *cookie = NULL;

  for (; option[len]; len++)
    {
      if (option[len] == ' ')
	spaces++;
      if (option[len] == '?' && !cookie)
	cookie = const_cast<char *> (&option[len]);
    }
  char *writable = XNEWVEC (char, len + 1);
  memcpy (writable, option, len + 1);
  if (cookie)
    {
      len = cookie - option;
      cookie = writable + len;
      *cookie = 0;
    }

  if (writable[0] == '|')
    {
      /* A program to spawn and talk to.  */
      /* Split writable at white-space.  No space-containing args
	 for you!  */
      char **argv = XALLOCAVEC (char *, spaces + 2);
      unsigned arg_no = 0;

      for (char *ptr = writable + 1; ; ptr++)
	{
	  argv[arg_no] = ptr;
	  for (;; ptr++)
	    {
	      if (*ptr == ' ')
		break;
	      else if (*ptr)
		continue;
	      else if (ptr != cookie)
		break;
	      else if (arg_no != 1)
		{
		  /* Not a cookie after all.  */
		  *cookie = '?';
		  cookie = NULL;
		}
	    }
	  if (!arg_no++)
	    len = ptr - (writable + 1);	  
	  if (!*ptr)
	    break;
	  *ptr = 0;
	}
      argv[arg_no] = NULL;

      pex = pex_init (PEX_USE_PIPES, progname, NULL);
      to = pex_input_pipe (pex, false);
      if (!to)
	{
	  err = errno;
	  errmsg = "connecting input";
	}
      else
	{
	  int flags = PEX_SEARCH;

	  /* Use strcmp to detect default, so we may explicitly name
	     it with additional args in tests etc.  */
	  if ((option == dflt || 0 == strcmp (argv[0], dflt + 1))
	      && save_decoded_options[0].opt_index == OPT_SPECIAL_program_name
	      && save_decoded_options[0].arg != progname)
	    {
	      /* Prepend the invoking path.  */
	      const char *fullname = save_decoded_options[0].arg;
	      size_t dir_len = progname - fullname;
	      char *argv0 = XNEWVEC (char, dir_len + len + 1);
	      memcpy (argv0, fullname, dir_len);
	      memcpy (argv0 + dir_len, argv[0], len + 1);
	      argv[0] = argv0;
	      flags = 0;
	    }
	  errmsg = pex_run (pex, flags, argv[0], argv, NULL, NULL, &err);
	  if (!flags)
	    XDELETEVEC (argv[0]);
	}

      if (!errmsg)
	{
	  from = pex_read_output (pex, false);
	  if (from)
	    {
	      fd_to = fileno (to);
	      fd_from = fileno (from);
	    }
	  else
	    {
	      err = errno;
	      errmsg = "connecting output";
	      fclose (to);
	      to = NULL;
	    }
	}
      name = writable;
    }
  else if (writable[0] == '<')
    {
      /* File descriptors, inc stdin/out.  */
      int from = -1, to = -1;
      char *ptr = writable + 1, *eptr;
      from = strtoul (ptr, &eptr, 0);
      if (*eptr == '>')
	{
	  ptr = eptr + 1;
	  to = strtoul (ptr, &eptr, 0);
	  if (eptr != ptr && from == -1)
	    from = to;
	}
      if (*eptr)
	errmsg = "parsing";
      else
	{
	  if (eptr == writable + 2)
	    {
	      from = fileno (stdin);
	      to = fileno (stdout);
	    }
	  fd_to = to;
	  fd_from = from;
	}
      name = writable;
    }

  if (!name)
    {
      int fd;

      /* Does it look like a socket?  */
#ifdef NETWORKING
#ifdef HAVE_AF_UNIX
      sockaddr_un un;
      size_t un_len = 0;
      un.sun_family = AF_UNSPEC;
#endif
      int port = 0;
#ifdef HAVE_AF_INET6
      struct addrinfo *addrs = NULL;
#endif
#endif
      if (writable[0] == '=')
	{
	  /* A local socket.  */
#ifdef HAVE_AF_UNIX
	  if (len < sizeof (un.sun_path))
	    {
	      memset (&un, 0, sizeof (un));
	      un.sun_family = AF_UNIX;
	      memcpy (un.sun_path, writable + 1, len);
	    }
	  un_len = offsetof (struct sockaddr_un, sun_path) + len + 1;
#else
	  errmsg = "unix protocol unsupported";
#endif
	  name = writable;
	}
      else if (char *colon = (char *)memrchr (writable, ':', len))
	{
	  /* Try a hostname:port address.  */
	  char *endp;
	  port = strtoul (colon + 1, &endp, 10);
	  if (port && endp != colon + 1 && !*endp)
	    {
	      /* Ends in ':number', treat as ipv6 domain socket.  */
#ifdef HAVE_AF_INET6
	      addrinfo hints;

	      hints.ai_flags = AI_NUMERICSERV;
	      hints.ai_family = AF_INET6;
	      hints.ai_socktype = SOCK_STREAM;
	      hints.ai_protocol = 0;
	      hints.ai_addrlen = 0;
	      hints.ai_addr = NULL;
	      hints.ai_canonname = NULL;
	      hints.ai_next = NULL;

	      *colon = 0;
	      /* getaddrinfo requires a port number, but is quite
		 happy to accept invalid ones.  So don't rely on it.  */
	      if (int e = getaddrinfo (colon == writable ? NULL : writable,
				       "0", &hints, &addrs))
		{
		  err = e;
		  errmsg = "resolving address";
		}
	      else
		un.sun_family = AF_INET6;
	      *colon = ':';
#else
	      errmsg = "ipv6 protocol unsupported";
#endif
	      name = writable;
	    }
	}
      
      if (un.sun_family != AF_UNSPEC)
	{
	  fd = socket (un.sun_family, SOCK_STREAM, 0);
	  if (fd < 0)
	    ;
#ifdef HAVE_AF_UNIX
	  else if (un.sun_family == AF_UNIX)
	    {
	      if (connect (fd, (sockaddr *)&un, un_len) < 0)
		{
		  close (fd);
		  fd = -1;
		}
	    }
#endif
#ifdef HAVE_AF_INET6
	  else if (un.sun_family == AF_INET6)
	    {
	      struct addrinfo *next;
	      for (next = addrs; next; next = next->ai_next)
		if (next->ai_family == AF_INET6
		    && next->ai_socktype == SOCK_STREAM)
		  {
		    sockaddr_in6 *in6 = (sockaddr_in6 *)next->ai_addr;
		    in6->sin6_port = htons (port);
		    if (ntohs (in6->sin6_port) != port)
		      errno = EINVAL;
		    else if (!connect (fd, next->ai_addr, next->ai_addrlen))
		      break;
		  }

	      if (!next)
		{
		  close (fd);
		  fd = -1;
		}
	    }
#endif
	  else
	    gcc_unreachable ();

#ifdef HAVE_AF_INET6
	  freeaddrinfo (addrs);
#endif
	  if (fd >= 0)
	    /* We have a socket.  */
	    fd_from = fd_to = fd;
	  else if (!errmsg)
	    {
	      err = errno;
	      errmsg = "connecting socket";
	    }
	}
    }

  if (!name)
    {
      /* Try a mapping file.  */
      from = fopen (writable, "r");
      if (from)
	fd_from = fileno (from);
      else
	{
	  err = errno;
	  errmsg = "opening";
	}
      name = writable;
    }

  if (errmsg)
    {
      errno = err;
      error_at (loc, err <= 0 ? G_("failed %s of mapper %qs: %s")
		: G_("failed %s of mapper %qs: %m"),
		errmsg, name ? name : option,
		err < 0 ? gai_strerror (err) : _("Facility not provided"));
      kill (loc);
      return;
    }

  if (noisy_p ())
    {
      fprintf (stderr, " mapper:%s", name);
      fflush (stderr);
    }
  dump () && dump ("Initialized mapper");

  pos = end = buffer = XNEWVEC (char, size);

  if (fd_to >= 0)
    {
#ifdef SIGPIPE
      /* We need to ignore sig pipe for a while.  */
      sigpipe = signal (SIGPIPE, SIG_IGN);
#endif
      if (!handshake (loc, cookie ? cookie + 1 : main_input_filename))
	kill (loc);
    }
  else
    {
      /* A mapping file.  Read it.  */
      dump () && dump ("Reading mapping file %s", name);

      bool starting = true;
      for (int r; (r = get_response (loc)) >= 0;)
	if (r)
	  {
	    char *mod = response_token (loc);
	    bool ignore = false;
	    char *file = NULL;

	    /* Ignore non-cookie lines.  */
	    if (cookie && 0 != strcmp (mod, cookie + 1))
	      ignore = true;
	    else
	      {
		if (cookie)
		  mod = response_token (loc);
		if (mod)
		  file = response_token (loc, true);
	      }

	    if (!response_eol (loc, ignore))
	      continue;

	    if (!file)
	      continue;

	    if (starting && 0 == strcmp (mod, "$root"))
	      {
		set_cmi_repo (file);
		continue;
	      }
	    
	    starting = false;
	    file = maybe_strip_cmi_prefix (file);
	    module_state *state = get_module (mod);
	    if (!state)
	      response_unexpected (loc);
	    else if (!state->filename)
	      state->filename = xstrdup (file);
	    else if (strcmp (state->filename, file))
	      warning_at (loc, 0, "ignoring conflicting mapping of %qs to %qs",
			  state->get_flatname (), file);
	  }
      fclose (from);
      from = NULL;
      /* Leave fd_from alone to show liveness.  */
    }
}

/* Close down the mapper.  Mark it as not restartable.  */

void
module_mapper::kill (location_t loc)
{
  if (!is_live ())
    return;

  dump () && dump ("Killing mapper %s", name);

  if (to)
    {
      fclose (to);
      to = NULL;
      fd_to = -1;
    }
#ifdef NETWORKING
  else if (fd_to >= 0)
    {
      shutdown (fd_to, SHUT_WR);
      fd_to = -1;
    }
#endif

  if (pex)
    {
      int status;

      pex_get_status (pex, 1, &status);
      pex_free (pex);
      pex = NULL;

      if (WIFSIGNALED (status))
	error_at (loc, "mapper %qs died by signal %s",
		  name, strsignal (WTERMSIG (status)));
      else if (WIFEXITED (status) && WEXITSTATUS (status) != 0)
	error_at (loc, "mapper %qs exit status %d",
		  name, WEXITSTATUS (status));
      from = NULL;
      fd_from = -1;
    }
  else if (fd_from >= 0)
    {
      if (!is_file ())
	close (fd_from);
      fd_from = -1;
    }

#ifdef SIGPIPE
  if (sigpipe != SIG_IGN)
    /* Restore sigpipe.  */
    signal (SIGPIPE, sigpipe);
#endif

  XDELETEVEC (buffer);
  buffer = NULL;
}

/* Create a new mapper connecting to OPTION.  */

module_mapper *
module_mapper::make (location_t loc)
{
  const char *option = module_mapper_name;
  if (!option)
    option = getenv ("CXX_MODULE_MAPPER");
  return new module_mapper (loc, option);
}

/* Send a command to the mapper.  */

void
module_mapper::send_command (location_t loc, const char *format, ...)
{
  size_t actual = 0;
  if (pos != buffer)
    pos = end = buffer;
  if (batching)
    *end++ = '+';
  else if (end != buffer)
    *end++ = '-';

  for (;;)
    {
      va_list args;
      va_start (args, format);
      size_t available = (buffer + size) - end;
      actual = vsnprintf (end, available, format, args);
      va_end (args);
      if (actual < available)
	break;

      size = size * 2 + actual + 20;
      char *next = XRESIZEVEC (char, buffer, size);
      end = next + (end - buffer);
      buffer = pos = next;
    }

  if (batching)
    dump (dumper::MAPPER) && dump ("Mapper pending request:%s", end);
  else
    dump (dumper::MAPPER) && dump ("Mapper request:%s", buffer);
  end += actual;
  *end++ = '\n';
  if (!batching)
    {
      if (is_live () && end - buffer != write (fd_to, buffer, end - buffer))
	error_at (loc, "failed write to mapper %qs: %m", name);
      end = pos = buffer;
    }
}

/* Read a response from the mapper.  -ve -> end, 0 -> blank, +ve -> something*/

int
module_mapper::get_response (location_t loc)
{
  if (batching)
    pos = end + 1;
  else
    {
      gcc_assert (pos == end);
      size_t off = 0;
      bool bol = true;
      bool last = false;
      int stop = 0;

      if (is_live ())
	{
	  for (;;)
	    {
	      if (fd_to < 0)
		{
		  /* We're reading a file.  There can be no
		     continuations.  */
		  if (!fgets (buffer + off, size - off, from))
		    {
		      stop = feof (from) ? +1 : -1;
		      break;
		    }
		  off += strlen (buffer + off);
		  if (off && buffer[off - 1] == '\n')
		    break;
		}
	      else
		{
		  /* Reading a pipe or socket.  */
		  int bytes = read (fd_from, buffer + off, size - off - 1);
		  if (bytes <= 0)
		    {
		      stop = bytes ? -1 : +1;
		      break;
		    }
		  while (bytes)
		    {
		      if (bol)
			{
			  if (buffer[off] == '+')
			    batching = true;
			  else
			    last = true;
			}
		      bol = false;
		      if (char *eol
			  = (char *)memchr (buffer + off, '\n', size - off))
			{
			  bol = true;
			  unsigned nline = eol + 1 - buffer;
			  bytes -= nline - off;
			  off = nline;
			}
		      else
			{
			  off += bytes;
			  bytes = 0;
			  break;
			}
		    }
		  if (bol && last)
		    break;
		}
	      if (off + 1 == size)
		{
		  size *= 2;
		  buffer = XRESIZEVEC (char, buffer, size);
		}
	    }

	  if (stop)
	    {
	      if (stop < 0)
		error_at (loc, "failed read of mapper %qs: %m", name);
	      else if (is_server ())
		error_at (loc, "unexpected close from mapper %qs", name);
	      start = NULL;
	      return -1;
	    }

	  off--;
	}

      buffer[off] = 0;
      dump (dumper::MAPPER) && dump ("Mapper response:%s", buffer);
      end = buffer + off;
      pos = buffer;
    }

  for (;; pos = end + 1)
    {
      start = pos;
      end = NULL;
      if (*pos == '+')
	{
	  pos++;
	  end = strchr (pos, '\n');
	  if (end)
	    *end = 0;
	}

      if (!end)
	{
	  if (*pos == '-')
	    pos++;
	  end = pos + strlen (pos);
	  batching = false;
	}

      while (*pos && ISSPACE (*pos))
	pos++;

      if (*pos)
	return true;
      if (!batching)
	break;
    }

  return false;
}

void
module_mapper::response_unexpected (location_t loc)
{
  if (start)
    {
      /* Restore the whitespace we zapped tokenizing.  */
      for (char *ptr = start; ptr != pos; ptr++)
	if (!*ptr)
	  *ptr = ' ';
      error_at (loc, "mapper response malformed: %qs", start);
    }
  pos = end;
}

bool
module_mapper::response_eol (location_t loc, bool ignore)
{
  bool at_end = eol_p ();
  if (!at_end && !ignore)
    response_unexpected (loc);
  pos = end;
  return at_end;
}

char *
module_mapper::response_token (location_t loc, bool all)
{
  char *ptr = pos;

  if (ptr == end)
    {
      response_unexpected (loc);
      ptr = NULL;
    }
  else if (all)
    pos = end;
  else
    {
      char *eptr = ptr;
      while (eptr != end && !ISSPACE (*eptr))
	eptr++;

      if (eptr != end)
	{
	  *eptr++ = 0;
	  while (eptr != end && ISSPACE (*eptr))
	    eptr++;
	}
      pos = eptr;
    }

  return ptr;
}

int
module_mapper::response_word (location_t loc, const char *option, ...)
{
  if (const char *tok = response_token (loc))
    {
      va_list args;
      int count = 0;

      va_start (args, option);
      do
	{
	  if (!strcmp (option, tok))
	    {
	      va_end (args);
	      return count;
	    }
	  count++;
	  option = va_arg (args, const char *);
	}
      while (option);
      va_end (args);
      response_unexpected (loc);
    }
  return -1;
}

/*  Module mapper protocol non-canonical precis:

    HELLO version kind cookie
    	-> HELLO/ERROR response
    IMPORT module-name
    	-> OK bmipath
	-> ERROR
    EXPORT module-name
    	-> OK bmipath
    DONE module-name
    	No response
    RESET
        No response
 */

/* Start handshake.  */

bool
module_mapper::handshake (location_t loc, const char *cookie)
{
  send_command (loc, "HELLO %d GCC %s", MAPPER_VERSION, cookie);

  bool ok = get_response (loc) > 0;
  switch (response_word (loc, "HELLO", "ERROR", NULL))
    {
    default:
      ok = false;
      break;

    case 0: /* HELLO $ver $agent $repo */
      {
	const char *ver = response_token (loc);
	const char *agent = !eol_p () ? response_token (loc) : NULL;
	char *repo = !eol_p () ? response_token (loc, true) : NULL;

	if (ver)
	  dump () && dump ("Connected to mapper:%s version %s",
			   agent ? agent : "unknown", ver);
	if (response_eol (loc))
	  {
	    if (repo)
	      set_cmi_repo (repo);
	    ok = true;
	  }
      }
      break;

    case 1: /* ERROR $msg */
      error_at (loc, "mapper handshake failure: %s", response_error ());
      ok = false;
      break;
    }

  return ok;
}

/* IMPORT or EXPORT query.  */

void
module_mapper::imex_query (const module_state *state, bool exporting)
{
  send_command (state->from_loc, "%sPORT %s",
		exporting ? "EX" : "IM",
		state->get_flatname ());
}

/* Response to import/export query.  */

char *
module_mapper::cmi_response (const module_state *state)
{
  char *filename = NULL;

  switch (response_word (state->from_loc, "OK", "ERROR", NULL))
    {
    default:
      break;

    case 0: /* OK $bmifile  */
      filename = response_token (state->from_loc, true);
      filename = maybe_strip_cmi_prefix (filename);
      response_eol (state->from_loc);
      break;

    case 1: /* ERROR $msg */
      error_at (state->from_loc, "mapper cannot provide module %qs: %s",
		state->get_flatname (), response_error ());
      break;
    }

  return filename;
}

/* Import query.  */

char *
module_mapper::import_export (const module_state *state, bool export_p)
{
  module_mapper *mapper = get (state->from_loc);

  if (mapper->is_server ())
    {
      mapper->imex_query (state, export_p);
      return mapper->imex_response (state);
    }

  return NULL;
}

/* Export done.  */

bool
module_mapper::export_done (const module_state *state)
{
  bool ok = true;
  module_mapper *mapper = get (state->from_loc);
  
  if (mapper->is_server ())
    {
      dump (dumper::MAPPER) && dump ("Completed mapper");
      mapper->send_command (state->from_loc, "DONE %s",
			    state->get_flatname ());
    }
  else
    ok = mapper->is_live ();

  return ok;
}

/* Include translation.  Query if PATH should be turned into a header
   import.  Return false if it should remain a #include, true
   otherwise.  If READER is non-NULL, do the translation by pushing a
   buffer containing the translation text (ending in two \n's).  */

bool
module_mapper::translate_include (location_t loc, const char *path)
{
  bool xlate = false;

  if (mapper->is_server ())
    {
      send_command (loc, "INCLUDE %s", path);
      if (get_response (loc) <= 0)
	return false;

      switch (response_word (loc, "IMPORT", "TEXT", NULL))
	{
	default:
	  break;
	case 0:  /* Divert to import.  */
	  xlate = true;
	  break;
	case 1:  /* Treat as include.  */
	  break;
	}
      response_eol (loc);
    }
  else if (mapper->is_live ())
    {
      /* Sadly we intern ever include name.  Adjusting to not do this
	 will pessimize modul lookup from the parser.  */
      tree name = get_identifier (path);

      xlate = get_module_slot (name, NULL, false, false) != NULL;
    }

  return xlate;
}

/* If this is an alias, return the aliased module after transferring
   the exported flag.  Return the actual import in either case.  */

module_state *
module_state::resolve_alias ()
{
  module_state *result = this;
  if (is_alias ())
    {
      result = u1.alias;
      dump (dumper::MAPPER) && dump ("%M is an alias of %M", this, result);
    }
  return result;
}

/* If THIS is the current purview, issue an import error and return false.  */

bool
module_state::check_not_purview (location_t loc)
{
  module_state *imp = (*modules)[MODULE_PURVIEW];
  if (imp && !imp->name)
    imp = imp->parent;
  if (imp == this)
    {
      /* Cannot import the current module.  */
      error_at (loc, "cannot import module %qs in its own purview",
		get_flatname ());
      inform (from_loc, "module %qs declared here", get_flatname ());
      return false;
    }
  return true;
}

/* Module name substitutions.  */
static vec<module_state *,va_heap> substs;

void
module_state::mangle ()
{
  if (subst)
    mangle_substitution ('W', subst - 1);
  else
    {
      if (parent)
	parent->mangle ();
      if (!is_partition ())
	{
	  substs.safe_push (this);
	  subst = substs.length ();
	  mangle_identifier (name);
	}
    }
}

void
mangle_module (int mod)
{
  module_state *imp = (*modules)[mod];

  if (!imp->name)
    /* Set when importing the primary module interface.  */
    imp = imp->parent;

  imp->mangle ();
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
      fprintf (stderr, mod < MODULE_LIMIT ? " %s:%s:%u" : " %s:%s",
	       what, get_flatname (), mod);
      fflush (stderr);
    }
}

/* A human-readable README section.  The contents of this section to
   not contribute to the CRC, so the contents can change per
   compilation.  That allows us to embed CWD, hostname, build time and
   what not.  It is a STRTAB that may be extracted with:
     readelf -pgnu.c++.README $(module).gcm */

void
module_state::write_readme (elf_out *to, const char *options,
			    const cpp_hashnode *node)
{
  bytes_out readme (to);

  readme.begin (false);

  readme.printf ("GNU C++ %smodule%s%s",
		 is_header () ? "header " : is_partition () ? "" : "primary ",
		 is_header () ? ""
		 : is_interface () ? " interface" : " implementation",
		 is_partition () ? " partition" : "");

  /* Compiler's version.  */
  readme.printf ("compiler: %s", version_string);

  /* Module format version.  */
  verstr_t string;
  version2string (MODULE_VERSION, string);
  readme.printf ("version: %s", string);

  /* Module information.  */
  readme.printf ("module: %s", get_flatname ());
  readme.printf ("source: %s", main_input_filename);
  readme.printf ("options: %s", options);
  if (node)
    readme.printf ("macro: %s", NODE_NAME (node));

  /* The following fields could be expected to change between
     otherwise identical compilations.  Consider a distributed build
     system.  */
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
    time_t now = time (NULL);
    if (now != time_t (-1))
      {
	struct tm *time;

	time = gmtime (&now);
	readme.print_time ("build", time, "UTC");

#if defined (__USE_MISC) || defined (__USE_BSD) /* Is there a better way?  */
	time = localtime (&now);
	readme.print_time ("local", time, time->tm_zone);
#endif
      }
  }

  /* Its direct imports.  */
  for (unsigned ix = MODULE_IMPORT_BASE; ix < modules->length (); ix++)
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

  for (unsigned ix = MODULE_IMPORT_BASE; ix < modules->length (); ix++)
    {
      module_state *imp = (*modules)[ix];

      if (imp->remap >= MODULE_IMPORT_BASE && imp->is_direct () == direct)
	count++;
    }

  gcc_assert (!direct || count);

  sec.u (count);
  for (unsigned ix = MODULE_IMPORT_BASE; ix < modules->length (); ix++)
    {
      module_state *imp = (*modules)[ix];

      if (imp->remap >= MODULE_IMPORT_BASE && imp->is_direct () == direct)
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
	      write_location (sec, imp->from_loc);
	      sec.str (imp->filename);
	      sec.u (imp->exported_p);
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
      if (ix >= slurp ()->remap->length ()
	  || ix < MODULE_IMPORT_BASE || (*slurp ()->remap)[ix])
	{
	  sec.set_overrun ();
	  break;
	}

      const char *name = sec.str (NULL);
      module_state *imp = get_module (name);
      unsigned crc = sec.u32 ();
      bool exported = false;

      /* If the import is a partition, it must be the same primary
	 module as this TU.  */
      if (imp && imp->is_partition () &&
	  (!named_module_p ()
	   || (get_primary ((*modules)[MODULE_PURVIEW]) != get_primary (imp))))
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
	  exported = sec.u ();

	  if (sec.get_overrun ())
	    break;

	  if (!imp->check_not_purview (loc))
	    continue;

	  if (imp->is_detached ())
	    imp->attach (floc);

	  if (!imp->is_imported ())
	    {
	      unsigned n = dump.push (imp);
	      imp->maybe_create_loc ();
	      imp->crc = crc;

	      if (imp->filename)
		fname = NULL;
	      else if (!fname[0])
		fname = module_mapper::import_export (imp, false);

	      if (imp->mod == MODULE_NONE)
		{
		  /* Must import the partition now, as inter-module
		     references from the partition we must be in
		     require it.  The deduping machinery better be
		     working ...  */
		  dump () && dump ("Importing elided partition %M", imp);
		  gcc_assert (imp->is_partition () && is_partition ());
		  imp->mod = MODULE_UNKNOWN;
		}

	      if (!imp->do_import (fname, reader))
		imp = NULL;
	      dump.pop (n);
	      if (!imp)
		continue;
	    }

	  if (is_partition () && !imp->is_partition ())
	    imp->from_partition_p = true;
	}
      else
	{
	  /* An indirect import, find it, it should already be here.  */
	  if (imp->is_detached ())
	    {
	      error_at (loc, "indirect import %qs is not already loaded", name);
	      continue;
	    }
	}

      if (imp->crc != crc)
	error_at (loc, "import %qs has CRC mismatch", imp->get_flatname ());

      imp = imp->resolve_alias ();

      (*slurp ()->remap)[ix] = imp->mod;
      if (lmaps)
	set_import (imp, exported);
      dump () && dump ("Found %simport:%u %M->%u", !lmaps ? "indirect "
		       : exported ? "exported " : "", ix, imp,
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

  dump () && dump ("Reading %u imports",
		   slurp ()->remap->length () - MODULE_IMPORT_BASE);
  dump.indent ();

  /* Read the imports.  */
  unsigned direct = read_imports (sec, reader, lmaps);
  unsigned indirect = read_imports (sec, NULL, NULL);
  if (direct + indirect + MODULE_IMPORT_BASE != slurp ()->remap->length ())
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

  for (unsigned ix = MODULE_IMPORT_BASE; ix != modules->length (); ix++)
    {
      module_state *imp = (*modules)[ix];
      if (imp->is_partition ())
	{
	  dump () && dump ("Writing elided partition %M (crc=%x)",
			   imp, imp->crc);
	  sec.str (imp->get_flatname ());
	  sec.u32 (imp->crc);
	  write_location (sec,
			  imp->is_direct () ? imp->from_loc : UNKNOWN_LOCATION);
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
      if (!imp || !imp->is_partition () || !imp->is_detached ()
	  || get_primary (imp) != this)
	{
	  sec.set_overrun ();
	  break;
	}

      /* Attach the partition without loading it.  We'll have to load
	 for real if it's indirectly imported.  */
      imp->attach (floc);
      imp->crc = crc;
      imp->mod = MODULE_NONE; /* Mark as wierd.   */
      if (!imp->filename && fname[0])
	imp->filename = xstrdup (fname);
    }

  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* Contents of a cluster.  */
enum cluster_tag {
  ct_decl,	/* A decl.  */
  ct_defn,	/* A definition.  */
  ct_bind,	/* A binding.  */
  ct_horcrux,	/* Preseed reference to unnamed decl.  */
  ct_hwm
};

/* Declaration modifiers.  */
enum ct_decl_flags 
{
  cdf_is_voldemort = 0x1,	/* Is a voldemort.  */
  cdf_is_specialization = 0x2,  /* Some kind of specialization.  */
  cdf_is_partial = 0x4,		/* A partial specialization.  */
};

/* Binding modifiers.  */
enum ct_bind_flags
{
  cbf_export = 0x1,	/* An exported decl.  */
  cbf_using = 0x2,	/* A using decl.  */
  cbf_hidden = 0x4,	/* A hidden (friend) decl.  */
};


static unsigned
sort_mergeables (depset *scc[], unsigned size)
{
  depset::hash table (size, true);

  dump.indent ();

  unsigned binding_lwm = size;
  bool refs_unnamed = false;
  for (unsigned ix = size; ix--;)
    {
      depset *dep = scc[ix];
      if (dep->is_binding ())
	{
	  /* Move bindings to the end.  */
	  if (--binding_lwm != ix)
	    {
	      scc[ix] = scc[binding_lwm];
	      scc[binding_lwm] = dep;
	    }
	}
      else
	{
	  table.add_mergeable (dep);
	  if (dep->refs_unnamed () && dep->is_mergeable ())
	    refs_unnamed = true;
	}
    }
  dump (dumper::MERGE) && dump ("Ordering %u depsets", binding_lwm);
  if (refs_unnamed)
    /* Insert horcrux markers for the unnameable decls outside of the
       mergeable set.  */
    for (unsigned ix = binding_lwm; ix--;)
      {
	depset *dep = scc[ix];
	if (dep->refs_unnamed () && dep->is_mergeable ())
	  {
	    for (unsigned jx = dep->is_marked ();
		 jx != dep->deps.length (); jx++)
	      {
		depset *d = dep->deps[jx];
		if ((d->get_entity_kind () == depset::EK_UNNAMED
		     || d->get_entity_kind () == depset::EK_SPECIALIZATION
		     || (d->get_entity_kind () == depset::EK_DECL
			 && d->is_pseudo_spec ()))
		    /* And not in this cluster. */
		    && d->cluster)
		  table.add_mergeable_horcrux (d);
	      }
	  }
      }

  table.find_dependencies ();

  vec<depset *> order = table.connect ();

  unsigned cluster = 0;
  unsigned pos = 0;
  unsigned count = 0;
  for (unsigned ix = 0; ix != order.length (); ix++)
    if (order[ix]->is_marked ())
      {
	gcc_checking_assert (pos != binding_lwm);
	depset *dep = order[ix]->deps[0];
	scc[pos++] = dep;

	/* Each entity must be its own cluster.  */
	gcc_assert (order[ix]->cluster != cluster);
	cluster = order[ix]->cluster;
	if (dep->is_mergeable ())
	  {
	    count++;
	    dump (dumper::MERGE) && dump ("Mergeable %u is %N",
					  ix, dep->get_entity ());
	  }
      }
  gcc_checking_assert (pos == binding_lwm);

  order.release ();
  dump (dumper::MERGE) && dump ("Ordered %u mergeables", count);
  dump.outdent ();

  return count;
}

/* Write the cluster of depsets in SCC[0-SIZE).  These are ordered
   defns < decls < bindings.  Returns number of non-implicit template
   specializations. */

void
module_state::write_cluster (elf_out *to, depset *scc[], unsigned size,
			     depset::hash &table, unsigned &unnamed,
			     unsigned *crc_ptr)
{
  dump () && dump ("Writing section:%u %u depsets", scc[0]->section, size);
  dump.indent ();

  unsigned incoming_unnamed = unnamed;
  bool refs_unnamed_p = false;

  /* Sort the cluster according to its mergeable entities.  */
  unsigned mergeables = sort_mergeables (scc, size);

  if (dump (dumper::CLUSTER))
    {
      dump ("Cluster members:");
      dump.indent ();
      for (unsigned ix = 0; ix != size; ix++)
	{
	  depset *dep = scc[ix];
	  tree decl = dep->get_entity ();

	  if (dep->is_binding ())
	    dump ("[%u]=%s %P", ix, dep->entity_kind_name (),
		  decl, dep->get_name ());
	  else
	    {
	      gcc_checking_assert (dep->get_entity_kind ()
				   != depset::EK_REDIRECT);
	      dump ("[%u]=%s %s %N", ix, dep->entity_kind_name (),
		    dep->has_defn () ? "definition" : "declaration",
		    dep->get_entity ());
	    }
	}
      dump.outdent ();
    }

  /* Determine horcrux numbers for unnamed decls.   */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];

      if (b->is_binding ())
	continue;
      
      gcc_checking_assert (!b->is_unreached ());
      tree decl = b->get_entity ();

      if (b->refs_unnamed ())
	refs_unnamed_p = true;

      if (b->get_entity_kind () == depset::EK_UNNAMED
	  // FIXME: Should friend specializations be voldemorty?
	  || b->get_entity_kind () == depset::EK_SPECIALIZATION)
	{
	  /* There is no binding for this decl.  It is therefore not
	     findable by name.  Determine its horcrux number.  */
	  dump () && dump ("Unnamed %u %N", unnamed, decl);
	  b->cluster = ++unnamed;
	}
    }

  trees_out sec (to, this, table, scc[0]->section);
  sec.begin ();

  sec.u (mergeables);

  if (refs_unnamed_p)
    /* We contain references to unnamed decls.  Seed those that are
       imported or in earlier clusters (others will be within this
       cluster).  */
    for (unsigned ix = 0; ix != size; ix++)
      if (!scc[ix]->is_binding () && scc[ix]->refs_unnamed ())
	{
	  depset *b = scc[ix];

	  for (unsigned jx = b->is_marked (); jx != b->deps.length (); jx++)
	    {
	      depset *d = b->deps[jx];
	      if ((d->get_entity_kind () == depset::EK_UNNAMED
		   || d->get_entity_kind () == depset::EK_SPECIALIZATION)
		  && d->cluster <= incoming_unnamed)
		{
		  tree u_decl = d->get_entity ();
		  if (!TREE_VISITED (u_decl))
		    {
		      bool is_imported = d->is_import ();
		      gcc_checking_assert (is_imported == !d->cluster);
		      sec.u (ct_horcrux);
		      unsigned owner = 0;
		      unsigned index = d->cluster - 1;
		      if (is_imported)
			{
			  tree o_decl = get_module_owner (u_decl);
			  owner = MAYBE_DECL_MODULE_OWNER (o_decl);
			  module_state *import = (*modules)[owner];
			  /* It must be in the unnamed map.  */
			  index = *unnamed_map->get (DECL_UID (u_decl));
			  index -= import->unnamed_lwm;
			  gcc_checking_assert (index < import->unnamed_num);
			  owner = import->remap;
			}
		      sec.u (owner);
		      sec.u (index);
		      unsigned tag = sec.insert (u_decl);
		      dump () && dump ("Inserted:%d horcrux:%u@%u for %N",
				       tag, index, owner, u_decl);

		      tree proxy = u_decl;
		      if (TREE_CODE (proxy) == TEMPLATE_DECL)
			{
			  proxy = DECL_TEMPLATE_RESULT (proxy);
			  tag = sec.insert (proxy);
			  dump ()
			    && dump ("Inserted:%d template result %C:%N",
				     tag, TREE_CODE (proxy), proxy);
			}
		      if (TREE_CODE (proxy) == TYPE_DECL
			  && (DECL_ORIGINAL_TYPE (proxy)
			      || TYPE_STUB_DECL (TREE_TYPE (proxy)) == proxy))
			{
			  proxy = TREE_TYPE (proxy);
			  tag = sec.insert (proxy);
			  dump () && dump ("Inserted:%d type %C:%N",
					   tag, TREE_CODE (proxy), proxy);
			}
		    }
		}
	      else
		/* All imported entities will have zero cluster.  */
		gcc_checking_assert (d->is_binding () ||
				     !d->is_import ());
	    }
	}

  /* Mark members for walking.  */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];

      if (b->is_binding ())
	for (unsigned jx = b->deps.length (); jx--;)
	  {
	    depset *dep = b->deps[jx];
	    gcc_checking_assert (dep->get_entity_kind () == depset::EK_USING
				 || TREE_VISITED (dep->get_entity ()));
	  }
      else if (b->get_entity_kind () != depset::EK_USING)
	{
	  tree decl = b->get_entity ();

	  sec.mark_declaration (decl, b->has_defn ());
	  if (b->is_mergeable ())
	    // FIXME: Perhaps tree_value should interrogate the depset?
	    sec.mark_mergeable (decl, b->get_entity_kind () == depset::EK_CLONE
				? trees_out::tag_clone
				: trees_out::tag_mergeable);
	}
    }

  depset *namer = NULL;

  /* Every declaration.  */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];
      if (b->is_binding ())
	continue;

      tree decl = b->get_entity ();
      dump () && dump ("Depset:%u %s %C:%N", ix, b->entity_kind_name (),
		       TREE_CODE (decl), decl);

      unsigned flags = 0;
      switch (b->get_entity_kind ())
	{
	case depset::EK_SPECIALIZATION:
	  flags |= cdf_is_specialization;
	  if (b->is_partial ())
	    flags |= cdf_is_partial;
	  /* FALLTHROUGH.  */

	case depset::EK_DECL:
	case depset::EK_UNNAMED:
	case depset::EK_CLONE:
	  sec.u (ct_decl);
	  sec.tree_ctx (decl, false, NULL_TREE);
	  dump () && dump ("Wrote declaration of %N", decl);

	  if (b->cluster)
	    flags |= cdf_is_voldemort;
	  sec.u (flags);

	  if (flags & cdf_is_voldemort)
	    {
	      dump () && dump ("Voldemort:%u %N", b->cluster - 1, decl);
	      sec.u (b->cluster - 1);
	    }

	  /* Is this a good enough human name?  */
	  if (b->get_entity_kind () != depset::EK_UNNAMED)
	    if (!namer)
	      namer = b;
	  break;

	default:;
	}
    }

  /* Now every definition and the bindings.  Bindings have been
     sorted last.  This is an assumption of the lazy specialization
     machinery.  */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];
      tree decl = b->get_entity ();
      switch (b->get_entity_kind ())
	{
	case depset::EK_BINDING:
	  {
	    gcc_assert (TREE_CODE (decl) == NAMESPACE_DECL);
	    dump () && dump ("Depset:%u binding %C:%P", ix, TREE_CODE (decl),
			     decl, b->get_name ());
	    sec.u (ct_bind);
	    sec.tree_ctx (decl, false, decl);
	    sec.tree_node (b->get_name ());

	    /* Write in reverse order, so reading will see the exports
	       first, thus building the overload chain will be
	       optimized.  */
	    for (unsigned jx = b->deps.length (); jx--;)
	      {
		depset *dep = b->deps[jx];
		tree decl = dep->get_entity ();
		unsigned flags = 0;
		if (dep->get_entity_kind () == depset::EK_USING)
		  {
		    flags |= cbf_using;
		    if (OVL_EXPORT_P (decl))
		      flags |= cbf_export;
		    decl = OVL_FUNCTION (decl);
		  }
		else
		  {
		    /* An implicit typedef must be at zero.  */
		    gcc_assert (!DECL_IMPLICIT_TYPEDEF_P (decl) || !jx);
		    if (dep->is_hidden ())
		      flags |= cbf_hidden;
		    else if (DECL_MODULE_EXPORT_P (decl))
		      flags |= cbf_export;
		  }

		gcc_checking_assert (DECL_P (decl));

		sec.i (flags);
		sec.tree_node (decl);
	      }

	    /* Terminate the list.  */
	    sec.i (-1);
	  }
	  break;

	case depset::EK_SPECIALIZATION:
	case depset::EK_DECL:
	case depset::EK_UNNAMED:
	case depset::EK_CLONE:
	  if (b->has_defn ())
	    {
	      tree decl = b->get_entity ();
	      sec.u (ct_defn);
	      sec.tree_node (decl);
	      dump () && dump ("Writing definition of %N", decl);
	      sec.write_definition (decl);

	      /* Is this a good enough human name?  */
	      if (b->get_entity_kind () != depset::EK_UNNAMED)
		if (!namer || !namer->has_defn ())
		  namer = b;
	    }
	  break;

	default:;
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

  unsigned snum = sec.end (to, name, crc_ptr);

  for (unsigned ix = size; ix--;)
    gcc_checking_assert (scc[ix]->section == snum);

  dump.outdent ();
  dump () && dump ("Wrote section:%u named-by:%N", scc[0]->section, naming_decl);
}

// FIXME: when DECL didn't go via match_mergeable_specialization we'll
// need to add it to the hash table too.  */

static void
install_specialization (tree decl, bool is_partial)
{
  // FIXME: see note about most_general_template.  Perhaps we should
  // be explicitly writing it?
  int use;
  tree args = NULL_TREE;
  tree tpl = decl;
  while (tree ti = node_template_info (tpl, use))
    {
      if (!args)
	args = TI_ARGS (ti);
      tpl = TI_TEMPLATE (ti);
    }

  if (is_partial)
    {
      /* Add onto DECL_TEMPLATE_SPECIALIZATIONS.  */
      tree specs = tree_cons (args, decl, DECL_TEMPLATE_SPECIALIZATIONS (tpl));
      TREE_TYPE (specs) = TREE_TYPE (DECL_TEMPLATE_RESULT (decl));
      DECL_TEMPLATE_SPECIALIZATIONS (tpl) = specs;
      dump (dumper::MERGE) &&
	dump ("Adding partial specialization %N to %N", decl,
	      TREE_TYPE (DECL_TEMPLATE_RESULT (tpl)));
    }

  /* Add onto DECL_TEMPLATE_INSTANTIATIONS.  */
  // FIXME:do it
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

  unsigned mergeables = sec.u ();
  dump (dumper::MERGE) && dump ("%d mergeable entities", mergeables);
  if (mergeables)
    sec.reserve_mergeable (mergeables);

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
	    bool dedup = (TREE_PUBLIC (ns)
			  && (is_primary ()
			      || is_partition ()
			      || is_header ()));

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

		if (decls && TREE_CODE (decl) == TYPE_DECL)
		  {
		    /* Stat hack.  */
		    if (type || !DECL_IMPLICIT_TYPEDEF_P (decl))
		      sec.set_overrun ();
		    type = decl;
		  }
		else
		  {
		    if (decls
			|| (flags & (cbf_hidden | cbf_using))
			|| (dedup && TREE_CODE (decl) == FUNCTION_DECL)
			|| DECL_FUNCTION_TEMPLATE_P (decl))
		      {
			decls = ovl_make (decl, decls);
			if (flags & cbf_using)
			  {
			    OVL_USING_P (decls) = dedup = true;
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

		    if (flags & cbf_export)
		      visible = decls;
		  }
	      }

	    if (!decls)
	      sec.set_overrun ();

	    if (sec.get_overrun ())
	      break; /* Bail.  */

	    if (is_primary () || is_partition ())
	      visible = decls;

	    dump () && dump ("Binding of %P", ns, name);
	    if (!set_module_binding (ns, name, mod,
				     is_primary () || is_partition (),
				     decls, type, visible))
	      sec.set_overrun ();

	    if (type && !sec.is_existing_mergeable (type))
	      add_module_decl (ns, name, type);

	    for (ovl_iterator iter (decls); iter; ++iter)
	      if (!iter.using_p ())
		{
		  tree decl = *iter;
		  if (!sec.is_existing_mergeable (decl))
		    add_module_decl (ns, name, decl);
		}
	  }
	  break;

	case ct_horcrux:
	  /* Resurrect a node from a horcrux.  */
	  {
	    unsigned owner = sec.u ();
	    unsigned index = sec.u ();
	    module_state *import = this;

	    if (owner)
	      {
		owner = slurp ()->remap_module (owner);
		if (!owner)
		  goto bad_tom_riddle;
		import = (*modules)[owner];
	      }

	    if (index < import->unnamed_num)
	      {
		unnamed_entity *uent
		  = &(*unnamed_ary)[import->unnamed_lwm + index];

		if (uent->slot.is_lazy ())
		  import->lazy_load (NULL, NULL, &uent->slot, false);

		if (tree decl = uent->slot)
		  {
		    int tag = sec.insert (decl);
		    dump () && dump ("Inserted:%d horcrux:%u@%u %C:%N", tag,
				     index, owner, TREE_CODE (decl), decl);
		    tree proxy = decl;
		    if (TREE_CODE (proxy) == TEMPLATE_DECL)
		      {
			proxy = DECL_TEMPLATE_RESULT (proxy);
			tag = sec.insert (proxy);
			dump ()
			  && dump ("Inserted:%d template result %C:%N",
				   tag, TREE_CODE (proxy), proxy);
		      }
		    if (TREE_CODE (proxy) == TYPE_DECL
			&& (DECL_ORIGINAL_TYPE (proxy)
			    || TYPE_STUB_DECL (TREE_TYPE (proxy)) == proxy))
		      {
			proxy = TREE_TYPE (proxy);
			tag = sec.insert (proxy);
			dump () && dump ("Inserted:%d type %C:%N",
					 tag, TREE_CODE (proxy), proxy);
		      }
		  }
	      }
	    else
	      {
	      bad_tom_riddle:
		sec.set_overrun ();
	      }
	  }
	  break;

	case ct_decl:
	  /* A decl.  */
	  {
	    tree decl = sec.tree_node ();
	    dump () && dump ("Read declaration of %N", decl);

	    unsigned flags = sec.u ();
	    if (sec.get_overrun ())
	      break;

	    if (flags & cdf_is_voldemort)
	      {
		/* An unnamed node, register it.  */
		unsigned unnamed = sec.u ();
		if (decl && unnamed < unnamed_num)
		  {
		    unsigned index = unnamed_lwm + unnamed;
		    unnamed_entity *uent = &(*unnamed_ary)[index];
		    uent->slot = decl;
		    bool present = unnamed_map->put (DECL_UID (decl), index);
		    gcc_checking_assert (!present);
		    dump () && dump ("Voldemort decl:%u [%u] %N",
				     unnamed, index, decl);
		  }
		else
		  sec.set_overrun ();
	      }

	    if (flags & cdf_is_specialization)
	      install_specialization (decl, flags & cdf_is_partial);
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
     parsing or instantiating a function.  Save it away.  */
  tree old_cfd = current_function_decl;
  struct function *old_cfun = cfun;
  while (tree decl = sec.post_process ())
    {
      bool abstract = TREE_CODE (decl) == TEMPLATE_DECL;
      if (abstract)
	decl = DECL_TEMPLATE_RESULT (decl);

      current_function_decl = decl;
      allocate_struct_function (decl, abstract);
      cfun->language = ggc_cleared_alloc<language_function> ();
      cfun->language->base.x_stmt_tree.stmts_are_full_exprs_p = 1;

      if (!abstract)
	{
	  if (DECL_COMDAT (decl))
	    comdat_linkage (decl);
	  note_vague_linkage_fn (decl);
	  cgraph_node::finalize_function (decl, true);
	}
    }
  set_cfun (old_cfun);
  current_function_decl = old_cfd;

  dump.outdent ();
  dump () && dump ("Read section:%u", snum);

  if (!sec.end (from ()))
    return false;

  return true;
}

/* SPACES is a sorted vector of namespaces.  Write out the namespaces
   to MOD_SNAME_PFX.nms section.

   Each namespace is:
     u:name,
     u:context, number of containing namespace (0 == ::)
     u:inline_p/export_p/public_p  */

void
module_state::write_namespaces (elf_out *to, depset::hash &table,
				auto_vec<depset *> &spaces,
				unsigned *crc_p)
{
  dump () && dump ("Writing namespaces");
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  for (unsigned ix = 0; ix != spaces.length (); ix++)
    {
      depset *b = spaces[ix];
      tree ns = b->get_entity ();

      gcc_checking_assert (TREE_CODE (ns) == NAMESPACE_DECL);

      b->section = ix + 1;
      unsigned ctx_num = 0;
      tree ctx = CP_DECL_CONTEXT (ns);
      if (ctx != global_namespace)
	ctx_num = table.find_entity (ctx)->section;
      bool export_p = DECL_MODULE_EXPORT_P (ns);
      bool inline_p = DECL_NAMESPACE_INLINE_P (ns);
      bool public_p = TREE_PUBLIC (ns);

      /* We should only be naming public namespaces, or our own
	 private ones.  */
      gcc_checking_assert (public_p
			   || DECL_MODULE_OWNER (ns) == MODULE_PURVIEW);
      unsigned flags = 0;
      if (export_p)
	flags |= 1;
      if (inline_p)
	flags |= 2;
      if (public_p)
	flags |= 4;
      dump () && dump ("Writing namespace %u %N%s%s%s, parent:%u",
		       b->section, ns, export_p ? ", export" : "",
		       public_p ? ", public" : "",
		       inline_p ? ", inline" : "", ctx_num);

      sec.u (to->name (DECL_NAME (ns)));
      if (!DECL_NAME (ns))
	{
	  gcc_checking_assert (DECL_ASSEMBLER_NAME_SET_P (ns));
	  sec.u (to->name (DECL_ASSEMBLER_NAME_RAW (ns)));
	}
      sec.u (ctx_num);
      /* Don't use bools, because this can be near the end of the
	 section, and it won't save anything anyway.  */
      sec.u (flags);
      write_location (sec, DECL_MODULE_OWNER (ns) == MODULE_PURVIEW
		      ? DECL_SOURCE_LOCATION (ns) : UNKNOWN_LOCATION);
    }

  sec.end (to, to->name (MOD_SNAME_PFX ".nms"), crc_p);
  dump.outdent ();
}

/* Read the namespace hierarchy from MOD_SNAME_PFX.namespace.  Fill in
   SPACES from that data.  */

bool
module_state::read_namespaces (auto_vec<tree> &spaces)
{
  bytes_in sec;

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".nms"))
    return false;

  dump () && dump ("Reading namespaces");
  dump.indent ();

  spaces.safe_push (global_namespace);
  while (sec.more_p ())
    {
      unsigned name = sec.u ();
      unsigned anon_name = name ? 0 : sec.u ();
      unsigned parent = sec.u ();
      /* See comment in write_namespace about why not bits.  */
      unsigned flags = sec.u ();
      location_t src_loc = read_location (sec);

      if (parent >= spaces.length ())
	sec.set_overrun ();
      if (sec.get_overrun ())
	break;

      tree id = name ? get_identifier (from ()->name (name)) : NULL_TREE;
      tree anon_id = anon_name
	? get_identifier (from ()->name (anon_name)) : NULL_TREE;
      bool public_p = flags & 4;
      bool inline_p = flags & 2;
      bool export_p = flags & 1;

      dump () && dump ("Read namespace %P%s%s%s, %u",
		       spaces[parent], id, export_p ? ", export" : "",
		       public_p ? ", public" : "",
		       inline_p ? ", inline" : "", spaces.length ());
      bool visible_p = (export_p
			|| (public_p && (is_partition () || is_primary ())));
      tree inner = add_imported_namespace (spaces[parent], id, mod,
					   src_loc, visible_p, inline_p,
					   anon_id);
      if (export_p && is_partition ())
	DECL_MODULE_EXPORT_P (inner) = true;

      spaces.safe_push (inner);
    }
  dump.outdent ();
  if (!sec.end (from ()))
    return false;

  return true;
}

/* Write the binding TABLE to MOD_SNAME_PFX.bind

   Each binding is:
     u:name
     u:context - number of containing namespace
     u:section - section number of binding. */

unsigned
module_state::write_bindings (elf_out *to, vec<depset *> sccs,
			      depset::hash &table, unsigned *crc_p)
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
	  unsigned ns_num = 0;
	  tree ns = b->get_entity ();
	  if (ns != global_namespace)
	    ns_num = table.find_entity (ns)->section;
	  dump () && dump ("Bindings %P section:%u", ns, b->get_name (),
			   b->section);
	  sec.u (to->name (b->get_name ()));
	  sec.u (ns_num);
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
module_state::read_bindings (auto_vec<tree> &spaces, unsigned num,
			     const range_t &range)
{
  bytes_in sec;

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".bnd"))
    return false;

  dump () && dump ("Reading binding table");
  dump.indent ();
  for (; !sec.get_overrun () && num--;)
    {
      const char *name = from ()->name (sec.u ());
      unsigned nsnum = sec.u ();
      unsigned snum = sec.u ();

      if (nsnum >= spaces.length () || !name
	  || snum < range.first || snum >= range.second)
	sec.set_overrun ();
      if (!sec.get_overrun ())
	{
	  tree ctx = spaces[nsnum];
	  tree id = get_identifier (name);
	  dump () && dump ("Bindings %P section:%u", ctx, id, snum);
	  if (mod >= MODULE_IMPORT_BASE
	      && !import_module_binding (ctx, id, mod, snum))
	    break;
	}
    }

  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* Write the unnamed table to MOD_SNAME_PFX.vdm

   Each entry is a section number, key-locator tuple.
   Keys are so specialiazations can be loaded when instantiating the
   thing they're keyed to.  */

void
module_state::write_unnamed (elf_out *to, vec<depset *> depsets,
			     depset::hash &table,
			     unsigned count, unsigned *crc_p)
{
  dump () && dump ("Writing unnamed");
  dump.indent ();

  trees_out sec (to, this, table);
  sec.begin ();

  unsigned current = 0;
  for (unsigned ix = 0; ix < depsets.length (); ix++)
    {
      depset *d = depsets[ix];

      if (d->cluster)
	{
	  tree uent = d->get_entity ();
	  dump () && dump ("Unnamed %d %N section:%u",
			   current, uent, d->section);
	  current++;

	  gcc_checking_assert (!d->is_unreached ()
			       && d->cluster == current);
	  sec.u (d->section);

	  if (d->get_entity_kind () == depset::EK_SPECIALIZATION)
	    {
	      tree key = get_module_owner (uent);
	      unsigned owner = MAYBE_DECL_MODULE_OWNER (key);
	      unsigned import_kind = MODULE_IMPORT_BASE;
	      if (owner)
		{
		  module_state *import = (*modules)[owner];

		  if (import->is_header ())
		    import_kind = MODULE_NONE;
		  else if (import->is_partition ())
		    import_kind = MODULE_PURVIEW;
		}

	      tree ctx = CP_DECL_CONTEXT (key);
	      gcc_checking_assert (TREE_CODE (ctx) == NAMESPACE_DECL);
	      sec.tree_ctx (ctx, true, NULL);
	      sec.tree_node (DECL_NAME (key));
	      sec.u (import_kind);
	      dump () && dump ("Specialization %N section:%u keyed to %N (%u)",
			       uent, d->section, key, import_kind);
	    }
	  else
	    sec.tree_node (NULL);
	}
      }
  gcc_assert (count == current);
  sec.end (to, to->name (MOD_SNAME_PFX ".vld"), crc_p);
  dump.outdent ();
}

bool
module_state::read_unnamed (unsigned count, const range_t &range)
{
  trees_in sec (this);

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".vld"))
    return false;

  dump () && dump ("Reading unnamed");
  dump.indent ();

  vec_safe_reserve (unnamed_ary, count);
  unsigned ix;
  for (ix = 0; ix != count; ix++)
    {
      unnamed_entity *uent = unnamed_ary->quick_push (unnamed_entity ());
      unsigned snum = sec.u ();

      if (snum < range.first || snum >= range.second)
	sec.set_overrun ();
      if (sec.get_overrun ())
	break;

      dump () && dump ("Unnamed %u(%u) section:%u", ix, ix + unnamed_lwm, snum);
      uent->slot.set_lazy (snum);

      uent->ns = sec.tree_node ();
      if (uent->ns)
	{
	  uent->id = sec.tree_node ();
	  unsigned import_kind = sec.u ();

	  /* It's now a regular import kind, if it's not part of the
	     same module.  */
	  if (import_kind == MODULE_PURVIEW
	      && !(is_primary () || is_partition ()))
	    import_kind = MODULE_IMPORT_BASE;
	  dump () && dump ("Specialization key %P (%u) section:%u",
			   uent->ns, uent->id, import_kind, snum);
	  if (specset::table->add (uent->ns, uent->id, ix + unnamed_lwm))
	    if (!note_pending_specializations (uent->ns, uent->id, import_kind))
	      sec.set_overrun ();
	}
    }
  unnamed_num = ix;

  dump.outdent ();
  if (!sec.end (from ()))
    return false;
  return true;
}

/* Read & write locations.  */
enum loc_kind {
  LK_ORDINARY,
  LK_MACRO,
  LK_ADHOC,
  LK_IMPORT_ORDINARY,
  LK_IMPORT_MACRO
};

static const module_state *
module_for_ordinary_loc (location_t loc)
{
  unsigned pos = MODULE_IMPORT_BASE;
  unsigned len = modules->length () - pos;

  while (len)
    {
      unsigned half = len / 2;
      module_state *probe = (*modules)[pos + half];
      if (loc < probe->ordinary_locs.first)
	len = half;
      else if (loc < probe->ordinary_locs.second)
	return probe;
      else
	{
	  pos += half + 1;
	  len = len - (half + 1);
	}
    }

  return NULL;
}

static const module_state *
module_for_macro_loc (location_t loc)
{
  unsigned pos = MODULE_IMPORT_BASE;
  unsigned len = modules->length () - pos;

  while (len)
    {
      unsigned half = len / 2;
      module_state *probe = (*modules)[pos + half];
      if (loc >= probe->macro_locs.second)
	len = half;
      else if (loc >= probe->macro_locs.first)
	return probe;
      else
	{
	  pos += half + 1;
	  len = len - (half + 1);
	}
    }

  return NULL;
}

void
module_state::write_location (bytes_out &sec, location_t loc)
{
  if (IS_ADHOC_LOC (loc))
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
    }
  else if (IS_MACRO_LOC (loc))
    {
      if (const loc_spans::span *span = spans.macro (loc))
	{
	  unsigned off = MAX_LOCATION_T - loc;

	  off -= span->macro_delta;

	  sec.u (LK_MACRO);
	  sec.u (off);
	  dump (dumper::LOCATION)
	    && dump ("Macro location %u output %u", loc, off);
	}
      else if (const module_state *import = module_for_macro_loc (loc))
	{
	  unsigned off = import->macro_locs.second - loc - 1;
	  sec.u (LK_IMPORT_MACRO);
	  sec.u (import->remap);
	  sec.u (off);
	  dump (dumper::LOCATION)
	    && dump ("Imported macro location %u output %u:%u",
		     loc, import->remap, off);
	}
      else
	gcc_unreachable ();
    }
  else if (IS_ORDINARY_LOC (loc))
    {
      if (const loc_spans::span *span = spans.ordinary (loc))
	{
	  unsigned off = loc;

	  off += span->ordinary_delta;
	  sec.u (LK_ORDINARY);
	  sec.u (off);

	  dump (dumper::LOCATION)
	    && dump ("Ordinary location %u output %u", loc, off);
	}
      else if (const module_state *import = module_for_ordinary_loc (loc))
	{
	  unsigned off = loc - import->ordinary_locs.first;
	  sec.u (LK_IMPORT_ORDINARY);
	  sec.u (import->remap);
	  sec.u (off);
	  dump (dumper::LOCATION)
	    && dump ("Imported ordinary location %u output %u:%u",
		     import->remap, import->remap, off);
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
    case LK_ADHOC:
      {
	dump (dumper::LOCATION) && dump ("Adhoc location");
	locus = read_location (sec);
	source_range range;
	range.m_start = read_location (sec);
	if (range.m_start == UNKNOWN_LOCATION)
	  range.m_start = locus;
	range.m_finish = read_location (sec);
	if (locus != loc && range.m_start != loc && range.m_finish != loc)
	  locus = get_combined_adhoc_loc (line_table, locus, range, NULL);
      }
      break;

    case LK_MACRO:
      {
	unsigned off = sec.u ();
	location_t adjusted = MAX_LOCATION_T - off;

	adjusted -= slurp ()->loc_deltas.second;
	if (adjusted < macro_locs.first)
	  sec.set_overrun ();
	else if (adjusted < macro_locs.second)
	  locus = adjusted;
	else
	  sec.set_overrun ();
	dump (dumper::LOCATION)
	  && dump ("Macro %u becoming %u", off, locus);
      }
      break;

    case LK_ORDINARY:
      {
	unsigned off = sec.u ();
	location_t adjusted = off;

	adjusted += slurp ()->loc_deltas.first;
	if (adjusted >= ordinary_locs.second)
	  sec.set_overrun ();
	else if (adjusted >= ordinary_locs.first)
	  locus = adjusted;
	else if (adjusted < spans.main_start ())
	  locus = off;

	dump (dumper::LOCATION)
	  && dump ("Ordinary location %u becoming %u", off, locus);
      }
      break;

     case LK_IMPORT_MACRO:
     case LK_IMPORT_ORDINARY:
       {
	 unsigned mod = slurp ()->remap_module (sec.u ());
	 unsigned off = sec.u ();

	 if (mod < MODULE_IMPORT_BASE)
	   sec.set_overrun ();
	 else
	   {
	     const module_state *import = (*modules)[mod];
	     if (kind == LK_IMPORT_MACRO)
	       {
		 if (off < import->macro_locs.second - macro_locs.first)
		   locus = import->macro_locs.second - off - 1;
		 else
		   sec.set_overrun ();
	       }
	     else
	       {
		 if (off < (import->ordinary_locs.second
			    - import->ordinary_locs.first))
		   locus = import->ordinary_locs.first + off;
		 else
		   sec.set_overrun ();
	       }
	   }
       }
       break;

    default:
      sec.set_overrun ();
      break;
    }

  return locus;
}

/* Prepare the span adjustments.  */
// FIXME: The location streaming does not consider running out of
// locations in either the module interface, nor in the importers.
// At least we fail with a hard error though.

unsigned
module_state::prepare_locations ()
{
  dump () && dump ("Preparing locations");
  dump.indent ();

  /* Figure the alignment of ordinary location spans.  */
  unsigned max_rager = 0;  /* Brains! */
  for (unsigned ix = loc_spans::SPAN_FIRST; ix != spans.length (); ix++)
    {
      loc_spans::span &span = spans[ix];
      line_map_ordinary const *omap
	= linemap_check_ordinary (linemap_lookup (line_table,
						  span.ordinary.first));

      /* We should exactly match up.  */
      gcc_checking_assert (MAP_START_LOCATION (omap) == span.ordinary.first);
      for (; MAP_START_LOCATION (omap) < span.ordinary.second; omap++)
	{
	  /* We should never find a module linemap in an interval.  */
	  gcc_checking_assert (!MAP_MODULE_P (omap));

	  if (max_rager < omap->m_range_bits)
	    max_rager = omap->m_range_bits;
	}
    }

  /* Adjust the maps.  Ordinary ones ascend, and we must maintain
     alignment.  Macro ones descend, but are unaligned.  */
  location_t ord_off = spans[loc_spans::SPAN_FIRST].ordinary.first;
  location_t mac_off = spans[loc_spans::SPAN_FIRST].macro.second;
  location_t range_mask = (1u << max_rager) - 1;

  dump () && dump ("Ordinary maps range bits:%u, preserve:%x, zero:%u",
		   max_rager, ord_off & range_mask, ord_off & ~range_mask);

  for (unsigned ix = loc_spans::SPAN_FIRST; ix != spans.length (); ix++)
    {
      loc_spans::span &span = spans[ix];

      span.macro_delta = mac_off - span.macro.second;
      mac_off -= span.macro.second - span.macro.first;
      dump () && dump ("Macro span:%u [%u,%u):%u->%d(%u)", ix,
		       span.macro.first, span.macro.second,
		       span.macro.second - span.macro.first,
		       span.macro_delta, span.macro.first + span.macro_delta);

      line_map_ordinary const *omap
	= linemap_check_ordinary (linemap_lookup (line_table,
						  span.ordinary.first));
      location_t base = MAP_START_LOCATION (omap);

      /* Preserve the low MAX_RAGER bits of base by incrementing ORD_OFF.  */
      unsigned low_bits = base & range_mask;
      if ((ord_off & range_mask) > low_bits)
	low_bits += range_mask + 1;
      ord_off = (ord_off & ~range_mask) + low_bits;
      span.ordinary_delta = ord_off - base;

      for (; MAP_START_LOCATION (omap) < span.ordinary.second; omap++)
	{
	  location_t start_loc = MAP_START_LOCATION (omap);
	  unsigned to = start_loc + span.ordinary_delta;
	  location_t end_loc = MAP_START_LOCATION (omap + 1);
	  
	  dump () && dump ("Ordinary span:%u [%u,%u):%u->%d(%u)", ix, start_loc,
			   end_loc, end_loc - start_loc,
			   span.ordinary_delta, to);

	  /* There should be no change in the low order bits.  */
	  gcc_checking_assert (((start_loc ^ to) & range_mask) == 0);
	}
      /* The ending serialized value.  */
      ord_off = span.ordinary.second + span.ordinary_delta;
    }
  dump () && dump ("Ordinary hwm:%u macro lwm:%u", ord_off, mac_off);

  dump.outdent ();

  return max_rager;
}

/* Write the location maps.  This also determines the shifts for the
   location spans.  */
// FIXME: I do not prune the unreachable locations.  Modules with
// textually-large GMFs could well cause us to run out of locations.
// Regular single-file modules could also be affected.

void
module_state::write_locations (elf_out *to, unsigned max_rager,
			       bool has_partitions, unsigned *crc_p)
{
  dump () && dump ("Writing locations");
  dump.indent ();

  range_t num_maps (0, 0);
  vec<const char *> filenames;
  filenames.create (20);

  /* Count the maps and determine the unique filenames.  */
  for (unsigned ix = loc_spans::SPAN_FIRST; ix != spans.length (); ix++)
    {
      loc_spans::span &span = spans[ix];

      if (span.macro.first != span.macro.second)
	{
	  unsigned count
	    = linemap_lookup_macro_index (line_table, span.macro.first) + 1;
	  count -= linemap_lookup_macro_index (line_table,
					       span.macro.second - 1);
	  dump (dumper::LOCATION) && dump ("Span:%u %u macro maps", ix, count);
	  num_maps.second += count;
	}

      line_map_ordinary const *omap
	= linemap_check_ordinary (linemap_lookup (line_table,
						  span.ordinary.first));

      /* We should exactly match up.  */
      gcc_checking_assert (MAP_START_LOCATION (omap) == span.ordinary.first);
      line_map_ordinary const *fmap = omap;
      for (; MAP_START_LOCATION (omap) < span.ordinary.second; omap++)
	{
	  const char *fname = ORDINARY_MAP_FILE_NAME (omap);

	  /* We should never find a module linemap in an interval.  */
	  gcc_checking_assert (!MAP_MODULE_P (omap));

	  /* We expect very few filenames, so just an array.  */
	  for (unsigned jx = filenames.length (); jx--;)
	    {
	      const char *name = filenames[jx];
	      if (0 == strcmp (name, fname))
		{
		  /* Reset the linemap's name, because for things like
		     preprocessed input we could have multple
		     instances of the same name, and we'd rather not
		     percolate that.  */
		  const_cast<line_map_ordinary *> (omap)->to_file = name;
		  fname = NULL;
		  break;
		}
	    }
	  if (fname)
	    filenames.safe_push (fname);
	}

      unsigned count = omap - fmap;
      gcc_checking_assert (count);
      num_maps.first += count;
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

  /* The reserved locations.  */
  dump () && dump ("Reserved locations [%u,%u) macro [%u,%u)",
		   spans[loc_spans::SPAN_RESERVED].ordinary.first,
		   spans[loc_spans::SPAN_RESERVED].ordinary.second,
		   spans[loc_spans::SPAN_RESERVED].macro.first,
		   spans[loc_spans::SPAN_RESERVED].macro.second);
  sec.u (spans[loc_spans::SPAN_RESERVED].ordinary.second);
  sec.u (spans[loc_spans::SPAN_RESERVED].macro.first);

  {
    /* Write the ordinary maps.   */
    location_t offset = spans[loc_spans::SPAN_FIRST].ordinary.first;
    location_t range_mask = (1u << max_rager) - 1;

    dump () && dump ("Ordinary maps:%u, range bits:%u, preserve:%x, zero:%u",
		     num_maps.first, max_rager, offset & range_mask,
		     offset & ~range_mask);
    sec.u (num_maps.first);	/* Num maps.  */
    sec.u (max_rager);		/* Maximum range bits  */
    sec.u (offset & range_mask);	/* Bits to preserve.  */
    sec.u (offset & ~range_mask);

    for (unsigned ix = loc_spans::SPAN_FIRST; ix != spans.length (); ix++)
      {
	loc_spans::span &span = spans[ix];
	line_map_ordinary const *omap
	  = linemap_check_ordinary (linemap_lookup (line_table,
						    span.ordinary.first));
	for (; MAP_START_LOCATION (omap) < span.ordinary.second; omap++)
	  {
	    location_t start_loc = MAP_START_LOCATION (omap);
	    unsigned to = start_loc + span.ordinary_delta;

	    dump (dumper::LOCATION)
	      && dump ("Span:%u ordinary [%u,%u)->%u", ix, start_loc,
		       MAP_START_LOCATION (omap + 1), to);

	    /* There should be no change in the low order bits.  */
	    gcc_checking_assert (((start_loc ^ to) & range_mask) == 0);
	    sec.u (to);

	    /* Making accessors just for here, seems excessive.  */
	    sec.u (omap->reason);
	    sec.u (omap->sysp);
	    sec.u (omap->m_range_bits);
	    sec.u (omap->m_column_and_range_bits - omap->m_range_bits);

	    const char *fname = ORDINARY_MAP_FILE_NAME (omap);
	    for (unsigned ix = 0; ix != filenames.length (); ix++)
	      if (filenames[ix] == fname)
		{
		  sec.u (ix);
		  break;
		}
	    sec.u (ORDINARY_MAP_STARTING_LINE_NUMBER (omap));

	    /* Write the included from location, which means reading
	       it while reading in the ordinary maps.  So we'd better
	       not be getting ahead of ourselves.  */
	    location_t from = linemap_included_from (omap);
	    gcc_checking_assert (from < MAP_START_LOCATION (omap));
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
	/* The ending serialized value.  */
	offset = MAP_START_LOCATION (omap) + span.ordinary_delta;
      }
    dump () && dump ("Ordinary location hwm:%u", offset);
    sec.u (offset);
  }

  {
    /* Write the macro maps.  */
    dump () && dump ("Macro maps:%u", num_maps.second);
    sec.u (num_maps.second);

    location_t offset = spans[loc_spans::SPAN_FIRST].macro.second;
    sec.u (offset);

    unsigned macro_num = 0;
    for (unsigned ix = loc_spans::SPAN_FIRST; ix != spans.length (); ix++)
      {
	loc_spans::span &span = spans[ix];
	if (span.macro.first == span.macro.second)
	  continue;

	for (unsigned first
	       = linemap_lookup_macro_index (line_table, span.macro.second - 1);
	     first < LINEMAPS_MACRO_USED (line_table);
	     first++)
	  {
	    line_map_macro const *mmap
	      = LINEMAPS_MACRO_MAP_AT (line_table, first);
	    location_t start_loc = MAP_START_LOCATION (mmap);
	    if (start_loc < span.macro.first)
	      break;
	    if (macro_num == num_maps.second)
	      {
		/* We're ending on an empty macro expansion.  The
		   preprocessor doesn't prune such things.  */
		// FIXME: This goes to show we should be eliding all
		// macro expansions that are not covering any location
		// we need to output (just like non-macro locations,
		// mentioned above).  They also happen in #if
		// conditionals, that we don't care about at all.
		gcc_checking_assert (!mmap->n_tokens);
		continue;
	      }

	    sec.u (offset);
	    sec.u (mmap->n_tokens);
	    sec.cpp_node (mmap->macro);
	    write_location (sec, mmap->expansion);
	    const location_t *locs = mmap->macro_locations;
	    /* There are lots of identical runs.  */
	    location_t prev = 0;
	    unsigned count = 0;
	    unsigned runs = 0;
	    for (unsigned jx = mmap->n_tokens * 2; jx--;)
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
	      && dump ("Span:%u macro:%u %I %u/%u*2 locations [%u,%u)->%u",
		       ix, macro_num, identifier (mmap->macro),
		       runs, mmap->n_tokens,
		       start_loc, start_loc + mmap->n_tokens,
		       start_loc + span.macro_delta);
	    macro_num++;
	    offset -= mmap->n_tokens;
	    gcc_checking_assert (offset == start_loc + span.macro_delta);
	  }
      }
    dump () && dump ("Macro location lwm:%u", offset);
    sec.u (offset);
    gcc_assert (macro_num == num_maps.second);
  }

  filenames.release ();

  sec.end (to, to->name (MOD_SNAME_PFX ".loc"), crc_p);
  dump.outdent ();
}

bool
module_state::read_locations ()
{
  bytes_in sec;

  if (!sec.begin (loc, from (), MOD_SNAME_PFX ".loc"))
    return false;
  dump () && dump ("Reading locations");
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

  unsigned reserved_ord = sec.u ();
  unsigned reserved_mac = sec.u ();
  dump () && dump ("Reserved locations <%u && >=%u", reserved_ord, reserved_mac);

  {
    /* Read the ordinary maps.  */
    unsigned num_ordinary = sec.u (); 
    unsigned max_rager = sec.u ();
    unsigned low_bits = sec.u ();
    location_t zero = sec.u ();
    location_t range_mask = (1u << max_rager) - 1;

    dump () && dump ("Ordinary maps:%u, range bits:%u, preserve:%x, zero:%u",
		     num_ordinary, max_rager, low_bits, zero);

    location_t offset = line_table->highest_location + 1;
    /* Ensure offset doesn't go backwards at the start.  */
    if ((offset & range_mask) > low_bits)
      offset += range_mask + 1;
    offset = (offset & ~range_mask);

    /* We need to insert our maps if we're a partition of the primary
       module interface.  */
    if (module_interface_p () && !module_partition_p () && is_partition ())
      spans.open (offset + low_bits);

    line_map_ordinary *maps = static_cast<line_map_ordinary *>
      (line_map_new_raw (line_table, false, num_ordinary));

    location_t lwm = offset;
    slurp ()->loc_deltas.first = offset - zero;
    ordinary_locs.first = zero + low_bits + slurp ()->loc_deltas.first;
    dump () && dump ("Ordinary loc delta %d", slurp ()->loc_deltas.first);

    for (unsigned ix = 0; ix != num_ordinary && !sec.get_overrun (); ix++)
      {
	line_map_ordinary *map = &maps[ix];
	unsigned hwm = sec.u ();

	/* Record the current HWM so that the below read_location is
	   ok.  */
	ordinary_locs.second = hwm + slurp ()->loc_deltas.first;
	map->start_location = hwm + (offset - zero);
	if (map->start_location < lwm)
	  sec.set_overrun ();
	lwm = map->start_location;
	dump (dumper::LOCATION) && dump ("Map:%u %u->%u", ix, hwm, lwm);
	map->reason = lc_reason (sec.u ());
	map->sysp = sec.u ();
	map->m_range_bits = sec.u ();
	map->m_column_and_range_bits = map->m_range_bits + sec.u ();

	unsigned fnum = sec.u ();
	map->to_file = (fnum < filenames.length () ? filenames[fnum] : "");
	map->to_line = sec.u ();

	/* Root the outermost map at our location.  */
	location_t from = read_location (sec);
	map->included_from = from != UNKNOWN_LOCATION ? from : loc;
      }

    location_t hwm = sec.u ();
    ordinary_locs.second = hwm + slurp ()->loc_deltas.first;

    /* highest_location is the one handed out, not the next one to
       hand out.  */
    line_table->highest_location = ordinary_locs.second - 1;

    if (lwm > line_table->highest_location)
      /* We ran out of locations, fail.  */
      sec.set_overrun ();
    dump () && dump ("Ordinary location hwm:%u", ordinary_locs.second);
  }

  {
    /* Read the macro maps.  */
    unsigned num_macros = sec.u ();
    location_t zero = sec.u ();
    dump () && dump ("Macro maps:%u zero:%u", num_macros, zero);

    location_t offset = LINEMAPS_MACRO_LOWEST_LOCATION (line_table);
    slurp ()->loc_deltas.second = zero - offset;
    macro_locs.second = zero - slurp ()->loc_deltas.second;
    dump () && dump ("Macro loc delta %d", slurp ()->loc_deltas.second);

    for (unsigned ix = 0; ix != num_macros && !sec.get_overrun (); ix++)
      {
	unsigned lwm = sec.u ();
	/* Record the current LWM so that the below read_location is
	   ok.  */
	macro_locs.first = lwm - slurp ()->loc_deltas.second;

	unsigned n_tokens = sec.u ();
	cpp_hashnode *node = sec.cpp_node ();
	location_t exp_loc = read_location (sec);

	const line_map_macro *macro
	  = linemap_enter_macro (line_table, node, exp_loc, n_tokens);
	if (!macro)
	  /* We ran out of numbers, bail out (and that'll set overrun
	     due to unread data.  */
	  break;

	location_t *locs = macro->macro_locations;
	location_t tok_loc = loc;
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
	  && dump ("Macro:%u %I %u/%u*2 locations [%u,%u)",
		   ix, identifier (node), runs, n_tokens,
		   MAP_START_LOCATION (macro),
		   MAP_START_LOCATION (macro) + n_tokens);
      }
    location_t lwm = sec.u ();
    macro_locs.first = lwm - slurp ()->loc_deltas.second;

    dump () && dump ("Macro location lwm:%u", macro_locs.first);

    if (module_interface_p () && !module_partition_p () && is_partition ())
      spans.close ();
  }

  filenames.release ();
  
  dump.outdent ();
  if (!sec.end (from ()))
    return false;

  return true;
}

/* Serialize the definition of MACRO.  */

void
module_state::write_define (bytes_out &sec, const cpp_macro *macro, bool located)
{
  sec.u (macro->count);

  sec.b (macro->fun_like);
  sec.b (macro->variadic);
  sec.b (macro->syshdr);
  sec.bflush ();

  if (located)
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
      if (located)
	write_location (sec, token->src_loc);
      sec.u (token->type);
      sec.u (token->flags);
      switch (cpp_token_val_index (token))
	{
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

	case CPP_TOKEN_FLD_STR:
	  /* A string, number or comment.  Not always NUL terminated,
	     we stream out in a single contatenation with embedded
	     NULs as that's a safe default.  */
	  len += token->val.str.len + 1;
	  sec.u (token->val.str.len);
	  break;

	case CPP_TOKEN_FLD_ARG_NO:
	  /* An argument reference.  */
	  sec.u (token->val.macro_arg.arg_no);
	  sec.cpp_node (token->val.macro_arg.spelling);
	  break;

	case CPP_TOKEN_FLD_NONE:
	  break;

	  /* I don't think the following occur inside a macro itself.  */
	case CPP_TOKEN_FLD_SOURCE:
	case CPP_TOKEN_FLD_TOKEN_NO:
	case CPP_TOKEN_FLD_PRAGMA:
	default:
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
module_state::read_define (bytes_in &sec, cpp_reader *reader, bool located) const
{
  unsigned count = sec.u ();
  /* We rely on knowing cpp_reader's hash table is ident_hash, and
     it's subobject allocator is stringpool_ggc_alloc and that is just
     a wrapper for ggc_alloc_atomic.  */
  cpp_macro *macro
    = (cpp_macro *)ggc_alloc_atomic (sizeof (cpp_macro)
				     + sizeof (cpp_token) * (count - !!count));
  memset (macro, 0, sizeof (cpp_macro) + sizeof (cpp_token) * (count - !!count));

  macro->count = count;
  macro->kind = cmk_macro;
  macro->imported = true;

  macro->fun_like = sec.b ();
  macro->variadic = sec.b ();
  macro->syshdr = sec.b ();
  sec.bflush ();

  macro->line = located ? read_location (sec) : loc;

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
      token->src_loc = located ? read_location (sec) : loc;
      token->type = cpp_ttype (sec.u ());
      token->flags = sec.u ();
      switch (cpp_token_val_index (token))
	{
	case CPP_TOKEN_FLD_NODE:
	  /* An identifier.  */
	  token->val.node.node = sec.cpp_node ();
	  token->val.node.spelling = sec.cpp_node ();
	  if (!token->val.node.spelling)
	    token->val.node.spelling = token->val.node.node;
	  break;

	case CPP_TOKEN_FLD_STR:
	  /* A string, number or comment.  */
	  token->val.str.len = sec.u ();
	  len += token->val.str.len + 1;
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

	case CPP_TOKEN_FLD_NONE:
	  break;

	default:
	  sec.set_overrun ();
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
struct macro_export {
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

  private:
    enum layout
      {
       L_MOD = 1,		/* Bit zero is always 1.  */
       L_UNDEF = MODULE_BITS + 1,	/* Has an undef.  */
       L_DEF = MODULE_BITS + 2,		/* Has a def.  */
       L_MOD_MASK = (1u << MODULE_BITS) - 1 /* Which module,  */
      };

  public:
    /* Not a regular ctor, because we put it in a union, and that's
       not allowed in C++ 98.  */
    static slot ctor (unsigned mod)
    {
      slot s;
      s.bits = 1 | (mod << L_MOD);
      s.offset = -1;
      return s;
    }

  public:
    bool get_undef () const 
    {
      return (bits >> L_UNDEF) & 1;
    }
    bool get_def () const 
    {
      return (bits >> L_DEF) & 1;
    }
    unsigned get_mod () const
    {
      return (bits >> L_MOD) & L_MOD_MASK;
    }
    void set_undef ()
    {
      bits |= 1u << L_UNDEF;
    }
    void set_def ()
    {
      bits |= 1u << L_DEF;
    }
    void clear_def ()
    {
      bits &= ~(1u << L_DEF);
    }
  };

private:
  typedef vec<slot, va_heap, vl_embed> ary_t;
  union either {
    /* Discriminated by bit 0.  The expected case is that there will
       be exactly one slot per macro, hence the effort of packing
       that.  */
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
    return u.single.bits & 1;
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
  slot &append (unsigned);
};

/* O is a new import to append to the list for.  If we're an empty
   set, initialize us.  */

macro_import::slot &
macro_import::append (unsigned mod)
{
  if (!occupied_p ())
    {
      u.single = slot::ctor (mod);
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
      return *u.ary->quick_push (slot::ctor (mod));
    }
}

/* We're going to export something.  Make sure the first import slot
   is us.  */

macro_import::slot &
macro_import::exported ()
{
  if (occupied_p () && !(*this)[0].get_mod ())
    return (*this)[0];

  slot *a = &append (0);
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

static vec<macro_export, va_heap, vl_embed> *macro_exports;

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
      if (!macro->imported && !macro->lazy && macro->line >= spans.main_start ())
	{
	  gcc_checking_assert (macro->kind == cmk_macro);
	  /* I don't want to deal with this corner case, that I suspect is
	     a devil's advocate reading of the standard.  */
	  gcc_checking_assert (!macro->extra_tokens);

	  macro_import::slot &slot = get_macro_imports (node).exported ();
	  macro_export &exp = get_macro_export (slot);
	  slot.set_def ();
	  exp.def = macro;
	  exporting = true;
	}

  if (!exporting && node->deferred)
    {
      macro_import &imports = (*macro_imports)[node->deferred - 1];
      macro_import::slot &slot = imports[0];
      if (!slot.get_mod ())
	{
	  gcc_checking_assert (slot.get_def () || slot.get_undef ());
	  exporting = true;
	}
    }

  if (exporting)
    static_cast<auto_vec<cpp_hashnode *> *> (data_)->safe_push (node);

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

/* Write out the exported defines.  This is two sections, one
   containing the definitions, the other a table of node names.  */
// FIXME: macros from command line and forced headers are not to be
// exported.

unsigned
module_state::write_macros (elf_out *to, cpp_reader *reader, unsigned *crc_p)
{
  dump () && dump ("Writing macros");
  dump.indent ();

  auto_vec<cpp_hashnode *> macros;
  cpp_forall_identifiers (reader, maybe_add_macro, &macros);

  const cpp_hashnode *controlling_node = cpp_main_controlling_macro (reader);
  unsigned count = 0;
  if (macros.length ())
    {
      dump (dumper::MACRO) && dump ("No more than %u macros", macros.length ());

      macros.qsort (macro_loc_cmp);

      /* Write the defs */
      bytes_out sec (to);
      sec.begin ();

      for (unsigned ix = macros.length (); ix--;)
	{
	  cpp_hashnode *node = macros[ix];
	  macro_import::slot &slot = (*macro_imports)[node->deferred - 1][0];
	  gcc_assert (!slot.get_mod ()
		      && (slot.get_undef () || slot.get_def ()));

	  macro_export &mac = (*macro_exports)[slot.offset];
	  gcc_assert (slot.get_undef () == (mac.undef_loc != UNKNOWN_LOCATION)
		      && slot.get_def () == (mac.def != NULL));

	  if (IDENTIFIER_KEYWORD_P (identifier (node)))
	    {
	      warning_at (mac.def->line, 0,
			  "not exporting %<#define %E%> as it is a keyword",
			  identifier (node));
	      slot.offset = 0;
	      continue;
	    }

	  if (node == controlling_node)
	    {
	      /* The controlling macro is written in the config, not
		 here.  */
	      dump () && dump ("Controlling macro %I",
			       identifier (controlling_node));
	      slot.offset = 0;
	      continue;
	    }

	  count++;
	  slot.offset = sec.pos;
	  dump (dumper::MACRO)
	    && dump ("Writing macro %s%s%s %I at %u",
		     slot.get_undef () ? "#undef" : "",
		     slot.get_undef () && slot.get_def () ? " & " : "",
		     slot.get_def () ? "#define" : "",
		     identifier (node), slot.offset);
	  if (mac.undef_loc != UNKNOWN_LOCATION)
	    write_location (sec, mac.undef_loc);
	  if (mac.def)
	    write_define (sec, mac.def);
	}
      sec.end (to, to->name (MOD_SNAME_PFX ".def"), crc_p);

      if (count)
	{
	  /* Write the table.  */
	  bytes_out sec (to);
	  sec.begin ();
	  sec.u (count);

	  for (unsigned ix = macros.length (); ix--;)
	    {
	      const cpp_hashnode *node = macros[ix];
	      macro_import::slot &slot = (*macro_imports)[node->deferred - 1][0];

	      if (slot.offset)
		{
		  sec.cpp_node (node);
		  sec.u ((slot.get_undef () << 0)
			 | ((slot.get_def ()) << 1));
		  sec.u (slot.offset);
		}
	    }
	  sec.end (to, to->name (MOD_SNAME_PFX ".mac"), crc_p);
	}
    }

  dump.outdent ();
  return count;
}

bool
module_state::read_macros ()
{
  /* Get the tbl section.  */
  if (!slurp ()->macro_tbl.begin (loc, from (), MOD_SNAME_PFX ".mac"))
    return false;

  /* Get the def section.  */
  if (!slurp ()->macro_defs.begin (loc, from (), MOD_SNAME_PFX ".def"))
    return false;

  return true;
}

/* Install the macro name table.  */
// FIXME: Deal with clobbering controlling macros

void
module_state::install_macros ()
{
  bytes_in &sec = slurp ()->macro_tbl;
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
      macro_import::slot &slot = imp.append (mod);
      unsigned flags = sec.u ();
      if ((flags >> 0) & 1)
	slot.set_undef ();
      if ((flags >> 1) & 1)
	slot.set_def ();
      slot.offset = sec.u ();

      dump (dumper::MACRO)
	&& dump ("Read %s macro %s%s%s %I at %u",
		 imp.length () > 1 ? "add" : "new",
		 slot.get_undef () ? "#undef" : "",
		 slot.get_undef () && slot.get_def () ? " & " : "",
		 slot.get_def () ? "#define" : "",
		 identifier (node), slot.offset);

      /* We'll leak an imported definition's TOKEN_FLD_STR's data
	 here.  But that only happens when we've had to resolve the
	 deferred macro before this import -- why are you doing
	 that?  */
      if (cpp_macro *cur = cpp_set_deferred_macro (node))
	if (!cur->imported)
	  {
	    macro_import::slot &slot = imp.exported ();
	    macro_export &exp = get_macro_export (slot);
	    exp.def = cur;
	    slot.set_def ();
	    dump (dumper::MACRO)
	      && dump ("Saving current #define %I", identifier (node));
	  }
    }

  /* We're now done with the table.  */
  elf_in::release (slurp ()->from, sec);

  dump.outdent ();
}

/* Import the transitive macros.  */

void
module_state::import_macros ()
{
  bitmap_ior_into (headers, slurp ()->headers);

  bitmap_iterator bititer;
  unsigned bitnum;
  EXECUTE_IF_SET_IN_BITMAP (slurp ()->headers, 0, bitnum, bititer)
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
  slot.set_undef ();
  exp.def = NULL;
  slot.clear_def ();

  dump (dumper::MACRO) && dump ("Recording macro #undef %I", identifier (node));

  dump.pop (n);
}

/* NODE is a deferred macro node.  Determine the defintion and return
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

  if (!(imports[0].get_undef () && !imports[0].get_mod ()))
    {
      /* Calculate the set of visible header imports.  */
      bitmap_copy (visible, headers);
      for (unsigned ix = imports.length (); ix--;)
	{
	  const macro_import::slot &slot = imports[ix];
	  unsigned mod = slot.get_mod ();
	  if (slot.get_undef () && bitmap_bit_p (visible, mod))
	    {
	      bitmap arg = mod ? (*modules)[mod]->slurp ()->headers : headers;
	      bitmap_and_compl_into (visible, arg);
	      bitmap_set_bit (visible, mod);
	    }
	}
    }
  bitmap_set_bit (visible, 0);

  /* Now find the macros that are still visible.  */
  bool failed = false;
  cpp_macro *def = NULL;
  auto_vec<macro_export> defs (imports.length ());
  for (unsigned ix = imports.length (); ix--;)
    {
      const macro_import::slot &slot = imports[ix];
      unsigned mod = slot.get_mod ();
      if (bitmap_bit_p (visible, mod))
	{
	  macro_export *pushed = NULL;
	  if (mod)
	    {
	      const module_state *imp = (*modules)[mod];
	      bytes_in &sec = imp->slurp ()->macro_defs;
	      if (!sec.get_overrun ())
		{
		  dump (dumper::MACRO)
		    && dump ("Reading macro %s%s%s %I module %M at %u",
			     slot.get_undef () ? "#undef" : "",
			     slot.get_undef () && slot.get_def ()
			     ? " & " : "",
			     slot.get_def () ? "#define" : "",
			     identifier (node), imp, slot.offset);
		  sec.random_access (slot.offset);

		  macro_export exp;
		  if (slot.get_undef ())
		    exp.undef_loc = imp->read_location (sec);
		  if (slot.get_def ())
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
      error_at (loc, "inconsistent imported macro definition %qE",
		identifier (node));
      for (unsigned ix = defs.length (); ix--;)
	{
	  macro_export &exp = defs[ix];
	  if (exp.undef_loc)
	    inform (exp.undef_loc, "#undef %E", identifier (node));
	  if (exp.def)
	    inform (exp.def->line, "#define %s",
		    cpp_macro_definition (reader, node, exp.def));
	}
      def = NULL;
    }

  dump.pop (n);

  return def;
}

/* Compare bindings for two namespaces.  Those closer to :: are
   less.  */

static int
space_cmp (const void *a_, const void *b_)
{
  depset *a = *(depset *const *)a_;
  depset *b = *(depset *const *)b_;
  tree ns_a = a->get_entity ();
  tree ns_b = b->get_entity ();

  gcc_checking_assert (ns_a != ns_b);

  /* Deeper namespaces come after shallower ones.  */
  if (int delta = int (SCOPE_DEPTH (ns_a)) - int (SCOPE_DEPTH (ns_b)))
    return delta;

  /* Otherwise order by UID for consistent results.  */
  return DECL_UID (ns_a) < DECL_UID (ns_b) ? -1 : +1;
}

/* Tool configuration:  MOD_SNAME_PFX .config

   This is data that confirms current state (or fails).

   u32:version
   u32:crc
   u:module-name

   controlling_macro

   u:<target-triplet>
   u:<host-triplet>
   s:options

   u:fixed_trees->length()
   u32:global_crc

   u:modules->length ()
   direct-imports
   indirect-imports

   u:decl-section-lwm
   u:decl-section-hwm
   u:unnamed
*/

/* Data for config reading and writing.  */
struct module_state_config {
  const char *opt_str;
  range_t sec_range;
  unsigned num_unnamed;
  unsigned num_imports;
  unsigned num_partitions;
  unsigned num_bindings;
  unsigned num_macros;
  const cpp_hashnode *controlling_node;
  module_state *alias;

public:
  module_state_config ()
    :opt_str (get_opts ()), sec_range (0,0), num_unnamed (0),
     num_imports (0), num_partitions (0),
     num_bindings (0), num_macros (0),
     controlling_node (NULL), alias (NULL)
  {
  }
  static void release ()
  {
    XDELETEVEC (opts);
    opts = NULL;
  }

private:
  static const char *get_opts ();
  static char *opts;
};

char *module_state_config::opts;

/* Generate a string of the compilation options.  */

const char *
module_state_config::get_opts ()
{
  if (opts)
    return opts;

  /* Concatenate important options.  */
  size_t opt_alloc = EXPERIMENT (2, 200);
  size_t opt_len = 0;
  char *opt_str = XNEWVEC (char, opt_alloc);

  for (unsigned ix = 0; ix != save_decoded_options_count; ix++)
    {
      const cl_decoded_option *opt = &save_decoded_options[ix];
      if (opt->opt_index >= N_OPTS)
	continue;
      // FIXME: There's probably a better way to get options we care
      // about?  What does LTO do?
      const char *text = opt->orig_option_with_args_text;

      if (opt->opt_index >= N_OPTS)
	continue;

      /* Not an option (a filename or somesuch).  */
      if (text[0] != '-')
	continue;

      /* Not -f* -g* -m* -O* -std=* */
      if (!strchr ("fgmO", text[1])
	  && 0 != strncmp (&text[1], "std=", 4))
	continue;

      /* Drop module-related options we don't need to preserve.  */
      if (opt->opt_index == OPT_fmodule_lazy
	  || opt->opt_index == OPT_fmodule_header
	  || opt->opt_index == OPT_fforce_module_macros
	  || opt->opt_index == OPT_fmodule_mapper_
	  || opt->opt_index == OPT_fmodule_only
	  || opt->opt_index == OPT_fmodules_ts)
	continue;

      /* Drop random options.  */
      if (opt->opt_index == OPT_frandom_seed
	  || opt->opt_index == OPT_frandom_seed_)
	continue;

      /* Drop -fpic.  */
      if (opt->opt_index == OPT_fpic
	  || opt->opt_index == OPT_fPIC
	  || opt->opt_index == OPT_fPIE)
	continue;

      /* Drop profiling.  */
      if (opt->opt_index >= OPT_fprofile
	   && opt->opt_index <= OPT_fprofile_values)
	continue;

      /* Drop diagnostic formatting options.  */
      if (opt->opt_index == OPT_fmessage_length_
	  || (opt->opt_index >= OPT_fdiagnostics_color_
	      && opt->opt_index <= OPT_fdiagnostics_show_template_tree))
	continue;

      /* Drop any dump control options.  */
      if (opt->opt_index >= OPT_fdump_
	  && opt->opt_index <= OPT_fdump_unnumbered_links)
	continue;

      /* Drop preprocessing options.  */
      if (opt->opt_index == OPT_fpreprocessed
	  || opt->opt_index == OPT_fdirectives_only)
	continue;

      size_t l = strlen (text);
      if (opt_alloc < opt_len + l + 2)
	{
	  opt_alloc = (opt_len + l + 2) * 2;
	  opt_str = XRESIZEVEC (char, opt_str, opt_alloc);
	}
      if (opt_len)
	opt_str[opt_len++] = ' ';
      memcpy (&opt_str[opt_len], text, l);
      opt_len += l;
    }

  opt_str[opt_len] = 0;

  opts = opt_str;

  return opts;
}

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

  if (!is_header ())
    ;
  else if (config.controlling_node)
    {
      gcc_assert (cpp_user_macro_p (config.controlling_node));
      dump () && dump ("Controlling macro=%I",
		       identifier (config.controlling_node));
      cfg.cpp_node (config.controlling_node);
      write_define (cfg, config.controlling_node->value.macro, false);
    }
  else
    cfg.u (0);

  /* Configuration. */
  dump () && dump ("Writing target='%s', host='%s'",
		   TARGET_MACHINE, HOST_MACHINE);
  unsigned target = to->name (TARGET_MACHINE);
  unsigned host = (!strcmp (TARGET_MACHINE, HOST_MACHINE)
		   ? target : to->name (HOST_MACHINE));
  cfg.u (target);
  cfg.u (host);

  cfg.str (config.opt_str);

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

  cfg.u (config.sec_range.first);
  cfg.u (config.sec_range.second);
  dump () && dump ("Declaration sections are [%u,%u)",
		   config.sec_range.first, config.sec_range.second);

  cfg.u (config.num_bindings);
  dump () && dump ("Bindings %u", config.num_bindings);
  cfg.u (config.num_unnamed);
  dump () && dump ("Unnamed %u", config.num_unnamed);
  cfg.u (config.num_macros);
  dump () && dump ("Macros %u", config.num_macros);

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
module_state::read_config (cpp_reader *reader, module_state_config &config)
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
      verstr_t my_string, their_string;

      version2string (my_ver, my_string);
      version2string (their_ver, their_string);

      if (!IS_EXPERIMENTAL (my_ver)
	  || !IS_EXPERIMENTAL (their_ver)
	  || MODULE_MAJOR (my_ver) != MODULE_MAJOR (their_ver))
	{
	  /* Non-experimental or majors differ, decline.  */
	  error_at (loc, "compiled module is %sversion %s",
		    IS_EXPERIMENTAL (their_ver) ? "experimental " : "",
		    their_string);
	  inform (loc, "compiler is %sversion %s",
		  IS_EXPERIMENTAL (my_ver) ? "experimental " : "",
		  my_string);
	  cfg.set_overrun ();
	  goto done;
	}
      else
	/* Minors differ, give it a go.  */
	if (warning_at (loc, 0, "compiled module is experimental version %s",
			their_string))
	  {
	    inform (loc, "compiler is experimental version %s,"
		    " close enough? \xc2\xaf\\_(\xe3\x83\x84)_/\xc2\xaf",
		    my_string);
	    note_cmi_name ();
	  }
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

  /* Read controlling macro.  We do this before validating the CRC,
     as the latter is computed from names we originally used.  */
  if (!is_header ())
    ;
  else if (cpp_hashnode *node = cfg.cpp_node ())
    {
      cpp_macro *macro = read_define (cfg, reader, false);
      if (cfg.get_overrun ())
	goto done;

      dump () && dump ("Controlling macro is %I", identifier (node));
      if (cpp_user_macro_p (node))
	{
	  /* Already defined, find alias. Expect pseudo-import  */
	  cpp_macro *existing = node->value.macro;
	  if (!existing)
	    existing = cpp_get_deferred_macro (reader, node, loc);

	  if (!existing)
	    ;
	  else if (!existing->imported)
	    {
	      error_at (loc, "controlling macro %E was not set by an import",
			identifier (node));
	      inform (existing->line, "controlling macro defined here");
	    }
	  else if (!node->deferred)
	    {
	      error_at (loc, "circular alias of controlling macro %E",
			identifier (node));
	      inform (existing->line, "aliased here");
	    }
	  else
	    {
	      /* It's an alias.  Go find it.  */
	      macro_import &imp = (*macro_imports)[node->deferred - 1];
	      macro_import::slot &slot = imp[0];

	      config.alias = (*modules)[slot.get_mod ()];
	      cfg.no_more ();
	      goto done;
	    }
	}

      /* Install as pseudo-import.  We'll complete this installation
	 once the module number's assigned.  */
      cpp_set_deferred_macro (node, macro);
      config.controlling_node = node;
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

  /* Check compilation options.  For the moment we requre exact
     match.  */
  {
    const char *their_opts = cfg.str ();
    if (strcmp (their_opts, config.opt_str))
      {
	error_at (loc, "compilation options differ %qs, expected %qs",
		  their_opts, config.opt_str);
	cfg.set_overrun ();
	goto done;
      }
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

  /* Allocate the REMAP vector.  */
  slurp ()->alloc_remap (config.num_imports);

  /* Random config data.  */
  config.sec_range.first = cfg.u ();
  config.sec_range.second = cfg.u ();
  dump () && dump ("Declaration sections are [%u,%u)",
		   config.sec_range.first, config.sec_range.second);

  config.num_bindings = cfg.u ();
  dump () && dump ("Bindings %u", config.num_bindings);
  config.num_unnamed = cfg.u ();
  dump () && dump ("Unnamed %u", config.num_unnamed);

  config.num_macros = cfg.u ();
  dump () && dump ("Macros %u", config.num_macros);

  if (config.sec_range.first > config.sec_range.second
      || config.sec_range.second > from ()->get_section_limit ())
    {
      error_at (loc, "paradoxical declaration section range");
      cfg.set_overrun ();
      goto done;
    }

 done:
  return cfg.end (from ());
}

/* Use ELROND format to record the following sections:
     qualified-names	    : binding value(s)
     MOD_SNAME_PFX.README   : human readable, strings
     MOD_SNAME_PFX.ENV      : environment strings, strings
     MOD_SNAME_PFX.nms 	    : namespace hierarchy
     MOD_SNAME_PFX.bnd      : binding table
     MOD_SNAME_PFX.tpl	    : template table
     MOD_SNAME_PFX.vld      : unnamed table
     MOD_SNAME_PFX.imp      : import table
     MOD_SNAME_PFX.prt      : partitions table
     MOD_SNAME_PFX.loc      : locations
     MOD_SNAME_PFX.def      : macro definitions
     MOD_SNAME_PFX.mac      : macro index
     MOD_SNAME_PFX.cfg      : config data
*/

void
module_state::write (elf_out *to, cpp_reader *reader)
{
  /* Figure out remapped module numbers, which might elide
     partitions.  */
  bitmap partitions = NULL;
  if (!is_header () && !is_partition ())
    partitions = BITMAP_GGC_ALLOC ();

  unsigned mod_hwm = MODULE_IMPORT_BASE;
  for (unsigned ix = MODULE_IMPORT_BASE; ix != modules->length (); ix++)
    {
      module_state *imp = (*modules)[ix];

      /* Promote any non-partition import from a partition, unless
	 we're a partition.  */
      if (!is_partition () && !imp->is_partition () && imp->from_partition_p)
	imp->direct_p = true;

      /* Write any import that is not a partition, unless we're a
	 partition.  */
      if (!partitions || !imp->is_partition ())
	imp->remap = mod_hwm++;
      else
	{
	  dump () && dump ("Partition %M %u", imp, ix);
	  bitmap_set_bit (partitions, ix);
	  imp->remap = MODULE_PURVIEW;
	  /* All interface partitions must be exported.  */
	  if (imp->is_interface () && !bitmap_bit_p (exports, imp->mod))
	    {
	      error_at (imp->loc, "interface partition is not exported");
	      bitmap_set_bit (exports, imp->mod);
	    }
	}
    }

  /* Find the set of decls we must write out.  */
  depset::hash table (DECL_NAMESPACE_BINDINGS (global_namespace)->size () * 8);
  /* Add the specializations before the writables, so that we can
     detect injected friend specializations.  */
  table.add_specializations (true, partitions);
  table.add_specializations (false, partitions);
  table.add_writables (global_namespace, partitions);
  table.find_dependencies ();
  // FIXME: Find reachable GMF entities from non-emitted pieces.  It'd
  // be nice to have a flag telling us this walk's necessary.  Even
  // better to not do it (why are we making visible implementation
  // details?) Fight the spec!

  if (!table.finalize_dependencies ())
    {
      to->set_error ();
      return;
    }

#if CHECKING_P
  // FIXME: Do we need to clear this? why would we write out a
  // definition we read in?
  /* We're done verifying at-most once reading, reset to verify
     at-most once writing.  */
  delete note_defs;
  note_defs = new hash_set<tree> (1000);
#endif

  /* Determine Strongy Connected Components.  */
  vec<depset *> sccs = table.connect ();

  unsigned crc = 0;
  unsigned range_bits = prepare_locations ();
  module_state_config config;

  config.num_imports = mod_hwm;
  config.num_partitions = modules->length () - mod_hwm;

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
  config.sec_range.first = config.sec_range.second = to->get_section_limit ();
  for (unsigned size, ix = 0; ix < sccs.length (); ix += size)
    {
      depset **base = &sccs[ix];

      /* Count the members in this cluster.  */
      for (size = 1; ix + size < sccs.length (); size++)
	{
	  if (base[size]->cluster != base[0]->cluster)
	    break;
	  base[size]->cluster = base[size]->section = 0;

	  /* Namespaces and imported entities should be their own
	     clusters.  */
	  gcc_checking_assert (base[size]->get_entity_kind ()
			       != depset::EK_NAMESPACE);
	  gcc_checking_assert (base[size]->get_entity_kind ()
			       != depset::EK_REDIRECT);
	  gcc_checking_assert (base[size]->is_binding ()
			       || !base[size]->is_import ());
	}
      base[0]->cluster = base[0]->section = 0;

      // FIXME: Unneeded.
      /* Sort the cluster.  Later processing makes use of the ordering
	 of defns < decls < bindings. */
      qsort (base, size, sizeof (depset *), cluster_cmp);

      if (base[0]->get_entity_kind () == depset::EK_NAMESPACE)
	{
	  /* A namespace decl, these are handled specially.  */
	  gcc_checking_assert (size == 1);
	  n_spaces++;
	}
      else if (base[0]->get_entity_kind () == depset::EK_REDIRECT)
	gcc_checking_assert (size == 1);
      else if (!base[0]->is_binding () && base[0]->is_import ())
	gcc_checking_assert (size == 1);
      else
	{
	  /* Save the size in the first member's cluster slot.  */
	  base[0]->cluster = size;

	  for (unsigned jx = 0; jx != size; jx++)
	    /* Set the section number.  */
	    base[jx]->section = config.sec_range.second;

	  config.sec_range.second++;
	}
    }

  /* Write the clusters.  Namespace decls are put in the spaces array.
     The meaning of depset::cluster changes to provide the
     unnamed-decl count of the depset's decl (and remains zero for
     non-decls and non-unnamed).  */
  auto_vec<depset *> spaces (n_spaces);
  for (unsigned size, ix = 0; ix < sccs.length (); ix += size)
    {
      depset **base = &sccs[ix];
      size = base[0]->cluster;

      if (!size)
	{
	  /* A namespace, import or redirect.  */
	  tree decl = base[0]->get_entity ();
	  depset::entity_kind ek = base[0]->get_entity_kind ();
	  const char *kind = "";
	  if (ek == depset::EK_NAMESPACE)
	    {
	      spaces.quick_push (base[0]);
	      kind = "namespace";
	    }
	  else if (ek == depset::EK_REDIRECT)
	    kind = "redirect";
	  else
	    {
	      gcc_checking_assert (base[0]->is_import ());
	      kind = "import";
	    }

	  dump (dumper::CLUSTER) && dump ("Cluster %s %N", kind, decl);
	  size = 1;
	}
      else
	{
	  /* Cluster is now used to number unnamed decls.  */
	  base[0]->cluster = 0;

	  write_cluster (to, base, size, table, config.num_unnamed, &crc);
	}
    }

  /* We'd better have written as many sections and found as many
     namespaces as we predicted.  */
  gcc_assert (config.sec_range.second == to->get_section_limit ()
	      && spaces.length () == n_spaces);

  /* Write the namespaces.  */
  spaces.qsort (space_cmp);
  write_namespaces (to, table, spaces, &crc);

  /* Write the bindings themselves.  */
  config.num_bindings = write_bindings (to, sccs, table, &crc);

  /* Write the unnamed.  */
  if (config.num_unnamed)
    write_unnamed (to, sccs, table, config.num_unnamed, &crc);

  /* Write the import table.  */
  if (config.num_imports > MODULE_IMPORT_BASE)
    write_imports (to, &crc);

  /* Write elided partition table.  */
  if (config.num_partitions)
    write_partitions (to, config.num_partitions, &crc);

  /* Write the line maps.  */
  write_locations (to, range_bits, config.num_partitions, &crc);

  config.num_macros = header_module_p () ? write_macros (to, reader, &crc) : 0;
  config.controlling_node = cpp_main_controlling_macro (reader);

  /* And finish up.  */
  write_config (to, config, crc);

  sccs.release ();

  /* Human-readable info.  */
  write_readme (to, config.opt_str, config.controlling_node);
  // FIXME: Write ths info to the 'reproducer' file yet to be implemented
  // write_env (to);

  trees_out::instrument ();
  dump () && dump ("Wrote %u sections", to->get_section_limit ());
}

/* Read a CMI from FD.  E is errno from its fopen.  Reading will
   be lazy, if this is an import and flag_module_lazy is in effect.  */

module_state *
module_state::read (int fd, int e, cpp_reader *reader)
{
  gcc_checking_assert (!u1.slurp);
  u1.slurp = new slurping (new elf_in (fd, e));
  if (!from ()->begin (loc))
    return NULL;

  module_state_config config;

  if (!read_config (reader, config))
    return NULL;

  if (config.alias)
    return config.alias;

  if (!read_locations ())
    return NULL;

  /* Read the import table.  */
  if (config.num_imports > MODULE_IMPORT_BASE
      && !read_imports (reader, line_table))
    return NULL;

  /* Read the elided partition table, if we're the primary partition.  */
  if (config.num_partitions && is_primary ()
      && !read_partitions (config.num_partitions))
    return NULL;

  /* Determine the module's number.  */
  gcc_checking_assert (mod == MODULE_UNKNOWN);
  gcc_checking_assert (this != (*modules)[MODULE_PURVIEW]);

  unsigned ix = modules->length ();
  if (ix == MODULE_LIMIT)
    {
      sorry ("too many modules loaded (limit is %u)", ix);
      from ()->set_error (elf::E_BAD_IMPORT);
      return NULL;
    }

  vec_safe_push (modules, this);
  /* We always import and export ourselves. */
  bitmap_set_bit (imports, ix);
  bitmap_set_bit (exports, ix);
  if (is_header ())
    bitmap_set_bit (slurp ()->headers, ix);
  mod = remap = ix;

  (*slurp ()->remap)[MODULE_PURVIEW] = mod;
  dump () && dump ("Assigning %M module number %u", this, mod);

  /* Finish registering the controlling macro.  */
  if (config.controlling_node
      && !config.controlling_node->deferred
      && config.controlling_node->value.macro)
    {
      cpp_hashnode *node = const_cast <cpp_hashnode *> (config.controlling_node);
      get_macro_imports (node).append (mod);
      dump () && dump ("Registering controlling macro %I", identifier (node));
    }

  /* We should not have been frozen during the importing done by
     read_config.  */
  gcc_assert (!from ()->is_frozen ());

  /* Look away.  Look away now.  */
  extern cpp_options *cpp_opts;
  if (config.num_macros && !cpp_opts->preprocessed)
    if (!read_macros ())
      return NULL;

  if (!flag_preprocess_only)
    {
      /* Read the namespace hierarchy. */
      auto_vec<tree> spaces;
      if (!read_namespaces (spaces))
	return NULL;

      /* And the bindings.  */
      if (!read_bindings (spaces, config.num_bindings, config.sec_range))
	return NULL;

      /* And unnamed.  */
      unnamed_lwm = vec_safe_length (unnamed_ary);
      if (config.num_unnamed
	  && !read_unnamed (config.num_unnamed, config.sec_range))
	return NULL;
    }

  /* We're done with the string and non-decl sections now.  */
  from ()->release ();
  slurp ()->remaining = config.sec_range.second - config.sec_range.first;
  slurp ()->lru = ++lazy_lru;

  if (!flag_preprocess_only && !flag_module_lazy)
    {
      /* Read the sections in forward order, so that dependencies are read
	 first.  See note about tarjan_connect.  */
      unsigned hwm = config.sec_range.second;
      for (unsigned ix = config.sec_range.first; ix != hwm; ix++)
	{
	  load_section (ix);
	  if (from ()->get_error ())
	    break;
	}
    }

  return NULL;
}

void
module_state::maybe_defrost ()
{
  if (from ()->is_frozen ())
    {
      if (lazy_open >= lazy_limit)
	freeze_an_elf ();
      dump () && dump ("Defrosting '%s'", filename);
      from ()->defrost (maybe_add_cmi_prefix (filename));
      lazy_open++;
    }
}

/* Load section SNUM, dealing with laziness.  It doesn't matter if we
   have multiple concurrent loads, because we do not use TREE_VISITED
   when reading back in.  */

void
module_state::load_section (unsigned snum)
{
  maybe_defrost ();

  unsigned old_current = slurp ()->current;
  slurp ()->current = snum;
  slurp ()->lru = 0;  /* Do not swap out.  */
  read_cluster (snum);
  slurp ()->lru = ++lazy_lru;
  slurp ()->current = old_current;
  slurp ()->remaining--;
}

/* After a reading operation, make sure things are still ok.  If not,
   emit an error and clean up.  In order to get some kind of context
   information, OUTERMOST is true, if this is the outermost cause of a
   read happening (eiher an import, or a lazy binding found during
   name-lookup).  In the latter case NS and ID provide the binding.  */

bool
module_state::check_read (unsigned diag_count, tree ns, tree id)
{
  bool done = (slurp ()->current == ~0u
	       && (from ()->get_error () || !slurp ()->remaining));
  if (done)
    {
      lazy_open--;
      if (slurp ()->macro_defs.size)
	from ()->preserve (slurp ()->macro_defs);
      if (slurp ()->macro_tbl.size)
	from ()->preserve (slurp ()->macro_tbl);
      from ()->end ();
    }

  bool ok = true;
  if (int e = from ()->get_error ())
    {
      const char *err = from ()->get_error (filename);
      /* Failure to read a module is going to cause big
	 problems, so bail out, if this is the top level.
	 Otherwise return NULL to let our importer know (and
	 fail).  */
      if (slurp ()->remaining && id)
	error_at (loc, "failed to load binding %<%E%s%E@%s%>: %s",
		  ns, &"::"[ns == global_namespace ? 2 : 0], id,
		  get_flatname (), err);
      else
	error_at (loc, "failed to read compiled module: %s", err);
      note_cmi_name ();

      if (e == EMFILE
	  || e == ENFILE
#if MAPPED_READING
	  || e == ENOMEM
#endif
	  || false)
	inform (loc, "consider using %<-fno-module-lazy%>,"
		" reducing %<--param %s%> value,"
		" or increasing the per-process file descriptor limit",
		compiler_params[PARAM_LAZY_MODULES].option);
      ok = false;
    }

  if (done)
    {
      slurp ()->close ();
      if (!is_header ())
	slurped ();
    }

  if (id && diag_count && diag_count <= unsigned (errorcount + warningcount)
      && flag_module_lazy)
    inform (input_location,
	    is_header () ? G_("during lazy loading of %<%E%s%E%>")
	    : G_("during lazy loading of %<%E%s%E@%s%>"),
	    ns, ns == global_namespace ? "" : "::", id, get_flatname ());

  if (!ok && diag_count)
    fatal_error (loc, "jumping off the crazy train to crashville");

  return ok;
}

/* Return the IDENTIFIER_NODE naming module IX.  This is the name
   including dots.  */

char const *
module_name (unsigned ix)
{
  module_state *imp = (*modules)[ix];

  if (!imp->name)
    imp = imp->parent;

  return imp->get_flatname ();
}

char const *
module_name (tree decl)
{
  if (TREE_CODE (decl) == NAMESPACE_DECL
      ? !DECL_NAMESPACE_ALIAS (decl)
      : !DECL_NAMESPACE_SCOPE_P (decl))
    return NULL;

  if (unsigned owner = MAYBE_DECL_MODULE_OWNER (decl))
    {
      module_state *module = (*modules)[owner];
      if (!module->name)
	module = module->parent;

      if (!module->is_header ())
	return module->get_flatname ();
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

/* Return the bitmap describing what modules are visible along the
   path of instantiation.  If we're not instantiation, this will be
   the visible imports of the TU.  */
// FIXME: Should we cache this?  smoosh it into tinst_level?

bitmap
module_visible_instantiation_path (bitmap *path_map_p)
{
  if (!modules_p ())
    return NULL;

  bitmap visible = (*modules)[MODULE_NONE]->imports;

  if (tinst_level *path = current_instantiation ())
    {
      bitmap path_map = BITMAP_GGC_ALLOC ();
      bitmap_set_bit (path_map, MODULE_NONE);
      bitmap_set_bit (path_map, MODULE_PURVIEW);

      bitmap tmp = BITMAP_GGC_ALLOC ();
      bitmap_copy (tmp, visible);
      visible = tmp;
      for (; path; path = path->next)
	{
	  tree decl = path->tldcl;
	  if (TREE_CODE (decl) == TREE_LIST)
	    decl = TREE_PURPOSE (decl);
	  if (TYPE_P (decl))
	    decl = TYPE_STUB_DECL (decl);
	  decl = get_module_owner (decl);
	  if (unsigned mod = MAYBE_DECL_MODULE_OWNER (decl))
	    if (!bitmap_bit_p (path_map, mod))
	      {
		bitmap_set_bit (path_map, mod);
		bitmap imports = (*modules)[mod]->imports;
		bitmap_ior_into (visible, imports);
	      }
	}
      *path_map_p = path_map;
    }

  return visible;
}

/* We've just directly imported OTHER.  Update our import/export
   bitmaps.  IS_EXPORT is true if we're reexporting the OTHER.  */

void
module_state::set_import (module_state const *other, bool is_export)
{
  gcc_checking_assert (this != other);

  /* We see OTHER's exports (which include's OTHER).
     If OTHER is the primary interface or a partition we'll see its
     imports.  */
  bitmap_ior_into (imports, other->is_primary () || other->is_partition ()
		   ? other->imports : other->exports);

  if (is_export)
    /* We'll export OTHER's exports.  */
    bitmap_ior_into (exports, other->exports);

  if (is_header () && other->is_header () && mod >= MODULE_IMPORT_BASE)
    /* We only see OTHER's headers if it is header.  */
    bitmap_ior_into (slurp ()->headers, other->slurp ()->headers);
}

/* Return the namespace-scope decl that determines the owning module
   of DECL.  That may be DECL itself, or it may DECL's context, or it
   may be some other DECL (for instance an unscoped enum's CONST_DECLs
   are owned by the TYPE_DECL).  */

tree
get_module_owner (tree decl, bool inst_owner_p)
{
  gcc_checking_assert (TREE_CODE (decl) != NAMESPACE_DECL);

  for (tree ctx;; decl = ctx)
    {
      int use;
      tree ti = node_template_info (decl, use);
      if (use > 0)
	{
	  if (inst_owner_p)
	    return decl;
	  decl = DECL_TEMPLATE_RESULT (TI_TEMPLATE (ti));
	}

      ctx = CP_DECL_CONTEXT (decl);
      if (TREE_CODE (ctx) == NAMESPACE_DECL)
	break;
      if (TYPE_P (ctx))
	{
	  if (tree tn = TYPE_STUB_DECL (ctx))
	    ctx = tn;
	  else
	    /* Always return something, global_namespace is a useful
	       non-owning decl.  */
	    return global_namespace;
	}
    }

  decl = STRIP_TEMPLATE (decl);
  
  /* An enumeration is controlled by its enum-decl.  Its
     enumerations may not have that as DECL_CONTEXT.  */
  if (TREE_CODE (decl) == CONST_DECL
      && TREE_CODE (TREE_TYPE (decl)) == ENUMERAL_TYPE
      /*
	&& DECL_CONTEXT (decl) == DECL_CONTEXT (TYPE_NAME (TREE_TYPE (decl)))*/)
    decl = TYPE_NAME (TREE_TYPE (decl));

  return decl;
}

/* Is it permissible to redeclare an entity with owner FROM.  */

bool
module_may_redeclare (unsigned from)
{
  if (from == MODULE_PURVIEW)
    return true;

  if (from == MODULE_NONE)
    return !module_purview_p ();

  return (*modules)[from]->is_primary ();
}

/* Set the module EXPORT and OWNER fields on DECL.  Only
   namespace-scope entites get this.  */

void
set_module_owner (tree decl)
{
  if (!modules_p ())
    return;

  int use_tpl = -1;
  node_template_info (decl, use_tpl);
  if (use_tpl > 0)
    {
      /* Some kind of specialization.  */
      retrofit_lang_decl (decl);
      DECL_MODULE_OWNER (decl)
	= module_purview_p () ? MODULE_PURVIEW : MODULE_NONE;
      return;
    }

  if (!DECL_NAMESPACE_SCOPE_P (decl))
    return;

  gcc_checking_assert (STRIP_TEMPLATE (decl) == get_module_owner (decl, true));

  // FIXME: Check ill-formed linkage

  if (module_purview_p ())
    {
      retrofit_lang_decl (decl);
      DECL_MODULE_OWNER (decl) = MODULE_PURVIEW;

      if (module_exporting_p ())
	{
	  gcc_assert (TREE_CODE (decl) != NAMESPACE_DECL);
	  DECL_MODULE_EXPORT_P (decl) = true;
	}
    }
}

/* DECL has been implicitly declared, set its module owner from
   FROM.  */

void
set_implicit_module_owner (tree decl, tree from)
{
  if (!modules_p ())
    return;
  if (!DECL_NAMESPACE_SCOPE_P (decl))
    return;

  if (unsigned owner = MAYBE_DECL_MODULE_OWNER (from))
    {
      DECL_MODULE_EXPORT_P (decl) = DECL_MODULE_EXPORT_P (from);
      retrofit_lang_decl (decl);
      DECL_MODULE_OWNER (decl) = owner;
    }
}

/* ENUMTYPE is an unscoped enum in namespace scope.  Fixup its
   CONST_DECLs to match the enum's TYPE_DECL.  */

void
fixup_unscoped_enum_owner (tree enumtype)
{
  tree tdef = TYPE_NAME (enumtype);
  if (unsigned owner = MAYBE_DECL_MODULE_OWNER (tdef))
    {
      bool exported = DECL_MODULE_EXPORT_P (tdef);

      for (tree values = TYPE_VALUES (enumtype); values;
	   values = TREE_CHAIN (values))
	{
	  tree decl = TREE_VALUE (values);

	  DECL_MODULE_EXPORT_P (decl) = exported;
	  retrofit_lang_decl (decl);
	  DECL_MODULE_OWNER (decl) = owner;
	}
    }
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

/* Read the CMI file for a module.  FNAME, if not NULL, is the name we
   know it as.  */

bool
module_state::do_import (char const *fname, cpp_reader *reader)
{
  gcc_assert (global_namespace == current_scope ()
	      && !is_imported () && loc != UNKNOWN_LOCATION);
  unsigned diags = is_direct () ? errorcount + warningcount + 1 : 0;

  if (lazy_open >= lazy_limit)
    freeze_an_elf ();

  if (fname)
    {
      gcc_assert (!filename);
      filename = xstrdup (fname);
    }

  if (mkdeps *deps = cpp_get_deps (reader))
    deps_add_module (deps, get_flatname ());

  int fd = -1;
  int e = ENOENT;
  if (filename)
    {
      const char *file = maybe_add_cmi_prefix (filename);
      dump () && dump ("CMI is %s", file);
      fd = open (file, O_RDONLY | O_CLOEXEC);
      e = errno;
    }

  announce ("importing");
  imported_p = true;
  lazy_open++;
  module_state *alias = read (fd, e, reader);
  bool ok = check_read (diags);
  if (alias)
    {
      slurped ();
      alias_p = true;
      u1.alias = alias;
    }
  announce (flag_module_lazy && mod != MODULE_PURVIEW ? "lazy" : "imported");

  return ok;
}

/* Import this module now.  Fatal error on failure.  LAZY is true if
   we're a lazy pending imports (which will have preserved the line
   map already).  */

void
module_state::direct_import (cpp_reader *reader, bool lazy)
{
  unsigned n = dump.push (this);

  direct_p = true;
  if (!is_imported () && mod == MODULE_UNKNOWN)
    {
      char *fname = NULL;
      unsigned pre_hwm = 0;

      if (!lazy)
	{
	  /* Preserve the state of the line-map.  */
	  pre_hwm = LINEMAPS_ORDINARY_USED (line_table);
	  if (module_has_cmi_p ())
	    spans.close ();

	  maybe_create_loc ();
	  fname = module_mapper::import_export (this, false);
	}

      if (!do_import (fname, reader)
	  && !flag_preprocess_only)
	fatal_error (loc, "returning to gate for a mechanical issue");

      /* Restore the line-map state.  */
      if (!lazy)
	{
	  linemap_module_restore (line_table, pre_hwm);
	  if (module_has_cmi_p ())
	    spans.open ();
	}
    }

  if (is_imported ())
    {
      module_state *imp = resolve_alias ();
      imp->direct_p = true;
      if (exported_p)
	imp->exported_p = true;

      (*modules)[MODULE_NONE]->set_import (imp, imp->exported_p);
      if (imp->is_header ())
	imp->import_macros ();
    }

  dump.pop (n);
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
  if ((!lazy_limit || !PARAM_VALUE (PARAM_LAZY_MODULES))
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
      if (candidate && candidate->u1.slurp && candidate->slurp ()->lru
	  && candidate->from ()->is_freezable ()
	  && (!victim || victim->slurp ()->lru > candidate->slurp ()->lru))
	victim = candidate;
    }

  if (victim)
    {
      dump () && dump ("Freezing '%s'", victim->filename);
      if (victim->slurp ()->macro_defs.size)
	/* Save the macro definitions to a buffer.  */
	victim->from ()->preserve (victim->slurp ()->macro_defs);
      if (victim->slurp ()->macro_tbl.size)
	/* Save the macro definitions to a buffer.  */
	victim->from ()->preserve (victim->slurp ()->macro_tbl);
      victim->from ()->freeze ();
      lazy_open--;
    }
  else
    dump () && dump ("No module available for freezing");
}

/* *SLOT is a lazy binding in namespace NS named ID.  Load it, or die
   trying.  */
// FIXME: Should we emit something when noisy ()?
bool
module_state::lazy_load (tree ns, tree id, mc_slot *mslot, bool outermost)
{
  unsigned n = dump.push (this);

  unsigned snum = mslot->get_lazy ();
  dump () && dump ("Lazily binding %P@%N section:%u", ns, id, name, snum);

  unsigned diags = outermost ? errorcount + warningcount + 1 : 0;
  if (snum < slurp ()->current && flag_module_lazy)
    load_section (snum);

  if (mslot->is_lazy ())
    {
      *mslot = NULL_TREE;
      from ()->set_error (elf::E_BAD_LAZY);
    }

  bool ok = check_read (diags, ns, id);
  gcc_assert (ok || !outermost);
 
  dump.pop (n);

  return ok;
}

/* Load MOD's binding for NS::ID into *MSLOT.  *MSLOT contains the
   lazy cookie.  OUTER is true if this is the outermost lazy, (used
   for diagnostics).  */

void
lazy_load_binding (unsigned mod, tree ns, tree id, mc_slot *mslot, bool outer)
{
  gcc_checking_assert (mod >= MODULE_IMPORT_BASE);
  (*modules)[mod]->lazy_load (ns, id, mslot, outer);
  gcc_assert (!mslot->is_lazy ());
}

module_state *
module_for_unnamed (unsigned unnamed)
{
  unsigned pos = MODULE_IMPORT_BASE;
  unsigned len = modules->length () - pos;
  while (len)
    {
      unsigned half = len / 2;
      module_state *probe = (*modules)[pos + half];
      if (unnamed < probe->unnamed_lwm)
	len = half;
      else if (unnamed < probe->unnamed_lwm + probe->unnamed_num)
	return probe;
      else
	{
	  pos += half + 1;
	  len = len - (half + 1);
	}
    }
  gcc_unreachable ();
}

/* Load any pending specializations of TMPL.  Called just before
   instantiating TMPL.  */

void
lazy_load_specializations (tree tmpl)
{
  gcc_checking_assert (DECL_TEMPLATE_LAZY_SPECIALIZATIONS_P (tmpl));

  tree owner = get_module_owner (tmpl);

  if (specset *set
      = specset::table->lookup (CP_DECL_CONTEXT (owner), DECL_NAME (owner)))
    {
      unsigned n = dump.push (NULL);
      dump () && dump ("Reading %u pending specializations keyed to %N",
		       set->num, owner);
      dump.indent ();
      for (unsigned ix = 0; ix != set->num; ix++)
	{
	  unsigned unnamed = set->pending[ix];
	  unnamed_entity *uent = &(*unnamed_ary)[unnamed];
	  if (uent->slot.is_lazy ())
	    {
	      module_state *module = module_for_unnamed (unnamed);
	      module->lazy_load (NULL, NULL, &uent->slot, true);
	    }
	  else
	    dump () && dump ("Specialization %u already loaded", ix);
	}

      note_loaded_specializations (set->ns, set->name);

      /* We own set, so delete it now.  */
      delete set;
      dump.outdent ();
      dump.pop (n);
    }
}

/* Import the module NAME into the current TU and maybe re-export it.
   Return true if the import affects macro state.  */

bool
import_module (module_state *imp, location_t from_loc, bool exporting,
	       tree, cpp_reader *reader, bool /*in_extern_c*/)
{
  if (module_exporting_p ())
    exporting = true;

  gcc_assert (global_namespace == current_scope ());
  from_loc = ordinary_loc_of (line_table, from_loc);

  if (!imp->check_not_purview (from_loc))
    return false;

  if (imp->is_detached ())
    imp->attach (from_loc);

  if (exporting)
    imp->exported_p = true;

  if (imp->is_header ())
    imp->direct_import (reader, false);
  else
    vec_safe_push (pending_imports, imp);

  return imp->is_header ();
}

/* Declare the name of the current module to be NAME.  EXPORTING_p is
   true if this TU is the exporting module unit.  */

bool
declare_module (module_state *state, location_t from_loc, bool exporting_p,
		tree, cpp_reader *)
{
  gcc_assert (global_namespace == current_scope ());
  from_loc = ordinary_loc_of (line_table, from_loc);

  if (module_purview_p () || !state->is_detached ())
    {
      if (module_purview_p ())
	state = (*modules)[MODULE_PURVIEW];
      error_at (from_loc, module_purview_p ()
		? G_("module already declared")
		: G_("module already imported"));
      inform (state->from_loc,
	      module_purview_p ()
	      ? G_("module %qs declared here")
	      : G_("module %qs imported here"),
	      state->get_flatname ());
      return false;
    }

  gcc_assert (!(*modules)[MODULE_PURVIEW]);

  state->attach (from_loc);

  /* Yer a module, 'arry.  */
  module_kind &= ~MK_GLOBAL;
  module_kind |= MK_MODULE;

  module_state *gmf = (*modules)[MODULE_NONE];
  if (state->is_partition () || exporting_p)
    {
      if (state->is_partition ())
	module_kind |= MK_PARTITION;

      if (exporting_p)
	{
	  state->interface_p = true;
	  module_kind |= MK_INTERFACE;
	}

      if (state->is_header ())
	module_kind |= MK_GLOBAL | MK_EXPORTING;

      /* Copy the importing information we may have already done.  */
      state->imports = gmf->imports;

      state->mod = MODULE_PURVIEW;
      gmf = state;
      (*modules)[MODULE_NONE] = state;
    }
  else
    {
      state->primary_p = state->interface_p = true;
      gmf->parent = state; /* So mangler knows module identity.  */
    }

  (*modules)[MODULE_PURVIEW] = gmf;

  vec_safe_push (pending_imports, state);

  return true;
}

/* Track if NODE undefs an imported macro.  */

void
module_cpp_undef (cpp_reader *reader, location_t loc, cpp_hashnode *node)
{
  if (!header_module_p ())
    {
      /* Turn us off.  */
      struct cpp_callbacks *cb = cpp_get_callbacks (reader);
      if (cb->undef == lang_hooks.preprocess_undef)
	{
	  cb->undef = NULL;
	  lang_hooks.preprocess_undef = NULL;
	}
    }
  if (lang_hooks.preprocess_undef)
    module_state::undef_macro (reader, loc, node);
}

cpp_macro *
module_cpp_deferred_macro (cpp_reader *reader, location_t loc,
			   cpp_hashnode *node)
{
  return module_state::deferred_macro (reader, loc, node);
}

/* NAME & LEN are a preprocessed header name, including the
   surrounding "" or <> characters.  Return the raw string name of the
   module to which it refers.  This will be an absolute path, or begin
   with ., so it is immediately distinguishable from a (non-header
   unit) module name.  If SEARCH is true, ask the preprocessor to
   locate the header to which it refers using the appropriate include
   path.  Note that we do never do \ processing of the string, as that
   matches the preprocessor's behaviour.  */

static const char *
canonicalize_header_name (cpp_reader *reader, location_t loc, bool unquoted,
			  const char *str, size_t &len_r)
{
  size_t len = len_r;
  gcc_checking_assert (unquoted
		       || (len >= 2
			   && (reader || str[0] == '"')
			   && ((str[0] == '<' && str[len-1] == '>')
			       || (str[0] == '"' && str[len-1] == '"'))));
  static char *buf = 0;
  static size_t alloc = 0;

  if (reader)
    {
      if (len >= alloc)
	{
	  alloc = len * 2;
	  buf = XRESIZEVEC (char, buf, alloc);
	}
      len -= 2;
      memcpy (buf, str + 1, len);
      buf[len] = 0;
      if (const char *hdr
	  = cpp_find_header_unit (reader, buf, str[0] == '<', loc))
	{
	  len = strlen (hdr);
	  str = hdr;
	}
    }
  else if (!unquoted)
    {
      str += 1;
      len -= 2;
    }

  /* Non-searched paths, should have already gone through this check,
     but perhaps the user did something strange with preprocessed source?  */
  if (!IS_ABSOLUTE_PATH (str)
      && !(str[0] == '.' && IS_DIR_SEPARATOR (str[1])))
    {
      if (len >= alloc)
	{
	  alloc = len * 2;
	  buf = XRESIZEVEC (char, buf, alloc);
	}
      buf[0] = '.';
      buf[1] = DIR_SEPARATOR;
      memcpy (buf + 2, str, len);
      len += 2;
      buf[len] = 0;
      str = buf;
    }

  len_r = len;
  return str;
}

tree
module_map_header (cpp_reader *reader, location_t loc, bool search,
		   const char *str, size_t len)
{
  str = canonicalize_header_name (search ? reader : NULL, loc, false, str, len);
  return build_string (len, str);
}

/* Figure out whether to treat HEADER as an include or an import.  */

bool
module_translate_include (cpp_reader *reader, line_maps *lmaps, location_t loc,
			  const char *path)
{
  if (!modules_p ())
    {
      /* Turn off.  */
      cpp_get_callbacks (reader)->translate_include = NULL;
      return false;
    }

  if (!spans.init_p ())
    /* Before the main file, don't divert.  */
    return false;

  dump.push (NULL);

  dump (dumper::MAPPER) && dump ("Checking include translation '%s'", path);
  bool res = false;
  module_mapper *mapper = module_mapper::get (loc);
  if (mapper->is_live ())
    {
      size_t len = strlen (path);
      path = canonicalize_header_name (NULL, loc, true, path, len);
      res = mapper->translate_include (loc, path);
    }

  dump (dumper::MAPPER) && dump (res ? "Translating include to import"
				 : "Keeping include as include");

  if (res)
    {
      /* Push the translation text.  */
      loc = ordinary_loc_of (lmaps, loc);
      const line_map_ordinary *map
	= linemap_check_ordinary (linemap_lookup (lmaps, loc));
      unsigned col = SOURCE_COLUMN (map, loc);
      col -= (col != 0); /* Columns are 1-based.  */

      size_t len = strlen (path);
      char *res = XNEWVEC (char, len + 60 + col);

      /* Internal keyword to permit use inside extern "C" {...}.
	 Bad glibc! No biscuit!  */
      strcpy (res, "__import");
      size_t actual = 8;
      if (col > actual)
	{
	  memset (res + actual, ' ', col - actual);
	  actual = col;
	}
      /* No need to encode characters, that's not how header names are
	 handled.  */
      res[actual++] = '"';
      memcpy (res + actual, path, len);
      actual += len;
      res[actual++] = '"';
      strcpy (res + actual, ";\n\n");
      actual += 3;
      /* cpplib will delete the buffer.  */
      cpp_push_buffer (reader, reinterpret_cast<unsigned char *> (res),
		       actual, false);
    }
  dump.pop (0);

  return res;
}

void
module_preprocess (mkdeps *deps, module_state *state, int is_module)
{
  if (is_module)
    /* Record the module, so that partition imports resolve
       correctly.  */
    (*modules)[MODULE_PURVIEW] = state;

  if (!state->flatname)
    state->set_flatname ();

  const char *path = NULL;
  if (is_module > 0
      /* Partitions always produce a CMI.  */
      || (is_module < 0 && state->is_partition ()))
    {
      path = state->filename;
      if (!path)
	path = module_mapper::import_export (state, true);
      path = path ? maybe_add_cmi_prefix (path) : "";
    }

  deps_add_module (deps, state->get_flatname (),
		   path, state->is_header ());
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
      if (flag_header_unit)
	{
	  /* Tell the preprocessor this is an include file.  */
	  cpp_retrofit_as_include (reader);
	  /* Set the module header name from the main_input_filename.  */
	  const char *main = main_input_filename;
	  size_t len = strlen (main);
	  main = canonicalize_header_name (NULL, 0, true, main, len);
	  module_state *state = get_module (build_string (len, main));
	  if (!flag_preprocess_only)
	    {
	      declare_module (state, spans.main_start (), true, NULL, reader);
	      process_deferred_imports (reader);
	    }
	  else if (mkdeps *deps = cpp_get_deps (reader))
	    module_preprocess (deps, state, 1);
	}
    }
}

/* Process any deferred imports.   */

void
process_deferred_imports (cpp_reader *reader)
{
  if (!vec_safe_length (pending_imports))
    return;

  dump.push (NULL);
  dump () && dump ("Processing %u deferred imports",
		   vec_safe_length (pending_imports));

  /* Preserve the state of the line-map.  */
  unsigned pre_hwm = LINEMAPS_ORDINARY_USED (line_table);
  if (module_has_cmi_p ())
    spans.close ();

  module_state *imp = (*pending_imports)[0];
  module_mapper *mapper = module_mapper::get (imp->from_loc);
  bool has_bmi = imp->mod == MODULE_PURVIEW;
  bool any = false;

  if (mapper->is_server ())
    /* Send batched request to mapper.  */
    for (unsigned ix = 0; ix != pending_imports->length (); ix++)
      {
	imp = (*pending_imports)[ix];
	if (!imp->filename && !imp->imported_p)
	  {
	    /* The user may be directly importing the same module
	       twice in a single block (they do this kind of thing).
	       We only want one filename request, abuse the imported
	       flag to do that.  */
	    imp->imported_p = true;
	    if (!any)
	      mapper->cork ();
	    any = true;
	    mapper->imex_query (imp, !ix && has_bmi);
	  }
      }

  if (any)
    mapper->uncork (imp->from_loc);

  for (unsigned ix = 0; ix != pending_imports->length (); ix++)
    {
      imp = (*pending_imports)[ix];

      /* Read the mapper's responses.  */
      if (any && !imp->filename)
	{
	  imp->imported_p = false;
	  if (char *fname = mapper->imex_response (imp))
	    imp->filename = xstrdup (fname);
	}

      imp->maybe_create_loc ();
    }

  if (any)
    mapper->maybe_uncork (imp->loc);

  /* Now do the importing, which might cause additional requests
     (although nested import filenames are usually in their
     importer's import table).  */
  for (unsigned ix = has_bmi ? 1 : 0;
       ix != pending_imports->length (); ix++)
    {
      module_state *imp = (*pending_imports)[ix];
      imp->direct_import (reader, true);
    }

  dump.pop (0);

  vec_free (pending_imports);

  linemap_module_restore (line_table, pre_hwm);
  if (module_has_cmi_p ())
    spans.open ();
}

/* VAL is a global tree, add it to the global vec if it is
   interesting.  Add some of its targets, if they too are
   interesting.  */

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
init_module_processing (cpp_reader *reader)
{
  /* PCH should not be reachable because of lang-specs, but the
     user could have overriden that.  */
  if (pch_file)
    fatal_error (input_location,
		 "C++ modules are incompatible with precompiled headers");
  if (flag_use_repository)
    fatal_error (input_location,
		 "C++ modules are incompatible with a template repository");

  if (flag_preprocess_only)
    {
      cpp_options *cpp_opts = cpp_get_options (reader);
      if (flag_no_output
	  || (cpp_opts->deps.style != DEPS_NONE
	      && !cpp_opts->deps.need_preprocessor_output))
	{
	  warning (0, flag_dump_macros == 'M'
		   ? G_("macro debug output may be incomplete with modules")
		   : G_("module dependencies require full preprocessing"));
	  if (cpp_opts->deps.style != DEPS_NONE)
	    inform (input_location, "you should use the %<-%s%> option",
		    cpp_opts->deps.style == DEPS_SYSTEM ? "MD" : "MMD");
	}
    }

  /* :: is always exported.  */
  DECL_MODULE_EXPORT_P (global_namespace) = true;

  modules_hash = hash_table<module_state_hash>::create_ggc (31);

  vec_safe_reserve (modules, 20);
  for (unsigned ix = MODULE_IMPORT_BASE; ix--;)
    modules->quick_push (NULL);

  /* Create module for current TU.  */
  module_state *current
    = new (ggc_alloc<module_state> ()) module_state (NULL_TREE, NULL, false);
  current->mod = MODULE_NONE;
  bitmap_set_bit (current->imports, MODULE_NONE);
  (*modules)[MODULE_NONE] = current;

  gcc_checking_assert (!fixed_trees);

  headers = BITMAP_GGC_ALLOC ();

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

    if (unsigned parm = PARAM_VALUE (PARAM_LAZY_MODULES))
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
  vec_alloc (fixed_trees, 200);

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
  global_crc = crc32_unsigned (crc, fixed_trees->length ());
  dump ("") && dump ("Created %u unique globals, crc=%x",
		     fixed_trees->length (), global_crc);
  for (unsigned ix = fixed_trees->length (); ix--;)
    TREE_VISITED ((*fixed_trees)[ix]) = false;

  dump.pop (0);

  if (!flag_module_lazy)
    /* Get the mapper now, if we're not being lazy.  */
    module_mapper::get (BUILTINS_LOCATION);

  if (!flag_preprocess_only)
    {
      unnamed_map = new unnamed_map_t (31);
      specset::table = new specset::hash (400);
    }

#if CHECKING_P
  note_defs = new hash_set<tree> (1000);
#endif

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

/* Write the CMI, if we're a module interface.  */

void
finish_module_processing (cpp_reader *reader)
{
  if (header_module_p ())
    module_kind &= ~MK_EXPORTING;

  if (flag_module_macros)
    {
      /* Force loading of any remaining deferred macros.  This will
	 produce diagnostics if they are ill-formed.  */
      unsigned n = dump.push (NULL);
      cpp_forall_identifiers (reader, load_macros, NULL);
      dump.pop (n);
    }

  module_state *state = NULL;
  if (modules_p ())
    state = (*modules)[MODULE_PURVIEW];

  if (!state || state->mod != MODULE_PURVIEW)
    {
      if (flag_module_only)
	warning (0, "%<-fmodule-only%> used for non-interface");
    }
  else
    {
      int fd = -1;
      int e = ENOENT;

      spans.close ();
      /* Force a valid but empty line map at the end.  This simplifies
	 the line table preparation and writing logic.  */
      linemap_add (line_table, LC_ENTER, false, "", 0);

      /* We write to a tmpname, and then atomically rename.  */
      const char *path = NULL;
      char *tmp_name = NULL;

      if (state->filename)
	{
	  size_t len = 0;
	  path = maybe_add_cmi_prefix (state->filename, &len);
	  tmp_name = XNEWVEC (char, len + 3);
	  memcpy (tmp_name, path, len);
	  strcpy (&tmp_name[len], "~");

	  if (mkdeps *deps = cpp_get_deps (reader))
	    deps_add_module (deps, state->get_flatname (), path,
			     state->is_header ());

	  if (!errorcount)
	    for (unsigned again = 2; ; again--)
	      {
		fd = open (tmp_name, O_RDWR | O_CREAT | O_TRUNC | O_CLOEXEC,
			   S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
		e = errno;
		if (fd >= 0 || !again || e != ENOENT)
		  break;
		create_dirs (tmp_name);
	      }
	}
      unsigned n = dump.push (state);
      state->announce ("creating");
      dump () && dump ("CMI is %s", path);

      if (errorcount)
	warning_at (state->from_loc, 0,
		    "not writing module %qs due to errors",
		    state->get_flatname ());
      else
	{
	  elf_out to (fd, e);
	  if (to.begin ())
	    state->write (&to, reader);
	  if (to.end ())
	    if (rename (tmp_name, path))
	      to.set_error (errno);

	  if (to.get_error ())
	    {
	      error_at (state->from_loc,
			"failed to write compiled module: %s",
			to.get_error (state->filename));
	      state->note_cmi_name ();
	    }
	}

      if (!errorcount)
	module_mapper::export_done (state);
      else if (path)
	{
	  /* We failed, attempt to erase all evidence we even tried.  */
	  unlink (tmp_name);
	  unlink (path);
	  XDELETEVEC (tmp_name);
	}

      dump.pop (n);
      ggc_collect ();
    }

  /* We're done with the macro tables now.  */
  vec_free (macro_exports);
  vec_free (macro_imports);
  headers = NULL;

  /* We're now done with everything but the module names.  */
  set_cmi_repo (NULL);
  module_mapper::fini (input_location);
  module_state_config::release ();

#if CHECKING_P
  delete note_defs;
  note_defs = NULL;
#endif

  if (modules)
    for (unsigned ix = modules->length (); --ix >= MODULE_IMPORT_BASE;)
      if (module_state *state = (*modules)[ix])
	state->release ();

  /* No need to lookup modules anymore.  */
  modules_hash = NULL;

  /* Or unnamed entitites.  */
  delete unnamed_map;
  unnamed_map = NULL;
  unnamed_ary = NULL;

  /* Or remember any pending specializations.  */
  delete specset::table;
  specset::table = NULL;

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
  switch (opt_code (code))
    {
    case OPT_fmodule_mapper_:
      module_mapper_name = str;
      return true;

    case OPT_fmodule_header:
      flag_header_unit = 1;
      flag_modules = 1;
      return true;

    default:
      return false;
    }
}

#include "gt-cp-module.h"
