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

   There are two different modules proposal, TS and ATOM.  Supporting
   both causes complications, but such is the price of
   experimentation.  You may not mix compilations of the two schemes.
   
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

   OPTIMIZATION: Outside of the current TU, we only need ADL to work.
   We could optimize regular lookup for the current TU by glomming all
   the visible decls on its slot.  Perhaps wait until design is a
   little more settled though.

   There is only one instance of each extern-linkage namespace.  It
   appears in every module slot that makes it visible.  It also
   appears in MODULE_SLOT_GLOBAL. (it is an ODR violation if they
   collide with some other global module entity.) FIXME:Not yet implemented

   A module import can bring in entities that cannot be found by name
   lookup.  You use decltype tricks to get at it.  I am not sure
   whether these should be DECL_HIDDEN for that import's binding, or
   should just not be in the symbol table.

   A module interface compilation produces a Binary Module Interface
   (BMI).  I use ELROND format, which allows a bunch of named sections
   containing arbitrary data.  Although I don't defend against
   actively hostile BMIs, there is some checksumming involved to
   verify data integrity.  When dumping out an interface, we generate
   a list of all the namespace-scope DECLS that are needed.  From that
   we determine the strongly connected components (SCC) within this
   TU.  Each SCC is dumped to a separate section of the BMI.  We
   generate a binding table section, mapping each namespace&name to a
   defining section.  This allows lazy loading.

   Notice this means we embed section indices into the contents of
   other sections.  Thus random manipulation of the BMI file by ELF
   tools may well break it.  The kosher way would probably be to
   introduce indirection via section symbols, but that would require
   defining a relocation type.

   References to decls not in the same SCC are by two different
   mechanisms.  The simplest is for extern or module linkage entities,
   which are by module, context, name & type.  We look in exactly that
   scope, potentially lazily load it.  Other cases are by a per-module
   vector of such decls.  Again, slots in this may be lazily loaded.
   The three cases are:

   * Local linkage entities, which will be subject to linkage
   promotion.  FIXME: Linkage promotion not implemented.

   * Global module entity.  We must merge global decls across
   modules.  FIXME: Not implemented

   * Voldemort types.  These are unspellable types -- a local class
   from a function for instance.  We generate a table of these and
   refer to them by index.  FIXME: indirect cases not implemented.

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

   module_state - module object

   slurping - data needed during loading
   spewing : slurping - data needed during interface writing

   module_mapper - mapper object

   The ELROND objects use mmap, for both reading and writing.  If mmap
   is unavailable, fileno IO is used to read and write blocks of data.

   The mapper object uses fileno IO to communicate with the server or
   program.  We batch requests in ATOM mode to reduce the number of
   round trips.

   I have a confession: tri-valued bools are not the worst thing in
   this file.  */

/* MODULE_STAMP is a #define passed in from the Makefile.  When
   present, it is used for version stamping the binary files, and
   indicates experimentalness of the module system.  It is very
   experimental right now.  */
#ifndef MODULE_STAMP
#error "Stahp! What are you doing? This is not ready yet."
#define MODULE_STAMP 0
#endif

/* Mapper Protocol version.  Very new.  */
#define MAPPER_VERSION 0

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
#include "stor-layout.h"
#include "version.h"
#include "tree-diagnostic.h"
#include "params.h"
#include "toplev.h"
#include "opts.h"
#include "attribs.h"
#include "intl.h"

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

#if HAVE_MMAP_FILE && _POSIX_MAPPED_FILES > 0
/* mmap, munmap.  */
#define MAPPED_READING 1
#if HAVE_SYSCONF && defined (_SC_PAGE_SIZE)
/* msync, sysconf (_SC_PAGE_SIZE), ftruncate  */
/* posix_fallocate used if available.  */
#define MAPPED_WRITING 1
#endif
#endif

/* Id for dumping module information.  */
int module_dump_id;

/* We have a few more special module owners.  */
#define MODULE_UNKNOWN (~0U)    /* Not yet known.  */

/* Prefix for section names.  (Not system-defined, so no leading dot.)  */
#define MOD_SNAME_PFX "gnu.c++"

/* Get the version of this compiler.  This is negative, when it is a
   date-time stamp indicating experimentalness of the system.  See
   above about MODULE_STAMP.  */

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

static inline int
version2date (int v)
{
  if (v < 0)
    return unsigned (-v) / 10000 + 20000000;
  else
    return v;
}

/* Version to time.  Only understand times when experimental.  */

static inline unsigned
version2time (int v)
{
  if (MODULE_STAMP && v < 0)
    return unsigned (-v) % 10000;
  else
    return 0;
}

/* Format a version for user consumption.  Only attach time
   information for experimental builds.  */

typedef char verstr_t[32];
static void
version2string (int version, verstr_t &out)
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
struct nodel_ptr_hash : pointer_hash<T>, typed_noop_remove <T *> {
  /* Nothing is deletable.  Everything is insertable.  */
  static bool is_deleted (T *) { return false; }
  static void mark_deleted (T *) { gcc_unreachable (); }
};

/* Map from pointer to signed integer.   */
typedef simple_hashmap_traits<nodel_ptr_hash<void>, int> ptr_int_traits;
typedef hash_map<void *,signed,ptr_int_traits> ptr_int_hash_map;

/* Variable length buffer. */

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
    needed = MODULE_STAMP ? 100 : 10000;

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
  const char *buf (size_t); /* Read a fixed-length buffer.  */
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
  void str (const char *, size_t);  /* Write string of known length.  */
  void buf (const char *, size_t);  /* Write fixed length buffer.  */

public:
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

/* If CRC_PTR non-null, set the CRC of the buffer.  Mix the CRC into
   that pointed to by CRC_PTR.  Otherwise store zero.  */

void
bytes_out::set_crc (unsigned *crc_ptr)
{
  gcc_checking_assert (pos >= 4);
  unsigned crc = 0;
  if (crc_ptr)
    {
      crc = calc_crc (pos);
      unsigned accum = *crc_ptr;
      /* Only mix the existing *CRC_PTR if it is non-zero.  */
      accum = accum ? crc32_unsigned (accum, crc) : crc;
      *crc_ptr = accum;
    }
  *(unsigned *)buffer = crc;
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

void
bytes_out::buf (const char *buf, size_t len)
{
  align (sizeof (void *) * 2);
  if (char *ptr = write (len))
    memcpy (ptr, buf, len);
}

const char *
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
  buf (string, len + 1);
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
  const char *str = buf (len + 1);
  if (!str || str[len])
    {
      set_overrun ();
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
      E_BAD_LAZY = -2,  /* Badly ordered laziness.  */
      E_BAD_IMPORT = -3 /* A nested import failed.  */
    };

protected:
  /* File identification.  On-disk representation.  */
  struct ident {
    uint8_t magic[4];
    uint8_t klass; /* 4:CLASS32 */
    uint8_t data; /* 5:DATA2[LM]SB */
    uint8_t version; /* 6:EV_CURRENT  */
    uint8_t osabi; /* 7:OSABI_NONE */
    uint8_t abiver; /* 8: 0 */
    uint8_t pad[7]; /* 9-15 */
  };
  /* File header.  On-disk representation.  */
  struct header {
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
  struct section {
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
  data hdr;	  /* The header.  */
  data sectab; 	/* The section table.  */
  data strtab;  /* String table.  */
  int fd;   /* File descriptor we're reading or writing.  */
  int err; 	  /* Sticky error code.  */

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
  int has_error () const
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
    return !has_error ();
  }
  /* Finish reading/writing file.  Return NULL or error string.  */
  bool end ();
};

/* Return error string.  */

const char *
elf::get_error (const char *name) const
{
  if (!name)
    return "Unknown BMI mapping";

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

  return has_error ();
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

public:
  static void grow (data &data, unsigned needed)
  {
    gcc_checking_assert (!data.buffer);
#ifndef MAPPED_READING
    data.buffer = XNEWVEC (char, needed);
#endif
    data.size = needed;
  }
  static void shrink (data &data)
  {
#ifndef MAPPED_READING
    XDELETEVEC (data.buffer);
#endif
    data.buffer = NULL;
    data.size = 0;
  }

public:
  const section *get_section (unsigned s) const
  {
    if (s * sizeof (section) < sectab.size)
      return reinterpret_cast <const section *>
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
#ifdef MAPPED_READING
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
#ifdef MAPPED_WRITING
  unsigned offset;		/* Offset of the mapping.  */
  unsigned extent;		/* Length of mapping.  */
  unsigned page_size;		/* System page size.  */
#endif

public:
  elf_out (int fd, int e)
    :parent (fd, e), identtab (500), pos (0)
  {
#ifdef MAPPED_WRITING
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

#ifdef MAPPED_WRITING
private:
  void create_mapping (unsigned ext, bool extending = true);
  void remove_mapping ();
#endif

protected:
  using allocator::grow;
  virtual char *grow (char *, unsigned needed);
#ifdef MAPPED_WRITING
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
#ifdef MAPPED_WRITING
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
#ifdef MAPPED_READING
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
#ifdef MAPPED_READING
      if (ok)
	{
	  char *mapping = reinterpret_cast <char *>
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
#ifdef MAPPED_READING
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
#ifdef MAPPED_READING  
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
	= reinterpret_cast <const section *> (&sectab.buffer[pos]);

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

#ifdef MAPPED_READING
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

  const header *h = reinterpret_cast <const header *> (hdr.buffer);
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

#ifdef MAPPED_WRITING
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
	mapping = mmap (NULL, ext * 2, PROT_READ + PROT_WRITE,
			MAP_SHARED, fd, offset);
      if (mapping != MAP_FAILED)
	ext *= 2;
    }
  if (mapping == MAP_FAILED)
    {
      if (!extending || !posix_fallocate (fd, offset, ext))
	mapping = mmap (NULL, ext, PROT_READ + PROT_WRITE,
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

#ifdef MAPPED_WRITING
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
#ifndef MAPPED_WRITING
	  /* Align the section on disk, should help the necessary copies.
	     fseeking to extend is non-portable.  */
	  static char zero[SECTION_ALIGN];
	  if (::write (fd, &zero, padding) != padding)
	    set_error (errno);
#endif
	  pos += padding;
	}
#ifdef MAPPED_WRITING
      data = hdr.buffer + (pos - offset);
#endif
    }

#ifdef MAPPED_WRITING
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

#ifdef MAPPED_WRITING
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
  gcc_checking_assert (identifier_p (ident));
  bool existed;
  int *slot = &identtab.get_or_insert (ident, &existed);
  if (!existed)
    *slot = strtab_write (IDENTIFIER_POINTER (ident),
			  IDENTIFIER_LENGTH (ident) + 1);
  return *slot;
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
  section *sec = reinterpret_cast <section *> (sectab.buffer + sectab.pos);
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
#ifdef MAPPED_WRITING
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

#ifdef MAPPED_WRITING
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

#ifdef MAPPED_WRITING
  /* Start a mapping.  */
  create_mapping (MODULE_STAMP ? page_size
		  : (32767 + page_size) & ~(page_size - 1));
  if (!hdr.buffer)
    return false;
#endif

  /* Write an empty header.  */
  grow (hdr, sizeof (header), true);
  header *h = reinterpret_cast <header *> (hdr.buffer);
  memset (h, 0, sizeof (header));
  hdr.pos = hdr.size;
  write (hdr);
  return has_error () == 0;
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
	  reinterpret_cast <section *> (sectab.buffer)->link = strndx;
	  strndx = SHN_XINDEX;
	}
      unsigned shnum = sectab.pos / sizeof (section);
      if (shnum >= SHN_LORESERVE)
	{
	  reinterpret_cast <section *> (sectab.buffer)->size = shnum;
	  shnum = SHN_XINDEX;
	}

      unsigned shoff = write (sectab);

#ifdef MAPPED_WRITING
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
      if (!has_error ())
	{
	  /* Write the correct header now.  */
	  header *h = reinterpret_cast <header *> (hdr.buffer);
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

#ifdef MAPPED_WRITING
      remove_mapping ();
      if (ftruncate (fd, length))
	set_error (errno);
#endif
    }

  data::simple_memory.shrink (sectab);
  data::simple_memory.shrink (strtab);

  return parent::end ();
}

/* A dependency set.  These are not quite the decl-sets of the TS.  We
   only record namespace-scope decls here.   A depset can be one of:

   1) A namespace binding, keyed by {ns,name} tuple.  The
   dependencies are the declarations on that binding.

   2) A namespace-scope declaration, keyed by {decl,NULL} tuple.  The
   dependencies are those of the declaration.  One of which will be
   the namespace-binding's depset.

   3) A namespace-scope definition, keyed by {decl,decl} tuple.  The
   dependencies are those of the definition.  One of which will be the
   declaration's depset.

   If a depset is dependent on a namespace-scope decl, we add depset
   #2 to the dependencies.  If its dependent on the body of such a decl,
   we add depset #3 to the dependencies.  */

class depset {
public:
  typedef std::pair<tree,tree> key_type;

private:
  key_type key;

public:
  vec<depset *> deps;  /* Depsets in this TU we reference.  */

public:
  unsigned cluster : 31; /* Strongly connected cluster.  */
  bool is_unnamed : 1;   /* This decl is not found by name.  */
  unsigned section : 31; /* Section written to.  */
  bool refs_unnamed : 1;  /* A dependency is not found by name.  */
  /* During SCC construction, section is lowlink, until the depset is
     removed from the stack.   */

public:
  inline operator const key_type & () const
  {
    return key;
  }
  /* A binding key -- NAMESPACE_DECL & IDENTIFIER.  */
  inline static key_type binding_key (tree decl, tree name)
  {
    gcc_checking_assert (TREE_CODE (decl) == NAMESPACE_DECL
			 && TREE_CODE (name) == IDENTIFIER_NODE);
    return key_type (decl, name);
  }
  /* A declaration key -- namespace-scope DECL.  */
  inline static key_type decl_key (tree decl)
  {
    return key_type (decl, NULL_TREE);
  }
  /* A definition key -- namespace-scope DECL.  */
  inline static key_type defn_key (tree defn, bool is_def = true)
  {
    /* Create a decl key.  */
    key_type res = decl_key (defn);
    if (is_def)
      {
	/* And turn it into a defn key.  */
	gcc_checking_assert (TREE_CODE (defn) != NAMESPACE_DECL
			     || DECL_NAMESPACE_ALIAS (defn));
	res.second = res.first;
      }
    return res;
  }

public:
  depset (const key_type &key);
  ~depset ();

public:
  bool is_binding () const
  {
    return !is_decl () && !is_defn ();
  }
  bool is_decl () const
  {
    return !key.second;
  }
  bool is_defn () const
  {
    return key.first == key.second;
  }

public:
  tree get_decl () const
  {
    return key.first;
  }
  tree get_name () const
  {
    gcc_checking_assert (is_binding ());
    return key.second;
  }

public:
  /* Traits for a hash table of pointers to bindings.  */
  struct traits {
    /* Each entry is a pointer to a binding. */
    typedef depset *value_type;
    /* We lookup by container:maybe-identifier pair.  */
    typedef key_type compare_type;

    /* Hash by pointer value.  */
    inline static hashval_t hash (tree container, tree name)
    {
      hashval_t res = pointer_hash<tree_node>::hash (container);
      hashval_t name_hash = pointer_hash<tree_node>::hash (name);
      return iterative_hash_hashval_t (res, name_hash);
    }

    /* hash and equality for compare_type.  */
    inline static hashval_t hash (const compare_type &p)
    {
      hashval_t a = pointer_hash<tree_node>::hash (p.first);
      hashval_t b = pointer_hash<tree_node>::hash (p.second);
      return iterative_hash_hashval_t (a, b);
    }
    inline static bool equal (const value_type b, const compare_type &p)
    {
      return b->key == p;
    }

    /* (re)hasher for a binding itself.  */
    inline static hashval_t hash (const value_type b)
    {
      return hash (b->key);
    }
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
    bool sneakoscope;        /* Detecting dark magic (of a voldemort type).  */

  public:
    hash (size_t size)
      : parent (size), worklist (), current (NULL), sneakoscope (false)
    {
      worklist.reserve (size);
    }
    ~hash ()
    {
    }

  private:
    void insert (depset *d)
    {
      depset **slot = maybe_insert (*d);
      gcc_checking_assert (!*slot);
      *slot = d;
    }
    depset **maybe_insert (const key_type &, bool = true);
    depset *maybe_add_declaration (tree decl);

  public:
    depset *find (const key_type &);

  public:
    depset *add_dependency (tree decl, int kind);
    tree add_binding (tree ns, tree name, auto_vec<tree> &decls);
    depset *get_work ()
    {
      current = worklist.length () ? worklist.pop () : NULL;
      return current;
    }
  };

public:
  struct tarjan {
    auto_vec<depset *> *result;
    vec<depset *> stack;
    unsigned index;

    tarjan (auto_vec<depset *> &result)
      : result (&result), stack (), index (0)
    {
    }
    ~tarjan () 
    {
      gcc_assert (!stack.length ());
    }

  public:
    void connect (depset *);
  };
};

depset::depset (const key_type &key)
  :key (key), deps (), cluster (0), is_unnamed (false),
   section (0), refs_unnamed (false)
{
}

depset::~depset ()
{
}

/* Tree tags.  */
enum tree_tag {
  tt_null,		/* NULL_TREE.  */
  tt_fixed,		/* Fixed vector index.  */
  tt_node,		/* New node.  */
  tt_id,  		/* Identifier node.  */
  tt_conv_id,		/* Conversion operator name.  */
  tt_tinfo_var,		/* Typeinfo object. */
  tt_tinfo_typedef,	/* Typeinfo typedef.  */
  tt_named_type,	/* TYPE_DECL for type.  */
  tt_named_decl,  	/* Named decl. */
  tt_inst,		/* A template instantiation.  */
  tt_binfo,		/* A BINFO.  */
  tt_as_base,		/* An As-Base type.  */
  tt_vtable		/* A vtable.  */
};

/* Tree stream reader.  Note that reading a stream doesn't mark the
   read trees with TREE_VISITED.  Thus it's quite safe to have
   multiple concurrent readers.  Which is good, because lazy
   loading. */
class trees_in : public bytes_in {
  typedef bytes_in parent;

private:
  module_state *state;		/* Module being imported.  */
  auto_vec<tree> back_refs;	/* Back references.  */

public:
  trees_in (module_state *);
  ~trees_in ();

public:
  int insert (tree);

private:
  tree finish_type (tree);

private:
  tree start (tree_code);
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
  tree tree_binfo ();
  bool tree_node_raw (tree);

public:
  tree chained_decls ();  /* Follow DECL_CHAIN.  */
  vec<tree, va_gc> *tree_vec (); /* vec of tree.  */
  vec<tree_pair_s, va_gc> *tree_pair_vec (); /* vec of tree_pair.  */

public:
  /* Read a tree node.  */
  tree tree_node ();
};

trees_in::trees_in (module_state *state)
  :parent (), state (state), back_refs (500)
{
}

trees_in::~trees_in ()
{
}

/* Tree stream writer.  */
class trees_out : public bytes_out {
  typedef bytes_out parent;

private:
  module_state *state;		/* The module we are writing.  */
  ptr_int_hash_map tree_map; 	/* Trees to references */
  depset::hash *dep_hash;    	/* Dependency table.  */
  int ref_num;			/* Back reference number.  */

public:
  trees_out (allocator *, module_state *);
  ~trees_out ();

public:
  bool depending_p () const
  {
    return dep_hash != NULL;
  }

private:
  void mark_trees ();
  void unmark_trees ();

public:
  void begin ();
  unsigned end (elf_out *sink, unsigned name, unsigned *crc_ptr);

public:
  void begin (depset::hash *);
  void end ();

public:
  /* Mark a node for by-value streaming.  */
  void mark_node (tree);

private:
  void tag (int rt)
  {
    records++;
    i (rt);
  }
public:
  int insert (tree, bool = false);
  int maybe_insert_typeof (tree);

private:
  void start (tree_code, tree);

public:
  void core_bools (tree);

private:
  void core_vals (tree);
  void lang_type_bools (tree);
  void lang_type_vals (tree);
  void lang_decl_bools (tree);
  void lang_decl_vals (tree);
  tree tree_binfo (tree, int, bool);
  void tree_node_raw (tree);

public:
  void chained_decls (tree);
  void tree_vec (vec<tree, va_gc> *);
  void tree_pair_vec (vec<tree_pair_s, va_gc> *);

public:
  void tree_node (tree);
  void tree_value (tree, bool force);
  bool tree_decl (tree, bool force, bool looking_inside,
		  unsigned owner = MODULE_UNKNOWN);
  bool tree_type (tree, bool force, bool looking_inside,
		  unsigned owner = MODULE_UNKNOWN);
  int tree_ref (tree);
  void tree_ctx (tree, bool looing_inside, unsigned owner = MODULE_UNKNOWN);

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

trees_out::trees_out (allocator *mem, module_state *state)
  :parent (mem), state (state), tree_map (500),
   dep_hash (NULL), ref_num (0)
{
}

trees_out::~trees_out ()
{
}

/* Likewise, this cannot be a member of module_state.  */

typedef std::pair<unsigned,unsigned> range_t;

/* Data needed by a module during the process of loading.  */
struct GTY((tag("true"), desc ("%h.from != NULL"))) slurping {
  vec<mc_slot, va_gc> *unnamed;		/* Unnamed decls.  */
  vec<unsigned, va_gc_atomic> *remap;	/* Module owner remapping.  */
  elf_in *GTY((skip)) from;     	/* The elf loader.  */

  vec<const char *, va_gc_atomic> *filenames;
  range_t GTY((skip)) early_locs;	/* Early locs.  */
  range_t GTY((skip)) late_locs;	/* Late locs.  */
  range_t GTY((skip)) loc_offsets;	/* Early and late offsets.  */

  unsigned current;	/* Section currently being loaded.  */
  unsigned remaining;	/* Number of lazy sections yet to read.  */
  unsigned lru;		/* An LRU counter.  */

  bool pre_early_ok;

 public:
  slurping (elf_in *);
  ~slurping ();

 public:
  void alloc_remap (unsigned size)
  {
    gcc_assert (!remap);
    vec_safe_reserve (remap, size);
    for (unsigned ix = size; ix--;)
      remap->quick_push (0);
  }
  void alloc_unnamed (unsigned size)
  {
    gcc_assert (!unnamed);
    vec_safe_reserve (unnamed, size);
  }
  void free_filenames ()
  {
    vec_free (filenames);
    filenames = NULL;
  }
  void alloc_filenames (unsigned size = 0)
  {
    gcc_assert (!filenames);
    vec_safe_reserve (filenames, size ? size : 10, size != 0);
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
  : unnamed (NULL), remap (NULL), from (from),
    filenames (NULL), early_locs (0, 0), late_locs (0, 0), loc_offsets (0, 0),
    current (~0u), remaining (0), lru (0), pre_early_ok (false)
{
}

slurping::~slurping ()
{
  vec_free (remap);
  remap = NULL;
  vec_free (unnamed);
  unnamed = NULL;
  if (from)
    {
      from->end ();
      delete from;
      from = NULL;
    }
}

/* Additional data needed when writing.  There's only ever one
   writer, so we don't mind wasting some space of the base class.   */

struct GTY ((tag ("false"))) spewing : public slurping {
 public:
  range_t GTY((skip)) early_loc_map;	/* Early location line maps.  */
  range_t GTY((skip)) late_loc_map;		/* Late location line maps.  */
  
 public:
  spewing ()
    : slurping (NULL),
    early_loc_map (0, 0), late_loc_map (0, 0)
    {
    }

 public:
  unsigned prepare_linemaps (line_maps *, bool early_p);
};

/* State of a particular module. */

class GTY(()) module_state {
 public:
  /* We always import & export ourselves.  */
  bitmap imports;	/* Transitive modules we're importing.  */
  bitmap exports;	/* Subset of that, that we're exporting.  */

  module_state *parent;

  tree name;		/* Name of the module.  */
  tree vec_name;  	/* Name as a vector.  */

  slurping *slurp;	/* Data for loading.  */

  const char *fullname;	/* Full name of module.  */
  char *filename;	/* BMI Filename */
  /* The LOC is unset until we import the module.  */
  location_t loc; 	/* Location referring to module itself.  */
  /* The FROM_LOC is unset until we process a declaration.  */
  location_t from_loc;  /* Location module was imported at.  */

  unsigned mod;		/* Module owner number.  */
  unsigned crc;		/* CRC we saw reading it in. */

  unsigned depth : 16;  /* Depth, direct imports are 0 */

  bool legacy : 1;

  bool direct : 1;	/* A direct import.  */
  bool exported : 1;	/* We are exported.  */
  bool imported : 1;	/* Import has been done.  */

 public:
  module_state (tree name, module_state *);
  ~module_state ();

 public:
  void release (bool at_eof = true);

 public:
  /* Is this not a real module?  */
  bool is_detached () const
  {
    return from_loc == UNKNOWN_LOCATION;
  }
  bool is_imported () const
  {
    return imported;
  }
  bool is_legacy () const
  {
    return legacy;
  }

 public:
  slurping *slurper () const
  {
    gcc_checking_assert (slurp && slurp->from);
    return slurp;
  }
  spewing *spewer () const
  {
    gcc_checking_assert (slurp && !slurp->from);
    return static_cast <spewing *> (slurp);
  }

 public:
  void set_import (module_state const *, bool is_export);
  void announce (const char *) const;

 public:
  /* Read and write module.  */
  void write (elf_out *to, line_maps *);
  void read (int fd, int e, line_maps *, bool);

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
  bool check_read (bool outermost, tree ns = NULL_TREE, tree id = NULL_TREE);

 private:
  /* The README, for human consumption.  */
  void write_readme (elf_out *to, const char *opts);

  /* Import tables. */
  void write_imports (bytes_out &cfg, bool direct_p);
  unsigned read_imports (bytes_in &cfg, line_maps *maps);
  void write_imports (elf_out *to, unsigned *crc_ptr);
  bool read_imports (line_maps *);

  /* The configuration.  */
  void write_config (elf_out *to, const char *opts, const range_t &sec_range,
		     unsigned unnamed, unsigned crc);
  bool read_config (range_t &sec_range, unsigned &unnamed, bool);

 private:
  /* Serialize various definitions. */
  static void mark_definition (trees_out &out, tree decl);
  static void write_definition (trees_out &out, tree decl);
  bool read_definition (trees_in &in, tree decl);

  static void mark_template_def (trees_out &out, tree decl);
  static void write_template_def (trees_out &out, tree decl);
  bool read_template_def (trees_in &in, tree decl);

  static void mark_function_def (trees_out &out, tree decl);
  static void write_function_def (trees_out &out, tree decl);
  bool read_function_def (trees_in &in, tree decl);

  static void mark_var_def (trees_out &out, tree decl);
  static void write_var_def (trees_out &out, tree decl);
  bool read_var_def (trees_in &in, tree decl);

  static void mark_class_def (trees_out &out, tree type);
  static void write_class_def (trees_out &out, tree type);
  bool read_class_def (trees_in &in, tree decl);

  static void mark_enum_def (trees_out &out, tree type);
  static void write_enum_def (trees_out &out, tree type);
  bool read_enum_def (trees_in &in, tree decl);

  static void write_binfos (trees_out &out, tree type);
  bool read_binfos (trees_in &in, tree type);

 private:
  /* Add writable bindings to hash table.  */
  static void add_writables (depset::hash &table, tree ns);
  /* Build dependency graph of hash table.  */
  void find_dependencies (depset::hash &table);

  static void write_bindings (elf_out *to, depset::hash &table,
			      unsigned *crc_ptr);
  bool read_bindings (auto_vec<tree> &spaces, const range_t &range);

  static void write_namespaces (elf_out *to, depset::hash &table,
				auto_vec<depset *> &spaces, unsigned *crc_ptr);
  bool read_namespaces (auto_vec<tree> &spaces);

  void write_cluster (elf_out *to, depset *depsets[], unsigned size,
		      unsigned &unnamed, unsigned *crc_ptr);
  bool read_cluster (unsigned snum);

  void write_unnamed (elf_out *to, auto_vec<depset *> &depsets,
		      unsigned count, unsigned *crc_ptr);
  bool read_unnamed (unsigned count, const range_t &range);

 private:
  void prepare_locations (line_maps *);
  void write_locations (elf_out *to, line_maps *, bool early, unsigned *crc_ptr);
  bool read_locations (line_maps *, bool early);

 public:
  void write_location (bytes_out &, location_t);
  location_t read_location (bytes_in &);

 public:
  void set_loc (line_maps *lmaps, const module_state *container = NULL,
		location_t floc = UNKNOWN_LOCATION);
  void attach (location_t);
  bool do_import (const char *filename, line_maps *, bool check_crc);

 private:
  static int maybe_add_global (tree, unsigned &);

 public:
  static void init ();
  static void fini ();
  static int atom_preamble (location_t loc, line_maps *);
};

/* Hash module state by name.  This cannot be a member of
   module_state, because of GTY restrictions.  */

struct module_state_hash : nodel_ptr_hash<module_state> {
  typedef std::pair<tree,module_state *> compare_type; /* identifer/parent */

  static inline hashval_t hash (const value_type m);
  static inline hashval_t hash (const compare_type &n);
  static inline bool equal (const value_type existing,
			    const compare_type &candidate);
};

module_state::module_state (tree name, module_state *parent)
  : imports (BITMAP_GGC_ALLOC ()), exports (BITMAP_GGC_ALLOC ()),
    parent (parent), name (name), vec_name (NULL_TREE), slurp (NULL),
    fullname (NULL), filename (NULL),
    loc (UNKNOWN_LOCATION), from_loc (UNKNOWN_LOCATION),
    mod (MODULE_UNKNOWN), crc (0), depth (65535)
{
  legacy = direct = exported = imported = false;
  if (name && (IDENTIFIER_POINTER (name)[0] == '"'
	       || IDENTIFIER_POINTER (name)[0] == '<'))
    legacy = true;
  gcc_checking_assert (!(parent && legacy));
}

module_state::~module_state ()
{
  release ();
}

/* Hash module state.  */

hashval_t
module_state_hash::hash (const value_type m)
{
  hashval_t h = pointer_hash<module_state>::hash (m->parent);

  return iterative_hash_hashval_t (h, IDENTIFIER_HASH_VALUE (m->name));
}

/* Hash a name.  */
hashval_t
module_state_hash::hash (const compare_type &c)
{
  hashval_t h = pointer_hash<module_state>::hash (c.second);

  return iterative_hash_hashval_t (h, IDENTIFIER_HASH_VALUE (c.first));
}

/* Lookup by IDENTIFIER_NODE or TREE_LIST.  */

bool
module_state_hash::equal (const value_type existing,
			  const compare_type &candidate)
{
  return (existing->name == candidate.first
	  && existing->parent == candidate.second);
}

/* Some flag values: */

/* Mapper name.  */
static const char *module_mapper_name;

/* Legacy header mode.  */
static const char *module_legacy_name;

/* End of the prefix line maps.  */
static unsigned prefix_line_maps_hwm;
static unsigned prefix_locations_hwm;
location_t module_preamble_end_loc;

/* BMI repository path and workspace.  */
static char *bmi_repo;
static size_t bmi_repo_length;
static char *bmi_path;
static size_t bmi_path_alloc;

/* Global trees.  */
static const std::pair<tree *, unsigned> global_tree_arys[] =
  {
    std::pair<tree *, unsigned> (sizetype_tab, stk_type_kind_last),
    std::pair<tree *, unsigned> (integer_types, itk_none),
    std::pair<tree *, unsigned> (global_trees, TI_MAX),
    std::pair<tree *, unsigned> (cp_global_trees, CPTI_MAX),
    std::pair<tree *, unsigned> (NULL, 0)
  };
static GTY(()) vec<tree, va_gc> *fixed_trees;
static unsigned global_crc;

/* Significant options for this compilation.   */
static char *our_opts;

static unsigned lazy_lru;  /* LRU counter.  */
static int lazy_open;	 /* Remaining limit for unfrozen loaders.   */

/* Vector of module state.  Indexed by OWNER.  Always has 2 slots.  */
static GTY(()) vec<module_state *, va_gc> *modules;

/* Hash of module state, findable by NAME. */
static hash_table<module_state_hash> *modules_hash;

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
  static module_mapper *make (location_t loc, const char *);

public:
  static module_mapper *get (location_t loc)
  {
    if (!mapper)
      mapper = make (loc, module_mapper_name);
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
  void uncork ()
  {
    batching = false;
  }
  bool corked () const
  {
    return batching;
  }
  bool eol_p () const
  {
    return pos == end;
  }

public:
  void imex_query (const module_state *, int dir);
  char *imex_response (const module_state *state)
  {
    return get_response (state->from_loc) > 0 ? bmi_response (state) : NULL;
  }
  void bewait_cmd (location_t loc)
  {
    send_command (loc, "BEWAIT");
  }
  module_state *bewait_response (location_t);
  int divert_include (cpp_reader *, line_maps *, location_t, const char *, bool);

public:
  /* After a response that may be corked, eat blank lines until it is
     uncorked.  */
  void maybe_uncork (location_t loc)
  {
    while (corked ())
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
  char *bmi_response (const module_state *);

private:
  static module_mapper *mapper;
};

/* Our module mapper (created lazily).  */
module_mapper *module_mapper::mapper;

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
      unsigned current = dumps ? dumps->stack.length () : 0;
      unsigned count = current ? current * 2 : MODULE_STAMP ? 1 : 20;
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
		  str = m->fullname;
	      }
	    fputs (str, dumps->stream);
	  }
	  break;
	case 'N': /* Name.  */
	  {
	    tree t = va_arg (args, tree);
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
	    int v = va_arg (args, unsigned);
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
set_bmi_repo (char *r)
{
  XDELETEVEC (bmi_repo);
  XDELETEVEC (bmi_path);
  bmi_path_alloc = 0;

  bmi_repo = NULL;
  bmi_repo_length = 0;
  if (r)
    {
      size_t len = strlen (r);
      if (len > 1 && IS_DIR_SEPARATOR (r[len-1]))
	r[--len] = 0;
      if (0 != strcmp (r, "."))
	{
	  bmi_repo = XNEWVEC (char, len + 1);
	  memcpy (bmi_repo, r, len + 1);
	  bmi_repo_length = len;
	}
    }
}

/* TO is a repo-relative name.  Provide one that we may use from where
   we are.  */

static const char *
maybe_add_bmi_prefix (const char *to)
{
  if (bmi_repo && !IS_ABSOLUTE_PATH (to))
    {
      size_t len = strlen (to);
      if (bmi_path_alloc < bmi_repo_length + len + 2)
	{
	  XDELETEVEC (bmi_path);
	  bmi_path_alloc = bmi_repo_length + len * 2 + 2;
	  bmi_path = XNEWVEC (char, bmi_path_alloc);
	  memcpy (bmi_path, bmi_repo, bmi_repo_length);
	  bmi_path[bmi_repo_length] = DIR_SEPARATOR;
	}
      memcpy (&bmi_path[bmi_repo_length + 1], to, len + 1);
      to = bmi_path;
    }
  
  return to;
}

/* If BMI path TO begins with the prefix, return a pointer to the
   trailing suffix.  Otherwise return TO.  */

static char *
maybe_strip_bmi_prefix (char *to)
{
  if (bmi_repo)
    {
      if (0 == strncmp (to, bmi_repo, bmi_repo_length))
	{
	  char *res = to;
	  for (size_t probe = bmi_repo_length;
	       IS_DIR_SEPARATOR (to[probe]);)
	    res = &to[++probe];
	  to = res;
	}
    }
  return to;
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

/* Setup and teardown for an outputting tree walk.  */

void
trees_out::begin ()
{
  gcc_assert (!depending_p () && !tree_map.elements ());

  mark_trees ();
  parent::begin ();
}

unsigned
trees_out::end (elf_out *sink, unsigned name, unsigned *crc_ptr)
{
  gcc_checking_assert (streaming_p ());

  unmark_trees ();
  return parent::end (sink, name, crc_ptr);
}

/* Setup and teardown for a dependency or external seeding walk.  */

void
trees_out::begin (depset::hash *hash)
{
  gcc_assert (!dep_hash && !streaming_p ());

  dep_hash = hash;
  mark_trees ();
  /* Do not parent::begin -- we're not streaming.  */
}

void
trees_out::end ()
{
  gcc_assert (!streaming_p ());

  unmark_trees ();
  dep_hash = NULL;
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
      bool existed = tree_map.put (val, ix + 1);
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
      tree node = reinterpret_cast <tree> ((*iter).first);
      int ref = (*iter).second;
      gcc_checking_assert (TREE_VISITED (node) && ref);
      TREE_VISITED (node) = false;
    }
}

/* Mark DECL for by-value walking.  We do this by inserting it into
   the tree map with a reference of zero.  May be called multiple
   times on the same node.  */

void
trees_out::mark_node (tree decl)
{
  gcc_checking_assert (DECL_P (decl) || IS_FAKE_BASE_TYPE (decl));

  if (TREE_VISITED (decl))
    gcc_checking_assert (!*tree_map.get (decl));
  else
    {
      bool existed = tree_map.put (decl, 0);
      gcc_checking_assert (!existed);
      TREE_VISITED (decl) = true;

      /* If the node is a template, mark the underlying decl too.  (The
	 reverse does not need to be checked for.)  */
      if (TREE_CODE (decl) == TEMPLATE_DECL)
	mark_node (DECL_TEMPLATE_RESULT (decl));
    }
}

/* Insert T into the map, return its back reference number.
   FORCING indicates whether it is already expected to have a forcing
   entry.  The forcing entry may be a preseed, in which case restore
   its value.  */

int
trees_out::insert (tree t, bool forcing)
{
  gcc_checking_assert (TREE_VISITED (t) == forcing);
  TREE_VISITED (t) = true;

  bool existed;
  int &slot = tree_map.get_or_insert (t, &existed);
  gcc_checking_assert (existed == forcing && (!forcing || !slot));
  slot = --ref_num;
  return slot;
}

/* If DECL is the TYPE_NAME of its type, insert the type into the map
   (unless it's already there).  Return the inserted tag, or 0.  */

int
trees_out::maybe_insert_typeof (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (type && !TREE_VISITED (type) && TYPE_NAME (type) == decl)
    return insert (type);
  return 0;
}

/* Insert T into the backreference array.  Return its back reference
   number.  */

int
trees_in::insert (tree t)
{
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
	    error_at (state->loc, "failed to merge %#qD", t);
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
      tree_ctx (t->decl_minimal.context, true, MODULE_PURVIEW);

      if (streaming_p ())
	state->write_location (*this, t->decl_minimal.locus);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      /* Likewise, stream the name first.  */
      WT (t->type_common.name);
      tree_ctx (t->type_common.context, true, MODULE_PURVIEW);

      /* By construction we want to make sure we have the canonical
	 and main variants already in the type table, so emit them
	 now.  */
      WT (t->type_common.main_variant);
      WT (t->type_common.canonical);

      /* type_common.next_variant is internally manipulated.  */
      /* type_common.pointer_to, type_common.reference_to.  */

      if (streaming_p ())
	{
	  WU (t->type_common.precision);
	  WU (t->type_common.contains_placeholder_bits);
	  WU (t->type_common.mode);
	  WU (t->type_common.align);
	}

      WT (t->type_common.size);
      WT (t->type_common.size_unit);
      WT (t->type_common.attributes);

      WT (t->type_common.common.chain); /* TYPE_STUB_DECL.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      if (code != ENUMERAL_TYPE || ENUM_IS_SCOPED (t))
	WT (t->typed.type);
      else if (streaming_p ())
	{
	  // FIXME it'd be nice if we could make TREE_TYPE of the
	  // enum's underlying type point to the original integral
	  // type.
	  tree type = t->typed.type;
	  int precision = TYPE_PRECISION (type);
	  unsigned itk;
	  tree name = DECL_NAME (TYPE_NAME (type));
	  for (itk = itk_none; itk--;)
	    if (integer_types[itk]
		&& DECL_NAME (TYPE_NAME (integer_types[itk])) == name)
	      break;
	  WU (itk);
	  WU (precision);
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

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      if (streaming_p ())
	{
	  WU (t->decl_common.mode);
	  WU (t->decl_common.off_align);
	  WU (t->decl_common.align);
	}

      WT (t->decl_common.size);
      WT (t->decl_common.size_unit);
      WT (t->decl_common.attributes);
      switch (code)
	// FIXME: Perhaps this should be done with the later
	// polymorphic check?
	{
	default:
	  break;
	case VAR_DECL:
	  // FIXME:Perhaps always write DECL_INITIAL?
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
      WT (t->field_decl.qualifier);
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
	   // FIXME:For some reason, some tcc_expression nodes do not claim
	   //  to contain TS_EXP.  I think this is a bug. */
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
      WT (((lang_tree_node *)t)->lambda_expression.capture_list);
      WT (((lang_tree_node *)t)->lambda_expression.this_capture);
      WT (((lang_tree_node *)t)->lambda_expression.extra_scope);
      /* pending_proxies is a parse-time thing.  */
      gcc_assert (!((lang_tree_node *)t)->lambda_expression.pending_proxies);
      if (streaming_p ())
	{
	  WU (((lang_tree_node *)t)->lambda_expression.default_capture_mode);
	  WU (((lang_tree_node *)t)->lambda_expression.discriminator);
	}
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

      RT (t->type_common.size);
      RT (t->type_common.size_unit);
      RT (t->type_common.attributes);

      RT (t->type_common.common.chain); /* TYPE_STUB_DECL.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      if (code != ENUMERAL_TYPE || ENUM_IS_SCOPED (t))
	RT (t->typed.type);
      else
	{
	  unsigned itk, precision;
	  RU (itk);
	  RU (precision);
	  if (itk >= itk_none)
	    set_overrun ();
	  else
	    {
	      tree type = integer_types[itk];
	      if (!type || precision > TYPE_PRECISION (type))
		set_overrun ();
	      else if (precision != TYPE_PRECISION (type))
		{
		  type = build_distinct_type_copy (type);
		  TYPE_PRECISION (type) = precision;
		  set_min_and_max_values_for_integral_type (type, precision,
							    TYPE_SIGN (type));
		}
	      t->typed.type = type;
	    }
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

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      RUC (machine_mode, t->decl_common.mode);
      RU (t->decl_common.off_align);
      RU (t->decl_common.align);

      RT (t->decl_common.size);
      RT (t->decl_common.size_unit);
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
      RT (((lang_tree_node *)t)->lambda_expression.capture_list);
      RT (((lang_tree_node *)t)->lambda_expression.this_capture);
      RT (((lang_tree_node *)t)->lambda_expression.extra_scope);
      /* lambda_expression.pending_proxies is NULL  */
      RUC (cp_lambda_default_capture_mode_type,
	   ((lang_tree_node *)t)->lambda_expression.default_capture_mode);
      RU (((lang_tree_node *)t)->lambda_expression.discriminator);
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
    default:
      gcc_unreachable ();

    case lds_fn:  /* lang_decl_fn.  */
      if (streaming_p ())
	{
	  if (DECL_NAME (t) && IDENTIFIER_OVL_OP_P (DECL_NAME (t)))
	    WU (lang->u.fn.ovl_op_code);

	  if (lang->u.fn.thunk_p)
	    wi (lang->u.fn.u5.fixed_offset);
	}

      if (!lang->u.fn.thunk_p)
	WT (lang->u.fn.u5.cloned_function);
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
  // FIXME:This is a property of the befriender
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
	  dump () && dump ("Wrote derived %sBINFO %u %N of %N",
			   is_virt ? "virtual " : "", ix, binfo, inh);
	  u (ix);
	}
    }
  else
    {
      dom = BINFO_TYPE (binfo);
      tree_ctx (dom, false, true);

      if (streaming_p ())
	{
	  dump () && dump ("Wrote dominating BINFO %N", dom);
	  i (via_virt ? -depth : depth);
	}
    }
  return dom;
}

tree
trees_in::tree_binfo ()
{
  tree dom = tree_node ();
  dump () && dump ("Read dominating binfo %N", dom);
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
	  dump () && dump ("Read derived BINFO %N", binfo);
	}
    }
  return binfo;
}

/* The raw tree node.  If the node is a decl, we've already determined
   it is to be by value.  The tree's code has been written, and we've
   been inserted into the back-reference table.  Stream the bools and
   vals, including any {decl,type}_lang_specific piece.  */

void
trees_out::tree_node_raw (tree t)
{
  tree_code_class klass = TREE_CODE_CLASS (TREE_CODE (t));
  bool specific = false;

  /* The only decls we should stream out are those from this module,
     or the global module.  All other decls should be by name.  */
  gcc_checking_assert (klass != tcc_declaration
		       || MAYBE_DECL_MODULE_OWNER (t) < MODULE_IMPORT_BASE
		       || t != get_module_owner (t));

  if (klass == tcc_type || klass == tcc_declaration)
    {
      if (klass == tcc_declaration)
	specific = DECL_LANG_SPECIFIC (t) != NULL;
      else if (TYPE_MAIN_VARIANT (t) == t)
	specific = TYPE_LANG_SPECIFIC (t) != NULL;
      else
	gcc_assert (TYPE_LANG_SPECIFIC (t)
		    == TYPE_LANG_SPECIFIC (TYPE_MAIN_VARIANT (t)));
      if (streaming_p ())
	{
	  b (specific);
	  if (specific && VAR_P (t))
	    b (DECL_DECOMPOSITION_P (t));
	}
    }

  if (streaming_p ())
    {
      core_bools (t);
      if (specific)
	{
	  if (klass == tcc_type)
	    lang_type_bools (t);
	  else
	    lang_decl_bools (t);
	}
      bflush ();
    }

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

  /* We should never walk a namespace.  */
  gcc_checking_assert (TREE_CODE (t) != NAMESPACE_DECL);
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

/* If T is a back reference, fixed reference or NULL, write it out and
   return zero.  Otherwise return -1 if we must write by value, or +1
   otherwise.  */

int
trees_out::tree_ref (tree t)
{
  if (!t)
    {
      if (streaming_p ())
	{
	  /* NULL_TREE -> tt_null.  */
	  nulls++;
	  i (tt_null);
	}
      return 0;
    }

  if (TREE_VISITED (t))
    {
      /* An already-visited tree.  It must be in the map.  */
      int *val_ptr = tree_map.get (t);
      int val = *val_ptr;

      if (!val)
	/* An entry we should walk into.  */
	return -1;

      if (streaming_p ())
	{
	  const char *kind;

	  refs++;
	  if (val < 0)
	    {
	      /* Back reference -> -ve number  */
	      i (val);
	      kind = "backref";
	    }
	  else
	    {
	      /* Fixed reference -> tt_fixed */
	      i (tt_fixed), u (--val);
	      kind = "fixed";
	    }

	  dump () && dump ("Wrote %s:%d %C:%N%S", kind,
			   val, TREE_CODE (t), t, t);
	}
      return 0;
    }

  return 1;
}

/* CTX is a context of some node, with owning module OWNER (if
   known).  Write it out.  */
// FIXME:return indicator if we discoverd a voldemort
void
trees_out::tree_ctx (tree ctx, bool looking_inside, unsigned module)
{
  int ref = tree_ref (ctx);
  if (ref)
    {
      bool force = ref < 0;
      if (TYPE_P (ctx)
	  ? tree_type (ctx, force, looking_inside, module)
	  : tree_decl (ctx, force, looking_inside, module))
	tree_value (ctx, force);
    }
}

/* Reference DECL.  FORCE is true, if we know we're writing this by
   value.  OWNER is if this is known to be in a particular module
   (defaults to -1, if this is the innermost decl).  Return true if we
   should write this decl by value.  */

bool
trees_out::tree_decl (tree decl, bool force, bool looking_inside, unsigned owner)
{
  gcc_checking_assert (DECL_P (decl));

  if (force)
    {
      /* If we requested by-value, this better not be an import.  */
      gcc_assert (MAYBE_DECL_MODULE_OWNER (get_module_owner (decl))
		  <= MODULE_IMPORT_BASE);
      return true;
    }

  if (TREE_CODE (decl) == PARM_DECL
      || !DECL_CONTEXT (decl))
    {
      /* If we cannot name this, it better be the inner-most decl we
	 asked about.  */
      gcc_assert (!looking_inside);
      return true;
    }

  if (!tree_ref (decl))
    /* If this is a fixed decl, we're done.  */
    return false;

  if (TREE_CODE (decl) == TEMPLATE_DECL
      && RECORD_OR_UNION_CODE_P (TREE_CODE (DECL_CONTEXT (decl)))
      && !DECL_MEMBER_TEMPLATE_P (decl))
    {
      /* An implicit member template, we should not meet as an import.  */
      gcc_assert (MAYBE_DECL_MODULE_OWNER (get_module_owner (decl))
		  <= MODULE_IMPORT_BASE);
      return true;
    }

  const char *kind = NULL;
  tree ti = NULL_TREE;
  int use_tpl = -1;
  if ((TREE_CODE (decl) == FUNCTION_DECL
       || TREE_CODE (decl) == VAR_DECL
       || TREE_CODE (decl) == TYPE_DECL)
      && DECL_LANG_SPECIFIC (decl))
    {
      use_tpl = DECL_USE_TEMPLATE (decl);
      ti = DECL_TEMPLATE_INFO (decl);
    }

  if (!ti && TREE_CODE (decl) == TYPE_DECL
      && TYPE_LANG_SPECIFIC (TREE_TYPE (decl)))
    {
      ti = TYPE_TEMPLATE_INFO (TREE_TYPE (decl));
      use_tpl = CLASSTYPE_USE_TEMPLATE (TREE_TYPE (decl));
    }

  if (!ti)
    ;
  else if (use_tpl & 1)
    {
      /* Some kind of instantiation. */
      tree tpl = TI_TEMPLATE (ti);
      if (!RECORD_OR_UNION_CODE_P (TREE_CODE (DECL_CONTEXT (tpl)))
	  || DECL_MEMBER_TEMPLATE_P (tpl))
	{
	  if (streaming_p ())
	    i (tt_inst);
	  tree_ctx (tpl, false);
	  tree_node (INNERMOST_TEMPLATE_ARGS (TI_ARGS (ti)));
	  kind = "instantiation";
	  owner = MODULE_UNKNOWN;
	  goto insert;
	}
    }
  else if (!use_tpl)
    {
      /* Primary.  */
    }
  else
    gcc_unreachable ();

  {
    /* Find the owning module and determine what to do.  */
    if (owner == MODULE_UNKNOWN)
      {
	/* Find the owning module and determine what to do.  */
	gcc_assert (TREE_CODE (decl) != NAMESPACE_DECL);

	tree owner_decl = get_module_owner (decl);
	owner = MAYBE_DECL_MODULE_OWNER (owner_decl);

	/* We should not get cross-module references to the pseudo
	   template of a member of a template class.  */
	gcc_assert (TREE_CODE (decl) != TEMPLATE_DECL
		    || TREE_CODE (CP_DECL_CONTEXT (decl)) == NAMESPACE_DECL
		    || DECL_MEMBER_TEMPLATE_P (decl)
		    || owner < MODULE_IMPORT_BASE);

	if (TREE_CODE (owner_decl) == FUNCTION_DECL
	    && owner_decl != decl
	    && (TREE_CODE (decl) != TEMPLATE_DECL
		|| DECL_TEMPLATE_RESULT (decl) != owner_decl))
	  {
	    /* We cannot look up inside a function by name.  */
	    gcc_assert (owner < MODULE_IMPORT_BASE);

	    return true;
	  }
      }

    bool is_import = owner >= MODULE_IMPORT_BASE;
    tree ctx = CP_DECL_CONTEXT (decl);

    if (TREE_CODE (ctx) == FUNCTION_DECL)
      {
	/* Some internal decl of the function.  */
	if (!DECL_IMPLICIT_TYPEDEF_P (decl))
	  return true;

	if (depending_p ())
	  {
	    bool unnameable = dep_hash->sneakoscope || is_import;
	    if (!unnameable)
	      {
		/* If the owning function is not within
		   dep_hash->current, it is also a voldemort.  */

		// FIXME: for now, not nested of nested.  Here it'd be
		// nice to just call the context dumper and get some
		// kind of result back 'hey, you're voldemorty'
		gcc_assert (TREE_CODE (CP_DECL_CONTEXT (ctx)) == NAMESPACE_DECL);
		if (dep_hash->current->get_decl () != ctx)
		  unnameable = true;
	      }

	    if (unnameable)
	      {
		/* We've found a voldemort type.  Add it as a
		   dependency.  */
		dep_hash->add_dependency (decl, looking_inside);
		kind = "unnamed";
		goto insert;
	      }
	  }
      }

    /* A named decl -> tt_named_decl.  */
    if (streaming_p ())
      {
	i (tt_named_decl);
	u (owner);
	tree_ctx (ctx, true, owner);
      }
    else if (!is_import)
      {
	/* Build out dependencies.  */
	if (TREE_CODE (ctx) != NAMESPACE_DECL)
	  tree_ctx (ctx, true, owner);
	else if (DECL_SOURCE_LOCATION (decl) != BUILTINS_LOCATION)
	  dep_hash->add_dependency (decl, looking_inside);
      }

    tree name = DECL_NAME (decl);
    tree_node (name);
    if (streaming_p ())
      {
	int ident = get_lookup_ident (ctx, owner, name, decl);
	i (ident);
	/* Make sure we can find it by name.  */
	gcc_checking_assert (decl == lookup_by_ident (ctx, owner, name, ident));
      }
    kind = is_import ? "import" : "named decl";
  }

 insert:
  int tag = insert (decl);
  if (streaming_p ())
    dump () && dump ("Wrote %s:%d %C:%N@%M", kind, tag, TREE_CODE (decl), decl,
		     owner == MODULE_UNKNOWN ? NULL : (*modules)[owner]);

  if (tree type = TREE_TYPE (decl))
    {
      /* Make sure the imported type is in the map too.  Otherwise we
	 get different RECORD_TYPEs for the same type, and things go
	 south.  */
      int tag = maybe_insert_typeof (decl);
      if (streaming_p ())
	{
	  u (tag != 0);
	  if (tag)
	    dump () && dump ("Wrote decl's type:%d %C:%N%S", tag,
			     TREE_CODE (type), type, type);
	}
    }

  return false;
}

bool
trees_out::tree_type (tree type, bool force, bool looking_inside, unsigned owner)
{
  gcc_assert (TYPE_P (type));
  if (force)
    return true;

  if (IS_FAKE_BASE_TYPE (type))
    {
      /* A fake base type -> tt_as_base.  */
      if (streaming_p ())
	{
	  i (tt_as_base);
	  dump () && dump ("Writing as_base for %N", TYPE_CONTEXT (type));
	}
      tree_ctx (TYPE_NAME (TYPE_CONTEXT (type)), true, owner);
      return false;
    }

  if (TYPE_NAME (type)
      && TREE_TYPE (TYPE_NAME (type)) == type
      && !tree_map.get (TYPE_NAME (type)))
    {
      /* A new named type -> tt_named_type.  */
      tree name = TYPE_NAME (type);
      /* Make sure this is not a named builtin. We should find
	 those some other way to be canonically correct.  */
      gcc_assert (DECL_SOURCE_LOCATION (name) != BUILTINS_LOCATION);
      if (streaming_p ())
	{
	  i (tt_named_type);
	  dump () && dump ("Writing interstitial named type %C:%N%S",
			   TREE_CODE (name), name, name);
	}
      tree_ctx (name, looking_inside, owner);
      if (streaming_p ())
	dump () && dump ("Wrote named type %C:%N%S",
			 TREE_CODE (name), name, name);

      /* The type itself could be a variant of TREE_TYPE (name), so
	 stream it out in its own right.  We'll find the name in the
	 map, so not end up here next time.  */
      tree_node (type);
      return false;
    }

  return true;
}

/* T is a node that must be written by value.  Do that.  FORCE is
   needed for consitency checking.  */

void
trees_out::tree_value (tree t, bool force)
{
  if (streaming_p ())
    {
      /* A new node -> tt_node.  */
      tree_code code = TREE_CODE (t);

      unique++;
      i (tt_node);
      u (code);

      start (code, t);
    }

  int tag = insert (t, force);
  if (streaming_p ())
    dump () && dump ("Writing:%d %C:%N%S%s", tag, TREE_CODE (t), t, t,
		     TREE_CODE_CLASS (TREE_CODE (t)) == tcc_declaration
		     && DECL_MODULE_EXPORT_P (t) ? " (exported)" : "");
  tree_node_raw (t);
  if (streaming_p ())
    dump () && dump ("Written:%d %C:%N", tag, TREE_CODE (t), t);
}

/* Stream out tree node T.  We automatically create local back
   references, which is essentially the lisp self-referential
   structure pretty-printer.  */

void
trees_out::tree_node (tree t)
{
  dump.indent ();
  bool force = false;
  int ref = tree_ref (t);
  if (!ref)
    goto done;
  if (ref < 0)
    /* An entry we should walk into.  */
    force = true;

  if (TREE_CODE (t) == IDENTIFIER_NODE)
    {
      gcc_assert (!force);

      /* An identifier node -> tt_id or, tt_conv_id.  */
      bool conv_op = IDENTIFIER_CONV_OP_P (t);

      if (streaming_p ())
	i (conv_op ? tt_conv_id : tt_id);
      if (conv_op)
	tree_node (TREE_TYPE (t));
      else if (streaming_p ())
	str (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));

      int tag = insert (t);
      if (streaming_p ())
	dump () && dump ("Written:%d %sidentifier:%N",
			 tag, conv_op ? "conv_op_" : "",
			 conv_op ? TREE_TYPE (t) : t);
      goto done;
    }

  if (TREE_CODE (t) == TREE_BINFO)
    {
      gcc_assert (!force);
      /* A BINFO -> tt_binfo.
	 We must do this by reference.  We stream the binfo tree
	 itself when streaming its owning RECORD_TYPE.  */
      if (streaming_p ())
	i (tt_binfo);
      tree_binfo (t, 0, false);

      // FIXME:IS this true?
      /* If the dominating type was an import, we will not have put this
	 in the map.  Do that now.  */
      int tag = TREE_VISITED (t) ? 0 : insert (t);
      if (streaming_p ())
	{
	  u (tag != 0);
	  if (tag)
	    dump () && dump ("Inserting binfo:%d %N", tag, t);
	}
      goto done;
    }

  if (TREE_CODE (t) == VAR_DECL && DECL_TINFO_P (t))
    {
      /* A typeinfo object -> tt_tinfo_var.  These need recreating by
	 the loader.  The type it is for is stashed on the name's
	 TREE_TYPE.  */
      gcc_assert (!force);
      tree type = TREE_TYPE (DECL_NAME (t));
      if (streaming_p ())
	i (tt_tinfo_var);
      tree_node (type);
      int tag = insert (t);
      if (streaming_p ())
	dump () && dump ("Wrote typeinfo:%d %S for %N", tag, t, type);
      goto done;
    }

  if (TREE_CODE (t) == TYPE_DECL && DECL_TINFO_P (t))
    {
      /* A typeinfo pseudo type -> tt_tinfo_typedef.  */
      gcc_assert (!force);
      unsigned ix = get_pseudo_tinfo_index (TREE_TYPE (t));

      if (streaming_p ())
	{
	  i (tt_tinfo_typedef);
	  u (ix);
	}
      unsigned tag = insert (t);
      if (streaming_p ())
	dump () && dump ("Wrote:%d typeinfo pseudo %u %N", tag, ix, t);
      goto done;
    }

  if (TREE_CODE (t) == VAR_DECL && !force && DECL_ARTIFICIAL (t))
    {
      tree ctx = CP_DECL_CONTEXT (t);
      if (TREE_CODE (ctx) == RECORD_TYPE && TYPE_LANG_SPECIFIC (ctx))
	{
	  /* Try a VTABLE.  */
	  unsigned ix = 0;
	  for (tree vtables = CLASSTYPE_VTABLES (ctx);
	       vtables; ix++, vtables = DECL_CHAIN (vtables))
	    if (vtables == t)
	      {
		if (streaming_p ())
		  {
		    u (tt_vtable);
		    u (ix);
		    dump () && dump ("Writing vtable %N[%u]", ctx, ix);
		  }
		tree_node (ctx);
		goto done;
	      }
	}
    }

  if (TYPE_P (t) && !tree_type (t, force, false))
    goto done;

  if (DECL_P (t) && !tree_decl (t, force, false))
    goto done;

  /* Otherwise by value */
  tree_value (t, force);

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
	dump () && dump ("Read backref:%d found %C:%N%S", tag,
			 TREE_CODE (res), res, res);
      break;

    case tt_fixed:
      {
	/* A fixed ref, find it in the fixed_ref array.   */
	unsigned fix = u ();
	if (fix < (*fixed_trees).length ())
	  {
	    res = (*fixed_trees)[fix];
	    dump () && dump ("Read fixed:%u %C:%N%S", fix,
			     TREE_CODE (res), res, res);
	  }

	if (!res)
	  set_overrun ();
      }
      break;

    case tt_named_type:
      /* An interstitial type name.  Read the name and then the type again.  */
      res = tree_node ();
      dump () && dump ("Read named type %C:%N%S",
		       res ? TREE_CODE (res) : ERROR_MARK, res, res);
      if (!res || TREE_CODE (res) != TYPE_DECL)
	set_overrun ();
      res = tree_node ();
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

    case tt_tinfo_typedef:
      {
	/* A pseudo typeinfo typedef.  Get the index and recreate the pseudo.  */
	unsigned ix = u ();

	res = TYPE_NAME (get_pseudo_tinfo_type (ix));
	int tag = insert (res);
	dump () && dump ("Created tinfo_typedef:%d %u %N", tag, ix, res);
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

    case tt_inst:
      const char *kind;
      unsigned owner;
      {
	tree tpl = tree_node ();
	tree args = tree_node ();
	if (TREE_CODE (tpl) != TEMPLATE_DECL)
	  {
	    tree ti = NULL_TREE;
	    if (TREE_CODE (tpl) == TYPE_DECL)
	      ti = TYPE_TEMPLATE_INFO (TREE_TYPE (tpl));
	    else if (DECL_LANG_SPECIFIC (tpl))
	      ti = DECL_TEMPLATE_INFO (tpl);
	    tpl = ti ? TI_TEMPLATE (tpl) : NULL_TREE;
	    if (!(tpl &&
		  RECORD_OR_UNION_CODE_P (TREE_CODE (CP_DECL_CONTEXT (tpl)))
		  && !DECL_MEMBER_TEMPLATE_P (tpl)))
	      tpl = NULL_TREE;
	  }

	if (!tpl)
	  set_overrun ();
	else if (TREE_CODE (DECL_TEMPLATE_RESULT (tpl)) != TYPE_DECL)
	  {
	    res = instantiate_template (tpl, args, tf_error);
	    mark_used (res, tf_none); // FIXME:this may be too early
	  }
	else
	  {
	    res = lookup_template_class (tpl, args, NULL_TREE, NULL_TREE,
					 0, tf_error);
	    complete_type (res); // FIXME:Probably too early
	    res = TYPE_NAME (res);
	  }
	kind = "Instantiation";
	owner = tpl ? MAYBE_DECL_MODULE_OWNER (tpl) : 0;
      }
      goto finish_decl;

    case tt_named_decl:
      {
	/* A named decl.  */
	owner = u ();
	tree ctx = tree_node ();
	tree name = tree_node ();
	owner = (owner < state->slurp->remap->length ()
		 ? (*state->slurp->remap)[owner] : MODULE_NONE);
	int ident = i ();
	if (owner != MODULE_NONE && !get_overrun ())
	  res = lookup_by_ident (ctx, owner, name, ident);

	if (!res)
	  {
	    error_at (state->loc, "failed to find %<%E%s%E@%M%>",
		      ctx, &"::"[2 * (ctx == global_namespace)],
		      name, (*modules)[owner]);
	    set_overrun ();
	  }
	else if (TREE_CODE (res) != TYPE_DECL
		 && owner != state->mod)
	  mark_used (res, tf_none);

	kind = owner != state->mod ?  "Imported" : "Named";
      }
      finish_decl:
      {
	int tag = insert (res);
	if (res)
	  {
	    dump () && dump ("%s:%d %C:%N@%M", kind, tag, TREE_CODE (res),
			     res, (*modules)[owner]);
	    if (TREE_TYPE (res) && u ())
	      {
		/* Insert the type too.  */
		tree type = TREE_TYPE (res);
		tag = insert (type);
		dump () && dump ("Read imported type:%d %C:%N%S", tag,
				 TREE_CODE (type), type, type);
	      }
	  }
      }
      break;

    case tt_binfo:
      {
	/* A BINFO.  Walk the tree of the dominating type.  */
	res = tree_binfo ();
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

    case tt_vtable:
      {
	unsigned ix = u ();
	tree ctx = tree_node ();
	dump () && dump ("Reading vtable %N[%u]", ctx, ix);
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

    case tt_node:
      {
	/* A new node.  Stream it in.  */
	unsigned c = u ();
	if (c >= MAX_TREE_CODES)
	  {
	    error_at (state->loc, "unknown tree code %qd" , c);
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
	    back_refs[~tag] = NULL_TREE;
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
	    back_refs[~tag] = res;
	    dump () && dump ("Remapping:%d to %C:%N%S", tag,
			     res ? TREE_CODE (res) : ERROR_MARK, res, res);
	  }
	break;
      }
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

  if (RECORD_OR_UNION_CODE_P (TREE_CODE (type))
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

/* Return non-zero if DECL has a definition that would be interesting to
   write out.  */

static bool
has_definition (tree decl)
{
 again:
  switch (TREE_CODE (decl))
    {
    default:
      break;

    case TEMPLATE_DECL:
      decl = DECL_TEMPLATE_RESULT (decl);
      goto again;

    case FUNCTION_DECL:
      if (!DECL_INITIAL (decl))
	/* Not defined.  */
	break;

      if (DECL_TEMPLATE_INFO (decl))
	{
	  if (!(DECL_USE_TEMPLATE (decl) & 1))
	    return true;
	}
      else if (DECL_DECLARED_INLINE_P (decl))
	return true;
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

/* Lookup and maybe add a depset of CONTAINER and NAME.  */

depset **
depset::hash::maybe_insert (const key_type &key, bool insert)
{
  depset **slot = find_slot_with_hash (key, traits::hash (key),
				       insert ? INSERT : NO_INSERT);

  return slot;
}

depset *
depset::hash::find (const key_type &key)
{
  depset **slot = maybe_insert (key, false);

  return slot ? *slot : NULL;
}

/* DECL is a newly discovered dependency of current.  Append it to
   current's depset.  Push it into the worklist, if it's not there.
   The decls discovered at this point are not export or module
   linkage.  We don't add them to the binding depset -- they are not
   findable by name.  Note that depending on the resolution of ADL
   mechanism, we may need to revisit that.   */

// kind < 0 - binding
// kind > 0 - containing scope
// kind = 0 regular

depset *
depset::hash::add_dependency (tree decl, int kind)
{
  bool has_def = has_definition (decl);
  key_type key = defn_key (decl, has_def);
  depset **slot = maybe_insert (key);
  depset *dep = *slot;

  if (!dep)
    {
      *slot = dep = new depset (key);
      worklist.safe_push (dep);

      if (kind >= 0
	  && !(TREE_CODE (decl) == NAMESPACE_DECL
	       && !DECL_NAMESPACE_ALIAS (decl)))
	/* Any not-for-binding depset is not found by name.  */
	dep->is_unnamed = true;
    }

  dump () && dump ("%s on %s %N added",
		   kind < 0 ? "Binding" : "Dependency",
		   dep->is_defn () ? "definition" : "declaration", decl);

  if (kind >= 0)
    {
      if (dep->is_unnamed)
	current->refs_unnamed = true;
      current->deps.safe_push (dep);
      if (TREE_CODE (decl) == TYPE_DECL
	  && UNSCOPED_ENUM_P (TREE_TYPE (decl))
	  && CP_DECL_CONTEXT (current->get_decl ()) == TREE_TYPE (decl))
	/* Unscoped enum values are pushed into the containing
	   scope.  Insert a dependency to the current binding, if it
	   is one of the enum constants.  */
	dep->deps.safe_push (current);
    }

  return dep;
}

/* DECLS is a vector of decls that must be written out (export or
   module-linkage).  Create the relevant depsets for the binding and
   its conents.  If a member is a namespace, return that.   */

tree
depset::hash::add_binding (tree ns, tree name, auto_vec<tree> &decls)
{
  tree res = NULL_TREE;
  depset *bind = new depset (binding_key (ns, name));

  bind->deps.reserve_exact (decls.length ());
  /* Reverse ordering, so exported things are first.  */
  for (unsigned ix = decls.length (); ix--;)
    {
      tree decl = decls[ix];

      gcc_checking_assert (DECL_P (decl));
      depset *dep = add_dependency (decl, -1);
      if (TREE_CODE (decl) == NAMESPACE_DECL && !DECL_NAMESPACE_ALIAS (decl))
	{
	  gcc_checking_assert (!res);
	  res = decl;
	}
      else
	{
	  bind->deps.quick_push (dep);
	  dep->deps.safe_push (bind);
	}
    }

  if (bind->deps.length ())
    insert (bind);
  else
    delete bind;

  return res;
}

/* Core of TARJAN's algorithm to find Strongly Connected Components
   within a graph.  See https://en.wikipedia.org/wiki/
   Tarjan%27s_strongly_connected_components_algorithm for details.

   We use depset::section as lowlink.  Completed nodes have
   depset::cluster containing the cluster number, with the top
   bit set.

   A useful property is that the output vector is a reverse
   topological sort of the resulting DAG.  In our case that means
   dependent SCCs are found before their dependers.  */

void
depset::tarjan::connect (depset *v)
{
  v->cluster = v->section = ++index;
  stack.safe_push (v);

  /* Walk all our dependencies.  */
  for (unsigned ix = v->deps.length (); ix--;)
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
	  result->quick_push (p);
	}
      while (p != v);
    }
}

/* Module name substitutions.  */
static vec<module_state *,va_heap> substs;

void
mangle_module (int mod)
{
  module_state *state = (*modules)[mod];

  substs.safe_push (state);
  tree vec_name = state->vec_name;
  for (int ix = 0; ix < TREE_VEC_LENGTH (vec_name); ix++)
    mangle_identifier (TREE_VEC_ELT (vec_name, ix));
}

/* Clean up substitutions.  */
void
mangle_module_fini ()
{
  while (substs.length ())
    {
      module_state *m = substs.pop ();
    }
}

/* Find or create module NAME & PARENT in the hash table.  */

module_state *
get_module (tree name, module_state *parent)
{
  module_state_hash::compare_type ct (name, parent);
  hashval_t hv = module_state_hash::hash (ct);
  module_state **slot = modules_hash->find_slot_with_hash (ct, hv, INSERT);
  module_state *state = *slot;
  if (!state)
    {
      state = new (ggc_alloc<module_state> ()) module_state (name, parent);
      *slot = state;
    }
  return state;
}

/* Process string name PTR into a module_state.  */

static module_state *
get_module (const char *ptr)
{
  if (ptr[0] == '"' || ptr[0] == '<')
    {
      /* A legacy name.  */
      size_t len = strlen (ptr);
      if (len < 3 || ptr[len-1] != (ptr[0] == '"' ? '"' : '>'))
	return NULL;
      return get_module (get_identifier_with_length (ptr, len), NULL);
     }

  module_state *parent = NULL;
  for (const char *probe = ptr;; probe++)
    if (!*probe || *probe == '.')
      {
	size_t len = probe - ptr;
	if (!len)
	  return NULL;
	parent = get_module (get_identifier_with_length (ptr, len), parent);
	ptr = probe;
	if (!*ptr++)
	  break;
      }
  return parent;
}

/* VAL is a global tree, add it to the global vec if it is
   interesting.  Add some of its targets, if they too are
   interesting.  */

int
module_state::maybe_add_global (tree val, unsigned &crc)
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
module_state::init ()
{
  modules_hash = new hash_table<module_state_hash> (30);

  vec_safe_reserve (modules, 20);
  for (unsigned ix = MODULE_IMPORT_BASE; ix--;)
    modules->quick_push (NULL);

  /* Create module for current TU.  */
  module_state *current
    = new (ggc_alloc <module_state> ()) module_state (NULL, NULL);
  current->mod = MODULE_NONE;
  bitmap_set_bit (current->imports, MODULE_NONE);
  (*modules)[MODULE_NONE] = current;

  gcc_checking_assert (!fixed_trees);

  dump.push (NULL);

  /* Determine lazy handle bound.  */
  lazy_open = PARAM_VALUE (PARAM_LAZY_MODULES);
  if (!lazy_open)
    {
      lazy_open = 100;
#if HAVE_GETRLIMIT
      struct rlimit rlimit;
      if (!getrlimit (RLIMIT_NOFILE, &rlimit))
	lazy_open = (rlimit.rlim_cur > 1000000
		     ? 1000000 : unsigned (rlimit.rlim_cur));
#endif
      /* Use 3/4's of the available handles.  */
      lazy_open = lazy_open * 3 / 4;
    }
  dump () && dump ("Lazy limit is %u", lazy_open);

  if (modules_atom_p () && flag_module_preamble >= 0
      && flag_module_preamble < 65536)
    dump () && dump ("Preamble ends after %d", flag_module_preamble);

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
}

/* Delete post-parsing state.  */

void
module_state::fini ()
{
  for (unsigned ix = modules->length (); --ix >= MODULE_IMPORT_BASE;)
    (*modules)[ix]->release ();

  delete modules_hash;
  modules_hash = NULL;
}

/* Free up state.  If ALL is true, we're completely done.  If ALL is
   false, we've completed reading in the module (but have not
   completed parsing).  */

void
module_state::release (bool at_eof)
{
  if (at_eof)
    {
      imports = NULL;
      exports = NULL;
      if (fullname && fullname != IDENTIFIER_POINTER (name))
	XDELETEVEC (const_cast <char *> (fullname));
      fullname = NULL;
    }

  if (slurp)
    {
      if (slurp->from)
	delete slurp;
      else
	delete spewer ();
      slurp = NULL;
    }
}

/* Announce WHAT about the module.  */

void
module_state::announce (const char *what) const
{
  if (noisy_p ())
    {
      fprintf (stderr, mod < MODULE_LIMIT ? " %s:%s:%u" : " %s:%s",
	       what, fullname, mod);
      fflush (stderr);
    }
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

/* Create a mapper.  The mapper may be dead.  Yes, I'm embedding some
   client-side socket handling in the compiler.  At least it's not
   ipv4.  */

module_mapper::module_mapper (location_t loc, const char *option)
  : name (NULL), from (NULL), to (NULL), pex (NULL), sigpipe (SIG_IGN),
    /* Exercise buffer expansion code.  */
    buffer (NULL), size (MODULE_STAMP ? 3 : 200), pos (NULL), end (NULL),
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
	cookie = const_cast <char *> (&option[len]);
    }
  char *writable = XNEWVEC (char, len + 1);
  memcpy (writable, option, len + 1);
  if (cookie)
    {
      len = cookie - option;
      cookie = writable + len;
      *cookie = 0;
    }

  if (option[0] == '|')
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

  if (!name)
    {
      int fd;

      /* Does it look like a socket?  */
#ifdef NETWORKING
#ifdef HAVE_AF_UNIX
      sockaddr_un un;
      size_t un_len = 0;
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
	      *colon = ':';
#else
	      errmsg = "ipv6 protocol unsupported";
#endif
	      name = writable;
	    }
	}
      
      if (name)
	{
#ifdef HAVE_AF_UNIX
	  if (un_len)
	    {
	      fd = socket (un.sun_family, SOCK_STREAM, 0);
	      if (fd < 0 || connect (fd, (sockaddr *)&un, un_len) < 0)
		if (fd >= 0)
		  {
		    close (fd);
		    fd = -1;
		  }
	    }
#endif
#ifdef HAVE_AF_INET6
	  fd = socket (AF_INET6, SOCK_STREAM, 0);
	  if (fd >= 0)
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
		set_bmi_repo (file);
		continue;
	      }
	    
	    starting = false;
	    file = maybe_strip_bmi_prefix (file);
	    module_state *state = get_module (mod);
	    if (!state)
	      response_unexpected (loc);
	    else if (!state->filename)
	      state->filename = xstrdup (file);
	    else if (strcmp (state->filename, file))
	      warning_at (loc, 0, "ignoring conflicting mapping of %qM to %qs",
			  state, file);
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
module_mapper::make (location_t loc, const char *option)
{
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

  if (*format)
    for (;;)
      {
	va_list args;
	va_start (args, format);
	size_t available = (buffer + size) - end;
	gcc_checking_assert (available);
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
    dump () && dump ("Mapper pending request:%s", end);
  else
    dump () && dump ("Mapper request:%s", buffer);
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
      dump () && dump ("Mapper response:%s", buffer);
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
    BYIMPORT module-name
    	No response
    BEWAIT
	-> module-name OK bmipath
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

    case 0: /* HELLO $ver $repo */
      {
	if (char *ver = response_token (loc))
	  dump () && dump ("Connected to mapper version %s", ver);
	char *repo = response_token (loc, true);
	if (response_eol (loc))
	  {
	    if (repo)
	      set_bmi_repo (repo);
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

/* BYIMPORT, IMPORT or EXPORT query.  */

void
module_mapper::imex_query (const module_state *state, int importing)
{
  send_command (state->from_loc, "%sPORT %s",
		!importing ? "EX" : "BYIM" + importing + 1,
		state->fullname);
}

/* Response to import/export query.  */

char *
module_mapper::bmi_response (const module_state *state)
{
  char *filename = NULL;

  switch (response_word (state->from_loc, "OK", "ERROR", NULL))
    {
    default:
      break;

    case 0: /* OK $bmifile  */
      filename = response_token (state->from_loc, true);
      filename = maybe_strip_bmi_prefix (filename);
      response_eol (state->from_loc);
      break;

    case 1: /* ERROR $msg */
      error_at (state->from_loc, "mapper cannot provide module %qM: %s",
		state, response_error ());
      break;
    }

  return filename;
}

/* A response to a bewait request.  */

module_state *
module_mapper::bewait_response (location_t loc)
{
  if (get_response (loc) <= 0)
    return NULL;

  const char *name = response_token (loc);
  if (!name)
    return NULL;
  if (0 == strcmp (name, "-"))
    {
      if (0 != response_word (loc, "ERROR", NULL))
	error_at (loc, "mapper bewait failure: %s", response_error ());
      return NULL;
    }
  else if (module_state *state = get_module (name))
    {
      char *fname = bmi_response (state);
      if (!state->direct || state->filename)
	error_at (loc, "unexpected bewait reponse from mapper %qs", name);
      else if (fname)
	state->filename = xstrdup (fname);
      return state;
    }
  else
    response_unexpected (loc);

  return NULL;
}

/* Import query.  */

char *
module_mapper::import_export (const module_state *state, bool export_p)
{
  module_mapper *mapper = get (state->from_loc);

  if (mapper->is_server ())
    {
      mapper->imex_query (state, export_p ? 0 : +1);
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
      dump () && dump ("Completed mapper");
      mapper->send_command (state->from_loc, "DONE %s", state->fullname);
    }
  else
    ok = mapper->is_live ();

  return ok;
}

/* Include diversion.  Query if include FILE should be turned into an
   import of a legacy header.  Return 0 if it should remain a #include.
   If READER is non-NULL, do the diversion by pushing a buffer
   containing the diverted text (ending in two \n's).  Return non-zero
   indicator of who owns the pushed buffer.  */

int module_mapper::divert_include (cpp_reader *reader, line_maps *lmaps,
				   location_t loc, const char *file, bool angle)
{
  send_command (loc, "INCLUDE %c%s%c",
		angle ? '<' : '"', file, angle ? '>' : '"');
  if (get_response (loc) <= 0)
    return 0;

  int action = 0;
  const char *diversion = NULL;
  // FIXME:Search response?
  switch (response_word (loc, "IMPORT", "INCLUDE", NULL))
    {
    default:
      break;
    case 0:  /* Divert to import.  */
      action = 1;
      if (!eol_p ())
	diversion = response_token (loc);
      break;
    case 1:  /* Treat as include.  */
      break;
    }
  response_eol (loc);
  if (!action)
    return 0;

  if (reader)
    {
      loc = ordinary_loc_of (lmaps, loc);
      const line_map_ordinary *map
	= linemap_check_ordinary (linemap_lookup (lmaps, loc));
      unsigned col = SOURCE_COLUMN (map, loc);
      col -= (col != 0); /* Columns are 1-based.  */

      if (diversion)
	file = diversion;

      /* Divert.   */
      size_t len = strlen (file);
      char *res = XNEWVEC (char, len + 60 + col);

      /* Indent so the filename falls at the same column as the original
	 source.  */
      strcpy (res, " import ");
      size_t actual = 8;
      if (col > actual)
	{
	  memset (res + actual, ' ', col - actual);
	  actual = col;
	}
      if (!diversion)
	res[actual++] = angle ? '<' : '"';
      memcpy (res + actual, file, len);
      actual += len;
      if (!diversion)
	res[actual++] = angle ? '>' : '"';
      strcpy (res + actual, ";\n\n");
      actual += 3;
      cpp_push_buffer (reader, reinterpret_cast <unsigned char *> (res),
		       actual, false);
    }
  return +1;  /* cpplib will delete the buffer.  */
}

/* Generate a string of the compilation options.  */

static char *
get_option_string  ()
{
  /* Concatenate important options.  */
  size_t opt_alloc = MODULE_STAMP ? 2 : 200;
  size_t opt_len = 0;
  char *opt_str = XNEWVEC (char, opt_alloc);

  for (unsigned ix = 0; ix != save_decoded_options_count; ix++)
    {
      const cl_decoded_option *opt = &save_decoded_options[ix];
      if (opt->opt_index >= N_OPTS)
	continue;
      // FIXME:There's probably a better way to get options we care
      // about?  What does LTO do?
      const char *text = opt->orig_option_with_args_text;

      if (opt->opt_index >= N_OPTS)
	continue;

      /* Some module-related options we don't need to preserve.  */
      if (opt->opt_index == OPT_fmodule_lazy
	  || opt->opt_index == OPT_fmodule_preamble_
	  || opt->opt_index == OPT_fmodule_mapper_
	  || opt->opt_index == OPT_fmodules_legacy_
	  || opt->opt_index == OPT_fmodules_legacy
	  || opt->opt_index == OPT_fmodules_atom
	  || opt->opt_index == OPT_fmodules_ts)
	continue;

      /* -f* -g* -m* -O* -std=* */
      if (text[0] != '-'
	  || (!strchr ("fgmO", text[1])
	      && 0 != strncmp (&text[1], "std=", 4)))
	continue;

      /* Some random options we shouldn't preserve.  */
      if (opt->opt_index == OPT_frandom_seed
	  || opt->opt_index == OPT_frandom_seed_
	  /* Drop any diagnostic formatting options.  */
	  || opt->opt_index == OPT_fmessage_length_
	  || (opt->opt_index >= OPT_fdiagnostics_color_
	      && opt->opt_index <= OPT_fdiagnostics_show_template_tree)
	  /* Drop any dump control options.  */
	  || (opt->opt_index >= OPT_fdump_
	      && opt->opt_index <= OPT_fdump_unnumbered_links))
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

  return opt_str;
}

/* A human-readable README section.  It is a STRTAB that may be
   extracted with:
     readelf -p.gnu.c++.README $(module).nms */

void
module_state::write_readme (elf_out *to, const char *options)
{
  bytes_out readme (to);

  readme.begin (false);

  readme.printf ("GNU C++ Module (%s%s)", is_legacy () ? "Legacy "  : "",
		 modules_atom_p () ? "ATOM" : "TS");
  /* Compiler's version.  */
  readme.printf ("compiler:%s", version_string);

  /* Module format version.  */
  verstr_t string;
  version2string (get_version (), string);
  readme.printf ("version:%s", string);

  /* Module information.  */
  readme.printf ("module:%s", fullname);
  readme.printf ("source:%s", main_input_filename);

  readme.printf ("options:%s", options);

  /* Its direct imports.  */
  for (unsigned ix = MODULE_IMPORT_BASE; ix < modules->length (); ix++)
    {
      module_state *state = (*modules)[ix];
      if (state->direct)
	readme.printf ("import:%s %s", state->fullname, state->filename);
    }

  readme.end (to, to->name (MOD_SNAME_PFX ".README"), NULL);
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
    count += (*modules)[ix]->direct == direct;

  sec.u (count);
  for (unsigned ix = MODULE_IMPORT_BASE; ix < modules->length (); ix++)
    {
      module_state *state = (*modules)[ix];
      if (state->direct == direct)
	{
	  dump () && dump ("Writing %simport:%u %M (crc=%x)",
			   !direct ? "indirect "
			   : state->exported ? "exported " : "",
			   ix, state, state->crc);
	  sec.u (ix);
	  sec.str (state->fullname);
	  sec.u32 (state->crc);
	  if (direct)
	    {
	      write_location (sec, state->from_loc);
	      sec.str (state->filename);
	      sec.u (state->exported);
	    }
	}
    }
}

unsigned
module_state::read_imports (bytes_in &sec, line_maps *lmaps)
{
  unsigned count = sec.u ();
  typedef std::pair<unsigned, bool> ix_bool_t;
  typedef std::pair<module_state *, ix_bool_t> tuple;
  auto_vec<tuple> imports (count);
  unsigned loaded = 0;

  /* First read the table, and initialize locations.
     We do this in two passes to cluster line maps.  */
  while (count--)
    {
      unsigned ix = sec.u ();
      if (ix >= slurp->remap->length ()
	  || ix < MODULE_IMPORT_BASE || (*slurp->remap)[ix])
	break;

      size_t len;
      const char *name = sec.str (&len);
      module_state *imp = get_module (name);
      unsigned crc = sec.u32 ();
      bool exported = false;

      if (lmaps)
	{
	  /* A direct import, maybe load it.  */
	  size_t len;
	  location_t floc = read_location (sec);
	  const char *filename_str = sec.str (&len);

	  exported = sec.u ();
	  if (sec.get_overrun ())
	    break;
	  if (imp->is_detached ())
	    imp->attach (from_loc);
	  module_state *purview = (*modules)[MODULE_PURVIEW];
	  if (purview == imp)
	    {
	      /* Cannot import the current module.  */
	      error_at (loc, "cannot import module %qM in its own purview",
			imp);
	      inform (imp->from_loc, "module %qM declared here", imp);
	      imp = NULL;
	    }
	  else
	    {
	      imp->set_loc (lmaps, this, floc);
	      if (!imp->is_imported ())
		{
		  imp->crc = crc;
		  if (!imp->filename && filename_str[0])
		    {
		      char *rel = XNEWVEC (char, len + 1);
		      memcpy (rel, filename_str, len + 1);
		      imp->filename = rel;
		    }
		}
	      else if (imp->crc != crc)
		{
		  error_at (loc, "import %qM has CRC mismatch", imp);
		  imp = NULL;
		}
	    }
	}
      else if (imp->is_detached ())
	/* An indirect import, find it, it should be there.  */
	error_at (loc, "indirect import %qM is not already loaded", imp);

      if (imp)
	imports.quick_push (tuple (imp, ix_bool_t (ix, exported)));
    }

  /* Now process the imports.  */
  for (unsigned ix = 0; ix != imports.length (); ix++)
    {
      tuple &tup = imports[ix];
      if (!tup.first->is_imported ())
	{
	  char *fname = NULL;
	  unsigned n = dump.push (tup.first);
	  if (!tup.first->filename)
	    fname = module_mapper::import_export (tup.first, false);
	  if (!tup.first->do_import (fname, lmaps, true))
	    tup.first = NULL;
	  dump.pop (n);
	}

      if (tup.first)
	{
	  (*slurp->remap)[tup.second.first] = tup.first->mod;
	  if (lmaps)
	    set_import (tup.first, tup.second.second);
	  dump () && dump ("Found %simport:%u %M->%u",
			   !lmaps ? "indirect " : tup.first->exported
			   ? "exported " : "", tup.second.first, tup.first,
			   tup.first->mod);
	  loaded++;
	}
    }
  return loaded;
}

/* Write the import table to MOD_SNAME_PFX.imp.  */

void
module_state::write_imports (elf_out *to, unsigned *crc_ptr)
{
  if (modules->length () == MODULE_IMPORT_BASE)
    return;

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
module_state::read_imports (line_maps *lmaps)
{
  if (slurp->remap->length () == MODULE_IMPORT_BASE)
    return true;

  bytes_in sec;

  if (!sec.begin (loc, slurp->from, MOD_SNAME_PFX ".imp"))
    return false;

  dump () && dump ("Reading %u imports",
		   slurp->remap->length () - MODULE_IMPORT_BASE);
  dump.indent ();

  /* Read the imports.  */
  unsigned direct = read_imports (sec, lmaps);
  unsigned indirect = read_imports (sec, NULL);
  if (direct + indirect + MODULE_IMPORT_BASE != slurp->remap->length ())
    slurp->from->set_error (elf::E_BAD_IMPORT);

  dump.outdent ();
  if (!sec.end (slurp->from))
    return false;
  return true;
}

/* Tool configuration:  MOD_SNAME_PFX .config

   This is data that confirms current state (or fails).

   u32:version
   u32:crc
   u:atom_p
   u:module-name
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

void
module_state::write_config (elf_out *to, const char *opt_str,
			    const range_t &sec_range,
			    unsigned unnamed, unsigned inner_crc)
{
  bytes_out cfg (to);

  cfg.begin ();

  /* Write version and inner crc as u32 values, for easier
     debug inspection.  */
  dump () && dump ("Writing version=%V, inner_crc=%x",
		   get_version (), inner_crc);
  cfg.u32 (unsigned (get_version ()));
  cfg.u32 (inner_crc);

  cfg.u (modules_atom_p ());
  cfg.u (to->name (fullname));

  /* Configuration. */
  dump () && dump ("Writing target='%s', host='%s'",
		   TARGET_MACHINE, HOST_MACHINE);
  unsigned target = to->name (TARGET_MACHINE);
  unsigned host = (!strcmp (TARGET_MACHINE, HOST_MACHINE)
		   ? target : to->name (HOST_MACHINE));
  cfg.u (target);
  cfg.u (host);

  cfg.str (opt_str);

  /* Global tree information.  We write the globals crc separately,
     rather than mix it directly into the overall crc, as it is used
     to ensure data match between instances of the compiler, not
     integrity of the file.  */
  dump () && dump ("Writing globals=%u, crc=%x",
		   fixed_trees->length (), global_crc);
  cfg.u (fixed_trees->length ());
  cfg.u32 (global_crc);

  cfg.u (modules->length ());

  dump () && dump ("Declaration sections are [%u,%u)",
		   sec_range.first, sec_range.second);
  cfg.u (sec_range.first);
  cfg.u (sec_range.second);

  dump () && dump ("Unnamed %u decls", unnamed);
  cfg.u (unnamed);

  /* Now generate CRC, we'll have incorporated the inner CRC because
     of its serialization above.  */
  cfg.end (to, to->name (MOD_SNAME_PFX ".cfg"), &crc);
  dump () && dump ("Writing CRC=%x", crc);
}

bool
module_state::read_config (range_t &sec_range, unsigned &unnamed, bool check_crc)
{
  bytes_in cfg;

  if (!cfg.begin (loc, slurp->from, MOD_SNAME_PFX ".cfg"))
    return false;

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
	  error_at (loc, "file is version %s, this is version %s",
		    their_string, my_string);
	  goto fail;
	}
      else if (my_time != their_time)
	/* Times differ, give it a go.  */
	warning_at (loc, 0, "file is version %s, compiler is version %s,"
		    " perhaps close enough? \xc2\xaf\\_(\xe3\x83\x84)_/\xc2\xaf",
		    their_string, my_string);
    }

  /*  We wrote the inner crc merely to merge it, so simply read it
      back and forget it.  */
  cfg.u32 ();

  if (modules_atom_p () != cfg.u ())
    {
      error_at (loc, "TS/ATOM mismatch");
    fail:
      cfg.set_overrun ();
      return cfg.end (slurp->from);
    }

  /* Check the CRC after the above sanity checks, so that the user is
     clued in.  */
  unsigned e_crc = crc;
  crc = cfg.get_crc ();
  dump () && dump ("Reading CRC=%x", crc);
  if (check_crc && crc != e_crc)
    {
      error_at (loc, "module %qM CRC mismatch", this);
      goto fail;
    }

  /* Check module name.  */
  const char *their_name = slurp->from->name (cfg.u ());
  if (strcmp (their_name, fullname))
    {
      error_at (loc, "module %qs found, expected module %qs",
		their_name, fullname);
      goto fail;
    }

  /* Check target & host.  */
  const char *their_target = slurp->from->name (cfg.u ());
  const char *their_host = slurp->from->name (cfg.u ());
  dump () && dump ("Read target='%s', host='%s'", their_target, their_host);
  if (strcmp (their_target, TARGET_MACHINE)
      || strcmp (their_host, HOST_MACHINE))
    {
      error_at (loc, "target & host is %qs:%qs, expected %qs:%qs",
		their_target, TARGET_MACHINE, their_host, HOST_MACHINE);
      goto fail;
    }

  /* Check compilation options.  For the moment we requre exact
     match.  */
  const char *their_opts = cfg.str ();
  if (!our_opts)
    our_opts = get_option_string ();
  if (strcmp (their_opts, our_opts))
    {
      error_at (loc, "compilation options differ %qs, expected %qs",
		their_opts, our_opts);
      goto fail;
    }
  
  /* Check global trees.  */
  unsigned their_fixed_length = cfg.u ();
  unsigned their_fixed_crc = cfg.u32 ();
  dump () && dump ("Read globals=%u, crc=%x",
		   their_fixed_length, their_fixed_crc);
  if (their_fixed_length != fixed_trees->length ()
      || their_fixed_crc != global_crc)
    {
      error_at (loc, "fixed tree mismatch");
      goto fail;
    }

  /* Allocate the REMAP vector.  */
  unsigned imports = cfg.u ();
  slurp->alloc_remap (imports);

  sec_range.first = cfg.u ();
  sec_range.second = cfg.u ();
  dump () && dump ("Declaration sections are [%u,%u)",
		   sec_range.first, sec_range.second);

  unnamed = cfg.u ();
  dump () && dump ("%u unnamed decls", unnamed);

  if (sec_range.first > sec_range.second
      || sec_range.second > slurp->from->get_section_limit ())
    {
      error_at (loc, "paradoxical declaration section range");
      goto fail;
    }

  return cfg.end (slurp->from);
}

/* The following writer functions rely on the current behaviour of
   depset::hash::add_dependency making the decl and defn depset nodes
   depend on eachother.  That way we don't have to worry about seeding
   the tree map with named decls that cannot be looked up by name (I.e
   template and function parms).  We know the decl and definition will
   be in the same cluster, which is what we want.  */

void
module_state::write_function_def (trees_out &out, tree decl)
{
  out.tree_node (DECL_RESULT (decl));
  out.tree_node (DECL_INITIAL (decl));
  out.tree_node (DECL_SAVED_TREE (decl));
  if (DECL_DECLARED_CONSTEXPR_P (decl))
    out.tree_node (find_constexpr_fundef (decl));
}

void
module_state::mark_function_def (trees_out &, tree)
{
}

bool
module_state::read_function_def (trees_in &in, tree decl)
{
  tree result = in.tree_node ();
  tree initial = in.tree_node ();
  tree saved = in.tree_node ();
  tree constexpr_body = (DECL_DECLARED_CONSTEXPR_P (decl)
			 ? in.tree_node () : NULL_TREE);

  if (in.get_overrun ())
    return NULL_TREE;

  if (TREE_CODE (CP_DECL_CONTEXT (decl)) == NAMESPACE_DECL)
    {
      if (mod != MAYBE_DECL_MODULE_OWNER (decl))
	{
	  error_at (loc, "unexpected definition of %q#D", decl);
	  in.set_overrun ();
	  return false;
	}

      if (!MAYBE_DECL_MODULE_PURVIEW_P (decl)
	  && DECL_SAVED_TREE (decl))
	return true; // FIXME check same
    }

  DECL_RESULT (decl) = result;
  DECL_INITIAL (decl) = initial;
  DECL_SAVED_TREE (decl) = saved;
  if (constexpr_body)
    register_constexpr_fundef (decl, constexpr_body);

  /* When lazy loading is in effect, we can be in the middle of
     parsing or instantiating a function.  */
  // FIXME:This smells bad
  tree old_cfd = current_function_decl;
  struct function *old_cfun = cfun;
  current_function_decl = decl;
  allocate_struct_function (decl, false);
  cfun->language = ggc_cleared_alloc<language_function> ();
  cfun->language->base.x_stmt_tree.stmts_are_full_exprs_p = 1;
  set_cfun (old_cfun);
  current_function_decl = old_cfd;

  if (!DECL_TEMPLATE_INFO (decl) || DECL_USE_TEMPLATE (decl))
    {
      comdat_linkage (decl);
      note_vague_linkage_fn (decl);
      cgraph_node::finalize_function (decl, false);
    }

  return true;
}

void
module_state::write_var_def (trees_out &out, tree decl)
{
  out.tree_node (DECL_INITIAL (decl));
}

void
module_state::mark_var_def (trees_out &, tree)
{
}

bool
module_state::read_var_def (trees_in &in, tree decl)
{
  tree init = in.tree_node ();

  if (in.get_overrun ())
    return false;

  DECL_INITIAL (decl) = init;

  return true;
}

/* Write the binfo heirarchy of TYPE.  The binfos are chained in DFS
   order, but it is a strongly connected graph, so requires two
   passes.  */

void
module_state::write_binfos (trees_out &out, tree type)
{
  /* Stream out types and sizes in DFS order, forcing each binfo
     into the map.  */
  for (tree child = TYPE_BINFO (type); child; child = TREE_CHAIN (child))
    {
      out.tree_node (BINFO_TYPE (child));
      // FIXME:The assertion in this comment is wrong
      /* We might have tagged the binfo during a by-reference walk.
	 Force a new tag now.  */
      int tag = out.insert (child, TREE_VISITED (child));
      if (out.streaming_p ())
	{
	  out.u (BINFO_N_BASE_BINFOS (child));

	  dump () && dump ("Wrote binfo:%d child %N of %N",
			   tag, BINFO_TYPE (child), type);
	}
    }

  out.tree_node (NULL_TREE);

  if (out.streaming_p ())
    {
      if (TYPE_LANG_SPECIFIC (type))
	{
	  unsigned nvbases = vec_safe_length (CLASSTYPE_VBASECLASSES (type));
	  out.u (nvbases);
	  if (nvbases)
	    dump () && dump ("Type %N has %u vbases", type, nvbases);
	}

      /* Stream out contents in DFS order.  */
      for (tree child = TYPE_BINFO (type); child; child = TREE_CHAIN (child))
	{
	  dump () && dump ("Writing binfo:%N of %N contents", child, type);

	  out.core_bools (child);
	  out.bflush ();
	  out.tree_node (child->binfo.offset);
	  out.tree_node (child->binfo.inheritance);
	  out.tree_node (child->binfo.vtable);
	  out.tree_node (child->binfo.virtuals);
	  out.tree_node (child->binfo.vptr_field);
	  out.tree_node (child->binfo.vtt_subvtt);
	  out.tree_node (child->binfo.vtt_vptr);

	  out.tree_vec (BINFO_BASE_ACCESSES (child));
	  unsigned num = vec_safe_length (BINFO_BASE_ACCESSES (child));
	  gcc_checking_assert (BINFO_N_BASE_BINFOS (child) == num);
	  for (unsigned ix = 0; ix != num; ix++)
	    out.tree_node (BINFO_BASE_BINFO (child, ix));
	}
    }
}

/* Read the binfo heirarchy of TYPE.  Sets TYPE_BINFO and
   CLASSTYPE_VBASECLASSES.  */

bool
module_state::read_binfos (trees_in &in, tree type)
{
  tree *binfo_p = &TYPE_BINFO (type);

  /* Stream in the types and sizes in DFS order.  */
  while (tree t = in.tree_node ())
    {
      unsigned n_children = in.u ();
      if (in.get_overrun ())
	return false;
      tree child = make_tree_binfo (n_children);
      BINFO_TYPE (child) = t;

      int tag = in.insert (child);
      dump () && dump ("Read binfo:%d child %N of %N", tag, child, type);
      *binfo_p = child;
      binfo_p = &TREE_CHAIN (child);
    }

  unsigned nvbases = 0;
  vec<tree, va_gc> *vbase_vec = NULL;
  if (TYPE_LANG_SPECIFIC (type))
    {
      nvbases = in.u ();
      if (nvbases)
	{
	  vec_alloc (vbase_vec, nvbases);
	  CLASSTYPE_VBASECLASSES (type) = vbase_vec;
	  dump () && dump ("Type %N has %u vbases", type, nvbases);
	}
    }

  /* Stream in the contents in DFS order.  */
  for (tree child = TYPE_BINFO (type); child; child = TREE_CHAIN (child))
    {
      dump () && dump ("Reading binfo:%N of %N contents", child, type);

      in.core_bools (child);
      in.bflush ();
      child->binfo.offset = in.tree_node ();
      child->binfo.inheritance = in.tree_node ();
      child->binfo.vtable = in.tree_node ();
      child->binfo.virtuals = in.tree_node ();
      child->binfo.vptr_field = in.tree_node ();
      child->binfo.vtt_subvtt = in.tree_node ();
      child->binfo.vtt_vptr = in.tree_node ();

      BINFO_BASE_ACCESSES (child) = in.tree_vec ();
      if (in.get_overrun ())
	return false;
      unsigned num = vec_safe_length (BINFO_BASE_ACCESSES (child));
      for (unsigned ix = 0; ix != num; ix++)
	BINFO_BASE_APPEND (child, in.tree_node ());

      if (BINFO_VIRTUAL_P (child))
	{
	  if (vec_safe_length (vbase_vec) == nvbases)
	    {
	      in.set_overrun ();
	      return false;
	    }
	  vbase_vec->quick_push (child);
	}
    }

  if (vec_safe_length (vbase_vec) != nvbases)
    in.set_overrun ();

  return !in.get_overrun ();
}

void
module_state::write_class_def (trees_out &out, tree type)
{
  out.chained_decls (TYPE_FIELDS (type));
  out.tree_node (TYPE_VFIELD (type));
  if (TYPE_LANG_SPECIFIC (type))
    {
      out.tree_vec (CLASSTYPE_MEMBER_VEC (type));
      out.tree_node (CLASSTYPE_FRIEND_CLASSES (type));
      out.tree_node (CLASSTYPE_LAMBDA_EXPR (type));

      /* TYPE_CONTAINS_VPTR_P looks at the vbase vector, which the
	 reader won't know at this point.  */
      // FIXME Think about better ordering
      int has_vptr = TYPE_CONTAINS_VPTR_P (type);
      if (out.streaming_p ())
	out.i (has_vptr);
      if (has_vptr)
	{
	  out.tree_vec (CLASSTYPE_PURE_VIRTUALS (type));
	  out.tree_pair_vec (CLASSTYPE_VCALL_INDICES (type));
	  out.tree_node (CLASSTYPE_KEY_METHOD (type));
	}
    }

  write_binfos (out, type);
  
  if (TYPE_LANG_SPECIFIC (type))
    {
      out.tree_node (CLASSTYPE_PRIMARY_BINFO (type));

      tree as_base = CLASSTYPE_AS_BASE (type);
      out.tree_node (as_base);
      if (as_base && as_base != type)
	write_class_def (out, as_base);

      tree vtables = CLASSTYPE_VTABLES (type);
      out.chained_decls (vtables);
      /* Write the vtable initializers.  */
      for (; vtables; vtables = TREE_CHAIN (vtables))
	out.tree_node (DECL_INITIAL (vtables));
    }

  // lang->nested_udts

  /* Now define all the members.  */
  for (tree member = TYPE_FIELDS (type); member; member = TREE_CHAIN (member))
    if (has_definition (member))
      {
	out.tree_node (member);
	write_definition (out, member);
      }

  /* End of definitions.  */
  out.tree_node (NULL_TREE);
}

void
module_state::mark_class_def (trees_out &out, tree type)
{
  for (tree member = TYPE_FIELDS (type); member; member = DECL_CHAIN (member))
    {
      out.mark_node (member);
      if (has_definition (member))
	mark_definition (out, member);
    }

  if (TYPE_LANG_SPECIFIC (type))
    {
      tree as_base = CLASSTYPE_AS_BASE (type);
      if (as_base && as_base != type)
	{
	  out.mark_node (as_base);
	  mark_class_def (out, as_base);
	}

      for (tree vtables = CLASSTYPE_VTABLES (type);
	   vtables; vtables = TREE_CHAIN (vtables))
	{
	  out.mark_node (vtables);
	  mark_var_def (out, vtables);
	}
    }
}

/* Nop sorting, needed for resorting the member vec.  */

static void
nop (void *, void *)
{
}

bool
module_state::read_class_def (trees_in &in, tree type)
{
  tree fields = in.chained_decls ();
  tree vfield = in.tree_node ();
  vec<tree, va_gc> *member_vec = NULL;
  vec<tree, va_gc> *pure_virts = NULL;
  vec<tree_pair_s, va_gc> *vcall_indices = NULL;
  tree key_method = NULL_TREE;
  tree lambda = NULL_TREE;
  tree friends = NULL_TREE;

  if (TYPE_LANG_SPECIFIC (type))
    {
      member_vec = in.tree_vec ();
      friends = in.tree_node ();
      lambda = in.tree_node ();

      int has_vptr = in.i ();
      if (has_vptr)
	{
	  pure_virts = in.tree_vec ();
	  vcall_indices = in.tree_pair_vec ();
	  key_method = in.tree_node ();
	}
    }

  // lang->nested_udts

  // FIXME: Sanity check stuff

  if (in.get_overrun ())
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

      /* Resort the member vector.  */
      resort_type_member_vec (member_vec, NULL, nop, NULL);
    }

  if (!read_binfos (in, type))
    return false;

  if (TYPE_LANG_SPECIFIC (type))
    {
      CLASSTYPE_PRIMARY_BINFO (type) = in.tree_node ();

      tree as_base = in.tree_node ();
      CLASSTYPE_AS_BASE (type) = as_base;
      if (as_base && as_base != type)
	{
	  TYPE_CONTEXT (as_base) = type;
	  read_class_def (in, as_base);
	}

      /* Read the vtables.  */
      // FIXME: via read_var_def?
      tree vtables = in.chained_decls ();
      if (!CLASSTYPE_KEY_METHOD (type) && vtables)
	vec_safe_push (keyed_classes, type);
      CLASSTYPE_VTABLES (type) = vtables;
      for (; vtables; vtables = TREE_CHAIN (vtables))
	DECL_INITIAL (vtables) = in.tree_node ();
    }

  // FIXME:
  if (false/*TREE_CODE (decl) == TEMPLATE_DECL*/)
    CLASSTYPE_DECL_LIST (type) = in.tree_node ();

  /* Propagate to all variants.  */
  fixup_type_variants (type);

  /* Now define all the members.  */
  while (tree member = in.tree_node ())
    {
      if (in.get_overrun ())
	break;
      if (!read_definition (in, member))
	break;
    }

  return !in.get_overrun ();
}

void
module_state::write_enum_def (trees_out &out, tree type)
{
  out.tree_node (TYPE_VALUES (type));
  out.tree_node (TYPE_MIN_VALUE (type));
  out.tree_node (TYPE_MAX_VALUE (type));
}

void
module_state::mark_enum_def (trees_out &out, tree type)
{
  if (!UNSCOPED_ENUM_P (type))
    for (tree values = TYPE_VALUES (type); values; values = TREE_CHAIN (values))
      out.mark_node (TREE_VALUE (values));
}

bool
module_state::read_enum_def (trees_in &in, tree type)
{
  tree values = in.tree_node ();
  tree min = in.tree_node ();
  tree max = in.tree_node ();

  if (in.get_overrun ())
    return false;

  TYPE_VALUES (type) = values;
  TYPE_MIN_VALUE (type) = min;
  TYPE_MAX_VALUE (type) = max;

  return true;
}

void
module_state::write_template_def (trees_out &out, tree decl)
{
  tree res = DECL_TEMPLATE_RESULT (decl);
  if (TREE_CODE (res) == TYPE_DECL && CLASS_TYPE_P (TREE_TYPE (res)))
    out.tree_node (CLASSTYPE_DECL_LIST (TREE_TYPE (res)));
  write_definition (out, res);
}

void
module_state::mark_template_def (trees_out &out, tree decl)
{
  tree res = DECL_TEMPLATE_RESULT (decl);
  if (TREE_CODE (res) == TYPE_DECL && CLASS_TYPE_P (TREE_TYPE (res)))
    for (tree decls = CLASSTYPE_DECL_LIST (TREE_TYPE (res));
	 decls; decls = TREE_CHAIN (decls))
      {
	/* There may be decls here, that are not on the member vector.
	   for instance forward declarations of member tagged types.  */
	tree member = TREE_VALUE (decls);
	if (TYPE_P (member))
	  /* In spite of its name, non-decls appear :(.  */
	  member = TYPE_NAME (member);
	gcc_assert (DECL_CONTEXT (member) == TREE_TYPE (decl));
	out.mark_node (member);
      }
  mark_definition (out, res);
}

bool
module_state::read_template_def (trees_in &in, tree decl)
{
  tree res = DECL_TEMPLATE_RESULT (decl);
  if (TREE_CODE (res) == TYPE_DECL && CLASS_TYPE_P (TREE_TYPE (res)))
    CLASSTYPE_DECL_LIST (TREE_TYPE (res)) = in.tree_node ();
  return read_definition (in, res);
}

/* Write out the body of DECL.  See above circularity note.  */

void
module_state::write_definition (trees_out &out, tree decl)
{
  if (out.streaming_p ())
    dump () && dump ("Writing definition %C:%N", TREE_CODE (decl), decl);

  switch (TREE_CODE (decl))
    {
    default:
      gcc_unreachable ();

    case TEMPLATE_DECL:
      write_template_def (out, decl);
      break;

    case FUNCTION_DECL:
      write_function_def (out, decl);
      break;

    case VAR_DECL:
      write_var_def (out, decl);
      break;

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (DECL_IMPLICIT_TYPEDEF_P (decl)
		    && TYPE_MAIN_VARIANT (type) == type);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  write_enum_def (out, type);
	else
	  write_class_def (out, type);
      }
      break;
    }
}

/* Mark the body of DECL.  */

void
module_state::mark_definition (trees_out &out, tree decl)
{
  switch (TREE_CODE (decl))
    {
    default:
      gcc_unreachable ();

    case TEMPLATE_DECL:
      mark_template_def (out, decl);
      break;

    case FUNCTION_DECL:
      mark_function_def (out, decl);
      break;

    case VAR_DECL:
      mark_var_def (out, decl);
      break;

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (DECL_IMPLICIT_TYPEDEF_P (decl)
		    && TYPE_MAIN_VARIANT (type) == type);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  mark_enum_def (out, type);
	else
	  mark_class_def (out, type);
      }
      break;
    }
}

/* Read in the body of DECL.  See above circularity note.  */

bool
module_state::read_definition (trees_in &in, tree decl)
{
  dump () && dump ("Reading definition %C %N", TREE_CODE (decl), decl);

  switch (TREE_CODE (decl))
    {
    default:
      break;

    case TEMPLATE_DECL:
      return read_template_def (in, decl);

    case FUNCTION_DECL:
      return read_function_def (in, decl);

    case VAR_DECL:
      return read_var_def (in, decl);

    case TYPE_DECL:
      {
	tree type = TREE_TYPE (decl);
	gcc_assert (DECL_IMPLICIT_TYPEDEF_P (decl)
		    && TYPE_MAIN_VARIANT (type) == type);
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  return read_enum_def (in, type);
	else
	  return read_class_def (in, type);
      }
      break;
    }

  return false;
}

/* Compare members of a cluster.  Order defn < decl < bind.  depsets
   of the same kind can be arbitrary, but we want something
   stable.  */

static int
cluster_cmp (const void *a_, const void *b_)
{
  depset *a = *(depset *const *)a_;
  depset *b = *(depset *const *)b_;

  bool is_defn = a->is_defn ();
  if (is_defn != b->is_defn ())
    /* Exactly one is a defn.  It comes first.  */
    return is_defn ? -1 : +1;

  if (!is_defn)
    {
      /* Neither is a defn, try order-by-decl.  */
      bool is_decl = a->is_decl ();
      if (is_decl != b->is_decl ())
	/* Exactly one is a decl.  It comes first.  */
	return is_decl ? -1 : +1;
    }

  /* They are both the same kind.  Order for qsort stability.  */
  tree a_decl = a->get_decl ();
  tree b_decl = b->get_decl ();

  if (a_decl != b_decl)
    /* Different decls, order by their UID.  */
    return DECL_UID (a_decl) < DECL_UID (b_decl) ? -1 : +1;

  /* Same decl.  They must be bindings.  Order by identifier hash
     (hey, it's a consistent number).  */
  gcc_checking_assert (a->is_binding ()
		       && a->get_name () != b->get_name ());
  return (IDENTIFIER_HASH_VALUE (a->get_name ())
	  < IDENTIFIER_HASH_VALUE (b->get_name ())
	  ? -1 : +1);
}

/* Contents of a cluster.  */
enum cluster_tag {
  ct_decl,	/* A decl.  */
  ct_defn,	/* A defn.  */
  ct_bind,	/* A binding.  */
  ct_horcrux,	/* Preseed reference to unnamed decl.  */
  ct_hwm
};

/* Write the cluster of depsets in SCC[0-SIZE).  These are ordered
   defns < decls < bindings.  */

void
module_state::write_cluster (elf_out *to, depset *scc[], unsigned size,
			     unsigned &unnamed, unsigned *crc_ptr)
{
  trees_out sec (to, this);
  sec.begin ();

  dump () && dump ("Writing SCC:%u %u depsets", scc[0]->section, size);
  dump.indent ();

  unsigned incoming_unnamed = unnamed;
  bool refs_unnamed_p = false;

  /* Determine horcrux numbers for unnamed decls.  Count lazy
     definitions.  */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];
      tree decl = b->get_decl ();

      if (b->refs_unnamed)
	refs_unnamed_p = true;

      if (b->is_unnamed)
	{
	  /* There is no binding for this decl.  It is therefore not
	     findable by name.  Determine its horcrux number.  */
	  dump () && dump ("Unnamed %u %N", unnamed, decl);
	  b->cluster = ++unnamed;
	}
    }

  if (refs_unnamed_p)
    /* We contain references to unnamed decls.  Seed those that are in
       earlier clusters (others will be within this cluster).  */
    for (unsigned ix = 0; ix != size; ix++)
      if (scc[ix]->refs_unnamed)
	{
	  depset *b = scc[ix];

	  for (unsigned jx = 0; jx != b->deps.length (); jx++)
	    {
	      depset *d = b->deps[jx];
	      if (d->is_unnamed && d->cluster <= incoming_unnamed)
		{
		  tree u_decl = d->get_decl ();
		  if (!TREE_VISITED (u_decl))
		    {
		      gcc_checking_assert (d->cluster);
		      sec.u (ct_horcrux);
		      sec.u (d->cluster - 1);
		      sec.insert (u_decl);
		      int type_tag = sec.maybe_insert_typeof (u_decl);
		      sec.u (type_tag != 0);
		      dump () && dump ("Created horcrux:%u for %N",
				       d->cluster - 1, u_decl);
		    }
		}
	    }
	}

  /* Mark members for walking.  */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];

      if (b->is_binding ())
	for (unsigned jx = b->deps.length (); jx--;)
	  gcc_checking_assert (TREE_VISITED (b->deps[jx]->get_decl ()));
      else
	{
	  tree decl = b->get_decl ();

	  sec.mark_node (decl);
	  if (b->is_defn ())
	    mark_definition (sec, decl);
	}
    }

  /* Now write every member.  */
  for (unsigned ix = 0; ix != size; ix++)
    {
      depset *b = scc[ix];
      dump () && dump (b->is_decl () ? "Depset:%u declaration %N"
		       : b->is_defn () ? "Depset:%u definition %N"
		       : "Depset:%u binding %P", ix, b->get_decl (),
		       b->is_binding () ? b->get_name () : NULL_TREE);
      tree decl = b->get_decl ();
#if 0
      // FIXME:Violated by global module entities that I get wrong
      gcc_checking_assert ((TREE_CODE (d) == NAMESPACE_DECL
			    && !DECL_NAMESPACE_ALIAS (d)
			    && TREE_PUBLIC (d))
			   || DECL_MODULE_OWNER (d) == MODULE_PURVIEW);
#endif

      if (b->is_binding ())
	{
	  sec.u (ct_bind);
	  sec.tree_ctx (decl, false, MODULE_PURVIEW);
	  sec.tree_node (b->get_name ());
	  /* Write in forward order, so reading will see the
	     exports first, thus building the overload chain will be
	     optimized.  */
	  for (unsigned jx = 0; jx != b->deps.length (); jx++)
	    sec.tree_node (b->deps[jx]->get_decl ());
	  /* Terminate the list.  */
	  sec.tree_node (NULL);
	}
      else
	{
	  sec.u (b->is_decl () ? ct_decl : ct_defn);
	  sec.tree_ctx (decl, false, MODULE_PURVIEW);
	  if (b->cluster)
	    dump () && dump ("Voldemort:%u %N", b->cluster - 1, decl);
	  sec.u (b->cluster);
	  if (b->is_defn ()) 
	    write_definition (sec, decl);
	}
    }

  /* We don't find the section by name.  Use depset's decl's name for
     human friendliness.  Prefer a defn over a decl.  */
  tree naming_decl = scc[0]->get_decl ();
  unsigned name = to->qualified_name (naming_decl, scc[0]->is_defn ());
  unsigned snum = sec.end (to, name, crc_ptr);

  for (unsigned ix = size; ix--;)
    gcc_checking_assert (scc[ix]->section == snum);

  dump.outdent ();
  dump () && dump ("Wrote SCC:%u section:%N", scc[0]->section, naming_decl);
}

/* Read a cluster from section SNUM.  */

bool
module_state::read_cluster (unsigned snum)
{
  trees_in sec (this);

  if (!sec.begin (loc, slurp->from, snum))
    return false;

  dump () && dump ("Reading section:%u", snum);
  dump.indent ();
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
	    tree type = NULL_TREE;

	    while (tree decl = sec.tree_node ())
	      {
		if (TREE_CODE (decl) == TYPE_DECL)
		  {
		    if (type)
		      sec.set_overrun ();
		    type = decl;
		  }
		else if (decls
			 || (TREE_CODE (decl) == TEMPLATE_DECL
			     && (TREE_CODE (DECL_TEMPLATE_RESULT (decl))
				 == FUNCTION_DECL)))
		  {
		    if (!DECL_DECLARES_FUNCTION_P (decl)
			|| (decls
			    && TREE_CODE (decls) != OVERLOAD
			    && TREE_CODE (decls) != FUNCTION_DECL))
		      sec.set_overrun ();
		    decls = ovl_make (decl, decls);
		    if (DECL_MODULE_EXPORT_P (decl))
		      OVL_EXPORT_P (decls) = true;
		  }
		else
		  decls = decl;
	      }

	    if (!set_module_binding (ns, name, mod, decls, type))
	      {
		sec.set_overrun ();
		dump () && dump ("Binding of %P", ns, name);
	      }
	  }
	  break;

	case ct_horcrux:
	  /* Resurrect a node from a horcrux.  */
	  {
	    unsigned index = sec.u ();
	    if (index < vec_safe_length (slurp->unnamed))
	      {
		mc_slot *slot = &(*slurp->unnamed)[index];

		if (slot->is_lazy ())
		  lazy_load (NULL, NULL, slot, false);
		tree decl = *slot;
		unsigned tag = sec.insert (decl);
		if (sec.u ())
		  sec.insert (TREE_TYPE (decl));
		dump () && dump ("Inserted horcrux:%d %N", tag, decl);
	      }
	    else
	      sec.set_overrun ();
	  }
	  break;

	case ct_decl:
	case ct_defn:
	  /* A decl or defn.  */
	  {
	    tree decl = sec.tree_node ();
	    if (unsigned voldemort = sec.u ())
	      {
		/* An unnamed node, register it.  */
		if (voldemort - 1 < vec_safe_length (slurp->unnamed))
		  {
		    (*slurp->unnamed)[voldemort - 1] = decl;
		    dump () && dump ("Voldemort decl:%u %N",
				     voldemort - 1, decl);
		  }
		else
		  sec.set_overrun ();
	      }
	    if (ct == ct_defn)
	      /* A definition.  */
	      read_definition (sec, decl);
	  }
	  break;
	}
    }
  dump.outdent ();

  if (!sec.end (slurp->from))
    return false;

  return true;
}

/* SPACES is a sorted vector of namespaces.  Write out the namespaces
   to MOD_SNAME_PFX.nms section.

   Each namespace is:
     u:name,
     u:context, number of containing namespace (0 == ::)
     u:inline_p  */

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
      tree ns = b->get_decl ();

      gcc_checking_assert (TREE_CODE (ns) == NAMESPACE_DECL
			   && (DECL_NAME (ns) || !TREE_PUBLIC (ns))
			   && (TREE_PUBLIC (ns) == DECL_MODULE_EXPORT_P (ns)));

      b->section = ix + 1;
      unsigned ctx_num = 0;
      tree ctx = CP_DECL_CONTEXT (ns);
      if (ctx != global_namespace)
	ctx_num = table.find (depset::decl_key (ctx))->section;
      dump () && dump ("Writing namespace %u %N parent:%u",
		       b->section, ns, ctx_num);

      sec.u (to->name (DECL_NAME (ns)));
      sec.u (ctx_num);
      /* Don't use a bool, because this can be near the end of the
	 section, and it won't save anything anyway.  */
      sec.u (DECL_NAMESPACE_INLINE_P (ns));
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

  if (!sec.begin (loc, slurp->from, MOD_SNAME_PFX ".nms"))
    return false;

  dump () && dump ("Reading namespaces");
  dump.indent ();

  spaces.safe_push (global_namespace);
  while (sec.more_p ())
    {
      const char *name = slurp->from->name (sec.u ());
      unsigned parent = sec.u ();
      /* See comment in write_namespace about why not a bit.  */
      bool inline_p = bool (sec.u ());

      if (parent >= spaces.length ())
	sec.set_overrun ();
      if (sec.get_overrun ())
	break;

      tree id = name ? get_identifier (name) : NULL_TREE;
      dump () && dump ("Read namespace %P %u",
		       spaces[parent], id, spaces.length ());
      tree inner = add_imported_namespace (spaces[parent],
					   mod, id, inline_p);
      spaces.safe_push (inner);
    }
  dump.outdent ();
  if (!sec.end (slurp->from))
    return false;

  return true;
}

/* Write the binding TABLE to MOD_SNAME_PFX.bind

   Each binding is:
     u:name
     u:context - number of containing namespace
     u:section - section number of binding. */

void
module_state::write_bindings (elf_out *to, depset::hash &table, unsigned *crc_p)
{
  dump () && dump ("Writing binding table");
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  depset::hash::iterator end (table.end ());
  for (depset::hash::iterator iter (table.begin ()); iter != end; ++iter)
    {
      depset *b = *iter;
      if (b->is_binding ())
	{
	  unsigned ns_num = 0;
	  tree ns = b->get_decl ();
	  if (ns != global_namespace)
	    ns_num = table.find (depset::decl_key (ns))->section;
	  dump () && dump ("Bindings %P section:%u", ns, b->get_name (),
			   b->section);
	  sec.u (to->name (b->get_name ()));
	  sec.u (ns_num);
	  sec.u (b->section);
	}
    }

  sec.end (to, to->name (MOD_SNAME_PFX ".bnd"), crc_p);
  dump.outdent ();
}

/* Read the binding table from MOD_SNAME_PFX.bind.  */

bool
module_state::read_bindings (auto_vec<tree> &spaces, const range_t &range)
{
  bytes_in sec;

  if (!sec.begin (loc, slurp->from, MOD_SNAME_PFX ".bnd"))
    return false;

  dump () && dump ("Reading binding table");
  dump.indent ();
  while (sec.more_p ())
    {
      const char *name = slurp->from->name (sec.u ());
      unsigned nsnum = sec.u ();
      unsigned snum = sec.u ();

      if (nsnum >= spaces.length () || !name
	  || snum < range.first || snum >= range.second)
	sec.set_overrun ();
      if (sec.get_overrun ())
	break;
      tree ctx = spaces[nsnum];
      tree id = get_identifier (name);
      dump () && dump ("Bindings %P section:%u", ctx, id, snum);
      if (mod >= MODULE_IMPORT_BASE
	  && !import_module_binding (ctx, id, mod, snum))
	break;
    }

  dump.outdent ();
  if (!sec.end (slurp->from))
    return false;
  return true;
}

/* Write the unnamed table to MOD_SNAME_PFX.vdm

   Each entry is a section number.  */

void
module_state::write_unnamed (elf_out *to, auto_vec<depset *> &depsets,
			     unsigned count, unsigned *crc_p)
{
  if (!count)
    return;

  dump () && dump ("Writing unnamed");
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  unsigned current = 0;
  for (unsigned ix = 0; ix < depsets.length (); ix++)
    {
      depset *d = depsets[ix];

      if (d->cluster)
	{
	  dump () && dump ("Unnamed %d %N section:%u",
			   current, d->get_decl (), d->section);
	  current++;
	  gcc_checking_assert (d->cluster == current);
	  sec.u (d->section);
	}
      }
  gcc_assert (count == current);
  sec.end (to, to->name (MOD_SNAME_PFX ".vld"), crc_p);
  dump.outdent ();
}

bool
module_state::read_unnamed (unsigned count, const range_t &range)
{
  if (!count)
    return true;

  bytes_in sec;

  if (!sec.begin (loc, slurp->from, MOD_SNAME_PFX ".vld"))
    return false;

  dump () && dump ("Reading unnamed");
  dump.indent ();

  slurp->alloc_unnamed (count);
  for (unsigned ix = 0; ix != count; ix++)
    {
      unsigned snum = sec.u ();

      if (snum < range.first || snum >= range.second)
	sec.set_overrun ();
      if (sec.get_overrun ())
	break;

      dump () && dump ("Unnamed %u section:%u", ix, snum);

      mc_slot s;
      s = NULL_TREE;
      s.set_lazy (snum);
      slurp->unnamed->quick_push (s);
    }

  dump.outdent ();
  if (!sec.end (slurp->from))
    return false;
  return true;
}

/* Read & write locations.  */

void
module_state::write_location (bytes_out &sec, location_t loc)
{
  if (!modules_atom_p ())
    return;

  if (!IS_ORDINARY_LOC (loc))
    // FIXME: implement macro & adhoc
    loc = UNKNOWN_LOCATION;

  spewing *spew = spewer ();
  gcc_assert (loc < spew->early_locs.second
	      || (loc >= spew->late_locs.first && loc < spew->late_locs.second));
  sec.u (unsigned (loc));
}

location_t
module_state::read_location (bytes_in &sec)
{
  if (!modules_atom_p ())
    return loc;

  unsigned off = sec.u ();
  slurping *slurp = slurper ();
  source_location loc = UNKNOWN_LOCATION;

  /* late_locs may not have been read in yet.  */
  if (off < slurp->early_locs.first)
    loc = (slurp->pre_early_ok || off <= BUILTINS_LOCATION
	   ? off : UNKNOWN_LOCATION);
  else if (off < slurp->early_locs.second)
    loc = off + slurp->loc_offsets.first;
  else if (off >= slurp->late_locs.first
	   && off < slurp->late_locs.second)
    loc = off + slurp->loc_offsets.second;
  else
    sec.set_overrun ();

  return loc;
}

/* Prepare for location streaming.  We break the line maps into two
   blocks -- those before the preamble processing, and those after.
   The gap will be occupied by maps from imports (including each
   import itself).  */

unsigned
spewing::prepare_linemaps (line_maps *lmaps, bool early_p)
{
  unsigned lwm = (early_p ? early_loc_map : late_loc_map).first;
  unsigned hwm = (early_p ? early_loc_map : late_loc_map).second;

  unsigned mask = 0;
  for (unsigned ix = lwm; ix != hwm; ix++)
    {
      const line_map_ordinary *map = LINEMAPS_ORDINARY_MAP_AT (lmaps, ix);
      const char *fname = ORDINARY_MAP_FILE_NAME (map);

      dump () && dump ("Map %u file=%s", ix, fname);
      if (map->m_column_and_range_bits > mask)
	mask = map->m_column_and_range_bits;
      /* We expect very few filenames, so just an array.  */
      for (unsigned jx = filenames->length (); jx--;)
	{
	  const char *name = (*filenames)[jx];
	  if (0 == strcmp (name, fname))
	    {
	      /* Reset the linemap's name, because for things like
	         preprocessed input we could have multple instances of
	         the same name, and we'd rather not percolate
	         that.  */
	      const_cast <line_map_ordinary *> (map)->to_file = name;
	      fname = NULL;
	      break;
	    }
	}
      if (fname)
	vec_safe_push (filenames, fname);
    }

  unsigned first = MAP_START_LOCATION (LINEMAPS_ORDINARY_MAP_AT (lmaps, lwm));
  unsigned last = (early_p
		   ? MAP_START_LOCATION (LINEMAPS_ORDINARY_MAP_AT (lmaps, hwm))
		   : lmaps->highest_location + 1);

  (early_p ? early_locs : late_locs).first = first;
  (early_p ? early_locs : late_locs).second = last;

  return mask;
}

/* Setup for streaming locations.  We break the line maps into two
   blocks -- those before the preamble processing, and those after.
   The gap will be occupied by maps from imports (including each
   import itself).  We detect the preamble map index range, and the
   locations range for both early and late locs.  */

void
module_state::prepare_locations (line_maps *lmaps)
{
  spewing *spew = spewer ();

  /* Skip over the command-line, built-in and forced header line
     maps.  We require (& check) them to be the same in all TUs.  */
  if (spew->early_loc_map.second)
    spew->early_loc_map.first = prefix_line_maps_hwm;

  spew->late_loc_map.second = LINEMAPS_ORDINARY_USED (lmaps);

  /* Collect the set of file names, location ranges and alignment.  */
  spew->loc_offsets.first = spew->prepare_linemaps (lmaps, true);
  spew->loc_offsets.second = spew->prepare_linemaps (lmaps, false);
}

/* Write the early or late location maps.  The early maps include the
   filename table.  */

void
module_state::write_locations (elf_out *to, line_maps *lmaps,
			       bool early_p, unsigned *crc_p)
{
  dump () && dump ("Writing %s locations", early_p ? "early" : "late");
  dump.indent ();

  bytes_out sec (to);
  sec.begin ();

  spewing *spew = spewer ();

  if (early_p)
    {
      /* Write the filename table.  */
      unsigned len = spew->filenames->length ();
      sec.u (len);
      dump () && dump ("%u source file names", len);
      for (unsigned ix = 0; ix != len; ix++)
	{
	  const char *fname = (*spew->filenames)[ix];
	  dump () && dump ("Source file[%u]=%s", ix, fname);
	  sec.str (fname);
	}
      sec.u (prefix_line_maps_hwm);
      sec.u (prefix_locations_hwm);
    }

  unsigned lwm = (early_p ? spew->early_loc_map : spew->late_loc_map).first;
  unsigned hwm = (early_p ? spew->early_loc_map : spew->late_loc_map).second;
  sec.u (hwm - lwm);
  unsigned first = (early_p ? spew->early_locs : spew->late_locs).first;
  unsigned last =  (early_p ? spew->early_locs : spew->late_locs).second;
  sec.u (first);
  sec.u (last);
  unsigned align = (early_p ? spew->loc_offsets.first
		    : spew->loc_offsets.second);
  sec.u (align);

  dump () && dump ("Linemaps:%u locations:[%u:%u) alignment:%u",
		   hwm - lwm, first, last, align);

  for (unsigned ix = lwm; ix != hwm; ix++)
    {
      const line_map_ordinary *map = LINEMAPS_ORDINARY_MAP_AT (lmaps, ix);

      sec.u (MAP_START_LOCATION (map));
      /* Making accessors just for here, seems excessive.  */
      sec.u (map->reason);
      sec.u (map->sysp);
      sec.u (map->m_column_and_range_bits);
      sec.u (map->m_range_bits);
      const char *fname = ORDINARY_MAP_FILE_NAME (map);
      for (unsigned ix = 0; ix != spew->filenames->length (); ix++)
	if ((*spew->filenames)[ix] == fname)
	  {
	    sec.u (ix);
	    break;
	  }
      sec.u (ORDINARY_MAP_STARTING_LINE_NUMBER (map));
      write_location (sec, linemap_included_from (map));
    }

  const char *name = early_p ? MOD_SNAME_PFX ".elo" : MOD_SNAME_PFX ".llo";
  sec.end (to, to->name (name), crc_p);
  dump.outdent ();
}

/* Read the early or late location maps.  The early ones must be read
   first.  */

bool
module_state::read_locations (line_maps *lmaps, bool early_p)
{
  bytes_in sec;

  const char *name = early_p ? MOD_SNAME_PFX ".elo" : MOD_SNAME_PFX ".llo";
  if (!sec.begin (loc, slurp->from, name))
    return false;
  dump () && dump ("Reading %s locations", early_p ? "early" : "late");
  dump.indent ();

  slurping *slurp = slurper ();
  if (early_p)
    {
      /* Read the filename table.  */
      unsigned len = sec.u ();
      dump () && dump ("%u source file names", len);
      slurp->alloc_filenames (len);
      for (unsigned ix = 0; ix != len; ix++)
	{
	  size_t l;
	  const char *buf = sec.str (&l);
	  char *fname = XNEWVEC (char, l + 1);
	  memcpy (fname, buf, l + 1);
	  dump () && dump ("Source file[%u]=%s", ix, fname);
	  /* We leak these names into the line-map table.  But it
	     doesn't own them.  */
	  slurp->filenames->quick_push (fname);
	}

      unsigned pre_lwm = sec.u ();
      unsigned pre_loc = sec.u ();
      slurp->pre_early_ok = (pre_lwm == prefix_line_maps_hwm
			     && pre_loc == prefix_locations_hwm);
    }

  unsigned num_maps = sec.u ();
  unsigned first_loc = sec.u ();
  unsigned last_loc = sec.u ();
  unsigned align = sec.u ();

  dump () && dump ("Linemaps:%u locations:[%u:%u) alignment:%u",
		   num_maps, first_loc, last_loc, align);


  /* Map first_loc to > hwm such that align bits are preserved.  */
  int new_first = ((lmaps->highest_location + (1u << align) - 1)
		   & ~((1u << align) - 1u));
  unsigned offset = new_first - first_loc;
  
  dump () && dump ("Remap offset %d", int (offset));
  (early_p ? slurp->early_locs : slurp->late_locs).first = first_loc;
  (early_p ? slurp->early_locs : slurp->late_locs).second = last_loc;
  (early_p ? slurp->loc_offsets.first : slurp->loc_offsets.second) = offset;

  line_map_ordinary *base = static_cast <line_map_ordinary *>
    (line_map_new_raw (lmaps, false, num_maps));

  lmaps->highest_location = last_loc + offset - 1;

  for (unsigned ix = 0; ix != num_maps; ix++)
    {
      line_map_ordinary *map = base + ix;
      map->start_location = sec.u () + offset;
      map->reason = lc_reason (sec.u ());
      map->sysp = sec.u ();
      map->m_column_and_range_bits = sec.u ();
      map->m_range_bits = sec.u ();
      unsigned fnum = sec.u ();
      map->to_file = (fnum < slurp->filenames->length ()
		      ? (*slurp->filenames)[fnum] : "");
      map->to_line = sec.u ();
      map->included_from = read_location (sec);
      if (map->included_from == UNKNOWN_LOCATION)
	map->included_from = loc;
    }

  dump.outdent ();
  if (!sec.end (slurp->from))
    return false;

  if (early_p && !slurp->pre_early_ok)
    /* Clue the user in.  */
    warning_at (first_loc + offset, 0,
		"prefix mismatch, earlier locations are not represented");

  return true;
}


/* Recursively find all the namespace bindings of NS.
   Add a depset for every binding that contains an export or
   module-linkage entity.  Add a defining depset for every such decl
   that we need to write a definition.  Such defining depsets depend
   on the binding depset.  Returns true if we contain something
   explicitly exported.  */

void
module_state::add_writables (depset::hash &table, tree ns)
{
  auto_vec<tree> decls;

  dump () && dump ("Finding writables in %N", ns);
  dump.indent ();
  hash_table<named_decl_hash>::iterator end
    (DECL_NAMESPACE_BINDINGS (ns)->end ());
  for (hash_table<named_decl_hash>::iterator iter
	 (DECL_NAMESPACE_BINDINGS (ns)->begin ()); iter != end; ++iter)
    {
      tree bind = *iter;

      if (TREE_CODE (bind) == MODULE_VECTOR)
	{
	  bind = (MODULE_VECTOR_CLUSTER
		  (bind, (MODULE_SLOT_CURRENT / MODULE_VECTOR_SLOTS_PER_CLUSTER))
		  .slots[MODULE_SLOT_CURRENT % MODULE_VECTOR_SLOTS_PER_CLUSTER]);
	  if (!bind)
	    continue;
	}

      if (tree name = extract_module_decls (bind, decls))
	{
	  dump () && dump ("Writable bindings at %P", ns, name);
	  if (tree inner = table.add_binding (ns, name, decls))
	    {
	      gcc_checking_assert (TREE_PUBLIC (inner));
	      add_writables (table, inner);
	    }
	  decls.truncate (0);
	}
    }
  dump.outdent ();
}

/* Iteratively find dependencies.  During the walk we may find more
   entries on the same binding that need walking.  */

void
module_state::find_dependencies (depset::hash &table)
{
  trees_out walker (NULL, this);

  while (depset *d = table.get_work ())
    {
      walker.begin (&table);

      gcc_checking_assert (!d->is_binding ());
      tree decl = d->get_decl ();
      dump () && dump ("%s %N", d->is_decl () ? "Declaration" : "Definition",
		       decl);
      dump.indent ();
      walker.mark_node (decl);
      if (d->is_defn ())
	mark_definition (walker, decl);
      /* Turn the Sneakoscope on when depending the decl.  */
      table.sneakoscope = true;
      walker.tree_node (decl);
      table.sneakoscope = false;
      if (d->is_defn ())
	write_definition (walker, decl);
      dump.outdent ();
      walker.end ();
    }
}

/* Compare bindings for two namespaces.  Those closer to :: are
   less.  */

static int
space_cmp (const void *a_, const void *b_)
{
  depset *a = *(depset *const *)a_;
  depset *b = *(depset *const *)b_;
  tree ns_a = a->get_decl ();
  tree ns_b = b->get_decl ();

  gcc_checking_assert (ns_a != ns_b);

  /* Deeper namespaces come after shallower ones.  */
  if (int delta = int (SCOPE_DEPTH (ns_a)) - int (SCOPE_DEPTH (ns_b)))
    return delta;

  /* Otherwise order by UID for consistent results.  */
  return DECL_UID (ns_a) < DECL_UID (ns_b) ? -1 : +1;
}

/* Use ELROND format to record the following sections:
     1     MOD_SNAME_PFX.README   : human readable, stunningly STRTAB-like
     [2-N) qualified-names	  : binding value(s)
     N     MOD_SNAME_PFX.nms 	  : namespace hierarchy
     N+1   MOD_SNAME_PFX.bnd      : binding table
     N+2   MOD_SNAME_PFX.vld      : unnamed table
     N+3   MOD_SNAME_PFX.imp      : import table
     N+4   MOD_SNAME_PFX.elo      : early locations
     N+5   MOD_SNAME_PFX.llo      : late locations
     N+6   MOD_SNAME_PFX.cfg      : config data
*/

void
module_state::write (elf_out *to, line_maps *lmaps)
{
  unsigned crc = 0;

  if (!our_opts)
    our_opts = get_option_string ();
  write_readme (to, our_opts);

  depset::hash table (200);

  /* Find the set of decls we must write out.  */
  add_writables (table, global_namespace);

  find_dependencies (table);

  if (modules_atom_p ())
    {
      spewer ()->alloc_filenames ();
      prepare_locations (lmaps);
    }

  /* Find the SCCs. */
  auto_vec<depset *> sccs (table.size ());
  depset::tarjan connector (sccs);

  /* Iteration over the hash table is an unspecified ordering.  That's
     good (for now) because it'll give us some random code coverage.
     We may want to fill the worklist and sort it in future
     though?  */
  depset::hash::iterator end (table.end ());
  for (depset::hash::iterator iter (table.begin ()); iter != end; ++iter)
    {
      depset *v = *iter;
      if (!v->cluster)
	connector.connect (v);
    }

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
  range_t range;
  range.first = range.second = to->get_section_limit ();
  for (unsigned size, ix = 0; ix < sccs.length (); ix += size)
    {
      depset **base = &sccs[ix];

      /* Count the members in this cluster.  */
      for (size = 1; ix + size < sccs.length (); size++)
	{
	  if (base[size]->cluster != base[0]->cluster)
	    break;
	  base[size]->cluster = base[size]->section = 0;
	  gcc_checking_assert (base[size]->is_binding ()
			       || (TREE_CODE (base[size]->get_decl ())
				   != NAMESPACE_DECL)
			       || (DECL_NAMESPACE_ALIAS
				   (base[size]->get_decl ())));
	}
      base[0]->cluster = base[0]->section = 0;

      /* Sort the cluster.  Later processing makes use of the ordering
	 of defns < decls < bindings. */
      qsort (base, size, sizeof (depset *), cluster_cmp);

      if (base[0]->is_decl ()
	  && TREE_CODE (base[0]->get_decl ()) == NAMESPACE_DECL
	  && !DECL_NAMESPACE_ALIAS (base[0]->get_decl ()))
	/* A namespace decl, these are handled specially.  */
	n_spaces++;
      else
	{
	  /* Save the size in the first member's cluster slot.  */
	  base[0]->cluster = size;
	  /* Set the section number.  */
	  for (unsigned jx = size; jx--;)
	    base[jx]->section = range.second;
	  range.second++;
	}
    }

  /* Write the clusters.  Namespace decls are put in the spaces array.
     The meaning of depset::cluster changes to provide the
     unnamed-decl count of the depset's decl (and remains zero for
     non-decls and non-unnamed).  */
  auto_vec<depset *> spaces (n_spaces);
  unsigned unnamed = 0;
  for (unsigned size, ix = 0; ix < sccs.length (); ix += size)
    {
      depset **base = &sccs[ix];
      size = base[0]->cluster;

      if (!size)
	{
	  /* A namespace  */
	  spaces.quick_push (base[0]);
	  size = 1;
	}
      else
	{
	  /* Cluster is now used to number unnamed decls.  */
	  base[0]->cluster = 0;

	  write_cluster (to, base, size, unnamed, &crc);
	}
    }

  /* We'd better have written as many sections and found as many
     namespaces as we predicted.  */
  gcc_assert (range.second == to->get_section_limit ()
	      && spaces.length () == n_spaces);

  /* Write the namespaces.  */
  (spaces.qsort) (space_cmp);
  write_namespaces (to, table, spaces, &crc);

  /* Write the bindings themselves.  */
  write_bindings (to, table, &crc);

  /* Write the unnamed.  */
  write_unnamed (to, sccs, unnamed, &crc);

  /* Write the import table.  */
  write_imports (to, &crc);

  /* Write the line maps.  */
  if (modules_atom_p ())
    {
      write_locations (to, lmaps, true, &crc);
      write_locations (to, lmaps, false, &crc);
      spewer ()->free_filenames ();
    }

  /* And finish up.  */
  write_config (to, our_opts, range, unnamed, crc);

  trees_out::instrument ();
  dump () && dump ("Wrote %u sections", to->get_section_limit ());
}

/* Read a BMI from STREAM.  E is errno from its fopen.  Reading will
   be lazy, if this is an import and flag_module_lazy is in effect.  */

void
module_state::read (int fd, int e, line_maps *lmaps, bool check_crc)
{
  gcc_checking_assert (!slurp);
  slurp = new slurping (new elf_in (fd, e));
  if (!slurp->from->begin (loc))
    return;

  range_t range;
  unsigned unnamed;

  if (!read_config (range, unnamed, check_crc))
    return;

  /* Read the early locations.  */
  if (modules_atom_p ())
    read_locations (lmaps, true);

  /* Read the import table.  */
  if (!read_imports (lmaps))
    return;

  /* Determine the module's number.  */
  gcc_checking_assert (mod == MODULE_UNKNOWN);
  if (this == (*modules)[MODULE_PURVIEW])
    mod = MODULE_PURVIEW;
  else
    {
      unsigned ix = modules->length ();
      if (ix == MODULE_LIMIT)
	{
	  sorry ("too many modules loaded (limit is %u)", ix);
	  slurp->from->set_error (elf::E_BAD_IMPORT);
	  return;
	}
      else
	{
	  vec_safe_push (modules, this);
	  bitmap_set_bit (imports, ix);
	  bitmap_set_bit (exports, ix);
	}
      mod = ix;
    }

  (*slurp->remap)[MODULE_PURVIEW] = mod;
  dump () && dump ("Assigning %N module number %u", name, mod);

  /* We should not have been frozen during the importing done by
     read_config.  */
  gcc_assert (!slurp->from->is_frozen ());

  /* Read the late locations.  */
  if (modules_atom_p ())
    {
      read_locations (lmaps, false);
      /* We'll leak if we returned early, but that's gonna be a fatal
	 error, so who cares?  */
      slurper ()->free_filenames ();
    }

  /* Read the namespace hierarchy. */
  auto_vec<tree> spaces;
  if (!read_namespaces (spaces))
    return;

  /* And the bindings.  */
  if (!read_bindings (spaces, range))
    return;

  /* And unnamed.  */
  if (!read_unnamed (unnamed, range))
    return;

  /* We're done with the string and non-decl sections now.  */
  slurp->from->release ();
  slurp->remaining = range.second - range.first;
  slurp->lru = ++lazy_lru;

  if (mod == MODULE_PURVIEW || !flag_module_lazy)
    {
      /* Read the sections in forward order, so that dependencies are read
	 first.  See note about tarjan_connect.  */
      unsigned hwm = range.second;
      for (unsigned ix = range.first; ix != hwm; ix++)
	{
	  load_section (ix);
	  if (slurp->from->has_error ())
	    break;
	}
    }
}

void
module_state::maybe_defrost ()
{
  if (slurp->from->is_frozen ())
    {
      if (lazy_open <= 0)
	freeze_an_elf ();
      dump () && dump ("Defrosting %s", filename);
      slurp->from->defrost (maybe_add_bmi_prefix (filename));
      lazy_open--;
    }
}

/* Load section SNUM, dealing with laziness.  */

void
module_state::load_section (unsigned snum)
{
  maybe_defrost ();

  unsigned old_current = slurp->current;
  slurp->current = snum;
  slurp->lru = 0;  /* Do not swap out.  */
  read_cluster (snum);
  slurp->lru = ++lazy_lru;
  slurp->current = old_current;
  slurp->remaining--;
}

/* After a reading operation, make sure things are still ok.  If not,
   emit an error and clean up.  In order to get some kind of context
   information, OUTERMOST is true, if this is the outermost cause of a
   read happening (eiher an import, or a lazy binding found during
   name-lookup).  In the latter case NS and ID provide the binding.  */

bool
module_state::check_read (bool outermost, tree ns, tree id)
{
  bool done = (slurp->current == ~0u
	       && (slurp->from->has_error () || !slurp->remaining));
  if (done)
    {
      lazy_open++;
      slurp->from->end ();
    }

  int e = slurp->from->has_error ();
  if (e)
    {
      const char *err = slurp->from->get_error (filename);
      /* Failure to read a module is going to cause big
	 problems, so bail out, if this is the top level.
	 Otherwise return NULL to let our importer know (and
	 fail).  */
      if (slurp->remaining && id)
	error_at (loc, "failed to load binding %<%E%s%E@%M%>: %s",
		  ns, &"::"[ns == global_namespace ? 2 : 0], id, this, err);
      else if (filename)
	error_at  (loc, "failed to read module %qs: %s", filename, err);
      else
	error_at  (loc, "failed to read module: %s", err);

      if (e == EMFILE
	  || e == ENFILE
#ifdef MAPPED_READING
	  || e == ENOMEM
#endif
	  || false)
	inform (loc, "consider using %<-fno-module-lazy%> or"
		" reducing %<--param %s%> value",
		compiler_params[PARAM_LAZY_MODULES].option);
    }

  if (done)
    release (false);

  if (e && outermost)
    fatal_error (loc, "jumping off the crazy train to crashville");

  return e;
}

/* Return the IDENTIFIER_NODE naming module IX.  This is the name
   including dots.  */

char const *
module_name (unsigned ix)
{
  return (*modules)[ix]->fullname;
}

/* Return the vector of IDENTIFIER_NODES naming module IX.  These are
   individual identifers per sub-module component.  */

tree
module_vec_name (unsigned ix)
{
  return (*modules)[ix]->vec_name;
}

/* Return the bitmap describing what modules are imported into
   MODULE.  Remember, we always import ourselves.  */

bitmap
module_import_bitmap (unsigned ix)
{
  return (*modules)[ix]->imports;
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

static GTY(()) module_state *proclaimer;
static int export_depth; /* -1 for singleton export.  */

/* Nest a module export level.  Return true if we were already in a
   level.  */

int
push_module_export (bool singleton, module_state *proclaiming)
{
  int previous = export_depth;

  if (proclaiming)
    {
      proclaimer = proclaiming;
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
   by the TYPE_DECL).  It might not be an outermost namespace-scope
   decl.  */

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
      /* The implicit typedef of a tagged type has its own module
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
  
  if (!modules)
    /* We can be called when modules are not enabled.  */
    return;

  // FIXME: check ill-formed linkage

  if ((*modules)[MODULE_PURVIEW])
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

  if (!modules)
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

/* Return true iff we're in the purview of a named module.  */

bool
module_purview_p ()
{
  /* We get called very early on, so need to check there's a vector.  */
  return modules && (*modules)[MODULE_PURVIEW];
}

/* Return true iff we're the interface TU (this also means we're in a
   module purview.  */

bool
module_interface_p ()
{
  return ((*modules)[MODULE_PURVIEW]
	  && (*modules)[MODULE_PURVIEW]->exported);
}

/* Create a location for module.  FROM is the importing module, which
   is NULL for direct importation.  If FROM is shallower than
   whatever may have previously set the location, we're reseated to
   be FROM from.  */

void
module_state::set_loc (line_maps *lmaps,
		       const module_state *from, location_t floc)
{
  unsigned lwm = (from ? from->depth : 0) + 1;
  gcc_checking_assert (lwm != 65536);
  if (depth > lwm)
    {
      depth = lwm;
      if (!from)
	floc = from_loc;
      const char *name_str = NULL;
      if (loc == UNKNOWN_LOCATION)
	name_str = fullname;
      else
	dump () && dump ("Reseating %M to import of %M", this, from);
      loc = linemap_module_loc (lmaps, floc, loc, name_str);
    }
}

void
module_state::attach (location_t from)
{
  from_loc = from;
  /* Create a TREE_VEC of components.  */
  auto_vec<tree,5> ids;
  size_t len = 0;
  for (module_state *probe = this; probe; probe = probe->parent)
    {
      ids.safe_push (probe->name);
      len += IDENTIFIER_LENGTH (probe->name);
    }
  unsigned elts = ids.length ();
  if (elts == 1)
    fullname = IDENTIFIER_POINTER (name);
  else
    {
      fullname = XNEWVEC (char, ids.length () + len);
      len = 0;
    }

  vec_name = make_tree_vec (elts);
  for (unsigned ix = 0; ix != elts; ix++)
    {
      tree elt = ids.pop ();
      TREE_VEC_ELT (vec_name, ix) = elt;
      if (elts > 1)
	{
	  if (len)
	    const_cast <char *> (fullname)[len++] = '.';
	  memcpy (const_cast <char *> (fullname) + len,
		  IDENTIFIER_POINTER (elt), IDENTIFIER_LENGTH (elt) + 1);
	  len += IDENTIFIER_LENGTH (elt);
	}
    }
}

/* Read the BMI file for a module.  FNAME, if not NULL, is the name we
   know it as.  CRC_PTR points to the CRC value we expect.  */

bool
module_state::do_import (char const *fname, line_maps *lmaps, bool check_crc)
{
  gcc_assert (global_namespace == current_scope ()
	      && !is_imported () && loc != UNKNOWN_LOCATION);

  if (lazy_open <= 0)
    freeze_an_elf ();

  if (fname)
    {
      gcc_assert (!filename);
      filename = xstrdup (fname);
    }

  int fd = -1;
  int e = ENOENT;
  if (filename)
    {
      fd = open (maybe_add_bmi_prefix (filename), O_RDONLY | O_CLOEXEC);
      e = errno;
    }

  announce ("importing");
  imported = true;
  lazy_open--;
  
  read (fd, e, lmaps, check_crc);
  bool failed = check_read (direct && !modules_atom_p ());
  announce (flag_module_lazy && mod != MODULE_PURVIEW ? "lazy" : "imported");

  return !failed;
}

/* Pick a victim module to freeze its reader.  */

void
module_state::freeze_an_elf ()
{
  module_state *victim = NULL;
  for (unsigned ix = modules->length (); ix--;)
    {
      module_state *candidate = (*modules)[ix];
      if (candidate && candidate->slurp && candidate->slurp->lru
	  && candidate->slurp->from->is_freezable ()
	  && (!victim || victim->slurp->lru > candidate->slurp->lru))
	victim = candidate;
    }

  if (victim)
    {
      dump () && dump ("Freezing %s", victim->filename);
      victim->slurp->from->freeze ();
      lazy_open++;
    }
  else
    dump () && dump ("No module available for freezing");
}

/* *SLOT is a lazy binding in namepsace NS named ID.  Load it, or die
   trying.  */

bool
module_state::lazy_load (tree ns, tree id, mc_slot *mslot, bool outermost)
{
  unsigned n = dump.push (this);

  unsigned snum = mslot->get_lazy ();
  dump () && dump ("Lazily binding %P@%N section:%u", ns, id, name, snum);

  if (snum < slurp->current && flag_module_lazy)
    load_section (snum);

  if (mslot->is_lazy ())
    slurp->from->set_error (elf::E_BAD_LAZY);

  bool failed = check_read (outermost, ns, id);
  gcc_assert (!failed || !outermost);
 
  dump.pop (n);

  return !failed;
}

/* Load MOD's binding for NS::ID into *MSLOT.  *MSLOT contains the
   lazy cookie.  OUTER is true if this is the outermost lazy, (used
   for diagnostics).  */

void
lazy_load_binding (unsigned mod, tree ns, tree id, mc_slot *mslot, bool outer)
{
  gcc_checking_assert (mod >= MODULE_IMPORT_BASE);
  (*modules)[mod]->lazy_load (ns, id, mslot, outer);
}

void
pp_module_name (pretty_printer *pp, module_state *state)
{
  pp_string (pp, state->fullname);
}

/* Import the module NAME into the current TU and maybe re-export it.  */

void
import_module (module_state *imp, location_t from_loc, bool exporting,
	       tree, line_maps *lmaps)
{
  if (export_depth)
    exporting = true;

  gcc_assert (global_namespace == current_scope ());
  from_loc = ordinary_loc_of (lmaps, from_loc);

  module_state *purview = (*modules)[MODULE_PURVIEW];
  if (purview == imp)
    {
      /* Cannot import the current module.  */
      error_at (from_loc, "cannot import module %qM in its own purview", imp);
      inform (imp->from_loc, "module %qM declared here", imp);
      return;
    }

  if (imp->is_detached ())
    imp->attach (from_loc);

  imp->direct = true;
  if (exporting)
    imp->exported = true;

  if (!modules_atom_p ())
    {
      if (!imp->is_imported ())
	{
	  unsigned n = dump.push (imp);
	  char *fname = module_mapper::import_export (imp, false);
	  imp->set_loc (lmaps);
	  imp->do_import (fname, lmaps, false);
	  dump.pop (n);
	}
      (*modules)[MODULE_NONE]->set_import (imp, imp->exported);
    }

  gcc_assert (global_namespace == current_scope ());
}

/* Declare the name of the current module to be NAME.  EXPORTING_p is
   true if this TU is the exporting module unit.  */

void
declare_module (module_state *state, location_t from_loc, bool exporting_p,
		tree, line_maps *lmaps)
{
  gcc_assert (global_namespace == current_scope ());

  from_loc = ordinary_loc_of (lmaps, from_loc);
  if (module_state *purview = (*modules)[MODULE_PURVIEW])
    {
      /* Already declared the module.  */
      error_at (from_loc, "cannot declare module in purview of module %qM",
		purview);
      return;
    }

  if (!state->is_detached ())
    {
      /* Cannot be module unit of an imported module.  */
      error_at (from_loc, "cannot declare module after import");
      inform (state->from_loc, "module %qM imported here", state);
      return;
    }

  if (state->is_legacy () != modules_legacy_p ())
    error_at (from_loc,
	      state->is_legacy ()
	      ? G_("legacy module only permitted with %<-fmodules-header%>")
	      : G_("legacy module expected with %<-fmodules-header%>"));


  state->attach (from_loc);

  if (state->is_legacy ())
    {
      module_legacy_name = state->fullname;

      /* The user may have named the module before the main file.  */
      const line_map_ordinary *map
	= linemap_check_ordinary (linemap_lookup (lmaps, from_loc));
      atom_main_file (lmaps, map,
		      map - LINEMAPS_ORDINARY_MAPS (line_table));
    }

  /* Copy any importing information we may have already done.  */
  if (!modules_atom_p ())
    {
      module_state *global = (*modules)[MODULE_NONE];
      state->imports = global->imports;
    }
  else
    gcc_checking_assert (modules->length () == MODULE_IMPORT_BASE);

  (*modules)[MODULE_NONE] = state;
  (*modules)[MODULE_PURVIEW] = state;
  current_module = MODULE_PURVIEW;
  if (exporting_p)
    {
      state->slurp = new spewing ();
      state->exported = true;
      state->mod = MODULE_PURVIEW;
    }
  else
    {
      state->direct = true;
      state->mod = MODULE_UNKNOWN;
    }

  if (!modules_atom_p ())
    {
      unsigned n = dump.push (state);
      char *fname = module_mapper::import_export (state, exporting_p);
      state->set_loc (lmaps);
      if (!exporting_p)
	state->do_import (fname, lmaps, false);
      else if (fname)
	state->filename = xstrdup (fname);
      dump.pop (n);
    }
}

/* Reverse source import location order.  */

static int
module_from_cmp (const void *a_, const void *b_)
{
  module_state *a = *(module_state *const *)a_;
  module_state *b = *(module_state *const *)b_;

  return a->from_loc > b->from_loc ? -1 : +1;
}

/* The module preamble has been parsed.  Now load all the imports.
   LOC is the location of the first declaration (the module decl, if
   this is a module).  */

int
module_state::atom_preamble (location_t loc, line_maps *lmaps)
{
  /* Iterate over the module hash, informing the mapper of every not
     loaded (direct) import.  */
  unsigned limit = modules_hash->elements ();
  if (!limit)
    /* The hash table could be empty due to parse errors.  */
    return 0;

  dump.push (NULL);
  dump () && dump ("Processing preamble");

  /* Preserve the state of the line-map.  */
  unsigned pre_hwm = LINEMAPS_ORDINARY_USED (lmaps);

  /* For a consistent order, and avoiding hash table iteration during
     actual import, we create a vector of imports.  */
  auto_vec<module_state *> directs (limit);

  module_state *interface = NULL;
  if (module_state *mod = (*modules)[MODULE_PURVIEW])
    if (mod->exported)
      interface = mod;

  module_mapper *mapper = module_mapper::get (loc);
  if (!mapper->is_live ())
    return false;

  hash_table<module_state_hash>::iterator end (modules_hash->end ());
  for (hash_table<module_state_hash>::iterator iter (modules_hash->begin ());
       iter != end; ++iter)
    {
      module_state *imp = *iter;
      gcc_checking_assert (mapper->is_file () || !imp->filename);
      if (imp->direct)
	directs.quick_push (imp);
      else
	gcc_checking_assert (imp == interface || imp->is_detached ());
    }

  if (!mapper->is_file ())
    {
      /* Send batched request to mapper.  */
      if (directs.length ())
	mapper->cork ();
      if (interface)
	mapper->imex_query (interface, 0);
      if (directs.length ())
	{
	  /* Put in reverse source order, for consistency.  */
	  (directs.qsort) (module_from_cmp);
	  for (unsigned ix = directs.length (); ix--;)
	    mapper->imex_query (directs[ix], -1);
	  mapper->uncork ();
	  mapper->bewait_cmd (loc);
	}

      /* Read the mapper's responses.  */
      if (interface)
	if (char *fname = mapper->imex_response (interface))
	  interface->filename = xstrdup (fname);
      if (directs.length ())
	{
	  bool bewait = true;
	  for (unsigned ix = directs.length (); ix--;)
	    {
	      if (!mapper->corked ())
		{
		  if (!bewait)
		    mapper->bewait_cmd (loc);
		  bewait = !bewait;
		}
	      if (module_state *imp = mapper->bewait_response (loc))
		dump () && dump ("Received BMI name for %M", imp);
	      else
		break;
	    }
	}
      mapper->maybe_uncork (loc);
    }

  /* Check we know all the BMIs.  There's no point trying to load any
     if some are missing.  */
  bool ok = true;
  for (unsigned ix = directs.length (); ix--;)
    {
      module_state *imp = directs[ix];
      if (!imp->filename)
	{
	  ok = false;
	  error_at (imp->from_loc, "module %qM is unknown", imp);
	}
      imp->set_loc (lmaps);
    }

  if (ok)
    {
      /* Now do the importing, which might cause additional requests
	 (although nested import filenames are usually in their
	 importer's import table).  */
      while (directs.length ())
	{
	  module_state *imp = directs.pop ();
	  unsigned n = dump.push (imp);
	  if (imp->is_imported () || imp->do_import (NULL, lmaps, false))
	    {
	      if (imp->mod != MODULE_PURVIEW)
		(*modules)[MODULE_NONE]->set_import (imp, imp->exported);
	    }
	  else
	    ok = false;
	  dump.pop (n);
	}
    }

  if (interface)
    {
      interface->set_loc (lmaps);

      /* Record the size of the hole the preamble created in the line
	 table.  */
      spewing *spew = interface->spewer ();
      spew->early_loc_map.second = pre_hwm;
      spew->late_loc_map.first = LINEMAPS_ORDINARY_USED (lmaps);
    }

  dump.pop (0);

  unsigned adj = linemap_module_restore (lmaps, pre_hwm);

  return ok ? int (adj) : -1;
}

/* Figure out whether to treat HEADER as an include or an import.  */

static int
do_divert_include (cpp_reader *reader, line_maps *lmaps, location_t loc,
		   const char *header, bool angle)
{
  if (!prefix_locations_hwm)
    /* Before the main file, don't divert.  */
    return 0;

  if (module_preamble_end_loc)
    reader = NULL;

  dump.push (NULL);

  dump () && dump ("Checking %sdiversion of include %c%s%c",
		   reader ? "" : "post-preamble ",
		   angle ? '<' : '"', header, angle ? '>' : '"');
  int res = 0;
  module_mapper *mapper = module_mapper::get (loc);
  if (mapper->is_live ())
    res = mapper->divert_include (reader, lmaps, loc, header, angle);

  if (reader)
    dump () && dump (res ? "Diverting include to import"
		     : "Keeping include as include");
  else if (res)
    {
      bool warned = warning_at (loc, OPT_Wlegacy_header,
				"header %c%s%c cannot be a legacy module"
				" because it is after the preamble",
				angle ? '<' : '"', header, angle ? '>' : '"');
      /* We should have issued a warning, because that was the only
	 point of checking.  */
      gcc_checking_assert (warned);
      if (warned)
	inform (module_preamble_end_loc, "preamble ended here");

      /* Don't actually divert! */
      res = 0;
    }

  dump.pop (0);

  return res;
}

cpp_divert_include_t *
atom_divert_include ()
{
  /* Wlegacy-header should default differently in preprocessing mode
     than in compilation mode.  */
  if (warn_legacy_header < 0)
    warn_legacy_header = !flag_preprocess_only;

  /* We enable include diversion in atom mode -- not just legacy
     header mode.  */
  return modules_atom_p () ? do_divert_include : NULL;
}

void
atom_preamble_end (cpp_reader *reader, location_t loc)
{
  module_preamble_end_loc = loc;

  if (!warn_legacy_header)
    {
      /* Turn off incude diversion.  */
      cpp_callbacks *cb = cpp_get_callbacks (reader);
      cb->divert_include = NULL;
    }
}

/* We've just properly entered the main source file.  I.e. after the
   command line, builtins and forced headers.  Record the line map and
   location of this map.  Note we may be called more than once.  The
   first call sticks.  */

void
atom_main_file (line_maps *, const line_map_ordinary *map, unsigned ix)
{
  if (modules_atom_p () && !prefix_locations_hwm)
    {
      prefix_line_maps_hwm = ix;
      prefix_locations_hwm = MAP_START_LOCATION (map);
    }
}

bool
maybe_atom_legacy_module (line_maps *lmaps)
{
  if (!modules_legacy_p ())
    return false;

  if (!module_legacy_name
      || (module_legacy_name[0] != '"'
	  && module_legacy_name[0] != '<'))
    {
      /* Set the module header name from the main_input_filename.  */
      const char *main = module_legacy_name;
      size_t len, pos;
      if (!main)
	{
	  main = main_input_filename;
	  len = strlen (main), pos = len;
	  for (; pos-- > 0; pos--)
	    if (main[pos] == '.'
		&& IS_DIR_SEPARATOR (main[pos-1])
		&& IS_DIR_SEPARATOR (main[pos+1]))
	      {
		pos += 2;
		break;
	      }
	  len -= pos;
	}
      else
	len = strlen (module_legacy_name), pos = 0;

      /* Create a user legacy module.  */
      char *name = XNEWVEC (char, len + 3);
      module_legacy_name = name;
      name[0] = '"';
      name[len + 1] = '"';
      name[len + 2] = 0;
      memcpy (name + 1, main + pos, len);
    }

  location_t loc
    =  MAP_START_LOCATION (LINEMAPS_MAP_AT (lmaps, false,
					    prefix_line_maps_hwm - 1));
  tree name = get_identifier (module_legacy_name);

  declare_module (get_module (name, NULL), loc, true, NULL, lmaps);
  /* Everything is exported.  */
  push_module_export (false, NULL);
  return true;
}

/* Do processing after a (non-empty) atom preamble has been parsed.  LOC is the
   final location of the preamble.  */

unsigned
atom_module_preamble (location_t loc, line_maps *lmaps)
{
  int adj = module_state::atom_preamble (loc, lmaps);
  if (adj < 0)
    fatal_error (loc, "returning to gate for a mechanical issue");

  if (modules_legacy_p ())
    /* Everything is exported.  */
    push_module_export (false, NULL);

  return unsigned (adj);
}

/* Convert the module search path.  */

void
init_module_processing ()
{
  /* PCH should not be reachable because of lang-specsm but the
     user could have overriden that.  */
  if (pch_file)
    fatal_error (input_location,
		 "C++ modules incompatible with precompiled headers");

  /* Check for ill-formed combinations.  */
  if (!modules_atom_p ()
      && (module_legacy_name || flag_module_preamble >= 0))
    {
      flag_modules = module_legacy_name ? -2 : -1;
      error ("%s not suppported with %<-fmodules-ts%>",
	     module_legacy_name ? "legacy headers" : "preamble parsing");
    }

  module_state::init ();
}

/* Finalize the module at end of parsing.  */

void
finish_module (line_maps *lmaps)
{
  if (modules_legacy_p ())
    pop_module_export (0);

  module_state *state = (*modules)[MODULE_PURVIEW];
  if (!state || !state->exported)
    ;/* Not a module interface.  */
  else if (errorcount)
    warning_at (state->loc, 0, "not exporting module due to errors");
  else
    {
      int fd = -1;
      int e = ENOENT;
      if (state->filename)
	{
	  fd = open (maybe_add_bmi_prefix (state->filename),
		     O_RDWR | O_CREAT | O_TRUNC | O_CLOEXEC,
		     S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
	  e = errno;
	}
      unsigned n = dump.push (state);
      state->announce ("creating");

      elf_out to (fd, e);
      if (to.begin ())
	state->write (&to, lmaps);
      if (to.end ())
	error_at (state->loc, "failed to export module: %s",
		  to.get_error (state->filename));

      dump.pop (n);
      if (!errorcount)
	module_mapper::export_done (state);
      ggc_collect ();
    }

  if (state)
    {
      if (state->exported && state->filename && errorcount)
	unlink (state->filename);
      state->release ();
    }

  XDELETEVEC (our_opts);
  our_opts = NULL;

  set_bmi_repo (NULL);
  module_mapper::fini (input_location);
  module_state::fini ();
}

/* Try and exec ourselves to repeat the preamble scan with
   foreknowledge of where it ends.  Sure, this way of doing it sucks,
   performance wise,  but that's one of the ways of encouraging users
   to explicitly disambiguate the difficult case.  */

void
maybe_repeat_preamble (location_t loc, int count ATTRIBUTE_UNUSED, cpp_reader *)
{
  if (flag_module_preamble >= 0)
    /* Something went wrong.  Don't try again.  */
    return;

  if (strcmp (main_input_filename, "-") == 0)
    /* We cannot rescan stdin.  */
    return;

#ifdef HAVE_EXECV
  /* Exec ourselves.  */
  dump.push (NULL);
  dump () && dump ("About to reexec with prefix length %u", count);
  module_state::fini ();
  module_mapper::fini (loc);

  /* The preprocessor does not leave files open, so we can ignore the
     pfile arg.  */

  int argc = original_argc;
  char **argv = XNEWVEC (char *, argc + 2 + 10);
  memcpy (argv, original_argv, argc * sizeof (char *));

  /* Use the extra space for the new option.  */
  char *fpreamble = reinterpret_cast <char *> (&argv[argc + 2]);
  argv[argc++] = fpreamble;
  argv[argc] = NULL;

  /* It's dangerous to go alone!  Take this.  */
  sprintf (fpreamble, "-fmodule-preamble=%d", count);

  dump.pop (0);
  if (noisy_p ())
    fprintf (stderr, "Reinvoking %s with %s due to ambiguous preamble\n",
	     argv[0], fpreamble);

  /* You have to wake up.  */
  execv (argv[0], argv);
  fatal_error (loc, "I was stung by a Space Bee");
#endif
}

/* If CODE is a module option, handle it & return true.  Otherwise
   return false.  For unknown reasons I cannot get the option
   generation machinery to set fmodule-mapper or -fmodule-header to
   make a string type option variable.  */

bool
handle_module_option (unsigned code, const char *str, int num)
{
  switch (opt_code (code))
    {
    case OPT_fmodule_preamble_:
      flag_module_preamble = num;
      /* Default ATOM.  */
      if (!flag_modules)
	flag_modules = -1;
      return true;

    case OPT_fmodules_atom:
      /* Don't drop out of legacy mode.  */
      if (flag_modules >= 0)
	flag_modules = -1;
      return true;

    case OPT_fmodule_mapper_:
      module_mapper_name = str;
      return true;

    case OPT_fmodules_legacy_:
      {
	/* Default ATOM legacy.  */
	size_t len = strlen (str);
	if (str[0] == '<' ? str[len-1] != '>'
	    : str[0] == '"' ? str[len-1] != '"'
	    : false)
	  error ("legacy name %qs is ill-formed", str);
	else if (str[0] == '"' && (str[1] == '"' || str[1] == '<'))
	  error ("legacy name %qs appears to be double quoted", str);
	else
	  module_legacy_name = str;

	if (flag_modules <= 0)
	  flag_modules = -2;
      }
      return true;

    default:
      return false;
    }
}

#include "gt-cp-module.h"

#if 1
/* Use of vec<unsigned, va_gc_atomic> caused these fns to be needed.  */
void gt_pch_nx (unsigned int &) {}
void gt_pch_nx (unsigned int *, void (*)(void *, void *), void *) {}
/* Use of vec<char *, va_gc_atomic> caused this fn to be needed.  */
void gt_pch_nx (char *&)  {}
/* I know not why.  */
#endif
