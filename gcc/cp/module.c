/* -*- C++ -*- modules.  Experimental!
   Copyright (C) 2017 Free Software Foundation, Inc.
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

/* MODULE_STAMP is a #define passed in from the Make file.  When
   present, it is used for version stamping the binary files, and
   indicates experimentalness of the module system.

   Comments in this file have a non-negligible chance of being wrong
   or at least inaccurate.  Due to (a) my misunderstanding, (b)
   ambiguities that I have interpretted differently to original intent
   (c) changes in the specification, (d) my poor wording.  */

#ifndef MODULE_STAMP
#error "Stahp! What are you doing? This is not ready yet."
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
#include "tree-diagnostic.h"

/* Id for dumping the class heirarchy.  */
int module_dump_id;
 
/* State of a particular module. */
struct GTY(()) module_state {
  /* We always import & export ourselves.  */
  bitmap imports;	/* Transitive modules we're importing.  */
  bitmap exports;	/* Subset of that, that we're exporting.  */
  tree name;		/* Name of the module.  */
  vec<tree, va_gc> *name_parts;  /* Split parts of name.  */
  hashval_t name_hash;  /* Name hash. */
  int direct_import;	/* Direct import/rexport of main module.  */
  unsigned mod;		/* Module index.  */
  unsigned crc;		/* CRC we saw reading it in. */
  unsigned HOST_WIDE_INT stamp;	/* Timestamp we saw reading it in.  */

 public:
  module_state ();

 public:
  void freeze (const module_state *);
  void set_index (unsigned index);
  void set_name (tree name, hashval_t, unsigned crc);
  void do_import (unsigned index, bool is_export);

 public:
  void dump (FILE *, bool);
};

/* Hash module state by name.  */

struct module_state_hash : ggc_remove <module_state *>
{
  typedef module_state *value_type;
  typedef tree compare_type; /* An identifier.  */

  static hashval_t hash (const value_type m)
  {
    return m->name_hash;
  }
  static bool equal (const value_type existing, compare_type candidate)
  {
    if (TREE_CODE (candidate) != TREE_CODE (existing->name))
      return false;
    else if (identifier_p (candidate))
      return existing->name == candidate;
    else
      return (TREE_STRING_LENGTH (existing->name)
	      == TREE_STRING_LENGTH (candidate))
	&& !memcmp (TREE_STRING_POINTER (existing->name),
		    TREE_STRING_POINTER (candidate),
		    TREE_STRING_LENGTH (candidate));
  }

  static inline void mark_empty (value_type &p) {p = NULL;}
  static inline bool is_empty (value_type p) {return !p;}

  /* Nothing is deletable.  Everything is insertable.  */
  static bool is_deleted (value_type) { return false; }
  static void mark_deleted (value_type) { gcc_unreachable (); }
};

/* Vector of module state.  */
static GTY(()) vec<module_state *, va_gc> *modules;

/* We need a module state, even if we're not a module.  We promote
   this to a real module upon meeting the module declaration.  */
static GTY(()) module_state *this_module;

/* Map from identifier to module index. */
static GTY(()) hash_table<module_state_hash> *module_hash;

/* Module search path.  */
static cpp_dir *module_path;

/* Longest module path.  */
static size_t module_path_max;

static int validate_module_name (bool, const char *, size_t);

module_state::module_state ()
  : imports (BITMAP_GGC_ALLOC ()), exports (BITMAP_GGC_ALLOC ()),
    name (NULL_TREE), name_parts (NULL), direct_import (0), mod (~0u),
    crc (0), stamp (0)
{
}

void module_state::freeze (const module_state *other)
{
  gcc_assert (!other->name);
  bitmap_copy (imports, other->imports);
}

/* We've been assigned INDEX.  Mark the self-import-export bits.  */

void
module_state::set_index (unsigned index)
{
  gcc_checking_assert (mod == ~0u);
  bitmap_set_bit (imports, index);
  bitmap_set_bit (exports, index);
}

/* Set NAME and PARTS fields from incoming NAME.  The name must have
   already been checked for well-formedness.  */

void
module_state::set_name (tree name_, hashval_t hash_, unsigned crc_)
{
  name = name_;
  name_hash = hash_;
  crc = crc_;

  size_t len;
  const char *ptr;
  char sep;
  char *buffer = NULL;

  if (identifier_p (name))
    {
      len = IDENTIFIER_LENGTH (name);
      ptr = IDENTIFIER_POINTER (name);
      sep = '.';
    }
  else
    {
      len = TREE_STRING_LENGTH (name) - 1;
      ptr = TREE_STRING_POINTER (name);
      sep = DIR_SEPARATOR;
    }

  const char *dot;
  do
    {
      dot = (const char *)memchr (ptr, sep, len);
      size_t l = dot ? dot - ptr : len;

      gcc_assert (l);

      size_t id_l = l;
      const char *id_p = ptr;

      if (sep != '.')
	{
	  /* Look for identfier-veboten characters and escape them.
	     That is leading digits, non-alphanumeric chars and _.
	     Escape as _<esc>
	       0x21-0x2f (14) _A->_O
	       0x30-0x3a (11) _P->_Z
	       0x3b-0x3f (5)  _g->_k
	       0x40	 (1)  _l
	       0x5B-0x5f (5)  _m->_q
	       0x60	 (1)  _r
	       0x7b-0x7f (5)  _s->_w
	     Notice _0-_9, _a-_f are not used.	  */
	  bool escape = ISDIGIT (*ptr);
	  for (size_t frag = l; !escape && frag--;)
	    if (!ISALNUM (ptr[frag]))
	      escape = true;
	  if (escape)
	    {
	      if (!buffer)
		buffer = XNEWVEC (char, len * 2);
	      id_p = buffer;
	      id_l = 0;
	      for (size_t frag = 0; frag != l; frag++)
		{
		  unsigned char c = ptr[frag];
		  if (!(ISALPHA (c) || (frag && ISDIGIT (c))))
		    {
		      if (c >= 0x21 && c <= 0x2f)
			c = 'A' + (c - 0x21);
		      else if (c >= 0x30 && c <= 0x3a)
			c = 'P' + (c - 0x30);
		      else if (c >= 0x3b && c <= 0x40)
			c = 'g' + (c - 0x3b);
		      else if (c >= 0x5b && c <= 0x60)
			c = 'm' + (c - 0x5b);
		      else if (c >= 0x7b && c <= 0x7f)
			c = 's' + (c - 0x7b);
		      else
			gcc_unreachable ();
		      buffer[id_l++] = '_';
		    }
		  buffer[id_l++] = c;
		}
	    }
	}

      vec_safe_reserve (name_parts,
			vec_safe_length (name_parts) + 1,
			!name_parts && !dot);
      name_parts->quick_push (get_identifier_with_length (id_p, id_l));
      if (dot)
	l++;
      ptr += l;
      len -= l;
    }
  while (dot);

  XDELETEVEC (buffer);
}

/* Return the IDENTIFIER_NODE naming module IX.  This is the name
   including dots.  */

tree
module_name (unsigned ix)
{
  return (*modules)[ix]->name;
}

/* Return the vector of IDENTIFIER_NODES naming module IX.  These are
   individual identifers per sub-module component.  */

vec<tree, va_gc> *
module_name_parts (unsigned ix)
{
  return (*modules)[ix]->name_parts;
}

/* Return the bitmap describing what modules are imported into
   MODULE.  Remember, we always import ourselves.  */

bitmap
module_import_bitmap (unsigned ix)
{
  const module_state *state = (*modules)[ix];

  return state ? state->imports : NULL;
}

/* Return the context that controls what module DECL is in.  That is
   the outermost-non-namespace context.  */

tree
module_context (tree decl)
{
  for (;;)
    {
      tree outer = CP_DECL_CONTEXT (decl);
      if (TYPE_P (outer))
	{
	  // FIXME CLASSTYPE_AS_BASE doesn't have a name.
	  if (tree name = TYPE_NAME (outer))
	    outer = name;
	  else
	    outer = TYPE_NAME (CP_TYPE_CONTEXT (outer));
	}
      if (TREE_CODE (outer) == NAMESPACE_DECL)
	break;
      decl = outer;
    }
  return decl;
}

/* We've just directly imported INDEX.  Update our import/export
   bitmaps.  IS_EXPORT is true if we're reexporting the module.  */

void
module_state::do_import (unsigned index, bool is_export)
{
  module_state *other = (*modules)[index];

  if (this == this_module)
    other->direct_import = 1 + is_export;
  bitmap_ior_into (imports, other->exports);
  if (is_export)
    bitmap_ior_into (exports, other->exports);
}

enum import_kind 
{
  ik_indirect,
  ik_direct,
  ik_interface,
  ik_implementation
};

/* Byte serializer base.  */
class cpm_serial {
protected:
  FILE *stream;
public:
  const char *name;
protected:
  char *buffer;
  size_t pos;
  size_t len;
  size_t alloc;
  int err;
  unsigned bit_val;
  unsigned bit_pos;
  unsigned crc;

public:
  cpm_serial (FILE *, const char *);
  ~cpm_serial ();

public:
  int error ()
  {
    return err;
  }


public:
  unsigned get_crc ()
  {
    return crc + !crc;
  }

public:
  /* Set an error.  We store the first errno.  */
  void bad (int e = -1)
  {
    if (!err)
      err = e;
  }

protected:
  /* Finish bit packet.  Compute crc of bits used, rewind the bytes
     not used.  */
  unsigned bit_flush ()
  {
    gcc_assert (bit_pos);
    unsigned bytes = (bit_pos + 7) / 8;
    pos -= 4 - bytes;
    crc_unsigned_n (bit_val, bytes);
    bit_pos = 0;
    bit_val = 0;
    return bytes;
  }

protected:
  void crc_unsigned_n (unsigned v, unsigned n)
  {
    crc = crc32_unsigned_n (crc, v, n);
  }
  void crc_buffer (const char *ptr, size_t l);
  template<typename T> void crc_unsigned (T v)
  {
    unsigned bytes = sizeof (T);
    while (bytes > 4)
      {
	bytes -= 4;
	crc_unsigned_n (unsigned (v >> (bytes * 8)), 4);
      }
    crc_unsigned_n (unsigned (v), bytes);
  }
};

cpm_serial::cpm_serial (FILE *s, const char *n)
  :stream (s), name (n), pos (0), len (0),
   /* Force testing of buffer extension. */
   alloc (MODULE_STAMP ? 1 : 32768),
   err (0), bit_val (0), bit_pos (0), crc (0)
{
  buffer = XNEWVEC (char, alloc);
}

cpm_serial::~cpm_serial ()
{
  gcc_assert (pos == len || err);
  XDELETEVEC (buffer);
}

void
cpm_serial::crc_buffer (const char *ptr, size_t l)
{
  unsigned c = crc;
  for (size_t ix = 0; ix != l; ix++)
    c = crc32_byte (c, ptr[ix]);
  crc = c;
}

class cpm_stream;

/* Byte stream writer.  */
class cpm_writer : public cpm_serial {
  /* Bit instrumentation.  */
  unsigned spans[3];
  unsigned lengths[3];
  int is_set;
  unsigned checksums;

public:
  cpm_writer (FILE *s, const char *n)
    : cpm_serial (s, n)
  {
    spans[0] = spans[1] = spans[2] = 0;
    lengths[0] = lengths[1] = lengths[2] = 0;
    is_set = -1;
    checksums = 0;
  }
  ~cpm_writer ()
  {
  }

private:
  size_t reserve (size_t);
  void flush ();
public:
  void seek (unsigned);

public:
  void raw (unsigned);

public:
  int done ()
  {
    flush ();
    if (fflush (stream))
      bad (errno);
    return error ();
  }

public:
  void checkpoint ();
  void instrument (cpm_stream *d);

public:
  void b (bool);
  void bflush ();

public:
  void c (unsigned char);
  void i (int);
  void u (unsigned);
  void s (size_t s);
  void wi (HOST_WIDE_INT);
  void wu (unsigned HOST_WIDE_INT);
  void str (const char *, size_t);
  void module_name (tree);
  void buf (const char *, size_t);
};

/* Byte stream reader.  */
class cpm_reader : public cpm_serial {
public:
  cpm_reader (FILE *s, const char *n)
    : cpm_serial (s, n)
  {
  }
  ~cpm_reader ()
  {
  }

private:
  size_t fill (size_t);
public:
  unsigned raw ();

public:
  int done (bool atend = true)
  {
    if (atend && fill (1))
      bad ();
    return error ();
  }

public:
  bool checkpoint ();

public:
  bool b ();
  void bflush ();
private:
  void bfill ();

public:
  int c ();
  int i ();
  unsigned u ();
  size_t s ();
  HOST_WIDE_INT wi ();
  unsigned HOST_WIDE_INT wu ();
  const char *str (size_t * = NULL);
  tree module_name ();
  const char *buf (size_t);
};

/* Checkpoint a crc.  */

inline void
cpm_writer::checkpoint ()
{
  checksums++;
  raw (crc);
}

bool
cpm_reader::checkpoint ()
{
  if (raw () != crc)
    {
      /* Map checksum error onto a reasonably specific errno.  */
#if defined (EPROTO)
      bad (EPROTO);
#elif defined (EBADMSG)
      bad (EBADMSG);
#elif defined (EIO)
      bad (EIO);
#else
      bad ();
#endif
    }
  return !error ();
}

/* Finish a set of bools.  */

void
cpm_writer::bflush ()
{
  if (bit_pos)
    {
      raw (bit_val);
      lengths[2] += bit_flush ();
    }
  spans[2]++;
  is_set = -1;
}

void
cpm_reader::bflush ()
{
  if (bit_pos)
    bit_flush ();
}

/* When reading, we don't know how many bools we'll read in.  So read
   4 bytes-worth, and then rewind when flushing if we didn't need them
   all.  */

void
cpm_reader::bfill ()
{
  bit_val = raw ();
}

/* Low level cpm_readers and cpm_writers.  I did think about making these
   templatized, but that started to look error prone, so went with
   type-specific names.
   b - bools,
   i, u - ints/unsigned
   wi/wu - wide ints/unsigned
   s - size_t
   buf - fixed size buffer
   str - variable length string  */

/* Bools are packed into bytes.  You cannot mix bools and non-bools.
   You must call bflush before emitting another type.  So batch your
   bools.

   It may be worth optimizing for most bools being zero.  some kind of
   run-length encoding?  */

void
cpm_writer::b (bool x)
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
      raw (bit_val);
      lengths[2] += bit_flush ();
    }
}

bool
cpm_reader::b ()
{
  if (!bit_pos)
    bfill ();
  bool v = (bit_val >> bit_pos++) & 1;
  if (bit_pos == 32)
    bit_flush ();
  return v;
}

/* Exactly 4 bytes.  Used internally for bool packing and crc
   transfer -- hence no crc here.  */

void
cpm_writer::raw (unsigned val)
{
  reserve (4);
  buffer[pos++] = val;
  buffer[pos++] = val >> 8;
  buffer[pos++] = val >> 16;
  buffer[pos++] = val >> 24;
}

unsigned
cpm_reader::raw ()
{
  unsigned val = 0;
  if (fill (4) != 4)
    bad ();
  else
    {
      val |= (unsigned char)buffer[pos++];
      val |= (unsigned char)buffer[pos++] << 8;
      val |= (unsigned char)buffer[pos++] << 16;
      val |= (unsigned char)buffer[pos++] << 24;
    }

  return val;
}

/* Chars are unsigned and written as single bytes. */

void
cpm_writer::c (unsigned char v)
{
  reserve (1);
  buffer[pos++] = v;
  crc_unsigned (v);
}

int
cpm_reader::c ()
{
  int v = 0;
  if (fill (1))
    v = (unsigned char)buffer[pos++];
  else
    bad ();
  crc_unsigned (v);
  return v;
}

/* Ints are written as sleb128.  I suppose we could pack the first
   few bits into any partially-filled bool buffer.  */

void
cpm_writer::i (int v)
{
  crc_unsigned (v);
  reserve ((sizeof (v) * 8 + 6) / 7);

  int end = v < 0 ? -1 : 0;
  bool more;

  do
    {
      unsigned byte = v & 127;
      v >>= 6; /* Signed shift.  */
      more = v != end;
      buffer[pos++] = byte | (more << 7);
      v >>= 1; /* Signed shift.  */
    }
  while (more);
}

int
cpm_reader::i ()
{
  int v = 0;
  unsigned bit = 0;
  size_t bytes = fill ((sizeof (v) * 8 + 6) / 7);
  unsigned byte;

  do
    {
      if (!bytes--)
	{
	  bad ();
	  return v;
	}
      byte = buffer[pos++];
      v |= (byte & 127) << bit;
      bit += 7;
    }
  while (byte & 128);

  if (byte & 0x40 && bit < sizeof (v) * 8)
    v |= ~(unsigned)0 << bit;
  crc_unsigned (v);
  return v;
}

/* Unsigned are written as uleb128.  */

void
cpm_writer::u (unsigned v)
{
  crc_unsigned (v);
  reserve ((sizeof (v) * 8 + 6) / 7);

  bool more;
  do
    {
      unsigned byte = v & 127;
      v >>= 7;
      more = v != 0;
      buffer[pos++] = byte | (more << 7);
    }
  while (more);
}

unsigned
cpm_reader::u ()
{
  unsigned v = 0;
  unsigned bit = 0;
  size_t bytes = fill ((sizeof (v) * 8 + 6) / 7);
  unsigned byte;

  do
    {
      if (!bytes--)
	{
	  bad ();
	  return v;
	}
      byte = buffer[pos++];
      v |= (byte & 127) << bit;
      bit += 7;
    }
  while (byte & 128);
  crc_unsigned (v);

  return v;
}

void
cpm_writer::wi (HOST_WIDE_INT v)
{
  crc_unsigned (v);
  reserve ((sizeof (v) * 8 + 6) / 7);

  int end = v < 0 ? -1 : 0;
  bool more;

  do
    {
      unsigned byte = v & 127;
      v >>= 6; /* Signed shift.  */
      more = v != end;
      buffer[pos++] = byte | (more << 7);
      v >>= 1; /* Signed shift.  */
    }
  while (more);
}

HOST_WIDE_INT
cpm_reader::wi ()
{
  HOST_WIDE_INT v = 0;
  unsigned bit = 0;
  size_t bytes = fill ((sizeof (v) * 8 + 6) / 7);
  unsigned byte;

  do
    {
      if (!bytes--)
	{
	  bad ();
	  return v;
	}
      byte = buffer[pos++];
      v |= (byte & 127) << bit;
      bit += 7;
    }
  while (byte & 128);

  if (byte & 0x40 && bit < sizeof (v) * 8)
    v |= ~(unsigned HOST_WIDE_INT)0 << bit;
  crc_unsigned (v);
  return v;
}

inline void
cpm_writer::wu (unsigned HOST_WIDE_INT v)
{
  wi ((HOST_WIDE_INT) v);
}

inline unsigned HOST_WIDE_INT
cpm_reader::wu ()
{
  return (unsigned HOST_WIDE_INT) wi ();
}

inline void
cpm_writer::s (size_t s)
{
  if (sizeof (s) == sizeof (unsigned))
    u (s);
  else
    wu (s);
}

inline size_t
cpm_reader::s ()
{
  if (sizeof (size_t) == sizeof (unsigned))
    return u ();
  else
    return wu ();
}

void
cpm_writer::buf (const char *buf, size_t len)
{
  crc_buffer (buf, len);
  reserve (len);
  memcpy (buffer + pos, buf, len);
  pos += len;
}

const char *
cpm_reader::buf (size_t len)
{
  size_t have = fill (len);
  char *buf = &buffer[pos];
  if (have < len)
    {
      memset (buf + have, 0, len - have);
      bad ();
    }
  pos += have;
  crc_buffer (buf, len);
  return buf;
}

/* Strings:
   u:length
   checkpoint
   buf:bytes
*/

void
cpm_writer::str (const char *string, size_t len)
{
  s (len);
  checkpoint ();
  buf (string, len + 1);
}

const char *
cpm_reader::str (size_t *len_p)
{
  size_t len = s ();

  /* We're about to trust some user data.  */
  if (!checkpoint ())
    len = 0;
  *len_p = len;
  const char *str = buf (len + 1);
  if (str[len])
    {
      /* Force read string to be not totally broken.  */
      buffer[pos-1] = 0;
      bad ();
    }
  return str;
}

void
cpm_writer::module_name (tree name)
{
  size_t len;
  const char *ptr;
  if (identifier_p (name))
    {
      ptr = IDENTIFIER_POINTER (name);
      len = IDENTIFIER_LENGTH (name);
    }
  else
    {
      ptr = TREE_STRING_POINTER (name);
      len = TREE_STRING_LENGTH (name);
    }
  str (ptr, len);
}

tree
cpm_reader::module_name ()
{
  size_t l;
  const char *mod = str (&l);

  /* If it ends in NUL, it was a string name.  */
  bool ident_name = l ? mod[l-1] : flag_modules == 1;
  int code = validate_module_name (ident_name, mod, l);
  if (code > -2)
    {
      ::error ("module name %qs is malformed", mod);
      return NULL_TREE;
    }

  tree name;
  if (ident_name)
    name = get_identifier_with_length (mod, l);
  else
    {
      name = build_string (l, mod);
      TREE_TYPE (name) = char_array_type_node;
      name = fix_string_type (name);
    }
  return name;
}

void
cpm_writer::flush ()
{
  size_t bytes = fwrite (buffer, 1, pos, stream);

  if (bytes != pos)
    bad (errno);
  pos = 0;
}

void
cpm_writer::seek (unsigned loc)
{
  flush ();
  fseek (stream, long (loc), SEEK_SET);
}

size_t
cpm_writer::reserve (size_t want)
{
  size_t have = alloc - pos;
  if (have < want)
    {
      flush ();
      if (alloc < want)
	{
	  alloc = want + (want / 8); /* Some hysteresis.  */
	  buffer = XRESIZEVEC (char, buffer, alloc);
	}
      have = alloc;
    }
  return have;
}

size_t
cpm_reader::fill (size_t want)
{
  size_t have = len - pos;
  if (have < want)
    {
      memmove (buffer, buffer + pos, len - pos);
      len -= pos;
      pos = 0;
      if (alloc < want)
	{
	  alloc = want + (want / 8); /* Some hysteresis.  */
	  buffer = XRESIZEVEC (char, buffer, alloc);
	}
      if (size_t bytes = fread (buffer + len, 1, alloc - len, stream))
	{
	  len += bytes;
	  have = len;
	}
      else if (ferror (stream))
	{
	  clearerr (stream);
	  bad (errno);
	}
    }
  return have < want ? have : want;
}

/* Module cpm_stream base.  */
class cpm_stream {
public:
  /* Record tags.  */
  enum record_tag
  {
    /* Module-specific records.  */
    rt_eof,		/* End Of File.  duh! */
    rt_conf,		/* Config info (baked in stuff like target-triplet) */
    rt_stamp,		/* Date stamp etc.  */
    rt_flags,		/* Flags that affect AST compatibility.  */
    rt_import,		/* An import. */
    rt_binding,		/* A name-binding.  */
    rt_definition,	/* A definition. */
    rt_identifier,	/* An identifier node.  */
    rt_conv_identifier,	/* A conversion operator name.  */
    rt_trees,		/* Global trees.  */
    rt_type_name,	/* A type name.  */
    rt_typeinfo_var,	/* A typeinfo object.  */
    rt_typeinfo_pseudo, /* A typeinfo pseudo type.  */
    rt_tree_base = 0x100,	/* Tree codes.  */
    rt_ref_base = 0x1000	/* Back-reference indices.  */
  };
  struct gtp {
    const tree *ptr;
    unsigned num;
  };

public:
  static const gtp global_tree_arys[];

private:
  unsigned tag;

private:
  FILE *d;
  unsigned depth;
  unsigned nesting;
  bool bol;

public:
  cpm_stream (cpm_stream *chain = NULL);
  ~cpm_stream ();

protected:
  /* Allocate a new reference index.  */
  unsigned next ()
  {
    return tag++;
  }

public:
  static const char *ident ();
  static int version ();

  /* Version to date. */
  static unsigned v2d (int v)
  {
    if (MODULE_STAMP && v < 0)
      return -v / 10000 + 20000000;
    else
      return v;
  }

  /* Version to time. */
  static unsigned v2t (int v)
  {
    if (MODULE_STAMP && v < 0)
      return -v % 10000;
    else
      return 0;
  }
  typedef char tbuf[8];
  typedef char dbuf[16];
  void t2s (unsigned, tbuf &);
  void d2s (unsigned, dbuf &);

public:
  FILE *dumps () const
  {
    return d;
  }
  bool dump () const 
  {
    return d != NULL;
  }
  bool dump (const char *, ...);
  void nest ()
  {
    nesting++;
  }
  void unnest ()
  {
    nesting--;
  }
};

cpm_stream::cpm_stream (cpm_stream *chain)
  : tag (rt_ref_base), d (chain ? chain->d : NULL),
    depth (chain ? chain->depth + 1 : 0), nesting (0), bol (true)
{
  gcc_assert (MAX_TREE_CODES <= rt_ref_base - rt_tree_base);
  if (!chain)
    d = dump_begin (module_dump_id, NULL);
}

cpm_stream::~cpm_stream ()
{
  if (!depth && d)
    dump_end (module_dump_id, d);
}

void
cpm_stream::t2s (unsigned t, tbuf &buf)
{
  sprintf (buf, "%02u:%02u", t / 100, t % 100);
}

void
cpm_stream::d2s (unsigned d, dbuf &buf)
{
  sprintf (buf, "%04u/%02u/%02u", d / 10000, (d / 100) % 100, (d % 100));
}

static bool
dump_nested_name (tree t, FILE *d)
{
  if (t && TYPE_P (t))
    t = TYPE_NAME (t);

  if (t && DECL_P (t))
    {
      if (t == global_namespace)
	;
      else if (tree ctx = DECL_CONTEXT (t))
	if (TREE_CODE (ctx) == TRANSLATION_UNIT_DECL
	    || dump_nested_name (ctx, d))
	  fputs ("::", d);
      t = DECL_NAME (t);
    }

  if (t && TREE_CODE (t) == IDENTIFIER_NODE)
    {
      fwrite (IDENTIFIER_POINTER (t), 1, IDENTIFIER_LENGTH (t), d);
      return true;
    }

  if (t && TREE_CODE (t) == STRING_CST)
    {
      fwrite (TREE_STRING_POINTER (t), 1, TREE_STRING_LENGTH (t) - 1, d);
      return true;
    }

  return false;
}

/* Specialized printfy thing.  */

bool
cpm_stream::dump (const char *format, ...)
{
  va_list args;
  bool no_nl = format[0] == '+';
  format += no_nl;

  if (bol)
    {
      if (depth)
	{
	  /* Module import indenting.  */
	  const char *indent = ">>>>";
	  const char *dots   = ">...>";
	  if (depth > strlen (indent))
	    indent = dots;
	  else
	    indent += strlen (indent) - depth;
	  fputs (indent, d);
	}
      if (nesting)
	{
	  /* Tree indenting.  */
	  const char *indent = "      ";
	  const char *dots  =  "   ... ";
	  if (nesting > strlen (indent))
	    indent = dots;
	  else
	    indent += strlen (indent) - nesting;
	  fputs (indent, d);
	}
    }

  va_start (args, format);
  while (const char *esc = strchr (format, '%'))
    {
      fwrite (format, 1, (size_t)(esc - format), d);
      format = ++esc;
      switch (*format++)
	{
	case 'C': /* Code */
	  {
	    tree_code code = (tree_code)va_arg (args, unsigned);
	    fputs (get_tree_code_name (code), d);
	    break;
	  }
	case 'I': /* Identifier.  */
	  {
	    tree t = va_arg (args, tree);
	    dump_nested_name (t, d);
	    break;
	  }
	case 'N': /* Name.  */
	  {
	    tree t = va_arg (args, tree);
	    fputc ('\'', d);
	    dump_nested_name (t, d);
	    fputc ('\'', d);
	    break;
	  }
	case 'M': /* Mangled name */
	  {
	    tree t = va_arg (args, tree);
	    if (t && TYPE_P (t))
	      t = TYPE_NAME (t);
	    if (t && HAS_DECL_ASSEMBLER_NAME_P (t)
		&& DECL_ASSEMBLER_NAME_SET_P (t))
	      {
		fputc ('(', d);
		fputs (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t)), d);
		fputc (')', d);
	      }
	    break;
	  }
	case 'P': /* Pair.  */
	  {
	    tree ctx = va_arg (args, tree);
	    tree name = va_arg (args, tree);
	    fputc ('\'', d);
	    dump_nested_name (ctx, d);
	    if (ctx && ctx != global_namespace)
	      fputs ("::", d);
	    dump_nested_name (name, d);
	    fputc ('\'', d);
	    break;
	  }
	case 'R': /* Ratio */
	  {
	    unsigned a = va_arg (args, unsigned);
	    unsigned b = va_arg (args, unsigned);
	    fprintf (d, "%.1f", (float) a / (b + !b));
	    break;
	  }
	case 'U': /* long unsigned.  */
	  {
	    unsigned long u = va_arg (args, unsigned long);
	    fprintf (d, "%lu", u);
	    break;
	  }
	case 'V': /* Verson.  */
	  {
	    unsigned v = va_arg (args, unsigned);
	    tbuf time;
	    dbuf date;
	    t2s (v2t (v), time);
	    d2s (v2d (v), date);
	    fprintf (d, "%s-%s", date, time);
	    break;
	  }
	case 'p': /* Pointer. */
	  {
	    void *p = va_arg (args, void *);
	    fprintf (d, "%p", p);
	    break;
	  }
	case 's': /* String. */
	  {
	    const char *s = va_arg (args, char *);
	    fputs (s, d);
	    break;
	  }
	case 'u': /* Unsigned.  */
	  {
	    unsigned u = va_arg (args, unsigned);
	    fprintf (d, "%u", u);
	    break;
	  }
	case 'x': /* Hex. */
	  {
	    unsigned x = va_arg (args, unsigned);
	    fprintf (d, "%x", x);
	    break;
	  }
	default:
	  gcc_unreachable ();
	}
    }
  fputs (format, d);
  va_end (args);
  if (!no_nl)
    {
      bol = true;
      fputc ('\n', d);
    }
  return true;
}

const char *
cpm_stream::ident ()
{
  return "g++m";
}

int
cpm_stream::version ()
{
  /* If the on-disk format changes, update the version number.  */
  int version = 20170210;

#if defined (MODULE_STAMP)
  /* MODULE_STAMP is a decimal encoding YYYYMMDDhhmm or YYYYMMDD in
     local timezone.  Using __TIME__ doesnt work very well with
     boostrapping!  */
  version = -(MODULE_STAMP > 2000LL * 10000 * 10000
	      ? int (MODULE_STAMP - 2000LL * 10000 * 10000)
	      : int (MODULE_STAMP - 2000LL * 10000) * 10000);
#endif
  return version;
}

/* cpm_stream cpms_out.  */
class cpms_out : public cpm_stream {
  cpm_writer w;

  struct non_null : pointer_hash <void> {
    static bool is_deleted (value_type) {return false;}
    static void remove (value_type) {}
  };
  typedef simple_hashmap_traits<non_null, unsigned> traits;
  hash_map<void *,unsigned,traits> tree_map; /* trees to ids  */

  /* Tree instrumentation. */
  unsigned unique;
  unsigned refs;
  unsigned nulls;
  unsigned records;

public:
  cpms_out (FILE *, const char *, tree);
  ~cpms_out ();

  void instrument ();

public:
  void header (tree);
  void tag_eof ();
  void tag_conf ();
  void tag_import (unsigned ix, const module_state *);
  void tag_trees ();
  tree tag_binding (tree ns, bool, tree name, tree ovl);
  void maybe_tag_definition (tree decl);
  void tag_definition (tree node, tree maybe_template);
  int done ()
  {
    return w.done ();
  }

private:
  void tag (record_tag rt)
  {
    records++;
    w.u (rt);
  }
  unsigned insert (tree);
  void start (tree_code, tree);
  void loc (location_t);
  bool mark_present (tree);
  void globals (const tree *, unsigned);
  void core_bools (tree);
  void core_vals (tree);
  void lang_type_bools (tree);
  void lang_type_vals (tree);
  void lang_decl_bools (tree);
  void lang_decl_vals (tree);
  void tree_node_raw (tree_code, tree);
  void chained_decls (tree);
  void tree_vec (vec<tree, va_gc> *);
  void tree_pair_vec (vec<tree_pair_s, va_gc> *);
  void define_function (tree, tree);
  void define_class (tree, tree);
  void ident_imported_decl (tree ctx, unsigned mod, tree decl);

public:
  void tree_node (tree);
  void bindings (tree ns);
};

cpms_out::cpms_out (FILE *s, const char *n, tree name)
  :cpm_stream (), w (s, n)
{
  unique = refs = nulls = 0;
  records = 0;
  dump () && dump ("Writing module %I", name);
}

cpms_out::~cpms_out ()
{
}

void
cpm_writer::instrument (cpm_stream *d)
{
  d->dump ("Wrote %U bytes", ftell (stream));
  d->dump ("Wrote %u bits in %u bytes", lengths[0] + lengths[1],
	   lengths[2]);
  for (unsigned ix = 0; ix < 2; ix++)
    d->dump ("  %u %s spans of %R bits", spans[ix],
	     ix ? "one" : "zero", lengths[ix], spans[ix]);
  d->dump ("  %u blocks with %R bits padding", spans[2],
	   lengths[2] * 8 - (lengths[0] + lengths[1]), spans[2]);
  d->dump ("Wrote %u checksums", checksums);
}

void
cpms_out::instrument ()
{
  if (dump ())
    {
      dump ("");
      w.instrument (this);
      dump ("Wrote %u trees", unique + refs + nulls);
      dump ("  %u unique", unique);
      dump ("  %u references", refs);
      dump ("  %u nulls", nulls);
      dump ("Wrote %u records", records);
    }
}

/* Cpm_Stream in.  */
class cpms_in : public cpm_stream {
  cpm_reader r;

  /* Module state being initialized.  */
  module_state *state;

  typedef simple_hashmap_traits<int_hash<unsigned,0>,void *> traits;
  hash_map<unsigned,void *,traits> tree_map; /* ids to trees  */

  unsigned mod_ix; /* Module index.  */
  unsigned crc;    /* Expected crc.  */

  /* Remapping from incoming module indices to current TU. */
  unsigned remap_num;
  unsigned *remap_vec;

public:
  cpms_in (FILE *, const char *, module_state *, cpms_in *);
  ~cpms_in ();

public:
  bool header ();
  int tag_eof ();
  bool tag_conf ();
  bool tag_import ();
  bool tag_binding ();
  tree tag_definition ();
  bool tag_trees ();
  int read_item ();
  int done ()
  {
    return r.done ();
  }
  unsigned get_mod () const
  {
    return mod_ix;
  }

private:
  unsigned insert (tree);
  tree finish_type (tree);

private:
  bool alloc_remap_vec (unsigned limit);
  tree start (tree_code);
  tree finish (tree, int);
  location_t loc ();
  bool mark_present (tree);
  bool globals (const tree *, unsigned);
  bool core_bools (tree);
  bool core_vals (tree);
  bool lang_type_bools (tree);
  bool lang_type_vals (tree);
  bool lang_decl_bools (tree);
  bool lang_decl_vals (tree);
  bool tree_node_raw (tree_code, tree, tree, tree, int);
  tree chained_decls ();
  vec<tree, va_gc> *tree_vec ();
  vec<tree_pair_s, va_gc> *tree_pair_vec ();
  tree define_function (tree, tree);
  tree define_class (tree, tree);
  tree ident_imported_decl (tree ctx, unsigned mod, tree name);

public:
  tree tree_node ();
};

cpms_in::cpms_in (FILE *s, const char *n, module_state *state_, cpms_in *from)
  :cpm_stream (from), r (s, n), state (state_), mod_ix (GLOBAL_MODULE_INDEX),
   remap_num (0), remap_vec (NULL)
{
  dump () && dump ("Importing %I", state->name);
}

cpms_in::~cpms_in ()
{
  XDELETE (remap_vec);
}

unsigned
cpms_out::insert (tree t)
{
  unsigned tag = next ();
  bool existed = tree_map.put (t, tag);
  gcc_assert (!existed);
  return tag;
}

unsigned
cpms_in::insert (tree t)
{
  unsigned tag = next ();
  bool existed = tree_map.put (tag, t);
  gcc_assert (!existed);
  return tag;
}

static unsigned do_module_import (location_t, tree, import_kind,
				  unsigned crc, cpms_in * = NULL);

/* File header
   buf:ident
   raw:version
   raw:CRC -- not checksummed
   str:module
*/

void
cpms_out::header (tree name)
{
  char const *id = ident ();
  w.buf (id, strlen (id));

  int v = version ();
  gcc_assert (v < 0); /* Not ready for prime-time.  */
  w.raw (unsigned (v));

  w.raw (0);

  dump () && dump ("Writing \"%s\" version=%V", id, v);

  w.module_name (name);
}

// FIXME: What we really want to do at this point is read the header
// and then allow hooking out to rebuild it if there are
// checksum/version mismatches.  Other errors should barf.

bool
cpms_in::header ()
{
  const char *id = ident ();
  const char *i = r.buf (strlen (id));
  if (memcmp (id, i, strlen (id)))
    {
      error ("%qs is not a module file", r.name);
      return false;
    }

  /* Check version.  */
  int ver = version ();
  int v = int (r.raw ());
  if (v != ver)
    {
      int ver_date = v2d (ver);
      int v_date = v2d (v);
      dbuf v_dform, ver_dform;
      d2s (v_date, v_dform);
      d2s (ver_date, ver_dform);

      if (ver_date != v_date)
	{
	  /* Dates differ, decline.  */
	  error ("%qs built by version %s, this is version %s",
		 r.name, v_dform, ver_dform);
	  return false;
	}
      else
	{
	  /* Times differ, give it a go.  */
	  int ver_time = v2t (ver);
	  int v_time = v2t (v);
	  tbuf v_tform, ver_tform;
	  t2s (v_time, v_tform);
	  t2s (ver_time, ver_tform);
	  warning (0, "%qs is version %s, but build time was %s, not %s",
		   r.name, v_dform, v_tform, ver_tform);
	}
    }
  dump () && dump  ("Expecting %V found %V", ver, v);

  unsigned crc = r.raw ();
  dump () && dump ("Readinging CRC=%x", crc);
  if (state->crc && state->crc != crc)
    {
      error ("module %qE (%qs) CRC mismatch", state->name, r.name);
      return false;
    }
  else
    state->crc = crc;

  /* Check module name.  */
  tree name = r.module_name ();
  if (!name)
    return false;
  if (!module_state_hash::equal (state, name))
    {
      error ("%qs is module %qE, expected module %qE",
	     r.name, name, state->name);
      return false;
    }

  return true;
}

void
cpms_out::tag_eof ()
{
  tag (rt_eof);
  w.checkpoint ();

  w.seek (strlen (ident ()) + sizeof (unsigned));
  unsigned crc = w.get_crc ();
  w.raw (crc);
  dump () && dump ("Writing eof, CRC=%x", crc);
}

int
cpms_in::tag_eof ()
{
  dump () && dump ("Read eof");
  if (!r.checkpoint ())
    return false;

  /* Record the crc.  */
  unsigned crc = r.get_crc ();
  gcc_assert (!state->crc || state->crc == crc);
  state->crc = crc;

  return -1; /* Denote EOF.  */
}

/* Record config info
   str:<target-triplet>
   str:<host-triplet>  ; lock this for now.
*/

void
cpms_out::tag_conf ()
{
  dump () && dump ("Writing target='%s', host='%s'",
		   TARGET_MACHINE, HOST_MACHINE);
  tag (rt_conf);
  w.str (TARGET_MACHINE, strlen (TARGET_MACHINE));
  w.str (HOST_MACHINE, strlen (HOST_MACHINE));
  w.checkpoint ();
}

bool
cpms_in::tag_conf ()
{
  size_t l;
  const char *targ = r.str (&l);
  if (strcmp (targ, TARGET_MACHINE))
    {
      error ("%qs is target %qs, expected %qs", r.name, targ, TARGET_MACHINE);
      return false;
    }
  const char *host = r.str (&l);
  if (strcmp (host, HOST_MACHINE))
    {
      error ("%qs is host %qs, expected %qs", r.name, host, HOST_MACHINE);
      return false;
    }

  if (!r.checkpoint ())
    return false;

  dump () && dump ("Read target='%s', host='%s'",
		   TARGET_MACHINE, HOST_MACHINE);

  return true;
}

/* Dump the global trees directly to save encoding them for no reason.
   Further types such as sizetype and global_namespace are oddly
   recursive, and this avoids having to deal with that in the
   cpm_reader.

   <ary>*
   <u:0>
*/

const cpm_stream::gtp cpm_stream::global_tree_arys[] =
  {
    {sizetype_tab, stk_type_kind_last},
    {global_trees, TI_MAX},
    {cp_global_trees, CPTI_MAX},
    {NULL, 0}
  };

void
cpms_out::tag_trees ()
{
  tag (rt_trees);
  globals (&DECL_CONTEXT (global_namespace), 1);
  for (unsigned ix = 0; global_tree_arys[ix].ptr; ix++)
    globals (global_tree_arys[ix].ptr, global_tree_arys[ix].num);
  w.checkpoint ();
}

bool
cpms_in::tag_trees ()
{
  if (!globals (&DECL_CONTEXT (global_namespace), 1))
    return false;
  for (unsigned ix = 0; global_tree_arys[ix].ptr; ix++)
    if (!globals (global_tree_arys[ix].ptr, global_tree_arys[ix].num))
      return false;
  return r.checkpoint ();
}

bool
cpms_out::mark_present (tree t)
{
  bool existed = true;

  if (t)
    {
      unsigned *val = &tree_map.get_or_insert (t, &existed);
      if (!existed)
	*val = next ();
    }
  w.b (existed);

  return existed;
}

bool
cpms_in::mark_present (tree t)
{
  bool existed = r.b ();

  if (!existed)
    {
      unsigned tag = next ();

      gcc_assert (t);
      tree_map.put (tag, t);
    }
  return existed;
}

/* Global tree array
   u:count
   b[]:insert_p  */

void
cpms_out::globals (const tree *ary, unsigned num)
{
  w.u (num);

  dump () && dump ("+Writing %u globals", num);

  unsigned outer = 0, inner = 0;
  for (unsigned ix = 0; ix != num; ix++)
    {
      dump () && !(ix & 31) && dump ("") && dump ("+\t%u:", ix);

      tree t = ary[ix];
      unsigned first = !mark_present (t);
      if (first)
	{
	  outer++;
	  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPED)
	      && !mark_present (TREE_TYPE (t)))
	    {
	      first++;
	      inner++;
	    }
	}
      dump () && dump ("+%u", first);
    }
  w.bflush ();
  dump () && dump ("") && dump ("Wrote %u unique %u inner", outer, inner);

  w.checkpoint ();
}

bool
cpms_in::globals (const tree *ary, unsigned num)
{
  if (r.u () != num)
    return false;

  dump () && dump ("+Reading %u globals", num);

  unsigned outer = 0, inner = 0;
  for (unsigned ix = 0; ix != num; ix++)
    {
      dump () && !(ix & 31) && dump ("") && dump ("+\t%u:", ix);

      tree t = ary[ix];
      unsigned first = !mark_present (t);
      if (first)
	{
	  outer++;
	  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPED)
	      && !mark_present (TREE_TYPE (t)))
	    {
	      first++;
	      inner++;
	    }
	}
      dump () && dump ("+%u", first);
    }
  r.bflush ();
  dump () && dump ("") && dump ("Read %u unique %u inner", outer, inner);

  return r.checkpoint ();
}

/* Item import
   u:index
   u:direct
   u:crc
   wu:stamp
   str:module_name  */

// FIXME: Should we stream the pathname to the import?

void
cpms_out::tag_import (unsigned ix, const module_state *state)
{
  dump () && dump ("Writing %simport %I (crc=%x)",
		   state->direct_import == 2 ? "export " :
		   state->direct_import ? "" : "indirect ",
		   state->name, state->crc);
  tag (rt_import);
  w.u (ix);
  w.u (state->direct_import);
  w.u (state->crc);
  w.module_name (state->name);
  w.checkpoint ();
}

bool
cpms_in::alloc_remap_vec (unsigned limit)
{
  if (!remap_num && limit < MODULE_INDEX_LIMIT)
    {
      if (limit < THIS_MODULE_INDEX)
	limit = THIS_MODULE_INDEX;

      remap_num = limit + 1;
      remap_vec = XNEWVEC (unsigned, remap_num);
      memset (remap_vec, 0, sizeof (unsigned) * remap_num);
    }
  return limit < remap_num;
}

bool
cpms_in::tag_import ()
{
  unsigned ix = r.u ();
  unsigned direct = r.u ();
  unsigned crc = r.u ();
  tree imp = r.module_name ();

  if (!imp)
    return false;

  if (!r.checkpoint ())
    return false;

  /* Not designed to import after having assigned our number. */
  if (mod_ix)
    {
      error ("misordered import %qE", imp);
      return false;
    }
  if (!alloc_remap_vec (ix))
    {
      error ("import %u is out of range", ix);
      return false;
    }

  dump () && dump ("Begin nested %simport %I",
		   direct == 2 ? "export " : direct ? "" : "indirect ", imp);
  int imp_ix = do_module_import (UNKNOWN_LOCATION, imp,
				 direct ? ik_direct : ik_indirect,
				 crc, this);
  if (imp_ix != GLOBAL_MODULE_INDEX)
    {
      remap_vec[ix] = imp_ix;
      if (direct)
	{
	  bool is_export = direct == 2;
	  dump () && dump ("Direct %simport %I %u",
			   is_export ? "export " : "", imp, imp_ix);
	  state->do_import (imp_ix, direct == 2);
	}
    }

  dump () && dump ("Completed nested import %I #%u %s", imp,
		   imp_ix, imp_ix != GLOBAL_MODULE_INDEX ? "ok" : "failed");
  return imp_ix != GLOBAL_MODULE_INDEX;
}

/* NAME is bound to OVL in namespace NS.  Write out the binding.
   We also write out the definitions of things in the binding list.
   NS must have already have a binding somewhere.

   tree:ns
   tree:name
   b:main_p
   b:stat_p
   [tree:stat_type]
   tree:binding
*/

/* Return a child namespace to walk.  */

tree
cpms_out::tag_binding (tree ns, bool main_p, tree name, tree binding)
{
  tree type = NULL_TREE;
  tree value = ovl_skip_hidden (decapsulate_binding (binding, &type));

  // FIXME
  if (type && DECL_IS_BUILTIN (type))
    type = NULL_TREE;

  if (value)
    {
      if (TREE_CODE (value) == NAMESPACE_DECL && !DECL_NAMESPACE_ALIAS (value))
	{
	  gcc_assert (!type);
	  return value;
	}

      /* Don't write builtins (mostly).  */
      // FIXME: write the builtin
      while (TREE_CODE (value) == OVERLOAD
	     && DECL_IS_BUILTIN (OVL_FUNCTION (value)))
	value = OVL_CHAIN (value);

      if (TREE_CODE (value) != OVERLOAD)
	{
	  if (DECL_IS_BUILTIN (value))
	    value = NULL_TREE; // FIXME
	  else if (TREE_CODE (value) == CONST_DECL)
	    {
	      gcc_assert (TREE_CODE (CP_DECL_CONTEXT (value))
			  == ENUMERAL_TYPE);
	      value = NULL_TREE;
	    }
	  else if (TREE_CODE (value) == VAR_DECL && DECL_TINFO_P (value))
	    value = NULL_TREE;
	}
    }

  if (!value && !type)
    return NULL_TREE;

  dump () && dump ("Writing %N %s bindings for %N", ns,
		   main_p ? "main" : "global", name);

  tag (rt_binding);
  tree_node (ns);
  tree_node (name);
  w.b (main_p);
  w.b (type != NULL_TREE);
  w.bflush ();
  if (type)
    tree_node (type);
  tree_node (value);
  w.checkpoint ();

  if (type && !DECL_IS_BUILTIN (type))
    maybe_tag_definition (type);
  for (ovl_iterator iter (value); iter; ++iter)
    if (!DECL_IS_BUILTIN (*iter))
      maybe_tag_definition (*iter);

  return NULL_TREE;
}

bool
cpms_in::tag_binding ()
{
  tree ns = tree_node ();
  tree name = tree_node ();
  unsigned main_p = r.b ();
  unsigned stat_p = r.b ();
  r.bflush ();
  dump () && dump ("Reading %N %s binding for %N", ns,
		   main_p ? "main" : "global", name);

  tree type = stat_p ? tree_node () : NULL_TREE;
  tree value = tree_node ();

  if (!r.checkpoint ())
    return false;

  return push_module_binding (ns, name, main_p ? mod_ix : GLOBAL_MODULE_INDEX,
			      value, type);
}

/* Stream a function definition.  */

void
cpms_out::define_function (tree decl, tree)
{
  tree_node (DECL_RESULT (decl));
  tree_node (DECL_INITIAL (decl));
  tree_node (DECL_SAVED_TREE (decl));
}

tree
cpms_in::define_function (tree decl, tree maybe_template)
{
  tree result = tree_node ();
  tree initial = tree_node ();
  tree saved = tree_node ();

  if (r.error ())
    return NULL_TREE;

  if (TREE_CODE (CP_DECL_CONTEXT (maybe_template)) == NAMESPACE_DECL)
    {
      unsigned mod = MAYBE_DECL_MODULE_INDEX (maybe_template);
      if (mod == GLOBAL_MODULE_INDEX
	  && DECL_SAVED_TREE (decl))
	return decl; // FIXME check same
      else if (mod != mod_ix)
	{
	  error ("unexpected definition of %q#D", decl);
	  r.bad ();
	  return NULL_TREE;
	}
    }

  DECL_RESULT (decl) = result;
  DECL_INITIAL (decl) = initial;
  DECL_SAVED_TREE (decl) = saved;

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

/* A chained set of decls.  */

void
cpms_out::chained_decls (tree decls)
{
  for (; decls; decls = DECL_CHAIN (decls))
    tree_node (decls);
  tree_node (NULL_TREE);
}

tree
cpms_in::chained_decls ()
{
  tree decls = NULL_TREE;
  for (tree *chain = &decls; chain && !r.error ();)
    if (tree decl = tree_node ())
      {
	if (!DECL_P (decl))
	  r.bad ();
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
cpms_out::tree_vec (vec<tree, va_gc> *v)
{
  unsigned len = vec_safe_length (v);
  w.u (len);
  if (len)
    for (unsigned ix = 0; ix != len; ix++)
      tree_node ((*v)[ix]);
}

vec<tree, va_gc> *
cpms_in::tree_vec ()
{
  vec<tree, va_gc> *v = NULL;
  if (unsigned len = r.u ())
    {
      vec_alloc (v, len);
      for (unsigned ix = 0; ix != len; ix++)
	v->quick_push (tree_node ());
    }
  return v;
}

/* A vector of tree pairs.  */

void
cpms_out::tree_pair_vec (vec<tree_pair_s, va_gc> *v)
{
  unsigned len = vec_safe_length (v);
  w.u (len);
  if (len)
    for (unsigned ix = 0; ix != len; ix++)
      {
	tree_pair_s const &s = (*v)[ix];
	tree_node (s.purpose);
	tree_node (s.value);
      }
}

vec<tree_pair_s, va_gc> *
cpms_in::tree_pair_vec ()
{
  vec<tree_pair_s, va_gc> *v = NULL;
  if (unsigned len = r.u ())
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
cpms_out::define_class (tree type, tree maybe_template)
{
  chained_decls (TYPE_FIELDS (type));
  tree_node (TYPE_VFIELD (type));
  tree_node (TYPE_BINFO (type));
  if (TYPE_LANG_SPECIFIC (type))
    {
      tree_node (CLASSTYPE_PRIMARY_BINFO (type));
      tree_vec (CLASSTYPE_VBASECLASSES (type));

      tree_vec (CLASSTYPE_MEMBER_VEC (type));
      tree_node (CLASSTYPE_FRIEND_CLASSES (type));
      tree_node (CLASSTYPE_LAMBDA_EXPR (type));

      if (TYPE_CONTAINS_VPTR_P (type))
	{
	  tree_vec (CLASSTYPE_PURE_VIRTUALS (type));
	  tree_pair_vec (CLASSTYPE_VCALL_INDICES (type));
	  tree_node (CLASSTYPE_KEY_METHOD (type));
	}

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
    // FIXME:non-members and whatnot
    if (DECL_DECLARES_FUNCTION_P (member))
      maybe_tag_definition (member);
  tree_node (NULL_TREE);
}

/* Nop sorted needed for resorting the member vec.  */

static void
nop (void *, void *)
{
}

tree
cpms_in::define_class (tree type, tree maybe_template)
{
  gcc_assert (TYPE_MAIN_VARIANT (type) == type);

  tree fields = chained_decls ();
  tree vfield = tree_node ();
  tree binfo = tree_node ();
  vec<tree, va_gc> *member_vec = NULL;
  tree primary = NULL_TREE;
  vec<tree, va_gc> *vbases = NULL;
  vec<tree, va_gc> *pure_virts = NULL;
  vec<tree_pair_s, va_gc> *vcall_indices = NULL;
  tree key_method = NULL_TREE;
  tree vtables = NULL_TREE;
  tree lambda = NULL_TREE;
  tree friends = NULL_TREE;

  if (TYPE_LANG_SPECIFIC (type))
    {
      primary = tree_node ();
      vbases = tree_vec ();

      member_vec = tree_vec ();
      friends = tree_node ();
      lambda = tree_node ();

      /* TYPE_VBASECLASSES is not set yet, so TYPE_CONTAINS_VPTR will
	 malfunction.  */
      if (TYPE_POLYMORPHIC_P (type) || vbases)
	{
	  pure_virts = tree_vec ();
	  vcall_indices = tree_pair_vec ();
	  key_method = tree_node ();
	}
      vtables = chained_decls ();
    }

  tree decl_list = NULL_TREE;
  if (TREE_CODE (maybe_template) == TEMPLATE_DECL)
    decl_list = tree_node ();

  // lang->nested_udts

  // FIXME: Sanity check stuff

  if (r.error ())
    return NULL_TREE;

  TYPE_FIELDS (type) = fields;
  TYPE_VFIELD (type) = vfield;
  TYPE_BINFO (type) = binfo;

  if (TYPE_LANG_SPECIFIC (type))
    {
      CLASSTYPE_PRIMARY_BINFO (type) = primary;
      CLASSTYPE_VBASECLASSES (type) = vbases;

      CLASSTYPE_FRIEND_CLASSES (type) = friends;
      CLASSTYPE_LAMBDA_EXPR (type) = lambda;

      CLASSTYPE_MEMBER_VEC (type) = member_vec;
      CLASSTYPE_PURE_VIRTUALS (type) = pure_virts;
      CLASSTYPE_VCALL_INDICES (type) = vcall_indices;

      CLASSTYPE_KEY_METHOD (type) = key_method;
      if (!key_method && TYPE_CONTAINS_VPTR_P (type))
	vec_safe_push (keyed_classes, type);

      CLASSTYPE_VTABLES (type) = vtables;
      /* Read the vtable initializers.  */
      for (; vtables; vtables = TREE_CHAIN (vtables))
	DECL_INITIAL (vtables) = tree_node ();

      CLASSTYPE_DECL_LIST (type) = decl_list;

      /* Resort the member vector.  */
      resort_type_member_vec (member_vec, NULL, nop, NULL);
    }

  /* Propagate to all variants.  */
  fixup_type_variants (type);

  /* Now define all the members.  */
  while (tree_node ())
    if (r.error ())
      break;

  if (TYPE_LANG_SPECIFIC (type))
    {
      if (tree tdef
	  = get_class_binding_direct (type, as_base_identifier, true))
	CLASSTYPE_AS_BASE (type) = TREE_TYPE (tdef);
      else
	CLASSTYPE_AS_BASE (type) = type;
    }
  
  return type;
}

/* Write out DECL's definition, if importers need it.  */

void
cpms_out::maybe_tag_definition (tree t)
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
      switch (TREE_CODE (TREE_TYPE (t)))
	{
	default:
	  return;

	case RECORD_TYPE:
	case UNION_TYPE:
	  if (!maybe_template && !COMPLETE_TYPE_P (TREE_TYPE (t)))
	    return;
	  break;
	}
      break;

    case FUNCTION_DECL:
      if (!DECL_SAVED_TREE (t))
	return;
      if (DECL_TEMPLATE_INFO (t))
	{
	  if (DECL_USE_TEMPLATE (t) & 1)
	    return;
	}
      else if (!DECL_DECLARED_INLINE_P (t))
	return;
      if (DECL_CLONED_FUNCTION_P (t))
	return;
      break;
    }

  tag_definition (t, maybe_template);
}

/* Write out T's definition  */

void
cpms_out::tag_definition (tree t, tree maybe_template)
{
  dump () && dump ("Writing%s definition for %C:%N%M",
		   maybe_template ? " template" : "", TREE_CODE (t), t, t);

  if (!maybe_template)
    maybe_template = t;
  tag (rt_definition);
  tree_node (maybe_template);

  switch (TREE_CODE (t))
    {
    default:
      gcc_unreachable ();
    case FUNCTION_DECL:
      define_function (t, maybe_template);
      break;
    case TYPE_DECL:
      t = TREE_TYPE (t);
      switch (TREE_CODE (t))
	{
	default:
	  gcc_unreachable ();
	case RECORD_TYPE:
	case UNION_TYPE:
	  define_class (t, maybe_template);
	  break;
	}
      break;
    }
}

tree
cpms_in::tag_definition ()
{
  tree t = tree_node ();
  dump () && dump ("Reading definition for %C:%N%M", TREE_CODE (t), t, t);

  if (r.error ())
    return NULL_TREE;

  tree maybe_template = t;
  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

  switch (TREE_CODE (t))
    {
    default:
      // FIXME: read other things
      t = NULL_TREE;
      break;

    case FUNCTION_DECL:
      t = define_function (t, maybe_template);
      if (t && maybe_clone_body (t) && !DECL_DECLARED_CONSTEXPR_P (t))
	DECL_SAVED_TREE (t) = NULL_TREE;
      break;

    case TYPE_DECL:
      t = TREE_TYPE (t);
      switch (TREE_CODE (t))
	{
	default:
	  t = NULL_TREE;
	  break;

	case RECORD_TYPE:
	case UNION_TYPE:
	  t = define_class (t, maybe_template);
	}
      break;
    }

  return t;
}

int
cpms_in::read_item ()
{
  if (r.error ())
    return false;

  unsigned rt = r.u ();

  switch (rt)
    {
    case rt_conf:
      return tag_conf ();
    case rt_import:
      return tag_import ();

    default:
      break;
    }

  if (mod_ix == GLOBAL_MODULE_INDEX)
    {
      if (state == this_module)
	{
	  mod_ix = THIS_MODULE_INDEX;
	  (*modules)[mod_ix] = state;
	}
      else
	{
	  mod_ix = modules->length ();
	  if (mod_ix == MODULE_INDEX_LIMIT)
	    {
	      sorry ("too many modules loaded (limit is %u)", mod_ix);
	      r.bad ();
	      return -1;
	    }
	  vec_safe_push (modules, state);
	  state->set_index (mod_ix);
	}
      alloc_remap_vec (THIS_MODULE_INDEX);
      remap_vec[THIS_MODULE_INDEX] = mod_ix;

      dump () && dump ("Assigning %N module index %u", state->name, mod_ix);
    }

  switch (rt)
    {
    case rt_eof:
      return tag_eof ();
    case rt_binding:
      return tag_binding ();
    case rt_definition:
      return tag_definition () != NULL_TREE;
    case rt_trees:
      return tag_trees ();

    default:
      error (rt < rt_tree_base ? "unknown key %qd"
	     : rt < rt_ref_base ? "unexpected tree code %qd"
	     : "unexpected tree reference %qd",rt);
      r.bad ();
      return false;
    }
}

/* Read & write locations.  */

void
cpms_out::loc (location_t)
{
  // FIXME:Do something
}

location_t
cpms_in::loc ()
{
  // FIXME:Do something^-1
  return UNKNOWN_LOCATION;
}

/* Start tree write.  Write information to allocate the receiving
   node.  */

void
cpms_out::start (tree_code code, tree t)
{
  switch (code)
    {
    default:
      if (TREE_CODE_CLASS (code) == tcc_vl_exp)
	w.u (VL_EXP_OPERAND_LENGTH (t));
      break;
    case IDENTIFIER_NODE:
      gcc_unreachable ();
      break;
    case TREE_BINFO:
      w.u (BINFO_N_BASE_BINFOS (t));
      break;
    case TREE_VEC:
      w.u (TREE_VEC_LENGTH (t));
      break;
    case STRING_CST:
      w.str (TREE_STRING_POINTER (t), TREE_STRING_LENGTH (t));
      break;
    case VECTOR_CST:
      w.u (VECTOR_CST_NELTS (t));
      break;
    case INTEGER_CST:
      w.u (TREE_INT_CST_NUNITS (t));
      w.u (TREE_INT_CST_EXT_NUNITS (t));
      w.u (TREE_INT_CST_OFFSET_NUNITS (t));
      break;
    case OMP_CLAUSE:
      gcc_unreachable (); // FIXME:
    }
}

/* Start tree read.  Allocate the receiving node.  */

tree
cpms_in::start (tree_code code)
{
  tree t = NULL_TREE;
  
  switch (code)
    {
    default:
      if (TREE_CODE_CLASS (code) == tcc_vl_exp)
	{
	  unsigned ops = r.u ();
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
	const char *str = r.str (&l);
	t = build_string (l, str);
      }
      break;
    case TREE_BINFO:
      t = make_tree_binfo (r.u ());
      break;
    case TREE_VEC:
      t = make_tree_vec (r.u ());
      break;
    case VECTOR_CST:
      t = make_vector (r.u ());
      break;
    case INTEGER_CST:
      {
	unsigned n = r.u ();
	unsigned e = r.u ();
	t = make_int_cst (n, e);
	TREE_INT_CST_OFFSET_NUNITS(t) = r.u ();
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
cpms_in::finish (tree t, int node_module)
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

  if (DECL_P (t) && node_module == GLOBAL_MODULE_INDEX)
    {
      tree ctx = CP_DECL_CONTEXT (t);

      if (TREE_CODE (ctx) == NAMESPACE_DECL)
	{
	  /* A global-module decl.  See if there's already a duplicate.  */
	  tree old = merge_global_decl (ctx, t);

	  if (!old)
	    error ("failed to merge %#qD", t);
	  else
	    dump () && dump ("%s decl %N%M, (%p)",
			     old == t ? "New" : "Existing",
			     old, old, (void *)old);

	  return old;
	}
    }

  if (TREE_CODE (t) == INTEGER_CST)
    {
      // FIXME:Remap small ints?
    }

  return t;
}

/* The structure streamers access the raw fields, because the
   alternative, of using the accessor macros can require using
   different accessors for the same underlying field, depending on the
   tree code.  That's both confusing and annoying.  */

/* Read & write the core boolean flags.  */

void
cpms_out::core_bools (tree t)
{
#define WB(X) (w.b (X))
  tree_code code = TREE_CODE (t);

  WB (t->base.side_effects_flag);
  WB (t->base.constant_flag);
  WB (t->base.addressable_flag);
  WB (t->base.volatile_flag);
  WB (t->base.readonly_flag);
  WB (t->base.asm_written_flag);
  WB (t->base.nowarning_flag);
  // visited is zero
  WB (t->base.used_flag);
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
      WB (t->decl_common.decl_flag_1);
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
cpms_in::core_bools (tree t)
{
#define RB(X) ((X) = r.b ())
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
  return !r.error ();
}

void
cpms_out::lang_decl_bools (tree t)
{
#define WB(X) (w.b (X))
  const struct lang_decl *lang = DECL_LANG_SPECIFIC (t);

  WB (lang->u.base.language == lang_cplusplus);
  WB ((lang->u.base.use_template >> 0) & 1);
  WB ((lang->u.base.use_template >> 1) & 1);
  WB (lang->u.base.not_really_extern);
  WB (lang->u.base.initialized_in_class);
  WB (lang->u.base.repo_available_p);
  WB (lang->u.base.threadprivate_or_deleted_p);
  WB (lang->u.base.anticipated_p);
  WB (lang->u.base.friend_or_tls);
  WB (lang->u.base.odr_used);
  WB (lang->u.base.u2sel);
  WB (lang->u.base.concept_p);
  WB (lang->u.base.var_declared_inline_p);
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
cpms_in::lang_decl_bools (tree t)
{
#define RB(X) ((X) = r.b ())
  struct lang_decl *lang = DECL_LANG_SPECIFIC (t);

  lang->u.base.language = r.b () ? lang_cplusplus : lang_c;
  unsigned v;
  v = r.b () << 0;
  v |= r.b () << 1;
  lang->u.base.use_template = v;
  RB (lang->u.base.not_really_extern);
  RB (lang->u.base.initialized_in_class);
  RB (lang->u.base.repo_available_p);
  RB (lang->u.base.threadprivate_or_deleted_p);
  RB (lang->u.base.anticipated_p);
  RB (lang->u.base.friend_or_tls);
  RB (lang->u.base.odr_used);
  RB (lang->u.base.u2sel);
  RB (lang->u.base.concept_p);
  RB (lang->u.base.var_declared_inline_p);
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
  return !r.error ();
}

void
cpms_out::lang_type_bools (tree t)
{
#define WB(X) (w.b (X))
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
cpms_in::lang_type_bools (tree t)
{
#define RB(X) ((X) = r.b ())
  struct lang_type *lang = TYPE_LANG_SPECIFIC (t);

  RB (lang->has_type_conversion);
  RB (lang->has_copy_ctor);
  RB (lang->has_default_ctor);
  RB (lang->const_needs_init);
  RB (lang->ref_needs_init);
  RB (lang->has_const_copy_assign);
  unsigned v;
  v = r.b () << 0;
  v |= r.b () << 1;
  lang->use_template = v;

  RB (lang->has_mutable);
  RB (lang->com_interface);
  RB (lang->non_pod_class);
  RB (lang->nearly_empty_p);
  RB (lang->user_align);
  RB (lang->has_copy_assign);
  RB (lang->has_new);
  RB (lang->has_array_new);
  v = r.b () << 0;
  v |= r.b () << 1;
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
  return !r.error ();
}

/* Read & write the core values and pointers.  */

void
cpms_out::core_vals (tree t)
{
#define WU(X) (w.u (X))
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
	w.wu (TREE_INT_CST_ELT (t, ix));
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
      /* decl_minimal.name & decl_minimal.context already read in.  */
      loc (t->decl_minimal.locus);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      WU (t->decl_common.mode);
      WU (t->decl_common.off_align);
      WU (t->decl_common.align);

      WT (t->decl_common.size_unit);
      WT (t->decl_common.attributes);
      switch (code)
	{
	default:
	  break;
	case PARM_DECL:
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
    gcc_unreachable (); // FIXME

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
      if (!RECORD_OR_UNION_CODE_P (code))
	{
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
	   /* For some reason, some tcc_expression nodes do not claim
	      to contain TS_EXP.  */
	   || TREE_CODE_CLASS (code) == tcc_expression)
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
    {
      WT (t->binfo.offset);
      WT (t->binfo.vtable);
      WT (t->binfo.virtuals);
      WT (t->binfo.vptr_field);
      WT (t->binfo.inheritance);
      WT (t->binfo.vtt_subvtt);
      WT (t->binfo.vtt_vptr);
      gcc_assert (BINFO_N_BASE_BINFOS (t)
		  == vec_safe_length (BINFO_BASE_ACCESSES (t)));
      tree_vec (BINFO_BASE_ACCESSES (t));
      if (unsigned num = BINFO_N_BASE_BINFOS (t))
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
cpms_in::core_vals (tree t)
{
#define RU(X) ((X) = r.u ())
#define RUC(T,X) ((X) = T (r.u ()))
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
	TREE_INT_CST_ELT (t, ix) = r.wu ();
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
      /* decl_minimal.name & decl_minimal.context already read in.  */
      /* Don't zap the locus just yet, we don't record it correctly
	 and thus lose all location information.  */
      /* t->decl_minimal.locus = */
      loc ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      RUC (machine_mode, t->decl_common.mode);
      RU (t->decl_common.off_align);
      RU (t->decl_common.align);

      RT (t->decl_common.size_unit);
      RT (t->decl_common.attributes);
      switch (code)
	{
	default:
	  break;
	case PARM_DECL:
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
    gcc_unreachable (); // FIXME

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
      if (!RECORD_OR_UNION_CODE_P (code))
	{
	  RT (t->type_non_common.values);
	  /* POINTER and REFERENCE types hold NEXT_{PTR,REF}_TO.  We
	     store a marker there to indicate whether we're on the
	     referred to type's pointer/reference to list.  */
	  RT (t->type_non_common.minval);
	  if (POINTER_TYPE_P (t) && t->type_non_common.minval
	      && t->type_non_common.minval != t)
	    {
	      t->type_non_common.minval = NULL_TREE;
	      r.bad ();
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
	   || TREE_CODE_CLASS (code) == tcc_expression)
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
    {
      RT (t->binfo.offset);
      RT (t->binfo.vtable);
      RT (t->binfo.virtuals);
      RT (t->binfo.vptr_field);
      RT (t->binfo.inheritance);
      RT (t->binfo.vtt_subvtt);
      RT (t->binfo.vtt_vptr);
      BINFO_BASE_ACCESSES (t) = tree_vec ();
      if (BINFO_BASE_ACCESSES (t))
	{
	  unsigned num = BINFO_BASE_ACCESSES (t)->length ();
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
      if (unsigned len = r.u ())
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
  return !r.error ();
}

void
cpms_out::lang_decl_vals (tree t)
{
  const struct lang_decl *lang = DECL_LANG_SPECIFIC (t);
#define WU(X) (w.u (X))
#define WT(X) (tree_node (X))
  /* Module index already written.  */
  switch (lang->u.base.selector)
    {
    case lds_fn:  /* lang_decl_fn.  */
      WU (lang->u.fn.operator_code);
      if (lang->u.fn.thunk_p)
	w.wi (lang->u.fn.u5.fixed_offset);
      else
	WT (lang->u.fn.u5.cloned_function);
      /* FALLTHROUGH.  */
    case lds_min:  /* lang_decl_min.  */
      WT (lang->u.min.template_info);
      if (lang->u.base.u2sel)
	WU (lang->u.min.u2.discriminator);
      else
	WT (lang->u.min.u2.access);
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
cpms_in::lang_decl_vals (tree t)
{
  struct lang_decl *lang = DECL_LANG_SPECIFIC (t);
#define RU(X) ((X) = r.u ())
#define RT(X) ((X) = tree_node ())

  /* Module index already read.  */

  switch (lang->u.base.selector)
    {
    case lds_fn:  /* lang_decl_fn.  */
      {
	unsigned code = r.u ();
	/* It seems to be hard to check this is in range.  */
	lang->u.fn.operator_code = (tree_code)code;
	if (lang->u.fn.thunk_p)
	  lang->u.fn.u5.fixed_offset = r.wi ();
	else
	  RT (lang->u.fn.u5.cloned_function);
      }
      /* FALLTHROUGH.  */
    case lds_min:  /* lang_decl_min.  */
      RT (lang->u.min.template_info);
      if (lang->u.base.u2sel)
	RU (lang->u.min.u2.discriminator);
      else
	RT (lang->u.min.u2.access);
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
  return !r.error ();
}

/* Most of the value contents of lang_type is streamed in
   define_class.  */

void
cpms_out::lang_type_vals (tree t)
{
  const struct lang_type *lang = TYPE_LANG_SPECIFIC (t);
#define WU(X) (w.u (X))
#define WT(X) (tree_node (X))
  WU (lang->align);
  WT (lang->befriending_classes);
#undef WU
#undef WT
}

bool
cpms_in::lang_type_vals (tree t)
{
  struct lang_type *lang = TYPE_LANG_SPECIFIC (t);
#define RU(X) ((X) = r.u ())
#define RT(X) ((X) = tree_node ())
  RU (lang->align);
  RT (lang->befriending_classes);
#undef RU
#undef RT
  return !r.error ();
}

/* Refer to imported decls via reference information, not directly.  */

void
cpms_out::ident_imported_decl (tree ctx, unsigned mod, tree decl)
{
  if (TREE_CODE (ctx) == NAMESPACE_DECL)
    {
      unsigned key = get_ident_in_namespace (ctx, mod, DECL_NAME (decl), decl);
      w.u (key);
    }
  else if (TYPE_P (ctx))
    {
      // FIXME: use get_class_binding_direct
      /* Until class scopes look like namespace scopes, we'll have to
	 search the TYPE_FIELDS and TYPE_METHODS array.  Ew.  */
      int key = 0;
      tree probe = TYPE_FIELDS (ctx);

      for (; probe != decl; probe = TREE_CHAIN (probe))
	key++;
      w.s (key);
    }
  else
    gcc_unreachable ();
}

tree
cpms_in::ident_imported_decl (tree ctx, unsigned mod, tree name)
{
  tree res;

  if (TREE_CODE (ctx) == NAMESPACE_DECL)
    {
      unsigned key = r.u ();
      res = find_by_ident_in_namespace (ctx, mod, name, key);
    }
  else if (TYPE_P (ctx))
    {
      // FIXME: See above
      /* Until class scopes look like namespace scopes, we'll have to
	 search the TYPE_FIELDS and TYPE_METHODS array.  Ew.  */
      int key = r.s ();
      tree probe = TYPE_FIELDS (ctx);

      for (; key; probe = TREE_CHAIN (probe))
	{
	  if (!probe)
	    break;
	  key--;
	}
      res = probe;
    }
  else
    gcc_unreachable ();

  return res;
}

/* The raw tree node.  We've already dealt with the code, and in the
   case of decls, determining name, context & module.  Stream the
   bools and vals without post-processing.  Caller is responsible for
   checkpointing.  */

void
cpms_out::tree_node_raw (tree_code code, tree t)
{
  tree_code_class klass = TREE_CODE_CLASS (code);
  bool specific = false;

  if (klass == tcc_type || klass == tcc_declaration)
    {
      if (klass == tcc_declaration)
	specific = DECL_LANG_SPECIFIC (t) != NULL;
      else if (TYPE_MAIN_VARIANT (t) == t)
	specific = TYPE_LANG_SPECIFIC (t) != NULL;
      else
	gcc_assert (TYPE_LANG_SPECIFIC (t)
		    == TYPE_LANG_SPECIFIC (TYPE_MAIN_VARIANT (t)));
      w.b (specific);
      if (specific && code == VAR_DECL)
	w.b (DECL_DECOMPOSITION_P (t));
    }

  core_bools (t);
  if (specific)
    {
      if (klass == tcc_type)
	lang_type_bools (t);
      else
	lang_decl_bools (t);
    }
  w.bflush ();
  w.checkpoint ();

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
cpms_in::tree_node_raw (tree_code code, tree t, tree name, tree ctx,
			int node_module)
{
  tree_code_class klass = TREE_CODE_CLASS (code);
  bool specific = false;
  bool lied = false;

  if (klass == tcc_type || klass == tcc_declaration)
    {
      specific = r.b ();
      if (specific
	  &&  (klass == tcc_type
	       ? !maybe_add_lang_type_raw (t)
	       : !maybe_add_lang_decl_raw (t, code == VAR_DECL && r.b ())))
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
  r.bflush ();
  if (lied || !r.checkpoint ())
    return false;

  if (klass == tcc_declaration)
    {
      DECL_CONTEXT (t) = ctx;
      DECL_NAME (t) = name;
    }

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
	  if (node_module >= 0)
	    DECL_MODULE_INDEX (t) = node_module;
	  if (!lang_decl_vals (t))
	    return false;
	}
    }
  else if (klass == tcc_type)
    TYPE_LANG_SPECIFIC (t) = TYPE_LANG_SPECIFIC (TYPE_MAIN_VARIANT (t));

  return true;
}

/* Write either the decl (as a declaration) itself (and create a
   mapping for it), or write the existing mapping or write null.  This
   is essentially the lisp self-referential structure pretty-printer,
   except that we implicitly number every node, so need neither two
   passes, nor explicit labelling.
*/

void
cpms_out::tree_node (tree t)
{
  if (!t)
    {
      nulls++;
      w.u (0); /* This also matches t_eof, but we cannot be confused. */
      return;
    }

  nest ();
  if (unsigned *val = tree_map.get (t))
    {
      refs++;
      w.u (*val);
      dump () && dump ("Wrote:%u referenced %C:%N%M", *val,
		       TREE_CODE (t), t, t);
      unnest ();
      return;
    }

  if (TREE_CODE_CLASS (TREE_CODE (t)) == tcc_type && TYPE_NAME (t))
    {
      tree name = TYPE_NAME (t);

      gcc_assert (TREE_CODE (name) == TYPE_DECL);
      if (DECL_TINFO_P (name))
	{
	  unsigned ix = get_pseudo_tinfo_index (t);

	  /* Make sure we're identifying this exact variant.  */
	  gcc_assert (get_pseudo_tinfo_type (ix) == t);
	  w.u (rt_typeinfo_pseudo);
	  w.u (ix);
	  unsigned tag = insert (t);
	  dump () && dump ("Wrote:%u typeinfo pseudo %u %N", tag, ix, t);
	  unnest ();
	  return;
	}
      else if (!tree_map.get (name))
	{
	  /* T is a named type whose name we have not met yet.  Write the
	     type name as an interstitial, and then start over.  */
	  dump () && dump ("Writing interstitial type name %C:%N%M",
			   TREE_CODE (name), name, name);
	  w.u (rt_type_name);
	  tree_node (name);
	  dump () && dump ("Wrote interstitial type name %C:%N%M",
			   TREE_CODE (name), name, name);
	  unnest ();
	  /* The type could be a variant of TREE_TYPE (name).  */
	  tree_node (t);
	  return;
	}
    }

  if (TREE_CODE (t) == VAR_DECL && DECL_TINFO_P (t))
    {
      /* T is a typeinfo object.  These need recreating by the loader.
	 The type it is for is stashed on the name's TREE_TYPE.  */
      tree type = TREE_TYPE (DECL_NAME (t));
      dump () && dump ("Writing typeinfo %M for %N", t, type);
      w.u (rt_typeinfo_var);
      tree_node (type);
      unsigned tag = insert (t);
      dump () && dump ("Wrote:%u typeinfo %M for %N", tag, t, type);
      unnest ();
      return;
    }

  if (TREE_CODE (t) == IDENTIFIER_NODE)
    {
      /* An identifier node.  Stream the name or type.  */
      bool conv_op = IDENTIFIER_CONV_OP_P (t);

      w.u (conv_op ? rt_conv_identifier : rt_identifier);
      if (conv_op)
	{
	  t = TREE_TYPE (t);
	  tree_node (t);
	}
      else
	w.str (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));
      unsigned tag = insert (t);
      dump () && dump ("Written:%u %sidentifier:%N",
		       tag, conv_op ? "conv_op_" : "", t);
      unnest ();
      return;
    }

  /* Generic node streaming.  */    
  tree_code code = TREE_CODE (t);
  tree_code_class klass = TREE_CODE_CLASS (code);
  gcc_assert (rt_tree_base + code < rt_ref_base);

  unique++;
  w.u (rt_tree_base + code);

  bool body = true;
  if (klass == tcc_declaration)
    {
      /* Write out ctx, name & maybe import reference info.  */
      tree_node (DECL_CONTEXT (t));
      tree_node (DECL_NAME (t));

      tree module_ctx = module_context (t);
      unsigned node_module = MAYBE_DECL_MODULE_INDEX (module_ctx);
      w.u ((node_module << 1) | (module_ctx == t));
      if (node_module >= IMPORTED_MODULE_BASE)
	{
	  ident_imported_decl (CP_DECL_CONTEXT (t), node_module, t);
	  dump () && dump ("Writing imported %N@%I", t,
			   module_name (node_module));
	  body = false;
	}
    }

  if (body)
    start (code, t);

  unsigned tag = insert (t);
  dump () && dump ("Writing:%u %C:%N%M%s", tag, TREE_CODE (t), t, t,
		   klass == tcc_declaration && DECL_MODULE_EXPORT_P (t)
		   ? " (exported)": "");

  if (body)
    tree_node_raw (code, t);
  else if (TREE_TYPE (t))
    {
      tree type = TREE_TYPE (t);
      bool existed;
      unsigned *val = &tree_map.get_or_insert (type, &existed);
      if (!existed)
	{
	  tag = next ();
	  *val = tag;
	  dump () && dump ("Writing:%u %C:%N%M imported type", tag,
			   TREE_CODE (type), type, type);
	}
      w.u (existed);
    }

  w.checkpoint ();

  unnest ();
}

/* Read in a tree using TAG.  TAG is either a back reference, or a
   TREE_CODE for a new TREE.  For any tree that is a DECL, this does
   not read in a definition (initial value, class defn, function body,
   instantiations, whatever).  Return true on success.  Sets *TP to
   error_mark_node if TAG is totally bogus.  */

tree
cpms_in::tree_node ()
{
  unsigned tag = r.u ();

  if (!tag)
    return NULL_TREE;

  nest ();
  if (tag >= rt_ref_base)
    {
      tree *val = (tree *)tree_map.get (tag);
      if (!val || !*val)
	{
	  error ("unknown tree reference %qd", tag);
	  r.bad ();
	  return NULL_TREE;
	}

      tree res = *val;
      dump () && dump ("Read:%u found %C:%N%M", tag,
		       TREE_CODE (res), res, res);
      unnest ();
      return res;
    }

  if (tag == rt_type_name)
    {
      /* An interstitial type name.  Read the name and then start
	 over.  */
      tree name = tree_node ();
      if (!name || TREE_CODE (name) != TYPE_DECL)
	r.bad ();
      else
	dump () && dump ("Read interstitial type name %C:%N%M",
			 TREE_CODE (name), name, name);
      unnest ();
      return tree_node ();
    }
  else if (tag == rt_typeinfo_var)
    {
      /* A typeinfo.  Get the type and recreate the var decl.  */
      tree var = NULL_TREE, type = tree_node ();
      if (!type || !TYPE_P (type))
	r.bad ();
      else
	{
	  var = get_tinfo_decl (type);
	  unsigned tag = insert (var);
	  dump () && dump ("Created:%u typeinfo var %M for %N",
			   tag, var, type);
	}
      unnest ();
      return var;
    }
  else if (tag == rt_typeinfo_pseudo)
    {
      /* A pseuto typeinfo.  Get the index and recreate the pseudo.  */
      unsigned ix = r.u ();
      tree type = NULL_TREE;

      if (ix >= 1000)
	r.bad ();
      else
	type = get_pseudo_tinfo_type (ix);

      unsigned tag = insert (type);
      dump () && dump ("Created:%u typeinfo pseudo %u %N", tag, ix, type);
      unnest ();
      return type;
    }
  else if (tag == rt_definition)
    {
      /* An immediate definition.  */
      tree res = tag_definition ();
      if (res)
	dump () && dump ("Read immediate definition %C:%N%M",
			 TREE_CODE (res), res, res);
      unnest ();
      return res;
    }
  else if (tag == rt_identifier)
    {
      size_t l;
      const char *str = r.str (&l);
      tree id = get_identifier_with_length (str, l);
      tag = insert (id);
      dump () && dump ("Read:%u identifier:%N", tag, id);
      unnest ();
      return id;
    }
  else if (tag == rt_conv_identifier)
    {
      tree t = tree_node ();
      tree id = make_conv_op_name (t);
      tag = insert (id);
      dump () && dump ("Read:%u conv_op_identifier:%N", tag, t);
      unnest ();
      return id;
    }
  else if (tag < rt_tree_base || tag >= rt_tree_base + MAX_TREE_CODES)
    {
      error (tag < rt_tree_base ? "unexpected key %qd"
	     : "unknown tree code %qd" , tag);
      r.bad ();
      unnest ();
      return NULL_TREE;
    }

  tree_code code = tree_code (tag - rt_tree_base);
  tree_code_class klass = TREE_CODE_CLASS (code);
  tree t = NULL_TREE;

  bool body = true;
  tree name = NULL_TREE;
  tree ctx = NULL_TREE;
  int node_module = -1;
  int set_module = -1;

  if (klass == tcc_declaration)
    {
      ctx = tree_node ();
      name = tree_node ();
      if (!r.error ())
	{
	  unsigned incoming = r.u ();
	  bool self_node = incoming & 1;
	  incoming >>= 1;

	  if (incoming < remap_num)
	    node_module = remap_vec[incoming];
	  if (self_node)
	    set_module = node_module;
	  if (incoming >= IMPORTED_MODULE_BASE
	      && (node_module < 0
		  || node_module == GLOBAL_MODULE_INDEX
		  || node_module == THIS_MODULE_INDEX
		  || node_module == int (mod_ix)))
	    r.bad ();
	}

      if (r.error ())
	{
	  unnest ();
	  return NULL_TREE;
	}

      gcc_assert (node_module != THIS_MODULE_INDEX
		  || mod_ix == THIS_MODULE_INDEX);
      
      if (node_module != GLOBAL_MODULE_INDEX && node_module != int (mod_ix))
	{
	  tree cp_ctx = (TREE_CODE (ctx) == TRANSLATION_UNIT_DECL
			 ? global_namespace : ctx);

	  t = ident_imported_decl (cp_ctx, node_module, name);
	  if (!t || TREE_CODE (t) != code)
	    {
	      if (cp_ctx != global_namespace)
		error ("failed to find %<%E::%E@%E%>",
		       cp_ctx, name, module_name (node_module));
	      else
		error ("failed to find %<%E@%E%>",
		       name, module_name (node_module));
	      r.bad ();
	      t = NULL_TREE;
	    }
	  dump () && dump ("Importing %P@%I",
			   cp_ctx, name, module_name (node_module));
	  body = false;
	}
    }

  if (body)
    t = start (code);

  /* Insert into map.  */
  tag = insert (t);
  dump () && dump ("%s:%u %C:%N", body ? "Reading" : "Imported", tag,
		   code, name);

  if (body)
    {
      if (!tree_node_raw (code, t, name, ctx, set_module))
	goto barf;
    }
  else if (TREE_TYPE (t) && !r.u ())
    {
      tree type = TREE_TYPE (t);
      tag = insert (type);
      dump () && dump ("Read:%u %C:%N%M imported type", tag,
		       TREE_CODE (type), type, type);
    }

  if (!r.checkpoint ())
    {
    barf:
      tree_map.put (tag, NULL_TREE);
      r.bad ();
      unnest ();
      return NULL_TREE;
    }

  if (body)
    {
      tree found = finish (t, node_module);

      if (found != t)
	{
	  /* Update the mapping.  */
	  t = found;
	  tree_map.put (tag, t);
	  dump () && dump ("Index %u remapping %C:%N%M", tag,
			   t ? TREE_CODE (t) : ERROR_MARK, t, t);
	}
    }

  unnest ();
  return t;
}

/* Walk the bindings of NS, writing out the bindings for the global
   module and the main module.  */

void
cpms_out::bindings (tree ns)
{
  if (ns == mangle_namespace)
    {
      dump () && dump ("Skipping namespace %N", ns);
      return;
    }
  dump () && dump ("Walking namespace %N", ns);

  hash_table<named_decl_hash>::iterator end
    (DECL_NAMESPACE_BINDINGS (ns)->end ());
  for (hash_table<named_decl_hash>::iterator iter
	 (DECL_NAMESPACE_BINDINGS (ns)->begin ()); iter != end; ++iter)
    {
      tree global = *iter;
      tree inner = NULL_TREE;

      if (TREE_CODE (global) == MODULE_VECTOR)
	{
	  const module_cluster *cluster = &MODULE_VECTOR_CLUSTER (global, 0);
	  tree name = MODULE_VECTOR_NAME (global);
	  global = cluster->slots[GLOBAL_MODULE_INDEX];

	  if (tree main = cluster->slots[THIS_MODULE_INDEX])
	    inner = tag_binding (ns, true, name, main);
	}

      if (global)
	if (tree ginner = tag_binding (ns, false, OVL_NAME (global), global))
	  {
	    gcc_assert (!inner || inner == ginner);
	    inner = ginner;
	  }

      if (inner)
	bindings (inner);
    }

  dump () && dump ("Walked namespace %N", ns);
}

/* Mangling for module files.  */
#define MOD_FNAME_SFX ".nms" /* New Module System.  Honest.  */
#define MOD_FNAME_DOT '-'

static location_t module_loc;	 /* Location of the module decl.  */
static GTY(()) tree proclaimer;
static int export_depth; /* -1 for singleton export.  */

/* Rebuild a streamed in type.  */
// FIXME: c++-specific types are not in the canonical type hash.
// Perhaps that should be changed?

tree
cpms_in::finish_type (tree type)
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
      if (RECORD_OR_UNION_CODE_P (TREE_CODE (type)))
	{
	  /* The main variant might already have been defined, copy
	     the bits of its definition that we need.  */
	  TYPE_BINFO (type) = TYPE_BINFO (main);
	  TYPE_VFIELD (type) = TYPE_VFIELD (main);
	  TYPE_FIELDS (type) = TYPE_FIELDS (main);
	}

      /* CANONICAL_TYPE is either already correctly remapped.  Or
         correctly already us.  FIXME:Are we sure about this?  */
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

  return type;
}

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

/* Unnest a module export level.  */

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

/* Set the module EXPORT and INDEX fields on DECL.  */

void
decl_set_module (tree decl)
{
  if (export_depth)
    DECL_MODULE_EXPORT_P (decl) = true;

  if (this_module && this_module->name)
    {
      retrofit_lang_decl (decl);
      DECL_MODULE_INDEX (decl) = THIS_MODULE_INDEX;
    }
}

/* Return true iff we're in the purview of a named module.  */

bool
module_purview_p ()
{
  return this_module && this_module->name;
}

/* Return true iff we're the interface TU (this also means we're in a
   module perview.  */

bool
module_interface_p ()
{
  return this_module && this_module->direct_import;
}

/* Read a module NAME file name FNAME on STREAM.  Returns its module
   index, or 0 */

static unsigned
read_module (FILE *stream, const char *fname, module_state *state,
	     cpms_in *from = NULL)
{
  cpms_in in (stream, fname, state, from);

  if (!quiet_flag)
    {
      
      fprintf (stderr, " importing:%s(%s)",
	       identifier_p (state->name) ? IDENTIFIER_POINTER (state->name)
	       : TREE_STRING_POINTER (state->name), fname);
      fflush (stderr);
      pp_needs_newline (global_dc->printer) = true;
      diagnostic_set_last_function (global_dc, (diagnostic_info *) NULL);
    }

  int ok = in.header ();
  if (ok)
    do
      ok = in.read_item ();
    while (ok > 0);

  if (int e = in.done ())
    {
      /* strerror and friends returns capitalized strings.  */
      char const *err = e >= 0 ? xstrerror (e) : "Bad file data";
      char c = TOLOWER (err[0]);
      error ("%c%s", c, err + 1);
      ok = false;
    }

  if (ok)
    {
      ok = in.get_mod ();
      gcc_assert (ok >= THIS_MODULE_INDEX);
    }
  else
    /* Failure to read a module is going to cause big problems, so
       bail out now.  */
    fatal_error (input_location,
		 "failed to read module %qE (%qs)", state->name, fname);

  return ok;
}

static int
validate_module_name (bool from_ident, const char *ptr, size_t len)
{
  char sep = from_ident ? '.' : DIR_SEPARATOR;
  len += from_ident;

  for (size_t frag = 0; len--; ptr++)
    if (!len || *ptr == sep)
      {
	if (!frag)
	  return -1;
	frag = 0;
      }
    else if (!ISGRAPH (*ptr))
      return (unsigned char)*ptr;
    else if (from_ident
	     && !(ISALPHA (*ptr) || *ptr == '_' || (frag && ISDIGIT (*ptr))))
      return (unsigned char)*ptr;
    else
      frag++;
  return -2;
}

/* Validate that the name is ok.  */

tree
validate_module_name (const cp_expr &name)
{
  tree id = *name;

  if (!id)
    ;
  else if (identifier_p (id))
    ;
  else if (TYPE_PRECISION (TREE_TYPE (TREE_TYPE (id)))
	   != TYPE_PRECISION (char_type_node))
    {
      error_at (name.get_location (),
		"module name is not a simple string literal");
      id = NULL_TREE;
    }
  else if (TREE_STRING_LENGTH (id) > FILENAME_MAX)
    {
      error_at (name.get_location (), "module name is too long");
      id = NULL_TREE;
    }
  else
    {
      int code = validate_module_name (false, TREE_STRING_POINTER (id),
				       TREE_STRING_LENGTH (id));
      if (code >= 0)
	{
	  error_at (name.get_location (),
		      "module name contains %qc> character", code);
	  id = NULL_TREE;
	}
      else if (code == -1)
	{
	  error_at (name.get_location (),
		    "module name contains empty component");
	  id = NULL_TREE;
	}
    }

  return id;
}

/* Convert a module name into a file name.  The name is malloced.
 */

static char *
module_to_filename (tree id, size_t &len)
{
  size_t id_len;
  const char *id_chars;

  if (identifier_p (id))
    {
      id_len = IDENTIFIER_LENGTH (id);
      id_chars = IDENTIFIER_POINTER (id);
    }
  else
    {
      id_len = TREE_STRING_LENGTH (id) - 1;
      id_chars = TREE_STRING_POINTER (id);
    }

  size_t sfx_len = strlen (MOD_FNAME_SFX);
  len = id_len + sfx_len;

  char *buffer = XNEWVEC (char, id_len + sfx_len + 1);
  memcpy (buffer, id_chars, id_len);
  memcpy (buffer + id_len, MOD_FNAME_SFX, sfx_len + 1);

  if (identifier_p (id))
    {
      char dot = MOD_FNAME_DOT;
      if (dot != '.')
	for (char *ptr = buffer; id_len--; ptr++)
	  if (*ptr == '.')
	    *ptr = dot;
    }

  return buffer;
}

/* Search the module path for a binary module file called NAME.
   Updates NAME with path found.  */

static FILE *
search_module_path (char *&name, size_t name_len, tree mname)
{
  char *buffer = XNEWVEC (char, module_path_max + name_len + 2);
  bool once = false;

 again:
  if (!IS_ABSOLUTE_PATH (name))
    for (const cpp_dir *dir = module_path; dir; dir = dir->next)
      {
	memcpy (buffer, dir->name, dir->len);
	buffer[dir->len] = DIR_SEPARATOR;
	memcpy (buffer + dir->len + 1, name, name_len + 1);

	if (FILE *stream = fopen (buffer, "rb"))
	  {
	    XDELETE (name);
	    name = buffer;
	    return stream;
	  }
      }
  else if (FILE *stream = fopen (buffer, "rb"))
    return stream;

  if (once)
    return NULL;

  once = true;

  /* wrapper <module-name> <module-bmi-file> <source-file> <this-file>
     We may want to pass the multilib directory fragment too.  */

  unsigned len = 0;
  const char *argv[6];
  argv[len++] = flag_module_wrapper;
  argv[len++] = (identifier_p (mname) ? IDENTIFIER_POINTER (mname)
		 : TREE_STRING_POINTER (mname));
  argv[len++] = name;
  argv[len++] = main_input_filename;
  argv[len++] = expand_location (input_location).file;

  if (!quiet_flag)
    {
      if (pp_needs_newline (global_dc->printer))
	{
	  pp_needs_newline (global_dc->printer) = false;
	  fprintf (stderr, "\n");
	}
      fprintf (stderr, "%s module wrapper:", progname);
      for (unsigned ix = 0; ix != len; ix++)
	fprintf (stderr, "%s%s", &" "[!ix], argv[ix]);
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
      error_at (UNKNOWN_LOCATION, "%s %qs %m", errmsg, argv[0]);
    }
  else if (WIFSIGNALED (status))
    error_at (UNKNOWN_LOCATION, "module hook %qs died by signal %s",
	      argv[0], strsignal (WTERMSIG (status)));
  else if (WIFEXITED (status) && WEXITSTATUS (status) != 0)
    error_at (UNKNOWN_LOCATION, "module hook %qs exit status %d",
	      argv[0], WEXITSTATUS (status));

  goto again;
}

static FILE *
make_module_file (char *&name, size_t name_len)
{
  size_t root_len = 0;
  if (flag_module_root)
    {
      root_len = strlen (flag_module_root);

      char *buffer = XNEWVEC (char, root_len + name_len + 2);
      memcpy (buffer, flag_module_root, root_len);
      buffer[root_len] = DIR_SEPARATOR;
      memcpy (buffer + root_len + 1, name, name_len + 1);

      XDELETE (name);
      name = buffer;
    }

  FILE *stream = fopen (name, "wb");
  if (!stream && root_len && errno == ENOENT)
    {
      /* Try and create the missing directories.  */
      char *base = name + root_len + 1;
      char *end = base + name_len;

      for (;; base++)
	{
	  /* There can only be one dir separator!  */
	  base = (char *)memchr (base, DIR_SEPARATOR, end - base);
	  if (!base)
	    break;

	  *base = 0;
	  int failed = mkdir (name, S_IRWXU | S_IRWXG | S_IRWXO);
	  *base = DIR_SEPARATOR;
	  if (failed
	      /* Maybe racing with another creator (of a *different*
		 submodule).  */
	      && errno != EEXIST)
	    return NULL;
	}

      /* Have another go.  */
      stream = fopen (name, "wb");
    }

  return stream;
}

/* Import the module NAME into the current TU.  This includes the
   main module's interface and as implementation.  */

unsigned
do_module_import (location_t loc, tree name, import_kind kind,
		  unsigned crc, cpms_in *from)
{
  if (!module_hash)
    {
      module_hash = hash_table<module_state_hash>::create_ggc (31);
      vec_safe_reserve (modules, IMPORTED_MODULE_BASE);
      this_module = new (ggc_alloc <module_state> ()) module_state ();
      for (unsigned ix = IMPORTED_MODULE_BASE; ix--;)
	modules->quick_push (NULL);
      /* Insert map as unless/until we declare a module, we're the
	 global modle.  */
      if (kind <= ik_interface)
	(*modules)[GLOBAL_MODULE_INDEX] = this_module;
    }

  hashval_t hash;
  if (identifier_p (name))
    hash = IDENTIFIER_HASH_VALUE (name);
  else
    hash = hashval_t (crc32_string (TREE_STRING_LENGTH (name),
				    TREE_STRING_POINTER (name)));
  module_state **slot = module_hash->find_slot_with_hash (name, hash, INSERT);
  unsigned index = GLOBAL_MODULE_INDEX;
  module_state *state = *slot;

  if (state)
    switch (state->mod)
      {
      case ~0U:
	error_at (loc, "circular dependency of module %qE", name);
	return GLOBAL_MODULE_INDEX;
      case GLOBAL_MODULE_INDEX:
	error_at (loc, "already failed to read module %qE", name);
	return GLOBAL_MODULE_INDEX;
      case THIS_MODULE_INDEX:
	error_at (loc, "already declared as module %qE", name);
	return GLOBAL_MODULE_INDEX;
      default:
	if (kind >= ik_interface)
	  {
	    error_at (loc, "module %qE already imported", name);
	    return GLOBAL_MODULE_INDEX;
	  }
	index = state->mod;
      }
  else if (kind == ik_indirect)
    {
      /* The ordering of the import table implies that indirect
	 imports should have already been loaded.  */
      error ("indirect import %qE not present", name);
      index = GLOBAL_MODULE_INDEX;
    }
  else
    {
      if (kind >= ik_interface)
	{
	  state = this_module;
	  state->set_index (THIS_MODULE_INDEX);
	  (*modules)[THIS_MODULE_INDEX] = state;
	}
      else
	state = new (ggc_alloc<module_state> ()) module_state ();

      state->set_name (name, hash, crc);

      *slot = state;

      if (kind == ik_interface)
	index = THIS_MODULE_INDEX;
      else
	{
          /* First look in the module file map. If not found, fall back to the
             default mapping. */
          char *fname = NULL;
	  size_t fname_len;

	  if (!identifier_p (name))
	    ;
	  else if (char **slot = module_files.get (IDENTIFIER_POINTER (name)))
	    {
	      fname_len = strlen (*slot);
	      fname = XNEWVEC (char, fname_len + 1);
	      memcpy (fname, *slot, fname_len + 1);
	    }

          if (!fname)
            fname = module_to_filename (name, fname_len);

	  if (FILE *stream = search_module_path (fname, fname_len, name))
	    {
	      gcc_assert (global_namespace == current_scope ());
	      index = read_module (stream, fname, state, from);
	      fclose (stream);
	      gcc_assert (state->mod == ~0U);
	    }
	  else
	    error_at (loc, "cannot find module %qE (%qs): %m", name, fname);

          XDELETE (fname);
	}
    }

  if (index != GLOBAL_MODULE_INDEX && crc && crc != state->crc)
    {
      error ("module %qE crc mismatch", name);
      index = GLOBAL_MODULE_INDEX;
    }

  state->mod = index;

  return index;
}

/* Import the module NAME into the current TU and maybe re-export it.  */

void
import_module (const cp_expr &name, tree)
{
  gcc_assert (global_namespace == current_scope ());
  unsigned index = do_module_import (name.get_location (),
				     *name, ik_direct, 0, 0);
  if (index != GLOBAL_MODULE_INDEX)
    this_module->do_import (index, export_depth != 0);
  gcc_assert (global_namespace == current_scope ());
}

/* Declare the name of the current module to be NAME. ATTRS is used to
   determine if this is the interface or not.  */

void
declare_module (const cp_expr &name, bool inter, tree)
{
  if (this_module && this_module->name)
    {
      error_at (name.get_location (), "module %qE already declared", *name);
      inform (module_loc, "existing declaration");
      return;
    }

  gcc_assert (global_namespace == current_scope ());

  module_loc = name.get_location ();

  module_state *frozen = NULL;
  if (modules)
    {
      /* If we did any importing already, freeze it.  */
      gcc_assert ((*modules)[GLOBAL_MODULE_INDEX] == this_module);
      frozen = new (ggc_alloc <module_state> ()) module_state (*this_module);
      frozen->freeze (this_module);
    }

  unsigned index = do_module_import
    (name.get_location (), *name,
     inter ? ik_interface : ik_implementation, 0, 0);
  if (index != GLOBAL_MODULE_INDEX)
    {
      gcc_assert (index == THIS_MODULE_INDEX);

      (*modules)[GLOBAL_MODULE_INDEX] = frozen;
      current_module = index;
      this_module->direct_import = inter;
    }
}

static void
write_module (FILE *stream, const char *fname, tree name)
{
  cpms_out out (stream, fname, name);

  out.header (name);
  out.tag_conf ();
  // FIXME:Write 'important' flags etc

  /* Write the direct imports.  Write in reverse order, so that when
     checking an indirect import we should have already read it.  */
  for (unsigned ix = modules->length (); --ix > THIS_MODULE_INDEX;)
    if (module_state *state = (*modules)[ix])
      out.tag_import (ix, state);

  out.tag_trees ();

  /* Write decls.  */
  out.bindings (global_namespace);

  out.tag_eof ();
  if (int e = out.done ())
    error ("failed to write module %qE (%qs): %s", name, fname,
	   e >= 0 ? xstrerror (errno) : "Bad file data");
  out.instrument ();
}

/* Convert the module search path.  */

void
init_module_processing ()
{
  module_path = get_added_cpp_dirs (INC_CXX_MPATH);
  for (const cpp_dir *path = module_path; path; path = path->next)
    if (path->len > module_path_max)
      module_path_max = path->len;

  if (!flag_module_wrapper)
    {
      flag_module_wrapper = getenv ("CXX_MODULE_WRAPPER");
      if (!flag_module_wrapper)
	flag_module_wrapper = "false";
    }
}

/* Finalize the module at end of parsing.  */

void
finish_module ()
{
  if (this_module && this_module->direct_import)
    {
      char *fname;
      size_t fname_len;

      if (module_output)
	{
	  fname_len = strlen (module_output);
	  fname = XNEWVEC (char, fname_len + 1);
	  memcpy (fname, module_output, fname_len + 1);
	}
      else
	fname = module_to_filename (this_module->name, fname_len);

      if (errorcount)
	;
      else if (FILE *stream = make_module_file (fname, fname_len))
	{
	  write_module (stream, fname, this_module->name);
	  fclose (stream);
	}
      else
	error_at (module_loc, "cannot open module interface %qE (%qs): %m",
		  this_module->name, fname);

      if (errorcount)
	unlink (fname);

      XDELETE (fname);
    }
  else if (module_output)
    error ("-fmodule-output specified for non-module interface compilation");

  /* GC can clean up the detritus.  */
  module_hash = NULL;
  this_module = NULL;
}

#include "gt-cp-module.h"
