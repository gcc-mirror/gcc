/* -*- C++ -*- modules.  Experimental!
   Copyright (C) 2017 Free Software Foundation, Inc.
   Written by Nathan Sidwell <nathan@acm.org>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "dumpfile.h"

#define EXPERIMENTAL 1 /* Shtop! This module is not ready yet! */

namespace {
  
/* Byte serializer base.  */
class seriator
{
  static const size_t ALLOC = 32768;

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
  unsigned char bits;
  unsigned bit_pos;

public:
  seriator (FILE *, const char *);
  ~seriator ();

public:
  int error ()
  {
    return err;
  }
public:
  /* Set an error.  We store the first errno.  */
  void bad (int e = -1)
  {
    if (!err)
      err = e;
  }
};

seriator::seriator (FILE *s, const char *n)
  :stream (s), name (n), pos (0), len (0), alloc (ALLOC),
   err (0), bits (0), bit_pos (0)
{
  buffer = (char *) xmalloc (alloc);
}

seriator::~seriator ()
{
  gcc_assert (pos == len || err);
  free (buffer);
}

/* Byte stream writer.  */
class writer : public seriator
{
public:
  writer (FILE *s, const char *n)
    : seriator (s, n)
  {
  }
  ~writer ()
  {
  }

private:
  void flush_bits ();
  size_t reserve (size_t);
  void flush ();

public:
  int done ()
  {
    flush ();
    return error ();
  }

public:
  void b (bool);
  void c (unsigned char);
  void i (int);
  void u (unsigned);
  void s (size_t s);
  void wi (HOST_WIDE_INT);
  void wu (unsigned HOST_WIDE_INT);
  void str (const char *, size_t);
  void buf (const char *, size_t);
};

/* Byte stream reader.  */
class reader : public seriator
{
public:
  reader (FILE *s, const char *n)
    : seriator (s, n)
  {
  }
  ~reader ()
  {
  }

private:
  void flush_bits ()
  {
    bit_pos = 0;
  }
  size_t reserve (size_t);
  void flush ();

public:
  bool peek_u (unsigned u);
  int done ()
  {
    if (reserve (1))
      bad ();
    return error ();
  }
  bool b ();
  int c ();
  int i ();
  unsigned u ();
  size_t s ();
  HOST_WIDE_INT wi ();
  unsigned HOST_WIDE_INT wu ();
  const char *str (size_t * = NULL);
  const char *buf (size_t);
};

/* Low level readers and writers.  I did think about making these
   templatized, but that started to look error prone, so went with
   type-specific names.
   b - bools,
   i, u - ints/unsigned
   wi/wu - wide ints/unsigned
   s - size_t
   buf - fixed size buffer
   str - variable length string  */

/* Bools are packed into bytes.  These are automatically flushed when
   full, or we change to a different type.  */

void writer::b (bool x)
{
  bits |= unsigned (x) << bit_pos++;
  if (bit_pos == 8)
    flush_bits ();
}

bool reader::b ()
{
  if (!bit_pos)
    bits = c ();
  bool v = (bits >> bit_pos) & 1;
  bit_pos = (bit_pos + 1) & 7;
  return v;
}

/* Chars are unsigned and written as single bytes.  */

void writer::c (unsigned char x)
{
  reserve (1);
  buffer[pos++] = x;
}

int reader::c ()
{
  if (reserve (1))
    return (unsigned char)buffer[pos++];
  bad ();
  return 0;
}

/* Ints are written as sleb128.  I suppose we could pack the first
   few bits into any partially-filled bool buffer.  */

void writer::i (int x)
{
  reserve ((sizeof (x) * 8 + 6) / 7);

  int end = x < 0 ? -1 : 0;
  bool more;

  do
    {
      unsigned byte = x & 127;
      x >>= 6; /* Signed shift.  */
      more = x != end;
      buffer[pos++] = byte | (more << 7);
      x >>= 1; /* Signed shift.  */
    }
  while (more);
}

int reader::i ()
{
  int v = 0;
  unsigned bit = 0;
  size_t bytes = reserve ((sizeof (v) * 8 + 6) / 7);
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
    v |= ~(int)0 << bit;
  return v;
}

/* Unsigned are written as uleb128 too.  */

inline void writer::u (unsigned x)
{
  reserve ((sizeof (x) * 8 + 6) / 7);

  bool more;
  do
    {
      unsigned byte = x & 127;
      x >>= 7;
      more = x != 0;
      buffer[pos++] = byte | (more << 7);
    }
  while (more);
}

inline unsigned reader::u ()
{
  unsigned v = 0;
  unsigned bit = 0;
  size_t bytes = reserve ((sizeof (v) * 8 + 6) / 7);
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

  return v;
}

/* Peek at the next char and return true, if it matches U.  */
inline bool reader::peek_u (unsigned u)
{
  gcc_assert (u < 128);

  if (reserve (1))
    return ((unsigned char)buffer[pos]) == u;
  return false;
}

void writer::wi (HOST_WIDE_INT x)
{
  reserve ((sizeof (x) * 8 + 6) / 7);

  int end = x < 0 ? -1 : 0;
  bool more;

  do
    {
      unsigned byte = x & 127;
      x >>= 6; /* Signed shift.  */
      more = x != end;
      buffer[pos++] = byte | (more << 7);
      x >>= 1; /* Signed shift.  */
    }
  while (more);
}

HOST_WIDE_INT reader::wi ()
{
  HOST_WIDE_INT v = 0;
  unsigned bit = 0;
  size_t bytes = reserve ((sizeof (v) * 8 + 6) / 7);
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
    v |= ~(HOST_WIDE_INT)0 << bit;
  return v;
}

inline void writer::wu (unsigned HOST_WIDE_INT x)
{
  wi ((HOST_WIDE_INT) x);
}

inline unsigned HOST_WIDE_INT reader::wu ()
{
  return (unsigned HOST_WIDE_INT) wi ();
}

inline void writer::s (size_t s)
{
  if (sizeof (s) == sizeof (unsigned))
    u (s);
  else
    wu (s);
}

inline size_t reader::s ()
{
  if (sizeof (size_t) == sizeof (unsigned))
    return u ();
  else
    return wu ();
}

void writer::buf (const char *buf, size_t len)
{
  reserve (len);
  memcpy (buffer + pos, buf, len);
  pos += len;
}

const char *reader::buf (size_t len)
{
  size_t have = reserve (len);
  char *v = buffer + pos;
  if (have < len)
    {
      memset (v + have, 0, len - have);
      bad ();
    }
  pos += have;
  return v;
}
  
void writer::str (const char *string, size_t len)
{
  s (len);
  buf (string, len + 1);
}

const char *reader::str (size_t *len_p)
{
  size_t len = s ();
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
writer::flush ()
{
  flush_bits ();
  size_t bytes = fwrite (buffer, 1, pos, stream);
  
  if (bytes != pos && !err)
    err = errno;
  pos = 0;
}

void
reader::flush ()
{
  flush_bits ();
  memmove (buffer, buffer + pos, len - pos);
  len -= pos;
  pos = 0;
}

void
writer::flush_bits ()
{
  if (bit_pos)
    {
      int v = bits;

      bit_pos = 0;
      bits = 0;
      c (v);
    }
}

size_t
writer::reserve (size_t want)
{
  flush_bits ();
  size_t have = alloc - pos;
  if (have < want)
    {
      flush ();
      if (alloc < want)
	{
	  alloc = want;
	  buffer = (char *) xrealloc (buffer, alloc);
	}
      have = alloc;
    }
  return have;
}

size_t
reader::reserve (size_t want)
{
  flush_bits ();
  size_t have = len - pos;
  if (have < want)
    {
      flush ();
      if (alloc < want)
	{
	  alloc = want;
	  buffer = (char *) xrealloc (buffer, alloc);
	}
      size_t bytes = fread (buffer + len, 1, alloc - len, stream);
      len += bytes;
      have = len;
    }
  return have > want ? want : have;
}

/* Module streamer base.  */
class streamer
{
public:
  /* Record tags.  */
  enum tags
  {
    /* Module-specific records.  */
    t_eof,
    t_conf,
    t_flags,
    t_import,
    /* Tree codes.  */
    t_tree_base = 0x100,
    /* First reference index.  */
    t_ref_base = 0x1000
  };

private:
  unsigned index;

public:
  streamer () : index (t_ref_base)
  {
    gcc_assert (MAX_TREE_CODES <= t_ref_base - t_tree_base);
  }

protected:
  /* Allocate a new reference index.  */
  unsigned next ()
  {
    return index++;
  }

public:
  static const char *ident ();
  static int version ();

  /* Version to date. */
  static unsigned v2d (int v)
  {
    if (EXPERIMENTAL && v < 0)
      return -v / 10000 + 20000000;
    else
      return v;
  }

  /* Version to time. */
  static unsigned v2t (int v)
  {
    if (EXPERIMENTAL && v < 0)
      return -v % 10000;
    else
      return 0;
  }
};

const char *streamer::ident ()
{
  return "g++m";
}

int streamer::version ()
{
  /* If the on-disk format changes, update the version number.  */
  int version = 20170210;

  if (EXPERIMENTAL)
    {
      /* Force the version to be very volatile.  */
      /* __DATE__ "mon dd yyyy" */
      int year = ((__DATE__[7] - '0') * 1000
		  + (__DATE__[8] - '0') * 100
		  + (__DATE__[9] - '0') * 10
		  + (__DATE__[10] - '0') * 1);
      /* JanFebMarAprMayJunJulAugSepOctNovDec */
      int mon = (__DATE__[0] == 'J' // Jan Jun Jul
		 ? (__DATE__[1] == 'a' ? 1 // Jan
		    : __DATE__[2] == 'n' ? 6 // Jun
		    : __DATE__[2] == 'l' ? 7 // Jul
		    : 0) // oops
		 : __DATE__[0] == 'F' ? 2 // Feb
		 : __DATE__[0] == 'M' // Mar May
		 ? (__DATE__[2] == 'r' ? 3 // Mar
		    : __DATE__[2] == 'y' ? 5 // May
		: 0) // oops
		 : __DATE__[0] == 'A' // Apr Aug
		 ? (__DATE__[1] == 'p' ? 4 // Apr
		    : __DATE__[1] == 'u' ? 8 // Aug
		    : 0) // oops
		 : __DATE__[0] == 'S' ? 9 // Sep
		 : __DATE__[0] == 'O' ? 10 // Oct
		 : __DATE__[0] == 'N' ? 11 // Nov
		 : __DATE__[0] == 'D' ? 12 // Dec
		 : 0); // oops
      int day = ((__DATE__[4] == ' ' ? 0 : (__DATE__[4] - '0') * 10)
		 + (__DATE__[5] - '0') * 1);
      
      /* __TIME__ "hh:mm:ss" */
      int hour = ((__TIME__[0] - '0') * 10
		  + (__TIME__[1] - '0') * 1);
      int min =  ((__TIME__[3] - '0') * 10
		  + (__TIME__[4] - '0') * 1);
      int date = (((year % 100) * 100) + mon) * 100 + day;
      int time = (hour * 100) + min;
      
      version = -((date * 10000) + time);
    }
  return version;
}

/* streamer out.  */
class out : public streamer
{
  writer w;
  hash_map<tree,unsigned> map; /* trees to ids  */
  
public:
  out (FILE *, const char *);
  ~out ();

public:
  void header (FILE *, tree);
  void tag_eof ();
  void tag_conf (FILE *);
  void tag_import (FILE *, tree, bool);
  int done ()
  {
    return w.done ();
  }

public:
  void write_tree (FILE *, tree);
  void walk_namespace (FILE *, bool, tree);
};

out::out (FILE *s, const char *n)
  :w (s, n)
{
}

out::~out ()
{
}

/* Streamer in.  */
class in : public streamer
{
  reader r;
  typedef unbounded_int_hashmap_traits<unsigned,tree> traits;
  hash_map<unsigned,tree,traits> map; /* ids to trees  */
  bool impl;

public:
  in (FILE *, const char *, bool);
  ~in ();

public:
  bool header (FILE *, tree);
  int tag_eof (FILE *);
  bool tag_conf (FILE *);
  int tag_import (FILE *, tree &);
  int read_one (FILE *, tree &);
  int done ()
  {
    return r.done ();
  }

public:
  bool read_tree (FILE *, tree *, unsigned = 0);
};

in::in (FILE *s, const char *n, bool is_impl)
  :r (s, n), impl (is_impl)
{
}

in::~in ()
{
}

void
out::header (FILE *d, tree name)
{
  char const *id = ident ();
  w.buf (id, strlen (id));

  int v = version ();
  gcc_assert (v < 0); /* Not ready for prime-time.  */
  if (d)
    fprintf (d, "Writing \"%s\" %d:%04d\n", id, v2d (v), v2t (v));
  w.i (v);
  w.str (IDENTIFIER_POINTER (name), IDENTIFIER_LENGTH (name));
}

bool
in::header (FILE *d, tree name)
{
  const char *id = ident ();
  const char *i = r.buf (strlen (id));
  if (memcmp (id, i, strlen (id)))
    {
      error ("%qs is not a module file", r.name);
      return false;
    }

  int ver = version ();
  int v = r.i ();
  int ver_date = v2d (ver);
  int ver_time = v2t (ver);
  int v_date = v2d (v);
  int v_time = v2t (v);
  if (v != ver)
    {
      bool have_a_go = false;
      if (ver_date != v_date)
	/* Dates differ, decline.  */
	error ("%qs is version %d, require version %d",
	       r.name, v_date, ver_date);
      else
	{
	  /* Times differ, give it a go.  */
	  warning (0, "%qs is version %d, but timestamp is %d, not %d",
		   r.name, v_date, v_time, ver_time);
	  have_a_go = true;
	}
      if (!have_a_go)
	{
	  r.bad (-1);
	  return false;
	}
    }
  if (d)
    fprintf (d, "Expecting %d:%04d found %d:%04d\n", ver_date, ver_time,
	     v_date, v_time);

  size_t l;
  const char *n = r.str (&l);
  if (l != IDENTIFIER_LENGTH (name)
      || memcmp (n, IDENTIFIER_POINTER (name), l))
    {
      error ("%qs is module %qs, expected module %qE", r.name, n, name);
      return false;
    }

  return true;
}

void
out::tag_eof ()
{
  w.c (t_eof);
}

int
in::tag_eof (FILE *d)
{
  if (d)
    fprintf (d, "Read eof\n");
  return -1; /* Denote EOF.  */
}

/* Record config info
   str:<target-triplet>
   str:<host-triplet>  ; lock this for now.
*/

void
out::tag_conf (FILE *d)
{
  if (d)
    fprintf (d, "Writing target='%s', host='%s'\n",
	     TARGET_MACHINE, HOST_MACHINE);
  w.c (t_conf);
  w.str (TARGET_MACHINE, strlen (TARGET_MACHINE));
  w.str (HOST_MACHINE, strlen (HOST_MACHINE));
}

bool
in::tag_conf (FILE *d)
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

  if (d)
    fprintf (d, "Read target='%s', host='%s'\n", TARGET_MACHINE, HOST_MACHINE);

  return true;
}

/* Record import
   b:is_export
   str:module_name  */

void
out::tag_import (FILE *d, tree name, bool is_export)
{
  if (d)
    fprintf (d, "Writing %s '%s'\n", is_export ? "export module" : "import",
	     IDENTIFIER_POINTER (name));
  w.c (t_import);
  w.b (is_export);
  w.str (IDENTIFIER_POINTER (name), IDENTIFIER_LENGTH (name));
}

int
in::tag_import (FILE *d, tree &imp)
{
  bool is_exp = r.b ();
  size_t l;
  const char *mod = r.str (&l);

  /* Validate name.  Dotted sequence of identifiers.  */
  size_t dot = 0;
  for (size_t ix = 0; ix != l; ix++)
    if (ISALPHA (mod[ix]) || mod[ix] == '_')
      continue;
    else if (dot == ix)
      goto bad;
    else if (mod[ix] == '.')
      dot = ix + 1;
    else if (!ISDIGIT (mod[ix]))
      goto bad;
  if (!l || dot == l)
    goto bad;

  imp = get_identifier_with_length (mod, l);
  if (d)
    fprintf (d, "Read import '%s'\n", mod);
  return (is_exp ? 1 : 0) | 0x10;
 bad:
  error ("module name '%qs' is malformed", mod);
  return false;
}

int
in::read_one (FILE *d, tree &imp)
{
  unsigned tag = r.u ();
  if (tag == t_eof)
    return tag_eof (d);
  else if (tag == t_conf)
    return tag_conf (d);
  else if (tag == t_import)
    return tag_import (d, imp);

  tree t;
  if (!read_tree (d, &t, tag))
    {
      if (t == error_mark_node)
	error ("unknown key %qd", tag);
      r.bad ();
      return false;
    }
  // FIXME: read body
  return true;
}

/* Write either the decl (as a declaration) itself (and create a
   mapping for it), or write the existing mapping.  This is
   essentially the lisp self-referential structure pretty-printer,
   except that we implicitly number every node, so need neither two
   passes, nor explicit labelling.   */

void
out::write_tree (FILE *d, tree t)
{
  if (!t)
    {
      w.u (0); /* This also matches t_eof, but we cannot be confused. */
      return;
    }
  
  bool existed;
  unsigned *val = &map.get_or_insert (t, &existed);
  if (existed)
    {
      w.u (*val);
      return;
    }

  *val = next ();
  tree_code code = TREE_CODE (t);
  if (d)
    fprintf (d, "Writing:%u %s (%s:%d)\n", *val, get_tree_code_name (code),
	     tree_code_class_strings [TREE_CODE_CLASS (code)], code);

  gcc_assert (t_tree_base + code < t_ref_base);
  w.u (t_tree_base + code);
  // FIXME:Write length info
  // Write core bits
  // Write lang_specific info
  // write lang_specific bits
  // Write core pointers & vals
  // Write lang_specific pointers & vals
}

/* Read in a tree using TAG.  TAG is either a back reference, or a
   TREE_CODE for a new TREE.  For any tree that is a DECL, this does
   not read in a definition (initial value, class defn, function body,
   instantiations, whatever).  Return true on success.  Sets *TP to
   error_mark_node if TAG is totally bogus.  */

bool
in::read_tree (FILE *d, tree *tp, unsigned tag)
{
  if (!tag)
    tag = r.u ();

  if (!tag)
    {
      *tp = NULL_TREE;
      return true;
    }

  if (tag >= t_ref_base)
    {
      tree *val = map.get (tag);

      *tp = val ? *val : error_mark_node;
      if (d)
	fprintf (d, "Reading:%u mapping to %p (%s)\n", tag, (void *)*tp,
		 *tp ? get_tree_code_name (TREE_CODE (*tp)) : "NULL");
      return val != NULL;
    }

  if (tag < t_tree_base || tag >= t_tree_base + MAX_TREE_CODES)
    {
      *tp = error_mark_node;
      return false;
    }

  tree_code code = tree_code (tag - t_tree_base);

  // FIXME:Get length
  tree t = NULL_TREE;

  // FIXME:alloc tree

  // Insert into map -- fixup for duplicates?
  // Perhaps we should add it later, but we do need to reserve the
  // number now.
  tag = next ();
  bool existed = map.put (tag, t);
  gcc_assert (!existed);
  if (d)
    fprintf (d, "Reading:%u %s (%s:%d)\n", tag, get_tree_code_name (code),
	     tree_code_class_strings [TREE_CODE_CLASS (code)], code);

  // FIXME:Read core bits
  // FIXME:read lang_specific bits
  // FIXME:Read core ptrs & vals
  // FIXME:Read lang_specific ptrs & vals

  *tp = t;

  // FIXME:Do decl-specific processing, dup decls and symbol table things
  return true;
}

void
out::walk_namespace (FILE *d, bool defns, tree ns)
{
  gcc_assert (!defns); // FIXME: todo

  bool mod_ns = CURRENT_MODULE_NAMESPACE_P (ns);

  /* Don't walk into other module's namespaces.  */
  if (MODULE_NAMESPACE_P (ns) && !mod_ns)
    {
      if (d)
	fprintf (d, "Skipping namespace '%s'\n",
		 IDENTIFIER_POINTER (DECL_NAME (ns)));
      return;
    }

  if (d)
    fprintf (d, "Walking namespace '%s'\n",
	     IDENTIFIER_POINTER (DECL_NAME (ns)));

  const cp_binding_level *b = NAMESPACE_LEVEL (ns);
  for (tree name = b->names; name; name = TREE_CHAIN (name))
    if (mod_ns || MODULE_EXPORT_P (name))
      {
	// FIXME:Add other decls later
	switch (TREE_CODE (name))
	  {
	  case FUNCTION_DECL:
	    write_tree (d, name);
	  case VAR_DECL:
	  case TYPE_DECL:
	    break;
	  default:
	    gcc_unreachable ();
	  }
	
      }
  for (tree inner = b->namespaces; inner;
       inner = DECL_CHAIN (inner))
    walk_namespace (d, defns, inner);
}

}
		  
/* Mangling for module files.  */
#define MOD_FNAME_PFX "g++-"
#define MOD_FNAME_SFX ".nms" /* New Module System.  Honest.  */
#define MOD_FNAME_DOT '-'

/* Mangling for module symbol.  */
#define MOD_SYM_PFX "_M"
#if !defined (NO_DOT_IN_LABEL)
#define MOD_SYM_DOT '.'
#elif !defined (NO_DOLLAR_IN_LABEL)
#define MOD_SYM_DOT '$'
#else
#define MOD_SYM_DOT '_'
#endif

static GTY(()) tree module_namespace_name;
static GTY(()) tree module_name;
static location_t module_loc;
static GTY(()) tree proclaimer;
static bool is_interface;
static int export_depth; /* -1 for singleton export.  */

/* The set of imported modules.  The current declared module is
   included in this set too.  Maps to an import_kind.  */
static GTY(()) hash_map<tree, unsigned> *imported_modules;
enum import_kind
{
  ik_indirect,/* Import via import.  */
  ik_import,  /* Regular import.  */
  ik_export,  /* Exported import.  */
  ik_impl,    /* The implementation */
  ik_inter    /* The interface.  */
};

/* Lazily open the dumping stream, if enabled. */
static inline FILE *
dopen ()
{
  return dump_begin (TDI_lang, NULL);
}

static inline void
dclose (FILE *stream)
{
  if (stream)
    dump_end (TDI_lang, stream);
}

/* If we're in the purview of a module, push its local namespace.  */

void
push_module_namespace (bool do_it)
{
  gcc_assert (TREE_CODE (current_scope ()) == NAMESPACE_DECL
	      && (!do_it || module_namespace_name));
  if (do_it && push_namespace (module_namespace_name) < 0)
    {
      MODULE_NAMESPACE_P (current_namespace) = true;
      make_namespace_inline ();
    }
}

/* If we're in the current module's local namespace, pop out of it.  */

bool
pop_module_namespace ()
{
  gcc_assert (TREE_CODE (current_scope ()) == NAMESPACE_DECL);
  bool do_it = CURRENT_MODULE_NAMESPACE_P (current_namespace);
  if (do_it)
    pop_namespace ();
  return do_it;
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

/* Return true iff we're in the purview of a named module.  */

bool
module_purview_p ()
{
  return module_name;
}

/* Return true iff we're the interface TU (this also means we're in a
   module perview.  */

bool
module_interface_p ()
{
  return is_interface;
}

/* Convert a module name into a file name.  The name is malloced.

   (for the moment) this replaces '.' with '-' adds a prefix and
   suffix.

   FIXME: Add host-applicable hooks.  */

static char *
module_to_ext (tree id, const char *pfx, const char *sfx, char dot)
{
  char *name = concat (pfx, IDENTIFIER_POINTER (id), sfx, NULL);
  char *ptr = name + strlen (pfx);
  size_t len = IDENTIFIER_LENGTH (id);

  if (dot != '.')
    for (; len--; ptr++)
      if (*ptr == '.')
	*ptr = dot;

  return name;
}

static char *
module_to_filename (tree id)
{
  return module_to_ext (id, MOD_FNAME_PFX, MOD_FNAME_SFX, MOD_FNAME_DOT);
}

static bool
do_import_module (location_t, tree, tree, import_kind);

/* Read a module NAME file name FNAME on STREAM.  */

static bool
read_module (FILE *stream, const char *fname, tree name, import_kind kind)
{
  in in (stream, fname, kind == ik_impl);
  FILE *d = dopen ();

  if (d)
    fprintf (d, "Importing '%s'\n", IDENTIFIER_POINTER (name));

  int ok = in.header (d, name);
  if (ok)
    {
      tree imp;
      while ((ok = in.read_one (d, imp)) > 0)
	if (ok & 0x10)
	  {
	    /* We close the dump file around the inner import, as that
	       will reopen it.  We don't close the module file we're
	       reading from.  This could lead to a lot of concurrent
	       open files.  Should that be a problem, we should adjust
	       read_one to cope with reading a series of imports
	       before we then save & close file state.  */
	    if (d)
	      fprintf (d, "Begin nested import '%s'\n",
		       IDENTIFIER_POINTER (imp));
	    dclose (d);
	    // FIXME: importing is undoubtabtly more complicated,
	    // I have not got things right
	    ok = do_import_module (UNKNOWN_LOCATION, imp, NULL_TREE,
				   kind == ik_impl ? ik_import : ik_indirect);
	    d = dopen ();
	    if (d)
	      fprintf (d, "Completed nested import '%s' %s\n",
		       IDENTIFIER_POINTER (imp), ok ? "ok" : "failed");
	    if (!ok)
	      {
		inform (UNKNOWN_LOCATION, "while importing %qE (%qs)",
			name, fname);
		/* Bail now, things are likely to go really bad.  */
		break;
	      }
	  }
    }

  if (int e = in.done ())
    {
      error ("failed to read module %qE (%qs): %s", name, fname,
	     e >= 0 ? xstrerror (errno) : "Bad file data");
      ok = false;
    }

  dclose (d);
  return ok;
}

/* Import the module NAME into the current TU. */

static bool
do_import_module (location_t loc, tree name, tree, import_kind kind)
{
  if (!imported_modules)
    imported_modules = new hash_map<tree, unsigned>;

  bool existed;
  unsigned *val = &imported_modules->get_or_insert (name, &existed);

  if (!existed)
    *val = kind;
  else
    {
      if (*val >= ik_impl)
	{
	  error_at (loc, "already declared as module %qE", name);
	  return false;
	}
      else if (kind >= ik_impl)
	{
	  error_at (loc, "module %qE already imported", name);
	  return false;
	}

      if (*val < kind)
	*val = kind;
      return true;
    }
  if (kind == ik_inter)
    return true;

  // FIXME:Path search along the -I path
  // FIXME: Think about make dependency generation
  char *fname = module_to_filename (name);
  FILE *stream = fopen (fname, "rb");
  bool ok = false;

  if (!stream)
    error_at (loc, "cannot find module %qE (%qs): %m", name, fname);
  else
    {
      ok = read_module (stream, fname, name, kind);
      fclose (stream);
    }
  free (fname);
  return ok;
}

void
import_module (location_t loc, tree name, tree attrs)
{
  do_import_module (loc, name, attrs, ik_import);
}

/* Import the module NAME into the current TU and re-export it.  */

void
export_module (location_t loc, tree name, tree attrs)
{
  do_import_module (loc, name, attrs, ik_export);
}

/* Declare the name of the current module to be NAME. ATTRS is used to
   determine if this is the interface or not.  */

void
declare_module (location_t loc, tree name, tree attrs)
{
  if (module_name)
    {
      error_at (loc, "module %qE already declared", name);
      inform (module_loc, "existing declaration");
      return;
    }

  /* Look for 'interface' attribute.  There's no point caching the
     identifier, because module declaration occurs at most once.  */
  bool inter = lookup_attribute ("interface", attrs) != NULL_TREE;

  if (!inter)
    {
      // FIXME: Command line switches or file suffix check?
    }

  module_name = name;
  module_loc = loc;
  char *sym = module_to_ext (name, MOD_SYM_PFX, NULL, MOD_SYM_DOT);
  module_namespace_name = get_identifier (sym);
  free (sym);

  do_import_module (loc, name, attrs, inter ? ik_inter : ik_impl);

  push_module_namespace (true);
  is_interface = inter;
}

typedef std::pair<out *, FILE *> write_import_cl;
inline bool /* Cannot be static, due to c++-98 external linkage
	       requirement. */
write_import (const tree &name, const unsigned &kind,
	      write_import_cl const &cl)
{
  if (kind == ik_import || kind == ik_export)
    cl.first->tag_import (cl.second, name, kind == ik_export);
  return false;
}

static void
write_module (FILE *stream, const char *fname, tree name)
{
  out out (stream, fname);
  FILE *d = dopen ();

  if (d)
    fprintf (d, "Writing module '%s'\n", IDENTIFIER_POINTER (name));

  out.header (d, name);
  out.tag_conf (d);
  // FIXME:Write 'important' flags etc

  /* Write the import table.  */
  imported_modules->traverse<const write_import_cl &, write_import>
    (write_import_cl (&out, d));
  
  /* Write decls.  */
  out.walk_namespace (d, false, global_namespace);

  // FIXME:Write defns

  out.tag_eof ();
  if (int e = out.done ())
    error ("failed to write module %qE (%qs): %s", name, fname,
	   e >= 0 ? xstrerror (errno) : "Bad file data");
  dclose (d);
}

/* Finalize the module at end of parsing.  */

void
finish_module ()
{
  if (is_interface)
    {
      // FIXME:option to specify location? take dirname from output file?
      char *fname = module_to_filename (module_name);

      if (!errorcount)
	{
	  FILE *stream = fopen (fname, "wb");

	  if (!stream)
	    error_at (module_loc, "cannot open module interface %qE (%qs): %m",
		      module_name, fname);
	  else
	    {
	      write_module (stream, fname, module_name);
	      fclose (stream);
	    }
	}
      if (errorcount)
	unlink (fname);
      free (fname);
    }

  delete imported_modules;
  imported_modules = NULL;
}

#include "gt-cp-module.h"
