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

/* Byte serializer base.  */
class cpm_serial
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
  unsigned bits;
  unsigned bit_pos;

public:
  cpm_serial (FILE *, const char *);
  ~cpm_serial ();

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

cpm_serial::cpm_serial (FILE *s, const char *n)
  :stream (s), name (n), pos (0), len (0), alloc (ALLOC),
   err (0), bits (0), bit_pos (0)
{
  buffer = (char *) xmalloc (alloc);
}

cpm_serial::~cpm_serial ()
{
  gcc_assert (pos == len || err);
  free (buffer);
}

/* Byte stream cpm_writer.  */
class cpm_writer : public cpm_serial
{
public:
  cpm_writer (FILE *s, const char *n)
    : cpm_serial (s, n)
  {
  }
  ~cpm_writer ()
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
  void bstart ();
  void bend ();

public:
  void c (unsigned char);
  void i (int);
  void u (unsigned);
  void s (size_t s);
  void wi (HOST_WIDE_INT);
  void wu (unsigned HOST_WIDE_INT);
  void str (const char *, size_t);
  void buf (const char *, size_t);
};

/* Byte stream cpm_reader.  */
class cpm_reader : public cpm_serial
{
public:
  cpm_reader (FILE *s, const char *n)
    : cpm_serial (s, n)
  {
  }
  ~cpm_reader ()
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

public:
  bool b ();
  void bstart ();
  void bend ();

public:
  int c ();
  int i ();
  unsigned u ();
  size_t s ();
  HOST_WIDE_INT wi ();
  unsigned HOST_WIDE_INT wu ();
  const char *str (size_t * = NULL);
  const char *buf (size_t);
};

/* Low level cpm_readers and cpm_writers.  I did think about making these
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

void cpm_writer::b (bool x)
{
  bits |= unsigned (x) << bit_pos++;
  if (bit_pos == 8)
    flush_bits ();
}

bool cpm_reader::b ()
{
  if (!bit_pos)
    bits = c ();
  bool v = (bits >> bit_pos) & 1;
  bit_pos = (bit_pos + 1) & 7;
  return v;
}

/* Chars are unsigned and written as single bytes.  */

void cpm_writer::c (unsigned char x)
{
  reserve (1);
  buffer[pos++] = x;
}

int cpm_reader::c ()
{
  if (reserve (1))
    return (unsigned char)buffer[pos++];
  bad ();
  return 0;
}

/* Ints are written as sleb128.  I suppose we could pack the first
   few bits into any partially-filled bool buffer.  */

void cpm_writer::i (int x)
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

int cpm_reader::i ()
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
    v |= ~(unsigned)0 << bit;
  return v;
}

/* Unsigned are written as uleb128 too.  */

inline void cpm_writer::u (unsigned x)
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

inline unsigned cpm_reader::u ()
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
inline bool cpm_reader::peek_u (unsigned u)
{
  gcc_assert (u < 128);

  if (reserve (1))
    return ((unsigned char)buffer[pos]) == u;
  return false;
}

void cpm_writer::wi (HOST_WIDE_INT x)
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

HOST_WIDE_INT cpm_reader::wi ()
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
    v |= ~(unsigned HOST_WIDE_INT)0 << bit;
  return v;
}

inline void cpm_writer::wu (unsigned HOST_WIDE_INT x)
{
  wi ((HOST_WIDE_INT) x);
}

inline unsigned HOST_WIDE_INT cpm_reader::wu ()
{
  return (unsigned HOST_WIDE_INT) wi ();
}

inline void cpm_writer::s (size_t s)
{
  if (sizeof (s) == sizeof (unsigned))
    u (s);
  else
    wu (s);
}

inline size_t cpm_reader::s ()
{
  if (sizeof (size_t) == sizeof (unsigned))
    return u ();
  else
    return wu ();
}

void cpm_writer::buf (const char *buf, size_t len)
{
  reserve (len);
  memcpy (buffer + pos, buf, len);
  pos += len;
}

const char *cpm_reader::buf (size_t len)
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
  
void cpm_writer::str (const char *string, size_t len)
{
  s (len);
  buf (string, len + 1);
}

const char *cpm_reader::str (size_t *len_p)
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
cpm_writer::flush ()
{
  flush_bits ();
  size_t bytes = fwrite (buffer, 1, pos, stream);
  
  if (bytes != pos && !err)
    err = errno;
  pos = 0;
}

void
cpm_reader::flush ()
{
  flush_bits ();
  memmove (buffer, buffer + pos, len - pos);
  len -= pos;
  pos = 0;
}

void
cpm_writer::flush_bits ()
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
cpm_writer::reserve (size_t want)
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
cpm_reader::reserve (size_t want)
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

/* Module cpm_stream base.  */
class cpm_stream
{
public:
  /* Record tags.  */
  enum record_tag
  {
    /* Module-specific records.  */
    rt_eof,  /* End Of File.  duh! */
    rt_conf, /* Config info (baked in stuff like target-triplet) */
    rt_flags, /* Flags that affect AST generation, such as fshort-enum.  */
    rt_import, /* A nested import. */
    rt_trees, /* Global trees.  */
    rt_tree_base = 0x100,      /* Tree codes.  */
    rt_ref_base = 0x1000    /* Back-reference indices.  */
  };
  struct gtp 
  {
    const tree *ptr;
    unsigned num;
  };

public:
  static const gtp global_tree_arys[];

private:
  unsigned index;

public:
  cpm_stream () : index (rt_ref_base)
  {
    gcc_assert (MAX_TREE_CODES <= rt_ref_base - rt_tree_base);
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

const char *cpm_stream::ident ()
{
  return "g++m";
}

int cpm_stream::version ()
{
  /* If the on-disk format changes, update the version number.  */
  int version = 20170210;

  if (EXPERIMENTAL)
    {
      /* Force the version to be very volatile.  */
      static const char DATE[] = __DATE__;    /* "mon dd yyyy" */
      int year = ((DATE[7] - '0') * 1000
		  + (DATE[8] - '0') * 100
		  + (DATE[9] - '0') * 10
		  + (DATE[10] - '0') * 1);
      /* JanFebMarAprMayJunJulAugSepOctNovDec */
      int mon = (DATE[0] == 'J' // Jan Jun Jul
		 ? (DATE[1] == 'a' ? 1 // Jan
		    : DATE[2] == 'n' ? 6 // Jun
		    : DATE[2] == 'l' ? 7 // Jul
		    : 0) // oops
		 : DATE[0] == 'F' ? 2 // Feb
		 : DATE[0] == 'M' // Mar May
		 ? (DATE[2] == 'r' ? 3 // Mar
		    : DATE[2] == 'y' ? 5 // May
		    : 0) // oops
		 : DATE[0] == 'A' // Apr Aug
		 ? (DATE[1] == 'p' ? 4 // Apr
		    : DATE[1] == 'u' ? 8 // Aug
		    : 0) // oops
		 : DATE[0] == 'S' ? 9 // Sep
		 : DATE[0] == 'O' ? 10 // Oct
		 : DATE[0] == 'N' ? 11 // Nov
		 : DATE[0] == 'D' ? 12 // Dec
		 : 0); // oops
      int day = ((DATE[4] == ' ' ? 0 : (DATE[4] - '0') * 10)
		 + (DATE[5] - '0') * 1);
      gcc_assert (year && mon && day);

      static const char TIME[] = __TIME__;  /* "hh:mm:ss" */
      int hour = ((TIME[0] - '0') * 10
		  + (TIME[1] - '0') * 1);
      int min =  ((TIME[3] - '0') * 10
		  + (TIME[4] - '0') * 1);

      int date = (((year % 100) * 100) + mon) * 100 + day; /* YYMMDD */
      int time = (hour * 100) + min;		/* hhmm */
      version = -((date * 10000) + time);       /* -YYMMDDhhmm */
    }
  return version;
}

/* cpm_stream cpms_out.  */
class cpms_out : public cpm_stream
{
  cpm_writer w;
  hash_map<tree,unsigned> map; /* trees to ids  */
  
public:
  cpms_out (FILE *, const char *);
  ~cpms_out ();

public:
  void header (FILE *, tree);
  void tag_eof ();
  void tag_conf (FILE *);
  void tag_import (FILE *, tree, bool);
  void tag_trees (FILE *);
  int done ()
  {
    return w.done ();
  }

private:
  void start (tree_code, tree);
  void write_loc (location_t);
  void write_tree_ary (FILE *, unsigned, const gtp *);
  void write_core_bools (FILE *, tree);
  void write_core_vals (FILE *, tree);
  void write_decl_lang_bools (FILE *, tree);

public:
  void write_tree (FILE *, tree);
  void walk_namespace (FILE *, bool, tree);
};

cpms_out::cpms_out (FILE *s, const char *n)
  :w (s, n)
{
}

cpms_out::~cpms_out ()
{
}

/* Cpm_Stream in.  */
class cpms_in : public cpm_stream
{
  cpm_reader r;
  typedef unbounded_int_hashmap_traits<unsigned,tree> traits;
  hash_map<unsigned,tree,traits> map; /* ids to trees  */
  tree scope;
  bool impl;

public:
  cpms_in (FILE *, const char *, bool);
  ~cpms_in ();

public:
  bool header (FILE *, tree);
  int tag_eof (FILE *);
  bool tag_conf (FILE *);
  int tag_import (FILE *, tree &);
  bool tag_trees (FILE *);
  int read_one (FILE *, tree &);
  int done ()
  {
    return r.done ();
  }

private:
  tree finish_namespace (FILE *, tree);
  tree finish_function (FILE *, tree);
  tree finish_type (FILE *, tree);

private:
  tree start (tree_code);
  tree finish (FILE *, tree);
  location_t read_loc ();
  bool read_tree_ary (FILE *, unsigned, const gtp *);
  bool read_core_bools (FILE *, tree);
  bool read_core_vals (FILE *, tree);
  bool read_decl_lang_bools (FILE *, tree);

private:
  void set_scope (tree);

public:
  bool read_tree (FILE *, tree *, unsigned = 0);
};

cpms_in::cpms_in (FILE *s, const char *n, bool is_impl)
  :r (s, n), scope (NULL_TREE), impl (is_impl)
{
}

cpms_in::~cpms_in ()
{
  if (scope)
    pop_inner_scope (global_namespace, scope);
}

void
cpms_out::header (FILE *d, tree name)
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
cpms_in::header (FILE *d, tree name)
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
cpms_out::tag_eof ()
{
  w.u (rt_eof);
}

int
cpms_in::tag_eof (FILE *d)
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
cpms_out::tag_conf (FILE *d)
{
  if (d)
    fprintf (d, "Writing target='%s', host='%s'\n",
	     TARGET_MACHINE, HOST_MACHINE);
  w.u (rt_conf);
  w.str (TARGET_MACHINE, strlen (TARGET_MACHINE));
  w.str (HOST_MACHINE, strlen (HOST_MACHINE));
}

bool
cpms_in::tag_conf (FILE *d)
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

/* Dump the global trees directly to save encoding them for no reason.
   Further types such as sizetype and global_namespace are oddly
   recursive, and this avoids having to deal with that in the
   cpm_reader.

   u:count
   <ary>*
*/

const cpm_stream::gtp cpm_stream::global_tree_arys[] =
  {
    {global_trees, TI_MAX},
    {cp_global_trees, CPTI_MAX},
    {&global_namespace, 1},
    {NULL, 0}
  };

void
cpms_out::tag_trees (FILE *d)
{
  w.u (rt_trees);
  unsigned ix;
  for (ix = 0; global_tree_arys[ix].ptr; ix++)
    continue;
  w.u (ix);
  for (ix = 0; global_tree_arys[ix].ptr; ix++)
    write_tree_ary (d, ix, &global_tree_arys[ix]);
}

bool
cpms_in::tag_trees (FILE *d)
{
  unsigned n = r.u ();
  unsigned ix;

  for (ix = 0; ix != n && global_tree_arys[ix].ptr; ix++)
    if (!read_tree_ary (d, ix, &global_tree_arys[ix]))
      return false;

  if (ix != n || global_tree_arys[ix].ptr)
    {
      error ("%qs has %u arrays, expected %u", r.name, n, ix);
      return false;
    }
  return true;
}

/* Global tree array
   u:count
   b[]:insert_p  */

void
cpms_out::write_tree_ary (FILE *d, unsigned ary_num, const gtp *ary_p)
{
  const tree *ary = ary_p->ptr;
  unsigned num = ary_p->num;

  w.u (num);

  unsigned n = 0;
  for (unsigned ix = 0; ix != num; ix++)
    {
      bool insert = false;
  
      if (ary[ix])
	{
	  bool existed;
	  unsigned *val = &map.get_or_insert (ary[ix], &existed);
	  if (!existed)
	    {
	      n++;
	      *val = next ();
	      insert = true;
	    }
	  if (d)
	    fprintf (d, "Fixed %u:%u index %u is %p (%s)%s\n",
		     ary_num, ix, *val, (void *)ary[ix],
		     get_tree_code_name (TREE_CODE (ary[ix])),
		     insert ? " inserted" : "");
	}
      w.b (insert);
    }
  if (d)
    fprintf (d, "Writing %u fixed trees (%d unique)\n", num, n);
}

bool
cpms_in::read_tree_ary (FILE *d, unsigned ary_num, const gtp *ary_p)
{
  const tree *ary = ary_p->ptr;
  unsigned num = ary_p->num;

  unsigned n = r.u ();
  if (n != num)
    {
      error ("%qs array %u %u trees, expected %u", r.name, ary_num, n, num);
      return false;
    }

  n = 0;
  for (unsigned ix = 0; ix != num; ix++)
    if (r.b ())
      {
	tree t = ary[ix];
	unsigned tag = next ();

	gcc_assert (t);
	n++;
	map.put (tag, t);
	if (d)
	  fprintf (d, "Fixed %u:%u index %u is %p (%s)\n",
		   ary_num, ix, tag, (void *)t,
		   get_tree_code_name (TREE_CODE (t)));
      }

  if (d)
    fprintf (d, "Reading %u fixed trees (%d unique)\n", num, n);
  return true;
}

/* Record import
   b:is_export
   str:module_name  */

void
cpms_out::tag_import (FILE *d, tree name, bool is_export)
{
  if (d)
    fprintf (d, "Writing %s '%s'\n", is_export ? "export module" : "import",
	     IDENTIFIER_POINTER (name));
  w.u (rt_import);
  w.b (is_export);
  w.str (IDENTIFIER_POINTER (name), IDENTIFIER_LENGTH (name));
}

int
cpms_in::tag_import (FILE *d, tree &imp)
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
cpms_in::read_one (FILE *d, tree &imp)
{
  unsigned rt = r.u ();
  switch (rt)
    {
    case rt_eof:
      return tag_eof (d);
    case rt_conf:
      return tag_conf (d);
    case rt_import:
      return tag_import (d, imp);
    case rt_trees:
      return tag_trees (d);

    default:
      break;
    }
  
  tree t;
  if (!read_tree (d, &t, rt))
    {
      if (t == error_mark_node)
	error ("unknown key %qd", rt);
      r.bad ();
      return false;
    }
  // FIXME: read body
  return true;
}

/* Read & write locations.  */

void
cpms_out::write_loc (location_t)
{
  // FIXME:Do something
}

location_t
cpms_in::read_loc ()
{
  // FIXME:Do something^-1
  return UNKNOWN_LOCATION;
}

void
cpms_in::set_scope (tree ctx)
{
  if (ctx != scope)
    {
      if (scope)
	pop_inner_scope (global_namespace, scope);
      else
	gcc_assert (current_scope () == global_namespace);
      scope = ctx;
      tree pop = push_inner_scope (ctx);
      gcc_assert (pop == global_namespace);
    }
}

/* Start tree write.  Write information to allocate the receiving
   node.  */

void
cpms_out::start (tree_code code, tree t)
{
  switch (code)
    {
    default:
      break;
    case IDENTIFIER_NODE:
      w.str (IDENTIFIER_POINTER (t), IDENTIFIER_LENGTH (t));
      break;
    case TREE_BINFO:
      w.u (BINFO_N_BASE_BINFOS (t));
      break;
    case TREE_VEC:
      w.u (TREE_VEC_LENGTH (t));
      break;
    case CALL_EXPR:
      w.u (VL_EXP_OPERAND_LENGTH (t));
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
      t = make_node (code);
      break;
    case IDENTIFIER_NODE:
    case STRING_CST:
      {
	size_t l;
	const char *str = r.str (&l);
	if (code == IDENTIFIER_NODE)
	  t = get_identifier_with_length (str, l);
	else
	  t = build_string (l, str);
      }
      break;
    case TREE_BINFO:
      t = make_tree_binfo (r.u ());
      break;
    case TREE_VEC:
      t = make_tree_vec (r.u ());
      break;
    case CALL_EXPR:
      t = build_vl_exp (CALL_EXPR, r.s ());
      break;
    case VECTOR_CST:
      t = make_vector (r.u ());
      break;
    case INTEGER_CST:
      {
	unsigned n = r.u ();
	unsigned e = r.u ();
	t = make_int_cst (n, e);
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
cpms_in::finish (FILE *d, tree t)
{
  if (TYPE_P (t))
    return finish_type (d, t);

  switch (TREE_CODE (t))
    {
    default: break;

    case NAMESPACE_DECL:
      return finish_namespace (d, t);

    case FUNCTION_DECL:
      return finish_function (d, t);
    }
  
  return t;
}

/* Read & write the core boolean flags.  */

void
cpms_out::write_core_bools (FILE *, tree t)
{
#define WB(X) (w.b (X))
  WB (TREE_ADDRESSABLE (t));
  WB (TREE_THIS_VOLATILE (t));
  WB (TREE_PUBLIC (t));
  WB (TREE_PRIVATE (t));
  WB (TREE_PROTECTED (t));
  WB (TREE_DEPRECATED (t));

  if (TREE_CODE (t) != TREE_VEC)
    {
      WB (TREE_LANG_FLAG_0 (t));
      WB (TREE_LANG_FLAG_1 (t));
      WB (TREE_LANG_FLAG_2 (t));
      WB (TREE_LANG_FLAG_3 (t));
      WB (TREE_LANG_FLAG_4 (t));
      WB (TREE_LANG_FLAG_5 (t));
      WB (TREE_LANG_FLAG_6 (t));
    }
  
  if (TYPE_P (t))
    {
      WB (TYPE_UNSIGNED (t));
      WB (TYPE_ARTIFICIAL (t));
      WB (TYPE_LANG_FLAG_0 (t));
      WB (TYPE_LANG_FLAG_1 (t));
      WB (TYPE_LANG_FLAG_2 (t));
      WB (TYPE_LANG_FLAG_3 (t));
      WB (TYPE_LANG_FLAG_4 (t));
      WB (TYPE_LANG_FLAG_5 (t));
      WB (TYPE_LANG_FLAG_6 (t));
      WB (TYPE_LANG_FLAG_7 (t));
    }
  else
    {
      WB (TREE_SIDE_EFFECTS (t));
      WB (TREE_CONSTANT (t));
      WB (TREE_READONLY (t));
      WB (TREE_NO_WARNING (t));
    }
  
  if (DECL_P (t))
    {
      WB (DECL_UNSIGNED (t));
      WB (DECL_NAMELESS (t));
    }

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPE_COMMON))
    {
      WB (TYPE_STRING_FLAG (t));
      WB (TYPE_NEEDS_CONSTRUCTING (t));
      WB (TYPE_PACKED (t));
      WB (TYPE_RESTRICT (t));
      WB (TYPE_USER_ALIGN (t));
      WB (TYPE_READONLY (t));
    }

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_COMMON))
    {
      WB (DECL_NONLOCAL (t));
      WB (DECL_VIRTUAL_P (t));
      WB (DECL_IGNORED_P (t));
      WB (DECL_ABSTRACT_P (t));
      WB (DECL_ARTIFICIAL (t));
      WB (DECL_USER_ALIGN (t));
      WB (DECL_PRESERVE_P (t));
      WB (DECL_EXTERNAL (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_WITH_VIS))
    {
      WB (DECL_COMMON (t));
      WB (DECL_DLLIMPORT_P (t));
      WB (DECL_WEAK (t));
      WB (DECL_SEEN_IN_BIND_EXPR_P (t));
      WB (DECL_COMDAT (t));
      WB (DECL_VISIBILITY_SPECIFIED (t));

      switch (TREE_CODE (t))
	{
	default:
	  break;
	case VAR_DECL:
	  WB (DECL_HARD_REGISTER (t));
	  WB (DECL_IN_CONSTANT_POOL (t));
	  break;
	case FUNCTION_DECL:
	  WB (DECL_FINAL_P (t));
	  WB (DECL_CXX_CONSTRUCTOR_P (t));
	  WB (DECL_CXX_DESTRUCTOR_P (t));
	  break;
	}
    }
  // FIXME: Add more
#undef WB
}

bool
cpms_in::read_core_bools (FILE *, tree t)
{
#define RB(X) ((X) = r.b ())
  RB (TREE_ADDRESSABLE (t));
  RB (TREE_THIS_VOLATILE (t));
  RB (TREE_PUBLIC (t));
  RB (TREE_PRIVATE (t));
  RB (TREE_PROTECTED (t));
  RB (TREE_DEPRECATED (t));

  if (TREE_CODE (t) != TREE_VEC)
    {
      RB (TREE_LANG_FLAG_0 (t));
      RB (TREE_LANG_FLAG_1 (t));
      RB (TREE_LANG_FLAG_2 (t));
      RB (TREE_LANG_FLAG_3 (t));
      RB (TREE_LANG_FLAG_4 (t));
      RB (TREE_LANG_FLAG_5 (t));
      RB (TREE_LANG_FLAG_6 (t));
    }
  
  if (TYPE_P (t))
    {
      RB (TYPE_UNSIGNED (t));
      RB (TYPE_ARTIFICIAL (t));
      RB (TYPE_LANG_FLAG_0 (t));
      RB (TYPE_LANG_FLAG_1 (t));
      RB (TYPE_LANG_FLAG_2 (t));
      RB (TYPE_LANG_FLAG_3 (t));
      RB (TYPE_LANG_FLAG_4 (t));
      RB (TYPE_LANG_FLAG_5 (t));
      RB (TYPE_LANG_FLAG_6 (t));
      RB (TYPE_LANG_FLAG_7 (t));
    }
  else
    {
      RB (TREE_SIDE_EFFECTS (t));
      RB (TREE_CONSTANT (t));
      RB (TREE_READONLY (t));
      RB (TREE_NO_WARNING (t));
    }
  
  if (DECL_P (t))
    {
      RB (DECL_UNSIGNED (t));
      RB (DECL_NAMELESS (t));
    }

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPE_COMMON))
    {
      RB (TYPE_STRING_FLAG (t));
      RB (TYPE_NEEDS_CONSTRUCTING (t));
      RB (TYPE_PACKED (t));
      RB (TYPE_RESTRICT (t));
      RB (TYPE_USER_ALIGN (t));
      RB (TYPE_READONLY (t));
    }

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_COMMON))
    {
      RB (DECL_NONLOCAL (t));
      RB (DECL_VIRTUAL_P (t));
      RB (DECL_IGNORED_P (t));
      RB (DECL_ABSTRACT_P (t));
      RB (DECL_ARTIFICIAL (t));
      RB (DECL_USER_ALIGN (t));
      RB (DECL_PRESERVE_P (t));
      RB (DECL_EXTERNAL (t));
    }

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_WITH_VIS))
    {
      RB (DECL_COMMON (t));
      RB (DECL_DLLIMPORT_P (t));
      RB (DECL_WEAK (t));
      RB (DECL_SEEN_IN_BIND_EXPR_P (t));
      RB (DECL_COMDAT (t));
      RB (DECL_VISIBILITY_SPECIFIED (t));

      switch (TREE_CODE (t))
	{
	default:
	  break;
	case VAR_DECL:
	  RB (DECL_HARD_REGISTER (t));
	  RB (DECL_IN_CONSTANT_POOL (t));
	  break;
	case FUNCTION_DECL:
	  RB (DECL_FINAL_P (t));
	  RB (DECL_CXX_CONSTRUCTOR_P (t));
	  RB (DECL_CXX_DESTRUCTOR_P (t));
	  break;
	}
    }
      
  // Add more
#undef RB
  return true;
}

void
cpms_out::write_decl_lang_bools (FILE *d, tree t)
{
#define WB(X) (w.b (X))
  WB (DECL_LANGUAGE (t) == lang_cplusplus);
#undef WB
}

bool
cpms_in::read_decl_lang_bools (FILE *d, tree t)
{
#define RB(X) ((X) = r.b ())
  SET_DECL_LANGUAGE (t, r.b () ? lang_cplusplus : lang_c);
#undef RB
  return true;
}

/* Read & write the core values and pointers.  */

void
cpms_out::write_core_vals (FILE *d, tree t)
{
#define WU(X) (w.u (X))
#define WT(X) (write_tree (d, X))

  WT (TREE_TYPE (t));

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_LIST))
    {
      WT (TREE_PURPOSE (t));
      WT (TREE_VALUE (t));
      WT (TREE_CHAIN (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPE_COMMON))
    {
      /* By construction we want to make sure we have the canonical
	 and main variants already in the type table, so emit them
	 now.  */
      WT (TYPE_MAIN_VARIANT (t));
      WT (TYPE_CANONICAL (t));

      WU (TYPE_MODE_RAW (t));
      WU (TYPE_PRECISION (t));
      WU (TYPE_ALIGN (t));
      WT (TYPE_SIZE (t));
      WT (TYPE_SIZE_UNIT (t));
      WT (TYPE_ATTRIBUTES (t));
      WT (TYPE_NAME (t));
      WT (CP_TYPE_CONTEXT (t));
      WT (TYPE_STUB_DECL (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPE_NON_COMMON))
    {
      switch (TREE_CODE (t))
	{
	default:
	  break;
	case ENUMERAL_TYPE:
	  WT (TYPE_VALUES (t));
	  break;
	case  ARRAY_TYPE:
	  WT (TYPE_DOMAIN (t));
	  break;
	case FUNCTION_TYPE:
	case METHOD_TYPE:
	  WT (TYPE_ARG_TYPES (t));
	  break;
	}
      if (!POINTER_TYPE_P (t))
	WT (TYPE_MINVAL (t));
      WT (TYPE_MAXVAL (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_MINIMAL))
    {
      WT (DECL_NAME (t));
      WT (CP_DECL_CONTEXT (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_COMMON))
    {
      WU (DECL_MODE (t));
      WU (DECL_ALIGN (t));
      WT (DECL_SIZE (t));
      WT (DECL_SIZE_UNIT (t));
      WT (DECL_ATTRIBUTES (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_NON_COMMON))
    {
      if (TREE_CODE (t) == TYPE_DECL)
	WT (DECL_ORIGINAL_TYPE (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_WITH_VIS))
    {
      WU (DECL_VISIBILITY (t));
      WT (DECL_ASSEMBLER_NAME_SET_P (t)
	  ? DECL_ASSEMBLER_NAME (t) : NULL_TREE);
    }
#undef WT
#undef WU
}

bool
cpms_in::read_core_vals (FILE *d, tree t)
{
#define RU(X) ((X) = r.u ())
#define RM(X) ((X) = machine_mode (r.u ()))
#define RT(X) if (!read_tree (d, &(X))) return false
  RT (TREE_TYPE (t));

   if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_LIST))
    {
      RT (TREE_PURPOSE (t));
      RT (TREE_VALUE (t));
      RT (TREE_CHAIN (t));
    }
 if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPE_COMMON))
    {
      RT (TYPE_MAIN_VARIANT (t));
      RT (TYPE_CANONICAL (t));

      RM (TYPE_MODE_RAW (t));
      RU (TYPE_PRECISION (t));
      SET_TYPE_ALIGN (t, r.u ());
      RT (TYPE_SIZE (t));
      RT (TYPE_SIZE_UNIT (t));
      RT (TYPE_ATTRIBUTES (t));
      RT (TYPE_NAME (t));
      RT (TYPE_CONTEXT (t));
      RT (TYPE_STUB_DECL (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPE_NON_COMMON))
    {
      switch (TREE_CODE (t))
	{
	default:
	  break;
	case ENUMERAL_TYPE:
	  RT (TYPE_VALUES (t));
	  break;
	case  ARRAY_TYPE:
	  RT (TYPE_DOMAIN (t));
	  break;
	case FUNCTION_TYPE:
	case METHOD_TYPE:
	  RT (TYPE_ARG_TYPES (t));
	  break;
	}
      if (!POINTER_TYPE_P (t))
	RT (TYPE_MINVAL (t));
      RT (TYPE_MAXVAL (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_MINIMAL))
    {
      RT (DECL_NAME (t));
      RT (DECL_CONTEXT (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_COMMON))
    {
      RM (DECL_MODE (t));
      SET_DECL_ALIGN (t, r.u ());
      RT (DECL_SIZE (t));
      RT (DECL_SIZE_UNIT (t));
      RT (DECL_ATTRIBUTES (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_NON_COMMON))
    {
      if (TREE_CODE (t) == TYPE_DECL)
	RT (DECL_ORIGINAL_TYPE (t));
    }
  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_WITH_VIS))
    {
      DECL_VISIBILITY (t) = symbol_visibility (r.u ());
      tree name;
      RT (name);
      if (name)
	SET_DECL_ASSEMBLER_NAME (t, name);
    }
#undef RT
#undef RM
#undef RU
  return true;
}

/* Write either the decl (as a declaration) itself (and create a
   mapping for it), or write the existing mapping.  This is
   essentially the lisp self-referential structure pretty-printer,
   except that we implicitly number every node, so need neither two
   passes, nor explicit labelling.   */

void
cpms_out::write_tree (FILE *d, tree t)
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
    {
      tree name = t;
      
      if (TYPE_P (name))
	name = TYPE_NAME (name);
      if (!name)
	;
      else if (DECL_P (name))
	name = DECL_NAME (name);
      else if (TREE_CODE (name) != IDENTIFIER_NODE)
	name = NULL_TREE;
      fprintf (d, "Writing:%u %p (%s:%s)\n", *val, (void *)t,
	       get_tree_code_name (TREE_CODE (t)),
	       name ? IDENTIFIER_POINTER (name) : "");
    }

  gcc_assert (rt_tree_base + code < rt_ref_base);
  w.u (rt_tree_base + code);
  start (code, t);

  if (code != IDENTIFIER_NODE)
    {
      write_core_bools (d, t);

      if (TYPE_P (t))
	{
	  bool has_specific = TYPE_LANG_SPECIFIC (t) != NULL;
	  w.b (has_specific);
	  // FIXME:write lang_specific bits
	}
      else if (DECL_P (t))
	{
	  bool has_specific = DECL_LANG_SPECIFIC (t) != NULL;
	  w.b (has_specific);
	  if (has_specific)
	    write_decl_lang_bools (d, t);
	}

      write_core_vals (d, t);
      // FIXME:Write lang_specific pointers & vals
    }
}

/* Read in a tree using TAG.  TAG is either a back reference, or a
   TREE_CODE for a new TREE.  For any tree that is a DECL, this does
   not read in a definition (initial value, class defn, function body,
   instantiations, whatever).  Return true on success.  Sets *TP to
   error_mark_node if TAG is totally bogus.  */

bool
cpms_in::read_tree (FILE *d, tree *tp, unsigned tag)
{
  if (!tag)
    tag = r.u ();

  if (!tag)
    {
      *tp = NULL_TREE;
      return true;
    }

  if (tag >= rt_ref_base)
    {
      tree *val = map.get (tag);

      *tp = val ? *val : error_mark_node;
      if (d)
	fprintf (d, "Reading:%u found %p (%s)\n", tag, (void *)*tp,
		 *tp ? get_tree_code_name (TREE_CODE (*tp)) : "NULL");
      return val != NULL;
    }

  if (tag < rt_tree_base || tag >= rt_tree_base + MAX_TREE_CODES)
    {
      *tp = error_mark_node;
      return false;
    }

  tree_code code = tree_code (tag - rt_tree_base);

  tree t = start (code);

  /* Insert into map.  */
  tag = next ();
  bool existed = map.put (tag, t);
  gcc_assert (!existed);
  if (d)
    fprintf (d, "Reading:%u %s\n", tag, get_tree_code_name (code));

  if (code != IDENTIFIER_NODE)
    {
      if (!read_core_bools (d, t))
	return false;

      if (TYPE_P (t) && r.b ())
	{
	  if (!maybe_add_lang_type_raw (t))
	    {
	      /* We were lied to about needing lang_type.  */
	      r.bad ();
	      return false;
	    }
	  
	  // FIXME:read lang_specific bits
	}
      else if (DECL_P (t) && r.b ())
	{
	  if (!maybe_add_lang_decl_raw (t))
	    {
	      /* We were lied to about needing lang_decl.  */
	      r.bad ();
	      return false;
	    }

	  if (!read_decl_lang_bools (d, t))
	    return false;
	}

      if (!read_core_vals (d, t))
	return false;

      // FIXME:Read lang_specific ptrs & vals
    }

  tree found = finish (d, t);
  if (found != t)
    {
      /* Update the mapping.  */
      t = found;
      map.put (tag, t);
    }
  *tp = t ? t : error_mark_node;

  if (d && t)
    {
      tree name = found;
      
      if (TYPE_P (name))
	name = TYPE_NAME (name);
      if (!name)
	;
      else if (DECL_P (name))
	name = DECL_NAME (name);
      else if (TREE_CODE (name) != IDENTIFIER_NODE)
	name = NULL_TREE;
      fprintf (d, "Index %u inserting %p (%s:%s)\n", tag, (void *)found,
	       get_tree_code_name (TREE_CODE (found)),
	       name ? IDENTIFIER_POINTER (name) : "");
    }

  return t != NULL_TREE;
}

void
cpms_out::walk_namespace (FILE *d, bool defns, tree ns)
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
    if (mod_ns || MODULE_EXPORT_P (name)
	// FIXME: set MODULE_EXPORT_P properly.  Utter hack here
	|| !DECL_EXTERN_C_P (name))
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

static GTY(()) tree global_name; /* Name for global module namespace.  */
static GTY(()) tree module_name; /* Name for this module namespaces. */
static GTY(()) tree module_user; /* Name presented in diagnostics.  */
static location_t module_loc;	 /* Location of the module decl.  */
static GTY(()) tree proclaimer;
static bool is_interface;		/* We are the interface TU. */
static int export_depth; /* -1 for singleton export.  */

/* Rebuild a streamed in type.  */
// FIXME: c++-specific types are not in the canonical type hash.
// Perhaps that should be changed?

tree
cpms_in::finish_type (FILE *d, tree type)
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
	if (check_base_type (type, probe)
	    && TYPE_QUALS (type) == TYPE_QUALS (probe)
	    && comp_except_specs (TYPE_RAISES_EXCEPTIONS (type),
				  TYPE_RAISES_EXCEPTIONS (probe), ce_exact)
	    && type_memfn_rqual (type) == type_memfn_rqual (probe))
	  {
	    if (d)
	      fprintf (d, "Type %p already found as %p variant of %p\n",
		       (void *)type, (void *)probe, (void *)main);
	    free_node (type);
	    type = probe;
	    goto found_variant;
	  }

      /* Splice it into the main variant list.  */
      if (d)
	fprintf (d, "Type %p added as variant of %p\n",
		 (void *)type, (void *)main);
      TYPE_NEXT_VARIANT (type) = TYPE_NEXT_VARIANT (main);
      TYPE_NEXT_VARIANT (main) = type;
      /* CANONICAL_TYPE is either already correctly remapped.  Or
         correctly already us.  FIXME:Are we sure abcpms_out this?  */
    found_variant:;
    }
  else if (!TYPE_STRUCTURAL_EQUALITY_P (type))
    {
      gcc_assert (TYPE_ALIGN (type));
      hashval_t hash = type_hash_default (type);
      /* type_hash_canon frees type, if we find it already.  */
      type = type_hash_canon (hash, type);
      // FIXME: This is where it'd be nice to determine if type
      // was already found.  See above.
      if (d)
	fprintf (d, "Adding type %p as canonical %p\n",
		 (void *)main, (void *)type);
    }

  return type;
}

/* Finish a function decl FN.  Insert into the symbol table or do
   duplicate decl processing.  */

tree
cpms_in::finish_function (FILE *d, tree fn)
{
  // FIXME: want to look exactly in scope,  no using decls etc.
  tree cur = lookup_qualified_name (DECL_CONTEXT (fn), DECL_NAME (fn),
				    false, false, false);
  if (cur == error_mark_node)
    cur = NULL_TREE;
  else
    {
      return fn;
      gcc_unreachable (); // FIXME: deal with overloads & duplicates
    }

  set_scope (DECL_CONTEXT (fn));
  fn = pushdecl (fn);
  if (fn == error_mark_node)
    return fn; // FIXME:why?
  if (DECL_CONTEXT (fn) == global_namespace)
    DECL_CONTEXT (fn) = DECL_CONTEXT (global_namespace);

  if (d)
    fprintf (d, "Inserting function decl %s (%p)\n",
	     IDENTIFIER_POINTER (DECL_NAME (fn)), (void *)fn);
  return fn;
}

/* NS has just been read in.  Find the actual namespace we should be
   using.  */

tree
cpms_in::finish_namespace (FILE *d, tree ns)
{
  tree res = NULL_TREE;

  /* We will not have frobbed the namespace yet.  */
  set_scope (DECL_CONTEXT (ns));
  if (int push = push_namespace (DECL_NAME (ns)))
    {
      bool inline_p = NAMESPACE_INLINE_P (ns);
      bool module_p = MODULE_NAMESPACE_P (ns);

      res = current_namespace;
      if (module_p && (!impl || DECL_NAME (ns) != module_name))
	inline_p = false;

      if (push < 0)
	{
	  if (module_p)
	    MODULE_NAMESPACE_P (res) = true;
	  if (inline_p)
	    make_namespace_inline ();
	  if (d)
	    fprintf (d, "Creating%s namespace %s (%p)\n",
		     inline_p ? " inline" : "",
		     IDENTIFIER_POINTER (DECL_NAME (ns)), (void *)res);
	}
      else
	{
	  /* Existing namespace.  Make sure it matches.  */
	  if (inline_p != NAMESPACE_INLINE_P (ns)
	      || module_p != MODULE_NAMESPACE_P (ns))
	    {
	      error ("mismatched namespace %qD", res);
	      res = NULL_TREE;
	    }
	}
      /* Pop cpms_out of the namespace so our scope cache is correct.  */
      pop_namespace ();
    }
  free_node (ns);

  return res;
}

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
	      && (!do_it || module_name));
  if (do_it && push_namespace (module_name) < 0)
    {
      MODULE_NAMESPACE_P (current_namespace) = true;
      make_namespace_inline ();
    }
}

/* If we're in the current module's local namespace, pop cpms_out of it.  */

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
  return module_user;
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
  cpms_in in (stream, fname, kind == ik_impl);
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
  // FIXME: Think abcpms_out make dependency generation
  char *fname = module_to_filename (name);
  FILE *stream = fopen (fname, "rb");
  bool ok = false;

  if (!stream)
    error_at (loc, "cannot find module %qE (%qs): %m", name, fname);
  else
    {
      tree ctx = current_scope ();
      ok = read_module (stream, fname, name, kind);
      gcc_assert (ctx == current_scope ());
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
  if (module_user)
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

  module_user = name;
  module_loc = loc;
  char *sym = module_to_ext (name, MOD_SYM_PFX, NULL, MOD_SYM_DOT);
  module_name = get_identifier (sym);
  free (sym);

  do_import_module (loc, name, attrs, inter ? ik_inter : ik_impl);

  is_interface = inter;
  if (is_interface) // FIXME:we should do in both cases (or neither)
    push_module_namespace (true);
}

typedef std::pair<cpms_out *, FILE *> write_import_cl;
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
  cpms_out out (stream, fname);
  FILE *d = dopen ();

  if (d)
    fprintf (d, "Writing module '%s'\n", IDENTIFIER_POINTER (name));

  out.header (d, name);
  out.tag_conf (d);
  // FIXME:Write 'important' flags etc

  out.tag_trees (d);

  /* Write the import table.  */
  imported_modules->traverse<const write_import_cl &, write_import>
    (write_import_cl (&out, d));
  
  /* Write decls.  */
  out.walk_namespace (d, false, global_namespace);

  // FIXME:Write defns.  Probably fine just to do it during the first
  // namespace walk.

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
      char *fname = module_to_filename (module_user);

      if (!errorcount)
	{
	  FILE *stream = fopen (fname, "wb");

	  if (!stream)
	    error_at (module_loc, "cannot open module interface %qE (%qs): %m",
		      module_user, fname);
	  else
	    {
	      write_module (stream, fname, module_user);
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
