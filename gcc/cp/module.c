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
  bool ok ()
  {
    errno = err;
    return !err;
  }
public:
  void bad (int e)
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

public:
  bool flush ();

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
  bool b ();
  unsigned char c ();
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
  
void writer::c (unsigned char x)
{
  reserve (1);
  buffer[pos++] = x;
}

unsigned char reader::c ()
{
  if (reserve (1))
    return buffer[pos++];
  bad (EILSEQ);
  return 0;
}
  
void writer::i (int x)
{
  reserve ((sizeof (x) * 8 + 6) / 7);

  int end = x < 0 ? -1 : 0;
  unsigned byte;
  for (;;)
    {
      byte = x & 127;
      x >>= 7;
      if (x == end)
	break;
      buffer[pos++] = byte | 128;
    }
  buffer[pos++] = byte;
}

int reader::i ()
{
  int v = 0;
  unsigned bit = 0;
  size_t bytes = reserve ((sizeof (v) * 8 + 6) / 7);
  unsigned byte;

  for (;;)
    {
      if (!bytes--)
	{
	  bad (EILSEQ);
	  return v;
	}
      byte = buffer[pos++];
      v |= (byte & 127) << bit;
      bit += 7;
      if (!(byte & 128))
	break;
    }
  if (byte & 0x40 && bit < sizeof (v) * 8)
    v |= ~(int)0 << bit;
  return v;
}

inline void writer::u (unsigned x)
{
  i (int (x));
}

inline unsigned reader::u ()
{
  return unsigned (i ());
}

void writer::wi (HOST_WIDE_INT x)
{
  reserve ((sizeof (x) * 8 + 6) / 7);

  int end = x < 0 ? -1 : 0;
  do
    {
      unsigned byte = x & 127;
      x >>= 7;
      if (x != end)
	byte |= 128;
      buffer[pos++] = byte;
    }
  while (x != end);
}

HOST_WIDE_INT reader::wi ()
{
  HOST_WIDE_INT v = 0;
  unsigned bit = 0;
  size_t bytes = reserve ((sizeof (v) * 8 + 6) / 7);
  unsigned byte;

  for (;;)
    {
      if (!bytes--)
	{
	  bad (EILSEQ);
	  return v;
	}
      byte = buffer[pos++];
      v |= (byte & 127) << bit;
      bit += 7;
      if (!(byte & 128))
	break;
    }
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
      bad (EILSEQ);
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
  return buf (len + 1);
}

bool
writer::flush ()
{
  flush_bits ();
  size_t bytes = fwrite (buffer, 1, pos, stream);
  
  if (bytes != pos && !err)
    err = errno;
  pos = 0;
  return ok ();
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
      reserve (1);
      c (bits);
      bit_pos = 0;
      bits = 0;
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
  enum tags
  {
    t_eof,
    t_ref,
    t_decl
  };

public:
  static const char *ident ();
  static int version ();
  static unsigned v2d (int v)
  {
    if (EXPERIMENTAL && v < 0)
      return -v / 10000 + 20000000;
    else
      return v;
  }
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
  bool done ()
  {
    return w.flush ();
  }
  void ref (tree, bool = false);
  void decl (tree);
};

out::out (FILE *s, const char *n)
  :w (s, n)
{
}

out::~out ()
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
    fprintf (d, "writing \"%s\" %d:%04d\n", id, v2d (v), v2t (v));
  w.i (v);
  w.str (IDENTIFIER_POINTER (name), IDENTIFIER_LENGTH (name));
}

/* Streamer in.  */
class in : public streamer
{
  reader r;
  typedef unbounded_int_hashmap_traits<unsigned,tree> traits;
  hash_map<unsigned,tree,traits> map; /* ids to trees  */

public:
  in (FILE *, const char *);
  ~in ();

public:
  bool header (FILE *, tree);
};

in::in (FILE *s, const char *n)
  :r (s, n)
{
}

in::~in ()
{
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
	  warning (0, "%qs version %d, but time is %d, not %d",
		   r.name, v_date, v_time, ver_time);
	  have_a_go = true;
	}
      if (!have_a_go)
	{
	  r.bad (EINVAL);
	  return false;
	}
    }
  if (d)
    fprintf (d, "Reading %d:%04d\n", v_date, v_time);
  if (d)
    fprintf (d, "Expecting %d:%04d\n", ver_date, ver_time);

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
static bool is_interface; // Probably change for file handle?
static int export_depth; /* -1 for singleton export.  */

/* The set of imported modules.  The current declared module is
   included in this set too.  Maps to an import_kind.  */
static GTY(()) hash_map<tree, unsigned> *imported_modules;
enum import_kind
{
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
push_module_namespace ()
{
  gcc_assert (TREE_CODE (current_scope ()) == NAMESPACE_DECL);
  if (module_namespace_name && push_namespace (module_namespace_name) < 0)
    {
      MODULE_NAMESPACE_P (current_namespace) = true;
      make_namespace_inline ();
    }
}

/* If we're in the current module's local namespace, pop out of it.  */

void
pop_module_namespace ()
{
  gcc_assert (TREE_CODE (current_scope ()) == NAMESPACE_DECL);
  if (CURRENT_MODULE_NAMESPACE_P (current_namespace))
    pop_namespace ();
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


static void
read_module (FILE *stream, const char *fname, tree name)
{
  in in (stream, fname);
  FILE *d =  dopen ();
  
  if (d)
    fprintf (d, "importing '%s'\n", IDENTIFIER_POINTER (name));

  if (!in.header (d, name))
    goto done;
  
  // FIXME: some code needed here
 done:
  dclose (d);
}

/* Import the module NAME into the current TU. */

static void
do_import_module (location_t loc, tree name, tree attrs, import_kind kind)
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
	  return;
	}
      else if (kind >= ik_impl)
	{
	  error_at (loc, "module %qE already imported", name);
	  return;
	}

      if (*val < kind)
	*val = kind;
      return;
    }
  if (kind == ik_inter)
    return;

  // FIXME:Path search along the -I path
  // FIXME: Think about make dependency generation
  char *fname = module_to_filename (name);
  FILE *stream = fopen (fname, "rb");

  if (!stream)
    error_at (loc, "cannot find module %qE (%qs): %m", name, fname);
  else
    {
      read_module (stream, fname, name);
      fclose (stream);
    }
  free (fname);
  return;
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

  push_module_namespace ();
  is_interface = inter;
}

static void
write_module (FILE *stream, const char *fname, tree name)
{
  out out (stream, fname);
  FILE *d = dopen ();

  if (d)
    fprintf (d, "writing module '%s'\n", IDENTIFIER_POINTER (name));

  out.header (d, name);
  
  // FIXME:Write 'important' flags etc
  // FIXME:Write import table
  // FIXME:Write decls & defns

  if (!out.done ())
    error ("failed to write module file %qE (%qs): %m", name, fname);
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
