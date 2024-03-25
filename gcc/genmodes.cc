/* Generate the machine mode enumeration and associated tables.
   Copyright (C) 2003-2024 Free Software Foundation, Inc.

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

#include "bconfig.h"
#include "system.h"
#include "errors.h"

/* enum mode_class is normally defined by machmode.h but we can't
   include that header here.  */
#include "mode-classes.def"

#define DEF_MODE_CLASS(M) M
enum mode_class { MODE_CLASSES, MAX_MODE_CLASS };
#undef DEF_MODE_CLASS

/* Text names of mode classes, for output.  */
#define DEF_MODE_CLASS(M) #M
static const char *const mode_class_names[MAX_MODE_CLASS] =
{
  MODE_CLASSES
};
#undef DEF_MODE_CLASS
#undef MODE_CLASSES

#ifdef EXTRA_MODES_FILE
# define HAVE_EXTRA_MODES 1
#else
# define HAVE_EXTRA_MODES 0
# define EXTRA_MODES_FILE ""
#endif

/* Data structure for building up what we know about a mode.
   They're clustered by mode class.  */
struct mode_data
{
  struct mode_data *next;	/* next this class - arbitrary order */

  const char *name;		/* printable mode name -- SI, not SImode */
  enum mode_class cl;		/* this mode class */
  unsigned int order;		/* top-level sorting order */
  unsigned int precision;	/* size in bits, equiv to TYPE_PRECISION */
  unsigned int bytesize;	/* storage size in addressable units */
  unsigned int ncomponents;	/* number of subunits */
  unsigned int alignment;	/* mode alignment */
  const char *format;		/* floating point format - float modes only */

  struct mode_data *component;	/* mode of components */
  struct mode_data *wider;	/* next wider mode */

  struct mode_data *contained;  /* Pointer to list of modes that have
				   this mode as a component.  */
  struct mode_data *next_cont;  /* Next mode in that list.  */

  struct mode_data *complex;	/* complex type with mode as component.  */
  const char *file;		/* file and line of definition, */
  unsigned int line;		/* for error reporting */
  unsigned int counter;		/* Rank ordering of modes */
  unsigned int ibit;		/* the number of integral bits */
  unsigned int fbit;		/* the number of fractional bits */
  bool need_nunits_adj;		/* true if this mode needs dynamic nunits
				   adjustment */
  bool need_bytesize_adj;	/* true if this mode needs dynamic size
				   adjustment */
  unsigned int int_n;		/* If nonzero, then __int<INT_N> will be defined */
  bool boolean;
};

static struct mode_data *modes[MAX_MODE_CLASS];
static unsigned int n_modes[MAX_MODE_CLASS];
static struct mode_data *void_mode;

static const struct mode_data blank_mode = {
  0, "<unknown>", MAX_MODE_CLASS,
  0, -1U, -1U, -1U, -1U,
  0, 0, 0, 0, 0, 0,
  "<unknown>", 0, 0, 0, 0, false, false, 0,
  false
};

static htab_t modes_by_name;

/* Data structure for recording target-specified runtime adjustments
   to a particular mode.  We support varying the byte size, the
   alignment, and the floating point format.  */
struct mode_adjust
{
  struct mode_adjust *next;
  struct mode_data *mode;
  const char *adjustment;

  const char *file;
  unsigned int line;
};

static struct mode_adjust *adj_nunits;
static struct mode_adjust *adj_bytesize;
static struct mode_adjust *adj_alignment;
static struct mode_adjust *adj_format;
static struct mode_adjust *adj_ibit;
static struct mode_adjust *adj_fbit;
static struct mode_adjust *adj_precision;

/* Mode class operations.  */
static enum mode_class
complex_class (enum mode_class c)
{
  switch (c)
    {
    case MODE_INT: return MODE_COMPLEX_INT;
    case MODE_PARTIAL_INT: return MODE_COMPLEX_INT;
    case MODE_FLOAT: return MODE_COMPLEX_FLOAT;
    default:
      error ("no complex class for class %s", mode_class_names[c]);
      return MODE_RANDOM;
    }
}

static enum mode_class
vector_class (enum mode_class cl)
{
  switch (cl)
    {
    case MODE_INT: return MODE_VECTOR_INT;
    case MODE_FLOAT: return MODE_VECTOR_FLOAT;
    case MODE_FRACT: return MODE_VECTOR_FRACT;
    case MODE_UFRACT: return MODE_VECTOR_UFRACT;
    case MODE_ACCUM: return MODE_VECTOR_ACCUM;
    case MODE_UACCUM: return MODE_VECTOR_UACCUM;
    default:
      error ("no vector class for class %s", mode_class_names[cl]);
      return MODE_RANDOM;
    }
}

/* Utility routines.  */
static inline struct mode_data *
find_mode (const char *name)
{
  struct mode_data key;

  key.name = name;
  return (struct mode_data *) htab_find (modes_by_name, &key);
}

static struct mode_data *
new_mode (enum mode_class cl, const char *name,
	  const char *file, unsigned int line)
{
  struct mode_data *m;
  static unsigned int count = 0;

  m = find_mode (name);
  if (m)
    {
      error ("%s:%d: duplicate definition of mode \"%s\"",
	     trim_filename (file), line, name);
      error ("%s:%d: previous definition here", m->file, m->line);
      return m;
    }

  m = XNEW (struct mode_data);
  memcpy (m, &blank_mode, sizeof (struct mode_data));
  m->cl = cl;
  m->name = name;
  if (file)
    m->file = trim_filename (file);
  m->line = line;
  m->counter = count++;

  m->next = modes[cl];
  modes[cl] = m;
  n_modes[cl]++;

  *htab_find_slot (modes_by_name, m, INSERT) = m;

  return m;
}

static hashval_t
hash_mode (const void *p)
{
  const struct mode_data *m = (const struct mode_data *)p;
  return htab_hash_string (m->name);
}

static int
eq_mode (const void *p, const void *q)
{
  const struct mode_data *a = (const struct mode_data *)p;
  const struct mode_data *b = (const struct mode_data *)q;

  return !strcmp (a->name, b->name);
}

#define for_all_modes(C, M)			\
  for (C = 0; C < MAX_MODE_CLASS; C++)		\
    for (M = modes[C]; M; M = M->next)

static void ATTRIBUTE_UNUSED
new_adjust (const char *name,
	    struct mode_adjust **category, const char *catname,
	    const char *adjustment,
	    enum mode_class required_class_from,
	    enum mode_class required_class_to,
	    const char *file, unsigned int line)
{
  struct mode_data *mode = find_mode (name);
  struct mode_adjust *a;

  file = trim_filename (file);

  if (!mode)
    {
      error ("%s:%d: no mode \"%s\"", file, line, name);
      return;
    }

  if (required_class_from != MODE_RANDOM
      && (mode->cl < required_class_from || mode->cl > required_class_to))
    {
      error ("%s:%d: mode \"%s\" is not among class {%s, %s}",
	     file, line, name, mode_class_names[required_class_from] + 5,
	     mode_class_names[required_class_to] + 5);
      return;
    }

  for (a = *category; a; a = a->next)
    if (a->mode == mode)
      {
	error ("%s:%d: mode \"%s\" already has a %s adjustment",
	       file, line, name, catname);
	error ("%s:%d: previous adjustment here", a->file, a->line);
	return;
      }

  a = XNEW (struct mode_adjust);
  a->mode = mode;
  a->adjustment = adjustment;
  a->file = file;
  a->line = line;

  a->next = *category;
  *category = a;
}

/* Diagnose failure to meet expectations in a partially filled out
   mode structure.  */
enum requirement { SET, UNSET, OPTIONAL };

#define validate_field_(mname, fname, req, val, unset, file, line) do {	\
  switch (req)								\
    {									\
    case SET:								\
      if (val == unset)							\
	error ("%s:%d: (%s) field %s must be set",			\
	       file, line, mname, fname);				\
      break;								\
    case UNSET:								\
      if (val != unset)							\
	error ("%s:%d: (%s) field %s must not be set",			\
	       file, line, mname, fname);				\
    case OPTIONAL:							\
      break;								\
    }									\
} while (0)

#define validate_field(M, F) \
  validate_field_(M->name, #F, r_##F, M->F, blank_mode.F, M->file, M->line)

static void
validate_mode (struct mode_data *m,
	       enum requirement r_precision,
	       enum requirement r_bytesize,
	       enum requirement r_component,
	       enum requirement r_ncomponents,
	       enum requirement r_format)
{
  validate_field (m, precision);
  validate_field (m, bytesize);
  validate_field (m, component);
  validate_field (m, ncomponents);
  validate_field (m, format);
}
#undef validate_field
#undef validate_field_

/* Given a partially-filled-out mode structure, figure out what we can
   and fill the rest of it in; die if it isn't enough.  */
static void
complete_mode (struct mode_data *m)
{
  unsigned int alignment;

  if (!m->name)
    {
      error ("%s:%d: mode with no name", m->file, m->line);
      return;
    }
  if (m->cl == MAX_MODE_CLASS)
    {
      error ("%s:%d: %smode has no mode class", m->file, m->line, m->name);
      return;
    }

  switch (m->cl)
    {
    case MODE_RANDOM:
      /* Nothing more need be said.  */
      if (!strcmp (m->name, "VOID"))
	void_mode = m;

      validate_mode (m, UNSET, UNSET, UNSET, UNSET, UNSET);

      m->precision = 0;
      m->bytesize = 0;
      m->ncomponents = 0;
      m->component = 0;
      break;

    case MODE_CC:
      /* Again, nothing more need be said.  For historical reasons,
	 the size of a CC mode is four units.  */
      validate_mode (m, UNSET, UNSET, UNSET, UNSET, UNSET);

      m->bytesize = 4;
      m->ncomponents = 1;
      m->component = 0;
      break;

    case MODE_INT:
    case MODE_FLOAT:
    case MODE_DECIMAL_FLOAT:
    case MODE_FRACT:
    case MODE_UFRACT:
    case MODE_ACCUM:
    case MODE_UACCUM:
      /* A scalar mode must have a byte size, may have a bit size,
	 and must not have components.   A float mode must have a
         format.  */
      validate_mode (m, OPTIONAL, SET, UNSET, UNSET,
		     (m->cl == MODE_FLOAT || m->cl == MODE_DECIMAL_FLOAT)
		     ? SET : UNSET);

      m->ncomponents = 1;
      m->component = 0;
      break;

    case MODE_OPAQUE:
      /* Opaque modes have size and precision.  */
      validate_mode (m, OPTIONAL, SET, UNSET, UNSET, UNSET);

      m->ncomponents = 1;
      m->component = 0;
      break;

    case MODE_PARTIAL_INT:
      /* A partial integer mode uses ->component to say what the
	 corresponding full-size integer mode is, and may also
	 specify a bit size.  */
      validate_mode (m, OPTIONAL, UNSET, SET, UNSET, UNSET);

      m->bytesize = m->component->bytesize;

      m->ncomponents = 1;
      break;

    case MODE_COMPLEX_INT:
    case MODE_COMPLEX_FLOAT:
      /* Complex modes should have a component indicated, but no more.  */
      validate_mode (m, UNSET, UNSET, SET, UNSET, UNSET);
      m->ncomponents = 2;
      if (m->component->precision != (unsigned int)-1)
	m->precision = 2 * m->component->precision;
      m->bytesize = 2 * m->component->bytesize;
      break;

    case MODE_VECTOR_BOOL:
      validate_mode (m, UNSET, SET, SET, SET, UNSET);
      break;

    case MODE_VECTOR_INT:
    case MODE_VECTOR_FLOAT:
    case MODE_VECTOR_FRACT:
    case MODE_VECTOR_UFRACT:
    case MODE_VECTOR_ACCUM:
    case MODE_VECTOR_UACCUM:
      /* Vector modes should have a component and a number of components.  */
      validate_mode (m, UNSET, UNSET, SET, SET, UNSET);
      if (m->component->precision != (unsigned int)-1)
	m->precision = m->ncomponents * m->component->precision;
      m->bytesize = m->ncomponents * m->component->bytesize;
      break;

    default:
      gcc_unreachable ();
    }

  /* If not already specified, the mode alignment defaults to the largest
     power of two that divides the size of the object.  Complex types are
     not more aligned than their contents.  */
  if (m->cl == MODE_COMPLEX_INT || m->cl == MODE_COMPLEX_FLOAT)
    alignment = m->component->bytesize;
  else
    alignment = m->bytesize;

  m->alignment = alignment & (~alignment + 1);

  /* If this mode has components, make the component mode point back
     to this mode, for the sake of adjustments.  */
  if (m->component)
    {
      m->next_cont = m->component->contained;
      m->component->contained = m;
    }
}

static void
complete_all_modes (void)
{
  struct mode_data *m;
  int cl;

  for_all_modes (cl, m)
    complete_mode (m);
}

/* For each mode in class CLASS, construct a corresponding complex mode.  */
#define COMPLEX_MODES(C) make_complex_modes (MODE_##C, __FILE__, __LINE__)
static void
make_complex_modes (enum mode_class cl,
		    const char *file, unsigned int line)
{
  struct mode_data *m;
  struct mode_data *c;
  enum mode_class cclass = complex_class (cl);

  if (cclass == MODE_RANDOM)
    return;

  for (m = modes[cl]; m; m = m->next)
    {
      char *p, *buf;
      size_t m_len;

      /* Skip BImode.  FIXME: BImode probably shouldn't be MODE_INT.  */
      if (m->boolean)
	continue;

      m_len = strlen (m->name);
      /* The leading "1 +" is in case we prepend a "C" below.  */
      buf = (char *) xmalloc (1 + m_len + 1);

      /* Float complex modes are named SCmode, etc.
	 Int complex modes are named CSImode, etc.
         This inconsistency should be eliminated.  */
      p = 0;
      if (cl == MODE_FLOAT)
	{
	  memcpy (buf, m->name, m_len + 1);
	  p = strchr (buf, 'F');
	  if (p == 0 && strchr (buf, 'D') == 0)
	    {
	      error ("%s:%d: float mode \"%s\" has no 'F' or 'D'",
		     m->file, m->line, m->name);
	      free (buf);
	      continue;
	    }
	}
      if (p != 0)
	*p = 'C';
      else
	{
	  buf[0] = 'C';
	  memcpy (buf + 1, m->name, m_len + 1);
	}

      c = new_mode (cclass, buf, file, line);
      c->component = m;
      m->complex = c;
    }
}

/* For all modes in class CL, construct vector modes of width WIDTH,
   having as many components as necessary.  ORDER is the sorting order
   of the mode, with smaller numbers indicating a higher priority.  */
#define VECTOR_MODES_WITH_PREFIX(PREFIX, C, W, ORDER) \
  make_vector_modes (MODE_##C, #PREFIX, W, ORDER, __FILE__, __LINE__)
#define VECTOR_MODES(C, W) VECTOR_MODES_WITH_PREFIX (V, C, W, 0)
static void ATTRIBUTE_UNUSED
make_vector_modes (enum mode_class cl, const char *prefix, unsigned int width,
		   unsigned int order, const char *file, unsigned int line)
{
  struct mode_data *m;
  struct mode_data *v;
  /* Big enough for a 32-bit UINT_MAX plus the text.  */
  char buf[12];
  unsigned int ncomponents;
  enum mode_class vclass = vector_class (cl);

  if (vclass == MODE_RANDOM)
    return;

  for (m = modes[cl]; m; m = m->next)
    {
      /* Do not construct vector modes with only one element, or
	 vector modes where the element size doesn't divide the full
	 size evenly.  */
      ncomponents = width / m->bytesize;
      if (ncomponents < 2)
	continue;
      if (width % m->bytesize)
	continue;

      /* Skip QFmode and BImode.  FIXME: this special case should
	 not be necessary.  */
      if (cl == MODE_FLOAT && m->bytesize == 1)
	continue;
      if (m->boolean)
	continue;

      if ((size_t) snprintf (buf, sizeof buf, "%s%u%s", prefix,
			     ncomponents, m->name) >= sizeof buf)
	{
	  error ("%s:%d: mode name \"%s\" is too long",
		 m->file, m->line, m->name);
	  continue;
	}

      v = new_mode (vclass, xstrdup (buf), file, line);
      v->order = order;
      v->component = m;
      v->ncomponents = ncomponents;
    }
}

/* Create a vector of booleans called NAME with COUNT elements and
   BYTESIZE bytes in total.  */
#define VECTOR_BOOL_MODE(NAME, COUNT, COMPONENT, BYTESIZE)		\
  make_vector_bool_mode (#NAME, COUNT, #COMPONENT, BYTESIZE,		\
			 __FILE__, __LINE__)
static void ATTRIBUTE_UNUSED
make_vector_bool_mode (const char *name, unsigned int count,
		       const char *component, unsigned int bytesize,
		       const char *file, unsigned int line)
{
  struct mode_data *m = find_mode (component);
  if (!m)
    {
      error ("%s:%d: no mode \"%s\"", file, line, component);
      return;
    }

  struct mode_data *v = new_mode (MODE_VECTOR_BOOL, name, file, line);
  v->component = m;
  v->ncomponents = count;
  v->bytesize = bytesize;
}

/* Input.  */

#define _SPECIAL_MODE(C, N) \
  make_special_mode (MODE_##C, #N, __FILE__, __LINE__)
#define RANDOM_MODE(N) _SPECIAL_MODE (RANDOM, N)
#define CC_MODE(N) _SPECIAL_MODE (CC, N)

static void
make_special_mode (enum mode_class cl, const char *name,
		   const char *file, unsigned int line)
{
  new_mode (cl, name, file, line);
}

#define INT_MODE(N, Y) FRACTIONAL_INT_MODE (N, -1U, Y)
#define FRACTIONAL_INT_MODE(N, B, Y) \
  make_int_mode (#N, B, Y, __FILE__, __LINE__)

static void
make_int_mode (const char *name,
	       unsigned int precision, unsigned int bytesize,
	       const char *file, unsigned int line)
{
  struct mode_data *m = new_mode (MODE_INT, name, file, line);
  m->bytesize = bytesize;
  m->precision = precision;
}

#define BOOL_MODE(N, B, Y) \
  make_bool_mode (#N, B, Y, __FILE__, __LINE__)

static void
make_bool_mode (const char *name,
		unsigned int precision, unsigned int bytesize,
		const char *file, unsigned int line)
{
  struct mode_data *m = new_mode (MODE_INT, name, file, line);
  m->bytesize = bytesize;
  m->precision = precision;
  m->boolean = true;
}

#define OPAQUE_MODE(N, B)			\
  make_opaque_mode (#N, -1U, B, __FILE__, __LINE__)

static void ATTRIBUTE_UNUSED
make_opaque_mode (const char *name,
		  unsigned int precision,
		  unsigned int bytesize,
		  const char *file, unsigned int line)
{
  struct mode_data *m = new_mode (MODE_OPAQUE, name, file, line);
  m->bytesize = bytesize;
  m->precision = precision;
}

#define FRACT_MODE(N, Y, F) \
	make_fixed_point_mode (MODE_FRACT, #N, Y, 0, F, __FILE__, __LINE__)

#define UFRACT_MODE(N, Y, F) \
	make_fixed_point_mode (MODE_UFRACT, #N, Y, 0, F, __FILE__, __LINE__)

#define ACCUM_MODE(N, Y, I, F) \
	make_fixed_point_mode (MODE_ACCUM, #N, Y, I, F, __FILE__, __LINE__)

#define UACCUM_MODE(N, Y, I, F) \
	make_fixed_point_mode (MODE_UACCUM, #N, Y, I, F, __FILE__, __LINE__)

/* Create a fixed-point mode by setting CL, NAME, BYTESIZE, IBIT, FBIT,
   FILE, and LINE.  */

static void
make_fixed_point_mode (enum mode_class cl,
		       const char *name,
		       unsigned int bytesize,
		       unsigned int ibit,
		       unsigned int fbit,
		       const char *file, unsigned int line)
{
  struct mode_data *m = new_mode (cl, name, file, line);
  m->bytesize = bytesize;
  m->ibit = ibit;
  m->fbit = fbit;
}

#define FLOAT_MODE(N, Y, F)             FRACTIONAL_FLOAT_MODE (N, -1U, Y, F)
#define FRACTIONAL_FLOAT_MODE(N, B, Y, F) \
  make_float_mode (#N, B, Y, #F, __FILE__, __LINE__)

static void
make_float_mode (const char *name,
		 unsigned int precision, unsigned int bytesize,
		 const char *format,
		 const char *file, unsigned int line)
{
  struct mode_data *m = new_mode (MODE_FLOAT, name, file, line);
  m->bytesize = bytesize;
  m->precision = precision;
  m->format = format;
}

#define DECIMAL_FLOAT_MODE(N, Y, F)	\
	FRACTIONAL_DECIMAL_FLOAT_MODE (N, -1U, Y, F)
#define FRACTIONAL_DECIMAL_FLOAT_MODE(N, B, Y, F)	\
  make_decimal_float_mode (#N, B, Y, #F, __FILE__, __LINE__)

static void
make_decimal_float_mode (const char *name,
			 unsigned int precision, unsigned int bytesize,
			 const char *format,
			 const char *file, unsigned int line)
{
  struct mode_data *m = new_mode (MODE_DECIMAL_FLOAT, name, file, line);
  m->bytesize = bytesize;
  m->precision = precision;
  m->format = format;
}

#define RESET_FLOAT_FORMAT(N, F) \
  reset_float_format (#N, #F, __FILE__, __LINE__)
static void ATTRIBUTE_UNUSED
reset_float_format (const char *name, const char *format,
		    const char *file, unsigned int line)
{
  struct mode_data *m = find_mode (name);
  if (!m)
    {
      error ("%s:%d: no mode \"%s\"", file, line, name);
      return;
    }
  if (m->cl != MODE_FLOAT && m->cl != MODE_DECIMAL_FLOAT)
    {
      error ("%s:%d: mode \"%s\" is not a FLOAT class", file, line, name);
      return;
    }
  m->format = format;
}

/* __intN support.  */
#define INT_N(M,PREC)				\
  make_int_n (#M, PREC, __FILE__, __LINE__)
static void ATTRIBUTE_UNUSED
make_int_n (const char *m, int bitsize,
            const char *file, unsigned int line)
{
  struct mode_data *component = find_mode (m);
  if (!component)
    {
      error ("%s:%d: no mode \"%s\"", file, line, m);
      return;
    }
  if (component->cl != MODE_INT
      && component->cl != MODE_PARTIAL_INT)
    {
      error ("%s:%d: mode \"%s\" is not class INT or PARTIAL_INT", file, line, m);
      return;
    }
  if (component->int_n != 0)
    {
      error ("%s:%d: mode \"%s\" already has an intN", file, line, m);
      return;
    }

  component->int_n = bitsize;
}

/* Partial integer modes are specified by relation to a full integer
   mode.  */
#define PARTIAL_INT_MODE(M,PREC,NAME)				\
  make_partial_integer_mode (#M, #NAME, PREC, __FILE__, __LINE__)
static void ATTRIBUTE_UNUSED
make_partial_integer_mode (const char *base, const char *name,
			   unsigned int precision,
			   const char *file, unsigned int line)
{
  struct mode_data *m;
  struct mode_data *component = find_mode (base);
  if (!component)
    {
      error ("%s:%d: no mode \"%s\"", file, line, name);
      return;
    }
  if (component->cl != MODE_INT)
    {
      error ("%s:%d: mode \"%s\" is not class INT", file, line, name);
      return;
    }

  m = new_mode (MODE_PARTIAL_INT, name, file, line);
  m->precision = precision;
  m->component = component;
}

/* A single vector mode can be specified by naming its component
   mode and the number of components.  */
#define VECTOR_MODE_WITH_PREFIX(PREFIX, C, M, N, ORDER) \
  make_vector_mode (MODE_##C, #PREFIX, #M, N, ORDER, __FILE__, __LINE__);
#define VECTOR_MODE(C, M, N) VECTOR_MODE_WITH_PREFIX(V, C, M, N, 0);
static void ATTRIBUTE_UNUSED
make_vector_mode (enum mode_class bclass,
		  const char *prefix,
		  const char *base,
		  unsigned int ncomponents,
		  unsigned int order,
		  const char *file, unsigned int line)
{
  struct mode_data *v;
  enum mode_class vclass = vector_class (bclass);
  struct mode_data *component = find_mode (base);
  char namebuf[16];

  if (vclass == MODE_RANDOM)
    return;
  if (component == 0)
    {
      error ("%s:%d: no mode \"%s\"", file, line, base);
      return;
    }
  if (component->cl != bclass
      && (component->cl != MODE_PARTIAL_INT
	  || bclass != MODE_INT))
    {
      error ("%s:%d: mode \"%s\" is not class %s",
	     file, line, base, mode_class_names[bclass] + 5);
      return;
    }

  if ((size_t)snprintf (namebuf, sizeof namebuf, "%s%u%s", prefix,
			ncomponents, base) >= sizeof namebuf)
    {
      error ("%s:%d: mode name \"%s\" is too long",
	     file, line, base);
      return;
    }

  v = new_mode (vclass, xstrdup (namebuf), file, line);
  v->order = order;
  v->ncomponents = ncomponents;
  v->component = component;
}

/* Adjustability.  */
#define _ADD_ADJUST(A, M, X, C1, C2) \
  new_adjust (#M, &adj_##A, #A, #X, MODE_##C1, MODE_##C2, __FILE__, __LINE__)

#define ADJUST_NUNITS(M, X)    _ADD_ADJUST (nunits, M, X, RANDOM, RANDOM)
#define ADJUST_BYTESIZE(M, X)  _ADD_ADJUST (bytesize, M, X, RANDOM, RANDOM)
#define ADJUST_ALIGNMENT(M, X) _ADD_ADJUST (alignment, M, X, RANDOM, RANDOM)
#define ADJUST_PRECISION(M, X) _ADD_ADJUST (precision, M, X, RANDOM, RANDOM)
#define ADJUST_FLOAT_FORMAT(M, X)    _ADD_ADJUST (format, M, X, FLOAT, FLOAT)
#define ADJUST_IBIT(M, X)  _ADD_ADJUST (ibit, M, X, ACCUM, UACCUM)
#define ADJUST_FBIT(M, X)  _ADD_ADJUST (fbit, M, X, FRACT, UACCUM)

static int bits_per_unit;
static int max_bitsize_mode_any_int;
static int max_bitsize_mode_any_mode;

static void
create_modes (void)
{
#include "machmode.def"

  /* So put the default value unless the target needs a non standard
     value. */
#ifdef BITS_PER_UNIT
  bits_per_unit = BITS_PER_UNIT;
#else
  bits_per_unit = 8;
#endif

#ifdef MAX_BITSIZE_MODE_ANY_INT
  max_bitsize_mode_any_int = MAX_BITSIZE_MODE_ANY_INT;
#else
  max_bitsize_mode_any_int = 0;
#endif

#ifdef MAX_BITSIZE_MODE_ANY_MODE
  max_bitsize_mode_any_mode = MAX_BITSIZE_MODE_ANY_MODE;
#else
  max_bitsize_mode_any_mode = 0;
#endif
}

#ifndef NUM_POLY_INT_COEFFS
#define NUM_POLY_INT_COEFFS 1
#endif

/* Processing.  */

/* Sort a list of modes into the order needed for the WIDER field:
   major sort by precision, minor sort by component precision.

   For instance:
     QI < HI < SI < DI < TI
     V4QI < V2HI < V8QI < V4HI < V2SI.

   If the precision is not set, sort by the bytesize.  A mode with
   precision set gets sorted before a mode without precision set, if
   they have the same bytesize; this is the right thing because
   the precision must always be smaller than the bytesize * BITS_PER_UNIT.
   We don't have to do anything special to get this done -- an unset
   precision shows up as (unsigned int)-1, i.e. UINT_MAX.  */
static int
cmp_modes (const void *a, const void *b)
{
  const struct mode_data *const m = *(const struct mode_data *const*)a;
  const struct mode_data *const n = *(const struct mode_data *const*)b;

  if (m->order > n->order)
    return 1;
  else if (m->order < n->order)
    return -1;

  if (m->bytesize > n->bytesize)
    return 1;
  else if (m->bytesize < n->bytesize)
    return -1;

  if (m->precision > n->precision)
    return 1;
  else if (m->precision < n->precision)
    return -1;

  if (!m->component && !n->component)
    {
      if (m->counter < n->counter)
	return -1;
      else
	return 1;
    }

  if (m->component->bytesize > n->component->bytesize)
    return 1;
  else if (m->component->bytesize < n->component->bytesize)
    return -1;

  if (m->component->precision > n->component->precision)
    return 1;
  else if (m->component->precision < n->component->precision)
    return -1;

  if (m->counter < n->counter)
    return -1;
  else
    return 1;
}

static void
calc_wider_mode (void)
{
  int c;
  struct mode_data *m;
  struct mode_data **sortbuf;
  unsigned int max_n_modes = 0;
  unsigned int i, j;

  for (c = 0; c < MAX_MODE_CLASS; c++)
    max_n_modes = MAX (max_n_modes, n_modes[c]);

  /* Allocate max_n_modes + 1 entries to leave room for the extra null
     pointer assigned after the qsort call below.  */
  sortbuf = XALLOCAVEC (struct mode_data *, max_n_modes + 1);

  for (c = 0; c < MAX_MODE_CLASS; c++)
    {
      /* "wider" is not meaningful for MODE_RANDOM and MODE_CC.
	 However, we want these in textual order, and we have
	 precisely the reverse.  */
      if (c == MODE_RANDOM || c == MODE_CC)
	{
	  struct mode_data *prev, *next;

	  for (prev = 0, m = modes[c]; m; m = next)
	    {
	      m->wider = void_mode;

	      /* this is nreverse */
	      next = m->next;
	      m->next = prev;
	      prev = m;
	    }
	  modes[c] = prev;
	}
      else
	{
	  if (!modes[c])
	    continue;

	  for (i = 0, m = modes[c]; m; i++, m = m->next)
	    sortbuf[i] = m;

	  (qsort) (sortbuf, i, sizeof (struct mode_data *), cmp_modes);

	  sortbuf[i] = 0;
	  for (j = 0; j < i; j++)
	    {
	      sortbuf[j]->next = sortbuf[j + 1];
	      if (c == MODE_PARTIAL_INT)
		sortbuf[j]->wider = sortbuf[j]->component;
	      else
		sortbuf[j]->wider = sortbuf[j]->next;
	    }

	  modes[c] = sortbuf[0];
	}
    }
}

/* Text to add to the constant part of a poly_int initializer in
   order to fill out te whole structure.  */
#if NUM_POLY_INT_COEFFS == 1
#define ZERO_COEFFS ""
#elif NUM_POLY_INT_COEFFS == 2
#define ZERO_COEFFS ", 0"
#else
#error "Unknown value of NUM_POLY_INT_COEFFS"
#endif

/* Output routines.  */

#define tagged_printf(FMT, ARG, TAG) do {		\
  int count_ = printf ("  " FMT ",", ARG);		\
  printf ("%*s/* %s */\n", 27 - count_, "", TAG);	\
} while (0)

#define print_decl(TYPE, NAME, ASIZE) \
  puts ("\nconst " TYPE " " NAME "[" ASIZE "] =\n{");

#define print_maybe_const_decl(TYPE, NAME, ASIZE, NEEDS_ADJ)	\
  printf ("\n" TYPE " " NAME "[" ASIZE "] = \n{\n",		\
	  NEEDS_ADJ ? "" : "const ")

#define print_closer() puts ("};")

/* Compute the max bitsize of some of the classes of integers.  It may
   be that there are needs for the other integer classes, and this
   code is easy to extend.  */
static void
emit_max_int (void)
{
  unsigned int max, mmax;
  struct mode_data *i;
  int j;

  puts ("");

  printf ("#define BITS_PER_UNIT (%d)\n", bits_per_unit); 
 
  if (max_bitsize_mode_any_int == 0)
    {
      for (max = 1, i = modes[MODE_INT]; i; i = i->next)
	if (max < i->bytesize)
	  max = i->bytesize;
      mmax = max;
      for (max = 1, i = modes[MODE_PARTIAL_INT]; i; i = i->next)
	if (max < i->bytesize)
	  max = i->bytesize;
      if (max > mmax)
	mmax = max;
      printf ("#define MAX_BITSIZE_MODE_ANY_INT (%d*BITS_PER_UNIT)\n", mmax);
    }
  else
    printf ("#define MAX_BITSIZE_MODE_ANY_INT %d\n", max_bitsize_mode_any_int);

  if (max_bitsize_mode_any_mode == 0)
    {
      mmax = 0;
      for (j = 0; j < MAX_MODE_CLASS; j++)
	for (i = modes[j]; i; i = i->next)
	  if (mmax < i->bytesize)
	    mmax = i->bytesize;
      printf ("#define MAX_BITSIZE_MODE_ANY_MODE (%d*BITS_PER_UNIT)\n", mmax);
    }
  else
    printf ("#define MAX_BITSIZE_MODE_ANY_MODE %d\n",
	    max_bitsize_mode_any_mode);
}

/* Emit mode_size_inline routine into insn-modes.h header.  */
static void
emit_mode_size_inline (void)
{
  int c;
  struct mode_adjust *a;
  struct mode_data *m;

  /* Size adjustments must be propagated to all containing modes.  */
  for (a = adj_bytesize; a; a = a->next)
    {
      a->mode->need_bytesize_adj = true;
      for (m = a->mode->contained; m; m = m->next_cont)
	m->need_bytesize_adj = true;
    }

  /* Changing the number of units by a factor of X also changes the size
     by a factor of X.  */
  for (mode_adjust *a = adj_nunits; a; a = a->next)
    a->mode->need_bytesize_adj = true;

  printf ("\
#ifdef __cplusplus\n\
inline __attribute__((__always_inline__))\n\
#else\n\
extern __inline__ __attribute__((__always_inline__, __gnu_inline__))\n\
#endif\n\
poly_uint16\n\
mode_size_inline (machine_mode mode)\n\
{\n\
  extern %spoly_uint16 mode_size[NUM_MACHINE_MODES];\n\
  gcc_assert (mode >= 0 && mode < NUM_MACHINE_MODES);\n\
  switch (mode)\n\
    {\n", adj_nunits || adj_bytesize ? "" : "const ");

  for_all_modes (c, m)
    if (!m->need_bytesize_adj)
      printf ("    case E_%smode: return %u;\n", m->name, m->bytesize);

  puts ("\
    default: return mode_size[mode];\n\
    }\n\
}\n");
}

/* Emit mode_nunits_inline routine into insn-modes.h header.  */
static void
emit_mode_nunits_inline (void)
{
  int c;
  struct mode_data *m;

  for (mode_adjust *a = adj_nunits; a; a = a->next)
    a->mode->need_nunits_adj = true;

  printf ("\
#ifdef __cplusplus\n\
inline __attribute__((__always_inline__))\n\
#else\n\
extern __inline__ __attribute__((__always_inline__, __gnu_inline__))\n\
#endif\n\
poly_uint16\n\
mode_nunits_inline (machine_mode mode)\n\
{\n\
  extern %spoly_uint16 mode_nunits[NUM_MACHINE_MODES];\n\
  switch (mode)\n\
    {\n", adj_nunits ? "" : "const ");

  for_all_modes (c, m)
    if (!m->need_nunits_adj)
      printf ("    case E_%smode: return %u;\n", m->name, m->ncomponents);

  puts ("\
    default: return mode_nunits[mode];\n\
    }\n\
}\n");
}

/* Emit mode_inner_inline routine into insn-modes.h header.  */
static void
emit_mode_inner_inline (void)
{
  int c;
  struct mode_data *m;

  puts ("\
#ifdef __cplusplus\n\
inline __attribute__((__always_inline__))\n\
#else\n\
extern __inline__ __attribute__((__always_inline__, __gnu_inline__))\n\
#endif\n\
unsigned short\n\
mode_inner_inline (machine_mode mode)\n\
{\n\
  extern const unsigned short mode_inner[NUM_MACHINE_MODES];\n\
  gcc_assert (mode >= 0 && mode < NUM_MACHINE_MODES);\n\
  switch (mode)\n\
    {");

  for_all_modes (c, m)
    printf ("    case E_%smode: return E_%smode;\n", m->name,
	    c != MODE_PARTIAL_INT && m->component
	    ? m->component->name : m->name);

  puts ("\
    default: return mode_inner[mode];\n\
    }\n\
}\n");
}

/* Emit mode_unit_size_inline routine into insn-modes.h header.  */
static void
emit_mode_unit_size_inline (void)
{
  int c;
  struct mode_data *m;

  puts ("\
#ifdef __cplusplus\n\
inline __attribute__((__always_inline__))\n\
#else\n\
extern __inline__ __attribute__((__always_inline__, __gnu_inline__))\n\
#endif\n\
unsigned char\n\
mode_unit_size_inline (machine_mode mode)\n\
{\n\
  extern CONST_MODE_UNIT_SIZE unsigned char mode_unit_size[NUM_MACHINE_MODES];\
\n\
  gcc_assert (mode >= 0 && mode < NUM_MACHINE_MODES);\n\
  switch (mode)\n\
    {");

  for_all_modes (c, m)
    {
      const char *name = m->name;
      struct mode_data *m2 = m;
      if (c != MODE_PARTIAL_INT && m2->component)
	m2 = m2->component;
      if (!m2->need_bytesize_adj)
	printf ("    case E_%smode: return %u;\n", name, m2->bytesize);
    }

  puts ("\
    default: return mode_unit_size[mode];\n\
    }\n\
}\n");
}

/* Emit mode_unit_precision_inline routine into insn-modes.h header.  */
static void
emit_mode_unit_precision_inline (void)
{
  int c;
  struct mode_data *m;

  puts ("\
#ifdef __cplusplus\n\
inline __attribute__((__always_inline__))\n\
#else\n\
extern __inline__ __attribute__((__always_inline__, __gnu_inline__))\n\
#endif\n\
unsigned short\n\
mode_unit_precision_inline (machine_mode mode)\n\
{\n\
  extern const unsigned short mode_unit_precision[NUM_MACHINE_MODES];\n\
  gcc_assert (mode >= 0 && mode < NUM_MACHINE_MODES);\n\
  switch (mode)\n\
    {");

  for_all_modes (c, m)
    {
      struct mode_data *m2
	= (c != MODE_PARTIAL_INT && m->component) ? m->component : m;
      if (m2->precision != (unsigned int)-1)
	printf ("    case E_%smode: return %u;\n", m->name, m2->precision);
      else
	printf ("    case E_%smode: return %u*BITS_PER_UNIT;\n",
		m->name, m2->bytesize);
    }

  puts ("\
    default: return mode_unit_precision[mode];\n\
    }\n\
}\n");
}

/* Return the best machine mode class for MODE, or null if machine_mode
   should be used.  */

static const char *
get_mode_class (struct mode_data *mode)
{
  switch (mode->cl)
    {
    case MODE_INT:
    case MODE_PARTIAL_INT:
      return "scalar_int_mode";

    case MODE_FRACT:
    case MODE_UFRACT:
    case MODE_ACCUM:
    case MODE_UACCUM:
      return "scalar_mode";

    case MODE_FLOAT:
    case MODE_DECIMAL_FLOAT:
      return "scalar_float_mode";

    case MODE_COMPLEX_INT:
    case MODE_COMPLEX_FLOAT:
      return "complex_mode";

    default:
      return NULL;
    }
}

static void
emit_insn_modes_h (void)
{
  int c;
  struct mode_data *m, *first, *last;
  int n_int_n_ents = 0;

  printf ("/* Generated automatically from machmode.def%s%s\n",
	   HAVE_EXTRA_MODES ? " and " : "",
	   EXTRA_MODES_FILE);

  puts ("\
   by genmodes.  */\n\
\n\
#ifndef GCC_INSN_MODES_H\n\
#define GCC_INSN_MODES_H\n\
\n\
enum machine_mode\n{");

  for (c = 0; c < MAX_MODE_CLASS; c++)
    for (m = modes[c]; m; m = m->next)
      {
	int count_ = printf ("  E_%smode,", m->name);
	printf ("%*s/* %s:%d */\n", 27 - count_, "",
		 trim_filename (m->file), m->line);
	printf ("#define HAVE_%smode\n", m->name);
	printf ("#ifdef USE_ENUM_MODES\n");
	printf ("#define %smode E_%smode\n", m->name, m->name);
	printf ("#else\n");
	if (const char *mode_class = get_mode_class (m))
	  printf ("#define %smode (%s ((%s::from_int) E_%smode))\n",
		  m->name, mode_class, mode_class, m->name);
	else
	  printf ("#define %smode ((void) 0, E_%smode)\n",
		  m->name, m->name);
	printf ("#endif\n");
      }

  puts ("  MAX_MACHINE_MODE,\n");

  for (c = 0; c < MAX_MODE_CLASS; c++)
    {
      first = modes[c];
      last = 0;
      for (m = first; m; last = m, m = m->next)
	;

      /* Don't use BImode for MIN_MODE_INT, since otherwise the middle
	 end will try to use it for bitfields in structures and the
	 like, which we do not want.  Only the target md file should
	 generate BImode widgets.  Since some targets such as ARM/MVE
	 define boolean modes with multiple bits, handle those too.  */
      if (first && first->boolean)
	{
	  struct mode_data *last_bool = first;
	  printf ("  MIN_MODE_BOOL = E_%smode,\n", first->name);

	  while (first && first->boolean)
	    {
	      last_bool = first;
	      first = first->next;
	    }

	  printf ("  MAX_MODE_BOOL = E_%smode,\n\n", last_bool->name);
	}

      if (first && last)
	printf ("  MIN_%s = E_%smode,\n  MAX_%s = E_%smode,\n\n",
		 mode_class_names[c], first->name,
		 mode_class_names[c], last->name);
      else
	printf ("  MIN_%s = E_%smode,\n  MAX_%s = E_%smode,\n\n",
		 mode_class_names[c], void_mode->name,
		 mode_class_names[c], void_mode->name);
    }

  puts ("\
  NUM_MACHINE_MODES = MAX_MACHINE_MODE\n\
};\n");

  /* Define a NUM_* macro for each mode class, giving the number of modes
     in the class.  */
  for (c = 0; c < MAX_MODE_CLASS; c++)
    {
      printf ("#define NUM_%s ", mode_class_names[c]);
      if (modes[c])
	printf ("(MAX_%s - MIN_%s + 1)\n", mode_class_names[c],
		mode_class_names[c]);
      else
	printf ("0\n");
    }
  printf ("\n");

  /* I can't think of a better idea, can you?  */
  printf ("#define CONST_MODE_NUNITS%s\n", adj_nunits ? "" : " const");
  printf ("#define CONST_MODE_PRECISION%s\n", adj_nunits ? "" : " const");
  printf ("#define CONST_MODE_SIZE%s\n",
	  adj_bytesize || adj_nunits ? "" : " const");
  printf ("#define CONST_MODE_UNIT_SIZE%s\n", adj_bytesize ? "" : " const");
  printf ("#define CONST_MODE_BASE_ALIGN%s\n", adj_alignment ? "" : " const");
#if 0 /* disabled for backward compatibility, temporary */
  printf ("#define CONST_REAL_FORMAT_FOR_MODE%s\n", adj_format ? "" :" const");
#endif
  printf ("#define CONST_MODE_IBIT%s\n", adj_ibit ? "" : " const");
  printf ("#define CONST_MODE_FBIT%s\n", adj_fbit ? "" : " const");
  printf ("#define CONST_MODE_MASK%s\n", adj_nunits ? "" : " const");
  emit_max_int ();

  for_all_modes (c, m)
    if (m->int_n)
      n_int_n_ents ++;

  printf ("#define NUM_INT_N_ENTS %d\n", n_int_n_ents);

  printf ("#define NUM_POLY_INT_COEFFS %d\n", NUM_POLY_INT_COEFFS);

  puts ("\
\n\
#endif /* insn-modes.h */");
}

static void
emit_insn_modes_inline_h (void)
{
  printf ("/* Generated automatically from machmode.def%s%s\n",
	   HAVE_EXTRA_MODES ? " and " : "",
	   EXTRA_MODES_FILE);

  puts ("\
   by genmodes.  */\n\
\n\
#ifndef GCC_INSN_MODES_INLINE_H\n\
#define GCC_INSN_MODES_INLINE_H");

  puts ("\n#if !defined (USED_FOR_TARGET) && GCC_VERSION >= 4001\n");
  emit_mode_size_inline ();
  emit_mode_nunits_inline ();
  emit_mode_inner_inline ();
  emit_mode_unit_size_inline ();
  emit_mode_unit_precision_inline ();
  puts ("#endif /* GCC_VERSION >= 4001 */");

  puts ("\
\n\
#endif /* insn-modes-inline.h */");
}

static void
emit_insn_modes_c_header (void)
{
  printf ("/* Generated automatically from machmode.def%s%s\n",
	   HAVE_EXTRA_MODES ? " and " : "",
	   EXTRA_MODES_FILE);

  puts ("\
   by genmodes.  */\n\
\n\
#include \"config.h\"\n\
#include \"system.h\"\n\
#include \"coretypes.h\"\n\
#include \"tm.h\"\n\
#include \"real.h\"");
}

static void
emit_min_insn_modes_c_header (void)
{
  printf ("/* Generated automatically from machmode.def%s%s\n",
	   HAVE_EXTRA_MODES ? " and " : "",
	   EXTRA_MODES_FILE);

  puts ("\
   by genmodes.  */\n\
\n\
#include \"bconfig.h\"\n\
#include \"system.h\"\n\
#include \"coretypes.h\"");
}

static void
emit_mode_name (void)
{
  int c;
  struct mode_data *m;

  print_decl ("char *const", "mode_name", "NUM_MACHINE_MODES");

  for_all_modes (c, m)
    printf ("  \"%s\",\n", m->name);

  print_closer ();
}

static void
emit_mode_class (void)
{
  int c;
  struct mode_data *m;

  print_decl ("unsigned char", "mode_class", "NUM_MACHINE_MODES");

  for_all_modes (c, m)
    tagged_printf ("%s", mode_class_names[m->cl], m->name);

  print_closer ();
}

static void
emit_mode_precision (void)
{
  int c;
  struct mode_data *m;

  print_maybe_const_decl ("%spoly_uint16", "mode_precision",
			  "NUM_MACHINE_MODES", adj_nunits);

  for_all_modes (c, m)
    if (m->precision != (unsigned int)-1)
      tagged_printf ("{ %u" ZERO_COEFFS " }", m->precision, m->name);
    else
      tagged_printf ("{ %u * BITS_PER_UNIT" ZERO_COEFFS " }",
		     m->bytesize, m->name);

  print_closer ();
}

static void
emit_mode_size (void)
{
  int c;
  struct mode_data *m;

  print_maybe_const_decl ("%spoly_uint16", "mode_size",
			  "NUM_MACHINE_MODES", adj_nunits || adj_bytesize);

  for_all_modes (c, m)
    tagged_printf ("{ %u" ZERO_COEFFS " }", m->bytesize, m->name);

  print_closer ();
}

static void
emit_mode_nunits (void)
{
  int c;
  struct mode_data *m;

  print_maybe_const_decl ("%spoly_uint16", "mode_nunits",
			  "NUM_MACHINE_MODES", adj_nunits);

  for_all_modes (c, m)
    tagged_printf ("{ %u" ZERO_COEFFS " }", m->ncomponents, m->name);

  print_closer ();
}

static void
emit_mode_wider (void)
{
  int c;
  struct mode_data *m;

  print_decl ("unsigned short", "mode_next", "NUM_MACHINE_MODES");

  for_all_modes (c, m)
    tagged_printf ("E_%smode",
		   m->wider ? m->wider->name : void_mode->name,
		   m->name);

  print_closer ();
  print_decl ("unsigned short", "mode_wider", "NUM_MACHINE_MODES");

  for_all_modes (c, m)
    {
      struct mode_data *m2 = 0;

      if (m->cl == MODE_INT
	  || m->cl == MODE_PARTIAL_INT
	  || m->cl == MODE_FLOAT
	  || m->cl == MODE_DECIMAL_FLOAT
	  || m->cl == MODE_COMPLEX_FLOAT
	  || m->cl == MODE_FRACT
	  || m->cl == MODE_UFRACT
	  || m->cl == MODE_ACCUM
	  || m->cl == MODE_UACCUM)
	for (m2 = m->wider; m2 && m2 != void_mode; m2 = m2->wider)
	  {
	    if (m2->bytesize == m->bytesize
		&& m2->precision == m->precision)
	      continue;
	    break;
	  }

      if (m2 == void_mode)
	m2 = 0;
      tagged_printf ("E_%smode",
		     m2 ? m2->name : void_mode->name,
		     m->name);
    }

  print_closer ();
  print_decl ("unsigned short", "mode_2xwider", "NUM_MACHINE_MODES");

  for_all_modes (c, m)
    {
      struct mode_data * m2;

      for (m2 = m;
	   m2 && m2 != void_mode;
	   m2 = m2->wider)
	{
	  if (m2->bytesize < 2 * m->bytesize)
	    continue;
	  if (m->precision != (unsigned int) -1)
	    {
	      if (m2->precision != 2 * m->precision)
		continue;
	    }
	  else
	    {
	      if (m2->precision != (unsigned int) -1)
		continue;
	    }

	  /* For vectors we want twice the number of components,
	     with the same element type.  */
	  if (m->cl == MODE_VECTOR_BOOL
	      || m->cl == MODE_VECTOR_INT
	      || m->cl == MODE_VECTOR_FLOAT
	      || m->cl == MODE_VECTOR_FRACT
	      || m->cl == MODE_VECTOR_UFRACT
	      || m->cl == MODE_VECTOR_ACCUM
	      || m->cl == MODE_VECTOR_UACCUM)
	    {
	      if (m2->ncomponents != 2 * m->ncomponents)
		continue;
	      if (m->component != m2->component)
		continue;
	    }

	  break;
	}
      if (m2 == void_mode)
	m2 = 0;
      tagged_printf ("E_%smode",
		     m2 ? m2->name : void_mode->name,
		     m->name);
    }

  print_closer ();
}

static void
emit_mode_complex (void)
{
  int c;
  struct mode_data *m;

  print_decl ("unsigned short", "mode_complex", "NUM_MACHINE_MODES");

  for_all_modes (c, m)
    tagged_printf ("E_%smode",
		   m->complex ? m->complex->name : void_mode->name,
		   m->name);

  print_closer ();
}

static void
emit_mode_mask (void)
{
  int c;
  struct mode_data *m;

  print_maybe_const_decl ("%sunsigned HOST_WIDE_INT", "mode_mask_array",
			  "NUM_MACHINE_MODES", adj_nunits);
  puts ("\
#define MODE_MASK(m)                          \\\n\
  ((m) >= HOST_BITS_PER_WIDE_INT)             \\\n\
   ? HOST_WIDE_INT_M1U                        \\\n\
   : (HOST_WIDE_INT_1U << (m)) - 1\n");

  for_all_modes (c, m)
    if (m->precision != (unsigned int)-1)
      tagged_printf ("MODE_MASK (%u)", m->precision, m->name);
    else
      tagged_printf ("MODE_MASK (%u*BITS_PER_UNIT)", m->bytesize, m->name);

  puts ("#undef MODE_MASK");
  print_closer ();
}

static void
emit_mode_inner (void)
{
  int c;
  struct mode_data *m;

  print_decl ("unsigned short", "mode_inner", "NUM_MACHINE_MODES");

  for_all_modes (c, m)
    tagged_printf ("E_%smode",
		   c != MODE_PARTIAL_INT && m->component
		   ? m->component->name : m->name,
		   m->name);

  print_closer ();
}

/* Emit mode_unit_size array into insn-modes.cc file.  */
static void
emit_mode_unit_size (void)
{
  int c;
  struct mode_data *m;

  print_maybe_const_decl ("%sunsigned char", "mode_unit_size",
			  "NUM_MACHINE_MODES", adj_bytesize);

  for_all_modes (c, m)
    tagged_printf ("%u",
		   c != MODE_PARTIAL_INT && m->component
		   ? m->component->bytesize : m->bytesize, m->name);

  print_closer ();
}

/* Emit mode_unit_precision array into insn-modes.cc file.  */
static void
emit_mode_unit_precision (void)
{
  int c;
  struct mode_data *m;

  print_decl ("unsigned short", "mode_unit_precision", "NUM_MACHINE_MODES");

  for_all_modes (c, m)
    {
      struct mode_data *m2 = (c != MODE_PARTIAL_INT && m->component) ?
			     m->component : m;
      if (m2->precision != (unsigned int)-1)
	tagged_printf ("%u", m2->precision, m->name);
      else
	tagged_printf ("%u*BITS_PER_UNIT", m2->bytesize, m->name);
    }

  print_closer ();
}


static void
emit_mode_base_align (void)
{
  int c;
  struct mode_data *m;

  print_maybe_const_decl ("%sunsigned short",
			  "mode_base_align", "NUM_MACHINE_MODES",
			  adj_alignment);

  for_all_modes (c, m)
    tagged_printf ("%u", m->alignment, m->name);

  print_closer ();
}

static void
emit_class_narrowest_mode (void)
{
  int c;

  print_decl ("unsigned short", "class_narrowest_mode", "MAX_MODE_CLASS");

  for (c = 0; c < MAX_MODE_CLASS; c++)
    {
      /* Bleah, all this to get the comment right for MIN_MODE_INT.  */
      struct mode_data *m = modes[c];
      while (m && m->boolean)
	m = m->next;
      const char *comment_name = (m ? m : void_mode)->name;

      tagged_printf ("MIN_%s", mode_class_names[c], comment_name);
    }

  print_closer ();
}

static void
emit_real_format_for_mode (void)
{
  struct mode_data *m;

  /* The entities pointed to by this table are constant, whether
     or not the table itself is constant.

     For backward compatibility this table is always writable
     (several targets modify it in TARGET_OPTION_OVERRIDE).   FIXME:
     convert all said targets to use ADJUST_FORMAT instead.  */
#if 0
  print_maybe_const_decl ("const struct real_format *%s",
			  "real_format_for_mode",
			  "MAX_MODE_FLOAT - MIN_MODE_FLOAT + 1",
			  format);
#else
  print_decl ("struct real_format *\n", "real_format_for_mode",
	      "MAX_MODE_FLOAT - MIN_MODE_FLOAT + 1 "
	      "+ MAX_MODE_DECIMAL_FLOAT - MIN_MODE_DECIMAL_FLOAT + 1");
#endif

  /* The beginning of the table is entries for float modes.  */
  for (m = modes[MODE_FLOAT]; m; m = m->next)
    if (!strcmp (m->format, "0"))
      tagged_printf ("%s", m->format, m->name);
    else
      tagged_printf ("&%s", m->format, m->name);

  /* The end of the table is entries for decimal float modes.  */
  for (m = modes[MODE_DECIMAL_FLOAT]; m; m = m->next)
    if (!strcmp (m->format, "0"))
      tagged_printf ("%s", m->format, m->name);
    else
      tagged_printf ("&%s", m->format, m->name);

  print_closer ();
}

static void
emit_mode_adjustments (void)
{
  int c;
  struct mode_adjust *a;
  struct mode_data *m;

  if (adj_nunits)
    printf ("\n"
	    "void\n"
	    "adjust_mode_mask (machine_mode mode)\n"
	    "{\n"
	    "  unsigned int precision;\n"
	    "  if (GET_MODE_PRECISION (mode).is_constant (&precision)\n"
	    "      && precision < HOST_BITS_PER_WIDE_INT)\n"
	    "    mode_mask_array[mode] = (HOST_WIDE_INT_1U << precision) - 1;"
	    "\n"
	    "  else\n"
	    "    mode_mask_array[mode] = HOST_WIDE_INT_M1U;\n"
	    "}\n");

  puts ("\
\nvoid\
\ninit_adjust_machine_modes (void)\
\n{\
\n  poly_uint16 ps ATTRIBUTE_UNUSED;\n\
  size_t s ATTRIBUTE_UNUSED;");

  for (a = adj_nunits; a; a = a->next)
    {
      m = a->mode;
      printf ("\n"
	      "  {\n"
	      "    /* %s:%d */\n  ps = %s;\n",
	      a->file, a->line, a->adjustment);
      printf ("    int old_factor = vector_element_size"
	      " (mode_precision[E_%smode], mode_nunits[E_%smode]);\n",
	      m->name, m->name);
      printf ("    mode_precision[E_%smode] = ps * old_factor;\n", m->name);
      printf ("    if (!multiple_p (mode_precision[E_%smode],"
	      " BITS_PER_UNIT, &mode_size[E_%smode]))\n", m->name, m->name);
      printf ("      mode_size[E_%smode] = -1;\n", m->name);
      printf ("    mode_nunits[E_%smode] = ps;\n", m->name);
      printf ("    adjust_mode_mask (E_%smode);\n", m->name);
      printf ("  }\n");
    }

  /* Size adjustments must be propagated to all containing modes.
     A size adjustment forces us to recalculate the alignment too.  */
  for (a = adj_bytesize; a; a = a->next)
    {
      printf ("\n  /* %s:%d */\n", a->file, a->line);
      switch (a->mode->cl)
	{
	case MODE_VECTOR_BOOL:
	case MODE_VECTOR_INT:
	case MODE_VECTOR_FLOAT:
	case MODE_VECTOR_FRACT:
	case MODE_VECTOR_UFRACT:
	case MODE_VECTOR_ACCUM:
	case MODE_VECTOR_UACCUM:
	  printf ("  ps = %s;\n", a->adjustment);
	  printf ("  s = mode_unit_size[E_%smode];\n", a->mode->name);
	  break;

	default:
	  printf ("  ps = s = %s;\n", a->adjustment);
	  printf ("  mode_unit_size[E_%smode] = s;\n", a->mode->name);
	  break;
	}
      printf ("  mode_size[E_%smode] = ps;\n", a->mode->name);
      printf ("  mode_base_align[E_%smode] = known_alignment (ps);\n",
	      a->mode->name);

      for (m = a->mode->contained; m; m = m->next_cont)
	{
	  switch (m->cl)
	    {
	    case MODE_COMPLEX_INT:
	    case MODE_COMPLEX_FLOAT:
	      printf ("  mode_size[E_%smode] = 2*s;\n", m->name);
	      printf ("  mode_unit_size[E_%smode] = s;\n", m->name);
	      printf ("  mode_base_align[E_%smode] = s & (~s + 1);\n",
		      m->name);
	      break;

	    case MODE_VECTOR_BOOL:
	      /* Changes to BImode should not affect vector booleans.  */
	      break;

	    case MODE_VECTOR_INT:
	    case MODE_VECTOR_FLOAT:
	    case MODE_VECTOR_FRACT:
	    case MODE_VECTOR_UFRACT:
	    case MODE_VECTOR_ACCUM:
	    case MODE_VECTOR_UACCUM:
	      printf ("  mode_size[E_%smode] = %d * ps;\n",
		      m->name, m->ncomponents);
	      printf ("  mode_unit_size[E_%smode] = s;\n", m->name);
	      printf ("  mode_base_align[E_%smode]"
		      " = known_alignment (%d * ps);\n",
		      m->name, m->ncomponents);
	      break;

	    default:
	      internal_error (
	      "mode %s is neither vector nor complex but contains %s",
	      m->name, a->mode->name);
	      /* NOTREACHED */
	    }
	}
    }

  /* Alignment adjustments propagate too.
     ??? This may not be the right thing for vector modes.  */
  for (a = adj_alignment; a; a = a->next)
    {
      printf ("\n  /* %s:%d */\n  s = %s;\n",
	      a->file, a->line, a->adjustment);
      printf ("  mode_base_align[E_%smode] = s;\n", a->mode->name);

      for (m = a->mode->contained; m; m = m->next_cont)
	{
	  switch (m->cl)
	    {
	    case MODE_COMPLEX_INT:
	    case MODE_COMPLEX_FLOAT:
	      printf ("  mode_base_align[E_%smode] = s;\n", m->name);
	      break;

	    case MODE_VECTOR_BOOL:
	      /* Changes to BImode should not affect vector booleans.  */
	      break;

	    case MODE_VECTOR_INT:
	    case MODE_VECTOR_FLOAT:
	    case MODE_VECTOR_FRACT:
	    case MODE_VECTOR_UFRACT:
	    case MODE_VECTOR_ACCUM:
	    case MODE_VECTOR_UACCUM:
	      printf ("  mode_base_align[E_%smode] = %d*s;\n",
		      m->name, m->ncomponents);
	      break;

	    default:
	      internal_error (
	      "mode %s is neither vector nor complex but contains %s",
	      m->name, a->mode->name);
	      /* NOTREACHED */
	    }
	}
    }

  /* Ibit adjustments don't have to propagate.  */
  for (a = adj_ibit; a; a = a->next)
    {
      printf ("\n  /* %s:%d */\n  s = %s;\n",
	      a->file, a->line, a->adjustment);
      printf ("  mode_ibit[E_%smode] = s;\n", a->mode->name);
    }

  /* Fbit adjustments don't have to propagate.  */
  for (a = adj_fbit; a; a = a->next)
    {
      printf ("\n  /* %s:%d */\n  s = %s;\n",
	      a->file, a->line, a->adjustment);
      printf ("  mode_fbit[E_%smode] = s;\n", a->mode->name);
    }

  /* Real mode formats don't have to propagate anywhere.  */
  for (a = adj_format; a; a = a->next)
    printf ("\n  /* %s:%d */\n  REAL_MODE_FORMAT (E_%smode) = %s;\n",
	    a->file, a->line, a->mode->name, a->adjustment);

  /* Adjust precision to the actual bits size.  */
  for (a = adj_precision; a; a = a->next)
    switch (a->mode->cl)
      {
	case MODE_VECTOR_BOOL:
	  printf ("\n  /* %s:%d.  */\n  ps = %s;\n", a->file, a->line,
		  a->adjustment);
	  printf ("  mode_precision[E_%smode] = ps;\n", a->mode->name);
	  break;
	default:
	  internal_error ("invalid use of ADJUST_PRECISION for mode %s",
			  a->mode->name);
	  /* NOTREACHED.  */
      }

  /* Ensure there is no mode size equals -1.  */
  for_all_modes (c, m)
    printf ("\n  gcc_assert (maybe_ne (mode_size[E_%smode], -1));\n",
	    m->name);

  puts ("}");
}

/* Emit ibit for all modes.  */

static void
emit_mode_ibit (void)
{
  int c;
  struct mode_data *m;

  print_maybe_const_decl ("%sunsigned char",
			  "mode_ibit", "NUM_MACHINE_MODES",
			  adj_ibit);

  for_all_modes (c, m)
    tagged_printf ("%u", m->ibit, m->name);

  print_closer ();
}

/* Emit fbit for all modes.  */

static void
emit_mode_fbit (void)
{
  int c;
  struct mode_data *m;

  print_maybe_const_decl ("%sunsigned char",
			  "mode_fbit", "NUM_MACHINE_MODES",
			  adj_fbit);

  for_all_modes (c, m)
    tagged_printf ("%u", m->fbit, m->name);

  print_closer ();
}

/* Emit __intN for all modes.  */

static void
emit_mode_int_n (void)
{
  int c;
  struct mode_data *m;
  struct mode_data **mode_sort;
  int n_modes = 0;
  int i, j;

  print_decl ("int_n_data_t", "int_n_data", "");

  n_modes = 0;
  for_all_modes (c, m)
    if (m->int_n)
      n_modes ++;
  mode_sort = XALLOCAVEC (struct mode_data *, n_modes);

  n_modes = 0;
  for_all_modes (c, m)
    if (m->int_n)
      mode_sort[n_modes++] = m;

  /* Yes, this is a bubblesort, but there are at most four (and
     usually only 1-2) entries to sort.  */
  for (i = 0; i<n_modes - 1; i++)
    for (j = i + 1; j < n_modes; j++)
      if (mode_sort[i]->int_n > mode_sort[j]->int_n)
	std::swap (mode_sort[i], mode_sort[j]);

  for (i = 0; i < n_modes; i ++)
    {
      m = mode_sort[i];
      printf(" {\n");
      tagged_printf ("%u", m->int_n, m->name);
      printf ("{ E_%smode },", m->name);
      printf(" },\n");
    }

  print_closer ();
}


static void
emit_insn_modes_c (void)
{
  emit_insn_modes_c_header ();
  emit_mode_name ();
  emit_mode_class ();
  emit_mode_precision ();
  emit_mode_size ();
  emit_mode_nunits ();
  emit_mode_wider ();
  emit_mode_complex ();
  emit_mode_mask ();
  emit_mode_inner ();
  emit_mode_unit_size ();
  emit_mode_unit_precision ();
  emit_mode_base_align ();
  emit_class_narrowest_mode ();
  emit_real_format_for_mode ();
  emit_mode_adjustments ();
  emit_mode_ibit ();
  emit_mode_fbit ();
  emit_mode_int_n ();
}

static void
emit_min_insn_modes_c (void)
{
  emit_min_insn_modes_c_header ();
  emit_mode_name ();
  emit_mode_class ();
  emit_mode_nunits ();
  emit_mode_wider ();
  emit_mode_inner ();
  emit_class_narrowest_mode ();
}

/* Master control.  */
int
main (int argc, char **argv)
{
  bool gen_header = false, gen_inlines = false, gen_min = false;
  progname = argv[0];

  if (argc == 1)
    ;
  else if (argc == 2 && !strcmp (argv[1], "-h"))
    gen_header = true;
  else if (argc == 2 && !strcmp (argv[1], "-i"))
    gen_inlines = true;
  else if (argc == 2 && !strcmp (argv[1], "-m"))
    gen_min = true;
  else
    {
      error ("usage: %s [-h|-i|-m] > file", progname);
      return FATAL_EXIT_CODE;
    }

  modes_by_name = htab_create_alloc (64, hash_mode, eq_mode, 0, xcalloc, free);

  create_modes ();
  complete_all_modes ();

  if (have_error)
    return FATAL_EXIT_CODE;

  calc_wider_mode ();

  if (gen_header)
    emit_insn_modes_h ();
  else if (gen_inlines)
    emit_insn_modes_inline_h ();
  else if (gen_min)
    emit_min_insn_modes_c ();
  else
    emit_insn_modes_c ();

  if (fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;
  return SUCCESS_EXIT_CODE;
}
