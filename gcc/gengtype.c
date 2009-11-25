/* Process source files and output type information.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
#include "gengtype.h"
#include "errors.h"	/* for fatal */
#include "double-int.h"

/* Data types, macros, etc. used only in this file.  */

/* Kinds of types we can understand.  */
enum typekind {
  TYPE_SCALAR,
  TYPE_STRING,
  TYPE_STRUCT,
  TYPE_UNION,
  TYPE_POINTER,
  TYPE_ARRAY,
  TYPE_LANG_STRUCT,
  TYPE_PARAM_STRUCT
};

typedef unsigned lang_bitmap;

/* A way to pass data through to the output end.  */
struct options
{
  struct options *next;
  const char *name;
  const char *info;
};

/* Option data for the 'nested_ptr' option.  */
struct nested_ptr_data
{
  type_p type;
  const char *convert_to;
  const char *convert_from;
};

/* A name and a type.  */
struct pair
{
  pair_p next;
  const char *name;
  type_p type;
  struct fileloc line;
  options_p opt;
};

#define NUM_PARAM 10

/* A description of a type.  */
enum gc_used_enum
  {
    GC_UNUSED = 0,
    GC_USED,
    GC_MAYBE_POINTED_TO,
    GC_POINTED_TO
  };

struct type
{
  enum typekind kind;
  type_p next;
  type_p pointer_to;
  enum gc_used_enum gc_used;
  union {
    type_p p;
    struct {
      const char *tag;
      struct fileloc line;
      pair_p fields;
      options_p opt;
      lang_bitmap bitmap;
      type_p lang_struct;
    } s;
    bool scalar_is_char;
    struct {
      type_p p;
      const char *len;
    } a;
    struct {
      type_p stru;
      type_p param[NUM_PARAM];
      struct fileloc line;
    } param_struct;
  } u;
};

#define UNION_P(x)					\
 ((x)->kind == TYPE_UNION || 				\
  ((x)->kind == TYPE_LANG_STRUCT 			\
   && (x)->u.s.lang_struct->kind == TYPE_UNION))
#define UNION_OR_STRUCT_P(x)			\
 ((x)->kind == TYPE_UNION 			\
  || (x)->kind == TYPE_STRUCT 			\
  || (x)->kind == TYPE_LANG_STRUCT)

/* Structure representing an output file.  */
struct outf
{
  struct outf *next;
  const char *name;
  size_t buflength;
  size_t bufused;
  char *buf;
};
typedef struct outf * outf_p;

/* An output file, suitable for definitions, that can see declarations
   made in INPUT_FILE and is linked into every language that uses
   INPUT_FILE.  May return NULL in plugin mode. */
extern outf_p get_output_file_with_visibility
   (const char *input_file);
const char *get_output_file_name (const char *);

/* Print, like fprintf, to O.  No-op if O is NULL. */
static void oprintf (outf_p o, const char *S, ...)
     ATTRIBUTE_PRINTF_2;

/* The list of output files.  */
static outf_p output_files;

/* The plugin input files and their number; in that case only
   a single file is produced.  */
static char** plugin_files;
static size_t nb_plugin_files;
/* the generated plugin output name & file */
static outf_p plugin_output;

/* The output header file that is included into pretty much every
   source file.  */
static outf_p header_file;

/* Source directory.  */
static const char *srcdir;

/* Length of srcdir name.  */
static size_t srcdir_len = 0;

static outf_p create_file (const char *, const char *);

static const char * get_file_basename (const char *);
static const char * get_file_realbasename (const char *);
static const char * get_file_srcdir_relative_path (const char *);

static int get_prefix_langdir_index (const char *);
static const char * get_file_langdir (const char *);


/* Nonzero iff an error has occurred.  */
bool hit_error = false;

static void gen_rtx_next (void);
static void write_rtx_next (void);
static void open_base_files (void);
static void close_output_files (void);

/* Report an error at POS, printing MSG.  */

void
error_at_line (struct fileloc *pos, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);

  fprintf (stderr, "%s:%d: ", pos->file, pos->line);
  vfprintf (stderr, msg, ap);
  fputc ('\n', stderr);
  hit_error = true;

  va_end (ap);
}

/* asprintf, but produces fatal message on out-of-memory.  */
char *
xasprintf (const char *format, ...)
{
  int n;
  char *result;
  va_list ap;

  va_start (ap, format);
  n = vasprintf (&result, format, ap);
  if (result == NULL || n < 0)
    fatal ("out of memory");
  va_end (ap);

  return result;
}

/* Input file handling. */

/* Table of all input files.  */
static const char **gt_files;
static size_t num_gt_files;

/* A number of places use the name of this file for a location for
   things that we can't rely on the source to define.  Make sure we
   can still use pointer comparison on filenames.  */
static const char this_file[] = __FILE__;

/* Vector of per-language directories.  */
static const char **lang_dir_names;
static size_t num_lang_dirs;

/* An array of output files suitable for definitions.  There is one
   BASE_FILES entry for each language.  */
static outf_p *base_files;

/* Return a bitmap which has bit `1 << BASE_FILE_<lang>' set iff
   INPUT_FILE is used by <lang>.

   This function should be written to assume that a file _is_ used
   if the situation is unclear.  If it wrongly assumes a file _is_ used,
   a linker error will result.  If it wrongly assumes a file _is not_ used,
   some GC roots may be missed, which is a much harder-to-debug problem.

   The relevant bitmap is stored immediately before the file's name in the
   buffer set up by read_input_list.  It may be unaligned, so we have to
   read it byte-by-byte.  */

static lang_bitmap
get_lang_bitmap (const char *gtfile)
{

  if (gtfile == this_file)
    /* Things defined in this file are universal.  */
    return (((lang_bitmap)1) << num_lang_dirs) - 1;
  else
    {
      lang_bitmap n = 0;
      int i;
      for (i = -(int) sizeof (lang_bitmap); i < 0; i++)
	n = (n << CHAR_BIT) + (unsigned char)gtfile[i];
      return n;
    }
}

/* Set the bitmap returned by get_lang_bitmap.  The only legitimate
   caller of this function is read_input_list.  */
static void
set_lang_bitmap (char *gtfile, lang_bitmap n)
{
  int i;
  for (i = -1; i >= -(int) sizeof (lang_bitmap); i--)
    {
      gtfile[i] = n & ((1U << CHAR_BIT)-1);
      n >>= CHAR_BIT;
    }
}

/* Scan the input file, LIST, and determine how much space we need to
   store strings in.  Also, count the number of language directories
   and files.  The numbers returned are overestimates as they does not
   consider repeated files.  */
static size_t
measure_input_list (FILE *list)
{
  size_t n = 0;
  int c;
  bool atbol = true;
  num_lang_dirs = 0;
  num_gt_files = plugin_files ? nb_plugin_files : 0;
  while ((c = getc (list)) != EOF)
    {
      n++;
      if (atbol)
	{
	  if (c == '[')
	    num_lang_dirs++;
	  else
	    {
	      /* Add space for a lang_bitmap before the input file name.  */
	      n += sizeof (lang_bitmap);
	      num_gt_files++;
	    }
	  atbol = false;
	}

      if (c == '\n')
	atbol = true;
    }

  rewind (list);
  return n;
}

/* Read one input line from LIST to HEREP (which is updated).  A
   pointer to the string is returned via LINEP.  If it was a language
   subdirectory in square brackets, strip off the square brackets and
   return true.  Otherwise, leave space before the string for a
   lang_bitmap, and return false.  At EOF, returns false, does not
   touch *HEREP, and sets *LINEP to NULL.  POS is used for
   diagnostics.  */
static bool
read_input_line (FILE *list, char **herep, char **linep,
		 struct fileloc *pos)
{
  char *here = *herep;
  char *line;
  int c = getc (list);

  /* Read over whitespace.  */
  while (c == '\n' || c == ' ')
    c = getc (list);

  if (c == EOF)
    {
      *linep = 0;
      return false;
    }
  else if (c == '[')
    {
      /* No space for a lang_bitmap is necessary.  Discard the '['. */
      c = getc (list);
      line = here;
      while (c != ']' && c != '\n' && c != EOF)
	{
	  *here++ = c;
	  c = getc (list);
	}
      *here++ = '\0';

      if (c == ']')
	{
	  c = getc (list);  /* eat what should be a newline */
	  if (c != '\n' && c != EOF)
	    error_at_line (pos, "junk on line after language tag [%s]", line);
	}
      else
	error_at_line (pos, "missing close bracket for language tag [%s", line);

      *herep = here;
      *linep = line;
      return true;
    }
  else
    {
      /* Leave space for a lang_bitmap.  */
      memset (here, 0, sizeof (lang_bitmap));
      here += sizeof (lang_bitmap);
      line = here;
      do
	{
	  *here++ = c;
	  c = getc (list);
	}
      while (c != EOF && c != '\n');
      *here++ = '\0';
      *herep = here;
      *linep = line;
      return false;
    }
}

/* Read the list of input files from LIST and compute all of the
   relevant tables.  There is one file per line of the list.  At
   first, all the files on the list are language-generic, but
   eventually a line will appear which is the name of a language
   subdirectory in square brackets, like this: [cp].  All subsequent
   files are specific to that language, until another language
   subdirectory tag appears.  Files can appear more than once, if
   they apply to more than one language.  */
static void
read_input_list (const char *listname)
{
  FILE *list = fopen (listname, "r");
  if (!list)
    fatal ("cannot open %s: %s", listname, strerror (errno));
  else
    {
      struct fileloc epos;
      size_t bufsz = measure_input_list (list);
      char *buf = XNEWVEC (char, bufsz);
      char *here = buf;
      char *committed = buf;
      char *limit = buf + bufsz;
      char *line;
      bool is_language;
      size_t langno = 0;
      size_t nfiles = 0;
      lang_bitmap curlangs = (1 << num_lang_dirs) - 1;

      epos.file = listname;
      epos.line = 0;

      lang_dir_names = XNEWVEC (const char *, num_lang_dirs);
      gt_files = XNEWVEC (const char *, num_gt_files);

      for (;;)
	{
	next_line:
	  epos.line++;
	  committed = here;
	  is_language = read_input_line (list, &here, &line, &epos);
	  gcc_assert (here <= limit);
	  if (line == 0)
	    break;
	  else if (is_language)
	    {
	      size_t i;
	      gcc_assert (langno <= num_lang_dirs);
	      for (i = 0; i < langno; i++)
		if (strcmp (lang_dir_names[i], line) == 0)
		  {
		    error_at_line (&epos, "duplicate language tag [%s]", line);
		    curlangs = 1 << i;
		    here = committed;
		    goto next_line;
		  }

	      curlangs = 1 << langno;
	      lang_dir_names[langno++] = line;
	    }
	  else
	    {
	      size_t i;
	      gcc_assert (nfiles <= num_gt_files);
	      for (i = 0; i < nfiles; i++)
		if (strcmp (gt_files[i], line) == 0)
		  {
		    /* Throw away the string we just read, and add the
		       current language to the existing string's bitmap.  */
		    lang_bitmap bmap = get_lang_bitmap (gt_files[i]);
		    if (bmap & curlangs)
		      error_at_line (&epos, "file %s specified more than once "
				     "for language %s", line, langno == 0
				     ? "(all)"
				     : lang_dir_names[langno - 1]);

		    bmap |= curlangs;
		    set_lang_bitmap (CONST_CAST(char *, gt_files[i]), bmap);
		    here = committed;
		    goto next_line;
		  }

	      set_lang_bitmap (line, curlangs);
	      gt_files[nfiles++] = line;
	    }
	}
      /* Update the global counts now that we know accurately how many
	 things there are.  (We do not bother resizing the arrays down.)  */
      num_lang_dirs = langno;
      /* Add the plugin files if provided.  */
      if (plugin_files)
	{
	  size_t i;
	  for (i = 0; i < nb_plugin_files; i++)
	    gt_files[nfiles++] = plugin_files[i];
	}
      num_gt_files = nfiles;
    }

  /* Sanity check: any file that resides in a language subdirectory
     (e.g. 'cp') ought to belong to the corresponding language.
     ??? Still true if for instance ObjC++ is enabled and C++ isn't?
     (Can you even do that?  Should you be allowed to?)  */
  {
    size_t f;
    for (f = 0; f < num_gt_files; f++)
      {
	lang_bitmap bitmap = get_lang_bitmap (gt_files[f]);
	const char *basename = get_file_basename (gt_files[f]);
	const char *slashpos = strchr (basename, '/');

	if (slashpos)
	  {
	    size_t l;
	    for (l = 0; l < num_lang_dirs; l++)
	      if ((size_t)(slashpos - basename) == strlen (lang_dir_names [l])
		  && memcmp (basename, lang_dir_names[l],
			     strlen (lang_dir_names[l])) == 0)
		{
		  if (!(bitmap & (1 << l)))
		    error ("%s is in language directory '%s' but is not "
			   "tagged for that language",
			   basename, lang_dir_names[l]);
		  break;
		}
          }
      }
  }

  if (ferror (list))
    fatal ("error reading %s: %s", listname, strerror (errno));

  fclose (list);
}



/* The one and only TYPE_STRING.  */

static struct type string_type = {
  TYPE_STRING, 0, 0, GC_USED, {0}
};

/* The two and only TYPE_SCALARs.  Their u.scalar_is_char flags are
   set to appropriate values at the beginning of main.  */

static struct type scalar_nonchar = {
  TYPE_SCALAR, 0, 0, GC_USED, {0}
};
static struct type scalar_char = {
  TYPE_SCALAR, 0, 0, GC_USED, {0}
};

/* Lists of various things.  */

static pair_p typedefs;
static type_p structures;
static type_p param_structs;
static pair_p variables;

static type_p find_param_structure
  (type_p t, type_p param[NUM_PARAM]);
static type_p adjust_field_tree_exp (type_p t, options_p opt);
static type_p adjust_field_rtx_def (type_p t, options_p opt);

/* Define S as a typedef to T at POS.  */

void
do_typedef (const char *s, type_p t, struct fileloc *pos)
{
  pair_p p;

  /* temporary kludge - gengtype doesn't handle conditionals or
     macros.  Ignore any attempt to typedef CUMULATIVE_ARGS, unless it
     is coming from this file (main() sets them up with safe dummy
     definitions).  */
  if (!strcmp (s, "CUMULATIVE_ARGS") && pos->file != this_file)
    return;

  for (p = typedefs; p != NULL; p = p->next)
    if (strcmp (p->name, s) == 0)
      {
	if (p->type != t)
	  {
	    error_at_line (pos, "type `%s' previously defined", s);
	    error_at_line (&p->line, "previously defined here");
	  }
	return;
      }

  p = XNEW (struct pair);
  p->next = typedefs;
  p->name = s;
  p->type = t;
  p->line = *pos;
  typedefs = p;
}

/* Define S as a typename of a scalar.  Cannot be used to define
   typedefs of 'char'.  Note: is also used for pointer-to-function
   typedefs (which are therefore not treated as pointers).  */

void
do_scalar_typedef (const char *s, struct fileloc *pos)
{
  do_typedef (s, &scalar_nonchar, pos);
}

/* Return the type previously defined for S.  Use POS to report errors.  */

type_p
resolve_typedef (const char *s, struct fileloc *pos)
{
  pair_p p;
  for (p = typedefs; p != NULL; p = p->next)
    if (strcmp (p->name, s) == 0)
      return p->type;
  error_at_line (pos, "unidentified type `%s'", s);
  return &scalar_nonchar;  /* treat as "int" */
}

/* Create and return a new structure with tag NAME (or a union iff
   ISUNION is nonzero), at POS with fields FIELDS and options O.  */

type_p
new_structure (const char *name, int isunion, struct fileloc *pos,
	       pair_p fields, options_p o)
{
  type_p si;
  type_p s = NULL;
  lang_bitmap bitmap = get_lang_bitmap (pos->file);

  /* temporary kludge - gengtype doesn't handle conditionals or
     macros.  Ignore any attempt to define struct location_s, unless
     it is coming from this file (main() sets it up safely). */
  if (!strcmp (name, "location_s") && !isunion
      && pos->file != this_file)
    return find_structure (name, 0);

  for (si = structures; si != NULL; si = si->next)
    if (strcmp (name, si->u.s.tag) == 0
	&& UNION_P (si) == isunion)
      {
	type_p ls = NULL;
	if (si->kind == TYPE_LANG_STRUCT)
	  {
	    ls = si;

	    for (si = ls->u.s.lang_struct; si != NULL; si = si->next)
	      if (si->u.s.bitmap == bitmap)
		s = si;
	  }
	else if (si->u.s.line.file != NULL && si->u.s.bitmap != bitmap)
	  {
	    ls = si;
	    si = XCNEW (struct type);
	    memcpy (si, ls, sizeof (struct type));
	    ls->kind = TYPE_LANG_STRUCT;
	    ls->u.s.lang_struct = si;
	    ls->u.s.fields = NULL;
	    si->next = NULL;
	    si->pointer_to = NULL;
	    si->u.s.lang_struct = ls;
	  }
	else
	  s = si;

	if (ls != NULL && s == NULL)
	  {
	    s = XCNEW (struct type);
	    s->next = ls->u.s.lang_struct;
	    ls->u.s.lang_struct = s;
	    s->u.s.lang_struct = ls;
	  }
	break;
      }

  if (s == NULL)
    {
      s = XCNEW (struct type);
      s->next = structures;
      structures = s;
    }

  if (s->u.s.line.file != NULL
      || (s->u.s.lang_struct && (s->u.s.lang_struct->u.s.bitmap & bitmap)))
    {
      error_at_line (pos, "duplicate definition of '%s %s'",
		     isunion ? "union" : "struct", s->u.s.tag);
      error_at_line (&s->u.s.line, "previous definition here");
    }

  s->kind = isunion ? TYPE_UNION : TYPE_STRUCT;
  s->u.s.tag = name;
  s->u.s.line = *pos;
  s->u.s.fields = fields;
  s->u.s.opt = o;
  s->u.s.bitmap = bitmap;
  if (s->u.s.lang_struct)
    s->u.s.lang_struct->u.s.bitmap |= bitmap;

  /* Reset location_s's location to input.h so that we know where to
     write out its mark routine.  */
  if (!strcmp (name, "location_s") && !isunion
      && pos->file == this_file)
    {
      size_t n;
      for (n = 0; n < num_gt_files; n++)
	if (!strcmp (gt_files[n] + strlen (gt_files[n]) - strlen ("input.h"),
		     "input.h"))
	  {
	    s->u.s.line.file = gt_files[n];
	    break;
	  }
    }

    return s;
}

/* Return the previously-defined structure with tag NAME (or a union
   iff ISUNION is nonzero), or a new empty structure or union if none
   was defined previously.  */

type_p
find_structure (const char *name, int isunion)
{
  type_p s;

  for (s = structures; s != NULL; s = s->next)
    if (strcmp (name, s->u.s.tag) == 0
	&& UNION_P (s) == isunion)
      return s;

  s = XCNEW (struct type);
  s->next = structures;
  structures = s;
  s->kind = isunion ? TYPE_UNION : TYPE_STRUCT;
  s->u.s.tag = name;
  structures = s;
  return s;
}

/* Return the previously-defined parameterized structure for structure
   T and parameters PARAM, or a new parameterized empty structure or
   union if none was defined previously.  */

static type_p
find_param_structure (type_p t, type_p param[NUM_PARAM])
{
  type_p res;

  for (res = param_structs; res; res = res->next)
    if (res->u.param_struct.stru == t
	&& memcmp (res->u.param_struct.param, param,
		   sizeof (type_p) * NUM_PARAM) == 0)
      break;
  if (res == NULL)
    {
      res = XCNEW (struct type);
      res->kind = TYPE_PARAM_STRUCT;
      res->next = param_structs;
      param_structs = res;
      res->u.param_struct.stru = t;
      memcpy (res->u.param_struct.param, param, sizeof (type_p) * NUM_PARAM);
    }
  return res;
}

/* Return a scalar type with name NAME.  */

type_p
create_scalar_type (const char *name)
{
  if (!strcmp (name, "char") || !strcmp (name, "unsigned char"))
    return &scalar_char;
  else
    return &scalar_nonchar;
}

/* Return a pointer to T.  */

type_p
create_pointer (type_p t)
{
  if (! t->pointer_to)
    {
      type_p r = XCNEW (struct type);
      r->kind = TYPE_POINTER;
      r->u.p = t;
      t->pointer_to = r;
    }
  return t->pointer_to;
}

/* Return an array of length LEN.  */

type_p
create_array (type_p t, const char *len)
{
  type_p v;

  v = XCNEW (struct type);
  v->kind = TYPE_ARRAY;
  v->u.a.p = t;
  v->u.a.len = len;
  return v;
}

/* Return an options structure with name NAME and info INFO.  NEXT is the
   next option in the chain.  */

options_p
create_option (options_p next, const char *name, const void *info)
{
  options_p o = XNEW (struct options);
  o->next = next;
  o->name = name;
  o->info = (const char*) info;
  return o;
}

/* Return an options structure for a "nested_ptr" option.  */
options_p
create_nested_ptr_option (options_p next, type_p t,
			  const char *to, const char *from)
{
  struct nested_ptr_data *d = XNEW (struct nested_ptr_data);

  d->type = adjust_field_type (t, 0);
  d->convert_to = to;
  d->convert_from = from;
  return create_option (next, "nested_ptr", d);
}

/* Add a variable named S of type T with options O defined at POS,
   to `variables'.  */

void
note_variable (const char *s, type_p t, options_p o, struct fileloc *pos)
{
  pair_p n;
  n = XNEW (struct pair);
  n->name = s;
  n->type = t;
  n->line = *pos;
  n->opt = o;
  n->next = variables;
  variables = n;
}

/* Most-general structure field creator.  */
static pair_p
create_field_all (pair_p next, type_p type, const char *name, options_p opt,
		  const char *file, int line)
{
  pair_p field;

  field = XNEW (struct pair);
  field->next = next;
  field->type = type;
  field->name = name;
  field->opt = opt;
  field->line.file = file;
  field->line.line = line;
  return field;
}

/* Create a field that came from the source code we are scanning,
   i.e. we have a 'struct fileloc', and possibly options; also,
   adjust_field_type should be called.  */
pair_p
create_field_at (pair_p next, type_p type, const char *name, options_p opt,
		 struct fileloc *pos)
{
  return create_field_all (next, adjust_field_type (type, opt),
			   name, opt, pos->file, pos->line);
}

/* Create a fake field with the given type and name.  NEXT is the next
   field in the chain.  */
#define create_field(next,type,name) \
    create_field_all(next,type,name, 0, this_file, __LINE__)

/* Like create_field, but the field is only valid when condition COND
   is true.  */

static pair_p
create_optional_field_ (pair_p next, type_p type, const char *name,
			const char *cond, int line)
{
  static int id = 1;
  pair_p union_fields;
  type_p union_type;

  /* Create a fake union type with a single nameless field of type TYPE.
     The field has a tag of "1".  This allows us to make the presence
     of a field of type TYPE depend on some boolean "desc" being true.  */
  union_fields = create_field (NULL, type, "");
  union_fields->opt = create_option (union_fields->opt, "dot", "");
  union_fields->opt = create_option (union_fields->opt, "tag", "1");
  union_type = new_structure (xasprintf ("%s_%d", "fake_union", id++), 1,
			      &lexer_line, union_fields, NULL);

  /* Create the field and give it the new fake union type.  Add a "desc"
     tag that specifies the condition under which the field is valid.  */
  return create_field_all (next, union_type, name,
			   create_option (0, "desc", cond),
			   this_file, line);
}
#define create_optional_field(next,type,name,cond)	\
       create_optional_field_(next,type,name,cond,__LINE__)

/* Reverse a linked list of 'struct pair's in place.  */
pair_p
nreverse_pairs (pair_p list)
{
  pair_p prev = 0, p, next;
  for (p = list; p; p = next)
    {
      next = p->next;
      p->next = prev;
      prev = p;
    }
  return prev;
}


/* We don't care how long a CONST_DOUBLE is.  */
#define CONST_DOUBLE_FORMAT "ww"
/* We don't want to see codes that are only for generator files.  */
#undef GENERATOR_FILE

enum rtx_code {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS) ENUM ,
#include "rtl.def"
#undef DEF_RTL_EXPR
  NUM_RTX_CODE
};

static const char * const rtx_name[NUM_RTX_CODE] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   NAME ,
#include "rtl.def"
#undef DEF_RTL_EXPR
};

static const char * const rtx_format[NUM_RTX_CODE] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   FORMAT ,
#include "rtl.def"
#undef DEF_RTL_EXPR
};

static int rtx_next_new[NUM_RTX_CODE];

/* We also need codes and names for insn notes (not register notes).
   Note that we do *not* bias the note values here.  */
enum insn_note {
#define DEF_INSN_NOTE(NAME) NAME,
#include "insn-notes.def"
#undef DEF_INSN_NOTE

  NOTE_INSN_MAX
};

/* We must allocate one more entry here, as we use NOTE_INSN_MAX as the
   default field for line number notes.  */
static const char *const note_insn_name[NOTE_INSN_MAX+1] = {
#define DEF_INSN_NOTE(NAME) #NAME,
#include "insn-notes.def"
#undef DEF_INSN_NOTE
};

#undef CONST_DOUBLE_FORMAT
#define GENERATOR_FILE

/* Generate the contents of the rtx_next array.  This really doesn't belong
   in gengtype at all, but it's needed for adjust_field_rtx_def.  */

static void
gen_rtx_next (void)
{
  int i;
  for (i = 0; i < NUM_RTX_CODE; i++)
    {
      int k;

      rtx_next_new[i] = -1;
      if (strncmp (rtx_format[i], "iuu", 3) == 0)
	rtx_next_new[i] = 2;
      else if (i == COND_EXEC || i == SET || i == EXPR_LIST || i == INSN_LIST)
	rtx_next_new[i] = 1;
      else
	for (k = strlen (rtx_format[i]) - 1; k >= 0; k--)
	  if (rtx_format[i][k] == 'e' || rtx_format[i][k] == 'u')
	    rtx_next_new[i] = k;
    }
}

/* Write out the contents of the rtx_next array.  */
static void
write_rtx_next (void)
{
  outf_p f = get_output_file_with_visibility (NULL);
  int i;
  if (!f)
    return;

  oprintf (f, "\n/* Used to implement the RTX_NEXT macro.  */\n");
  oprintf (f, "EXPORTED_CONST unsigned char rtx_next[NUM_RTX_CODE] = {\n");
  for (i = 0; i < NUM_RTX_CODE; i++)
    if (rtx_next_new[i] == -1)
      oprintf (f, "  0,\n");
    else
      oprintf (f,
	       "  RTX_HDR_SIZE + %d * sizeof (rtunion),\n",
	       rtx_next_new[i]);
  oprintf (f, "};\n");
}

/* Handle `special("rtx_def")'.  This is a special case for field
   `fld' of struct rtx_def, which is an array of unions whose values
   are based in a complex way on the type of RTL.  */

static type_p
adjust_field_rtx_def (type_p t, options_p ARG_UNUSED (opt))
{
  pair_p flds = NULL;
  options_p nodot;
  int i;
  type_p rtx_tp, rtvec_tp, tree_tp, mem_attrs_tp, note_union_tp, scalar_tp;
  type_p bitmap_tp, basic_block_tp, reg_attrs_tp, constant_tp, symbol_union_tp;

  if (t->kind != TYPE_UNION)
    {
      error_at_line (&lexer_line,
		     "special `rtx_def' must be applied to a union");
      return &string_type;
    }

  nodot = create_option (NULL, "dot", "");

  rtx_tp = create_pointer (find_structure ("rtx_def", 0));
  rtvec_tp = create_pointer (find_structure ("rtvec_def", 0));
  tree_tp = create_pointer (find_structure ("tree_node", 1));
  mem_attrs_tp = create_pointer (find_structure ("mem_attrs", 0));
  reg_attrs_tp = create_pointer (find_structure ("reg_attrs", 0));
  bitmap_tp = create_pointer (find_structure ("bitmap_element_def", 0));
  basic_block_tp = create_pointer (find_structure ("basic_block_def", 0));
  constant_tp = create_pointer (find_structure ("constant_descriptor_rtx", 0));
  scalar_tp = &scalar_nonchar;  /* rtunion int */

  {
    pair_p note_flds = NULL;
    int c;

    for (c = 0; c <= NOTE_INSN_MAX; c++)
      {
	switch (c)
	  {
	  case NOTE_INSN_MAX:
	  case NOTE_INSN_DELETED_LABEL:
	    note_flds = create_field (note_flds, &string_type, "rt_str");
	    break;

	  case NOTE_INSN_BLOCK_BEG:
	  case NOTE_INSN_BLOCK_END:
	    note_flds = create_field (note_flds, tree_tp, "rt_tree");
	    break;

	  case NOTE_INSN_VAR_LOCATION:
	    note_flds = create_field (note_flds, rtx_tp, "rt_rtx");
	    break;

	  default:
	    note_flds = create_field (note_flds, scalar_tp, "rt_int");
	    break;
	  }
	/* NOTE_INSN_MAX is used as the default field for line
	   number notes.  */
	if (c == NOTE_INSN_MAX)
	  note_flds->opt = create_option (nodot, "default", "");
	else
	  note_flds->opt = create_option (nodot, "tag", note_insn_name[c]);
      }
    note_union_tp = new_structure ("rtx_def_note_subunion", 1,
				   &lexer_line, note_flds, NULL);
  }
  /* Create a type to represent the various forms of SYMBOL_REF_DATA.  */
  {
    pair_p sym_flds;

    sym_flds = create_field (NULL, tree_tp, "rt_tree");
    sym_flds->opt = create_option (nodot, "default", "");

    sym_flds = create_field (sym_flds, constant_tp, "rt_constant");
    sym_flds->opt = create_option (nodot, "tag", "1");

    symbol_union_tp = new_structure ("rtx_def_symbol_subunion", 1,
				     &lexer_line, sym_flds, NULL);
  }
  for (i = 0; i < NUM_RTX_CODE; i++)
    {
      pair_p subfields = NULL;
      size_t aindex, nmindex;
      const char *sname;
      type_p substruct;
      char *ftag;

      for (aindex = 0; aindex < strlen (rtx_format[i]); aindex++)
	{
	  type_p t;
	  const char *subname;

	  switch (rtx_format[i][aindex])
	    {
	    case '*':
	    case 'i':
	    case 'n':
	    case 'w':
	      t = scalar_tp;
	      subname = "rt_int";
	      break;

	    case '0':
	      if (i == MEM && aindex == 1)
		t = mem_attrs_tp, subname = "rt_mem";
	      else if (i == JUMP_INSN && aindex == 8)
		t = rtx_tp, subname = "rt_rtx";
	      else if (i == CODE_LABEL && aindex == 4)
		t = scalar_tp, subname = "rt_int";
	      else if (i == CODE_LABEL && aindex == 5)
		t = rtx_tp, subname = "rt_rtx";
	      else if (i == LABEL_REF
		       && (aindex == 1 || aindex == 2))
		t = rtx_tp, subname = "rt_rtx";
	      else if (i == NOTE && aindex == 4)
		t = note_union_tp, subname = "";
	      else if (i == NOTE && aindex == 5)
		t = scalar_tp, subname = "rt_int";
	      else if (i == NOTE && aindex >= 7)
		t = scalar_tp, subname = "rt_int";
	      else if (i == ADDR_DIFF_VEC && aindex == 4)
		t = scalar_tp, subname = "rt_int";
	      else if (i == VALUE && aindex == 0)
		t = scalar_tp, subname = "rt_int";
	      else if (i == DEBUG_EXPR && aindex == 0)
		t = tree_tp, subname = "rt_tree";
	      else if (i == REG && aindex == 1)
		t = scalar_tp, subname = "rt_int";
	      else if (i == REG && aindex == 2)
		t = reg_attrs_tp, subname = "rt_reg";
	      else if (i == SCRATCH && aindex == 0)
		t = scalar_tp, subname = "rt_int";
	      else if (i == SYMBOL_REF && aindex == 1)
		t = scalar_tp, subname = "rt_int";
	      else if (i == SYMBOL_REF && aindex == 2)
		t = symbol_union_tp, subname = "";
	      else if (i == BARRIER && aindex >= 3)
		t = scalar_tp, subname = "rt_int";
	      else
		{
		  error_at_line (&lexer_line,
			"rtx type `%s' has `0' in position %lu, can't handle",
				 rtx_name[i], (unsigned long) aindex);
		  t = &string_type;
		  subname = "rt_int";
		}
	      break;

	    case 's':
	    case 'S':
	    case 'T':
	      t = &string_type;
	      subname = "rt_str";
	      break;

	    case 'e':
	    case 'u':
	      t = rtx_tp;
	      subname = "rt_rtx";
	      break;

	    case 'E':
	    case 'V':
	      t = rtvec_tp;
	      subname = "rt_rtvec";
	      break;

	    case 't':
	      t = tree_tp;
	      subname = "rt_tree";
	      break;

	    case 'b':
	      t = bitmap_tp;
	      subname = "rt_bit";
	      break;

	    case 'B':
	      t = basic_block_tp;
	      subname = "rt_bb";
	      break;

	    default:
	      error_at_line (&lexer_line,
		     "rtx type `%s' has `%c' in position %lu, can't handle",
			     rtx_name[i], rtx_format[i][aindex],
			     (unsigned long)aindex);
	      t = &string_type;
	      subname = "rt_int";
	      break;
	    }

	  subfields = create_field (subfields, t,
				    xasprintf (".fld[%lu].%s",
					       (unsigned long) aindex,
					       subname));
	  subfields->opt = nodot;
	  if (t == note_union_tp)
	    subfields->opt = create_option (subfields->opt, "desc",
					    "NOTE_KIND (&%0)");
	  if (t == symbol_union_tp)
	    subfields->opt = create_option (subfields->opt, "desc",
					    "CONSTANT_POOL_ADDRESS_P (&%0)");
	}

      if (i == SYMBOL_REF)
	{
	  /* Add the "block_sym" field if SYMBOL_REF_HAS_BLOCK_INFO_P holds.  */
	  type_p field_tp = find_structure ("block_symbol", 0);
	  subfields
	    = create_optional_field (subfields, field_tp, "block_sym",
				     "SYMBOL_REF_HAS_BLOCK_INFO_P (&%0)");
	}

      sname = xasprintf ("rtx_def_%s", rtx_name[i]);
      substruct = new_structure (sname, 0, &lexer_line, subfields, NULL);

      ftag = xstrdup (rtx_name[i]);
      for (nmindex = 0; nmindex < strlen (ftag); nmindex++)
	ftag[nmindex] = TOUPPER (ftag[nmindex]);

      flds = create_field (flds, substruct, "");
      flds->opt = create_option (nodot, "tag", ftag);
    }

  return new_structure ("rtx_def_subunion", 1, &lexer_line, flds, nodot);
}

/* Handle `special("tree_exp")'.  This is a special case for
   field `operands' of struct tree_exp, which although it claims to contain
   pointers to trees, actually sometimes contains pointers to RTL too.
   Passed T, the old type of the field, and OPT its options.  Returns
   a new type for the field.  */

static type_p
adjust_field_tree_exp (type_p t, options_p opt ATTRIBUTE_UNUSED)
{
  pair_p flds;
  options_p nodot;

  if (t->kind != TYPE_ARRAY)
    {
      error_at_line (&lexer_line,
		     "special `tree_exp' must be applied to an array");
      return &string_type;
    }

  nodot = create_option (NULL, "dot", "");

  flds = create_field (NULL, t, "");
  flds->opt = create_option (nodot, "length",
			     "TREE_OPERAND_LENGTH ((tree) &%0)");
  flds->opt = create_option (flds->opt, "default", "");

  return new_structure ("tree_exp_subunion", 1, &lexer_line, flds, nodot);
}

/* Perform any special processing on a type T, about to become the type
   of a field.  Return the appropriate type for the field.
   At present:
   - Converts pointer-to-char, with no length parameter, to TYPE_STRING;
   - Similarly for arrays of pointer-to-char;
   - Converts structures for which a parameter is provided to
     TYPE_PARAM_STRUCT;
   - Handles "special" options.
*/

type_p
adjust_field_type (type_p t, options_p opt)
{
  int length_p = 0;
  const int pointer_p = t->kind == TYPE_POINTER;
  type_p params[NUM_PARAM];
  int params_p = 0;
  int i;

  for (i = 0; i < NUM_PARAM; i++)
    params[i] = NULL;

  for (; opt; opt = opt->next)
    if (strcmp (opt->name, "length") == 0)
      length_p = 1;
    else if (strcmp (opt->name, "param_is") == 0
	     || (strncmp (opt->name, "param", 5) == 0
		 && ISDIGIT (opt->name[5])
		 && strcmp (opt->name + 6, "_is") == 0))
      {
	int num = ISDIGIT (opt->name[5]) ? opt->name[5] - '0' : 0;

	if (! UNION_OR_STRUCT_P (t)
	    && (t->kind != TYPE_POINTER || ! UNION_OR_STRUCT_P (t->u.p)))
	  {
	    error_at_line (&lexer_line,
   "option `%s' may only be applied to structures or structure pointers",
			   opt->name);
	    return t;
	  }

	params_p = 1;
	if (params[num] != NULL)
	  error_at_line (&lexer_line, "duplicate `%s' option", opt->name);
	if (! ISDIGIT (opt->name[5]))
	  params[num] = create_pointer (CONST_CAST2(type_p, const char *, opt->info));
	else
	  params[num] = CONST_CAST2 (type_p, const char *, opt->info);
      }
    else if (strcmp (opt->name, "special") == 0)
      {
	const char *special_name = opt->info;
	if (strcmp (special_name, "tree_exp") == 0)
	  t = adjust_field_tree_exp (t, opt);
	else if (strcmp (special_name, "rtx_def") == 0)
	  t = adjust_field_rtx_def (t, opt);
	else
	  error_at_line (&lexer_line, "unknown special `%s'", special_name);
      }

  if (params_p)
    {
      type_p realt;

      if (pointer_p)
	t = t->u.p;
      realt = find_param_structure (t, params);
      t = pointer_p ? create_pointer (realt) : realt;
    }

  if (! length_p
      && pointer_p
      && t->u.p->kind == TYPE_SCALAR
      && t->u.p->u.scalar_is_char)
    return &string_type;
  if (t->kind == TYPE_ARRAY && t->u.a.p->kind == TYPE_POINTER
      && t->u.a.p->u.p->kind == TYPE_SCALAR
      && t->u.a.p->u.p->u.scalar_is_char)
    return create_array (&string_type, t->u.a.len);

  return t;
}


static void set_gc_used_type (type_p, enum gc_used_enum, type_p *);
static void set_gc_used (pair_p);

/* Handle OPT for set_gc_used_type.  */

static void
process_gc_options (options_p opt, enum gc_used_enum level, int *maybe_undef,
		    int *pass_param, int *length, int *skip, type_p *nested_ptr)
{
  options_p o;
  for (o = opt; o; o = o->next)
    if (strcmp (o->name, "ptr_alias") == 0 && level == GC_POINTED_TO)
      set_gc_used_type (CONST_CAST2 (type_p, const char *, o->info),
			GC_POINTED_TO, NULL);
    else if (strcmp (o->name, "maybe_undef") == 0)
      *maybe_undef = 1;
    else if (strcmp (o->name, "use_params") == 0)
      *pass_param = 1;
    else if (strcmp (o->name, "length") == 0)
      *length = 1;
    else if (strcmp (o->name, "skip") == 0)
      *skip = 1;
    else if (strcmp (o->name, "nested_ptr") == 0)
      *nested_ptr = ((const struct nested_ptr_data *) o->info)->type;
}

/* Set the gc_used field of T to LEVEL, and handle the types it references.  */

static void
set_gc_used_type (type_p t, enum gc_used_enum level, type_p param[NUM_PARAM])
{
  if (t->gc_used >= level)
    return;

  t->gc_used = level;

  switch (t->kind)
    {
    case TYPE_STRUCT:
    case TYPE_UNION:
      {
	pair_p f;
	int dummy;
	type_p dummy2;

	process_gc_options (t->u.s.opt, level, &dummy, &dummy, &dummy, &dummy,
			    &dummy2);

	for (f = t->u.s.fields; f; f = f->next)
	  {
	    int maybe_undef = 0;
	    int pass_param = 0;
	    int length = 0;
	    int skip = 0;
	    type_p nested_ptr = NULL;
	    process_gc_options (f->opt, level, &maybe_undef, &pass_param,
				&length, &skip, &nested_ptr);

	    if (nested_ptr && f->type->kind == TYPE_POINTER)
	      set_gc_used_type (nested_ptr, GC_POINTED_TO,
				pass_param ? param : NULL);
	    else if (length && f->type->kind == TYPE_POINTER)
	      set_gc_used_type (f->type->u.p, GC_USED, NULL);
	    else if (maybe_undef && f->type->kind == TYPE_POINTER)
	      set_gc_used_type (f->type->u.p, GC_MAYBE_POINTED_TO, NULL);
	    else if (pass_param && f->type->kind == TYPE_POINTER && param)
	      set_gc_used_type (find_param_structure (f->type->u.p, param),
				GC_POINTED_TO, NULL);
	    else if (skip)
	      ; /* target type is not used through this field */
	    else
	      set_gc_used_type (f->type, GC_USED, pass_param ? param : NULL);
	  }
	break;
      }

    case TYPE_POINTER:
      set_gc_used_type (t->u.p, GC_POINTED_TO, NULL);
      break;

    case TYPE_ARRAY:
      set_gc_used_type (t->u.a.p, GC_USED, param);
      break;

    case TYPE_LANG_STRUCT:
      for (t = t->u.s.lang_struct; t; t = t->next)
	set_gc_used_type (t, level, param);
      break;

    case TYPE_PARAM_STRUCT:
      {
	int i;
	for (i = 0; i < NUM_PARAM; i++)
	  if (t->u.param_struct.param[i] != 0)
	    set_gc_used_type (t->u.param_struct.param[i], GC_USED, NULL);
      }
      if (t->u.param_struct.stru->gc_used == GC_POINTED_TO)
	level = GC_POINTED_TO;
      else
	level = GC_USED;
      t->u.param_struct.stru->gc_used = GC_UNUSED;
      set_gc_used_type (t->u.param_struct.stru, level,
			t->u.param_struct.param);
      break;

    default:
      break;
    }
}

/* Set the gc_used fields of all the types pointed to by VARIABLES.  */

static void
set_gc_used (pair_p variables)
{
  pair_p p;
  for (p = variables; p; p = p->next)
    set_gc_used_type (p->type, GC_USED, NULL);
}

/* File mapping routines.  For each input file, there is one output .c file
   (but some output files have many input files), and there is one .h file
   for the whole build.  */

/* Output file handling.  */

/* Create and return an outf_p for a new file for NAME, to be called
   ONAME.  */

static outf_p
create_file (const char *name, const char *oname)
{
  static const char *const hdr[] = {
    "   Copyright (C) 2004, 2007, 2009 Free Software Foundation, Inc.\n",
    "\n",
    "This file is part of GCC.\n",
    "\n",
    "GCC is free software; you can redistribute it and/or modify it under\n",
    "the terms of the GNU General Public License as published by the Free\n",
    "Software Foundation; either version 3, or (at your option) any later\n",
    "version.\n",
    "\n",
    "GCC is distributed in the hope that it will be useful, but WITHOUT ANY\n",
    "WARRANTY; without even the implied warranty of MERCHANTABILITY or\n",
    "FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License\n",
    "for more details.\n",
    "\n",
    "You should have received a copy of the GNU General Public License\n",
    "along with GCC; see the file COPYING3.  If not see\n",
    "<http://www.gnu.org/licenses/>.  */\n",
    "\n",
    "/* This file is machine generated.  Do not edit.  */\n"
  };
  outf_p f;
  size_t i;

  gcc_assert (name != NULL);
  gcc_assert (oname != NULL);
  f = XCNEW (struct outf);
  f->next = output_files;
  f->name = oname;
  output_files = f;

  oprintf (f, "/* Type information for %s.\n", name);
  for (i = 0; i < ARRAY_SIZE (hdr); i++)
    oprintf (f, "%s", hdr[i]);
  return f;
}

/* Print, like fprintf, to O.
   N.B. You might think this could be implemented more efficiently
   with vsnprintf().  Unfortunately, there are C libraries that
   provide that function but without the C99 semantics for its return
   value, making it impossible to know how much space is required.  */
void
oprintf (outf_p o, const char *format, ...)
{
  char *s;
  size_t slength;
  va_list ap;

  /* In plugin mode, the O could be a NULL pointer, so avoid crashing
     in that case.  */
  if (!o)
    return;

  va_start (ap, format);
  slength = vasprintf (&s, format, ap);
  if (s == NULL || (int)slength < 0)
    fatal ("out of memory");
  va_end (ap);

  if (o->bufused + slength > o->buflength)
    {
      size_t new_len = o->buflength;
      if (new_len == 0)
	new_len = 1024;
      do {
	new_len *= 2;
      } while (o->bufused + slength >= new_len);
      o->buf = XRESIZEVEC (char, o->buf, new_len);
      o->buflength = new_len;
    }
  memcpy (o->buf + o->bufused, s, slength);
  o->bufused += slength;
  free (s);
}

/* Open the global header file and the language-specific header files.  */

static void
open_base_files (void)
{
  size_t i;

  if (nb_plugin_files > 0 && plugin_files)
    return;

  header_file = create_file ("GCC", "gtype-desc.h");

  base_files = XNEWVEC (outf_p, num_lang_dirs);

  for (i = 0; i < num_lang_dirs; i++)
    base_files[i] = create_file (lang_dir_names[i],
				 xasprintf ("gtype-%s.h", lang_dir_names[i]));

  /* gtype-desc.c is a little special, so we create it here.  */
  {
    /* The order of files here matters very much.  */
    static const char *const ifiles [] = {
      "config.h", "system.h", "coretypes.h", "tm.h", "varray.h",
      "hashtab.h", "splay-tree.h",  "obstack.h", "bitmap.h", "input.h",
      "tree.h", "rtl.h", "function.h", "insn-config.h", "expr.h",
      "hard-reg-set.h", "basic-block.h", "cselib.h", "insn-addr.h",
      "optabs.h", "libfuncs.h", "debug.h", "ggc.h", "cgraph.h",
      "tree-flow.h", "reload.h", "cpp-id-data.h", "tree-chrec.h",
      "cfglayout.h", "except.h", "output.h", "gimple.h", "cfgloop.h",
      "target.h", "ipa-prop.h", NULL
    };
    const char *const *ifp;
    outf_p gtype_desc_c;

    gtype_desc_c = create_file ("GCC", "gtype-desc.c");
    for (ifp = ifiles; *ifp; ifp++)
      oprintf (gtype_desc_c, "#include \"%s\"\n", *ifp);

    /* Make sure we handle "cfun" specially.  */
    oprintf (gtype_desc_c, "\n/* See definition in function.h.  */\n");
    oprintf (gtype_desc_c, "#undef cfun\n");
  }
}

/* For F a filename, return the real basename of F, with all the directory
   components skipped.  */

static const char *
get_file_realbasename (const char *f)
{
  const char * lastslash = strrchr (f, '/');

  return (lastslash != NULL) ? lastslash + 1 : f;
}

/* For F a filename, return the relative path to F from $(srcdir) if the
   latter is a prefix in F, NULL otherwise.  */

static const char *
get_file_srcdir_relative_path (const char *f)
{
  if (strlen (f) > srcdir_len
      && IS_DIR_SEPARATOR (f[srcdir_len])
      && memcmp (f, srcdir, srcdir_len) == 0)
    return f + srcdir_len + 1;
  else
    return NULL;
}

/* For F a filename, return the relative path to F from $(srcdir) if the
   latter is a prefix in F, or the real basename of F otherwise.  */

static const char *
get_file_basename (const char *f)
{
  const char * srcdir_path = get_file_srcdir_relative_path (f);

  return (srcdir_path != NULL) ? srcdir_path : get_file_realbasename (f);
}

/* For F a filename, return the lang_dir_names relative index of the language
   directory that is a prefix in F, if any, -1 otherwise.  */

static int
get_prefix_langdir_index (const char *f)
{
  size_t f_len = strlen (f);
  size_t lang_index;

  for (lang_index = 0; lang_index < num_lang_dirs; lang_index++)
    {
      const char * langdir = lang_dir_names [lang_index];
      size_t langdir_len = strlen (langdir);

      if (f_len > langdir_len
	  && IS_DIR_SEPARATOR (f[langdir_len])
	  && memcmp (f, langdir, langdir_len) == 0)
	return lang_index;
    }

  return -1;
}

/* For F a filename, return the name of language directory where F is located,
   if any, NULL otherwise.  */

static const char *
get_file_langdir (const char *f)
{
  /* Get the relative path to F from $(srcdir) and find the language by
     comparing the prefix with language directory names.  If F is not even
     srcdir relative, no point in looking further.  */

  int lang_index;
  const char * srcdir_relative_path = get_file_srcdir_relative_path (f);

  if (!srcdir_relative_path)
    return NULL;

  lang_index = get_prefix_langdir_index (srcdir_relative_path);

  return (lang_index >= 0) ? lang_dir_names [lang_index] : NULL;
}

/* The gt- output file name for F.  */

static const char *
get_file_gtfilename (const char *f)
{
  /* Cook up an initial version of the gt- file name from the file real
     basename and the language name, if any.  */

  const char *basename = get_file_realbasename (f);
  const char *langdir = get_file_langdir (f);

  char * result =
    (langdir ? xasprintf ("gt-%s-%s", langdir, basename)
     : xasprintf ("gt-%s", basename));

  /* Then replace all non alphanumerics characters by '-' and change the
     extenstion to ".h".  We expect the input filename extension was at least
     one character long.  */

  char *s = result;

  for (; *s != '.'; s++)
    if (! ISALNUM (*s) && *s != '-')
      *s = '-';

  memcpy (s, ".h", sizeof (".h"));

  return result;
}

/* An output file, suitable for definitions, that can see declarations
   made in INPUT_FILE and is linked into every language that uses
   INPUT_FILE.  */

outf_p
get_output_file_with_visibility (const char *input_file)
{
  outf_p r;
  size_t len;
  const char *basename;
  const char *for_name;
  const char *output_name;

  /* This can happen when we need a file with visibility on a
     structure that we've never seen.  We have to just hope that it's
     globally visible.  */
  if (input_file == NULL)
    input_file = "system.h";

  /* In plugin mode, return NULL unless the input_file is one of the
     plugin_files.  */
  if (plugin_files)
    {
      size_t i;
      for (i = 0; i < nb_plugin_files; i++)
	if (strcmp (input_file, plugin_files[i]) == 0)
	  return plugin_output;

      return NULL;
    }

  /* Determine the output file name.  */
  basename = get_file_basename (input_file);

  len = strlen (basename);
  if ((len > 2 && memcmp (basename+len-2, ".c", 2) == 0)
      || (len > 2 && memcmp (basename+len-2, ".y", 2) == 0)
      || (len > 3 && memcmp (basename+len-3, ".in", 3) == 0))
    {
      output_name = get_file_gtfilename (input_file);
      for_name = basename;
    }
  /* Some headers get used by more than one front-end; hence, it
     would be inappropriate to spew them out to a single gtype-<lang>.h
     (and gengtype doesn't know how to direct spewage into multiple
     gtype-<lang>.h headers at this time).  Instead, we pair up these
     headers with source files (and their special purpose gt-*.h headers).  */
  else if (strcmp (basename, "c-common.h") == 0)
    output_name = "gt-c-common.h", for_name = "c-common.c";
  else if (strcmp (basename, "c-lang.h") == 0)
    output_name = "gt-c-decl.h", for_name = "c-decl.c";
  else if (strcmp (basename, "c-tree.h") == 0)
    output_name = "gt-c-decl.h", for_name = "c-decl.c";
  else if (strncmp (basename, "cp", 2) == 0 && IS_DIR_SEPARATOR (basename[2])
	   && strcmp (basename + 3, "cp-tree.h") == 0)
    output_name = "gt-cp-tree.h", for_name = "cp/tree.c";
  else if (strncmp (basename, "cp", 2) == 0 && IS_DIR_SEPARATOR (basename[2])
	   && strcmp (basename + 3, "decl.h") == 0)
    output_name = "gt-cp-decl.h", for_name = "cp/decl.c";
  else if (strncmp (basename, "cp", 2) == 0 && IS_DIR_SEPARATOR (basename[2])
	   && strcmp (basename + 3, "name-lookup.h") == 0)
    output_name = "gt-cp-name-lookup.h", for_name = "cp/name-lookup.c";
  else if (strncmp (basename, "objc", 4) == 0 && IS_DIR_SEPARATOR (basename[4])
	   && strcmp (basename + 5, "objc-act.h") == 0)
    output_name = "gt-objc-objc-act.h", for_name = "objc/objc-act.c";
  else
    {
      int lang_index = get_prefix_langdir_index (basename);

      if (lang_index >= 0)
	return base_files[lang_index];

      output_name = "gtype-desc.c";
      for_name = NULL;
    }

  /* Look through to see if we've ever seen this output filename before.  */
  for (r = output_files; r; r = r->next)
    if (strcmp (r->name, output_name) == 0)
      return r;

  /* If not, create it.  */
  r = create_file (for_name, output_name);

  gcc_assert (r && r->name);
  return r;
}

/* The name of an output file, suitable for definitions, that can see
   declarations made in INPUT_FILE and is linked into every language
   that uses INPUT_FILE.  */

const char *
get_output_file_name (const char *input_file)
{
  outf_p o =  get_output_file_with_visibility (input_file);
  if (o)
    return o->name;
  return NULL;
}

/* Check if existing file is equal to the in memory buffer. */

static bool
is_file_equal (outf_p of)
{
  FILE *newfile = fopen (of->name, "r");
  size_t i;
  bool equal;
  if (newfile == NULL)
    return false;

  equal = true;
  for (i = 0; i < of->bufused; i++)
    {
      int ch;
      ch = fgetc (newfile);
      if (ch == EOF || ch != (unsigned char) of->buf[i])
	{
	  equal = false;
	  break;
	}
    }
  fclose (newfile);
  return equal;
}

/* Copy the output to its final destination,
   but don't unnecessarily change modification times.  */

static void
close_output_files (void)
{
  outf_p of;

  for (of = output_files; of; of = of->next)
    {

      if (!is_file_equal(of))
      {
        FILE *newfile = fopen (of->name, "w");
        if (newfile == NULL)
          fatal ("opening output file %s: %s", of->name, strerror (errno));
        if (fwrite (of->buf, 1, of->bufused, newfile) != of->bufused)
          fatal ("writing output file %s: %s", of->name, strerror (errno));
        if (fclose (newfile) != 0)
          fatal ("closing output file %s: %s", of->name, strerror (errno));
      }
      free(of->buf);
      of->buf = NULL;
      of->bufused = of->buflength = 0;
    }
}

struct flist {
  struct flist *next;
  int started_p;
  const char *name;
  outf_p f;
};

struct walk_type_data;

/* For scalars and strings, given the item in 'val'.
   For structures, given a pointer to the item in 'val'.
   For misc. pointers, given the item in 'val'.
*/
typedef void (*process_field_fn)
     (type_p f, const struct walk_type_data *p);
typedef void (*func_name_fn)
     (type_p s, const struct walk_type_data *p);

/* Parameters for write_types.  */

struct write_types_data
{
  const char *prefix;
  const char *param_prefix;
  const char *subfield_marker_routine;
  const char *marker_routine;
  const char *reorder_note_routine;
  const char *comment;
  int skip_hooks;		/* skip hook generation if non zero */
};

static void output_escaped_param (struct walk_type_data *d,
				  const char *, const char *);
static void output_mangled_typename (outf_p, const_type_p);
static void walk_type (type_p t, struct walk_type_data *d);
static void write_func_for_structure
     (type_p orig_s, type_p s, type_p * param,
      const struct write_types_data *wtd);
static void write_types_process_field
     (type_p f, const struct walk_type_data *d);
static void write_types (outf_p output_header,
                         type_p structures,
			 type_p param_structs,
			 const struct write_types_data *wtd);
static void write_types_local_process_field
     (type_p f, const struct walk_type_data *d);
static void write_local_func_for_structure
     (type_p orig_s, type_p s, type_p * param);
static void write_local (outf_p output_header,
                         type_p structures,
			 type_p param_structs);
static void write_enum_defn (type_p structures, type_p param_structs);
static int contains_scalar_p (type_p t);
static void put_mangled_filename (outf_p , const char *);
static void finish_root_table (struct flist *flp, const char *pfx,
			       const char *tname, const char *lastname,
			       const char *name);
static void write_root (outf_p , pair_p, type_p, const char *, int,
			struct fileloc *, const char *, bool);
static void write_array (outf_p f, pair_p v,
			 const struct write_types_data *wtd);
static void write_roots (pair_p, bool);

/* Parameters for walk_type.  */

struct walk_type_data
{
  process_field_fn process_field;
  const void *cookie;
  outf_p of;
  options_p opt;
  const char *val;
  const char *prev_val[4];
  int indent;
  int counter;
  struct fileloc *line;
  lang_bitmap bitmap;
  type_p *param;
  int used_length;
  type_p orig_s;
  const char *reorder_fn;
  bool needs_cast_p;
  bool fn_wants_lvalue;
};

/* Print a mangled name representing T to OF.  */

static void
output_mangled_typename (outf_p of, const_type_p t)
{
  if (t == NULL)
    oprintf (of, "Z");
  else switch (t->kind)
    {
    case TYPE_POINTER:
      oprintf (of, "P");
      output_mangled_typename (of, t->u.p);
      break;
    case TYPE_SCALAR:
      oprintf (of, "I");
      break;
    case TYPE_STRING:
      oprintf (of, "S");
      break;
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_LANG_STRUCT:
      oprintf (of, "%lu%s", (unsigned long) strlen (t->u.s.tag), t->u.s.tag);
      break;
    case TYPE_PARAM_STRUCT:
      {
	int i;
	for (i = 0; i < NUM_PARAM; i++)
	  if (t->u.param_struct.param[i] != NULL)
	    output_mangled_typename (of, t->u.param_struct.param[i]);
	output_mangled_typename (of, t->u.param_struct.stru);
      }
      break;
    case TYPE_ARRAY:
      gcc_unreachable ();
    }
}

/* Print PARAM to D->OF processing escapes.  D->VAL references the
   current object, D->PREV_VAL the object containing the current
   object, ONAME is the name of the option and D->LINE is used to
   print error messages.  */

static void
output_escaped_param (struct walk_type_data *d, const char *param,
		      const char *oname)
{
  const char *p;

  for (p = param; *p; p++)
    if (*p != '%')
      oprintf (d->of, "%c", *p);
    else switch (*++p)
      {
      case 'h':
	oprintf (d->of, "(%s)", d->prev_val[2]);
	break;
      case '0':
	oprintf (d->of, "(%s)", d->prev_val[0]);
	break;
      case '1':
	oprintf (d->of, "(%s)", d->prev_val[1]);
	break;
      case 'a':
	{
	  const char *pp = d->val + strlen (d->val);
	  while (pp[-1] == ']')
	    while (*pp != '[')
	      pp--;
	  oprintf (d->of, "%s", pp);
	}
	break;
      default:
	error_at_line (d->line, "`%s' option contains bad escape %c%c",
		       oname, '%', *p);
      }
}

/* Call D->PROCESS_FIELD for every field (or subfield) of D->VAL,
   which is of type T.  Write code to D->OF to constrain execution (at
   the point that D->PROCESS_FIELD is called) to the appropriate
   cases.  Call D->PROCESS_FIELD on subobjects before calling it on
   pointers to those objects.  D->PREV_VAL lists the objects
   containing the current object, D->OPT is a list of options to
   apply, D->INDENT is the current indentation level, D->LINE is used
   to print error messages, D->BITMAP indicates which languages to
   print the structure for, and D->PARAM is the current parameter
   (from an enclosing param_is option).  */

static void
walk_type (type_p t, struct walk_type_data *d)
{
  const char *length = NULL;
  const char *desc = NULL;
  int maybe_undef_p = 0;
  int use_param_num = -1;
  int use_params_p = 0;
  options_p oo;
  const struct nested_ptr_data *nested_ptr_d = NULL;

  d->needs_cast_p = false;
  for (oo = d->opt; oo; oo = oo->next)
    if (strcmp (oo->name, "length") == 0)
      length = oo->info;
    else if (strcmp (oo->name, "maybe_undef") == 0)
      maybe_undef_p = 1;
    else if (strncmp (oo->name, "use_param", 9) == 0
	     && (oo->name[9] == '\0' || ISDIGIT (oo->name[9])))
      use_param_num = oo->name[9] == '\0' ? 0 : oo->name[9] - '0';
    else if (strcmp (oo->name, "use_params") == 0)
      use_params_p = 1;
    else if (strcmp (oo->name, "desc") == 0)
      desc = oo->info;
    else if (strcmp (oo->name, "mark_hook") == 0)
      ;
    else if (strcmp (oo->name, "nested_ptr") == 0)
      nested_ptr_d = (const struct nested_ptr_data *) oo->info;
    else if (strcmp (oo->name, "dot") == 0)
      ;
    else if (strcmp (oo->name, "tag") == 0)
      ;
    else if (strcmp (oo->name, "special") == 0)
      ;
    else if (strcmp (oo->name, "skip") == 0)
      ;
    else if (strcmp (oo->name, "default") == 0)
      ;
    else if (strcmp (oo->name, "descbits") == 0)
      ;
    else if (strcmp (oo->name, "param_is") == 0)
      ;
    else if (strncmp (oo->name, "param", 5) == 0
	     && ISDIGIT (oo->name[5])
	     && strcmp (oo->name + 6, "_is") == 0)
      ;
    else if (strcmp (oo->name, "chain_next") == 0)
      ;
    else if (strcmp (oo->name, "chain_prev") == 0)
      ;
    else if (strcmp (oo->name, "chain_circular") == 0)
      ;
    else if (strcmp (oo->name, "reorder") == 0)
      ;
    else
      error_at_line (d->line, "unknown option `%s'\n", oo->name);

  if (d->used_length)
    length = NULL;

  if (use_params_p)
    {
      int pointer_p = t->kind == TYPE_POINTER;

      if (pointer_p)
	t = t->u.p;
      if (! UNION_OR_STRUCT_P (t))
	error_at_line (d->line, "`use_params' option on unimplemented type");
      else
	t = find_param_structure (t, d->param);
      if (pointer_p)
	t = create_pointer (t);
    }

  if (use_param_num != -1)
    {
      if (d->param != NULL && d->param[use_param_num] != NULL)
	{
	  type_p nt = d->param[use_param_num];

	  if (t->kind == TYPE_ARRAY)
	    nt = create_array (nt, t->u.a.len);
	  else if (length != NULL && t->kind == TYPE_POINTER)
	    nt = create_pointer (nt);
	  d->needs_cast_p = (t->kind != TYPE_POINTER
			     && (nt->kind == TYPE_POINTER
				 || nt->kind == TYPE_STRING));
	  t = nt;
	}
      else
	error_at_line (d->line, "no parameter defined for `%s'",
		       d->val);
    }

  if (maybe_undef_p
      && (t->kind != TYPE_POINTER || ! UNION_OR_STRUCT_P (t->u.p)))
    {
      error_at_line (d->line,
		     "field `%s' has invalid option `maybe_undef_p'\n",
		     d->val);
      return;
    }

  switch (t->kind)
    {
    case TYPE_SCALAR:
    case TYPE_STRING:
      d->process_field (t, d);
      break;

    case TYPE_POINTER:
      {
	if (maybe_undef_p
	    && t->u.p->u.s.line.file == NULL)
	  {
	    oprintf (d->of, "%*sgcc_assert (!%s);\n", d->indent, "", d->val);
	    break;
	  }

	if (! length)
	  {
	    if (! UNION_OR_STRUCT_P (t->u.p)
		&& t->u.p->kind != TYPE_PARAM_STRUCT)
	      {
		error_at_line (d->line,
			       "field `%s' is pointer to unimplemented type",
			       d->val);
		break;
	      }

	    if (nested_ptr_d)
	      {
		const char *oldprevval2 = d->prev_val[2];

		if (! UNION_OR_STRUCT_P (nested_ptr_d->type))
		  {
		    error_at_line (d->line,
				   "field `%s' has invalid "
				   "option `nested_ptr'\n",
				   d->val);
		    return;
		  }

		d->prev_val[2] = d->val;
		oprintf (d->of, "%*s{\n", d->indent, "");
		d->indent += 2;
		d->val = xasprintf ("x%d", d->counter++);
		oprintf (d->of, "%*s%s %s * %s%s =\n", d->indent, "",
			 (nested_ptr_d->type->kind == TYPE_UNION
			  ? "union" : "struct"),
			 nested_ptr_d->type->u.s.tag,
			 d->fn_wants_lvalue ? "" : "const ",
			 d->val);
		oprintf (d->of, "%*s", d->indent + 2, "");
		output_escaped_param (d, nested_ptr_d->convert_from,
				      "nested_ptr");
		oprintf (d->of, ";\n");

		d->process_field (nested_ptr_d->type, d);

		if (d->fn_wants_lvalue)
		  {
		    oprintf (d->of, "%*s%s = ", d->indent, "",
			     d->prev_val[2]);
		    d->prev_val[2] = d->val;
		    output_escaped_param (d, nested_ptr_d->convert_to,
					  "nested_ptr");
		    oprintf (d->of, ";\n");
		  }

		d->indent -= 2;
		oprintf (d->of, "%*s}\n", d->indent, "");
		d->val = d->prev_val[2];
		d->prev_val[2] = oldprevval2;
	      }
	    else
	      d->process_field (t->u.p, d);
	  }
	else
	  {
	    int loopcounter = d->counter++;
	    const char *oldval = d->val;
	    const char *oldprevval3 = d->prev_val[3];
	    char *newval;

	    oprintf (d->of, "%*sif (%s != NULL) {\n", d->indent, "", d->val);
	    d->indent += 2;
	    oprintf (d->of, "%*ssize_t i%d;\n", d->indent, "", loopcounter);
	    oprintf (d->of, "%*sfor (i%d = 0; i%d != (size_t)(", d->indent, "",
		     loopcounter, loopcounter);
	    output_escaped_param (d, length, "length");
	    oprintf (d->of, "); i%d++) {\n", loopcounter);
	    d->indent += 2;
	    d->val = newval = xasprintf ("%s[i%d]", oldval, loopcounter);
	    d->used_length = 1;
	    d->prev_val[3] = oldval;
	    walk_type (t->u.p, d);
	    free (newval);
	    d->val = oldval;
	    d->prev_val[3] = oldprevval3;
	    d->used_length = 0;
	    d->indent -= 2;
	    oprintf (d->of, "%*s}\n", d->indent, "");
	    d->process_field(t, d);
	    d->indent -= 2;
	    oprintf (d->of, "%*s}\n", d->indent, "");
	  }
      }
      break;

    case TYPE_ARRAY:
      {
	int loopcounter = d->counter++;
	const char *oldval = d->val;
	char *newval;

	/* If it's an array of scalars, we optimize by not generating
	   any code.  */
	if (t->u.a.p->kind == TYPE_SCALAR)
	  break;

	/* When walking an array, compute the length and store it in a
	   local variable before walking the array elements, instead of
	   recomputing the length expression each time through the loop.
	   This is necessary to handle tcc_vl_exp objects like CALL_EXPR,
	   where the length is stored in the first array element,
	   because otherwise that operand can get overwritten on the
	   first iteration.  */
	oprintf (d->of, "%*s{\n", d->indent, "");
	d->indent += 2;
	oprintf (d->of, "%*ssize_t i%d;\n", d->indent, "", loopcounter);
	oprintf (d->of, "%*ssize_t l%d = (size_t)(",
		 d->indent, "", loopcounter);
	if (length)
	  output_escaped_param (d, length, "length");
	else
	  oprintf (d->of, "%s", t->u.a.len);
	oprintf (d->of, ");\n");

	oprintf (d->of, "%*sfor (i%d = 0; i%d != l%d; i%d++) {\n",
		 d->indent, "",
		 loopcounter, loopcounter, loopcounter, loopcounter);
	d->indent += 2;
	d->val = newval = xasprintf ("%s[i%d]", oldval, loopcounter);
	d->used_length = 1;
	walk_type (t->u.a.p, d);
	free (newval);
	d->used_length = 0;
	d->val = oldval;
	d->indent -= 2;
	oprintf (d->of, "%*s}\n", d->indent, "");
	d->indent -= 2;
	oprintf (d->of, "%*s}\n", d->indent, "");
      }
      break;

    case TYPE_STRUCT:
    case TYPE_UNION:
      {
	pair_p f;
	const char *oldval = d->val;
	const char *oldprevval1 = d->prev_val[1];
	const char *oldprevval2 = d->prev_val[2];
	const int union_p = t->kind == TYPE_UNION;
	int seen_default_p = 0;
	options_p o;

	if (! t->u.s.line.file)
	  error_at_line (d->line, "incomplete structure `%s'", t->u.s.tag);

	if ((d->bitmap & t->u.s.bitmap) != d->bitmap)
	  {
	    error_at_line (d->line,
			   "structure `%s' defined for mismatching languages",
			   t->u.s.tag);
	    error_at_line (&t->u.s.line, "one structure defined here");
	  }

	/* Some things may also be defined in the structure's options.  */
	for (o = t->u.s.opt; o; o = o->next)
	  if (! desc && strcmp (o->name, "desc") == 0)
	    desc = o->info;

	d->prev_val[2] = oldval;
	d->prev_val[1] = oldprevval2;
	if (union_p)
	  {
	    if (desc == NULL)
	      {
		error_at_line (d->line, "missing `desc' option for union `%s'",
			       t->u.s.tag);
		desc = "1";
	      }
	    oprintf (d->of, "%*sswitch (", d->indent, "");
	    output_escaped_param (d, desc, "desc");
	    oprintf (d->of, ")\n");
	    d->indent += 2;
	    oprintf (d->of, "%*s{\n", d->indent, "");
	  }
	for (f = t->u.s.fields; f; f = f->next)
	  {
	    options_p oo;
	    const char *dot = ".";
	    const char *tagid = NULL;
	    int skip_p = 0;
	    int default_p = 0;
	    int use_param_p = 0;
	    char *newval;

	    d->reorder_fn = NULL;
	    for (oo = f->opt; oo; oo = oo->next)
	      if (strcmp (oo->name, "dot") == 0)
		dot = oo->info;
	      else if (strcmp (oo->name, "tag") == 0)
		tagid = oo->info;
	      else if (strcmp (oo->name, "skip") == 0)
		skip_p = 1;
	      else if (strcmp (oo->name, "default") == 0)
		default_p = 1;
	      else if (strcmp (oo->name, "reorder") == 0)
		d->reorder_fn = oo->info;
	      else if (strncmp (oo->name, "use_param", 9) == 0
		       && (oo->name[9] == '\0' || ISDIGIT (oo->name[9])))
		use_param_p = 1;

	    if (skip_p)
	      continue;

	    if (union_p && tagid)
	      {
		oprintf (d->of, "%*scase %s:\n", d->indent, "", tagid);
		d->indent += 2;
	      }
	    else if (union_p && default_p)
	      {
		oprintf (d->of, "%*sdefault:\n", d->indent, "");
		d->indent += 2;
		seen_default_p = 1;
	      }
	    else if (! union_p && (default_p || tagid))
	      error_at_line (d->line,
			     "can't use `%s' outside a union on field `%s'",
			     default_p ? "default" : "tag", f->name);
	    else if (union_p && ! (default_p || tagid)
		     && f->type->kind == TYPE_SCALAR)
	      {
		fprintf (stderr,
	"%s:%d: warning: field `%s' is missing `tag' or `default' option\n",
			 d->line->file, d->line->line, f->name);
		continue;
	      }
	    else if (union_p && ! (default_p || tagid))
	      error_at_line (d->line,
			     "field `%s' is missing `tag' or `default' option",
			     f->name);

	    d->line = &f->line;
	    d->val = newval = xasprintf ("%s%s%s", oldval, dot, f->name);
	    d->opt = f->opt;
	    d->used_length = false;

	    if (union_p && use_param_p && d->param == NULL)
	      oprintf (d->of, "%*sgcc_unreachable ();\n", d->indent, "");
	    else
	      walk_type (f->type, d);

	    free (newval);

	    if (union_p)
	      {
		oprintf (d->of, "%*sbreak;\n", d->indent, "");
		d->indent -= 2;
	      }
	  }
	d->reorder_fn = NULL;

	d->val = oldval;
	d->prev_val[1] = oldprevval1;
	d->prev_val[2] = oldprevval2;

	if (union_p && ! seen_default_p)
	  {
	    oprintf (d->of, "%*sdefault:\n", d->indent, "");
	    oprintf (d->of, "%*s  break;\n", d->indent, "");
	  }
	if (union_p)
	  {
	    oprintf (d->of, "%*s}\n", d->indent, "");
	    d->indent -= 2;
	  }
      }
      break;

    case TYPE_LANG_STRUCT:
      {
	type_p nt;
	for (nt = t->u.s.lang_struct; nt; nt = nt->next)
	  if ((d->bitmap & nt->u.s.bitmap) == d->bitmap)
	    break;
	if (nt == NULL)
	  error_at_line (d->line, "structure `%s' differs between languages",
			 t->u.s.tag);
	else
	  walk_type (nt, d);
      }
      break;

    case TYPE_PARAM_STRUCT:
      {
	type_p *oldparam = d->param;

	d->param = t->u.param_struct.param;
	walk_type (t->u.param_struct.stru, d);
	d->param = oldparam;
      }
      break;

    default:
      gcc_unreachable ();
    }
}

/* process_field routine for marking routines.  */

static void
write_types_process_field (type_p f, const struct walk_type_data *d)
{
  const struct write_types_data *wtd;
  const char *cast = d->needs_cast_p ? "(void *)" : "";
  wtd = (const struct write_types_data *) d->cookie;

  switch (f->kind)
    {
    case TYPE_POINTER:
      oprintf (d->of, "%*s%s (%s%s", d->indent, "",
	       wtd->subfield_marker_routine, cast, d->val);
      if (wtd->param_prefix)
	{
	  oprintf (d->of, ", %s", d->prev_val[3]);
	  if (d->orig_s)
	    {
	      oprintf (d->of, ", gt_%s_", wtd->param_prefix);
	      output_mangled_typename (d->of, d->orig_s);
	    }
	  else
	    oprintf (d->of, ", gt_%sa_%s", wtd->param_prefix, d->prev_val[0]);

	  if (f->u.p->kind == TYPE_PARAM_STRUCT
	      && f->u.p->u.s.line.file != NULL)
	    {
	      oprintf (d->of, ", gt_e_");
	      output_mangled_typename (d->of, f);
	    }
	  else if (UNION_OR_STRUCT_P (f)
		   && f->u.p->u.s.line.file != NULL)
	    {
	      oprintf (d->of, ", gt_ggc_e_");
	      output_mangled_typename (d->of, f);
	    }
	  else
	    oprintf (d->of, ", gt_types_enum_last");
	}
      oprintf (d->of, ");\n");
      if (d->reorder_fn && wtd->reorder_note_routine)
	oprintf (d->of, "%*s%s (%s%s, %s, %s);\n", d->indent, "",
		 wtd->reorder_note_routine, cast, d->val,
		 d->prev_val[3], d->reorder_fn);
      break;

    case TYPE_STRING:
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_LANG_STRUCT:
    case TYPE_PARAM_STRUCT:
      oprintf (d->of, "%*sgt_%s_", d->indent, "", wtd->prefix);
      output_mangled_typename (d->of, f);
      oprintf (d->of, " (%s%s);\n", cast, d->val);
      if (d->reorder_fn && wtd->reorder_note_routine)
	oprintf (d->of, "%*s%s (%s%s, %s%s, %s);\n", d->indent, "",
		 wtd->reorder_note_routine, cast, d->val, cast, d->val,
		 d->reorder_fn);
      break;

    case TYPE_SCALAR:
      break;

    default:
      gcc_unreachable ();
    }
}

/* A subroutine of write_func_for_structure.  Write the enum tag for S.  */

static void
output_type_enum (outf_p of, type_p s)
{
  if (s->kind == TYPE_PARAM_STRUCT && s->u.s.line.file != NULL)
    {
      oprintf (of, ", gt_e_");
      output_mangled_typename (of, s);
    }
  else if (UNION_OR_STRUCT_P (s) && s->u.s.line.file != NULL)
    {
      oprintf (of, ", gt_ggc_e_");
      output_mangled_typename (of, s);
    }
  else
    oprintf (of, ", gt_types_enum_last");
}

/* For S, a structure that's part of ORIG_S, and using parameters
   PARAM, write out a routine that:
   - Takes a parameter, a void * but actually of type *S
   - If SEEN_ROUTINE returns nonzero, calls write_types_process_field on each
     field of S or its substructures and (in some cases) things
     that are pointed to by S.
*/

static void
write_func_for_structure (type_p orig_s, type_p s, type_p *param,
			  const struct write_types_data *wtd)
{
  const char *fn = s->u.s.line.file;
  int i;
  const char *chain_next = NULL;
  const char *chain_prev = NULL;
  const char *chain_circular = NULL;
  const char *mark_hook_name = NULL;
  options_p opt;
  struct walk_type_data d;

  /* This is a hack, and not the good kind either.  */
  for (i = NUM_PARAM - 1; i >= 0; i--)
    if (param && param[i] && param[i]->kind == TYPE_POINTER
	&& UNION_OR_STRUCT_P (param[i]->u.p))
      fn = param[i]->u.p->u.s.line.file;

  memset (&d, 0, sizeof (d));
  d.of = get_output_file_with_visibility (fn);

  for (opt = s->u.s.opt; opt; opt = opt->next)
    if (strcmp (opt->name, "chain_next") == 0)
      chain_next = opt->info;
    else if (strcmp (opt->name, "chain_prev") == 0)
      chain_prev = opt->info;
    else if (strcmp (opt->name, "chain_circular") == 0)
      chain_circular = opt->info;
    else if (strcmp (opt->name, "mark_hook") == 0)
      mark_hook_name = opt->info;

  if (chain_prev != NULL && chain_next == NULL)
    error_at_line (&s->u.s.line, "chain_prev without chain_next");
  if (chain_circular != NULL && chain_next != NULL)
    error_at_line (&s->u.s.line, "chain_circular with chain_next");
  if (chain_circular != NULL)
    chain_next = chain_circular;

  d.process_field = write_types_process_field;
  d.cookie = wtd;
  d.orig_s = orig_s;
  d.opt = s->u.s.opt;
  d.line = &s->u.s.line;
  d.bitmap = s->u.s.bitmap;
  d.param = param;
  d.prev_val[0] = "*x";
  d.prev_val[1] = "not valid postage";  /* Guarantee an error.  */
  d.prev_val[3] = "x";
  d.val = "(*x)";

  oprintf (d.of, "\n");
  oprintf (d.of, "void\n");
  if (param == NULL)
    oprintf (d.of, "gt_%sx_%s", wtd->prefix, orig_s->u.s.tag);
  else
    {
      oprintf (d.of, "gt_%s_", wtd->prefix);
      output_mangled_typename (d.of, orig_s);
    }
  oprintf (d.of, " (void *x_p)\n");
  oprintf (d.of, "{\n");
  oprintf (d.of, "  %s %s * %sx = (%s %s *)x_p;\n",
	   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag,
	   chain_next == NULL ? "const " : "",
	   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag);
  if (chain_next != NULL)
    oprintf (d.of, "  %s %s * xlimit = x;\n",
	     s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag);
  if (chain_next == NULL)
    {
      oprintf (d.of, "  if (%s (x", wtd->marker_routine);
      if (wtd->param_prefix)
	{
	  oprintf (d.of, ", x, gt_%s_", wtd->param_prefix);
	  output_mangled_typename (d.of, orig_s);
	  output_type_enum (d.of, orig_s);
	}
      oprintf (d.of, "))\n");
    }
  else
    {
      if (chain_circular != NULL)
	oprintf (d.of, "  if (!%s (xlimit", wtd->marker_routine);
      else
	oprintf (d.of, "  while (%s (xlimit", wtd->marker_routine);
      if (wtd->param_prefix)
	{
	  oprintf (d.of, ", xlimit, gt_%s_", wtd->param_prefix);
	  output_mangled_typename (d.of, orig_s);
	  output_type_enum (d.of, orig_s);
	}
      oprintf (d.of, "))\n");
      if (chain_circular != NULL)
	oprintf (d.of, "    return;\n  do\n");
      if (mark_hook_name && !wtd->skip_hooks)
	{
	  oprintf (d.of, "    {\n");
	  oprintf (d.of, "      %s (xlimit);\n   ", mark_hook_name);
	}
      oprintf (d.of, "   xlimit = (");
      d.prev_val[2] = "*xlimit";
      output_escaped_param (&d, chain_next, "chain_next");
      oprintf (d.of, ");\n");
      if (mark_hook_name && !wtd->skip_hooks)
	oprintf (d.of, "    }\n");
      if (chain_prev != NULL)
	{
	  oprintf (d.of, "  if (x != xlimit)\n");
	  oprintf (d.of, "    for (;;)\n");
	  oprintf (d.of, "      {\n");
	  oprintf (d.of, "        %s %s * const xprev = (",
		   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag);

	  d.prev_val[2] = "*x";
	  output_escaped_param (&d, chain_prev, "chain_prev");
	  oprintf (d.of, ");\n");
	  oprintf (d.of, "        if (xprev == NULL) break;\n");
	  oprintf (d.of, "        x = xprev;\n");
	  oprintf (d.of, "        (void) %s (xprev",
		   wtd->marker_routine);
	  if (wtd->param_prefix)
	    {
	      oprintf (d.of, ", xprev, gt_%s_", wtd->param_prefix);
	      output_mangled_typename (d.of, orig_s);
	      output_type_enum (d.of, orig_s);
	    }
	  oprintf (d.of, ");\n");
	  oprintf (d.of, "      }\n");
	}
      if (chain_circular != NULL)
	{
	  oprintf (d.of, "  while (%s (xlimit", wtd->marker_routine);
	  if (wtd->param_prefix)
	    {
	      oprintf (d.of, ", xlimit, gt_%s_", wtd->param_prefix);
	      output_mangled_typename (d.of, orig_s);
	      output_type_enum (d.of, orig_s);
	    }
	  oprintf (d.of, "));\n");
	  if (mark_hook_name && !wtd->skip_hooks)
	    oprintf (d.of, "  %s (xlimit);\n", mark_hook_name);
	  oprintf (d.of, "  do\n");
	}
      else
	oprintf (d.of, "  while (x != xlimit)\n");
    }
  oprintf (d.of, "    {\n");
  if (mark_hook_name && chain_next == NULL && !wtd->skip_hooks)
    {
      oprintf (d.of, "      %s (x);\n", mark_hook_name);
    }
  d.prev_val[2] = "*x";
  d.indent = 6;
  walk_type (s, &d);

  if (chain_next != NULL)
    {
      oprintf (d.of, "      x = (");
      output_escaped_param (&d, chain_next, "chain_next");
      oprintf (d.of, ");\n");
    }

  oprintf (d.of, "    }\n");
  if (chain_circular != NULL)
    oprintf (d.of, "  while (x != xlimit);\n");
  oprintf (d.of, "}\n");
}

/* Write out marker routines for STRUCTURES and PARAM_STRUCTS.  */

static void
write_types (outf_p output_header, type_p structures, type_p param_structs,
	     const struct write_types_data *wtd)
{
  type_p s;

  oprintf (output_header, "\n/* %s*/\n", wtd->comment);
  /* We first emit the macros and the declarations. Functions' code is
     emitted afterwards.  This is needed in plugin mode.  */
  oprintf (output_header, "/* macros and declarations */\n");
  for (s = structures; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO
	|| s->gc_used == GC_MAYBE_POINTED_TO)
      {
	options_p opt;

	if (s->gc_used == GC_MAYBE_POINTED_TO
	    && s->u.s.line.file == NULL)
	  continue;

	oprintf (output_header, "#define gt_%s_", wtd->prefix);
	output_mangled_typename (output_header, s);
	oprintf (output_header, "(X) do { \\\n");
	oprintf (output_header,
		 "  if (X != NULL) gt_%sx_%s (X);\\\n", wtd->prefix,
		 s->u.s.tag);
	oprintf (output_header,
		 "  } while (0)\n");

	for (opt = s->u.s.opt; opt; opt = opt->next)
	  if (strcmp (opt->name, "ptr_alias") == 0)
	    {
	      const_type_p const t = (const_type_p) opt->info;
	      if (t->kind == TYPE_STRUCT
		  || t->kind == TYPE_UNION
		  || t->kind == TYPE_LANG_STRUCT)
		oprintf (output_header,
			 "#define gt_%sx_%s gt_%sx_%s\n",
			 wtd->prefix, s->u.s.tag, wtd->prefix, t->u.s.tag);
	      else
		error_at_line (&s->u.s.line,
			       "structure alias is not a structure");
	      break;
	    }
	if (opt)
	  continue;

	/* Declare the marker procedure only once.  */
	oprintf (output_header,
		 "extern void gt_%sx_%s (void *);\n",
		 wtd->prefix, s->u.s.tag);

	if (s->u.s.line.file == NULL)
	  {
	    fprintf (stderr, "warning: structure `%s' used but not defined\n",
		     s->u.s.tag);
	    continue;
	  }
      }

  for (s = param_structs; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO)
      {
	type_p stru = s->u.param_struct.stru;

	/* Declare the marker procedure.  */
	oprintf (output_header, "extern void gt_%s_", wtd->prefix);
	output_mangled_typename (output_header, s);
	oprintf (output_header, " (void *);\n");

	if (stru->u.s.line.file == NULL)
	  {
	    fprintf (stderr, "warning: structure `%s' used but not defined\n",
		     s->u.s.tag);
	    continue;
	  }
      }

  /* At last we emit the functions code.  */
  oprintf (output_header, "\n/* functions code */\n");
  for (s = structures; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO
	|| s->gc_used == GC_MAYBE_POINTED_TO)
      {
	options_p opt;

	if (s->gc_used == GC_MAYBE_POINTED_TO
	    && s->u.s.line.file == NULL)
	  continue;
	for (opt = s->u.s.opt; opt; opt = opt->next)
	  if (strcmp (opt->name, "ptr_alias") == 0)
	    break;
	if (opt)
	  continue;

	if (s->kind == TYPE_LANG_STRUCT)
	  {
	    type_p ss;
	    for (ss = s->u.s.lang_struct; ss; ss = ss->next)
	      write_func_for_structure (s, ss, NULL, wtd);
	  }
	else
	  write_func_for_structure (s, s, NULL, wtd);
      }
  for (s = param_structs; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO)
      {
	type_p *param = s->u.param_struct.param;
	type_p stru = s->u.param_struct.stru;
	if (stru->u.s.line.file == NULL)
	  continue;
	if (stru->kind == TYPE_LANG_STRUCT)
	  {
	    type_p ss;
	    for (ss = stru->u.s.lang_struct; ss; ss = ss->next)
	      write_func_for_structure (s, ss, param, wtd);
	  }
	else
	  write_func_for_structure (s, stru, param, wtd);
      }
}

static const struct write_types_data ggc_wtd =
{
  "ggc_m", NULL, "ggc_mark", "ggc_test_and_set_mark", NULL,
  "GC marker procedures.  ",
  FALSE
};

static const struct write_types_data pch_wtd =
{
  "pch_n", "pch_p", "gt_pch_note_object", "gt_pch_note_object",
  "gt_pch_note_reorder",
  "PCH type-walking procedures.  ",
  TRUE
};

/* Write out the local pointer-walking routines.  */

/* process_field routine for local pointer-walking.  */

static void
write_types_local_process_field (type_p f, const struct walk_type_data *d)
{
  switch (f->kind)
    {
    case TYPE_POINTER:
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_LANG_STRUCT:
    case TYPE_PARAM_STRUCT:
    case TYPE_STRING:
      oprintf (d->of, "%*sif ((void *)(%s) == this_obj)\n", d->indent, "",
	       d->prev_val[3]);
      oprintf (d->of, "%*s  op (&(%s), cookie);\n", d->indent, "", d->val);
      break;

    case TYPE_SCALAR:
      break;

    default:
      gcc_unreachable ();
    }
}

/* For S, a structure that's part of ORIG_S, and using parameters
   PARAM, write out a routine that:
   - Is of type gt_note_pointers
   - Calls PROCESS_FIELD on each field of S or its substructures.
*/

static void
write_local_func_for_structure (type_p orig_s, type_p s, type_p *param)
{
  const char *fn = s->u.s.line.file;
  int i;
  struct walk_type_data d;

  /* This is a hack, and not the good kind either.  */
  for (i = NUM_PARAM - 1; i >= 0; i--)
    if (param && param[i] && param[i]->kind == TYPE_POINTER
	&& UNION_OR_STRUCT_P (param[i]->u.p))
      fn = param[i]->u.p->u.s.line.file;

  memset (&d, 0, sizeof (d));
  d.of = get_output_file_with_visibility (fn);
  d.process_field = write_types_local_process_field;
  d.opt = s->u.s.opt;
  d.line = &s->u.s.line;
  d.bitmap = s->u.s.bitmap;
  d.param = param;
  d.prev_val[0] = d.prev_val[2] = "*x";
  d.prev_val[1] = "not valid postage";  /* Guarantee an error.  */
  d.prev_val[3] = "x";
  d.val = "(*x)";
  d.fn_wants_lvalue = true;

  oprintf (d.of, "\n");
  oprintf (d.of, "void\n");
  oprintf (d.of, "gt_pch_p_");
  output_mangled_typename (d.of, orig_s);
  oprintf (d.of, " (ATTRIBUTE_UNUSED void *this_obj,\n"
	   "\tvoid *x_p,\n"
	   "\tATTRIBUTE_UNUSED gt_pointer_operator op,\n"
	   "\tATTRIBUTE_UNUSED void *cookie)\n");
  oprintf (d.of, "{\n");
  oprintf (d.of, "  %s %s * const x ATTRIBUTE_UNUSED = (%s %s *)x_p;\n",
	   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag,
	   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag);
  d.indent = 2;
  walk_type (s, &d);
  oprintf (d.of, "}\n");
}

/* Write out local marker routines for STRUCTURES and PARAM_STRUCTS.  */

static void
write_local (outf_p output_header, type_p structures, type_p param_structs)
{
  type_p s;

  if (!output_header)
    return;
  oprintf (output_header, "\n/* Local pointer-walking routines.  */\n");
  for (s = structures; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO
	|| s->gc_used == GC_MAYBE_POINTED_TO)
      {
	options_p opt;

	if (s->u.s.line.file == NULL)
	  continue;

	for (opt = s->u.s.opt; opt; opt = opt->next)
	  if (strcmp (opt->name, "ptr_alias") == 0)
	    {
	      const_type_p const t = (const_type_p) opt->info;
	      if (t->kind == TYPE_STRUCT
		  || t->kind == TYPE_UNION
		  || t->kind == TYPE_LANG_STRUCT)
		{
		  oprintf (output_header, "#define gt_pch_p_");
		  output_mangled_typename (output_header, s);
		  oprintf (output_header, " gt_pch_p_");
		  output_mangled_typename (output_header, t);
		  oprintf (output_header, "\n");
		}
	      else
		error_at_line (&s->u.s.line,
			       "structure alias is not a structure");
	      break;
	    }
	if (opt)
	  continue;

	/* Declare the marker procedure only once.  */
	oprintf (output_header, "extern void gt_pch_p_");
	output_mangled_typename (output_header, s);
	oprintf (output_header,
	 "\n    (void *, void *, gt_pointer_operator, void *);\n");

	if (s->kind == TYPE_LANG_STRUCT)
	  {
	    type_p ss;
	    for (ss = s->u.s.lang_struct; ss; ss = ss->next)
	      write_local_func_for_structure (s, ss, NULL);
	  }
	else
	  write_local_func_for_structure (s, s, NULL);
      }

  for (s = param_structs; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO)
      {
	type_p * param = s->u.param_struct.param;
	type_p stru = s->u.param_struct.stru;

	/* Declare the marker procedure.  */
	oprintf (output_header, "extern void gt_pch_p_");
	output_mangled_typename (output_header, s);
	oprintf (output_header,
	 "\n    (void *, void *, gt_pointer_operator, void *);\n");

	if (stru->u.s.line.file == NULL)
	  {
	    fprintf (stderr, "warning: structure `%s' used but not defined\n",
		     s->u.s.tag);
	    continue;
	  }

	if (stru->kind == TYPE_LANG_STRUCT)
	  {
	    type_p ss;
	    for (ss = stru->u.s.lang_struct; ss; ss = ss->next)
	      write_local_func_for_structure (s, ss, param);
	  }
	else
	  write_local_func_for_structure (s, stru, param);
      }
}

/* Write out the 'enum' definition for gt_types_enum.  */

static void
write_enum_defn (type_p structures, type_p param_structs)
{
  type_p s;

  if (!header_file)
    return;
  oprintf (header_file, "\n/* Enumeration of types known.  */\n");
  oprintf (header_file, "enum gt_types_enum {\n");
  for (s = structures; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO
	|| s->gc_used == GC_MAYBE_POINTED_TO)
      {
	if (s->gc_used == GC_MAYBE_POINTED_TO
	    && s->u.s.line.file == NULL)
	  continue;

	oprintf (header_file, " gt_ggc_e_");
	output_mangled_typename (header_file, s);
	oprintf (header_file, ", \n");
      }
  for (s = param_structs; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO)
      {
	oprintf (header_file, " gt_e_");
	output_mangled_typename (header_file, s);
	oprintf (header_file, ", \n");
      }
  oprintf (header_file, " gt_types_enum_last\n");
  oprintf (header_file, "};\n");
}

/* Might T contain any non-pointer elements?  */

static int
contains_scalar_p (type_p t)
{
  switch (t->kind)
    {
    case TYPE_STRING:
    case TYPE_POINTER:
      return 0;
    case TYPE_ARRAY:
      return contains_scalar_p (t->u.a.p);
    default:
      /* Could also check for structures that have no non-pointer
	 fields, but there aren't enough of those to worry about.  */
      return 1;
    }
}

/* Mangle FN and print it to F.  */

static void
put_mangled_filename (outf_p f, const char *fn)
{
  const char *name = get_output_file_name (fn);
  if (!f || !name)
    return;
  for (; *name != 0; name++)
    if (ISALNUM (*name))
      oprintf (f, "%c", *name);
    else
      oprintf (f, "%c", '_');
}

/* Finish off the currently-created root tables in FLP.  PFX, TNAME,
   LASTNAME, and NAME are all strings to insert in various places in
   the resulting code.  */

static void
finish_root_table (struct flist *flp, const char *pfx, const char *lastname,
		   const char *tname, const char *name)
{
  struct flist *fli2;

  for (fli2 = flp; fli2; fli2 = fli2->next)
    if (fli2->started_p)
      {
	oprintf (fli2->f, "  %s\n", lastname);
	oprintf (fli2->f, "};\n\n");
      }

  for (fli2 = flp; fli2 && base_files; fli2 = fli2->next)
    if (fli2->started_p)
      {
	lang_bitmap bitmap = get_lang_bitmap (fli2->name);
	int fnum;

	for (fnum = 0; bitmap != 0; fnum++, bitmap >>= 1)
	  if (bitmap & 1)
	    {
	      oprintf (base_files[fnum],
		       "extern const struct %s gt_%s_",
		       tname, pfx);
	      put_mangled_filename (base_files[fnum], fli2->name);
	      oprintf (base_files[fnum], "[];\n");
	    }
      }

  {
    size_t fnum;
    for (fnum = 0; base_files && fnum < num_lang_dirs; fnum++)
      oprintf (base_files [fnum],
	       "EXPORTED_CONST struct %s * const %s[] = {\n",
	       tname, name);
  }


  for (fli2 = flp; fli2; fli2 = fli2->next)
    if (fli2->started_p)
      {
	lang_bitmap bitmap = get_lang_bitmap (fli2->name);
	int fnum;

	fli2->started_p = 0;

	for (fnum = 0; base_files && bitmap != 0; fnum++, bitmap >>= 1)
	  if (bitmap & 1)
	    {
	      oprintf (base_files[fnum], "  gt_%s_", pfx);
	      put_mangled_filename (base_files[fnum], fli2->name);
	      oprintf (base_files[fnum], ",\n");
	    }
      }

  {
    size_t fnum;
    for (fnum = 0; base_files && fnum < num_lang_dirs; fnum++)
      {
	oprintf (base_files[fnum], "  NULL\n");
	oprintf (base_files[fnum], "};\n");
      }
  }
}

/* Write out to F the table entry and any marker routines needed to
   mark NAME as TYPE.  The original variable is V, at LINE.
   HAS_LENGTH is nonzero iff V was a variable-length array.  IF_MARKED
   is nonzero iff we are building the root table for hash table caches.  */

static void
write_root (outf_p f, pair_p v, type_p type, const char *name, int has_length,
	    struct fileloc *line, const char *if_marked, bool emit_pch)
{
  switch (type->kind)
    {
    case TYPE_STRUCT:
      {
	pair_p fld;
	for (fld = type->u.s.fields; fld; fld = fld->next)
	  {
	    int skip_p = 0;
	    const char *desc = NULL;
	    options_p o;

	    for (o = fld->opt; o; o = o->next)
	      if (strcmp (o->name, "skip") == 0)
		skip_p = 1;
	      else if (strcmp (o->name, "desc") == 0)
		desc = o->info;
	      else if (strcmp (o->name, "param_is") == 0)
		;
	      else
		error_at_line (line,
		       "field `%s' of global `%s' has unknown option `%s'",
			       fld->name, name, o->name);

	    if (skip_p)
	      continue;
	    else if (desc && fld->type->kind == TYPE_UNION)
	      {
		pair_p validf = NULL;
		pair_p ufld;

		for (ufld = fld->type->u.s.fields; ufld; ufld = ufld->next)
		  {
		    const char *tag = NULL;
		    options_p oo;

		    for (oo = ufld->opt; oo; oo = oo->next)
		      if (strcmp (oo->name, "tag") == 0)
			tag = oo->info;
		    if (tag == NULL || strcmp (tag, desc) != 0)
		      continue;
		    if (validf != NULL)
		      error_at_line (line,
			   "both `%s.%s.%s' and `%s.%s.%s' have tag `%s'",
				     name, fld->name, validf->name,
				     name, fld->name, ufld->name,
				     tag);
		    validf = ufld;
		  }
		if (validf != NULL)
		  {
		    char *newname;
		    newname = xasprintf ("%s.%s.%s",
					 name, fld->name, validf->name);
		    write_root (f, v, validf->type, newname, 0, line,
				if_marked, emit_pch);
		    free (newname);
		  }
	      }
	    else if (desc)
	      error_at_line (line,
		     "global `%s.%s' has `desc' option but is not union",
			     name, fld->name);
	    else
	      {
		char *newname;
		newname = xasprintf ("%s.%s", name, fld->name);
		write_root (f, v, fld->type, newname, 0, line, if_marked,
			    emit_pch);
		free (newname);
	      }
	  }
      }
      break;

    case TYPE_ARRAY:
      {
	char *newname;
	newname = xasprintf ("%s[0]", name);
	write_root (f, v, type->u.a.p, newname, has_length, line, if_marked,
		    emit_pch);
	free (newname);
      }
      break;

    case TYPE_POINTER:
      {
	type_p ap, tp;

	oprintf (f, "  {\n");
	oprintf (f, "    &%s,\n", name);
	oprintf (f, "    1");

	for (ap = v->type; ap->kind == TYPE_ARRAY; ap = ap->u.a.p)
	  if (ap->u.a.len[0])
	    oprintf (f, " * (%s)", ap->u.a.len);
	  else if (ap == v->type)
	    oprintf (f, " * ARRAY_SIZE (%s)", v->name);
	oprintf (f, ",\n");
	oprintf (f, "    sizeof (%s", v->name);
	for (ap = v->type; ap->kind == TYPE_ARRAY; ap = ap->u.a.p)
	  oprintf (f, "[0]");
	oprintf (f, "),\n");

	tp = type->u.p;

	if (! has_length && UNION_OR_STRUCT_P (tp))
	  {
	    oprintf (f, "    &gt_ggc_mx_%s,\n", tp->u.s.tag);
	    if (emit_pch)
	      oprintf (f, "    &gt_pch_nx_%s", tp->u.s.tag);
	    else
	      oprintf (f, "    NULL");
	  }
	else if (! has_length && tp->kind == TYPE_PARAM_STRUCT)
	  {
	    oprintf (f, "    &gt_ggc_m_");
	    output_mangled_typename (f, tp);
	    if (emit_pch)
	      {
		oprintf (f, ",\n    &gt_pch_n_");
		output_mangled_typename (f, tp);
	      }
	    else
	      oprintf (f, ",\n    NULL");
	  }
	else if (has_length
		 && (tp->kind == TYPE_POINTER || UNION_OR_STRUCT_P (tp)))
	  {
	    oprintf (f, "    &gt_ggc_ma_%s,\n", name);
	    if (emit_pch)
	      oprintf (f, "    &gt_pch_na_%s", name);
	    else
	      oprintf (f, "    NULL");
	  }
	else
	  {
	    error_at_line (line,
			   "global `%s' is pointer to unimplemented type",
			   name);
	  }
	if (if_marked)
	  oprintf (f, ",\n    &%s", if_marked);
	oprintf (f, "\n  },\n");
      }
      break;

    case TYPE_STRING:
      {
	oprintf (f, "  {\n");
	oprintf (f, "    &%s,\n", name);
	oprintf (f, "    1, \n");
	oprintf (f, "    sizeof (%s),\n", v->name);
	oprintf (f, "    (gt_pointer_walker) &gt_ggc_m_S,\n");
	oprintf (f, "    (gt_pointer_walker) &gt_pch_n_S\n");
	oprintf (f, "  },\n");
      }
      break;

    case TYPE_SCALAR:
      break;

    default:
      error_at_line (line,
		     "global `%s' is unimplemented type",
		     name);
    }
}

/* This generates a routine to walk an array.  */

static void
write_array (outf_p f, pair_p v, const struct write_types_data *wtd)
{
  struct walk_type_data d;
  char *prevval3;

  memset (&d, 0, sizeof (d));
  d.of = f;
  d.cookie = wtd;
  d.indent = 2;
  d.line = &v->line;
  d.opt = v->opt;
  d.bitmap = get_lang_bitmap (v->line.file);
  d.param = NULL;

  d.prev_val[3] = prevval3 = xasprintf ("&%s", v->name);

  if (wtd->param_prefix)
    {
      oprintf (f, "static void gt_%sa_%s\n", wtd->param_prefix, v->name);
      oprintf (f,
       "    (void *, void *, gt_pointer_operator, void *);\n");
      oprintf (f, "static void gt_%sa_%s (ATTRIBUTE_UNUSED void *this_obj,\n",
	       wtd->param_prefix, v->name);
      oprintf (d.of,
	       "      ATTRIBUTE_UNUSED void *x_p,\n"
	       "      ATTRIBUTE_UNUSED gt_pointer_operator op,\n"
	       "      ATTRIBUTE_UNUSED void * cookie)\n");
      oprintf (d.of, "{\n");
      d.prev_val[0] = d.prev_val[1] = d.prev_val[2] = d.val = v->name;
      d.process_field = write_types_local_process_field;
      walk_type (v->type, &d);
      oprintf (f, "}\n\n");
    }

  d.opt = v->opt;
  oprintf (f, "static void gt_%sa_%s (void *);\n",
	   wtd->prefix, v->name);
  oprintf (f, "static void\ngt_%sa_%s (ATTRIBUTE_UNUSED void *x_p)\n",
	   wtd->prefix, v->name);
  oprintf (f, "{\n");
  d.prev_val[0] = d.prev_val[1] = d.prev_val[2] = d.val = v->name;
  d.process_field = write_types_process_field;
  walk_type (v->type, &d);
  free (prevval3);
  oprintf (f, "}\n\n");
}

/* Output a table describing the locations and types of VARIABLES.  */

static void
write_roots (pair_p variables, bool emit_pch)
{
  pair_p v;
  struct flist *flp = NULL;

  for (v = variables; v; v = v->next)
    {
      outf_p f = get_output_file_with_visibility (v->line.file);
      struct flist *fli;
      const char *length = NULL;
      int deletable_p = 0;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "length") == 0)
	  length = o->info;
	else if (strcmp (o->name, "deletable") == 0)
	  deletable_p = 1;
	else if (strcmp (o->name, "param_is") == 0)
	  ;
	else if (strncmp (o->name, "param", 5) == 0
		 && ISDIGIT (o->name[5])
		 && strcmp (o->name + 6, "_is") == 0)
	  ;
	else if (strcmp (o->name, "if_marked") == 0)
	  ;
	else
	  error_at_line (&v->line,
			 "global `%s' has unknown option `%s'",
			 v->name, o->name);

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f && f)
	  break;
      if (fli == NULL)
	{
	  fli = XNEW (struct flist);
	  fli->f = f;
	  fli->next = flp;
	  fli->started_p = 0;
	  fli->name = v->line.file;
	  gcc_assert(fli->name);
	  flp = fli;

	  oprintf (f, "\n/* GC roots.  */\n\n");
	}

      if (! deletable_p
	  && length
	  && v->type->kind == TYPE_POINTER
	  && (v->type->u.p->kind == TYPE_POINTER
	      || v->type->u.p->kind == TYPE_STRUCT))
	{
	  write_array (f, v, &ggc_wtd);
	  write_array (f, v, &pch_wtd);
	}
    }

  for (v = variables; v; v = v->next)
    {
      outf_p f = get_output_file_with_visibility (v->line.file);
      struct flist *fli;
      int skip_p = 0;
      int length_p = 0;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "length") == 0)
	  length_p = 1;
	else if (strcmp (o->name, "deletable") == 0
		 || strcmp (o->name, "if_marked") == 0)
	  skip_p = 1;

      if (skip_p)
	continue;

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (! fli->started_p)
	{
	  fli->started_p = 1;

	  oprintf (f, "EXPORTED_CONST struct ggc_root_tab gt_ggc_r_");
	  put_mangled_filename (f, v->line.file);
	  oprintf (f, "[] = {\n");
	}

      write_root (f, v, v->type, v->name, length_p, &v->line, NULL, emit_pch);
    }

  finish_root_table (flp, "ggc_r", "LAST_GGC_ROOT_TAB", "ggc_root_tab",
		     "gt_ggc_rtab");

  for (v = variables; v; v = v->next)
    {
      outf_p f = get_output_file_with_visibility (v->line.file);
      struct flist *fli;
      int skip_p = 1;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "deletable") == 0)
	  skip_p = 0;
	else if (strcmp (o->name, "if_marked") == 0)
	  skip_p = 1;

      if (skip_p)
	continue;

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (! fli->started_p)
	{
	  fli->started_p = 1;

	  oprintf (f, "EXPORTED_CONST struct ggc_root_tab gt_ggc_rd_");
	  put_mangled_filename (f, v->line.file);
	  oprintf (f, "[] = {\n");
	}

      oprintf (f, "  { &%s, 1, sizeof (%s), NULL, NULL },\n",
	       v->name, v->name);
    }

  finish_root_table (flp, "ggc_rd", "LAST_GGC_ROOT_TAB", "ggc_root_tab",
		     "gt_ggc_deletable_rtab");

  for (v = variables; v; v = v->next)
    {
      outf_p f = get_output_file_with_visibility (v->line.file);
      struct flist *fli;
      const char *if_marked = NULL;
      int length_p = 0;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "length") == 0)
	  length_p = 1;
	else if (strcmp (o->name, "if_marked") == 0)
	  if_marked = o->info;

      if (if_marked == NULL)
	continue;

      if (v->type->kind != TYPE_POINTER
	  || v->type->u.p->kind != TYPE_PARAM_STRUCT
	  || v->type->u.p->u.param_struct.stru != find_structure ("htab", 0))
	{
	  error_at_line (&v->line, "if_marked option used but not hash table");
	  continue;
	}

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (! fli->started_p)
	{
	  fli->started_p = 1;

	  oprintf (f, "EXPORTED_CONST struct ggc_cache_tab gt_ggc_rc_");
	  put_mangled_filename (f, v->line.file);
	  oprintf (f, "[] = {\n");
	}

      write_root (f, v, v->type->u.p->u.param_struct.param[0],
		  v->name, length_p, &v->line, if_marked, emit_pch);
    }

  finish_root_table (flp, "ggc_rc", "LAST_GGC_CACHE_TAB", "ggc_cache_tab",
		     "gt_ggc_cache_rtab");

  if (!emit_pch)
    return;

  for (v = variables; v; v = v->next)
    {
      outf_p f = get_output_file_with_visibility (v->line.file);
      struct flist *fli;
      int length_p = 0;
      int if_marked_p = 0;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "length") == 0)
	  length_p = 1;
	else if (strcmp (o->name, "if_marked") == 0)
	  if_marked_p = 1;

      if (! if_marked_p)
	continue;

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (! fli->started_p)
	{
	  fli->started_p = 1;

	  oprintf (f, "EXPORTED_CONST struct ggc_root_tab gt_pch_rc_");
	  put_mangled_filename (f, v->line.file);
	  oprintf (f, "[] = {\n");
	}

      write_root (f, v, v->type, v->name, length_p, &v->line, NULL, emit_pch);
    }

  finish_root_table (flp, "pch_rc", "LAST_GGC_ROOT_TAB", "ggc_root_tab",
		     "gt_pch_cache_rtab");

  for (v = variables; v; v = v->next)
    {
      outf_p f = get_output_file_with_visibility (v->line.file);
      struct flist *fli;
      int skip_p = 0;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "deletable") == 0
	    || strcmp (o->name, "if_marked") == 0)
	  skip_p = 1;

      if (skip_p)
	continue;

      if (! contains_scalar_p (v->type))
	continue;

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (! fli->started_p)
	{
	  fli->started_p = 1;

	  oprintf (f, "EXPORTED_CONST struct ggc_root_tab gt_pch_rs_");
	  put_mangled_filename (f, v->line.file);
	  oprintf (f, "[] = {\n");
	}

      oprintf (f, "  { &%s, 1, sizeof (%s), NULL, NULL },\n",
	       v->name, v->name);
    }

  finish_root_table (flp, "pch_rs", "LAST_GGC_ROOT_TAB", "ggc_root_tab",
		     "gt_pch_scalar_rtab");
}

/* Record the definition of a generic VEC structure, as if we had expanded
   the macros in vec.h:

   typedef struct VEC_<type>_base GTY(()) {
   unsigned num;
   unsigned alloc;
   <type> GTY((length ("%h.num"))) vec[1];
   } VEC_<type>_base

   where the GTY(()) tags are only present if is_scalar is _false_.  */

void
note_def_vec (const char *type_name, bool is_scalar, struct fileloc *pos)
{
  pair_p fields;
  type_p t;
  options_p o;
  type_p len_ty = create_scalar_type ("unsigned");
  const char *name = concat ("VEC_", type_name, "_base", (char *)0);

  if (is_scalar)
    {
      t = create_scalar_type (type_name);
      o = 0;
    }
  else
    {
      t = resolve_typedef (type_name, pos);
      o = create_option (0, "length", "%h.num");
    }

  /* We assemble the field list in reverse order.  */
  fields = create_field_at (0, create_array (t, "1"), "vec", o, pos);
  fields = create_field_at (fields, len_ty, "alloc", 0, pos);
  fields = create_field_at (fields, len_ty, "num", 0, pos);

  do_typedef (name, new_structure (name, 0, pos, fields, 0), pos);
}

/* Record the definition of an allocation-specific VEC structure, as if
   we had expanded the macros in vec.h:

   typedef struct VEC_<type>_<astrat> {
     VEC_<type>_base base;
   } VEC_<type>_<astrat>;
*/
void
note_def_vec_alloc (const char *type, const char *astrat, struct fileloc *pos)
{
  const char *astratname = concat ("VEC_", type, "_", astrat, (char *)0);
  const char *basename = concat ("VEC_", type, "_base", (char *)0);

  pair_p field = create_field_at (0, resolve_typedef (basename, pos),
				  "base", 0, pos);

  do_typedef (astratname, new_structure (astratname, 0, pos, field, 0), pos);
}


int
main (int argc, char **argv)
{
  size_t i;
  static struct fileloc pos = { this_file, 0 };
  char* inputlist = 0;
  outf_p output_header;
  char* plugin_output_filename = NULL;
  /* fatal uses this */
  progname = "gengtype";

  if (argc >= 6 && !strcmp (argv[1], "-P"))
    {
      plugin_output_filename = argv[2];
      plugin_output = create_file ("GCC", plugin_output_filename);
      srcdir = argv[3];
      inputlist = argv[4];
      nb_plugin_files = argc - 5;
      plugin_files = XCNEWVEC (char *, nb_plugin_files);
      for (i = 0; i < nb_plugin_files; i++)
      {
        /* Place an all zero lang_bitmap before the plugin file
	   name.  */
        char *name = argv[i + 5];
        int len = strlen(name) + 1 + sizeof (lang_bitmap);
        plugin_files[i] = XCNEWVEC (char, len) + sizeof (lang_bitmap);
        strcpy (plugin_files[i], name);
      }
    }
  else if (argc == 3)
    {
      srcdir = argv[1];
      inputlist = argv[2];
    }
  else
    fatal ("usage: gengtype [-P pluginout.h] srcdir input-list "
           "[file1 file2 ... fileN]");

  srcdir_len = strlen (srcdir);

  read_input_list (inputlist);
  if (hit_error)
    return 1;

  scalar_char.u.scalar_is_char = true;
  scalar_nonchar.u.scalar_is_char = false;
  gen_rtx_next ();

  /* These types are set up with #define or else outside of where
     we can see them.  */
  pos.line = __LINE__ + 1;
  do_scalar_typedef ("CUMULATIVE_ARGS", &pos); pos.line++;
  do_scalar_typedef ("REAL_VALUE_TYPE", &pos); pos.line++;
  do_scalar_typedef ("FIXED_VALUE_TYPE", &pos); pos.line++;
  do_scalar_typedef ("double_int", &pos); pos.line++;
  do_scalar_typedef ("uint64_t", &pos); pos.line++;
  do_scalar_typedef ("uint8", &pos); pos.line++;
  do_scalar_typedef ("jword", &pos); pos.line++;
  do_scalar_typedef ("JCF_u2", &pos); pos.line++;
  do_scalar_typedef ("void", &pos); pos.line++;
  do_typedef ("PTR", create_pointer (resolve_typedef ("void", &pos)), &pos);

  for (i = 0; i < num_gt_files; i++)
    parse_file (gt_files[i]);

  if (hit_error)
    return 1;

  set_gc_used (variables);

  open_base_files ();
  write_enum_defn (structures, param_structs);
  output_header = plugin_output ? plugin_output : header_file;
  write_types (output_header, structures, param_structs, &ggc_wtd);
  if (plugin_files == NULL)
    {
      write_types (header_file, structures, param_structs, &pch_wtd);
      write_local (header_file, structures, param_structs);
    }
  write_roots (variables, plugin_files == NULL);
  write_rtx_next ();
  close_output_files ();

  if (plugin_files)
  {
    for (i = 0; i < nb_plugin_files; i++)
      free (plugin_files[i] - sizeof (lang_bitmap));
    free (plugin_files);
  }

  if (hit_error)
    return 1;
  return 0;
}
