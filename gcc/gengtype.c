/* Process source files and output type information.
   Copyright (C) 2002-2013 Free Software Foundation, Inc.

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

#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif
#include "system.h"
#include "errors.h"		/* for fatal */
#include "getopt.h"
#include "double-int.h"
#include "version.h"		/* for version_string & pkgversion_string.  */
#include "hashtab.h"
#include "xregex.h"
#include "obstack.h"
#include "gengtype.h"
#include "filenames.h"

/* Data types, macros, etc. used only in this file.  */


/* The list of output files.  */
outf_p output_files;

/* The output header file that is included into pretty much every
   source file.  */
outf_p header_file;


/* The name of the file containing the list of input files.  */
static char *inputlist;

/* The plugin input files and their number; in that case only
   a single file is produced.  */
static input_file **plugin_files;
static size_t nb_plugin_files;

/* The generated plugin output file and name.  */
static outf_p plugin_output;
static char *plugin_output_filename;

/* Our source directory and its length.  */
const char *srcdir;
size_t srcdir_len;

/* Variables used for reading and writing the state.  */
const char *read_state_filename;
const char *write_state_filename;

/* Variables to help debugging.  */
int do_dump;
int do_debug;

/* Level for verbose messages.  */
int verbosity_level;

/* We have a type count and use it to set the state_number of newly
   allocated types to some unique negative number.  */
static int type_count;

/* The backup directory should be in the same file system as the
   generated files, otherwise the rename(2) system call would fail.
   If NULL, no backup is made when overwriting a generated file.  */
static const char* backup_dir;	/* (-B) program option.  */


static outf_p create_file (const char *, const char *);

static const char *get_file_basename (const input_file *);
static const char *get_file_realbasename (const input_file *);

static int get_prefix_langdir_index (const char *);
static const char *get_file_langdir (const input_file *);

static void dump_pair (int indent, pair_p p);
static void dump_type (int indent, type_p p);
static void dump_type_list (int indent, type_p p);


/* Nonzero iff an error has occurred.  */
bool hit_error = false;

static void gen_rtx_next (void);
static void write_rtx_next (void);
static void open_base_files (void);
static void close_output_files (void);

/* Report an error at POS, printing MSG.  */

void
error_at_line (const struct fileloc *pos, const char *msg, ...)
{
  va_list ap;

  gcc_assert (pos != NULL && pos->file != NULL);
  va_start (ap, msg);

  fprintf (stderr, "%s:%d: ", get_input_file_name (pos->file), pos->line);
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

/* Locate the ultimate base class of struct S.  */

static const_type_p
get_ultimate_base_class (const_type_p s)
{
  while (s->u.s.base_class)
    s = s->u.s.base_class;
  return s;
}

/* Input file handling. */

/* Table of all input files.  */
const input_file **gt_files;
size_t num_gt_files;

/* A number of places use the name of this "gengtype.c" file for a
   location for things that we can't rely on the source to define.
   Make sure we can still use pointer comparison on filenames.  */
input_file* this_file;
/* The "system.h" file is likewise specially useful.  */
input_file* system_h_file;

/* Vector of per-language directories.  */
const char **lang_dir_names;
size_t num_lang_dirs;

/* An array of output files suitable for definitions.  There is one
   BASE_FILES entry for each language.  */
static outf_p *base_files;



#if ENABLE_CHECKING
/* Utility debugging function, printing the various type counts within
   a list of types.  Called through the DBGPRINT_COUNT_TYPE macro.  */
void
dbgprint_count_type_at (const char *fil, int lin, const char *msg, type_p t)
{
  int nb_types = 0, nb_scalar = 0, nb_string = 0;
  int nb_struct = 0, nb_union = 0, nb_array = 0, nb_pointer = 0;
  int nb_lang_struct = 0, nb_param_struct = 0;
  int nb_user_struct = 0, nb_undefined = 0;
  type_p p = NULL;
  for (p = t; p; p = p->next)
    {
      nb_types++;
      switch (p->kind)
	{
	case TYPE_UNDEFINED:
	  nb_undefined++;
	case TYPE_SCALAR:
	  nb_scalar++;
	  break;
	case TYPE_STRING:
	  nb_string++;
	  break;
	case TYPE_STRUCT:
	  nb_struct++;
	  break;
	case TYPE_USER_STRUCT:
	  nb_user_struct++;
	  break;
	case TYPE_UNION:
	  nb_union++;
	  break;
	case TYPE_POINTER:
	  nb_pointer++;
	  break;
	case TYPE_ARRAY:
	  nb_array++;
	  break;
	case TYPE_LANG_STRUCT:
	  nb_lang_struct++;
	  break;
	case TYPE_PARAM_STRUCT:
	  nb_param_struct++;
	  break;
	case TYPE_NONE:
	  gcc_unreachable ();
	}
    }
  fprintf (stderr, "\n" "%s:%d: %s: @@%%@@ %d types ::\n",
	   lbasename (fil), lin, msg, nb_types);
  if (nb_scalar > 0 || nb_string > 0)
    fprintf (stderr, "@@%%@@ %d scalars, %d strings\n", nb_scalar, nb_string);
  if (nb_struct > 0 || nb_union > 0)
    fprintf (stderr, "@@%%@@ %d structs, %d unions\n", nb_struct, nb_union);
  if (nb_pointer > 0 || nb_array > 0)
    fprintf (stderr, "@@%%@@ %d pointers, %d arrays\n", nb_pointer, nb_array);
  if (nb_lang_struct > 0 || nb_param_struct > 0)
    fprintf (stderr, "@@%%@@ %d lang_structs, %d param_structs\n",
	     nb_lang_struct, nb_param_struct);
  if (nb_user_struct > 0)
    fprintf (stderr, "@@%%@@ %d user_structs\n", nb_user_struct);
  if (nb_undefined > 0)
    fprintf (stderr, "@@%%@@ %d undefined types\n", nb_undefined);
  fprintf (stderr, "\n");
}
#endif /* ENABLE_CHECKING */

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
read_input_line (FILE *list, char **herep, char **linep, struct fileloc *pos)
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
	  c = getc (list);	/* eat what should be a newline */
	  if (c != '\n' && c != EOF)
	    error_at_line (pos, "junk on line after language tag [%s]", line);
	}
      else
	error_at_line (pos, "missing close bracket for language tag [%s",
		       line);

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
    fatal ("cannot open %s: %s", listname, xstrerror (errno));
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

      epos.file = input_file_by_name (listname);
      epos.line = 0;

      lang_dir_names = XNEWVEC (const char *, num_lang_dirs);
      gt_files = XNEWVEC (const input_file *, num_gt_files);

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
		    error_at_line (&epos, "duplicate language tag [%s]",
				   line);
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
	      input_file *inpf = input_file_by_name (line);
	      gcc_assert (nfiles <= num_gt_files);
	      for (i = 0; i < nfiles; i++)
		/* Since the input_file-s are uniquely hash-consed, we
		   can just compare pointers! */
		if (gt_files[i] == inpf)
		  {
		    /* Throw away the string we just read, and add the
		       current language to the existing string's bitmap.  */
		    lang_bitmap bmap = get_lang_bitmap (inpf);
		    if (bmap & curlangs)
		      error_at_line (&epos,
				     "file %s specified more than once "
				     "for language %s", line,
				     langno ==
				     0 ? "(all)" : lang_dir_names[langno -
								  1]);

		    bmap |= curlangs;
		    set_lang_bitmap (inpf, bmap);
		    here = committed;
		    goto next_line;
		  }

	      set_lang_bitmap (inpf, curlangs);
	      gt_files[nfiles++] = inpf;
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
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
	const char *slashpos2 = strchr (basename, '\\');

	if (!slashpos || (slashpos2 && slashpos2 < slashpos))
	  slashpos = slashpos2;
#endif

	if (slashpos)
	  {
	    size_t l;
	    for (l = 0; l < num_lang_dirs; l++)
	      if ((size_t) (slashpos - basename) == strlen (lang_dir_names[l])
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
    fatal ("error reading %s: %s", listname, xstrerror (errno));

  fclose (list);
}



/* The one and only TYPE_STRING.  */

struct type string_type = {
  TYPE_STRING, 0, 0, 0, GC_USED, {0}
};

/* The two and only TYPE_SCALARs.  Their u.scalar_is_char flags are
   set early in main.  */

struct type scalar_nonchar = {
  TYPE_SCALAR, 0, 0, 0, GC_USED, {0}
};

struct type scalar_char = {
  TYPE_SCALAR, 0, 0, 0, GC_USED, {0}
};

/* Lists of various things.  */

pair_p typedefs = NULL;
type_p structures = NULL;
type_p param_structs = NULL;
pair_p variables = NULL;

static type_p find_param_structure (type_p t, type_p param[NUM_PARAM]);
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
  p->opt = NULL;
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


/* Define TYPE_NAME to be a user defined type at location POS.  */

type_p
create_user_defined_type (const char *type_name, struct fileloc *pos)
{
  type_p ty = find_structure (type_name, TYPE_USER_STRUCT);

  /* We might have already seen an incomplete decl of the given type,
     in which case we won't have yet seen a GTY((user)), and the type will
     only have kind "TYPE_STRUCT".  Mark it as a user struct.  */
  ty->kind = TYPE_USER_STRUCT;

  ty->u.s.line = *pos;
  ty->u.s.bitmap = get_lang_bitmap (pos->file);
  do_typedef (type_name, ty, pos);

  /* If TYPE_NAME specifies a template, create references to the types
     in the template by pretending that each type is a field of TY.
     This is needed to make sure that the types referenced by the
     template are marked as used.  */
  char *str = xstrdup (type_name);
  char *open_bracket = strchr (str, '<');
  if (open_bracket)
    {
      /* We only accept simple template declarations (see
	 require_template_declaration), so we only need to parse a
	 comma-separated list of strings, implicitly assumed to
	 be type names.  */
      char *arg = open_bracket + 1;
      char *type_id = strtok (arg, ",>");
      pair_p fields = 0;
      while (type_id)
	{
	  /* Create a new field for every type found inside the template
	     parameter list.  */
	  const char *field_name = xstrdup (type_id);
	  type_p arg_type = resolve_typedef (field_name, pos);
	  fields = create_field_at (fields, arg_type, field_name, 0, pos);
	  type_id = strtok (0, ",>");
	}

      /* Associate the field list to TY.  */
      ty->u.s.fields = fields;
    }
  free (str);

  return ty;
}


/* Given a typedef name S, return its associated type.  Return NULL if
   S is not a registered type name.  */

static type_p
type_for_name (const char *s)
{
  pair_p p;

  /* Special-case support for types within a "gcc::" namespace.  Rather
     than fully-supporting namespaces, simply strip off the "gcc::" prefix
     where present.  This allows us to have GTY roots of this form:
         extern GTY(()) gcc::some_type *some_ptr;
     where the autogenerated functions will refer to simply "some_type",
     where they can be resolved into their namespace.  */
  if (0 == strncmp (s, "gcc::", 5))
    s += 5;

  for (p = typedefs; p != NULL; p = p->next)
    if (strcmp (p->name, s) == 0)
      return p->type;
  return NULL;
}


/* Create an undefined type with name S and location POS.  Return the
   newly created type.  */

static type_p
create_undefined_type (const char *s, struct fileloc *pos)
{
  type_p ty = find_structure (s, TYPE_UNDEFINED);
  ty->u.s.line = *pos;
  ty->u.s.bitmap = get_lang_bitmap (pos->file);
  do_typedef (s, ty, pos);
  return ty;
}


/* Return the type previously defined for S.  Use POS to report errors.  */

type_p
resolve_typedef (const char *s, struct fileloc *pos)
{
  bool is_template_instance = (strchr (s, '<') != NULL);
  type_p p = type_for_name (s);

  /* If we did not find a typedef registered, generate a TYPE_UNDEFINED
     type for regular type identifiers.  If the type identifier S is a
     template instantiation, however, we treat it as a user defined
     type.

     FIXME, this is actually a limitation in gengtype.  Supporting
     template types and their instances would require keeping separate
     track of the basic types definition and its instances.  This
     essentially forces all template classes in GC to be marked
     GTY((user)).  */
  if (!p)
    p = (is_template_instance)
	? create_user_defined_type (s, pos)
	: create_undefined_type (s, pos);

  return p;
}

/* Add SUBCLASS to head of linked list of BASE's subclasses.  */

void add_subclass (type_p base, type_p subclass)
{
  gcc_assert (union_or_struct_p (base));
  gcc_assert (union_or_struct_p (subclass));

  subclass->u.s.next_sibling_class = base->u.s.first_subclass;
  base->u.s.first_subclass = subclass;
}

/* Create and return a new structure with tag NAME at POS with fields
   FIELDS and options O.  The KIND of structure must be one of
   TYPE_STRUCT, TYPE_UNION or TYPE_USER_STRUCT.  */

type_p
new_structure (const char *name, enum typekind kind, struct fileloc *pos,
	       pair_p fields, options_p o, type_p base_class)
{
  type_p si;
  type_p s = NULL;
  lang_bitmap bitmap = get_lang_bitmap (pos->file);
  bool isunion = (kind == TYPE_UNION);

  gcc_assert (union_or_struct_p (kind));

  for (si = structures; si != NULL; si = si->next)
    if (strcmp (name, si->u.s.tag) == 0 && UNION_P (si) == isunion)
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
	    type_count++;
	    si = XCNEW (struct type);
	    memcpy (si, ls, sizeof (struct type));
	    ls->kind = TYPE_LANG_STRUCT;
	    ls->u.s.lang_struct = si;
	    ls->u.s.fields = NULL;
	    si->next = NULL;
	    si->state_number = -type_count;
	    si->pointer_to = NULL;
	    si->u.s.lang_struct = ls;
	  }
	else
	  s = si;

	if (ls != NULL && s == NULL)
	  {
	    type_count++;
	    s = XCNEW (struct type);
	    s->state_number = -type_count;
	    s->next = ls->u.s.lang_struct;
	    ls->u.s.lang_struct = s;
	    s->u.s.lang_struct = ls;
	  }
	break;
      }

  if (s == NULL)
    {
      type_count++;
      s = XCNEW (struct type);
      s->state_number = -type_count;
      s->next = structures;
      structures = s;
    }

  if (s->u.s.lang_struct && (s->u.s.lang_struct->u.s.bitmap & bitmap))
    {
      error_at_line (pos, "duplicate definition of '%s %s'",
		     isunion ? "union" : "struct", s->u.s.tag);
      error_at_line (&s->u.s.line, "previous definition here");
    }

  s->kind = kind;
  s->u.s.tag = name;
  s->u.s.line = *pos;
  s->u.s.fields = fields;
  s->u.s.opt = o;
  s->u.s.bitmap = bitmap;
  if (s->u.s.lang_struct)
    s->u.s.lang_struct->u.s.bitmap |= bitmap;
  s->u.s.base_class = base_class;
  if (base_class)
    add_subclass (base_class, s);

  return s;
}

/* Return the previously-defined structure or union with tag NAME,
   or a new empty structure or union if none was defined previously.
   The KIND of structure must be one of TYPE_STRUCT, TYPE_UNION or
   TYPE_USER_STRUCT.  */

type_p
find_structure (const char *name, enum typekind kind)
{
  type_p s;
  bool isunion = (kind == TYPE_UNION);

  gcc_assert (kind == TYPE_UNDEFINED || union_or_struct_p (kind));

  for (s = structures; s != NULL; s = s->next)
    if (strcmp (name, s->u.s.tag) == 0 && UNION_P (s) == isunion)
      return s;

  type_count++;
  s = XCNEW (struct type);
  s->next = structures;
  s->state_number = -type_count;
  structures = s;
  s->kind = kind;
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
      type_count++;
      res = XCNEW (struct type);
      res->kind = TYPE_PARAM_STRUCT;
      res->next = param_structs;
      res->state_number = -type_count;
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
  if (!t->pointer_to)
    {
      type_p r = XCNEW (struct type);
      type_count++;
      r->state_number = -type_count;
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

  type_count++;
  v = XCNEW (struct type);
  v->kind = TYPE_ARRAY;
  v->state_number = -type_count;
  v->u.a.p = t;
  v->u.a.len = len;
  return v;
}

/* Return a string options structure with name NAME and info INFO.
   NEXT is the next option in the chain.  */
options_p
create_string_option (options_p next, const char *name, const char *info)
{
  options_p o = XNEW (struct options);
  o->kind = OPTION_STRING;
  o->next = next;
  o->name = name;
  o->info.string = info;
  return o;
}

/* Create a type options structure with name NAME and info INFO.  NEXT
   is the next option in the chain.  */
options_p
create_type_option (options_p next, const char* name, type_p info)
{
  options_p o = XNEW (struct options);
  o->next = next;
  o->name = name;
  o->kind = OPTION_TYPE;
  o->info.type = info;
  return o;
}

/* Create a nested pointer options structure with name NAME and info
   INFO.  NEXT is the next option in the chain.  */
options_p
create_nested_option (options_p next, const char* name,
                      struct nested_ptr_data* info)
{
  options_p o;
  o = XNEW (struct options);
  o->next = next;
  o->name = name;
  o->kind = OPTION_NESTED;
  o->info.nested = info;
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
  return create_nested_option (next, "nested_ptr", d);
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
		  const input_file *inpf, int line)
{
  pair_p field;

  field = XNEW (struct pair);
  field->next = next;
  field->type = type;
  field->name = name;
  field->opt = opt;
  field->line.file = inpf;
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
    create_field_all (next,type,name, 0, this_file, __LINE__)

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
  union_fields->opt = 
    create_string_option (union_fields->opt, "dot", "");
  union_fields->opt = 
    create_string_option (union_fields->opt, "tag", "1");
  union_type = 
    new_structure (xasprintf ("%s_%d", "fake_union", id++), TYPE_UNION,
                   &lexer_line, union_fields, NULL, NULL);

  /* Create the field and give it the new fake union type.  Add a "desc"
     tag that specifies the condition under which the field is valid.  */
  return create_field_all (next, union_type, name,
			   create_string_option (0, "desc", cond), 
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

enum rtx_code
{
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS) ENUM ,
#include "rtl.def"
#undef DEF_RTL_EXPR
  NUM_RTX_CODE
};

static const char *const rtx_name[NUM_RTX_CODE] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   NAME ,
#include "rtl.def"
#undef DEF_RTL_EXPR
};

static const char *const rtx_format[NUM_RTX_CODE] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   FORMAT ,
#include "rtl.def"
#undef DEF_RTL_EXPR
};

static int rtx_next_new[NUM_RTX_CODE];

/* We also need codes and names for insn notes (not register notes).
   Note that we do *not* bias the note values here.  */
enum insn_note
{
#define DEF_INSN_NOTE(NAME) NAME,
#include "insn-notes.def"
#undef DEF_INSN_NOTE

  NOTE_INSN_MAX
};

/* We must allocate one more entry here, as we use NOTE_INSN_MAX as the
   default field for line number notes.  */
static const char *const note_insn_name[NOTE_INSN_MAX + 1] = {
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
	       "  RTX_HDR_SIZE + %d * sizeof (rtunion),\n", rtx_next_new[i]);
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
  type_p basic_block_tp, reg_attrs_tp, constant_tp, symbol_union_tp;

  if (t->kind != TYPE_UNION)
    {
      error_at_line (&lexer_line,
		     "special `rtx_def' must be applied to a union");
      return &string_type;
    }

  nodot = create_string_option (NULL, "dot", "");

  rtx_tp = create_pointer (find_structure ("rtx_def", TYPE_STRUCT));
  rtvec_tp = create_pointer (find_structure ("rtvec_def", TYPE_STRUCT));
  tree_tp = create_pointer (find_structure ("tree_node", TYPE_UNION));
  mem_attrs_tp = create_pointer (find_structure ("mem_attrs", TYPE_STRUCT));
  reg_attrs_tp = 
    create_pointer (find_structure ("reg_attrs", TYPE_STRUCT));
  basic_block_tp = 
    create_pointer (find_structure ("basic_block_def", TYPE_STRUCT));
  constant_tp =
    create_pointer (find_structure ("constant_descriptor_rtx", TYPE_STRUCT));
  scalar_tp = &scalar_nonchar;	/* rtunion int */

  {
    pair_p note_flds = NULL;
    int c;

    for (c = 0; c <= NOTE_INSN_MAX; c++)
      {
	switch (c)
	  {
	  case NOTE_INSN_MAX:
	  case NOTE_INSN_DELETED_LABEL:
	  case NOTE_INSN_DELETED_DEBUG_LABEL:
	    note_flds = create_field (note_flds, &string_type, "rt_str");
	    break;

	  case NOTE_INSN_BLOCK_BEG:
	  case NOTE_INSN_BLOCK_END:
	    note_flds = create_field (note_flds, tree_tp, "rt_tree");
	    break;

	  case NOTE_INSN_VAR_LOCATION:
	  case NOTE_INSN_CALL_ARG_LOCATION:
	    note_flds = create_field (note_flds, rtx_tp, "rt_rtx");
	    break;

	  default:
	    note_flds = create_field (note_flds, scalar_tp, "rt_int");
	    break;
	  }
	/* NOTE_INSN_MAX is used as the default field for line
	   number notes.  */
	if (c == NOTE_INSN_MAX)
	  note_flds->opt = 
	    create_string_option (nodot, "default", "");
	else
	  note_flds->opt = 
	    create_string_option (nodot, "tag", note_insn_name[c]);
      }
    note_union_tp = new_structure ("rtx_def_note_subunion", TYPE_UNION,
				   &lexer_line, note_flds, NULL, NULL);
  }
  /* Create a type to represent the various forms of SYMBOL_REF_DATA.  */
  {
    pair_p sym_flds;
    sym_flds = create_field (NULL, tree_tp, "rt_tree");
    sym_flds->opt = create_string_option (nodot, "default", "");
    sym_flds = create_field (sym_flds, constant_tp, "rt_constant");
    sym_flds->opt = create_string_option (nodot, "tag", "1");
    symbol_union_tp = new_structure ("rtx_def_symbol_subunion", TYPE_UNION,
				     &lexer_line, sym_flds, NULL, NULL);
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
	      else if (i == CODE_LABEL && aindex == 5)
		t = scalar_tp, subname = "rt_int";
	      else if (i == CODE_LABEL && aindex == 4)
		t = rtx_tp, subname = "rt_rtx";
	      else if (i == LABEL_REF && (aindex == 1 || aindex == 2))
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
	      else if (i == JUMP_TABLE_DATA && aindex >= 5)
		t = scalar_tp, subname = "rt_int";
	      else if (i == BARRIER && aindex >= 3)
		t = scalar_tp, subname = "rt_int";
	      else if (i == ENTRY_VALUE && aindex == 0)
		t = rtx_tp, subname = "rt_rtx";
	      else
		{
		  error_at_line 
		    (&lexer_line,
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

	    case 'B':
	      t = basic_block_tp;
	      subname = "rt_bb";
	      break;

	    default:
	      error_at_line
		(&lexer_line,
		 "rtx type `%s' has `%c' in position %lu, can't handle",
		 rtx_name[i], rtx_format[i][aindex],
		 (unsigned long) aindex);
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
	    subfields->opt =
	      create_string_option (subfields->opt, "desc",
				    "NOTE_KIND (&%0)");
	  if (t == symbol_union_tp)
	    subfields->opt = 
	      create_string_option (subfields->opt, "desc",
				    "CONSTANT_POOL_ADDRESS_P (&%0)");
	}

      if (i == SYMBOL_REF)
	{
	  /* Add the "block_sym" field if SYMBOL_REF_HAS_BLOCK_INFO_P
	     holds.  */
	  type_p field_tp = find_structure ("block_symbol", TYPE_STRUCT);
	  subfields
	    = create_optional_field (subfields, field_tp, "block_sym",
				     "SYMBOL_REF_HAS_BLOCK_INFO_P (&%0)");
	}

      sname = xasprintf ("rtx_def_%s", rtx_name[i]);
      substruct = new_structure (sname, TYPE_STRUCT, &lexer_line, subfields,
				 NULL, NULL);

      ftag = xstrdup (rtx_name[i]);
      for (nmindex = 0; nmindex < strlen (ftag); nmindex++)
	ftag[nmindex] = TOUPPER (ftag[nmindex]);
      flds = create_field (flds, substruct, "");
      flds->opt = create_string_option (nodot, "tag", ftag);
    }
  return new_structure ("rtx_def_subunion", TYPE_UNION, &lexer_line, flds,
			nodot, NULL);
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

  nodot = create_string_option (NULL, "dot", "");

  flds = create_field (NULL, t, "");
  flds->opt = create_string_option (nodot, "length",
				    "TREE_OPERAND_LENGTH ((tree) &%0)");
  flds->opt = create_string_option (flds->opt, "default", "");

  return new_structure ("tree_exp_subunion", TYPE_UNION, &lexer_line, flds,
			nodot, NULL);
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
      {
	if (length_p)
	  error_at_line (&lexer_line, "duplicate `%s' option", opt->name);
	if (t->u.p->kind == TYPE_SCALAR || t->u.p->kind == TYPE_STRING)
	  {
	    error_at_line (&lexer_line,
			   "option `%s' may not be applied to "
			   "arrays of atomic types", opt->name);
	  }
	length_p = 1;
      }
    else if ((strcmp (opt->name, "param_is") == 0
	      || (strncmp (opt->name, "param", 5) == 0
		  && ISDIGIT (opt->name[5])
		  && strcmp (opt->name + 6, "_is") == 0))
	     && opt->kind == OPTION_TYPE)
      {
	int num = ISDIGIT (opt->name[5]) ? opt->name[5] - '0' : 0;

	if (!union_or_struct_p (t)
	    && (t->kind != TYPE_POINTER || !union_or_struct_p (t->u.p)))
	  {
	    error_at_line (&lexer_line,
			   "option `%s' may only be applied to structures or structure pointers",
			   opt->name);
	    return t;
	  }

	params_p = 1;
	if (params[num] != NULL)
	  error_at_line (&lexer_line, "duplicate `%s' option", opt->name);
	if (!ISDIGIT (opt->name[5]))
	  params[num] = create_pointer (opt->info.type);
	else
	  params[num] = opt->info.type;
      }
    else if (strcmp (opt->name, "special") == 0
	     && opt->kind == OPTION_STRING)
      {
	const char *special_name = opt->info.string;
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

  if (!length_p
      && pointer_p && t->u.p->kind == TYPE_SCALAR && t->u.p->u.scalar_is_char)
    return &string_type;
  if (t->kind == TYPE_ARRAY && t->u.a.p->kind == TYPE_POINTER
      && t->u.a.p->u.p->kind == TYPE_SCALAR
      && t->u.a.p->u.p->u.scalar_is_char)
    return create_array (&string_type, t->u.a.len);

  return t;
}


static void set_gc_used_type (type_p, enum gc_used_enum, type_p *,
			      bool = false);
static void set_gc_used (pair_p);

/* Handle OPT for set_gc_used_type.  */

static void
process_gc_options (options_p opt, enum gc_used_enum level, int *maybe_undef,
		    int *pass_param, int *length, int *skip,
		    type_p *nested_ptr)
{
  options_p o;
  for (o = opt; o; o = o->next)
    if (strcmp (o->name, "ptr_alias") == 0 && level == GC_POINTED_TO
	&& o->kind == OPTION_TYPE)
      set_gc_used_type (o->info.type,
			GC_POINTED_TO, NULL);
    else if (strcmp (o->name, "maybe_undef") == 0)
      *maybe_undef = 1;
    else if (strcmp (o->name, "use_params") == 0)
      *pass_param = 1;
    else if (strcmp (o->name, "length") == 0)
      *length = 1;
    else if (strcmp (o->name, "skip") == 0)
      *skip = 1;
    else if (strcmp (o->name, "nested_ptr") == 0
	     && o->kind == OPTION_NESTED)
      *nested_ptr = ((const struct nested_ptr_data *) o->info.nested)->type;
}


/* Set the gc_used field of T to LEVEL, and handle the types it references.

   If ALLOWED_UNDEFINED_TYPES is true, types of kind TYPE_UNDEFINED
   are set to GC_UNUSED.  Otherwise, an error is emitted for
   TYPE_UNDEFINED types.  This is used to support user-defined
   template types with non-type arguments.

   For instance, when we parse a template type with enum arguments
   (e.g. MyType<AnotherType, EnumValue>), the parser created two
   artificial fields for 'MyType', one for 'AnotherType', the other
   one for 'EnumValue'.

   At the time that we parse this type we don't know that 'EnumValue'
   is really an enum value, so the parser creates a TYPE_UNDEFINED
   type for it.  Since 'EnumValue' is never resolved to a known
   structure, it will stay with TYPE_UNDEFINED.

   Since 'MyType' is a TYPE_USER_STRUCT, we can simply ignore
   'EnumValue'.  Generating marking code for it would cause
   compilation failures since the marking routines assumes that
   'EnumValue' is a type.  */

static void
set_gc_used_type (type_p t, enum gc_used_enum level, type_p param[NUM_PARAM],
		  bool allow_undefined_types)
{
  if (t->gc_used >= level)
    return;

  t->gc_used = level;

  switch (t->kind)
    {
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_USER_STRUCT:
      {
	pair_p f;
	int dummy;
	type_p dummy2;
	bool allow_undefined_field_types = (t->kind == TYPE_USER_STRUCT);

	process_gc_options (t->u.s.opt, level, &dummy, &dummy, &dummy, &dummy,
			    &dummy2);

	if (t->u.s.base_class)
	  set_gc_used_type (t->u.s.base_class, level, param,
			    allow_undefined_types);
	/* Anything pointing to a base class might actually be pointing
	   to a subclass.  */
	for (type_p subclass = t->u.s.first_subclass; subclass;
	     subclass = subclass->u.s.next_sibling_class)
	  set_gc_used_type (subclass, level, param,
			    allow_undefined_types);

	FOR_ALL_INHERITED_FIELDS(t, f)
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
	      ;			/* target type is not used through this field */
	    else
	      set_gc_used_type (f->type, GC_USED, pass_param ? param : NULL,
				allow_undefined_field_types);
	  }
	break;
      }

    case TYPE_UNDEFINED:
      if (level > GC_UNUSED)
	{
	  if (!allow_undefined_types)
	    error_at_line (&t->u.s.line, "undefined type `%s'", t->u.s.tag);
	  t->gc_used = GC_UNUSED;
	}
      break;

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
  int nbvars = 0;
  pair_p p;
  for (p = variables; p; p = p->next)
    {
      set_gc_used_type (p->type, GC_USED, NULL);
      nbvars++;
    };
  if (verbosity_level >= 2)
    printf ("%s used %d GTY-ed variables\n", progname, nbvars);
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
    "   Copyright (C) 2004-2013 Free Software Foundation, Inc.\n",
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
  if (s == NULL || (int) slength < 0)
    fatal ("out of memory");
  va_end (ap);

  if (o->bufused + slength > o->buflength)
    {
      size_t new_len = o->buflength;
      if (new_len == 0)
	new_len = 1024;
      do
	{
	  new_len *= 2;
	}
      while (o->bufused + slength >= new_len);
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
    static const char *const ifiles[] = {
      "config.h", "system.h", "coretypes.h", "tm.h",
      "hashtab.h", "splay-tree.h", "obstack.h", "bitmap.h", "input.h",
      "tree.h", "rtl.h", "function.h", "insn-config.h", "expr.h",
      "hard-reg-set.h", "basic-block.h", "cselib.h", "insn-addr.h",
      "optabs.h", "libfuncs.h", "debug.h", "ggc.h", "cgraph.h",
      "pointer-set.h", "hash-table.h", "vec.h", "ggc.h", "basic-block.h",
      "tree-ssa-alias.h", "internal-fn.h", "gimple-fold.h", "tree-eh.h",
      "gimple-expr.h", "is-a.h",
      "gimple.h", "gimple-iterator.h", "gimple-ssa.h", "tree-cfg.h",
      "tree-phinodes.h", "ssa-iterators.h", "stringpool.h", "tree-ssanames.h",
      "tree-ssa-loop.h", "tree-ssa-loop-ivopts.h", "tree-ssa-loop-manip.h",
      "tree-ssa-loop-niter.h", "tree-into-ssa.h", "tree-dfa.h", 
      "tree-ssa.h", "reload.h", "cpp-id-data.h", "tree-chrec.h",
      "except.h", "output.h",  "cfgloop.h",
      "target.h", "ipa-prop.h", "lto-streamer.h", "target-globals.h",
      "ipa-inline.h", "dwarf2out.h", NULL
    };
    const char *const *ifp;
    outf_p gtype_desc_c;

    gtype_desc_c = create_file ("GCC", "gtype-desc.c");
    for (ifp = ifiles; *ifp; ifp++)
      oprintf (gtype_desc_c, "#include \"%s\"\n", *ifp);

    /* Make sure we handle "cfun" specially.  */
    oprintf (gtype_desc_c, "\n/* See definition in function.h.  */\n");
    oprintf (gtype_desc_c, "#undef cfun\n");

    oprintf (gtype_desc_c,
	     "\n"
	     "/* Types with a \"gcc::\" namespace have it stripped\n"
	     "   during gengtype parsing.  Provide a \"using\" directive\n"
	     "   to ensure that the fully-qualified types are found.  */\n"
	     "using namespace gcc;\n");
  }
}

/* For INPF an input file, return the real basename of INPF, with all
   the directory components skipped.  */

static const char *
get_file_realbasename (const input_file *inpf)
{
  return lbasename (get_input_file_name (inpf));
}

/* For INPF a filename, return the relative path to INPF from
   $(srcdir) if the latter is a prefix in INPF, NULL otherwise.  */

const char *
get_file_srcdir_relative_path (const input_file *inpf)
{
  const char *f = get_input_file_name (inpf);
  if (strlen (f) > srcdir_len
      && IS_DIR_SEPARATOR (f[srcdir_len])
      && strncmp (f, srcdir, srcdir_len) == 0)
    return f + srcdir_len + 1;
  else
    return NULL;
}

/*  For INPF an input_file, return the relative path to INPF from
    $(srcdir) if the latter is a prefix in INPF, or the real basename
    of INPF otherwise. */

static const char *
get_file_basename (const input_file *inpf)
{
  const char *srcdir_path = get_file_srcdir_relative_path (inpf);

  return (srcdir_path != NULL) ? srcdir_path : get_file_realbasename (inpf);
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
      const char *langdir = lang_dir_names[lang_index];
      size_t langdir_len = strlen (langdir);

      if (f_len > langdir_len
	  && IS_DIR_SEPARATOR (f[langdir_len])
	  && memcmp (f, langdir, langdir_len) == 0)
	return lang_index;
    }

  return -1;
}

/* For INPF an input file, return the name of language directory where
   F is located, if any, NULL otherwise.  */

static const char *
get_file_langdir (const input_file *inpf)
{
  /* Get the relative path to INPF from $(srcdir) and find the
     language by comparing the prefix with language directory names.
     If INPF is not even srcdir relative, no point in looking
     further.  */

  int lang_index;
  const char *srcdir_relative_path = get_file_srcdir_relative_path (inpf);
  const char *r;

  if (!srcdir_relative_path)
    return NULL;

  lang_index = get_prefix_langdir_index (srcdir_relative_path);
  if (lang_index < 0 && strncmp (srcdir_relative_path, "c-family", 8) == 0)
    r = "c-family";
  else if (lang_index >= 0)
    r = lang_dir_names[lang_index];
  else
    r = NULL;

  return r;
}

/* The gt- output file name for INPF.  */

static const char *
get_file_gtfilename (const input_file *inpf)
{
  /* Cook up an initial version of the gt- file name from the file real
     basename and the language name, if any.  */

  const char *basename = get_file_realbasename (inpf);
  const char *langdir = get_file_langdir (inpf);

  char *result =
    (langdir ? xasprintf ("gt-%s-%s", langdir, basename)
     : xasprintf ("gt-%s", basename));

  /* Then replace all non alphanumerics characters by '-' and change the
     extension to ".h".  We expect the input filename extension was at least
     one character long.  */

  char *s = result;

  for (; *s != '.'; s++)
    if (!ISALNUM (*s) && *s != '-')
      *s = '-';

  memcpy (s, ".h", sizeof (".h"));

  return result;
}

/* Each input_file has its associated output file outf_p.  The
   association is computed by the function
   get_output_file_with_visibility.  The associated file is cached
   inside input_file in its inpoutf field, so is really computed only
   once.  Associated output file paths (i.e. output_name-s) are
   computed by a rule based regexp machinery, using the files_rules
   array of struct file_rule_st.  A for_name is also computed, giving
   the source file name for which the output_file is generated; it is
   often the last component of the input_file path.  */


/*
 Regexpr machinery to compute the output_name and for_name-s of each
 input_file.  We have a sequence of file rules which gives the POSIX
 extended regular expression to match an input file path, and two
 transformed strings for the corresponding output_name and the
 corresponding for_name.  The transformed string contain dollars: $0
 is replaced by the entire match, $1 is replaced by the substring
 matching the first parenthesis in the regexp, etc.  And $$ is replaced
 by a single verbatim dollar.  The rule order is important.  The
 general case is last, and the particular cases should come before.
 An action routine can, when needed, update the out_name & for_name
 and/or return the appropriate output file.  It is invoked only when a
 rule is triggered.  When a rule is triggered, the output_name and
 for_name are computed using their transform string in while $$, $0,
 $1, ... are suitably replaced.  If there is an action, it is called.
 In some few cases, the action can directly return the outf_p, but
 usually it just updates the output_name and for_name so should free
 them before replacing them.  The get_output_file_with_visibility
 function creates an outf_p only once per each output_name, so it
 scans the output_files list for previously seen output file names.
 */

/* Signature of actions in file rules.  */
typedef outf_p (frul_actionrout_t) (input_file*, char**, char**);


struct file_rule_st {
  const char* frul_srcexpr;	/* Source string for regexp.  */
  int frul_rflags;		/* Flags passed to regcomp, usually
				 * REG_EXTENDED.  */
  regex_t* frul_re;		/* Compiled regular expression
				   obtained by regcomp.  */
  const char* frul_tr_out;	/* Transformation string for making
				 * the output_name, with $1 ... $9 for
				 * subpatterns and $0 for the whole
				 * matched filename.  */
  const char* frul_tr_for;	/* Tranformation string for making the
				   for_name.  */
  frul_actionrout_t* frul_action; /* The action, if non null, is
				   * called once the rule matches, on
				   * the transformed out_name &
				   * for_name.  It could change them
				   * and/or give the output file.  */
};

/* File rule action handling *.h files.  */
static outf_p header_dot_h_frul (input_file*, char**, char**);

/* File rule action handling *.c files.  */
static outf_p source_dot_c_frul (input_file*, char**, char**);

#define NULL_REGEX (regex_t*)0

/* The prefix in our regexp-s matching the directory.  */
#define DIR_PREFIX_REGEX "^(([^/]*/)*)"

#define NULL_FRULACT (frul_actionrout_t*)0

/* The array of our rules governing file name generation.  Rules order
   matters, so change with extreme care!  */

struct file_rule_st files_rules[] = {
  /* The general rule assumes that files in subdirectories belong to a
     particular front-end, and files not in subdirectories are shared.
     The following rules deal with exceptions - files that are in
     subdirectories and yet are shared, and files that are top-level,
     but are not shared.  */

  /* the c-family/ source directory is special.  */
  { DIR_PREFIX_REGEX "c-family/([[:alnum:]_-]*)\\.c$",
    REG_EXTENDED, NULL_REGEX,
    "gt-c-family-$3.h", "c-family/$3.c", NULL_FRULACT},

  { DIR_PREFIX_REGEX "c-family/([[:alnum:]_-]*)\\.h$",
    REG_EXTENDED, NULL_REGEX,
    "gt-c-family-$3.h", "c-family/$3.h", NULL_FRULACT},

  /* Both c-lang.h & c-tree.h gives gt-c-c-decl.h for c-decl.c !  */
  { DIR_PREFIX_REGEX "c/c-lang\\.h$",
    REG_EXTENDED, NULL_REGEX, "gt-c-c-decl.h", "c/c-decl.c", NULL_FRULACT},

  { DIR_PREFIX_REGEX "c/c-tree\\.h$",
    REG_EXTENDED, NULL_REGEX, "gt-c-c-decl.h", "c/c-decl.c", NULL_FRULACT},

  /* cp/cp-tree.h gives gt-cp-tree.h for cp/tree.c !  */
  { DIR_PREFIX_REGEX "cp/cp-tree\\.h$",
    REG_EXTENDED, NULL_REGEX,
    "gt-cp-tree.h", "cp/tree.c", NULL_FRULACT },

  /* cp/decl.h & cp/decl.c gives gt-cp-decl.h for cp/decl.c !  */
  { DIR_PREFIX_REGEX "cp/decl\\.[ch]$",
    REG_EXTENDED, NULL_REGEX,
    "gt-cp-decl.h", "cp/decl.c", NULL_FRULACT },

  /* cp/name-lookup.h gives gt-cp-name-lookup.h for cp/name-lookup.c !  */
  { DIR_PREFIX_REGEX "cp/name-lookup\\.h$",
    REG_EXTENDED, NULL_REGEX,
    "gt-cp-name-lookup.h", "cp/name-lookup.c", NULL_FRULACT },

  /* cp/parser.h gives gt-cp-parser.h for cp/parser.c !  */
  { DIR_PREFIX_REGEX "cp/parser\\.h$",
    REG_EXTENDED, NULL_REGEX,
    "gt-cp-parser.h", "cp/parser.c", NULL_FRULACT },

  /* objc/objc-act.h gives gt-objc-objc-act.h for objc/objc-act.c !  */
  { DIR_PREFIX_REGEX "objc/objc-act\\.h$",
    REG_EXTENDED, NULL_REGEX,
    "gt-objc-objc-act.h", "objc/objc-act.c", NULL_FRULACT },

  /* objc/objc-map.h gives gt-objc-objc-map.h for objc/objc-map.c !  */
  { DIR_PREFIX_REGEX "objc/objc-map\\.h$",
    REG_EXTENDED, NULL_REGEX,
    "gt-objc-objc-map.h", "objc/objc-map.c", NULL_FRULACT },

  /* General cases.  For header *.h and source *.c or *.cc files, we
   * need special actions to handle the language.  */

  /* Source *.c files are using get_file_gtfilename to compute their
     output_name and get_file_basename to compute their for_name
     through the source_dot_c_frul action.  */
  { DIR_PREFIX_REGEX "([[:alnum:]_-]*)\\.c$",
    REG_EXTENDED, NULL_REGEX, "gt-$3.h", "$3.c", source_dot_c_frul},

  /* Source *.cc files are using get_file_gtfilename to compute their
     output_name and get_file_basename to compute their for_name
     through the source_dot_c_frul action.  */
  { DIR_PREFIX_REGEX "([[:alnum:]_-]*)\\.cc$",
    REG_EXTENDED, NULL_REGEX, "gt-$3.h", "$3.cc", source_dot_c_frul},

  /* Common header files get "gtype-desc.c" as their output_name,
   * while language specific header files are handled specially.  So
   * we need the header_dot_h_frul action.  */
  { DIR_PREFIX_REGEX "([[:alnum:]_-]*)\\.h$",
    REG_EXTENDED, NULL_REGEX, "gt-$3.h", "$3.h", header_dot_h_frul},

  { DIR_PREFIX_REGEX "([[:alnum:]_-]*)\\.in$",
    REG_EXTENDED, NULL_REGEX, "gt-$3.h", "$3.in", NULL_FRULACT},

  /* Mandatory null last entry signaling end of rules.  */
  {NULL, 0, NULL_REGEX, NULL, NULL, NULL_FRULACT}
};

/* Special file rules action for handling *.h header files.  It gives
   "gtype-desc.c" for common headers and corresponding output
   files for language-specific header files.  */
static outf_p
header_dot_h_frul (input_file* inpf, char**poutname,
		   char**pforname ATTRIBUTE_UNUSED)
{
  const char *basename = 0;
  int lang_index = 0;
  DBGPRINTF ("inpf %p inpname %s outname %s forname %s",
	     (void*) inpf, get_input_file_name (inpf),
	     *poutname, *pforname);
  basename = get_file_basename (inpf);
  lang_index = get_prefix_langdir_index (basename);
  DBGPRINTF ("basename %s lang_index %d", basename, lang_index);

  if (lang_index >= 0)
    {
      /* The header is language specific.  Given output_name &
	 for_name remains unchanged.  The base_files array gives the
	 outf_p.  */
      DBGPRINTF ("header_dot_h found language specific @ %p '%s'",
		 (void*) base_files[lang_index],
		 (base_files[lang_index])->name);
      return base_files[lang_index];
    }
  else
    {
      /* The header is common to all front-end languages.  So
	 output_name is "gtype-desc.c" file.  The calling function
	 get_output_file_with_visibility will find its outf_p.  */
      free (*poutname);
      *poutname = xstrdup ("gtype-desc.c");
      DBGPRINTF ("special 'gtype-desc.c' for inpname %s",
		 get_input_file_name (inpf));
      return NULL;
    }
}


/* Special file rules action for handling *.c source files using
 * get_file_gtfilename to compute their output_name and
 * get_file_basename to compute their for_name.  The output_name is
 * gt-<LANG>-<BASE>.h for language specific source files, and
 * gt-<BASE>.h for common source files.  */
static outf_p
source_dot_c_frul (input_file* inpf, char**poutname, char**pforname)
{
  char *newbasename = CONST_CAST (char*, get_file_basename (inpf));
  char *newoutname = CONST_CAST (char*, get_file_gtfilename (inpf));
  DBGPRINTF ("inpf %p inpname %s original outname %s forname %s",
	     (void*) inpf, get_input_file_name (inpf),
	     *poutname, *pforname);
  DBGPRINTF ("newoutname %s", newoutname);
  DBGPRINTF ("newbasename %s", newbasename);
  free (*poutname);
  free (*pforname);
  *poutname = newoutname;
  *pforname = newbasename;
  return NULL;
}

/* Utility function for get_output_file_with_visibility which returns
 * a malloc-ed substituted string using TRS on matching of the FILNAM
 * file name, using the PMATCH array.  */
static char*
matching_file_name_substitute (const char *filnam, regmatch_t pmatch[10],
			       const char *trs)
{
  struct obstack str_obstack;
  char *str = NULL;
  char *rawstr = NULL;
  const char *pt = NULL;
  DBGPRINTF ("filnam %s", filnam);
  obstack_init (&str_obstack);
  for (pt = trs; *pt; pt++) {
    char c = *pt;
    if (c == '$')
      {
	if (pt[1] == '$')
	  {
	    /* A double dollar $$ is substituted by a single verbatim
	       dollar, but who really uses dollar signs in file
	       paths? */
	    obstack_1grow (&str_obstack, '$');
	  }
	else if (ISDIGIT (pt[1]))
	  {
	    /* Handle $0 $1 ... $9 by appropriate substitution.  */
	    int dolnum = pt[1] - '0';
	    int so = pmatch[dolnum].rm_so;
	    int eo = pmatch[dolnum].rm_eo;
	    DBGPRINTF ("so=%d eo=%d dolnum=%d", so, eo, dolnum);
	    if (so>=0 && eo>=so)
	      obstack_grow (&str_obstack, filnam + so, eo - so);
	  }
	else
	  {
	    /* This can happen only when files_rules is buggy! */
	    gcc_unreachable ();
	  }
	/* Always skip the character after the dollar.  */
	pt++;
      }
    else
      obstack_1grow (&str_obstack, c);
  }
  obstack_1grow (&str_obstack, '\0');
  rawstr = XOBFINISH (&str_obstack, char *);
  str = xstrdup (rawstr);
  obstack_free (&str_obstack, NULL);
  DBGPRINTF ("matched replacement %s", str);
  rawstr = NULL;
  return str;
}


/* An output file, suitable for definitions, that can see declarations
   made in INPF and is linked into every language that uses INPF.
   Since the result is cached inside INPF, that argument cannot be
   declared constant, but is "almost" constant. */

outf_p
get_output_file_with_visibility (input_file *inpf)
{
  outf_p r;
  char *for_name = NULL;
  char *output_name = NULL;
  const char* inpfname;

  /* This can happen when we need a file with visibility on a
     structure that we've never seen.  We have to just hope that it's
     globally visible.  */
  if (inpf == NULL)
    inpf = system_h_file;

  /* The result is cached in INPF, so return it if already known.  */
  if (inpf->inpoutf)
    return inpf->inpoutf;

  /* In plugin mode, return NULL unless the input_file is one of the
     plugin_files.  */
  if (plugin_files)
    {
      size_t i;
      for (i = 0; i < nb_plugin_files; i++)
	if (inpf == plugin_files[i]) 
	  {
	    inpf->inpoutf = plugin_output;
	    return plugin_output;
	  }

      return NULL;
    }

  inpfname = get_input_file_name (inpf);

  /* Try each rule in sequence in files_rules until one is triggered. */
  {
    int rulix = 0;
    DBGPRINTF ("passing input file @ %p named %s through the files_rules",
	       (void*) inpf, inpfname);

    for (; files_rules[rulix].frul_srcexpr != NULL; rulix++)
      {
	DBGPRINTF ("rulix#%d srcexpr %s",
		   rulix, files_rules[rulix].frul_srcexpr);

	if (!files_rules[rulix].frul_re)
	  {
	    /* Compile the regexpr lazily.  */
	    int err = 0;
	    files_rules[rulix].frul_re = XCNEW (regex_t);
	    err = regcomp (files_rules[rulix].frul_re,
			   files_rules[rulix].frul_srcexpr,
			   files_rules[rulix].frul_rflags);
	    if (err)
	      {
		/* The regular expression compilation fails only when
		   file_rules is buggy.  */
		gcc_unreachable ();
	      }
	  }

	output_name = NULL;
	for_name = NULL;

	/* Match the regexpr and trigger the rule if matched.  */
	{
	  /* We have exactly ten pmatch-s, one for each $0, $1, $2,
	     $3, ... $9.  */
	  regmatch_t pmatch[10];
	  memset (pmatch, 0, sizeof (pmatch));
	  if (!regexec (files_rules[rulix].frul_re,
			inpfname, 10, pmatch, 0))
	    {
	      DBGPRINTF ("input @ %p filename %s matched rulix#%d pattern %s",
			 (void*) inpf, inpfname, rulix,
			 files_rules[rulix].frul_srcexpr);
	      for_name =
		matching_file_name_substitute (inpfname, pmatch,
					       files_rules[rulix].frul_tr_for);
	      DBGPRINTF ("for_name %s", for_name);
	      output_name =
		matching_file_name_substitute (inpfname, pmatch,
					       files_rules[rulix].frul_tr_out);
	      DBGPRINTF ("output_name %s", output_name);
	      if (files_rules[rulix].frul_action)
		{
		  /* Invoke our action routine.  */
		  outf_p of = NULL;
		  DBGPRINTF ("before action rulix#%d output_name %s for_name %s",
			     rulix, output_name, for_name);
		  of =
		    (files_rules[rulix].frul_action) (inpf,
						      &output_name, &for_name);
		  DBGPRINTF ("after action rulix#%d of=%p output_name %s for_name %s",
			     rulix, (void*)of, output_name, for_name);
		  /* If the action routine returned something, give it back
		     immediately and cache it in inpf.  */
		  if (of)
		    {
		      inpf->inpoutf = of;
		      return of;
		    }
		}
	      /* The rule matched, and had no action, or that action did
		 not return any output file but could have changed the
		 output_name or for_name.  We break out of the loop on the
		 files_rules.  */
	      break;
	    }
	  else
	    {
	      /* The regexpr did not match.  */
	      DBGPRINTF ("rulix#%d did not match %s pattern %s",
			 rulix, inpfname, files_rules[rulix].frul_srcexpr);
	      continue;
	    }
	}
      }
  }
  if (!output_name || !for_name)
    {
      /* This should not be possible, and could only happen if the
	 files_rules is incomplete or buggy.  */
      fatal ("failed to compute output name for %s", inpfname);
    }

  /* Look through to see if we've ever seen this output filename
     before.  If found, cache the result in inpf.  */
  for (r = output_files; r; r = r->next)
    if (filename_cmp (r->name, output_name) == 0)
      {
	inpf->inpoutf = r;
	DBGPRINTF ("found r @ %p for output_name %s for_name %s", (void*)r,
		   output_name, for_name);
	return r;
      }

  /* If not found, create it, and cache it in inpf.  */
  r = create_file (for_name, output_name);

  gcc_assert (r && r->name);
  DBGPRINTF ("created r @ %p for output_name %s for_name %s", (void*) r,
	     output_name, for_name);
  inpf->inpoutf = r;
  return r;


}

/* The name of an output file, suitable for definitions, that can see
   declarations made in INPF and is linked into every language that
   uses INPF.  */

const char *
get_output_file_name (input_file* inpf)
{
  outf_p o = get_output_file_with_visibility (inpf);
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
  if (equal && EOF != fgetc (newfile))
    equal = false;
  fclose (newfile);
  return equal;
}

/* Copy the output to its final destination,
   but don't unnecessarily change modification times.  */

static void
close_output_files (void)
{
  int nbwrittenfiles = 0;
  outf_p of;

  for (of = output_files; of; of = of->next)
    {
      if (!is_file_equal (of))
	{
	  FILE *newfile = NULL;
	  char *backupname = NULL;
	  /* Back up the old version of the output file gt-FOO.c as
	     BACKUPDIR/gt-FOO.c~ if we have a backup directory.  */
	  if (backup_dir)
	    {
	      backupname = concat (backup_dir, "/",
				   lbasename (of->name), "~", NULL);
	      if (!access (of->name, F_OK) && rename (of->name, backupname))
		fatal ("failed to back up %s as %s: %s",
		       of->name, backupname, xstrerror (errno));
	    }

	  newfile = fopen (of->name, "w");
	  if (newfile == NULL)
	    fatal ("opening output file %s: %s", of->name, xstrerror (errno));
	  if (fwrite (of->buf, 1, of->bufused, newfile) != of->bufused)
	    fatal ("writing output file %s: %s", of->name, xstrerror (errno));
	  if (fclose (newfile) != 0)
	    fatal ("closing output file %s: %s", of->name, xstrerror (errno));
	  nbwrittenfiles++;
	  if (verbosity_level >= 2 && backupname)
	    printf ("%s wrote #%-3d %s backed-up in %s\n",
		    progname, nbwrittenfiles, of->name, backupname);
	  else if (verbosity_level >= 1)
	    printf ("%s write #%-3d %s\n", progname, nbwrittenfiles, of->name);
	  free (backupname);
	}
      else 
	{ 
	  /* output file remains unchanged. */
	  if (verbosity_level >= 2)
	    printf ("%s keep %s\n", progname, of->name);
	}
      free (of->buf);
      of->buf = NULL;
      of->bufused = of->buflength = 0;
    }
  if (verbosity_level >= 1)
    printf ("%s wrote %d files.\n", progname, nbwrittenfiles);
}

struct flist
{
  struct flist *next;
  int started_p;
  const input_file* file;
  outf_p f;
};

struct walk_type_data;

/* For scalars and strings, given the item in 'val'.
   For structures, given a pointer to the item in 'val'.
   For misc. pointers, given the item in 'val'.
*/
typedef void (*process_field_fn) (type_p f, const struct walk_type_data * p);
typedef void (*func_name_fn) (type_p s, const struct walk_type_data * p);

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
static void write_func_for_structure (type_p orig_s, type_p s, type_p *param,
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
  (const_type_p orig_s, type_p s, type_p *param);
static void write_local (outf_p output_header,
			 type_p structures, type_p param_structs);
static int contains_scalar_p (type_p t);
static void put_mangled_filename (outf_p, const input_file *);
static void finish_root_table (struct flist *flp, const char *pfx,
			       const char *tname, const char *lastname,
			       const char *name);
static void write_root (outf_p, pair_p, type_p, const char *, int,
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
  const struct fileloc *line;
  lang_bitmap bitmap;
  type_p *param;
  int used_length;
  type_p orig_s;
  const char *reorder_fn;
  bool needs_cast_p;
  bool fn_wants_lvalue;
  bool in_record_p;
  int loopcounter;
  bool in_ptr_field;
  bool have_this_obj;
};


/* Given a string TYPE_NAME, representing a C++ typename, return a valid
   pre-processor identifier to use in a #define directive.  This replaces
   special characters used in C++ identifiers like '>', '<' and ':' with
   '_'.

   If no C++ special characters are found in TYPE_NAME, return
   TYPE_NAME.  Otherwise, return a copy of TYPE_NAME with the special
   characters replaced with '_'.  In this case, the caller is
   responsible for freeing the allocated string.  */

static const char *
filter_type_name (const char *type_name)
{
  if (strchr (type_name, '<') || strchr (type_name, ':'))
    {
      size_t i;
      char *s = xstrdup (type_name);
      for (i = 0; i < strlen (s); i++)
	if (s[i] == '<' || s[i] == '>' || s[i] == ':' || s[i] == ',')
	  s[i] = '_';
      return s;
    }
  else
    return type_name;
}


/* Print a mangled name representing T to OF.  */

static void
output_mangled_typename (outf_p of, const_type_p t)
{
  if (t == NULL)
    oprintf (of, "Z");
  else
    switch (t->kind)
      {
      case TYPE_NONE:
      case TYPE_UNDEFINED:
	gcc_unreachable ();
	break;
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
      case TYPE_USER_STRUCT:
	{
	  /* For references to classes within an inheritance hierarchy,
	     only ever reference the ultimate base class, since only
	     it will have gt_ functions.  */
	  t = get_ultimate_base_class (t);
	  const char *id_for_tag = filter_type_name (t->u.s.tag);
	  oprintf (of, "%lu%s", (unsigned long) strlen (id_for_tag),
		   id_for_tag);
	  if (id_for_tag != t->u.s.tag)
	    free (CONST_CAST (char *, id_for_tag));
	}
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
    else
      switch (*++p)
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

const char *
get_string_option (options_p opt, const char *key)
{
  for (; opt; opt = opt->next)
    if (strcmp (opt->name, key) == 0)
      return opt->info.string;
  return NULL;
}

/* Machinery for avoiding duplicate tags within switch statements.  */
struct seen_tag
{
  const char *tag;
  struct seen_tag *next;
};

int
already_seen_tag (struct seen_tag *seen_tags, const char *tag)
{
  /* Linear search, so O(n^2), but n is currently small.  */
  while (seen_tags)
    {
      if (!strcmp (seen_tags->tag, tag))
	return 1;
      seen_tags = seen_tags->next;
    }
  /* Not yet seen this tag. */
  return 0;
}

void
mark_tag_as_seen (struct seen_tag **seen_tags, const char *tag)
{
  /* Add to front of linked list. */
  struct seen_tag *new_node = XCNEW (struct seen_tag);
  new_node->tag = tag;
  new_node->next = *seen_tags;
  *seen_tags = new_node;
}

static void
walk_subclasses (type_p base, struct walk_type_data *d,
		 struct seen_tag **seen_tags)
{
  for (type_p sub = base->u.s.first_subclass; sub != NULL;
       sub = sub->u.s.next_sibling_class)
    {
      const char *type_tag = get_string_option (sub->u.s.opt, "tag");
      if (type_tag && !already_seen_tag (*seen_tags, type_tag))
	{
	  mark_tag_as_seen (seen_tags, type_tag);
	  oprintf (d->of, "%*scase %s:\n", d->indent, "", type_tag);
	  d->indent += 2;
	  oprintf (d->of, "%*s{\n", d->indent, "");
	  d->indent += 2;
	  oprintf (d->of, "%*s%s *sub = static_cast <%s *> (x);\n",
		   d->indent, "", sub->u.s.tag, sub->u.s.tag);
	  const char *old_val = d->val;
	  d->val = "(*sub)";
	  walk_type (sub, d);
	  d->val = old_val;
	  d->indent -= 2;
	  oprintf (d->of, "%*s}\n", d->indent, "");
	  oprintf (d->of, "%*sbreak;\n", d->indent, "");
	  d->indent -= 2;
	}
      walk_subclasses (sub, d, seen_tags);
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
  const char *type_tag = NULL;
  int maybe_undef_p = 0;
  int use_param_num = -1;
  int use_params_p = 0;
  int atomic_p = 0;
  options_p oo;
  const struct nested_ptr_data *nested_ptr_d = NULL;

  d->needs_cast_p = false;
  for (oo = d->opt; oo; oo = oo->next)
    if (strcmp (oo->name, "length") == 0 && oo->kind == OPTION_STRING)
      length = oo->info.string;
    else if (strcmp (oo->name, "maybe_undef") == 0)
      maybe_undef_p = 1;
    else if (strncmp (oo->name, "use_param", 9) == 0
	     && (oo->name[9] == '\0' || ISDIGIT (oo->name[9])))
      use_param_num = oo->name[9] == '\0' ? 0 : oo->name[9] - '0';
    else if (strcmp (oo->name, "use_params") == 0)
      use_params_p = 1;
    else if (strcmp (oo->name, "desc") == 0 && oo->kind == OPTION_STRING)
      desc = oo->info.string;
    else if (strcmp (oo->name, "mark_hook") == 0)
      ;
    else if (strcmp (oo->name, "nested_ptr") == 0 
	     && oo->kind == OPTION_NESTED)
      nested_ptr_d = (const struct nested_ptr_data *) oo->info.nested;
    else if (strcmp (oo->name, "dot") == 0)
      ;
    else if (strcmp (oo->name, "tag") == 0)
      type_tag = oo->info.string;
    else if (strcmp (oo->name, "special") == 0)
      ;
    else if (strcmp (oo->name, "skip") == 0)
      ;
    else if (strcmp (oo->name, "atomic") == 0)
      atomic_p = 1;
    else if (strcmp (oo->name, "default") == 0)
      ;
    else if (strcmp (oo->name, "param_is") == 0)
      ;
    else if (strncmp (oo->name, "param", 5) == 0
	     && ISDIGIT (oo->name[5]) && strcmp (oo->name + 6, "_is") == 0)
      ;
    else if (strcmp (oo->name, "chain_next") == 0)
      ;
    else if (strcmp (oo->name, "chain_prev") == 0)
      ;
    else if (strcmp (oo->name, "chain_circular") == 0)
      ;
    else if (strcmp (oo->name, "reorder") == 0)
      ;
    else if (strcmp (oo->name, "variable_size") == 0)
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
      if (!union_or_struct_p (t))
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
	error_at_line (d->line, "no parameter defined for `%s'", d->val);
    }

  if (maybe_undef_p
      && (t->kind != TYPE_POINTER || !union_or_struct_p (t->u.p)))
    {
      error_at_line (d->line,
		     "field `%s' has invalid option `maybe_undef_p'\n",
		     d->val);
      return;
    }

  if (atomic_p && (t->kind != TYPE_POINTER) && (t->kind != TYPE_STRING))
    {
      error_at_line (d->line, "field `%s' has invalid option `atomic'\n", d->val);
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
	d->in_ptr_field = true;
	if (maybe_undef_p && t->u.p->u.s.line.file == NULL)
	  {
	    oprintf (d->of, "%*sgcc_assert (!%s);\n", d->indent, "", d->val);
	    break;
	  }

	/* If a pointer type is marked as "atomic", we process the
	   field itself, but we don't walk the data that they point to.

	   There are two main cases where we walk types: to mark
	   pointers that are reachable, and to relocate pointers when
	   writing a PCH file.  In both cases, an atomic pointer is
	   itself marked or relocated, but the memory that it points
	   to is left untouched.  In the case of PCH, that memory will
	   be read/written unchanged to the PCH file.  */
	if (atomic_p)
	  {
	    oprintf (d->of, "%*sif (%s != NULL) {\n", d->indent, "", d->val);
	    d->indent += 2;
	    d->process_field (t, d);
	    d->indent -= 2;
	    oprintf (d->of, "%*s}\n", d->indent, "");
	    break;
	  }

	if (!length)
	  {
	    if (!union_or_struct_p (t->u.p)
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

		if (!union_or_struct_p (nested_ptr_d->type))
		  {
		    error_at_line (d->line,
				   "field `%s' has invalid "
				   "option `nested_ptr'\n", d->val);
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
			 d->fn_wants_lvalue ? "" : "const ", d->val);
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
	    int loopcounter = d->loopcounter;
	    const char *oldval = d->val;
	    const char *oldprevval3 = d->prev_val[3];
	    char *newval;

	    oprintf (d->of, "%*sif (%s != NULL) {\n", d->indent, "", d->val);
	    d->indent += 2;
	    oprintf (d->of, "%*ssize_t i%d;\n", d->indent, "", loopcounter);
	    oprintf (d->of, "%*sfor (i%d = 0; i%d != (size_t)(", d->indent,
		     "", loopcounter, loopcounter);
	    if (!d->in_record_p)
	      output_escaped_param (d, length, "length");
	    else
	      oprintf (d->of, "l%d", loopcounter);
	    if (d->have_this_obj)
	      /* Try to unswitch loops (see PR53880).  */
	      oprintf (d->of, ") && ((void *)%s == this_obj", oldval);
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
	    d->process_field (t, d);
	    d->indent -= 2;
	    oprintf (d->of, "%*s}\n", d->indent, "");
	  }
	d->in_ptr_field = false;
      }
      break;

    case TYPE_ARRAY:
      {
	int loopcounter;
	const char *oldval = d->val;
	char *newval;

	/* If it's an array of scalars, we optimize by not generating
	   any code.  */
	if (t->u.a.p->kind == TYPE_SCALAR)
	  break;

	if (length)
	  loopcounter = d->loopcounter;
	else
	  loopcounter = d->counter++;

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
	if (!d->in_record_p || !length)
	  {
	    oprintf (d->of, "%*ssize_t l%d = (size_t)(",
		     d->indent, "", loopcounter);
	    if (length)
	      output_escaped_param (d, length, "length");
	    else
	      oprintf (d->of, "%s", t->u.a.len);
	    oprintf (d->of, ");\n");
	  }

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
	const char *struct_mark_hook = NULL;
	const int union_p = t->kind == TYPE_UNION;
	int seen_default_p = 0;
	options_p o;
	int lengths_seen = 0;
	int endcounter;
	bool any_length_seen = false;

	if (!t->u.s.line.file)
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
	  if (!desc && strcmp (o->name, "desc") == 0
	      && o->kind == OPTION_STRING)
	    desc = o->info.string;
	  else if (!struct_mark_hook && strcmp (o->name, "mark_hook") == 0
		   && o->kind == OPTION_STRING)
	    struct_mark_hook = o->info.string;

	if (struct_mark_hook)
	  oprintf (d->of, "%*s%s (&%s);\n",
		   d->indent, "", struct_mark_hook, oldval);

	d->prev_val[2] = oldval;
	d->prev_val[1] = oldprevval2;
	if (union_p)
	  {
	    if (desc == NULL)
	      {
		error_at_line (d->line,
			       "missing `desc' option for union `%s'",
			       t->u.s.tag);
		desc = "1";
	      }
	    oprintf (d->of, "%*sswitch (", d->indent, "");
	    output_escaped_param (d, desc, "desc");
	    oprintf (d->of, ")\n");
	    d->indent += 2;
	    oprintf (d->of, "%*s{\n", d->indent, "");
	  }
	else if (desc)
	  {
	    /* We have a "desc" option on a struct, signifying the
	       base class within a GC-managed inheritance hierarchy.
	       The current code specialcases the base class, then walks
	       into subclasses, recursing into this routine to handle them.
	       This organization requires the base class to have a case in
	       the switch statement, and hence a tag value is mandatory
	       for the base class.   This restriction could be removed, but
	       it would require some restructing of this code.  */
	    if (!type_tag)
	      {
		error_at_line (d->line,
			       "missing `tag' option for type `%s'",
			       t->u.s.tag);
	      }
	    oprintf (d->of, "%*sswitch (", d->indent, "");
	    output_escaped_param (d, desc, "desc");
	    oprintf (d->of, ")\n");
	    d->indent += 2;
	    oprintf (d->of, "%*s{\n", d->indent, "");
	    oprintf (d->of, "%*scase %s:\n", d->indent, "", type_tag);
	    d->indent += 2;
	  }

	FOR_ALL_INHERITED_FIELDS (t, f)
	  {
	    options_p oo;
	    int skip_p = 0;
	    const char *fieldlength = NULL;

	    d->reorder_fn = NULL;
	    for (oo = f->opt; oo; oo = oo->next)
	      if (strcmp (oo->name, "skip") == 0)
		skip_p = 1;
	      else if (strcmp (oo->name, "length") == 0
		       && oo->kind == OPTION_STRING)
		fieldlength = oo->info.string;

	    if (skip_p)
	      continue;
	    if (fieldlength)
	      {
	        lengths_seen++;
		d->counter++;
		if (!union_p)
		  {
		    if (!any_length_seen)
		      {
			oprintf (d->of, "%*s{\n", d->indent, "");
			d->indent += 2;
		      }
		    any_length_seen = true;

		    oprintf (d->of, "%*ssize_t l%d = (size_t)(",
			     d->indent, "", d->counter - 1);
		    output_escaped_param (d, fieldlength, "length");
		    oprintf (d->of, ");\n");
		  }
	      }
	  }
	endcounter = d->counter;

	FOR_ALL_INHERITED_FIELDS (t, f)
	  {
	    options_p oo;
	    const char *dot = ".";
	    const char *tagid = NULL;
	    int skip_p = 0;
	    int default_p = 0;
	    int use_param_p = 0;
	    const char *fieldlength = NULL;
	    char *newval;

	    d->reorder_fn = NULL;
	    for (oo = f->opt; oo; oo = oo->next)
	      if (strcmp (oo->name, "dot") == 0
		  && oo->kind == OPTION_STRING)
		dot = oo->info.string;
	      else if (strcmp (oo->name, "tag") == 0
		       && oo->kind == OPTION_STRING)
		tagid = oo->info.string;
	      else if (strcmp (oo->name, "skip") == 0)
		skip_p = 1;
	      else if (strcmp (oo->name, "default") == 0)
		default_p = 1;
	      else if (strcmp (oo->name, "reorder") == 0
		  && oo->kind == OPTION_STRING)
		d->reorder_fn = oo->info.string;
	      else if (strncmp (oo->name, "use_param", 9) == 0
		       && (oo->name[9] == '\0' || ISDIGIT (oo->name[9])))
		use_param_p = 1;
	      else if (strcmp (oo->name, "length") == 0
		       && oo->kind == OPTION_STRING)
		fieldlength = oo->info.string;

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
	    else if (!union_p && (default_p || tagid))
	      error_at_line (d->line,
			     "can't use `%s' outside a union on field `%s'",
			     default_p ? "default" : "tag", f->name);
	    else if (union_p && !(default_p || tagid)
		     && f->type->kind == TYPE_SCALAR)
	      {
		fprintf (stderr,
			 "%s:%d: warning: field `%s' is missing `tag' or `default' option\n",
			 get_input_file_name (d->line->file), d->line->line, 
			 f->name);
		continue;
	      }
	    else if (union_p && !(default_p || tagid))
	      error_at_line (d->line,
			     "field `%s' is missing `tag' or `default' option",
			     f->name);

	    if (fieldlength)
	      {
		d->loopcounter = endcounter - lengths_seen--;
	      }

	    d->line = &f->line;
	    d->val = newval = xasprintf ("%s%s%s", oldval, dot, f->name);
	    d->opt = f->opt;
	    d->used_length = false;
	    d->in_record_p = !union_p;

	    if (union_p && use_param_p && d->param == NULL)
	      oprintf (d->of, "%*sgcc_unreachable ();\n", d->indent, "");
	    else
	      walk_type (f->type, d);

	    d->in_record_p = false;

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

	if (union_p && !seen_default_p)
	  {
	    oprintf (d->of, "%*sdefault:\n", d->indent, "");
	    oprintf (d->of, "%*s  break;\n", d->indent, "");
	  }

	if (desc && !union_p)
	  {
		oprintf (d->of, "%*sbreak;\n", d->indent, "");
		d->indent -= 2;
          }
	if (union_p)
	  {
	    oprintf (d->of, "%*s}\n", d->indent, "");
	    d->indent -= 2;
	  }
	else if (desc)
	  {
	    /* Add cases to handle subclasses.  */
	    struct seen_tag *tags = NULL;
	    walk_subclasses (t, d, &tags);

	    /* Ensure that if someone forgets a "tag" option that we don't
	       silent fail to traverse that subclass's fields.  */
	    if (!seen_default_p)
	      {
		oprintf (d->of, "%*s/* Unrecognized tag value.  */\n",
			 d->indent, "");
		oprintf (d->of, "%*sdefault: gcc_unreachable (); \n",
			 d->indent, "");
	      }

	    /* End of the switch statement */
	    oprintf (d->of, "%*s}\n", d->indent, "");
	    d->indent -= 2;
	  }
	if (any_length_seen)
	  {
	    d->indent -= 2;
	    oprintf (d->of, "%*s}\n", d->indent, "");
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

    case TYPE_USER_STRUCT:
      d->process_field (t, d);
      break;

    case TYPE_NONE:
    case TYPE_UNDEFINED:
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
    case TYPE_NONE:
    case TYPE_UNDEFINED:
      gcc_unreachable ();
    case TYPE_POINTER:
      oprintf (d->of, "%*s%s (%s%s", d->indent, "",
	       wtd->subfield_marker_routine, cast, d->val);
      if (wtd->param_prefix)
	{
	  if (f->u.p->kind == TYPE_SCALAR)
	    /* The current type is a pointer to a scalar (so not
	       considered like a pointer to instances of user defined
	       types) and we are seeing it; it means we must be even
	       more careful about the second argument of the
	       SUBFIELD_MARKER_ROUTINE call.  That argument must
	       always be the instance of the type for which
	       write_func_for_structure was called - this really is
	       what the function SUBFIELD_MARKER_ROUTINE expects.
	       That is, it must be an instance of the ORIG_S type
	       parameter of write_func_for_structure.  The convention
	       is that that argument must be "x" in that case (as set
	       by write_func_for_structure).  The problem is, we can't
	       count on d->prev_val[3] to be always set to "x" in that
	       case.  Sometimes walk_type can set it to something else
	       (to e.g cooperate with write_array when called from
	       write_roots).  So let's set it to "x" here then.  */
	    oprintf (d->of, ", x");
	  else
	    oprintf (d->of, ", %s", d->prev_val[3]);
	  if (d->orig_s)
	    {
	      oprintf (d->of, ", gt_%s_", wtd->param_prefix);
	      output_mangled_typename (d->of, d->orig_s);
	    }
	  else
	    oprintf (d->of, ", gt_%sa_%s", wtd->param_prefix, d->prev_val[0]);
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
    case TYPE_USER_STRUCT:
      if (f->kind == TYPE_USER_STRUCT && !d->in_ptr_field)
	{
	  /* If F is a user-defined type and the field is not a
	     pointer to the type, then we should not generate the
	     standard pointer-marking code.  All we need to do is call
	     the user-provided marking function to process the fields
	     of F.  */
	  oprintf (d->of, "%*sgt_%sx (&(%s));\n", d->indent, "", wtd->prefix,
		   d->val);
	}
      else
	{
	  oprintf (d->of, "%*sgt_%s_", d->indent, "", wtd->prefix);
	  output_mangled_typename (d->of, f);
	  oprintf (d->of, " (%s%s);\n", cast, d->val);
	  if (d->reorder_fn && wtd->reorder_note_routine)
	    oprintf (d->of, "%*s%s (%s%s, %s%s, %s);\n", d->indent, "",
		     wtd->reorder_note_routine, cast, d->val, cast, d->val,
		     d->reorder_fn);
	}
      break;

    case TYPE_SCALAR:
      break;

    case TYPE_ARRAY:
      gcc_unreachable ();
    }
}

/* Return an output file that is suitable for definitions which can
   reference struct S */

static outf_p
get_output_file_for_structure (const_type_p s, type_p *param)
{
  const input_file *fn;
  int i;

  gcc_assert (union_or_struct_p (s));
  fn = s->u.s.line.file;

  /* This is a hack, and not the good kind either.  */
  for (i = NUM_PARAM - 1; i >= 0; i--)
    if (param && param[i] && param[i]->kind == TYPE_POINTER
	&& union_or_struct_p (param[i]->u.p))
      fn = param[i]->u.p->u.s.line.file;

  /* The call to get_output_file_with_visibility may update fn by
     caching its result inside, so we need the CONST_CAST.  */
  return get_output_file_with_visibility (CONST_CAST (input_file*, fn));
}


/* Returns the specifier keyword for a string or union type S, empty string
   otherwise.  */

static const char *
get_type_specifier (const type_p s)
{
  if (s->kind == TYPE_STRUCT)
    return "struct ";
  else if (s->kind == TYPE_LANG_STRUCT)
    return get_type_specifier (s->u.s.lang_struct);
  else if (s->kind == TYPE_UNION)
    return "union ";
  return "";
}


/* Emits a declaration for type TY (assumed to be a union or a
   structure) on stream OUT.  */

static void
write_type_decl (outf_p out, type_p ty)
{
  if (union_or_struct_p (ty))
    oprintf (out, "%s%s", get_type_specifier (ty), ty->u.s.tag);
  else if (ty->kind == TYPE_SCALAR)
    {
      if (ty->u.scalar_is_char)
	oprintf (out, "const char");
      else
	oprintf (out, "void");
    }
  else if (ty->kind == TYPE_POINTER)
    {
      write_type_decl (out, ty->u.p);
      oprintf (out, " *");
    }
  else if (ty->kind == TYPE_ARRAY)
    {
      write_type_decl (out, ty->u.a.p);
      oprintf (out, " *");
    }
  else if (ty->kind == TYPE_STRING)
    {
      oprintf (out, "const char *");
    }
  else
    gcc_unreachable ();
}


/* Write on OF the name of the marker function for structure S. PREFIX
   is the prefix to use (to distinguish ggc from pch markers).  */

static void
write_marker_function_name (outf_p of, type_p s, const char *prefix)
{
  if (union_or_struct_p (s))
    {
      const char *id_for_tag = filter_type_name (s->u.s.tag);
      oprintf (of, "gt_%sx_%s", prefix, id_for_tag);
      if (id_for_tag != s->u.s.tag)
	free (CONST_CAST (char *, id_for_tag));
    }
  else if (s->kind == TYPE_PARAM_STRUCT)
    {
      oprintf (of, "gt_%s_", prefix);
      output_mangled_typename (of, s);
    }
  else
    gcc_unreachable ();
}

/* Write on OF a user-callable routine to act as an entry point for
   the marking routine for S, generated by write_func_for_structure.
   PREFIX is the prefix to use to distinguish ggc and pch markers.  */

static void
write_user_func_for_structure_ptr (outf_p of, type_p s, const char *prefix)
{
  /* Parameterized structures are not supported in user markers. There
     is no way for the marker function to know which specific type
     to use to generate the call to the void * entry point.  For
     instance, a marker for struct htab may need to call different
     routines to mark the fields, depending on the paramN_is attributes.

     A user-defined marker that accepts 'struct htab' as its argument
     would not know which variant to call. Generating several entry
     points accepting 'struct htab' would cause multiply-defined
     errors during compilation.  */
  gcc_assert (union_or_struct_p (s));

  type_p alias_of = NULL;
  for (options_p opt = s->u.s.opt; opt; opt = opt->next)
    if (strcmp (opt->name, "ptr_alias") == 0)
      {
	/* ALIAS_OF is set if ORIG_S is marked "ptr_alias". This means that
	   we do not generate marking code for ORIG_S here. Instead, a
	   forwarder #define in gtype-desc.h will cause every call to its
	   marker to call the target of this alias.

	   However, we still want to create a user entry code for the
	   aliased type. So, if ALIAS_OF is set, we only generate the
	   user-callable marker function.  */
	alias_of = opt->info.type;
	break;
      }

  oprintf (of, "\nvoid\n");
  oprintf (of, "gt_%sx (", prefix);
  write_type_decl (of, s);
  oprintf (of, " *& x)\n");
  oprintf (of, "{\n");
  oprintf (of, "  if (x)\n    ");
  write_marker_function_name (of, alias_of ? alias_of : s, prefix);
  oprintf (of, " ((void *) x);\n");
  oprintf (of, "}\n");
}


/* Write a function to mark all the fields of type S on OF.  PREFIX
   and D are as in write_user_marking_functions.  */

static void
write_user_func_for_structure_body (type_p s, const char *prefix,
				    struct walk_type_data *d)
{
  oprintf (d->of, "\nvoid\n");
  oprintf (d->of, "gt_%sx (", prefix);
  write_type_decl (d->of, s);
  oprintf (d->of, "& x_r ATTRIBUTE_UNUSED)\n");
  oprintf (d->of, "{\n");
  oprintf (d->of, "  ");
  write_type_decl (d->of, s);
  oprintf (d->of, " * ATTRIBUTE_UNUSED x = &x_r;\n");
  d->val = "(*x)";
  d->indent = 2;
  walk_type (s, d);
  oprintf (d->of, "}\n");
}


/* Emit the user-callable functions needed to mark all the types used
   by the user structure S.  PREFIX is the prefix to use to
   distinguish ggc and pch markers.  D contains data needed to pass to
   walk_type when traversing the fields of a type.

   For every type T referenced by S, two routines are generated: one
   that takes 'T *', marks the pointer and calls the second routine,
   which just marks the fields of T.  */

static void
write_user_marking_functions (type_p s, const char *prefix,
			      struct walk_type_data *d)
{
  gcc_assert (s->kind == TYPE_USER_STRUCT);

  for (pair_p fld = s->u.s.fields; fld; fld = fld->next)
    {
      type_p fld_type = fld->type;
      if (fld_type->kind == TYPE_POINTER)
	{
	  type_p pointed_to_type = fld_type->u.p;
	  if (union_or_struct_p (pointed_to_type))
	    write_user_func_for_structure_ptr (d->of, pointed_to_type, prefix);
	}
      else if (union_or_struct_p (fld_type))
	write_user_func_for_structure_body (fld_type, prefix, d);
    }
}


/* For S, a structure that's part of ORIG_S, and using parameters
   PARAM, write out a routine that:
   - Takes a parameter, a void * but actually of type *S
   - If SEEN_ROUTINE returns nonzero, calls write_types_process_field on each
   field of S or its substructures and (in some cases) things
   that are pointed to by S.  */

static void
write_func_for_structure (type_p orig_s, type_p s, type_p *param,
			  const struct write_types_data *wtd)
{
  const char *chain_next = NULL;
  const char *chain_prev = NULL;
  const char *chain_circular = NULL;
  const char *mark_hook_name = NULL;
  options_p opt;
  struct walk_type_data d;

  if (s->u.s.base_class)
    {
      /* Verify that the base class has a "desc", since otherwise
	 the traversal hooks there won't attempt to visit fields of
	 subclasses such as this one.  */
      const_type_p ubc = get_ultimate_base_class (s);
      if ((!opts_have (ubc->u.s.opt, "user")
	   && !opts_have (ubc->u.s.opt, "desc")))
	error_at_line (&s->u.s.line,
		       ("'%s' is a subclass of non-GTY(user) GTY class '%s'"
			", but '%s' lacks a discriminator 'desc' option"),
		       s->u.s.tag, ubc->u.s.tag, ubc->u.s.tag);

      /* Don't write fns for subclasses, only for the ultimate base class
	 within an inheritance hierarchy.  */
      return;
    }

  memset (&d, 0, sizeof (d));
  d.of = get_output_file_for_structure (s, param);
  for (opt = s->u.s.opt; opt; opt = opt->next)
    if (strcmp (opt->name, "chain_next") == 0
	&& opt->kind == OPTION_STRING)
      chain_next = opt->info.string;
    else if (strcmp (opt->name, "chain_prev") == 0
	     && opt->kind == OPTION_STRING)
      chain_prev = opt->info.string;
    else if (strcmp (opt->name, "chain_circular") == 0
	     && opt->kind == OPTION_STRING)
      chain_circular = opt->info.string;
    else if (strcmp (opt->name, "mark_hook") == 0
	     && opt->kind == OPTION_STRING)
      mark_hook_name = opt->info.string;
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
  d.prev_val[1] = "not valid postage";	/* Guarantee an error.  */
  d.prev_val[3] = "x";
  d.val = "(*x)";
  d.have_this_obj = false;

  oprintf (d.of, "\n");
  oprintf (d.of, "void\n");
  write_marker_function_name (d.of, orig_s, wtd->prefix);
  oprintf (d.of, " (void *x_p)\n");
  oprintf (d.of, "{\n  ");
  write_type_decl (d.of, s);
  oprintf (d.of, " * %sx = (", chain_next == NULL ? "const " : "");
  write_type_decl (d.of, s);
  oprintf (d.of, " *)x_p;\n");
  if (chain_next != NULL)
    {
      /* TYPE_USER_STRUCTs should not occur here.  These structures
	 are completely handled by user code.  */
      gcc_assert (orig_s->kind != TYPE_USER_STRUCT);

      oprintf (d.of, "  ");
      write_type_decl (d.of, s);
      oprintf (d.of, " * xlimit = x;\n");
    }
  if (chain_next == NULL)
    {
      oprintf (d.of, "  if (%s (x", wtd->marker_routine);
      if (wtd->param_prefix)
	{
	  oprintf (d.of, ", x, gt_%s_", wtd->param_prefix);
	  output_mangled_typename (d.of, orig_s);
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
	  oprintf (d.of, "        (void) %s (xprev", wtd->marker_routine);
	  if (wtd->param_prefix)
	    {
	      oprintf (d.of, ", xprev, gt_%s_", wtd->param_prefix);
	      output_mangled_typename (d.of, orig_s);
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
  if (orig_s->kind != TYPE_USER_STRUCT)
    walk_type (s, &d);
  else
    {
      /* User structures have no fields to walk. Simply generate a call
	 to the user-provided structure marker.  */
      oprintf (d.of, "%*sgt_%sx (x);\n", d.indent, "", wtd->prefix);
    }

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

  if (orig_s->kind == TYPE_USER_STRUCT)
    write_user_marking_functions (orig_s, wtd->prefix, &d);
}


/* Write out marker routines for STRUCTURES and PARAM_STRUCTS.  */

static void
write_types (outf_p output_header, type_p structures, type_p param_structs,
	     const struct write_types_data *wtd)
{
  int nbfun = 0;		/* Count the emitted functions.  */
  type_p s;

  oprintf (output_header, "\n/* %s*/\n", wtd->comment);

  /* We first emit the macros and the declarations. Functions' code is
     emitted afterwards.  This is needed in plugin mode.  */
  oprintf (output_header, "/* Macros and declarations.  */\n");
  for (s = structures; s; s = s->next)
    /* Do not emit handlers for derived classes; we only ever deal with
       the ultimate base class within an inheritance hierarchy.  */
    if ((s->gc_used == GC_POINTED_TO || s->gc_used == GC_MAYBE_POINTED_TO)
        && !s->u.s.base_class)
      {
	options_p opt;

	if (s->gc_used == GC_MAYBE_POINTED_TO && s->u.s.line.file == NULL)
	  continue;

	const char *s_id_for_tag = filter_type_name (s->u.s.tag);

	oprintf (output_header, "#define gt_%s_", wtd->prefix);
	output_mangled_typename (output_header, s);
	oprintf (output_header, "(X) do { \\\n");
	oprintf (output_header,
		 "  if (X != NULL) gt_%sx_%s (X);\\\n", wtd->prefix,
		 s_id_for_tag);
	oprintf (output_header, "  } while (0)\n");

	for (opt = s->u.s.opt; opt; opt = opt->next)
	  if (strcmp (opt->name, "ptr_alias") == 0
	      && opt->kind == OPTION_TYPE)
	    {
	      const_type_p const t = (const_type_p) opt->info.type;
	      if (t->kind == TYPE_STRUCT
		  || t->kind == TYPE_UNION || t->kind == TYPE_LANG_STRUCT)
		{
		  const char *t_id_for_tag = filter_type_name (t->u.s.tag);
		  oprintf (output_header,
			   "#define gt_%sx_%s gt_%sx_%s\n",
			   wtd->prefix, s->u.s.tag, wtd->prefix, t_id_for_tag);
		  if (t_id_for_tag != t->u.s.tag)
		    free (CONST_CAST (char *, t_id_for_tag));
		}
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
		 wtd->prefix, s_id_for_tag);

	if (s_id_for_tag != s->u.s.tag)
	  free (CONST_CAST (char *, s_id_for_tag));

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
		     stru->u.s.tag);
	    continue;
	  }
      }

  /* At last we emit the functions code.  */
  oprintf (output_header, "\n/* functions code */\n");
  for (s = structures; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO || s->gc_used == GC_MAYBE_POINTED_TO)
      {
	options_p opt;

	if (s->gc_used == GC_MAYBE_POINTED_TO && s->u.s.line.file == NULL)
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
	      {
		nbfun++;
		DBGPRINTF ("writing func #%d lang_struct ss @ %p '%s'",
			   nbfun, (void*) ss, ss->u.s.tag);
		write_func_for_structure (s, ss, NULL, wtd);
	      }
	  }
	else
	  {
	    nbfun++;
	    DBGPRINTF ("writing func #%d struct s @ %p '%s'",
		       nbfun, (void*) s, s->u.s.tag);
	    write_func_for_structure (s, s, NULL, wtd);
	  }
      }
    else
      {
	/* Structure s is not possibly pointed to, so can be ignored.  */
	DBGPRINTF ("ignored s @ %p  '%s' gc_used#%d",
		   (void*)s,  s->u.s.tag,
		   (int) s->gc_used);
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
	      {
		nbfun++;
		DBGPRINTF ("writing func #%d param lang_struct ss @ %p '%s'",
			   nbfun, (void*) ss,  ss->u.s.tag);
		write_func_for_structure (s, ss, param, wtd);
	      }
	  }
	else
	  {
	    nbfun++;
	    DBGPRINTF ("writing func #%d param struct s @ %p stru @ %p '%s'",
		       nbfun, (void*) s,
		       (void*) stru,  stru->u.s.tag);
	    write_func_for_structure (s, stru, param, wtd);
	  }
      }
    else
      { 
	/* Param structure s is not pointed to, so should be ignored.  */
	DBGPRINTF ("ignored s @ %p", (void*)s);
      }
  if (verbosity_level >= 2)
    printf ("%s emitted %d routines for %s\n",
	    progname, nbfun, wtd->comment);
}

static const struct write_types_data ggc_wtd = {
  "ggc_m", NULL, "ggc_mark", "ggc_test_and_set_mark", NULL,
  "GC marker procedures.  ",
  FALSE
};

static const struct write_types_data pch_wtd = {
  "pch_n", "pch_p", "gt_pch_note_object", "gt_pch_note_object",
  "gt_pch_note_reorder",
  "PCH type-walking procedures.  ",
  TRUE
};

/* Write out the local pointer-walking routines.  */

/* process_field routine for local pointer-walking for user-callable
   routines.  The difference between this and
   write_types_local_process_field is that, in this case, we do not
   need to check whether the given pointer matches the address of the
   parent structure.  This check was already generated by the call
   to gt_pch_nx in the main gt_pch_p_*() function that is calling
   this code.  */

static void
write_types_local_user_process_field (type_p f, const struct walk_type_data *d)
{
  switch (f->kind)
    {
    case TYPE_POINTER:
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_LANG_STRUCT:
    case TYPE_PARAM_STRUCT:
    case TYPE_STRING:
      oprintf (d->of, "%*s  op (&(%s), cookie);\n", d->indent, "", d->val);
      break;

    case TYPE_USER_STRUCT:
      if (d->in_ptr_field)
	oprintf (d->of, "%*s  op (&(%s), cookie);\n", d->indent, "", d->val);
      else
	oprintf (d->of, "%*s  gt_pch_nx (&(%s), op, cookie);\n",
		 d->indent, "", d->val);
      break;

    case TYPE_SCALAR:
      break;

    case TYPE_ARRAY:
    case TYPE_NONE:
    case TYPE_UNDEFINED:
      gcc_unreachable ();
    }
}


/* Write a function to PCH walk all the fields of type S on OF.
   D contains data needed by walk_type to recurse into the fields of S.  */

static void
write_pch_user_walking_for_structure_body (type_p s, struct walk_type_data *d)
{
  oprintf (d->of, "\nvoid\n");
  oprintf (d->of, "gt_pch_nx (");
  write_type_decl (d->of, s);
  oprintf (d->of, "* x ATTRIBUTE_UNUSED,\n"
	   "\tATTRIBUTE_UNUSED gt_pointer_operator op,\n"
	   "\tATTRIBUTE_UNUSED void *cookie)\n");
  oprintf (d->of, "{\n");
  d->val = "(*x)";
  d->indent = 2;
  d->process_field = write_types_local_user_process_field;
  walk_type (s, d);
  oprintf (d->of, "}\n");
}


/* Emit the user-callable functions needed to mark all the types used
   by the user structure S.  PREFIX is the prefix to use to
   distinguish ggc and pch markers. CHAIN_NEXT is set if S has the
   chain_next option defined.  D contains data needed to pass to
   walk_type when traversing the fields of a type.

   For every type T referenced by S, two routines are generated: one
   that takes 'T *', marks the pointer and calls the second routine,
   which just marks the fields of T.  */

static void
write_pch_user_walking_functions (type_p s, struct walk_type_data *d)
{
  gcc_assert (s->kind == TYPE_USER_STRUCT);

  for (pair_p fld = s->u.s.fields; fld; fld = fld->next)
    {
      type_p fld_type = fld->type;
      if (union_or_struct_p (fld_type))
	write_pch_user_walking_for_structure_body (fld_type, d);
    }
}


/* process_field routine for local pointer-walking.  */

static void
write_types_local_process_field (type_p f, const struct walk_type_data *d)
{
  gcc_assert (d->have_this_obj);
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

    case TYPE_USER_STRUCT:
      oprintf (d->of, "%*sif ((void *)(%s) == this_obj)\n", d->indent, "",
	       d->prev_val[3]);
      if (d->in_ptr_field)
	oprintf (d->of, "%*s  op (&(%s), cookie);\n", d->indent, "", d->val);
      else
	oprintf (d->of, "%*s  gt_pch_nx (&(%s), op, cookie);\n",
		 d->indent, "", d->val);
      break;

    case TYPE_SCALAR:
      break;

    case TYPE_ARRAY:
    case TYPE_NONE:
    case TYPE_UNDEFINED:
      gcc_unreachable ();
    }
}


/* For S, a structure that's part of ORIG_S, and using parameters
   PARAM, write out a routine that:
   - Is of type gt_note_pointers
   - Calls PROCESS_FIELD on each field of S or its substructures.
*/

static void
write_local_func_for_structure (const_type_p orig_s, type_p s, type_p *param)
{
  struct walk_type_data d;

  /* Don't write fns for subclasses, only for the ultimate base class
     within an inheritance hierarchy.  */
  if (s->u.s.base_class)
    return;

  memset (&d, 0, sizeof (d));
  d.of = get_output_file_for_structure (s, param);
  d.process_field = write_types_local_process_field;
  d.opt = s->u.s.opt;
  d.line = &s->u.s.line;
  d.bitmap = s->u.s.bitmap;
  d.param = param;
  d.prev_val[0] = d.prev_val[2] = "*x";
  d.prev_val[1] = "not valid postage";	/* Guarantee an error.  */
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
  oprintf (d.of, "  %s %s * x ATTRIBUTE_UNUSED = (%s %s *)x_p;\n",
	   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag,
	   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag);
  d.indent = 2;
  d.have_this_obj = true;

  if (s->kind != TYPE_USER_STRUCT)
    walk_type (s, &d);
  else
    {
      /* User structures have no fields to walk. Simply generate a
	 call to the user-provided PCH walker.  */
      oprintf (d.of, "%*sif ((void *)(%s) == this_obj)\n", d.indent, "",
	       d.prev_val[3]);
      oprintf (d.of, "%*s  gt_pch_nx (&(%s), op, cookie);\n",
	       d.indent, "", d.val);
    }

  oprintf (d.of, "}\n");

  /* Write user-callable entry points for the PCH walking routines.  */
  if (orig_s->kind == TYPE_USER_STRUCT)
    write_pch_user_walking_functions (s, &d);
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
    if (s->gc_used == GC_POINTED_TO || s->gc_used == GC_MAYBE_POINTED_TO)
      {
	options_p opt;

	if (s->u.s.line.file == NULL)
	  continue;
 	for (opt = s->u.s.opt; opt; opt = opt->next)
	  if (strcmp (opt->name, "ptr_alias") == 0
	      && opt->kind == OPTION_TYPE)
	    {
	      const_type_p const t = (const_type_p) opt->info.type;
	      if (t->kind == TYPE_STRUCT
		  || t->kind == TYPE_UNION || t->kind == TYPE_LANG_STRUCT)
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
	type_p *param = s->u.param_struct.param;
	type_p stru = s->u.param_struct.stru;

	/* Declare the marker procedure.  */
	oprintf (output_header, "extern void gt_pch_p_");
	output_mangled_typename (output_header, s);
	oprintf (output_header,
		 "\n    (void *, void *, gt_pointer_operator, void *);\n");

	if (stru->u.s.line.file == NULL)
	  {
	    fprintf (stderr, "warning: structure `%s' used but not defined\n",
		     stru->u.s.tag);
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

/* Nonzero if S is a type for which typed GC allocators should be output.  */

#define USED_BY_TYPED_GC_P(s)						\
  ((s->kind == TYPE_POINTER						\
    && (s->u.p->gc_used == GC_POINTED_TO				\
	|| s->u.p->gc_used == GC_USED))					\
   || (union_or_struct_p (s)   						\
       && ((s)->gc_used == GC_POINTED_TO				\
	   || ((s)->gc_used == GC_MAYBE_POINTED_TO			\
	       && s->u.s.line.file != NULL)				\
	   || ((s)->gc_used == GC_USED					\
	       && strncmp (s->u.s.tag, "anonymous", strlen ("anonymous"))) \
	   || (s->u.s.base_class && opts_have (s->u.s.opt, "tag")))))



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
    case TYPE_USER_STRUCT:
      /* User-marked structures will typically contain pointers.  */
      return 0;
    default:
      /* Could also check for structures that have no non-pointer
         fields, but there aren't enough of those to worry about.  */
      return 1;
    }
}

/* Mangle INPF and print it to F.  */

static void
put_mangled_filename (outf_p f, const input_file *inpf)
{
  /* The call to get_output_file_name may indirectly update fn since
     get_output_file_with_visibility caches its result inside, so we
     need the CONST_CAST.  */
  const char *name = get_output_file_name (CONST_CAST (input_file*, inpf));
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
	lang_bitmap bitmap = get_lang_bitmap (fli2->file);
	int fnum;

	for (fnum = 0; bitmap != 0; fnum++, bitmap >>= 1)
	  if (bitmap & 1)
	    {
	      oprintf (base_files[fnum],
		       "extern const struct %s gt_%s_", tname, pfx);
	      put_mangled_filename (base_files[fnum], fli2->file);
	      oprintf (base_files[fnum], "[];\n");
	    }
      }

  {
    size_t fnum;
    for (fnum = 0; base_files && fnum < num_lang_dirs; fnum++)
      oprintf (base_files[fnum],
	       "EXPORTED_CONST struct %s * const %s[] = {\n", tname, name);
  }


  for (fli2 = flp; fli2; fli2 = fli2->next)
    if (fli2->started_p)
      {
	lang_bitmap bitmap = get_lang_bitmap (fli2->file);
	int fnum;

	fli2->started_p = 0;

	for (fnum = 0; base_files && bitmap != 0; fnum++, bitmap >>= 1)
	  if (bitmap & 1)
	    {
	      oprintf (base_files[fnum], "  gt_%s_", pfx);
	      put_mangled_filename (base_files[fnum], fli2->file);
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

/* Write the first three fields (pointer, count and stride) for
   root NAME to F.  V and LINE are as for write_root.

   Return true if the entry could be written; return false on error.  */

static bool
start_root_entry (outf_p f, pair_p v, const char *name, struct fileloc *line)
{
  type_p ap;

  if (!v)
    {
      error_at_line (line, "`%s' is too complex to be a root", name);
      return false;
    }

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
  return true;
}

/* A subroutine of write_root for writing the roots for field FIELD_NAME,
   which has type FIELD_TYPE.  Parameters F to EMIT_PCH are the parameters
   of the caller.  */

static void
write_field_root (outf_p f, pair_p v, type_p type, const char *name,
		  int has_length, struct fileloc *line, const char *if_marked,
		  bool emit_pch, type_p field_type, const char *field_name)
{
  struct pair newv;
  /* If the field reference is relative to V, rather than to some
     subcomponent of V, we can mark any subarrays with a single stride.
     We're effectively treating the field as a global variable in its
     own right.  */
  if (v && type == v->type)
    {
      newv = *v;
      newv.type = field_type;
      newv.name = ACONCAT ((v->name, ".", field_name, NULL));
      v = &newv;
    }
  /* Otherwise, any arrays nested in the structure are too complex to
     handle.  */
  else if (field_type->kind == TYPE_ARRAY)
    v = NULL;
  write_root (f, v, field_type, ACONCAT ((name, ".", field_name, NULL)),
	      has_length, line, if_marked, emit_pch);
}

/* Write out to F the table entry and any marker routines needed to
   mark NAME as TYPE.  V can be one of three values:

     - null, if NAME is too complex to represent using a single
       count and stride.  In this case, it is an error for NAME to
       contain any gc-ed data.

     - the outermost array that contains NAME, if NAME is part of an array.

     - the C variable that contains NAME, if NAME is not part of an array.

   LINE is the line of the C source that declares the root variable.
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
	      else if (strcmp (o->name, "desc") == 0
		       && o->kind == OPTION_STRING)
		desc = o->info.string;
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
		      if (strcmp (oo->name, "tag") == 0
			  && oo->kind == OPTION_STRING)
			tag = oo->info.string;
		    if (tag == NULL || strcmp (tag, desc) != 0)
		      continue;
		    if (validf != NULL)
		      error_at_line (line,
				     "both `%s.%s.%s' and `%s.%s.%s' have tag `%s'",
				     name, fld->name, validf->name,
				     name, fld->name, ufld->name, tag);
		    validf = ufld;
		  }
		if (validf != NULL)
		  write_field_root (f, v, type, name, 0, line, if_marked,
				    emit_pch, validf->type,
				    ACONCAT ((fld->name, ".",
					      validf->name, NULL)));
	      }
	    else if (desc)
	      error_at_line (line,
			     "global `%s.%s' has `desc' option but is not union",
			     name, fld->name);
	    else
	      write_field_root (f, v, type, name, 0, line, if_marked,
				emit_pch, fld->type, fld->name);
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

    case TYPE_USER_STRUCT:
      error_at_line (line, "`%s' must be a pointer type, because it is "
	             "a GC root and its type is marked with GTY((user))",
		     v->name);
      break;

    case TYPE_POINTER:
      {
	const_type_p tp;

	if (!start_root_entry (f, v, name, line))
	  return;

	tp = type->u.p;

	if (!has_length && union_or_struct_p (tp))
	  {
	    tp = get_ultimate_base_class (tp);
	    const char *id_for_tag = filter_type_name (tp->u.s.tag);
	    oprintf (f, "    &gt_ggc_mx_%s,\n", id_for_tag);
	    if (emit_pch)
	      oprintf (f, "    &gt_pch_nx_%s", id_for_tag);
	    else
	      oprintf (f, "    NULL");
	    if (id_for_tag != tp->u.s.tag)
	      free (CONST_CAST (char *, id_for_tag));
	  }
	else if (!has_length && tp->kind == TYPE_PARAM_STRUCT)
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
		 && (tp->kind == TYPE_POINTER || union_or_struct_p (tp)))
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
	if (!start_root_entry (f, v, name, line))
	  return;

	oprintf (f, "    (gt_pointer_walker) &gt_ggc_m_S,\n");
	oprintf (f, "    (gt_pointer_walker) &gt_pch_n_S\n");
	oprintf (f, "  },\n");
      }
      break;

    case TYPE_SCALAR:
      break;

    case TYPE_NONE:
    case TYPE_UNDEFINED:
    case TYPE_UNION:
    case TYPE_LANG_STRUCT:
    case TYPE_PARAM_STRUCT:
      error_at_line (line, "global `%s' is unimplemented type", name);
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
      oprintf (f, "    (void *, void *, gt_pointer_operator, void *);\n");
      oprintf (f, "static void gt_%sa_%s (ATTRIBUTE_UNUSED void *this_obj,\n",
	       wtd->param_prefix, v->name);
      oprintf (d.of,
	       "      ATTRIBUTE_UNUSED void *x_p,\n"
	       "      ATTRIBUTE_UNUSED gt_pointer_operator op,\n"
	       "      ATTRIBUTE_UNUSED void * cookie)\n");
      oprintf (d.of, "{\n");
      d.prev_val[0] = d.prev_val[1] = d.prev_val[2] = d.val = v->name;
      d.process_field = write_types_local_process_field;
      d.have_this_obj = true;
      walk_type (v->type, &d);
      oprintf (f, "}\n\n");
    }

  d.opt = v->opt;
  oprintf (f, "static void gt_%sa_%s (void *);\n", wtd->prefix, v->name);
  oprintf (f, "static void\ngt_%sa_%s (ATTRIBUTE_UNUSED void *x_p)\n",
	   wtd->prefix, v->name);
  oprintf (f, "{\n");
  d.prev_val[0] = d.prev_val[1] = d.prev_val[2] = d.val = v->name;
  d.process_field = write_types_process_field;
  d.have_this_obj = false;
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
      outf_p f = 
	get_output_file_with_visibility (CONST_CAST (input_file*,
						     v->line.file));
      struct flist *fli;
      const char *length = NULL;
      int deletable_p = 0;
      options_p o;
      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "length") == 0
	    && o->kind == OPTION_STRING)
	  length = o->info.string;
	else if (strcmp (o->name, "deletable") == 0)
	  deletable_p = 1;
	else if (strcmp (o->name, "param_is") == 0)
	  ;
	else if (strncmp (o->name, "param", 5) == 0
		 && ISDIGIT (o->name[5]) && strcmp (o->name + 6, "_is") == 0)
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
	  fli->file = v->line.file;
	  gcc_assert (fli->file);
	  flp = fli;

	  oprintf (f, "\n/* GC roots.  */\n\n");
	}

      if (!deletable_p
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
      outf_p f = get_output_file_with_visibility (CONST_CAST (input_file*,
							      v->line.file));
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
      if (!fli->started_p)
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
      outf_p f = get_output_file_with_visibility (CONST_CAST (input_file*,
							      v->line.file));
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
      if (!fli->started_p)
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
      outf_p f = get_output_file_with_visibility (CONST_CAST (input_file*,
							      v->line.file));
      struct flist *fli;
      const char *if_marked = NULL;
      int length_p = 0;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "length") == 0)
	  length_p = 1;
	else if (strcmp (o->name, "if_marked") == 0
		       && o->kind == OPTION_STRING)
	  if_marked = o->info.string;
       if (if_marked == NULL)
	continue;
      if (v->type->kind != TYPE_POINTER
	  || v->type->u.p->kind != TYPE_PARAM_STRUCT
	  || v->type->u.p->u.param_struct.stru != find_structure ("htab",
	                                                          TYPE_STRUCT))
	{
	  error_at_line (&v->line,
			 "if_marked option used but not hash table");
	  continue;
	}

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (!fli->started_p)
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
      outf_p f = get_output_file_with_visibility (CONST_CAST (input_file*,
							      v->line.file));
      struct flist *fli;
      int length_p = 0;
      int if_marked_p = 0;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "length") == 0)
	  length_p = 1;
	else if (strcmp (o->name, "if_marked") == 0)
	  if_marked_p = 1;

      if (!if_marked_p)
	continue;

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (!fli->started_p)
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
      outf_p f = get_output_file_with_visibility (CONST_CAST (input_file*,
							      v->line.file));
      struct flist *fli;
      int skip_p = 0;
      options_p o;

      for (o = v->opt; o; o = o->next)
	if (strcmp (o->name, "deletable") == 0
	    || strcmp (o->name, "if_marked") == 0)
	  {
	    skip_p = 1;
	    break;
	  }

      if (skip_p)
	continue;

      if (!contains_scalar_p (v->type))
	continue;

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (!fli->started_p)
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

/* TRUE if type S has the GTY variable_size annotation.  */

static bool
variable_size_p (const type_p s)
{
  options_p o;
  for (o = s->u.s.opt; o; o = o->next)
    if (strcmp (o->name, "variable_size") == 0)
      return true;
  return false;
}

enum alloc_quantity
{ single, vector };

/* Writes one typed allocator definition into output F for type
   identifier TYPE_NAME with optional type specifier TYPE_SPECIFIER.
   The allocator name will contain ALLOCATOR_TYPE.  If VARIABLE_SIZE
   is true, the allocator will have an extra parameter specifying
   number of bytes to allocate.  If QUANTITY is set to VECTOR, a
   vector allocator will be output.  */

static void
write_typed_alloc_def (outf_p f, 
                       bool variable_size, const char *type_specifier,
                       const char *type_name, const char *allocator_type,
                       enum alloc_quantity quantity)
{
  bool two_args = variable_size && (quantity == vector);
  gcc_assert (f != NULL);
  const char *type_name_as_id = filter_type_name (type_name);
  oprintf (f, "#define ggc_alloc_%s%s", allocator_type, type_name_as_id);
  oprintf (f, "(%s%s%s) ",
	   (variable_size ? "SIZE" : ""),
	   (two_args ? ", " : ""),
	   (quantity == vector) ? "n" : "");
  oprintf (f, "((%s%s *)", type_specifier, type_name);
  oprintf (f, "(ggc_internal_%salloc_stat (", allocator_type);
  if (variable_size)
    oprintf (f, "SIZE");
  else
    oprintf (f, "sizeof (%s%s)", type_specifier, type_name);
  if (quantity == vector)
    oprintf (f, ", n");
  oprintf (f, " MEM_STAT_INFO)))\n");
  if (type_name_as_id != type_name)
    free (CONST_CAST (char *, type_name_as_id));
}

/* Writes a typed allocator definition into output F for a struct or
   union S, with a given ALLOCATOR_TYPE and QUANTITY for ZONE.  */

static void
write_typed_struct_alloc_def (outf_p f,
			      const type_p s, const char *allocator_type,
			      enum alloc_quantity quantity)
{
  gcc_assert (union_or_struct_p (s));
  write_typed_alloc_def (f, variable_size_p (s), get_type_specifier (s),
                         s->u.s.tag, allocator_type, quantity);
}

/* Writes a typed allocator definition into output F for a typedef P,
   with a given ALLOCATOR_TYPE and QUANTITY for ZONE.  */

static void
write_typed_typedef_alloc_def (outf_p f,
                               const pair_p p, const char *allocator_type,
                               enum alloc_quantity quantity)
{
  write_typed_alloc_def (f, variable_size_p (p->type), "", p->name,
                         allocator_type, quantity);
}

/* Writes typed allocator definitions into output F for the types in
   STRUCTURES and TYPEDEFS that are used by GC.  */

static void
write_typed_alloc_defns (outf_p f,
                         const type_p structures, const pair_p typedefs)
{
  type_p s;
  pair_p p;

  gcc_assert (f != NULL);
  oprintf (f,
	   "\n/* Allocators for known structs and unions.  */\n\n");
  for (s = structures; s; s = s->next)
    {
      if (!USED_BY_TYPED_GC_P (s))
	continue;
      gcc_assert (union_or_struct_p (s));
      /* In plugin mode onput output ggc_alloc macro definitions
	 relevant to plugin input files.  */
      if (nb_plugin_files > 0 
	  && ((s->u.s.line.file == NULL) || !s->u.s.line.file->inpisplugin))
	continue;
      write_typed_struct_alloc_def (f, s, "", single);
      write_typed_struct_alloc_def (f, s, "cleared_", single);
      write_typed_struct_alloc_def (f, s, "vec_", vector);
      write_typed_struct_alloc_def (f, s, "cleared_vec_", vector);
    }

  oprintf (f, "\n/* Allocators for known typedefs.  */\n");
  for (p = typedefs; p; p = p->next)
    {
      s = p->type;
      if (!USED_BY_TYPED_GC_P (s) || (strcmp (p->name, s->u.s.tag) == 0))
	continue;
      /* In plugin mode onput output ggc_alloc macro definitions
	 relevant to plugin input files.  */
      if (nb_plugin_files > 0) 
	{
	  struct fileloc* filoc = type_fileloc (s);
	  if (!filoc || !filoc->file->inpisplugin)
	    continue;
	};
      write_typed_typedef_alloc_def (f, p, "", single);
      write_typed_typedef_alloc_def (f, p, "cleared_", single);
      write_typed_typedef_alloc_def (f, p, "vec_", vector);
      write_typed_typedef_alloc_def (f, p, "cleared_vec_", vector);
    }
}

/* Prints not-as-ugly version of a typename of T to OF.  Trades the uniquness
   guaranteee for somewhat increased readability.  If name conflicts do happen,
   this funcion will have to be adjusted to be more like
   output_mangled_typename.  */

static void
output_typename (outf_p of, const_type_p t)
{
  switch (t->kind)
    {
    case TYPE_STRING:
      oprintf (of, "str");
      break;
    case TYPE_SCALAR:
      oprintf (of, "scalar");
      break;
    case TYPE_POINTER:
      output_typename (of, t->u.p);
      break;
    case TYPE_STRUCT:
    case TYPE_USER_STRUCT:
    case TYPE_UNION:
    case TYPE_LANG_STRUCT:
      oprintf (of, "%s", t->u.s.tag);
      break;
    case TYPE_PARAM_STRUCT:
      {
	int i;
	for (i = 0; i < NUM_PARAM; i++)
	  if (t->u.param_struct.param[i] != NULL)
	    {
	      output_typename (of, t->u.param_struct.param[i]);
	      oprintf (of, "_");
	    }
	output_typename (of, t->u.param_struct.stru);
	break;
      }
    case TYPE_NONE:
    case TYPE_UNDEFINED:
    case TYPE_ARRAY:
      gcc_unreachable ();
    }
}

/* Writes a typed GC allocator for type S that is suitable as a callback for
   the splay tree implementation in libiberty.  */

static void
write_splay_tree_allocator_def (const_type_p s)
{
  outf_p of = get_output_file_with_visibility (NULL);
  oprintf (of, "void * ggc_alloc_splay_tree_");
  output_typename (of, s);
  oprintf (of, " (int sz, void * nl)\n");
  oprintf (of, "{\n");
  oprintf (of, "  return ggc_splay_alloc (sz, nl);\n");
  oprintf (of, "}\n\n");
}

/* Writes typed GC allocators for PARAM_STRUCTS that are suitable as callbacks
   for the splay tree implementation in libiberty.  */

static void
write_splay_tree_allocators (const_type_p param_structs)
{
  const_type_p s;

  oprintf (header_file, "\n/* Splay tree callback allocators.  */\n");
  for (s = param_structs; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO)
      {
	oprintf (header_file, "extern void * ggc_alloc_splay_tree_");
	output_typename (header_file, s);
	oprintf (header_file, " (int, void *);\n");
	write_splay_tree_allocator_def (s);
      }
}

#define INDENT 2

/* Dumps the value of typekind KIND.  */

static void
dump_typekind (int indent, enum typekind kind)
{
  printf ("%*ckind = ", indent, ' ');
  switch (kind)
    {
    case TYPE_SCALAR:
      printf ("TYPE_SCALAR");
      break;
    case TYPE_STRING:
      printf ("TYPE_STRING");
      break;
    case TYPE_STRUCT:
      printf ("TYPE_STRUCT");
      break;
    case TYPE_UNDEFINED:
      printf ("TYPE_UNDEFINED");
      break;
    case TYPE_USER_STRUCT:
      printf ("TYPE_USER_STRUCT");
      break;
    case TYPE_UNION:
      printf ("TYPE_UNION");
      break;
    case TYPE_POINTER:
      printf ("TYPE_POINTER");
      break;
    case TYPE_ARRAY:
      printf ("TYPE_ARRAY");
      break;
    case TYPE_LANG_STRUCT:
      printf ("TYPE_LANG_STRUCT");
      break;
    case TYPE_PARAM_STRUCT:
      printf ("TYPE_PARAM_STRUCT");
      break;
    default:
      gcc_unreachable ();
    }
  printf ("\n");
}

/* Dumps the value of GC_USED flag.  */

static void
dump_gc_used (int indent, enum gc_used_enum gc_used)
{
  printf ("%*cgc_used = ", indent, ' ');
  switch (gc_used)
    {
    case GC_UNUSED:
      printf ("GC_UNUSED");
      break;
    case GC_USED:
      printf ("GC_USED");
      break;
    case GC_MAYBE_POINTED_TO:
      printf ("GC_MAYBE_POINTED_TO");
      break;
    case GC_POINTED_TO:
      printf ("GC_POINTED_TO");
      break;
    default:
      gcc_unreachable ();
    }
  printf ("\n");
}

/* Dumps the type options OPT.  */

static void
dump_options (int indent, options_p opt)
{
  options_p o;
  printf ("%*coptions = ", indent, ' ');
  o = opt;
  while (o)
    {
      switch (o->kind)
	{
	case OPTION_STRING:
	  printf ("%s:string %s ", o->name, o->info.string);
	  break;
	case OPTION_TYPE:
	  printf ("%s:type ", o->name);
	  dump_type (indent+1, o->info.type);
	  break;
	case OPTION_NESTED:
	  printf ("%s:nested ", o->name);
	  break;
	case OPTION_NONE:
	  gcc_unreachable ();
	}
      o = o->next;
    }
  printf ("\n");
}

/* Dumps the source file location in LINE.  */

static void
dump_fileloc (int indent, struct fileloc line)
{
  printf ("%*cfileloc: file = %s, line = %d\n", indent, ' ', 
	  get_input_file_name (line.file),
	  line.line);
}

/* Recursively dumps the struct, union, or a language-specific
   struct T.  */

static void
dump_type_u_s (int indent, type_p t)
{
  pair_p fields;

  gcc_assert (union_or_struct_p (t));
  printf ("%*cu.s.tag = %s\n", indent, ' ', t->u.s.tag);
  dump_fileloc (indent, t->u.s.line);
  printf ("%*cu.s.fields =\n", indent, ' ');
  fields = t->u.s.fields;
  while (fields)
    {
      dump_pair (indent + INDENT, fields);
      fields = fields->next;
    }
  printf ("%*cend of fields of type %p\n", indent, ' ', (void *) t);
  dump_options (indent, t->u.s.opt);
  printf ("%*cu.s.bitmap = %X\n", indent, ' ', t->u.s.bitmap);
  if (t->kind == TYPE_LANG_STRUCT)
    {
      printf ("%*cu.s.lang_struct:\n", indent, ' ');
      dump_type_list (indent + INDENT, t->u.s.lang_struct);
    }
}

/* Recursively dumps the array T.  */

static void
dump_type_u_a (int indent, type_p t)
{
  gcc_assert (t->kind == TYPE_ARRAY);
  printf ("%*clen = %s, u.a.p:\n", indent, ' ', t->u.a.len);
  dump_type_list (indent + INDENT, t->u.a.p);
}

/* Recursively dumps the parameterized struct T.  */

static void
dump_type_u_param_struct (int indent, type_p t)
{
  int i;
  gcc_assert (t->kind == TYPE_PARAM_STRUCT);
  printf ("%*cu.param_struct.stru:\n", indent, ' ');
  dump_type_list (indent, t->u.param_struct.stru);
  dump_fileloc (indent, t->u.param_struct.line);
  for (i = 0; i < NUM_PARAM; i++)
    {
      if (t->u.param_struct.param[i] == NULL)
	continue;
      printf ("%*cu.param_struct.param[%d]:\n", indent, ' ', i);
      dump_type (indent + INDENT, t->u.param_struct.param[i]);
    }
}

/* Recursively dumps the type list T.  */

static void
dump_type_list (int indent, type_p t)
{
  type_p p = t;
  while (p)
    {
      dump_type (indent, p);
      p = p->next;
    }
}

static htab_t seen_types;

/* Recursively dumps the type T if it was not dumped previously.  */

static void
dump_type (int indent, type_p t)
{
  PTR *slot;

  if (seen_types == NULL)
    seen_types = htab_create (100, htab_hash_pointer, htab_eq_pointer, NULL);

  printf ("%*cType at %p: ", indent, ' ', (void *) t);
  slot = htab_find_slot (seen_types, t, INSERT);
  if (*slot != NULL)
    {
      printf ("already seen.\n");
      return;
    }
  *slot = t;
  printf ("\n");

  dump_typekind (indent, t->kind);
  printf ("%*cpointer_to = %p\n", indent + INDENT, ' ',
	  (void *) t->pointer_to);
  dump_gc_used (indent + INDENT, t->gc_used);
  switch (t->kind)
    {
    case TYPE_SCALAR:
      printf ("%*cscalar_is_char = %s\n", indent + INDENT, ' ',
	      t->u.scalar_is_char ? "true" : "false");
      break;
    case TYPE_STRING:
      break;
    case TYPE_STRUCT:
    case TYPE_UNION:
    case TYPE_LANG_STRUCT:
    case TYPE_USER_STRUCT:
      dump_type_u_s (indent + INDENT, t);
      break;
    case TYPE_POINTER:
      printf ("%*cp:\n", indent + INDENT, ' ');
      dump_type (indent + INDENT, t->u.p);
      break;
    case TYPE_ARRAY:
      dump_type_u_a (indent + INDENT, t);
      break;
    case TYPE_PARAM_STRUCT:
      dump_type_u_param_struct (indent + INDENT, t);
      break;
    default:
      gcc_unreachable ();
    }
  printf ("%*cEnd of type at %p\n", indent, ' ', (void *) t);
}

/* Dumps the pair P.  */

static void
dump_pair (int indent, pair_p p)
{
  printf ("%*cpair: name = %s\n", indent, ' ', p->name);
  dump_type (indent, p->type);
  dump_fileloc (indent, p->line);
  dump_options (indent, p->opt);
  printf ("%*cEnd of pair %s\n", indent, ' ', p->name);
}

/* Dumps the list of pairs PP.  */

static void
dump_pair_list (const char *name, pair_p pp)
{
  pair_p p;
  printf ("%s:\n", name);
  for (p = pp; p != NULL; p = p->next)
    dump_pair (0, p);
  printf ("End of %s\n\n", name);
}

/* Dumps the STRUCTURES.  */

static void
dump_structures (const char *name, type_p structures)
{
  printf ("%s:\n", name);
  dump_type_list (0, structures);
  printf ("End of %s\n\n", name);
}

/* Dumps the internal structures of gengtype.  This is useful to debug
   gengtype itself, or to understand what it does, e.g. for plugin
   developers.  */

static void
dump_everything (void)
{
  dump_pair_list ("typedefs", typedefs);
  dump_structures ("structures", structures);
  dump_structures ("param_structs", param_structs);
  dump_pair_list ("variables", variables);

  /* Allocated with the first call to dump_type.  */
  htab_delete (seen_types);
}



/* Option specification for getopt_long.  */
static const struct option gengtype_long_options[] = {
  {"help", no_argument, NULL, 'h'},
  {"version", no_argument, NULL, 'V'},
  {"verbose", no_argument, NULL, 'v'},
  {"dump", no_argument, NULL, 'd'},
  {"debug", no_argument, NULL, 'D'},
  {"plugin", required_argument, NULL, 'P'},
  {"srcdir", required_argument, NULL, 'S'},
  {"backupdir", required_argument, NULL, 'B'},
  {"inputs", required_argument, NULL, 'I'},
  {"read-state", required_argument, NULL, 'r'},
  {"write-state", required_argument, NULL, 'w'},
  /* Terminating NULL placeholder.  */
  {NULL, no_argument, NULL, 0},
};


static void
print_usage (void)
{
  printf ("Usage: %s\n", progname);
  printf ("\t -h | --help " " \t# Give this help.\n");
  printf ("\t -D | --debug "
	  " \t# Give debug output to debug %s itself.\n", progname);
  printf ("\t -V | --version " " \t# Give version information.\n");
  printf ("\t -v | --verbose  \t# Increase verbosity.  Can be given several times.\n");
  printf ("\t -d | --dump " " \t# Dump state for debugging.\n");
  printf ("\t -P | --plugin <output-file> <plugin-src> ... "
	  " \t# Generate for plugin.\n");
  printf ("\t -S | --srcdir <GCC-directory> "
	  " \t# Specify the GCC source directory.\n");
  printf ("\t -B | --backupdir <directory> "
	  " \t# Specify the backup directory for updated files.\n");
  printf ("\t -I | --inputs <input-list> "
	  " \t# Specify the file with source files list.\n");
  printf ("\t -w | --write-state <state-file> " " \t# Write a state file.\n");
  printf ("\t -r | --read-state <state-file> " " \t# Read a state file.\n");
}

static void
print_version (void)
{
  printf ("%s %s%s\n", progname, pkgversion_string, version_string);
  printf ("Report bugs: %s\n", bug_report_url);
}

/* Parse the program options using getopt_long... */
static void
parse_program_options (int argc, char **argv)
{
  int opt = -1;
  while ((opt = getopt_long (argc, argv, "hVvdP:S:B:I:w:r:D",
			     gengtype_long_options, NULL)) >= 0)
    {
      switch (opt)
	{
	case 'h':		/* --help */
	  print_usage ();
	  break;
	case 'V':		/* --version */
	  print_version ();
	  break;
	case 'd':		/* --dump */
	  do_dump = 1;
	  break;
	case 'D':		/* --debug */
	  do_debug = 1;
	  break;
	case 'v':		/* --verbose */
	  verbosity_level++;
	  break;
	case 'P':		/* --plugin */
	  if (optarg)
	    plugin_output_filename = optarg;
	  else
	    fatal ("missing plugin output file name");
	  break;
	case 'S':		/* --srcdir */
	  if (optarg)
	    srcdir = optarg;
	  else
	    fatal ("missing source directory");
	  srcdir_len = strlen (srcdir);
	  break;
	case 'B':		/* --backupdir */
	  if (optarg)
	    backup_dir = optarg;
	  else
	    fatal ("missing backup directory");
	  break;
	case 'I':		/* --inputs */
	  if (optarg)
	    inputlist = optarg;
	  else
	    fatal ("missing input list");
	  break;
	case 'r':		/* --read-state */
	  if (optarg)
	    read_state_filename = optarg;
	  else
	    fatal ("missing read state file");
	  DBGPRINTF ("read state %s\n", optarg);
	  break;
	case 'w':		/* --write-state */
	  DBGPRINTF ("write state %s\n", optarg);
	  if (optarg)
	    write_state_filename = optarg;
	  else
	    fatal ("missing write state file");
	  break;
	default:
	  fprintf (stderr, "%s: unknown flag '%c'\n", progname, opt);
	  print_usage ();
	  fatal ("unexpected flag");
	}
    };
  if (plugin_output_filename)
    {
      /* In plugin mode we require some input files.  */
      int i = 0;
      if (optind >= argc)
	fatal ("no source files given in plugin mode");
      nb_plugin_files = argc - optind;
      plugin_files = XNEWVEC (input_file*, nb_plugin_files);
      for (i = 0; i < (int) nb_plugin_files; i++)
	{
	  char *name = argv[i + optind];
	  plugin_files[i] = input_file_by_name (name);
	}
    }
}



/******* Manage input files.  ******/

/* Hash table of unique input file names.  */
static htab_t input_file_htab;

/* Find or allocate a new input_file by hash-consing it.  */
input_file*
input_file_by_name (const char* name)
{
  PTR* slot;
  input_file* f = NULL;
  int namlen = 0;
  if (!name)
    return NULL;
  namlen = strlen (name);
  f = XCNEWVAR (input_file, sizeof (input_file)+namlen+2);
  f->inpbitmap = 0;
  f->inpoutf = NULL;
  f->inpisplugin = false;
  strcpy (f->inpname, name);
  slot = htab_find_slot (input_file_htab, f, INSERT);
  gcc_assert (slot != NULL);
  if (*slot)
    {
      /* Already known input file.  */
      free (f);
      return (input_file*)(*slot);
    }
  /* New input file.  */
  *slot = f;
  return f;
    }

/* Hash table support routines for input_file-s.  */
static hashval_t
htab_hash_inputfile (const void *p)
{
  const input_file *inpf = (const input_file *) p;
  gcc_assert (inpf);
  return htab_hash_string (get_input_file_name (inpf));
}

static int
htab_eq_inputfile (const void *x, const void *y)
{
  const input_file *inpfx = (const input_file *) x;
  const input_file *inpfy = (const input_file *) y;
  gcc_assert (inpfx != NULL && inpfy != NULL);
  return !filename_cmp (get_input_file_name (inpfx), get_input_file_name (inpfy));
}


int
main (int argc, char **argv)
{
  size_t i;
  static struct fileloc pos = { NULL, 0 };
  outf_p output_header;

  /* Mandatory common initializations.  */
  progname = "gengtype";	/* For fatal and messages.  */
  /* Create the hash-table used to hash-cons input files.  */
  input_file_htab =
    htab_create (800, htab_hash_inputfile, htab_eq_inputfile, NULL);
  /* Initialize our special input files.  */
  this_file = input_file_by_name (__FILE__);
  system_h_file = input_file_by_name ("system.h");
  /* Set the scalar_is_char union number for predefined scalar types.  */
  scalar_nonchar.u.scalar_is_char = FALSE;
  scalar_char.u.scalar_is_char = TRUE;

  parse_program_options (argc, argv);

#if ENABLE_CHECKING
  if (do_debug)
    {
      time_t now = (time_t) 0;
      time (&now);
      DBGPRINTF ("gengtype started pid %d at %s",
		 (int) getpid (), ctime (&now));
    }
#endif	/* ENABLE_CHECKING */

  /* Parse the input list and the input files.  */
  DBGPRINTF ("inputlist %s", inputlist);
  if (read_state_filename)
    {
      if (inputlist)
	fatal ("input list %s cannot be given with a read state file %s",
	       inputlist, read_state_filename);
      read_state (read_state_filename);
      DBGPRINT_COUNT_TYPE ("structures after read_state", structures);
      DBGPRINT_COUNT_TYPE ("param_structs after read_state", param_structs);
    }
  else if (inputlist)
    {
      /* These types are set up with #define or else outside of where
         we can see them.  We should initialize them before calling
         read_input_list.  */
#define POS_HERE(Call) do { pos.file = this_file; pos.line = __LINE__; \
	Call;} while (0)
      POS_HERE (do_scalar_typedef ("CUMULATIVE_ARGS", &pos));
      POS_HERE (do_scalar_typedef ("REAL_VALUE_TYPE", &pos));
      POS_HERE (do_scalar_typedef ("FIXED_VALUE_TYPE", &pos));
      POS_HERE (do_scalar_typedef ("double_int", &pos));
      POS_HERE (do_scalar_typedef ("uint64_t", &pos));
      POS_HERE (do_scalar_typedef ("uint8", &pos));
      POS_HERE (do_scalar_typedef ("uintptr_t", &pos));
      POS_HERE (do_scalar_typedef ("jword", &pos));
      POS_HERE (do_scalar_typedef ("JCF_u2", &pos));
      POS_HERE (do_scalar_typedef ("void", &pos));
      POS_HERE (do_typedef ("PTR", 
			    create_pointer (resolve_typedef ("void", &pos)),
			    &pos));
#undef POS_HERE
      read_input_list (inputlist);
      for (i = 0; i < num_gt_files; i++)
	{
	  parse_file (get_input_file_name (gt_files[i]));
	  DBGPRINTF ("parsed file #%d %s", 
		     (int) i, get_input_file_name (gt_files[i]));
	}
      if (verbosity_level >= 1)
	printf ("%s parsed %d files with %d GTY types\n", 
		progname, (int) num_gt_files, type_count);

      DBGPRINT_COUNT_TYPE ("structures after parsing", structures);
      DBGPRINT_COUNT_TYPE ("param_structs after parsing", param_structs);

    }
  else
    fatal ("either an input list or a read state file should be given");
  if (hit_error)
    return 1;


  if (plugin_output_filename)
    {
      size_t ix = 0;
      /* In plugin mode, we should have read a state file, and have
	 given at least one plugin file.  */
      if (!read_state_filename)
	fatal ("No read state given in plugin mode for %s",
	       plugin_output_filename);

      if (nb_plugin_files == 0 || !plugin_files)
	fatal ("No plugin files given in plugin mode for %s",
	       plugin_output_filename);

      /* Parse our plugin files and augment the state.  */
      for (ix = 0; ix < nb_plugin_files; ix++)
	{
	  input_file* pluginput = plugin_files [ix];
	  pluginput->inpisplugin = true;
	  parse_file (get_input_file_name (pluginput));
	}
      if (hit_error)
	return 1;

      plugin_output = create_file ("GCC", plugin_output_filename);
      DBGPRINTF ("created plugin_output %p named %s",
		 (void *) plugin_output, plugin_output->name);
    }
  else
    {				/* No plugin files, we are in normal mode.  */
      if (!srcdir)
	fatal ("gengtype needs a source directory in normal mode");
    }
  if (hit_error)
    return 1;

  gen_rtx_next ();

  /* The call to set_gc_used may indirectly call find_param_structure
     hence enlarge the param_structs list of types.  */
  set_gc_used (variables);

 /* The state at this point is read from the state input file or by
    parsing source files and optionally augmented by parsing plugin
    source files.  Write it now.  */
  if (write_state_filename)
    {
      DBGPRINT_COUNT_TYPE ("structures before write_state", structures);
      DBGPRINT_COUNT_TYPE ("param_structs before write_state", param_structs);

      if (hit_error)
	fatal ("didn't write state file %s after errors", 
	       write_state_filename);

      DBGPRINTF ("before write_state %s", write_state_filename);
      write_state (write_state_filename);

      if (do_dump)
	dump_everything ();

      /* After having written the state file we return immediately to
	 avoid generating any output file.  */
      if (hit_error)
	return 1;
      else
	return 0;
    }


  open_base_files ();

  output_header = plugin_output ? plugin_output : header_file;
  write_typed_alloc_defns (output_header, structures, typedefs);
  DBGPRINT_COUNT_TYPE ("structures before write_types outputheader",
		       structures);
  DBGPRINT_COUNT_TYPE ("param_structs before write_types outputheader",
		       param_structs);

  write_types (output_header, structures, param_structs, &ggc_wtd);
  if (plugin_files == NULL)
    {
      DBGPRINT_COUNT_TYPE ("structures before write_types headerfil",
			   structures);
      DBGPRINT_COUNT_TYPE ("param_structs before write_types headerfil",
			   param_structs);
      write_types (header_file, structures, param_structs, &pch_wtd);
      write_local (header_file, structures, param_structs);
    }
  write_splay_tree_allocators (param_structs);
  write_roots (variables, plugin_files == NULL);
  write_rtx_next ();
  close_output_files ();

  if (do_dump)
    dump_everything ();

  /* Don't bother about free-ing any input or plugin file, etc.  */

  if (hit_error)
    return 1;
  return 0;
}
