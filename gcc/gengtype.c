/* Process source files and output type information.
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "hconfig.h"
#include "system.h"
#include "gengtype.h"
#include "gtyp-gen.h"

/* Nonzero iff an error has occurred.  */
static int hit_error = 0;

static void open_base_files PARAMS ((void));
static void close_output_files PARAMS ((void));


/* Report an error at POS, printing MSG.  */

void
error_at_line VPARAMS ((struct fileloc *pos, const char *msg, ...))
{
  VA_OPEN (ap, msg);
  VA_FIXEDARG (ap, struct fileloc *, pos);
  VA_FIXEDARG (ap, const char *, msg);

  fprintf (stderr, "%s:%d: ", pos->file, pos->line);
  vfprintf (stderr, msg, ap);
  fputc ('\n', stderr);
  hit_error = 1;

  VA_CLOSE (ap);
}

/* vasprintf, but produces fatal message on out-of-memory.  */
int
xvasprintf (result, format, args)
     char ** result;
     const char *format;
     va_list args;
{
  int ret = vasprintf (result, format, args);
  if (*result == NULL || ret < 0)
    {
      fputs ("gengtype: out of memory", stderr);
      xexit (1);
    }
  return ret;
}

/* Wrapper for xvasprintf.  */
char *
xasprintf VPARAMS ((const char *format, ...))
{
  char *result;
  VA_OPEN (ap, format);
  VA_FIXEDARG (ap, const char *, format);
  xvasprintf (&result, format, ap);
  VA_CLOSE (ap);
  return result;
}

/* The one and only TYPE_STRING.  */

struct type string_type = {
  TYPE_STRING, NULL, NULL, GC_USED
  UNION_INIT_ZERO
}; 

/* Lists of various things.  */

static pair_p typedefs;
static type_p structures;
static type_p param_structs;
static pair_p variables;

/* Define S as a typedef to T at POS.  */

void
do_typedef (s, t, pos)
     const char *s;
     type_p t;
     struct fileloc *pos;
{
  pair_p p;

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

  p = xmalloc (sizeof (struct pair));
  p->next = typedefs;
  p->name = s;
  p->type = t;
  p->line = *pos;
  typedefs = p;
}

/* Return the type previously defined for S.  Use POS to report errors.   */

type_p
resolve_typedef (s, pos)
     const char *s;
     struct fileloc *pos;
{
  pair_p p;
  for (p = typedefs; p != NULL; p = p->next)
    if (strcmp (p->name, s) == 0)
      return p->type;
  error_at_line (pos, "unidentified type `%s'", s);
  return create_scalar_type ("char", 4);
}

/* Create a new structure with tag NAME (or a union iff ISUNION is nonzero),
   at POS with fields FIELDS and options O.  */

void
new_structure (name, isunion, pos, fields, o)
     const char *name;
     int isunion;
     struct fileloc *pos;
     pair_p fields;
     options_p o;
{
  type_p si;
  type_p s = NULL;
  lang_bitmap bitmap = get_base_file_bitmap (pos->file);

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
	    si = xcalloc (1, sizeof (struct type));
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
	    s = xcalloc (1, sizeof (struct type));
	    s->next = ls->u.s.lang_struct;
	    ls->u.s.lang_struct = s;
	    s->u.s.lang_struct = ls;
	  }
	break;
      }
  
  if (s == NULL)
    {
      s = xcalloc (1, sizeof (struct type));
      s->next = structures;
      structures = s;
    }

  if (s->u.s.line.file != NULL
      || (s->u.s.lang_struct && (s->u.s.lang_struct->u.s.bitmap & bitmap)))
    {
      error_at_line (pos, "duplicate structure definition");
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
}

/* Return the previously-defined structure with tag NAME (or a union
   iff ISUNION is nonzero), or a new empty structure or union if none
   was defined previously.  */

type_p
find_structure (name, isunion)
     const char *name;
     int isunion;
{
  type_p s;

  for (s = structures; s != NULL; s = s->next)
    if (strcmp (name, s->u.s.tag) == 0 
	&& UNION_P (s) == isunion)
      return s;

  s = xcalloc (1, sizeof (struct type));
  s->next = structures;
  structures = s;
  s->kind = isunion ? TYPE_UNION : TYPE_STRUCT;
  s->u.s.tag = name;
  structures = s;
  return s;
}

/* Return a scalar type with name NAME.  */

type_p
create_scalar_type (name, name_len)
     const char *name;
     size_t name_len;
{
  type_p r = xcalloc (1, sizeof (struct type));
  r->kind = TYPE_SCALAR;
  r->u.sc = xmemdup (name, name_len, name_len + 1);
  return r;
}

/* Return a pointer to T.  */

type_p
create_pointer (t)
     type_p t;
{
  if (! t->pointer_to)
    {
      type_p r = xcalloc (1, sizeof (struct type));
      r->kind = TYPE_POINTER;
      r->u.p = t;
      t->pointer_to = r;
    }
  return t->pointer_to;
}

/* Return an array of length LEN.  */

type_p
create_array (t, len)
     type_p t;
     const char *len;
{
  type_p v;
  
  v = xcalloc (1, sizeof (*v));
  v->kind = TYPE_ARRAY;
  v->u.a.p = t;
  v->u.a.len = len;
  return v;
}

/* Perform any special processing on a type T, about to become the type
   of a field.  Return the appropriate type for the field.
   At present:
   - Converts pointer-to-char, with no length parameter, to TYPE_STRING;
   - Similarly for arrays of pointer-to-char;
   - Converts structures for which a parameter is provided to
   TYPE_PARAM_STRUCT.
*/   

type_p
adjust_field_type (t, opt)
     type_p t;
     options_p opt;
{
  int length_p = 0;
  const int pointer_p = t->kind == TYPE_POINTER;
  
  for (; opt; opt = opt->next)
    if (strcmp (opt->name, "length") == 0)
      length_p = 1;
    else if (strcmp (opt->name, "param_is") == 0)
      {
	type_p realt;

	if (pointer_p)
	  t = t->u.p;
	
	for (realt = param_structs; realt; realt = realt->next)
	  if (realt->u.param_struct.stru == t
	      && realt->u.param_struct.param == (type_p) opt->info)
	    return pointer_p ? create_pointer (realt) : realt;
	realt = xcalloc (1, sizeof (*realt));
	realt->kind = TYPE_PARAM_STRUCT;
	realt->next = param_structs;
	param_structs = realt;
	realt->u.param_struct.stru = t;
	realt->u.param_struct.param = (type_p) opt->info;
	return pointer_p ? create_pointer (realt) : realt;
      }
  
  if (! length_p
      && pointer_p
      && t->u.p->kind == TYPE_SCALAR
      && (strcmp (t->u.p->u.sc, "char") == 0
	  || strcmp (t->u.p->u.sc, "unsigned char") == 0))
    return &string_type;
  if (t->kind == TYPE_ARRAY && t->u.a.p->kind == TYPE_POINTER
      && t->u.a.p->u.p->kind == TYPE_SCALAR
      && (strcmp (t->u.a.p->u.p->u.sc, "char") == 0
	  || strcmp (t->u.a.p->u.p->u.sc, "unsigned char") == 0))
    return create_array (&string_type, t->u.a.len);

  return t;
}

/* Add a variable named S of type T with options O defined at POS,
   to `variables'.  */

void
note_variable (s, t, o, pos)
     const char *s;
     type_p t;
     options_p o;
     struct fileloc *pos;
{
  pair_p n;
  n = xmalloc (sizeof (*n));
  n->name = s;
  n->type = t;
  n->line = *pos;
  n->opt = o;
  n->next = variables;
  variables = n;
}

/* Create a union for YYSTYPE, as yacc would do it, given a fieldlist FIELDS
   and information about the correspondance between token types and fields
   in TYPEINFO.  POS is used for error messages.  */

void
note_yacc_type (o, fields, typeinfo, pos)
     options_p o;
     pair_p fields;
     pair_p typeinfo;
     struct fileloc *pos;
{
  pair_p p;
  pair_p *p_p;
  
  for (p = typeinfo; p; p = p->next)
    {
      pair_p m;
      
      if (p->name == NULL)
	continue;

      if (p->type == (type_p) 1)
	{
	  pair_p pp;
	  int ok = 0;
	  
	  for (pp = typeinfo; pp; pp = pp->next)
	    if (pp->type != (type_p) 1
		&& strcmp (pp->opt->info, p->opt->info) == 0)
	      {
		ok = 1;
		break;
	      }
	  if (! ok)
	    continue;
	}

      for (m = fields; m; m = m->next)
	if (strcmp (m->name, p->name) == 0)
	  p->type = m->type;
      if (p->type == NULL)
	{
	  error_at_line (&p->line, 
			 "couldn't match fieldname `%s'", p->name);
	  p->name = NULL;
	}
    }
  
  p_p = &typeinfo;
  while (*p_p)
    {
      pair_p p = *p_p;

      if (p->name == NULL
	  || p->type == (type_p) 1)
	*p_p = p->next;
      else
	p_p = &p->next;
    }

  new_structure ("yy_union", 1, pos, typeinfo, o);
  do_typedef ("YYSTYPE", find_structure ("yy_union", 1), pos);
}

static void process_gc_options PARAMS ((options_p, enum gc_used_enum, int *));
static void set_gc_used_type PARAMS ((type_p, enum gc_used_enum));
static void set_gc_used PARAMS ((pair_p));

/* Handle OPT for set_gc_used_type.  */

static void
process_gc_options (opt, level, maybe_undef)
     options_p opt;
     enum gc_used_enum level;
     int *maybe_undef;
{
  options_p o;
  for (o = opt; o; o = o->next)
    if (strcmp (o->name, "ptr_alias") == 0 && level == GC_POINTED_TO)
      set_gc_used_type ((type_p) o->info, GC_POINTED_TO);
    else if (strcmp (o->name, "maybe_undef") == 0)
      *maybe_undef = 1;
}

/* Set the gc_used field of T to LEVEL, and handle the types it references.  */

static void
set_gc_used_type (t, level)
     type_p t;
     enum gc_used_enum level;
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

	process_gc_options (t->u.s.opt, level, &dummy);

	for (f = t->u.s.fields; f; f = f->next)
	  {
	    int maybe_undef = 0;
	    process_gc_options (t->u.s.opt, level, &maybe_undef);
	    
	    if (maybe_undef && f->type->kind == TYPE_POINTER)
	      set_gc_used_type (f->type->u.p, GC_MAYBE_POINTED_TO);
	    else
	      set_gc_used_type (f->type, GC_USED);
	  }
	break;
      }

    case TYPE_POINTER:
      set_gc_used_type (t->u.p, GC_POINTED_TO);
      break;

    case TYPE_ARRAY:
      set_gc_used_type (t->u.a.p, GC_USED);
      break;
      
    case TYPE_LANG_STRUCT:
      for (t = t->u.s.lang_struct; t; t = t->next)
	set_gc_used_type (t, level);
      break;

    case TYPE_PARAM_STRUCT:
      set_gc_used_type (t->u.param_struct.param, GC_POINTED_TO);
      set_gc_used_type (t->u.param_struct.stru, GC_USED);
      break;

    default:
      break;
    }
}

/* Set the gc_used fileds of all the types pointed to by VARIABLES.  */

static void
set_gc_used (variables)
     pair_p variables;
{
  pair_p p;
  for (p = variables; p; p = p->next)
    set_gc_used_type (p->type, GC_USED);
}

/* File mapping routines.  For each input file, there is one output .c file
   (but some output files have many input files), and there is one .h file
   for the whole build.  */

/* The list of output files.  */
static outf_p output_files;

/* The output header file that is included into pretty much every
   source file.  */
outf_p header_file;

/* Number of files specified in gtfiles.  */
#define NUM_GT_FILES (ARRAY_SIZE (all_files) - 1)

/* Number of files in the language files array.  */
#define NUM_LANG_FILES (ARRAY_SIZE (lang_files) - 1)

/* Length of srcdir name.  */
static int srcdir_len = 0;

#define NUM_BASE_FILES (ARRAY_SIZE (lang_dir_names) - 1)
outf_p base_files[NUM_BASE_FILES];

static outf_p create_file PARAMS ((const char *, const char *));
static const char * get_file_basename PARAMS ((const char *));

/* Create and return an outf_p for a new file for NAME, to be called
   ONAME.  */

static outf_p
create_file (name, oname)
     const char *name;
     const char *oname;
{
  static const char *const hdr[] = {
    "   Copyright (C) 2002 Free Software Foundation, Inc.\n",
    "\n",
    "This file is part of GCC.\n",
    "\n",
    "GCC is free software; you can redistribute it and/or modify it under\n",
    "the terms of the GNU General Public License as published by the Free\n",
    "Software Foundation; either version 2, or (at your option) any later\n",
    "version.\n",
    "\n",
    "GCC is distributed in the hope that it will be useful, but WITHOUT ANY\n",
    "WARRANTY; without even the implied warranty of MERCHANTABILITY or\n",
    "FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License\n",
    "for more details.\n",
    "\n",
    "You should have received a copy of the GNU General Public License\n",
    "along with GCC; see the file COPYING.  If not, write to the Free\n",
    "Software Foundation, 59 Temple Place - Suite 330, Boston, MA\n",
    "02111-1307, USA.  */\n",
    "\n",
    "/* This file is machine generated.  Do not edit.  */\n"
  };
  outf_p f;
  size_t i;
  
  f = xcalloc (sizeof (*f), 1);
  f->next = output_files;
  f->name = oname;
  output_files = f;

  oprintf (f, "/* Type information for %s.\n", name);
  for (i = 0; i < ARRAY_SIZE (hdr); i++)
    oprintf (f, "%s", hdr[i]);
  return f;
}

/* Print, like fprintf, to O.  */
void 
oprintf VPARAMS ((outf_p o, const char *format, ...))
{
  char *s;
  size_t slength;
  
  VA_OPEN (ap, format);
  VA_FIXEDARG (ap, outf_p, o);
  VA_FIXEDARG (ap, const char *, format);
  slength = xvasprintf (&s, format, ap);

  if (o->bufused + slength > o->buflength)
    {
      size_t new_len = o->buflength;
      if (new_len == 0)
	new_len = 1024;
      do {
	new_len *= 2;
      } while (o->bufused + slength >= new_len);
      o->buf = xrealloc (o->buf, new_len);
      o->buflength = new_len;
    }
  memcpy (o->buf + o->bufused, s, slength);
  o->bufused += slength;
  free (s);
  VA_CLOSE (ap);
}

/* Open the global header file and the language-specific header files.  */

static void open_base_files PARAMS((void));

static void
open_base_files ()
{
  size_t i;
  
  header_file = create_file ("GCC", "gtype-desc.h");

  for (i = 0; i < NUM_BASE_FILES; i++)
    base_files[i] = create_file (lang_dir_names[i], 
				 xasprintf ("gtype-%s.h", lang_dir_names[i]));

  /* gtype-desc.c is a little special, so we create it here.  */
  {
    /* The order of files here matters very much.  */
    static const char *const ifiles [] = {
      "config.h", "system.h", "varray.h", "hashtab.h",
      "bitmap.h", "tree.h", "rtl.h", "function.h", "insn-config.h",
      "expr.h", "hard-reg-set.h", "basic-block.h", "cselib.h",
      "insn-addr.h", "ssa.h", "optabs.h", "libfuncs.h",
      "debug.h", "ggc.h",
      NULL
    };
    const char *const *ifp;
    outf_p gtype_desc_c;
      
    gtype_desc_c = create_file ("GCC", "gtype-desc.c");
    for (ifp = ifiles; *ifp; ifp++)
      oprintf (gtype_desc_c, "#include \"%s\"\n", *ifp);
  }
}

/* Determine the pathname to F relative to $(srcdir).  */

static const char *
get_file_basename (f)
     const char *f;
{
  size_t len;
  const char *basename;
  unsigned i;
  
  basename = strrchr (f, '/');
  
  if (!basename)
    return f;
  
  len = strlen (f);
  basename++;
  
  for (i = 1; i < NUM_BASE_FILES; i++)
    {
      const char * s1;
      const char * s2;
      int l1;
      int l2;
      s1 = basename - strlen (lang_dir_names [i]) - 1;
      s2 = lang_dir_names [i];
      l1 = strlen (s1);
      l2 = strlen (s2);
      if (l1 >= l2 && !memcmp (s1, s2, l2))
        {
          basename -= l2 + 1;
          if ((basename - f - 1) != srcdir_len)
            abort (); /* Match is wrong - should be preceded by $srcdir.  */
          break;
        }
    }
  
  return basename;
}

/* Return a bitmap which has bit `1 << BASE_FILE_<lang>' set iff
   INPUT_FILE is used by <lang>.  

   This function should be written to assume that a file _is_ used
   if the situation is unclear.  If it wrongly assumes a file _is_ used,
   a linker error will result.  If it wrongly assumes a file _is not_ used,
   some GC roots may be missed, which is a much harder-to-debug problem.  */

unsigned
get_base_file_bitmap (input_file)
     const char *input_file;
{
  const char *basename = get_file_basename (input_file);
  const char *slashpos = strchr (basename, '/');
  unsigned j;
  unsigned k;
  unsigned bitmap;
  
  if (slashpos)
    {
      size_t i;
      for (i = 1; i < NUM_BASE_FILES; i++)
	if ((size_t)(slashpos - basename) == strlen (lang_dir_names [i])
	    && memcmp (basename, lang_dir_names[i], strlen (lang_dir_names[i])) == 0)
          {
            /* It's in a language directory, set that language.  */
            bitmap = 1 << i;
            return bitmap;
          }

      abort (); /* Should have found the language.  */
    }

  /* If it's in any config-lang.in, then set for the languages
     specified.  */

  bitmap = 0;

  for (j = 0; j < NUM_LANG_FILES; j++)
    {
      if (!strcmp(input_file, lang_files[j]))
        {
          for (k = 0; k < NUM_BASE_FILES; k++)
            {
              if (!strcmp(lang_dir_names[k], langs_for_lang_files[j]))
                bitmap |= (1 << k);
            }
        }
    }
    
  /* Otherwise, set all languages.  */
  if (!bitmap)
    bitmap = (1 << NUM_BASE_FILES) - 1;

  return bitmap;
}

/* An output file, suitable for definitions, that can see declarations
   made in INPUT_FILE and is linked into every language that uses
   INPUT_FILE.  */

outf_p
get_output_file_with_visibility (input_file)
     const char *input_file;
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

  /* Determine the output file name.  */
  basename = get_file_basename (input_file);

  len = strlen (basename);
  if ((len > 2 && memcmp (basename+len-2, ".c", 2) == 0)
      || (len > 2 && memcmp (basename+len-2, ".y", 2) == 0)
      || (len > 3 && memcmp (basename+len-3, ".in", 3) == 0))
    {
      char *s;
      
      output_name = s = xasprintf ("gt-%s", basename);
      for (; *s != '.'; s++)
	if (! ISALNUM (*s) && *s != '-')
	  *s = '-';
      memcpy (s, ".h", sizeof (".h"));
      for_name = basename;
    }
  else if (strcmp (basename, "c-common.h") == 0)
    output_name = "gt-c-common.h", for_name = "c-common.c";
  else if (strcmp (basename, "c-tree.h") == 0)
    output_name = "gt-c-decl.h", for_name = "c-decl.c";
  else 
    {
      size_t i;
      
      for (i = 0; i < NUM_BASE_FILES; i++)
	if (memcmp (basename, lang_dir_names[i], strlen (lang_dir_names[i])) == 0
	    && basename[strlen(lang_dir_names[i])] == '/')
	  return base_files[i];

      output_name = "gtype-desc.c";
      for_name = NULL;
    }

  /* Look through to see if we've ever seen this output filename before.  */
  for (r = output_files; r; r = r->next)
    if (strcmp (r->name, output_name) == 0)
      return r;

  /* If not, create it.  */
  r = create_file (for_name, output_name);

  return r;
}

/* The name of an output file, suitable for definitions, that can see
   declarations made in INPUT_FILE and is linked into every language
   that uses INPUT_FILE.  */

const char *
get_output_file_name (input_file)
     const char *input_file;
{
  return get_output_file_with_visibility (input_file)->name;
}

/* Copy the output to its final destination,
   but don't unnecessarily change modification times.  */

static void close_output_files PARAMS ((void));

static void
close_output_files ()
{
  outf_p of;
  
  for (of = output_files; of; of = of->next)
    {
      FILE * newfile;

      newfile = fopen (of->name, "r");
      if (newfile != NULL )
	{
	  int no_write_p;
	  size_t i;

	  for (i = 0; i < of->bufused; i++)
	    {
	      int ch;
	      ch = fgetc (newfile);
	      if (ch == EOF || ch != (unsigned char) of->buf[i])
		break;
	    }
	  no_write_p = i == of->bufused && fgetc (newfile) == EOF;
	  fclose (newfile);

	  if (no_write_p)
	    continue;
	}

      newfile = fopen (of->name, "w");
      if (newfile == NULL)
	{
	  perror ("opening output file");
	  exit (1);
	}
      if (fwrite (of->buf, 1, of->bufused, newfile) != of->bufused)
	{
	  perror ("writing output file");
	  exit (1);
	}
      if (fclose (newfile) != 0)
	{
	  perror ("closing output file");
	  exit (1);
	}
    }
}

struct flist {
  struct flist *next;
  int started_p;
  const char *name;
  outf_p f;
};

static void output_escaped_param PARAMS ((outf_p , const char *, const char *,
					  const char *, const char *,
					  struct fileloc *));
static void write_gc_structure_fields 
  PARAMS ((outf_p , type_p, const char *, const char *, options_p, 
	   int, struct fileloc *, lang_bitmap, type_p));
static void write_gc_marker_routine_for_structure PARAMS ((type_p, type_p));
static void write_gc_types PARAMS ((type_p structures, type_p param_structs));
static void put_mangled_filename PARAMS ((outf_p , const char *));
static void finish_root_table PARAMS ((struct flist *flp, const char *pfx, 
				       const char *tname, const char *lastname,
				       const char *name));
static void write_gc_root PARAMS ((outf_p , pair_p, type_p, const char *, int,
				   struct fileloc *, const char *));
static void write_gc_roots PARAMS ((pair_p));

static int gc_counter;

/* Print PARAM to OF processing escapes.  VAL references the current object,
   PREV_VAL the object containing the current object, ONAME is the name
   of the option and LINE is used to print error messages.  */

static void
output_escaped_param (of, param, val, prev_val, oname, line)
     outf_p of;
     const char *param;
     const char *val;
     const char *prev_val;
     const char *oname;
     struct fileloc *line;
{
  const char *p;
  
  for (p = param; *p; p++)
    if (*p != '%')
      oprintf (of, "%c", *p);
    else if (*++p == 'h')
      oprintf (of, "(%s)", val);
    else if (*p == '0')
      oprintf (of, "(*x)");
    else if (*p == '1')
      oprintf (of, "(%s)", prev_val);
    else
      error_at_line (line, "`%s' option contains bad escape %c%c",
		     oname, '%', *p);
}

/* Write out code to OF which marks the fields of S.  VAL references
   the current object, PREV_VAL the object containing the current
   object, OPTS is a list of options to apply, INDENT is the current
   indentation level, LINE is used to print error messages, BITMAP
   indicates which languages to print the structure for, and PARAM is
   the current parameter (from an enclosing param_is option).  */

static void
write_gc_structure_fields (of, s, val, prev_val, opts, indent, line, bitmap,
			   param)
     outf_p of;
     type_p s;
     const char *val;
     const char *prev_val;
     options_p opts;
     int indent;
     struct fileloc *line;
     lang_bitmap bitmap;
     type_p param;
{
  pair_p f;
  int tagcounter = -1;

  if (! s->u.s.line.file)
    error_at_line (line, "incomplete structure `%s'", s->u.s.tag);
  else if ((s->u.s.bitmap & bitmap) != bitmap)
    {
      error_at_line (line, "structure defined for mismatching languages");
      error_at_line (&s->u.s.line, "one structure defined here");
    }
  
  if (s->kind == TYPE_UNION)
    {
      const char *tagexpr = NULL;
      options_p oo;
      
      tagcounter = ++gc_counter;
      for (oo = opts; oo; oo = oo->next)
	if (strcmp (oo->name, "desc") == 0)
	  tagexpr = (const char *)oo->info;
      if (tagexpr == NULL)
	{
	  tagexpr = "1";
	  error_at_line (line, "missing `desc' option");
	}

      oprintf (of, "%*s{\n", indent, "");
      indent += 2;
      oprintf (of, "%*sunsigned int tag%d = (", indent, "", tagcounter);
      output_escaped_param (of, tagexpr, val, prev_val, "desc", line);
      oprintf (of, ");\n");
    }
  
  for (f = s->u.s.fields; f; f = f->next)
    {
      const char *tagid = NULL;
      const char *length = NULL;
      const char *special = NULL;
      int skip_p = 0;
      int always_p = 0;
      int maybe_undef_p = 0;
      int use_param_p = 0;
      options_p oo;
      type_p t = f->type;
      
      if (t->kind == TYPE_SCALAR
	  || (t->kind == TYPE_ARRAY 
	      && t->u.a.p->kind == TYPE_SCALAR))
	continue;
      
      for (oo = f->opt; oo; oo = oo->next)
	if (strcmp (oo->name, "length") == 0)
	  length = (const char *)oo->info;
	else if (strcmp (oo->name, "maybe_undef") == 0)
	  maybe_undef_p = 1;
	else if (strcmp (oo->name, "tag") == 0)
	  tagid = (const char *)oo->info;
	else if (strcmp (oo->name, "special") == 0)
	  special = (const char *)oo->info;
	else if (strcmp (oo->name, "skip") == 0)
	  skip_p = 1;
	else if (strcmp (oo->name, "always") == 0)
	  always_p = 1;
	else if (strcmp (oo->name, "desc") == 0 && UNION_P (t))
	  ;
 	else if (strcmp (oo->name, "descbits") == 0 && UNION_P (t))
	  ;
 	else if (strcmp (oo->name, "param_is") == 0)
	  ;
	else if (strcmp (oo->name, "use_param") == 0)
	  use_param_p = 1;
	else
	  error_at_line (&f->line, "unknown field option `%s'\n", oo->name);

      if (skip_p)
	continue;
      
      if (use_param_p)
	{
	  if (param != NULL)
	    {
	      type_p t1;
	      type_p nt = param;
	      int arraycount = 0;
	      
	      for (t1 = t; t->kind == TYPE_ARRAY; t = t->u.a.p)
		arraycount++;
	      for (; t->kind == TYPE_POINTER; t = t->u.p)
		nt = create_pointer (nt);
	      while (arraycount-- > 0)
		nt = create_array (nt, t->u.a.len);
	      t = nt;
	    }
	  else if (s->kind == TYPE_UNION && ! always_p && tagid)
	    ;
	  else
	    error_at_line (&f->line, "no parameter defined");
	}

      if (maybe_undef_p
	  && (t->kind != TYPE_POINTER
	      || t->u.p->kind != TYPE_STRUCT))
	error_at_line (&f->line, 
		       "field `%s' has invalid option `maybe_undef_p'\n",
		       f->name);
      if (s->kind == TYPE_UNION && ! always_p )
	{
	  if (! tagid)
	    {
	      error_at_line (&f->line, "field `%s' has no tag", f->name);
	      continue;
	    }
	  oprintf (of, "%*sif (tag%d == (%s)) {\n", indent, "", 
		   tagcounter, tagid);
	  indent += 2;
	}
      
      switch (t->kind)
	{
	case TYPE_STRING:
	  /* Do nothing; strings go in the string pool.  */
	  break;

	case TYPE_LANG_STRUCT:
	  {
	    type_p ti;
	    for (ti = t->u.s.lang_struct; ti; ti = ti->next)
	      if (ti->u.s.bitmap & bitmap)
		{
		  t = ti;
		  break;
		}
	    if (ti == NULL)
	      {
		error_at_line (&f->line, 
			       "structure not defined for this language");
		break;
	      }
	  }
	  /* Fall through... */
	case TYPE_STRUCT:
	case TYPE_UNION:
	  {
	    char *newval;

	    newval = xasprintf ("%s.%s", val, f->name);
	    write_gc_structure_fields (of, t, newval, val, f->opt, indent, 
				       &f->line, bitmap, param);
	    free (newval);
	    break;
	  }

	case TYPE_POINTER:
	  if (! length)
	    {
	      if (maybe_undef_p
		  && t->u.p->u.s.line.file == NULL)
		oprintf (of, "%*sif (%s.%s) abort();\n", indent, "",
			 val, f->name);
	      else if (UNION_OR_STRUCT_P (t->u.p))
		oprintf (of, "%*sgt_ggc_m_%s (%s.%s);\n", indent, "", 
			 t->u.p->u.s.tag, val, f->name);
	      else if (t->u.p->kind == TYPE_PARAM_STRUCT)
		oprintf (of, "%*sgt_ggc_mm_%d%s_%s (%s.%s);\n", indent, "",
			 (int) strlen (t->u.p->u.param_struct.param->u.s.tag),
			 t->u.p->u.param_struct.param->u.s.tag,
			 t->u.p->u.param_struct.stru->u.s.tag,
			 val, f->name);
	      else
		error_at_line (&f->line, "field `%s' is pointer to scalar",
			       f->name);
	      break;
	    }
	  else if (t->u.p->kind == TYPE_SCALAR
		   || t->u.p->kind == TYPE_STRING)
	    oprintf (of, "%*sggc_mark (%s.%s);\n", indent, "", 
		     val, f->name);
	  else
	    {
	      int loopcounter = ++gc_counter;
	      
	      oprintf (of, "%*sif (%s.%s != NULL) {\n", indent, "",
		       val, f->name);
	      indent += 2;
	      oprintf (of, "%*ssize_t i%d;\n", indent, "", loopcounter);
	      oprintf (of, "%*sggc_set_mark (%s.%s);\n", indent, "", 
		       val, f->name);
	      oprintf (of, "%*sfor (i%d = 0; i%d < (size_t)(", indent, "", 
		       loopcounter, loopcounter);
	      output_escaped_param (of, length, val, prev_val, "length", line);
	      oprintf (of, "); i%d++) {\n", loopcounter);
	      indent += 2;
	      switch (t->u.p->kind)
		{
		case TYPE_STRUCT:
		case TYPE_UNION:
		  {
		    char *newval;
		    
		    newval = xasprintf ("%s.%s[i%d]", val, f->name, 
					loopcounter);
		    write_gc_structure_fields (of, t->u.p, newval, val,
					       f->opt, indent, &f->line,
					       bitmap, param);
		    free (newval);
		    break;
		  }
		case TYPE_POINTER:
		  if (UNION_OR_STRUCT_P (t->u.p->u.p))
		    oprintf (of, "%*sgt_ggc_m_%s (%s.%s[i%d]);\n", indent, "", 
			     t->u.p->u.p->u.s.tag, val, f->name,
			     loopcounter);
		  else
		    error_at_line (&f->line, 
				   "field `%s' is array of pointer to scalar",
				   f->name);
		  break;
		default:
		  error_at_line (&f->line, 
				 "field `%s' is array of unimplemented type",
				 f->name);
		  break;
		}
	      indent -= 2;
	      oprintf (of, "%*s}\n", indent, "");
	      indent -= 2;
	      oprintf (of, "%*s}\n", indent, "");
	    }
	  break;

	case TYPE_ARRAY:
	  {
	    int loopcounter = ++gc_counter;
	    type_p ta;
	    int i;

	    if (! length &&
		(strcmp (t->u.a.len, "0") == 0
		 || strcmp (t->u.a.len, "1") == 0))
	      error_at_line (&f->line, 
			     "field `%s' is array of size %s",
			     f->name, t->u.a.len);
	    
	    /* Arrays of scalars can be ignored.  */
	    for (ta = t; ta->kind == TYPE_ARRAY; ta = ta->u.a.p)
	      ;
	    if (ta->kind == TYPE_SCALAR
		|| ta->kind == TYPE_STRING)
	      break;

	    oprintf (of, "%*s{\n", indent, "");
	    indent += 2;

	    if (special != NULL && strcmp (special, "tree_exp") == 0)
	      {
		oprintf (of, "%*sconst size_t tree_exp_size = (",
                         indent, "");
		output_escaped_param (of, length, val, prev_val,
				      "length", line);
		oprintf (of, ");\n");

		length = "first_rtl_op (TREE_CODE ((tree)&%h))";
	      }

	    for (ta = t, i = 0; ta->kind == TYPE_ARRAY; ta = ta->u.a.p, i++)
	      {
		oprintf (of, "%*ssize_t i%d_%d;\n", 
			 indent, "", loopcounter, i);
		oprintf (of, "%*sconst size_t ilimit%d_%d = (",
			 indent, "", loopcounter, i);
		if (i == 0 && length != NULL)
		  output_escaped_param (of, length, val, prev_val, 
					"length", line);
		else
		  oprintf (of, "%s", ta->u.a.len);
		oprintf (of, ");\n");
	      }
		
	    for (ta = t, i = 0; ta->kind == TYPE_ARRAY; ta = ta->u.a.p, i++)
	      {
		oprintf (of, 
		 "%*sfor (i%d_%d = 0; i%d_%d < ilimit%d_%d; i%d_%d++) {\n",
			 indent, "", loopcounter, i, loopcounter, i,
			 loopcounter, i, loopcounter, i);
		indent += 2;
	      }

	    if (ta->kind == TYPE_POINTER
		&& (ta->u.p->kind == TYPE_STRUCT
		    || ta->u.p->kind == TYPE_UNION))
	      {
		oprintf (of, "%*sgt_ggc_m_%s (%s.%s", 
			 indent, "", ta->u.p->u.s.tag, val, f->name);
		for (ta = t, i = 0; 
		     ta->kind == TYPE_ARRAY; 
		     ta = ta->u.a.p, i++)
		  oprintf (of, "[i%d_%d]", loopcounter, i);
		oprintf (of, ");\n");
	      }
	    else if (ta->kind == TYPE_STRUCT || ta->kind == TYPE_UNION)
	      {
		char *newval;
		int len;
		
		len = strlen (val) + strlen (f->name) + 2;
		for (ta = t; ta->kind == TYPE_ARRAY; ta = ta->u.a.p)
		  len += sizeof ("[i_]") + 2*6;
		
		newval = xmalloc (len);
		sprintf (newval, "%s.%s", val, f->name);
		for (ta = t, i = 0; 
		     ta->kind == TYPE_ARRAY; 
		     ta = ta->u.a.p, i++)
		  sprintf (newval + strlen (newval), "[i%d_%d]", 
			   loopcounter, i);
		write_gc_structure_fields (of, t->u.p, newval, val,
					   f->opt, indent, &f->line, bitmap,
					   param);
		free (newval);
	      }
	    else if (ta->kind == TYPE_POINTER && ta->u.p->kind == TYPE_SCALAR
		     && use_param_p && param == NULL)
	      oprintf (of, "%*sabort();\n", indent, "");
	    else
	      error_at_line (&f->line, 
			     "field `%s' is array of unimplemented type",
			     f->name);
	    for (ta = t, i = 0; ta->kind == TYPE_ARRAY; ta = ta->u.a.p, i++)
	      {
		indent -= 2;
		oprintf (of, "%*s}\n", indent, "");
	      }

	    if (special != NULL && strcmp (special, "tree_exp") == 0)
	      {
		oprintf (of, 
		 "%*sfor (; i%d_0 < tree_exp_size; i%d_0++)\n",
			 indent, "", loopcounter, loopcounter);
		oprintf (of, "%*s  gt_ggc_m_rtx_def (%s.%s[i%d_0]);\n",
			 indent, "", val, f->name, loopcounter);
		special = NULL;
	      }

	    indent -= 2;
	    oprintf (of, "%*s}\n", indent, "");
	    break;
	  }

	default:
	  error_at_line (&f->line, 
			 "field `%s' is unimplemented type",
			 f->name);
	  break;
	}
      
      if (s->kind == TYPE_UNION && ! always_p )
	{
	  indent -= 2;
	  oprintf (of, "%*s}\n", indent, "");
	}
      if (special)
	error_at_line (&f->line, "unhandled special `%s'", special);
    }
  if (s->kind == TYPE_UNION)
    {
      indent -= 2;
      oprintf (of, "%*s}\n", indent, "");
    }
}

/* Write out a marker routine for S.  PARAM is the parameter from an
   enclosing PARAM_IS option.  */

static void
write_gc_marker_routine_for_structure (s, param)
     type_p s;
     type_p param;
{
  outf_p f;
  if (param == NULL)
    f = get_output_file_with_visibility (s->u.s.line.file);
  else
    f = get_output_file_with_visibility (param->u.s.line.file);
  
  oprintf (f, "%c", '\n');
  oprintf (f, "void\n");
  if (param == NULL)
    oprintf (f, "gt_ggc_mx_%s (x_p)\n", s->u.s.tag);
  else
    oprintf (f, "gt_ggc_mm_%d%s_%s (x_p)\n", (int) strlen (param->u.s.tag),
	     param->u.s.tag, s->u.s.tag);
  oprintf (f, "      void *x_p;\n");
  oprintf (f, "{\n");
  oprintf (f, "  %s %s * const x = (%s %s *)x_p;\n",
	   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag,
	   s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag);
  oprintf (f, "  if (! ggc_test_and_set_mark (x))\n");
  oprintf (f, "    return;\n");
  
  gc_counter = 0;
  write_gc_structure_fields (f, s, "(*x)", "not valid postage",
			     s->u.s.opt, 2, &s->u.s.line, s->u.s.bitmap,
			     param);
  
  oprintf (f, "}\n");
}

/* Write out marker routines for STRUCTURES and PARAM_STRUCTS.  */

static void
write_gc_types (structures, param_structs)
     type_p structures;
     type_p param_structs;
{
  type_p s;
  
  oprintf (header_file, "\n/* GC marker procedures.  */\n");
  for (s = structures; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO
	|| s->gc_used == GC_MAYBE_POINTED_TO)
      {
	options_p opt;
	
	if (s->gc_used == GC_MAYBE_POINTED_TO
	    && s->u.s.line.file == NULL)
	  continue;

	oprintf (header_file,
		 "#define gt_ggc_m_%s(X) do { \\\n", s->u.s.tag);
	oprintf (header_file,
		 "  if (X != NULL) gt_ggc_mx_%s (X);\\\n", s->u.s.tag);
	oprintf (header_file,
		 "  } while (0)\n");
	
	for (opt = s->u.s.opt; opt; opt = opt->next)
	  if (strcmp (opt->name, "ptr_alias") == 0)
	    {
	      type_p t = (type_p) opt->info;
	      if (t->kind == TYPE_STRUCT 
		  || t->kind == TYPE_UNION
		  || t->kind == TYPE_LANG_STRUCT)
		oprintf (header_file,
			 "#define gt_ggc_mx_%s gt_ggc_mx_%s\n",
			 s->u.s.tag, t->u.s.tag);
	      else
		error_at_line (&s->u.s.line, 
			       "structure alias is not a structure");
	      break;
	    }
	if (opt)
	  continue;

	/* Declare the marker procedure only once.  */
	oprintf (header_file, 
		 "extern void gt_ggc_mx_%s PARAMS ((void *));\n",
		 s->u.s.tag);
  
	if (s->u.s.line.file == NULL)
	  {
	    fprintf (stderr, "warning: structure `%s' used but not defined\n", 
		     s->u.s.tag);
	    continue;
	  }
  
	if (s->kind == TYPE_LANG_STRUCT)
	  {
	    type_p ss;
	    for (ss = s->u.s.lang_struct; ss; ss = ss->next)
	      write_gc_marker_routine_for_structure (ss, NULL);
	  }
	else
	  write_gc_marker_routine_for_structure (s, NULL);
      }

  for (s = param_structs; s; s = s->next)
    if (s->gc_used == GC_POINTED_TO)
      {
	type_p param = s->u.param_struct.param;
	type_p stru = s->u.param_struct.stru;

	if (param->kind != TYPE_STRUCT && param->kind != TYPE_UNION
	    && param->kind != TYPE_LANG_STRUCT)
	  {
	    error_at_line (&s->u.param_struct.line,
			   "unsupported parameter type");
	    continue;
	  }
	
	/* Declare the marker procedure.  */
	oprintf (header_file, 
		 "extern void gt_ggc_mm_%d%s_%s PARAMS ((void *));\n",
		 (int) strlen (param->u.s.tag), param->u.s.tag,
		 stru->u.s.tag);
  
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
	      write_gc_marker_routine_for_structure (ss, param);
	  }
	else
	  write_gc_marker_routine_for_structure (stru, param);
      }
}

/* Mangle FN and print it to F.  */

static void
put_mangled_filename (f, fn)
     outf_p f;
     const char *fn;
{
  const char *name = get_output_file_name (fn);
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
finish_root_table (flp, pfx, lastname, tname, name)
     struct flist *flp;
     const char *pfx;
     const char *tname;
     const char *lastname;
     const char *name;
{
  struct flist *fli2;
  unsigned started_bitmap = 0;
  
  for (fli2 = flp; fli2; fli2 = fli2->next)
    if (fli2->started_p)
      {
	oprintf (fli2->f, "  %s\n", lastname);
	oprintf (fli2->f, "};\n\n");
      }

  for (fli2 = flp; fli2; fli2 = fli2->next)
    if (fli2->started_p)
      {
	lang_bitmap bitmap = get_base_file_bitmap (fli2->name);
	int fnum;

	for (fnum = 0; bitmap != 0; fnum++, bitmap >>= 1)
	  if (bitmap & 1)
	    {
	      oprintf (base_files[fnum],
		       "extern const struct %s gt_ggc_%s_",
		       tname, pfx);
	      put_mangled_filename (base_files[fnum], fli2->name);
	      oprintf (base_files[fnum], "[];\n");
	    }
      }

  for (fli2 = flp; fli2; fli2 = fli2->next)
    if (fli2->started_p)
      {
	lang_bitmap bitmap = get_base_file_bitmap (fli2->name);
	int fnum;

	fli2->started_p = 0;

	for (fnum = 0; bitmap != 0; fnum++, bitmap >>= 1)
	  if (bitmap & 1)
	    {
	      if (! (started_bitmap & (1 << fnum)))
		{
		  oprintf (base_files [fnum],
			   "const struct %s * const %s[] = {\n",
			   tname, name);
		  started_bitmap |= 1 << fnum;
		}
	      oprintf (base_files[fnum], "  gt_ggc_%s_", pfx);
	      put_mangled_filename (base_files[fnum], fli2->name);
	      oprintf (base_files[fnum], ",\n");
	    }
      }

  {
    unsigned bitmap;
    int fnum;
    
    for (bitmap = started_bitmap, fnum = 0; bitmap != 0; fnum++, bitmap >>= 1)
      if (bitmap & 1)
	{
	  oprintf (base_files[fnum], "  NULL\n");
	  oprintf (base_files[fnum], "};\n\n");
	}
  }
}

/* Write out to F the table entry and any marker routines needed to
   mark NAME as TYPE.  The original variable is V, at LINE.
   HAS_LENGTH is nonzero iff V was a variable-length array.  IF_MARKED
   is nonzero iff we are building the root table for hash table caches.  */

static void
write_gc_root (f, v, type, name, has_length, line, if_marked)
     outf_p f;
     pair_p v;
     type_p type;
     const char *name;
     int has_length;
     struct fileloc *line;
     const char *if_marked;
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
		desc = (const char *)o->info;
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
			tag = (const char *)oo->info;
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
		    write_gc_root (f, v, validf->type, newname, 0, line,
				   if_marked);
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
		write_gc_root (f, v, fld->type, newname, 0, line, if_marked);
		free (newname);
	      }
	  }
      }
      break;

    case TYPE_ARRAY:
      {
	char *newname;
	newname = xasprintf ("%s[0]", name);
	write_gc_root (f, v, type->u.a.p, newname, has_length, line, if_marked);
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
	    oprintf (f, "    &gt_ggc_mx_%s\n", tp->u.s.tag);
	  }
	else if (! has_length && tp->kind == TYPE_PARAM_STRUCT)
	  {
	    oprintf (f, "    &gt_ggc_mm_%d%s_%s",
		     (int) strlen (tp->u.param_struct.param->u.s.tag),
		     tp->u.param_struct.param->u.s.tag,
		     tp->u.param_struct.stru->u.s.tag);
	  }
	else if (has_length
		 && (tp->kind == TYPE_POINTER || UNION_OR_STRUCT_P (tp)))
	  {
	    oprintf (f, "    &gt_ggc_ma_%s", name);
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

    case TYPE_SCALAR:
    case TYPE_STRING:
      break;
      
    default:
      error_at_line (line, 
		     "global `%s' is unimplemented type",
		     name);
    }
}

/* Output a table describing the locations and types of VARIABLES.  */

static void
write_gc_roots (variables)
     pair_p variables;
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
	  length = (const char *)o->info;
	else if (strcmp (o->name, "deletable") == 0)
	  deletable_p = 1;
	else if (strcmp (o->name, "param_is") == 0)
	  ;
	else if (strcmp (o->name, "if_marked") == 0)
	  ;
	else
	  error_at_line (&v->line, 
			 "global `%s' has unknown option `%s'",
			 v->name, o->name);

      for (fli = flp; fli; fli = fli->next)
	if (fli->f == f)
	  break;
      if (fli == NULL)
	{
	  fli = xmalloc (sizeof (*fli));
	  fli->f = f;
	  fli->next = flp;
	  fli->started_p = 0;
	  fli->name = v->line.file;
	  flp = fli;

	  oprintf (f, "\n/* GC roots.  */\n\n");
	}

      if (! deletable_p
	  && length
	  && v->type->kind == TYPE_POINTER
	  && (v->type->u.p->kind == TYPE_POINTER
	      || v->type->u.p->kind == TYPE_STRUCT))
	{
	  oprintf (f, "static void gt_ggc_ma_%s PARAMS ((void *));\n",
		   v->name);
	  oprintf (f, "static void\ngt_ggc_ma_%s (x_p)\n      void *x_p;\n",
		   v->name);
	  oprintf (f, "{\n");
	  oprintf (f, "  size_t i;\n");

	  if (v->type->u.p->kind == TYPE_POINTER)
	    {
	      type_p s = v->type->u.p->u.p;

	      oprintf (f, "  %s %s ** const x = (%s %s **)x_p;\n",
		       s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag,
		       s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag);
	      oprintf (f, "  if (ggc_test_and_set_mark (x))\n");
	      oprintf (f, "    for (i = 0; i < (%s); i++)\n", length);
	      if (s->kind != TYPE_STRUCT && s->kind != TYPE_UNION)
		{
		  error_at_line (&v->line, 
				 "global `%s' has unsupported ** type",
				 v->name);
		  continue;
		}

	      oprintf (f, "      gt_ggc_m_%s (x[i]);\n", s->u.s.tag);
	    }
	  else
	    {
	      type_p s = v->type->u.p;

	      oprintf (f, "  %s %s * const x = (%s %s *)x_p;\n",
		       s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag,
		       s->kind == TYPE_UNION ? "union" : "struct", s->u.s.tag);
	      oprintf (f, "  if (ggc_test_and_set_mark (x))\n");
	      oprintf (f, "    for (i = 0; i < (%s); i++)\n", length);
	      oprintf (f, "      {\n");
	      write_gc_structure_fields (f, s, "x[i]", "x[i]",
					 v->opt, 8, &v->line, s->u.s.bitmap,
					 NULL);
	      oprintf (f, "      }\n");
	    }

	  oprintf (f, "}\n\n");
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

	  oprintf (f, "const struct ggc_root_tab gt_ggc_r_");
	  put_mangled_filename (f, v->line.file);
	  oprintf (f, "[] = {\n");
	}

      write_gc_root (f, v, v->type, v->name, length_p, &v->line, NULL);
    }

  finish_root_table (flp, "r", "LAST_GGC_ROOT_TAB", "ggc_root_tab", 
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

	  oprintf (f, "const struct ggc_root_tab gt_ggc_rd_");
	  put_mangled_filename (f, v->line.file);
	  oprintf (f, "[] = {\n");
	}
      
      oprintf (f, "  { &%s, 1, sizeof (%s), NULL },\n",
	       v->name, v->name);
    }
  
  finish_root_table (flp, "rd", "LAST_GGC_ROOT_TAB", "ggc_root_tab",
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
	  if_marked = (const char *) o->info;

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

	  oprintf (f, "const struct ggc_cache_tab gt_ggc_rc_");
	  put_mangled_filename (f, v->line.file);
	  oprintf (f, "[] = {\n");
	}
      
      write_gc_root (f, v, create_pointer (v->type->u.p->u.param_struct.param),
		     v->name, length_p, &v->line, if_marked);
    }
  
  finish_root_table (flp, "rc", "LAST_GGC_CACHE_TAB", "ggc_cache_tab",
		     "gt_ggc_cache_rtab");
}


extern int main PARAMS ((int argc, char **argv));
int 
main(argc, argv)
     int argc ATTRIBUTE_UNUSED;
     char **argv ATTRIBUTE_UNUSED;
{
  unsigned i;
  static struct fileloc pos = { __FILE__, __LINE__ };
  unsigned j;
  
  srcdir_len = strlen (srcdir);

  do_typedef ("CUMULATIVE_ARGS",
	      create_scalar_type ("CUMULATIVE_ARGS", 
				  strlen ("CUMULATIVE_ARGS")),
	      &pos);
  do_typedef ("REAL_VALUE_TYPE",
	      create_scalar_type ("REAL_VALUE_TYPE", 
				  strlen ("REAL_VALUE_TYPE")),
	      &pos);
  do_typedef ("PTR", create_pointer (create_scalar_type ("void",
							 strlen ("void"))),
	      &pos);

  do_typedef ("HARD_REG_SET", create_array (
	      create_scalar_type ("unsigned long", strlen ("unsigned long")),
	      "2"), &pos);

  for (i = 0; i < NUM_GT_FILES; i++)
    {
      int dupflag = 0;
      /* Omit if already seen.  */
      for (j = 0; j < i; j++)
        {
          if (!strcmp (all_files[i], all_files[j]))
            {
              dupflag = 1;
              break;
            }
        }
      if (!dupflag)
        parse_file (all_files[i]);
    }

  if (hit_error != 0)
    exit (1);

  set_gc_used (variables);

  open_base_files ();
  write_gc_types (structures, param_structs);
  write_gc_roots (variables);
  close_output_files ();

  return (hit_error != 0);
}
