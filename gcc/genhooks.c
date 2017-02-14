/* Process target.def to create initialization macros definition in
   target-hooks-def.h and documentation in target-hooks.texi.
   Copyright (C) 2009-2017 Free Software Foundation, Inc.

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

struct hook_desc { const char *doc, *type, *name, *param, *init, *docname; };
static struct hook_desc hook_array[] = {
#define HOOK_VECTOR_1(NAME, FRAGMENT)	\
  { 0, 0, #NAME, 0, 0, HOOK_TYPE },
#define DEFHOOKPOD(NAME, DOC, TYPE, INIT) \
  { DOC, #TYPE, HOOK_PREFIX #NAME, 0, #INIT, HOOK_TYPE },
#define DEFHOOK(NAME, DOC, TYPE, PARAMS, INIT) \
  { DOC, #TYPE, HOOK_PREFIX #NAME, #PARAMS, #INIT, HOOK_TYPE },
#define DEFHOOK_UNDOC(NAME, DOC, TYPE, PARAMS, INIT) \
  { "*", #TYPE, HOOK_PREFIX #NAME, #PARAMS, #INIT, HOOK_TYPE },
#include "target.def"
#include "c-family/c-target.def"
#include "common/common-target.def"
#undef DEFHOOK
};

/* For each @Fcode in the first paragraph of the documentation string DOC,
   print an @findex directive.  HOOK_NAME is the name of the hook this bit of
   documentation pertains to.  */
static void
emit_findices (const char *doc, const char *hook_name)
{
  const char *end = strstr (doc, "\n\n");
  const char *fcode;

  while ((fcode = strstr (doc, "@Fcode{")) && (!end || fcode < end))
    {
      fcode += strlen ("@Fcode{");
      doc = strchr (fcode, '}');
      if (!doc)
	fatal ("Malformed @Fcode for hook %s\n", hook_name);
      printf ("@findex %.*s\n", (int) (doc - fcode), fcode);
      doc = fcode;
    }
}

/* Return an upper-case copy of IN.  */
static char *
upstrdup (const char *in)
{
  char *p, *ret = xstrdup (in);
  for (p = ret; *p; p++)
    *p = TOUPPER (*p);
  return ret;
}

/* Struct for 'start hooks' which start a sequence of consecutive hooks
   that are defined in target.def and to be documented in tm.texi.  */
struct s_hook
{
  char *name;
  int pos;
};

static hashval_t
s_hook_hash (const void *p)
{
  const struct s_hook *s_hook = (const struct s_hook *)p;
  return htab_hash_string (s_hook->name);
}

static int
s_hook_eq_p (const void *p1, const void *p2)
{
  return (strcmp (((const struct s_hook *) p1)->name, 
		  ((const struct s_hook *) p2)->name) == 0);
}

/* Read the documentation file with name IN_FNAME, perform substitutions
   to incorporate information from hook_array, and emit the result on stdout.
   Hooks defined with DEFHOOK / DEFHOOKPOD are emitted at the place of a
   matching @hook in the input file; if there is no matching @hook, the
   hook is emitted after the hook that precedes it in target.def .
   Usually, the emitted hook documentation starts with the hook
   signature, followed by the string from the doc field.
   The documentation is bracketed in @deftypefn / @deftypevr and a matching
   @end.
   While emitting the doc field, @Fcode is translated to @code, and an
   @findex entry is added to the affected paragraph.
   If the doc field starts with '*', the leading '*' is stripped, and the doc
   field is otherwise emitted unaltered; no function signature/
   @deftypefn/deftypevr/@end is emitted.
   In particular, a doc field of "*" means not to emit any ocumentation for
   this target.def / hook_array entry at all (there might be documentation
   for this hook in the file named IN_FNAME, though).
   A doc field of 0 is used to append the hook signature after the previous
   hook's signture, so that one description can be used for a group of hooks.
   When the doc field is "", @deftypefn/@deftypevr and the hook signature
   is emitted, but not the matching @end.  This allows all the free-form
   documentation to be placed in IN_FNAME, to work around GPL/GFDL
   licensing incompatibility issues.  */
static void
emit_documentation (const char *in_fname)
{
  int i, j;
  char buf[1000];
  htab_t start_hooks = htab_create (99, s_hook_hash, s_hook_eq_p, (htab_del) 0);
  FILE *f;

  /* Enter all the start hooks in start_hooks.  */
  f = fopen (in_fname, "r");
  if (!f)
    {
      perror ("");
      fatal ("Couldn't open input file");
    }
  while (fscanf (f, "%*[^@]"), buf[0] = '\0',
	 fscanf (f, "@%5[^ \n]", buf) != EOF)
    {
      void **p;
      struct s_hook *shp;

      if (strcmp (buf, "hook") != 0)
	continue;
      buf[0] = '\0';
      fscanf (f, "%999s", buf);
      shp = XNEW (struct s_hook);
      shp->name = upstrdup (buf);
      shp->pos = -1;
      p = htab_find_slot (start_hooks, shp, INSERT);
      if (*p != HTAB_EMPTY_ENTRY)
	fatal ("Duplicate placement for hook %s\n", shp->name);
      *(struct s_hook **) p = shp;
    }
  fclose (f);
  /* For each hook in hook_array, if it is a start hook, store its position.  */
  for (i = 0; i < (int) (sizeof hook_array / sizeof hook_array[0]); i++)
    {
      struct s_hook sh, *shp;
      void *p;

      if (!hook_array[i].doc || strcmp (hook_array[i].doc, "*") == 0)
	continue;
      sh.name = upstrdup (hook_array[i].name);
      p = htab_find (start_hooks, &sh);
      if (p)
	{
	  shp = (struct s_hook *) p;
	  if (shp->pos >= 0)
	    fatal ("Duplicate hook %s\n", sh.name);
	  shp->pos = i;
	}
      else
	fatal ("No place specified to document hook %s\n", sh.name);
      free (sh.name);
    }
  /* Copy input file to stdout, substituting @hook directives with the
     corresponding hook documentation sequences.  */
  f = fopen (in_fname, "r");
  if (!f)
    {
      perror ("");
      fatal ("Couldn't open input file");
    }
  for (;;)
    {
      struct s_hook sh, *shp;
      int c = getc (f);
      char *name;

      if (c == EOF)
	break;
      if (c != '@')
	{
	  putchar (c);
	  continue;
	}
      buf[0] = '\0';
      fscanf (f, "%5[^ \n]", buf);
      if (strcmp (buf, "hook") != 0)
	{
	  printf ("@%s", buf);
	  continue;
	}
      fscanf (f, "%999s", buf);
      sh.name = name = upstrdup (buf);
      shp = (struct s_hook *) htab_find (start_hooks, &sh);
      if (!shp || shp->pos < 0)
	fatal ("No documentation for hook %s\n", sh.name);
      i = shp->pos;
      do
	{
	  const char *q, *e;
	  const char *deftype;
	  const char *doc, *fcode, *p_end;

	  /* A leading '*' means to output the documentation string without
	     further processing.  */
	  if (*hook_array[i].doc == '*')
	    printf ("%s", hook_array[i].doc + 1);
	  else
	    {
	      if (i != shp->pos)
		printf ("\n\n");
	      emit_findices (hook_array[i].doc, name);

	      /* Print header.  Function-valued hooks have a parameter list, 
		 unlike POD-valued ones.  */
	      deftype = hook_array[i].param ? "deftypefn" : "deftypevr";
	      printf ("@%s {%s} ", deftype, hook_array[i].docname);
	      if (strchr (hook_array[i].type, ' '))
		printf ("{%s}", hook_array[i].type);
	      else
		printf ("%s", hook_array[i].type);
	      printf (" %s", name);
	      if (hook_array[i].param)
		{
		  /* Print the parameter list, with the parameter names
		     enclosed in @var{}.  */
		  printf (" ");
		  for (q = hook_array[i].param; (e = strpbrk (q, " *,)"));
		       q = e + 1)
		    /* Type names like 'int' are followed by a space, sometimes
		       also by '*'.  'void' should appear only in "(void)".  */
		    if (*e == ' ' || *e == '*' || *q == '(')
		      printf ("%.*s", (int) (e - q + 1), q);
		    else
		      printf ("@var{%.*s}%c", (int) (e - q), q, *e);
		}
	      /* POD-valued hooks sometimes come in groups with common
		 documentation.*/
	      for (j = i + 1;
		   j < (int) (sizeof hook_array / sizeof hook_array[0])
		   && hook_array[j].doc == 0 && hook_array[j].type; j++)
		{
		  char *namex = upstrdup (hook_array[j].name);

		  printf ("\n@%sx {%s} {%s} %s",
			  deftype, hook_array[j].docname,
			  hook_array[j].type, namex);
		}
	      if (hook_array[i].doc[0])
		{
		  printf ("\n");
		  /* Print each documentation paragraph in turn.  */
		  for (doc = hook_array[i].doc; *doc; doc = p_end)
		    {
		      /* Find paragraph end.  */
		      p_end = strstr (doc, "\n\n");
		      p_end = (p_end ? p_end + 2 : doc + strlen (doc));
		      /* Print paragraph, emitting @Fcode as @code.  */
		      for (; (fcode = strstr (doc, "@Fcode{")) && fcode < p_end;
			   doc = fcode + 2)
			printf ("%.*s@", (int) (fcode - doc), doc);
		      printf ("%.*s", (int) (p_end - doc), doc);
		      /* Emit function indices for next paragraph.  */
		      emit_findices (p_end, name);
		    }
		  printf ("\n@end %s", deftype);
		}
	    }
	  if (++i >= (int) (sizeof hook_array / sizeof hook_array[0])
	      || !hook_array[i].doc)
	    break;
	  free (name);
	  sh.name = name = upstrdup (hook_array[i].name);
	}
      while (!htab_find (start_hooks, &sh));
      free (name);
    }
}

/* Emit #defines to stdout (this will be redirected to generate
   target-hook-def.h) which set target hooks initializer macros
   to their default values.  These should only be emitted for hooks
   whose type is given by DOCNAME.  */
static void
emit_init_macros (const char *docname)
{
  int i;
  const int MAX_NEST = 2;
  int print_nest, nest = 0;

  for (print_nest = 0; print_nest <= MAX_NEST; print_nest++)
    {
      for (i = 0; i < (int) (sizeof hook_array / sizeof hook_array[0]); i++)
	{
	  char *name = upstrdup (hook_array[i].name);

	  if (strcmp (hook_array[i].docname, docname) != 0)
	    continue;

	  if (!hook_array[i].type)
	    {
	      if (*name)
		{
		  if (nest && nest == print_nest)
		    printf ("    %s, \\\n", name);
		  nest++;
		  if (nest > MAX_NEST)
		    fatal ("Unexpected nesting of %s\n", name);
		  if (nest == print_nest)
		    printf ("\n#define %s \\\n  { \\\n", name);
		}
	      else
		{
		  if (nest == print_nest)
		    printf ("  }\n");
		  nest--;
		}
	      continue;
	    }
	  if (0 == print_nest)
	    {
	      /* Output default definitions of target hooks.  */
	      printf ("#ifndef %s\n#define %s %s\n#endif\n",
		      name, name, hook_array[i].init);
	    }
	  if (nest == print_nest)
	    printf ("    %s, \\\n", name);
	}
    }
}

int
main (int argc, char **argv)
{
  progname = "genhooks";

  if (argc >= 3)
    emit_documentation (argv[2]);
  else
    emit_init_macros (argv[1]);
  return 0;
}
