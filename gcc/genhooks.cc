/* Process target.def to create initialization macros definition in
   target-hooks-def.h and documentation in target-hooks.texi.
   Copyright (C) 2009-2022 Free Software Foundation, Inc.

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
#include "d/d-target.def"
#undef DEFHOOK
};

/* Return an upper-case copy of IN.  */
static char *
upstrdup (const char *in)
{
  char *p, *ret = xstrdup (in);
  for (p = ret; *p; p++)
    *p = TOUPPER (*p);
  return ret;
}

/* Emit shared .rst.in file that is used by the corresponding
   .. include:: tm.rst.in
     :start-after: [HOOK_NAME]
     :end-before: [HOOK_NAME]

   If the doc field starts with '*', the leading '*' is stripped, and the doc
   field is otherwise emitted unaltered; no function signature/
   @deftypefn/deftypevr/@end is emitted.
   In particular, a doc field of "*" means not to emit any ocumentation for
   this target.def / hook_array entry at all (there might be documentation
   for this hook in the file named IN_FNAME, though).

   This allows all the free-form
   documentation to be placed in IN_FNAME, to work around GPL/GFDL
   licensing incompatibility issues.  */

static void
emit_documentation (void)
{
  /* For each hook in hook_array, if it is a start hook, store its position.  */
  for (int i = 0; i < (int) (ARRAY_SIZE (hook_array)); i++)
    {
      if (hook_array[i].doc == NULL
	  || strcmp (hook_array[i].doc, "") == 0
	  || strcmp (hook_array[i].doc, "*") == 0)
	continue;
      const char *hook_name = upstrdup (hook_array[i].name);
      printf ("[%s]\n", hook_name);
      /* Print header.  Function-valued hooks have a parameter list,
	 unlike POD-valued ones.  */
      const char *deftype = hook_array[i].param ? "function" : "c:var";
      printf (".. %s:: ", deftype);
      if (strchr (hook_array[i].type, ' '))
	printf ("%s", hook_array[i].type);
      else
	printf ("%s", hook_array[i].type);
      printf (" %s", hook_name);
      if (hook_array[i].param)
	{
	  const char *q, *e;
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
	      printf ("%.*s%c", (int) (e - q), q, *e);
	}

      printf ("\n");
      if (hook_array[i].doc[0])
	{
	  const char *doc, *p_end;
	  printf ("\n");
	  /* Print each documentation paragraph in turn.  */
	  for (doc = hook_array[i].doc; *doc; doc = p_end)
	    {
	      /* Find paragraph end.  */
	      p_end = strstr (doc, "\n");
	      p_end = (p_end ? p_end + 1 : doc + strlen (doc));
	      printf ("  %.*s", (int) (p_end - doc), doc);
	    }
	  printf ("\n");
	}

      printf ("\n[%s]\n\n", hook_name);
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
      for (i = 0; i < (int) (ARRAY_SIZE (hook_array)); i++)
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
	  if (print_nest == 0)
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

  if (argc == 1)
    emit_documentation ();
  else
    emit_init_macros (argv[1]);
  return 0;
}
