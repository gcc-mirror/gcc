/* Specific flags and argument handling of the C preprocessor.
   Copyright (C) 1999 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"

/* The `cpp' executable installed in $(bindir) and $(cpp_install_dir)
   is a customized version of the gcc driver.  It forces -E; -S and -c
   are errors.  It defaults to -x c for files with unrecognized
   extensions, unless -x options appear in argv, in which case we
   assume the user knows what they're doing.  If no explicit input is
   mentioned, it will read stdin. */

/* Snarfed from gcc.c: */

/* This defines which switch letters take arguments.  */

#define DEFAULT_SWITCH_TAKES_ARG(CHAR) \
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o' \
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u' \
   || (CHAR) == 'I' || (CHAR) == 'm' || (CHAR) == 'x' \
   || (CHAR) == 'L' || (CHAR) == 'A' || (CHAR) == 'V' \
   || (CHAR) == 'B' || (CHAR) == 'b')

#ifndef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) DEFAULT_SWITCH_TAKES_ARG(CHAR)
#endif

/* This defines which multi-letter switches take arguments.  */

#define DEFAULT_WORD_SWITCH_TAKES_ARG(STR)		\
 (!strcmp (STR, "Tdata") || !strcmp (STR, "Ttext")	\
  || !strcmp (STR, "Tbss") || !strcmp (STR, "include")	\
  || !strcmp (STR, "imacros") || !strcmp (STR, "aux-info") \
  || !strcmp (STR, "idirafter") || !strcmp (STR, "iprefix") \
  || !strcmp (STR, "iwithprefix") || !strcmp (STR, "iwithprefixbefore") \
  || !strcmp (STR, "isystem") || !strcmp (STR, "specs"))

#ifndef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) DEFAULT_WORD_SWITCH_TAKES_ARG (STR)
#endif

/* Suffixes for known sorts of input files.  We let gcc.c worry about
   which are appropriate preprocessor input.  */
static const char *const known_suffixes[] =
{
  ".c",  ".C",   ".s",   ".S",   ".m",
  ".cc", ".cxx", ".cpp", ".cp",  ".c++",
  ".i",  ".ii",  ".mi",  ".o",   ".a",
  NULL
};

/* Filter argc and argv before processing by the gcc driver proper. */
void
lang_specific_driver (errfn, in_argc, in_argv, in_added_libraries)
     void (*errfn) PVPROTO((const char *, ...));
     int *in_argc;
     char ***in_argv;
     int *in_added_libraries ATTRIBUTE_UNUSED;
{
  int argc = *in_argc;
  char **argv = *in_argv;
  
  /* Do we need to read stdin? */
  int read_stdin;

  /* Do we need to insert -E? */
  int need_E;

  /* Do we need to fixup files with unrecognized suffixes? */
  int need_fixups;

  /* Table of input files with unrecognized suffixes. */
  char *urs_tab;
  int urs_count;
  int urs_block;

  int i, j, quote;
  char **new_argv;
  int new_argc;

  /* First pass.  If we see an -S or -c, barf.  If we see an input file,
     turn off read_stdin, and if it has an unrecognizable suffix, mark
     it for fixup. */
  urs_tab = xmalloc (argc);
  memset (urs_tab, 0, argc);
  urs_count = 0;
  urs_block = 0;
  quote = 0;
  read_stdin = 1;
  need_E = 1;
  need_fixups = 1;
  for (i = 1; i < argc; i++)
    {
      if (quote == 1)
	{
	  quote = 0;
	  continue;
	}
      
      if (argv[i][0] == '-')
	{
	  if (argv[i][1] == '\0')
	    read_stdin = 0;
	  else if (argv[i][2] == '\0')
	    {
	      if (argv[i][1] == 'E')
		need_E = 0;
	      else if (argv[i][1] == 'S' || argv[i][1] == 'c')
		{
		  (*errfn) ("`%s' is not a legal option to the preprocessor",
			    argv[i]);
		  goto done;
		}
	      else if (argv[i][1] == 'x')
		{
		  need_fixups = 0;
		  quote = 1;
		}
	      else if (SWITCH_TAKES_ARG (argv[i][1]))
		quote = 1;
	    }
	  else if (argv[i][1] == 'x')
	    need_fixups = 0;
	  else if (WORD_SWITCH_TAKES_ARG (&argv[i][1]))
	    quote = 1;
	}
      else /* not an option */
	{
	  int l = strlen (argv[i]);
	  int known = 0;
	  const char **suff;
	  
	  read_stdin = 0;
	  for (suff = known_suffixes; *suff; suff++)
	    if (!strcmp (*suff, &argv[i][l - strlen(*suff)]))
	      {
		known = 1;
		break;
	      }

	  if (known)
	    {
	      if (urs_block)
		{
		  urs_block = 0;
		  urs_tab[i] = 2;
		  urs_count++;
		}
	    }
	  else
	    {
	      if (!urs_block)
		{
		  urs_block = 1;
		  urs_tab[i] = 1;
		  urs_count++;
		}
	    }
	}
    }

  /* If we were given an -E option and an input file, and no input
     files have unrecognized suffixes, we can bail early.  */
  if (!need_E && !read_stdin && (!need_fixups || urs_count == 0))
    goto done;

  new_argc = argc + need_E + read_stdin + (need_fixups ? urs_count : 0);
  new_argv = xmalloc (new_argc * sizeof(char *));

  new_argv[0] = argv[0];
  if (need_E)
    {
      new_argv[1] = "-E";
      j = 2;
    }
  else
    j = 1;

  if (need_fixups)
    for (i = 1; i < argc; i++, j++)
      {
	if (urs_tab[i])
	  new_argv[j++] = (urs_tab[i] == 1) ? "-xc" : "-xnone";

	new_argv[j] = argv[i];
      }
  else
    memcpy (&new_argv[j], &argv[1], (argc - 1)*sizeof (char *));

  if (read_stdin)
    new_argv[j] = "-";

  *in_argc = new_argc;
  *in_argv = new_argv;

done:
  free (urs_tab);
}

/* Called before linking.  Returns 0 on success and -1 on failure. */
int lang_specific_pre_link ()
{
  return 0;  /* Not used for cpp. */
}

/* Number of extra output files that lang_specific_pre_link may generate. */
int lang_specific_extra_outfiles = 0;  /* Not used for cpp. */
