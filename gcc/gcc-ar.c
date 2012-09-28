/* Wrapper for ar/ranlib/nm to pass the LTO plugin.
   Copyright (C) 2011, 2012 Free Software Foundation, Inc.
   Contributed by Andi Kleen.

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

#include "config.h"
#include "system.h"
#include "libiberty.h"

#ifndef PERSONALITY
#error "Please set personality"
#endif

static const char standard_libexec_prefix[] = STANDARD_LIBEXEC_PREFIX;
static const char standard_bin_prefix[] = STANDARD_BINDIR_PREFIX;
static const char *const target_machine = TARGET_MACHINE;

static const char dir_separator[] = { DIR_SEPARATOR, 0 };

int 
main(int ac, char **av)
{
  const char *nprefix;
  const char *exe_name;
  char *plugin;
  int k, status, err;
  const char *err_msg;
  const char **nargv;
  bool is_ar = !strcmp (PERSONALITY, "ar");
  int exit_code = FATAL_EXIT_CODE;

  exe_name = PERSONALITY;
#ifdef CROSS_DIRECTORY_STRUCTURE
  exe_name = concat (target_machine, "-", exe_name, NULL);
#endif

  /* Find plugin */
  /* XXX implement more magic from gcc.c? */
  nprefix = getenv ("GCC_EXEC_PREFIX");
  if (!nprefix)
    nprefix = av[0];
  else
    nprefix = concat (nprefix, "gcc-" PERSONALITY, NULL);

  nprefix = make_relative_prefix (nprefix,
				  standard_bin_prefix,
				  standard_libexec_prefix);
  if (nprefix == NULL)
    nprefix = standard_libexec_prefix;

  plugin = concat (nprefix,
		   dir_separator,
                   DEFAULT_TARGET_MACHINE, 
		   dir_separator,
		   DEFAULT_TARGET_VERSION,
	           dir_separator,
		   LTOPLUGINSONAME,
		   NULL);
  if (access (plugin, R_OK))
    {
      fprintf (stderr, "%s: Cannot find plugin %s\n", av[0], plugin);
      exit (1);
    }

  /* Create new command line with plugin */
  nargv = XCNEWVEC (const char *, ac + 4);
  nargv[0] = exe_name;
  nargv[1] = "--plugin";
  nargv[2] = plugin;
  if (is_ar && av[1] && av[1][0] != '-')
    av[1] = concat("-", av[1], NULL);
  for (k = 1; k < ac; k++)
    nargv[2 + k] = av[k];
  nargv[2 + k] = NULL;

  /* Run utility */
  /* ??? the const is misplaced in pex_one's argv? */
  err_msg = pex_one (PEX_LAST|PEX_SEARCH, 
		     exe_name, 
		     CONST_CAST2 (char * const *, const char **, nargv),
		     concat("gcc-", exe_name, NULL), 
		     NULL,NULL,  &status, &err);
  if (err_msg) 
    fprintf(stderr, "Error running %s: %s\n", exe_name, err_msg);
  else if (status)
    {
      if (WIFSIGNALED (status))
	{
	  int sig = WTERMSIG (status);
	  fprintf (stderr, "%s terminated with signal %d [%s]%s\n",
		   exe_name, sig, strsignal(sig),
		   WCOREDUMP(status) ? ", core dumped" : "");
	}
      else if (WIFEXITED (status))
	exit_code = WEXITSTATUS (status);
    }
  else
    exit_code = SUCCESS_EXIT_CODE;

  return exit_code;
}
