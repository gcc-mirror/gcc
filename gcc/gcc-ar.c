/* Wrapper for ar/ranlib/nm to pass the LTO plugin.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
#include "file-find.h"

#ifndef PERSONALITY
#error "Please set personality"
#endif

/* The exec prefix as derived at compile-time from --prefix.  */

static const char standard_exec_prefix[] = STANDARD_EXEC_PREFIX;

/* The libexec prefix as derived at compile-time from --prefix.  */

static const char standard_libexec_prefix[] = STANDARD_LIBEXEC_PREFIX;

/* The bindir prefix as derived at compile-time from --prefix.  */

static const char standard_bin_prefix[] = STANDARD_BINDIR_PREFIX;

/* A relative path to be used in finding the location of tools
   relative to this program.  */

static const char *const tooldir_base_prefix = TOOLDIR_BASE_PREFIX;

/* The exec prefix as relocated from the location of this program.  */

static const char *self_exec_prefix;

/* The libexec prefix as relocated from the location of this program.  */

static const char *self_libexec_prefix;

/* The tools prefix as relocated from the location of this program.  */

static const char *self_tooldir_prefix;

/* The name of the machine that is being targeted.  */

static const char *const target_machine = DEFAULT_TARGET_MACHINE;

/* The target version.  */

static const char *const target_version = DEFAULT_TARGET_VERSION;

/* The collection of target specific path prefixes.  */

static struct path_prefix target_path;

/* The collection path prefixes.  */

static struct path_prefix path;

/* The directory separator.  */

static const char dir_separator[] = { DIR_SEPARATOR, 0 };

static void
setup_prefixes (const char *exec_path)
{
  const char *self;

  self = getenv ("GCC_EXEC_PREFIX");
  if (!self)
    self = exec_path;
  else
    self = concat (self, "gcc-" PERSONALITY, NULL);

  /* Relocate the exec prefix.  */
  self_exec_prefix = make_relative_prefix (self,
					   standard_bin_prefix,
					   standard_exec_prefix);
  if (self_exec_prefix == NULL)
    self_exec_prefix = standard_exec_prefix;

  /* Relocate libexec prefix.  */
  self_libexec_prefix = make_relative_prefix (self,
					      standard_bin_prefix,
					      standard_libexec_prefix);
  if (self_libexec_prefix == NULL)
    self_libexec_prefix = standard_libexec_prefix;


  /* Build the relative path to the target-specific tool directory.  */
  self_tooldir_prefix = concat (tooldir_base_prefix, target_machine,
				dir_separator, NULL);
  self_tooldir_prefix = concat (self_exec_prefix, target_machine, 
				dir_separator, target_version, dir_separator,
				self_tooldir_prefix, NULL);

  /* Add the target-specific tool bin prefix.  */
  prefix_from_string (concat (self_tooldir_prefix, "bin", NULL), &target_path);

  /* Add the target-specific libexec prefix.  */
  self_libexec_prefix = concat (self_libexec_prefix, target_machine, 
				dir_separator, target_version,
				dir_separator, NULL);
  prefix_from_string (self_libexec_prefix, &target_path);

  /* Add path as a last resort.  */
  prefix_from_env ("PATH", &path);
}

int 
main(int ac, char **av)
{
  const char *exe_name;
  char *plugin;
  int k, status, err;
  const char *err_msg;
  const char **nargv;
  bool is_ar = !strcmp (PERSONALITY, "ar");
  int exit_code = FATAL_EXIT_CODE;

  setup_prefixes (av[0]);

  /* Find the GCC LTO plugin */
  plugin = find_a_file (&target_path, LTOPLUGINSONAME);
  if (!plugin)
    {
      fprintf (stderr, "%s: Cannot find plugin '%s'\n", av[0], LTOPLUGINSONAME);
      exit (1);
    }

  /* Find the wrapped binutils program.  */
  exe_name = find_a_file (&target_path, PERSONALITY);
  if (!exe_name)
    {
#ifdef CROSS_DIRECTORY_STRUCTURE
      const char *cross_exe_name;

      cross_exe_name = concat (target_machine, "-", PERSONALITY, NULL);
      exe_name = find_a_file (&path, cross_exe_name);
      if (!exe_name)
	{
	  fprintf (stderr, "%s: Cannot find binary '%s'\n", av[0],
		   cross_exe_name);
	  exit (1);
	}
#else
      fprintf (stderr, "%s: Cannot find binary '%s'\n", av[0], PERSONALITY);
      exit (1);
#endif
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
