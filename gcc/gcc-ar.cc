/* Wrapper for ar/ranlib/nm to pass the LTO plugin.
   Copyright (C) 2011-2023 Free Software Foundation, Inc.
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
main (int ac, char **av)
{
  const char *exe_name;
#if HAVE_LTO_PLUGIN > 0
  char *plugin;
#endif
  int k, status, err;
  const char *err_msg;
  const char **nargv;
  bool is_ar = !strcmp (PERSONALITY, "ar");
  int exit_code = FATAL_EXIT_CODE;
  int i;

  setup_prefixes (av[0]);

  /* Not using getopt for now.  */
  for (i = 0; i < ac; i++)
      if (startswith (av[i], "-B"))
	{
	  const char *arg = av[i] + 2;
	  const char *end;
	  size_t len;

	  memmove (av + i, av + i + 1, sizeof (char *) * ((ac + 1) - i));
	  ac--;
	  if (*arg == 0)
	    {
	      arg = av[i];
	      if (!arg)
		{
		  fprintf (stderr, "Usage: gcc-ar [-B prefix] ar arguments ...\n");
		  exit (EXIT_FAILURE);
		}
	      memmove (av + i, av + i + 1, sizeof (char *) * ((ac + 1) - i));
	      ac--;
	      i++;
	    }
	  /* else it's a joined argument  */

	  len = strlen (arg);
	  if (len > 0)
	    len--;
	  end = arg + len;

	  /* Always add a dir separator for the prefix list.  */
	  if (end > arg && !IS_DIR_SEPARATOR (*end))
	    {
	      static const char dir_separator_str[] = { DIR_SEPARATOR, 0 };
	      arg = concat (arg, dir_separator_str, NULL);
	    }

	  add_prefix_begin (&path, arg);
	  add_prefix_begin (&target_path, arg);
	  break;
	}

#if HAVE_LTO_PLUGIN > 0
  /* Find the GCC LTO plugin */
  plugin = find_a_file (&target_path, LTOPLUGINSONAME, R_OK);
  if (!plugin)
    {
      fprintf (stderr, "%s: Cannot find plugin '%s'\n", av[0], LTOPLUGINSONAME);
      exit (1);
    }
#endif

  /* Find the wrapped binutils program.  */
  exe_name = find_a_file (&target_path, PERSONALITY, X_OK);
  if (!exe_name)
    {
      const char *real_exe_name = PERSONALITY;
#ifdef CROSS_DIRECTORY_STRUCTURE
      real_exe_name = concat (target_machine, "-", PERSONALITY, NULL);
#endif
      exe_name = find_a_file (&path, real_exe_name, X_OK);
      if (!exe_name)
	{
	  fprintf (stderr, "%s: Cannot find binary '%s'\n", av[0],
		   real_exe_name);
	  exit (1);
	}
    }

  /* Create new command line with plugin - if we have one, otherwise just
     copy the command through.  */
  nargv = XCNEWVEC (const char *, ac + 4);
  nargv[0] = exe_name;
#if HAVE_LTO_PLUGIN > 0
  nargv[1] = "--plugin";
  nargv[2] = plugin;
  if (is_ar && av[1] && av[1][0] != '-')
    av[1] = concat ("-", av[1], NULL);
  for (k = 1; k < ac; k++)
    nargv[2 + k] = av[k];
  nargv[2 + k] = NULL;
#else
  if (is_ar && av[1] && av[1][0] != '-')
    av[1] = concat ("-", av[1], NULL);
  for (k = 1; k < ac; k++)
    nargv[k] = av[k];
  nargv[k] = NULL;
#endif

  /* Run utility */
  /* ??? the const is misplaced in pex_one's argv? */
  err_msg = pex_one (PEX_LAST|PEX_SEARCH, 
		     exe_name, 
		     CONST_CAST2 (char * const *, const char **, nargv),
		     concat ("gcc-", exe_name, NULL),
		     NULL,NULL,  &status, &err);
  if (err_msg) 
    fprintf (stderr, "Error running %s: %s\n", exe_name, err_msg);
  else if (status)
    {
      if (WIFSIGNALED (status))
	{
	  int sig = WTERMSIG (status);
	  fprintf (stderr, "%s terminated with signal %d [%s]%s\n",
		   exe_name, sig, strsignal (sig),
		   WCOREDUMP (status) ? ", core dumped" : "");
	}
      else if (WIFEXITED (status))
	exit_code = WEXITSTATUS (status);
    }
  else
    exit_code = SUCCESS_EXIT_CODE;

  return exit_code;
}
