/* Wrapper to call lto.  Used by collect2 and the linker plugin.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.

   Factored out of collect2 by Rafael Espindola <espindola@google.com>

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


/* This program is passed a gcc, a list of gcc arguments and a list of
   object files containing IL. It scans the argument list to check if
   we are in whopr mode or not modifies the arguments and needed and
   prints a list of output files on stdout.

   Example:

   $ lto-wrapper gcc/xgcc -B gcc a.o b.o -o test -flto

   The above will print something like
   /tmp/ccwbQ8B2.lto.o

   If WHOPR is used instead, more than one file might be produced
   ./ccXj2DTk.lto.ltrans.o
   ./ccCJuXGv.lto.ltrans.o
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "diagnostic.h"
#include "obstack.h"
#include "opts.h"
#include "options.h"
#include "simple-object.h"

/* From lto-streamer.h which we cannot include with -fkeep-inline-functions.
   ???  Split out a lto-streamer-core.h.  */

#define LTO_SECTION_NAME_PREFIX         ".gnu.lto_"

/* End of lto-streamer.h copy.  */

int debug;				/* true if -save-temps.  */
int verbose;				/* true if -v.  */

enum lto_mode_d {
  LTO_MODE_NONE,			/* Not doing LTO.  */
  LTO_MODE_LTO,				/* Normal LTO.  */
  LTO_MODE_WHOPR			/* WHOPR.  */
};

/* Current LTO mode.  */
static enum lto_mode_d lto_mode = LTO_MODE_NONE;

static char *ltrans_output_file;
static char *flto_out;
static char *args_name;
static unsigned int nr;
static char **input_names;
static char **output_names;
static char *makefile;

static void maybe_unlink_file (const char *);

 /* Delete tempfiles.  */

static void
lto_wrapper_cleanup (void)
{
  static bool cleanup_done = false;
  unsigned int i;

  if (cleanup_done)
    return;

  /* Setting cleanup_done prevents an infinite loop if one of the
     calls to maybe_unlink_file fails. */
  cleanup_done = true;

  if (ltrans_output_file)
    maybe_unlink_file (ltrans_output_file);
  if (flto_out)
    maybe_unlink_file (flto_out);
  if (args_name)
    maybe_unlink_file (args_name);
  if (makefile)
    maybe_unlink_file (makefile);
  for (i = 0; i < nr; ++i)
    {
      maybe_unlink_file (input_names[i]);
      if (output_names[i])
	maybe_unlink_file (output_names[i]);
    }
}

static void
fatal_signal (int signum)
{
  signal (signum, SIG_DFL);
  lto_wrapper_cleanup ();
  /* Get the same signal again, this time not handled,
     so its normal effect occurs.  */
  kill (getpid (), signum);
}

/* Just die. CMSGID is the error message. */

static void __attribute__ ((format (printf, 1, 2)))
fatal (const char * cmsgid, ...)
{
  va_list ap;

  va_start (ap, cmsgid);
  fprintf (stderr, "lto-wrapper: ");
  vfprintf (stderr, _(cmsgid), ap);
  fprintf (stderr, "\n");
  va_end (ap);

  lto_wrapper_cleanup ();
  exit (FATAL_EXIT_CODE);
}


/* Die when sys call fails. CMSGID is the error message.  */

static void __attribute__ ((format (printf, 1, 2)))
fatal_perror (const char *cmsgid, ...)
{
  int e = errno;
  va_list ap;

  va_start (ap, cmsgid);
  fprintf (stderr, "lto-wrapper: ");
  vfprintf (stderr, _(cmsgid), ap);
  fprintf (stderr, ": %s\n", xstrerror (e));
  va_end (ap);

  lto_wrapper_cleanup ();
  exit (FATAL_EXIT_CODE);
}


/* Execute a program, and wait for the reply. ARGV are the arguments. The
   last one must be NULL. */

static struct pex_obj *
collect_execute (char **argv)
{
  struct pex_obj *pex;
  const char *errmsg;
  int err;

  if (verbose)
    {
      char **p_argv;
      const char *str;

      for (p_argv = argv; (str = *p_argv) != (char *) 0; p_argv++)
	fprintf (stderr, " %s", str);

      fprintf (stderr, "\n");
    }

  fflush (stdout);
  fflush (stderr);

  pex = pex_init (0, "lto-wrapper", NULL);
  if (pex == NULL)
    fatal_perror ("pex_init failed");

  /* Do not use PEX_LAST here, we use our stdout for communicating with
     collect2 or the linker-plugin.  Any output from the sub-process
     will confuse that.  */
  errmsg = pex_run (pex, PEX_SEARCH, argv[0], argv, NULL,
		    NULL, &err);
  if (errmsg != NULL)
    {
      if (err != 0)
	{
	  errno = err;
	  fatal_perror (errmsg);
	}
      else
	fatal (errmsg);
    }

  return pex;
}


/* Wait for a process to finish, and exit if a nonzero status is found.
   PROG is the program name. PEX is the process we should wait for. */

static int
collect_wait (const char *prog, struct pex_obj *pex)
{
  int status;

  if (!pex_get_status (pex, 1, &status))
    fatal_perror ("can't get program status");
  pex_free (pex);

  if (status)
    {
      if (WIFSIGNALED (status))
	{
	  int sig = WTERMSIG (status);
	  if (WCOREDUMP (status))
	    fatal ("%s terminated with signal %d [%s], core dumped",
		   prog, sig, strsignal (sig));
	  else
	    fatal ("%s terminated with signal %d [%s]",
		   prog, sig, strsignal (sig));
	}

      if (WIFEXITED (status))
	fatal ("%s returned %d exit status", prog, WEXITSTATUS (status));
    }

  return 0;
}


/* Unlink a temporary LTRANS file unless requested otherwise.  */

static void
maybe_unlink_file (const char *file)
{
  if (! debug)
    {
      if (unlink_if_ordinary (file)
	  && errno != ENOENT)
	fatal_perror ("deleting LTRANS file %s", file);
    }
  else
    fprintf (stderr, "[Leaving LTRANS %s]\n", file);
}


/* Execute program ARGV[0] with arguments ARGV. Wait for it to finish.  */

static void
fork_execute (char **argv)
{
  struct pex_obj *pex;
  char *new_argv[3];
  char *at_args;
  FILE *args;
  int status;

  args_name = make_temp_file (".args");
  at_args = concat ("@", args_name, NULL);
  args = fopen (args_name, "w");
  if (args == NULL)
    fatal ("failed to open %s", args_name);

  status = writeargv (&argv[1], args);

  if (status)
    fatal ("could not write to temporary file %s",  args_name);

  fclose (args);

  new_argv[0] = argv[0];
  new_argv[1] = at_args;
  new_argv[2] = NULL;

  pex = collect_execute (new_argv);
  collect_wait (new_argv[0], pex);

  maybe_unlink_file (args_name);
  args_name = NULL;
  free (at_args);
}

/* Template of LTRANS dumpbase suffix.  */
#define DUMPBASE_SUFFIX ".ltrans18446744073709551615"

/* Create decoded options from the COLLECT_GCC and COLLECT_GCC_OPTIONS
   environment according to LANG_MASK.  */

static void
get_options_from_collect_gcc_options (const char *collect_gcc,
				      const char *collect_gcc_options,
				      unsigned int lang_mask,
				      struct cl_decoded_option **decoded_options,
				      unsigned int *decoded_options_count)
{
  struct obstack argv_obstack;
  char *argv_storage;
  const char **argv;
  int j, k, argc;

  argv_storage = xstrdup (collect_gcc_options);
  obstack_init (&argv_obstack);
  obstack_ptr_grow (&argv_obstack, collect_gcc);

  for (j = 0, k = 0; argv_storage[j] != '\0'; ++j)
    {
      if (argv_storage[j] == '\'')
	{
	  obstack_ptr_grow (&argv_obstack, &argv_storage[k]);
	  ++j;
	  do
	    {
	      if (argv_storage[j] == '\0')
		fatal ("malformed COLLECT_GCC_OPTIONS");
	      else if (strncmp (&argv_storage[j], "'\\''", 4) == 0)
		{
		  argv_storage[k++] = '\'';
		  j += 4;
		}
	      else if (argv_storage[j] == '\'')
		break;
	      else
		argv_storage[k++] = argv_storage[j++];
	    }
	  while (1);
	  argv_storage[k++] = '\0';
	}
    }

  obstack_ptr_grow (&argv_obstack, NULL);
  argc = obstack_object_size (&argv_obstack) / sizeof (void *) - 1;
  argv = XOBFINISH (&argv_obstack, const char **);

  decode_cmdline_options_to_array (argc, (const char **)argv,
				   lang_mask,
				   decoded_options, decoded_options_count);
  obstack_free (&argv_obstack, NULL);
}

/* Append OPTION to the options array DECODED_OPTIONS with size
   DECODED_OPTIONS_COUNT.  */

static void
append_option (struct cl_decoded_option **decoded_options,
	       unsigned int *decoded_options_count,
	       struct cl_decoded_option *option)
{
  ++*decoded_options_count;
  *decoded_options
    = (struct cl_decoded_option *)
	xrealloc (*decoded_options,
		  (*decoded_options_count
		   * sizeof (struct cl_decoded_option)));
  memcpy (&(*decoded_options)[*decoded_options_count - 1], option,
	  sizeof (struct cl_decoded_option));
}

/* Try to merge and complain about options FDECODED_OPTIONS when applied
   ontop of DECODED_OPTIONS.  */

static void
merge_and_complain (struct cl_decoded_option **decoded_options,
		    unsigned int *decoded_options_count,
		    struct cl_decoded_option *fdecoded_options,
		    unsigned int fdecoded_options_count)
{
  unsigned int i, j;

  /* ???  Merge options from files.  Most cases can be
     handled by either unioning or intersecting
     (for example -fwrapv is a case for unioning,
     -ffast-math is for intersection).  Most complaints
     about real conflicts between different options can
     be deferred to the compiler proper.  Options that
     we can neither safely handle by intersection nor
     unioning would need to be complained about here.
     Ideally we'd have a flag in the opt files that
     tells whether to union or intersect or reject.
     In absence of that it's unclear what a good default is.
     It's also difficult to get positional handling correct.  */

  /* The following does what the old LTO option code did,
     union all target and a selected set of common options.  */
  for (i = 0; i < fdecoded_options_count; ++i)
    {
      struct cl_decoded_option *foption = &fdecoded_options[i];
      switch (foption->opt_index)
	{
	case OPT_SPECIAL_unknown:
	case OPT_SPECIAL_ignore:
	case OPT_SPECIAL_program_name:
	case OPT_SPECIAL_input_file:
	  break;

	default:
	  if (!(cl_options[foption->opt_index].flags & CL_TARGET))
	    break;

	  /* Fallthru.  */
	case OPT_fPIC:
	case OPT_fpic:
	case OPT_fpie:
	case OPT_fcommon:
	case OPT_fexceptions:
	case OPT_fnon_call_exceptions:
	case OPT_fgnu_tm:
	  /* Do what the old LTO code did - collect exactly one option
	     setting per OPT code, we pick the first we encounter.
	     ???  This doesn't make too much sense, but when it doesn't
	     then we should complain.  */
	  for (j = 0; j < *decoded_options_count; ++j)
	    if ((*decoded_options)[j].opt_index == foption->opt_index)
	      break;
	  if (j == *decoded_options_count)
	    append_option (decoded_options, decoded_options_count, foption);
	  break;

	case OPT_ftrapv:
	case OPT_fstrict_overflow:
	case OPT_ffp_contract_:
	  /* For selected options we can merge conservatively.  */
	  for (j = 0; j < *decoded_options_count; ++j)
	    if ((*decoded_options)[j].opt_index == foption->opt_index)
	      break;
	  if (j == *decoded_options_count)
	    append_option (decoded_options, decoded_options_count, foption);
	  /* FP_CONTRACT_OFF < FP_CONTRACT_ON < FP_CONTRACT_FAST,
	     -fno-trapv < -ftrapv,
	     -fno-strict-overflow < -fstrict-overflow  */
	  else if (foption->value < (*decoded_options)[j].value)
	    (*decoded_options)[j] = *foption;
	  break;

	case OPT_fwrapv:
	  /* For selected options we can merge conservatively.  */
	  for (j = 0; j < *decoded_options_count; ++j)
	    if ((*decoded_options)[j].opt_index == foption->opt_index)
	      break;
	  if (j == *decoded_options_count)
	    append_option (decoded_options, decoded_options_count, foption);
	  /* -fwrapv > -fno-wrapv.  */
	  else if (foption->value > (*decoded_options)[j].value)
	    (*decoded_options)[j] = *foption;
	  break;

	case OPT_freg_struct_return:
	case OPT_fpcc_struct_return:
	  for (j = 0; j < *decoded_options_count; ++j)
	    if ((*decoded_options)[j].opt_index == foption->opt_index)
	      break;
	  if (j == *decoded_options_count)
	    fatal ("Option %s not used consistently in all LTO input files",
		   foption->orig_option_with_args_text);
	  break;
	}
    }
}

/* Execute gcc. ARGC is the number of arguments. ARGV contains the arguments. */

static void
run_gcc (unsigned argc, char *argv[])
{
  unsigned i, j;
  const char **new_argv;
  const char **argv_ptr;
  char *list_option_full = NULL;
  const char *linker_output = NULL;
  const char *collect_gcc, *collect_gcc_options;
  int parallel = 0;
  int jobserver = 0;
  bool no_partition = false;
  struct cl_decoded_option *fdecoded_options = NULL;
  unsigned int fdecoded_options_count = 0;
  struct cl_decoded_option *decoded_options;
  unsigned int decoded_options_count;
  struct obstack argv_obstack;
  int new_head_argc;

  /* Get the driver and options.  */
  collect_gcc = getenv ("COLLECT_GCC");
  if (!collect_gcc)
    fatal ("environment variable COLLECT_GCC must be set");
  collect_gcc_options = getenv ("COLLECT_GCC_OPTIONS");
  if (!collect_gcc_options)
    fatal ("environment variable COLLECT_GCC_OPTIONS must be set");
  get_options_from_collect_gcc_options (collect_gcc, collect_gcc_options,
					CL_LANG_ALL,
					&decoded_options,
					&decoded_options_count);

  /* Look at saved options in the IL files.  */
  for (i = 1; i < argc; ++i)
    {
      char *data, *p;
      char *fopts;
      int fd;
      const char *errmsg;
      int err;
      off_t file_offset = 0, offset, length;
      long loffset;
      simple_object_read *sobj;
      int consumed;
      struct cl_decoded_option *f2decoded_options;
      unsigned int f2decoded_options_count;
      char *filename = argv[i];
      if ((p = strrchr (argv[i], '@'))
	  && p != argv[i] 
	  && sscanf (p, "@%li%n", &loffset, &consumed) >= 1
	  && strlen (p) == (unsigned int) consumed)
	{
	  filename = XNEWVEC (char, p - argv[i] + 1);
	  memcpy (filename, argv[i], p - argv[i]);
	  filename[p - argv[i]] = '\0';
	  file_offset = (off_t) loffset;
	}
      fd = open (argv[i], O_RDONLY);
      if (fd == -1)
	continue;
      sobj = simple_object_start_read (fd, file_offset, "__GNU_LTO", 
	  			       &errmsg, &err);
      if (!sobj)
	{
	  close (fd);
	  continue;
	}
      if (!simple_object_find_section (sobj, LTO_SECTION_NAME_PREFIX "." "opts",
				       &offset, &length, &errmsg, &err))
	{
	  simple_object_release_read (sobj);
	  close (fd);
	  continue;
	}
      lseek (fd, file_offset + offset, SEEK_SET);
      data = (char *)xmalloc (length);
      read (fd, data, length);
      fopts = data;
      do
	{
	  get_options_from_collect_gcc_options (collect_gcc,
						fopts, CL_LANG_ALL,
						&f2decoded_options,
						&f2decoded_options_count);
	  if (!fdecoded_options)
	    {
	      fdecoded_options = f2decoded_options;
	      fdecoded_options_count = f2decoded_options_count;
	    }
	  else
	    merge_and_complain (&fdecoded_options,
				&fdecoded_options_count,
				f2decoded_options, f2decoded_options_count);

	  fopts += strlen (fopts) + 1;
	}
      while (fopts - data < length);

      free (data);
      simple_object_release_read (sobj);
      close (fd);
    }

  /* Initalize the common arguments for the driver.  */
  obstack_init (&argv_obstack);
  obstack_ptr_grow (&argv_obstack, collect_gcc);
  obstack_ptr_grow (&argv_obstack, "-xlto");
  obstack_ptr_grow (&argv_obstack, "-c");

  /* Append compiler driver arguments as far as they were merged.  */
  for (j = 1; j < fdecoded_options_count; ++j)
    {
      struct cl_decoded_option *option = &fdecoded_options[j];

      /* File options have been properly filtered by lto-opts.c.  */
      switch (option->opt_index)
	{
	  /* Drop arguments that we want to take from the link line.  */
	  case OPT_flto_:
	  case OPT_flto:
	  case OPT_flto_partition_none:
	  case OPT_flto_partition_1to1:
	  case OPT_flto_partition_balanced:
	      continue;

	  default:
	      break;
	}

      /* For now do what the original LTO option code was doing - pass
	 on any CL_TARGET flag and a few selected others.  */
      switch (option->opt_index)
	{
	case OPT_fPIC:
	case OPT_fpic:
	case OPT_fpie:
	case OPT_fcommon:
	case OPT_fexceptions:
	case OPT_fnon_call_exceptions:
	case OPT_fgnu_tm:
	case OPT_freg_struct_return:
	case OPT_fpcc_struct_return:
	case OPT_ffp_contract_:
	case OPT_fwrapv:
	case OPT_ftrapv:
	case OPT_fstrict_overflow:
	  break;

	default:
	  if (!(cl_options[option->opt_index].flags & CL_TARGET))
	    continue;
	}

      /* Pass the option on.  */
      for (i = 0; i < option->canonical_option_num_elements; ++i)
	obstack_ptr_grow (&argv_obstack, option->canonical_option[i]);
    }

  /* Append linker driver arguments.  Compiler options from the linker
     driver arguments will override / merge with those from the compiler.  */
  for (j = 1; j < decoded_options_count; ++j)
    {
      struct cl_decoded_option *option = &decoded_options[j];

      /* Do not pass on frontend specific flags not suitable for lto.  */
      if (!(cl_options[option->opt_index].flags
	    & (CL_COMMON|CL_TARGET|CL_DRIVER|CL_LTO)))
	continue;

      switch (option->opt_index)
	{
	case OPT_o:
	  linker_output = option->arg;
	  /* We generate new intermediate output, drop this arg.  */
	  continue;

	case OPT_save_temps:
	  debug = 1;
	  break;

	case OPT_v:
	  verbose = 1;
	  break;

	case OPT_flto_partition_none:
	  no_partition = true;
	  break;

	case OPT_flto_:
	  if (strcmp (option->arg, "jobserver") == 0)
	    {
	      jobserver = 1;
	      parallel = 1;
	    }
	  else
	    {
	      parallel = atoi (option->arg);
	      if (parallel <= 1)
		parallel = 0;
	    }
	  /* Fallthru.  */

	case OPT_flto:
	  lto_mode = LTO_MODE_WHOPR;
	  /* We've handled these LTO options, do not pass them on.  */
	  continue;

	case OPT_freg_struct_return:
	case OPT_fpcc_struct_return:
	  /* Ignore these, they are determined by the input files.
	     ???  We fail to diagnose a possible mismatch here.  */
	  continue;

	default:
	  break;
	}

      /* Pass the option on.  */
      for (i = 0; i < option->canonical_option_num_elements; ++i)
	obstack_ptr_grow (&argv_obstack, option->canonical_option[i]);
    }

  if (no_partition)
    {
      lto_mode = LTO_MODE_LTO;
      jobserver = 0;
      parallel = 0;
    }

  if (linker_output)
    {
      char *output_dir, *base, *name;
      bool bit_bucket = strcmp (linker_output, HOST_BIT_BUCKET) == 0;

      output_dir = xstrdup (linker_output);
      base = output_dir;
      for (name = base; *name; name++)
	if (IS_DIR_SEPARATOR (*name))
	  base = name + 1;
      *base = '\0';

      linker_output = &linker_output[base - output_dir];
      if (*output_dir == '\0')
	{
	  static char current_dir[] = { '.', DIR_SEPARATOR, '\0' };
	  output_dir = current_dir;
	}
      if (!bit_bucket)
	{
	  obstack_ptr_grow (&argv_obstack, "-dumpdir");
	  obstack_ptr_grow (&argv_obstack, output_dir);
	}

      obstack_ptr_grow (&argv_obstack, "-dumpbase");
    }

  /* Remember at which point we can scrub args to re-use the commons.  */
  new_head_argc = obstack_object_size (&argv_obstack) / sizeof (void *);

  if (lto_mode == LTO_MODE_LTO)
    {
      flto_out = make_temp_file (".lto.o");
      if (linker_output)
	obstack_ptr_grow (&argv_obstack, linker_output);
      obstack_ptr_grow (&argv_obstack, "-o");
      obstack_ptr_grow (&argv_obstack, flto_out);
    }
  else 
    {
      const char *list_option = "-fltrans-output-list=";
      size_t list_option_len = strlen (list_option);
      char *tmp;

      if (linker_output)
	{
	  char *dumpbase = (char *) xmalloc (strlen (linker_output)
					     + sizeof (".wpa") + 1);
	  strcpy (dumpbase, linker_output);
	  strcat (dumpbase, ".wpa");
	  obstack_ptr_grow (&argv_obstack, dumpbase);
	}

      if (linker_output && debug)
	{
	  ltrans_output_file = (char *) xmalloc (strlen (linker_output)
						 + sizeof (".ltrans.out") + 1);
	  strcpy (ltrans_output_file, linker_output);
	  strcat (ltrans_output_file, ".ltrans.out");
	}
      else
	ltrans_output_file = make_temp_file (".ltrans.out");
      list_option_full = (char *) xmalloc (sizeof (char) *
		         (strlen (ltrans_output_file) + list_option_len + 1));
      tmp = list_option_full;

      obstack_ptr_grow (&argv_obstack, tmp);
      strcpy (tmp, list_option);
      tmp += list_option_len;
      strcpy (tmp, ltrans_output_file);

      if (jobserver)
	obstack_ptr_grow (&argv_obstack, xstrdup ("-fwpa=jobserver"));
      else if (parallel > 1)
	{
	  char buf[256];
	  sprintf (buf, "-fwpa=%i", parallel);
	  obstack_ptr_grow (&argv_obstack, xstrdup (buf));
	}
      else
        obstack_ptr_grow (&argv_obstack, "-fwpa");
    }

  /* Append the input objects and possible preceding arguments.  */
  for (i = 1; i < argc; ++i)
    obstack_ptr_grow (&argv_obstack, argv[i]);
  obstack_ptr_grow (&argv_obstack, NULL);

  new_argv = XOBFINISH (&argv_obstack, const char **);
  argv_ptr = &new_argv[new_head_argc];
  fork_execute (CONST_CAST (char **, new_argv));

  if (lto_mode == LTO_MODE_LTO)
    {
      printf ("%s\n", flto_out);
      free (flto_out);
      flto_out = NULL;
    }
  else
    {
      FILE *stream = fopen (ltrans_output_file, "r");
      FILE *mstream = NULL;
      struct obstack env_obstack;

      if (!stream)
	fatal_perror ("fopen: %s", ltrans_output_file);

      /* Parse the list of LTRANS inputs from the WPA stage.  */
      obstack_init (&env_obstack);
      nr = 0;
      for (;;)
	{
	  const unsigned piece = 32;
	  char *output_name = NULL;
	  char *buf, *input_name = (char *)xmalloc (piece);
	  size_t len;

	  buf = input_name;
cont:
	  if (!fgets (buf, piece, stream))
	    break;
	  len = strlen (input_name);
	  if (input_name[len - 1] != '\n')
	    {
	      input_name = (char *)xrealloc (input_name, len + piece);
	      buf = input_name + len;
	      goto cont;
	    }
	  input_name[len - 1] = '\0';

	  if (input_name[0] == '*')
	    output_name = &input_name[1];

	  nr++;
	  input_names = (char **)xrealloc (input_names, nr * sizeof (char *));
	  output_names = (char **)xrealloc (output_names, nr * sizeof (char *));
	  input_names[nr-1] = input_name;
	  output_names[nr-1] = output_name;
	}
      fclose (stream);
      maybe_unlink_file (ltrans_output_file);
      ltrans_output_file = NULL;

      if (parallel)
	{
	  makefile = make_temp_file (".mk");
	  mstream = fopen (makefile, "w");
	}

      /* Execute the LTRANS stage for each input file (or prepare a
	 makefile to invoke this in parallel).  */
      for (i = 0; i < nr; ++i)
	{
	  char *output_name;
	  char *input_name = input_names[i];
	  /* If it's a pass-through file do nothing.  */
	  if (output_names[i])
	    continue;

	  /* Replace the .o suffix with a .ltrans.o suffix and write
	     the resulting name to the LTRANS output list.  */
	  obstack_grow (&env_obstack, input_name, strlen (input_name) - 2);
	  obstack_grow (&env_obstack, ".ltrans.o", sizeof (".ltrans.o"));
	  output_name = XOBFINISH (&env_obstack, char *);

	  /* Adjust the dumpbase if the linker output file was seen.  */
	  if (linker_output)
	    {
	      char *dumpbase
		  = (char *) xmalloc (strlen (linker_output)
				      + sizeof (DUMPBASE_SUFFIX) + 1);
	      snprintf (dumpbase,
			strlen (linker_output) + sizeof (DUMPBASE_SUFFIX),
			"%s.ltrans%u", linker_output, i);
	      argv_ptr[0] = dumpbase;
	    }

	  argv_ptr[1] = "-fltrans";
	  argv_ptr[2] = "-o";
	  argv_ptr[3] = output_name;
	  argv_ptr[4] = input_name;
	  argv_ptr[5] = NULL;
	  if (parallel)
	    {
	      fprintf (mstream, "%s:\n\t@%s ", output_name, new_argv[0]);
	      for (j = 1; new_argv[j] != NULL; ++j)
		fprintf (mstream, " '%s'", new_argv[j]);
	      fprintf (mstream, "\n");
	      /* If we are not preserving the ltrans input files then
	         truncate them as soon as we have processed it.  This
		 reduces temporary disk-space usage.  */
	      if (! debug)
		fprintf (mstream, "\t@-touch -r %s %s.tem > /dev/null 2>&1 "
			 "&& mv %s.tem %s\n",
			 input_name, input_name, input_name, input_name); 
	    }
	  else
	    {
	      fork_execute (CONST_CAST (char **, new_argv));
	      maybe_unlink_file (input_name);
	    }

	  output_names[i] = output_name;
	}
      if (parallel)
	{
	  struct pex_obj *pex;
	  char jobs[32];

	  fprintf (mstream, "all:");
	  for (i = 0; i < nr; ++i)
	    fprintf (mstream, " \\\n\t%s", output_names[i]);
	  fprintf (mstream, "\n");
	  fclose (mstream);
	  if (!jobserver)
	    {
	      /* Avoid passing --jobserver-fd= and similar flags 
		 unless jobserver mode is explicitly enabled.  */
	      putenv (xstrdup ("MAKEFLAGS="));
	      putenv (xstrdup ("MFLAGS="));
	    }
	  new_argv[0] = getenv ("MAKE");
	  if (!new_argv[0])
	    new_argv[0] = "make";
	  new_argv[1] = "-f";
	  new_argv[2] = makefile;
	  i = 3;
	  if (!jobserver)
	    {
	      snprintf (jobs, 31, "-j%d", parallel);
	      new_argv[i++] = jobs;
	    }
	  new_argv[i++] = "all";
	  new_argv[i++] = NULL;
	  pex = collect_execute (CONST_CAST (char **, new_argv));
	  collect_wait (new_argv[0], pex);
	  maybe_unlink_file (makefile);
	  makefile = NULL;
	  for (i = 0; i < nr; ++i)
	    maybe_unlink_file (input_names[i]);
	}
      for (i = 0; i < nr; ++i)
	{
	  fputs (output_names[i], stdout);
	  putc ('\n', stdout);
	  free (input_names[i]);
	}
      nr = 0;
      free (output_names);
      free (input_names);
      free (list_option_full);
      obstack_free (&env_obstack, NULL);
    }

  obstack_free (&argv_obstack, NULL);
}


/* Entry point.  */

int
main (int argc, char *argv[])
{
  const char *p;

  gcc_obstack_init (&opts_obstack);

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  gcc_init_libintl ();

  diagnostic_initialize (global_dc, 0);

  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, fatal_signal);
#ifdef SIGHUP
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    signal (SIGHUP, fatal_signal);
#endif
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, fatal_signal);
#ifdef SIGPIPE
  if (signal (SIGPIPE, SIG_IGN) != SIG_IGN)
    signal (SIGPIPE, fatal_signal);
#endif
#ifdef SIGCHLD
  /* We *MUST* set SIGCHLD to SIG_DFL so that the wait4() call will
     receive the signal.  A different setting is inheritable */
  signal (SIGCHLD, SIG_DFL);
#endif

  /* We may be called with all the arguments stored in some file and
     passed with @file.  Expand them into argv before processing.  */
  expandargv (&argc, &argv);

  run_gcc (argc, argv);

  return 0;
}
