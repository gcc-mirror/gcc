/* Wrapper to call lto.  Used by collect2 and the linker plugin.
   Copyright (C) 2009-2021 Free Software Foundation, Inc.

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
#include "lto-section-names.h"
#include "collect-utils.h"
#include "opts-diagnostic.h"

/* Environment variable, used for passing the names of offload targets from GCC
   driver to lto-wrapper.  */
#define OFFLOAD_TARGET_NAMES_ENV	"OFFLOAD_TARGET_NAMES"
#define OFFLOAD_TARGET_DEFAULT_ENV	"OFFLOAD_TARGET_DEFAULT"

/* By default there is no special suffix for target executables.  */
#ifdef TARGET_EXECUTABLE_SUFFIX
#define HAVE_TARGET_EXECUTABLE_SUFFIX
#else
#define TARGET_EXECUTABLE_SUFFIX ""
#endif

enum lto_mode_d {
  LTO_MODE_NONE,			/* Not doing LTO.  */
  LTO_MODE_LTO,				/* Normal LTO.  */
  LTO_MODE_WHOPR			/* WHOPR.  */
};

/* Current LTO mode.  */
static enum lto_mode_d lto_mode = LTO_MODE_NONE;

static char *ltrans_output_file;
static char *flto_out;
static unsigned int nr;
static int *ltrans_priorities;
static char **input_names;
static char **output_names;
static char **offload_names;
static char *offload_objects_file_name;
static char *makefile;
static unsigned int num_deb_objs;
static const char **early_debug_object_names;
static bool xassembler_options_error = false;

const char tool_name[] = "lto-wrapper";

/* Delete tempfiles.  Called from utils_cleanup.  */

void
tool_cleanup (bool)
{
  unsigned int i;

  if (ltrans_output_file)
    maybe_unlink (ltrans_output_file);
  if (flto_out)
    maybe_unlink (flto_out);
  if (offload_objects_file_name)
    maybe_unlink (offload_objects_file_name);
  if (makefile)
    maybe_unlink (makefile);
  if (early_debug_object_names)
    for (i = 0; i < num_deb_objs; ++i)
      if (early_debug_object_names[i])
	maybe_unlink (early_debug_object_names[i]);
  for (i = 0; i < nr; ++i)
    {
      maybe_unlink (input_names[i]);
      if (output_names[i])
	maybe_unlink (output_names[i]);
    }
}

static void
lto_wrapper_cleanup (void)
{
  utils_cleanup (false);
}

/* Unlink a temporary LTRANS file unless requested otherwise.  */

void
maybe_unlink (const char *file)
{
  if (!save_temps)
    {
      if (unlink_if_ordinary (file)
	  && errno != ENOENT)
	fatal_error (input_location, "deleting LTRANS file %s: %m", file);
    }
  else if (verbose)
    fprintf (stderr, "[Leaving LTRANS %s]\n", file);
}

/* Template of LTRANS dumpbase suffix.  */
#define DUMPBASE_SUFFIX "ltrans18446744073709551615"

/* Create decoded options from the COLLECT_GCC and COLLECT_GCC_OPTIONS
   environment.  */

static vec<cl_decoded_option>
get_options_from_collect_gcc_options (const char *collect_gcc,
				      const char *collect_gcc_options)
{
  cl_decoded_option *decoded_options;
  unsigned int decoded_options_count;
  struct obstack argv_obstack;
  const char **argv;
  int argc;

  obstack_init (&argv_obstack);
  obstack_ptr_grow (&argv_obstack, collect_gcc);

  parse_options_from_collect_gcc_options (collect_gcc_options,
					  &argv_obstack, &argc);
  argv = XOBFINISH (&argv_obstack, const char **);

  decode_cmdline_options_to_array (argc, (const char **)argv, CL_DRIVER,
				   &decoded_options, &decoded_options_count);
  vec<cl_decoded_option> decoded;
  decoded.create (decoded_options_count);
  for (unsigned i = 0; i < decoded_options_count; ++i)
    decoded.quick_push (decoded_options[i]);
  free (decoded_options);

  obstack_free (&argv_obstack, NULL);

  return decoded;
}

/* Find option in OPTIONS based on OPT_INDEX.  -1 value is returned
   if the option is not present.  */

static int
find_option (vec<cl_decoded_option> &options, size_t opt_index)
{
  for (unsigned i = 0; i < options.length (); ++i)
    if (options[i].opt_index == opt_index)
      return i;

  return -1;
}

static int
find_option (vec<cl_decoded_option> &options, cl_decoded_option *option)
{
  return find_option (options, option->opt_index);
}

/* Try to merge and complain about options FDECODED_OPTIONS when applied
   ontop of DECODED_OPTIONS.  */

static void
merge_and_complain (vec<cl_decoded_option> decoded_options,
		    vec<cl_decoded_option> fdecoded_options,
		    vec<cl_decoded_option> decoded_cl_options)
{
  unsigned int i, j;
  cl_decoded_option *pic_option = NULL;
  cl_decoded_option *pie_option = NULL;
  cl_decoded_option *cf_protection_option = NULL;

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

  /* Look for a -fcf-protection option in the link-time options
     which overrides any -fcf-protection from the lto sections.  */
  for (i = 0; i < decoded_cl_options.length (); ++i)
    {
      cl_decoded_option *foption = &decoded_cl_options[i];
      if (foption->opt_index == OPT_fcf_protection_)
	{
	  cf_protection_option = foption;
	}
    }
  
  /* The following does what the old LTO option code did,
     union all target and a selected set of common options.  */
  for (i = 0; i < fdecoded_options.length (); ++i)
    {
      cl_decoded_option *foption = &fdecoded_options[i];
      int existing_opt = find_option (decoded_options, foption);
      switch (foption->opt_index)
	{
	case OPT_SPECIAL_unknown:
	case OPT_SPECIAL_ignore:
	case OPT_SPECIAL_warn_removed:
	case OPT_SPECIAL_program_name:
	case OPT_SPECIAL_input_file:
	  break;

	default:
	  if (!(cl_options[foption->opt_index].flags & CL_TARGET))
	    break;

	  /* Fallthru.  */
	case OPT_fdiagnostics_show_caret:
	case OPT_fdiagnostics_show_labels:
	case OPT_fdiagnostics_show_line_numbers:
	case OPT_fdiagnostics_show_option:
	case OPT_fdiagnostics_show_location_:
	case OPT_fshow_column:
	case OPT_fcommon:
	case OPT_fgnu_tm:
	case OPT_g:
	  /* Do what the old LTO code did - collect exactly one option
	     setting per OPT code, we pick the first we encounter.
	     ???  This doesn't make too much sense, but when it doesn't
	     then we should complain.  */
	  if (existing_opt == -1)
	    decoded_options.safe_push (*foption);
	  break;

	/* Figure out what PIC/PIE level wins and merge the results.  */
	case OPT_fPIC:
	case OPT_fpic:
	  pic_option = foption;
	  break;
	case OPT_fPIE:
	case OPT_fpie:
	  pie_option = foption;
	  break;

	case OPT_fopenmp:
	case OPT_fopenacc:
	  /* For selected options we can merge conservatively.  */
	  if (existing_opt == -1)
	    decoded_options.safe_push (*foption);
	  /* -fopenmp > -fno-openmp,
	     -fopenacc > -fno-openacc  */
	  else if (foption->value > decoded_options[existing_opt].value)
	    decoded_options[existing_opt] = *foption;
	  break;

	case OPT_fopenacc_dim_:
	  /* Append or check identical.  */
	  if (existing_opt == -1)
	    decoded_options.safe_push (*foption);
	  else if (strcmp (decoded_options[existing_opt].arg, foption->arg))
	    fatal_error (input_location,
			 "option %s with different values",
			 foption->orig_option_with_args_text);
	  break;

	case OPT_fcf_protection_:
	  /* Default to link-time option, else append or check identical.  */
	  if (!cf_protection_option
	      || cf_protection_option->value == CF_CHECK)
	    {
	      if (existing_opt == -1)
		decoded_options.safe_push (*foption);
	      else if (decoded_options[existing_opt].value != foption->value)
		{
		  if (cf_protection_option
		      && cf_protection_option->value == CF_CHECK)
		    fatal_error (input_location,
				 "option %qs with mismatching values"
				 " (%s, %s)",
				 "-fcf-protection",
				 decoded_options[existing_opt].arg,
				 foption->arg);
		  else
		    {
		      /* Merge and update the -fcf-protection option.  */
		      decoded_options[existing_opt].value
			&= (foption->value & CF_FULL);
		      switch (decoded_options[existing_opt].value)
			{
			case CF_NONE:
			  decoded_options[existing_opt].arg = "none";
			  break;
			case CF_BRANCH:
			  decoded_options[existing_opt].arg = "branch";
			  break;
			case CF_RETURN:
			  decoded_options[existing_opt].arg = "return";
			  break;
			default:
			  gcc_unreachable ();
			}
		    }
		}
	    }
	  break;

	case OPT_O:
	case OPT_Ofast:
	case OPT_Og:
	case OPT_Os:
	  existing_opt = -1;
	  for (j = 0; j < decoded_options.length (); ++j)
	    if (decoded_options[j].opt_index == OPT_O
		|| decoded_options[j].opt_index == OPT_Ofast
		|| decoded_options[j].opt_index == OPT_Og
		|| decoded_options[j].opt_index == OPT_Os)
	      {
		existing_opt = j;
		break;
	      }
	  if (existing_opt == -1)
	    decoded_options.safe_push (*foption);
	  else if (decoded_options[existing_opt].opt_index == foption->opt_index
		   && foption->opt_index != OPT_O)
	    /* Exact same options get merged.  */
	    ;
	  else
	    {
	      /* For mismatched option kinds preserve the optimization
	         level only, thus merge it as -On.  This also handles
		 merging of same optimization level -On.  */
	      int level = 0;
	      switch (foption->opt_index)
		{
		case OPT_O:
		  if (foption->arg[0] == '\0')
		    level = MAX (level, 1);
		  else
		    level = MAX (level, atoi (foption->arg));
		  break;
		case OPT_Ofast:
		  level = MAX (level, 3);
		  break;
		case OPT_Og:
		  level = MAX (level, 1);
		  break;
		case OPT_Os:
		  level = MAX (level, 2);
		  break;
		default:
		  gcc_unreachable ();
		}
	      switch (decoded_options[existing_opt].opt_index)
		{
		case OPT_O:
		  if (decoded_options[existing_opt].arg[0] == '\0')
		    level = MAX (level, 1);
		  else
		    level = MAX (level,
				 atoi (decoded_options[existing_opt].arg));
		  break;
		case OPT_Ofast:
		  level = MAX (level, 3);
		  break;
		case OPT_Og:
		  level = MAX (level, 1);
		  break;
		case OPT_Os:
		  level = MAX (level, 2);
		  break;
		default:
		  gcc_unreachable ();
		}
	      decoded_options[existing_opt].opt_index = OPT_O;
	      char *tem;
	      tem = xasprintf ("-O%d", level);
	      decoded_options[existing_opt].arg = &tem[2];
	      decoded_options[existing_opt].canonical_option[0] = tem;
	      decoded_options[existing_opt].value = 1;
	    }
	  break;
 

	case OPT_foffload_abi_:
	  if (existing_opt == -1)
	    decoded_options.safe_push (*foption);
	  else if (foption->value != decoded_options[existing_opt].value)
	    fatal_error (input_location,
			 "option %s not used consistently in all LTO input"
			 " files", foption->orig_option_with_args_text);
	  break;


	case OPT_foffload_:
	  decoded_options.safe_push (*foption);
	  break;

	case OPT_flto_:
	  if (existing_opt == -1)
	    decoded_options.safe_push (*foption);
	  else
	    {
	      if (strcmp (foption->arg, decoded_options[existing_opt].arg) != 0)
		{
		  /* -flto=auto is preferred.  */
		  if (strcmp (decoded_options[existing_opt].arg, "auto") == 0)
		    ;
		  else if (strcmp (foption->arg, "auto") == 0
			   || strcmp (foption->arg, "jobserver") == 0)
		    decoded_options[existing_opt].arg = foption->arg;
		  else if (strcmp (decoded_options[existing_opt].arg,
				   "jobserver") != 0)
		    {
		      int n = atoi (foption->arg);
		      int original_n = atoi (decoded_options[existing_opt].arg);
		      if (n > original_n)
			decoded_options[existing_opt].arg = foption->arg;
		    }
		}
	    }
	  break;
	}
    }

  /* Merge PIC options:
      -fPIC + -fpic = -fpic
      -fPIC + -fno-pic = -fno-pic
      -fpic/-fPIC + nothing = nothing.
     It is a common mistake to mix few -fPIC compiled objects into otherwise
     non-PIC code.  We do not want to build everything with PIC then.

     Similarly we merge PIE options, however in addition we keep
      -fPIC + -fPIE = -fPIE
      -fpic + -fPIE = -fpie
      -fPIC/-fpic + -fpie = -fpie

     It would be good to warn on mismatches, but it is bit hard to do as
     we do not know what nothing translates to.  */
    
  for (unsigned int j = 0; j < decoded_options.length ();)
    if (decoded_options[j].opt_index == OPT_fPIC
	|| decoded_options[j].opt_index == OPT_fpic)
      {
	/* -fno-pic in one unit implies -fno-pic everywhere.  */
	if (decoded_options[j].value == 0)
	  j++;
	/* If we have no pic option or merge in -fno-pic, we still may turn
	   existing pic/PIC mode into pie/PIE if -fpie/-fPIE is present.  */
	else if ((pic_option && pic_option->value == 0)
		 || !pic_option)
	  {
	    if (pie_option)
	      {
		bool big = decoded_options[j].opt_index == OPT_fPIC
			   && pie_option->opt_index == OPT_fPIE;
		decoded_options[j].opt_index = big ? OPT_fPIE : OPT_fpie;
		if (pie_option->value)
		  decoded_options[j].canonical_option[0]
		    = big ? "-fPIE" : "-fpie";
		else
		  decoded_options[j].canonical_option[0] = "-fno-pie";
		decoded_options[j].value = pie_option->value;
		j++;
	      }
	    else if (pic_option)
	      {
		decoded_options[j] = *pic_option;
		j++;
	      }
	    /* We do not know if target defaults to pic or not, so just remove
	       option if it is missing in one unit but enabled in other.  */
	    else
	      decoded_options.ordered_remove (j);
	  }
	else if (pic_option->opt_index == OPT_fpic
		 && decoded_options[j].opt_index == OPT_fPIC)
	  {
	    decoded_options[j] = *pic_option;
	    j++;
	  }
	else
	  j++;
      }
   else if (decoded_options[j].opt_index == OPT_fPIE
	    || decoded_options[j].opt_index == OPT_fpie)
      {
	/* -fno-pie in one unit implies -fno-pie everywhere.  */
	if (decoded_options[j].value == 0)
	  j++;
	/* If we have no pie option or merge in -fno-pie, we still preserve
	   PIE/pie if pic/PIC is present.  */
	else if ((pie_option && pie_option->value == 0)
		 || !pie_option)
	  {
	    /* If -fPIC/-fpic is given, merge it with -fPIE/-fpie.  */
	    if (pic_option)
	      {
		if (pic_option->opt_index == OPT_fpic
		    && decoded_options[j].opt_index == OPT_fPIE)
		  {
		    decoded_options[j].opt_index = OPT_fpie;
		    decoded_options[j].canonical_option[0]
		      = pic_option->value ? "-fpie" : "-fno-pie";
		  }
		else if (!pic_option->value)
		  decoded_options[j].canonical_option[0] = "-fno-pie";
		decoded_options[j].value = pic_option->value;
		j++;
	      }
	    else if (pie_option)
	      {
		decoded_options[j] = *pie_option;
		j++;
	      }
	    /* Because we always append pic/PIE options this code path should
	       not happen unless the LTO object was built by old lto1 which
	       did not contain that logic yet.  */
	    else
	      decoded_options.ordered_remove (j);
	  }
	else if (pie_option->opt_index == OPT_fpie
		 && decoded_options[j].opt_index == OPT_fPIE)
	  {
	    decoded_options[j] = *pie_option;
	    j++;
	  }
	else
	  j++;
      }
   else
     j++;

  if (!xassembler_options_error)
    for (i = j = 0; ; i++, j++)
      {
	int existing_opt_index
	  = find_option (decoded_options, OPT_Xassembler);
	int existing_opt2_index
	  = find_option (fdecoded_options, OPT_Xassembler);

	cl_decoded_option *existing_opt = NULL;
	cl_decoded_option *existing_opt2 = NULL;
	if (existing_opt_index != -1)
	  existing_opt = &decoded_options[existing_opt_index];
	if (existing_opt2_index != -1)
	  existing_opt2 = &fdecoded_options[existing_opt2_index];

	if (existing_opt == NULL && existing_opt2 == NULL)
	  break;
	else if (existing_opt != NULL && existing_opt2 == NULL)
	  {
	    warning (0, "Extra option to %<-Xassembler%>: %s,"
		     " dropping all %<-Xassembler%> and %<-Wa%> options.",
		     existing_opt->arg);
	    xassembler_options_error = true;
	    break;
	  }
	else if (existing_opt == NULL && existing_opt2 != NULL)
	  {
	    warning (0, "Extra option to %<-Xassembler%>: %s,"
		     " dropping all %<-Xassembler%> and %<-Wa%> options.",
		     existing_opt2->arg);
	    xassembler_options_error = true;
	    break;
	  }
	else if (strcmp (existing_opt->arg, existing_opt2->arg) != 0)
	  {
	    warning (0, "Options to %<-Xassembler%> do not match: %s, %s,"
		     " dropping all %<-Xassembler%> and %<-Wa%> options.",
		     existing_opt->arg, existing_opt2->arg);
	    xassembler_options_error = true;
	    break;
	  }
      }
}

/* Auxiliary function that frees elements of PTR and PTR itself.
   N is number of elements to be freed.  If PTR is NULL, nothing is freed.
   If an element is NULL, subsequent elements are not freed.  */

static void **
free_array_of_ptrs (void **ptr, unsigned n)
{
  if (!ptr)
    return NULL;
  for (unsigned i = 0; i < n; i++)
    {
      if (!ptr[i])
	break;
      free (ptr[i]);
    }
  free (ptr);
  return NULL;
}

/* Parse STR, saving found tokens into PVALUES and return their number.
   Tokens are assumed to be delimited by ':'.  If APPEND is non-null,
   append it to every token we find.  */

static unsigned
parse_env_var (const char *str, char ***pvalues, const char *append)
{
  const char *curval, *nextval;
  char **values;
  unsigned num = 1, i;

  curval = strchr (str, ':');
  while (curval)
    {
      num++;
      curval = strchr (curval + 1, ':');
    }

  values = (char**) xmalloc (num * sizeof (char*));
  curval = str;
  nextval = strchr (curval, ':');
  if (nextval == NULL)
    nextval = strchr (curval, '\0');

  int append_len = append ? strlen (append) : 0;
  for (i = 0; i < num; i++)
    {
      int l = nextval - curval;
      values[i] = (char*) xmalloc (l + 1 + append_len);
      memcpy (values[i], curval, l);
      values[i][l] = 0;
      if (append)
	strcat (values[i], append);
      curval = nextval + 1;
      nextval = strchr (curval, ':');
      if (nextval == NULL)
	nextval = strchr (curval, '\0');
    }
  *pvalues = values;
  return num;
}

/* Append options OPTS from lto or offload_lto sections to ARGV_OBSTACK.  */

static void
append_compiler_options (obstack *argv_obstack, vec<cl_decoded_option> opts)
{
  /* Append compiler driver arguments as far as they were merged.  */
  for (unsigned int j = 1; j < opts.length (); ++j)
    {
      cl_decoded_option *option = &opts[j];

      /* File options have been properly filtered by lto-opts.c.  */
      switch (option->opt_index)
	{
	/* Drop arguments that we want to take from the link line.  */
	case OPT_flto_:
	case OPT_flto:
	case OPT_flto_partition_:
	  continue;

	default:
	  break;
	}

      /* For now do what the original LTO option code was doing - pass
	 on any CL_TARGET flag and a few selected others.  */
      switch (option->opt_index)
	{
	case OPT_fdiagnostics_show_caret:
	case OPT_fdiagnostics_show_labels:
	case OPT_fdiagnostics_show_line_numbers:
	case OPT_fdiagnostics_show_option:
	case OPT_fdiagnostics_show_location_:
	case OPT_fshow_column:
	case OPT_fPIC:
	case OPT_fpic:
	case OPT_fPIE:
	case OPT_fpie:
	case OPT_fcommon:
	case OPT_fgnu_tm:
	case OPT_fopenmp:
	case OPT_fopenacc:
	case OPT_fopenacc_dim_:
	case OPT_foffload_abi_:
	case OPT_fcf_protection_:
	case OPT_g:
	case OPT_O:
	case OPT_Ofast:
	case OPT_Og:
	case OPT_Os:
	  break;

	case OPT_Xassembler:
	  /* When we detected a mismatch in assembler options between
	     the input TU's fall back to previous behavior of ignoring them.  */
	  if (xassembler_options_error)
	    continue;
	  break;

	default:
	  if (!(cl_options[option->opt_index].flags & CL_TARGET))
	    continue;
	}

      /* Pass the option on.  */
      for (unsigned int i = 0; i < option->canonical_option_num_elements; ++i)
	obstack_ptr_grow (argv_obstack, option->canonical_option[i]);
    }
}

/* Append diag options in OPTS to ARGV_OBSTACK.  */

static void
append_diag_options (obstack *argv_obstack, vec<cl_decoded_option> opts)
{
  /* Append compiler driver arguments as far as they were merged.  */
  for (unsigned int j = 1; j < opts.length (); ++j)
    {
      cl_decoded_option *option = &opts[j];

      switch (option->opt_index)
	{
	case OPT_fdiagnostics_color_:
	case OPT_fdiagnostics_format_:
	case OPT_fdiagnostics_show_caret:
	case OPT_fdiagnostics_show_labels:
	case OPT_fdiagnostics_show_line_numbers:
	case OPT_fdiagnostics_show_option:
	case OPT_fdiagnostics_show_location_:
	case OPT_fshow_column:
	  break;
	default:
	  continue;
	}

      /* Pass the option on.  */
      for (unsigned int i = 0; i < option->canonical_option_num_elements; ++i)
	obstack_ptr_grow (argv_obstack, option->canonical_option[i]);
    }
}


/* Append linker options OPTS to ARGV_OBSTACK.  */

static void
append_linker_options (obstack *argv_obstack, vec<cl_decoded_option> opts)
{
  /* Append linker driver arguments.  Compiler options from the linker
     driver arguments will override / merge with those from the compiler.  */
  for (unsigned int j = 1; j < opts.length (); ++j)
    {
      cl_decoded_option *option = &opts[j];

      /* Do not pass on frontend specific flags not suitable for lto.  */
      if (!(cl_options[option->opt_index].flags
	    & (CL_COMMON|CL_TARGET|CL_DRIVER|CL_LTO)))
	continue;

      switch (option->opt_index)
	{
	case OPT_o:
	case OPT_flto_:
	case OPT_flto:
	  /* We've handled these LTO options, do not pass them on.  */
	  continue;

	case OPT_fopenmp:
	case OPT_fopenacc:
	  /* Ignore -fno-XXX form of these options, as otherwise
	     corresponding builtins will not be enabled.  */
	  if (option->value == 0)
	    continue;
	  break;

	default:
	  break;
	}

      /* Pass the option on.  */
      for (unsigned int i = 0; i < option->canonical_option_num_elements; ++i)
	obstack_ptr_grow (argv_obstack, option->canonical_option[i]);
    }
}

/* Extract options for TARGET offload compiler from OPTIONS and append
   them to ARGV_OBSTACK.  */

static void
append_offload_options (obstack *argv_obstack, const char *target,
			vec<cl_decoded_option> options)
{
  for (unsigned i = 0; i < options.length (); i++)
    {
      const char *cur, *next, *opts;
      char **argv;
      unsigned argc;
      cl_decoded_option *option = &options[i];

      if (option->opt_index != OPT_foffload_)
	continue;

      /* If option argument starts with '-' then no target is specified.  That
	 means offload options are specified for all targets, so we need to
	 append them.  */
      if (option->arg[0] == '-')
	opts = option->arg;
      else
	{
	  opts = strchr (option->arg, '=');
	  /* If there are offload targets specified, but no actual options,
	     there is nothing to do here.  */
	  if (!opts)
	    continue;

	  cur = option->arg;

	  while (cur < opts)
	    {
	      next = strchr (cur, ',');
	      if (next == NULL)
		next = opts;
	      next = (next > opts) ? opts : next;

	      /* Are we looking for this offload target?  */
	      if (strlen (target) == (size_t) (next - cur)
		  && strncmp (target, cur, next - cur) == 0)
		break;

	      /* Skip the comma or equal sign.  */
	      cur = next + 1;
	    }

	  if (cur >= opts)
	    continue;

	  opts++;
	}

      argv = buildargv (opts);
      for (argc = 0; argv[argc]; argc++)
	obstack_ptr_grow (argv_obstack, argv[argc]);
    }
}

/* Check whether NAME can be accessed in MODE.  This is like access,
   except that it never considers directories to be executable.  */

static int
access_check (const char *name, int mode)
{
  if (mode == X_OK)
    {
      struct stat st;

      if (stat (name, &st) < 0
	  || S_ISDIR (st.st_mode))
	return -1;
    }

  return access (name, mode);
}

/* Prepare a target image for offload TARGET, using mkoffload tool from
   COMPILER_PATH.  Return the name of the resultant object file.  */

static char *
compile_offload_image (const char *target, const char *compiler_path,
		       unsigned in_argc, char *in_argv[],
		       vec<cl_decoded_option> compiler_opts,
		       vec<cl_decoded_option> linker_opts)
{
  char *filename = NULL;
  char *dumpbase;
  char **argv;
  char *suffix
    = XALLOCAVEC (char, sizeof ("/accel//mkoffload") + strlen (target));
  strcpy (suffix, "/accel/");
  strcat (suffix, target);
  strcat (suffix, "/mkoffload");

  char **paths = NULL;
  unsigned n_paths = parse_env_var (compiler_path, &paths, suffix);

  const char *compiler = NULL;
  for (unsigned i = 0; i < n_paths; i++)
    if (access_check (paths[i], X_OK) == 0)
      {
	compiler = paths[i];
	break;
      }
#if OFFLOAD_DEFAULTED
  if (!compiler && getenv (OFFLOAD_TARGET_DEFAULT_ENV))
    {
      free_array_of_ptrs ((void **) paths, n_paths);
      return NULL;
    }
#endif

  if (!compiler)
    fatal_error (input_location,
		 "could not find %s in %s (consider using %<-B%>)",
		 suffix + 1, compiler_path);

  dumpbase = concat (dumppfx, "x", target, NULL);

  /* Generate temporary output file name.  */
  if (save_temps)
    filename = concat (dumpbase, ".o", NULL);
  else
    filename = make_temp_file (".target.o");

  struct obstack argv_obstack;
  obstack_init (&argv_obstack);
  obstack_ptr_grow (&argv_obstack, compiler);
  if (save_temps)
    obstack_ptr_grow (&argv_obstack, "-save-temps");
  if (verbose)
    obstack_ptr_grow (&argv_obstack, "-v");
  obstack_ptr_grow (&argv_obstack, "-o");
  obstack_ptr_grow (&argv_obstack, filename);

  /* Append names of input object files.  */
  for (unsigned i = 0; i < in_argc; i++)
    obstack_ptr_grow (&argv_obstack, in_argv[i]);

  /* Append options from offload_lto sections.  */
  append_compiler_options (&argv_obstack, compiler_opts);
  append_diag_options (&argv_obstack, linker_opts);

  obstack_ptr_grow (&argv_obstack, "-dumpbase");
  obstack_ptr_grow (&argv_obstack, dumpbase);

  /* Append options specified by -foffload last.  In case of conflicting
     options we expect offload compiler to choose the latest.  */
  append_offload_options (&argv_obstack, target, compiler_opts);
  append_offload_options (&argv_obstack, target, linker_opts);

  obstack_ptr_grow (&argv_obstack, NULL);
  argv = XOBFINISH (&argv_obstack, char **);
  fork_execute (argv[0], argv, true, "offload_args");
  obstack_free (&argv_obstack, NULL);

  free_array_of_ptrs ((void **) paths, n_paths);
  return filename;
}


/* The main routine dealing with offloading.
   The routine builds a target image for each offload target.  IN_ARGC and
   IN_ARGV specify options and input object files.  As all of them could contain
   target sections, we pass them all to target compilers.  */

static void
compile_images_for_offload_targets (unsigned in_argc, char *in_argv[],
				    vec<cl_decoded_option> compiler_opts,
				    vec<cl_decoded_option> linker_opts)
{
  char **names = NULL;
  const char *target_names = getenv (OFFLOAD_TARGET_NAMES_ENV);
  if (!target_names)
    return;
  unsigned num_targets = parse_env_var (target_names, &names, NULL);
  int next_name_entry = 0;

  const char *compiler_path = getenv ("COMPILER_PATH");
  if (!compiler_path)
    goto out;

  /* Prepare an image for each target and save the name of the resultant object
     file to the OFFLOAD_NAMES array.  It is terminated by a NULL entry.  */
  offload_names = XCNEWVEC (char *, num_targets + 1);
  for (unsigned i = 0; i < num_targets; i++)
    {
      offload_names[next_name_entry]
	= compile_offload_image (names[i], compiler_path, in_argc, in_argv,
				 compiler_opts, linker_opts);
      if (!offload_names[next_name_entry])
#if OFFLOAD_DEFAULTED
	continue;
#else
	fatal_error (input_location,
		     "problem with building target image for %s", names[i]);
#endif
      next_name_entry++;
    }

#if OFFLOAD_DEFAULTED
  if (next_name_entry == 0)
    {
      free (offload_names);
      offload_names = NULL;
    }
#endif

 out:
  free_array_of_ptrs ((void **) names, num_targets);
}

/* Copy a file from SRC to DEST.  */

static void
copy_file (const char *dest, const char *src)
{
  FILE *d = fopen (dest, "wb");
  FILE *s = fopen (src, "rb");
  char buffer[512];
  while (!feof (s))
    {
      size_t len = fread (buffer, 1, 512, s);
      if (ferror (s) != 0)
	fatal_error (input_location, "reading input file");
      if (len > 0)
	{
	  fwrite (buffer, 1, len, d);
	  if (ferror (d) != 0)
	    fatal_error (input_location, "writing output file");
	}
    }
  fclose (d);
  fclose (s);
}

/* Find the crtoffloadtable.o file in LIBRARY_PATH, make copy and pass name of
   the copy to the linker.  */

static void
find_crtoffloadtable (int save_temps, const char *dumppfx)
{
  char **paths = NULL;
  const char *library_path = getenv ("LIBRARY_PATH");
  if (!library_path)
    return;
  unsigned n_paths = parse_env_var (library_path, &paths, "/crtoffloadtable.o");

  unsigned i;
  for (i = 0; i < n_paths; i++)
    if (access_check (paths[i], R_OK) == 0)
      {
	/* The linker will delete the filename we give it, so make a copy.  */
	char *crtoffloadtable;
	if (!save_temps)
	  crtoffloadtable = make_temp_file (".crtoffloadtable.o");
	else
	  crtoffloadtable = concat (dumppfx, "crtoffloadtable.o", NULL);
	copy_file (crtoffloadtable, paths[i]);
	printf ("%s\n", crtoffloadtable);
	XDELETEVEC (crtoffloadtable);
	break;
      }
  if (i == n_paths)
    fatal_error (input_location,
		 "installation error, cannot find %<crtoffloadtable.o%>");

  free_array_of_ptrs ((void **) paths, n_paths);
}

/* A subroutine of run_gcc.  Examine the open file FD for lto sections with
   name prefix PREFIX, at FILE_OFFSET, and store any options we find in OPTS.
   Return true if we found a matching section, false
   otherwise.  COLLECT_GCC holds the value of the environment variable with
   the same name.  */

static bool
find_and_merge_options (int fd, off_t file_offset, const char *prefix,
			vec<cl_decoded_option> decoded_cl_options,
			vec<cl_decoded_option> *opts, const char *collect_gcc)
{
  off_t offset, length;
  char *data;
  char *fopts;
  const char *errmsg;
  int err;
  vec<cl_decoded_option> fdecoded_options;

  simple_object_read *sobj;
  sobj = simple_object_start_read (fd, file_offset, "__GNU_LTO",
				   &errmsg, &err);
  if (!sobj)
    return false;

  char *secname = XALLOCAVEC (char, strlen (prefix) + sizeof (".opts"));
  strcpy (secname, prefix);
  strcat (secname, ".opts");
  if (!simple_object_find_section (sobj, secname, &offset, &length,
				   &errmsg, &err))
    {
      simple_object_release_read (sobj);
      return false;
    }

  lseek (fd, file_offset + offset, SEEK_SET);
  data = (char *)xmalloc (length);
  read (fd, data, length);
  fopts = data;
  bool first = true;
  do
    {
      vec<cl_decoded_option> f2decoded_options
	= get_options_from_collect_gcc_options (collect_gcc, fopts);
      if (first)
	{
	  fdecoded_options = f2decoded_options;
	  first = false;
	}
      else
	merge_and_complain (fdecoded_options, f2decoded_options,
			    decoded_cl_options);

      fopts += strlen (fopts) + 1;
    }
  while (fopts - data < length);

  free (data);
  simple_object_release_read (sobj);
  *opts = fdecoded_options;
  return true;
}

/* Copy early debug info sections from INFILE to a new file whose name
   is returned.  Return NULL on error.  */

const char *
debug_objcopy (const char *infile, bool rename)
{
  char *outfile;
  const char *errmsg;
  int err;

  const char *p;
  const char *orig_infile = infile;
  off_t inoff = 0;
  long loffset;
  int consumed;
  if ((p = strrchr (infile, '@'))
      && p != infile
      && sscanf (p, "@%li%n", &loffset, &consumed) >= 1
      && strlen (p) == (unsigned int) consumed)
    {
      char *fname = xstrdup (infile);
      fname[p - infile] = '\0';
      infile = fname;
      inoff = (off_t) loffset;
    }
  int infd = open (infile, O_RDONLY | O_BINARY);
  if (infd == -1)
    return NULL;
  simple_object_read *inobj = simple_object_start_read (infd, inoff,
							"__GNU_LTO",
							&errmsg, &err);
  if (!inobj)
    return NULL;

  off_t off, len;
  if (simple_object_find_section (inobj, ".gnu.debuglto_.debug_info",
				  &off, &len, &errmsg, &err) != 1)
    {
      if (errmsg)
	fatal_error (0, "%s: %s", errmsg, xstrerror (err));

      simple_object_release_read (inobj);
      close (infd);
      return NULL;
    }

  if (save_temps)
    outfile = concat (orig_infile, ".debug.temp.o", NULL);
  else
    outfile = make_temp_file (".debug.temp.o");
  errmsg = simple_object_copy_lto_debug_sections (inobj, outfile, &err, rename);
  if (errmsg)
    {
      unlink_if_ordinary (outfile);
      fatal_error (0, "%s: %s", errmsg, xstrerror (err));
    }

  simple_object_release_read (inobj);
  close (infd);

  return outfile;
}

/* Helper for qsort: compare priorities for parallel compilation.  */

int
cmp_priority (const void *a, const void *b)
{
  return *((const int *)b)-*((const int *)a);
}

/* Number of CPUs that can be used for parallel LTRANS phase.  */

static unsigned long nthreads_var = 0;

#ifdef HAVE_PTHREAD_AFFINITY_NP
unsigned long cpuset_size;
static unsigned long get_cpuset_size;
cpu_set_t *cpusetp;

unsigned long
static cpuset_popcount (unsigned long cpusetsize, cpu_set_t *cpusetp)
{
#ifdef CPU_COUNT_S
  /* glibc 2.7 and above provide a macro for this.  */
  return CPU_COUNT_S (cpusetsize, cpusetp);
#else
#ifdef CPU_COUNT
  if (cpusetsize == sizeof (cpu_set_t))
    /* glibc 2.6 and above provide a macro for this.  */
    return CPU_COUNT (cpusetp);
#endif
  size_t i;
  unsigned long ret = 0;
  STATIC_ASSERT (sizeof (cpusetp->__bits[0]) == sizeof (unsigned long int));
  for (i = 0; i < cpusetsize / sizeof (cpusetp->__bits[0]); i++)
    {
      unsigned long int mask = cpusetp->__bits[i];
      if (mask == 0)
	continue;
      ret += __builtin_popcountl (mask);
    }
  return ret;
#endif
}
#endif

/* At startup, determine the default number of threads.  It would seem
   this should be related to the number of cpus online.  */

static void
init_num_threads (void)
{
#ifdef HAVE_PTHREAD_AFFINITY_NP
#if defined (_SC_NPROCESSORS_CONF) && defined (CPU_ALLOC_SIZE)
  cpuset_size = sysconf (_SC_NPROCESSORS_CONF);
  cpuset_size = CPU_ALLOC_SIZE (cpuset_size);
#else
  cpuset_size = sizeof (cpu_set_t);
#endif

  cpusetp = (cpu_set_t *) xmalloc (gomp_cpuset_size);
  do
    {
      int ret = pthread_getaffinity_np (pthread_self (), gomp_cpuset_size,
					cpusetp);
      if (ret == 0)
	{
	  /* Count only the CPUs this process can use.  */
	  nthreads_var = cpuset_popcount (cpuset_size, cpusetp);
	  if (nthreads_var == 0)
	    break;
	  get_cpuset_size = cpuset_size;
#ifdef CPU_ALLOC_SIZE
	  unsigned long i;
	  for (i = cpuset_size * 8; i; i--)
	    if (CPU_ISSET_S (i - 1, cpuset_size, cpusetp))
	      break;
	  cpuset_size = CPU_ALLOC_SIZE (i);
#endif
	  return;
	}
      if (ret != EINVAL)
	break;
#ifdef CPU_ALLOC_SIZE
      if (cpuset_size < sizeof (cpu_set_t))
	cpuset_size = sizeof (cpu_set_t);
      else
	cpuset_size = cpuset_size * 2;
      if (cpuset_size < 8 * sizeof (cpu_set_t))
	cpusetp
	  = (cpu_set_t *) realloc (cpusetp, cpuset_size);
      else
	{
	  /* Avoid fatal if too large memory allocation would be
	     requested, e.g. kernel returning EINVAL all the time.  */
	  void *p = realloc (cpusetp, cpuset_size);
	  if (p == NULL)
	    break;
	  cpusetp = (cpu_set_t *) p;
	}
#else
      break;
#endif
    }
  while (1);
  cpuset_size = 0;
  nthreads_var = 1;
  free (cpusetp);
  cpusetp = NULL;
#endif
#ifdef _SC_NPROCESSORS_ONLN
  nthreads_var = sysconf (_SC_NPROCESSORS_ONLN);
#endif
}

/* Test and return reason why a jobserver cannot be detected.  */

static const char *
jobserver_active_p (void)
{
  #define JS_PREFIX "jobserver is not available: "
  #define JS_NEEDLE "--jobserver-auth="

  const char *makeflags = getenv ("MAKEFLAGS");
  if (makeflags == NULL)
    return JS_PREFIX "%<MAKEFLAGS%> environment variable is unset";

  const char *n = strstr (makeflags, JS_NEEDLE);
  if (n == NULL)
    return JS_PREFIX "%<" JS_NEEDLE "%> is not present in %<MAKEFLAGS%>";

  int rfd = -1;
  int wfd = -1;

  if (sscanf (n + strlen (JS_NEEDLE), "%d,%d", &rfd, &wfd) == 2
      && rfd > 0
      && wfd > 0
      && is_valid_fd (rfd)
      && is_valid_fd (wfd))
    return NULL;
  else
    return JS_PREFIX "cannot access %<" JS_NEEDLE "%> file descriptors";
}

/* Print link to -flto documentation with a hint message.  */

void
print_lto_docs_link ()
{
  const char *url = get_option_url (NULL, OPT_flto);

  pretty_printer pp;
  pp.url_format = URL_FORMAT_DEFAULT;
  pp_string (&pp, "see the ");
  pp_begin_url (&pp, url);
  pp_string (&pp, "%<-flto%> option documentation");
  pp_end_url (&pp);
  pp_string (&pp, " for more information");
  inform (UNKNOWN_LOCATION, pp_formatted_text (&pp));
}

/* Test that a make command is present and working, return true if so.  */

static bool
make_exists (void)
{
  const char *make = "make";
  char **make_argv = buildargv (getenv ("MAKE"));
  if (make_argv)
    make = make_argv[0];
  const char *make_args[] = {make, "--version", NULL};

  int exit_status = 0;
  int err = 0;
  const char *errmsg
    = pex_one (PEX_SEARCH, make_args[0], CONST_CAST (char **, make_args),
	       "make", NULL, NULL, &exit_status, &err);
  freeargv (make_argv);
  return errmsg == NULL && exit_status == 0 && err == 0;
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
  const char *collect_gcc;
  char *collect_gcc_options;
  int parallel = 0;
  int jobserver = 0;
  bool jobserver_requested = false;
  int auto_parallel = 0;
  bool no_partition = false;
  const char *jobserver_error = NULL;
  vec<cl_decoded_option> fdecoded_options;
  fdecoded_options.create (16);
  vec<cl_decoded_option> offload_fdecoded_options = vNULL;
  struct obstack argv_obstack;
  int new_head_argc;
  bool have_lto = false;
  bool have_offload = false;
  unsigned lto_argc = 0, ltoobj_argc = 0;
  char **lto_argv, **ltoobj_argv;
  bool linker_output_rel = false;
  bool skip_debug = false;
  unsigned n_debugobj;
  const char *incoming_dumppfx = dumppfx = NULL;
  static char current_dir[] = { '.', DIR_SEPARATOR, '\0' };

  /* Get the driver and options.  */
  collect_gcc = getenv ("COLLECT_GCC");
  if (!collect_gcc)
    fatal_error (input_location,
		 "environment variable %<COLLECT_GCC%> must be set");
  collect_gcc_options = getenv ("COLLECT_GCC_OPTIONS");
  if (!collect_gcc_options)
    fatal_error (input_location,
		 "environment variable %<COLLECT_GCC_OPTIONS%> must be set");

  char *collect_as_options = getenv ("COLLECT_AS_OPTIONS");

  /* Prepend -Xassembler to each option, and append the string
     to collect_gcc_options.  */
  if (collect_as_options)
    {
      obstack temporary_obstack;
      obstack_init (&temporary_obstack);

      prepend_xassembler_to_collect_as_options (collect_as_options,
						&temporary_obstack);
      obstack_1grow (&temporary_obstack, '\0');

      char *xassembler_opts_string
	= XOBFINISH (&temporary_obstack, char *);
      collect_gcc_options = concat (collect_gcc_options, xassembler_opts_string,
				    NULL);
    }

  vec<cl_decoded_option> decoded_options
    = get_options_from_collect_gcc_options (collect_gcc, collect_gcc_options);

  /* Allocate array for input object files with LTO IL,
     and for possible preceding arguments.  */
  lto_argv = XNEWVEC (char *, argc);
  ltoobj_argv = XNEWVEC (char *, argc);

  /* Look at saved options in the IL files.  */
  for (i = 1; i < argc; ++i)
    {
      char *p;
      int fd;
      off_t file_offset = 0;
      long loffset;
      int consumed;
      char *filename = argv[i];

      if (startswith (argv[i], "-foffload-objects="))
	{
	  have_offload = true;
	  offload_objects_file_name
	    = argv[i] + sizeof ("-foffload-objects=") - 1;
	  continue;
	}

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
      fd = open (filename, O_RDONLY | O_BINARY);
      /* Linker plugin passes -fresolution and -flinker-output options.
	 -flinker-output is passed only when user did not specify one and thus
	 we do not need to worry about duplicities with the option handling
	 below. */
      if (fd == -1)
	{
	  lto_argv[lto_argc++] = argv[i];
	  if (strcmp (argv[i], "-flinker-output=rel") == 0)
	    linker_output_rel = true;
	  continue;
	}

      if (find_and_merge_options (fd, file_offset, LTO_SECTION_NAME_PREFIX,
				  decoded_options, &fdecoded_options,
				  collect_gcc))
	{
	  have_lto = true;
	  ltoobj_argv[ltoobj_argc++] = argv[i];
	}
      close (fd);
    }

  /* Initalize the common arguments for the driver.  */
  obstack_init (&argv_obstack);
  obstack_ptr_grow (&argv_obstack, collect_gcc);
  obstack_ptr_grow (&argv_obstack, "-xlto");
  obstack_ptr_grow (&argv_obstack, "-c");

  append_compiler_options (&argv_obstack, fdecoded_options);
  append_linker_options (&argv_obstack, decoded_options);

  /* Process LTO-related options on merged options.  */
  for (j = 1; j < fdecoded_options.length (); ++j)
    {
      cl_decoded_option *option = &fdecoded_options[j];
      switch (option->opt_index)
	{
	case OPT_flto_:
	  if (strcmp (option->arg, "jobserver") == 0)
	    {
	      parallel = 1;
	      jobserver = 1;
	    }
	  else if (strcmp (option->arg, "auto") == 0)
	    {
	      parallel = 1;
	      auto_parallel = 1;
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
	  break;
	}
    }

  /* Scan linker driver arguments for things that are of relevance to us.  */
  for (j = 1; j < decoded_options.length (); ++j)
    {
      cl_decoded_option *option = &decoded_options[j];
      switch (option->opt_index)
	{
	case OPT_o:
	  linker_output = option->arg;
	  break;

	  /* We don't have to distinguish between -save-temps=* and
	     -save-temps, -dumpdir already carries that
	     information.  */
	case OPT_save_temps_:
	case OPT_save_temps:
	  save_temps = 1;
	  break;

	case OPT_v:
	  verbose = 1;
	  break;

	case OPT_flto_partition_:
	  if (strcmp (option->arg, "none") == 0)
	    no_partition = true;
	  break;

	case OPT_flto_:
	  if (strcmp (option->arg, "jobserver") == 0)
	    jobserver_requested = true;
	  break;

	case OPT_flinker_output_:
	  linker_output_rel = !strcmp (option->arg, "rel");
	  break;

	case OPT_g:
	  /* Recognize -g0.  */
	  skip_debug = option->arg && !strcmp (option->arg, "0");
	  break;

	case OPT_dumpdir:
	  incoming_dumppfx = dumppfx = option->arg;
	  break;

	default:
	  break;
	}
    }

  /* Output lto-wrapper invocation command.  */
  if (verbose)
    {
      for (i = 0; i < argc; ++i)
	{
	  fputs (argv[i], stderr);
	  fputc (' ', stderr);
	}
      fputc ('\n', stderr);
    }

  if (linker_output_rel)
    no_partition = true;

  if (no_partition)
    {
      lto_mode = LTO_MODE_LTO;
      jobserver = 0;
      jobserver_requested = false;
      auto_parallel = 0;
      parallel = 0;
    }
  else
    {
      jobserver_error = jobserver_active_p ();
      if (jobserver && jobserver_error != NULL)
	{
	  /* Fall back to auto parallelism.  */
	  jobserver = 0;
	  auto_parallel = 1;
	}
      else if (!jobserver && jobserver_error == NULL)
	{
	  parallel = 1;
	  jobserver = 1;
	}
    }

  /* We need make working for a parallel execution.  */
  if (parallel && !make_exists ())
    parallel = 0;

  if (!dumppfx)
    {
      if (!linker_output
	  || strcmp (linker_output, HOST_BIT_BUCKET) == 0)
	dumppfx = "a.";
      else
	{
	  const char *obase = lbasename (linker_output), *temp;

	  /* Strip the executable extension.  */
	  size_t blen = strlen (obase), xlen;
	  if ((temp = strrchr (obase + 1, '.'))
	      && (xlen = strlen (temp))
	      && (strcmp (temp, ".exe") == 0
#if defined(HAVE_TARGET_EXECUTABLE_SUFFIX)
		  || strcmp (temp, TARGET_EXECUTABLE_SUFFIX) == 0
#endif
		  || strcmp (obase, "a.out") == 0))
	    dumppfx = xstrndup (linker_output,
				obase - linker_output + blen - xlen + 1);
	  else
	    dumppfx = concat (linker_output, ".", NULL);
	}
    }

  /* If there's no directory component in the dumppfx, add one, so
     that, when it is used as -dumpbase, it overrides any occurrence
     of -dumpdir that might have been passed in.  */
  if (!dumppfx || lbasename (dumppfx) == dumppfx)
    dumppfx = concat (current_dir, dumppfx, NULL);

  /* Make sure some -dumpdir is passed, so as to get predictable
     -dumpbase overriding semantics.  If we got an incoming -dumpdir
     argument, we'll pass it on, so don't bother with another one
     then.  */
  if (!incoming_dumppfx)
    {
      obstack_ptr_grow (&argv_obstack, "-dumpdir");
      obstack_ptr_grow (&argv_obstack, "");
    }
  obstack_ptr_grow (&argv_obstack, "-dumpbase");

  /* Remember at which point we can scrub args to re-use the commons.  */
  new_head_argc = obstack_object_size (&argv_obstack) / sizeof (void *);

  if (have_offload)
    {
      unsigned i, num_offload_files;
      char **offload_argv;
      FILE *f;

      f = fopen (offload_objects_file_name, "r");
      if (f == NULL)
	fatal_error (input_location, "cannot open %s: %m",
		     offload_objects_file_name);
      if (fscanf (f, "%u ", &num_offload_files) != 1)
	fatal_error (input_location, "cannot read %s: %m",
		     offload_objects_file_name);
      offload_argv = XCNEWVEC (char *, num_offload_files);

      /* Read names of object files with offload.  */
      for (i = 0; i < num_offload_files; i++)
	{
	  const unsigned piece = 32;
	  char *buf, *filename = XNEWVEC (char, piece);
	  size_t len;

	  buf = filename;
cont1:
	  if (!fgets (buf, piece, f))
	    break;
	  len = strlen (filename);
	  if (filename[len - 1] != '\n')
	    {
	      filename = XRESIZEVEC (char, filename, len + piece);
	      buf = filename + len;
	      goto cont1;
	    }
	  filename[len - 1] = '\0';
	  offload_argv[i] = filename;
	}
      fclose (f);
      if (offload_argv[num_offload_files - 1] == NULL)
	fatal_error (input_location, "invalid format of %s",
		     offload_objects_file_name);
      maybe_unlink (offload_objects_file_name);
      offload_objects_file_name = NULL;

      /* Look at saved offload options in files.  */
      for (i = 0; i < num_offload_files; i++)
	{
	  char *p;
	  long loffset;
	  int fd, consumed;
	  off_t file_offset = 0;
	  char *filename = offload_argv[i];

	  if ((p = strrchr (offload_argv[i], '@'))
	      && p != offload_argv[i]
	      && sscanf (p, "@%li%n", &loffset, &consumed) >= 1
	      && strlen (p) == (unsigned int) consumed)
	    {
	      filename = XNEWVEC (char, p - offload_argv[i] + 1);
	      memcpy (filename, offload_argv[i], p - offload_argv[i]);
	      filename[p - offload_argv[i]] = '\0';
	      file_offset = (off_t) loffset;
	    }
	  fd = open (filename, O_RDONLY | O_BINARY);
	  if (fd == -1)
	    fatal_error (input_location, "cannot open %s: %m", filename);
	  if (!find_and_merge_options (fd, file_offset,
				       OFFLOAD_SECTION_NAME_PREFIX,
				       decoded_options, &offload_fdecoded_options,
				       collect_gcc))
	    fatal_error (input_location, "cannot read %s: %m", filename);
	  close (fd);
	  if (filename != offload_argv[i])
	    XDELETEVEC (filename);
	}

      compile_images_for_offload_targets (num_offload_files, offload_argv,
					  offload_fdecoded_options, decoded_options);

      free_array_of_ptrs ((void **) offload_argv, num_offload_files);

      if (offload_names)
	{
	  find_crtoffloadtable (save_temps, dumppfx);
	  for (i = 0; offload_names[i]; i++)
	    printf ("%s\n", offload_names[i]);
	  free_array_of_ptrs ((void **) offload_names, i);
	}
    }

  /* If object files contain offload sections, but do not contain LTO sections,
     then there is no need to perform a link-time recompilation, i.e.
     lto-wrapper is used only for a compilation of offload images.  */
  if (have_offload && !have_lto)
    goto finish;

  if (lto_mode == LTO_MODE_LTO)
    {
      /* -dumpbase argument for LTO.  */
      flto_out = concat (dumppfx, "lto.o", NULL);
      obstack_ptr_grow (&argv_obstack, flto_out);

      if (!save_temps)
	flto_out = make_temp_file (".lto.o");
      obstack_ptr_grow (&argv_obstack, "-o");
      obstack_ptr_grow (&argv_obstack, flto_out);
    }
  else 
    {
      const char *list_option = "-fltrans-output-list=";

      /* -dumpbase argument for WPA.  */
      char *dumpbase = concat (dumppfx, "wpa", NULL);
      obstack_ptr_grow (&argv_obstack, dumpbase);

      if (save_temps)
	ltrans_output_file = concat (dumppfx, "ltrans.out", NULL);
      else
	ltrans_output_file = make_temp_file (".ltrans.out");
      list_option_full = concat (list_option, ltrans_output_file, NULL);
      obstack_ptr_grow (&argv_obstack, list_option_full);

      if (jobserver)
	{
	  if (verbose)
	    fprintf (stderr, "Using make jobserver\n");
	  obstack_ptr_grow (&argv_obstack, xstrdup ("-fwpa=jobserver"));
	}
      else if (auto_parallel)
	{
	  char buf[256];
	  init_num_threads ();
	  if (nthreads_var == 0)
	    nthreads_var = 1;
	  if (verbose)
	    fprintf (stderr, "LTO parallelism level set to %ld\n",
		     nthreads_var);
	  sprintf (buf, "-fwpa=%ld", nthreads_var);
	  obstack_ptr_grow (&argv_obstack, xstrdup (buf));
	}
      else if (parallel > 1)
	{
	  char buf[256];
	  sprintf (buf, "-fwpa=%i", parallel);
	  obstack_ptr_grow (&argv_obstack, xstrdup (buf));
	}
      else
        obstack_ptr_grow (&argv_obstack, "-fwpa");
    }

  /* Append input arguments.  */
  for (i = 0; i < lto_argc; ++i)
    obstack_ptr_grow (&argv_obstack, lto_argv[i]);
  /* Append the input objects.  */
  for (i = 0; i < ltoobj_argc; ++i)
    obstack_ptr_grow (&argv_obstack, ltoobj_argv[i]);
  obstack_ptr_grow (&argv_obstack, NULL);

  new_argv = XOBFINISH (&argv_obstack, const char **);
  argv_ptr = &new_argv[new_head_argc];
  fork_execute (new_argv[0], CONST_CAST (char **, new_argv), true,
		"ltrans_args");

  /* Copy the early generated debug info from the objects to temporary
     files and append those to the partial link commandline.  */
  n_debugobj = 0;
  early_debug_object_names = NULL;
  if (! skip_debug)
    {
      early_debug_object_names = XCNEWVEC (const char *, ltoobj_argc+ 1);
      num_deb_objs = ltoobj_argc;
      for (i = 0; i < ltoobj_argc; ++i)
	{
	  const char *tem;
	  if ((tem = debug_objcopy (ltoobj_argv[i], !linker_output_rel)))
	    {
	      early_debug_object_names[i] = tem;
	      n_debugobj++;
	    }
	}
    }

  if (lto_mode == LTO_MODE_LTO)
    {
      printf ("%s\n", flto_out);
      if (!skip_debug)
	{
	  for (i = 0; i < ltoobj_argc; ++i)
	    if (early_debug_object_names[i] != NULL)
	      printf ("%s\n", early_debug_object_names[i]);	      
	}
      /* These now belong to collect2.  */
      free (flto_out);
      flto_out = NULL;
      free (early_debug_object_names);
      early_debug_object_names = NULL;
    }
  else
    {
      FILE *stream = fopen (ltrans_output_file, "r");
      FILE *mstream = NULL;
      struct obstack env_obstack;
      int priority;

      if (!stream)
	fatal_error (input_location, "%<fopen%>: %s: %m", ltrans_output_file);

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
          if (fscanf (stream, "%i\n", &priority) != 1)
	    {
	      if (!feof (stream))
	        fatal_error (input_location,
		             "corrupted ltrans output file %s",
			     ltrans_output_file);
	      break;
	    }
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
	  ltrans_priorities
	     = (int *)xrealloc (ltrans_priorities, nr * sizeof (int) * 2);
	  input_names = (char **)xrealloc (input_names, nr * sizeof (char *));
	  output_names = (char **)xrealloc (output_names, nr * sizeof (char *));
	  ltrans_priorities[(nr-1)*2] = priority;
	  ltrans_priorities[(nr-1)*2+1] = nr-1;
	  input_names[nr-1] = input_name;
	  output_names[nr-1] = output_name;
	}
      fclose (stream);
      maybe_unlink (ltrans_output_file);
      ltrans_output_file = NULL;

      if (nr > 1)
	{
	  if (jobserver_requested && jobserver_error != NULL)
	    {
	      warning (0, jobserver_error);
	      print_lto_docs_link ();
	    }
	  else if (parallel == 0)
	    {
	      warning (0, "using serial compilation of %d LTRANS jobs", nr);
	      print_lto_docs_link ();
	    }
	}

      if (parallel)
	{
	  makefile = make_temp_file (".mk");
	  mstream = fopen (makefile, "w");
	  qsort (ltrans_priorities, nr, sizeof (int) * 2, cmp_priority);
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
	  int dumpbase_len = (strlen (dumppfx) + sizeof (DUMPBASE_SUFFIX));
	  char *dumpbase = (char *) xmalloc (dumpbase_len + 1);
	  snprintf (dumpbase, dumpbase_len, "%sltrans%u.ltrans", dumppfx, i);
	  argv_ptr[0] = dumpbase;

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
	      if (! save_temps)
		fprintf (mstream, "\t@-touch -r %s %s.tem > /dev/null 2>&1 "
			 "&& mv %s.tem %s\n",
			 input_name, input_name, input_name, input_name); 
	    }
	  else
	    {
	      char argsuffix[sizeof (DUMPBASE_SUFFIX) + 1];
	      if (save_temps)
		snprintf (argsuffix, sizeof (DUMPBASE_SUFFIX),
			  "ltrans%u.ltrans_args", i);
	      fork_execute (new_argv[0], CONST_CAST (char **, new_argv),
			    true, save_temps ? argsuffix : NULL);
	      maybe_unlink (input_name);
	    }

	  output_names[i] = output_name;
	}
      if (parallel)
	{
	  struct pex_obj *pex;
	  char jobs[32];

	  fprintf (mstream,
		   ".PHONY: all\n"
		   "all:");
	  for (i = 0; i < nr; ++i)
	    {
	      int j = ltrans_priorities[i*2 + 1];
	      fprintf (mstream, " \\\n\t%s", output_names[j]);
	    }
	  fprintf (mstream, "\n");
	  fclose (mstream);
	  if (!jobserver)
	    {
	      /* Avoid passing --jobserver-fd= and similar flags 
		 unless jobserver mode is explicitly enabled.  */
	      putenv (xstrdup ("MAKEFLAGS="));
	      putenv (xstrdup ("MFLAGS="));
	    }

	  char **make_argv = buildargv (getenv ("MAKE"));
	  if (make_argv)
	    {
	      for (unsigned argc = 0; make_argv[argc]; argc++)
		obstack_ptr_grow (&argv_obstack, make_argv[argc]);
	    }
	  else
	    obstack_ptr_grow (&argv_obstack, "make");

	  obstack_ptr_grow (&argv_obstack, "-f");
	  obstack_ptr_grow (&argv_obstack, makefile);
	  if (!jobserver)
	    {
	      snprintf (jobs, 31, "-j%ld",
			auto_parallel ? nthreads_var : parallel);
	      obstack_ptr_grow (&argv_obstack, jobs);
	    }
	  obstack_ptr_grow (&argv_obstack, "all");
	  obstack_ptr_grow (&argv_obstack, NULL);
	  new_argv = XOBFINISH (&argv_obstack, const char **);

	  pex = collect_execute (new_argv[0], CONST_CAST (char **, new_argv),
				 NULL, NULL, PEX_SEARCH, false, NULL);
	  do_wait (new_argv[0], pex);
	  freeargv (make_argv);
	  maybe_unlink (makefile);
	  makefile = NULL;
	  for (i = 0; i < nr; ++i)
	    maybe_unlink (input_names[i]);
	}
      for (i = 0; i < nr; ++i)
	{
	  fputs (output_names[i], stdout);
	  putc ('\n', stdout);
	  free (input_names[i]);
	}
      if (!skip_debug)
	{
	  for (i = 0; i < ltoobj_argc; ++i)
	    if (early_debug_object_names[i] != NULL)
	      printf ("%s\n", early_debug_object_names[i]);	      
	}
      nr = 0;
      free (ltrans_priorities);
      free (output_names);
      output_names = NULL;
      free (early_debug_object_names);
      early_debug_object_names = NULL;
      free (input_names);
      free (list_option_full);
      obstack_free (&env_obstack, NULL);
    }

 finish:
  XDELETE (lto_argv);
  obstack_free (&argv_obstack, NULL);
}


/* Entry point.  */

int
main (int argc, char *argv[])
{
  const char *p;

  init_opts_obstack ();

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  gcc_init_libintl ();

  diagnostic_initialize (global_dc, 0);

  if (atexit (lto_wrapper_cleanup) != 0)
    fatal_error (input_location, "%<atexit%> failed");

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
