/* Specific flags and argument handling of the C preprocessor.
   Copyright (C) 1999-2014 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "tm.h"
#include "gcc.h"
#include "opts.h"

/* The `cpp' executable installed in $(bindir) and $(cpp_install_dir)
   is a customized version of the gcc driver.  It forces -E; -S and -c
   are errors.  It defaults to -x c for files with unrecognized
   extensions, unless -x options appear in argv, in which case we
   assume the user knows what they're doing.  If no explicit input is
   mentioned, it will read stdin.  */

/* Suffixes for known sorts of input files.  Note that we do not list
   files which are normally considered to have been preprocessed already,
   since the user's expectation is that `cpp' always preprocesses.  */
static const char *const known_suffixes[] =
{
  ".c",  ".C",   ".S",   ".m",
  ".cc", ".cxx", ".cpp", ".cp",  ".c++",
  ".sx",
  NULL
};

/* Filter the command line before processing by the gcc driver proper.  */
void
lang_specific_driver (struct cl_decoded_option **in_decoded_options,
		      unsigned int *in_decoded_options_count,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  struct cl_decoded_option *decoded_options = *in_decoded_options;
  unsigned int argc = *in_decoded_options_count;

  /* Do we need to read stdin? */
  int read_stdin = 1;

  /* Do we need to insert -E? */
  int need_E = 1;

  /* Have we seen an input file? */
  int seen_input = 0;

  /* Positions to insert -xc, -xassembler-with-cpp, and -o, if necessary.
     0 means unnecessary.  */
  unsigned int lang_c_here = 0;
  unsigned int lang_S_here = 0;
  unsigned int o_here = 0;

  /* Do we need to fix up an input file with an unrecognized suffix? */
  int need_fixups = 1;

  unsigned int i, j;
  struct cl_decoded_option *new_decoded_options;
  unsigned int new_argc;
  extern int is_cpp_driver;

  is_cpp_driver = 1;

  /* First pass.  If we see an -S or -c, barf.  If we see an input file,
     turn off read_stdin.  If we see a second input file, it is actually
     the output file.  If we see a third input file, barf.  */
  for (i = 1; i < argc; i++)
    {
      switch (decoded_options[i].opt_index)
	{
	case OPT_E:
	  need_E = 0;
	  break;

	case OPT_S:
	case OPT_c:
	  fatal_error ("%qs is not a valid option to the preprocessor",
		       decoded_options[i].orig_option_with_args_text);
	  return;

	case OPT_x:
	  need_fixups = 0;
	  break;

	case OPT_SPECIAL_input_file:
	  {
	    const char *file = decoded_options[i].arg;

	    if (strcmp (file, "-") == 0)
	      read_stdin = 0;
	    else
	      {
		seen_input++;
		if (seen_input == 3)
		  {
		    fatal_error ("too many input files");
		    return;
		  }
		else if (seen_input == 2)
		  {
		    o_here = i;
		  }
		else
		  {
		    read_stdin = 0;
		    if (need_fixups)
		      {
			int l = strlen (file);
			int known = 0;
			const char *const *suff;

			for (suff = known_suffixes; *suff; suff++)
			  if (!strcmp (*suff, &file[l - strlen(*suff)]))
			    {
			      known = 1;
			      break;
			    }

			if (! known)
			  {
			    /* .s files are a special case; we have to
			       treat them like .S files so
			       -D__ASSEMBLER__ will be in effect.  */
			    if (!strcmp (".s", &file[l - 2]))
			      lang_S_here = i;
			    else
			      lang_c_here = i;
			  }
		      }
		  }
	      }
	  }
	  break;
	}
    }

  /* If we don't need to edit the command line, we can bail early.  */

  new_argc = argc + need_E + read_stdin + !!lang_c_here + !!lang_S_here;

  if (new_argc == argc && !o_here)
    return;

  new_decoded_options = XNEWVEC (struct cl_decoded_option, new_argc);

  new_decoded_options[0] = decoded_options[0];
  j = 1;

  if (need_E)
    generate_option (OPT_E, NULL, 1, CL_DRIVER, &new_decoded_options[j++]);

  for (i = 1; i < argc; i++, j++)
    {
      if (i == lang_c_here)
	generate_option (OPT_x, "c", 1, CL_DRIVER, &new_decoded_options[j++]);
      else if (i == lang_S_here)
	generate_option (OPT_x, "assembler-with-cpp", 1, CL_DRIVER,
			 &new_decoded_options[j++]);
      else if (i == o_here)
	{
	  generate_option (OPT_o, decoded_options[i].arg, 1, CL_DRIVER,
			   &new_decoded_options[j]);
	  continue;
	}

      new_decoded_options[j] = decoded_options[i];
    }

  if (read_stdin)
    generate_option_input_file ("-", &new_decoded_options[j++]);

  *in_decoded_options_count = new_argc;
  *in_decoded_options = new_decoded_options;
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int lang_specific_pre_link (void)
{
  return 0;  /* Not used for cpp.  */
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;  /* Not used for cpp.  */
