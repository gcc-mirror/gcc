/* LTO IL options.

   Copyright (C) 2009-2021 Free Software Foundation, Inc.
   Contributed by Simon Baldwin <simonb@google.com>

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
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "opts.h"
#include "toplev.h"

/* Append the option piece OPT to the COLLECT_GCC_OPTIONS string
   set up by OB, appropriately quoted and separated by spaces
   (if !*FIRST_P).  */

static void
append_to_collect_gcc_options (struct obstack *ob,
			       bool *first_p, const char *opt)
{
  const char *p, *q = opt;
  if (!*first_p)
    obstack_grow (ob, " ", 1);
  obstack_grow (ob, "'", 1);
  while ((p = strchr (q, '\'')))
    {
      obstack_grow (ob, q, p - q);
      obstack_grow (ob, "'\\''", 4);
      q = ++p;
    }
  obstack_grow (ob, q, strlen (q));
  obstack_grow (ob, "'", 1);
  *first_p = false;
}

/* Write currently held options to an LTO IL section.  */

void
lto_write_options (void)
{
  char *section_name;
  struct obstack temporary_obstack;
  unsigned int i, j;
  char *args;
  bool first_p = true;

  section_name = lto_get_section_name (LTO_section_opts, NULL, 0, NULL);
  lto_begin_section (section_name, false);

  obstack_init (&temporary_obstack);

  if (!OPTION_SET_P (flag_openmp)
      && !global_options.x_flag_openmp)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
				   "-fno-openmp");
  if (!OPTION_SET_P (flag_openacc)
      && !global_options.x_flag_openacc)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
				   "-fno-openacc");
  /* Append PIC/PIE mode because its default depends on target and it is
     subject of merging in lto-wrapper.  */
  if (!OPTION_SET_P (flag_pic) && !OPTION_SET_P (flag_pie))
    {
       append_to_collect_gcc_options (&temporary_obstack, &first_p,
				      global_options.x_flag_pic == 2
				      ? "-fPIC"
				      : global_options.x_flag_pic == 1
				      ? "-fpic"
				      : global_options.x_flag_pie == 2
				      ? "-fPIE"
				      : global_options.x_flag_pie == 1
				      ? "-fpie"
				      : "-fno-pie");
    }

  if (!OPTION_SET_P (flag_cf_protection))
    {
      append_to_collect_gcc_options (
	&temporary_obstack, &first_p,
	global_options.x_flag_cf_protection == CF_NONE
	? "-fcf-protection=none"
	: global_options.x_flag_cf_protection == CF_FULL
	? "-fcf-protection=full"
	: global_options.x_flag_cf_protection == CF_BRANCH
	? "-fcf-protection=branch"
	: global_options.x_flag_cf_protection == CF_RETURN
	? "-fcf-protection=return"
	: "");
    }

  /* If debug info is enabled append -g.  */
  if (debug_info_level > DINFO_LEVEL_NONE)
    append_to_collect_gcc_options (&temporary_obstack, &first_p, "-g");

  /* Append options from target hook and store them to offload_lto section.  */
  if (lto_stream_offload_p)
    {
      char *offload_opts = targetm.offload_options ();
      char *offload_ptr = offload_opts;
      while (offload_ptr)
       {
	 char *next = strchr (offload_ptr, ' ');
	 if (next)
	   *next++ = '\0';
	 append_to_collect_gcc_options (&temporary_obstack, &first_p,
					offload_ptr);
	 offload_ptr = next;
       }
      free (offload_opts);
    }

  /* Output explicitly passed options.  */
  for (i = 1; i < save_decoded_options_count; ++i)
    {
      struct cl_decoded_option *option = &save_decoded_options[i];

      /* Skip explicitly some common options that we do not need.  */
      switch (option->opt_index)
      {
	case OPT_dumpbase:
	case OPT_SPECIAL_unknown:
	case OPT_SPECIAL_ignore:
	case OPT_SPECIAL_warn_removed:
	case OPT_SPECIAL_program_name:
	case OPT_SPECIAL_input_file:
	case OPT_dumpdir:
	case OPT_fresolution_:
	case OPT_fdebug_prefix_map_:
	case OPT_ffile_prefix_map_:
	case OPT_fmacro_prefix_map_:
	  continue;

	default:
	  break;
      }

      /* Skip frontend and driver specific options here.  */
      if (!(cl_options[option->opt_index].flags & (CL_COMMON|CL_TARGET|CL_LTO)))
	continue;

      /* Do not store target-specific options in offload_lto section.  */
      if ((cl_options[option->opt_index].flags & CL_TARGET)
	  && lto_stream_offload_p)
       continue;

      /* Drop options created from the gcc driver that will be rejected
	 when passed on to the driver again.  */
      if (cl_options[option->opt_index].cl_reject_driver)
	continue;

      /* Also drop all options that are handled by the driver as well,
	 which includes things like -o and -v or -fhelp for example.
	 We do not need those.  The only exception is -foffload option, if we
	 write it in offload_lto section.  Also drop all diagnostic options.  */
      if ((cl_options[option->opt_index].flags & (CL_DRIVER|CL_WARNING))
	  && (!lto_stream_offload_p
	      || option->opt_index != OPT_foffload_options_))
	continue;

      for (j = 0; j < option->canonical_option_num_elements; ++j)
	append_to_collect_gcc_options (&temporary_obstack, &first_p,
				       option->canonical_option[j]);
    }

  const char *collect_as_options = getenv ("COLLECT_AS_OPTIONS");
  if (collect_as_options)
    prepend_xassembler_to_collect_as_options (collect_as_options,
					      &temporary_obstack);

  obstack_grow (&temporary_obstack, "\0", 1);
  args = XOBFINISH (&temporary_obstack, char *);
  lto_write_data (args, strlen (args) + 1);
  lto_end_section ();

  obstack_free (&temporary_obstack, NULL);
  free (section_name);
}
