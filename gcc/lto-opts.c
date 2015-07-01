/* LTO IL options.

   Copyright (C) 2009-2015 Free Software Foundation, Inc.
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
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "tree.h"
#include "fold-const.h"
#include "predict.h"
#include "tm.h"
#include "hard-reg-set.h"
#include "function.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "bitmap.h"
#include "flags.h"
#include "opts.h"
#include "options.h"
#include "common/common-target.h"
#include "diagnostic.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "lto-section-names.h"
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

  section_name = lto_get_section_name (LTO_section_opts, NULL, NULL);
  lto_begin_section (section_name, false);

  obstack_init (&temporary_obstack);

  /* Output options that affect GIMPLE IL semantics and are implicitly
     enabled by the frontend.
     This for now includes an explicit set of options that we also handle
     explicitly in lto-wrapper.c.  In the end the effects on GIMPLE IL
     semantics should be explicitely encoded in the IL or saved per
     function rather than per compilation unit.  */
  /* -fexceptions causes the EH machinery to be initialized, enabling
     generation of unwind data so that explicit throw() calls work.  */
  if (!global_options_set.x_flag_exceptions
      && global_options.x_flag_exceptions)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
				   "-fexceptions");
  /* -fnon-call-exceptions changes the generation of exception
      regions.  It is enabled implicitly by the Go frontend.  */
  if (!global_options_set.x_flag_non_call_exceptions
      && global_options.x_flag_non_call_exceptions)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
				   "-fnon-call-exceptions");
  /* The default -ffp-contract changes depending on the language
     standard.  Pass thru conservative standard settings.  */
  if (!global_options_set.x_flag_fp_contract_mode)
    switch (global_options.x_flag_fp_contract_mode)
      {
      case FP_CONTRACT_OFF:
	append_to_collect_gcc_options (&temporary_obstack, &first_p,
				       "-ffp-contract=off");
	break;
      case FP_CONTRACT_ON:
	append_to_collect_gcc_options (&temporary_obstack, &first_p,
				       "-ffp-contract=on");
	break;
      case FP_CONTRACT_FAST:
	/* Nothing.  That merges conservatively and is the default for LTO.  */
	break;
      default:
	gcc_unreachable ();
      }
  /* The default -fmath-errno, -fsigned-zeros and -ftrapping-math change
     depending on the language (they can be disabled by the Ada and Java
     front-ends).  Pass thru conservative standard settings.  */
  if (!global_options_set.x_flag_errno_math)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
				   global_options.x_flag_errno_math
				   ? "-fmath-errno"
				   : "-fno-math-errno");
  if (!global_options_set.x_flag_signed_zeros)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
				   global_options.x_flag_signed_zeros
				   ? "-fsigned-zeros"
				   : "-fno-signed-zeros");
  if (!global_options_set.x_flag_trapping_math)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
				   global_options.x_flag_trapping_math
				   ? "-ftrapping-math"
				   : "-fno-trapping-math");
  /* We need to merge -f[no-]strict-overflow, -f[no-]wrapv and -f[no-]trapv
     conservatively, so stream out their defaults.  */
  if (!global_options_set.x_flag_wrapv
      && global_options.x_flag_wrapv)
    append_to_collect_gcc_options (&temporary_obstack, &first_p, "-fwrapv");
  if (!global_options_set.x_flag_trapv
      && !global_options.x_flag_trapv)
    append_to_collect_gcc_options (&temporary_obstack, &first_p, "-fno-trapv");
  if (!global_options_set.x_flag_strict_overflow
      && !global_options.x_flag_strict_overflow)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
			       "-fno-strict-overflow");

  if (!global_options_set.x_flag_openmp
      && !global_options.x_flag_openmp)
    append_to_collect_gcc_options (&temporary_obstack, &first_p, "-fno-openmp");
  if (!global_options_set.x_flag_openacc
      && !global_options.x_flag_openacc)
    append_to_collect_gcc_options (&temporary_obstack, &first_p,
				   "-fno-openacc");

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
	case OPT_SPECIAL_program_name:
	case OPT_SPECIAL_input_file:
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
	  && (!lto_stream_offload_p || option->opt_index != OPT_foffload_))
	continue;

      for (j = 0; j < option->canonical_option_num_elements; ++j)
	append_to_collect_gcc_options (&temporary_obstack, &first_p,
				       option->canonical_option[j]);
    }
  obstack_grow (&temporary_obstack, "\0", 1);
  args = XOBFINISH (&temporary_obstack, char *);
  lto_write_data (args, strlen (args) + 1);
  lto_end_section ();

  obstack_free (&temporary_obstack, NULL);
  free (section_name);
}
