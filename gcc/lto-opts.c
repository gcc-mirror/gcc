/* LTO IL options.

   Copyright 2009, 2010, 2011, 2012 Free Software Foundation, Inc.
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
#include "tree.h"
#include "hashtab.h"
#include "ggc.h"
#include "vec.h"
#include "bitmap.h"
#include "flags.h"
#include "opts.h"
#include "options.h"
#include "common/common-target.h"
#include "diagnostic.h"
#include "lto-streamer.h"
#include "toplev.h"

/* Write currently held options to an LTO IL section.  */

void
lto_write_options (void)
{
  struct lto_output_stream stream;
  char *section_name;
  struct obstack temporary_obstack;
  unsigned int i, j;
  char *args;

  section_name = lto_get_section_name (LTO_section_opts, NULL, NULL);
  lto_begin_section (section_name, false);
  memset (&stream, 0, sizeof (stream));

  obstack_init (&temporary_obstack);
  for (i = 1; i < save_decoded_options_count; ++i)
    {
      struct cl_decoded_option *option = &save_decoded_options[i];
      const char *q, *p;

      /* Skip frontend and driver specific options here.  */
      if (!(cl_options[option->opt_index].flags & (CL_COMMON|CL_TARGET|CL_LTO)))
	continue;

      /* Drop options created from the gcc driver that will be rejected
	 when passed on to the driver again.  */
      if (cl_options[option->opt_index].cl_reject_driver)
	continue;

      /* Also drop all options that are handled by the driver as well,
         which includes things like -o and -v or -fhelp for example.
	 We do not need those.  Also drop all diagnostic options.  */
      if (cl_options[option->opt_index].flags & (CL_DRIVER|CL_WARNING))
	continue;

      /* Skip explicitly some common options that we do not need.  */
      switch (option->opt_index)
	{
	case OPT_dumpbase:
	case OPT_SPECIAL_input_file:
	  continue;

	default:
	  break;
	}

      if (i != 1)
	obstack_grow (&temporary_obstack, " ", 1);
      obstack_grow (&temporary_obstack, "'", 1);
      q = option->canonical_option[0];
      while ((p = strchr (q, '\'')))
	{
	  obstack_grow (&temporary_obstack, q, p - q);
	  obstack_grow (&temporary_obstack, "'\\''", 4);
	  q = ++p;
	}
      obstack_grow (&temporary_obstack, q, strlen (q));
      obstack_grow (&temporary_obstack, "'", 1);

      for (j = 1; j < option->canonical_option_num_elements; ++j)
	{
	  obstack_grow (&temporary_obstack, " '", 2);
	  q = option->canonical_option[j];
	  while ((p = strchr (q, '\'')))
	    {
	      obstack_grow (&temporary_obstack, q, p - q);
	      obstack_grow (&temporary_obstack, "'\\''", 4);
	      q = ++p;
	    }
	  obstack_grow (&temporary_obstack, q, strlen (q));
	  obstack_grow (&temporary_obstack, "'", 1);
	}
    }
  obstack_grow (&temporary_obstack, "\0", 1);
  args = XOBFINISH (&temporary_obstack, char *);
  lto_output_data_stream (&stream, args, strlen (args) + 1);

  lto_write_stream (&stream);
  lto_end_section ();

  obstack_free (&temporary_obstack, NULL);
  free (section_name);
}
