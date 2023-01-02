/* Copyright (C) 2022-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "opts.h"

/* Perform early driver flags initializations that can't be achieved
   with specs.  In particular, we need to explicitly request a static
   link for rtps by default before lang_specific_driver gets control.  */

void vxworks_driver_init (unsigned int *in_decoded_options_count,
			  struct cl_decoded_option **in_decoded_options)
{
  unsigned int i;
  struct cl_decoded_option *decoded_options = *in_decoded_options;

  /* Arrange to add -static if we are going to link a rtp and there is no
     trace of any explicit request for a specific kind of link.  */
  bool wont_link = false;
  bool mrtp = false;
  bool link_kind_indication = false;

  /* The new argument list will be contained in this.  */
  struct cl_decoded_option *new_decoded_options;
  unsigned int num_options = *in_decoded_options_count;

  for (i = 1; i < num_options; i++)
    {
      if (decoded_options[i].errors & CL_ERR_MISSING_ARG)
	continue;

      switch (decoded_options[i].opt_index)
	{
	case OPT_static:
	case OPT_shared:
	case OPT_Bdynamic:
	case OPT_Bstatic:
	case OPT_non_static:
	  link_kind_indication = true;
	  break;

	case OPT_c:
	case OPT_r:
	case OPT_S:
	case OPT_E:
	case OPT_M:
	case OPT_MM:
	case OPT_fsyntax_only:
	  wont_link = true;
	  break;

	case OPT_mrtp:
	  mrtp = true;
	  break;

	default:
	  break;
      }
    }

  if (!wont_link && mrtp && !link_kind_indication)
    {
      num_options++;
      new_decoded_options = XNEWVEC(struct cl_decoded_option, num_options);

      for (i = 0; i < num_options - 1; i++)
	new_decoded_options[i] = decoded_options[i];

      generate_option(OPT_static, NULL, 1, CL_DRIVER,
		      &new_decoded_options[num_options - 1]);

      *in_decoded_options = new_decoded_options;
      *in_decoded_options_count = num_options;
    }
}
