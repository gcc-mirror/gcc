/* Dummy flag and argument handling of the jit "front-end".
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

/* Filter command line before processing by the gcc driver proper.  */
void
lang_specific_driver (struct cl_decoded_option **in_decoded_options ATTRIBUTE_UNUSED,
		      unsigned int *in_decoded_options_count ATTRIBUTE_UNUSED,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  /* Not used for jit.  */
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)
{
  return 0;  /* Not used for jit.  */
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;  /* Not used for jit.  */
