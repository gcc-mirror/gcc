/* Specific flags and argument handling of the C front-end.
   Copyright (C) 1999 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"

/* Filter argc and argv before processing by the gcc driver proper. */
void
lang_specific_driver (fn, in_argc, in_argv, in_added_libraries)
     void (*fn)() ATTRIBUTE_UNUSED;
     int *in_argc ATTRIBUTE_UNUSED;
     char ***in_argv ATTRIBUTE_UNUSED;
     int *in_added_libraries ATTRIBUTE_UNUSED;
{
  return;  /* Not used for C. */
}

/* Called before linking.  Returns 0 on success and -1 on failure. */
int
lang_specific_pre_link ()
{
  return 0;  /* Not used for C. */
}

/* Number of extra output files that lang_specific_pre_link may generate. */
int lang_specific_extra_outfiles = 0;  /* Not used for C. */
