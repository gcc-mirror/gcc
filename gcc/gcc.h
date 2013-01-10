/* Header file for modules that link with gcc.c
   Copyright (C) 1999-2013 Free Software Foundation, Inc.

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

#ifndef GCC_GCC_H
#define GCC_GCC_H

#include "version.h"
#include "diagnostic-core.h"

/* The mapping of a spec function name to the C function that
   implements it.  */
struct spec_function
{
  const char *name;
  const char *(*func) (int, const char **);
};

/* These are exported by gcc.c.  */
extern int do_spec (const char *);
extern void record_temp_file (const char *, int, int);
extern void pfatal_with_name (const char *) ATTRIBUTE_NORETURN;
extern void set_input (const char *);

/* Spec files linked with gcc.c must provide definitions for these.  */

/* Called before processing to change/add/remove arguments.  */
extern void lang_specific_driver (struct cl_decoded_option **,
				  unsigned int *, int *);

/* Called before linking.  Returns 0 on success and -1 on failure.  */
extern int lang_specific_pre_link (void);

extern int n_infiles;

/* Number of extra output files that lang_specific_pre_link may generate.  */
extern int lang_specific_extra_outfiles;

/* A vector of corresponding output files is made up later.  */

extern const char **outfiles;

#endif /* ! GCC_GCC_H */
