/* Header file for modules that link with gcc.c
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_GCC_H
#define GCC_GCC_H

#include "version.h"

/* These are exported by gcc.c.  */
extern int do_spec PARAMS ((const char *));
extern void record_temp_file PARAMS ((const char *, int, int));
extern void fancy_abort PARAMS ((void)) ATTRIBUTE_NORETURN;
extern const char *input_filename;
extern size_t input_filename_length;
extern void fatal PARAMS ((const char *, ...))
     ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
extern void error PARAMS ((const char *, ...)) ATTRIBUTE_PRINTF_1;
extern void pfatal_with_name PARAMS ((const char *)) ATTRIBUTE_NORETURN;
extern void set_input PARAMS ((const char *));

/* Spec files linked with gcc.c must provide definitions for these.  */

/* Called before processing to change/add/remove arguments.  */
extern void lang_specific_driver PARAMS ((int *, const char *const **, int *));

/* Called before linking.  Returns 0 on success and -1 on failure.  */
extern int lang_specific_pre_link PARAMS ((void));

extern int n_infiles;

/* Number of extra output files that lang_specific_pre_link may generate.  */
extern int lang_specific_extra_outfiles;

/* A vector of corresponding output files is made up later.  */

extern const char **outfiles;

#endif /* ! GCC_GCC_H */
