/* Command line option handling.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.

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

#ifndef GCC_OPTS_H
#define GCC_OPTS_H

extern void decode_options (unsigned int argc, const char **argv);

struct cl_option
{
  const char *opt_text;
  const char *help;
  unsigned short back_chain;
  unsigned char opt_len;
  unsigned int flags;
  int *flag_var;
  int has_set_value;
  int set_value;
};

extern const struct cl_option cl_options[];
extern const unsigned int cl_options_count;
extern const char *const lang_names[];

#define CL_REPORT		(1 << 23) /* Report argument with -fverbose-asm  */
#define CL_JOINED		(1 << 24) /* If takes joined argument.  */
#define CL_SEPARATE		(1 << 25) /* If takes a separate argument.  */
#define CL_REJECT_NEGATIVE	(1 << 26) /* Reject no- form.  */
#define CL_MISSING_OK		(1 << 27) /* Missing argument OK (joined).  */
#define CL_UINTEGER		(1 << 28) /* Argument is an integer >=0.  */
#define CL_COMMON		(1 << 29) /* Language-independent.  */
#define CL_UNDOCUMENTED		(1 << 30) /* Do not output with --help.  */

/* Input file names.  */

extern const char **in_fnames;

/* The count of input filenames.  */

extern unsigned num_in_fnames;

#endif
