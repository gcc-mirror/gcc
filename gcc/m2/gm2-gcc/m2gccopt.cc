/* m2gccopt.cc provides access to the gcc options.

Copyright (C) 2026 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#include "../gm2-lang.h"
#include "../m2-tree.h"

#include "m2assert.h"
#include "m2block.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2tree.h"
#include "m2type.h"
#define m2gccopt_c
#include "m2gccopt.h"

static cl_decoded_option *decoded_options;
static struct gcc_options local_options;
static struct gcc_options local_options_set;
static unsigned int decoded_count;


/* Decode front end command-line style of arguments
   from an argc/argv parameter list into the decoded_options array.  */

unsigned int
m2gccopt_DecodeArgv (int argc, const char **argv)
{
  decoded_count = 0;
  init_options_struct (&local_options, &local_options_set);
  decode_cmdline_options_to_array (argc, argv,
				   CL_DRIVER,
				   &decoded_options, &decoded_count);
  return decoded_count;
}


/* SetOption attempt to set option name.  Return true if the option was
   known and handled by gcc.  */

bool
m2gccopt_SetOption (const char *name)
{
  const char *argv[3];

  argv[0] = (const char *) "gm2";
  argv[1] = (const char *) name;
  argv[2] = NULL;
  return m2gccopt_DecodeArgv (2, argv) == 2;
}
