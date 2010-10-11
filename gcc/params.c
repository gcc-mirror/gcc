/* params.c - Run-time parameters.
   Copyright (C) 2001, 2003, 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>.

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
#include "tm.h"
#include "params.h"
#include "diagnostic-core.h"
#include "toplev.h"

/* An array containing the compiler parameters and their current
   values.  */

param_info *compiler_params;

/* The number of entries in the table.  */
static size_t num_compiler_params;

/* Add the N PARAMS to the current list of compiler parameters.  */

void
add_params (const param_info params[], size_t n)
{
  /* Allocate enough space for the new parameters.  */
  compiler_params = XRESIZEVEC (param_info, compiler_params,
				num_compiler_params + n);
  /* Copy them into the table.  */
  memcpy (compiler_params + num_compiler_params,
	  params,
	  n * sizeof (param_info));
  /* Keep track of how many parameters we have.  */
  num_compiler_params += n;
}

/* Set the value of the parameter given by NUM to VALUE.  If
   EXPLICIT_P, this is being set by the user; otherwise it is being
   set implicitly by the compiler.  */

static void
set_param_value_internal (compiler_param num, int value,
			  bool explicit_p)
{
  size_t i = (size_t) num;

  compiler_params[i].value = value;
  if (explicit_p)
    compiler_params[i].set = true;
}

/* Set the VALUE associated with the parameter given by NAME.  */

void
set_param_value (const char *name, int value)
{
  size_t i;

  /* Make sure nobody tries to set a parameter to an invalid value.  */
  gcc_assert (value != INVALID_PARAM_VAL);

  /* Scan the parameter table to find a matching entry.  */
  for (i = 0; i < num_compiler_params; ++i)
    if (strcmp (compiler_params[i].option, name) == 0)
      {
	if (value < compiler_params[i].min_value)
	  error ("minimum value of parameter %qs is %u",
		 compiler_params[i].option,
		 compiler_params[i].min_value);
	else if (compiler_params[i].max_value > compiler_params[i].min_value
		 && value > compiler_params[i].max_value)
	  error ("maximum value of parameter %qs is %u",
		 compiler_params[i].option,
		 compiler_params[i].max_value);
	else
	  set_param_value_internal ((compiler_param) i, value, true);
	return;
      }

  /* If we didn't find this parameter, issue an error message.  */
  error ("invalid parameter %qs", name);
}

/* Set the value of the parameter given by NUM to VALUE, implicitly,
   if it has not been set explicitly by the user.  */

void
maybe_set_param_value (compiler_param num, int value)
{
  if (!PARAM_SET_P (num))
    set_param_value_internal (num, value, false);
}

/* Set the default value of a parameter given by NUM to VALUE, before
   option processing.  */

void
set_default_param_value (compiler_param num, int value)
{
  gcc_assert (!PARAM_SET_P (num));
  set_param_value_internal (num, value, false);
}

/* Return the current value of num_compiler_params, for the benefit of
   plugins that use parameters as features.  */

size_t
get_num_compiler_params (void)
{
  return num_compiler_params;
}
