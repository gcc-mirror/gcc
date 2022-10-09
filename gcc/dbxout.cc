/* Copyright (C) 1987-2022 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"
#include "dbxout.h"

/* Record an element in the table of global destructors.  SYMBOL is
   a SYMBOL_REF of the function to be called; PRIORITY is a number
   between 0 and MAX_INIT_PRIORITY.  */

void
default_stabs_asm_out_destructor (rtx symbol ATTRIBUTE_UNUSED,
				  int priority ATTRIBUTE_UNUSED)
{
  sorry ("global destructors not supported on this target");
}

/* Likewise for global constructors.  */

void
default_stabs_asm_out_constructor (rtx symbol ATTRIBUTE_UNUSED,
				   int priority ATTRIBUTE_UNUSED)
{
  sorry ("global constructors not supported on this target");
}
