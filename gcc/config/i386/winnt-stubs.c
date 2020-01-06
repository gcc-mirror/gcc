/* Dummy subroutines for language-specific support on Windows.
   Contributed by Danny Smith (dannysmith@users.sourceforge.net)
   Copyright (C) 2005-2020 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"

bool
i386_pe_type_dllimport_p (tree)
{
  return false;
}


bool
i386_pe_type_dllexport_p (tree)
{
  return false;
}


void
i386_pe_adjust_class_at_definition (tree)
{ }
