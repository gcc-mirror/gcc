/* C++ modules.  Experimental!

   Copyright (C) 2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"

static module_name_t GTY(()) *declared_module;
static location_t module_location;

/* Declare the name of the current module to be NAME.  We do not know
   at this point whether we're the interface TU or just one of the
   regular members of the module.  */

void
declare_module_name (location_t loc, module_name_t *name)
{
  if (declared_module)
    {
      error_at (loc, "module already declared");
      inform (module_location, "existing declaration");
    }
  else
    {
      declared_module = name;
      module_location = loc;
    }
}
