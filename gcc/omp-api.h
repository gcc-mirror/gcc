/* Functions for querying whether a function name is reserved by the
   OpenMP API.  This is used for error checking.

   Copyright (C) 2023 Free Software Foundation, Inc.

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

#ifndef GCC_OMP_API_H
#define GCC_OMP_API_H

#include "coretypes.h"

/* In omp-general.cc, but declared in a separate header file for
   convenience of the Fortran front end.  */
extern bool omp_runtime_api_procname (const char *name);
extern bool omp_runtime_api_call (const_tree fndecl);

#endif
