/* C++ modules.  Experimental!	-*- c++ -*-
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
   Written by Nathan Sidwell <nathan@acm.org> while at FaceBook

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

/* Forward to the resolver in c++tools.  */

#include "config.h"
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#define INCLUDE_ALGORITHM
#define INCLUDE_MAP
#include "system.h"

// We don't want or need to be aware of networking
#define CODY_NETWORKING 0
#include "../../c++tools/resolver.cc"
