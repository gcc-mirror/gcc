/* Declarations for working with name_hint instances in the C++ frontend.
   Copyright (C) 2018-2019 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_CP_NAME_HINT_H
#define GCC_CP_NAME_HINT_H

/* class name_hint is declared in c-family/name-hint.h, but due
   to issues described in that header, we have to jump through some
   #define hoops to be able to include it.

   This header (cp/cp-name-hint.h) exists to limit the C++ frontend's
   exposure to the issue.  */

#include "c-family/name-hint.h"

extern name_hint suggest_alternatives_for (location_t, tree, bool);
extern name_hint suggest_alternatives_in_other_namespaces (location_t, tree);
extern name_hint suggest_alternative_in_explicit_scope (location_t, tree, tree);
extern name_hint suggest_alternative_in_scoped_enum (tree, tree);

#endif /* GCC_CP_NAME_HINT_H */
