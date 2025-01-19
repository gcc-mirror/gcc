/* A self-testing framework, for use by -fself-test.
   Copyright (C) 2015-2025 Free Software Foundation, Inc.

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

#ifndef GCC_SELFTEST_TREE_H
#define GCC_SELFTEST_TREE_H

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace selftest {

/* Helper function for selftests that need a function decl.  */

extern tree make_fndecl (tree return_type,
			 const char *name,
			 vec <tree> &param_types,
			 bool is_variadic = false);

} /* end of namespace selftest.  */

#endif /* #if CHECKING_P */

#endif /* GCC_SELFTEST_TREE_H */
