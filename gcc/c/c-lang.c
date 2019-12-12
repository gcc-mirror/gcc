/* Language-specific hook definitions for C front end.
   Copyright (C) 1991-2019 Free Software Foundation, Inc.

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
#include "c-tree.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "c-objc-common.h"

enum c_language_kind c_language = clk_c;

/* Lang hooks common to C and ObjC are declared in c-objc-common.h;
   consequently, there should be very few hooks below.  */

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU C"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT c_objc_common_init
#undef LANG_HOOKS_INIT_TS
#define LANG_HOOKS_INIT_TS c_common_init_ts

#if CHECKING_P
#undef LANG_HOOKS_RUN_LANG_SELFTESTS
#define LANG_HOOKS_RUN_LANG_SELFTESTS selftest::run_c_tests
#endif /* #if CHECKING_P */

#undef LANG_HOOKS_GET_SUBSTRING_LOCATION
#define LANG_HOOKS_GET_SUBSTRING_LOCATION c_get_substring_location

/* Each front end provides its own lang hook initializer.  */
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#if CHECKING_P

namespace selftest {

/* Implementation of LANG_HOOKS_RUN_LANG_SELFTESTS for the C frontend.  */

void
run_c_tests (void)
{
  /* Run selftests shared within the C family.  */
  c_family_tests ();

  /* Additional C-specific tests.  */
}

} // namespace selftest

#endif /* #if CHECKING_P */


#include "gtype-c.h"
