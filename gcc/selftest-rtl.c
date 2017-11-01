/* Selftest support for RTL.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.

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
#include "selftest.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "read-rtl-function.h"
#include "read-md.h"
#include "tree-core.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "selftest-rtl.h"

#if CHECKING_P

namespace selftest {

/* Compare rtx EXPECTED and ACTUAL using rtx_equal_p, calling
   ::selftest::pass if they are equal, aborting if they are non-equal.
   LOC is the effective location of the assertion, MSG describes it.  */

void
assert_rtx_eq_at (const location &loc, const char *msg,
		  rtx expected, rtx actual)
{
  if (rtx_equal_p (expected, actual))
    ::selftest::pass (loc, msg);
  else
    {
      fprintf (stderr, "%s:%i: %s: FAIL: %s\n", loc.m_file, loc.m_line,
	       loc.m_function, msg);
      fprintf (stderr, "  expected: ");
      print_rtl (stderr, expected);
      fprintf (stderr, "\n  actual: ");
      print_rtl (stderr, actual);
      fprintf (stderr, "\n");
      abort ();
    }
}

/* Compare rtx EXPECTED and ACTUAL by pointer equality, calling
   ::selftest::pass if they are equal, aborting if they are non-equal.
   LOC is the effective location of the assertion, MSG describes it.  */

void
assert_rtx_ptr_eq_at (const location &loc, const char *msg,
		      rtx expected, rtx actual)
{
  if (expected == actual)
    ::selftest::pass (loc, msg);
  else
    {
      fprintf (stderr, "%s:%i: %s: FAIL: %s\n", loc.m_file, loc.m_line,
	       loc.m_function, msg);
      fprintf (stderr, "  expected (at %p): ", (void *)expected);
      print_rtl (stderr, expected);
      fprintf (stderr, "\n  actual (at %p): ", (void *)actual);
      print_rtl (stderr, actual);
      fprintf (stderr, "\n");
      abort ();
    }
}

/* Constructor for selftest::rtl_dump_test.
   Read a dumped RTL function from PATH.
   Takes ownership of PATH, freeing in dtor.
   Use LOC as the effective location when reporting failures.  */

rtl_dump_test::rtl_dump_test (const location &loc, char *path)
  : m_path (path)
{
  bool read_ok = read_rtl_function_body (path);
  ASSERT_TRUE_AT (loc, read_ok);
}

/* Destructor for selftest::rtl_dump_test.
   Cleanup global state relating to the function, and free the path.  */

selftest::rtl_dump_test::~rtl_dump_test ()
{
  /* Cleanups.  */
  current_function_decl = NULL;
  free_after_compilation (cfun);
  set_cfun (NULL);
  free (m_path);
}

/* Get the insn with the given uid, or NULL if not found.  */

rtx_insn *
get_insn_by_uid (int uid)
{
  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_UID (insn) == uid)
      return insn;

  /* Not found.  */
  return NULL;
}

} // namespace selftest

#endif /* #if CHECKING_P */
