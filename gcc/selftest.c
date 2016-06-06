/* A self-testing framework, for use by -fself-test.
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

#if CHECKING_P

int selftest::num_passes;

/* Record the successful outcome of some aspect of a test.  */

void
selftest::pass (const char */*file*/, int /*line*/, const char */*msg*/)
{
  num_passes++;
}

/* Report the failed outcome of some aspect of a test and abort.  */

void
selftest::fail (const char *file, int line, const char *msg)
{
  fprintf (stderr,"%s:%i: FAIL: %s\n", file, line, msg);
  /* TODO: add calling function name as well?  */
  abort ();
}

#endif /* #if CHECKING_P */
