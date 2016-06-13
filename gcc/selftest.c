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

/* As "fail", but using printf-style formatted output.  */

void
selftest::fail_formatted (const char *file, int line, const char *fmt, ...)
{
  va_list ap;

  fprintf (stderr, "%s:%i: FAIL: ", file, line);
  /* TODO: add calling function name as well?  */
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fprintf (stderr, "\n");
  abort ();
}

/* Implementation detail of ASSERT_STREQ.  */

void
selftest::assert_streq (const char *file, int line,
			const char *desc_expected, const char *desc_actual,
			const char *val_expected, const char *val_actual)
{
  if (0 == strcmp (val_expected, val_actual))
    ::selftest::pass (file, line, "ASSERT_STREQ");
  else
    ::selftest::fail_formatted
	(file, line, "ASSERT_STREQ (%s, %s) expected=\"%s\" actual=\"%s\"",
	 desc_expected, desc_actual, val_expected, val_actual);
}


#endif /* #if CHECKING_P */
