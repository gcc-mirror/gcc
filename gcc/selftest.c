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
selftest::pass (const location &/*loc*/, const char */*msg*/)
{
  num_passes++;
}

/* Report the failed outcome of some aspect of a test and abort.  */

void
selftest::fail (const location &loc, const char *msg)
{
  fprintf (stderr,"%s:%i: %s: FAIL: %s\n", loc.m_file, loc.m_line,
	   loc.m_function, msg);
  abort ();
}

/* As "fail", but using printf-style formatted output.  */

void
selftest::fail_formatted (const location &loc, const char *fmt, ...)
{
  va_list ap;

  fprintf (stderr, "%s:%i: %s: FAIL: ", loc.m_file, loc.m_line,
	   loc.m_function);
  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  va_end (ap);
  fprintf (stderr, "\n");
  abort ();
}

/* Implementation detail of ASSERT_STREQ.
   Compare val_expected and val_actual with strcmp.  They ought
   to be non-NULL; fail gracefully if either are NULL.  */

void
selftest::assert_streq (const location &loc,
			const char *desc_expected, const char *desc_actual,
			const char *val_expected, const char *val_actual)
{
  /* If val_expected is NULL, the test is buggy.  Fail gracefully.  */
  if (val_expected == NULL)
    ::selftest::fail_formatted
	(loc, "ASSERT_STREQ (%s, %s) expected=NULL",
	 desc_expected, desc_actual);
  /* If val_actual is NULL, fail with a custom error message.  */
  if (val_actual == NULL)
    ::selftest::fail_formatted
	(loc, "ASSERT_STREQ (%s, %s) expected=\"%s\" actual=NULL",
	 desc_expected, desc_actual, val_expected);
  if (0 == strcmp (val_expected, val_actual))
    ::selftest::pass (loc, "ASSERT_STREQ");
  else
    ::selftest::fail_formatted
	(loc, "ASSERT_STREQ (%s, %s) expected=\"%s\" actual=\"%s\"",
	 desc_expected, desc_actual, val_expected, val_actual);
}

/* Implementation detail of ASSERT_STR_CONTAINS.
   Use strstr to determine if val_needle is is within val_haystack.
   ::selftest::pass if it is found.
   ::selftest::fail if it is not found.  */

void
selftest::assert_str_contains (const location &loc,
			       const char *desc_haystack,
			       const char *desc_needle,
			       const char *val_haystack,
			       const char *val_needle)
{
  /* If val_haystack is NULL, fail with a custom error message.  */
  if (val_haystack == NULL)
    ::selftest::fail_formatted
	(loc, "ASSERT_STR_CONTAINS (%s, %s) haystack=NULL",
	 desc_haystack, desc_needle);

  /* If val_needle is NULL, fail with a custom error message.  */
  if (val_needle == NULL)
    ::selftest::fail_formatted
	(loc, "ASSERT_STR_CONTAINS (%s, %s) haystack=\"%s\" needle=NULL",
	 desc_haystack, desc_needle, val_haystack);

  const char *test = strstr (val_haystack, val_needle);
  if (test)
    ::selftest::pass (loc, "ASSERT_STR_CONTAINS");
  else
    ::selftest::fail_formatted
	(loc, "ASSERT_STR_CONTAINS (%s, %s) haystack=\"%s\" needle=\"%s\"",
	 desc_haystack, desc_needle, val_haystack, val_needle);
}

/* Constructor.  Generate a name for the file.  */

selftest::named_temp_file::named_temp_file (const char *suffix)
{
  m_filename = make_temp_file (suffix);
  ASSERT_NE (m_filename, NULL);
}

/* Destructor.  Delete the tempfile.  */

selftest::named_temp_file::~named_temp_file ()
{
  unlink (m_filename);
  diagnostics_file_cache_forcibly_evict_file (m_filename);
  free (m_filename);
}

/* Constructor.  Create a tempfile using SUFFIX, and write CONTENT to
   it.  Abort if anything goes wrong, using LOC as the effective
   location in the problem report.  */

selftest::temp_source_file::temp_source_file (const location &loc,
					      const char *suffix,
					      const char *content)
: named_temp_file (suffix)
{
  FILE *out = fopen (get_filename (), "w");
  if (!out)
    ::selftest::fail_formatted (loc, "unable to open tempfile: %s",
				get_filename ());
  fprintf (out, "%s", content);
  fclose (out);
}

/* Selftests for the selftest system itself.  */

namespace selftest {

/* Sanity-check the ASSERT_ macros with various passing cases.  */

static void
test_assertions ()
{
  ASSERT_TRUE (true);
  ASSERT_FALSE (false);
  ASSERT_EQ (1, 1);
  ASSERT_EQ_AT (SELFTEST_LOCATION, 1, 1);
  ASSERT_NE (1, 2);
  ASSERT_STREQ ("test", "test");
  ASSERT_STREQ_AT (SELFTEST_LOCATION, "test", "test");
  ASSERT_STR_CONTAINS ("foo bar baz", "bar");
}

/* Verify named_temp_file.  */

static void
test_named_temp_file ()
{
  named_temp_file t (".txt");
  FILE *f = fopen (t.get_filename (), "w");
  if (!f)
    selftest::fail_formatted (SELFTEST_LOCATION,
			      "unable to open %s for writing",
			      t.get_filename ());
  fclose (f);
}

/* Run all of the selftests within this file.  */

void
selftest_c_tests ()
{
  test_assertions ();
  test_named_temp_file ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
