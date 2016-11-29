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

namespace selftest {

int num_passes;

/* Record the successful outcome of some aspect of a test.  */

void
pass (const location &/*loc*/, const char */*msg*/)
{
  num_passes++;
}

/* Report the failed outcome of some aspect of a test and abort.  */

void
fail (const location &loc, const char *msg)
{
  fprintf (stderr,"%s:%i: %s: FAIL: %s\n", loc.m_file, loc.m_line,
	   loc.m_function, msg);
  abort ();
}

/* As "fail", but using printf-style formatted output.  */

void
fail_formatted (const location &loc, const char *fmt, ...)
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
assert_streq (const location &loc,
	      const char *desc_expected, const char *desc_actual,
	      const char *val_expected, const char *val_actual)
{
  /* If val_expected is NULL, the test is buggy.  Fail gracefully.  */
  if (val_expected == NULL)
    fail_formatted (loc, "ASSERT_STREQ (%s, %s) expected=NULL",
		    desc_expected, desc_actual);
  /* If val_actual is NULL, fail with a custom error message.  */
  if (val_actual == NULL)
    fail_formatted (loc, "ASSERT_STREQ (%s, %s) expected=\"%s\" actual=NULL",
		    desc_expected, desc_actual, val_expected);
  if (0 == strcmp (val_expected, val_actual))
    pass (loc, "ASSERT_STREQ");
  else
    fail_formatted (loc, "ASSERT_STREQ (%s, %s) expected=\"%s\" actual=\"%s\"",
		    desc_expected, desc_actual, val_expected, val_actual);
}

/* Implementation detail of ASSERT_STR_CONTAINS.
   Use strstr to determine if val_needle is is within val_haystack.
   ::selftest::pass if it is found.
   ::selftest::fail if it is not found.  */

void
assert_str_contains (const location &loc,
		     const char *desc_haystack,
		     const char *desc_needle,
		     const char *val_haystack,
		     const char *val_needle)
{
  /* If val_haystack is NULL, fail with a custom error message.  */
  if (val_haystack == NULL)
    fail_formatted (loc, "ASSERT_STR_CONTAINS (%s, %s) haystack=NULL",
		    desc_haystack, desc_needle);

  /* If val_needle is NULL, fail with a custom error message.  */
  if (val_needle == NULL)
    fail_formatted (loc,
		    "ASSERT_STR_CONTAINS (%s, %s) haystack=\"%s\" needle=NULL",
		    desc_haystack, desc_needle, val_haystack);

  const char *test = strstr (val_haystack, val_needle);
  if (test)
    pass (loc, "ASSERT_STR_CONTAINS");
  else
    fail_formatted
	(loc, "ASSERT_STR_CONTAINS (%s, %s) haystack=\"%s\" needle=\"%s\"",
	 desc_haystack, desc_needle, val_haystack, val_needle);
}

/* Constructor.  Generate a name for the file.  */

named_temp_file::named_temp_file (const char *suffix)
{
  m_filename = make_temp_file (suffix);
  ASSERT_NE (m_filename, NULL);
}

/* Destructor.  Delete the tempfile.  */

named_temp_file::~named_temp_file ()
{
  unlink (m_filename);
  diagnostics_file_cache_forcibly_evict_file (m_filename);
  free (m_filename);
}

/* Constructor.  Create a tempfile using SUFFIX, and write CONTENT to
   it.  Abort if anything goes wrong, using LOC as the effective
   location in the problem report.  */

temp_source_file::temp_source_file (const location &loc,
				    const char *suffix,
				    const char *content)
: named_temp_file (suffix)
{
  FILE *out = fopen (get_filename (), "w");
  if (!out)
    fail_formatted (loc, "unable to open tempfile: %s", get_filename ());
  fprintf (out, "%s", content);
  fclose (out);
}

/* Read the contents of PATH into memory, returning a 0-terminated buffer
   that must be freed by the caller.
   Fail (and abort) if there are any problems, with LOC as the reported
   location of the failure.  */

char *
read_file (const location &loc, const char *path)
{
  FILE *f_in = fopen (path, "r");
  if (!f_in)
    fail_formatted (loc, "unable to open file: %s", path);

  /* Read content, allocating FIXME.  */
  char *result = NULL;
  size_t total_sz = 0;
  size_t alloc_sz = 0;
  char buf[4096];
  size_t iter_sz_in;

  while ( (iter_sz_in = fread (buf, 1, sizeof (buf), f_in)) )
    {
      gcc_assert (alloc_sz >= total_sz);
      size_t old_total_sz = total_sz;
      total_sz += iter_sz_in;
      /* Allow 1 extra byte for 0-termination.  */
      if (alloc_sz < (total_sz + 1))
	{
	  size_t new_alloc_sz = alloc_sz ? alloc_sz * 2: total_sz + 1;
	  result = (char *)xrealloc (result, new_alloc_sz);
	  alloc_sz = new_alloc_sz;
	}
      memcpy (result + old_total_sz, buf, iter_sz_in);
    }

  if (!feof (f_in))
    fail_formatted (loc, "error reading from %s: %s", path,
		    xstrerror (errno));

  fclose (f_in);

  /* 0-terminate the buffer.  */
  gcc_assert (total_sz < alloc_sz);
  result[total_sz] = '\0';

  return result;
}

/* Selftests for the selftest system itself.  */

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
    fail_formatted (SELFTEST_LOCATION,
		    "unable to open %s for writing", t.get_filename ());
  fclose (f);
}

/* Verify read_file (and also temp_source_file).  */

static void
test_read_file ()
{
  temp_source_file t (SELFTEST_LOCATION, "test1.s",
		      "\tjmp\t.L2\n");
  char *buf = read_file (SELFTEST_LOCATION, t.get_filename ());
  ASSERT_STREQ ("\tjmp\t.L2\n", buf);
  free (buf);
}

/* Run all of the selftests within this file.  */

void
selftest_c_tests ()
{
  test_assertions ();
  test_named_temp_file ();
  test_read_file ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
