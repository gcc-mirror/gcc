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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "selftest.h"
#include "intl.h"

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

/* Invoke "diff" to print the difference between VAL1 and VAL2
   on stdout.  */

static void
print_diff (const location &loc, const char *val1, const char *val2)
{
  temp_source_file tmpfile1 (loc, ".txt", val1);
  temp_source_file tmpfile2 (loc, ".txt", val2);
  const char *args[] = {"diff",
			"-up",
			tmpfile1.get_filename (),
			tmpfile2.get_filename (),
			NULL};
  int exit_status = 0;
  int err = 0;
  pex_one (PEX_SEARCH | PEX_LAST,
	   args[0], CONST_CAST (char **, args),
	   NULL, NULL, NULL, &exit_status, &err);
}

/* Implementation detail of ASSERT_STREQ.
   Compare val1 and val2 with strcmp.  They ought
   to be non-NULL; fail gracefully if either or both are NULL.  */

void
assert_streq (const location &loc,
	      const char *desc_val1, const char *desc_val2,
	      const char *val1, const char *val2)
{
  /* If val1 or val2 are NULL, fail with a custom error message.  */
  if (val1 == NULL)
    if (val2 == NULL)
      fail_formatted (loc, "ASSERT_STREQ (%s, %s) val1=NULL val2=NULL",
		      desc_val1, desc_val2);
    else
      fail_formatted (loc, "ASSERT_STREQ (%s, %s) val1=NULL val2=\"%s\"",
		      desc_val1, desc_val2, val2);
  else
    if (val2 == NULL)
      fail_formatted (loc, "ASSERT_STREQ (%s, %s) val1=\"%s\" val2=NULL",
		      desc_val1, desc_val2, val1);
    else
      {
	if (strcmp (val1, val2) == 0)
	  pass (loc, "ASSERT_STREQ");
	else
	  {
	    print_diff (loc, val1, val2);
	    fail_formatted
	      (loc, "ASSERT_STREQ (%s, %s)\n val1=\"%s\"\n val2=\"%s\"\n",
	       desc_val1, desc_val2, val1, val2);
	  }
      }
}

/* Implementation detail of ASSERT_STR_CONTAINS.
   Use strstr to determine if val_needle is within val_haystack.
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

/* Implementation detail of ASSERT_STR_STARTSWITH.
   Determine if VAL_STR starts with VAL_PREFIX.
   ::selftest::pass if VAL_STR does start with VAL_PREFIX.
   ::selftest::fail if it does not, or either is NULL (using
   DESC_STR and DESC_PREFIX in the error message).  */

void
assert_str_startswith (const location &loc,
		       const char *desc_str,
		       const char *desc_prefix,
		       const char *val_str,
		       const char *val_prefix)
{
  /* If val_str is NULL, fail with a custom error message.  */
  if (val_str == NULL)
    fail_formatted (loc, "ASSERT_STR_STARTSWITH (%s, %s) str=NULL",
		    desc_str, desc_prefix);

  /* If val_prefix is NULL, fail with a custom error message.  */
  if (val_prefix == NULL)
    fail_formatted (loc,
		    "ASSERT_STR_STARTSWITH (%s, %s) str=\"%s\" prefix=NULL",
		    desc_str, desc_prefix, val_str);

  if (startswith (val_str, val_prefix))
    pass (loc, "ASSERT_STR_STARTSWITH");
  else
    fail_formatted
	(loc, "ASSERT_STR_STARTSWITH (%s, %s) str=\"%s\" prefix=\"%s\"",
	 desc_str, desc_prefix, val_str, val_prefix);
}


/* Constructor.  Generate a name for the file.  */

named_temp_file::named_temp_file (const char *suffix,
				  file_cache *fc)
{
  m_filename = make_temp_file (suffix);
  ASSERT_NE (m_filename, NULL);
  m_file_cache = fc;
}

/* Destructor.  Delete the tempfile.  */

named_temp_file::~named_temp_file ()
{
  unlink (m_filename);
  if (m_file_cache)
    m_file_cache->forcibly_evict_file (m_filename);
  free (m_filename);
}

/* Constructor.  Create a tempfile using SUFFIX, and write CONTENT to
   it.  Abort if anything goes wrong, using LOC as the effective
   location in the problem report.  */

temp_source_file::temp_source_file (const location &loc,
				    const char *suffix,
				    const char *content,
				    file_cache *fc)
: named_temp_file (suffix, fc)
{
  FILE *out = fopen (get_filename (), "w");
  if (!out)
    fail_formatted (loc, "unable to open tempfile: %s", get_filename ());
  fprintf (out, "%s", content);
  fclose (out);
}

/* As above, but with a size, to allow for NUL bytes in CONTENT.  */

temp_source_file::temp_source_file (const location &loc,
				    const char *suffix,
				    const char *content,
				    size_t sz)
: named_temp_file (suffix)
{
  FILE *out = fopen (get_filename (), "w");
  if (!out)
    fail_formatted (loc, "unable to open tempfile: %s", get_filename ());
  fwrite (content, sz, 1, out);
  fclose (out);
}

/* Avoid introducing locale-specific differences in the results
   by hardcoding open_quote and close_quote.  */

auto_fix_quotes::auto_fix_quotes ()
{
  m_saved_open_quote = open_quote;
  m_saved_close_quote = close_quote;
  open_quote = "`";
  close_quote = "'";
}

/* Restore old values of open_quote and close_quote.  */

auto_fix_quotes::~auto_fix_quotes ()
{
  open_quote = m_saved_open_quote;
  close_quote = m_saved_close_quote;
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
	  size_t new_alloc_sz = alloc_sz ? alloc_sz * 2 : total_sz + 1;
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

/* The path of SRCDIR/testsuite/selftests.  */

const char *path_to_selftest_files = NULL;

/* Convert a path relative to SRCDIR/testsuite/selftests
   to a real path (either absolute, or relative to pwd).
   The result should be freed by the caller.  */

char *
locate_file (const char *name)
{
  ASSERT_NE (NULL, path_to_selftest_files);
  return concat (path_to_selftest_files, "/", name, NULL);
}

/* selftest::test_runner's ctor.  */

test_runner::test_runner (const char *name)
: m_name (name),
  m_start_time (get_run_time ())
{
}

/* selftest::test_runner's dtor.  Print a summary line to stderr.  */

test_runner::~test_runner ()
{
  /* Finished running tests.  */
  long finish_time = get_run_time ();
  long elapsed_time = finish_time - m_start_time;

  fprintf (stderr,
	   "%s: %i pass(es) in %ld.%06ld seconds\n",
	   m_name, num_passes,
	   elapsed_time / 1000000, elapsed_time % 1000000);
}

/* Selftests for libiberty.  */

/* Verify that xstrndup generates EXPECTED when called on SRC and N.  */

static void
assert_xstrndup_eq (const char *expected, const char *src, size_t n)
{
  char *buf = xstrndup (src, n);
  ASSERT_STREQ (expected, buf);
  free (buf);
}

/* Verify that xstrndup works as expected.  */

static void
test_xstrndup ()
{
  assert_xstrndup_eq ("", "test", 0);
  assert_xstrndup_eq ("t", "test", 1);
  assert_xstrndup_eq ("te", "test", 2);
  assert_xstrndup_eq ("tes", "test", 3);
  assert_xstrndup_eq ("test", "test", 4);
  assert_xstrndup_eq ("test", "test", 5);

  /* Test on an string without zero termination.  */
  const char src[4] = {'t', 'e', 's', 't'};
  assert_xstrndup_eq ("", src, 0);
  assert_xstrndup_eq ("t", src, 1);
  assert_xstrndup_eq ("te", src, 2);
  assert_xstrndup_eq ("tes", src, 3);
  assert_xstrndup_eq ("test", src, 4);
}

/* Run selftests for libiberty.  */

static void
test_libiberty ()
{
  test_xstrndup ();
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
  ASSERT_GT (2, 1);
  ASSERT_GT_AT (SELFTEST_LOCATION, 2, 1);
  ASSERT_LT (1, 2);
  ASSERT_LT_AT (SELFTEST_LOCATION, 1, 2);
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

/* Verify locate_file (and read_file).  */

static void
test_locate_file ()
{
  char *path = locate_file ("example.txt");
  char *buf = read_file (SELFTEST_LOCATION, path);
  ASSERT_STREQ ("example of a selftest file\n", buf);
  free (buf);
  free (path);
}

/* Run all of the selftests within this file.  */

void
selftest_cc_tests ()
{
  test_libiberty ();
  test_assertions ();
  test_named_temp_file ();
  test_read_file ();
  test_locate_file ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
