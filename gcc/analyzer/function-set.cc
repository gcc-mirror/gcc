/* Sets of function names.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "selftest.h"
#include "analyzer/function-set.h"

#if ENABLE_ANALYZER

namespace ana {

/* Return true if NAME is within this set.  */

bool
function_set::contains_name_p (const char *name) const
{
  /* Binary search.  */
  int min = 0;
  int max = m_count - 1;
  while (true)
    {
      if (min > max)
	return false;
      int midpoint = (min + max) / 2;
      gcc_assert ((size_t)midpoint < m_count);
      int cmp = strcmp (name, m_names[midpoint]);
      if (cmp == 0)
	return true;
      else if (cmp < 0)
	max = midpoint - 1;
      else
	min = midpoint + 1;
    }
}

/* Return true if FNDECL is within this set.  */

bool
function_set::contains_decl_p (tree fndecl) const
{
  gcc_assert (fndecl && DECL_P (fndecl));
  if (!maybe_special_function_p (fndecl))
    return false;
  return contains_name_p (IDENTIFIER_POINTER (DECL_NAME (fndecl)));
}

/* Assert that the list of names is in sorted order.  */

void
function_set::assert_sorted () const
{
#if CHECKING_P
  for (size_t idx = 1; idx < m_count; idx++)
    gcc_assert (strcmp (m_names[idx - 1], m_names[idx]) < 0);
#endif /* #if CHECKING_P  */
}

/* Assert that contains_p is true for all members of the set.  */

void
function_set::assert_sane () const
{
#if CHECKING_P
  for (size_t i = 0; i < m_count; i++)
    gcc_assert (contains_name_p (m_names[i]));
#endif /* #if CHECKING_P  */
}

#if CHECKING_P

namespace selftest {

/* Verify that an empty function_set works as expected.  */

static void
test_empty ()
{
  function_set fs (NULL, 0);
  fs.assert_sorted ();
  fs.assert_sane ();
  ASSERT_FALSE (fs.contains_name_p (""));
  ASSERT_FALSE (fs.contains_name_p ("haystack"));
}

/* Verify that a function_set with an odd number of elements works as
   expected.  */

static void
test_odd ()
{
  static const char * const names[3] = {"alpha", "beta", "gamma"};
  function_set fs (names, 3);
  fs.assert_sorted ();
  fs.assert_sane ();
  ASSERT_FALSE (fs.contains_name_p (""));
  ASSERT_FALSE (fs.contains_name_p ("haystack"));
}

/* Verify that a function_set with an even number of elements works as
   expected.  */

static void
test_even ()
{
  static const char * const names[3] = {"alpha", "beta"};
  function_set fs (names, 2);
  fs.assert_sorted ();
  fs.assert_sane ();
  ASSERT_FALSE (fs.contains_name_p (""));
  ASSERT_FALSE (fs.contains_name_p ("haystack"));
}

/* Verify that a function_set with some nontrivial stdio.h data works as
   expected.  */

static void
test_stdio_example ()
{
  static const char * const example[] = {
    "__fbufsize",
    "__flbf",
    "__fpending",
    "__fpurge",
    "__freadable",
    "__freading",
    "__fsetlocking",
    "__fwritable",
    "__fwriting",
    "clearerr_unlocked",
    "feof_unlocked",
    "ferror_unlocked",
    "fflush_unlocked",
    "fgetc_unlocked",
    "fgets",
    "fgets_unlocked",
    "fgetwc_unlocked",
    "fgetws_unlocked",
    "fileno_unlocked",
    "fputc_unlocked",
    "fputs_unlocked",
    "fputwc_unlocked",
    "fputws_unlocked",
    "fread_unlocked",
    "fwrite_unlocked",
    "getc_unlocked",
    "getwc_unlocked",
    "putc_unlocked"
  };
  const size_t count = sizeof(example) / sizeof (example[0]);
  function_set fs (example, count);
  fs.assert_sorted ();
  fs.assert_sane ();
  /* Examples of strings not present: before, after and alongside the
     sorted list.  */
  ASSERT_FALSE (fs.contains_name_p ("___"));
  ASSERT_FALSE (fs.contains_name_p ("Z"));
  ASSERT_FALSE (fs.contains_name_p ("fgets_WITH_A_PREFIX"));
}

/* Run all of the selftests within this file.  */

void
analyzer_function_set_cc_tests ()
{
  test_empty ();
  test_odd ();
  test_even ();
  test_stdio_example ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
