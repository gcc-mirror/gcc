/* { dg-additional-options "-Wno-analyzer-null-argument" } */
/* { dg-prune-output "leak of file descriptor" } */
/* { dg-skip-if "incomplete stdio" { avr-*-* } } */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

extern int mkostemps (char *, int, int);
extern void populate (char *buf);

#if defined(__APPLE__) && !defined(O_CLOEXEC)
# define O_CLOEXEC 0x1000000
#endif

void test_passthrough (char *s, int suffixlen, int flags)
{
  mkostemps (s, suffixlen, flags);
}

void test_string_literal_correct_placeholder (void)
{
  mkostemps ("/tmp/dataXXXXXX.db", 3, O_CLOEXEC); /* { dg-warning "'mkostemps' on a string literal \\\[STR30-C\\\]" } */
  /* { dg-message "use a writable character array" "fix suggestion" { target *-*-* } .-1 } */
}

void test_string_literal_suffix_off_by_one (void)
{
  mkostemps ("/tmp/dataXXXXXX.db", 2, O_CLOEXEC); /* { dg-warning "'mkostemps' on a string literal \\\[STR30-C\\\]" } */
}

void test_string_literal_missing_placeholder (void)
{
  mkostemps ("/tmp/data.db", 3, O_CLOEXEC); /* { dg-warning "'mkostemps' on a string literal \\\[STR30-C\\\]" } */
}

void test_string_literal_empty (void)
{
  mkostemps ("", 0, 0); /* { dg-warning "'mkostemps' on a string literal \\\[STR30-C\\\]" } */
}

void test_correct_with_suffix (void)
{
  char tmpl[] = "/tmp/dataXXXXXX.db";
  mkostemps (tmpl, 3, O_CLOEXEC);
}

void test_correct_zero_suffix (void)
{
  char tmpl[] = "/tmp/logXXXXXX";
  mkostemps (tmpl, 0, O_CLOEXEC);
}

void test_correct_long_suffix (void)
{
  char tmpl[] = "XXXXXX.json";
  mkostemps (tmpl, 5, 0);
}

void test_missing_placeholder_with_suffix (void)
{
  char tmpl[] = "/tmp/data.json";
  mkostemps (tmpl, 5, O_CLOEXEC); /* { dg-warning "'mkostemps' template string does not contain 'XXXXXX' before a 5-character suffix" } */
}

void test_placeholder_at_wrong_position (void)
{
  /* "XXXXXX" is at the end, but suffixlen says 2 chars should follow it. */
  char tmpl[] = "/tmp/dataXXXXXX";
  mkostemps (tmpl, 2, 0); /* { dg-warning "'mkostemps' template string does not contain 'XXXXXX' before a 2-character suffix" } */
}

void test_too_short_for_suffix (void)
{
  char tmpl[] = "XY";
  mkostemps (tmpl, 1, O_CLOEXEC); /* { dg-warning "'mkostemps' template string does not contain 'XXXXXX' before a 1-character suffix" } */
}

void test_empty_buffer_with_suffix (void)
{
  char tmpl[] = "";
  mkostemps (tmpl, 2, 0); /* { dg-warning "'mkostemps' template string does not contain 'XXXXXX' before a 2-character suffix" } */
}

void test_suffix_consumes_placeholder (void)
{
  char tmpl[] = "XXXXXX.c";
  mkostemps (tmpl, 6, O_CLOEXEC); /* { dg-warning "'mkostemps' template string does not contain 'XXXXXX' before a 6-character suffix" } */
}

void test_populated_buf (void)
{
  char tmpl[28];
  populate (tmpl);
  mkostemps (tmpl, 3, O_CLOEXEC);
}

void test_NULL (void)
{
  mkostemps (NULL, 0, 0); /* possibly -Wanalyzer-null-argument */
}

/* Getting a stashed constant can currently fail depending on the system
headers; see e.g.  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=108708

It may be needed to add a dg-skip for `test_redundant_o_rdwr`,
`test_redundant_o_creat`, `test_redundant_o_excl`, and
`test_redundant_combined` on some systems.  */

void test_redundant_o_rdwr (void)
{
  char tmpl[] = "/tmp/dataXXXXXX.db";
  mkostemps (tmpl, 3, O_RDWR); /* { dg-warning "'mkostemps' flags argument should not include 'O_RDWR', 'O_CREAT', or 'O_EXCL' as these are already implied" } */
}

void test_redundant_o_creat (void)
{
  char tmpl[] = "/tmp/dataXXXXXX.db";
  mkostemps (tmpl, 3, O_CREAT); /* { dg-warning "'mkostemps' flags argument should not include 'O_RDWR', 'O_CREAT', or 'O_EXCL' as these are already implied" } */
}

void test_redundant_o_excl (void)
{
  char tmpl[] = "/tmp/dataXXXXXX.db";
  mkostemps (tmpl, 3, O_EXCL); /* { dg-warning "'mkostemps' flags argument should not include 'O_RDWR', 'O_CREAT', or 'O_EXCL' as these are already implied" } */
}

void test_redundant_combined (void)
{
  char tmpl[] = "/tmp/dataXXXXXX.db";
  mkostemps (tmpl, 3, O_RDWR | O_CREAT | O_EXCL); /* { dg-warning "'mkostemps' flags argument should not include 'O_RDWR', 'O_CREAT', or 'O_EXCL' as these are already implied" } */
}

void test_valid_flags (void)
{
  char tmpl[] = "/tmp/dataXXXXXX.db";
  mkostemps (tmpl, 3, O_CLOEXEC);
}

void test_zero_flags (void)
{
  char tmpl[] = "/tmp/dataXXXXXX.db";
  mkostemps (tmpl, 3, 0);
}
