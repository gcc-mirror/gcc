/* { dg-additional-options "-Wno-analyzer-null-argument" } */
/* { dg-prune-output "leak of file descriptor" } */
/* { dg-skip-if "incomplete stdio" { avr-*-* } } */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

extern int mkostemp (char *, int);
extern void populate (char *buf);

#if defined(__APPLE__) && !defined(O_CLOEXEC)
# define O_CLOEXEC 0x1000000
#endif

void test_passthrough (char *s, int flags)
{
  mkostemp (s, flags);
}

void test_string_literal_correct_placeholder (void)
{
  mkostemp ("/tmp/testXXXXXX", O_CLOEXEC); /* { dg-warning "'mkostemp' on a string literal \\\[STR30-C\\\]" } */
  /* { dg-message "use a writable character array" "fix suggestion" { target *-*-* } .-1 } */
}

void test_string_literal_missing_placeholder (void)
{
  mkostemp ("/tmp/test", O_CLOEXEC); /* { dg-warning "'mkostemp' on a string literal \\\[STR30-C\\\]" } */
}

void test_string_literal_empty (void)
{
  mkostemp ("", 0); /* { dg-warning "'mkostemp' on a string literal \\\[STR30-C\\\]" } */
}

void test_correct (void)
{
  char tmpl[] = "/tmp/test.XXXXXX";
  mkostemp (tmpl, O_CLOEXEC);
}

void test_correct_minimal (void)
{
  char tmpl[] = "XXXXXX";
  mkostemp (tmpl, 0);
}

void test_correct_offset_into_buffer (void)
{
  char buf[] = "////XXXXXX";
  mkostemp (buf + 4, O_CLOEXEC);
}

void test_missing_placeholder (void)
{
  char tmpl[] = "/tmp/test";
  mkostemp (tmpl, O_CLOEXEC); /* { dg-warning "'mkostemp' template string does not end with 'XXXXXX'" } */
}

void test_too_short (void)
{
  char tmpl[] = "XXXX";
  mkostemp (tmpl, 0); /* { dg-warning "'mkostemp' template string does not end with 'XXXXXX'" } */
}

void test_empty_buffer (void)
{
  char tmpl[] = "";
  mkostemp (tmpl, O_CLOEXEC); /* { dg-warning "'mkostemp' template string does not end with 'XXXXXX'" } */
}

void test_trailing_nul_after_placeholder (void)
{
  char tmpl[] = "/tmp/testXXXXXXz";
  mkostemp (tmpl, 0); /* { dg-warning "'mkostemp' template string does not end with 'XXXXXX'" } */
}

void test_five_xs (void)
{
  char tmpl[] = "/tmp/testXXXXX";
  mkostemp (tmpl, O_CLOEXEC); /* { dg-warning "'mkostemp' template string does not end with 'XXXXXX'" } */
}

void test_populated_buf (void)
{
  char tmpl[24];
  populate (tmpl);
  mkostemp (tmpl, O_CLOEXEC);
}

void test_NULL (void)
{
  mkostemp (NULL, 0); /* possibly -Wanalyzer-null-argument */
}

/* Getting a stashed constant can currently fail depending on the system
headers; see e.g.  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=108708

It may be needed to add a dg-skip for `test_redundant_o_rdwr`,
`test_redundant_o_creat`, `test_redundant_o_excl`, and
`test_redundant_combined` on some systems.  */

void test_redundant_o_rdwr (void)
{
  char tmpl[] = "/tmp/testXXXXXX";
  mkostemp (tmpl, O_RDWR); /* { dg-warning "'mkostemp' flags argument should not include 'O_RDWR', 'O_CREAT', or 'O_EXCL' as these are already implied" } */
}

void test_redundant_o_creat (void)
{
  char tmpl[] = "/tmp/testXXXXXX";
  mkostemp (tmpl, O_CREAT); /* { dg-warning "'mkostemp' flags argument should not include 'O_RDWR', 'O_CREAT', or 'O_EXCL' as these are already implied" } */
}

void test_redundant_o_excl (void)
{
  char tmpl[] = "/tmp/testXXXXXX";
  mkostemp (tmpl, O_EXCL); /* { dg-warning "'mkostemp' flags argument should not include 'O_RDWR', 'O_CREAT', or 'O_EXCL' as these are already implied" } */
}

void test_redundant_combined (void)
{
  char tmpl[] = "/tmp/testXXXXXX";
  mkostemp (tmpl, O_RDWR | O_CREAT); /* { dg-warning "'mkostemp' flags argument should not include 'O_RDWR', 'O_CREAT', or 'O_EXCL' as these are already implied" } */
}

void test_valid_flags (void)
{
  char tmpl[] = "/tmp/testXXXXXX";
  mkostemp (tmpl, O_CLOEXEC);
}

void test_zero_flags (void)
{
  char tmpl[] = "/tmp/testXXXXXX";
  mkostemp (tmpl, 0);
}
