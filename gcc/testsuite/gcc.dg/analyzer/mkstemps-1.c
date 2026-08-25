/* { dg-additional-options "-Wno-analyzer-null-argument" } */
/* { dg-prune-output "leak of file descriptor" } */
/* { dg-skip-if "incomplete stdio" { avr-*-* } } */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

extern void populate (char *buf);

void test_passthrough (char *s, int suffixlen)
{
  mkstemps (s, suffixlen);
}

void test_string_literal_correct_placeholder (void)
{
  mkstemps ("/tmp/fooXXXXXX.txt", 4); /* { dg-warning "'mkstemps' on a string literal \\\[STR30-C\\\]" } */
  /* { dg-message "use a writable character array" "fix suggestion" { target *-*-* } .-1 } */
}

void test_string_literal_suffix_off_by_one (void)
{
  mkstemps ("/tmp/fooXXXXXX.txt", 3); /* { dg-warning "'mkstemps' on a string literal \\\[STR30-C\\\]" } */
}

void test_string_literal_missing_placeholder (void)
{
  mkstemps ("/tmp/foo.txt", 4); /* { dg-warning "'mkstemps' on a string literal \\\[STR30-C\\\]" } */
}

void test_string_literal_empty (void)
{
  mkstemps ("", 0); /* { dg-warning "'mkstemps' on a string literal \\\[STR30-C\\\]" } */
}

void test_correct_with_suffix (void)
{
  char tmpl[] = "/tmp/fooXXXXXX.txt";
  mkstemps (tmpl, 4);
}

void test_correct_zero_suffix (void)
{
  char tmpl[] = "/tmp/barXXXXXX";
  mkstemps (tmpl, 0);
}

void test_correct_single_char_suffix (void)
{
  char tmpl[] = "XXXXXXZ";
  mkstemps (tmpl, 1);
}

void test_missing_placeholder_with_suffix (void)
{
  char tmpl[] = "/tmp/foo.conf";
  mkstemps (tmpl, 5); /* { dg-warning "'mkstemps' template string does not contain 'XXXXXX' before a 5-character suffix" } */
}

void test_placeholder_at_wrong_position (void)
{
  /* "XXXXXX" is at the end, but suffixlen says 3 chars should follow it. */
  char tmpl[] = "/tmp/fooXXXXXX";
  mkstemps (tmpl, 3); /* { dg-warning "'mkstemps' template string does not contain 'XXXXXX' before a 3-character suffix" } */
}

void test_too_short_for_suffix (void)
{
  char tmpl[] = "abc";
  mkstemps (tmpl, 2); /* { dg-warning "'mkstemps' template string does not contain 'XXXXXX' before a 2-character suffix" } */
}

void test_empty_buffer_with_suffix (void)
{
  char tmpl[] = "";
  mkstemps (tmpl, 3); /* { dg-warning "'mkstemps' template string does not contain 'XXXXXX' before a 3-character suffix" } */
}

void test_suffix_too_large (void)
{
  char tmpl[] = "XXXXXXAB";
  mkstemps (tmpl, 4); /* { dg-warning "'mkstemps' template string does not contain 'XXXXXX' before a 4-character suffix" } */
}

void test_populated_buf (void)
{
  char tmpl[30];
  populate (tmpl);
  mkstemps (tmpl, 4);
}

void test_NULL (void)
{
  mkstemps (NULL, 0); /* possibly -Wanalyzer-null-argument */
}
