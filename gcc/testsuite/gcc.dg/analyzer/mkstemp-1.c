/* { dg-additional-options "-Wno-analyzer-null-argument" } */
/* { dg-prune-output "leak of file descriptor" } */
/* { dg-skip-if "incomplete stdio" { avr-*-* } } */

#include <stdio.h>
#include <stdlib.h>

extern void populate (char *buf);

void test_passthrough (char *s)
{
  mkstemp (s);
}

void test_string_literal_correct_placeholder (void)
{
  mkstemp ("/tmp/fooXXXXXX"); /* { dg-warning "'mkstemp' on a string literal \\\[STR30-C\\\]" } */
  /* { dg-message "use a writable character array" "fix suggestion" { target *-*-* } .-1 } */
}

void test_string_literal_missing_placeholder (void)
{
  mkstemp ("/tmp/foo"); /* { dg-warning "'mkstemp' on a string literal \\\[STR30-C\\\]" } */
}

void test_string_literal_empty (void)
{
  mkstemp (""); /* { dg-warning "'mkstemp' on a string literal \\\[STR30-C\\\]" } */
}

void test_correct (void)
{
  char tmpl[] = "/tmp/fooXXXXXX";
  mkstemp (tmpl);
}

void test_correct_minimal (void)
{
  char tmpl[] = "XXXXXX";
  mkstemp (tmpl);
}

void test_correct_offset_into_buffer (void)
{
  char buf[] = "/tmp/XXXXXX";
  /* Placeholder is still correct from the pointer's perspective.  */
  mkstemp (buf + 5);
}

void test_missing_placeholder_offset_into_buffer (void)
{
  char buf[] = "/tmp/XXXXXX";
  /* Placeholder is incorrect from the pointer's perspective.  */
  mkstemp (buf + 6); /* { dg-warning "'mkstemp' template string does not end with 'XXXXXX'" } */
}

void test_missing_placeholder (void)
{
  char tmpl[] = "/tmp/foo";
  mkstemp (tmpl); /* { dg-warning "'mkstemp' template string does not end with 'XXXXXX'" } */
}

void test_too_short (void)
{
  char tmpl[] = "XXX";
  mkstemp (tmpl); /* { dg-warning "'mkstemp' template string does not end with 'XXXXXX'" } */
}

void test_empty_buffer (void)
{
  char tmpl[] = "";
  mkstemp (tmpl); /* { dg-warning "'mkstemp' template string does not end with 'XXXXXX'" } */
}

void test_partial_placeholder (void)
{
  char tmpl[] = "/tmp/fooXXXXX_";
  mkstemp (tmpl); /* { dg-warning "'mkstemp' template string does not end with 'XXXXXX'" } */
}

void test_five_xs (void)
{
  char tmpl[] = "/tmp/fooXXXXX";
  mkstemp (tmpl); /* { dg-warning "'mkstemp' template string does not end with 'XXXXXX'" } */
}

void test_populated_buf (void)
{
  char tmpl[20];
  populate (tmpl);
  mkstemp (tmpl);
}

void test_NULL (void)
{
  mkstemp (NULL); /* possibly -Wanalyzer-null-argument */
}
