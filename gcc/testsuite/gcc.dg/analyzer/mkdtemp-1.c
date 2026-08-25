/* { dg-additional-options "-Wno-analyzer-null-argument" } */
/* { dg-skip-if "incomplete stdio" { avr-*-* } } */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

extern void populate (char *buf);

void test_passthrough (char *s)
{
  mkdtemp (s);
}

void test_string_literal_correct_placeholder (void)
{
  mkdtemp ("/var/tmp/dirXXXXXX"); /* { dg-warning "'mkdtemp' on a string literal \\\[STR30-C\\\]" } */
  /* { dg-message "use a writable character array" "fix suggestion" { target *-*-* } .-1 } */
}

void test_string_literal_missing_placeholder (void)
{
  mkdtemp ("/var/tmp/dir"); /* { dg-warning "'mkdtemp' on a string literal \\\[STR30-C\\\]" } */
}

void test_string_literal_empty (void)
{
  mkdtemp (""); /* { dg-warning "'mkdtemp' on a string literal \\\[STR30-C\\\]" } */
}

void test_correct (void)
{
  char tmpl[] = "/var/tmp/mydir.XXXXXX";
  mkdtemp (tmpl);
}

void test_correct_minimal (void)
{
  char tmpl[] = "XXXXXX";
  mkdtemp (tmpl);
}

void test_correct_offset_into_buffer (void)
{
  char buf[] = "prefixXXXXXX";
  mkdtemp (buf + 6);
}

void test_missing_placeholder (void)
{
  char tmpl[] = "/var/tmp/dir";
  mkdtemp (tmpl); /* { dg-warning "'mkdtemp' template string does not end with 'XXXXXX'" } */
}

void test_too_short (void)
{
  char tmpl[] = "XX";
  mkdtemp (tmpl); /* { dg-warning "'mkdtemp' template string does not end with 'XXXXXX'" } */
}

void test_empty_buffer (void)
{
  char tmpl[] = "";
  mkdtemp (tmpl); /* { dg-warning "'mkdtemp' template string does not end with 'XXXXXX'" } */
}

void test_partial_placeholder (void)
{
  char tmpl[] = "/var/tmp/dirXXXXX.";
  mkdtemp (tmpl); /* { dg-warning "'mkdtemp' template string does not end with 'XXXXXX'" } */
}

void test_four_xs (void)
{
  char tmpl[] = "/var/tmp/dirXXXX";
  mkdtemp (tmpl); /* { dg-warning "'mkdtemp' template string does not end with 'XXXXXX'" } */
}

void test_populated_buf (void)
{
  char tmpl[32];
  populate (tmpl);
  mkdtemp (tmpl);
}

void test_NULL (void)
{
  mkdtemp (NULL); /* possibly -Wanalyzer-null-argument */
}

void test_errno (char *s)
{
  errno = 0;
  char *result = mkdtemp (s);
  if (result == NULL)
    {
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      return;
    }
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
}

void test_success_non_null (void)
{
  char tmpl[] = "/var/tmp/dirXXXXXX";
  char *result = mkdtemp (tmpl);
  if (result != NULL)
    {
      __analyzer_eval (result == tmpl); /* { dg-warning "TRUE" } */
      __analyzer_eval (result != NULL); /* { dg-warning "TRUE" } */
    }
}

void test_no_lhs (char *s)
{
  mkdtemp (s); /* { dg-bogus "leak" } */
}
