/* { dg-additional-options "-Wno-analyzer-null-argument" } */
/* { dg-skip-if "incomplete stdio" { avr-*-* } } */

/* TODO: mktemp is deprecated per MSC24-C
   (https://wiki.sei.cmu.edu/confluence/x/hNYxBQ).
   Once a warning for deprecated functions exists, mktemp should
   also warn about its use.  Later versions of Darwin do warn here.  */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "analyzer-decls.h"

extern void populate (char *buf);

void test_passthrough (char *s)
{
  mktemp (s);
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_string_literal_correct_placeholder (void)
{
  mktemp ("/home/user/sessXXXXXX"); /* { dg-warning "'mktemp' on a string literal \\\[STR30-C\\\]" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
  /* { dg-message "use a writable character array" "fix suggestion" { target *-*-* } .-2 } */
}

void test_string_literal_missing_placeholder (void)
{
  mktemp ("/home/user/sess"); /* { dg-warning "'mktemp' on a string literal \\\[STR30-C\\\]" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_string_literal_empty (void)
{
  mktemp (""); /* { dg-warning "'mktemp' on a string literal \\\[STR30-C\\\]" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_correct (void)
{
  char tmpl[] = "/var/run/sock.XXXXXX";
  mktemp (tmpl);
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_correct_minimal (void)
{
  char tmpl[] = "XXXXXX";
  mktemp (tmpl);
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_correct_offset_into_buffer (void)
{
  char buf[] = "////XXXXXX";
  mktemp (buf + 4);
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_missing_placeholder_offset_into_buffer (void)
{
  char buf[] = "////XXXXXX";
  /* Placeholder is incorrect from the pointer's perspective.  */
  mktemp (buf + 5); /* { dg-warning "'mktemp' template string does not end with 'XXXXXX'" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_missing_placeholder (void)
{
  char tmpl[] = "/var/run/sock";
  mktemp (tmpl); /* { dg-warning "'mktemp' template string does not end with 'XXXXXX'" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_too_short (void)
{
  char tmpl[] = "XY";
  mktemp (tmpl); /* { dg-warning "'mktemp' template string does not end with 'XXXXXX'" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_empty_buffer (void)
{
  char tmpl[] = "";
  mktemp (tmpl); /* { dg-warning "'mktemp' template string does not end with 'XXXXXX'" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_partial_placeholder (void)
{
  char tmpl[] = "/var/run/sockXXXXX-";
  mktemp (tmpl); /* { dg-warning "'mktemp' template string does not end with 'XXXXXX'" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_four_xs (void)
{
  char tmpl[] = "/var/run/sockXXXX";
  mktemp (tmpl); /* { dg-warning "'mktemp' template string does not end with 'XXXXXX'" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_populated_buf (void)
{
  char tmpl[24];
  populate (tmpl);
  mktemp (tmpl);
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_NULL (void)
{
  mktemp (NULL); /* possibly -Wanalyzer-null-argument */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
}

void test_errno_bad_template (void)
{
  errno = 0;
  char tmpl[] = "/tmp/foo";
  char *result = mktemp (tmpl); /* { dg-warning "'mktemp' template string does not end with 'XXXXXX'" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
  __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
}

void test_failure_nul_byte (void)
{
  char tmpl[] = "/tmp/foo";
  char *result = mktemp (tmpl); /* { dg-warning "'mktemp' template string does not end with 'XXXXXX'" } */
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
  __analyzer_eval (result[0] == '\0'); /* { dg-warning "TRUE" } */
  __analyzer_eval (result == tmpl); /* { dg-warning "TRUE" } */
}

void test_success_path (void)
{
  char tmpl[] = "/tmp/testXXXXXX";
  char *result = mktemp (tmpl);
  /* { dg-warning {'mktemp' is deprecated:} "" { target *-*-darwin2* } .-1 } */
  if (result[0] != '\0')
    __analyzer_eval (result == tmpl); /* { dg-warning "TRUE" } */
}
