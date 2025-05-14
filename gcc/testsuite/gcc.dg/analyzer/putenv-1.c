/* { dg-additional-options "-Wno-analyzer-null-argument" } */
/* { dg-skip-if "has no putenv" { "avr-*-*" } } */

#include <stdio.h>
#include <stdlib.h>

extern void populate (char *buf);

void test_passthrough (char *s)
{
  putenv (s);
}

void test_str_lit (void)
{
  putenv ("NAME=value");
}

/* glibc allows strings without an equal sign.  */

void test_no_eq (void)
{
  putenv ("NAME");
}

void test_empty_string (void)
{
  putenv ("");
}

void test_NULL (void)
{
  putenv (NULL); /* possibly -Wanalyzer-null-argument */
}

void test_auto_buf_name_and_value (const char *name, const char *value)
{
  char buf[100]; /* { dg-message "'buf' declared on stack here" } */
  snprintf (buf, sizeof (buf), "%s=%s", name, value);
  putenv (buf); /* { dg-warning "'putenv' on a pointer to automatic variable 'buf' \\\[POS34-C\\\]" "warning" } */
  /* { dg-message "perhaps use 'setenv' rather than 'putenv'" "setenv suggestion" { target *-*-* } .-1 } */
}

void test_auto_buf_value (const char *value)
{
  char buf[100]; /* { dg-message "'buf' declared on stack here" } */
  snprintf (buf, sizeof (buf), "NAME=%s", value);
  putenv (buf); /* { dg-warning "'putenv' on a pointer to automatic variable 'buf' \\\[POS34-C\\\]" } */
}

void test_static_buf (const char *value)
{
  static char buf[100];
  snprintf (buf, sizeof (buf), "NAME=%s", value);
  putenv (buf);
}

static char global_buf[1024];

void test_global (const char *value)
{
  snprintf (global_buf, sizeof (global_buf), "NAME=%s", value);
  putenv (global_buf);
}

void test_alloca (void)
{
  char *buf = __builtin_alloca (256); /* { dg-message "region created on stack here" } */
  populate (buf);
  putenv (buf); /* { dg-warning "'putenv' on a pointer to an on-stack buffer \\\[POS34-C\\\]" } */
}

void test_malloc_1 (void)
{
  char *buf = malloc (1024);
  if (!buf)
    return;
  populate (buf);
  putenv (buf);
}

void test_malloc_2 (void)
{
  const char *kvstr = "NAME=value";
  size_t len = __builtin_strlen (kvstr);
  char *buf = __builtin_malloc (len + 1);
  if (!buf)
    return;
  __builtin_memcpy (buf, kvstr, len);
  buf[len] = '\0';
  putenv (buf); /* { dg-bogus "leak" } */
}

void test_arr (void)
{
  char arr[] = "NAME=VALUE"; /* { dg-message "'arr' declared on stack here" } */
  putenv (arr); /* { dg-warning "'putenv' on a pointer to automatic variable 'arr' \\\[POS34-C\\\]" } */
}

static void __attribute__((noinline))
__analyzer_test_inner (char *kvstr)
{
  putenv (kvstr); /* { dg-warning "'putenv' on a pointer to automatic variable 'arr_outer' \\\[POS34-C\\\]" } */
}

void test_outer (void)
{
  char arr_outer[] = "NAME=VALUE"; /* { dg-message "'arr_outer' declared on stack here" } */
  __analyzer_test_inner (arr_outer);
}

void test_unterminated (void)
{
  char buf[3] = "abc";
  putenv (buf); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of 'putenv'..." "event" { target *-*-* } .-1 } */
  /* { dg-warning "'putenv' on a pointer to automatic variable 'buf'" "POS34-C" { target *-*-* } .-2 } */
}

void test_uninitialized (void)
{
  char buf[16];
  putenv (buf); /* { dg-warning "use of uninitialized value 'buf\\\[0\\\]'" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of 'putenv'..." "event" { target *-*-* } .-1 } */
  /* { dg-warning "'putenv' on a pointer to automatic variable 'buf'" "POS34-C" { target *-*-* } .-2 } */
}
