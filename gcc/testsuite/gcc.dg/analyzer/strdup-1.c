#include <string.h>
#include <stdlib.h>

extern void requires_nonnull (void *ptr)
  __attribute__((nonnull));

void test_1 (const char *s)
{
  char *p = strdup (s); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'p'" } */

void test_2 (const char *s)
{
  char *p = strdup (s);
  free (p);
}

void test_3 (const char *s)
{
  char *p = strdup (s); /* { dg-message "this call could return NULL" } */
  requires_nonnull (p); /* { dg-warning "use of possibly-NULL 'p'" } */
}

/* Repeat tests for __builtin_strdup.  */
void test_4 (const char *s)
{
  char *p = __builtin_strdup (s); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'p'" } */

void test_5 (const char *s)
{
  char *p = __builtin_strdup (s);
  free (p);
}

void test_6 (const char *s)
{
  char *p = __builtin_strdup (s); /* { dg-message "this call could return NULL" } */
  requires_nonnull (p); /* { dg-warning "use of possibly-NULL 'p'" } */
}

char *test_unterminated (void)
{
  char buf[3] = "abc";
  return strdup (buf); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of 'strdup'..." "event" { target *-*-* } .-1 } */
}

char *test_uninitialized (void)
{
  char buf[16];
  return strdup (buf); /* { dg-warning "use of uninitialized value 'buf\\\[0\\\]'" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&buf'\\) of 'strdup'..." "event" { target *-*-* } .-1 } */
}
