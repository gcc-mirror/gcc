#include "analyzer-decls.h"

#define NULL ((void *)0)

/* Verify that the analyzer makes the simplifying assumption that we don't
   hit NULL when incrementing pointers to non-NULL memory regions.  */

static int * __attribute__((noinline))
maybe_inc_int_ptr (int *ptr)
{
  if (!ptr)
    return NULL;
  return ++ptr;
}

int
test_1 (void)
{
  int stack; /* { dg-message "region created on stack here" } */
  int *a = &stack;
  a = maybe_inc_int_ptr (a);
  a = maybe_inc_int_ptr (a);
  __analyzer_eval (a == NULL); /* { dg-warning "FALSE" } */
  __analyzer_eval (a != NULL); /* { dg-warning "TRUE" } */
  return *a; /* { dg-line test_1 } */

  /* { dg-warning "use of uninitialized value '\\*a'" "warning" { target *-*-* } test_1 } */
  /* { dg-warning "overread" "warning" { target *-*-* } test_1 } */
}

static const char * __attribute__((noinline))
maybe_inc_char_ptr (const char *ptr)
{
  if (!ptr)
    return NULL;
  return ++ptr;
}

char
test_s (void)
{
  const char *msg = "hello world";
  const char *a = msg;
  __analyzer_eval (*a == 'h'); /* { dg-warning "TRUE" } */
  a = maybe_inc_char_ptr (a);
  __analyzer_eval (*a == 'e'); /* { dg-warning "TRUE" } */
  a = maybe_inc_char_ptr (a);
  __analyzer_eval (*a == 'l'); /* { dg-warning "TRUE" } */
  a = maybe_inc_char_ptr (a);
  __analyzer_eval (*a == 'l'); /* { dg-warning "TRUE" } */
  a = maybe_inc_char_ptr (a);
  __analyzer_eval (*a == 'o'); /* { dg-warning "TRUE" } */
}
