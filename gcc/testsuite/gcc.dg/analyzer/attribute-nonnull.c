#include <stdlib.h>

extern void foo(void *ptrA, void *ptrB, void *ptrC) /* { dg-message "argument 1 of 'foo' must be non-null" } */
  __attribute__((nonnull (1, 3)));

extern void bar(void *ptrA, void *ptrB, void *ptrC) /* { dg-message "argument 1 of 'bar' must be non-null" } */
  __attribute__((nonnull));

// TODO: complain about NULL and possible NULL args
// FIXME: ought to complain about NULL args

void test_1 (void *p, void *q, void *r)
{
  foo(p, q, r);
  foo(NULL, q, r); /* { dg-warning "use of NULL where non-null expected" "warning" } */
  /* { dg-message "argument 1 NULL where non-null expected" "note" { target *-*-* } .-1 } */
  foo(p, NULL, r);
  foo(p, q, NULL); /* { dg-warning "use of NULL where non-null expected" } */
}

void test_1a (void *q, void *r)
{
  void *p = NULL;
  foo(p, q, r); /* { dg-warning "use of NULL 'p' where non-null expected" "warning" } */
  /* { dg-message "argument 1 \\('p'\\) NULL where non-null expected" "note" { target *-*-* } .-1 } */
}

void test_2 (void *p, void *q, void *r)
{
  bar(p, q, r);
  bar(NULL, q, r); /* { dg-warning "use of NULL where non-null expected" "warning" } */
  bar(p, NULL, r); /* { dg-warning "use of NULL where non-null expected" "warning" } */
  /* { dg-message "argument 2 NULL where non-null expected" "note" { target *-*-* } .-1 } */
  bar(p, q, NULL); /* { dg-warning "use of NULL where non-null expected" "warning" } */
}

void test_3 (void *q, void *r)
{
  void *p = malloc(1024); /* { dg-message "\\(1\\) this call could return NULL" } */

  foo(p, q, r); /* { dg-warning "use of possibly-NULL 'p' where non-null expected" "warning" } */
  /* { dg-message "argument 1 \\('p'\\) from \\(1\\) could be NULL where non-null expected" "note" { target *-*-* } .-1 } */

  foo(p, q, r);

  free(p);
}

void test_4 (void *q, void *r)
{
  void *p = malloc(1024); /* { dg-message "\\(1\\) this call could return NULL" } */

  bar(p, q, r); /* { dg-warning "use of possibly-NULL 'p' where non-null expected" "warning" } */
  /* { dg-message "argument 1 \\('p'\\) from \\(1\\) could be NULL where non-null expected" "note" { target *-*-* } .-1 } */

  bar(p, q, r);

  free(p);
}

/* Verify that we detect passing NULL to a __attribute__((nonnull)) function
   when it's called via a function pointer.  */

typedef void (*bar_t)(void *ptrA, void *ptrB, void *ptrC);

static bar_t __attribute__((noinline))
get_bar (void)
{
  return bar;
}

void test_5 (void *q, void *r)
{
  void *p = malloc(1024); /* { dg-message "\\(1\\) this call could return NULL" } */
  bar_t cb = get_bar ();
  cb(p, q, r); /* { dg-warning "use of possibly-NULL 'p' where non-null expected" "warning" } */
  /* { dg-message "argument 1 \\('p'\\) from \\(1\\) could be NULL where non-null expected" "note" { target *-*-* } .-1 } */
  /* TODO: do we want an event showing where cb is assigned "bar"?  */

  cb(p, q, r);

  free(p);
}
