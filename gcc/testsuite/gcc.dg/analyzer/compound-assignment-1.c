#include <stdlib.h>

struct ptr_wrapper
{
  int *ptr;
};

struct ptr_wrapper
test_1 (void)
{
  struct ptr_wrapper r;
  r.ptr = malloc (sizeof (int));
  return r;
}

struct ptr_wrapper
test_2 (void)
{
  struct ptr_wrapper r, s;
  r.ptr = malloc (sizeof (int));
  s = r;
  return s;
}

struct nested
{
  struct ptr_wrapper w;
};

struct nested
test_3 (void)
{
  struct nested n;
  n.w.ptr = malloc (sizeof (int));
  return n;
}

void test_4 (void)
{
  struct ptr_wrapper r;
  r.ptr = malloc (sizeof (int)); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'r.ptr'" } */
/* { dg-bogus "leak of '<unknown>'" "unknown leak" { xfail *-*-* } .-1 } */

static struct ptr_wrapper __attribute__((noinline))
called_by_test_5a (void)
{
  struct ptr_wrapper r;
  r.ptr = malloc (sizeof (int));
  return r;
}

void test_5a (void)
{
  struct ptr_wrapper q = called_by_test_5a ();  
} /* { dg-warning "leak of 'q.ptr'" } */
/* TODO: show the allocation point.  */

static struct ptr_wrapper __attribute__((noinline))
called_by_test_5b (void)
{
  struct ptr_wrapper r;
  r.ptr = malloc (sizeof (int));
  return r; /* { dg-warning "leak" } */
  /* TODO: show the allocation point.  */
}

void test_5b (void)
{
  called_by_test_5b ();
}
