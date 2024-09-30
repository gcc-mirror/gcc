/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

struct ptr_wrapper
{
  int *ptr;
};

struct ptr_wrapper
test_1 (void)
{
  struct ptr_wrapper r;
  r.ptr = (int *) malloc (sizeof (int));
  return r;
}

struct ptr_wrapper
test_2 (void)
{
  struct ptr_wrapper r, s;
  r.ptr = (int *) malloc (sizeof (int));
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
  n.w.ptr = (int *) malloc (sizeof (int));
  return n;
}

void test_4 (void)
{
  struct ptr_wrapper r;
  r.ptr = (int *) malloc (sizeof (int)); /* { dg-message "allocated here" } */
} /* { dg-warning "leak of 'r.ptr'" "" { target c } } */
/* { dg-warning "leak of 'r.ptr_wrapper::ptr'" "" { target c++ } .-1 } */
/* { dg-bogus "leak of '<unknown>'" "unknown leak" { target *-*-* } .-1 } */

static struct ptr_wrapper __attribute__((noinline))
called_by_test_5a (void)
{
  struct ptr_wrapper r;
  r.ptr = (int *) malloc (sizeof (int)); /* { dg-message "allocated here" } */
  return r;
}

void test_5a (void)
{
  struct ptr_wrapper q = called_by_test_5a ();  
} /* { dg-warning "leak of 'q.ptr'" "" { target c } } */
/* { dg-warning "leak of 'q.ptr_wrapper::ptr'" "" { target c++ } .-1 } */

static struct ptr_wrapper __attribute__((noinline))
called_by_test_5b (void)
{
  struct ptr_wrapper r;
  r.ptr = (int *) malloc (sizeof (int));
  return r; /* { dg-warning "leak of '<return-value>.ptr'" "" { target c } } */
  /* TODO: show the allocation point; improve above messages. C++ does show it.  */
}

void test_5b (void)
{
  called_by_test_5b (); }
/* { dg-warning "leak of '<anonymous>.ptr_wrapper::ptr'" "" { target c++ } .-1 } */
/* The closing } above is intentionally on the same line as the call, because
   otherwise the exact line of the diagnostics depends on whether the
   called_by_test_5b () call satisfies aggregate_value_p or not.  */
