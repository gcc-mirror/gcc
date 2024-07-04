#include "analyzer-decls.h"

struct S { long long a[16]; } s;

struct S
test_atomic_load (void)
{
  struct S r;
  __atomic_load (&s, &r, __ATOMIC_RELAXED);
  __analyzer_eval (r.a[0] == s.a[0]); /* { dg-warning "TRUE" } */
  __analyzer_eval (r.a[15] == s.a[15]); /* { dg-warning "TRUE" } */
  return r;
}

void
test_atomic_store (struct S x)
{
  __atomic_store (&s, &x, __ATOMIC_RELAXED);
  __analyzer_eval (s.a[0] == x.a[0]); /* { dg-warning "TRUE" } */
  __analyzer_eval (s.a[15] == x.a[15]); /* { dg-warning "TRUE" } */
}

struct S
test_atomic_exchange (struct S x)
{
  struct S init_x, init_s;
  struct S r;

  /* Capture initial values of x and s for comparison below.  */
  __atomic_load (&x, &init_x, __ATOMIC_RELAXED);
  __atomic_load (&s, &init_s, __ATOMIC_RELAXED);
  
  __atomic_exchange (&s, &x, &r, __ATOMIC_RELAXED);
  
  __analyzer_eval (s.a[0] == init_x.a[0]); /* { dg-warning "TRUE" } */
  __analyzer_eval (s.a[15] == init_x.a[15]); /* { dg-warning "TRUE" } */
  __analyzer_eval (r.a[0] == init_s.a[0]); /* { dg-warning "TRUE" } */
  __analyzer_eval (r.a[15] == init_s.a[15]); /* { dg-warning "TRUE" } */

  return r;
}

int
test_atomic_compare_exchange (struct S *e, struct S *d)
{
  return __atomic_compare_exchange (&s, e, d, 0,
				    __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}
