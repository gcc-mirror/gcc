#include <stdlib.h>

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct iter
{
  int start;
  int end;
  int step;
  int val;
};

struct iter *  __attribute__((noinline))
iter_new (int start, int end, int step)
{
  struct iter *it = (struct iter *)malloc (sizeof (struct iter));
  if (!it)
    abort ();
  it->start = start;
  it->end = end;
  it->step = step;
  it->val = start;
  return it;
}

int __attribute__((noinline))
iter_done_p (struct iter *it)
{
  return it->val >= it->end;
}

void __attribute__((noinline))
iter_next (struct iter *it)
{
  it->val += it->step;
}

/* Example of an iterator object, to see how well we cope with a well-disguised
   iteration from 0 to n with a step of 1.  */

void test(int n)
{
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  struct iter *it = iter_new (0, n, 1);
  while (!iter_done_p (it))
    {
      __analyzer_eval (it->val < n); /* { dg-warning "TRUE" "true" } */
      /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
      /* TODO(xfail^^^): ideally we ought to figure out i > 0 after 1st iteration.  */

      __analyzer_eval (it->val == 0); /* { dg-warning "TRUE" "true on 1st iter" } */
      /* { dg-warning "UNKNOWN" "unknown" { target *-*-* } .-1 } */
      /* TODO: should we ought to figure out i > 0 after 1st iteration?  */

      __analyzer_eval (it->val >= 0); /* { dg-warning "TRUE" } */

      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

      iter_next (it);
    }

  __analyzer_eval (it->val >= n); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */

  __analyzer_eval (it->val == n); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  /* TODO(xfail^^^): it only figures out i >= 256, rather than i == 256.  */

  free (it);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
