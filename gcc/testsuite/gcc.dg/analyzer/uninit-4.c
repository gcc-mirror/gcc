/* Example of interprocedural detection of an uninitialized field
   in a heap-allocated struct.  */

#include <stdlib.h>
#include "analyzer-decls.h"

struct foo
{
  int i;
  int j;
  int k;
};

struct foo *__attribute__((noinline))
alloc_foo (int a, int b)
{
  struct foo *p = malloc (sizeof (struct foo));
  if (!p)
    return NULL;
  p->i = a;
  p->k = b;
  return p;
}

void test (int x, int y, int z)
{
  struct foo *p = alloc_foo (x, z);
  if (!p)
    return;

  __analyzer_eval (p->i == x); /* { dg-warning "TRUE" } */

  __analyzer_eval (p->j == y); /* { dg-warning "UNKNOWN" "unknown" } */
  /* { dg-warning "use of uninitialized value '\\*p\\.j'" "uninit" { target *-*-* } .-1 } */

  __analyzer_eval (p->k == z); /* { dg-warning "TRUE" } */
  
  free (p);
}
