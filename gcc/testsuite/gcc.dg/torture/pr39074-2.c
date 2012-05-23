/* { dg-do run } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-fdump-tree-alias" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-fno-fat-lto-objects" } { "" } } */

#include <stdint.h>

int i;
uintptr_t __attribute__((noinline,const)) bar(int ***p) { return (uintptr_t)p; }
void __attribute__((noinline))
foo(void)
{
  int *y;
  int **a = &y, **x;
  int ***p;
  uintptr_t b;
  b = bar(&a);
  p = (int ***)b;
  x = *p;
  *x = &i; /* *ANYTHING = &i has to make sure that y points to i.  */
  *y = 0;
}
extern void abort (void);
int main()
{
  i = 1;
  foo ();
  if (i != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "y.._. = { i }" "alias" } } */
/* { dg-final { scan-tree-dump "y.._., points-to vars: { D..... }" "alias" } } */
/* { dg-final { cleanup-tree-dump "alias" } } */
