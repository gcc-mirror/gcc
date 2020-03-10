/* PR tree-optimization/93582 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return 9223372036854775806;" "fre1" } } */

union U { struct A { unsigned long long a : 1, b : 62, c : 1; } a; unsigned long long i; };

unsigned long long
foo (char *p)
{
  __builtin_memset (p - 0xfffffffffffffffULL, 0, 0xffffffffffffffeULL);
  __builtin_memset (p + 1, 0, 0xffffffffffffffeULL);
  union U *q = (union U *) (void *) (p - 4);
  q->a.b = -1;
  return q->i;
}
