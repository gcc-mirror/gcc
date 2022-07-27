/* PR middle-end/99578 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "&MEM" "optimized" } } */
/* { dg-final { scan-tree-dump-times "PHI <-?1\\\(\[0-9\]+\\\), -?1\\\(\[0-9\]+\\\)>" 2 "optimized" } } */

struct S { int a, b[4]; };
struct T { int a, b[8192], c[4]; };

int
foo (struct S *p)
{
  if (p) return -1;
  return p->b == (void *)4;
}

int
bar (struct T *p)
{
  if (p) return -1;
  return p->c == (void *)32772;
}
