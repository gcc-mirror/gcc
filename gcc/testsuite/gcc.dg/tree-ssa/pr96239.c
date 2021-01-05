/* PR tree-optimization/96239 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " r>> 8;" "optimized" { target bswap } } } */

union U { unsigned char c[2]; unsigned short s; };

unsigned short
foo (unsigned short x)
{
  union U u;
  u.s = x;
  unsigned char v = u.c[0];
  unsigned char w = u.c[1];
  u.c[0] = w;
  u.c[1] = v;
  return u.s;
}
