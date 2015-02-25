/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-O -fschedule-insns -fcheck-pointer-bounds -mmpx" } */

void bar(int *a, int *b, int *c, int *d, int *e, int *f);

int foo (int *a, int *b, int *c, int *d, int *e, int *f)
{
  bar (a, b, c, d, e, f);
  return *f;
}
