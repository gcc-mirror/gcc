/* { dg-do compile } */
/* { dg-options "-O2" } */

void bar (void *, void *);
int
foo (void *p1, void *p2)
{
  int ret1, ret2;
  __asm ("" : "=D" (ret1), "=S" (ret2));
  bar (p1, p2);
  return ret1 + ret2;
}
