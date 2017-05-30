/* PR rtl-optimization/80112 */
/* { dg-do compile } */
/* { dg-options "-Os -fmodulo-sched" } */

void **a;

void
foo (int c)
{
  void *d[] = {&&e, &&f};
  a = d;
  switch (c)
    {
    f:
      c = 9;
      /* FALLTHRU */
    case 9:
      goto *a++;
    e:;
    }
}
