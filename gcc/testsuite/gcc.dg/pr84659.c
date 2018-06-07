/* PR rtl-optimization/84659 */
/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling -fsel-sched-pipelining -fno-split-wide-types -fno-strict-aliasing -fno-tree-dce" } */

void
jk (int **lq, int *k4, long long int qv, int od)
{
  while (**lq < 1)
    {
      int uo;

      uo = ((od == 0) ? qv : *k4) != 1;
      ++**lq;
    }

  for (;;)
    {
    }
}
