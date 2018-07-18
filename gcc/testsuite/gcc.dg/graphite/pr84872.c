/* PR rtl-optimization/84872 */
/* { dg-do compile { target pthread } } */
/* { dg-options "-O1 -floop-parallelize-all -freorder-blocks-and-partition -fschedule-insns2 -fselective-scheduling2 -fsel-sched-pipelining -fno-tree-dce" } */

void
foo (int x)
{
  int a[2];
  int b, c = 0;

  for (b = 0; b < 2; ++b)
    a[b] = 0;
  for (b = 0; b < 2; ++b)
    a[b] = 0;

  while (c < 1)
    while (x < 1)
      ++x;
}
