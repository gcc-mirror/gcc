/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops -fno-forward-propagate -fno-tree-fre -w" } */

long long unsigned int ao;
int hk;

void
b8 (void)
{
  int *w9;

 c8:
  ao = 0;
  w9 = &ao;
  for (;;)
    for (hk = 0; hk < 1; ++hk)
      for (ao = 0; ao < 4; ++ao)
        {
          int f4 = (ao != 0) ? *w9 : hk;

          if (f4 != 0)
            goto c8;
        }
}
