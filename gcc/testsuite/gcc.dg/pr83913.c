/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -funroll-all-loops -fselective-scheduling -fsel-sched-pipelining -fschedule-insns -fno-dce -fno-forward-propagate -fno-rerun-cse-after-loop -fno-web" } */

int jo, z4;

int
be (long unsigned int l7, int nt)
{
  int en;

  jo = l7;
  for (en = 0; en < 24; ++en)
    {
      jo = (jo / z4) * (!!jo >= ((!!nt) & 2));
      if (jo == 0)
        nt = 0;
      else
        {
          nt = z4;
          ++z4;
          nt = (long unsigned int) nt == (l7 + 1);
        }
    }

  return nt;
}
