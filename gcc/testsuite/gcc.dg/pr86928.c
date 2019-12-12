/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fnon-call-exceptions -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops -fselective-scheduling -fno-if-conversion -fno-tree-dce -w" } */

int kn;

void
gd (short int sk)
{
  char *as;

  while (sk < 1)
    {
      char *ci;

      if (*ci == 0)
        *as += ci;

      for (kn = 0; kn < 18; ++kn)
        {
        }

      ++sk;
    }
}
