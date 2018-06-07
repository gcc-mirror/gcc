/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fmodulo-sched -fselective-scheduling2" } */
int vm, z0;
short int mz;

int
ny (void)
{
  int ch;

  for (ch = 0; ch < 6; ++ch)
    vm += ch / vm;

  return z0 + mz;
}
