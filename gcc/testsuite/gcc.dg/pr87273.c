/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-Os -fschedule-insns -fsel-sched-pipelining -fselective-scheduling -fno-ssa-phiopt -fno-tree-loop-im" } */
/* { dg-additional-options "-march=core2" { target i?86-*-* x86_64-*-* } } */

int sd;

void
w5 (int n4)
{
  long int *vq = (long int *) &n4;

  while (n4 < 1)
    {
      int ks;

      ks = !!(n4 + 1) ? ((++sd) == *vq) : 0;
      if (ks == 1 / *vq)
        *vq *= sd;
    }
}
