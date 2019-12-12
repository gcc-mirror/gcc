/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O1 -fpeephole2 -fschedule-insns2 -fsel-sched-pipelining -fselective-scheduling2 -ftree-loop-if-convert -fno-if-conversion -fno-move-loop-invariants -fno-split-wide-types -fno-tree-dominator-opts" } */
/* { dg-additional-options "-march=bonnell" { target x86_64-*-* } } */

__int128 jv;

void
zm (__int128 g9, unsigned short int sm, short int hk)
{
  while (hk < 1)
    {
      if (jv == 0)
        sm *= g9;

      if (sm < jv)
        hk = sm;

      g9 |= sm == hk;
    }
}
