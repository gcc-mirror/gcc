/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-std=gnu99 -O1 -fselective-scheduling2 -fschedule-insns2 -fcse-follow-jumps -fno-ssa-phiopt -fno-guess-branch-probability" } */
unsigned int ca;

void
v6 (long long unsigned int as, int p9)
{
  while (p9 < 1)
    as = (as != ca) || (as > 1);
}
