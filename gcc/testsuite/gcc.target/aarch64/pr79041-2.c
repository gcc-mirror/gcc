/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=large -mpc-relative-literal-loads" } */
/* { dg-require-effective-target lp64 } */

__int128
t (void)
{
  return (__int128)1 << 80;
}

/* { dg-final { scan-assembler "adr" } } */
/* { dg-final { scan-assembler-not "adrp" } } */
