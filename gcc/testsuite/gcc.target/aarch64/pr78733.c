/* { dg-do compile } */
/* { dg-options "-O2 -mpc-relative-literal-loads" } */

__int128
t (void)
{
  return (__int128)1 << 80;
}

/* { dg-final { scan-assembler "adr" } } */
