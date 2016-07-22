/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -mtune=generic -dp" } */

extern __int128 a, b, c, d, e, f;

void
foo (void)
{
  a = 0;
  b = -1;
  c = 0;
  d = -1;
  e = 0;
  f = -1;
}

/* { dg-final { scan-assembler-times "movv1ti_internal" 8 } } */
/* { dg-final { scan-assembler-not "\\*movdi_internal" { target nonpic } } } */
