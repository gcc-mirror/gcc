/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2 -mtune=generic -dp" } */

extern __int128 a, b;

void
foo (void)
{
  a = b;
}

/* { dg-final { scan-assembler-times "movv1ti_internal" 2 } } */
/* { dg-final { scan-assembler-not "\\*movdi_internal" } } */
