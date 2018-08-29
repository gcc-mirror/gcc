/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-options "-O2 -fpic -fno-reorder-blocks" } */

/* On the Linux 64-bit ABIs, we eliminate NOP in the 'rec' call even if
   -fpic is used.  The recursive call should call the local alias.  The
   Linux 32-bit ABIs do not require NOPs after the BL instruction.  */

int f (void);

void
g (void)
{
}

int
rec (int a)
{
  int ret = 0;
  if (a > 10 && f ())
    ret += rec (a - 1);
  g ();
  return a + ret;
}

/* { dg-final { scan-assembler-times {\mbl f\M}   1 } } */
/* { dg-final { scan-assembler-times {\mbl g\M}   1 } } */
/* { dg-final { scan-assembler-times {\mbl rec\M} 1 } } */
/* { dg-final { scan-assembler-times {\mnop\M}    2 } } */
