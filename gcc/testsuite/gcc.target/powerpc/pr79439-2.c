/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-options "-O2 -fpic -fno-reorder-blocks -mno-pcrel" } */
/* { dg-require-effective-target fpic } */

/* On the Linux 64-bit ABIs, we should not eliminate NOP in the 'rec' call if
   -fpic is used because rec can be interposed at link time (since it has an
   alias), and the recursive call should call the interposed function.  The
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

int rec_alias (int) __attribute__ ((alias ("rec")));

/* { dg-final { scan-assembler-times {\mbl f\M}   1 } } */
/* { dg-final { scan-assembler-times {\mbl g\M}   1 } } */
/* { dg-final { scan-assembler-times {\mbl rec\M} 1 } } */
/* { dg-final { scan-assembler-times {\mnop\M}    3 } } */
