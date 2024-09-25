/* { dg-do compile } */
/* { dg-options "-fzero-call-used-regs=used -fpic -O2" } */

extern double clobber_fp0 (void);

void foo (void)
{
  clobber_fp0 ();
}
