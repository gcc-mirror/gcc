/* PR target/104894 */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -fno-plt" } */

/* Verify we do not ICE on the following test case and that we emit one
   indirect call and one indirect sibcall, with r12 and CTR containing
   the function addresses.  */

void foo (void);

void
bar (void)
{
  foo ();
  foo ();
}

/* { dg-final { scan-assembler-times {\mmtctr 12\M} 2 } } */
/* { dg-final { scan-assembler-times {\mbctrl\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbctr\M} 1 } } */
/* { dg-final { scan-assembler-not {\mbl\M} } } */
