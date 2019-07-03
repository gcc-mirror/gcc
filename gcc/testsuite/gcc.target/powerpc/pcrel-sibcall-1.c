/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target powerpc_future_ok } */

/* Test that potential sibcalls are not generated when the caller preserves the
   TOC and the callee doesn't, or vice versa.  At present, -mcpu=future does
   not enable pc-relative mode.  Enable it here explicitly until it is turned
   on by default.  */

#pragma GCC target ("cpu=future,pcrel")
int x (void) __attribute__((noinline));
int y (void) __attribute__((noinline));
int xx (void) __attribute__((noinline));
  
int x (void)
{
  return 1;
}

int y (void)
{
  return 2;
}

int sib_call (void)
{
  return x ();
}

#pragma GCC target ("cpu=power9,no-pcrel")
int normal_call (void)
{
  return y ();
}

int xx (void)
{
  return 1;
}

#pragma GCC target ("cpu=future,pcrel")
int notoc_call (void)
{
  return xx ();
}

/* { dg-final { scan-assembler {\mb x@notoc\M} } } */
/* { dg-final { scan-assembler {\mbl y\M} } } */
/* { dg-final { scan-assembler {\mbl xx@notoc\M} } } */
