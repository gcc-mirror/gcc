/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */
/* { dg-require-effective-target powerpc_elfv2 } */

/* Test that potential sibcalls are not generated when the caller preserves
   the TOC and the callee doesn't, or vice versa.  */

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

#pragma GCC target ("cpu=power9")
int normal_call (void)
{
  return y ();
}

int xx (void)
{
  return 1;
}

#pragma GCC target ("cpu=future")
int notoc_call (void)
{
  return xx ();
}

/* { dg-final { scan-assembler {\mb x@notoc\M} } } */
/* { dg-final { scan-assembler {\mbl y\M} } } */
/* { dg-final { scan-assembler {\mbl xx@notoc\M} } } */
