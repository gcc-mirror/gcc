/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target power10_ok } */

/* Test that potential sibcalls are generated when the caller does not
   preserve the TOC, even for external calls; and that sibcalls are not
   generated when the caller preserves the TOC but the callee does not.  */

#pragma GCC target ("cpu=power10,pcrel")
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

extern int yy (void);

#pragma GCC target ("cpu=power10,pcrel")
int notoc_sibcall (void)
{
  return xx ();
}

int extern_sibcall (void)
{
  return yy ();
}

/* { dg-final { scan-assembler {\mb x@notoc\M} } } */
/* { dg-final { scan-assembler {\mbl y\M} } } */
/* { dg-final { scan-assembler {\mb xx@notoc\M} } } */
/* { dg-final { scan-assembler {\mb yy@notoc\M} } } */
