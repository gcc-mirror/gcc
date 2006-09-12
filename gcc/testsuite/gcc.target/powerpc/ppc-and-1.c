/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler "rlwinm r?\[0-9\]+,r?\[0-9\]+,0,0,30"  } } */
/* { dg-final { scan-assembler "rlwinm r?\[0-9\]+,r?\[0-9\]+,0,29,30"  } } */
/* { dg-final { scan-assembler-not "rldicr" } } */

/* Origin:Pete Steinmetz <steinmtz@us.ibm.com> */

/* PR 16457 - use rlwinm insn.  */

char *foo1 (char *p, unsigned int x)
{
  return p - (x & ~1);
}

char *foo2 (char *p, unsigned int x)
{
  return p - (x & 6);
}
