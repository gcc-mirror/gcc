/* A prefetch reaches memory through an address that PRFM can hold in an
   extended-register form, just like a load of the same address does.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
pf_uxtw (const unsigned char *b, unsigned int i)
{
  __builtin_prefetch (b + i, 0, 3);
}

void
pf_sxtw (const unsigned char *b, int i)
{
  __builtin_prefetch (b + i, 0, 3);
}

/* { dg-final { scan-assembler-times {prfm\tPLDL1KEEP, \[x[0-9]+, w[0-9]+, uxtw\]} 1 } } */
/* { dg-final { scan-assembler-times {prfm\tPLDL1KEEP, \[x[0-9]+, w[0-9]+, sxtw\]} 1 } } */
/* { dg-final { scan-assembler-not {add\tx[0-9]+, x[0-9]+, w[0-9]+, [us]xtw\n} } } */
