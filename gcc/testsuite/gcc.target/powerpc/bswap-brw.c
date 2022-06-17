/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* This tests whether GCC generates the ISA 3.1 BRW byte swap instruction for
   GPR data, but generates XXBRW for data in a vector register.  */

unsigned int
bswap_int (unsigned int a)
{
  return __builtin_bswap32 (a); /* { dg-final { scan-assembler {\mbrw\M} } } */
}

double
bswap_int_dbl (unsigned int a)
{
  unsigned int b = a;
  /* Force the value to be loaded into a vector register.  */
  __asm__ (" # %x0" : "+wa" (b));

  /* { dg-final { scan-assembler {\mxxbrw\M} {xfail {has_arch_pwr10 && {! has_arch_ppc64}}} } } */
  return (double) __builtin_bswap32 (b);
}
