/* { dg-do compile { target { lp64 } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* This tests whether GCC generates the ISA 3.1 BRW byte swap instruction for
   GPR data, but generates XXBRW for data in a vector register.  */

unsigned long long
bswap_ll (unsigned long long a)
{
  return __builtin_bswap64 (a); /* { dg-final { scan-assembler {\mbrd\M} } } */
}

double
bswap_ll_dbl (unsigned long long a)
{
  unsigned int b = a;
  /* Force the value to be loaded into a vector register.  */
  __asm__ (" # %x0" : "+wa" (b));

  /* { dg-final { scan-assembler {\mxxbrd\M} } } */
  return (double) __builtin_bswap64 (b);
}
