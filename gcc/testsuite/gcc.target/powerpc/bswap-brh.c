/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* This tests whether GCC generates the ISA 3.1 16-bit byte swap
   instruction BRH.  */

unsigned short
bswap_short (unsigned short a)
{
  return __builtin_bswap16 (a); /* { dg-final { scan-assembler {\mbrh\M} } } */
}
