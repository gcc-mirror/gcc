/* { dg-do compile } */
/* { dg-options "-O -mgpopt -march=r2" } */

/* The ldio/stio builtins must not use GP-relative addresses for
   small data objects in R2.  This is because the address offset field
   has been reduced to 12 bits in R2, and %gprel is a 16-bit relocation.  */

extern volatile unsigned int frob;

volatile unsigned int frob = 0;

void foo (unsigned int val)
{
  __builtin_stwio (&frob, val);
}

/* { dg-final { scan-assembler "stwio\\t" } } */
/* { dg-final { scan-assembler-not "stwio\\t.*%gprel(frob)" } } */

