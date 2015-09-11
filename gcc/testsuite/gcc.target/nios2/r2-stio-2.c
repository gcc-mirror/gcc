/* { dg-do compile } */
/* { dg-options "-O -mgpopt -march=r2 -mbypass-cache" } */

/* Implicit ldio/stio operations must not use GP-relative addresses for
   small data objects in R2.  This is because the address offset field
   has been reduced to 12 bits in R2, and %gprel is a 16-bit relocation.  */

extern volatile unsigned int frob;

volatile unsigned int frob = 0;

void foo (unsigned int val)
{
  frob = val;
}

/* { dg-final { scan-assembler "stwio\\t" } } */
/* { dg-final { scan-assembler-not "stwio\\t.*%gprel(frob)" } } */

