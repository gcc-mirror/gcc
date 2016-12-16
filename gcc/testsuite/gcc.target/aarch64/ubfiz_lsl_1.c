/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check that an X-reg UBFIZ can be simplified into a W-reg LSL.  */

long long
f2 (long long x)
{
  return (x << 5) & 0xffffffff;
}

/* { dg-final { scan-assembler "lsl\tw" } } */
/* { dg-final { scan-assembler-not "ubfiz\tx" } } */
