/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check that an X-reg UBFX can be simplified into a W-reg LSR.  */

int
f (unsigned long long x)
{
  x = (x >> 24) & 255;
  return x + 1;
}

/* { dg-final { scan-assembler "lsr\tw" } } */
/* { dg-final { scan-assembler-not "ubfx\tx" } } */
