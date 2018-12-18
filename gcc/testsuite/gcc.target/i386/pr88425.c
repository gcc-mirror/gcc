/* PR target/88425 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-times "sbb\[lq]\[ \t]" 8 } } */
/* { dg-final { scan-assembler-not "setbe\[ \t]" } } */

unsigned long
f1 (unsigned long x)
{
  return x < 123UL ? -1UL : 0;
}

unsigned long
f2 (unsigned int x)
{
  return x < 12345U ? -1UL : 0;
}

unsigned long
f3 (unsigned short *x)
{
  return x[0] < 1234U ? -1UL : 0;
}

unsigned long
f4 (unsigned char *x)
{
  return x[0] < 123U ? -1UL : 0;
}

unsigned int
f5 (unsigned long x)
{
  return x < 123UL ? -1U : 0;
}

unsigned int
f6 (unsigned int x)
{
  return x < 12345U ? -1U : 0;
}

unsigned int
f7 (unsigned short *x)
{
  return x[0] < 1234U ? -1U : 0;
}

unsigned int
f8 (unsigned char *x)
{
  return x[0] < 123U ? -1U : 0;
}
