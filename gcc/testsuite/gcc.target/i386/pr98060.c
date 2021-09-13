/* PR target/98060 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-not "set" } } */
/* { dg-final { scan-assembler-times "adc"  4 } } */
/* { dg-final { scan-assembler-times "sbb"  4 } } */

int r1 (unsigned v0, unsigned v1, int v2)
{
  return v2 + (v0 >= v1);
}

int r2 (unsigned v0, unsigned v1, int v2)
{
  return v2 + (v0 <= v1);
}

int r3 (unsigned v0, unsigned v1, int v2)
{
  return v2 + (v0 > v1);
}

int r4 (unsigned v0, unsigned v1, int v2)
{
  return v2 + (v0 < v1);
}

int r5 (unsigned v0, unsigned v1, int v2)
{
  return v2 - (v0 >= v1);
}

int r6 (unsigned v0, unsigned v1, int v2)
{
  return v2 - (v0 <= v1);
}

int r7 (unsigned v0, unsigned v1, int v2)
{
  return v2 - (v0 > v1);
}

int r8 (unsigned v0, unsigned v1, int v2)
{
  return v2 - (v0 < v1);
}
