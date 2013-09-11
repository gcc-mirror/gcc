/* Check that Thumb 16-bit shifts by immediate can be if-converted.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-options "-O2 -mthumb" } */

int
foo (int a, int b)
{
  if (a != b)
      a = a << 1;
  else
      a = a >> 1;

  return a + b;
}

/* { dg-final { scan-assembler "lslne" } } */
/* { dg-final { scan-assembler "asreq" } } */
