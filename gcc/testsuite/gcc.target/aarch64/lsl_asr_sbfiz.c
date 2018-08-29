/* { dg-do compile } */
/* { dg-options "-O3" } */

/* Check that a LSL followed by an ASR can be combined into a single SBFIZ
   instruction.  */

/* Using W-reg */

int
sbfiz32 (int x)
{
  return x << 29 >> 10;
}

/* Using X-reg */

long long
sbfiz64 (long long x)
{
  return x << 58 >> 20;
}

/* { dg-final { scan-assembler "sbfiz\tw" } } */
/* { dg-final { scan-assembler "sbfiz\tx" } } */
