/* PR middle-end/105253 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-popcnt" } */
/* { dg-final { scan-assembler-not "__popcount\[sd]i2" } } */

int
foo (unsigned long long *p)
{
  int i, cnt = 0;
  unsigned long long elem;
  for (i = 0; i < 4; i++)
    {
      for (elem = p[i]; elem; cnt++)
	elem &= elem - 1;
    }
  return cnt;
}
