/* PR target/63542 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -dA" } */
/* { dg-additional-options "-fpic" { target fpic } } */

float
foo (long long u)
{
  if (!(-(1LL << 53) < u && u < (1LL << 53)))
    {
      if ((unsigned long long) u & ((1ULL << 11) - 1))
	{
	  u &= ~((1ULL << 11) - 1);
	  u |= (1ULL << 11);
	}
    }
  double f = (int) (u >> (32));
  f *= 0x1p32f;
  f += (unsigned int) u;
  return (float) f;
}
