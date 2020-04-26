/* PR debug/65779 */
/* { dg-do assemble } */
/* { dg-options "-O2 -fcompare-debug" } */

unsigned long
foo (unsigned long x, unsigned char *y, unsigned int z)
{
  unsigned long a = x & 0xffff;
  unsigned long b = (x >> 16) & 0xffff;
  int k;
  if (y == 0) return 1L;
  while (z > 0)
    {
      k = z < 5552 ? z : 5552;
      z -= k;
      while (k >= 16)
	{
          a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  a += *y++; b += a;
	  k -= 16;
        }
      if (k != 0)
	do { a += *y++; b += a; } while (--k);
      a %= 65521L;
      b %= 65521L;
    }
  return (b << 16) | a;
}
