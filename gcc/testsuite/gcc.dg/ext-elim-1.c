/* PR rtl-optimization/51924 */
/* Testcase by Zdenek Sojka <zsojka@seznam.cz> */

/* { dg-do run } */
/* { dg-options "-O -free -fno-rename-registers -ftree-vectorize -funroll-loops" } */

typedef __UINT64_TYPE__ uint64_t;

uint64_t __attribute__ ((noinline, noclone))
bn_sub_words (uint64_t * r, const uint64_t * a, const uint64_t * b, int n)
{
  uint64_t t1, t2;
  unsigned c = 0;

  while (n)
    {
      t1 = a[0];
      t2 = b[0];
      r[0] = (t1 - t2 - c);
      if (t1 != t2)
	c = (t1 < t2);
      a++;
      b++;
      r++;
      n--;
    }
  return (c);
}

int
main (void)
{
  uint64_t r[2];
  uint64_t a[2] = { -1, -1 };
  uint64_t b[2] = { 0, 0 };
  if (bn_sub_words (r, a, b, 2) != 0)
    __builtin_abort ();
  return 0;
}
