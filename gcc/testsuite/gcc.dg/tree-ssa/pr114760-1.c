/* PR tree-optimization/114760 */
/* { dg-do compile } */
/* { dg-require-effective-target clz } */
/* { dg-require-effective-target ctz } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

unsigned
ntz32_1 (unsigned x)
{
  int n = 32;
  while (x != 0)
    {
      n = n - 1;
      x = x * 2;
    }
  return n;
}

unsigned
ntz32_2 (unsigned x)
{
  int n = 32;
  while (x != 0)
    {
      n = n - 1;
      x = x + x;
    }
  return n;
}

unsigned
ntz32_3 (unsigned x)
{
  int n = 32;
  while (x != 0)
    {
      n = n - 1;
      x = x << 1;
    }
  return n;
}

#define PREC (__CHAR_BIT__ * __SIZEOF_INT__)
int
nlz32_1 (unsigned int b) {
    int c = PREC;

    while (b != 0) {
	b >>= 1;
	c --;
    }

    return c;
}

int
nlz32_2 (unsigned int b) {
    int c = PREC;

    while (b != 0) {
	b /= 2;
	c --;
    }

    return c;
}

/* { dg-final { scan-tree-dump-times "__builtin_ctz|\\.CTZ" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_clz|\\.CLZ" 2 "optimized" } } */
