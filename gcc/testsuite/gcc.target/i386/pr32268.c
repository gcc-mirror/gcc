/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-O2" } */

extern void abort(void);

int __attribute__ ((__noinline__))
test_lt(__float128 x, __float128 y)
{
  return x < y;
}

int __attribute__ ((__noinline__))
test_gt (__float128 x, __float128 y)
{
  return x > y;
}

int main()
{
  __float128 a = 0.0;
  __float128 b = 1.0;

  int r;

  r = test_lt (a, b);
  if (r != ((double) a < (double) b))
    abort();

  r = test_gt (a, b);
  if (r != ((double) a > (double) b))
    abort();

  return 0;
}
