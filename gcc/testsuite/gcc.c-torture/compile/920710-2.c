/* { dg-additional-options "-std=gnu89" } */

union u
{
  struct {unsigned h, l;} i;
  double d;
};

foo (union u x)
{
  while (x.i.h++)
    {
      while (x.i.l-- > 0)
	;
      while (x.d++ > 0)
	;
    }
}

union n
{
  long long unsigned i;
  double d;
};

bar (union n x)
{
  int i;
  for (i = 0; i < 100; i++)
    {
      while (--x.i > 0)
	;
      while (++x.d > 0)
	;
    }
  return x.i;
}

