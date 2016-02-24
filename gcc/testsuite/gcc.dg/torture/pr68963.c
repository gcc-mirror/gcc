/* { dg-do run } */

static const float a[3] = { 1, 2, 3 };
int b = 3;

__attribute__((noinline, noclone)) void
bar (int x)
{
  if (x != b++)
    __builtin_abort ();
}

void
foo (float *x, int y)
{
  int i;
  for (i = 0; i < 2 * y; ++i)
    {
      if (i < y)
	x[i] = a[i];
      else
	{
	  bar (i);
	  x[i] = a[i - y];
	}
    }
}

int
main ()
{
  float x[10];
  unsigned int i;
  for (i = 0; i < 10; ++i)
    x[i] = 1337;
  foo (x, 3);
  for (i = 0; i < 10; ++i)
    if (x[i] != (i < 6 ? (i % 3) + 1 : 1337))
      __builtin_abort ();
  return 0;
}
