/* { dg-options "-O -floop-strip-mine" } */

int a[128][40];

void __attribute__ ((noinline, noclone))
foo (void)
{
  int i, j;
  for (i = 0; i < 40; i++)
    for (j = 0; j < 128; j++)
      a[j][i] = 4;
}

int
main ()
{
  int i, j;
  foo ();
  for (i = 0; i < 40; i++)
    for (j = 0; j < 128; j++)
      if (a[j][i] != 4)
	__builtin_abort ();
  return 0;
}
