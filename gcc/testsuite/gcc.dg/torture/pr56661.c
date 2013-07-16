/* { dg-do run } */

__attribute__((noinline, noclone)) void
bar (int *b)
{
  b[0] = b[1] = b[2] = 1;
}

__attribute__((noinline, noclone)) int
baz (int x)
{
  if (x != 1)
    __builtin_abort ();
}

void
foo (int x)
{
  if (x == 0)
    {
      int *b = __builtin_malloc (3 * sizeof (int));
      while (b[0])
	;
    }
  else if (x == 1)
    {
      int i, j;
      int *b = __builtin_malloc (3 * sizeof (int));
      for (i = 0; i < 2; i++)
	{
	  bar (b);
	  for (j = 0; j < 3; ++j)
	    baz (b[j]);
	  baz (b[0]);
	}
    }
}

int
main ()
{
  int x = 1;
  asm volatile ("" : "+r" (x));
  foo (x);
  return 0;
}
