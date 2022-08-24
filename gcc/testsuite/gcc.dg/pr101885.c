/* { dg-do run } */
/* { dg-options "-O3" } */
int a = 3, c;
short b = 5, d, f;
volatile short e;

__attribute__((noipa)) void
foo (void)
{
}

int
main ()
{
  for (f = 0; f != 2; f++)
    {
      int g = a;
      if (b)
	if (a)
	  for (a = b = 0; b <= 3; b++)
	    ;
      for (c = 0; c != 16; ++c)
	e;
    }
  b = d;
  foo ();
  if (a != 0)
    __builtin_abort ();
  return 0;
}

