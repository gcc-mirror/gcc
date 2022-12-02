/* { dg-do run } */

int a, b[1] = { 0 }, c, *d = b, e, *f, g;

__attribute__((noipa)) int
foo (const char *x)
{
  (void) x;
  return 0;
}

int
main ()
{
  for (int h = 0; a < 2; a++)
    {
      int i;
      for (g = 0; g < 2; g++)
	if (a < h)
	  {
	    e = i % 2;
	    c = *f;
	  }
      for (h = 0; h < 3; h++)
	{
	  if (d)
	    break;
	  i--;
	  foo ("0");
	}
    }
  return 0;
}
