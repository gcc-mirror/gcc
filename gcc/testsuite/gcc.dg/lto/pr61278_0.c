/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -O0 } } } */
/* { dg-extra-ld-options " -flto -O1 " } */

static unsigned int
fn1 (int p1, int p2)
{
  return 0;
}

char a, b, c;

char
foo (char *p)
{
  int i;
  for (b = 1 ; b > 0; b++)
    {
      for (i = 0; i < 2; i++)
	;
      for (a = 1; a > 0; a++)
	{
	  char d[1] = { 0 };
	  if (*p)
	    break;
	  c ^= fn1 (fn1 (fn1 (0, 0), 0), 0);
	}
    }
  return 0;
}
