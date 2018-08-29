/* { dg-do compile } */
/* { dg-options "-O1" } */

void __assert_fail (const char *, const char *, unsigned int, const char *);

int a, b, c, d, e, f, h;
unsigned char g;

int main ()
{
  int i, *j = &b;
  if (a)
    {
      if (h)
	{
	  int **k = &j;
	  d = 0;
	  *k = &e;
	}
      else
	for (b = 0; b > -28; b = g)
	  ;
      c || !j ? : __assert_fail ("c || !j", "small.c", 20, "main");
      if (f)
	for (i = 0; i < 1; i++)
	  ;
    }
  return 0;
}
