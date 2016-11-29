/* { dg-do compile } */

static int a = 2;
int b[1], c, d;

int main ()
{ 
  int e = a, *f = &b[0];
  if (d)
    for (e = 0; e < 1; e++)
      ;
  if (e)
    {
L1:
      if (b < f)
	__builtin_abort ();
      if (*f)
	c++;
      return 0;
    }
  f = 0;
  goto L1;
  return 0;
}
