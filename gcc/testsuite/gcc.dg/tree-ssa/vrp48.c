/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_failure (void);

static int __attribute__ ((noinline)) foo (int x)
{
  if (x >= 1)
    if (x <= 10)
      {
	if (x < 1 || x > 10)
	  link_failure ();
	x = x + 1;
      }
  return x;
}

int main (void)
{
  int i = foo (0);
  return 0;
}

