/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */
/* { dg-final { scan-tree-dump-not "link_error" "vrp1"} } */

extern void link_error (void);

__attribute__((noinline, noclone)) int
foo (unsigned int n, int r)
{
  int i;
  if (n > 0)
    {
      asm ("");
      if (n < 10)
	{
	  asm ("");
	  do
	    {
	      --n;
	      r *= 2;
	      if (n >= 9)
		link_error ();
	    }
	  while (n > 0);
	}
    }
  return r + n;
}

int
main ()
{
  foo (7, 2);
  return 0;
}
