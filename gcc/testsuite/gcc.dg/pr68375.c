/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-graph" } */

int a, c, d, e, g, h;
short f;

int
foo ()
{
}

short
fn1 (void)
{
  int j[2];
  for (; e; e++)
    if (j[0])
      for (;;)
	;
  if (!g)
    return f;
}

int
main (void)
{
  for (; a < 1; a++)
    {
      for (c = 0; c < 2; c++)
	{
	  d && (f = 0);
	  h = fn1 ();
	}
      __builtin_printf ("%d\n", (char) f);
   }

 return 0;
}

