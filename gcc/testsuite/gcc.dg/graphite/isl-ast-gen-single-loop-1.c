/* { dg-do run } */
/* { dg-options "-O2 -fgraphite-identity" } */

int
foo ()
{
  int i, res;

  for (i = 0, res = 0; i < 50; i++)
      res += i;

  return res;
}

extern void abort ();

int
main (void)
{ 
  int res = foo ();

  if (res != 1225)
    abort ();

  return 0;
}
