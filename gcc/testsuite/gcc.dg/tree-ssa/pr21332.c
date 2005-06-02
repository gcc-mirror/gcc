/* { dg-do run } */
/* { dg-options "-O2" } */

// this testcase fails also on amd64:

extern void abort (void);

int f ()
{
  return -1;
}

int main ()
{
  int b, c, i;

  b = 0;
  c = f ();
  if (c <= 0)
    {
      c = -c;
      for (i = 0; i < c; i++)
	  b = 1;
      if (!b)
	abort ();
    }
  return 0;
}
