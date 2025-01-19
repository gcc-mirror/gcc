/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcv_zvl256b -mabi=lp64d" } */

int *b;

inline void c (char *d, int e)
{
  d[0] = 0;
  d[1] = e;
}

void f ();

void h ()
{
  for (;;)
    {
      char *a;
      long g = 8;
      while (g)
	{
	  c (a, *b);
	  b++;
	  a += 2;
	  g--;
	}
      f ();
    }
}
