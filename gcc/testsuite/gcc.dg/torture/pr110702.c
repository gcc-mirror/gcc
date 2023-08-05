/* { dg-do run } */

void abort (void);

int a, b, c, d;
long e[9][7][4];

void f()
{
  for (; a >= 0; a--)
    {
      b = 0;
      for (; b <= 3; b++)
	{
	  c = 0;
	  for (; c <= 3; c++)
	    {
	      int *g = &d;
	      *g = e[0][0][b] | e[a][b][a];
	    }
	}
    }
}

int main()
{
  f();
  if (a != -1)
    abort ();
  return 0;
}
