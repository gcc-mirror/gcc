/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

struct A
{
  int m_fn1 ();
  short *m_fn2 ();
};

struct B
{
  void *fC;
};

int a, b;
unsigned char i;
void fn1 (unsigned char *p1, A &p2)
{
  int c = p2.m_fn1 ();
  for (int d = 0; c; d++)
    {
      short *e = p2.m_fn2 ();
      unsigned char *f = &p1[0];
      for (int g = 0; g < a; g++)
	{
	  int h = e[0];
	  b += h * f[g];
	}
    }
}

void fn2 (A &p1, A &p2, B &p3)
{
  int j = p2.m_fn1 ();
  for (int k = 0; j; k++)
    if (0)
      ;
    else
      fn1 (&i, p1);
  if (p3.fC)
    ;
  else
    ;
}
