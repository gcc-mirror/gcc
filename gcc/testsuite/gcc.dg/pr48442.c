/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-Os -fselective-scheduling2 --param max-sched-extend-regions-iters=100" } */
void f (void)
{
  unsigned *a2;
  int vertex2;
  int c, x2, dx2, dy2, s2;
  long m, b;
  do
    {
      if (dx2)
	dx2 = dx2 % dy2;
	s2 = (dx2 / dy2);
    }
  while (vertex2);
  for (;;)
    {
      c = x2;
      a2 = 0;
      if (c)
	{
	  m = b << (c);
	  *a2 = (*a2 & ~m);
	}
      x2 += s2;
    }
}
