/* { dg-do compile } */
/* { dg-options "-O2" } */

void g(int);

void
f (unsigned int x, _Bool y)
{
  for (int i = 0; i < 100; ++i)
    {
      if ((x >> 31) | y)
	g (1);
      for (int j = 0; j < 100; ++j)
	g (2);
    }
}

/* { dg-final { scan-assembler-times {and\t} 1 } } */
