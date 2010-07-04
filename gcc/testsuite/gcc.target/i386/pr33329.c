/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */
/* { dg-require-effective-target sse2 } */

extern void g (int *);

void f (void)
{
  int tabs[8], tabcount;

  for (tabcount = 1; tabcount <= 8; tabcount += 7)
    {
      int i;
      for (i = 0; i < 8; i++)
	tabs[i] = i * 2;
      g (tabs);
    }
}

