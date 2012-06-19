/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

extern void g (int *);

void f (void)
{
  int tabs[1024], tabcount;

  for (tabcount = 1; tabcount <= 8; tabcount += 7)
    {
      int i;
      for (i = 0; i < 1024; i++)
	tabs[i] = i * 12345;
      g (tabs);
    }
}

