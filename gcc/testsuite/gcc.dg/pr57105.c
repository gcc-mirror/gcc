/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-Os -fselective-scheduling2 -g" } */
int bar (int);
int *baz (int *);

void
foo (int a)
{
  while (bar (0))
    {
      int *c = baz (0);
      if (a)
	{
	  int i = *baz (c);
	}
      bar (*baz (c));
    }
}
