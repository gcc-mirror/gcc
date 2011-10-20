/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fno-cprop-registers -fno-dce -fno-forward-propagate -fselective-scheduling2 -funroll-loops -fno-web" } */
extern int a[];

void foo (void)
{
  int i;
  for (i = 0; i < 199; i++)
    {
      if (a[i] != i)
	__builtin_abort ();
    }
}
