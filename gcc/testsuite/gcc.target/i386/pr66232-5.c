/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -fpic -mx32" } */

extern void (*bar) (void);
void
foo (int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      if (!bar)
	continue;
      (*bar) ();
    }
}
