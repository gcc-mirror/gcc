/* PR optimization/11741  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -minline-all-stringops -march=pentium4" } */

void
foo (char *p)
{
  for (;;)
    {
      memcpy (p, p + 1, strlen (p));
      p++;
    }
}

