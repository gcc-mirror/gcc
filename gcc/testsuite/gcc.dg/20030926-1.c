/* PR optimization/11741  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -minline-all-stringops" } */
/* { dg-options "-O2 -minline-all-stringops -march=pentium4" { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */

extern void *memcpy (void *, const void *, __SIZE_TYPE__);
extern __SIZE_TYPE__ strlen (const char *);

void
foo (char *p)
{
  for (;;)
    {
      memcpy (p, p + 1, strlen (p));
      p++;
    }
}

