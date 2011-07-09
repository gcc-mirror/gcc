/* PR optimization/11741  */
/* { dg-do compile } */
/* { dg-options "-O2 -minline-all-stringops" } */
/* { dg-options "-O2 -minline-all-stringops -march=pentium4" { target ia32 } } */

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

