/* { dg-do compile } */
/* { dg-additional-options "-Wno-div-by-zero" } */

__attribute__ ((returns_twice)) int
bar (void);

void
foo (int *p, int x)
{
  *p = 0;
  while (*p < 1)
    {
      x = 0;
      while (x < 1)
        bar ();

      x /= 0;
    }

  foo (p, x);
}
