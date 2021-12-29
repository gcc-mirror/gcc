/* { dg-do compile } */
/* { dg-options "-O3 -fno-guess-branch-probability" } */

extern void bar (void);

void
foo (int x, int w)
{
  for (int y; y < w; y++)
    if (y < x)
      bar ();
}
