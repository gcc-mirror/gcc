/* PR middle-end/14752 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

void bar (void);

void foo (unsigned int a)
{
  if ((a >> 2) & 1)
    bar ();
}

void baz (unsigned int b)
{
  if ((~b >> 2) & 1)
    bar ();
}

/* { dg-final { scan-tree-dump-times "\\(a \& 4\\) != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\(b \& 4\\) == 0" 1 "original" } } */

