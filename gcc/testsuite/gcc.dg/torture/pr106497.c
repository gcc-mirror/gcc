/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-dce" } */

int n;

__attribute__ ((pure,returns_twice)) int
bar (void);

int
foo (int x)
{
  n = 0;

  bar ();

  if (x && n)
    return 0;

  foo (x);
}
