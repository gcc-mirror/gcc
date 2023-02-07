/* PR tree-optimization/106923 */
/* { dg-do compile } */
/* { dg-options "-O1 -finline-small-functions -fpartial-inlining --param max-inline-insns-single=1 --param uninlined-function-insns=10000" } */

int n;

int
baz (void);

__attribute__ ((returns_twice)) int
bar (void)
{
  if (baz ())
    ++n;

  return 0;
}

int
foo (void)
{
  return bar ();
}
