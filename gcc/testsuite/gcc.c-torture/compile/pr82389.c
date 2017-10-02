/* PR tree-optimization/82389 */

void bar (short);

void
foo (void)
{
  short a[5];
  int b;
  for (b = -1290603998; b < 5; b++)
    a[b] = 0;
  bar (a[3]);
}
