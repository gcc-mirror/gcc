/* PR ipa/84425 */

void bar (int);

void
foo (int x)
{
  if (x < 5)
    bar (x);
}

__attribute__((optimize(0))) void
bar (int x)
{
  if (x > 10)
    foo (x);
}
