/* PR sanitizer/119801 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=thread" } */

[[gnu::noipa]] int
bar (int *p)
{
  return ++*p;
}

int
foo (int *p)
{
  ++*p;
  [[gnu::musttail]] return bar (p);
}

[[gnu::noinline]] int
baz (int x)
{
  if (x < 10)
    return x;
  [[gnu::musttail]] return baz (x - 2);
}
