/* PR tree-optimization/113201 */

void foo (void) __attribute__((returns_twice));
void bar (void);

int
baz (void)
{
  int x = 42;
  foo ();
  while (--x)
    ;
  bar ();
  return x;
}
