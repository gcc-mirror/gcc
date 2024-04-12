/* PR sanitizer/114687 */
/* { dg-do compile } */

int a;
int foo (int);

__attribute__((pure, returns_twice)) int
bar (void)
{
  a = 1;
  while (a)
    a = 2;
  return a;
}

int
baz (void)
{
  int d = bar ();
  foo (d);
  return 0;
}
