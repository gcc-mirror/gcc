/* PR tree-optimization/71872 */

struct __attribute__((may_alias)) S { int a; };

void
foo (int *x, struct S *y)
{
  int i;
  for (i = 0; i < 16; i++)
    {
      int a = 0;
      if (*x)
        *(struct S *) y = *(struct S *) &a;
    }
}
