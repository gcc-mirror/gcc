/* PR tree-optimization/82838 */

struct S { unsigned short a, b, c; };
struct S f[10];

void
foo (int e)
{
  struct S *x;
  f[e].b = x[e].a;
  f[e].c = x[e].b;
}
