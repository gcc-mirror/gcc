/* PR tree-optimization/79284 */

struct S { unsigned a : 1; } b;
int c[64];

int
foo (int x)
{ 
  char e, f;
  for (e = 63; e; e--)
    f = (c[e] && ~0) != b.a;
  return f;
}
