/* PR tree-optimization/59388 */

int a;
struct S { unsigned int f:1; } b;

int
main ()
{
  a = (0 < b.f) | b.f;
  return a;
}
