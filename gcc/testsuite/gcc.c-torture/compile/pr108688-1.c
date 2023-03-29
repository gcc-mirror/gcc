/* PR tree-optimization/108688 */

union U { signed int d : 7; signed int e : 2; } u;
int a, b;

void
foo (void)
{
  for (int i = 0; i < 64; i++)
    {
      u.d = a;
      u.e ^= b;
    }
}
