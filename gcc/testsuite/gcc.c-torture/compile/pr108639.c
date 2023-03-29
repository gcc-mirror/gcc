/* PR tree-optimization/108639 */

long long a;

int
main ()
{
  a = a ? 0 || 0 % 0 : 0;
  a = a << a;
  return 0;
}
