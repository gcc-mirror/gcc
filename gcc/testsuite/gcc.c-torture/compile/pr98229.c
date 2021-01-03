/* PR rtl-optimization/98229 */

unsigned long long
foo (unsigned long long x)
{
  return x % ~0U;
}
