/* PR target/79197 */

unsigned long b;

unsigned long
foo (float *a, float *x)
{
  __builtin_memcpy (a, x, sizeof (float));
  return *a;
}
