/* PR tree-optimization/108638 */

long long a;
int b;

void
foo (void)
{
  for (a = 0; a < __SIZEOF_LONG_LONG__ * __CHAR_BIT__; a++)
    if (b)
      b |= a << a;
}
