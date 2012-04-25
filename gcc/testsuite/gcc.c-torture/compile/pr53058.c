/* PR tree-optimization/53058 */

int a, b, c;

void
foo ()
{
  c = b >> 16;
  if (c > 32767)
    c = 0;
  a = b;
}
