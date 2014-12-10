/* PR tree-optimization/61042 */

int a, b, c[1], d, f;

void
foo ()
{
  for (; b; b++)
    c[0] = c[f] && (d = a);
}
