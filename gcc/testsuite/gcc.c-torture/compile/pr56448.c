/* PR tree-optimization/56448 */

volatile int a[1];
int b;

void
foo ()
{
  for (;;)
    {
      int *c[3][6] = { 0, 0, 0, &b, 0, 0, 0, 0, &b, 0, 0, 0, 0, 0, 0, 0, &b, (int *) &a[0] };
      b = *c[2][5];
    }
}
