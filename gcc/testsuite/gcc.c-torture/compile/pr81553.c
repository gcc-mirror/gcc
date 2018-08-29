/* PR rtl-optimization/81553 */

int a, b, c, d;

void
foo (void)
{
  d = 1 >> c >> 1;
  b = ~(209883449764912897ULL & d) << (0 >= a) | ~d;
}
