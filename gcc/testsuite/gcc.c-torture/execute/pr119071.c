/* PR rtl-optimization/119071 */

int a, b;

int
main ()
{
  int c = 0;
  if (a + 2)
    c = 1;
  int d = (1 + c - 2 + c == 1) - 1;
  b = ((d + 1) << d) + d;
  if (b != 1)
    __builtin_abort ();
}
