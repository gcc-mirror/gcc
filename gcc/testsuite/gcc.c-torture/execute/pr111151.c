/* PR middle-end/111151 */

int
main ()
{
  unsigned a = (1U + __INT_MAX__) / 2U;
  unsigned b = 1U;
  unsigned c = (a * 2U > b * 2U ? a * 2U : b * 2U) * 2U;
  if (c != 0U)
    __builtin_abort ();
  int d = (-__INT_MAX__ - 1) / 2;
  int e = 10;
  int f = (d * 2 > e * 5 ? d * 2 : e * 5) * 6;
  if (f != 300)
    __builtin_abort ();
  int g = (-__INT_MAX__ - 1) / 2;
  int h = 0;
  int i = (g * 2 > h * 5 ? g * 2 : h * 5) / -1;
  if (i != 0)
    __builtin_abort ();
}
