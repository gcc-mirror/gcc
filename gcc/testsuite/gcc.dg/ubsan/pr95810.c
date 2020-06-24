/* PR middle-end/95810 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" } */

int
main ()
{
  int x = -__INT_MAX__ - 1;
  x = (x <= 0 ? x : -x);
  if (x != -__INT_MAX__ - 1)
    __builtin_abort ();
  return 0;
}
