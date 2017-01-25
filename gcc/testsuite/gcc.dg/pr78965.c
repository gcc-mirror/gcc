/* PR tree-optimization/78965 */
/* { dg-do run { target c99_runtime } } */
/* { dg-options "-O2" } */
/* { dg-add-options c99_runtime } */

int
main ()
{
  int a = 5, b = 6;
  int c = __builtin_snprintf (0, 0, "a%nb%nc", &a, &b);
  if (a + b + c != 6)
    __builtin_abort ();
  return 0;
}
