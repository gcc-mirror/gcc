/* PR tree-optimization/127965 */
/* { dg-do run } */
/* { dg-options "-O2" } */

int
main ()
{
  int x = (unsigned char) (-__SCHAR_MAX__ - 1);
  int v = __atomic_fetch_or (&x, -__INT_MAX__ - 1, __ATOMIC_SEQ_CST);
  if ((signed char) v >= 0)
    __builtin_abort ();
}
