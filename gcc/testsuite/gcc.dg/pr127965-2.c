/* PR tree-optimization/127965 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O2" } */

int
main ()
{
  int x = 0x20;
  int v = __atomic_fetch_or (&x, (unsigned char) (-__SCHAR_MAX__ - 1), __ATOMIC_SEQ_CST);
  if ((_BitInt (6)) v >= 0)
    __builtin_abort ();
}
