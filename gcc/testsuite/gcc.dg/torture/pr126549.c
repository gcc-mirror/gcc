/* PR tree-optimization/126549 */
/* { dg-do run { target float16 } } */
/* { dg-add-options float16 } */

[[gnu::noipa]] int
foo (unsigned a)
{
  unsigned u = a % 100001U;
  _Float16 h = (_Float16) u;
  return h > 65504.0f16;
}

int
main ()
{
  if (foo (70000U) != 1)
    __builtin_abort ();
}
