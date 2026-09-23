/* PR tree-optimization/126549 */
/* { dg-do run { target { float32 && bitint575 } } } */
/* { dg-add-options float32 } */

[[gnu::noipa]] int
foo (unsigned _BitInt(133) a)
{
  unsigned _BitInt(133) u = a % 5444517870735015415413993718908291383300uwb;
  _Float32 h = (_Float32) u;
  return h > __FLT32_MAX__;
}

int
main ()
{
  if (foo (5444517870735015415413993718908291383295uwb) != 1)
    __builtin_abort ();
}
