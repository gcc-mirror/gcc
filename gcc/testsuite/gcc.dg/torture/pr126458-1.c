/* { dg-do run { target bitint } } */
/* PR tree-optimization/126458 */

typedef unsigned _BitInt(17) u17;
typedef unsigned _BitInt(16) u16;
typedef unsigned _BitInt(15) u15;
typedef signed _BitInt(16) s16;

__attribute__((noipa)) int
neg (u16 a)
{
  return ((s16) a) < 0;
}

__attribute__((noipa)) u17
fref (u16 a)
{
  return neg (a) ? (u17) a : (u17)(u16) -1u;
}

__attribute__((noipa)) u17
f (u16 a)
{
  return ((s16) a) < 0 ? (u17) a : (u17)(u16) -1u;
}

int
main (void)
{
  static const u16 v[] = { ((u16)1u)<<15 , (u16)-2u, (u16)-1u, (u16)(u15)-1u, 0u };

  for (unsigned i = 0; i < sizeof v / sizeof v[0]; i++)
    if (f (v[i]) != fref (v[i]))
      __builtin_abort ();
  return 0;
}
