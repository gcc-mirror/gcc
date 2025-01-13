/* { dg-do run } */
/* { dg-options "-O2" } */

/* PR tree-optimization/118456 */
/* Check that shifted fields compared with a constants compare correctly even
   if the constant contains sign-extension bits not present in the bit
   range.  */

struct S { unsigned long long o; unsigned short a, b; } s;

__attribute__((noipa)) int
foo (void)
{
  return ((unsigned char) s.a) >> 3 == 17 && ((signed char) s.b) >> 2 == -27;
}

__attribute__((noipa)) int
bar (void)
{
  return ((unsigned char) s.a) >> 3 == 17 && ((signed char) s.b) >> 2 == -91;
}

__attribute__((noipa)) int
bars (void)
{
  return ((unsigned char) s.a) >> 3 == 17 && ((signed char) s.b) >> 2 == 37;
}

__attribute__((noipa)) int
baz (void)
{
  return ((unsigned char) s.a) >> 3 == 49 && ((signed char) s.b) >> 2 == -27;
}

__attribute__((noipa)) int
bazs (void)
{
  return ((unsigned char) s.a) >> 3 == (unsigned char) -15 && ((signed char) s.b) >> 2 == -27;
}

int
main ()
{
  s.a = 17 << 3;
  s.b = (unsigned short)(-27u << 2);
  if (foo () != 1
      || bar () != 0
      || bars () != 0
      || baz () != 0
      || bazs () != 0)
    __builtin_abort ();
  return 0;
}
