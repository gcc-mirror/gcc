/* PR target/128625 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2" } */

typedef __int128 V __attribute__((vector_size (16)));
struct S { V m; };

__attribute__((noipa)) int
f (struct S s)
{
  return s.m[0] >> 64;
}

__attribute__((noipa)) struct S
g (__int128 x)
{
  struct S s = { { x } };
  return s;
}

int
main ()
{
  __int128 x = ((__int128) 0x1234 << 64) | 0x5678;
  struct S s = { { x } };
  if (f (s) != 0x1234)
    __builtin_abort ();
  s = g (x);
  if (s.m[0] != x)
    __builtin_abort ();
  return 0;
}
