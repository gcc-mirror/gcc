/* PR tree-optimization/111015 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O2" } */

struct S { unsigned a : 4, b : 4; unsigned __int128 c : 70; } d;

__attribute__((noipa)) void
foo (unsigned __int128 x, unsigned char y, unsigned char z)
{
  d.a = y;
  d.b = z;
  d.c = x;
}

int
main ()
{
  foo (-1, 12, 5);
  if (d.a != 12
      || d.b != 5
      || d.c != (-1ULL | (((unsigned __int128) 0x3f) << 64)))
    __builtin_abort ();
  foo (0x123456789abcdef0ULL | (((unsigned __int128) 26) << 64), 7, 11);
  if (d.a != 7
      || d.b != 11
      || d.c != (0x123456789abcdef0ULL | (((unsigned __int128) 26) << 64)))
    __builtin_abort ();
}
