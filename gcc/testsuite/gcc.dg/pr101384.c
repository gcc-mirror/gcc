/* PR target/101384 */
/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi -w" } */

typedef unsigned char __attribute__((__vector_size__ (16))) U;
typedef unsigned short __attribute__((__vector_size__ (8 * sizeof (short)))) V;

U u;
V v;

__attribute__((noipa)) U
foo (void)
{
  U y = (U) { 0x80, 0xff, 0xff, 0xff, 0x80, 0xff, 0xff, 0xff,
              0x80, 0xff, 0xff, 0xff, 0x80, 0xff, 0xff, 0xff } + u;
  return y;
}

__attribute__((noipa)) V
bar (void)
{
  V y = (V) { 0x8000, 0xffff, 0x8000, 0xffff,
              0x8000, 0xffff, 0x8000, 0xffff } + v;
  return y;
}

int
main ()
{
  U x = foo ();
  for (unsigned i = 0; i < 16; i++)
    if (x[i] != ((i & 3) ? 0xff : 0x80))
      __builtin_abort ();
  V y = bar ();
  for (unsigned i = 0; i < 8; i++)
    if (y[i] != ((i & 1) ? 0xffff : 0x8000))
      __builtin_abort ();
  return 0;
}
