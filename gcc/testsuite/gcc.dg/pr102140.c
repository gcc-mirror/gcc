/* PR target/102140 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -fipa-cp -fno-tree-ccp -fno-tree-ter -Wno-psabi" } */

typedef int __attribute__((__vector_size__ (64))) U;
typedef __int128 __attribute__((__vector_size__ (64))) V;

int a, b;

static void
bar (char c, V v)
{
  v *= c;
  U u = a + (U) v;
  (union { U b; }) { u };
  b = 0;
}

void
foo (void)
{
  bar (1, (V){((__int128) 9223372036854775808ULL) << 64});
}
