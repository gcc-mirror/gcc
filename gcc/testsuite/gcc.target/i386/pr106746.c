/* { dg-do compile } */
/* { dg-options "-O2 -fsched2-use-superblocks -fcompare-debug -Wno-psabi" } */

typedef char __attribute__((__vector_size__ (64))) U;
typedef short __attribute__((__vector_size__ (64))) V;
typedef int __attribute__((__vector_size__ (64))) W;

char c;
U a;
U *r;
W foo0_v512u32_0;

void
foo (W)
{
  U u;
  V v;
  W w = __builtin_shuffle (foo0_v512u32_0, foo0_v512u32_0);
  u =
    __builtin_shufflevector (a, u, 3, 0, 4, 9, 9, 6, 7, 8, 5,
			     0, 6, 1, 8, 1, 2, 8, 6,
			     1, 8, 4, 9, 3, 8, 4, 6, 0, 9, 0, 1, 8, 2, 3, 3,
			     0, 4, 9, 9, 6, 7, 8, 5,
			     0, 6, 1, 8, 1, 2, 8, 6,
			     1, 8, 4, 9, 3, 8, 4, 6, 0, 9, 0, 1, 8, 2, 3);
  v *= c;
  w &= c;
  *r = (U) v + (U) w;
}
