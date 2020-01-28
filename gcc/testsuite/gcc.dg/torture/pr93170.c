/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-Wno-psabi" } */
/* { dg-additional-options "-frename-registers -fno-tree-forwprop -fno-tree-fre -fira-algorithm=priority -mstringop-strategy=loop --param=hot-bb-frequency-fraction=0 -Wno-psabi" { target { x86_64-*-* i?86-*-* } } } */

typedef unsigned char v64u8 __attribute__ ((vector_size (64)));
typedef unsigned short v64u16 __attribute__ ((vector_size (64)));
typedef unsigned int v64u32 __attribute__ ((vector_size (64)));
typedef unsigned long long v64u64 __attribute__ ((vector_size (64)));
typedef unsigned __int128 u128;
typedef unsigned __int128 v64u128 __attribute__ ((vector_size (64)));

int a, b, d, e;
v64u64 c;

v64u128
foo (u128 g, v64u16 h, v64u32 i, v64u128 j)
{
  c[e] = 0;
  j &= (i[1] <<= b);
  j >>= ((v64u128) h <= j);
  d = __builtin_popcountll (-((v64u8) i)[0]);
  return a + g + j;
}

int
main (void)
{
  v64u128 x = foo (0, (v64u16) { 0, 0, 0, 0, 0, 0, 0, 0, 5 }, (v64u32) { 2 },
		   (v64u128) { });
  if (x[0] || x[1] || x[2] || x[3])
    __builtin_abort ();
  return 0;
}
