/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -fipa-cp -w -Wno-psabi" } */

typedef unsigned __int128 u128;
typedef unsigned __int128 V __attribute__ ((vector_size (64)));

V x4;

static V
bar (u128 x2, u128 x3)
{
  while (x4[0]--)
    x2 /= x3 >>= 1;
  return x2 + x3 + x4;
}

void
foo (void)
{
  bar (0, 0);
}
