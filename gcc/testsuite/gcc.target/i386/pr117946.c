/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-O -favoid-store-forwarding -mavx10.1 -mprefer-avx128 --param=store-forwarding-max-distance=128 -Wno-psabi" } */
typedef __attribute__((__vector_size__ (64))) _Decimal32 V;

void
bar (float, float, float, float, float, _Complex, float, float, float,
     _BitInt(1023), _BitInt (1023), float, float, float, float, float, float,
     float, float, float, float, float, float, _Decimal64, float, float, float,
     V, float, _Decimal64);

void
foo ()
{
  bar (0, 0, 0, 0, 0, 0, 0, __builtin_nand64 ("nan"), 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (V){}, 0, 0);
}
