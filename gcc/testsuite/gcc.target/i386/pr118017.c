/* PR target/118017 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-Og -frounding-math -mno-80387 -mno-mmx -Wno-psabi" } */

typedef __attribute__((__vector_size__ (64))) _Float128 F;
typedef __attribute__((__vector_size__ (64))) _Decimal64 G;
typedef __attribute__((__vector_size__ (64))) _Decimal128 H;

void
bar(_Float32, _BitInt(1025), _BitInt(1025), _Float128, __int128, __int128,  F,
        int, int, G, _Float64, __int128, __int128, H, F);


void
foo ()
{
  bar ((__int128)68435455, 0, 0, 0, 0, 0, (F){}, 0, 0, (G){3689348814741910323},
       0, 0, 0, (H){0, (_Decimal128) ((__int128) 860933398830926 << 64),
       (_Decimal128) ((__int128) 966483857959145 << 64), 4},
       (F){(__int128) 3689348814741910323 << 64 | 3});
}
