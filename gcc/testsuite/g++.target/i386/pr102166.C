/* PR target/102166 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -std=c++14" } */

#include<immintrin.h>
__attribute__((target("amx-tile"))) void amx()
{
  _tile_loadd(0, 0, 0);
  _tile_release();
}

__attribute__((target("amx-int8"))) void amxint8()
{
  _tile_dpbssd(0, 1, 2);
}

__attribute__((target("amx-bf16"))) void amxbf16()
{
  _tile_dpbf16ps (0, 1, 2);
}
