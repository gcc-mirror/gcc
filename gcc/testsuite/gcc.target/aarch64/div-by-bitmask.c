/* { dg-do compile } */
/* { dg-additional-options "-O3 -std=c99" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#include <stdint.h>

#pragma GCC target "+nosve"

/*
** draw_bitmap1:
** ...
** 	addhn	v[0-9]+.8b, v[0-9]+.8h, v[0-9]+.8h
** 	addhn	v[0-9]+.8b, v[0-9]+.8h, v[0-9]+.8h
** 	uaddw	v[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8b
** 	uaddw	v[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8b
** 	uzp2	v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b
** ...
*/
void draw_bitmap1(uint8_t* restrict pixel, uint8_t level, int n)
{
  for (int i = 0; i < (n & -16); i+=1)
    pixel[i] = (pixel[i] * level) / 0xff;
}

void draw_bitmap2(uint8_t* restrict pixel, uint8_t level, int n)
{
  for (int i = 0; i < (n & -16); i+=1)
    pixel[i] = (pixel[i] * level) / 0xfe;
}

/*
** draw_bitmap3:
** ...
** 	addhn	v[0-9]+.4h, v[0-9]+.4s, v[0-9]+.4s
** 	addhn	v[0-9]+.4h, v[0-9]+.4s, v[0-9]+.4s
** 	uaddw	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4h
** 	uaddw	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4h
** 	uzp2	v[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h
** ...
*/
void draw_bitmap3(uint16_t* restrict pixel, uint16_t level, int n)
{
  for (int i = 0; i < (n & -16); i+=1)
    pixel[i] = (pixel[i] * level) / 0xffffU;
}

/*
** draw_bitmap4:
** ...
** 	addhn	v[0-9]+.2s, v[0-9]+.2d, v[0-9]+.2d
** 	addhn	v[0-9]+.2s, v[0-9]+.2d, v[0-9]+.2d
** 	uaddw	v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2s
** 	uaddw	v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2s
** 	uzp2	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
** ...
*/
void draw_bitmap4(uint32_t* restrict pixel, uint32_t level, int n)
{
  for (int i = 0; i < (n & -16); i+=1)
    pixel[i] = (pixel[i] * (uint64_t)level) / 0xffffffffUL;
}
