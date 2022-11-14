/* { dg-do compile } */
/* { dg-additional-options "-O2 -std=c99" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#include <stdint.h>

/*
** draw_bitmap1:
** ...
**	mul	z[0-9]+.h, p[0-9]+/m, z[0-9]+.h, z[0-9]+.h
**	addhnb	z[0-9]+.b, z[0-9]+.h, z[0-9]+.h
**	addhnb	z[0-9]+.b, z[0-9]+.h, z[0-9]+.h
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
**	mul	z[0-9]+.s, p[0-9]+/m, z[0-9]+.s, z[0-9]+.s
**	addhnb	z[0-9]+.h, z[0-9]+.s, z[0-9]+.s
**	addhnb	z[0-9]+.h, z[0-9]+.s, z[0-9]+.s
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
**	mul	z[0-9]+.d, p[0-9]+/m, z[0-9]+.d, z[0-9]+.d
**	addhnb	z[0-9]+.s, z[0-9]+.d, z[0-9]+.d
**	addhnb	z[0-9]+.s, z[0-9]+.d, z[0-9]+.d
** ...
*/
void draw_bitmap4(uint32_t* restrict pixel, uint32_t level, int n)
{
  for (int i = 0; i < (n & -16); i+=1)
    pixel[i] = (pixel[i] * (uint64_t)level) / 0xffffffffUL;
}
