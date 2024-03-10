/* { dg-do compile } */
/* { dg-additional-options "-O3 -std=c99" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+nosve"

/*
**test:
**	...
**	addv	(s[0-9]+), v[0-9]+.4s
**	fmov	w0, \1
**	and	w1, w0, 65535
**	add	w0, w1, w0, lsr 16
**	lsr	w0, w0, 1
**	ret
*/
int test (uint8_t *p, uint32_t t[1][1], int n) {

  int sum = 0;
  uint32_t a0;
  for (int i = 0; i < 4; i++, p++)
    t[i][0] = p[0];

  for (int i = 0; i < 4; i++) {
    {
      int t0 = t[0][i] + t[0][i];
      a0 = t0;
    };
    sum += a0;
  }
  return (((uint16_t)sum) + ((uint32_t)sum >> 16)) >> 1;
}
