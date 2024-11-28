/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=neoverse-n1+sve -fdump-tree-vect" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>
#include <string.h>

#define N 8
#define L 8

/*
**f:
**	...
**	ld1w	z[0-9]+.s, p([0-9]+)/z, \[x[0-9]+, z[0-9]+.s, sxtw\]
**	ld1w	z[0-9]+.s, p([0-9]+)/z, \[x[0-9]+, z[0-9]+.s, sxtw\]
**	st1w	z[0-9]+.s, p\1, \[x[0-9]+, z[0-9]+.s, sxtw\]
**	incb	x([0-9]+), all, mul #2
**	st1w	z[0-9]+.s, p\2, \[x\3, z[0-9]+.s, sxtw\]
**	ret
**	...
*/
void f(const uint8_t * restrict seq1,
       const uint8_t *idx, uint8_t *seq_out) {
  for (int i = 0; i < L; ++i) {
    uint8_t h = idx[i];
    memcpy((void *)&seq_out[i * N], (const void *)&seq1[h * N / 2], N / 2);
  }
}

