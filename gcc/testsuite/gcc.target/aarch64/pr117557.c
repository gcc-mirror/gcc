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
**	addvl	x([0-9]+), x([0-9]+), #2
**	...
**	ld1w	z[0-9]+.s, p([0-9]+)/z, \[x[0-9]+, z[0-9]+.s, sxtw\]
**	ld1w	z[0-9]+.s, p([0-9]+)/z, \[x[0-9]+, z[0-9]+.s, sxtw\]
**	st1w	z[0-9]+.s, p\3, \[x\2, z[0-9]+.s, sxtw\]
**	st1w	z[0-9]+.s, p\4, \[x\1, z[0-9]+.s, sxtw\]
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

