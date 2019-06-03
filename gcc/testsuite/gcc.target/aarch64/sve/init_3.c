/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -fno-schedule-insns -msve-vector-bits=256 --save-temps" } */

/* Case 2.1: Leading constants with stepped sequence.  */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b)
{
  return (vnx4si) { 1, 2, 3, 4, 5, 6, a, b };
}

/*
foo:
.LFB0:
        .cfi_startproc
        ptrue   p0.s, vl8
        index   z0.s, #6, #-1
        insr    z0.s, w0
        insr    z0.s, w1
        rev     z0.s, z0.s
        ret
*/

/* { dg-final { scan-assembler {\tindex\t(z[0-9]+\.s), #6, #-1\n\tinsr\t\1, w0\n\tinsr\t\1, w1\n\trev\t\1, \1} } } */
