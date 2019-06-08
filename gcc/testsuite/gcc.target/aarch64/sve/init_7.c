/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

/* Case 5.1: All elements.  */ 

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b, int c, int d, int e, int f, int g, int h)
{
  return (vnx4si) { a, b, c, d, e, f, g, h };
}

/*
foo:
.LFB0:
        .cfi_startproc
        mov     z0.s, w7
        insr    z0.s, w6
        insr    z0.s, w5
        insr    z0.s, w4
        insr    z0.s, w3
        insr    z0.s, w2
        insr    z0.s, w1
        insr    z0.s, w0
        ret
*/

/* { dg-final { scan-assembler {\tmov\t(z[0-9]+\.s), w7\n\tinsr\t\1, w6\n\tinsr\t\1, w5\n\tinsr\t\1, w4\n\tinsr\t\1, w3\n\tinsr\t\1, w2\n\tinsr\t\1, w1\n\tinsr\t\1, w0} } } */
