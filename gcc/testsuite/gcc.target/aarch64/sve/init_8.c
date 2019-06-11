/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

/* Case 5.2: Interleaved elements and constants.  */ 

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b, int c, int d)
{
  return (vnx4si) { a, 1, b, 2, c, 3, d, 4 }; 
}

/*
foo:
.LFB0:
        .cfi_startproc
        ptrue   p0.s, vl8
        adrp    x4, .LANCHOR0
        add     x4, x4, :lo12:.LANCHOR0
        ld1w    z1.s, p0/z, [x4]
        mov     z0.s, w3
        insr    z0.s, w2
        insr    z0.s, w1
        insr    z0.s, w0
        zip1    z0.s, z0.s, z1.s
        ret
*/

/* { dg-final { scan-assembler {\tld1w\t(z[0-9]+\.s), p[0-9]+/z, \[x[0-9]+\]\n\tmov\t(z[0-9]+\.s), w3\n\tinsr\t\2, w2\n\tinsr\t\2, w1\n\tinsr\t\2, w0\n\tzip1\t\2, \2, \1} } } */
