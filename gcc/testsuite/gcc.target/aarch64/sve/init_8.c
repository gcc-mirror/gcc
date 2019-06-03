/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -fno-schedule-insns -msve-vector-bits=256 --save-temps" } */

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
        mov     z0.s, w3
        adrp    x3, .LANCHOR0
        insr    z0.s, w2
        add     x3, x3, :lo12:.LANCHOR0
        insr    z0.s, w1
        ld1w    z1.s, p0/z, [x3]
        insr    z0.s, w0
        zip1    z0.s, z0.s, z1.s
        ret
*/

/* { dg-final { scan-assembler {\tmov\t(z[0-9]+\.s), w3\n\tadrp\t(x[0-9]+), \.LANCHOR0\n\tinsr\t\1, w2\n\tadd\t\2, \2, :lo12:\.LANCHOR0\n\tinsr\t\1, w1\n\tld1w\t(z[0-9]+\.s), p[0-9]+/z, \[\2\]\n\tinsr\t\1, w0\n\tzip1\t\1, \1, \3} } } */
