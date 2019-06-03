/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -fno-schedule-insns -msve-vector-bits=256 --save-temps" } */

/* Case 5.4: Interleaved repeating elements and non-repeating elements.  */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b, int c, int f)
{
  return (vnx4si) { a, f, b, f, c, f, c, f };
}

/*
foo:
.LFB0:
        .cfi_startproc
        mov     z0.s, w2
        mov     z1.s, w3
        insr    z0.s, w1
        ptrue   p0.s, vl8
        insr    z0.s, w0
        zip1    z0.s, z0.s, z1.s
        ret
*/

/* { dg-final { scan-assembler {\tmov\t(z[0-9]+\.s), w3\n\tmov\t(z[0-9]+\.s), w2\n.*\n\tinsr\t\2, w1\n\tinsr\t\2, w0\n\tzip1\t\2, \2, \1} } } */
