/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

/* Case 5.5: Interleaved repeating elements and trailing same elements.  */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b, int f) 
{
  return (vnx4si) { a, f, b, f, b, f, b, f };
}

/*
foo:
.LFB0:
        .cfi_startproc
        mov     z0.s, w1
        insr    z0.s, w0
        mov     z1.s, w2
        zip1    z0.s, z0.s, z1.s
        ret
*/

/* { dg-final { scan-assembler {\tmov\t(z[0-9]+\.s), w1\n\tinsr\t\1, w0\n\tmov\t(z[0-9]+\.s), w2\n\tzip1\t\1, \1, \2} } } */
