/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

/* Case 3: Trailing same element.  */ 

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b, int c)
{
  return (vnx4si) { a, b, c, c, c, c, c, c };
}

/*
foo:
.LFB0:
        .cfi_startproc
        mov     z0.s, w2
        insr    z0.s, w1
        insr    z0.s, w0
        ret
*/

/* { dg-final { scan-assembler {\tmov\t(z[0-9]+\.s), w2\n\tinsr\t\1, w1\n\tinsr\t\1, w0} } } */
