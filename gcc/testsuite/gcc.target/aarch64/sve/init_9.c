/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -fno-schedule-insns -msve-vector-bits=256 --save-temps" } */

/* Case 5.3: Repeated elements.  */ 

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b)
{
  return (vnx4si) { a, b, a, b, a, b, a, b };
}

/*
foo:
.LFB0:
        .cfi_startproc
        mov     z0.s, w0
        mov     z1.s, w1
        ptrue   p0.s, vl8
        zip1    z0.s, z0.s, z1.s
        ret
*/

/* { dg-final { scan-assembler {\tmov\t(z[0-9]+\.s), w0\n\tmov\t(z[0-9]+\.s), w1\n.*\tzip1\t\1, \1, \2} } } */
