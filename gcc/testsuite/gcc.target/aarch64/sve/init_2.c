/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -fno-schedule-insns -msve-vector-bits=256 --save-temps" } */

/* Case 1.2: Trailing constants with repeating sequence.  */

#include <stdint.h>

typedef int32_t vnx4si __attribute__((vector_size (32)));

__attribute__((noipa))
vnx4si foo(int a, int b)
{
  return (vnx4si) { a, b, 2, 3, 2, 3, 2, 3 };
}

/*
foo:
.LFB0:
        .cfi_startproc
        ptrue   p0.s, vl8
        adrp    x2, .LANCHOR0
        add     x2, x2, :lo12:.LANCHOR0
        ld1w    z0.s, p0/z, [x2]
        insr    z0.s, w1
        insr    z0.s, w0
        ret
*/

/* { dg-final { scan-assembler {\tld1w\t(z[0-9]+\.s), p[0-9]+/z, \[x[0-9]+\]\n\tinsr\t\1, w1\n\tinsr\t\1, w0} } } */
