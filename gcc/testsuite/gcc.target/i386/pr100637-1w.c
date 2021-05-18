/* PR target/100637 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -dp" } */

typedef short __v2hi __attribute__ ((__vector_size__ (4)));

__v2hi and (__v2hi a, __v2hi b) { return a & b; };
/* { dg-final { scan-assembler "andv2hi3" } } */

__v2hi andn (__v2hi a, __v2hi b) { return a & ~b; };
/* { dg-final { scan-assembler "andnotv2hi3" } } */

__v2hi or  (__v2hi a, __v2hi b) { return a | b; };
/* { dg-final { scan-assembler "iorv2hi3" } } */

__v2hi xor  (__v2hi a, __v2hi b) { return a ^ b; };
__v2hi not  (__v2hi a) { return ~a; };
/* { dg-final { scan-assembler-times "xorv2hi3" 2 } } */

__v2hi plus  (__v2hi a, __v2hi b) { return a + b; };
/* { dg-final { scan-assembler "addv2hi3" } } */

__v2hi minus  (__v2hi a, __v2hi b) { return a - b; };
__v2hi neg  (__v2hi a) { return -a; };
/* { dg-final { scan-assembler-times "subv2hi3" 2 } } */

__v2hi mul  (__v2hi a, __v2hi b) { return a * b; };
/* { dg-final { scan-assembler "mulv2hi3" } } */
