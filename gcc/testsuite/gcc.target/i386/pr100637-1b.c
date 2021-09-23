/* PR target/100637 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -dp" } */

typedef char __v4qi __attribute__ ((__vector_size__ (4)));

__v4qi and (__v4qi a, __v4qi b) { return a & b; };
/* { dg-final { scan-assembler "andv4qi3" } } */

__v4qi andn (__v4qi a, __v4qi b) { return a & ~b; };
/* { dg-final { scan-assembler "andnotv4qi3" } } */

__v4qi or  (__v4qi a, __v4qi b) { return a | b; };
/* { dg-final { scan-assembler "iorv4qi3" } } */

__v4qi xor  (__v4qi a, __v4qi b) { return a ^ b; };
__v4qi not  (__v4qi a) { return ~a; };
/* { dg-final { scan-assembler-times "xorv4qi3" 2 } } */

__v4qi plus  (__v4qi a, __v4qi b) { return a + b; };
/* { dg-final { scan-assembler "addv4qi3" } } */

__v4qi minus  (__v4qi a, __v4qi b) { return a - b; };
__v4qi neg  (__v4qi a) { return -a; };
/* { dg-final { scan-assembler-times "subv4qi3" 2 } } */
