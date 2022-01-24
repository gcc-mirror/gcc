/* PR target/103861 */
/* { dg-do compile } */
/* { dg-options "-O2 -dp" } */

typedef char __v2qi __attribute__ ((__vector_size__ (2)));
typedef unsigned char __v2qu __attribute__ ((__vector_size__ (2)));

__v2qi and (__v2qi a, __v2qi b) { return a & b; };

__v2qi andn (__v2qi a, __v2qi b) { return a & ~b; };

__v2qi or  (__v2qi a, __v2qi b) { return a | b; };

__v2qi xor  (__v2qi a, __v2qi b) { return a ^ b; };

__v2qi not  (__v2qi a) { return ~a; };

__v2qi plus  (__v2qi a, __v2qi b) { return a + b; };

__v2qi minus  (__v2qi a, __v2qi b) { return a - b; };

__v2qi neg  (__v2qi a) { return -a; };

__v2qi shl (__v2qi a, int b) { return a << b; };

__v2qi ashr (__v2qi a, int b) { return a >> b; };

__v2qu lshr  (__v2qu a, int b) { return a >> b; };

/* { dg-final { scan-assembler-not "insvhi" } } */
