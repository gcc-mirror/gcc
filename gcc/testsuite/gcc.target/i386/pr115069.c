/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */
/* { dg-final { scan-assembler-not "vpermq" } } */

typedef char v16qi __attribute__((vector_size(16)));

v16qi foo (v16qi a, v16qi b) {
    return a * b;
}
