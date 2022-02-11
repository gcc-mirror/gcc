/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef int v4si __attribute__((vector_size (16)));
typedef long long v2di __attribute__((vector_size (16)));

v4si foo (int a, v4si b)
{
    return (__extension__ (v4si) {~a, ~a, ~a, ~a}) & b;
}

v2di bar (long long a, v2di b)
{
    return (__extension__ (v2di) {~a, ~a}) & b;
}

/* { dg-final { scan-assembler-times "pandn" 2 } } */
