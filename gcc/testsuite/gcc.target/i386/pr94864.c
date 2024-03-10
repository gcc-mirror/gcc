/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx" } */

typedef double v2df __attribute__((vector_size(16)));

v2df move_sd(v2df a, v2df b)
{
    v2df result = a;
    result[0] = b[1];
    return result;
}

/* { dg-final { scan-assembler "unpckhpd\[\\t \]%xmm0, %xmm1" } } */
