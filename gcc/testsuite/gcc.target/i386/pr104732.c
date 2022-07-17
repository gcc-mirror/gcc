/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse -march=pentiumpro" } */

typedef long long v2di __attribute__((vector_size (16)));

v2di x;

void foo (long long a)
{
    v2di t = {a, a};
    x = ~t;
}

/* { dg-final { scan-assembler-times "notl" 2 } } */
