/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */
/* { dg-final { scan-assembler-times "xor\[a-z\]*\[\t \]*%xmm\[0-9\]\+,\[^,\]*" 1 } } */

typedef char v4qi __attribute__((vector_size(4)));
typedef char v16qi __attribute__((vector_size(16)));

v4qi a;
v16qi b;
void
foo (v4qi* c, v16qi* d)
{
    v4qi sum = __extension__(v4qi){0, 0, 0, 0};
    v16qi sum2 = __extension__(v16qi){0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0};
    for (int i = 0; i != 100; i++)
     sum += c[i];
    for (int i = 0 ; i != 100; i++)
      sum2 += d[i];
    a = sum;
    b = sum2;
}
