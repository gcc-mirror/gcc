/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */
/* { dg-final { scan-assembler-times "vpbroadcastq" 1 } } */

typedef long long v2di __attribute__((vector_size(16)));
typedef long long v4di __attribute__((vector_size(32)));
typedef long long v8di __attribute__((vector_size(64)));

extern v2di d1;
extern v4di d2;
extern v8di d3;

void
foo (long long a1, long long a2, long long a3, long long a4,
     long long a5, long long a6, long long a7)
{
  d1 = __extension__(v2di){a7, a7};
  d2 = __extension__(v4di){a7, a7, a7, a7};
  d3 = __extension__(v8di){a7, a7, a7, a7, a7, a7, a7, a7};
}
