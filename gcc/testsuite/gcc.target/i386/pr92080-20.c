/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */
/* { dg-final { scan-assembler-times "vbroadcastsd" 1 } } */

typedef double v2di __attribute__((vector_size(16)));
typedef double v4di __attribute__((vector_size(32)));
typedef double v8di __attribute__((vector_size(64)));

extern v2di d1;
extern v4di d2;
extern v8di d3;

void
foo (double a1, double a2, double a3, double a4,
     double a5, double a6, double a7)
{
  d1 = __extension__(v2di){a7, a7};
  d2 = __extension__(v4di){a7, a7, a7, a7};
  d3 = __extension__(v8di){a7, a7, a7, a7, a7, a7, a7, a7};
}
