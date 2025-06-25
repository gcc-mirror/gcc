/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */
/* { dg-final { scan-assembler-times "vbroadcastsd" 1 } } */

typedef double v2df __attribute__((vector_size(16)));
typedef double v4df __attribute__((vector_size(32)));
typedef double v8df __attribute__((vector_size(64)));

extern v2df d1;
extern v4df d2;
extern v8df d3;

void
foo ()
{
  d1 = __extension__(v2df){2.34, 2.34};
  d2 = __extension__(v4df){2.34, 2.34, 2.34, 2.34};
  d3 = __extension__(v8df){2.34, 2.34, 2.34, 2.34, 2.34, 2.34, 2.34, 2.34};
}
