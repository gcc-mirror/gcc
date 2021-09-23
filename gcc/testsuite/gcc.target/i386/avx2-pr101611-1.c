/* PR target/101611 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */
/* { dg-final { scan-assembler-times {\mvpsrlvq\M} 4 } } */
/* { dg-final { scan-assembler-times {\mvpxor\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvpsubq\M} 2 } } */

typedef long long V __attribute__((vector_size(32)));
typedef long long W __attribute__((vector_size(16)));

V foo (V a, V b) { return a >> b; }
W bar (W a, W b) { return a >> b; }
