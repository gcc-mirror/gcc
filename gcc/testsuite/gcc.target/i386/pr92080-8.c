/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastd" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq" 1 } } */

typedef int v4si __attribute__((vector_size(16)));
typedef long long int v2di __attribute__((vector_size(16)));
extern v4si s;
extern v2di l;

void
foo(void)
{
  l = __extension__(v2di){2,2};
  s = __extension__(v4si){2,2,2,2};
}
