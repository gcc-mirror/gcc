/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-final { scan-assembler-times {(?n)vpermilp[ds]} 2} } */
/* { dg-final { scan-assembler-not {(?n)vperm[dq]} } } */


typedef long long v4di __attribute__((vector_size(32)));
typedef int v8si __attribute__((vector_size(32)));

v4di
foo (v4di a)
{
  return __builtin_shufflevector (a, a, 1, 0, 3, 2);
}

v8si
foo1 (v8si a)
{
  return __builtin_shufflevector (a, a, 1, 0, 3, 2, 7, 6, 5, 4);
}

