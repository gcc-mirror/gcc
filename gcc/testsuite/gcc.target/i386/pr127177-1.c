/* PR target/127177 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mno-avx512f -ftrapping-math -fdump-tree-vect-details" } */

/* The unordered compares are quiet, under AVX use the _uq predicates by
   force.  */

#define TEST(NAME, EXPR)			\
  void NAME (int *r, float *a, float *b)	\
  {						\
    for (int i = 0; i < 1024; i++)		\
      r[i] = EXPR;				\
  }

TEST (t_isless, __builtin_isless (a[i], b[i]))
TEST (t_islessequal, __builtin_islessequal (a[i], b[i]))
TEST (t_isgreater, __builtin_isgreater (a[i], b[i]))
TEST (t_isgreaterequal, __builtin_isgreaterequal (a[i], b[i]))
TEST (t_uneq, !__builtin_islessgreater (a[i], b[i]))

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 5 "vect" } } */
/* { dg-final { scan-assembler-times "vcmpnlt_uqps" 2 } } */
/* { dg-final { scan-assembler-times "vcmpnle_uqps" 2 } } */
/* { dg-final { scan-assembler-times "vcmpeq_uqps" 1 } } */
/* { dg-final { scan-assembler-not "vcmpnltps" } } */
/* { dg-final { scan-assembler-not "vcmpnleps" } } */
/* { dg-final { scan-assembler-not "vcmpeq_usps" } } */
