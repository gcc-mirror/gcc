/* { dg-do compile } */
/* { dg-options "-O1 -march=x86-64-v4" } */
/* { dg-final { scan-assembler-times "vpbroadcastd" 3 } } */

typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v16si __attribute__((vector_size(64)));

extern v4si *s1;
extern v8si *s2;
extern v16si *s3;

int
foo (int i, int j)
{
  if (j == 1)
   s1[i] = __extension__(v4si){34, 34, 34, 34};
  else if (i == 1)
    s2[j] = __extension__(v8si){34, 34, 34, 34, 34, 34, 34, 34};
  if ((i + j) == 1234)
    i = foo (j, i);
  s3[i + j] = __extension__(v16si){34, 34, 34, 34, 34, 34, 34, 34,
				   34, 34, 34, 34, 34, 34, 34, 34};
  return i - j;
}
