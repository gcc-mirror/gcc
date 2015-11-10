/* { dg-do compile } */
/* { dg-options "-mavx512bw -O3 -fopenmp-simd -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 10 "vect" } } */
/* { dg-final { scan-assembler-not "maskmov" } } */

#define LENGTH 1000

long l1[LENGTH], l2[LENGTH];
int i1[LENGTH], i2[LENGTH];
short s1[LENGTH], s2[LENGTH];
char c1[LENGTH], c2[LENGTH];
double d1[LENGTH], d2[LENGTH];

int test1 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (l1[i] > l2[i])
      i1[i] = 1;
}

int test2 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (i1[i] > i2[i])
      s1[i] = 1;
}

int test3 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (s1[i] > s2[i])
      c1[i] = 1;
}

int test4 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (d1[i] > d2[i])
      c1[i] = 1;
}

int test5 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    i1[i] = l1[i] > l2[i] ? 3 : 4;
}

int test6 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    s1[i] = i1[i] > i2[i] ? 3 : 4;
}

int test7 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    c1[i] = s1[i] > s2[i] ? 3 : 4;
}

int test8 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    c1[i] = d1[i] > d2[i] ? 3 : 4;
}

int test9 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (l1[i] > l2[i] && i1[i] < i2[i])
      c1[i] = 1;
}

int test10 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (l1[i] > l2[i] && i1[i] < i2[i])
      c1[i] = 1;
    else
      c1[i] = 2;
}
