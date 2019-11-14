/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512dq -mno-stackrealign -O3 -fopenmp-simd -fdump-tree-vect-details" } */
/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 10 "vect" } } */
/* { dg-final { scan-assembler-not "maskmov" } } */

#define LENGTH 1000

long l1[LENGTH], l2[LENGTH];
int i1[LENGTH], i2[LENGTH];
short s1[LENGTH], s2[LENGTH];
char c1[LENGTH], c2[LENGTH];
double d1[LENGTH], d2[LENGTH];

int test1 ()
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (i1[i] > i2[i])
      l1[i] = 1;
}

int test2 (int n)
{
  int i;
  #pragma omp simd safelen(32)
  for (i = 0; i < LENGTH; i++)
    if (s1[i] > s2[i])
      i1[i] = 1;
}

int test3 (int n)
{
  int i;
  #pragma omp simd safelen(32)
  for (i = 0; i < LENGTH; i++)
    if (c1[i] > c2[i])
      s1[i] = 1;
}

int test4 (int n)
{
  int i;
  #pragma omp simd safelen(32)
  for (i = 0; i < LENGTH; i++)
    if (c1[i] > c2[i])
      d1[i] = 1;
}

int test5 (int n)
{
  int i;
  #pragma omp simd safelen(32)
  for (i = 0; i < LENGTH; i++)
    l1[i] = i1[i] > i2[i] ? 1 : 2;
}

int test6 (int n)
{
  int i;
  #pragma omp simd safelen(32)
  for (i = 0; i < LENGTH; i++)
    i1[i] = s1[i] > s2[i] ? 1 : 2;
}

int test7 (int n)
{
  int i;
  #pragma omp simd safelen(32)
  for (i = 0; i < LENGTH; i++)
    s1[i] = c1[i] > c2[i] ? 1 : 2;
}

int test8 (int n)
{
  int i;
  #pragma omp simd safelen(32)
  for (i = 0; i < LENGTH; i++)
    d1[i] = c1[i] > c2[i] ? 1 : 2;
}

int test9 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (c1[i] > c2[i] && i1[i] < i2[i])
      l1[i] = 1;
}

int test10 (int n)
{
  int i;
  #pragma omp simd safelen(16)
  for (i = 0; i < LENGTH; i++)
    if (c1[i] > c2[i] && i1[i] < i2[i])
      l1[i] = 1;
    else
      l1[i] = 2;
}
