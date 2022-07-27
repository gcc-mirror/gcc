/* { dg-do compile } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-vect-details -mprefer-vector-width=256" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 6 "vect" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(4\) double>} 1 "vect" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(8\) float>} 1 "vect" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(4\) long long int>} 1 "vect" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(8\) int>} 1 "vect" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(16\) short int>} 1 "vect" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(32\) char>} 1 "vect" } } */

#define N 10000
void
__attribute__((noipa))
foo_pd (_Complex double* a, _Complex double b)
{
  for (int i = 0; i != N; i++)
    a[i] = b;
}

void
__attribute__((noipa))
foo_ps (_Complex float* a, _Complex float b)
{
  for (int i = 0; i != N; i++)
    a[i] = b;
}

void
__attribute__((noipa))
foo_epi64 (_Complex long long* a, _Complex long long b)
{
  for (int i = 0; i != N; i++)
    a[i] = b;
}

void
__attribute__((noipa))
foo_epi32 (_Complex int* a, _Complex int b)
{
  for (int i = 0; i != N; i++)
    a[i] = b;
}

void
__attribute__((noipa))
foo_epi16 (_Complex short* a, _Complex short b)
{
  for (int i = 0; i != N; i++)
    a[i] = b;
}

void
__attribute__((noipa))
foo_epi8 (_Complex char* a, _Complex char b)
{
  for (int i = 0; i != N; i++)
    a[i] = b;
}
