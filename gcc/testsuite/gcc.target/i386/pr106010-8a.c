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
foo_pd (_Complex double* a)
{
  for (int i = 0; i != N; i++)
    a[i] = 1.0 + 2.0i;
}

void
__attribute__((noipa))
foo_ps (_Complex float* a)
{
  for (int i = 0; i != N; i++)
    a[i] = 1.0f + 2.0fi;
}

void
__attribute__((noipa))
foo_epi64 (_Complex long long* a)
{
  for (int i = 0; i != N; i++)
    a[i] = 1 + 2i;
}

void
__attribute__((noipa))
foo_epi32 (_Complex int* a)
{
  for (int i = 0; i != N; i++)
    a[i] = 1 + 2i;
}

void
__attribute__((noipa))
foo_epi16 (_Complex short* a)
{
  for (int i = 0; i != N; i++)
    a[i] = 1 + 2i;
}

void
__attribute__((noipa))
foo_epi8 (_Complex char* a)
{
  for (int i = 0; i != N; i++)
    a[i] = 1 + 2i;
}
