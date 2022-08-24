/* { dg-do compile } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-slp-details -mprefer-vector-width=256" } */
/* { dg-final { scan-tree-dump-times "basic block part vectorized using (?:32|64) byte vectors" 6 "slp2" } }*/
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(4\) double>} 2 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(8\) float>} 2 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(4\) long long int>} 2 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(8\) int>} 2 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(16\) short int>} 2 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(32\) char>} 2 "slp2" } } */

void
__attribute__((noipa))
foo_pd (_Complex double* a, _Complex double* __restrict b)
{
  a[0] = b[0];
  a[1] = b[1];
}

void
__attribute__((noipa))
foo_ps (_Complex float* a, _Complex float* __restrict b)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];

}

void
__attribute__((noipa))
foo_epi64 (_Complex long long* a, _Complex long long* __restrict b)
{
  a[0] = b[0];
  a[1] = b[1];
}

void
__attribute__((noipa))
foo_epi32 (_Complex int* a, _Complex int* __restrict b)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
}

void
__attribute__((noipa))
foo_epi16 (_Complex short* a, _Complex short* __restrict b)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
  a[4] = b[4];
  a[5] = b[5];
  a[6] = b[6];
  a[7] = b[7];
}

void
__attribute__((noipa))
foo_epi8 (_Complex char* a, _Complex char* __restrict b)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
  a[4] = b[4];
  a[5] = b[5];
  a[6] = b[6];
  a[7] = b[7];
  a[8] = b[8];
  a[9] = b[9];
  a[10] = b[10];
  a[11] = b[11];
  a[12] = b[12];
  a[13] = b[13];
  a[14] = b[14];
  a[15] = b[15];
}
