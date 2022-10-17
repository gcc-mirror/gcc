/* { dg-do compile } */
/* { dg-options "-O2 -mavx -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-slp-details -mprefer-vector-width=256" } */
/* { dg-final { scan-tree-dump-times "basic block part vectorized using (?:32|64) byte vectors" 6 "slp2" } }*/
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(4\) double>} 4 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(8\) float>} 4 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(4\) long long int>} 4 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(8\) int>} 4 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(16\) short int>} 4 "slp2" } } */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*MEM <vector\(32\) char>} 4 "slp2" } } */

void
__attribute__((noipa))
foo_pd (_Complex double* a, _Complex double* __restrict b)
{
  a[0] = b[2];
  a[1] = b[3];
  a[2] = b[0];
  a[3] = b[1];
}

void
__attribute__((noipa))
foo_ps (_Complex float* a, _Complex float* __restrict b)
{
  a[0] = b[4];
  a[1] = b[5];
  a[2] = b[6];
  a[3] = b[7];
  a[4] = b[0];
  a[5] = b[1];
  a[6] = b[2];
  a[7] = b[3];
}

void
__attribute__((noipa))
foo_epi64 (_Complex long long* a, _Complex long long* __restrict b)
{
  a[0] = b[2];
  a[1] = b[3];
  a[2] = b[0];
  a[3] = b[1];
}

void
__attribute__((noipa))
foo_epi32 (_Complex int* a, _Complex int* __restrict b)
{
  a[0] = b[4];
  a[1] = b[5];
  a[2] = b[6];
  a[3] = b[7];
  a[4] = b[0];
  a[5] = b[1];
  a[6] = b[2];
  a[7] = b[3];
}

void
__attribute__((noipa))
foo_epi16 (_Complex short* a, _Complex short* __restrict b)
{
  a[0] = b[8];
  a[1] = b[9];
  a[2] = b[10];
  a[3] = b[11];
  a[4] = b[12];
  a[5] = b[13];
  a[6] = b[14];
  a[7] = b[15];
  a[8] = b[0];
  a[9] = b[1];
  a[10] = b[2];
  a[11] = b[3];
  a[12] = b[4];
  a[13] = b[5];
  a[14] = b[6];
  a[15] = b[7];
}

void
__attribute__((noipa))
foo_epi8 (_Complex char* a, _Complex char* __restrict b)
{
  a[0] = b[16];
  a[1] = b[17];
  a[2] = b[18];
  a[3] = b[19];
  a[4] = b[20];
  a[5] = b[21];
  a[6] = b[22];
  a[7] = b[23];
  a[8] = b[24];
  a[9] = b[25];
  a[10] = b[26];
  a[11] = b[27];
  a[12] = b[28];
  a[13] = b[29];
  a[14] = b[30];
  a[15] = b[31];
  a[16] = b[0];
  a[17] = b[1];
  a[18] = b[2];
  a[19] = b[3];
  a[20] = b[4];
  a[21] = b[5];
  a[22] = b[6];
  a[23] = b[7];
  a[24] = b[8];
  a[25] = b[9];
  a[26] = b[10];
  a[27] = b[11];
  a[28] = b[12];
  a[29] = b[13];
  a[30] = b[14];
  a[31] = b[15];
}
