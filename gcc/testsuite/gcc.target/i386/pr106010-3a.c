/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -ftree-vectorize -fvect-cost-model=unlimited -fdump-tree-slp-details" } */
/* { dg-final { scan-tree-dump-times "basic block part vectorized using (?:32|64) byte vectors" 6 "slp2" } }*/
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*VEC_PERM_EXPR.*\{ 2, 3, 0, 1 \}} 2 "slp2" } }  */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*VEC_PERM_EXPR.*\{ 6, 7, 4, 5, 2, 3, 0, 1 \}} 1 "slp2" } }  */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*VEC_PERM_EXPR.*\{ 2, 3, 0, 1, 6, 7, 4, 5 \}} 1 "slp2" } }  */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*VEC_PERM_EXPR.*\{ 14, 15, 12, 13, 10, 11, 8, 9, 6, 7, 4, 5, 2, 3, 0, 1 \}} 1 "slp2" } }  */
/* { dg-final { scan-tree-dump-times {(?n)add new stmt:.*VEC_PERM_EXPR.*\{ 14, 15, 12, 13, 10, 11, 8, 9, 6, 7, 4, 5, 2, 3, 0, 1, 30, 31, 28, 29, 26, 27, 24, 25, 22, 23, 20, 21, 18, 19, 16, 17 \}} 1 "slp2" } }  */

void
__attribute__((noipa))
foo_pd (_Complex double* a, _Complex double* __restrict b)
{
  a[0] = b[1];
  a[1] = b[0];
}

void
__attribute__((noipa))
foo_ps (_Complex float* a, _Complex float* __restrict b)
{
  a[0] = b[1];
  a[1] = b[0];
  a[2] = b[3];
  a[3] = b[2];
}

void
__attribute__((noipa))
foo_epi64 (_Complex long long* a, _Complex long long* __restrict b)
{
  a[0] = b[1];
  a[1] = b[0];
}

void
__attribute__((noipa))
foo_epi32 (_Complex int* a, _Complex int* __restrict b)
{
  a[0] = b[3];
  a[1] = b[2];
  a[2] = b[1];
  a[3] = b[0];
}

void
__attribute__((noipa))
foo_epi16 (_Complex short* a, _Complex short* __restrict b)
{
  a[0] = b[7];
  a[1] = b[6];
  a[2] = b[5];
  a[3] = b[4];
  a[4] = b[3];
  a[5] = b[2];
  a[6] = b[1];
  a[7] = b[0];
}

void
__attribute__((noipa))
foo_epi8 (_Complex char* a, _Complex char* __restrict b)
{
  a[0] = b[7];
  a[1] = b[6];
  a[2] = b[5];
  a[3] = b[4];
  a[4] = b[3];
  a[5] = b[2];
  a[6] = b[1];
  a[7] = b[0];
  a[8] = b[15];
  a[9] = b[14];
  a[10] = b[13];
  a[11] = b[12];
  a[12] = b[11];
  a[13] = b[10];
  a[14] = b[9];
  a[15] = b[8];
}
