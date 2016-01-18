/* { dg-do compile } */
/* { dg-additional-options "-march=core-avx2 -fdump-tree-slp-details" } */

#define abs(x) (x) < 0 ? -(x) : (x)
int
foo (int* diff)
{
  int k, satd = 0, m[16], d[16];
  
    m[ 0] = diff[ 0] + diff[12];
    m[ 4] = diff[ 4] + diff[ 8];
    m[ 8] = diff[ 4] - diff[ 8];
    m[12] = diff[ 0] - diff[12];
    m[ 1] = diff[ 1] + diff[13];
    m[ 5] = diff[ 5] + diff[ 9];
    m[ 9] = diff[ 5] - diff[ 9];
    m[13] = diff[ 1] - diff[13];
    m[ 2] = diff[ 2] + diff[14];
    m[ 6] = diff[ 6] + diff[10];
    m[10] = diff[ 6] - diff[10];
    m[14] = diff[ 2] - diff[14];
    m[ 3] = diff[ 3] + diff[15];
    m[ 7] = diff[ 7] + diff[11];
    m[11] = diff[ 7] - diff[11];
    m[15] = diff[ 3] - diff[15];
    
    d[ 0] = m[ 0] + m[ 4];
    d[ 8] = m[ 0] - m[ 4];
    d[ 4] = m[ 8] + m[12];
    d[12] = m[12] - m[ 8];
    d[ 1] = m[ 1] + m[ 5];
    d[ 9] = m[ 1] - m[ 5];
    d[ 5] = m[ 9] + m[13];
    d[13] = m[13] - m[ 9];
    d[ 2] = m[ 2] + m[ 6];
    d[10] = m[ 2] - m[ 6];
    d[ 6] = m[10] + m[14];
    d[14] = m[14] - m[10];
    d[ 3] = m[ 3] + m[ 7];
    d[11] = m[ 3] - m[ 7];
    d[ 7] = m[11] + m[15];
    d[15] = m[15] - m[11];
    
    m[ 0] = d[ 0] + d[ 3];
    m[ 1] = d[ 1] + d[ 2];
    m[ 2] = d[ 1] - d[ 2];
    m[ 3] = d[ 0] - d[ 3];
    m[ 4] = d[ 4] + d[ 7];
    m[ 5] = d[ 5] + d[ 6];
    m[ 6] = d[ 5] - d[ 6];
    m[ 7] = d[ 4] - d[ 7];
    m[ 8] = d[ 8] + d[11];
    m[ 9] = d[ 9] + d[10];
    m[10] = d[ 9] - d[10];
    m[11] = d[ 8] - d[11];
    m[12] = d[12] + d[15];
    m[13] = d[13] + d[14];
    m[14] = d[13] - d[14];
    m[15] = d[12] - d[15];
    
    d[ 0] = m[ 0] + m[ 1];
    d[ 1] = m[ 0] - m[ 1];
    d[ 2] = m[ 2] + m[ 3];
    d[ 3] = m[ 3] - m[ 2];
    d[ 4] = m[ 4] + m[ 5];
    d[ 5] = m[ 4] - m[ 5];
    d[ 6] = m[ 6] + m[ 7];
    d[ 7] = m[ 7] - m[ 6];
    d[ 8] = m[ 8] + m[ 9];
    d[ 9] = m[ 8] - m[ 9];
    d[10] = m[10] + m[11];
    d[11] = m[11] - m[10];
    d[12] = m[12] + m[13];
    d[13] = m[12] - m[13];
    d[14] = m[14] + m[15];
    d[15] = m[15] - m[14];
    for (k=0; k<16; k++)
      satd += abs(d[k]);
  return satd;
}

/* { dg-final { scan-tree-dump "vectorization is not profitable" "slp1" } } */
/* { dg-final { scan-tree-dump-not "basic block vectorized" "slp1" } } */
