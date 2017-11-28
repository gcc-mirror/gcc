/* { dg-do compile } */
/* { dg-options "-O3 -funroll-loops -fno-tree-vectorize -fdump-tree-cunroll-details -fno-peel-loops" } */

#define N 8
#define M 14
typedef unsigned char e_u8;
e_u8 x[256];
#define MAX(a,b) ((a)>=(b)?(a):(b))
#define btype e_u8

static inline void bar1(e_u8 a[4][N], e_u8 b[4][N], btype n)
{
  int i, j;

  for(i = 0; i < 4; i++)
    for(j = 0; j < n; j++)
      a[i][j] ^= b[i][j];
}

static inline void bar2(e_u8 a[4][N], e_u8 b[256], btype n)
{
  int i, j;

  for(i = 0; i < 4; i++)
    for(j = 0; j < n; j++)
      a[i][j] = b[a[i][j]] ;
}

int foo1 (e_u8 a[4][N], int b1, int b2, e_u8 b[M+1][4][N])
{
  btype n;
  int r, m;

  switch (b2) {
    case 128: n = 4; break;
    case 192: n = 6; break;
    case 256: n = 8; break;
    default : return (-2);
  }
  switch (MAX(b1,b2)) {
    case 128: m = 10; break;
    case 192: m = 12; break;
    case 256: m = 14; break;
    default : return (-3);
  }
  bar1(a,b[m],n);
  bar2(a,x,n);
  return 0;
}

/* { dg-final { scan-tree-dump-times "loop with 3 iterations completely unrolled" 2 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "loop with 7 iterations completely unrolled" 2 "cunroll" } } */
