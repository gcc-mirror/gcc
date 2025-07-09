/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z16 -ftree-vectorize -fdump-tree-optimized" } */

#define TEST(T1,T2,N)                                                   \
  void                                                                  \
  avg##T1 (signed T1 *__restrict res, signed T1 *__restrict a,          \
           signed T1 *__restrict b)                                     \
  {                                                                     \
    for (int i = 0; i < N; ++i)                                         \
      res[i] = ((signed T2)a[i] + b[i] + 1) >> 1;                       \
  }                                                                     \
                                                                        \
  void                                                                  \
  uavg##T1 (unsigned T1 *__restrict res, unsigned T1 *__restrict a,     \
            unsigned T1 *__restrict b)                                  \
  {                                                                     \
    for (int i = 0; i < N; ++i)                                         \
      res[i] = ((unsigned T2)a[i] + b[i] + 1) >> 1;                     \
  }

TEST(char,short,16)
TEST(short,int,8)
TEST(int,long,4)

/* { dg-final { scan-tree-dump-times "\.AVG_CEIL" 6 "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump-times "\.AVG_CEIL" 4 "optimized" { target { ! lp64 } } } } */
