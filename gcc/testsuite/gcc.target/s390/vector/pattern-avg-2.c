/* { dg-do compile { target int128 } } */
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

TEST(long,__int128,2)

/* { dg-final { scan-tree-dump-times "\.AVG_CEIL" 2 "optimized" } } */
