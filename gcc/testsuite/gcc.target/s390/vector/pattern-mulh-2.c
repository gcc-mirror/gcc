/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -mzarch -march=arch15 -ftree-vectorize -fdump-tree-optimized" } */

#define TEST(T1,T2,N,S)                                                 \
  void                                                                  \
  mulh##T1 (signed T1 *__restrict res,                                  \
            signed T1 *__restrict l,                                    \
            signed T1 *__restrict r)                                    \
  {                                                                     \
    for (int i = 0; i < N; ++i)                                         \
      res[i] = (signed T1) (((signed T2)l[i] * (signed T2)r[i]) >> S);  \
  }                                                                     \
                                                                        \
  void                                                                  \
  umulh##T1 (unsigned T1 *__restrict res,                               \
             unsigned T1 *__restrict l,                                 \
             unsigned T1 *__restrict r)                                 \
  {                                                                     \
    for (int i = 0; i < N; ++i)                                         \
      res[i] = (unsigned T1)                                            \
        (((unsigned T2)l[i] * (unsigned T2)r[i]) >> S);                 \
  }

TEST(int,long,4,32)
TEST(long,__int128,2,64)

/* { dg-final { scan-tree-dump-times "\.MULH" 4 "optimized" } } */
