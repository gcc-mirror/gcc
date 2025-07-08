/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13 -ftree-vectorize -fdump-tree-optimized" } */

#define T(X,N)                                  \
  unsigned X                                    \
  reduce_and_##X (unsigned X *in)               \
  {                                             \
  unsigned X acc = (unsigned X)-1;              \
  for (int i = 0; i < N; i++)                   \
    acc &= in[i];                               \
  return acc;                                   \
  }                                             \
  unsigned X                                    \
  reduce_ior_##X (unsigned X *in)               \
  {                                             \
  unsigned X acc = 0;                           \
  for (int i = 0; i < N; i++)                   \
    acc |= in[i];                               \
  return acc;                                   \
  }                                             \
  unsigned X                                    \
  redue_xor_##X (unsigned X *in)                \
  {                                             \
  unsigned X acc = 0;                           \
  for (int i = 0; i < N; i++)                   \
    acc ^= in[i];                               \
  return acc;                                   \
  }

T(char,16)

T(short, 8)

T(int,4)

T(long,4)

/* { dg-final { scan-tree-dump-times "\.REDUC_AND" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_IOR" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_XOR" 4 "optimized" } } */
