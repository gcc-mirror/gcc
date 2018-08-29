/* PR tree-optimization/85467 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp" } */

#define TEST(N, T) \
typedef T V##N __attribute__ ((__vector_size__ (sizeof (T))));	\
								\
V##N								\
bar##N (V##N u, V##N v)						\
{								\
  do								\
    v *= (T)((V##N){}[0] ? u[v[0]] : 0);			\
  while ((V##N){}[0]);						\
  return v;							\
}								\
								\
void								\
foo##N (void)							\
{								\
  bar##N ((V##N){}, (V##N){});					\
}

TEST (1, char)
TEST (2, short)
TEST (3, int)
TEST (4, long)
TEST (5, long long)
#ifdef __SIZEOF_INT128__
TEST (6, __int128)
#endif
