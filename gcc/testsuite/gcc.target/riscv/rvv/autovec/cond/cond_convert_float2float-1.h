#include <stdint-gcc.h>

#define DEF_LOOP(OLD_TYPE, NEW_TYPE)                                           \
  void __attribute__ ((noipa))                                                 \
  test_##OLD_TYPE##_2_##NEW_TYPE (NEW_TYPE *__restrict r,                      \
				  OLD_TYPE *__restrict a,                      \
				  NEW_TYPE *__restrict b,                      \
				  OLD_TYPE *__restrict pred, int n)            \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	r[i] = pred[i] ? (NEW_TYPE) a[i] : b[i];                               \
      }                                                                        \
  }

/* FP -> wider-FP */
#define TEST_ALL_F2F_WIDER(T)                                                  \
  T (_Float16, float)                                                          \
  T (_Float16, double)                                                         \
  T (float, double)

/* FP -> narrower-FP */
#define TEST_ALL_F2F_NARROWER(T)                                               \
  T (float, _Float16)                                                          \
  T (double, _Float16)                                                         \
  T (double, float)

TEST_ALL_F2F_WIDER (DEF_LOOP)
TEST_ALL_F2F_NARROWER (DEF_LOOP)
