#include <stdint-gcc.h>

#define DEF_LOOP(OLD_TYPE, NEW_TYPE)                                           \
  void __attribute__ ((noipa))                                                 \
  test_##OLD_TYPE##_2_##NEW_TYPE (NEW_TYPE *__restrict r,                      \
				  OLD_TYPE *__restrict a, NEW_TYPE b,          \
				  OLD_TYPE *__restrict pred, int n)            \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	r[i] = pred[i] ? (NEW_TYPE) a[i] : b;                                  \
      }                                                                        \
  }

/* INT -> FP */
#define TEST_ALL_X2F_SAME(T)                                                   \
  T (uint16_t, _Float16)                                                       \
  T (int16_t, _Float16)                                                        \
  T (uint32_t, float)                                                          \
  T (int32_t, float)                                                           \
  T (uint64_t, double)                                                         \
  T (int64_t, double)

/* INT -> wider-FP */
#define TEST_ALL_X2F_WIDER(T)                                                  \
  T (uint16_t, float)                                                          \
  T (int16_t, float)                                                           \
  T (uint16_t, double)                                                         \
  T (int16_t, double)                                                          \
  T (uint32_t, double)                                                         \
  T (int32_t, double)

/* INT -> narrower-FP */
#define TEST_ALL_X2F_NARROWER(T)                                               \
  T (uint32_t, _Float16)                                                       \
  T (int32_t, _Float16)                                                        \
  T (uint64_t, _Float16)                                                       \
  T (int64_t, _Float16)                                                        \
  T (uint64_t, float)                                                          \
  T (int64_t, float)

TEST_ALL_X2F_SAME (DEF_LOOP)
TEST_ALL_X2F_WIDER (DEF_LOOP)
TEST_ALL_X2F_NARROWER (DEF_LOOP)
