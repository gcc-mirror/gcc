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

/* FP -> INT */
#define TEST_ALL_F2X_SAME(T)                                                   \
  T (float, uint32_t)                                                          \
  T (float, int32_t)                                                           \
  T (double, uint64_t)                                                         \
  T (double, int64_t)

/* FP -> wider-INT */
#define TEST_ALL_F2X_WIDER(T)                                                  \
  T (float, uint64_t)                                                          \
  T (float, int64_t)

/* FP -> narrower-INT */
#define TEST_ALL_F2X_NARROWER(T)                                               \
  T (float, uint8_t)                                                           \
  T (float, int8_t)                                                            \
  T (float, uint16_t)                                                          \
  T (float, int16_t)                                                           \
  T (double, uint8_t)                                                          \
  T (double, int8_t)                                                           \
  T (double, uint16_t)                                                         \
  T (double, int16_t)                                                          \
  T (double, uint32_t)                                                         \
  T (double, int32_t)

TEST_ALL_F2X_SAME (DEF_LOOP)
TEST_ALL_F2X_WIDER (DEF_LOOP)
TEST_ALL_F2X_NARROWER (DEF_LOOP)
