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

/* INT -> wider-INT */
#define TEST_ALL_X2X_WIDER(T)                                                  \
  T (uint8_t, uint16_t)                                                        \
  T (uint8_t, uint32_t)                                                        \
  T (uint8_t, uint64_t)                                                        \
  T (int8_t, int16_t)                                                          \
  T (int8_t, int32_t)                                                          \
  T (int8_t, int64_t)                                                          \
  T (uint16_t, uint32_t)                                                       \
  T (uint16_t, uint64_t)                                                       \
  T (int16_t, int32_t)                                                         \
  T (int16_t, int64_t)                                                         \
  T (uint32_t, uint64_t)                                                       \
  T (int32_t, int64_t)

/* INT -> narrower-INT */
#define TEST_ALL_X2X_NARROWER(T)                                               \
  T (uint16_t, uint8_t)                                                        \
  T (int16_t, int8_t)                                                          \
  T (uint32_t, uint8_t)                                                        \
  T (int32_t, int8_t)                                                          \
  T (uint64_t, uint8_t)                                                        \
  T (int64_t, int8_t)                                                          \
  T (uint32_t, uint16_t)                                                       \
  T (int32_t, int16_t)                                                         \
  T (uint64_t, uint16_t)                                                       \
  T (int64_t, int16_t)                                                         \
  T (uint64_t, uint32_t)                                                       \
  T (int64_t, int32_t)

TEST_ALL_X2X_WIDER (DEF_LOOP)
TEST_ALL_X2X_NARROWER (DEF_LOOP)
