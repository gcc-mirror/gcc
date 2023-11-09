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

/* FP -> INT */
#define TEST_ALL_F2X_SAME(T)                                                   \
  T (_Float16, uint16_t)                                                       \
  T (_Float16, int16_t)                                                        \

/* FP -> wider-INT */
#define TEST_ALL_F2X_WIDER(T)                                                  \
  T (_Float16, uint32_t)                                                       \
  T (_Float16, int32_t)                                                        \
  T (_Float16, uint64_t)                                                       \
  T (_Float16, int64_t)                                                        \

/* FP -> narrower-INT */
#define TEST_ALL_F2X_NARROWER(T)                                               \
  T (_Float16, uint8_t)                                                        \
  T (_Float16, int8_t)                                                         \

TEST_ALL_F2X_SAME (DEF_LOOP)
TEST_ALL_F2X_WIDER (DEF_LOOP)
TEST_ALL_F2X_NARROWER (DEF_LOOP)
