#include "p9-vec-length.h"

/* Test the case loop iteration less than VF.  */

/* For char.  */
#define N_uint8_t 15
#define N_int8_t 15
/* For short.  */
#define N_uint16_t 6
#define N_int16_t 6
/* For int/float.  */
#define N_uint32_t 3
#define N_int32_t 3
#define N_float 3
/* For long/double.  */
#define N_uint64_t 1
#define N_int64_t 1
#define N_double 1

#define test(TYPE)                                                             \
  extern TYPE a_##TYPE[N_##TYPE];                                              \
  extern TYPE b_##TYPE[N_##TYPE];                                              \
  extern TYPE c_##TYPE[N_##TYPE];                                              \
  void __attribute__ ((noinline, noclone)) test##TYPE ()                       \
  {                                                                            \
    unsigned int i = 0;                                                        \
    for (i = 0; i < N_##TYPE; i++)                                             \
      c_##TYPE[i] = a_##TYPE[i] + b_##TYPE[i];                                 \
  }

TEST_ALL (test)
