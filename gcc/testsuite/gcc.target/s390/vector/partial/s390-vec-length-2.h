#include "s390-vec-length.h"

/* Test the case loop iteration is unknown.  */

#define N 255

#define test(TYPE)                                                             \
  extern TYPE a_##TYPE[N];                                                     \
  extern TYPE b_##TYPE[N];                                                     \
  extern TYPE c_##TYPE[N];                                                     \
  void __attribute__ ((noinline, noclone)) test##TYPE (unsigned int n)         \
  {                                                                            \
    unsigned int i = 0;                                                        \
    for (i = 0; i < n; i++)                                                    \
      c_##TYPE[i] = a_##TYPE[i] + b_##TYPE[i];                                 \
  }

TEST_ALL (test)
