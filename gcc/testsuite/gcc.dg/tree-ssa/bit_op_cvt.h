#ifndef HAVE_DEFINED_BIT_OP_CVT
#define HAVE_DEFINED_BIT_OP_CVT

#include <stdint.h>

#define DEF_BIT_CVT_0(T1, T2, T3, OP, NAME) \
T1 test_bit_##NAME##_##T1##_##T2##_##T3##_0(T2 a, \
					    T3 b) \
{ \
  return (T1)(a OP (T3)b); \
}

#endif
