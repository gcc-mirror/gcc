#ifndef AVX10_HELPER_INCLUDED
#define AVX10_HELPER_INCLUDED

#define AVX10
#define AVX512FP16

#include "avx512f-helper.h"
#include "avx512f-mask-type.h"

#endif /* AVX10_HELPER_INCLUDED */

/* Intrinsic being tested. It has different deffinitions,
   depending on AVX512F_LEN, so it's outside include guards
   and in undefed away to silence warnings.  */
#if defined INTRINSIC
#undef INTRINSIC
#endif

#if AVX512F_LEN != 128
#define INTRINSIC(NAME) EVAL(_mm, AVX512F_LEN, NAME)
#else
#define INTRINSIC(NAME) _mm ## NAME
#endif
