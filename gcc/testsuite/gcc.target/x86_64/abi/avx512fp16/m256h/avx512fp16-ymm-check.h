#define AVX512VL(ebx) (ebx & bit_AVX512VL)
#define XSTATE_MASK (XSTATE_SSE | XSTATE_YMM | XSTATE_OPMASK)
#include "../avx512fp16-check.h"
