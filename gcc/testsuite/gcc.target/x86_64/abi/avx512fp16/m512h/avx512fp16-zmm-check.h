#define AVX512VL(ebx) 1
#define XSTATE_MASK (XSTATE_SSE | XSTATE_YMM | XSTATE_ZMM \
		     | XSTATE_HI_ZMM | XSTATE_OPMASK)
#include "../avx512fp16-check.h"
