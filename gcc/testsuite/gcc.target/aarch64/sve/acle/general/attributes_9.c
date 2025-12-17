#include <arm_sve.h>

#if __ARM_FEATURE_SVE_VECTOR_OPERATORS != 2
#error "__ARM_FEATURE_SVE_VECTOR_OPERATORS should be equal to 2"
#endif

#if __ARM_FEATURE_SVE_PREDICATE_OPERATORS != 2
#error "__ARM_FEATURE_SVE_PREDICATE_OPERATORS should be equal to 2"
#endif
