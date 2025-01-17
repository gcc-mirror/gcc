/* Template file for scalar saturating arithmetic validation.

   This file defines scalar saturating addition and subtraction functions for a
   given type. This type, along with the corresponding minimum and maximum
   values for that type, must be defined by any test file which includes this
   template file.  */

#ifndef SAT_ARIT_INC
#define SAT_ARIT_INC

#include <limits.h>

#ifndef UT
#define UT unsigned int
#define UMAX UINT_MAX
#define UMIN 0
#endif

UT uadd (UT a, UT b)
{
        UT sum = a + b;
        return sum < a ? UMAX : sum;
}

UT uadd2 (UT a, UT b)
{
        UT c;
        if (!__builtin_add_overflow(a, b, &c))
                return c;
        return UMAX;
}

UT usub (UT a, UT b)
{
        UT sum = a - b;
        return sum > a ? UMIN : sum;
}

#endif