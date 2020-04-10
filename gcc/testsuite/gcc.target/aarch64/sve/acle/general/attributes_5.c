/* { dg-options "-msve-vector-bits=128" } */

#if __ARM_BIG_ENDIAN && !__ARM_FEATURE_SVE_BITS
int pass = 1;
#else
#include "attributes_1.c"
#endif
