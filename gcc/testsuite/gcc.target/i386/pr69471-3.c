/* { dg-do compile } */
/* { dg-options "-march=native -march=knl" } */

/* NB: We want to verify that -march=native -march=processor is the same
   as -march=processor.  Since it is very unlikely that GCC will be built
   on KNL, -march=native will have -mno-avx512er and -march=knl should
   enable AVX512ER.  */

#ifndef __AVX512ER__
# error __AVX512ER__ is not defined
#endif
