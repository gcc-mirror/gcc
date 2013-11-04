/* Test __GCC_IEC_559 and __GCC_IEC_559_COMPLEX macros values.  */
/* { dg-do preprocess { target i?86-*-linux* x86_64-*-linux* powerpc*-*-linux* } } */
/* { dg-options "-std=c11" } */

#ifndef __GCC_IEC_559
# error "__GCC_IEC_559 not defined"
#endif
#ifndef __GCC_IEC_559_COMPLEX
# error "__GCC_IEC_559_COMPLEX not defined"
#endif
#if __GCC_IEC_559_COMPLEX > __GCC_IEC_559
# error "__GCC_IEC_559_COMPLEX > __GCC_IEC_559"
#endif
#if __GCC_IEC_559_COMPLEX < 0
# error "__GCC_IEC_559_COMPLEX < 0"
#endif

#if __GCC_IEC_559 < 2
# error "__GCC_IEC_559 < 2"
#endif
#if __GCC_IEC_559_COMPLEX < 2
# error "__GCC_IEC_559_COMPLEX < 2"
#endif
