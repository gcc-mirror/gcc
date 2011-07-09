/* { dg-do preprocess } */
/* { dg-require-effective-target ia32 } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=i486" } } */
/* { dg-options "-march=i486" } */

#ifndef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1
#error nonono
#endif

#ifndef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2
#error nonono
#endif

#ifndef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4
#error nonono
#endif

#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_8
#error nonono
#endif

#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_16
#error nonono
#endif

