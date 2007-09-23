/* { dg-do preprocess } */
/* { dg-mips-options "-mgp64" } */

#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1
#error nonono
#endif

#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2
#error nonono
#endif

#if defined (__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4) == defined (__mips16)
#error nonono
#endif

#if defined (__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8) == defined (__mips16)
#error nonono
#endif

#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_16
#error nonono
#endif
