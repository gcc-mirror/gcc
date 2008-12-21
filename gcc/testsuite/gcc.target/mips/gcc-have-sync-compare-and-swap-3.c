/* { dg-options "isa>=2 -mgp32 -mips16" } */

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
