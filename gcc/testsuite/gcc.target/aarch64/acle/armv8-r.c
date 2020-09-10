/* { dg-do compile } */
/* { dg-options "-march=armv8-r" } */

#if __ARM_ARCH_PROFILE != 'R'
#error ACLE architecture profile macro incorrect
#endif
