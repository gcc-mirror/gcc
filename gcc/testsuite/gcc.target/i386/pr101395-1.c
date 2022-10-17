/* { dg-do compile } */
/* { dg-options "-O2 -march=sapphirerapids" } */

#ifdef __x86_64__
# ifndef __UINTR__
#  error UINTR is not enabled for Sapphirerapids
# endif
#else
# ifdef __UINTR__
#  error UINTR is not usable in 32-bit mode
# endif
#endif
