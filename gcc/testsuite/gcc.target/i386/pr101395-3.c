/* { dg-do compile } */
/* { dg-options "-O2 -march=native -mno-uintr" } */

#ifdef __UINTR__
# error UINTR should be disabled
#endif
