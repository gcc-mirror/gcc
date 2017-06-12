/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -w" } */
/* { dg-require-effective-target sse2 } */

register int a __asm__("xmm0");
void fn1() {}
