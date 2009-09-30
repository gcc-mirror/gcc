/* Test that {,x,e,p,t,s,w,a,b,i}mmintrin.h, mm3dnow.h and mm_malloc.h are
   usable with -O -std=c89 -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-O -std=c89 -pedantic-errors -march=k8 -m3dnow -mavx -mfma4 -maes -mpclmul" } */

#include <x86intrin.h>

int dummy;
