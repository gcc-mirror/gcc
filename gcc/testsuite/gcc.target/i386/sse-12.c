/* Test that {,x,e,p,t,a}mmintrin.h, mm3dnow.h and mm_malloc.h are
   usable with -std=c89 -pedantic-errors.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-std=c89 -pedantic-errors -march=k8 -m3dnow -mssse3 -msse4a" } */

#include <ammintrin.h>
#include <tmmintrin.h>
#include <mm3dnow.h>

int dummy;
