/* Test that {,x,e,p,t,s,a}mmintrin.h, mm3dnow.h and mm_malloc.h are
   usable with -O -pedantic-errors.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -pedantic-errors -march=k8 -m3dnow -msse4.1 -msse4a" } */

#include <ammintrin.h>
#include <smmintrin.h>
#include <mm3dnow.h>

int dummy;
