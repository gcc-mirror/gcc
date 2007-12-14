/* Test that {,x,e,p,t,s,a}mmintrin.h, mm3dnow.h and mm_malloc.h are
   usable with -O -std=c89 -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-O -std=c89 -pedantic-errors -march=k8 -m3dnow -msse4 -msse5" } */

#include <bmmintrin.h>
#include <smmintrin.h>
#include <mm3dnow.h>

int dummy;
