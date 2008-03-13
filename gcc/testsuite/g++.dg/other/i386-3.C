/* Test that {,x,e,p,t,s,a,b}mmintrin.h, mm3dnow.h and mm_malloc.h are
   usable with -O -fkeep-inline-functions.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -fkeep-inline-functions -march=k8 -m3dnow -msse4 -msse5" } */

#include <bmmintrin.h>
#include <smmintrin.h>
#include <mm3dnow.h>
