/* Test that {,x,e,p,t,s,w,g,a,b}mmintrin.h, mm3dnow.h and mm_malloc.h are
   usable with -O -fkeep-inline-functions.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -fkeep-inline-functions -march=k8 -m3dnow -maes -mpclmul -mavx -msse5" } */

#include <wmmintrin.h>
#include <bmmintrin.h>
#include <gmmintrin.h>
#include <mm3dnow.h>
