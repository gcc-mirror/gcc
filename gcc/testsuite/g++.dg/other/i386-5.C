/* Test that {,x,e,p,t,s,w,a,b,i}mmintrin.h, mm3dnow.h and mm_malloc.h are
   usable with -O -fkeep-inline-functions.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -fkeep-inline-functions -march=k8 -m3dnow -mavx -msse5 -maes -mpclmul" } */

#include <x86intrin.h>
