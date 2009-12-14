/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -fkeep-inline-functions -march=k8 -m3dnow -mavx -mxop -maes -mpclmul -mpopcnt -mabm -mlwp" } */

/* Test that {,x,e,p,t,s,w,a,b,i}mmintrin.h, xopintrin.h, abmintrin.h,
   lwpintrin.h, popcntintrin.h and mm3dnow.h are usable with
   -O -fkeep-inline-functions.  */

#include <x86intrin.h>
