/* Test that {,x,e,p}mmintrin.h and mm_malloc.h are
   usable with -pedantic-errors.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-pedantic-errors -msse3" } */

#include <mmintrin.h>
#include <xmmintrin.h>
#include <emmintrin.h>
#include <pmmintrin.h>
#include <mm_malloc.h>

int dummy;
