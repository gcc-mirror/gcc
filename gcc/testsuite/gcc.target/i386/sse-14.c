/* { dg-do compile } */
/* { dg-options "-O0 -march=k8 -m3dnow -msse4.1 -msse5" } */

/* Test that the intrinsics compile without optimization.  All of them are
   defined as inline functions in {,x,e,p,t,s,a,b}mmintrin.h  and mm3dnow.h
   that reference the proper builtin functions.  Defining away "static" and
   "__inline" results in all of them being compiled as proper functions.  */

#define static
#define __inline

#include <bmmintrin.h>
#include <smmintrin.h>
#include <mm3dnow.h>
