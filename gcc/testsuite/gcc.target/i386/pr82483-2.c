/* PR target/82483 */
/* { dg-do compile } */
/* { dg-options "-mssse3 -mno-mmx -Wno-psabi" } */
/* { dg-error "needs isa option" "" { target *-*-* } 0 } */

#include <x86intrin.h>

__v1di f1 (__v1di x, __v1di y) { return __builtin_ia32_paddq (x, y); }
__v1di f2 (__v1di x, __v1di y) { return __builtin_ia32_psubq (x, y); }
