/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Os -ftree-slp-vectorize -mavx512vl" } */

#include "../../gcc.dg/vect/slp-widen-mult-u8.c"
