/* PR target/98537 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -std=c++11" } */

#define TYPEV char
#define TYPEW short

#define T_ARR						\
  __attribute__ ((target ("avx512vl,avx512bw")))

#include "avx512vl-pr98537-1.C"
