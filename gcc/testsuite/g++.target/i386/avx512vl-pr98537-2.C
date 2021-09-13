/* PR target/98537 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -std=c++11" } */

#define TYPEV float
#define TYPEW double

#include "avx512vl-pr98537-1.C"
