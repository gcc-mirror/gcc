/* { dg-do run { target { lp64 && p9vector_hw } } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=1" } */

/* { dg-additional-options "-mno-strict-align" { target opt_mstrict_align } } */

/* Check whether it runs successfully if we only vectorize the epilogue
   with vector access with length.  */

#include "p9-vec-length-run-7.h"

