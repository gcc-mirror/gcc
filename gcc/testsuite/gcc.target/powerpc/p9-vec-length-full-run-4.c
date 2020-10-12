/* { dg-do run { target { lp64 && p9vector_hw } } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -ftree-vectorize -fno-vect-cost-model" } */

/* { dg-additional-options "--param=vect-partial-vector-usage=2" } */

/* Check whether it runs successfully if we vectorize the loop fully
   with vector access with length.  */

#include "p9-vec-length-run-4.h"

