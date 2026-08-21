/* { dg-do compile { target bitint } } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-additional-options "-mmax-vectorization --param=vect-epilogues-nomask=0 -fdump-tree-vect-details" } */

#include "../../gcc.dg/vect/vect-sat-trunc-shift-sign-source-1.c"

/* { dg-final { scan-tree-dump-times "sat_trunc pattern recognized" 6 "vect" } } */
