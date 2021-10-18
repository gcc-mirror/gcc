/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -ftree-vectorize" } */

#define MODIFY
#define STORE
#define USE
#include "reassoc-46.h"

/* Check that if the loop accumulator has multiple uses inside the loop, it's
   not forced to the end of the reassociation chain.  */
/* { dg-final { scan-tree-dump-times {(?:vect_)?sum_[\d._]+ = (?:(?:vect_)?_[\d._]+ \+ (?:vect_)?sum_[\d._]+|(?:vect_)?sum_[\d._]+ \+ (?:vect_)?_[\d._]+)} 2 "optimized" } } */
